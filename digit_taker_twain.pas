(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Taker                                                             **
*******************************************************************************)

unit Digit_Taker_Twain;

{$mode ObjFPC}{$H+}

interface

uses
  simpleipc, syncipc, Process, Classes, SysUtils, Digit_Bridge, Digit_Taker_Twain_Types,
  Twain, DelphiTwain, Digit_Taker_Twain_SelectForm;

type
  { TDigIt_Taker_Twain }
  TDigIt_Taker_Twain = class(TDigIt_Taker)
  private
    rTwain:TCustomDelphiTwain;
    ipcProcess:TProcess;
    rCommsClient:TSyncIPCClient;
    ipcSourceList:array of TW_IDENTITY;
    SelectedSourceIPC:Boolean;
    SelectedSourceIndex,
    countTwain_Source,
    countIPC_Source,
    iTempFile:Integer;

    function getCommsClient: TSyncIPCClient;
    function getTwain: TCustomDelphiTwain;
    procedure SetTwainTranferMode;

    function IPC_Callback(AElapsedTime:DWord; AMsgID:Integer):Boolean;
    function IPC_GetDevicesList:Integer;
    function IPC_FindSource(AManufacturer, AProductFamily, AProductName:String):Integer;
    function IPC_OpenDevice(AIndex:Integer):Boolean;
    function IPC_Take(AFileName:String):Boolean;
    procedure FreeCommsClient;

    procedure RefreshList(ASender:TTwainSelectSource);

    property Twain: TCustomDelphiTwain read getTwain;
    property CommsClient:TSyncIPCClient read getCommsClient;

  public
    constructor Create(aParams :TPersistent); override;
    destructor Destroy; override;

    class function RegisterName: String; override;
    class function Params_GetClass : TPersistentClass; override;
    function Params_GetFromUser: Boolean; override;
    procedure Params_Set(newParams: TPersistent); override;
    class function UI_Title: String; override;
    class function UI_ImageIndex: Integer; override;
    function UI_Params_Summary: String; override;

    function Preview: String; override;
    function Take: String; override;
    function ReTake: String; override;
  end;

implementation

uses Controls, Forms, Dialogs, Digit_Types, BGRABitmapTypes
 // ,DigIt_Form_AnimAcquiring
  ;

{ TDigIt_Taker_Twain }

function TDigIt_Taker_Twain.getCommsClient: TSyncIPCClient;
var
   i:Integer;

   function isServerRunning(waitToStart:Boolean):Boolean;
   begin
     Result :=False;
     try
        if (rCommsClient = nil) then
        begin
          rCommsClient := TSyncIPCClient.Create(nil);
          rCommsClient.ServerID:=TWAIN32_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
          //rCommsClient.MsgCallback:=@IPC_Callback;    { #note 5 -oMaxM : ?? don't work }
        end;

        if waitToStart
        then begin
               i:=0;
               repeat
                 sleep(100);
                 try
                    rCommsClient.Connect;
                    inc(i);
                    Result :=rCommsClient.ServerRunning;
                 except
                   Result :=False;
                 end;
               until (i>666) or Result;
             end
        else begin
               rCommsClient.Connect;
               Result :=rCommsClient.ServerRunning;
             end;

     except
       FreeAndNil(rCommsClient);
     end;
   end;

begin
  try
    //if server is running do not start again
    if not(isServerRunning(false)) then
    begin
      if (ipcProcess = nil) then
      begin
        ipcProcess :=TProcess.Create(nil);
        ipcProcess.CurrentDirectory :=ApplicationDir;
        ipcProcess.Executable :=ApplicationDir+TWAIN32_SERVER_EXE;
        ipcProcess.StartupOptions:=[suoUseShowWindow];
        {$ifopt D+}
        ipcProcess.ShowWindow := swoShow;
        {$else}
        ipcProcess.Options:=[poDetached];
        ipcProcess.ShowWindow := swoHIDE;
        {$endif}
        ipcProcess.Execute;

        if not(isServerRunning(true))
        then raise Exception.Create(TWAIN32_SERVER_NAME+' not Running...');
      end;
    end;

  except
    FreeAndNil(rCommsClient);
    FreeAndNil(ipcProcess);
  end;

  Result :=rCommsClient;
end;

function TDigIt_Taker_Twain.getTwain: TCustomDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TCustomDelphiTwain.Create;
   // rTwain.OnTwainAcquire := @TwainTwainAcquire;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
  end;

  Result :=rTwain;
end;

procedure TDigIt_Taker_Twain.SetTwainTranferMode;
begin
end;

function TDigIt_Taker_Twain.IPC_Callback(AElapsedTime: DWord; AMsgID: Integer): Boolean;
begin
(*  if (FormAnimAcquiring<>nil)
  then begin
         Result:=FormAnimAcquiring.Aborted;
         FormAnimAcquiring.Repaint;
  end
  else Result :=False;

  Application.ProcessMessages; *)
end;

function TDigIt_Taker_Twain.IPC_GetDevicesList: Integer;
var
   recBuf:pTW_IDENTITY=nil;
   curBuf:pTW_IDENTITY;
   recSize, i, count:Integer;
   resType:TMessageType;

begin
  Result :=0;
  if Length(ipcSourceList)>0 then SetLength(ipcSourceList, 0);
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_LIST, mtSync_Null, recBuf, 0, recBuf, recSize);
     if (resType=mtSync_Pointer) and (recBuf<>nil) then
     begin
       count :=Trunc(recSize/SizeOf(TW_IDENTITY));
       SetLength(ipcSourceList, count);
       curBuf:=recBuf;
       for i:=0 to count-1 do
       begin
         ipcSourceList[i] :=curBuf^;
         Inc(curBuf);
       end;
       FreeMem(recBuf, recSize);
       Result:=count;
     end;

  except
    if (recBuf<>nil) then FreeMem(recBuf, recSize);
  end;
end;

function TDigIt_Taker_Twain.IPC_FindSource(AManufacturer, AProductFamily, AProductName: String): Integer;
var
   recSize, recBuf:Longint;
   AIdentity:TW_IDENTITY;
   resType:TMessageType;

begin
  Result :=-1;
  try
     AIdentity.Manufacturer:=AManufacturer;
     AIdentity.ProductFamily:=AProductFamily;
     AIdentity.ProductName:=AProductName;
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_FIND, mtSync_Var, AIdentity, sizeof(TW_IDENTITY), recBuf, recSize);
     if (resType=mtSync_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Taker_Twain.IPC_OpenDevice(AIndex: Integer): Boolean;
var
   recSize:Longint;
   recBuf:Boolean;
   resType:TMessageType;
   aUserInterface:TW_USERINTERFACE;

begin
  Result :=False;
  try
     aUserInterface.ShowUI:=False;
     aUserInterface.ModalUI:=True;
     aUserInterface.hParent:=Application.ActiveFormHandle;

     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_USERINTERFACE, mtSync_Var,
                                                  aUserInterface, SizeOf(TW_USERINTERFACE), recBuf, recSize);
     if (resType=mtSync_Integer)
     then Result:=recBuf;

     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_OPEN, mtSync_Integer, AIndex, 0, recBuf, recSize);
     if (resType=mtSync_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Taker_Twain.IPC_Take(AFileName: String): Boolean;
var
   recSize:Longint;
   recBuf:Boolean;
   resType:TMessageType;

begin
  Result :=False;
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_TAKE, mtSync_String, AFileName, 0, recBuf, recSize);
     if (resType=mtSync_Integer)
     then Result:=recBuf;

  except
  end;
end;

procedure TDigIt_Taker_Twain.FreeCommsClient;
var
   recSize, recBuf:Longint;
   resType:TMessageType;

begin
  if (rCommsClient<>nil) then
  begin
    //In Debug Stop the Process Manually
    {$ifopt D-}
    resType :=rCommsClient.SendSyncMessage(30000, MSG_TWAIN32_STOP, mtSync_Null, recBuf, 0, recBuf, recSize);
    {$endif}
    //No need to test RES_TWAIN32_STOPPED
    FreeAndNil(rCommsClient);
  end;
  FreeAndNil(ipcProcess);
end;

(*
procedure TDigIt_Taker_Twain.TwainTwainAcquire(Sender: TObject; const Index: Integer; Image: TBitmap;
  var Cancel: Boolean);
begin
  //  ImageHolder.Picture.Bitmap.Assign(Image);
    Cancel := True;//Only want one image
end;
*)

constructor TDigIt_Taker_Twain.Create(aParams: TPersistent);
begin
  rTwain:=nil;
  rCommsClient:=nil;
  SelectedSourceIPC:=False;
  SelectedSourceIndex:=-1;
  iTempFile :=0;

  inherited Create(aParams);
end;

destructor TDigIt_Taker_Twain.Destroy;
begin
  if (rTwain<>nil) then rTwain.Free;
  FreeCommsClient;

  //Delete previous scanned file
  if FileExists(TempDir+'twain_'+IntToStr(iTempFile-1)+'.bmp')
  then DeleteFile(TempDir+'twain_'+IntToStr(iTempFile-1)+'.bmp');

  inherited Destroy;
end;

class function TDigIt_Taker_Twain.RegisterName: String;
begin
  Result :='TDigIt_Taker_Twain';
end;

class function TDigIt_Taker_Twain.Params_GetClass: TPersistentClass;
begin
  result :=TDigIt_Taker_TwainParams;
end;

class function TDigIt_Taker_Twain.UI_Title: String;
begin
  Result :='Twain Source'; { #todo 2 -oMaxM : Usare Risorse per la Traduzione }
end;

class function TDigIt_Taker_Twain.UI_ImageIndex: Integer;
begin
  Result :=2;
end;

function TDigIt_Taker_Twain.UI_Params_Summary: String;
begin
  Result :='';
end;

function TDigIt_Taker_Twain.Preview: String;
begin
  try
     Result :=Take;
  finally
  end;
end;

function TDigIt_Taker_Twain.Take: String;
begin
  try
     Result :='';
   //  TFormAnimAcquiring.Execute;
    Application.ProcessMessages;

     //Delete previous scanned file
     if FileExists(TempDir+'twain_'+IntToStr(iTempFile-1)+'.bmp')
     then DeleteFile(TempDir+'twain_'+IntToStr(iTempFile-1)+'.bmp');

     if SelectedSourceIPC
     then begin
            Application.ProcessMessages;
            if IPC_Take(TempDir+'twain_'+IntToStr(iTempFile)+'.bmp')
            then begin
                   Result :=TempDir+'twain_'+IntToStr(iTempFile)+'.bmp';
                   Inc(iTempFile);
                 end
            else Result:='';
            Application.ProcessMessages;
          end
     else begin
            if Assigned(Twain.SelectedSource)
            then begin
                   Application.ProcessMessages;
                   Twain.SelectedSource.Loaded := TRUE;
                   //Twain.SelectedSource.ShowUI := False;//display interface
                   //Twain.SelectedSource.Modal:=False;
                   Twain.SelectedSource.TransferMode:=ttmFile;
                   Twain.SelectedSource.SetupFileTransfer(TempDir+'twain_'+IntToStr(iTempFile)+'.bmp', tfBMP);
                   Twain.SelectedSource.EnableSource(False, True, Application.ActiveFormHandle);
                   Result :=TempDir+'twain_'+IntToStr(iTempFile)+'.bmp';
                   Inc(iTempFile);
                   Application.ProcessMessages;
                 end
            else Result:='';
          end;

  finally
   // FreeAndNil(FormAnimAcquiring);
  end;
end;

function TDigIt_Taker_Twain.ReTake: String;
begin
  Result :=Take;
end;

procedure TDigIt_Taker_Twain.RefreshList(ASender:TTwainSelectSource);
begin
  //Load source manager and Enumerate Internal Devices
  Twain.SourceManagerLoaded :=False;
  Application.ProcessMessages; { #todo -oMaxM : Is really necessary? }
  Twain.SourceManagerLoaded :=True;
  countTwain_Source:=Twain.SourceCount;
  countIPC_Source :=IPC_GetDevicesList;
  ASender.FillList(ipcSourceList);
end;

function TDigIt_Taker_Twain.Params_GetFromUser: Boolean;
var
  selectedSource:Integer;

begin
  Result :=False;
  if (rParams=nil) then exit;
  try
     //Load source manager and Enumerate Internal Devices
     Twain.SourceManagerLoaded :=True;
     countTwain_Source:=Twain.SourceCount;

     countIPC_Source :=IPC_GetDevicesList;

     selectedSource :=TTwainSelectSource.Execute(@RefreshList, Twain, ipcSourceList, False, -1);
     if (selectedSource>-1) then
     begin
       SelectedSourceIPC :=(selectedSource>=countTwain_Source);
       if SelectedSourceIPC
       then begin
              SelectedSourceIndex :=selectedSource-countTwain_Source;
              IPC_OpenDevice(SelectedSourceIndex);
            end
       else begin
              SelectedSourceIndex :=selectedSource;
              Twain.SelectedSourceIndex :=SelectedSourceIndex;
            end;

       Result :=True;

       with TDigIt_Taker_TwainParams(rParams) do
       begin
         IPC_Scanner:=SelectedSourceIPC;
         if SelectedSourceIPC
         then begin
                Manufacturer :=ipcSourceList[SelectedSourceIndex].Manufacturer;
                ProductFamily :=ipcSourceList[SelectedSourceIndex].ProductFamily;
                ProductName :=ipcSourceList[SelectedSourceIndex].ProductName;
              end
         else begin
                Manufacturer :=Twain.SelectedSource.Manufacturer;
                ProductFamily :=Twain.SelectedSource.ProductFamily;
                ProductName :=Twain.SelectedSource.ProductName;
              end;
       end;
     end;

  finally
    FreeAndNil(TwainSelectSource);
  end;
end;

procedure TDigIt_Taker_Twain.Params_Set(newParams: TPersistent);
var
   dlgRes:TModalResult;

begin
  rParams :=newParams;
  with TDigIt_Taker_TwainParams(rParams) do
  begin
    SelectedSourceIndex :=-1;
    repeat
      if IPC_Scanner
      then SelectedSourceIndex :=IPC_FindSource(Manufacturer, ProductFamily, ProductName)
      else begin
             Twain.SourceManagerLoaded :=False;
             Application.ProcessMessages; { #todo -oMaxM : Is really necessary? }
             Twain.SourceManagerLoaded :=True;
             SelectedSourceIndex :=Twain.FindSource(Manufacturer, ProductFamily, ProductName);
           end;

      if (SelectedSourceIndex=-1)
      then begin
             { #todo 1 -oMaxM : Scanner not find, A Message to User with Retry }
             if (MessageDlg('DigIt Twain', 'Device not found...'#13#10+
                            ProductName+#13#10+Manufacturer, mtError, [mbRetry, mbAbort], 0)=mrAbort)
             then break;
           end;
    until (SelectedSourceIndex>-1);

    if (SelectedSourceIndex=-1)
    then Params_GetFromUser
    else begin
           SelectedSourceIPC :=IPC_Scanner;
           if IPC_Scanner
           then begin
                  if IPC_OpenDevice(SelectedSourceIndex) then
                  begin
                  end;
                  { #todo 1 -oMaxM : else ? }
                end
           else begin
                  Twain.SelectedSourceIndex:=SelectedSourceIndex;
                  if Assigned(Twain.SelectedSource) then
                  begin
                    Manufacturer :=Twain.SelectedSource.Manufacturer;
                    ProductFamily :=Twain.SelectedSource.ProductFamily;
                    ProductName :=Twain.SelectedSource.ProductName;
                  end;
                  { #todo 1 -oMaxM : else ? }
                end;
         end;
  end;
end;

initialization
  theBridge.Takers.Register(TDigIt_Taker_Twain.RegisterName, TDigIt_Taker_Twain);

end.

