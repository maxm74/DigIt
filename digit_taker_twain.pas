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
  simpleipc, MM_SyncIPC, Process, Classes, SysUtils, Twain, DelphiTwain, DelphiTwainUtils,
  Digit_Bridge, Digit_Taker_Twain_Types, Digit_Taker_Twain_SelectForm, Digit_Taker_Twain_SettingsForm;

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
    AcquireFileName:String;

    function getCommsClient: TSyncIPCClient;
    function getTwain: TCustomDelphiTwain;

    function IPC_GetDevicesList:Integer;
    function IPC_FindSource(AManufacturer, AProductFamily, AProductName:String):Integer;
    function IPC_OpenDevice(AIndex:Integer):Boolean;
    function IPC_ParamsSet:Boolean;
    function IPC_ParamsGet(var TwainCap:TTwainParamsCapabilities):Boolean;
    function IPC_Preview(AFileName:String):Boolean;
    function IPC_Take(AFileName:String):Boolean;

    function ParamsGet(var TwainCap:TTwainParamsCapabilities):Boolean;

    procedure TwainAcquireNative(Sender: TObject; const Index: Integer;
                                 nativeHandle: TW_UINT32; var Cancel: Boolean);
    function internalTake(isPreview:Boolean; var Data:Variant):TDigIt_TakerResultType;

    procedure FreeCommsClient;

    procedure RefreshList(ASender:TTwainSelectSource);

    property Twain: TCustomDelphiTwain read getTwain;
    property CommsClient:TSyncIPCClient read getCommsClient;

  public
    constructor Create(aParams :TPersistent); override;
    destructor Destroy; override;

    class function RegisterName: String; override;
    class function Params_GetClass: TPersistentClass; override;
    function Params_GetFromUser: Boolean; override;
    procedure Params_Set(newParams: TPersistent); override;
    class function UI_Title: String; override;
    class function UI_ImageIndex: Integer; override;
    function UI_Params_Summary: String; override;

    function Preview(var Data:Variant):TDigIt_TakerResultType; override;
    function Take(var Data:Variant):TDigIt_TakerResultType; override;
    function ReTake(var Data:Variant):TDigIt_TakerResultType; override;
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

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
  end;

  Result :=rTwain;
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
     aUserInterface.ModalUI:=False;
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

function TDigIt_Taker_Twain.IPC_ParamsSet: Boolean;
var
   recSize:Integer;
   resType:TMessageType;
   res:Boolean;

begin
  Result:=False;
  recSize :=SizeOf(TDigIt_Taker_TwainParams(rParams).TwainParams);
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_SET, mtSync_Var,
               TDigIt_Taker_TwainParams(rParams).TwainParams, recSize, res, recSize);
  Result := (resType=mtSync_Integer) and (res=True);
end;

function TDigIt_Taker_Twain.IPC_ParamsGet(var TwainCap: TTwainParamsCapabilities): Boolean;
var
   recStream:TMemoryStream=nil;
   recSize, i:Integer;
   resType:TMessageType;
   curBufExtended, recBufExtended:PSingle;
   curBufInteger, recBufInteger:PInteger;

begin
  Result:=False;
  FillChar(TwainCap, Sizeof(TwainCap), 0);
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_GET, mtSync_Null, i, 0, recStream, recSize);
  if (resType=mtSync_Stream) and (recStream<>nil) then
  try
    recStream.Position:=0;
    recSize :=Sizeof(TwainCap.ResolutionArray);
    recSize :=Sizeof(TwainCap);
    recSize :=Sizeof(Single);  { #todo 10 -oMaxM : Size differences between 32 and 64 Bit (Single?) }
    recStream.Read(TwainCap,
                   Sizeof(TwainCap)
                   -Sizeof(TwainCap.ResolutionArray)
                   -Sizeof(TwainCap.BitDepthArray)
                   );

    TwainCap.ResolutionArray :=nil;
    TwainCap.BitDepthArray:=nil;
    //Respect the order in TTwainParamsCapabilities Type
    GetMem(recBufExtended, TwainCap.ResolutionArraySize*Sizeof(Single));
    recStream.Read(recBufExtended^, TwainCap.ResolutionArraySize*Sizeof(Single));

    SetLength(TwainCap.ResolutionArray, TwainCap.ResolutionArraySize);
    curBufExtended:=recBufExtended;
    for i:=0 to  TwainCap.ResolutionArraySize-1 do
    begin
      TwainCap.ResolutionArray[i] :=curBufExtended^;
      Inc(curBufExtended);
    end;
    FreeMem(recBufExtended, TwainCap.ResolutionArraySize*Sizeof(Single));

    GetMem(recBufInteger, TwainCap.BitDepthArraySize*Sizeof(Integer));
    recStream.Read(recBufInteger^, TwainCap.BitDepthArraySize*Sizeof(Integer));

    SetLength(TwainCap.BitDepthArray, TwainCap.BitDepthArraySize);
    curBufInteger:=recBufInteger;
    for i:=0 to  TwainCap.BitDepthArraySize-1 do
    begin
      TwainCap.BitDepthArray[i] :=curBufInteger^;
      Inc(curBufInteger);
    end;
    FreeMem(recBufInteger, TwainCap.BitDepthArraySize*Sizeof(Integer));
    Result:=True;
  finally
    recStream.Free;
  end;
end;

function TDigIt_Taker_Twain.IPC_Preview(AFileName: String): Boolean;
var
   recSize:Longint;
   recBuf:Boolean;
   resType:TMessageType;

begin
  Result :=False;
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PREVIEW, mtSync_String, AFileName, 0, recBuf, recSize);
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

function TDigIt_Taker_Twain.ParamsGet(var TwainCap: TTwainParamsCapabilities): Boolean;
var
   capRet:TCapabilityRet;
   TwainSource:TTwainSource;
   Current: Integer;
   paperCurrent: TTwainPaperSize;
   pixelCurrent:TTwainPixelType;
   resolutionCurrent:Single;

begin
  TwainSource:=Twain.SelectedSource;
  TwainCap.PaperFeedingSet:=TwainSource.GetPaperFeeding;
  capRet :=TwainSource.GetPaperSizeSet(paperCurrent, TwainCap.PaperSizeDefault, TwainCap.PaperSizeSet);
  capRet :=TwainSource.GetIBitDepth(Current, TwainCap.BitDepthDefault, TwainCap.BitDepthArray);
  TwainCap.BitDepthArraySize :=Length(TwainCap.BitDepthArray);
  capRet :=TwainSource.GetIPixelType(pixelCurrent, TwainCap.PixelTypeDefault, TwainCap.PixelType);
  capRet :=TwainSource.GetIXResolution(resolutionCurrent, TwainCap.ResolutionDefault, TwainCap.ResolutionArray);
  TwainCap.ResolutionArraySize :=Length(TwainCap.ResolutionArray);
end;

procedure TDigIt_Taker_Twain.TwainAcquireNative(Sender: TObject; const Index: Integer;
                                                nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  WriteBitmapToFile(AcquireFileName, nativeHandle);
end;

function TDigIt_Taker_Twain.internalTake(isPreview: Boolean; var Data:Variant): TDigIt_TakerResultType;
var
   capRet:TCapabilityRet;
   TwainSource:TTwainSource;
   resTake:Boolean;

begin
  try
     Result :=trtFilename;
     Data :='';
   //  TFormAnimAcquiring.Execute; Application.ProcessMessages;

     //Delete previous scanned file
     if FileExists(TempDir+'twain_'+IntToStr(iTempFile-1)+'.bmp')
     then DeleteFile(TempDir+'twain_'+IntToStr(iTempFile-1)+'.bmp');

     AcquireFileName :=TempDir+'twain_'+IntToStr(iTempFile)+'.bmp';

     if SelectedSourceIPC
     then begin
            resTake :=IPC_ParamsSet;

            if isPreview
            then resTake :=IPC_Preview(AcquireFileName)
            else resTake :=IPC_Take(AcquireFileName);

            if resTake
            then begin
                   Data :=AcquireFileName;
                   Inc(iTempFile);
                 end
            else Data :='';
          end
     else begin
            if Assigned(Twain.SelectedSource)
            then begin
                   TwainSource:=Twain.SelectedSource;
                   TwainSource.Loaded :=True;

                   with TDigIt_Taker_TwainParams(rParams) do
                   begin
                   //Set Parameters, (after a capture the scanner reset it to default???)
                   capRet :=TwainSource.SetPaperFeeding(PaperFeed);
                   capRet :=TwainSource.SetDuplexEnabled(False);
                   capRet :=TwainSource.SetPaperSize(PaperSize);
                   capRet :=TwainSource.SetIPixelType(PixelType);

                   if isPreview
                   then begin
                          capRet :=TwainSource.SetIXResolution(75);
                          capRet :=TwainSource.SetIYResolution(75);
                        end
                   else begin
                          capRet :=TwainSource.SetIXResolution(Resolution);
                          capRet :=TwainSource.SetIYResolution(Resolution);
                        end;
                   capRet :=TwainSource.SetContrast(Contrast);
                   capRet :=TwainSource.SetBrightness(Brightness);
                   end;
                   Twain.SelectedSource.SetIndicators(True);

                   { #todo 10 -oMaxM : Switch to ttmNative Mode (my office Scanner fail if paper=tpsNone and dpi>150) see Tests}
                   TwainSource.TransferMode:=ttmNative; //ttmFile;
                   //TwainSource.SetupFileTransfer(AcquireFileName, tfBMP);
                   Twain.OnTwainAcquireNative:=@TwainAcquireNative;
                   TwainSource.EnableSource(False, False, Application.ActiveFormHandle);
                   //TwainSource.EnableSource(rUserInterface); if in Future we want in the Options

                   Data :=AcquireFileName;
                   Inc(iTempFile);
                 end
            else Data :='';
          end;

  finally
   // FreeAndNil(FormAnimAcquiring);
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

function TDigIt_Taker_Twain.Preview(var Data:Variant):TDigIt_TakerResultType;
begin
  Result :=internalTake(True, Data);
end;

function TDigIt_Taker_Twain.Take(var Data:Variant):TDigIt_TakerResultType;
begin
  Result :=internalTake(False, Data);
end;

function TDigIt_Taker_Twain.ReTake(var Data:Variant):TDigIt_TakerResultType;
begin
  Result :=internalTake(False, Data);
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
  TwainCap:TTwainParamsCapabilities;

begin
  Result :=False;
  if (rParams=nil) then exit;
  with TDigIt_Taker_TwainParams(rParams) do
  try
     if Twain.SourceManagerLoaded then
     begin
       //Close Current Scanner
       if Assigned(Twain.SelectedSource) then Twain.SelectedSource.Loaded:=False;
       Twain.SourceManagerLoaded:=False;
     end;

     //Load source manager and Enumerate Internal Devices
     Twain.SourceManagerLoaded :=True;
     countTwain_Source:=Twain.SourceCount;

     //Get 32bit Devices
     countIPC_Source :=IPC_GetDevicesList;

     selectedSource :=TTwainSelectSource.Execute(@RefreshList, Twain, ipcSourceList, TDigIt_Taker_TwainParams(rParams));
     if (selectedSource>-1) then
     begin
       //if the index of selected device is greater than countTwain_Source then is a 32bit scanner
       SelectedSourceIPC :=(selectedSource>=countTwain_Source);
       if SelectedSourceIPC
       then begin
              SelectedSourceIndex :=selectedSource-countTwain_Source;
              Result :=IPC_OpenDevice(SelectedSourceIndex);
            end
       else begin
              SelectedSourceIndex :=selectedSource;
              Twain.SelectedSourceIndex :=SelectedSourceIndex;
              Result :=(Twain.SelectedSource<>nil);
              if Result then Twain.SelectedSource.Loaded:=True;
            end;

       if Result then
       begin
           IPC_Scanner:=SelectedSourceIPC;
           if SelectedSourceIPC
           then begin
                  Manufacturer :=ipcSourceList[SelectedSourceIndex].Manufacturer;
                  ProductFamily :=ipcSourceList[SelectedSourceIndex].ProductFamily;
                  ProductName :=ipcSourceList[SelectedSourceIndex].ProductName;

                  IPC_ParamsGet(TwainCap);
                end
           else begin
                  Manufacturer :=Twain.SelectedSource.Manufacturer;
                  ProductFamily :=Twain.SelectedSource.ProductFamily;
                  ProductName :=Twain.SelectedSource.ProductName;

                  ParamsGet(TwainCap);
                end;

         TTwainSettingsSource.Execute(TwainCap, TDigIt_Taker_TwainParams(rParams));
       end;
     end;

  finally
    FreeAndNil(TwainSelectSource);
    FreeAndNil(TwainSettingsSource);
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

