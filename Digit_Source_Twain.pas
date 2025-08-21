(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Source                                                             **
*******************************************************************************)

unit Digit_Source_Twain;

{$mode ObjFPC}{$H+}

interface

uses
  simpleipc, SyncIPC, Process, Classes, SysUtils,
  MM_OpenArrayList,
  Twain, DelphiTwain, DelphiTwainTypes, DelphiTwainUtils, DelphiTwain_SelectForm, DelphiTwain_SettingsForm,
  Digit_Bridge_Intf, DigIt_Source_Common, Digit_Source_Twain_Types;

const
  DigIt_Source_Twain_Name = 'Twain Device';

resourcestring
  rsTwainName = 'Twain Device';
  rsTwainExcNotFound = 'Device not found...';

type
  { TDigIt_Source_Twain_Params }

  TDigIt_Source_Twain_Params  = class(TDigIt_Source_Common_Params)
  protected
    rScannerInfo: TTwainDeviceInfo;
    rTwainParams: TTwainParams;

    function GetSummary: String; override;

  public
    function GetFromUser: Boolean; stdcall; override;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; override;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; override;

    function OnSelected: Boolean; stdcall; override;

    constructor Create(AOwner: TDigIt_Source_Common); override;
  end;

  { TDigIt_Source_WIA_Files}

  TDigIt_Source_Twain_Files = class(specialize TOpenArray<String>, IDigIt_ArrayR_PChars)
    function Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;
  end;

  { TDigIt_Source_Twain }

  TDigIt_Source_Twain = class(TDigIt_Source_Common)
  private
    rTwain:TCustomDelphiTwain;
    TwainSource:TTwainSource;
    TwainCapGetted: Boolean;
    TwainCap: TTwainParamsCapabilities;
    rUserInterface: TW_USERINTERFACE;

    ipcProcess:TProcess;
    rCommsClient:TSyncIPCClient;
    ipcSourceList:array of TW_IDENTITY;
    countTwain_Source,
    countIPC_Source: Integer;
    DownloadedFiles: TDigIt_Source_Twain_Files;

    function getCommsClient: TSyncIPCClient;
    function getTwain: TCustomDelphiTwain;

    function IPC_GetDevicesList: Integer;
    function IPC_FindSource(AManufacturer, AProductFamily, AProductName: TW_STR32): Integer;
    function IPC_OpenDevice(AManufacturer, AProductFamily, AProductName: TW_STR32): Boolean;
    function IPC_ParamsSet: Boolean;
    function IPC_CapabilitiesGet: Boolean;
    function IPC_Preview(APath: String): Integer;
    function IPC_Take(APath: String): Integer;

    function GetTwainSource(ALoad: Boolean): TTwainSource;

    function TwainProcessMessages(Sender: TObject; const Index: Integer): Boolean;

    procedure TwainAcquireNative(Sender: TObject; const Index: Integer;
                                 nativeHandle: TW_UINT32; var Cancel: Boolean);

    procedure FreeCommsClient;

    procedure RefreshList(ASender:TTwainSelectSource);

    property Twain: TCustomDelphiTwain read getTwain;
    property CommsClient:TSyncIPCClient read getCommsClient;

  protected
    function GetParamsClass: TDigIt_Source_Common_ParamsClass; override;
    function GetName: String; override;

  public
    //IDigIt_Interface Implementation
    function Enabled: Boolean; stdcall; override;
    function UI_ImageIndex: Integer; stdcall; override;

    //IDigIt_Source Implementation
                                                       //Take a Picture and returns FileName/s
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall; override;
    procedure Clear; stdcall; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Controls, Forms, Dialogs, FileUtil, BGRABitmapTypes, Laz2_XMLCfg,
     Digit_Types, Digit_Bridge_Impl;

var
   TwainPath_Temp: String;
   Source_Twain : TDigIt_Source_Twain = nil;

{ TDigIt_Source_Twain_Params }

function TDigIt_Source_Twain_Params.GetFromUser: Boolean; stdcall;
var
  newSelectedInfo: TTwainDeviceInfo;
  useScannerDefault: Boolean;

begin
  Result :=False;

  with TDigIt_Source_Twain(rOwner) do
  try
     if Twain.SourceManagerLoaded then
     begin
       //Close Current Scanner if any
       if (Twain.SelectedSource <> nil)
       then Twain.SelectedSource.Loaded:= False;

       Twain.SourceManagerLoaded:=False;
     end;

     //Load source manager and Enumerate Internal Devices
     Twain.SourceManagerLoaded :=True;
     countTwain_Source:=Twain.SourceCount;

     //Get 32bit Devices
     countIPC_Source :=IPC_GetDevicesList;

     newSelectedInfo:= rScannerInfo;
     if TTwainSelectSource.Execute('DigIt', '(32 bit)', @RefreshList, Twain, ipcSourceList, newSelectedInfo) then
     begin
       useScannerDefault:= DeviceInfoDifferent(rScannerInfo, newSelectedInfo);

       //if the selected device is a 32bit scanner
       if newSelectedInfo.FromAddList
       then Result:= IPC_OpenDevice(newSelectedInfo.Manufacturer,
                                    newSelectedInfo.ProductFamily,
                                    newSelectedInfo.ProductName)
       else Result:= (Twain.SelectSource(newSelectedInfo.Manufacturer,
                                         newSelectedInfo.ProductFamily,
                                         newSelectedInfo.ProductName, True) <> nil);

       //If Device is opened
       if Result then
       begin
           rScannerInfo:= newSelectedInfo;
           if rScannerInfo.FromAddList
           then IPC_CapabilitiesGet
           else begin
                  Result:= (GetTwainSource(True)<>nil) and TwainSource.GetParamsCapabilities(TwainCap);
                  TwainCapGetted:= Result;
                end;

           TTwainSettingsSource.Execute(useScannerDefault, TwainCap, rTwainParams);
       end;
     end;

  finally
    TwainSelectSource.Free; TwainSelectSource:= Nil;
    TwainSettingsSource.Free; TwainSettingsSource:= Nil;
  end;
end;

function TDigIt_Source_Twain_Params.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);

     rScannerInfo.FromAddList:= XMLWork.GetValue(xml_RootPath+'/IPC_Scanner', False);
     rScannerInfo.Manufacturer:= XMLWork.GetValue(xml_RootPath+'/Manufacturer', '');
     rScannerInfo.ProductFamily:= XMLWork.GetValue(xml_RootPath+'/ProductFamily', '');
     rScannerInfo.ProductName:= XMLWork.GetValue(xml_RootPath+'/ProductName', '');
     rOwner.countTakes:= XMLWork.GetValue(xml_RootPath+'/CountTakes', 0);

     //Set Default Values
     rTwainParams.PaperFeed:= pfFlatbed;
     rTwainParams.PaperSize:= tpsNone;
     rTwainParams.PixelType:= tbdRgb;

     XMLWork.GetValue(xml_RootPath+'/PaperFeed', rTwainParams.PaperFeed, TypeInfo(TTwainPaperFeeding));
     XMLWork.GetValue(xml_RootPath+'/PaperSize', rTwainParams.PaperSize, TypeInfo(TTwainPaperSize));
     XMLWork.GetValue(xml_RootPath+'/PixelType', rTwainParams.PixelType, TypeInfo(TTwainPixelType));
     rTwainParams.Resolution:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Resolution', '100'));
     rTwainParams.Contrast:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Contrast', '0'));
     rTwainParams.Brightness:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Brightness', '0'));
     rTwainParams.BitDepth:= XMLWork.GetValue(xml_RootPath+'/BitDepth', 24);

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_Twain_Params.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);

     XMLWork.SetValue(xml_RootPath+'/IPC_Scanner', rScannerInfo.FromAddList);
     XMLWork.SetValue(xml_RootPath+'/Manufacturer', rScannerInfo.Manufacturer);
     XMLWork.SetValue(xml_RootPath+'/ProductFamily', rScannerInfo.ProductFamily);
     XMLWork.SetValue(xml_RootPath+'/ProductName', rScannerInfo.ProductName);
     XMLWork.SetValue(xml_RootPath+'/CountTakes', rOwner.countTakes);

     XMLWork.SetValue(xml_RootPath+'/PaperFeed', rTwainParams.PaperFeed, TypeInfo(TTwainPaperFeeding));
     XMLWork.SetValue(xml_RootPath+'/PaperSize', rTwainParams.PaperSize, TypeInfo(TTwainPaperSize));
     XMLWork.SetValue(xml_RootPath+'/PixelType', rTwainParams.PixelType, TypeInfo(TTwainPixelType));
     XMLWork.SetValue(xml_RootPath+'/Resolution', FloatToStr(rTwainParams.Resolution));
     XMLWork.SetValue(xml_RootPath+'/Contrast', FloatToStr(rTwainParams.Contrast));
     XMLWork.SetValue(xml_RootPath+'/Brightness', FloatToStr(rTwainParams.Brightness));
     XMLWork.SetValue(xml_RootPath+'/BitDepth', rTwainParams.BitDepth);

     XMLWork.Flush;

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_Twain_Params.GetSummary: String;
begin
  Result:= '';

  if (rScannerInfo.ProductName <> '') then
  begin
    Result:= rScannerInfo.ProductName;
    if (rTwainParams.PaperFeed = pfFeeder)
    then Result:= Result+' ('+rsFeeder+')'
    else Result:= Result+' ('+rsFlatbed+')';
  end;
end;

function TDigIt_Source_Twain_Params.OnSelected: Boolean; stdcall;
var
   aIndex: Integer;

begin
  Result:= False;

  with rScannerInfo, TDigIt_Source_Twain(rOwner) do
  begin
    aIndex:= -1;
    if (Manufacturer<>'') and (ProductFamily<>'') and (ProductName<>'') then
    repeat
      //Try to Open searching by Name Until Opened or user select Abort
      if FromAddList
      then aIndex :=IPC_FindSource(Manufacturer, ProductFamily, ProductName)
      else begin
             Twain.SourceManagerLoaded :=False;
             Application.ProcessMessages;
             Twain.SourceManagerLoaded :=True;
             aIndex :=Twain.FindSource(Manufacturer, ProductFamily, ProductName);
           end;

      if (aIndex = -1)
      then begin
             if (theBridge.MessageDlg('DigIt Twain', rsTwainExcNotFound+#13#10+ProductName+#13#10+Manufacturer,
                            mtError, [mbRetry, mbAbort], 0)=mrAbort)
             then break;
           end;
    until (aIndex > -1);

    if (aIndex = -1)
    then Result:= GetFromUser //User has selected Abort, Get Another Device from List
    else begin
           if FromAddList
           then Result:= IPC_OpenDevice(Manufacturer, ProductFamily, ProductName)
           else begin
                  Twain.SourceManagerLoaded:= True;
                  TwainSource:= GetTwainSource(False);
                  Result:= (TwainSource <> nil);
                  if Result then TwainSource.Loaded:= True;
                end;
         end;
  end;
end;

constructor TDigIt_Source_Twain_Params.Create(AOwner: TDigIt_Source_Common);
begin
  inherited Create(AOwner);

  FillChar(rScannerInfo, Sizeof(rScannerInfo), 0);
end;

{ TDigIt_Source_Twain_Files }

function TDigIt_Source_Twain_Files.Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;
begin
  aData:= nil;
  try
     aData:= StrNew(PChar(Get(aIndex)));
     Result:= True;
  except
     Result:= False;
  end;
end;

{ TDigIt_Source_Twain }

function TDigIt_Source_Twain.getCommsClient: TSyncIPCClient;
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
       rCommsClient.Free; rCommsClient:= Nil;
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
        ipcProcess.CurrentDirectory :=Path_Application;
        ipcProcess.StartupOptions:=[suoUseShowWindow];
        {$ifopt D+}
        ipcProcess.Executable :=Path_Application+TWAIN32_SERVER_EXE+'-dbg.exe';
        ipcProcess.ShowWindow := swoShow;
        {$else}
        ipcProcess.Executable :=Path_Application+TWAIN32_SERVER_EXE+'.exe';
        ipcProcess.Options:=[poDetached];
        ipcProcess.ShowWindow := swoHIDE;
        {$endif}
        ipcProcess.Execute;

        if not(isServerRunning(true))
        then raise Exception.Create(TWAIN32_SERVER_NAME+' not Running...');
      end;
    end;

  except
    rCommsClient.Free; rCommsClient:= Nil;
    ipcProcess.Free; ipcProcess:= Nil;
  end;

  Result :=rCommsClient;
end;

function TDigIt_Source_Twain.getTwain: TCustomDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TCustomDelphiTwain.Create;
    rTwain.OnProcessMessages:= @TwainProcessMessages;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;

    TwainCapGetted:= False;
  end;

  Result :=rTwain;
end;

function TDigIt_Source_Twain.IPC_GetDevicesList: Integer;
var
   recBuf:pTW_IDENTITY=nil;
   curBuf:pTW_IDENTITY;
   recSize, i, count:Integer;
   resType:TMessageType;

begin
  Result :=0;
  if Length(ipcSourceList)>0 then SetLength(ipcSourceList, 0);
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_LIST, mtData_Null, recBuf, 0, recBuf, recSize);
     if (resType=mtData_Pointer) and (recBuf<>nil) then
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

function TDigIt_Source_Twain.IPC_FindSource(AManufacturer, AProductFamily, AProductName: TW_STR32): Integer;
var
   recSize, recBuf:Longint;
   AIdentity:TW_IDENTITY;
   resType:TMessageType;

begin
  Result :=0;
  try
     AIdentity.Manufacturer:=AManufacturer;
     AIdentity.ProductFamily:=AProductFamily;
     AIdentity.ProductName:=AProductName;
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_FIND, mtData_Var,
                                                  AIdentity, sizeof(TW_IDENTITY), recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Source_Twain.IPC_OpenDevice(AManufacturer, AProductFamily, AProductName: TW_STR32): Boolean;
var
   recSize:Longint;
   AIdentity:TW_IDENTITY;
   recBuf:Boolean;
   resType:TMessageType;
   aUserInterface:TW_USERINTERFACE;

begin
  Result :=False;
  try
     aUserInterface.ShowUI:=False;
     aUserInterface.ModalUI:=False;
     aUserInterface.hParent:=Application.ActiveFormHandle;

     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_USERINTERFACE, mtData_Var,
                                                  aUserInterface, SizeOf(TW_USERINTERFACE), recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

     AIdentity.Manufacturer:=AManufacturer;
     AIdentity.ProductFamily:=AProductFamily;
     AIdentity.ProductName:=AProductName;
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_OPEN, mtData_Var,
                                                  AIdentity, SizeOf(TW_IDENTITY), recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Source_Twain.IPC_ParamsSet: Boolean;
var
   recSize:Integer;
   resType:TMessageType;
   res:Boolean;

begin
  Result:=False;
  if (rParams = nil) then exit;

  with (rParams as TDigIt_Source_Twain_Params) do
  begin
    recSize :=SizeOf(rTwainParams);
    resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_SET, mtData_Var,
                                          rTwainParams, recSize, res, recSize);
    Result := (resType = mtData_Integer) and (res = True);
  end;
end;

function TDigIt_Source_Twain.IPC_CapabilitiesGet: Boolean;
var
   recStream:TMemoryStream=nil;
   recSize, i:Integer;
   resType:TMessageType;
   curBufExtended, recBufExtended:PSingle;
   curBufInteger, recBufInteger:PInteger;

begin
  Result:= False;

  FillChar(TwainCap, Sizeof(TwainCap), 0);
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_CAPABILITIES_GET, mtData_Null, i, 0, recStream, recSize);
  if (resType=mtData_Stream) and (recStream<>nil) then
  try
    recStream.Position:=0;
    {$ifopt D+}
    recSize :=Sizeof(TwainCap.ResolutionArray);
    recSize :=Sizeof(TwainCap);
    recSize :=Sizeof(Single);
    {$endif}
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

  TwainCapGetted:= Result;
end;

function TDigIt_Source_Twain.IPC_Preview(APath: String): Integer;
var
   recSize: Longint;
   recBuf: Integer;
   resType: TMessageType;

begin
  Result :=0;
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PREVIEW, mtData_String, APath, 0, recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Source_Twain.IPC_Take(APath: String): Integer;
var
   recSize: Longint;
   recBuf: Integer;
   resType: TMessageType;

begin
  Result :=0;
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_TAKE, mtData_String, APath, 0, recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Source_Twain.GetTwainSource(ALoad: Boolean): TTwainSource;
begin
  Result:= Twain.SelectedSource;

  if (rParams = nil) then exit;

  with (rParams as TDigIt_Source_Twain_Params) do
  begin
    { #note 10 -oMaxM : For some reason Twain change the device order, so we MUST check if is our Selected Device }
    if (Result = nil) or (DeviceInfoDifferent(rScannerInfo, Result.SourceIdentity^))
    then Result:= Twain.SelectSource(rScannerInfo.Manufacturer,
                                     rScannerInfo.ProductFamily,
                                     rScannerInfo.ProductName, ALoad)
    else if ALoad then Result.Loaded:= True;
  end;

  TwainSource:= Result;
end;

function TDigIt_Source_Twain.TwainProcessMessages(Sender: TObject; const Index: Integer): Boolean;
begin
  Application.ProcessMessages;
  Result:= True;
end;

procedure TDigIt_Source_Twain.TwainAcquireNative(Sender: TObject; const Index: Integer;
                                                nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  with TCustomDelphiTwain(Sender).Source[Index] do
  try
    if (Download_Count = 0)
    then WriteBitmapToFile(Download_Path+Download_FileName+Download_Ext, nativeHandle)
    else WriteBitmapToFile(Download_Path+Download_FileName+
                           '-'+IntToStr(Download_Count)+Download_Ext, nativeHandle);

    Cancel := False;
  except
  end;
end;

procedure TDigIt_Source_Twain.FreeCommsClient;
var
   recBuf:Longint;

begin
  if (rCommsClient<>nil) then
  begin
    //In Debug Stop the Process Manually
    {$ifopt D-}
      rCommsClient.SendMessage(MSG_TWAIN32_STOP, mtData_Null, recBuf);
    {$endif}
    rCommsClient.Free; rCommsClient:= Nil;
  end;
  ipcProcess.Free; ipcProcess:= Nil;
end;

constructor TDigIt_Source_Twain.Create;
begin
  inherited Create;

  rTwain:= nil;
  rCommsClient:= nil;
  ipcProcess:= nil;
  DownloadedFiles:= TDigIt_Source_Twain_Files.Create;
end;

destructor TDigIt_Source_Twain.Destroy;
begin
  if (rTwain<>nil) then rTwain.Free;

  TwainCap.BitDepthArray:= nil;
  TwainCap.ResolutionArray:= nil;

  FreeCommsClient;

  if (DownloadedFiles<>nil) then DownloadedFiles.Free;
  if (rParams <> nil) then rParams.Release;

  inherited Destroy;
end;

procedure TDigIt_Source_Twain.RefreshList(ASender:TTwainSelectSource);
begin
  //Load source manager and Enumerate Internal Devices
  Twain.SourceManagerLoaded :=False;
  Application.ProcessMessages; { #todo -oMaxM : Is really necessary? }
  Twain.SourceManagerLoaded :=True;
  countTwain_Source:=Twain.SourceCount;
  countIPC_Source :=IPC_GetDevicesList;
  ASender.FillList(ipcSourceList);
end;

function TDigIt_Source_Twain.GetParamsClass: TDigIt_Source_Common_ParamsClass;
begin
  Result:= TDigIt_Source_Twain_Params;
end;

function TDigIt_Source_Twain.GetName: String;
begin
  Result:= rsTwainName;
end;

function TDigIt_Source_Twain.Enabled: Boolean; stdcall;
begin
  Result:= rEnabled and
          ((GetTwainDirectory(TWAINLIBRARY_32)<>'') or (GetTwainDirectory(TWAINLIBRARY_64)<>''));
end;

function TDigIt_Source_Twain.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 5;
end;

function TDigIt_Source_Twain.Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;
var
   sessPath,
   curPath,
   curFile: String;
   capRet: TCapabilityRet;
   i: Integer;

begin
  Result:= inherited Take(takeAction, aDataType, aData);
  if (rParams = nil) then exit;

  with (rParams as TDigIt_Source_Twain_Params) do
  try
     DownloadedFiles.Clear;
     inc(countTakes);

     sessPath:= theBridge.Settings.Path(ID_Path_Session);
     TwainPath_Temp:= theBridge.Settings.Path(ID_Path_Session_Scan)+'twain'+DirectorySeparator;
     curPath:= TwainPath_Temp+IntToStr(countTakes)+DirectorySeparator;

     if rScannerInfo.FromAddList
     then begin
            if IPC_ParamsSet then
            begin
              if (takeAction = takeActPreview)
              then Result :=IPC_Preview(curPath)
              else Result :=IPC_Take(curPath);
            end;
          end
     else begin
            if (GetTwainSource(True) <> nil) then
            begin
              with rTwainParams do
              begin
                //Set Parameters, (after a capture the scanner reset it to default???)
                capRet :=TwainSource.SetPaperFeeding(PaperFeed);
                capRet :=TwainSource.SetDuplexEnabled(False);
                capRet :=TwainSource.SetPaperSize(PaperSize);
                capRet :=TwainSource.SetIPixelType(PixelType);

                if (takeAction = takeActPreview)
                then begin
                       if not(TwainCapGetted) then TwainCapGetted:= TwainSource.GetParamsCapabilities(TwainCap);

                       if TwainCapGetted
                       then begin
                              capRet :=TwainSource.SetIXResolution(TwainCap.ResolutionMin);
                              capRet :=TwainSource.SetIYResolution(TwainCap.ResolutionMin);
                            end
                       else begin
                              capRet :=TwainSource.SetIXResolution(100);
                              capRet :=TwainSource.SetIYResolution(100);
                            end;
                     end
                else begin
                       capRet :=TwainSource.SetIXResolution(Resolution);
                       capRet :=TwainSource.SetIYResolution(Resolution);
                     end;

                capRet :=TwainSource.SetContrast(Contrast);
                capRet :=TwainSource.SetBrightness(Brightness);
              end;

              TwainSource.SetIndicators(True);

              { #note 10 -oMaxM : Switched to ttmNative Mode (my office Scanner fail if paper=tpsNone and dpi>150) see Tests}
              TwainSource.TransferMode:=ttmNative;
              Twain.OnTwainAcquireNative:=@TwainAcquireNative;

              rUserInterface.ModalUI:= False;
              rUserInterface.ShowUI:= False;
              rUserInterface.hParent:= Application.ActiveFormHandle;

              Result:= TwainSource.Download(rUserInterface, curPath, TwainFileBase, '.bmp', tfBMP);
            end;
          end;

     if (Result > 0)
     then begin
            if (Result = 1 )
            then begin
                   aData:= StrNew(PChar(curPath+TwainFileBase+'.bmp'));
                   aDataType:= diDataType_FileName;
                 end
            else begin
                   (*
                   SetLength(DownloadedFiles.rList, Result);

                   DownloadedFiles.rList[0]:= curPath+TwainFileBase+'.bmp';

                   for i:=1 to Result-1 do
                      DownloadedFiles.rList[i]:= curPath+TwainFileBase+'-'+IntToStr(i)+'.bmp';
                   *)
                   DownloadedFiles.Add(curPath+TwainFileBase+'.bmp');
                   for i:=1 to Result-1 do
                      DownloadedFiles.Add(curPath+TwainFileBase+'-'+IntToStr(i)+'.bmp');

                   aData:= DownloadedFiles as IDigIt_ArrayR_PChars;
                   aDataType:= diDataType_FileNameArray;
                 end;
          end
     else begin
            DeleteDirectory(curPath, False);
            dec(countTakes);
          end;

     Application.BringToFront;

  finally
  end;
end;

procedure TDigIt_Source_Twain.Clear; stdcall;
var
   i: Integer;

begin
  {$ifopt D-}
  for i:=0 to countTakes do
    DeleteDirectory(TwainPath_Temp+IntToStr(i)+DirectorySeparator, False);
  {$endif}

  countTakes:= -1;
  DownloadedFiles.Clear;
end;

initialization
  try
     TwainPath_Temp:= theBridge.Settings.Path(ID_Path_Session_Scan)+'twain'+DirectorySeparator;

     Source_Twain:= TDigIt_Source_Twain.Create;
     theBridge.Sources.Register(DigIt_Source_Twain_Name, Source_Twain);

  except
  end;

end.

