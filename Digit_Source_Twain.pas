(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Source                                                             **
*******************************************************************************)

unit Digit_Source_Twain;

{$mode ObjFPC}{$H+}

interface

uses
  simpleipc, SyncIPC, Process, Classes, SysUtils,
  Twain, DelphiTwain, DelphiTwainTypes, DelphiTwainUtils,
  Digit_Bridge_Intf, Digit_Source_Twain_Types, DelphiTwain_SelectForm, DelphiTwain_SettingsForm;

const
  DigIt_Source_Twain_Name = 'Twain Device';
  Twain_TakeFileName = 'twain_take.bmp';

resourcestring
  DigIt_Source_Twain_NameL = 'Twain Device';

type
  { TDigIt_Source_Twain }
  TDigIt_Source_Twain = class(TNoRefCountObject, IDigIt_Params, IDigIt_Source)
  private
    rScannerInfo: TTwainDeviceInfo;
    rParams: TTwainParams;
    rTwain:TCustomDelphiTwain;

    ipcProcess:TProcess;
    rCommsClient:TSyncIPCClient;
    ipcSourceList:array of TW_IDENTITY;
    countTwain_Source,
    countIPC_Source: Integer;
    AcquireFileName: String;
    rEnabled: Boolean;

    function getCommsClient: TSyncIPCClient;
    function getTwain: TCustomDelphiTwain;

    function IPC_GetDevicesList:Integer;
    function IPC_FindSource(AManufacturer, AProductFamily, AProductName: TW_STR32): Integer;
    function IPC_OpenDevice(AManufacturer, AProductFamily, AProductName: TW_STR32):Boolean;
    function IPC_ParamsSet:Boolean;
    function IPC_ParamsGet(var TwainCap:TTwainParamsCapabilities):Boolean;
    function IPC_Preview(AFileName:String):Boolean;
    function IPC_Take(AFileName:String):Boolean;

    function GetTwainSource(Load: Boolean): TTwainSource;

    function ParamsGet(var TwainCap:TTwainParamsCapabilities):Boolean;

    procedure TwainAcquireNative(Sender: TObject; const Index: Integer;
                                 nativeHandle: TW_UINT32; var Cancel: Boolean);
    function internalTake(isPreview:Boolean; var AFileName: String): DWord;

    procedure FreeCommsClient;

    procedure RefreshList(ASender:TTwainSelectSource);

    property Twain: TCustomDelphiTwain read getTwain;
    property CommsClient:TSyncIPCClient read getCommsClient;

  public
    //IDigIt_Params Implementation
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Summary(const ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;

    //IDigIt_Source Implementation
    function Flags: DWord; stdcall;
    function Init: Boolean; stdcall;
    function Release: Boolean; stdcall;
    function Enabled: Boolean; stdcall;
    function setEnabled(AEnabled: Boolean): Boolean; stdcall;

    function Params: IDigIt_Params; stdcall;
    function UI_Title(const AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

     //Take a Picture and returns FileName
    function Take(takeAction: DigIt_Source_TakeAction; MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Controls, Forms, Dialogs, Digit_Types, BGRABitmapTypes, Laz2_XMLCfg, Digit_Bridge_Impl
 // ,DigIt_Form_AnimAcquiring
  ;

var
   Source_Twain : TDigIt_Source_Twain = nil;


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

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
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
  recSize :=SizeOf(rParams);
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_SET, mtData_Var,
               rParams, recSize, res, recSize);
  Result := (resType = mtData_Integer) and (res = True);
end;

function TDigIt_Source_Twain.IPC_ParamsGet(var TwainCap: TTwainParamsCapabilities): Boolean;
var
   recStream:TMemoryStream=nil;
   recSize, i:Integer;
   resType:TMessageType;
   curBufExtended, recBufExtended:PSingle;
   curBufInteger, recBufInteger:PInteger;

begin
  Result:=False;
  FillChar(TwainCap, Sizeof(TwainCap), 0);
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_GET, mtData_Null, i, 0, recStream, recSize);
  if (resType=mtData_Stream) and (recStream<>nil) then
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

function TDigIt_Source_Twain.IPC_Preview(AFileName: String): Boolean;
var
   recSize:Longint;
   recBuf:Boolean;
   resType:TMessageType;

begin
  Result :=False;
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PREVIEW, mtData_String, AFileName, 0, recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Source_Twain.IPC_Take(AFileName: String): Boolean;
var
   recSize:Longint;
   recBuf:Boolean;
   resType:TMessageType;

begin
  Result :=False;
  try
     resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_TAKE, mtData_String, AFileName, 0, recBuf, recSize);
     if (resType=mtData_Integer)
     then Result:=recBuf;

  except
  end;
end;

function TDigIt_Source_Twain.GetTwainSource(Load: Boolean): TTwainSource;
begin
  Result:= Twain.SelectedSource;
  { #note 10 -oMaxM : For some reason Twain change the device order, so we MUST check if is our Selected Device }
  if (Result = nil) or (DeviceInfoDifferent(rScannerInfo, Result.SourceIdentity^))
  then Result:= Twain.SelectSource(rScannerInfo.Manufacturer,
                                   rScannerInfo.ProductFamily,
                                   rScannerInfo.ProductName, Load)
  else if Load then Result.Loaded:= True;
end;

function TDigIt_Source_Twain.ParamsGet(var TwainCap: TTwainParamsCapabilities): Boolean;
var
   TwainSource: TTwainSource;
   capRet:TCapabilityRet;
   bitCurrent: Integer;
   paperCurrent: TTwainPaperSize;
   pixelCurrent:TTwainPixelType;
   resolutionCurrent:Single;

begin
  Result:= False;
  TwainSource:= GetTwainSource(True);
  if (TwainSource <> nil) then
  begin
    TwainCap.PaperFeedingSet:=TwainSource.GetPaperFeeding;
    capRet :=TwainSource.GetPaperSizeSet(paperCurrent, TwainCap.PaperSizeDefault, TwainCap.PaperSizeSet);
    capRet :=TwainSource.GetIBitDepth(bitCurrent, TwainCap.BitDepthDefault, TwainCap.BitDepthArray);
    TwainCap.BitDepthArraySize :=Length(TwainCap.BitDepthArray);
    capRet :=TwainSource.GetIPixelType(pixelCurrent, TwainCap.PixelTypeDefault, TwainCap.PixelType);
    capRet :=TwainSource.GetIXResolution(resolutionCurrent, TwainCap.ResolutionDefault, TwainCap.ResolutionArray);
    TwainCap.ResolutionArraySize :=Length(TwainCap.ResolutionArray);

    Result:= True;
  end;
end;

procedure TDigIt_Source_Twain.TwainAcquireNative(Sender: TObject; const Index: Integer;
                                                nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  WriteBitmapToFile(AcquireFileName, nativeHandle);
end;

function TDigIt_Source_Twain.internalTake(isPreview: Boolean; var AFileName: String): DWord;
var
   capRet:TCapabilityRet;
   resTake:Boolean;
   TwainSource: TTwainSource;

begin
  try
     Result:= 0;
   //  TFormAnimAcquiring.Execute; Application.ProcessMessages;

     try
        //Delete previous scanned file
        (*if FileExists(Path_Temp+Twain_TakeFileName)
        then*) DeleteFile(Path_Temp+Twain_TakeFileName);
     except
        //Sometimes FileExists will raise and Exception (?)
     end;

     AcquireFileName:= Path_Temp+Twain_TakeFileName;

     if rScannerInfo.FromAddList
     then begin
            resTake :=IPC_ParamsSet;

            if isPreview
            then resTake :=IPC_Preview(AcquireFileName)
            else resTake :=IPC_Take(AcquireFileName);

            if resTake then
            begin
              AFileName:= AcquireFileName;
              Result:= Length(AcquireFileName);
            end;
          end
     else begin
            TwainSource:= GetTwainSource(True);
            if (TwainSource <> nil) then
            begin
//              TwainSource.Loaded :=True;
              with rParams do
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

              TwainSource.SetIndicators(True);

              { #note 10 -oMaxM : Switched to ttmNative Mode (my office Scanner fail if paper=tpsNone and dpi>150) see Tests}
              TwainSource.TransferMode:=ttmNative; //ttmFile;
              //TwainSource.SetupFileTransfer(AcquireFileName, tfBMP);
              Twain.OnTwainAcquireNative:=@TwainAcquireNative;
              TwainSource.EnableSource(False, False, Application.ActiveFormHandle);
              //TwainSource.EnableSource(rUserInterface); if in Future we want in the Options

              AFileName:= AcquireFileName;
              Result:= Length(AcquireFileName);
            end;
          end;

     Application.BringToFront;

  finally
   // FormAnimAcquiring.Free; FormAnimAcquiring:= Nil;
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
  FillChar(rScannerInfo, Sizeof(rScannerInfo), 0);
  rEnabled:= True;
end;

destructor TDigIt_Source_Twain.Destroy;
begin
  if (rTwain<>nil) then rTwain.Free;
  FreeCommsClient;

  try
     //Delete previous scanned file
     (*if FileExists(Path_Temp+Twain_TakeFileName)
     then*) DeleteFile(Path_Temp+Twain_TakeFileName);
  except
    //Strange Windows Error when closing the Application and deleting files
  end;

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

function TDigIt_Source_Twain.GetFromUser: Boolean; stdcall;
var
  //newSelectedID: Integer;
  newSelectedInfo: TTwainDeviceInfo;
  TwainCap: TTwainParamsCapabilities;
  useScannerDefault: Boolean;

begin
  Result :=False;
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
           then IPC_ParamsGet(TwainCap)
           else ParamsGet(TwainCap);

           TTwainSettingsSource.Execute(useScannerDefault, TwainCap, rParams);
       end;
     end;

  finally
    TwainSelectSource.Free; TwainSelectSource:= Nil;
    TwainSettingsSource.Free; TwainSettingsSource:= Nil;
  end;
end;

function TDigIt_Source_Twain.Duplicate: IDigIt_Params; stdcall;
begin
  Result:= nil;
end;

function TDigIt_Source_Twain.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

     //rParams.PaperFeed:= TTwainPaperFeeding(XMLWork.GetValue(xml_RootPath+'/PaperFeed', Integer(pfFlatbed)));
     //rParams.PaperSize:= TTwainPaperSize(XMLWork.GetValue(xml_RootPath+'/PaperSize', Integer(tpsNone)));
     //rParams.PixelType:= TTwainPixelType(XMLWork.GetValue(xml_RootPath+'/PixelType', Integer(tbdRgb)));
     XMLWork.GetValue(xml_RootPath+'/PaperFeed', rParams.PaperFeed, TypeInfo(TTwainPaperFeeding));
     XMLWork.GetValue(xml_RootPath+'/PaperSize', rParams.PaperSize, TypeInfo(TTwainPaperSize));
     XMLWork.GetValue(xml_RootPath+'/PixelType', rParams.PixelType, TypeInfo(TTwainPixelType));
     rParams.Resolution:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Resolution', '150'));
     rParams.Contrast:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Contrast', '0'));
     rParams.Brightness:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Brightness', '0'));
     rParams.BitDepth:= XMLWork.GetValue(xml_RootPath+'/BitDepth', 24);

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_Twain.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

     XMLWork.SetValue(xml_RootPath+'/PaperFeed', rParams.PaperFeed, TypeInfo(TTwainPaperFeeding));
     XMLWork.SetValue(xml_RootPath+'/PaperSize', rParams.PaperSize, TypeInfo(TTwainPaperSize));
     XMLWork.SetValue(xml_RootPath+'/PixelType', rParams.PixelType, TypeInfo(TTwainPixelType));
     XMLWork.SetValue(xml_RootPath+'/Resolution', FloatToStr(rParams.Resolution));
     XMLWork.SetValue(xml_RootPath+'/Contrast', FloatToStr(rParams.Contrast));
     XMLWork.SetValue(xml_RootPath+'/Brightness', FloatToStr(rParams.Brightness));
     XMLWork.SetValue(xml_RootPath+'/BitDepth', rParams.BitDepth);

     XMLWork.Flush;

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_Twain.Summary(const ASummary: PChar): Integer; stdcall;
begin
  Result:= 0;
end;

function TDigIt_Source_Twain.OnSet: Boolean; stdcall;
var
   dlgRes: TModalResult;
   TwainSource: TTwainSource;
   aIndex: Integer;

begin
  with rScannerInfo do
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
             if (MessageDlg('DigIt Twain', 'Device not found...'#13#10+
                            ProductName+#13#10+Manufacturer, mtError, [mbRetry, mbAbort], 0)=mrAbort)
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

function TDigIt_Source_Twain.Flags: DWord; stdcall;
begin
  Result:= DigIt_Source_TakeData_PICTUREFILE;
end;

function TDigIt_Source_Twain.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Twain.Enabled: Boolean; stdcall;
begin
  Result:= False;
//  Result:= rEnabled and
//          ((GetTwainDirectory(TWAINLIBRARY_32)<>'') or (GetTwainDirectory(TWAINLIBRARY_64)<>''));
end;

function TDigIt_Source_Twain.setEnabled(AEnabled: Boolean): Boolean; stdcall;
begin
  rEnabled:= AEnabled;
  Result:= rEnabled;
end;

function TDigIt_Source_Twain.Release: Boolean; stdcall;
begin
  Free;
  Result:= True;
end;

function TDigIt_Source_Twain.Params: IDigIt_Params; stdcall;
begin
  Result:= Self;
end;

function TDigIt_Source_Twain.UI_Title(const AUI_Title: PChar): Integer; stdcall;
begin
  StrPCopy(AUI_Title, DigIt_Source_Twain_NameL);
  Result:= Length(AUI_Title);
end;

function TDigIt_Source_Twain.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 2;
end;

function TDigIt_Source_Twain.Take(takeAction: DigIt_Source_TakeAction; MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;
var
   AFileName: String;

begin
  Result:= internalTake((takeAction=takeActPreview), AFileName);

  StrPLCopy(PChar(AData), AFileName, MaxDataSize);
  Result:= Length(AFileName);
end;

initialization
  try
     Source_Twain:= TDigIt_Source_Twain.Create;
     theBridge.Sources.Register(DigIt_Source_Twain_Name, Source_Twain);
  except
  end;

end.

