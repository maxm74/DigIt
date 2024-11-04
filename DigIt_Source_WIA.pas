(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   W.I.A. Source                                                             **
*******************************************************************************)

unit DigIt_Source_WIA;

{$mode ObjFPC}{$H+}

interface

uses 
  Windows, Classes, SysUtils,
  WIA, WiaDef, WIA_LH, WIA_SettingsForm,
  Digit_Bridge_Intf;

const
  DigIt_Source_WIA_Name = 'WIA Device';
  WIA_TakeFileName = 'wia_take.bmp';

resourcestring
  DigIt_Source_WIA_NameL = 'WIA Device';

type

  { TDigIt_Source_WIA }
  TDigIt_Source_WIA = class(TNoRefCountObject, IDigIt_Params, IDigIt_Source)
  private
    rDeviceID,
    rDeviceItem: String;
    rParams: TWIAParams;
    rWIA: TWIAManager;
    AcquireFileName: String;

    function getWIA: TWIAManager;

    function DeviceTransferEvent(AWiaManager: TWIAManager; AWiaDevice: TWIADevice;
                                 lFlags: LONG; pWiaTransferParams: PWiaTransferParams): Boolean;

(*
    function ParamsGet(var WIACap:TWIAParamsCapabilities):Boolean;

    procedure WIAAcquireNative(Sender: TObject; const Index: Integer;
                                 nativeHandle: TW_UINT32; var Cancel: Boolean);
    function internalTake(isPreview:Boolean; var AFileName: String): DWord;
*)
    property WIA: TWIAManager read getWIA;

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
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

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
   Source_WIA : TDigIt_Source_WIA = nil;


{ TDigIt_Source_WIA }

function TDigIt_Source_WIA.getWIA: TWIAManager;
begin
  //Create WIA
  if (rWIA = nil) then
  begin
    rWIA := TWIAManager.Create;
    rWia.OnAfterDeviceTransfer:= @DeviceTransferEvent;
  end;

  Result :=rWIA;
end;

function TDigIt_Source_WIA.DeviceTransferEvent(AWiaManager: TWIAManager; AWiaDevice: TWIADevice; lFlags: LONG;
                                               pWiaTransferParams: PWiaTransferParams): Boolean;
begin
  Result:= True;

  if (pWiaTransferParams <> nil) then
  Case pWiaTransferParams^.lMessage of
  WIA_TRANSFER_MSG_STATUS: begin
//    progressBar.Position:= pWiaTransferParams^.lPercentComplete;
//    lbProgress.Caption:= 'Downloading '+AWiaDevice.Download_FileName+AWiaDevice.Download_Ext+'  : '+IntToStr(AWiaDevice.Download_Count)+' file';
  end;
  WIA_TRANSFER_MSG_END_OF_STREAM: begin
//    lbProgress.Caption:= 'Downloaded '+AWiaDevice.Download_FileName+AWiaDevice.Download_Ext+'  : '+IntToStr(AWiaDevice.Download_Count)+' file';
  end;
  WIA_TRANSFER_MSG_END_OF_TRANSFER: begin
  end;
  WIA_TRANSFER_MSG_DEVICE_STATUS: begin
  end;
  WIA_TRANSFER_MSG_NEW_PAGE: begin
    //lbProgress.Caption:= AWiaDevice.Download_FileName+AWiaDevice.Download_Ext+'  : '+IntToStr(AWiaDevice.Download_Count+1);
  end
  else begin
//    Memo2.Lines.Add('WIA_TRANSFER_MSG_'+IntToHex(pWiaTransferParams^.lMessage)+' : '+IntToStr(pWiaTransferParams^.lPercentComplete)+'% err='+IntToHex(pWiaTransferParams^.hrErrorStatus));
  end;
  end;
end;

(*
function TDigIt_Source_WIA.ParamsGet(var WIACap: TWIAParamsCapabilities): Boolean;
var
   WIASource: TWIASource;
   capRet:TCapabilityRet;
   bitCurrent: Integer;
   paperCurrent: TWIAPaperSize;
   pixelCurrent:TWIAPixelType;
   resolutionCurrent:Single;

begin
  Result:= False;
  WIASource:= GetWIASource(True);
  if (WIASource <> nil) then
  begin
    WIACap.PaperFeedingSet:=WIASource.GetPaperFeeding;
    capRet :=WIASource.GetPaperSizeSet(paperCurrent, WIACap.PaperSizeDefault, WIACap.PaperSizeSet);
    capRet :=WIASource.GetIBitDepth(bitCurrent, WIACap.BitDepthDefault, WIACap.BitDepthArray);
    WIACap.BitDepthArraySize :=Length(WIACap.BitDepthArray);
    capRet :=WIASource.GetIPixelType(pixelCurrent, WIACap.PixelTypeDefault, WIACap.PixelType);
    capRet :=WIASource.GetIXResolution(resolutionCurrent, WIACap.ResolutionDefault, WIACap.ResolutionArray);
    WIACap.ResolutionArraySize :=Length(WIACap.ResolutionArray);

    Result:= True;
  end;
end;

procedure TDigIt_Source_WIA.WIAAcquireNative(Sender: TObject; const Index: Integer;
                                                nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  WriteBitmapToFile(AcquireFileName, nativeHandle);
end;

function TDigIt_Source_WIA.internalTake(isPreview: Boolean; var AFileName: String): DWord;
var
   capRet:TCapabilityRet;
   resTake:Boolean;
   WIASource: TWIASource;

begin
  try
     Result:= 0;
   //  TFormAnimAcquiring.Execute; Application.ProcessMessages;

     try
        //Delete previous scanned file
        (*if FileExists(Path_Temp+WIA_TakeFileName)
        then*) DeleteFile(Path_Temp+WIA_TakeFileName);
     except
        //Sometimes FileExists will raise and Exception (?)
     end;

     AcquireFileName:= Path_Temp+WIA_TakeFileName;

     if rDeviceInfo.FromAddList
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
            WIASource:= GetWIASource(True);
            if (WIASource <> nil) then
            begin
//              WIASource.Loaded :=True;
              with rParams do
              begin
              //Set Parameters, (after a capture the scanner reset it to default???)
              capRet :=WIASource.SetPaperFeeding(PaperFeed);
              capRet :=WIASource.SetDuplexEnabled(False);
              capRet :=WIASource.SetPaperSize(PaperSize);
              capRet :=WIASource.SetIPixelType(PixelType);

              if isPreview
              then begin
                     capRet :=WIASource.SetIXResolution(75);
                     capRet :=WIASource.SetIYResolution(75);
                   end
              else begin
                     capRet :=WIASource.SetIXResolution(Resolution);
                     capRet :=WIASource.SetIYResolution(Resolution);
                   end;
              capRet :=WIASource.SetContrast(Contrast);
              capRet :=WIASource.SetBrightness(Brightness);
              end;

              WIASource.SetIndicators(True);

              { #note 10 -oMaxM : Switched to ttmNative Mode (my office Scanner fail if paper=tpsNone and dpi>150) see Tests}
              WIASource.TransferMode:=ttmNative; //ttmFile;
              //WIASource.SetupFileTransfer(AcquireFileName, tfBMP);
              WIA.OnWIAAcquireNative:=@WIAAcquireNative;
              WIASource.EnableSource(False, False, Application.ActiveFormHandle);
              //WIASource.EnableSource(rUserInterface); if in Future we want in the Options

              AFileName:= AcquireFileName;
              Result:= Length(AcquireFileName);
            end;
          end;

     Application.BringToFront;

  finally
   // FormAnimAcquiring.Free; FormAnimAcquiring:= Nil;
  end;
end;
*)

constructor TDigIt_Source_WIA.Create;
begin
  inherited Create;

  rWIA:= nil;
end;

destructor TDigIt_Source_WIA.Destroy;
begin
  if (rWIA<>nil) then rWIA.Free;

  try
     //Delete previous scanned file
     (*if FileExists(Path_Temp+WIA_TakeFileName)
     then*) DeleteFile(Path_Temp+WIA_TakeFileName);
  except
    //Strange Windows Error when closing the Application and deleting files
  end;

  inherited Destroy;
end;

function TDigIt_Source_WIA.GetFromUser: Boolean; stdcall;
var
  //newSelectedID: Integer;
  //newSelectedInfo: TWIADeviceInfo;
  WIACap: TWIAParamsCapabilities;
  useScannerDefault: Boolean;

begin
  Result :=False;
  (*
  try
     if WIA.SourceManagerLoaded then
     begin
       //Close Current Scanner if any
       if (WIA.SelectedSource <> nil)
       then WIA.SelectedSource.Loaded:= False;

       WIA.SourceManagerLoaded:=False;
     end;

     //Load source manager and Enumerate Internal Devices
     WIA.SourceManagerLoaded :=True;
     countWIA_Source:=WIA.SourceCount;

     //Get 32bit Devices
     countIPC_Source :=IPC_GetDevicesList;

     newSelectedInfo:= rDeviceInfo;
     if TWIASelectSource.Execute('DigIt', '(32 bit)', @RefreshList, WIA, ipcSourceList, newSelectedInfo) then
     begin
       useScannerDefault:= DeviceInfoDifferent(rDeviceInfo, newSelectedInfo);

       //if the selected device is a 32bit scanner
       if newSelectedInfo.FromAddList
       then Result:= IPC_OpenDevice(newSelectedInfo.Manufacturer,
                                    newSelectedInfo.ProductFamily,
                                    newSelectedInfo.ProductName)
       else Result:= (WIA.SelectSource(newSelectedInfo.Manufacturer,
                                         newSelectedInfo.ProductFamily,
                                         newSelectedInfo.ProductName, True) <> nil);

       //If Device is opened
       if Result then
       begin
           rDeviceInfo:= newSelectedInfo;
           if rDeviceInfo.FromAddList
           then IPC_ParamsGet(WIACap)
           else ParamsGet(WIACap);

           TWIASettingsSource.Execute(useScannerDefault, WIACap, rParams);
       end;
     end;

  finally
    WIASelectSource.Free; WIASelectSource:= Nil;
    WIASettingsSource.Free; WIASettingsSource:= Nil;
  end;
  *)
end;

function TDigIt_Source_WIA.Duplicate: IDigIt_Params; stdcall;
begin
  Result:= nil;
end;

function TDigIt_Source_WIA.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);
(*
     rDeviceInfo.FromAddList:= XMLWork.GetValue(xml_RootPath+'/IPC_Scanner', False);
     rDeviceInfo.Manufacturer:= XMLWork.GetValue(xml_RootPath+'/Manufacturer', '');
     rDeviceInfo.ProductFamily:= XMLWork.GetValue(xml_RootPath+'/ProductFamily', '');
     rDeviceInfo.ProductName:= XMLWork.GetValue(xml_RootPath+'/ProductName', '');

     //rParams.PaperFeed:= TWIAPaperFeeding(XMLWork.GetValue(xml_RootPath+'/PaperFeed', Integer(pfFlatbed)));
     //rParams.PaperSize:= TWIAPaperSize(XMLWork.GetValue(xml_RootPath+'/PaperSize', Integer(tpsNone)));
     //rParams.PixelType:= TWIAPixelType(XMLWork.GetValue(xml_RootPath+'/PixelType', Integer(tbdRgb)));
     XMLWork.GetValue(xml_RootPath+'/PaperFeed', rParams.PaperFeed, TypeInfo(TWIAPaperFeeding));
     XMLWork.GetValue(xml_RootPath+'/PaperSize', rParams.PaperSize, TypeInfo(TWIAPaperSize));
     XMLWork.GetValue(xml_RootPath+'/PixelType', rParams.PixelType, TypeInfo(TWIAPixelType));
     rParams.Resolution:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Resolution', '150'));
     rParams.Contrast:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Contrast', '0'));
     rParams.Brightness:= StrToFloat(XMLWork.GetValue(xml_RootPath+'/Brightness', '0'));
     rParams.BitDepth:= XMLWork.GetValue(xml_RootPath+'/BitDepth', 24);
*)
     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_WIA.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);
(*
     XMLWork.SetValue(xml_RootPath+'/IPC_Scanner', rDeviceInfo.FromAddList);
     XMLWork.SetValue(xml_RootPath+'/Manufacturer', rDeviceInfo.Manufacturer);
     XMLWork.SetValue(xml_RootPath+'/ProductFamily', rDeviceInfo.ProductFamily);
     XMLWork.SetValue(xml_RootPath+'/ProductName', rDeviceInfo.ProductName);

     XMLWork.SetValue(xml_RootPath+'/PaperFeed', rParams.PaperFeed, TypeInfo(TWIAPaperFeeding));
     XMLWork.SetValue(xml_RootPath+'/PaperSize', rParams.PaperSize, TypeInfo(TWIAPaperSize));
     XMLWork.SetValue(xml_RootPath+'/PixelType', rParams.PixelType, TypeInfo(TWIAPixelType));
     XMLWork.SetValue(xml_RootPath+'/Resolution', FloatToStr(rParams.Resolution));
     XMLWork.SetValue(xml_RootPath+'/Contrast', FloatToStr(rParams.Contrast));
     XMLWork.SetValue(xml_RootPath+'/Brightness', FloatToStr(rParams.Brightness));
     XMLWork.SetValue(xml_RootPath+'/BitDepth', rParams.BitDepth);

     XMLWork.Flush;
*)
     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_WIA.Summary(const ASummary: PChar): Integer; stdcall;
begin
  Result:= 0;
end;

function TDigIt_Source_WIA.OnSet: Boolean; stdcall;
var
   dlgRes: TModalResult;
//   WIASource: TWIASource;
   aIndex: Integer;

begin
  (*
  with rDeviceInfo do
  begin
    aIndex:= -1;
    if (Manufacturer<>'') and (ProductFamily<>'') and (ProductName<>'') then
    repeat
      //Try to Open searching by Name Until Opened or user select Abort
      if FromAddList
      then aIndex :=IPC_FindSource(Manufacturer, ProductFamily, ProductName)
      else begin
             WIA.SourceManagerLoaded :=False;
             Application.ProcessMessages;
             WIA.SourceManagerLoaded :=True;
             aIndex :=WIA.FindSource(Manufacturer, ProductFamily, ProductName);
           end;

      if (aIndex = -1)
      then begin
             if (MessageDlg('DigIt WIA', 'Device not found...'#13#10+
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
                  WIA.SourceManagerLoaded:= True;
                  WIASource:= GetWIASource(False);
                  Result:= (WIASource <> nil);
                  if Result then WIASource.Loaded:= True;
                end;
         end;
  end;
  *)
end;

function TDigIt_Source_WIA.Flags: DWord; stdcall;
begin
  Result:= DigIt_Source_TakeData_PICTUREFILE;
end;

function TDigIt_Source_WIA.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_WIA.Enabled(AEnabled: Boolean): Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_WIA.Release: Boolean; stdcall;
begin
  Free;
  Result:= True;
end;

function TDigIt_Source_WIA.Params: IDigIt_Params; stdcall;
begin
  Result:= Self;
end;

function TDigIt_Source_WIA.UI_Title(const AUI_Title: PChar): Integer; stdcall;
begin
  StrPCopy(AUI_Title, DigIt_Source_WIA_NameL);
  Result:= Length(AUI_Title);
end;

function TDigIt_Source_WIA.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 2;
end;

function TDigIt_Source_WIA.Take(takeAction: DigIt_Source_TakeAction; MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;
var
   AFileName: String;

begin
  (*
  Result:= internalTake((takeAction=takeActPreview), AFileName);

  StrPLCopy(PChar(AData), AFileName, MaxDataSize);
  *)
  Result:= Length(AFileName);
end;

initialization
  try
     Source_WIA:= TDigIt_Source_WIA.Create;
     theBridge.Sources.Register(DigIt_Source_WIA_Name, Source_WIA);
  except
  end;

end.

