(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   W.I.A. Source                                                            **
*******************************************************************************)

unit DigIt_Source_WIA;

{$mode ObjFPC}{$H+}

interface

uses 
  Windows, Classes, SysUtils,
  MM_OpenArrayList, MM_Interface_Progress,
  WIA, WiaDef, WIA_LH, WIA_PaperSizes, WIA_SettingsForm,
  Digit_Bridge_Intf, DigIt_Source_Common;

const
  DigIt_Source_WIA_Name = 'WIA Device';
  WIA_TakeFileName = 'wia_take';

resourcestring
  rsWIAName = 'WIA Device';
  rsWIAAcquiring = 'Acquiring Page %d';
  rsWIAAcquiringTitle = 'Acquiring from %s';
  rsWIAAcquiringTotal = 'Acquiring from %s at %d dpi';
  rsWIAAcquired = 'Acquired %d Pages';
  rsWIADevItemSel = 'Device found '#13#10'%s %s'#13#10'but Item %s not...'#13#10'Select the First Item %s ?';
  rsWIAExcConnect = 'Error Connecting Device';
  rsWIAExcNotFound = 'Device not found...';
  rsWIACancelling = 'Cancelling acquisition';

type
  { TDigIt_Source_WIA_Params }

  TDigIt_Source_WIA_Params  = class(TDigIt_Source_Common_Params)
  protected
    DeviceID,
    DeviceName,
    DeviceManufacturer,
    DeviceItemName: String;
    DeviceItemIndex: Integer;
    DeviceItem: PWiaItem;
    WIACap: TWIAParamsCapabilities;
    WIAParams: TArrayWIAParams;
    ResMin,
    ResMax: Integer;

    function GetSummary: String; override;

  public
    function GetFromUser: Boolean; stdcall; override;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; override;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; override;

    function Select: Boolean; stdcall; override;

    constructor Create(AOwner: TDigIt_Source_Common); override;

    procedure SelectSourceItem(ASource: TWIADevice; AIndex: Integer);
  end;

  { TDigIt_Source_WIA_Files}

  TDigIt_Source_WIA_Files = class(TOpenArrayString, IDigIt_ArrayR_PChars)
    function Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;
  end;

  { TDigIt_Source_WIA }

  TDigIt_Source_WIA = class(TDigIt_Source_Common, IDigIt_Source_Items, IMM_ProgressCallback)
  private
    rWIA: TWIAManager;
    rWIASource: TWIADevice;
    DownloadedFiles: TDigIt_Source_WIA_Files;
    UserCancel: Boolean;
    Progress: IMM_Progress;

    function getWIA: TWIAManager;

    function DeviceTransferEvent(AWiaManager: TWIAManager; AWiaDevice: TWIADevice;
                                 lFlags: LONG; pWiaTransferParams: PWiaTransferParams): Boolean;

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

    //IDigIt_Source_Items Implementation
    function GetCount: DWord; stdcall;
    function Get(const AIndex: DWord; out aData: PChar): Boolean; stdcall;
    function RefreshList: DWord; stdcall;
    function Select(aIndex: Integer): Boolean; stdcall;
    function Selected(aIndex: Integer): Boolean; stdcall;

    //IDigIt_ProgressCallback Implementation
    procedure ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses Controls, Forms, Dialogs, FileUtil, BGRABitmapTypes, Laz2_XMLCfg,
     Digit_Types, Digit_Bridge_Impl;

var
   WIAPath_Temp: String;
   Source_WIA: TDigIt_Source_WIA = nil;

{ TDigIt_Source_WIA_Params }

function TDigIt_Source_WIA_Params.GetSummary: String;
begin
  Result:= '';

  if (DeviceName <> '') then
  begin
    Result:= DeviceName;
    if (DeviceItemName <> '') then Result:= Result+' ('+DeviceItemName+')';
  end;
end;

function TDigIt_Source_WIA_Params.GetFromUser: Boolean; stdcall;
var
  newItemIndex: Integer;
  initPar: TInitialItemValues;

begin
  Result :=False;

  with TDigIt_Source_WIA(rOwner) do
  if (getWIA <> nil) then
  try
     if (rWIASource <> nil) then
     begin
       newItemIndex:= rWIASource.SelectedItemIndex;

       //If Device is different start with Default Values  { #note 5 -oMaxM : Load Params from others devices in XML? }
       if (DeviceID <> rWIASource.ID)
       then initPar:= initDefault
       else initPar:= initParams;

       //Select Scanner Item and Settings to use
       Result:= TWIASettingsSource.Execute(rWIASource, newItemIndex, initPar, WIAParams);
       if Result then SelectSourceItem(rWIASource, newItemIndex);
     end
     else raise Exception.Create(rsWIAExcConnect);

  finally
  end;
end;

function TDigIt_Source_WIA_Params.Select: Boolean; stdcall;
var
   curSource: TWIADevice;
   curItem: PWIAItem;
   curItemName: String;
   aIndex: Integer;

begin
  Result:= False;

  with TDigIt_Source_WIA(rOwner) do
  if (getWIA <> nil) then
  try
     rWia.EnumAll:= False;
     aIndex:= -1;

     if (DeviceID <> '') or ((DeviceManufacturer <> '') and (DeviceName <> '')) then
     repeat
       {$ifopt D-}theBridge.Cursor(crHourGlass);{$endif}
       Application.ProcessMessages;

       rWia.RefreshDeviceList;

       //Try to Open searching by ID and ItemName
       rWIA.SelectDeviceItem(DeviceID, DeviceItemName, curSource, aIndex);
       if (curSource = nil)
       then begin
              //Device not founded, search with DeviceName/DeviceManufacturer
              aIndex:= rWia.FindDevice(DeviceName, DeviceManufacturer);
              if (aIndex >= 0) then
              begin
                //Device found, try to connect
                curSource:= rWia.Devices[aIndex];
                if (curSource <> nil)
                then begin
                       //Device connected, search for Item
                       if curSource.SelectItem(DeviceItemName)
                       then aIndex:= curSource.SelectedItemIndex
                       else begin
                              //Item not founded, ask the user what to do
                              aIndex:= -1;
                              curItem:= curSource.Items[0];
                              if (curItem <> nil) then
                              Case theBridge.MessageDlg('DigIt WIA',
                                              Format(rsWIADevItemSel, [DeviceName, DeviceManufacturer, DeviceItemName, curItem^.Name]),
                                              mtConfirmation, [mbYes, mbRetry, mbAbort], 0) of
                              mrYes: begin curSource.SelectedItemIndex:= 0; aIndex:= curSource.SelectedItemIndex; end;
                              mrAbort: break;
                              end;
                            end;
                     end
                else begin
                       //We have some Error connecting Device ask the user what to do
                       aIndex:= -1;
                       if (theBridge.MessageDlg('DigIt WIA', rsWIAExcConnect+#13#10+DeviceName+#13#10+DeviceManufacturer,
                                      mtError, [mbRetry, mbAbort], 0)=mrAbort)
                       then break;
                     end;
              end
              else if (theBridge.MessageDlg('DigIt WIA', rsWIAExcNotFound+#13#10+DeviceName+#13#10+DeviceManufacturer,
                       mtError, [mbRetry, mbAbort], 0)=mrAbort)
                   then break;
            end
       else begin
              //Device found and connected
              if (aIndex = -1) then
              begin
                //We have found the Device but not the Item, ask the user what to do
                curItem:= curSource.Items[0];
                if (curItem <> nil) then
                Case theBridge.MessageDlg('DigIt WIA',
                                Format(rsWIADevItemSel, [DeviceName, DeviceManufacturer, DeviceItemName, curItem^.Name]),
                                mtConfirmation, [mbYes, mbRetry, mbAbort], 0) of
                mrYes: begin curSource.SelectedItemIndex:= 0; aIndex:= curSource.SelectedItemIndex; end;
                mrAbort: break;
                end;
              end;
            end;
    until (aIndex > -1);

    theBridge.Cursor(crDefault);

    if (aIndex = -1)
    then Result:= Select(-1) and GetFromUser //User has selected Abort, Get Another Device from List and it's Params
    else begin
           SelectSourceItem(curSource, aIndex);
           Result:= (curSource <> nil);
         end;

  finally
  end;
end;

function TDigIt_Source_WIA_Params.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;
   i,
   iCount: Integer;
   curPath,
   curItemPath: String;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);

     DeviceID:= XMLWork.GetValue(xml_RootPath+'/ID', '');
     DeviceItemName:= XMLWork.GetValue(xml_RootPath+'/Item', '');
     DeviceManufacturer:= XMLWork.GetValue(xml_RootPath+'/Manufacturer', '');
     DeviceName:= XMLWork.GetValue(xml_RootPath+'/ProductName', '');
     rOwner.countTakes:= XMLWork.GetValue(xml_RootPath+'/CountTakes', 0);

     curPath:= xml_RootPath+'/WIAParams/';
     iCount:= XMLWork.GetValue(curPath+'Count', 0);

     WIAParams:= nil; //Avoid possible data overlaps by eliminating any existing array
     SetLength(WIAParams, iCount);

     for i:=0 to iCount-1 do
     with (WIAParams[i]) do
     begin
       curItemPath :=curPath+'Item' + IntToStr(i)+'/';

       //Set Default Values
       PaperType:= wptMAX;
       Rotation:= wrPortrait;
       HAlign:= waHLeft;
       VAlign:= waVTop;
       DataType:= wdtCOLOR;
       DocHandling:= [];

       NativeUI:= XMLWork.GetValue(curItemPath+'NativeUI', False);
       XMLWork.GetValue(curItemPath+'PaperType', PaperType, TypeInfo(TWIAPaperType));
       PaperW:= XMLWork.GetValue(curItemPath+'PaperW', 0);
       PaperH:= XMLWork.GetValue(curItemPath+'PaperH', 0);
       XMLWork.GetValue(curItemPath+'Rotation', Rotation, TypeInfo(TWIARotation));
       XMLWork.GetValue(curItemPath+'HAlign', HAlign, TypeInfo(TWIAAlignHorizontal));
       XMLWork.GetValue(curItemPath+'VAlign', VAlign, TypeInfo(TWIAAlignVertical));
       Resolution:= XMLWork.GetValue(curItemPath+'Resolution', 100);
       Contrast:= XMLWork.GetValue(curItemPath+'Contrast', 0);
       Brightness:= XMLWork.GetValue(curItemPath+'Brightness', 0);
       XMLWork.GetValue(curItemPath+'DataType', DataType, TypeInfo(TWIADataType));
       XMLWork.GetValue(curItemPath+'DocHandling', DocHandling, TypeInfo(TWIADocumentHandlingSet));
     end;

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_WIA_Params.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;
   i: Integer;
   curPath,
   curItemPath: String;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);

     XMLWork.SetValue(xml_RootPath+'/ID', DeviceID);
     XMLWork.SetValue(xml_RootPath+'/Item', DeviceItemName);
     XMLWork.SetValue(xml_RootPath+'/Manufacturer', DeviceManufacturer);
     XMLWork.SetValue(xml_RootPath+'/ProductName', DeviceName);
     XMLWork.SetValue(xml_RootPath+'/CountTakes', rOwner.countTakes);

     curPath:= xml_RootPath+'/WIAParams/';
     XMLWork.DeletePath(curPath);
     XMLWork.SetValue(curPath+'Count', Length(WIAParams));

     for i:=0 to Length(WIAParams)-1 do
     with (WIAParams[i]) do
     begin
       curItemPath :=curPath+'Item' + IntToStr(i)+'/';

       XMLWork.SetValue(curItemPath+'NativeUI', NativeUI);
       XMLWork.SetValue(curItemPath+'PaperType', PaperType, TypeInfo(TWIAPaperType));
       XMLWork.SetValue(curItemPath+'PaperW', PaperW);
       XMLWork.SetValue(curItemPath+'PaperH', PaperH);
       XMLWork.SetValue(curItemPath+'Rotation', Rotation, TypeInfo(TWIARotation));
       XMLWork.SetValue(curItemPath+'HAlign', HAlign, TypeInfo(TWIAAlignHorizontal));
       XMLWork.SetValue(curItemPath+'VAlign', VAlign, TypeInfo(TWIAAlignVertical));
       XMLWork.SetValue(curItemPath+'Resolution', Resolution);
       XMLWork.SetValue(curItemPath+'Contrast', Contrast);
       XMLWork.SetValue(curItemPath+'Brightness', Brightness);
       XMLWork.SetValue(curItemPath+'DataType', DataType, TypeInfo(TWIADataType));
       XMLWork.SetValue(curItemPath+'DocHandling', DocHandling, TypeInfo(TWIADocumentHandlingSet));
     end;

     XMLWork.Flush;

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

constructor TDigIt_Source_WIA_Params.Create(AOwner: TDigIt_Source_Common);
begin
  inherited Create(AOwner);

  DeviceID:= '';
  DeviceItemIndex:= -1;
end;

procedure TDigIt_Source_WIA_Params.SelectSourceItem(ASource: TWIADevice; AIndex: Integer);
begin
  with TDigIt_Source_WIA(rOwner) do
  if (ASource <> nil)
  then begin
         rWIASource:= ASource;
         rWIASource.SelectedItemIndex:= AIndex;
         DeviceID:= rWIASource.ID;
         DeviceItemIndex:= AIndex;
         DeviceItem:= rWIASource.SelectedItem;

         if (DeviceItem <> nil)
         then DeviceItemName:= DeviceItem^.Name
         else DeviceItemName:= '';

         DeviceManufacturer:= rWIASource.Manufacturer;
         DeviceName:= rWIASource.Name;
         rWIASource.GetResolutionsLimit(ResMin, ResMax);
       end
  else begin
         DeviceID:= '';
         DeviceItemIndex:= -1;
         DeviceItem:= nil;
         DeviceItemName:= '';
         DeviceManufacturer:= '';
         DeviceName:= '';
         ResMin:= 0;
         ResMax:= 0;
       end;
end;

{ TDigIt_Source_WIA_Files }

function TDigIt_Source_WIA_Files.Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;
begin
  aData:= nil;
  try
     aData:= StrNew(PChar(GetByIndex(aIndex)));
     Result:= True;
  except
     Result:= False;
  end;
end;

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
  Result:= not(UserCancel);

  if Result and (pWiaTransferParams <> nil) then
  Case pWiaTransferParams^.lMessage of
  WIA_TRANSFER_MSG_STATUS: begin
      if (Progress <> nil) then
      begin
        Progress.SetCurrentCaption(PChar(Format(rsWIAAcquiring, [AWiaDevice.Download_Count+1])));
        Progress.SetCurrentValue(pWiaTransferParams^.lPercentComplete);
      end;
  end;
  WIA_TRANSFER_MSG_END_OF_STREAM: begin
      if (Progress <> nil) then
      begin
        Progress.SetCurrentCaption(PChar(Format(rsWIAAcquired, [AWiaDevice.Download_Count])));
        Progress.SetCurrentValue(pWiaTransferParams^.lPercentComplete);
      end;
  end;
  WIA_TRANSFER_MSG_END_OF_TRANSFER: begin
      if (Progress <> nil) then
      begin
        Progress.SetTotal(0, 100, 100, False);
      end;
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

  Application.ProcessMessages;
end;

function TDigIt_Source_WIA.GetName: String;
begin
  Result:= rsWIAName;
end;

constructor TDigIt_Source_WIA.Create;
begin
  inherited Create;

  rWIA:= nil;
  rWIASource:= nil;
  DownloadedFiles:= TDigIt_Source_WIA_Files.Create;
end;

destructor TDigIt_Source_WIA.Destroy;
begin
  if (rWIA <> nil) then rWIA.Free;
  if (DownloadedFiles <> nil) then DownloadedFiles.Free;

  inherited Destroy;
end;

function TDigIt_Source_WIA.GetParamsClass: TDigIt_Source_Common_ParamsClass;
begin
  Result:= TDigIt_Source_WIA_Params;
end;

function TDigIt_Source_WIA.Enabled: Boolean; stdcall;
begin
  Result:= rEnabled and (getWIA <> nil) and (rWIA.DevMgrIntf <> nil);
end;

function TDigIt_Source_WIA.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 5;
end;

function TDigIt_Source_WIA.Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;
var
   aExt,
   sessPath,
   curPath: String;
   aFormat: TWIAImageFormat;
   capRet: Boolean;
   curParams: TWIAParams;

begin
  Result:= inherited Take(takeAction, aDataType, aData);
  if (rParams = nil) then exit;

  with (rParams as TDigIt_Source_WIA_Params) do
  try
     DownloadedFiles.Clear;
     inc(countTakes);

     sessPath:= theBridge.Settings.Path(ID_Path_Session);
     WIAPath_Temp:= theBridge.Settings.Path(ID_Path_Session_Scan)+'wia'+DirectorySeparator;
     curPath:= WIAPath_Temp+IntToStr(countTakes)+DirectorySeparator;

     if WIAParams[DeviceItemIndex].NativeUI
     then begin
            Result:= rWiaSource.DownloadNativeUI(Application.ActiveFormHandle, False,
                                        curPath, WIA_TakeFileName, DownloadedFiles.rList);
          end
     else begin
            UserCancel:= False;

            curParams:= WIAParams[DeviceItemIndex];

            if (takeAction = takeActPreview)
            then begin
                   //Take only 1 page
 { #todo 10 -oMaxM : When switching from pages=1 to pages=all my Brother scanner returns an image
              shrunk by a factor N within the correct sized page.
              N= (Resolution when pages=0) / (Resolution when pages=1). I'm stumped. }
 //                  capRet:= rWIASource.SetPages(1); //ignore Result

                   //Set minimum Resolution
                   curParams.Resolution:= ResMin;
                 end
            else capRet:= rWIASource.SetPages(0); //all Pages

            Progress:= theBridge.Progress;
            if (Progress <> nil) then
            begin
              Progress.SetTotalLabel(PChar(Format(rsWIAAcquiringTotal, [DeviceItemName, curParams.Resolution])));
              Progress.SetTotal(0, 100, 0, True);
              Progress.SetCurrentCaption(nil);
              Progress.SetCurrent(0, 100, 0, False);
              Progress.SetEventCallBack(Self as IMM_ProgressCallback);
              Progress.Show(PChar(Format(rsWIAAcquiringTitle, [DeviceName])));
            end;

            rWIASource.SetParams(curParams);

            { #todo 5 -oMaxM : Move To Download? }
            if WIAParams[DeviceItemIndex].DataType in [wdtRAW_RGB..wdtRAW_CMYK]
            then capRet:= rWIASource.SetImageFormat(wifRAW)
            else capRet:= rWIASource.SetImageFormat(wifBMP);
            //if not(capRet) then raise Exception.Create('SetImageFormat');

            if WIAParams[DeviceItemIndex].DataType in [wdtRAW_RGB..wdtRAW_CMYK]
            then begin
                   aFormat:= wifRAW;
                   aExt:= '.raw'
                 end
            else begin
                   aFormat:= wifBMP;
                   aExt:= '.bmp';
                 end;

            Result:= rWIASource.Download(curPath, WIA_TakeFileName, aExt,
                                         aFormat, DownloadedFiles.rList);
          end;

     if (Result > 0)
     then begin
            if (Result = 1 )
            then begin
                   aData:= StrNew(PChar(DownloadedFiles[0]));
                   aDataType:= diDataType_FileName;
                 end
            else begin
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
    if (Progress <> nil) then Progress.Hide;
  end;
end;

procedure TDigIt_Source_WIA.Clear; stdcall;
var
   i: Integer;

begin
  {$ifopt D-}
  for i:=0 to countTakes do
    DeleteDirectory(WIAPath_Temp+IntToStr(i)+DirectorySeparator, False);
  {$endif}

  countTakes:= -1;
  DownloadedFiles.Clear;
end;

function TDigIt_Source_WIA.GetCount: DWord; stdcall;
begin
  Result:= 0;
  if (getWIA <> nil) then
  try
     rWia.EnumAll:= False;
     Result:= rWia.DevicesCount;

  except
    Result:= 0;
  end;
end;

function TDigIt_Source_WIA.Get(const AIndex: DWord; out aData: PChar): Boolean; stdcall;
var
   curDevice: TWIADevice;

begin
  Result:= False;
  aData:= nil;

  if (getWIA <> nil) then
  try
     curDevice:= rWia.Devices[AIndex];
     Result:= (curDevice <> nil);

     if Result then
     begin
       aData:= StrNew(PChar(curDevice.Manufacturer+' '+curDevice.Name));
     end;

  except
    Result:= False;
    aData:= nil;
  end;
end;

function TDigIt_Source_WIA.RefreshList: DWord; stdcall;
begin
  Result:= 0;
  if (getWIA <> nil) then
  try
     rWia.EnumAll:= False;
     rWia.RefreshDeviceList;
     Result:= rWia.DevicesCount;

  except
    Result:= 0;
  end;
end;

function TDigIt_Source_WIA.Select(aIndex: Integer): Boolean; stdcall;
var
   curDevice: TWIADevice;

begin
  Result:= False;

  if (getWIA <> nil) then
  try
     curDevice:= rWia.Devices[aIndex];
     Result:= (curDevice <> nil);

     //Tell User using Select Device Dialog
     if not(Result) then
     begin
       rWia.EnumAll:= False;

       aIndex:= rWia.SelectDeviceDialog;
       Result:= (aIndex > -1);
     end;

     if Result then rWia.SelectedDeviceIndex:= aIndex; //Select the Device
     curDevice:= rWia.SelectedDevice;

     Result:= (curDevice <> nil);
     if Result and (rParams <> nil)
     then (rParams as TDigIt_Source_WIA_Params).SelectSourceItem(curDevice, curDevice.SelectedItemIndex);

  except
    Result:= False;
  end;
end;

function TDigIt_Source_WIA.Selected(aIndex: Integer): Boolean; stdcall;
begin
  Result:= False;

  if (getWIA <> nil) then
  try
     Result:= (rWIASource <> nil) and (rWia.Devices[aIndex].ID = rWIASource.ID);

  except
    Result:= False;
  end;
end;

procedure TDigIt_Source_WIA.ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;
begin
  UserCancel:= True;
  if (Progress<>nil) then Progress.SetCurrentCaption(PChar(rsWIACancelling));
end;

initialization
  try
     WIAPath_Temp:= theBridge.Settings.Path(ID_Path_Session_Scan)+'wia'+DirectorySeparator;

     Source_WIA:= TDigIt_Source_WIA.Create;
     theBridge.Sources.Register(DigIt_Source_WIA_Name, Source_WIA);

  except
  end;

end.

