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
  WIA, WiaDef, WIA_LH, WIA_PaperSizes, WIA_SettingsForm,
  Digit_Bridge_Intf;

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

  { TDigIt_Source_WIA }
  TDigIt_Source_WIA = class(TNoRefCountObject, IDigIt_Params, IDigIt_ROArray, IDigIt_Source, IDigIt_ProgressCallback)
  private
    rWIA: TWIAManager;
    rWIASource: TWIADevice;
    DeviceID,
    DeviceName,
    DeviceManufacturer,
    DeviceItemName: String;
    DeviceItemIndex: Integer;
    DeviceItem: PWiaItem;
    WIACap: TWIAParamsCapabilities;
    WIAParams: TArrayWIAParams;
    ResMin,
    ResMax,
    countTakes: Integer;
    DownloadedFiles: TStringArray;
    rEnabled,
    UserCancel: Boolean;
    Progress: IDigIt_Progress;

    function getWIA: TWIAManager;

    function DeviceTransferEvent(AWiaManager: TWIAManager; AWiaDevice: TWIADevice;
                                 lFlags: LONG; pWiaTransferParams: PWiaTransferParams): Boolean;

    procedure SelectSourceItem(ASource: TWIADevice; AIndex: Integer);

  public
    //IDigIt_Params Implementation
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Summary(const ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;

    //IDigIt_Source Implementation
    function Flags: TDigItInterfaceKind; stdcall;
    function Init: Boolean; stdcall;
    function Release: Boolean; stdcall;
    function Enabled: Boolean; stdcall;
    function setEnabled(AEnabled: Boolean): Boolean; stdcall;

    function Params: IDigIt_Params; stdcall;
    function UI_Title(out AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

     //Take a Picture and returns FileNames
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;

    procedure Clear; stdcall;

    //IDigIt_ROArray
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;

    //IDigIt_ProgressCallback
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

constructor TDigIt_Source_WIA.Create;
begin
  inherited Create;

  DeviceID:= '';
  DeviceItemIndex:= -1;
  rWIA:= nil;
  rWIASource:= nil;
  rEnabled:= True;
  countTakes:= -1;
  DownloadedFiles:= nil;
end;

destructor TDigIt_Source_WIA.Destroy;
begin
  if (rWIA<>nil) then rWIA.Free;
  DownloadedFiles:= nil;

  inherited Destroy;
end;

procedure TDigIt_Source_WIA.SelectSourceItem(ASource: TWIADevice; AIndex: Integer);
begin
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

function TDigIt_Source_WIA.GetFromUser: Boolean; stdcall;
var
  newItemIndex,
  newDeviceIndex: Integer;
  newWIASource: TWIADevice;
  initPar: TInitialItemValues;

begin
  Result :=False;

  if (getWIA <> nil) then
  try
     rWia.EnumAll:= False;

     //Open the Select Device Dialog
     newDeviceIndex:= rWia.SelectDeviceDialog;
     if (newDeviceIndex > -1) then
     begin
       //Select the Device
       rWia.SelectedDeviceIndex:= newDeviceIndex;
       newWIASource:= rWia.SelectedDevice;
       if (newWIASource = nil) then raise Exception.Create(rsWIAExcConnect);

       //if not(newWIASource.GetParamsCapabilities(WIACap) then raise Exception.Create('Error Get Params Capabilities');;

       newItemIndex:= newWIASource.SelectedItemIndex;

       //If Device is different start with Default Values  { #note 5 -oMaxM : Store Params for others devices in XML? }
       if (DeviceID <> newWIASource.ID)
       then initPar:= initDefault
       else initPar:= initParams;

       //Select Scanner Item and Settings to use
       Result:= TWIASettingsSource.Execute(newWIASource, newItemIndex, initParams, WIAParams);
       if Result
       then SelectSourceItem(newWIASource, newItemIndex);

     end;
  finally
  end;
end;

function TDigIt_Source_WIA.OnSet: Boolean; stdcall;
var
   curSource: TWIADevice;
   curItem: PWIAItem;
   curItemName: String;
   aIndex: Integer;

begin
  if (getWIA <> nil) then
  try
     rWia.EnumAll:= False;
     aIndex:= -1;

     if (DeviceID <> '') or ((DeviceManufacturer <> '') and (DeviceName <> '')) then
     repeat
       Application.ProcessMessages;
       rWia.RefreshDeviceList;

       //Try to Open searching by ID and ItemName
       rWIA.SelectDeviceItem(DeviceID, DeviceItemName, curSource, aIndex);
       if (curSource = nil)
       then begin
              //Device not finded, search with DeviceName/DeviceManufacturer
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
                              Case MessageDlg('DigIt WIA',
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
                       if (MessageDlg('DigIt WIA', rsWIAExcConnect+#13#10+DeviceName+#13#10+DeviceManufacturer,
                                      mtError, [mbRetry, mbAbort], 0)=mrAbort)
                       then break;
                     end;
              end
              else if (MessageDlg('DigIt WIA', rsWIAExcNotFound+#13#10+DeviceName+#13#10+DeviceManufacturer,
                       mtError, [mbRetry, mbAbort], 0)=mrAbort)
                   then break;
            end
       else begin
              //Device found and to connect
              if (aIndex = -1) then
              begin
                //We have finded the Device but not the Item, ask the user what to do
                curItem:= curSource.Items[0];
                if (curItem <> nil) then
                Case MessageDlg('DigIt WIA',
                                Format(rsWIADevItemSel, [DeviceName, DeviceManufacturer, DeviceItemName, curItem^.Name]),
                                mtConfirmation, [mbYes, mbRetry, mbAbort], 0) of
                mrYes: begin curSource.SelectedItemIndex:= 0; aIndex:= curSource.SelectedItemIndex; end;
                mrAbort: break;
                end;
              end;
            end;
    until (aIndex > -1);

    if (aIndex = -1)
    then Result:= GetFromUser //User has selected Abort, Get Another Device from List
    else SelectSourceItem(curSource, aIndex);

  finally
  end;
end;

function TDigIt_Source_WIA.Duplicate: IDigIt_Params; stdcall;
begin
  Result:= nil;
end;

function TDigIt_Source_WIA.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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
     countTakes:= XMLWork.GetValue(xml_RootPath+'/CountTakes', 0);

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

function TDigIt_Source_WIA.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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
     XMLWork.SetValue(xml_RootPath+'/CountTakes', countTakes);

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

function TDigIt_Source_WIA.Summary(const ASummary: PChar): Integer; stdcall;
begin
  Result:= 0;
end;

function TDigIt_Source_WIA.Flags: TDigItInterfaceKind; stdcall;
begin
  Result:= diSourceStd;
end;

function TDigIt_Source_WIA.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_WIA.Enabled: Boolean; stdcall;
begin
  Result:= rEnabled and (getWIA <> nil) and (rWIA.DevMgrIntf <> nil);
end;

function TDigIt_Source_WIA.setEnabled(AEnabled: Boolean): Boolean; stdcall;
begin
  rEnabled:= AEnabled;
  Result:= rEnabled;
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

function TDigIt_Source_WIA.UI_Title(out AUI_Title: PChar): Integer; stdcall;
begin
  AUI_Title:= StrNew(PChar(rsWIAName));
  Result:= Length(AUI_Title);
end;

function TDigIt_Source_WIA.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 5;
end;

function TDigIt_Source_WIA.Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;
var
   aExt,
   curPath: String;
   aFormat: TWIAImageFormat;
   capRet: Boolean;
   curParams: TWIAParams;

begin
  try
     Result:= 0;
     aData:= nil;
     aDataType:= diDataType_FileName;

     DownloadedFiles:= nil;
     inc(countTakes);

     WIAPath_Temp:= theBridge.Settings.Path_Session_Temp+'wia'+DirectorySeparator;
     curPath:= WIAPath_Temp+IntToStr(countTakes)+DirectorySeparator;

     if WIAParams[DeviceItemIndex].NativeUI
     then begin
            Result:= rWiaSource.DownloadNativeUI(Application.ActiveFormHandle, False,
                                        curPath, WIA_TakeFileName, DownloadedFiles);
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
              Progress.SetEventCallBack(Self as IDigIt_ProgressCallback);
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
                                         aFormat, DownloadedFiles);
          end;

     if (Result > 0)
     then if (Result = 1 )
          then aData:= StrNew(PChar(DownloadedFiles[0]))
          else aData:= Self as IDigIt_ROArray
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
  WIAPath_Temp:= theBridge.Settings.Path_Session_Temp+'wia'+DirectorySeparator;
  for i:=0 to countTakes do
    DeleteDirectory(WIAPath_Temp+IntToStr(i)+DirectorySeparator, False);
  {$endif}

  countTakes:= -1;
  DownloadedFiles:= nil;
end;

function TDigIt_Source_WIA.GetCount: DWord; stdcall;
begin
  Result:= Length(DownloadedFiles);
end;

function TDigIt_Source_WIA.Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;
begin
  Result:= (aIndex < Length(DownloadedFiles));
  if Result
  then aData:= StrNew(PChar(DownloadedFiles[aIndex]))
  else aData:= nil;
end;

procedure TDigIt_Source_WIA.ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;
begin
  UserCancel:= True;
  if (Progress<>nil) then Progress.SetCurrentCaption(PChar(rsWIACancelling));
end;

initialization
  try
     WIAPath_Temp:= theBridge.Settings.Path_Session_Temp+'wia'+DirectorySeparator;

     Source_WIA:= TDigIt_Source_WIA.Create;
     theBridge.Sources.Register(DigIt_Source_WIA_Name, Source_WIA);

  except
  end;

end.

