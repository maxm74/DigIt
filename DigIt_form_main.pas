(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Main Form                                                                **
*******************************************************************************)

unit DigIt_Form_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtDlgs,
  ExtCtrls, Menus, ComCtrls, ActnList, Spin, ShellCtrls, EditBtn, SpinEx,
  LCLVersion, LCLType, Laz2_XMLCfg, FPImage,
  BGRABitmap, BGRABitmapTypes, BGRAPapers,
  BGRAImageManipulation,  BGRASpeedButton, BCPanel, BCLabel, BCListBox, BGRAImageList,
  BCExpandPanels,BGRAFlashProgressBar,
  DigIt_Types, DigIt_Utils, DigIt_Counters, Digit_Bridge_Intf, Digit_Bridge_Impl;

resourcestring
  rsNewWork = 'Start a New Work Session?';
  rsContinueWork = 'Continue from last Work Session?'#13#10'%s';
  rsContinueAutoWork = 'Continue from Auto Saved Work Session?';
  rsSaveWork = 'Save the Work Session?';
  rsSavingWork = 'Saving the Work Session';
  rsSavingSources = 'Saving Sources Files';
  rsSavingCaptured = 'Saving Captured Files';
  rsSavingSessionFiles = 'Saving Session Files';
  rsSavingSwitch = 'Switching to New Work Session';
  rsSavingDone = 'Saved Done';
  rsSaveWorkCopy = 'Create a Copy of the Work Session or Move it?';
  rsNoFilesDownloaded = 'NO Files Downloaded ';
  rsTakeAgain = 'Replace the last %d taked files with a new Take?';
  rsNoMoreFiles = 'There are no more files to process, should I clear the Work Queue?';
  rsMiddlePage = 'The page is within the already processed range'#13#10'Should I put it in the middle?';
  rsClearQueue = 'Clear the Work Queue?';
  rsExcCreateCounter = 'Failed to Create the Counter';
  rsNotImpl = 'Not yet implemented';
  rsClearCrops = 'Clear Crop Areas ?';
  rsCropFull = 'Full Area';
  rsCropCust = 'Custom';
  rsCropToDo = '%d files to do';
  rsCounterPrev = 'Value Previous: %d';
  rsDeleteCaptured = 'Delete Captured Page %s ?';
  rsDeleteAll = 'Delete All Captured Pages?';
  rsDeleteAllFiles = 'Do I also Delete Files from the disk?';

  rsSourceNotFound = 'Source "%s" not Found, try Select another from Menù';
  rsSourceNotSelected = 'Cannot Select Source %d, try Select another from Menù';
  rsErrIncomplete = 'Operation not completed, do I keep the processed files?';
  rsErrLoadWork = 'Cannot Load Work Session'#13#10'%s';
  rsErrSaveWork = 'Cannot Save Work Session'#13#10'%s'#13#10'%s';

type
  { TDigIt_Main }

  TDigIt_Main = class(TForm)
    actCropGoBack: TAction;
    actCropAll: TAction;
    actCrop: TAction;
    actClearQueue: TAction;
    actCropGoNext: TAction;
    actCapturedDeleteAll: TAction;
    actCapturedDelete: TAction;
    actConvertFiles: TAction;
    actConvertPDF: TAction;
    actTakeBuildDuplex: TAction;
    actTakeRe: TAction;
    actTimerTake: TAction;
    actSessionSaveAs: TAction;
    actPreview: TAction;
    actCapturedRotateRight: TAction;
    actCapturedRotateLeft: TAction;
    ActionListC: TActionList;
    actOptions: TAction;
    actSessionOpen: TAction;
    actSessionSave: TAction;
    actSessionNew: TAction;
    actCropNext: TAction;
    actTake: TAction;
    ActionListMain: TActionList;
    BCLabel1: TBCLabel;
    BCLabel10: TBCLabel;
    BCLabel11: TBCLabel;
    BCLabel12: TBCLabel;
    BCLabel13: TBCLabel;
    BCLabel14: TBCLabel;
    BCLabel2: TBCLabel;
    BCLabel3: TBCLabel;
    BCLabel4: TBCLabel;
    BCLabel5: TBCLabel;
    BCLabel6: TBCLabel;
    BCLabel7: TBCLabel;
    BCLabel8: TBCLabel;
    imgListCaptured: TImageList;
    itemTake: TMenuItem;
    itemTakeAgain: TMenuItem;
    itemBuildDuplex: TMenuItem;
    menuDebug: TMenuItem;
    menuClearXML: TMenuItem;
    menuImageFormat: TMenuItem;
    menuExport: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    menuProjectSaveAs: TMenuItem;
    menuSaveXML: TMenuItem;
    menuLoadXML: TMenuItem;
    panelTop: TPanel;
    panelCounterList: TBCPanel;
    btCounter_Add: TBGRASpeedButton;
    btCounter_Del: TBGRASpeedButton;
    btPFlipH: TSpeedButton;
    btPRotateLeft: TSpeedButton;
    btPRotateRight: TSpeedButton;
    btPFlipV: TSpeedButton;
    btPRotate180: TSpeedButton;
    cbCounterList: TComboBox;
    imgListImgActions: TImageList;
    Label10: TLabel;
    Label9: TLabel;
    menuDestinations: TPopupMenu;
    itemCropModeFull: TMenuItem;
    itemCropModeCustom: TMenuItem;
    panelPageRotate: TBCPanel;
    btBox_Add: TBGRASpeedButton;
    btBox_Del: TBGRASpeedButton;
    btCFlipHLeft: TSpeedButton;
    btCFlipHRight: TSpeedButton;
    btCFlipVDown: TSpeedButton;
    btCFlipVUp: TSpeedButton;
    btCropCounter_Add: TBGRASpeedButton;
    btCropCounter_Del: TBGRASpeedButton;
    btCropDuplicate: TSpeedButton;
    btCropDuplicateOp: TSpeedButton;
    btCRotateLeft: TSpeedButton;
    btCRotateRight: TSpeedButton;
    btPageSizes: TSpeedButton;
    btPageSizesToCrops: TSpeedButton;
    btPaperSizes: TSpeedButton;
    btZBack: TSpeedButton;
    btZDown: TSpeedButton;
    btZFront: TSpeedButton;
    btZUp: TSpeedButton;
    cbBoxList: TComboBox;
    cbCropCounterList: TComboBox;
    edCounterName: TEdit;
    edCounterValue: TSpinEdit;
    edCounterValueStringDigits: TSpinEdit;
    edCounterValueStringPost: TEdit;
    edCounterValueStringPre: TEdit;
    edCropAspectPersonal: TEditButton;
    edCropHeight: TFloatSpinEdit;
    edCropLeft: TFloatSpinEdit;
    edCropName: TEdit;
    edCropTop: TFloatSpinEdit;
    edCropUnit_Type: TComboBox;
    edCropWidth: TFloatSpinEdit;
    edPageHeight: TFloatSpinEdit;
    edPageWidth: TFloatSpinEdit;
    edPage_UnitType: TComboBox;
    imgListMenu: TImageList;
    imgListThumb: TBGRAImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    imgManipulation: TBGRAImageManipulation;
    imgListMain: TImageList;
    lbCounterExample: TLabel;
    lbPrevious: TLabel;
    lvCaptured: TListView;
    menuTimerTakeConfig: TMenuItem;
    menuTimerTakeStop: TMenuItem;
    menuOptions: TMenuItem;
    OpenSessionDlg: TOpenDialog;
    panelCounter: TBCPanel;
    panelCropArea: TBCPanel;
    menuPaperSizes: TPopupMenu;
    menuSources: TPopupMenu;
    panelPageSize: TBCPanel;
    menuTimerTake: TPopupMenu;
    menuCropMode: TPopupMenu;
    menuTake: TPopupMenu;
    rgCropAspect: TRadioGroup;
    rollCounters: TBCExpandPanel;
    rollCrops: TBCExpandPanel;
    rollPages: TBCExpandPanel;
    SaveSessionDlg: TSaveDialog;
    Separator1: TMenuItem;
    menuProjectOpen: TMenuItem;
    menuProjectSave: TMenuItem;
    menuProjectNew: TMenuItem;
    MenuMain: TPopupMenu;
    SelectDirectory: TSelectDirectoryDialog;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    SpeedButton1: TSpeedButton;
    tbCaptured: TToolBar;
    tbCapturedRotateLeft: TToolButton;
    tbCapturedPDF: TToolButton;
    tbCapturedRotateRight: TToolButton;
    tbCropAll: TToolButton;
    tbCropMode: TToolButton;
    tbCropNext: TToolButton;
    tbCropPrev: TToolButton;
    tbCropSummary: TToolButton;
    tbDestination: TToolButton;
    tbMain: TToolBar;
    tbMenu: TToolButton;
    tbPreview: TToolButton;
    tbSepCrop: TToolButton;
    tbSepMenu: TToolButton;
    tbSepTake: TToolButton;
    tbSource: TToolButton;
    tbTake: TToolButton;
    tbTakeAgain: TToolButton;
    tbTimerTake: TToolButton;
    tbCrop: TToolBar;
    tbSepClear: TToolButton;
    tbClearQueue: TToolButton;
    ToolButton1: TToolButton;
    tbCapturedDelete: TToolButton;
    tbCapturedDeleteAll: TToolButton;
    tbCaptSep1: TToolButton;
    tbCaptSep2: TToolButton;

    procedure actCapturedDeleteAllExecute(Sender: TObject);
    procedure actCapturedDeleteExecute(Sender: TObject);
    procedure actConvertFilesExecute(Sender: TObject);
    procedure actConvertPDFExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actSessionNewExecute(Sender: TObject);
    procedure actSessionOpenExecute(Sender: TObject);
    procedure actSessionSaveAsExecute(Sender: TObject);
    procedure actSessionSaveExecute(Sender: TObject);
    procedure actCapturedRotateLeftExecute(Sender: TObject);
    procedure actCapturedRotateRightExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actTakeReExecute(Sender: TObject);
    procedure actTakeExecute(Sender: TObject);
    procedure actTimerTakeExecute(Sender: TObject);
    procedure actCropNextExecute(Sender: TObject);
    procedure actCropGoNextExecute(Sender: TObject);
    procedure actCropGoBackExecute(Sender: TObject);
    procedure actCropAllExecute(Sender: TObject);
    procedure actClearQueueExecute(Sender: TObject);
    procedure btCFlipHLeftClick(Sender: TObject);
    procedure btCFlipHRightClick(Sender: TObject);
    procedure btCFlipVDownClick(Sender: TObject);
    procedure btCFlipVUpClick(Sender: TObject);
    procedure btCropDuplicateClick(Sender: TObject);
    procedure btCRotateLeftClick(Sender: TObject);
    procedure btCRotateRightClick(Sender: TObject);
    procedure btPageSizesClick(Sender: TObject);
    procedure btPageSizesToCropsClick(Sender: TObject);
    procedure btPaperSizesClick(Sender: TObject);
    procedure btZBackClick(Sender: TObject);
    procedure btZDownClick(Sender: TObject);
    procedure btZFrontClick(Sender: TObject);
    procedure btZUpClick(Sender: TObject);
    procedure cbCounterListChange(Sender: TObject);
    procedure btCounter_AddClick(Sender: TObject);
    procedure btCounter_DelClick(Sender: TObject);
    procedure cbCropCounterListChange(Sender: TObject);
    procedure btCropCounter_AddClick(Sender: TObject);
    procedure btCropCounter_DelClick(Sender: TObject);
    procedure edPageHeightChange(Sender: TObject);
    procedure edPageWidthChange(Sender: TObject);
    procedure edPage_UnitTypeChange(Sender: TObject);
    procedure itemCropModeClick(Sender: TObject);
    procedure lvCapturedDblClick(Sender: TObject);
    procedure lvCapturedSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure edCounterNameEditingDone(Sender: TObject);
    procedure edCounterValueChange(Sender: TObject);
    procedure edCounterValueStringDigitsChange(Sender: TObject);
    procedure edCounterValueStringPreEditingDone(Sender: TObject);
    procedure edCounterValueStringPostEditingDone(Sender: TObject);
    procedure edCropNameEditingDone(Sender: TObject);
    procedure edCropUnit_TypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure edNameChange(Sender: TObject);

    procedure btBox_AddClick(Sender: TObject);
    procedure btBox_DelClick(Sender: TObject);
    procedure cbBoxListChange(Sender: TObject);
    procedure edCropHeightChange(Sender: TObject);
    procedure edCropLeftChange(Sender: TObject);
    procedure edCropTopChange(Sender: TObject);
    procedure edCropWidthChange(Sender: TObject);
    procedure menuDebugClick(Sender: TObject);
    procedure menuImageFormatClick(Sender: TObject);
    procedure rgCropAspectSelectionChanged(Sender: TObject);
    procedure btCropApplyAspectRatioClick(Sender: TObject);

    procedure AddedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure DeletedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure ChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure SelectedChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);

    procedure tbCapturedPDFClick(Sender: TObject);

  private
    { private declarations }
    dlgRes: TModalResult;

    lastNewBoxNum: Word;
    changingAspect,
    Closing,
    XML_Loading,
    inFillCounterUI,
    inFillBoxUI,
    inFillPagesUI,
    imgListThumb_Changed,
    rSessionModified: Boolean;
    Counters: TDigIt_CounterList;

    CropMode: TDigItCropMode;

    rSource: PSourceInfo;
    rSourceParams: IDigIt_Params;
    rSourceName: String;

    { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
    (*    rDestination: PDestinationInfo;
    rDestinationParams: IDigIt_Params;
    *)
    rDestinationName: String;

    SaveFormat: TBGRAImageFormat;
    SaveWriter: TFPCustomImageWriter;
    SaveExt,
    Session_File,
    LoadedFile: String;

    testI,
    lastCropped,
    lastLenTaked,     //Used in Take Again
    iSourceFiles,
    iCapturedFiles: Integer;
    SourceFiles: TSourceFileArray;
    CapturedFiles: TCapturedFileArray;

    function GetCurrentCropArea: TCropArea;

    function Counters_GetCurrent: TDigIt_Counter;
    procedure Counters_Dec;
    procedure Counters_Inc;
    procedure SetSessionModified(AValue: Boolean);

    procedure UI_FillBox(ABox :TCropArea);
    procedure UI_FillCounter(ACounter :TDigIt_Counter);
    procedure UI_FillCounters;
    procedure UI_FillPageSizes;
    procedure UI_ToolBar;
    procedure UI_ToolBarMods;
    procedure UI_MenuItemsChecks(newSourceI, newDestinationI: Integer);
    procedure UI_ThumbnailUpdate(AIndex: Integer; AFileName: String); overload;
    procedure UI_ThumbnailUpdate(AIndex: Integer; ABitmap: TBGRABitmap); overload;

    procedure DestinationMenuClick(Sender: TObject);
    procedure SourceMenuClick(Sender: TObject);

    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer);

    procedure UpdateBoxList;
    procedure UpdateCropAreaCountersList(ACounter :TDigIt_Counter);
    procedure CounterSelect(AIndex:Integer);

    function LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
    procedure EmptyImage(saveToXML: Boolean);

    procedure XML_LoadWork(IsAutoSave: Boolean);
    procedure XML_SaveWork(IsAutoSave: Boolean);
    procedure XML_ClearWork(AFromStartup: Boolean);  //Works on AutoSave
    function XML_LoadSessionFile(APath, AFile: String): Boolean; overload;
    function XML_LoadSessionFile(AFileName: String): Boolean; overload;
    function XML_SaveSessionFile(AFileName: String): Boolean;

    function XML_LoadSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
    procedure XML_SaveSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_LoadSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    function XML_LoadDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
    procedure XML_SaveDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_LoadLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveSource_CapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveSource_CapturedIndexes(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SavePageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure XML_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);

    procedure XML_OptionsLoad;
    procedure XML_OptionsSave;
    procedure XML_OptionsLoad_Session(aXML: TRttiXMLConfig; var APath, AFile: String);
    procedure XML_OptionsSave_Session(aXML: TRttiXMLConfig; const APath, AFile: String);

    function Source_Select(newSourceIndex: Integer): Boolean;
    function Destination_Select(newDestinationIndex: Integer): Boolean;

    procedure setCropMode(ANewCropMode: TDigItCropMode);

    function SourceFiles_Add(AArray: IDigIt_ROArray): Integer; overload;
    function SourceFiles_Add(AFileName: String): Integer; overload;

    function CropFile_Full(AFileName: String): Boolean; overload;
    procedure CropFile_Full(AStartIndex: Integer); overload;

    procedure Pages_InsertMiddle(ASourceFileIndex: Integer);

    procedure SetSaveWriter(AFormat: TBGRAImageFormat);
    procedure SetDefaultStartupValues;
    procedure SetDefaultSessionValues;
    procedure ClearCaptured;

  public
    procedure ItemSizesClick(Sender: TObject);
    procedure PageSizesClick(Sender: TObject);

    property Source: PSourceInfo read rSource;
    property SourceName: String read rSourceName;
    property SourceParams: IDigIt_Params read rSourceParams;

    (*
    property Destination: PDestinationInfo read rDestination;
    property DestinationParams: IDigIt_Params read rDestinationParams;
    *)
    property DestinationName: String read rDestinationName;

    property SessionModified: Boolean read rSessionModified write SetSessionModified;

  end;

var
  DigIt_Main: TDigIt_Main;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLProc, fppdf, FileUtil, LazFileUtils,
  BGRAWriteJPeg, BGRAFormatUI,
  MM_StrUtils,
  DigIt_Destinations, DigIt_Destination_SaveFiles_SettingsForm,
  //DigIt_Form_PDF,
  DigIt_Form_ExportFiles,
  DigIt_Form_Progress, DigIt_Form_Templates, DigIt_Form_BuildDuplex;


{ TDigIt_Main }

procedure TDigIt_Main.edCounterNameEditingDone(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  curCounter :=Counters_GetCurrent;
  if (curCounter<>nil) then
  begin
    curCounter.Name :=edCounterName.Text;
    UpdateCropAreaCountersList(curCounter);
  end;
end;

procedure TDigIt_Main.edCounterValueChange(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  if inFillCounterUI then exit;

  curCounter :=Counters_GetCurrent;
  if (curCounter<>nil) then
  begin
    curCounter.Value_Next :=edCounterValue.Value;
    lbCounterExample.Caption:=curCounter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCounterValueStringDigitsChange(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  if inFillCounterUI then exit;

  curCounter :=Counters_GetCurrent;
  if (curCounter<>nil) then
  begin
    curCounter.Value_StringDigits :=edCounterValueStringDigits.Value;
    lbCounterExample.Caption:=curCounter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCounterValueStringPreEditingDone(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  curCounter :=Counters_GetCurrent;
  if (curCounter<>nil) then
  begin
    curCounter.Value_StringPre :=edCounterValueStringPre.Text;
    lbCounterExample.Caption:=curCounter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCounterValueStringPostEditingDone(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  curCounter :=Counters_GetCurrent;
  if (curCounter<>nil) then
  begin
    curCounter.Value_StringPost :=edCounterValueStringPost.Text;
    lbCounterExample.Caption:=curCounter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCropNameEditingDone(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then CropArea.Name :=edCropName.Text;
end;

procedure TDigIt_Main.edCropUnit_TypeChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    CropArea.AreaUnit:=TResolutionUnit(edCropUnit_Type.ItemIndex);
    UI_FillBox(CropArea);
  end;
end;

procedure TDigIt_Main.FormCreate(Sender: TObject);
var
   i :Integer;

begin
  Counters :=TDigIt_CounterList.Create('Counters');

  Closing :=False;
  XML_Loading:= False;
  changingAspect :=False;
  inFillCounterUI :=False;
  inFillBoxUI :=False;
  inFillPagesUI :=False;
  imgListThumb_Changed:= False;

  lastNewBoxNum :=0;
  TStringList(cbBoxList.Items).OwnsObjects:=False;
  TStringList(cbCounterList.Items).OwnsObjects:=False;

  rSource:= nil;
  rSourceName:= '';;
  rSourceParams:= nil;

  //  rDestination:= nil;
  rDestinationName:= '';
  //  rDestinationParams:= nil;


  SetSaveWriter(ifJpeg);

  Session_File:= File_DefSession;

  SetDefaultStartupValues;

  BuildSourcesMenu(Self, menuSources, @SourceMenuClick);
  BuildDestinationsMenu(Self, menuDestinations, @DestinationMenuClick);

  {$ifopt D+}
    itemCropModeCustom.Enabled:= True;
    menuDebug.Visible:= True;
    lbPrevious.Visible:= True;
  {$endif}
end;

procedure TDigIt_Main.FormShow(Sender: TObject);
var
   i:Integer;
   optPath_Session,
   optFile_Session: String;
   sessLoaded: Boolean;

begin
  UI_ToolBarMods;

  sessLoaded:= False;
  try
     XML_OptionsLoad_Session(nil, optPath_Session, optFile_Session);

     //If in Options there is a Session Opened then Open It
     if (optPath_Session <> '') and (optFile_Session <> '') and
        FileExists(optPath_Session+optFile_Session+Ext_Sess) and
        (MessageDlg('DigIt', Format(rsContinueWork, [optPath_Session+optFile_Session]),
                    mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     then sessLoaded:= XML_LoadSessionFile(optPath_Session, optFile_Session);

     if not(sessLoaded) then
     begin
       //ask for open AutoSave if exists
       if FileExists(Path_DefSession+File_DefSession+Ext_AutoSess) then
       begin
         sessLoaded:= True;
         if (MessageDlg('DigIt', rsContinueAutoWork, mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then begin
                XML_LoadWork(True);
                sessLoaded:= True;
              end
         else begin
                XML_ClearWork(True);
                sessLoaded:= False;
              end;
       end;
     end;

     if not(sessLoaded)
     then setCropMode(diCropFull);

  except
    Path_Session:= Path_DefSession;
    Path_Session_Scan:= Path_DefSession_Scan;
    Path_Session_Pictures:= Path_DefSession_Pictures;

    SetDefaultStartupValues;

    setCropMode(diCropFull);
  end;
end;

procedure TDigIt_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  closing :=True;

  //AutoSave
  if SessionModified then //FileExists(Path_Session+Session_File+Ext_AutoSess)
  begin
    XML_SaveWork(True);

    //Save Current Session ?
    actSessionSaveExecute(nil);

    CanClose:= not(dlgRes = mrCancel);
  end;
end;

procedure TDigIt_Main.FormDestroy(Sender: TObject);
begin
  Counters.Free;
  theBridge.Free;
  SourceFiles:= nil;
  if (SaveWriter <> nil) then SaveWriter.Free;
end;

procedure TDigIt_Main.actPreviewExecute(Sender: TObject);
var
  curData: Pointer;
  curDataType: TDigItDataType;
  res: Integer;
  curImageFile: PChar;

begin
  try
      curData:= nil;
      res:= rSource^.Inst.Take(takeActPreview, curDataType, curData);
      if (res > 0) and (curData <> nil) then
      begin
        if (curDataType = diDataType_FileName) then
        begin
          if (res = 1)
          then curImageFile:= PChar(curData)
          else if (res > 1)
               then if not(IDigIt_ROArray(curData).Get(0, curImageFile))
                    then curImageFile:= '';

          if (curImageFile <> '') then
          begin
            LoadImage(curImageFile, True);
            StrDispose(curImageFile);
          end;
        end;
      end
      else MessageDlg(rsNoFilesDownloaded, mtError, [mbOk], 0);

  finally
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actTakeReExecute(Sender: TObject);
var
   lenSources: Integer;

begin
  if (SourceFiles <> nil) and
     (MessageDlg('DigIt', Format(rsTakeAgain, [lastLenTaked]),
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    lenSources:= Length(SourceFiles);
    if ((lenSources - lastLenTaked) > 0)
    then SetLength(SourceFiles, lenSources-lastLenTaked)
    else SourceFiles:= nil;
    lastLenTaked:= 0;

    actTake.Execute;
  end;
end;

procedure TDigIt_Main.actTakeExecute(Sender: TObject);
var
   curData: Pointer;
   curDataType: TDigItDataType;
   res, i,
   oldLength: Integer;
   curImageFile: PChar;
   curArray: IDigIt_ROArray;

begin
  try
      curData:= nil;
      res:= 0;

      if (Sender = actTake)
      then res:= rSource^.Inst.Take(takeActTake, curDataType, curData)
      else
      if (Sender = actTakeBuildDuplex)
      then res:= WizardBuildDuplexExecute(curDataType, curData);

      if (res > 0) and (curData <> nil) then
      begin
        if (curDataType = diDataType_FileName) then
        begin
          if (res = 1)
          then curImageFile:= PChar(curData)
          else if (res > 1) then curArray:= IDigIt_ROArray(curData);

          Case CropMode of
            diCropFull: begin
              DigIt_Progress.ProgressShow(rsProcessingImages, 1, res);

              if (res = 1 )
              then begin
                     CropFile_Full(curImageFile);
                     StrDispose(curImageFile);
                   end
              else begin
                     SourceFiles_Add(curArray);
                     CropFile_Full(0);
                   end;
              SourceFiles:= nil; iSourceFiles:= -1;
              rSource^.Inst.Clear;
            end;
            diCropCustom: begin
              //Add files to the queue array
              oldLength:= Length(SourceFiles);
              if (res = 1 )
              then begin
                     SourceFiles_Add(curImageFile);
                     StrDispose(curImageFile);
                   end
              else SourceFiles_Add(curArray);

              //The queue was empty, Start from the first file
              if (oldLength = 0) and (Length(SourceFiles) > 0) then
              begin
                iSourceFiles:= 0;
                LoadImage(SourceFiles[0].fName, False);
              end;

              (*
              //Crop directly the first File,
              //  I don't use iSourceFiles because the user could be here having done Next and then Prev
              if (oldLength = 0) and (Length(SourceFiles) > 0) then
              begin
                if LoadImage(SourceFiles[0]) then actCropNext.Execute;
              end;
              *)
            end;
          end;
        end;
      end
      else MessageDlg(rsNoFilesDownloaded, mtError, [mbOk], 0);

  finally
    XML_SaveWork(True);
    UI_ToolBar;
    UI_FillCounter(nil);

    DigIt_Progress.Hide;
    FreeAndNil(WizardBuildDuplex);
  end;
end;

procedure TDigIt_Main.actTimerTakeExecute(Sender: TObject);
begin
  //
end;

procedure TDigIt_Main.actCropNextExecute(Sender: TObject);
var
   lenSources: Integer;

  function CheckEndOfFiles: Boolean;
  begin
    Result:= (iSourceFiles >= lenSources);
    if Result and (lenSources > 1) then
    begin
      //User Confirmation
      if (MessageDlg('DigIt', rsNoMoreFiles, mtConfirmation,
                     [mbYes, mbNo], 0) = mrYes)
      then begin
             iSourceFiles:= -1;
             SourceFiles:= nil;
             rSource^.Inst.Clear;
           end;
    end;
  end;

begin
  try
    lenSources:= Length(SourceFiles);
    if (iSourceFiles = -1) then iSourceFiles:= 0;

    if (iSourceFiles = lenSources)
    then begin
           //We've already cut the last one, Re Crop the Last File, dec the Counters first
           Counters_Dec;
           imgManipulation.getAllBitmaps(@SaveCallBack, 1, True);
         end
    else begin
           if SourceFiles[iSourceFiles].fCrop
           then imgManipulation.getAllBitmaps(@SaveCallBack, 1, True) //Re Crop, Counters is already decremented by GoBack
           else begin  //(lastCropped < iSourceFiles)
                  if (lastCropped > iSourceFiles) then
                  begin
                    if (MessageDlg('DigIt', rsMiddlePage, mtConfirmation, [mbYes, mbCancel], 0) = mrCancel)
                    then exit
                    else begin Pages_InsertMiddle(iSourceFiles); exit; end;
                   end;
                  imgManipulation.getAllBitmaps(@SaveCallBack, 0, True);
                  SourceFiles[iSourceFiles].fCrop:= True;

                  if (iSourceFiles > lastCropped) then lastCropped:= iSourceFiles;
                end;

           inc(iSourceFiles);
         end;

    if not(CheckEndOfFiles)
    then if (iSourceFiles > -1) and (iSourceFiles < Length(SourceFiles))
         then if not(LoadImage(SourceFiles[iSourceFiles].fName, False))
              then begin
                     { #todo -oMaxM : do something if LoadImage Fails? }
                   end;

    lvCaptured.Selected:= lvCaptured.Items[iCapturedFiles];

  finally
    XML_SaveSource_CapturedFiles(nil, True);
    UI_FillCounter(nil);
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actCropGoNextExecute(Sender: TObject);
var
   new_iSourceFiles: Integer;

begin
  try
     new_iSourceFiles:= iSourceFiles;
     if (new_iSourceFiles >= 0) and (new_iSourceFiles < Length(SourceFiles)-1) then
     begin
       inc(new_iSourceFiles);
       if LoadImage(SourceFiles[new_iSourceFiles].fName, False) then
       begin
         if SourceFiles[new_iSourceFiles].fCrop then Counters_Inc;
         iSourceFiles:= new_iSourceFiles;

         lvCaptured.Selected:= lvCaptured.Items[iCapturedFiles];

         //XML_SaveWork([xmlSourceIndexes, xmlCapturedIndexes]);
         XML_SaveSource_CapturedFiles(nil, True);
         UI_FillCounter(nil);
         UI_ToolBar;
       end;
     end;

  finally
  end;
end;

procedure TDigIt_Main.actCropGoBackExecute(Sender: TObject);
var
   new_iSourceFiles: Integer;

begin
  try
     new_iSourceFiles:= iSourceFiles;
     if (new_iSourceFiles > 0) then
     begin
       //We've already cut the last one, start with the penultimate one
       if (new_iSourceFiles >= Length(SourceFiles)) then
       begin
         new_iSourceFiles:= Length(SourceFiles)-1;
         Counters_Dec;
       end;

       dec(new_iSourceFiles);

       if LoadImage(SourceFiles[new_iSourceFiles].fName, False) then
       begin
         if SourceFiles[new_iSourceFiles].fCrop then Counters_Dec;
         iSourceFiles:= new_iSourceFiles;

         lvCaptured.Selected:= lvCaptured.Items[iCapturedFiles];

         //XML_SaveWork([xmlSourceIndexes, xmlCapturedIndexes]);
         XML_SaveSource_CapturedFiles(nil, True);
         UI_FillCounter(nil);
         UI_ToolBar;
       end;
     end;

  finally
  end;
end;

procedure TDigIt_Main.actCropAllExecute(Sender: TObject);
var
   i: Integer;
   c: Integer;
   cStr: String;
   Finished: Boolean;

   function CheckEndOfFiles: Boolean;
   begin
     Result:= (iSourceFiles >= Length(SourceFiles));
     if Result then
     begin
       //User Confirmation
       if (MessageDlg('DigIt', rsNoMoreFiles, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
       then begin
              iSourceFiles:= -1;
              SourceFiles:= nil;
              rSource^.Inst.Clear;
            end;
     end;
   end;

begin
  try
    Finished:= False;
    c:= Length(SourceFiles);
    cStr:= IntToStr(c);
    DigIt_Progress.ProgressShow(rsProcessingImages, iSourceFiles, c);

    repeat
      DigIt_Progress.progressTotal.Position:= iSourceFiles;
      DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [iSourceFiles, cStr]);
      Application.ProcessMessages;
      if DigIt_Progress.Cancelled then break;

      imgManipulation.getAllBitmaps(@SaveCallBack, 0, True);
      SourceFiles[iSourceFiles].fCrop:= True;
      inc(iSourceFiles);

      Finished:= CheckEndOfFiles;
      if not(Finished)
      then if not(LoadImage(SourceFiles[iSourceFiles].fName, False))
           then begin
                  { #todo -oMaxM : do something if LoadImage Fails? }
                  break; //UserCancel:= True;
                end;

      lvCaptured.Selected:= lvCaptured.Items[iCapturedFiles];

      DigIt_Progress.progressTotal.Position:= iSourceFiles+1;
      DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [iSourceFiles-1, cStr]);
      Application.ProcessMessages;
    Until Finished or DigIt_Progress.Cancelled;

  finally
    //XML_SaveWork([xmlSourceIndexes, xmlSourceFiles, xmlCapturedIndexes, xmlCapturedFiles]);
    XML_SaveSource_CapturedFiles(nil, True);
    UI_FillCounter(nil);
    UI_ToolBar;

    DigIt_Progress.Hide;
  end;
end;

procedure TDigIt_Main.actClearQueueExecute(Sender: TObject);
begin
  if (MessageDlg('DigIt', rsClearQueue, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    iSourceFiles:= -1;
    lastCropped:= -1;
    lastLenTaked:= 0;
    SourceFiles:= nil;
    rSource^.Inst.Clear;

    //XML_SaveWork([xmlSourceIndexes, xmlSourceFiles, xmlCapturedIndexes]);
    XML_SaveSource_CapturedFiles(nil, True);
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.btCRotateLeftClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    if btCropDuplicateOp.Down then
    begin
      CropArea :=TCropArea.Create(imgManipulation, CropArea, True);
      imgManipulation.SelectedCropArea :=CropArea;
    end;
    CropArea.RotateLeft;
  end;
end;

procedure TDigIt_Main.btCFlipVDownClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    if btCropDuplicateOp.Down then
    begin
      CropArea :=TCropArea.Create(imgManipulation, CropArea, True);
      imgManipulation.SelectedCropArea :=CropArea;
    end;
    CropArea.FlipVDown;
  end;
end;

procedure TDigIt_Main.btCFlipHLeftClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    if btCropDuplicateOp.Down then
    begin
      CropArea :=TCropArea.Create(imgManipulation, CropArea, True);
      imgManipulation.SelectedCropArea :=CropArea;
    end;
    CropArea.FlipHLeft;
  end;
end;

procedure TDigIt_Main.btCFlipHRightClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    if btCropDuplicateOp.Down then
    begin
      CropArea :=TCropArea.Create(imgManipulation, CropArea, True);
      imgManipulation.SelectedCropArea :=CropArea;
    end;
    CropArea.FlipHRight;
  end;
end;

procedure TDigIt_Main.btCFlipVUpClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    if btCropDuplicateOp.Down then
    begin
      CropArea :=TCropArea.Create(imgManipulation, CropArea, True);
      imgManipulation.SelectedCropArea :=CropArea;
    end;
    CropArea.FlipVUp;
  end;
end;

procedure TDigIt_Main.btCropDuplicateClick(Sender: TObject);
var
   newCropArea :TCropArea;

begin
  if imgManipulation.SelectedCropArea<>nil then
  begin
    newCropArea :=TCropArea.Create(imgManipulation, imgManipulation.SelectedCropArea, True);
    imgManipulation.SelectedCropArea :=newCropArea;
    newCropArea.BorderColor :=VGALime;
  end;
end;

procedure TDigIt_Main.btCRotateRightClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    if btCropDuplicateOp.Down then
    begin
      CropArea :=TCropArea.Create(imgManipulation, CropArea, True);
      imgManipulation.SelectedCropArea :=CropArea;
    end;
    CropArea.RotateRight;
  end;
end;

procedure TDigIt_Main.btPageSizesClick(Sender: TObject);
begin
  menuPaperSizes.Items.Clear;
  BuildPaperSizesMenu(TResolutionUnit(edPage_UnitType.ItemIndex-1), Self, menuPaperSizes, @PageSizesClick, 4, 5);
  menuPaperSizes.PopUp;
end;

procedure TDigIt_Main.btPageSizesToCropsClick(Sender: TObject);
begin
  imgManipulation.SetEmptyImageSizeToCropAreas(True);

  if edPage_UnitType.ItemIndex=0
  then begin
         //if page size is not defined set it to PixelsPerCentimeter (fucking inch)
         imgManipulation.EmptyImage.ResolutionUnit:=ruPixelsPerCentimeter;
         edPage_UnitType.ItemIndex:=3;
       end
  else imgManipulation.EmptyImage.ResolutionUnit:=TResolutionUnit(edPage_UnitType.ItemIndex-1);

  UI_FillPageSizes;
end;

procedure TDigIt_Main.btPaperSizesClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil then
  begin
    menuPaperSizes.Items.Clear;
    BuildPaperSizesMenu(CropArea.AreaUnit, Self, menuPaperSizes, @ItemSizesClick, 4, 5);
    menuPaperSizes.PopUp;
  end;
end;

procedure TDigIt_Main.ItemSizesClick(Sender: TObject);
var
   ResUnit: TResolutionUnit;
   Paper: BGRAPapers.TPaperSize;
   CropArea :TCropArea;

begin
  if (Sender<>nil) then
  begin
    CropArea :=GetCurrentCropArea;
    if (CropArea<>nil) then
    begin
      PaperSizesMenuTag_decode(TMenuItem(Sender).Tag, ResUnit, Paper);
      CropArea.AreaUnit:=ResUnit;
      CropArea.SetSize(Paper.w, Paper.h);
    end;
  end;
end;

procedure TDigIt_Main.PageSizesClick(Sender: TObject);
var
   ResUnit: TResolutionUnit;
   Paper: BGRAPapers.TPaperSize;

begin
  if (Sender<>nil) then
  begin
    PaperSizesMenuTag_decode(TMenuItem(Sender).Tag, ResUnit, Paper);
    imgManipulation.SetEmptyImageSize(ResUnit, Paper.w, Paper.h);
    UI_FillPageSizes;
  end;
end;

procedure TDigIt_Main.btZBackClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringToBack;
         UpdateBoxList;
       end;
end;

procedure TDigIt_Main.btZDownClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringBackward;
         UpdateBoxList;
       end;
end;

procedure TDigIt_Main.btZFrontClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringToFront;
         UpdateBoxList;
       end;
end;

procedure TDigIt_Main.btZUpClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if CropArea<>nil
  then begin
         CropArea.BringForward;
         UpdateBoxList;
       end;
end;

procedure TDigIt_Main.cbCounterListChange(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  curCounter :=TDigIt_Counter(cbCounterList.Items.Objects[cbCounterList.ItemIndex]);
  UI_FillCounter(curCounter);
end;

procedure TDigIt_Main.btCounter_AddClick(Sender: TObject);
var
   newCounter: TDigIt_Counter;
   newIndex, newIndex2: Integer;
   curCropArea: TCropArea;

begin
  newCounter:=TDigIt_Counter.Create('');
  newIndex :=Counters.add(newCounter);
  if (newIndex<0)
  then begin
         newCounter.Free;
         raise Exception.Create(rsExcCreateCounter);
       end
  else begin
         newCounter.Name:='Counter '+IntToStr(newIndex);

         cbCounterList.AddItem(newCounter.Name, newCounter);
         cbCounterList.ItemIndex:=cbCounterList.Items.IndexOfObject(newCounter);

         newIndex2 :=cbCropCounterList.Items.Count;
         cbCropCounterList.AddItem(newCounter.Name, newCounter);

         //if is the first Counter and CropArea has no Counter assign this new
         if (newIndex2=0) or (Sender=nil) then
         begin
           curCropArea :=GetCurrentCropArea;
           if (curCropArea<>nil) and (curCropArea.UserData=-1) then
           begin
             curCropArea.UserData:=cbCounterList.ItemIndex;
             cbCropCounterList.ItemIndex:=curCropArea.UserData;
           end;
         end;

         UI_FillCounter(newCounter);
         edCounterName.SetFocus;
       end;
end;

procedure TDigIt_Main.btCounter_DelClick(Sender: TObject);
var
   curCounter :TDigIt_Counter;
   curIndex: Integer;
   CropArea :TCropArea;

begin
  curIndex :=cbCounterList.ItemIndex;
  curCounter :=TDigIt_Counter(cbCounterList.Items.Objects[curIndex]);
  Counters.Delete(curIndex);
  cbCounterList.Items.Delete(curIndex);

  cbCropCounterList.Items.Delete(curIndex);

  //if CropArea has this Counter assign nil
  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil) and (CropArea.UserData=curIndex) then
  begin
    CropArea.UserData :=-1;
    cbCropCounterList.ItemIndex:=-1;
  end;
end;

procedure TDigIt_Main.cbCropCounterListChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.UserData :=cbCropCounterList.ItemIndex;
end;

procedure TDigIt_Main.btCropCounter_AddClick(Sender: TObject);
begin
  rollCounters.Collapsed:=False;
  btCounter_AddClick(nil);
end;

procedure TDigIt_Main.btCropCounter_DelClick(Sender: TObject);
begin
  //
end;

procedure TDigIt_Main.edPageHeightChange(Sender: TObject);
begin
  if inFillPagesUI then exit;

  imgManipulation.SetEmptyImageSize(TResolutionUnit(edPage_UnitType.ItemIndex),
                                      edPageWidth.Value, edPageHeight.Value);
end;

procedure TDigIt_Main.edPageWidthChange(Sender: TObject);
begin
  if inFillPagesUI then exit;

  imgManipulation.SetEmptyImageSize(TResolutionUnit(edPage_UnitType.ItemIndex),
                                      edPageWidth.Value, edPageHeight.Value);
end;

procedure TDigIt_Main.edPage_UnitTypeChange(Sender: TObject);
begin
  if (edPage_UnitType.ItemIndex=0)
  then imgManipulation.SetEmptyImageSizeToNull
  else begin
         //if there is a size defined, change only the resolution unit
         if (panelPageSize.Enabled)
         then imgManipulation.EmptyImage.ResolutionUnit:=TResolutionUnit(edPage_UnitType.ItemIndex-1)
         else imgManipulation.SetEmptyImageSize(TResolutionUnit(edPage_UnitType.ItemIndex-1),
                                         edPageWidth.Value, edPageHeight.Value);
       end;
  UI_FillPageSizes;
end;

procedure TDigIt_Main.itemCropModeClick(Sender: TObject);
begin
  setCropMode(TDigItCropMode(TMenuItem(Sender).Tag));
end;

procedure TDigIt_Main.lvCapturedDblClick(Sender: TObject);
var
   captItem: TListItem;

begin
  captItem:= lvCaptured.Selected;
  if (captItem <> nil) then OpenDocument(CapturedFiles[captItem.Index].fName);
end;

procedure TDigIt_Main.lvCapturedSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UI_ToolBar;
end;

procedure TDigIt_Main.lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
var
   captItem: TListItem;

begin
  captItem:= lvCaptured.GetItemAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y);
  if (captItem <> nil) then HintInfo^.HintStr:= CapturedFiles[captItem.Index].fName;
end;

procedure TDigIt_Main.DestinationMenuClick(Sender: TObject);
begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  begin
    if Destination_Select(TMenuItem(Sender).Tag) then
    begin
      TMenuItem(Sender).Default:= True;
      UI_ToolBar;
    end;
  end;
end;

procedure TDigIt_Main.SourceMenuClick(Sender: TObject);
begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  begin
    if Source_Select(TMenuItem(Sender).Tag) then
    begin
      TMenuItem(Sender).Default:= True;
      UI_ToolBar;
    end;
  end;
end;

procedure TDigIt_Main.actOptionsExecute(Sender: TObject);
begin
  //
end;

procedure TDigIt_Main.actCapturedDeleteExecute(Sender: TObject);
var
   captItem: TListItem;
   iCur, iSelected: Integer;

begin
  captItem:= lvCaptured.Selected;

  { #todo -oMaxM : this works ONLY in FullArea Mode, in Custom? Delete only the Item?  }
  if (captItem <> nil) and
     (MessageDlg('DigIt', Format(rsDeleteCaptured, [captItem.Caption]),
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  try
     lvCaptured.BeginUpdate;

     iSelected:= captItem.Index;

     //Delete File and Thumb
     DeleteFile(CapturedFiles[iSelected].fName);
     imgListThumb.Delete(CapturedFiles[iSelected].iIndex);
     imgListThumb_Changed:= True;

     if (iSelected < Length(CapturedFiles)-1) then
       for iCur:=iSelected+1 to Length(CapturedFiles)-1 do
       begin
         RenameFile(CapturedFiles[iCur].fName, CapturedFiles[iCur-1].fName);

         CapturedFiles[iCur-1].fAge:= CapturedFiles[iCur].fAge;
       end;

     //Delete Last Item
     lvCaptured.Items.Delete(Length(CapturedFiles)-1);
     SetLength(CapturedFiles, Length(CapturedFiles)-1);
     iCapturedFiles:= Length(CapturedFiles)-1;

     //Select Item
     if (iSelected < Length(CapturedFiles)-1)
     then lvCaptured.Selected:= lvCaptured.Items[iSelected]
     else lvCaptured.Selected:= lvCaptured.Items[Length(CapturedFiles)-1];

     if (Counters[0].Value = Length(CapturedFiles)) then
     begin
       Counters[0].Value:= Length(CapturedFiles)-1;
       UI_FillCounter(nil);
     end;

  finally
    lvCaptured.EndUpdate;
  end;
end;

procedure TDigIt_Main.actConvertFilesExecute(Sender: TObject);
begin
  TDigIt_ExportFiles.Execute(Application.Title, nil, False);
end;

procedure TDigIt_Main.actConvertPDFExecute(Sender: TObject);
begin
  TDigIt_ExportFiles.Execute(Application.Title, nil, True);
end;

procedure TDigIt_Main.actCapturedDeleteAllExecute(Sender: TObject);
var
   captItem: TListItem;
   curFileName: String;
   i: Integer;
   nofileBMP: TBitmap;

begin
  if (MessageDlg('DigIt', rsDeleteAll, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
     nofileBMP:=TBitmap.Create;

     //Delete all File in FileSystem ?
     if (MessageDlg('DigIt', rsDeleteAllFiles, mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     then for i:=0 to Length(CapturedFiles)-1 do
            DeleteFile(CapturedFiles[i].fName);

     //Clear Array and ListView
     ClearCaptured;

     //Reset Counters
     Counters.Reset;

     EmptyImage(False);

     XML_SaveWork(True);

     UI_ToolBar;
     UI_FillCounter(nil);
  end;
end;

procedure TDigIt_Main.actSessionNewExecute(Sender: TObject);
begin
  try
   (*  if TDigIt_Templates.Execute then
     begin

     end;
     *)
    if (MessageDlg('DigIt', rsNewWork, mtConfirmation, [mbYes, mbNo], 0)=mrYes)
    then XML_ClearWork(False);

   finally
   end;
end;

procedure TDigIt_Main.actSessionOpenExecute(Sender: TObject);
begin
  try
     if (Path_Session <> Path_DefSession) then
     case MessageDlg('DigIt', rsSaveWork, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
     mrYes : actSessionSave.Execute;
     mrCancel: exit;
     end;

     if OpenSessionDlg.Execute then XML_LoadSessionFile(OpenSessionDlg.FileName);
  finally
  end;
end;

procedure TDigIt_Main.actSessionSaveAsExecute(Sender: TObject);
begin
  if SaveSessionDlg.Execute
  then XML_SaveSessionFile(SaveSessionDlg.FileName);
end;

procedure TDigIt_Main.actSessionSaveExecute(Sender: TObject);
var
   canSave: Boolean;

begin
  try
     if (Sender = nil)
     then begin
            if SessionModified
            then begin
                   dlgRes:= MessageDlg('DigIt', rsSaveWork, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
                   canSave:= (dlgRes=mrYes);
                 end
            else begin
                   dlgRes:= mrNo;
                   canSave:= False;
                 end;
          end
     else canSave:= True;

     if canSave then
     begin
       if (Path_Session = Path_DefSession)
       then begin
              //Save As...
              if SaveSessionDlg.Execute
              then XML_SaveSessionFile(SaveSessionDlg.FileName);
            end
       else XML_SaveWork(False);
     end;

  finally
  end;
end;

procedure TDigIt_Main.actCapturedRotateLeftExecute(Sender: TObject);
var
   captItem: TListItem;
   sourceBitmap,
   rotatedBitmap: TBGRABitmap;
   curFileName: String;
   oIndex: Integer;

begin
  captItem:= lvCaptured.Selected;

  if (captItem <> nil) then
  try
     curFileName:= CapturedFiles[captItem.Index].fName;
     sourceBitmap:= TBGRABitmap.Create(curFileName);
     rotatedBitmap:= sourceBitmap.RotateCCW(True);
     rotatedBitmap.SaveToFile(curFileName);   {#todo 2 -oMaxM : Save with the same Format Options }

     UI_ThumbnailUpdate(captItem.Index, rotatedBitmap);

     //Update the Thumbnail in ListView
     //lvCaptured.Selected:= nil;
     //lvCaptured.Selected:= captItem;
     oIndex:= captItem.ImageIndex;
     captItem.ImageIndex:= -1;
     captItem.ImageIndex:= oIndex;

  finally
     if (sourceBitmap <> nil) then sourceBitmap.Free;
     if (rotatedBitmap <> nil) then rotatedBitmap.Free;
  end;
end;

procedure TDigIt_Main.actCapturedRotateRightExecute(Sender: TObject);
var
   captItem: TListItem;
   sourceBitmap,
   rotatedBitmap: TBGRABitmap;
   curFileName: String;
   oIndex: Integer;

begin
  captItem:= lvCaptured.Selected;

  if (captItem <> nil) then
  try
     curFileName:= CapturedFiles[captItem.Index].fName;
     sourceBitmap:= TBGRABitmap.Create(curFileName);
     rotatedBitmap:= sourceBitmap.RotateCW(True);
     rotatedBitmap.SaveToFile(curFileName);  {#todo 2 -oMaxM : Save with the same Format Options }

     UI_ThumbnailUpdate(captItem.Index, rotatedBitmap);

     //Update the Thumbnail in ListView
     //lvCaptured.Selected:= nil;
     //lvCaptured.Selected:= captItem;
     oIndex:= captItem.ImageIndex;
     captItem.ImageIndex:= -1;
     captItem.ImageIndex:= oIndex;

  finally
     if (sourceBitmap <> nil) then sourceBitmap.Free;
     if (rotatedBitmap <> nil) then rotatedBitmap.Free;
  end;
end;

procedure TDigIt_Main.SaveCallBack(Bitmap: TBGRABitmap; CropArea: TCropArea; AUserData: Integer);
var
  cropCounter: TDigIt_Counter;
  savedFile: String;
  captItem: TListItem;

begin
  if (CropArea <> nil)
  then begin
         if (CropArea.UserData<0)
         then begin
                //No Counter, Save File with CropArea Name
                savedFile:=Path_Session_Pictures+CropArea.Name+'.'+SaveExt;
                Bitmap.SaveToFile(savedFile, SaveWriter);
              end
         else begin
                //Increment the Counter Value
                cropCounter :=TDigIt_Counter(Counters.items[CropArea.UserData]);
                cropCounter.Value:=cropCounter.Value+1;
                inc(iCapturedFiles);

                //Save File
                savedFile:=Path_Session_Pictures+cropCounter.GetValue+'.'+SaveExt;
                Bitmap.SaveToFile(savedFile, SaveWriter);
              end;
       end
  else begin
         //Increment the Counter Value
         cropCounter :=TDigIt_Counter(Counters.items[0]);
         cropCounter.Value:=cropCounter.Value+1;
         inc(iCapturedFiles);

         //Save File
         savedFile:=Path_Session_Pictures+cropCounter.GetValue+'.'+SaveExt;
         Bitmap.SaveToFile(savedFile, SaveWriter);
       end;

  captItem:= nil;

  if (AUserData=0)
  then begin
         //Crop, add file to Captured List and in Thumbnails
         iCapturedFiles:= Length(CapturedFiles);

         SetLength(CapturedFiles, iCapturedFiles+1);
         CapturedFiles[iCapturedFiles].fName:= savedFile;
         CapturedFiles[iCapturedFiles].fAge:= FileAge(savedFile);
         CapturedFiles[iCapturedFiles].iIndex:= 0;

         UI_ThumbnailUpdate(iCapturedFiles, Bitmap);

         captItem:= lvCaptured.Items.Add;
         captItem.Caption:= ExtractFileName(savedFile);
         captItem.ImageIndex:= CapturedFiles[iCapturedFiles].iIndex;
       end
  else if (iCapturedFiles > -1) and (iCapturedFiles < Length(CapturedFiles)) then
       begin
         //ReCrop, Update the image in Captured List and in Thumbnails
         CapturedFiles[iCapturedFiles].fName:= savedFile;
         CapturedFiles[iCapturedFiles].fAge:= FileAge(savedFile);

         UI_ThumbnailUpdate(iCapturedFiles, Bitmap);

         captItem:= lvCaptured.Items[iCapturedFiles];
         captItem.Caption:= ExtractFileName(savedFile);
         captItem.ImageIndex:= CapturedFiles[iCapturedFiles].iIndex;
       end;

  //Go to Item in Captured List
  if (captItem <> nil) then captItem.MakeVisible(False);

//  XML_SaveCapturedFiles(nil); { #note -oMaxM : Maybe done with an Index }
end;

procedure TDigIt_Main.UpdateBoxList;
var
   i :Integer;
   CropArea,
   SelArea:TCropArea;

begin
  cbBoxList.OnChange:=nil;

  //SelArea :=BGRAImageManipulation.SelectedCropArea;
  cbBoxList.Clear;
  for i:=0 to imgManipulation.CropAreas.Count-1 do
  begin
    CropArea :=imgManipulation.CropAreas.items[i];
    cbBoxList.AddItem(CropArea.Name, CropArea);
  end;
  //imgManipulation.SelectedCropArea :=SelArea;
  cbBoxList.ItemIndex:=cbBoxList.Items.IndexOfObject(imgManipulation.SelectedCropArea);

  cbBoxList.OnChange:=@cbBoxListChange;
end;

procedure TDigIt_Main.UpdateCropAreaCountersList(ACounter: TDigIt_Counter);
begin
  //
end;

procedure TDigIt_Main.CounterSelect(AIndex: Integer);
begin
  cbCounterList.ItemIndex:=AIndex;
  UI_FillCounter(nil);
end;

function TDigIt_Main.LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
var
   Bitmap,
   BitmapR :TBGRABitmap;

begin
  Result:= False;

  if (AImageFile<>'') and FileExists(AImageFile) then
  try
     BitmapR:= Nil;

     Bitmap:= TBGRABitmap.Create;
     Bitmap.LoadFromFile(AImageFile);

     //Pre processing Filters

     //Flip
     if btPFlipH.Down
     then Bitmap.HorizontalFlip;
     if btPFlipV.Down
     then Bitmap.VerticalFlip;

     //Rotate
     if btPRotateLeft.Down
     then BitmapR:= Bitmap.RotateCCW(True);
     if btPRotateRight.Down
     then BitmapR:= Bitmap.RotateCW(True);
     if btPRotate180.Down
     then BitmapR:= Bitmap.RotateUD(True);

     { #todo -oMaxM : In future Preprocessing filters as Interfaces Here }

     if (BitmapR <> Nil)
     then imgManipulation.Bitmap := BitmapR
     else imgManipulation.Bitmap := Bitmap;

//     imgManipulation.Bitmap.InvalidateBitmap;

     LoadedFile:= AImageFile;

     if saveToXML then XML_SaveLoadedImage(nil, True);

     Result:= True;

  finally
     if (Bitmap <> Nil) then Bitmap.Free;
     if (BitmapR <> Nil) then BitmapR.Free;
  end;
end;

procedure TDigIt_Main.EmptyImage(saveToXML: Boolean);
begin
  imgManipulation.Bitmap:= nil;
  LoadedFile:= '';
  if saveToXML then XML_SaveLoadedImage(nil, True);
end;

procedure TDigIt_Main.XML_LoadWork(IsAutoSave: Boolean);
var
   newSourceI,
   newDestinationI: Integer;
   aXML: TRttiXMLConfig;

begin
  try
     XML_Loading:= True;

     if IsAutoSave
     then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
     else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     newSourceI:= XML_LoadSource(aXML, IsAutoSave);
     XML_LoadSourceFiles(aXML, IsAutoSave);
     newDestinationI:= XML_LoadDestination(aXML, IsAutoSave);
     XML_LoadCapturedFiles(aXML, IsAutoSave);
     XML_LoadPageSettings(aXML, IsAutoSave);
     XML_LoadLoadedImage(aXML, IsAutoSave);
     Counters.Load(aXML, True);
     XML_LoadCropAreas(aXML, IsAutoSave);
     XML_LoadUserInterface(aXML, IsAutoSave);

     SessionModified:= IsAutoSave;

  finally
     XML_Loading:= False;
     aXML.Free;

     UI_MenuItemsChecks(newSourceI, newDestinationI);
     UI_FillCounters;
     UI_FillCounter(nil);
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.XML_SaveWork(IsAutoSave: Boolean);
var
   aXML: TRttiXMLConfig;
   curExt: String;

begin
  if (rSource <> Nil) and (rSource^.Inst <> Nil) then
  try
     if IsAutoSave
     then curExt:= Ext_AutoSess
     else curExt:= Ext_Sess;

     aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+curExt);

     XML_SaveSource(aXML, IsAutoSave);
     XML_SaveSourceFiles(aXML, IsAutoSave);
     XML_SaveDestination(aXML, IsAutoSave);
     XML_SaveCapturedFiles(aXML, IsAutoSave);
     XML_SavePageSettings(aXML, IsAutoSave);
     XML_SaveLoadedImage(aXML, IsAutoSave);

     Counters.Save(aXML, True);
     if (cbCounterList.ItemIndex > -1) then aXML.SetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);

     XML_SaveCropAreas(aXML, IsAutoSave);
     XML_SaveUserInterface(aXML, IsAutoSave);

     aXML.Flush;
     aXML.Free;

     //FPC Bug?
     //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
     //So we do it after destroying XML.

     if (rSource <> nil) then rSource^.Inst.Params.Save(PChar(Path_Session+Session_File+curExt), 'Source/Params');
//     if (rDestination <> nil) then rDestination^.Inst.Params.Save(PChar(Path_Session+Session_File+curExt), 'Destination/Params');

     if not(IsAutoSave) then SessionModified:= False;

  finally
  end;
end;

procedure TDigIt_Main.XML_ClearWork(AFromStartup: Boolean);
var
   i: Integer;
   nofileBMP: TBitmap;

begin
  DeleteFile(Path_DefSession+File_DefSession+Ext_AutoSess);
  DeleteFile(Path_DefSession+File_DefSession+Ext_AutoThumb);
  DeleteDirectory(Path_DefSession_Scan, True);
  DeleteDirectory(Path_DefSession_Pictures, True);

  if not(AFromStartup) then
  try
     SetDefaultSessionValues;
     SetDefaultStartupValues;

     //Clear Source Queque
     //rSource^.Inst.Clear;

     //Clear Captured Array and ListView
     ClearCaptured;

     //This would be like a Full Area Template
     imgManipulation.Bitmap:= nil;
     if (CropMode = diCropCustom) then imgManipulation.clearCropAreas;

     //Reset Counters
     Counters.RemoveAllButFirst;
     Counters.Reset;

     setCropMode(diCropFull);

     Caption :='DigIt';

  finally
    UI_FillCounters;
    UI_FillCounter(nil);
  end;

  UI_ToolBar;
end;

function TDigIt_Main.XML_LoadSessionFile(APath, AFile: String): Boolean;
var
   oldPath_Session,
   oldPath_Session_Scan,
   oldPath_Session_Pictures,
   oldSession_File: String;

begin
  try
    Result:= False;

    oldPath_Session:= Path_Session;
    oldPath_Session_Scan:= Path_Session_Scan;
    oldPath_Session_Pictures:= Path_Session_Pictures;
    oldSession_File:= Session_File;

    Path_Session:= APath;
    Path_Session_Scan:= Path_Session+'Scan'+DirectorySeparator;
    Path_Session_Pictures:= Path_Session+'Pictures'+DirectorySeparator;
    Session_File:= AFile;

    XML_LoadWork(False);

    Result:= True;

  except
    Result:= False;
  end;

  if not(Result) then
  begin
    MessageDlg('DigIt', Format(rsErrLoadWork, [APath+AFile]), mtError, [mbOk], 0);

    Path_Session:= oldPath_Session;
    Path_Session_Scan:= oldPath_Session_Scan;
    Path_Session_Pictures:= oldPath_Session_Pictures;
    {#note : it would be more complicated than that because we would have to reread everything from the auto save}
  end;
end;


function TDigIt_Main.XML_LoadSessionFile(AFileName: String): Boolean;
begin
  Result:= XML_LoadSessionFile(ExtractFilePath(AFileName),
                               ExtractFileNameWithoutExt(ExtractFileName(AFileName)));
end;

function TDigIt_Main.XML_SaveSessionFile(AFileName: String): Boolean;
var
   newPath_Session,
   newSession_File,
   curFileName,
   curFileNameR: String;
   fileSources,
   fileCaptured: TStringArray;
   isRelative,
   isMove: Boolean;
   i,
   lenSources,
   lenCaptured: Integer;

begin
  Result:= False;
  try

  if (AFileName <> '') then
  try
     newPath_Session:= ExtractFilePath(AFileName);
     newSession_File:= ExtractFileNameWithoutExt(ExtractFileName(AFileName));
     ForceDirectories(newPath_Session);

     //if Path is Default then we are saving a new Session, we must move the Files
     if (Path_Session = Path_DefSession)
     then isMove:= True
     else begin
            //Copy or Move temp/thumb Files from Current Work Session to New Session
            dlgRes:= QuestionDlg('DigIt', rsSaveWorkCopy, mtConfirmation,
                                [mrYes, 'Copy', 'IsDefault',
                                 mrNo, 'Move',
                                 mrCancel, 'IsCancel'], 0);

            Case dlgRes of
            mrYes: isMove:= False;
            mrNo: isMove:= True;
            mrCancel: exit;
            end;
          end;

     DigIt_Progress.ProgressShow(rsSavingWork, 0, 5);


     lenSources:= Length(SourceFiles);
     lenCaptured:= Length(CapturedFiles);

     SetLength(fileSources, lenSources);
     SetLength(fileCaptured, lenCaptured);

     //Copy Files to new Session, if is Relative convert Names else leave as is
     if DigIt_Progress.ProgressSetTotal(rsSavingSources, 1) then exit;
     for i:=0 to lenSources-1 do
     begin
       curFileNameR:= FullPathToRelativePath(Path_Session, SourceFiles[i].fName, isRelative);

       if isRelative
       then begin
              curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
              ForceDirectories(ExtractFilePath(curFileName));
              CopyFile(SourceFiles[i].fName, curFileName, True, True);
              if isMove then DeleteFile(SourceFiles[i].fName);

              fileSources[i]:= curFileName;
            end
       else begin
              {#todo -oMaxM : User may select if copy the file inside the Session Scan Folder Yes,No,YesAll,NoAll }
              fileSources[i]:= SourceFiles[i].fName;
            end;

       Application.ProcessMessages; if DigIt_Progress.Cancelled then exit;
     end;

     if DigIt_Progress.ProgressSetTotal(rsSavingCaptured, 2) then exit;
     for i:=0 to lenCaptured-1 do
     begin
       curFileNameR:= FullPathToRelativePath(Path_Session, CapturedFiles[i].fName, isRelative);

       if isRelative
       then begin
              curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
              ForceDirectories(ExtractFilePath(curFileName));
              CopyFile(CapturedFiles[i].fName, curFileName, True, True);
              if isMove then DeleteFile(CapturedFiles[i].fName);

              fileCaptured[i]:= curFileName;
            end
       else begin
              {#todo -oMaxM : User may select if copy the file inside the Session Pictures Folder Yes,No,YesAll,NoAll }
              fileCaptured[i]:= CapturedFiles[i].fName;
            end;

       Application.ProcessMessages; if DigIt_Progress.Cancelled then exit;
     end;

     if DigIt_Progress.ProgressSetTotal(rsSavingSessionFiles, 3) then exit;

     //Copy Loaded File
     curFileNameR:= FullPathToRelativePath(Path_Session, LoadedFile, isRelative);
     if isRelative then
     begin
       curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
       ForceDirectories(ExtractFilePath(curFileName));
       CopyFile(LoadedFile, curFileName, True, False);
       if isMove then DeleteFile(LoadedFile);

       LoadedFile:= curFileName;
     end;

     Application.ProcessMessages; if DigIt_Progress.Cancelled then exit;

     //Copy AutoSave Files
     curFileName:=Path_Session+Session_File+Ext_AutoSess;
     if FileExists(curFileName) then
     begin
       CopyFile(curFileName, newPath_Session+newSession_File+Ext_AutoSess, True, False);

       //if Path is Default then we are saving a new Session, we must move the autoSave Files
       if isMove then DeleteFile(curFileName);
     end;
     curFileName:=Path_Session+Session_File+Ext_AutoThumb;
     if FileExists(curFileName) then
     begin
       CopyFile(curFileName, newPath_Session+newSession_File+Ext_AutoThumb, True, False);
       if isMove then DeleteFile(curFileName);
     end;

     //if is Move then Delete old Session Files
     if isMove then
     begin
       DeleteFile(Path_Session+Session_File+Ext_Thumb);
       DeleteFile(Path_Session+Session_File+Ext_Sess);
     end;

     if DigIt_Progress.ProgressSetTotal(rsSavingSwitch, 4) then exit;

     //Populate the Files Arrays with new Filenames
     for i:=0 to lenSources-1 do
       SourceFiles[i].fName:= fileSources[i];

     for i:=0 to lenCaptured-1 do
       CapturedFiles[i].fName:= fileCaptured[i];

     //If is Relative convert Paths to new Session, else leave as is
     curFileNameR:= FullPathToRelativePath(Path_Session, Path_Session_Pictures, isRelative);
     if isRelative then Path_Session_Pictures:= RelativePathToFullPath(newPath_Session, curFileNameR);

     curFileNameR:= FullPathToRelativePath(Path_Session, Path_Session_Scan, isRelative);
     if isRelative then Path_Session_Scan:= RelativePathToFullPath(newPath_Session, curFileNameR);

     //Switch to New Session
     Path_Session:= newPath_Session;
     Session_File:= ExtractFileNameWithoutExt(ExtractFileName(AFileName));
     imgListThumb_Changed:= True;

     XML_SaveWork(False);

     if (Path_Session = Path_DefSession)
     then Caption :='DigIt'
     else Caption :='DigIt'+' - '+Session_File;

     DigIt_Progress.ProgressSetTotal(rsSavingDone, 5);
     Result:= True;

  except
    on E: Exception do
      MessageDlg('DigIt', Format(rsErrSaveWork, [AFileName, E.Message]), mtError, [mbOk], 0);
  end;

  finally
    if DigIt_Progress.Cancelled then
    begin
      dlgRes:= mrCancel;
    end;

    fileSources:= nil;
    fileCaptured:= nil;
    DigIt_Progress.Hide;
  end;
end;

function TDigIt_Main.XML_LoadSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
var
   aFree: Boolean;
   newSourceName: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     Result:= -1;

     //Load rSource and its Params
     newSourceName:= aXML.GetValue('Source/Name', '');

     if theBridge.SourcesImpl.Select(newSourceName)
     then begin
            if (theBridge.SourcesImpl.Selected <> rSource) then
            begin
              { #note -oMaxM : rSource Switched...Do something? }
            end;

            rSource:= theBridge.SourcesImpl.Selected;
            rSourceName:= theBridge.SourcesImpl.SelectedName;
            rSourceParams:= theBridge.SourcesImpl.SelectedParams;
            Result:= theBridge.SourcesImpl.SelectedIndex;
            theBridge.SourcesImpl.LoadSelectedParams(aXML.Filename, 'Source/Params');
          end
     else begin
            MessageDlg('DigIt', Format(rsSourceNotFound, [newSourceName]), mtInformation, [mbOk], 0);
            Result:= theBridge.SourcesImpl.SelectedIndex;
          end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   curExt: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree then
     begin
       if IsAutoSave
       then curExt:= Ext_AutoSess
       else curExt:= Ext_Sess;

       aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+curExt);
     end;

     //Save rSource and its Params
     aXML.SetValue('Source/Name', rSourceName);
     aXML.DeletePath('Source/Params/');

     if IsAutoSave then
     begin
       SessionModified:= True;

       if aFree then
       begin
         aXML.Free; aXML:= nil;

         //FPC Bug?
         //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
         //So we do it after destroying XML.

         if (rSource <> nil) then rSource^.Inst.Params.Save(PChar(Path_Session+Session_File+curExt), 'Source/Params');
       end;
     end;

  finally
    if aFree and (aXML <> nil) then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   curItemPath: String;
   i, iCount: Integer;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     //Load SourceFiles
     SourceFiles:= nil; //Avoid possible data overlaps by eliminating any existing array
     iCount:= aXML.GetValue(XML_SourceFiles+'Count', 0);
     iSourceFiles:= aXML.GetValue(XML_SourceFiles+'iSourceFiles', -1);
     lastCropped:= aXML.GetValue(XML_SourceFiles+'lastCropped', -1);
     lastLenTaked:= aXML.GetValue(XML_SourceFiles+'lastLenTaked', 0);
     SetLength(SourceFiles, iCount);
     for i:=0 to iCount-1 do
     begin
       curItemPath :=XML_SourceFiles+'Item' + IntToStr(i)+'/';
       SourceFiles[i].fCrop:= aXML.GetValue(curItemPath+'fCrop', False);
       SourceFiles[i].fName:= RelativePathToFullPath(Path_Session, aXML.GetValue(curItemPath+'fName', ''));
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   i: Integer;
   curItemPath: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     //Save SourceFiles array
     aXML.DeletePath(XML_SourceFiles);
     aXML.SetValue(XML_SourceFiles+'Count', Length(SourceFiles));
     aXML.SetValue(XML_SourceFiles+'iSourceFiles', iSourceFiles);
     aXML.SetValue(XML_SourceFiles+'lastCropped', lastCropped);
     aXML.SetValue(XML_SourceFiles+'lastLenTaked', lastLenTaked);
     for i:=0 to Length(SourceFiles)-1 do
     begin
       curItemPath :=XML_SourceFiles+'Item' + IntToStr(i)+'/';
       aXML.SetValue(curItemPath+'fCrop', SourceFiles[i].fCrop);
       aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(Path_Session, SourceFiles[i].fName));
     end;

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

function TDigIt_Main.XML_LoadDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     //Load Destination and its Params
     { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
 (*
     newDestinationName:= aXML.GetValue('Destination/Name', '');
     if (newDestinationName = '')
     then begin
            rDestination:= nil;
            rDestinationParams:= nil;
 *)
            rDestinationName:= '';
            Path_Session_Pictures:= RelativePathToFullPath(Path_Session, aXML.GetValue('Destination/Params/Path', ''));

            //Load Format and Create Writer
            SaveFormat:= ifJpeg;
            aXML.GetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
            SetSaveWriter(SaveFormat);
            aXML.ReadObject('Destination/Params/Writer/', SaveWriter);
            Result:= 0;
 (*
          end
     else begin
            if theBridge.DestinationsImpl.Select(newDestinationName) then
            begin
              rDestination:= theBridge.DestinationsImpl.Selected;
              rDestinationName:= theBridge.DestinationsImpl.SelectedName;
              rDestinationParams:= theBridge.DestinationsImpl.SelectedParams;
              Result:= theBridge.DestinationsImpl.SelectedIndex+1;
              theBridge.DestinationsImpl.LoadSelectedParams(aXML.Filename, 'Destination/Params');
            end;
          end;
 *)

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     //Save rDestination and its Params
(*     aXML.SetValue('Destination/Name', rDestinationName);
     if (rDestination = nil) then
     begin
*)
       aXML.DeletePath('Destination/Params/');
       aXML.SetValue('Destination/Params/Path', FullPathToRelativePath(Path_Session, Path_Session_Pictures));

       aXML.SetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
       aXML.WriteObject('Destination/Params/Writer/', SaveWriter);

//     end;

    if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   i,
   imgCount,
   newCount,
   newSelected: Integer;
   curAge: Longint;
   curExt,
   cuFileName,
   curItemPath: String;
   curItem: TListItem;
   imgListCountChanged: Boolean;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     if IsAutoSave
     then curExt:= Ext_AutoThumb
     else curExt:= Ext_Thumb;

     newCount := aXML.GetValue(XML_CapturedFiles+'Count', 0);
     iCapturedFiles:= aXML.GetValue(XML_CapturedFiles+'iCapturedFiles', -1);
     newSelected :=aXML.GetValue(XML_CapturedFiles+'Selected', -1);

     lvCaptured.BeginUpdate;
     lvCaptured.Clear;

     if FileExists(Path_Session+Session_File+curExt)
     then try
             imgListThumb.Clear;
             imgListThumb.LoadFromFile(Path_Session+Session_File+curExt);
             imgListCountChanged:= ((imgListThumb.Count-1) <> newCount); //0 is reserved for No File

          except
            imgListCountChanged:= True;
          end
     else imgListCountChanged:= True;

     SetLength(CapturedFiles, newCount);
     for i:=0 to newCount-1 do
     begin
         curItemPath :=XML_CapturedFiles+'Item' + IntToStr(i)+'/';
         CapturedFiles[i].fAge:= aXML.GetValue(curItemPath+'fAge', 0);
         CapturedFiles[i].fName:= RelativePathToFullPath(Path_Session, aXML.GetValue(curItemPath+'fName', ''));
         CapturedFiles[i].iIndex:= aXML.GetValue(curItemPath+'iIndex', 0);

         cuFileName:= CapturedFiles[i].fName;
         curItem:= lvCaptured.Items.Add;
         curItem.Caption:= ExtractFileName(cuFileName);

         if FileExists(cuFileName)
         then begin
                curAge:= FileAge(cuFileName);

                //Thumbs file don't exists we must add the image
                if imgListCountChanged then CapturedFiles[i].iIndex:= -1;

                //File is Changed, update the ImageList
                if (CapturedFiles[i].iIndex <= 0) or (CapturedFiles[i].fAge <> curAge) then
                begin
                  CapturedFiles[i].fAge:= curAge;

                  UI_ThumbnailUpdate(i, cuFileName);
                end;

                curItem.ImageIndex:= CapturedFiles[i].iIndex;
              end
         else curItem.ImageIndex:= 0; //CapturedFiles[i].iIndex :=0;
     end;

     if (newSelected > -1) and (newSelected < newCount)
     then begin
            lvCaptured.Selected:= lvCaptured.Items[newSelected];
            lvCaptured.Selected.MakeVisible(False);
          end
     else if (lvCaptured.Items.Count > 0) then lvCaptured.Items[lvCaptured.Items.Count-1].MakeVisible(False);

     lvCaptured.EndUpdate;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   i,
   lenCapturedFiles: Integer;
   curExt,
   curItemPath: String;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     if IsAutoSave
     then curExt:= Ext_AutoThumb
     else curExt:= Ext_Thumb;

     if imgListThumb_Changed then
     try
       imgListThumb.SaveToFile(Path_Session+Session_File+curExt);
       imgListThumb_Changed:= False;

     except
     end;

     //Save CapturedFiles array
     aXML.DeletePath(XML_CapturedFiles);
     lenCapturedFiles:= Length(CapturedFiles);

     if (lenCapturedFiles > 0) then
     begin
       aXML.SetValue(XML_CapturedFiles+'Count', Length(CapturedFiles));
       aXML.SetValue(XML_CapturedFiles+'iCapturedFiles', iCapturedFiles);

       if lvCaptured.Selected=nil
       then aXML.DeleteValue(XML_CapturedFiles+'Selected')
       else aXML.SetValue(XML_CapturedFiles+'Selected', lvCaptured.Selected.Index);

       for i:=0 to Length(CapturedFiles)-1 do
       begin
         curItemPath :=XML_CapturedFiles+'Item' + IntToStr(i)+'/';
         aXML.SetValue(curItemPath+'fAge', CapturedFiles[i].fAge);
         aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(Path_Session, CapturedFiles[i].fName));
         aXML.SetValue(curItemPath+'iIndex', CapturedFiles[i].iIndex);
       end;
     end;

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     LoadImage(RelativePathToFullPath(Path_Session, aXML.GetValue('LoadedFile', '')), False); //DON'T set to True, if you do not want infinite recursion

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     aXML.SetValue('LoadedFile', FullPathToRelativePath(Path_Session, LoadedFile));

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSource_CapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     XML_SaveSourceFiles(aXML, IsAutoSave);
     XML_SaveCapturedFiles(aXML, IsAutoSave);
     XML_SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSource_CapturedIndexes(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     if (Length(SourceFiles) > 0) then
     begin
       aXML.SetValue(XML_SourceFiles+'iSourceFiles', iSourceFiles);
       aXML.SetValue(XML_SourceFiles+'lastCropped', lastCropped);
       aXML.SetValue(XML_SourceFiles+'lastLenTaked', lastLenTaked);
     end;

     if (Length(CapturedFiles) > 0) then aXML.SetValue(XML_CapturedFiles+'iCapturedFiles', iCapturedFiles);

     if lvCaptured.Selected=nil
     then aXML.DeleteValue(XML_CapturedFiles+'Selected')
     else aXML.SetValue(XML_CapturedFiles+'Selected', lvCaptured.Selected.Index);

     XML_SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   newCropMode: TDigItCropMode;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
             then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
             else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     newCropMode:= TDigItCropMode(aXML.GetValue('CropMode', 0));
     setCropMode(newCropMode);
     if (newCropMode = diCropCustom) then
     begin
       UI_FillCounters;
       imgManipulation.CropAreas.Load(aXML, 'CropAreas');
       cbCounterList.ItemIndex :=aXML.GetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     aXML.SetValue('CropMode', Integer(CropMode));
     if (CropMode = diCropCustom)
     then imgManipulation.CropAreas.Save(aXML, 'CropAreas')
     else aXML.DeletePath('CropAreas');

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   selButton: Integer;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     selButton := aXML.GetValue(XML_PageSettings+'Rotate', -1);
     Case selButton of
     0: btPRotateLeft.Down:= True;
     1: btPRotateRight.Down:= True;
     2: btPRotate180.Down:= True;
     else begin
            btPRotateLeft.Down:= False;
            btPRotateRight.Down:= False;
            btPRotate180.Down:= False;
          end;
     end;

     selButton := aXML.GetValue(XML_PageSettings+'Flip', -1);
     Case selButton of
     0: btPFlipV.Down:= True;
     1: btPFlipH.Down:= True;
     else begin
            btPFlipV.Down:=  False;
            btPFlipH.Down:= False;
          end;
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SavePageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     if btPRotateLeft.Down
     then aXML.SetValue(XML_PageSettings+'Rotate', 0)
     else
     if btPRotateRight.Down
     then aXML.SetValue(XML_PageSettings+'Rotate', 1)
     else
     if btPRotate180.Down
     then aXML.SetValue(XML_PageSettings+'Rotate', 2)
     else aXML.SetValue(XML_PageSettings+'Rotate', -1);

     if btPFlipV.Down
     then aXML.SetValue(XML_PageSettings+'Flip', 0)
     else
     if btPFlipH.Down
     then aXML.SetValue(XML_PageSettings+'Flip', 1)
     else aXML.SetValue(XML_PageSettings+'Flip', -1);

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     //User Interface
     rollCrops.Collapsed:=aXML.GetValue('UI/rollCrops_Collapsed', False);
     rollPages.Collapsed:=aXML.GetValue('UI/rollPages_Collapsed', True);
     rollCounters.Collapsed:=aXML.GetValue('UI/rollCounters_Collapsed', True);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     //User Interface
     aXML.SetValue('UI/rollCrops_Collapsed', rollCrops.Collapsed);
     aXML.SetValue('UI/rollPages_Collapsed', rollPages.Collapsed);
     aXML.SetValue('UI/rollCounters_Collapsed', rollCounters.Collapsed);

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_OptionsLoad;
var
   aXML: TRttiXMLConfig;

begin
  try
     aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

  finally
    aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_OptionsSave;
var
   aXML: TRttiXMLConfig;

begin
  try
     aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

  finally
    aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_OptionsLoad_Session(aXML: TRttiXMLConfig; var APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

     APath:= SetDirSeparators(aXML.GetValue('Session/Path', ''));
     AFile:= SetDirSeparators(aXML.GetValue('Session/File', ''));

  finally
     if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_OptionsSave_Session(aXML: TRttiXMLConfig; const APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

     aXML.SetValue('Session/Path', APath);
     aXML.SetValue('Session/File', AFile);

  finally
     if aFree then aXML.Free;
  end;
end;

function TDigIt_Main.Source_Select(newSourceIndex: Integer): Boolean;
begin
  Result:= False;
  try
     if theBridge.SourcesImpl.Select(newSourceIndex, True)
     then begin
            if (theBridge.SourcesImpl.Selected <> rSource) then
            begin
              { #note -oMaxM : rSource Switched...Do something? }
            end;

            rSource:= theBridge.SourcesImpl.Selected;
            rSourceName:= theBridge.SourcesImpl.SelectedName;
            rSourceParams:= theBridge.SourcesImpl.SelectedParams;

            XML_SaveSource(nil, True);

            Result:= True;
          end
     else MessageDlg('DigIt', Format(rsSourceNotSelected, [newSourceIndex]), mtInformation, [mbOk], 0);

  except
    Result:= False;
  end;
end;

function TDigIt_Main.Destination_Select(newDestinationIndex: Integer): Boolean;
begin
  Result:= False;
  try
     if (newDestinationIndex = 0)
     then begin
            { #note 10 -oMaxM : Use of this function should be removed when SaveFiles is implemented as a descendant of IDigIt_Destination }
            if TDest_SaveFiles_Settings.Execute(SaveFormat, SaveWriter, Path_Session_Pictures)
            then XML_SaveDestination(nil, True);

            //this way we know it is SaveAs Destination
            //rDestination:= nil;
            //rDestinationParams:= nil;
            rDestinationName:= '';

            Result:= True;
          end
     else begin
          (*
            if theBridge.DestinationsImpl.Select(newDestinationIndex, True) then
            begin
              if (theBridge.DestinationsImpl.Selected <> rDestination) then
              begin
                { #note -oMaxM : rDestination Switched...Do something? }
             end;

             rDestination:= theBridge.DestinationsImpl.Selected;
             rDestinationName:= theBridge.DestinationsImpl.SelectedName;
             rDestinationParams:= theBridge.DestinationsImpl.SelectedParams;
             Result:= True;
            end;
           *)
          end;

  except
    Result:= False;
  end;
end;

procedure TDigIt_Main.edNameChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
  CropArea.Name :=edCropName.Text;
end;

procedure TDigIt_Main.btBox_AddClick(Sender: TObject);
var
   newCropArea :TCropArea;

begin
  if edCropUnit_Type.ItemIndex=0
  then newCropArea :=imgManipulation.addCropArea(Rect(50, 50, 100, 100))
  else newCropArea :=imgManipulation.addCropArea(Rect(1, 1, 2, 2), TResolutionUnit(edCropUnit_Type.ItemIndex));

  newCropArea.BorderColor :=VGALime;
  edCropName.SetFocus;
end;

procedure TDigIt_Main.btBox_DelClick(Sender: TObject);
var
   CropArea :TCropArea;
   curIndex :Integer;

begin
  curIndex :=cbBoxList.ItemIndex;
  if (curIndex>-1) then
  begin
    CropArea :=TCropArea(cbBoxList.Items.Objects[curIndex]);
    imgManipulation.delCropArea(CropArea);
    cbBoxList.ItemIndex:=cbBoxList.Items.IndexOfObject(imgManipulation.SelectedCropArea);
  end;
  UI_FillBox(imgManipulation.SelectedCropArea);
end;

procedure TDigIt_Main.cbBoxListChange(Sender: TObject);
begin
   imgManipulation.SelectedCropArea :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
end;

procedure TDigIt_Main.edCropHeightChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Height:=edCropHeight.Value;
end;

procedure TDigIt_Main.edCropLeftChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Left :=edCropLeft.Value;
end;

procedure TDigIt_Main.edCropTopChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Top :=edCropTop.Value;
end;

procedure TDigIt_Main.edCropWidthChange(Sender: TObject);
var
   CropArea :TCropArea;

begin
  if inFillBoxUI then exit;

  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil)
  then CropArea.Width:=edCropWidth.Value;
end;

procedure TDigIt_Main.menuDebugClick(Sender: TObject);
begin
  Case TMenuItem(Sender).Tag of
    0: XML_LoadWork(True);
    1: XML_SaveWork(True);
    2: XML_ClearWork(False);
  end;
end;

procedure TDigIt_Main.menuImageFormatClick(Sender: TObject);
begin
  TBGRAFormatUIContainer.Execute(SaveFormat, SaveWriter);
end;

procedure TDigIt_Main.rgCropAspectSelectionChanged(Sender: TObject);
begin
  if changingAspect then Exit;

  Case rgCropAspect.ItemIndex of
  0 : imgManipulation.SelectedCropArea.KeepAspectRatio:=bParent;
  1 : imgManipulation.SelectedCropArea.KeepAspectRatio:=bFalse;
  2 : begin
           imgManipulation.SelectedCropArea.KeepAspectRatio:=bTrue;
           imgManipulation.SelectedCropArea.AspectRatio:=edCropAspectPersonal.Text;
       end;
  end;
end;

procedure TDigIt_Main.btCropApplyAspectRatioClick(Sender: TObject);
begin
   if imgManipulation.SelectedCropArea.KeepAspectRatio=bTrue
   then imgManipulation.SelectedCropArea.AspectRatio:=edCropAspectPersonal.Text
   else begin
             imgManipulation.SelectedCropArea.KeepAspectRatio:=bTrue;
             imgManipulation.SelectedCropArea.AspectRatio:=edCropAspectPersonal.Text;
             changingAspect :=True;
             rgCropAspect.ItemIndex :=2;
             changingAspect :=False;
        end;
end;

procedure TDigIt_Main.AddedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
var
  curIndex :Integer;

begin
   curIndex :=imgManipulation.CropAreas.IndexOf(CropArea);

   if (CropArea.Name='') then CropArea.Name:='Name '+IntToStr(curIndex);

   CropArea.Icons:=[cIcoIndex];

   cbBoxList.AddItem(CropArea.Name, CropArea);
   cbBoxList.ItemIndex:=cbBoxList.Items.IndexOfObject(CropArea);

   //If there is only one CropArea add a Counter by Default
   if (imgManipulation.CropAreas.Count=1) then
   begin
     if (Counters.Count=0)
     then btCropCounter_AddClick(nil);
   end;

   //If there is only one Counter then the new Area have it by Default
   if (Counters.Count=1)
   then CropArea.UserData:=0;

   if not(XML_Loading) then
   begin
     UI_FillBox(CropArea);
     UI_ToolBar;
   end;
end;

procedure TDigIt_Main.DeletedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
var
   delIndex :Integer;
begin
  try
    if not(Closing) then
    begin
         delIndex :=cbBoxList.Items.IndexOfObject(CropArea);
         if (delIndex<>-1)
         then cbBoxList.Items.Delete(delIndex);
         panelCropArea.Enabled:=(cbBoxList.Items.Count>0);

         if not(XML_Loading) then UI_ToolBar;
    end;
  except
  end;
end;

procedure TDigIt_Main.ChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
begin
  if (cbBoxList.ItemIndex > -1) and (cbBoxList.Items.Objects[cbBoxList.ItemIndex] = CropArea) then
  begin
    UI_FillBox(CropArea);

    //if the Name is Changed change in the comboListbox
    if (CropArea.Name<>cbBoxList.Items.Strings[cbBoxList.ItemIndex])
    then cbBoxList.Items.Strings[cbBoxList.ItemIndex] :=CropArea.Name;
  end;
end;

procedure TDigIt_Main.SelectedChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
var
   newIndex :Integer;
begin
   if (imgManipulation.SelectedCropArea <> nil)
   then newIndex :=cbBoxList.Items.IndexOfObject(imgManipulation.SelectedCropArea)
   else newIndex :=-1;

   cbBoxList.ItemIndex:=newIndex;
   UI_FillBox(imgManipulation.SelectedCropArea);
end;

procedure TDigIt_Main.tbCapturedPDFClick(Sender: TObject);
begin
  TDigIt_ExportFiles.Execute(Application.Title, CapturedFiles, True);
end;

procedure TDigIt_Main.setCropMode(ANewCropMode: TDigItCropMode);
begin
  if (ANewCropMode <> CropMode) then
  begin
    Case ANewCropMode of
      diCropFull: begin
        if (CropMode = diCropCustom) then
        begin
          if (imgManipulation.CropAreas.Count > 0)
          then if (MessageDlg('DigIt', rsClearCrops, mtConfirmation, mbYesNo, 0) = mrNo)
               then exit;
        end;

        tbCrop.Visible:= False;
        imgManipulation.clearCropAreas;
        imgManipulation.Opacity:= 0;
        imgManipulation.Enabled:= False;
        rollCrops.Enabled:= False; rollCrops.Collapsed:= True;
        rollCounters.Collapsed:= True;

        panelCounterList.Enabled:= False;

        if (Counters.Count = 0)
        then Counters.Add('Counter 0')
        else Counters.RemoveAllButFirst;

        tbCropMode.Caption:= rsCropFull;
      end;
      diCropCustom: begin
        tbCrop.Visible:= True;
        imgManipulation.Opacity:= 128;
        imgManipulation.Enabled:= True;
        rollCrops.Enabled:= True; rollCrops.Collapsed:= False;
        rollCounters.Collapsed:= False;

        panelCounterList.Enabled:= True;

        tbCropMode.Caption:= rsCropCust;
      end;
    end;

    CropMode:= ANewCropMode;

    if not(XML_Loading) then
    begin
      UI_FillCounters;
      UI_FillCounter(nil);
      UI_ToolBar;
    end;
   end;
  tbCropMode.ImageIndex:= Integer(CropMode);
end;

(*
function TDigIt_Main.WaitForAFile(AFileName:String; ATimeOut: Integer): Boolean;
var
   timeStart, timeCur:DWord;

begin
  if (AFileName='') then exit;

  timeStart :=GetTickCount;
  Result:= FileExists(AFileName);
  while not(Result) and ((timeCur-timeStart) < ATimeOut) do
  begin
    Application.ProcessMessages;
    timeCur :=GetTickCount;
    Result:= FileExists(AFileName);
  end;
end;
*)

function TDigIt_Main.SourceFiles_Add(AArray: IDigIt_ROArray): Integer;
var
   oldLength, i: Integer;
   curImageFile: PChar;

begin
  Result:= 0;
  if (AArray <> nil) then
  begin
    //Add files to end of Array SourceFiles
    oldLength:= Length(SourceFiles);
    Result:= AArray.GetCount;
    if (Result > 0) then
    begin
      lastLenTaked:= Result;

      SetLength(SourceFiles, oldLength+Result);
      for i:=0 to Result-1 do
      begin
        if AArray.Get(i, curImageFile) then
        begin
          SourceFiles[oldLength+i].fCrop:= False;
          SourceFiles[oldLength+i].fName:= curImageFile;
          StrDispose(curImageFile);
        end;
      end;
    end;
  end;
end;

function TDigIt_Main.SourceFiles_Add(AFileName: String): Integer;
var
   oldLength: Integer;

begin
  Result:= 0;
  if (AFileName <> '') then
  begin
    //Add files to end of Array SourceFiles
    oldLength:= Length(SourceFiles);
    SetLength(SourceFiles, oldLength+1);
    SourceFiles[oldLength].fCrop:= False;
    SourceFiles[oldLength].fName:= AFileName;
    Result:= 1;
    lastLenTaked:= 1;
  end;
end;

function TDigIt_Main.CropFile_Full(AFileName: String): Boolean;
begin
  Result:= (AFileName <> '') and LoadImage(AFileName, False);
  if Result then SaveCallBack(imgManipulation.Bitmap, nil, 0);
end;

procedure TDigIt_Main.CropFile_Full(AStartIndex: Integer);
var
   i,
   c,
//   old_lastCropped,
   old_CounterValue,
   old_iCapturedFiles: Integer;
   cStr: String;
   UserCancel: Boolean;

begin
  try
     UserCancel:= False;

     //Store old Values so if user Cancel Operation we can rollback
     old_iCapturedFiles:= iCapturedFiles;
     old_CounterValue:= Counters[0].Value;
     //old_lastCropped:= lastCropped;

     c:= Length(SourceFiles);
     cStr:= IntToStr(c);
     DigIt_Progress.progressTotal.Min:= AStartIndex;
     DigIt_Progress.progressTotal.Max:= c;

     for i:=AStartIndex to c-1 do
     begin
       DigIt_Progress.progressTotal.Position:= i;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [i, cStr]);

       Application.ProcessMessages;

       UserCancel:= DigIt_Progress.Cancelled or not(CropFile_Full(SourceFiles[i].fName));
       if UserCancel then break;

       //SourceFiles[i].fCrop:= True;
       //if (i > lastCropped) then lastCropped:= i;
       //iSourceFiles:= i;

       DigIt_Progress.progressTotal.Position:= i+1;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [i, cStr]);

       Application.ProcessMessages; if DigIt_Progress.Cancelled then break;
     end;

     if UserCancel and
        (MessageDlg('DigIt', rsErrIncomplete, mtConfirmation, [mbYes, mbNo], 0)=mrNo) then
     begin
       for i:=old_iCapturedFiles+1 to iCapturedFiles do
       begin
         DeleteFile(CapturedFiles[i].fName);

         if (imgListThumb.Count > 1) then imgListThumb.Delete(imgListThumb.Count-1);
         if (lvCaptured.Items.Count > 0) then lvCaptured.Items.Delete(lvCaptured.Items.Count-1);
       end;
       imgListThumb_Changed:= True;
       SetLength(CapturedFiles, old_iCapturedFiles+1);
       iCapturedFiles:= old_iCapturedFiles;
       Counters[0].Value:= old_CounterValue;
     end;

  finally
    UI_ToolBar;
    UI_FillCounter(nil);
  end;
end;

procedure TDigIt_Main.Pages_InsertMiddle(ASourceFileIndex: Integer);
begin
  { #todo 5 -oMaxM : Re index the crops starting from the lastCropped }
  MessageDlg(rsNotImpl, mtInformation, [mbOk], 0);
end;

procedure TDigIt_Main.SetSaveWriter(AFormat: TBGRAImageFormat);
begin
  SaveFormat:= AFormat;
  try
     if (SaveWriter <> nil) then SaveWriter.Free;
     SaveWriter:= BGRABitmapTypes.CreateBGRAImageWriter(SaveFormat, True);
     SaveExt:= BGRABitmapTypes.SuggestImageExtension(SaveFormat);

  except
    SaveFormat:= ifJpeg;
    SaveWriter:= nil;
    SaveExt:= 'jpg';
  end;

  if (SaveWriter <> nil) then
  begin
    if (SaveWriter is TBGRAWriterJPEG) then TBGRAWriterJPEG(SaveWriter).CompressionQuality:= 100;
  end;
end;

procedure TDigIt_Main.SetDefaultStartupValues;
begin
  SourceFiles:= nil;
  iSourceFiles:= -1;
  CapturedFiles:= nil;
  iCapturedFiles:= -1;
  lastCropped:= -1;
  lastLenTaked:= 0;

  LoadedFile:= '';
  CropMode:= diCropNull; //setCropMode works only if there are changes
end;

procedure TDigIt_Main.SetDefaultSessionValues;
begin
  Path_Session:= Path_DefSession;
  Path_Session_Scan:= Path_DefSession_Scan;
  Path_Session_Pictures:= Path_DefSession_Pictures;
  Session_File:= File_DefSession;
  rSessionModified:= False;
end;

procedure TDigIt_Main.ClearCaptured;
var
   nofileBMP: TBitmap;

begin
  try
     nofileBMP:=TBitmap.Create;

     //Clear Array and ListView
     iCapturedFiles:= -1;
     CapturedFiles:= nil;
     lvCaptured.Clear;

     //Get the Fixed Thumbnail NoFile and re-add it at position 0
     imgListThumb.GetBitmap(0, nofileBMP);
     imgListThumb.Clear;
     imgListThumb.Add(noFileBMP, nil);
     imgListThumb_Changed:= True;

  finally
    noFileBMP.Free;
  end;
end;

function TDigIt_Main.GetCurrentCropArea: TCropArea;
begin
  if (cbBoxList.ItemIndex<0)
  then Result :=nil
  else Result :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
end;

function TDigIt_Main.Counters_GetCurrent: TDigIt_Counter;
begin
  Result:= nil;

  Case CropMode of
    diCropFull: begin
      Result:= Counters[0];
    end;
    else if (cbCounterList.ItemIndex > -1)
         then Result:= TDigIt_Counter(cbCounterList.Items.Objects[cbCounterList.ItemIndex]);
  end;
end;

procedure TDigIt_Main.Counters_Dec;
var
   i: Integer;
   cropCounter :TDigIt_Counter;
   cropArea: TCropArea;

begin
  for i:= 0 to imgManipulation.CropAreas.Count-1 do
  begin
    cropArea:= imgManipulation.CropAreas[i];
    //Decrement the Counter Value
    cropCounter :=TDigIt_Counter(Counters.items[cropArea.UserData]);
    cropCounter.Value:=cropCounter.Value-1;
    dec(iCapturedFiles);
  end;
end;

procedure TDigIt_Main.Counters_Inc;
var
   i: Integer;
   cropCounter :TDigIt_Counter;
   cropArea: TCropArea;

begin
  for i:= 0 to imgManipulation.CropAreas.Count-1 do
  begin
    cropArea:= imgManipulation.CropAreas[i];
    //Increment the Counter Value
    cropCounter :=TDigIt_Counter(Counters.items[cropArea.UserData]);
    cropCounter.Value:=cropCounter.Value+1;
    inc(iCapturedFiles);
  end;
end;

procedure TDigIt_Main.SetSessionModified(AValue: Boolean);
var
   addStr: String;

begin
  if (rSessionModified = AValue) then exit;

  rSessionModified:= AValue;

  if rSessionModified then addStr:= '* ' else  addStr:= '';

  if (Path_Session = Path_DefSession)
  then Caption:= addStr+'DigIt'
  else Caption:= addStr+'DigIt'+' - '+Session_File;
end;

procedure TDigIt_Main.UI_ToolBar;
var
   bCommonCond: Boolean;
   lenSources,
   lenCaptured,
   remSources: Integer;

begin
  bCommonCond:= (rSource<>nil) and (rSource^.Inst <> nil);
  actPreview.Enabled:= bCommonCond;

  lenCaptured:= Length(CapturedFiles);

  if (CropMode = diCropCustom)
  then begin
         lenSources:= Length(SourceFiles);
         bCommonCond:= bCommonCond and
                       not(imgManipulation.Empty) and (imgManipulation.CropAreas.Count > 0) and
                       DirectoryExists(Path_Session_Pictures);

         actTake.Enabled:= bCommonCond;
         actCrop.Enabled:= bCommonCond and
                           ((lenSources = 1) or (iSourceFiles >= 0) and (iSourceFiles >= lenSources-1));
         actCropNext.Enabled:= bCommonCond and (lenSources > 1) and (iSourceFiles >= 0) and (iSourceFiles < lenSources-1);
         actCropGoNext.Enabled:= actCropNext.Enabled;
         actCropGoBack.Enabled:= bCommonCond and (lenSources > 1) and (iSourceFiles > 0) and (iSourceFiles <= lenSources);
         actCropAll.Enabled:= actCropNext.Enabled and not(actCrop.Enabled);

         actClearQueue.Enabled:= bCommonCond and (lenSources > 0);
         if (lenSources > 0)
         then tbClearQueue.Caption:= actClearQueue.Caption+' ('+IntToStr(lenSources)+')'
         else tbClearQueue.Caption:= actClearQueue.Caption;

         if actCrop.Enabled
         then tbCropNext.Action:= actCrop
         else tbCropNext.Action:= actCropNext;

         tbCropSummary.Visible:= bCommonCond and (lenSources > 0);
         if tbCropSummary.Visible then
         begin
           remSources:= lenSources-iSourceFiles;
           tbCropSummary.Caption:= Format(rsCropToDo, [remSources]);
         end;

         actCapturedDelete.Enabled:= False; { #todo 2 -oMaxM : evaluate the complexity in case of multiple crop areas}
       end
  else begin
         actTake.Enabled:= bCommonCond and DirectoryExists(Path_Session_Pictures);

         actCapturedDelete.Enabled:= (lenCaptured > 0) and (lvCaptured.Selected <> nil);
       end;

  actTimerTake.Enabled:= actTake.Enabled;
  actTakeBuildDuplex.Enabled:= actTake.Enabled;
  actTakeRe.Enabled:= actTake.Enabled and (lastLenTaked > 0);

  //Captured Toolbar
  actCapturedDeleteAll.Enabled:= (lenCaptured > 0);
  actCapturedRotateLeft.Enabled:= (lenCaptured > 0) and (lvCaptured.Selected <> nil);
  actCapturedRotateRight.Enabled:= actCapturedRotateLeft.Enabled;
  tbCapturedPDF.Enabled:= (lenCaptured > 0);
end;

procedure TDigIt_Main.UI_ToolBarMods;
var
   i: Integer;
   curBtn: TToolButton;
   curAct: TAction;
   st:String;

begin
  for i:=0 to ActionListMain.ActionCount-1 do
  begin
    curAct:= TAction(ActionListMain.Actions[i]);
    if (curAct <> nil) and (curAct.ShortCut <> 0)
    then curAct.Caption:= curAct.Caption+' ('+ShortCutToText(curAct.ShortCut)+')';
  end;
  tbMain.AdjustSize;
  tbCrop.AdjustSize;
  tbCrop.Left:= tbMain.Left+tbMain.Width+40;

  for i:=0 to tbCaptured.ButtonCount-1 do
  begin
    curBtn:= tbCaptured.Buttons[i];
    if (curBtn <> nil) then curBtn.Hint:= curBtn.Caption;
  end;
end;

procedure TDigIt_Main.UI_MenuItemsChecks(newSourceI, newDestinationI: Integer);
var
   curItem: TMenuItem;
   i: Integer;

begin
  //No Default Item if index is -1
  if (newSourceI = -1)
  then for i:=0 to menuSources.Items.Count-1 do menuSources.Items[i].Default:= False;

  if (newDestinationI = -1)
  then for i:=0 to menuDestinations.Items.Count-1 do menuDestinations.Items[i].Default:= False;

  curItem:= FindMenuItemByTag(menuSources, newSourceI);
  if (curItem <> Nil) then curItem.Default:= True;

  curItem:= FindMenuItemByTag(menuDestinations, newDestinationI);
  if (curItem <> Nil) then curItem.Default:= True;
end;

procedure TDigIt_Main.UI_ThumbnailUpdate(AIndex: Integer; AFileName: String);
begin
  if (CapturedFiles[AIndex].iIndex <= 0)
  then CapturedFiles[AIndex].iIndex:= imgListThumb.AddProportionally(AFileName)
  else imgListThumb.ReplaceProportionally(CapturedFiles[AIndex].iIndex, AFileName);

  imgListThumb_Changed:= True;
end;

procedure TDigIt_Main.UI_ThumbnailUpdate(AIndex: Integer; ABitmap: TBGRABitmap);
begin
  if (CapturedFiles[AIndex].iIndex <= 0)
  then CapturedFiles[AIndex].iIndex:= imgListThumb.AddProportionally(ABitmap.Bitmap)
  else imgListThumb.ReplaceProportionally(CapturedFiles[AIndex].iIndex, ABitmap.Bitmap);

  imgListThumb_Changed:= True;
end;

procedure TDigIt_Main.UI_FillBox(ABox: TCropArea);
begin
   if (ABox<>nil)
   then begin
           inFillBoxUI :=True;
           panelCropArea.Enabled :=True;
           edCropName.Text :=ABox.Name;
           edCropUnit_Type.ItemIndex :=Integer(ABox.AreaUnit);

           if (ABox.AreaUnit=ruNone)
           then begin
                  edCropLeft.DecimalPlaces:=0;
                  edCropTop.DecimalPlaces:=0;
                  edCropWidth.DecimalPlaces:=0;
                  edCropHeight.DecimalPlaces:=0;
                end
           else begin
                  edCropLeft.DecimalPlaces:=3;
                  edCropTop.DecimalPlaces:=3;
                  edCropWidth.DecimalPlaces:=3;
                  edCropHeight.DecimalPlaces:=3;
                end;
         (*  edCropLeft.MaxValue:=ABox.MaxWidth;
           edCropTop.MaxValue:=ABox.MaxHeight;
           edCropWidth.MaxValue:=edCropLeft.MaxValue;
           edCropHeight.MaxValue:=edCropTop.MaxValue; *)

           edCropLeft.Value :=ABox.Left;
           edCropTop.Value :=ABox.Top;
           edCropWidth.Value :=ABox.Width;
           edCropHeight.Value :=ABox.Height;

           //Aspect Ratio
           changingAspect :=True;
           Case ABox.KeepAspectRatio of
           bParent :rgCropAspect.ItemIndex:=0;
           bFalse  :rgCropAspect.ItemIndex:=1;
           bTrue   :rgCropAspect.ItemIndex:=2;
           end;
           edCropAspectPersonal.Text:=ABox.AspectRatio;
           changingAspect:=False;

           cbCropCounterList.ItemIndex:=ABox.UserData;
           //if (ABox.UserData > -1) then CounterSelect(ABox.UserData);

           inFillBoxUI :=False;
        end
   else panelCropArea.Enabled :=False;
end;

procedure TDigIt_Main.UI_FillCounter(ACounter: TDigIt_Counter);
begin
   if (ACounter = nil) then ACounter:= Counters_GetCurrent;
   if (ACounter <> nil)
   then begin
           inFillCounterUI :=True;
           panelCounter.Enabled :=True;
           edCounterName.Text:=ACounter.Name;
           edCounterValue.Value:=ACounter.Value_Next;
           edCounterValueStringDigits.Value:=ACounter.Value_StringDigits;
           edCounterValueStringPre.Text:=ACounter.Value_StringPre;
           edCounterValueStringPost.Text:=ACounter.Value_StringPost;
           {$ifopt D+} lbPrevious.Caption:=Format(rsCounterPrev, [ACounter.Value_Previous]); {$endif}
           lbCounterExample.Caption:=ACounter.GetValue(True);
           inFillCounterUI :=False;
        end
   else panelCounter.Enabled :=False;
end;

procedure TDigIt_Main.UI_FillCounters;
var
   i:Integer;
   curCounter:TDigIt_Counter;
   curCropArea:TCropArea;

begin
  //Fill Counters related UI
  cbCounterList.Clear;
  cbCropCounterList.Clear;
  for i:=0 to Counters.Count-1 do
  begin
    curCounter :=Counters.items[i];
    cbCounterList.AddItem(curCounter.Name, curCounter);
    cbCropCounterList.AddItem(curCounter.Name, curCounter);
  end;
  curCropArea :=GetCurrentCropArea;
  if (curCropArea = nil)
  then begin
         cbCropCounterList.ItemIndex:= -1;
         if (Counters.Count > -1)
         then cbCounterList.ItemIndex:= 0
         else cbCounterList.ItemIndex:= -1;
       end
  else cbCropCounterList.ItemIndex:=curCropArea.UserData;
end;

procedure TDigIt_Main.UI_FillPageSizes;
begin
  inFillPagesUI :=True;

  panelPageSize.Enabled:=(edPage_UnitType.ItemIndex>0);
  if panelPageSize.Enabled
  then begin
         edPage_UnitType.ItemIndex :=Integer(imgManipulation.EmptyImage.ResolutionUnit)+1;

         if (imgManipulation.EmptyImage.ResolutionUnit=ruNone)
         then begin
                edPageWidth.DecimalPlaces:=0;
                edPageHeight.DecimalPlaces:=0;
              end
         else begin
               edPageWidth.DecimalPlaces:=3;
               edPageHeight.DecimalPlaces:=3;
              end;

         edPageWidth.Value:=imgManipulation.EmptyImage.ResolutionWidth;
         edPageHeight.Value:=imgManipulation.EmptyImage.ResolutionHeight;
       end;

  inFillPagesUI :=False;
end;


end.

