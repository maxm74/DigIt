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
  rsContinueAutoWork = 'Continue from Auto Saved Work Session?'#13#10'%s';
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
  rsCrop = 'Crop (F5)';
  rsCropNext = 'Crop + Next (F5)';
  rsCropFull = 'Full Area';
  rsCropCust = 'Custom';
  rsCropToDo = '%d files to do';
  rsCounterPrev = 'Value Previous: %d';
  rsDeleteCaptured = 'Delete Captured Page %s ?';
  rsDeleteAll = 'Delete All Captured Pages?';
  rsDeleteAllFiles = 'Do I also Delete Files from the disk?';

  rsSourceNotFound = 'Source %s not Found, try Select another from Menù';
  rsSourceNotSelected = 'Cannot Select Source %d, try Select another from Menù';
  rsErrIncomplete = 'Operation not completed, do I keep the processed files?';
  rsErrLoadWork = 'Cannot Load Work Session'#13#10'%s';
  rsErrSaveWork = 'Cannot Save Work Session'#13#10'%s'#13#10'%s';

type
  { TDigIt_Main }

  TDigIt_Main = class(TForm)
    actGoBack: TAction;
    actCropAll: TAction;
    actClearQueue: TAction;
    actGoNext: TAction;
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
    BCLabel15: TBCLabel;
    BCLabel2: TBCLabel;
    BCLabel3: TBCLabel;
    BCLabel4: TBCLabel;
    BCLabel5: TBCLabel;
    BCLabel6: TBCLabel;
    BCLabel7: TBCLabel;
    BCLabel8: TBCLabel;
    btCFlipHLeft: TSpeedButton;
    btCFlipHRight: TSpeedButton;
    btCFlipVDown: TSpeedButton;
    btCFlipVUp: TSpeedButton;
    btCropDuplicate: TSpeedButton;
    btCropDuplicateOp: TSpeedButton;
    btCrop_Add: TBGRASpeedButton;
    btCrop_Del: TBGRASpeedButton;
    btCRotateLeft: TSpeedButton;
    btCRotateRight: TSpeedButton;
    btPageSizes: TSpeedButton;
    btPageSizesToCrops: TSpeedButton;
    btPaperSizes: TSpeedButton;
    btPFlipH: TSpeedButton;
    btPFlipV: TSpeedButton;
    btPRotate180: TSpeedButton;
    btPRotateLeft: TSpeedButton;
    btPRotateRight: TSpeedButton;
    btZBack: TSpeedButton;
    btZDown: TSpeedButton;
    btZFront: TSpeedButton;
    btZUp: TSpeedButton;
    cbCropList: TComboBox;
    edCropAspectPersonal: TEditButton;
    edCropHeight: TFloatSpinEdit;
    edCropLeft: TFloatSpinEdit;
    edCropName: TEdit;
    edCropTop: TFloatSpinEdit;
    edCropUnit_Type: TComboBox;
    edCropWidth: TFloatSpinEdit;
    edPageHeight: TFloatSpinEdit;
    edPageWidth: TFloatSpinEdit;
    edPage_Fix: TComboBox;
    edPage_UnitType: TComboBox;
    imgListCaptured: TImageList;
    itemTake: TMenuItem;
    itemTakeAgain: TMenuItem;
    itemBuildDuplex: TMenuItem;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    menuDebug: TMenuItem;
    menuClearXML: TMenuItem;
    menuImageFormat: TMenuItem;
    menuExport: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    itemProfiles: TMenuItem;
    itemProfiles_Add: TMenuItem;
    itemProfiles_Remove: TMenuItem;
    menuProjectSaveAs: TMenuItem;
    menuSaveXML: TMenuItem;
    menuLoadXML: TMenuItem;
    panelCropArea: TBCPanel;
    panelPageRotate: TBCPanel;
    panelPageSize: TBCPanel;
    panelTop: TPanel;
    imgListImgActions: TImageList;
    menuDestinations: TPopupMenu;
    itemCropModeFull: TMenuItem;
    itemCropModeCustom: TMenuItem;
    edCounterValue: TSpinEdit;
    edCounterValueStringDigits: TSpinEdit;
    edCounterValueStringPost: TEdit;
    edCounterValueStringPre: TEdit;
    imgListMenu: TImageList;
    imgListThumb: TBGRAImageList;
    Label4: TLabel;
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
    menuPaperSizes: TPopupMenu;
    menuSources: TPopupMenu;
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
    Separator4: TMenuItem;
    Separator5: TMenuItem;
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
    tbCropBack: TToolButton;
    tbCapturedDelete: TToolButton;
    tbCapturedDeleteAll: TToolButton;
    tbCaptSep1: TToolButton;
    tbCaptSep2: TToolButton;
    tbCapturedToImg: TToolButton;

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
    procedure actGoNextExecute(Sender: TObject);
    procedure actGoBackExecute(Sender: TObject);
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
    procedure btPFlipClick(Sender: TObject);
    procedure btPRotateClick(Sender: TObject);
    procedure btZBackClick(Sender: TObject);
    procedure btZDownClick(Sender: TObject);
    procedure btZFrontClick(Sender: TObject);
    procedure btZUpClick(Sender: TObject);
    procedure edPageHeightChange(Sender: TObject);
    procedure edPageWidthChange(Sender: TObject);
    procedure edPage_UnitTypeChange(Sender: TObject);
    procedure itemCropModeClick(Sender: TObject);
    procedure lvCapturedDblClick(Sender: TObject);
    procedure lvCapturedSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
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

    procedure btCrop_AddClick(Sender: TObject);
    procedure btCrop_DelClick(Sender: TObject);
    procedure cbCropListChange(Sender: TObject);
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
    procedure Separator2DrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure Separator2MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);

    procedure tbCapturedPDFClick(Sender: TObject);
    procedure tbCapturedToImgClick(Sender: TObject);
    procedure MenuSourcePopup(Sender: TObject);

  private
    { private declarations }
    dlgRes: TModalResult;

    lastNewBoxNum: Word;
    changingAspect,
    Closing,
    SES_Loading,
    inFillCounterUI,
    inFillBoxUI,
    inFillPagesUI,
    imgListThumb_Changed,
    rSessionModified: Boolean;
    Counter: TDigIt_Counter;

    CropMode: TDigItCropMode;

    PageResizeUnitType: Integer;
    PageResize: TDigItFilter_Resize;
    PageRotate: TDigItFilter_Rotate;
    PageFlip: TDigItFilter_Flip;

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

    selectedProfile: Integer;
    Profiles: TStringArray;

    function GetCurrentCropArea: TCropArea;

    procedure Counter_Dec(AValue: Integer);
    procedure Counter_Inc(AValue: Integer);
    procedure Counter_Assign(AValue: Integer);

    procedure SetSessionModified(AValue: Boolean);

    procedure UI_DestinationMenuClick(Sender: TObject);
    procedure UI_SourceMenuClick(Sender: TObject);
    procedure UI_FillCropArea(ACropArea :TCropArea);
    procedure UI_FillCounter;
    procedure UI_SelectCurrentCaptured(AddValue: Integer=0);
    procedure UI_SelectNextCaptured(AddValue: Integer=0);
    procedure UI_FillPageSizes;
    procedure UI_FillPageRotateFlip;
    procedure UI_UpdateCropAreaList;
    procedure UI_ToolBar;
    procedure UI_ToolBarMods;
    procedure UI_MenuItemsChecks(newSourceI, newDestinationI: Integer);
    procedure UI_ThumbnailUpdate(AIndex: Integer; AFileName: String); overload;
    procedure UI_ThumbnailUpdate(AIndex: Integer; ABitmap: TBGRABitmap); overload;
    procedure UI_Caption;

                                            //Only the File Part, Path and Ext are added automatically
    function SaveImage(Bitmap: TBGRABitmap; AFileName: String): String;
    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer);

    function LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
    procedure EmptyImage(saveToXML: Boolean);

    procedure SetPageResizeUnitType(AValue: Integer; PredefValues: Boolean);

    function ResizeImage(ABitmap :TBGRABitmap; APageResize: TDigItFilter_Resize): TBGRABitmap;
    function RotateImage(ABitmap :TBGRABitmap; APageRotate: TDigItFilter_Rotate): TBGRABitmap;
    procedure FlipImage(ABitmap :TBGRABitmap; APageFlip: TDigItFilter_Flip);

    function LoadSessionFile(APath, AFile: String; IsAutoSave: Boolean=False): Boolean; overload;
    function LoadSessionFile(AFileName: String): Boolean; overload;
    function SaveSessionFile(AFileName: String): Boolean;

    procedure SES_Load(IsAutoSave: Boolean);
    procedure SES_Save(IsAutoSave: Boolean);
    procedure SES_ClearAutoSave(AFromStartup: Boolean);

    function SES_LoadSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
    procedure SES_SaveSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    function SES_LoadDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
    procedure SES_SaveDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveSource_CapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveSource_CapturedIndexes(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SavePageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);

    procedure CFG_Load;
    procedure CFG_Save;
    procedure CFG_Load_LastSession(aXML: TRttiXMLConfig; var APath, AFile: String);
    procedure CFG_Save_LastSession(aXML: TRttiXMLConfig; const APath, AFile: String);

    procedure PROF_Load;
    procedure PROF_Save;

    function Source_Select(newSourceIndex, newSourceSubIndex: Integer): Boolean;
    function Destination_Select(newDestinationIndex: Integer): Boolean;

    procedure setCropMode(ANewCropMode: TDigItCropMode);

    function SourceFiles_Add(AArray: IDigIt_ArrayR_PChars; AStartIndex: Integer): Integer; overload;
    function SourceFiles_Add(AFileName: String; AStartIndex: Integer): Integer; overload;
    procedure SourceFiles_Clear(ClearSourceInst: Boolean);

    function CropFile_Full(AFileName: String; isReTake: Boolean): Boolean; overload;
    procedure CropFile_Full(AStartIndex: Integer; isReTake: Boolean); overload;

    procedure CropFiles(ASourceFileIndex: Integer; isReTake: Boolean);

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
  BGRAUnits, BGRAWriteJPeg, BGRAWriteTiff, BGRAFormatUI,
  MM_StrUtils,
  DigIt_Destinations, DigIt_Destination_SaveFiles_SettingsForm,
  //DigIt_Form_PDF,
  DigIt_Form_ExportFiles,
  DigIt_Form_Progress, DigIt_Form_Templates, DigIt_Form_BuildDuplex;


{ TDigIt_Main }

procedure TDigIt_Main.edCounterValueChange(Sender: TObject);
begin
  if inFillCounterUI then exit;

  if (Counter<>nil) then
  begin
    Counter.Value_Next:= edCounterValue.Value;
    lbCounterExample.Caption:= Counter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCounterValueStringDigitsChange(Sender: TObject);
begin
  if inFillCounterUI then exit;

  if (Counter<>nil) then
  begin
    Counter.Value_StringDigits:= edCounterValueStringDigits.Value;
    lbCounterExample.Caption:= Counter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCounterValueStringPreEditingDone(Sender: TObject);
begin
  if (Counter<>nil) then
  begin
    Counter.Value_StringPre:= edCounterValueStringPre.Text;
    lbCounterExample.Caption:= Counter.GetValue(True);
  end;
end;

procedure TDigIt_Main.edCounterValueStringPostEditingDone(Sender: TObject);
begin
  if (Counter<>nil) then
  begin
    Counter.Value_StringPost:= edCounterValueStringPost.Text;
    lbCounterExample.Caption:= Counter.GetValue(True);
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
    UI_FillCropArea(CropArea);
  end;
end;

procedure TDigIt_Main.FormCreate(Sender: TObject);
begin
  Counter:= TDigIt_Counter.Create('Counter0');

  Closing :=False;
  SES_Loading:= False;
  changingAspect :=False;
  inFillCounterUI :=False;
  inFillBoxUI :=False;
  inFillPagesUI :=False;
  imgListThumb_Changed:= False;

  lastNewBoxNum :=0;
  TStringList(cbCropList.Items).OwnsObjects:=False;

  rSource:= nil;
  rSourceName:= '';;
  rSourceParams:= nil;

  //  rDestination:= nil;
  rDestinationName:= '';
  //  rDestinationParams:= nil;


  SetSaveWriter(ifJpeg);

  Session_File:= File_DefSession;

  SetDefaultStartupValues;

  {$ifopt D+}
  //Test: REmove IT
  SetLength(Profiles, 5);
  Profiles[0]:= 'Test Profile 0';
  Profiles[1]:= 'Test Profile 1';
  Profiles[2]:= 'Test Profile 2 caio';
  Profiles[3]:= 'Test Profile 3 sempronio';
  Profiles[4]:= 'Test Profile 4 Pluto';
  selectedProfile:= 2;
  {$endif}

  BuildProfilesMenu(Self, itemProfiles, nil, selectedProfile, Profiles);
  BuildDestinationsMenu(Self, menuDestinations, @UI_DestinationMenuClick);

  {$ifopt D+}
    imgManipulation.Rulers_Show:= True;
    imgManipulation.Rulers_Unit:= ruPixelsPerCentimeter;

    menuDebug.Visible:= True;
    lbPrevious.Visible:= True;
//    MenuMain.OwnerDraw:= True;
  {$endif}
end;

procedure TDigIt_Main.FormShow(Sender: TObject);
var
   optPath_Session,
   optFile_Session: String;
   sessLoaded: Boolean;

begin
  UI_ToolBarMods;

  sessLoaded:= False;
  try
     //CFG_Load;

     if FileExists(ParamStr(1)) and
        (MessageDlg('DigIt', Format(rsContinueWork, [ParamStr(1)]),
                    mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     then sessLoaded:= LoadSessionFile(ParamStr(1));

     if not(sessLoaded) then
     begin
       {#to-do : CFG_Load must Create a Record like Settings, Use IT}
       CFG_Load_LastSession(nil, optPath_Session, optFile_Session);

       //If in Options there is a Session Opened then Open It
       if (optPath_Session <> '') and (optFile_Session <> '') and
          FileExists(optPath_Session+optFile_Session+Ext_Sess) then
       begin
         //If there is an AutoSave with a date later than the session date ask user
         if FileExists(optPath_Session+optFile_Session+Ext_AutoSess) and
            (FileAge(optPath_Session+optFile_Session+Ext_AutoSess) > FileAge(optPath_Session+optFile_Session+Ext_Sess)) and
            (MessageDlg('DigIt', Format(rsContinueAutoWork, [optPath_Session+optFile_Session]),
                        mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then sessLoaded:= LoadSessionFile(optPath_Session, optFile_Session, True);

         if not(sessLoaded) and
           (MessageDlg('DigIt', Format(rsContinueWork, [optPath_Session+optFile_Session]),
                       mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then sessLoaded:= LoadSessionFile(optPath_Session, optFile_Session);
       end;
     end;

     if not(sessLoaded) then
     begin
       //ask for open AutoSave if exists
       if FileExists(Path_DefSession+File_DefSession+Ext_AutoSess) then
       begin
         if (MessageDlg('DigIt', Format(rsContinueAutoWork, ['<no name>']), mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then begin
                SES_Load(True);
                sessLoaded:= True;
              end
         else begin
                SES_ClearAutoSave(True);
                sessLoaded:= False;
              end;
       end;
     end;

     if not(sessLoaded) then setCropMode(diCropFull);

     if (Path_Session = Path_DefSession) then CFG_Save_LastSession(nil, '', '');

  except
    Path_Session:= Path_DefSession;
    Path_Session_Scan:= Path_DefSession_Scan;
    Path_Session_Pictures:= Path_DefSession_Pictures;

    SetDefaultStartupValues;

    setCropMode(diCropFull);
    CFG_Save_LastSession(nil, '', '');
  end;

  UI_Caption;
end;

procedure TDigIt_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  closing :=True;

  //AutoSave
  if SessionModified then //FileExists(Path_Session+Session_File+Ext_AutoSess)
  begin
    SES_Save(True);

    //Save Current Session ?
    actSessionSaveExecute(nil);

    CanClose:= not(dlgRes = mrCancel);
    closing:= CanClose;
  end;
end;

procedure TDigIt_Main.FormDestroy(Sender: TObject);
begin
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
        if (curDataType in [diDataType_FileName, diDataType_FileNameArray]) then
        begin
          if (curDataType = diDataType_FileName)
          then curImageFile:= PChar(curData)
          else if not(IDigIt_ArrayR_PChars(curData).Get(0, curImageFile))
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
   len, i,
   numTaked: Integer;
   hasCropped: Boolean;

begin
  if (CropMode = diCropFull) then UI_SelectCurrentCaptured(-lastLenTaked);

  if (MessageDlg('DigIt', Format(rsTakeAgain, [lastLenTaked]),
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  try
    Case CropMode of
      diCropFull: begin
        Counter_Dec(lastLenTaked);
        actTakeExecute(actTakeRe);
      end;
      diCropCustom: begin
        len:= Length(SourceFiles);
        numTaked:= lastLenTaked;
        hasCropped:= (len-numTaked <= lastCropped);

        actTakeExecute(actTakeRe);

        if hasCropped then
        begin
          iSourceFiles:= len-numTaked;
          for i:=len-numTaked to len-1 do
          begin
            if (SourceFiles[i].cCount > 0) then
            begin
              iSourceFiles:= i;
              LoadImage(SourceFiles[i].fName, False);
              CropFiles(i, True);
            end;
          end;
        end;
      end;
    end;

  finally
    SES_SaveSource_CapturedFiles(nil, True);

    UI_SelectCurrentCaptured;
    UI_FillCounter;
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actTakeExecute(Sender: TObject);
var
   curData: Pointer;
   curDataType: TDigItDataType;
   res,
   StartIndex,
   oldLength: Integer;

begin
  try
      curData:= nil;
      res:= 0;
      oldLength:= Length(SourceFiles);

      if (CropMode = diCropCustom) then
      begin
        //If we have processed all queue files automatically clear it
        if not(Sender = actTakeRe) and
           (oldLength > 0) and
           (iSourceFiles >= 0) and (iSourceFiles >= oldLength) then
        begin
          SourceFiles_Clear(False);
          oldLength:= 0;
        end;
      end;

      if (Sender = actTake) or (Sender = actTakeRe)
      then res:= rSource^.Inst.Take(takeActTake, curDataType, curData)
      else
      if (Sender = actTakeBuildDuplex)
      then res:= WizardBuildDuplexExecute(curDataType, curData);

      if (res > 0) and (curData <> nil) then
      begin
        if (curDataType in [diDataType_FileName, diDataType_FileNameArray]) then
        begin
          Case CropMode of
            diCropFull: begin
              DigIt_Progress.ProgressShow(rsProcessingImages, 1, res);

              if (curDataType = diDataType_FileName)
              then begin
                     CropFile_Full(PChar(curData), (Sender = actTakeRe));
                     StrDispose(PChar(curData));
                   end
              else begin
                     res:= SourceFiles_Add(IDigIt_ArrayR_PChars(curData), -1);
                     CropFile_Full(0, (Sender = actTakeRe));
                   end;

              lastLenTaked:= res;
              SourceFiles:= nil; iSourceFiles:= -1;
              rSource^.Inst.Clear;
            end;
            diCropCustom: begin
              if (Sender = actTakeRe)
              then StartIndex:= oldLength-lastLenTaked
              else StartIndex:= oldLength;

              //Add files to the queue array
              if (curDataType = diDataType_FileName)
              then begin
                     res:= SourceFiles_Add(PChar(curData), StartIndex);
                     StrDispose(PChar(curData));
                   end
              else res:= SourceFiles_Add(IDigIt_ArrayR_PChars(curData), StartIndex);

              if (Sender = actTakeRe)
              then begin
                     if (iSourceFiles > Length(SourceFiles))
                     then iSourceFiles:= Length(SourceFiles); //Repos iSourceFiles if outside
                   end
              else begin
                     //The queue was empty, Start from the first file
                     if (oldLength = 0) and (Length(SourceFiles) > 0) then
                     begin
                       iSourceFiles:= 0;
                       LoadImage(SourceFiles[0].fName, False);

                       //Crop the First File directly
                       actCropNextExecute(nil);
                     end;
                   end;

              lastLenTaked:= res;
            end;
          end;
        end;
      end
      else MessageDlg(rsNoFilesDownloaded, mtError, [mbOk], 0);

  finally
    SES_Save(True);

    if not(Sender = actTakeRe) then
    begin
      UI_ToolBar;
      UI_FillCounter;
    end;

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
      if (MessageDlg('DigIt', rsNoMoreFiles, mtConfirmation,
                     [mbYes, mbNo], 0) = mrYes)
      then SourceFiles_Clear(True);
    end;
  end;

begin
  try
    lenSources:= Length(SourceFiles);
    if (iSourceFiles = -1) then iSourceFiles:= 0;

    if (iSourceFiles = lenSources) or
       (SourceFiles[iSourceFiles].cCount > 0)
    then CropFiles(iSourceFiles, True) //Re Crop
    else begin
           CropFiles(iSourceFiles, False);

           if (iSourceFiles > lastCropped) then lastCropped:= iSourceFiles;

           inc(iSourceFiles);
         end;

    if not(CheckEndOfFiles)
    then if (iSourceFiles > -1) and (iSourceFiles < Length(SourceFiles))
         then if not(LoadImage(SourceFiles[iSourceFiles].fName, False))
              then begin
                     { #todo -oMaxM : do something if LoadImage Fails? }
                   end;

  finally
    SES_SaveSource_CapturedFiles(nil, True);

    UI_SelectNextCaptured;
    UI_FillCounter;
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actGoNextExecute(Sender: TObject);
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
         if (SourceFiles[new_iSourceFiles].cCount > 0)
         then Counter_Assign(SourceFiles[new_iSourceFiles].cStart);
         iSourceFiles:= new_iSourceFiles;
       end;
     end;

  finally
    //SES_Save([xmlSourceIndexes, xmlCapturedIndexes]);
    SES_SaveSource_CapturedFiles(nil, True);

    UI_SelectNextCaptured;
    UI_FillCounter;
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actGoBackExecute(Sender: TObject);
var
   new_iSourceFiles: Integer;

begin
  try
     new_iSourceFiles:= iSourceFiles;
     if (new_iSourceFiles > 0) then
     begin
       //We've already cut the last one, start with the penultimate one
       if (new_iSourceFiles >= Length(SourceFiles)) then new_iSourceFiles:= Length(SourceFiles)-1;

       dec(new_iSourceFiles);

       if LoadImage(SourceFiles[new_iSourceFiles].fName, False) then
       begin
         if (SourceFiles[new_iSourceFiles].cCount > 0)
         then Counter_Assign(SourceFiles[new_iSourceFiles].cStart);
         iSourceFiles:= new_iSourceFiles;
       end;
     end;

  finally
    //SES_Save([xmlSourceIndexes, xmlCapturedIndexes]);
    SES_SaveSource_CapturedFiles(nil, True);

    UI_SelectNextCaptured;
    UI_FillCounter;
    UI_ToolBar;
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

    if (lastCropped > -1) then
    begin
      iSourceFiles:= lastCropped+1;
      LoadImage(SourceFiles[iSourceFiles].fName, False);
      Counter_Assign(SourceFiles[lastCropped].cStart+SourceFiles[lastCropped].cCount);
    end;

    cStr:= IntToStr(c);
    DigIt_Progress.ProgressShow(rsProcessingImages, iSourceFiles, c);

    repeat
      DigIt_Progress.progressTotal.Position:= iSourceFiles;
      DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [iSourceFiles, cStr]);
      Application.ProcessMessages;
      if DigIt_Progress.Cancelled then break;

      CropFiles(iSourceFiles, False);
      inc(iSourceFiles);

      Finished:= CheckEndOfFiles;
      if not(Finished)
      then if not(LoadImage(SourceFiles[iSourceFiles].fName, False))
           then begin
                  { #todo -oMaxM : do something if LoadImage Fails? }
                  break;
                end;

      lvCaptured.Selected:= lvCaptured.Items[iCapturedFiles];

      DigIt_Progress.progressTotal.Position:= iSourceFiles+1;
      DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [iSourceFiles-1, cStr]);
      Application.ProcessMessages;
    Until Finished or DigIt_Progress.Cancelled;

  finally
    //SES_Save([xmlSourceIndexes, xmlSourceFiles, xmlCapturedIndexes, xmlCapturedFiles]);
    SES_SaveSource_CapturedFiles(nil, True);

    UI_SelectNextCaptured;
    UI_FillCounter;
    UI_ToolBar;

    DigIt_Progress.Hide;
  end;
end;

procedure TDigIt_Main.actClearQueueExecute(Sender: TObject);
begin
  if (MessageDlg('DigIt', rsClearQueue, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  try
    SourceFiles_Clear(True);

  finally
    SES_SaveSource_CapturedFiles(nil, True);

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

  if (edPage_UnitType.ItemIndex = 0)
  then BuildPaperSizesMenu(ruPixelsPerCentimeter, Self, menuPaperSizes, @PageSizesClick, 4, 5)
  else BuildPaperSizesMenu(TResolutionUnit(edPage_UnitType.ItemIndex-1), Self, menuPaperSizes, @PageSizesClick, 4, 5);

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

procedure TDigIt_Main.btPFlipClick(Sender: TObject);
var
   oldPageFlip: TDigItFilter_Flip;

begin
  oldPageFlip:= PageFlip;

  if btPFlipV.Down
  then PageFlip:= flipVertical
  else
  if btPFlipH.Down
  then PageFlip:= flipHorizontal
  else PageFlip:= flipNone;

  if not(imgManipulation.Empty) then
  begin
    if FileExists(LoadedFile)
    then LoadImage(LoadedFile, False)
    else begin
           //Flip Image to Initial Value
           FlipImage(imgManipulation.Bitmap, oldPageFlip);

           //Flip Image to New Value
           FlipImage(imgManipulation.Bitmap, PageFlip);

           imgManipulation.RefreshBitmap;
         end;
  end;
end;

procedure TDigIt_Main.btPRotateClick(Sender: TObject);
var
   oldPageRotate: TDigItFilter_Rotate;
   BitmapR,
   BitmapNewR: TBGRABitmap;

begin
  oldPageRotate:= PageRotate;

  if btPRotateLeft.Down
  then PageRotate:= rotLeft90
  else
  if btPRotateRight.Down
  then PageRotate:= rotRight90
  else
  if btPRotate180.Down
  then PageRotate:= rot180
  else PageRotate:= rotNone;

  if not(imgManipulation.Empty) then
  try
    BitmapR:= nil;
    BitmapNewR:= nil;

    if FileExists(LoadedFile)
    then LoadImage(LoadedFile, False)
    else begin
           //Retun Image to Initial Value
           Case oldPageRotate of
           rotNone: BitmapR:= imgManipulation.Bitmap;
           rotLeft90: BitmapR:= RotateImage(imgManipulation.Bitmap, rotRight90);
           rotRight90: BitmapR:= RotateImage(imgManipulation.Bitmap, rotLeft90);
           rot180: BitmapR:= RotateImage(imgManipulation.Bitmap, rot180);
           end;

           //Rotate Image to New Value
           if (PageRotate = rotNone)
           then imgManipulation.Bitmap:= BitmapR
           else begin
                  BitmapNewR:= RotateImage(BitmapR, PageRotate);
                  imgManipulation.Bitmap:= BitmapNewR;
                end;
         end;

  finally
    if (BitmapR <> nil) then BitmapR.Free;
    if (BitmapNewR <> nil) then BitmapNewR.Free;
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
  try
    PaperSizesMenuTag_decode(TMenuItem(Sender).Tag, ResUnit, Paper);

    if (PageResizeUnitType = 0)
    then SetPageResizeUnitType(Integer(ResUnit)+1, False);

    imgManipulation.SetEmptyImageSize(ResUnit, Paper.w, Paper.h);

    if not(imgManipulation.Empty) and
       FileExists(LoadedFile)
    then LoadImage(LoadedFile, False);

  finally
    UI_FillPageSizes;
  end;
end;

procedure TDigIt_Main.btZBackClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea :=GetCurrentCropArea;
  if (CropArea<>nil) then
  begin
    CropArea.BringToBack;
    UI_UpdateCropAreaList;
  end;
end;

procedure TDigIt_Main.btZDownClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea:= GetCurrentCropArea;
  if (CropArea<>nil) then
  begin
    CropArea.BringBackward;
    UI_UpdateCropAreaList;
  end;
end;

procedure TDigIt_Main.btZFrontClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea:= GetCurrentCropArea;
  if (CropArea<>nil) then
  begin
    CropArea.BringToFront;
    UI_UpdateCropAreaList;
  end;
end;

procedure TDigIt_Main.btZUpClick(Sender: TObject);
var
   CropArea :TCropArea;

begin
  CropArea:= GetCurrentCropArea;
  if (CropArea<>nil) then
  begin
    CropArea.BringForward;
    UI_UpdateCropAreaList;
  end;
end;

procedure TDigIt_Main.edPageHeightChange(Sender: TObject);
begin
  if inFillPagesUI or (PageResizeUnitType = 0) then exit;

  imgManipulation.SetEmptyImageSize(TResolutionUnit(PageResizeUnitType-1),
                                    edPageWidth.Value, edPageHeight.Value);

  if not(imgManipulation.Empty) and
     FileExists(LoadedFile)
  then LoadImage(LoadedFile, False);
end;

procedure TDigIt_Main.edPageWidthChange(Sender: TObject);
begin
  if inFillPagesUI or (PageResizeUnitType = 0) then exit;

  imgManipulation.SetEmptyImageSize(TResolutionUnit(PageResizeUnitType-1),
                                    edPageWidth.Value, edPageHeight.Value);

  if not(imgManipulation.Empty) and
     FileExists(LoadedFile)
  then LoadImage(LoadedFile, False);
end;

procedure TDigIt_Main.edPage_UnitTypeChange(Sender: TObject);
begin
  try
     SetPageResizeUnitType(edPage_UnitType.ItemIndex, True);

     if not(imgManipulation.Empty) and
        FileExists(LoadedFile)
     then LoadImage(LoadedFile, False);

  finally
    UI_FillPageSizes;
  end;
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

     if (Counter.Value = Length(CapturedFiles)) then
     begin
       Counter.Value:= Length(CapturedFiles)-1;
       UI_FillCounter;
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
   i: Integer;

begin
  if (MessageDlg('DigIt', rsDeleteAll, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
     //Delete all File in FileSystem ?
     if (MessageDlg('DigIt', rsDeleteAllFiles, mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     then for i:=0 to Length(CapturedFiles)-1 do
            DeleteFile(CapturedFiles[i].fName);

     //Clear Array and ListView
     ClearCaptured;

     //Reset Counter
     Counter.Reset;

     EmptyImage(False);

     SES_Save(True);

     UI_ToolBar;
     UI_FillCounter;
  end;
end;

procedure TDigIt_Main.actSessionNewExecute(Sender: TObject);
begin
  try
   (*  if TDigIt_Templates.Execute then
     begin

     end;
     *)
    if (MessageDlg('DigIt', rsNewWork, mtConfirmation, [mbYes, mbNo], 0)=mrYes) then
    begin
      SES_ClearAutoSave(False);
      CFG_Save_LastSession(nil, Path_Session, Session_File);
    end;

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

     if OpenSessionDlg.Execute then
     begin
       LoadSessionFile(OpenSessionDlg.FileName);
       CFG_Save_LastSession(nil, Path_Session, Session_File);
     end;

  finally
  end;
end;

procedure TDigIt_Main.actSessionSaveAsExecute(Sender: TObject);
begin
  if SaveSessionDlg.Execute then
  begin
    SaveSessionFile(SaveSessionDlg.FileName);
    CFG_Save_LastSession(nil, Path_Session, Session_File);
  end;
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

     if canSave
     then begin
            if (Path_Session = Path_DefSession)
            then actSessionSaveAs.Execute
            else SES_Save(False);
          end
     else begin
            //If we are really closing the Application then Clear AutoSave
            if (Sender = nil) and not(dlgRes = mrCancel) and
               (Path_Session = Path_DefSession)
            then SES_ClearAutoSave(True);
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
     oIndex:= captItem.ImageIndex;
     captItem.ImageIndex:= -1;
     captItem.ImageIndex:= oIndex;

  finally
     if (sourceBitmap <> nil) then sourceBitmap.Free;
     if (rotatedBitmap <> nil) then rotatedBitmap.Free;
  end;
end;

function TDigIt_Main.SaveImage(Bitmap: TBGRABitmap; AFileName: String): String;
begin
  Result:=Path_Session_Pictures+AFileName+ExtensionSeparator+SaveExt;

  { #todo -oMaxM : In future Presaving filters as Interfaces Here }

  //Adjust some Writers
  if (SaveWriter is TBGRAWriterTiff) then TBGRAWriterTiff(SaveWriter).Clear;

  Bitmap.SaveToFile(Result, SaveWriter);
end;

procedure TDigIt_Main.SaveCallBack(Bitmap: TBGRABitmap; CropArea: TCropArea; AUserData: Integer);
var
  savedFile: String;
  captItem: TListItem;

begin
  //Increment the Counter Value
  Counter.Value:= Counter.Value+1;

  inc(iCapturedFiles);

  //Save File
  savedFile:= SaveImage(Bitmap, Counter.GetValue);

  captItem:= nil;
  if (Boolean(AUserData) = False)
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

         captItem.ImageIndex:= -1; //WorkAround to update immediately Image
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

         captItem.ImageIndex:= -1;
         captItem.ImageIndex:= CapturedFiles[iCapturedFiles].iIndex;
       end;

  //Go to Item in Captured List
  if (captItem <> nil) then captItem.MakeVisible(False);

//  SES_SaveCapturedFiles(nil); { #note -oMaxM : Maybe done with an Index }
end;

function TDigIt_Main.LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
var
   Bitmap,
   BitmapR:TBGRABitmap;

begin
  Result:= False;

  if (AImageFile<>'') and FileExists(AImageFile) then
  try
     BitmapR:= nil;
     Bitmap:= TBGRABitmap.Create;
     Bitmap.LoadFromFile(AImageFile);

     //Pre processing Filters

     { #todo -oMaxM : In future Preprocessing filters as Interfaces Here }
(*
     //Resize
     if (PageResize <> resNone) then
     begin
       BitmapR:= ResizeImage(Bitmap, PageResize);
       if (BitmapR<>nil) then
       begin
         Bitmap.Free;
         Bitmap:= BitmapR;
         BitmapR:= nil;
       end;
     end;
*)
     //Rotate
     if (PageRotate <> rotNone) then
     begin
       BitmapR:= RotateImage(Bitmap, PageRotate);
       if (BitmapR<>nil) then
       begin
         Bitmap.Free;
         Bitmap:= BitmapR;
         BitmapR:= nil;
       end;
     end;

     //Flip
     if (PageFlip <> flipNone) then FlipImage(Bitmap, PageFlip);

     //Resize
     if (PageResize <> resNone) then
     begin
       BitmapR:= ResizeImage(Bitmap, PageResize);
       if (BitmapR<>nil) then
       begin
         Bitmap.Free;
         Bitmap:= BitmapR;
         BitmapR:= nil;
       end;
     end;

     imgManipulation.Bitmap:= Bitmap;

     LoadedFile:= AImageFile;

     if saveToXML then SES_SaveLoadedImage(nil, True);

     Result:= True;

  finally
     if (Bitmap <> Nil) then Bitmap.Free;
  end;
end;

procedure TDigIt_Main.EmptyImage(saveToXML: Boolean);
begin
  imgManipulation.Bitmap:= nil;
  LoadedFile:= '';
  if saveToXML then SES_SaveLoadedImage(nil, True);
end;

procedure TDigIt_Main.SetPageResizeUnitType(AValue: Integer; PredefValues: Boolean);
begin
  PageResizeUnitType:= AValue;
  edPage_UnitType.ItemIndex:= AValue;

  if (PageResizeUnitType = 0)
  then begin
         PageResize:= resNone;
         imgManipulation.SetEmptyImageSizeToNull;
       end
  else begin
         //if there is a size defined, change only the resolution unit
         if (panelPageSize.Enabled)
         then imgManipulation.EmptyImage.ResolutionUnit:= TResolutionUnit(edPage_UnitType.ItemIndex-1)
         else if PredefValues then
              Case TResolutionUnit(AValue-1) of
              ruNone: imgManipulation.SetEmptyImageSize(ruNone,
                                                        Paper_A_inch[4].w * 200, Paper_A_inch[4].h * 200);
              ruPixelsPerCentimeter: imgManipulation.SetEmptyImageSize(ruPixelsPerCentimeter,
                                                        Paper_A_cm[4].w, Paper_A_cm[4].h);
              ruPixelsPerInch: imgManipulation.SetEmptyImageSize(ruPixelsPerInch,
                                                        Paper_A_inch[4].w, Paper_A_inch[4].h);
              end;

         PageResize:= TDigItFilter_Resize(edPage_Fix.ItemIndex);
       end;
end;

function TDigIt_Main.ResizeImage(ABitmap: TBGRABitmap; APageResize: TDigItFilter_Resize): TBGRABitmap;
var
   newWidth, newHeight: Single;
   pixelWidth, pixelHeight: Integer;

begin
  Result:= nil;

  if (APageResize <> resNone) then
  begin
    if (imgManipulation.EmptyImage.ResolutionUnit = ruNone)
    then begin
           pixelWidth:= imgManipulation.EmptyImage.Width;
           pixelHeight:= imgManipulation.EmptyImage.Height;
         end
    else begin
           newWidth:= PhysicalSizeConvert(imgManipulation.EmptyImage.ResolutionUnit,
                                          imgManipulation.EmptyImage.ResolutionWidth,
                                          ABitmap.ResolutionUnit);
           newHeight:= PhysicalSizeConvert(imgManipulation.EmptyImage.ResolutionUnit,
                                           imgManipulation.EmptyImage.ResolutionHeight,
                                           ABitmap.ResolutionUnit);

           pixelWidth:= HalfUp(newWidth*ABitmap.ResolutionX);
           pixelHeight:= HalfUp(newHeight*ABitmap.ResolutionY);
         end;

    Case APageResize of
    resFixedWidth: pixelHeight:= GetProportionalSide(pixelWidth, ABitmap.Width, ABitmap.Height);
    resFixedHeight: pixelWidth:= GetProportionalSide(pixelHeight, ABitmap.Height, ABitmap.Width);
    end;

    Result:= ABitmap.Resample(pixelWidth, pixelHeight, rmFineResample, True);
  end;
end;

function TDigIt_Main.RotateImage(ABitmap: TBGRABitmap; APageRotate: TDigItFilter_Rotate): TBGRABitmap;
begin
  Case APageRotate of
    rotNone: Result:= nil;
    rotLeft90: Result:= ABitmap.RotateCCW(True);
    rotRight90: Result:= ABitmap.RotateCW(True);
    rot180: Result:= ABitmap.RotateUD(True);
  end;
end;

procedure TDigIt_Main.FlipImage(ABitmap :TBGRABitmap; APageFlip: TDigItFilter_Flip);
begin
  Case APageFlip of
    flipHorizontal: ABitmap.HorizontalFlip;
    flipVertical: ABitmap.VerticalFlip;
  end;
end;

function TDigIt_Main.LoadSessionFile(APath, AFile: String; IsAutoSave: Boolean
  ): Boolean;
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

     SES_Load(IsAutoSave);

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

function TDigIt_Main.LoadSessionFile(AFileName: String): Boolean;
begin
  Result:= LoadSessionFile(ExtractFilePath(AFileName),
                           ExtractFileNameWithoutExt(ExtractFileName(AFileName)));
end;

function TDigIt_Main.SaveSessionFile(AFileName: String): Boolean;
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
     //SES_Save(True);

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

     SES_Save(False);

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

procedure TDigIt_Main.SES_Load(IsAutoSave: Boolean);
var
   newSourceI,
   newDestinationI: Integer;
   aXML: TRttiXMLConfig;

begin
  try
     SES_Loading:= True;

     if IsAutoSave
     then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
     else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     newSourceI:= SES_LoadSource(aXML, IsAutoSave);
     SES_LoadSourceFiles(aXML, IsAutoSave);
     newDestinationI:= SES_LoadDestination(aXML, IsAutoSave);
     SES_LoadCapturedFiles(aXML, IsAutoSave);
     SES_LoadPageSettings(aXML, IsAutoSave);
     SES_LoadLoadedImage(aXML, IsAutoSave);
     Counter.Load(aXML, 'Counter', True);
     SES_LoadCropAreas(aXML, IsAutoSave);
     SES_LoadUserInterface(aXML, IsAutoSave);

     SessionModified:= IsAutoSave;

  finally
     SES_Loading:= False;
     aXML.Free;

     UI_MenuItemsChecks(newSourceI, newDestinationI);
     UI_FillCounter;
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.SES_Save(IsAutoSave: Boolean);
var
   aXML: TRttiXMLConfig;
   curExt: String;

begin
  //if (rSource <> Nil) and (rSource^.Inst <> Nil) then
  try
     if IsAutoSave
     then curExt:= Ext_AutoSess
     else curExt:= Ext_Sess;

     aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+curExt);

     SES_SaveSource(aXML, IsAutoSave);
     SES_SaveSourceFiles(aXML, IsAutoSave);
     SES_SaveDestination(aXML, IsAutoSave);
     SES_SaveCapturedFiles(aXML, IsAutoSave);
     SES_SavePageSettings(aXML, IsAutoSave);
     SES_SaveLoadedImage(aXML, IsAutoSave);

     Counter.Save(aXML, 'Counter', True);

     SES_SaveCropAreas(aXML, IsAutoSave);
     SES_SaveUserInterface(aXML, IsAutoSave);

     aXML.Flush;
     aXML.Free;

     //FPC Bug?
     //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
     //So we do it after destroying XML.

     if (rSource <> nil) and
        (rSource^.Inst <> Nil)
     then rSource^.Inst.Params.Save(PChar(Path_Session+Session_File+curExt), 'Source/Params');

//     if (rDestination <> nil) then rDestination^.Inst.Params.Save(PChar(Path_Session+Session_File+curExt), 'Destination/Params');

     if not(IsAutoSave) then SessionModified:= False;

  finally
  end;
end;

procedure TDigIt_Main.SES_ClearAutoSave(AFromStartup: Boolean);
var
   i: Integer;
   nofileBMP: TBitmap;

begin
  DeleteFile(Path_DefSession+File_DefSession+Ext_AutoSess);
  DeleteFile(Path_DefSession+File_DefSession+Ext_AutoThumb);
  DeleteDirectory(Path_DefSession_Scan, True);
  DeleteDirectory(Path_DefSession_Pictures, True);

  if AFromStartup
  then begin
       end
  else try
          SetDefaultSessionValues;
          SetDefaultStartupValues;

          //Clear Source Queque
          //rSource^.Inst.Clear;

          //Clear Captured Array and ListView
          ClearCaptured;

          //This would be like a Full Area Template
          imgManipulation.Bitmap:= nil;
          if (CropMode = diCropCustom) then imgManipulation.clearCropAreas;

          //Reset Counter
          Counter.Reset;

          setCropMode(diCropFull);

          Caption :='DigIt';

       finally
         UI_FillCounter;
       end;

  UI_ToolBar;
end;

function TDigIt_Main.SES_LoadSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
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

     if (newSourceName <> '') then
     begin
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
      end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_SaveSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

         if (rSource <> nil) and
            (rSource^.Inst <> nil)
         then rSource^.Inst.Params.Save(PChar(Path_Session+Session_File+curExt), 'Source/Params');
       end;
     end;

  finally
    if aFree and (aXML <> nil) then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_LoadSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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
     iCount:= aXML.GetValue(SES_SourceFiles+'Count', 0);
     iSourceFiles:= aXML.GetValue(SES_SourceFiles+'iSourceFiles', -1);
     lastCropped:= aXML.GetValue(SES_SourceFiles+'lastCropped', -1);
     lastLenTaked:= aXML.GetValue(SES_SourceFiles+'lastLenTaked', 0);
     SetLength(SourceFiles, iCount);
     for i:=0 to iCount-1 do
     begin
       curItemPath :=SES_SourceFiles+'Item' + IntToStr(i)+'/';
       SourceFiles[i].cCount:= aXML.GetValue(curItemPath+'cCount', 0);
       SourceFiles[i].cStart:= aXML.GetValue(curItemPath+'cStart', 0);
       SourceFiles[i].fName:= RelativePathToFullPath(Path_Session, aXML.GetValue(curItemPath+'fName', ''));
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_SaveSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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
     aXML.DeletePath(SES_SourceFiles);
     aXML.SetValue(SES_SourceFiles+'Count', Length(SourceFiles));
     aXML.SetValue(SES_SourceFiles+'iSourceFiles', iSourceFiles);
     aXML.SetValue(SES_SourceFiles+'lastCropped', lastCropped);
     aXML.SetValue(SES_SourceFiles+'lastLenTaked', lastLenTaked);
     for i:=0 to Length(SourceFiles)-1 do
     begin
       curItemPath :=SES_SourceFiles+'Item' + IntToStr(i)+'/';
       aXML.SetValue(curItemPath+'cCount', SourceFiles[i].cCount);
       aXML.SetValue(curItemPath+'cStart', SourceFiles[i].cStart);
       aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(Path_Session, SourceFiles[i].fName));
     end;

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

function TDigIt_Main.SES_LoadDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
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

procedure TDigIt_Main.SES_SaveDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

procedure TDigIt_Main.SES_LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

     newCount := aXML.GetValue(SES_CapturedFiles+'Count', 0);
     iCapturedFiles:= aXML.GetValue(SES_CapturedFiles+'iCapturedFiles', -1);
     newSelected :=aXML.GetValue(SES_CapturedFiles+'Selected', -1);

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
         curItemPath :=SES_CapturedFiles+'Item' + IntToStr(i)+'/';
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

procedure TDigIt_Main.SES_SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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
     aXML.DeletePath(SES_CapturedFiles);
     lenCapturedFiles:= Length(CapturedFiles);

     if (lenCapturedFiles > 0) then
     begin
       aXML.SetValue(SES_CapturedFiles+'Count', Length(CapturedFiles));
       aXML.SetValue(SES_CapturedFiles+'iCapturedFiles', iCapturedFiles);

       if lvCaptured.Selected=nil
       then aXML.DeleteValue(SES_CapturedFiles+'Selected')
       else aXML.SetValue(SES_CapturedFiles+'Selected', lvCaptured.Selected.Index);

       for i:=0 to Length(CapturedFiles)-1 do
       begin
         curItemPath :=SES_CapturedFiles+'Item' + IntToStr(i)+'/';
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

procedure TDigIt_Main.SES_LoadLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

procedure TDigIt_Main.SES_SaveLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

procedure TDigIt_Main.SES_SaveSource_CapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     SES_SaveSourceFiles(aXML, IsAutoSave);
     SES_SaveCapturedFiles(aXML, IsAutoSave);
     SES_SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_SaveSource_CapturedIndexes(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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
       aXML.SetValue(SES_SourceFiles+'iSourceFiles', iSourceFiles);
       aXML.SetValue(SES_SourceFiles+'lastCropped', lastCropped);
       aXML.SetValue(SES_SourceFiles+'lastLenTaked', lastLenTaked);
     end;

     if (Length(CapturedFiles) > 0) then aXML.SetValue(SES_CapturedFiles+'iCapturedFiles', iCapturedFiles);

     if lvCaptured.Selected=nil
     then aXML.DeleteValue(SES_CapturedFiles+'Selected')
     else aXML.SetValue(SES_CapturedFiles+'Selected', lvCaptured.Selected.Index);

     SES_SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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
       UI_FillCounter;
       imgManipulation.CropAreas.Load(aXML, 'CropAreas');
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

procedure TDigIt_Main.SES_LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

     aXML.ReadObject(SES_PageSettings+'Page/', imgManipulation.EmptyImage);

     PageResizeUnitType := aXML.GetValue(SES_PageSettings+'ResizeUnitType', 0);
     if (imgManipulation.EmptyImage.Width = 0) or
        (imgManipulation.EmptyImage.Height = 0) then PageResizeUnitType:= 0;

     PageResize:= resFixedWidth;
     aXML.GetValue(SES_PageSettings+'Resize', PageResize, TypeInfo(TDigItFilter_Resize));

     PageRotate:= rotNone;
     aXML.GetValue(SES_PageSettings+'Rotate', PageRotate, TypeInfo(TDigItFilter_Rotate));

     PageFlip:= flipNone;
     aXML.GetValue(SES_PageSettings+'Flip', PageFlip, TypeInfo(TDigItFilter_Flip));

  finally
    UI_FillPageSizes;
    UI_FillPageRotateFlip;

    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_SavePageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+Session_File+Ext_Sess);

     aXML.WriteObject(SES_PageSettings+'Page/', imgManipulation.EmptyImage);

     aXML.SetValue(SES_PageSettings+'ResizeUnitType', PageResizeUnitType);
     aXML.SetValue(SES_PageSettings+'Resize', PageResize, TypeInfo(TDigItFilter_Resize));
     aXML.SetValue(SES_PageSettings+'Rotate', PageRotate, TypeInfo(TDigItFilter_Rotate));
     aXML.SetValue(SES_PageSettings+'Flip', PageFlip, TypeInfo(TDigItFilter_Flip));

     if IsAutoSave then SessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.SES_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

procedure TDigIt_Main.SES_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
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

procedure TDigIt_Main.CFG_Load;
var
   aXML: TRttiXMLConfig;

begin
  try
     aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

  finally
    aXML.Free;
  end;
end;

procedure TDigIt_Main.CFG_Save;
var
   aXML: TRttiXMLConfig;

begin
  try
     aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

  finally
    aXML.Free;
  end;
end;

procedure TDigIt_Main.CFG_Load_LastSession(aXML: TRttiXMLConfig; var APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

     APath:= SetDirSeparators(aXML.GetValue('LastSession/Path', ''));
     AFile:= SetDirSeparators(aXML.GetValue('LastSession/File', ''));

  finally
     if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.CFG_Save_LastSession(aXML: TRttiXMLConfig; const APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(Path_Config+File_Options);

     aXML.SetValue('LastSession/Path', APath);
     aXML.SetValue('LastSession/File', AFile);

  finally
     if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.PROF_Load;
var
   aXML: TRttiXMLConfig;
   i, iCount: Integer;

begin
  try
     aXML:= TRttiXMLConfig.Create(Path_Config+File_Profiles);

     //Load Profiles
     Profiles:= nil; //Avoid possible data overlaps by eliminating any existing array
     iCount:= aXML.GetValue('Profiles/Count', 0);
     SetLength(Profiles, iCount);
     for i:=0 to iCount-1 do
     begin
       Profiles[i]:= aXML.GetValue('Profiles/Item'+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
     end;

  finally
    aXML.Free;
  end;
end;

procedure TDigIt_Main.PROF_Save;
var
   aXML: TRttiXMLConfig;
   i, iCount: Integer;

begin
  try
     aXML:= TRttiXMLConfig.Create(Path_Config+File_Profiles);

     //Save SourceFiles array
     aXML.DeletePath('Profiles/');
     aXML.SetValue('Profiles/Count', Length(Profiles));
     for i:=0 to Length(Profiles)-1 do
     begin
       aXML.SetValue('Profiles/Item'+IntToStr(i)+'/Name', Profiles[i]);
     end;

  finally
    aXML.Free;
  end;
end;

function TDigIt_Main.Source_Select(newSourceIndex, newSourceSubIndex: Integer): Boolean;
begin
  Result:= False;
  try
     if theBridge.SourcesImpl.Select(newSourceIndex, newSourceSubIndex, True) then
     begin
       if (theBridge.SourcesImpl.Selected <> rSource) then
       begin
         { #note -oMaxM : rSource Switched...Do something? }
       end;

       rSource:= theBridge.SourcesImpl.Selected;
       rSourceName:= theBridge.SourcesImpl.SelectedName;
       rSourceParams:= theBridge.SourcesImpl.SelectedParams;

       SES_SaveSource(nil, True);

       Result:= True;
     end;
     //else MessageDlg('DigIt', Format(rsSourceNotSelected, [newSourceIndex]), mtInformation, [mbOk], 0);

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
            if TDest_SaveFiles_Settings.Execute(SaveFormat, SaveWriter, Path_Session_Pictures) then
            begin
              SaveExt:= SuggestImageExtension(SaveFormat);
              SES_SaveDestination(nil, True);
            end;

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
  CropArea :=TCropArea(cbCropList.Items.Objects[cbCropList.ItemIndex]);
  CropArea.Name :=edCropName.Text;
end;

procedure TDigIt_Main.btCrop_AddClick(Sender: TObject);
var
   newCropArea :TCropArea;

begin
  if edCropUnit_Type.ItemIndex=0
  then newCropArea :=imgManipulation.addCropArea(Rect(50, 50, 100, 100))
  else newCropArea :=imgManipulation.addCropArea(Rect(1, 1, 2, 2), TResolutionUnit(edCropUnit_Type.ItemIndex));

  newCropArea.BorderColor :=VGALime;
  edCropName.SetFocus;
end;

procedure TDigIt_Main.btCrop_DelClick(Sender: TObject);
var
   CropArea :TCropArea;
   curIndex :Integer;

begin
  curIndex :=cbCropList.ItemIndex;
  if (curIndex>-1) then
  begin
    CropArea :=TCropArea(cbCropList.Items.Objects[curIndex]);
    imgManipulation.delCropArea(CropArea);
    cbCropList.ItemIndex:=cbCropList.Items.IndexOfObject(imgManipulation.SelectedCropArea);
  end;
  UI_FillCropArea(imgManipulation.SelectedCropArea);
end;

procedure TDigIt_Main.cbCropListChange(Sender: TObject);
begin
   imgManipulation.SelectedCropArea :=TCropArea(cbCropList.Items.Objects[cbCropList.ItemIndex]);
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
    0: SES_Load(True);
    1: SES_Save(True);
    2: SES_ClearAutoSave(False);
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
  //If we are in FullArea Mode and user add an Area switch to Custom Mode
  if (CropMode <> diCropCustom) then setCropMode(diCropCustom);

  curIndex :=imgManipulation.CropAreas.IndexOf(CropArea);

  if (CropArea.Name='') then CropArea.Name:='Name '+IntToStr(curIndex);

  CropArea.Icons:=[cIcoIndex];

  cbCropList.AddItem(CropArea.Name, CropArea);
  cbCropList.ItemIndex:=cbCropList.Items.IndexOfObject(CropArea);

  if not(SES_Loading) then
  begin
    UI_FillCropArea(CropArea);
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
         delIndex :=cbCropList.Items.IndexOfObject(CropArea);
         if (delIndex<>-1)
         then cbCropList.Items.Delete(delIndex);

         //If there are no more Crops switch to FullArea Mode
         if (imgManipulation.CropAreas.Count = 0)
         then setCropMode(diCropFull)
         else panelCropArea.Enabled:= (cbCropList.Items.Count>0);

         if not(SES_Loading) then UI_ToolBar;
    end;
  except
  end;
end;

procedure TDigIt_Main.ChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
begin
  if (cbCropList.ItemIndex > -1) and (cbCropList.Items.Objects[cbCropList.ItemIndex] = CropArea) then
  begin
    UI_FillCropArea(CropArea);

    //if the Name is Changed change in the comboListbox
    if (CropArea.Name<>cbCropList.Items.Strings[cbCropList.ItemIndex])
    then cbCropList.Items.Strings[cbCropList.ItemIndex] :=CropArea.Name;
  end;
end;

procedure TDigIt_Main.SelectedChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
var
   newIndex :Integer;
begin
   if (imgManipulation.SelectedCropArea <> nil)
   then newIndex :=cbCropList.Items.IndexOfObject(imgManipulation.SelectedCropArea)
   else newIndex :=-1;

   cbCropList.ItemIndex:=newIndex;
   UI_FillCropArea(imgManipulation.SelectedCropArea);
end;

procedure TDigIt_Main.Separator2DrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
var
   textS: TTextStyle;

begin
  ACanvas.DrawFocusRect(ARect);
  textS.Alignment:= taCenter;
  textS.Layout:= Graphics.tlCenter;
  textS.Opaque:= False;
  ACanvas.TextRect(ARect, 0,0, TMenuItem(Sender).Hint, textS);
  imgListMain.Draw(ACanvas, ARect.Left, ARect.Top, TMenuItem(Sender).ImageIndex, True);
end;

procedure TDigIt_Main.Separator2MeasureItem(Sender: TObject; ACanvas: TCanvas;
  var AWidth, AHeight: Integer);
begin
  AHeight:= 32;
end;

procedure TDigIt_Main.tbCapturedPDFClick(Sender: TObject);
begin
  TDigIt_ExportFiles.Execute(Application.Title, CapturedFiles, True);
end;

procedure TDigIt_Main.tbCapturedToImgClick(Sender: TObject);
begin
  TDigIt_ExportFiles.Execute(Application.Title, CapturedFiles, False);
end;

procedure TDigIt_Main.MenuSourcePopup(Sender: TObject);
begin
  BuildSourcesMenu(Self, menuSources, @UI_SourceMenuClick, Source);
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
        rollCrops.Enabled:= False; rollCrops.Collapsed:= True;
        rollCounters.Collapsed:= True;

        tbCropMode.Caption:= rsCropFull;
      end;
      diCropCustom: begin
        tbCrop.Visible:= True;
        imgManipulation.Opacity:= 128;
        rollCrops.Enabled:= True; rollCrops.Collapsed:= False;
        rollCounters.Collapsed:= False;
        tbCropMode.Caption:= rsCropCust;
      end;
    end;

    CropMode:= ANewCropMode;

    if not(SES_Loading) then
    begin
      UI_FillCounter;
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

function TDigIt_Main.SourceFiles_Add(AArray: IDigIt_ArrayR_PChars; AStartIndex: Integer): Integer;
var
   oldLength, i: Integer;
   curImageFile: PChar;

begin
  Result:= 0;
  if (AArray <> nil) then
  begin
    oldLength:= Length(SourceFiles);
    Result:= AArray.GetCount;
    if (Result > 0) then
    begin
      if (AStartIndex < 0) or (AStartIndex > oldLength)
      then AStartIndex:= oldLength;

      //Add more space, if needed, to end of Array SourceFiles
      if (AStartIndex+Result > oldLength) then SetLength(SourceFiles, AStartIndex+Result);

      for i:=0 to Result-1 do
        if AArray.Get(i, curImageFile) then
        begin
          SourceFiles[AStartIndex+i].fName:= curImageFile;
          StrDispose(curImageFile);
        end;
    end;
  end;
end;

function TDigIt_Main.SourceFiles_Add(AFileName: String; AStartIndex: Integer): Integer;
var
   oldLength: Integer;

begin
  Result:= 0;
  if (AFileName <> '') then
  begin
    //Add files to end of Array SourceFiles
    oldLength:= Length(SourceFiles);

    if (AStartIndex < 0) or (AStartIndex > oldLength)
    then AStartIndex:= oldLength;

    //Add more space, if needed, to end of Array SourceFiles
    if (AStartIndex+1 > oldLength) then SetLength(SourceFiles, AStartIndex+1);

    SourceFiles[AStartIndex].fName:= AFileName;
    Result:= 1;
  end;
end;

procedure TDigIt_Main.SourceFiles_Clear(ClearSourceInst: Boolean);
begin
  iSourceFiles:= -1;
  lastCropped:= -1;
  lastLenTaked:= 0;
  SourceFiles:= nil;
  if ClearSourceInst and (rSource <> nil) then rSource^.Inst.Clear;
end;

function TDigIt_Main.CropFile_Full(AFileName: String; isReTake: Boolean): Boolean;
begin
  Result:= (AFileName <> '') and LoadImage(AFileName, False);
  if Result then SaveCallBack(imgManipulation.Bitmap, nil, Integer(isReTake));
end;

procedure TDigIt_Main.CropFile_Full(AStartIndex: Integer; isReTake: Boolean);
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
     old_CounterValue:= Counter.Value;
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

       UserCancel:= DigIt_Progress.Cancelled or
                    not(CropFile_Full(SourceFiles[i].fName, isReTake and (i < lastLenTaked)));
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
       Counter.Value:= old_CounterValue;
     end;

  finally
    UI_ToolBar;
    UI_FillCounter;
  end;
end;

procedure TDigIt_Main.CropFiles(ASourceFileIndex: Integer; isReTake: Boolean);
var
   oldCount: DWord;

begin
  iSourceFiles:= ASourceFileIndex;

  if isReTake
  then begin
         oldCount:= SourceFiles[iSourceFiles].cCount;
         if (oldCount <> imgManipulation.CropAreas.Count) then
         begin
           if (oldCount < imgManipulation.CropAreas.Count)
           then begin
                  { #todo 10 -oMaxM : Re index - add space for more files }
                  MessageDlg('DigIt', 'To-Do: add space for more files', mtInformation, [mbOk], 0);
                end
           else begin
                  { #todo 10 -oMaxM : Re index - delete extra files }
                  MessageDlg('DigIt', 'To-Do: delete extra files', mtInformation, [mbOk], 0);
                end;
         end;

         Counter_Assign(SourceFiles[iSourceFiles].cStart);
       end
  else begin
         if (iSourceFiles < lastCropped) then
         begin
           { #todo 10 -oMaxM : Re index - delete extra files }
           MessageDlg('DigIt', 'To-Do: insert files', mtInformation, [mbOk], 0);
         end;

         SourceFiles[iSourceFiles].cStart:= Counter.Value;
       end;

  imgManipulation.getAllBitmaps(@SaveCallBack, Integer(isReTake), True);
  SourceFiles[iSourceFiles].cCount:= imgManipulation.CropAreas.Count;
end;

procedure TDigIt_Main.SetSaveWriter(AFormat: TBGRAImageFormat);
begin
  SaveFormat:= AFormat;
  try
     if (SaveWriter <> nil) then SaveWriter.Free;
     SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
     SaveExt:= SuggestImageExtension(SaveFormat);

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
  selectedProfile:= -1;

  LoadedFile:= '';

  PageResizeUnitType:= 0;
  PageResize:= resNone;
  PageRotate:= rotNone;
  PageFlip:= flipNone;

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
  if (cbCropList.ItemIndex<0)
  then Result :=nil
  else Result :=TCropArea(cbCropList.Items.Objects[cbCropList.ItemIndex]);
end;

procedure TDigIt_Main.Counter_Dec(AValue: Integer);
begin
  if (AValue > 0) then
  begin
    Counter.Value:= Counter.Value-AValue;
    dec(iCapturedFiles, AValue);
    if (iCapturedFiles < 0) then iCapturedFiles:= -1;
  end;
end;

procedure TDigIt_Main.Counter_Inc(AValue: Integer);
begin
  if (AValue > 0) then
  begin
    Counter.Value:= Counter.Value+AValue;
    inc(iCapturedFiles, AValue);
    if (iCapturedFiles > Length(CapturedFiles)-1) then iCapturedFiles:= Length(CapturedFiles)-1;
  end;
end;

procedure TDigIt_Main.Counter_Assign(AValue: Integer);
begin
  Counter.Value:= AValue;
  iCapturedFiles:= AValue-1;
  if (iCapturedFiles > Length(CapturedFiles)-1) then iCapturedFiles:= Length(CapturedFiles)-1;
end;

procedure TDigIt_Main.SetSessionModified(AValue: Boolean);
begin
  if (rSessionModified = AValue) then exit;

  rSessionModified:= AValue;

  UI_Caption;
end;

procedure TDigIt_Main.UI_DestinationMenuClick(Sender: TObject);
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

procedure TDigIt_Main.UI_SourceMenuClick(Sender: TObject);
var
   newSourceIndex,
   newSourceSubIndex: Integer;

begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  try
    SourcesMenuTag_decode(TMenuItem(Sender).Tag, newSourceIndex, newSourceSubIndex);
    Source_Select(newSourceIndex, newSourceSubIndex);

  finally
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.UI_FillCropArea(ACropArea: TCropArea);
begin
   if (ACropArea<>nil)
   then begin
           inFillBoxUI :=True;
           panelCropArea.Enabled :=True;
           edCropName.Text :=ACropArea.Name;
           edCropUnit_Type.ItemIndex :=Integer(ACropArea.AreaUnit);

           if (ACropArea.AreaUnit=ruNone)
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
         (*  edCropLeft.MaxValue:=ACropArea.MaxWidth;
           edCropTop.MaxValue:=ACropArea.MaxHeight;
           edCropWidth.MaxValue:=edCropLeft.MaxValue;
           edCropHeight.MaxValue:=edCropTop.MaxValue; *)

           edCropLeft.Value :=ACropArea.Left;
           edCropTop.Value :=ACropArea.Top;
           edCropWidth.Value :=ACropArea.Width;
           edCropHeight.Value :=ACropArea.Height;

           //Aspect Ratio
           changingAspect :=True;
           Case ACropArea.KeepAspectRatio of
           bParent :rgCropAspect.ItemIndex:=0;
           bFalse  :rgCropAspect.ItemIndex:=1;
           bTrue   :rgCropAspect.ItemIndex:=2;
           end;
           edCropAspectPersonal.Text:=ACropArea.AspectRatio;
           changingAspect:=False;

           inFillBoxUI :=False;
        end
   else panelCropArea.Enabled :=False;

   btPageSizesToCrops.Enabled:= (imgManipulation.CropAreas.Count > 0);
end;

procedure TDigIt_Main.UI_FillCounter;
begin
   if (Counter <> nil)
   then begin
           inFillCounterUI:= True;
           panelCounter.Enabled:= True;
           edCounterValue.Value:= Counter.Value_Next;
           edCounterValueStringDigits.Value:= Counter.Value_StringDigits;
           edCounterValueStringPre.Text:= Counter.Value_StringPre;
           edCounterValueStringPost.Text:= Counter.Value_StringPost;
           {$ifopt D+} lbPrevious.Caption:= Format(rsCounterPrev, [Counter.Value_Previous]); {$endif}
           lbCounterExample.Caption:= Counter.GetValue(True);
           inFillCounterUI:= False;
        end
   else panelCounter.Enabled:= False;
end;

procedure TDigIt_Main.UI_SelectCurrentCaptured(AddValue: Integer);
var
   selI: Integer;

begin
   selI:= iCapturedFiles+AddValue;

   if (selI >= 0) and (selI < lvCaptured.Items.Count)
   then lvCaptured.Selected:= lvCaptured.Items[selI]
   else lvCaptured.Selected:= nil;
end;

procedure TDigIt_Main.UI_SelectNextCaptured(AddValue: Integer);
var
   selI: Integer;

begin
   selI:= iCapturedFiles+AddValue+1;

   if (selI >= 0) and (selI < lvCaptured.Items.Count)
   then lvCaptured.Selected:= lvCaptured.Items[selI]
   else lvCaptured.Selected:= nil;
end;

procedure TDigIt_Main.UI_FillPageSizes;
begin
  inFillPagesUI:= True;

  panelPageSize.Enabled:= (PageResizeUnitType > 0);

  if panelPageSize.Enabled then
  begin
    if (PageResizeUnitType <> Integer(imgManipulation.EmptyImage.ResolutionUnit)+1) then
    begin
      imgManipulation.EmptyImage.ResolutionUnit:= TResolutionUnit(PageResizeUnitType-1);
    end;

    if (imgManipulation.EmptyImage.ResolutionUnit=ruNone)
    then begin
           edPageWidth.DecimalPlaces:=0;
           edPageHeight.DecimalPlaces:=0;
         end
    else begin
           edPageWidth.DecimalPlaces:=3;
           edPageHeight.DecimalPlaces:=3;
         end;

    edPageWidth.Value:= imgManipulation.EmptyImage.ResolutionWidth;
    edPageHeight.Value:= imgManipulation.EmptyImage.ResolutionHeight;
  end;

  inFillPagesUI:= False;
end;

procedure TDigIt_Main.UI_FillPageRotateFlip;
begin
  inFillPagesUI:= True;

  Case PageRotate of
    rotLeft90: btPRotateLeft.Down:= True;
    rotRight90: btPRotateRight.Down:= True;
    rot180: btPRotate180.Down:= True;
    else begin
      btPRotateLeft.Down:= False;
      btPRotateRight.Down:= False;
      btPRotate180.Down:= False;
    end;
  end;

  Case PageFlip of
    flipVertical: btPFlipV.Down:= True;
    flipHorizontal: btPFlipH.Down:= True;
    else begin
         btPFlipV.Down:=  False;
         btPFlipH.Down:= False;
    end;
  end;

  inFillPagesUI:= False;
end;

procedure TDigIt_Main.UI_UpdateCropAreaList;
var
   i :Integer;
   CropArea:TCropArea;

begin
  cbCropList.OnChange:=nil;

  cbCropList.Clear;
  for i:=0 to imgManipulation.CropAreas.Count-1 do
  begin
    CropArea :=imgManipulation.CropAreas.items[i];
    cbCropList.AddItem(CropArea.Name, CropArea);
  end;
  cbCropList.ItemIndex:=cbCropList.Items.IndexOfObject(imgManipulation.SelectedCropArea);

  cbCropList.OnChange:=@cbCropListChange;
end;

procedure TDigIt_Main.UI_ToolBar;
var
   bCommonCond,
   actCrop_Enabled: Boolean;
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
         actTakeRe.Enabled:= bCommonCond and (lastLenTaked > 0);
         actCrop_Enabled:= bCommonCond and
                           ((lenSources = 1) or (iSourceFiles >= 0) and (iSourceFiles >= lenSources-1));
         actCropNext.Enabled:= bCommonCond and (lenSources > 0);
                               //bCommonCond and (lenSources > 1) and (iSourceFiles >= 0) and (iSourceFiles < lenSources-1);
         actGoNext.Enabled:= actCropNext.Enabled and not(actCrop_Enabled);
         actGoBack.Enabled:= bCommonCond and (lenSources > 1) and (iSourceFiles > 0) and (iSourceFiles <= lenSources);
         actCropAll.Enabled:= actCropNext.Enabled and not(actCrop_Enabled);

         actClearQueue.Enabled:= bCommonCond and (lenSources > 0);
         if (lenSources > 0)
         then tbClearQueue.Caption:= actClearQueue.Caption+' ('+IntToStr(lenSources)+')'
         else tbClearQueue.Caption:= actClearQueue.Caption;

         if actCrop_Enabled
         then begin
                actCropNext.Caption:= rsCrop;
                actCropNext.ImageIndex:= 11;
              end
         else begin
                actCropNext.Caption:= rsCropNext;
                actCropNext.ImageIndex:= 12;
              end;

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
  tbCapturedToImg.Enabled:= (lenCaptured > 0);
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

procedure TDigIt_Main.UI_Caption;
var
   addStr: String;

begin
  if rSessionModified then addStr:= '* ' else  addStr:= '';

  if (Path_Session = Path_DefSession)
  then Caption :=addStr+'DigIt'
  else Caption :=addStr+'DigIt'+' - '+Session_File;
end;

end.

