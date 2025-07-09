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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, Menus, ComCtrls, ActnList, Spin, ShellCtrls, EditBtn, SpinEx,
  LCLVersion, LCLType, Laz2_XMLCfg, FPImage,
  BGRABitmap, BGRABitmapTypes, BGRAPapers,
  BGRAImageManipulation,  BGRASpeedButton, BCPanel, BCLabel, BCListBox, BGRAImageList,
  BCExpandPanels,
  DigIt_Types, DigIt_Utils, Digit_Bridge_Intf, Digit_Bridge_Impl,
  DigIt_Settings, DigIt_Sources, DigIt_Session, DigIt_Counter;

resourcestring
  rsNewWork = 'Start a New Work Session?';
  rsContinueWork = 'Continue from last Work Session?'#13#10'%s';
  rsContinueAutoWork = 'Continue from Auto Saved Work Session?'#13#10'%s';
  rsSaveWork = 'Save the Work Session?';
  //rsMiddlePage = 'The page is within the already processed range'#13#10'Should I put it in the middle?';
  //rsNotImpl = 'Not yet implemented';
  rsClearCrops = 'Clear Crop Areas ?';
  rsCrop = 'Crop (F5)';
  rsCropNext = 'Crop + Next (F5)';
  rsCropFull = 'Full Area';
  rsCropCust = 'Custom';
  rsCropToDo = '%d files to do';
  rsCounterPrev = 'Value Previous: %d';

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
    edPageUnit: TComboBox;
    edPageResizeType: TComboBox;
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
    itemProfiles_Edit: TMenuItem;
    itemProfiles_AddCurrent: TMenuItem;
    Separator4: TMenuItem;
    menuSaveProfiles: TMenuItem;
    menuSaveSettings: TMenuItem;
    menuProjectSaveAs: TMenuItem;
    menuSaveXML: TMenuItem;
    menuLoadXML: TMenuItem;
    menuProfiles: TPopupMenu;
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
    tbProfiles: TToolButton;

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
    procedure edPageResizeTypeChange(Sender: TObject);
    procedure itemCropModeClick(Sender: TObject);
    procedure itemProfiles_AddCurrentClick(Sender: TObject);
    procedure itemProfiles_EditClick(Sender: TObject);
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
    inFillCounterUI,
    inFillBoxUI,
    inFillPagesUI,
    imgListThumb_Changed: Boolean;

    lastCropped,
    lastLenTaked,     //Used in Take Again
    iSourceFiles,
    iCapturedFiles: Integer;

    Session: TDigIt_Session;
    Profiles: TStringArray;

    function GetCurrentCropArea: TCropArea;

    function GetSessionModified: Boolean;
    procedure SetSessionModified(AValue: Boolean);

    function LoadSessionFile(APath, AFile: String; IsAutoSave: Boolean=False): Boolean; overload;
    function LoadSessionFile(AFileName: String): Boolean; overload;
    function SaveSessionFile(AFileName: String): Boolean;

    //DELETE When Dialogs Interface is done
    procedure SES_Load(IsAutoSave: Boolean);
    procedure SES_Save(IsAutoSave: Boolean);
    procedure SES_ClearAutoSave(AFromStartup: Boolean);

    //Transform to TLoadSaveXMLEvent and pass to Session.OnLoad/Save
    procedure SES_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);

    procedure UI_DestinationMenuClick(Sender: TObject);
    procedure UI_SourceMenuClick(Sender: TObject);
    procedure UI_ProfileMenuClick(Sender: TObject);

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

    function LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
    procedure EmptyImage(saveToXML: Boolean);

    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer);

    procedure SetPageResizeType(AValue: TDigItFilter_Resize; PredefValues: Boolean);

(*oldcode
    procedure SES_LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SES_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
*)
    function Source_Select(newSourceIndex, newSourceSubIndex: Integer): Boolean;
    function Destination_Select(newDestinationIndex: Integer): Boolean;

    procedure setCropMode(ANewCropMode: TDigItCropMode);

(*    procedure CropFile_Full(AStartIndex: Integer; isReTake: Boolean); overload;
    procedure CropFiles(ASourceFileIndex: Integer; isReTake: Boolean);
*)
    procedure Clear_Captured;

    procedure ItemSizesClick(Sender: TObject);
    procedure PageSizesClick(Sender: TObject);

  public
    property SessionModified: Boolean read GetSessionModified write SetSessionModified;
  end;

var
  DigIt_Main: TDigIt_Main;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLProc, fppdf, FileUtil, LazFileUtils,
  BGRAUnits, BGRAWriteJPeg, BGRAWriteTiff, BGRAFormatUI,
  MM_StrUtils,
  DigIt_Destination_SaveFiles_SettingsForm,
  //DigIt_Form_PDF,
  DigIt_Form_ExportFiles, DigIt_Form_Templates, DigIt_Form_BuildDuplex, DigIt_Form_Profiles;


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
  Closing :=False;
  changingAspect :=False;
  inFillCounterUI :=False;
  inFillBoxUI :=False;
  inFillPagesUI :=False;
  imgListThumb_Changed:= False;

  lastNewBoxNum :=0;
  TStringList(cbCropList.Items).OwnsObjects:=False;

  Session:= TDigIt_Session.Create;

  Settings.Load(nil);

  TDigIt_Profiles_Form.LoadFromXML(Path_Config+File_Profiles, Profiles);
  BuildProfilesMenu(Self, menuProfiles, @UI_ProfileMenuClick, Profiles);
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
     //1) If Application is started with a Session File Param then Open It
     if FileExists(ParamStr(1)) and
        (MessageDlg('DigIt', Format(rsContinueWork, [ParamStr(1)]),
                    mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     then sessLoaded:= Session.LoadSessionFile(ParamStr(1));

     if not(sessLoaded) then
     begin
       //2) If in Settings there is a Session Opened then Open It
       optPath_Session:= Settings.Session.Startup_Path;
       optFile_Session:= Settings.Session.Startup_File;

       if (optPath_Session <> '') and (optFile_Session <> '') and
          FileExists(optPath_Session+optFile_Session+Ext_Sess) then
       begin
         //If there is an AutoSave with a date later than the session date ask user
         if FileExists(optPath_Session+optFile_Session+Ext_AutoSess) and
            (FileAge(optPath_Session+optFile_Session+Ext_AutoSess) > FileAge(optPath_Session+optFile_Session+Ext_Sess)) and
            (MessageDlg('DigIt', Format(rsContinueAutoWork, [optPath_Session+optFile_Session]),
                        mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then sessLoaded:= Session.LoadSessionFile(optPath_Session, optFile_Session, True);

         if not(sessLoaded) and
           (MessageDlg('DigIt', Format(rsContinueWork, [optPath_Session+optFile_Session]),
                       mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then sessLoaded:= Session.LoadSessionFile(optPath_Session, optFile_Session);
       end;
     end;

     if not(sessLoaded) then
     begin
       //3 Ask to Open AutoSave in AppData if exists
       if FileExists(Path_DefSession+File_DefSession+Ext_AutoSess) then
       begin
         if (MessageDlg('DigIt', Format(rsContinueAutoWork, ['<no name>']), mtConfirmation, [mbYes, mbNo], 0)=mrYes)
         then begin
                Session.Load(True);
                sessLoaded:= True;
              end
         else begin
                Session.ClearAutoSave(True);
                sessLoaded:= False;
              end;
       end;
     end;

     if not(sessLoaded) then
     begin
       //4 Set Last Used Source with it's Params
       Session.LoadSource(nil, False, '', Path_Config+File_Config);
       setCropMode(diCropFull);
     end;

     if (Path_Session = Path_DefSession) then Settings.Save_StartupSession(nil, '', '');

  except
    Path_Session:= Path_DefSession;
    Path_Session_Scan:= Path_DefSession_Scan;
    Path_Session_Pictures:= Path_DefSession_Pictures;

    Session.SetDefaultStartupValues;

    setCropMode(diCropFull);
    Settings.Save_StartupSession(nil, '', '');
  end;

  UI_Caption;
end;

procedure TDigIt_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  closing :=True;

  //AutoSave
  if SessionModified then //FileExists(Path_Session+Session_File+Ext_AutoSess)
  begin
    Session.Save(True);

    //Save Current Session ?
    actSessionSaveExecute(nil);
    CanClose:= not(dlgRes = mrCancel);

    closing:= CanClose;
  end;
end;

procedure TDigIt_Main.FormDestroy(Sender: TObject);
begin
  theBridge.Free;
//oldcode  SourceFiles:= nil;
//oldcode  if (SaveWriter <> nil) then SaveWriter.Free;
  Counter.Free;
  Session.Free;
end;

procedure TDigIt_Main.actPreviewExecute(Sender: TObject);
var
  curImageFile: String;

begin
  try
     if Sources.Take(takeActPreview, curImageFile)
     then LoadImage(curImageFile, True)
     else MessageDlg(rsNoFilesDownloaded, mtError, [mbOk], 0);

  finally
    UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actTakeReExecute(Sender: TObject);
begin
  if (Session.CropMode = diCropFull) then UI_SelectCurrentCaptured(-lastLenTaked);

  try
     Session.actTake(True);

  finally
     UI_SelectCurrentCaptured;
     UI_FillCounter;
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actTakeExecute(Sender: TObject);
var
   res: DWord;
   StartIndex,
   oldLength: Integer;

begin
  try
     if (Sender = actTake)
     then res:= Session.actTake(False)
     else
     if (Sender = actTakeBuildDuplex)
     then res:= Session.actTake(False, @TWizardBuildDuplex.Execute);

  finally
     UI_ToolBar;
     UI_FillCounter;
  end;
end;

procedure TDigIt_Main.actTimerTakeExecute(Sender: TObject);
begin
  //
end;

procedure TDigIt_Main.actCropNextExecute(Sender: TObject);
begin
  try
     Session.actCropNext;

  finally
     UI_SelectNextCaptured;
     UI_FillCounter;
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actGoNextExecute(Sender: TObject);
begin
  try
     Session.actGoNext;

  finally
     UI_SelectNextCaptured;
     UI_FillCounter;
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actGoBackExecute(Sender: TObject);
begin
  try
     Session.actGoBack;

  finally
     UI_SelectNextCaptured;
     UI_FillCounter;
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actCropAllExecute(Sender: TObject);
begin
  try
     Session.actCropAll;

  finally
     UI_SelectNextCaptured;
     UI_FillCounter;
     UI_ToolBar;
  end;
end;

procedure TDigIt_Main.actClearQueueExecute(Sender: TObject);
begin
  try
    Session.actClearQueue;

  finally
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

  if (edPageResizeType.ItemIndex = 0)
  then BuildPaperSizesMenu(ruPixelsPerCentimeter, Self, menuPaperSizes, @PageSizesClick, 4, 5)
  else BuildPaperSizesMenu(TResolutionUnit(edPageResizeType.ItemIndex-1), Self, menuPaperSizes, @PageSizesClick, 4, 5);

  menuPaperSizes.PopUp;
end;

procedure TDigIt_Main.btPageSizesToCropsClick(Sender: TObject);
begin
  imgManipulation.SetEmptyImageSizeToCropAreas(True);

  if edPageResizeType.ItemIndex=0
  then begin
         //if page size is not defined set it to PixelsPerCentimeter (fucking inch)
         imgManipulation.EmptyImage.ResolutionUnit:=ruPixelsPerCentimeter;
         edPageResizeType.ItemIndex:=3;
       end
  else imgManipulation.EmptyImage.ResolutionUnit:=TResolutionUnit(edPageResizeType.ItemIndex-1);

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
  oldPageFlip:= Session.PageFlip;

  if btPFlipV.Down
  then Session.PageFlip:= flipVertical
  else
  if btPFlipH.Down
  then Session.PageFlip:= flipHorizontal
  else Session.PageFlip:= flipNone;

  if not(imgManipulation.Empty) then
  begin
    if FileExists(Session.LoadedFile)
    then LoadImage(Session.LoadedFile, False)
    else begin
           //Flip Image to Initial Value
           Session.FlipImage(imgManipulation.Bitmap, oldPageFlip);

           //Flip Image to New Value
           Session.FlipImage(imgManipulation.Bitmap, Session.PageFlip);

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
  oldPageRotate:= Session.PageRotate;

  if btPRotateLeft.Down
  then Session.PageRotate:= rotLeft90
  else
  if btPRotateRight.Down
  then Session.PageRotate:= rotRight90
  else
  if btPRotate180.Down
  then Session.PageRotate:= rot180
  else Session.PageRotate:= rotNone;

  if not(imgManipulation.Empty) then
  try
    BitmapR:= nil;
    BitmapNewR:= nil;

    if FileExists(Session.LoadedFile)
    then LoadImage(Session.LoadedFile, False)
    else begin
           //Retun Image to Initial Value
           Case oldPageRotate of
           rotNone: BitmapR:= imgManipulation.Bitmap;
           rotLeft90: BitmapR:= Session.RotateImage(imgManipulation.Bitmap, rotRight90);
           rotRight90: BitmapR:= Session.RotateImage(imgManipulation.Bitmap, rotLeft90);
           rot180: BitmapR:= Session.RotateImage(imgManipulation.Bitmap, rot180);
           end;

           //Rotate Image to New Value
           if (Session.PageRotate = rotNone)
           then imgManipulation.Bitmap:= BitmapR
           else begin
                  BitmapNewR:= Session.RotateImage(BitmapR, Session.PageRotate);
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

    if (Session.PageResize = resFullSize)
    then SetPageResizeType(resFixedWidth, False);

    imgManipulation.SetEmptyImageSize(ResUnit, Paper.w, Paper.h);

    if not(imgManipulation.Empty) and
       FileExists(Session.LoadedFile)
    then LoadImage(Session.LoadedFile, False);

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
  if inFillPagesUI or (Session.PageResize = resFullsize) then exit;

  Session.PageSize.Height:= edPageHeight.Value;

  imgManipulation.SetEmptyImageSize(PhysicalToResolutionUnit(Session.PageSize.PhysicalUnit), edPageWidth.Value, edPageHeight.Value);

  if not(imgManipulation.Empty) and
     FileExists(Session.LoadedFile)
  then LoadImage(Session.LoadedFile, False);
end;

procedure TDigIt_Main.edPageWidthChange(Sender: TObject);
begin
  if inFillPagesUI or (Session.PageResize = resFullsize) then exit;

  Session.PageSize.Width:= edPageWidth.Value;

  imgManipulation.SetEmptyImageSize(PhysicalToResolutionUnit(Session.PageSize.PhysicalUnit), edPageWidth.Value, edPageHeight.Value);

  if not(imgManipulation.Empty) and
     FileExists(Session.LoadedFile)
  then LoadImage(Session.LoadedFile, False);
end;

procedure TDigIt_Main.edPageResizeTypeChange(Sender: TObject);
begin
  try
     SetPageResizeType(TDigItFilter_Resize(edPageResizeType.ItemIndex), True);

     if not(imgManipulation.Empty) and
        FileExists(Session.LoadedFile)
     then LoadImage(Session.LoadedFile, False);

  finally
    UI_FillPageSizes;
  end;
end;

procedure TDigIt_Main.itemCropModeClick(Sender: TObject);
begin
  setCropMode(TDigItCropMode(TMenuItem(Sender).Tag));
end;

procedure TDigIt_Main.itemProfiles_AddCurrentClick(Sender: TObject);
begin
  if TDigIt_Profiles_Form.Add(Path_Config+File_Profiles, Profiles, Sources.Selected, Sources.SelectedParams, Sources.SelectedName) then
  try
     BuildProfilesMenu(Self, menuProfiles, @UI_ProfileMenuClick, Profiles);
     menuProfiles.Items[Length(Profiles)-1].Default:= True;

  finally
  end;
end;

procedure TDigIt_Main.itemProfiles_EditClick(Sender: TObject);
begin
  if TDigIt_Profiles_Form.Execute(Path_Config+File_Profiles, Profiles) then
  try
     BuildProfilesMenu(Self, menuProfiles, @UI_ProfileMenuClick, Profiles);

  finally
    DigIt_Profiles_Form.Free; DigIt_Profiles_Form:= nil;
  end;
end;

procedure TDigIt_Main.lvCapturedDblClick(Sender: TObject);
var
   captItem: TListItem;

begin
  captItem:= lvCaptured.Selected;
  if (captItem <> nil) then OpenDocument(Session.CapturedFiles[captItem.Index].fName);
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
  if (captItem <> nil) then HintInfo^.HintStr:= Session.CapturedFiles[captItem.Index].fName;
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

     //Delete Thumb
     imgListThumb.Delete(Session.CapturedFiles[iSelected].iIndex);
     imgListThumb_Changed:= True;

     Session.actCapturedDelete(False, iSelected);

     //Delete Last Item
     lvCaptured.Items.Delete(Length(Session.CapturedFiles));

     //Select Item
     if (iSelected < Length(Session.CapturedFiles)-1)
     then lvCaptured.Selected:= lvCaptured.Items[iSelected]
     else lvCaptured.Selected:= lvCaptured.Items[Length(Session.CapturedFiles)-1];

  finally
     lvCaptured.EndUpdate;
     UI_FillCounter;
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
begin
  if (MessageDlg('DigIt', rsDeleteAll, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  try
     Session.actCapturedDeleteAll(False);

  finally
    UI_ToolBar;
    UI_FillCounter;
  end;
end;

procedure TDigIt_Main.actSessionNewExecute(Sender: TObject);
begin
  try
    if (MessageDlg('DigIt', rsNewWork, mtConfirmation, [mbYes, mbNo], 0)=mrYes) then
    begin
      Session.ClearAutoSave(False);
      Settings.Save_StartupSession(nil, Path_Session, Session.FileName);
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
       Session.LoadSessionFile(OpenSessionDlg.FileName);
       Settings.Save_StartupSession(nil, Path_Session, Session.FileName);
     end;

  finally
  end;
end;

procedure TDigIt_Main.actSessionSaveAsExecute(Sender: TObject);
begin
  if SaveSessionDlg.Execute then
  begin
    SaveSessionFile(SaveSessionDlg.FileName);
    Settings.Save_StartupSession(nil, Path_Session, Session.FileName);
  end;
end;

procedure TDigIt_Main.actSessionSaveExecute(Sender: TObject);
var
   canSave: Boolean;

begin
  try
     if (Sender = nil)
     then begin
            if Settings.Session.ConfirmSaveOnClose and Session.Modified
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
            else Session.Save(False);
          end
     else begin
            //If we are really closing the Application then Clear AutoSave
            if (Sender = nil) and not(dlgRes = mrCancel) and
               (Path_Session = Path_DefSession)
            then Session.ClearAutoSave(True);
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
     curFileName:= Session.CapturedFiles[captItem.Index].fName;
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
     curFileName:= Session.CapturedFiles[captItem.Index].fName;
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

procedure TDigIt_Main.SaveCallBack(Bitmap: TBGRABitmap; CropArea: TCropArea; AUserData: Integer);
var
  savedFile: String;
  captItem: TListItem;

begin
(*
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
*)
end;

function TDigIt_Main.LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
begin
  try
     Result:= Session.LoadImage(AImageFile, saveToXML);
     imgManipulation.Bitmap:= Session.Bitmap;

  finally
  end;
end;

procedure TDigIt_Main.EmptyImage(saveToXML: Boolean);
begin
  try
     Session.EmptyImage(saveToXML);
     imgManipulation.Bitmap:= nil;

  finally
  end;
end;

procedure TDigIt_Main.SetPageResizeType(AValue: TDigItFilter_Resize; PredefValues: Boolean);
begin
  Session.PageResize:= AValue;
  edPageResizeType.ItemIndex:= Integer(AValue);

  if (Session.PageResize = resFullsize)
  then imgManipulation.SetEmptyImageSizeToNull
  else begin
         //if there is a size defined, change only the resolution unit
         (*if (panelPageSize.Enabled)
         then begin
                imgManipulation.EmptyImage.ResolutionUnit:= TResolutionUnit(edPageUnit.ItemIndex);
              end
         else*) if PredefValues then
              Case Session.PageSize.PhysicalUnit of
              puPixel: imgManipulation.SetEmptyImageSize(ruNone,
                                                        Paper_A_inch[4].w * 200, Paper_A_inch[4].h * 200);
              puInch: imgManipulation.SetEmptyImageSize(ruPixelsPerInch,
                                                        Paper_A_inch[4].w, Paper_A_inch[4].h);
              puCentimeter: imgManipulation.SetEmptyImageSize(ruPixelsPerCentimeter,
                                                        Paper_A_cm[4].w, Paper_A_cm[4].h);
              end;

         Session.PageResize:= TDigItFilter_Resize(edPageResizeType.ItemIndex);
       end;
end;

function TDigIt_Main.LoadSessionFile(APath, AFile: String; IsAutoSave: Boolean): Boolean;
var
   oldPath_Session,
   oldPath_Session_Scan,
   oldPath_Session_Pictures,
   oldSession_File: String;

begin
  try
     Result:= Session.LoadSessionFile(APath, AFile, IsAutoSave);

     if not(Result) then
     begin

     end;

  finally
  end;
end;

function TDigIt_Main.LoadSessionFile(AFileName: String): Boolean;
begin
  Result:= LoadSessionFile(ExtractFilePath(AFileName),
                           ExtractFileNameWithoutExt(ExtractFileName(AFileName)));
end;

function TDigIt_Main.SaveSessionFile(AFileName: String): Boolean;
var
   isMove: Boolean;

begin
  try
     Result:= False;
     if (AFileName <> '') then
     try
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

        Session.SaveSessionFile(isMove, AFileName);
        imgListThumb_Changed:= True;
        Result:= True;

     except
       on E: Exception do MessageDlg('DigIt', Format(rsErrSaveWork, [AFileName, E.Message]), mtError, [mbOk], 0);
     end;

  finally
     if theBridge.ProgressCancelled then dlgRes:= mrCancel;
     theBridge.ProgressHide;
  end;
end;

procedure TDigIt_Main.SES_Load(IsAutoSave: Boolean);
var
   newSourceI,
   newDestinationI: Integer;
   aXML: TRttiXMLConfig;

begin
  try
     Session.Load(IsAutoSave);

  finally
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
  try
     Session.Save(IsAutoSave);

  finally
  end;
end;

procedure TDigIt_Main.SES_ClearAutoSave(AFromStartup: Boolean);
begin
  try
     Session.ClearAutoSave(AFromStartup);

     if not(AFromStartup) then
     try
        imgManipulation.Bitmap:= nil;
        if (Session.CropMode = diCropCustom) then imgManipulation.clearCropAreas;

     finally
        UI_FillCounter;
     end;

  finally
    UI_ToolBar;
    UI_Caption;
  end;
end;

(*
procedure TDigIt_Main.SES_LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   i,
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
*)
procedure TDigIt_Main.SES_LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
begin
  try
     //User Interface
     rollCrops.Collapsed:=aXML.GetValue('UI/rollCrops_Collapsed', False);
     rollPages.Collapsed:=aXML.GetValue('UI/rollPages_Collapsed', True);
     rollCounters.Collapsed:=aXML.GetValue('UI/rollCounters_Collapsed', True);

  finally
  end;
end;

procedure TDigIt_Main.SES_SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
begin
  try
     //User Interface
     aXML.SetValue('UI/rollCrops_Collapsed', rollCrops.Collapsed);
     aXML.SetValue('UI/rollPages_Collapsed', rollPages.Collapsed);
     aXML.SetValue('UI/rollCounters_Collapsed', rollCounters.Collapsed);

  finally
  end;
end;


function TDigIt_Main.Source_Select(newSourceIndex, newSourceSubIndex: Integer): Boolean;
var
   curSource: PSourceInfo;

begin
  Result:= False;
  try
     curSource:= Sources.Selected;
     if Sources.Select(newSourceIndex, newSourceSubIndex, True) then
     begin
       if (Sources.Selected <> curSource) then
       begin
         { #note -oMaxM : rSource Switched...Do something? }
       end;

       //Save Used Source in AutoSave
       Session.SaveSource(nil, True);

       //Save Last Used Source in Settings
       Session.SaveSource(nil, False, '', Path_Config+File_Config);

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
            if TDest_SaveFiles_Settings.Execute(Session.SaveFormat, Session.SaveWriter, Path_Session_Pictures) then
            begin
              Session.SaveExt:= SuggestImageExtension(Session.SaveFormat);
              Session.SaveDestination(nil, True);
            end;

            //this way we know it is SaveAs Destination
            //rDestination:= nil;
            //rDestinationParams:= nil;
            //rDestinationName:= '';

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
    3: begin
         Settings.Session.Startup_File:= 'testSess';
         Settings.Session.Startup_Path:= 'c:\tmp\testSess\';
         Settings.Session.ConfirmSaveOnClose:= True;
         Settings.Save(nil);
    end;
//    4: PROF_Save;
  end;
end;

procedure TDigIt_Main.menuImageFormatClick(Sender: TObject);
begin
  TBGRAFormatUIContainer.Execute(Session.SaveFormat, Session.SaveWriter);
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
  if (Session.CropMode <> diCropCustom) and not(imgManipulation.Empty) then setCropMode(diCropCustom);

  curIndex :=imgManipulation.CropAreas.IndexOf(CropArea);

  if (CropArea.Name='') then CropArea.Name:='Name '+IntToStr(curIndex);

  CropArea.Icons:=[cIcoIndex];

  cbCropList.AddItem(CropArea.Name, CropArea);
  cbCropList.ItemIndex:=cbCropList.Items.IndexOfObject(CropArea);

  if not(Session.Loading) then
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

         if not(Session.Loading) then UI_ToolBar;
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
  TDigIt_ExportFiles.Execute(Application.Title, Session.CapturedFiles, True);
end;

procedure TDigIt_Main.tbCapturedToImgClick(Sender: TObject);
begin
  TDigIt_ExportFiles.Execute(Application.Title, Session.CapturedFiles, False);
end;

procedure TDigIt_Main.MenuSourcePopup(Sender: TObject);
begin
  BuildSourcesMenu(Self, menuSources, @UI_SourceMenuClick, Sources.Selected);
end;

procedure TDigIt_Main.setCropMode(ANewCropMode: TDigItCropMode);
begin
  if (ANewCropMode <> Session.CropMode) then
  begin
    Case ANewCropMode of
      diCropFull: begin
        if (Session.CropMode = diCropCustom) then
        begin
          if (Length(Session.CropAreas) > 0)
          then if (MessageDlg('DigIt', rsClearCrops, mtConfirmation, mbYesNo, 0) = mrNo)
               then exit;
        end;

        tbCrop.Visible:= False;
        imgManipulation.clearCropAreas;
        imgManipulation.Opacity:= 0;
        imgManipulation.Enabled:= False;
        rollCrops.Enabled:= False; rollCrops.Collapsed:= True;
        rollCounters.Collapsed:= True;

        tbCropMode.Caption:= rsCropFull;
      end;
      diCropCustom: begin
        tbCrop.Visible:= True;
        imgManipulation.Opacity:= 64; //128; {#to-do Add to Settings}
        imgManipulation.Enabled:= True;
        rollCrops.Enabled:= True; rollCrops.Collapsed:= False;
        rollCounters.Collapsed:= False;
        tbCropMode.Caption:= rsCropCust;
      end;
    end;

    Session.CropMode:= ANewCropMode;

    if not(Session.Loading) then
    begin
      UI_FillCounter;
      UI_ToolBar;
    end;
   end;
  tbCropMode.ImageIndex:= Integer(Session.CropMode);
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

(*
procedure TDigIt_Main.CropFile_Full(AStartIndex: Integer; isReTake: Boolean);
var
   i,
   c,
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

     c:= Length(SourceFiles);
     cStr:= IntToStr(c);
     DigIt_Progress.progressTotal.Min:= AStartIndex;
     DigIt_Progress.progressTotal.Max:= c;

     for i:=AStartIndex to c-1 do
     begin
       DigIt_Progress.progressTotal.Position:= i;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [i, cStr]);

       Application.ProcessMessages;

       UserCancel:= DigIt_Progress.Cancelled;
       if UserCancel then break;

       UserCancel:= not(LoadImage(SourceFiles[i].fName, False));
       if UserCancel then break;

       try
          SaveCallBack(imgManipulation.Bitmap, nil, Integer( (isReTake and (i < lastLenTaked)) ));
       except
         UserCancel:= True;
       end;

       if UserCancel then break;

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

procedure TDigIt_Main.SetDefaultSessionValues;
begin
  Path_Session:= Path_DefSession;
  Path_Session_Scan:= Path_DefSession_Scan;
  Path_Session_Pictures:= Path_DefSession_Pictures;
  Session_File:= File_DefSession;
  rSessionModified:= False;
end;
*)

procedure TDigIt_Main.Clear_Captured;
var
   nofileBMP: TBitmap;

begin
  try
     //Clear Array
     Session.Clear_Captured;

     //Clear ListView
     nofileBMP:=TBitmap.Create;
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

function TDigIt_Main.GetSessionModified: Boolean;
begin
  Result:= Session.Modified;
end;

procedure TDigIt_Main.SetSessionModified(AValue: Boolean);
begin
  if (Session.Modified <> AValue) then
  begin
    Session.Modified:= AValue;

    UI_Caption;
  end;
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
    UI_Caption;
  end;
end;

procedure TDigIt_Main.UI_ProfileMenuClick(Sender: TObject);
begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  try
    if (Session.LoadSource(nil, False,
                           PROF_Item+IntToStr(TMenuItem(Sender).Tag)+'/',
                           Path_Config+File_Profiles) > -1) then
    begin
      TMenuItem(Sender).Default:= True;
    end;

  finally
    UI_ToolBar;
    UI_Caption;
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

  panelPageSize.Enabled:= (Session.PageResize <> resFullsize);

  if panelPageSize.Enabled then
  begin
    if (Session.PageSize.PhysicalUnit <> ResolutionToPhysicalUnit(imgManipulation.EmptyImage.ResolutionUnit)) then
    begin
      imgManipulation.EmptyImage.ResolutionUnit:= PhysicalToResolutionUnit(Session.PageSize.PhysicalUnit);
    end;

    if (Session.PageSize.PhysicalUnit = puPixel)
    then begin
           edPageWidth.DecimalPlaces:=0;
           edPageHeight.DecimalPlaces:=0;
         end
    else begin
           edPageWidth.DecimalPlaces:=3;
           edPageHeight.DecimalPlaces:=3;
         end;

    edPageWidth.Value:= Session.PageSize.Width;
    edPageHeight.Value:= Session.PageSize.Height;
  end;

  inFillPagesUI:= False;
end;

procedure TDigIt_Main.UI_FillPageRotateFlip;
begin
  inFillPagesUI:= True;

  Case Session.PageRotate of
    rotLeft90: btPRotateLeft.Down:= True;
    rotRight90: btPRotateRight.Down:= True;
    rot180: btPRotate180.Down:= True;
    else begin
      btPRotateLeft.Down:= False;
      btPRotateRight.Down:= False;
      btPRotate180.Down:= False;
    end;
  end;

  Case Session.PageFlip of
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
  bCommonCond:= (Sources.Selected<>nil) and (Sources.Selected^.Inst <> nil);
  actPreview.Enabled:= bCommonCond;
  itemProfiles_AddCurrent.Enabled:= bCommonCond;

  lenCaptured:= Length(Session.CapturedFiles);

  if (Session.CropMode = diCropCustom)
  then begin
         lenSources:= Length(Session.SourceFiles);
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
  if (Session.CapturedFiles[AIndex].iIndex <= 0)
  then Session.CapturedFiles[AIndex].iIndex:= imgListThumb.AddProportionally(AFileName)
  else imgListThumb.ReplaceProportionally(Session.CapturedFiles[AIndex].iIndex, AFileName);

  imgListThumb_Changed:= True;
end;

procedure TDigIt_Main.UI_ThumbnailUpdate(AIndex: Integer; ABitmap: TBGRABitmap);
begin
  if (Session.CapturedFiles[AIndex].iIndex <= 0)
  then Session.CapturedFiles[AIndex].iIndex:= imgListThumb.AddProportionally(ABitmap.Bitmap)
  else imgListThumb.ReplaceProportionally(Session.CapturedFiles[AIndex].iIndex, ABitmap.Bitmap);

  imgListThumb_Changed:= True;
end;

procedure TDigIt_Main.UI_Caption;
var
   capStr: String;

begin
  if Session.Modified then capStr:= '* ' else  capStr:= '';

  if (Path_Session = Path_DefSession)
  then capStr:= capStr+'DigIt'
  else capStr:= capStr+'DigIt'+' - '+Session.FileName;

  {#todo 2 -oMaxM : Really Use something like rSource^.Inst.UI_SourceTitle + (rSourceName) }
  //if (rSourceName <> '') then capStr:= capStr+'  ('+rSourceName+')';

  Caption:= capStr;
end;

end.

