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
  BGRAImageManipulation,  BGRASpeedButton, BCPanel, BCLabel, BCListBox, BGRAImageList, BCExpandPanels,
  DigIt_Types, DigIt_Utils, DigIt_Counters, Digit_Bridge_Intf, Digit_Bridge_Impl;

resourcestring
  rsContinueWork = 'Continue from last Work Session?';
  rsSaveWork = 'Save the Work Session?';
  rsSaveWorkCopy = 'Choose Yes to make a copy of the Work Session'#13#10'Choose No to move the Work Session';
  rsNoFilesDownloaded = 'NO Files Downloaded ';
  rsTakeAgain = 'Replace the last %d taked files with a new Take?';
  rsNoMoreFiles = 'There are no more files to process, should I clear the Work Queue?';
  rsMiddlePage = 'The page is within the already processed range'#13#10'Should I put it in the middle?';
  rsProcessingImages = 'Processing Images';
  rsProcessing = 'Processing %d / %s';
  rsProcessed = 'Processed %d / %s';
  rsClearQueue = 'Clear the Work Queue?';
  rsExcCreateCounter = 'Failed to Create the Counter';
  rsNotImpl = 'Not yet implemented';
  rsConvertPDF = 'Converting Images to PDF...';
  rsOpenSavedPDF = 'Conversion to PDF completed, do i Open it?';
  rsClearCrops = 'Clear Crop Areas ?';
  rsCropFull = 'Full Area';
  rsCropCust = 'Custom';
  rsCropToDo = '%d files to do';
  rsCounterPrev = 'Value Previous: %d';
  rsDeleteCaptured = 'Delete Captured Page %s ?';
  rsDeleteAll = 'Delete All Captured Pages?';
  rsDeleteAllFiles = 'Do I also Delete Files from the disk?';

  rsErrIncomplete = 'Operation not completed, do I keep the processed files?';

type
  TSourceFile = packed record
    fCrop: Boolean;
    fName: String;
  end;
  TSourceFileArray = array of TSourceFile;

  TCapturedFile = packed record
    fAge: LongInt;
    fName: String;
    iIndex: Integer;
  end;
  TCapturedFileArray = array of TCapturedFile;

  { TDigIt_Main }

  TDigIt_Main = class(TForm)
    actCropGoBack: TAction;
    actCropAll: TAction;
    actCrop: TAction;
    actClearQueue: TAction;
    actCropGoNext: TAction;
    actCapturedDeleteAll: TAction;
    actCapturedDelete: TAction;
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
    OpenProject: TOpenDialog;
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
    SaveProject: TSaveDialog;
    Separator1: TMenuItem;
    menuProjectOpen: TMenuItem;
    menuProjectSave: TMenuItem;
    menuProjectNew: TMenuItem;
    MenuMain: TPopupMenu;
    SavePDF: TSaveDialog;
    SelectDirectory: TSelectDirectoryDialog;
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
    procedure rgCropAspectSelectionChanged(Sender: TObject);
    procedure btCropApplyAspectRatioClick(Sender: TObject);

    procedure AddedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure DeletedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure ChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure SelectedChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);

    procedure tbCapturedPDFClick(Sender: TObject);

  private
    { private declarations }
    lastNewBoxNum: Word;
    changingAspect,
    Closing,
    XML_Loading,
    inFillCounterUI,
    inFillBoxUI,
    inFillPagesUI,
    imgListThumb_Changed,
    UserCancel: Boolean;
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
    SavePath,
    Session_File,         {#todo 10 -oMaxM : Really only the Name, the Thumb must have the same name but with .digt }
    LoadedFile: String;

    testI,
    lastCropped,
    lastLenTaked,     //Used in Take Again
    iSourceFiles,
    iCapturedFiles: Integer;
    SourceFiles: TSourceFileArray;
    CapturedFiles: TCapturedFileArray; { #todo 10 -oMaxM : Use this array and not ListItems }

    function GetCurrentCropArea: TCropArea;

    function Counters_GetCurrent: TDigIt_Counter;
    procedure Counters_Dec;
    procedure Counters_Inc;

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

    procedure ProgressCancelClick(Sender: TObject);

    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer);

    procedure UpdateBoxList;
    procedure UpdateCropAreaCountersList(ACounter :TDigIt_Counter);
    procedure CounterSelect(AIndex:Integer);
    function LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;

    procedure XML_LoadWork;
    procedure XML_SaveWork;
    procedure XML_ClearWork(AFromStartup: Boolean);
    procedure XML_LoadSessionFile(AFileName:String);
    procedure XML_SaveSessionFile(AFileName:String);

    function XML_LoadSource(aXML: TXMLConfig): Integer;
    procedure XML_SaveSource(aXML: TXMLConfig);
    procedure XML_LoadSourceFiles(aXML: TXMLConfig);
    procedure XML_SaveSourceFiles(aXML: TXMLConfig);
    function XML_LoadDestination(aXML: TXMLConfig): Integer;
    procedure XML_SaveDestination(aXML: TXMLConfig);
    procedure XML_LoadCapturedFiles(aXML: TXMLConfig);
    procedure XML_SaveCapturedFiles(aXML: TXMLConfig);
    procedure XML_LoadLoadedImage(aXML: TXMLConfig);
    procedure XML_SaveLoadedImage(aXML: TXMLConfig);
    procedure XML_SaveSource_CapturedFiles(aXML: TXMLConfig);
    procedure XML_SaveSource_CapturedIndexes(aXML: TXMLConfig);
    procedure XML_LoadCropAreas(aXML: TXMLConfig);
    procedure XML_SaveCropAreas(aXML: TXMLConfig);
    procedure XML_LoadPageSettings(aXML: TXMLConfig);
    procedure XML_SavePageSettings(aXML: TXMLConfig);
    procedure XML_LoadUserInterface(aXML: TXMLConfig);
    procedure XML_SaveUserInterface(aXML: TXMLConfig);

    function Source_Select(newSourceIndex: Integer): Boolean;
    function Destination_Select(newDestinationIndex: Integer): Boolean;

    procedure setSession_File(AValue: String);
    procedure setCropMode(ANewCropMode: TDigItCropMode);

//    function WaitForAFile(AFileName: String; ATimeOut: Integer): Boolean;

    function SourceFiles_Add(AArray: IDigIt_ROArray): Integer; overload;
    function SourceFiles_Add(AFileName: String): Integer; overload;

    function CropFile_Full(AFileName: String): Boolean; overload;
    procedure CropFile_Full(AStartIndex: Integer); overload;

    procedure ProgressImagesShow(TotalMin, TotalMax: Integer);

    procedure Pages_InsertMiddle(ASourceFileIndex: Integer);

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

  end;

var
  DigIt_Main: TDigIt_Main;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLProc, fppdf, FileUtil,
  BGRAFlashProgressBar,
  MM_StrUtils,
  DigIt_Destinations, DigIt_Destination_SaveFiles_SettingsForm, DigIt_Form_PDF,
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
  Session_File:= File_SessionDefault;

  LoadedFile:= '';

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
  SourceFiles:= nil;
  iSourceFiles:= -1;
  iCapturedFiles:= -1;
  lastCropped:= -1;
  lastLenTaked:= 0;

  BuildSourcesMenu(Self, menuSources, @SourceMenuClick);

//  rDestination:= nil;
//  rDestinationParams:= nil;
  rDestinationName:= '';
  BuildDestinationsMenu(Self, menuDestinations, @DestinationMenuClick);

  CropMode:= diCropNull; //setCropMode works only if there are changes

  {$ifopt D+}
    itemCropModeCustom.Enabled:= True;
    menuDebug.Visible:= True;
    lbPrevious.Visible:= True;
  {$endif}
end;

procedure TDigIt_Main.FormShow(Sender: TObject);
var
   i:Integer;

begin
  UI_ToolBarMods;

  DigIt_Progress.OnCancelClick:= @ProgressCancelClick;

  if FileExists(Path_Session+Session_File)
     {$ifopt D-}
      and (MessageDlg('DigIt', rsContinueWork, mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     {$endif}
  then XML_LoadWork
  else XML_ClearWork(True);
end;

procedure TDigIt_Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
   dlgRes: TModalResult;

begin
  closing :=True;

  if FileExists(Path_Session+Session_File) then
  begin
    dlgRes:= MessageDlg('DigIt', rsSaveWork, mtConfirmation, [mbYes, mbNo, mbCancel], 0);

    if (dlgRes = mrYes) then XML_SaveWork;

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
      UserCancel:= False;
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
              ProgressImagesShow(1, res);

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
                LoadImage(SourceFiles[0].fName, True);
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
    XML_SaveSource_CapturedFiles(nil);
    UI_ToolBar;
    UI_FillCounter(nil);

    DigIt_Progress.Hide;
    FreeAndNil(WizardBuildDuplex);
    UserCancel:= False;
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
    XML_SaveSource_CapturedFiles(nil);
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
         XML_SaveSource_CapturedFiles(nil);
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
         XML_SaveSource_CapturedFiles(nil);
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
    UserCancel:= False;
    Finished:= False;
    c:= Length(SourceFiles);
    cStr:= IntToStr(c);
    ProgressImagesShow(iSourceFiles, c);

    repeat
      DigIt_Progress.progressTotal.Position:= iSourceFiles;
      DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [iSourceFiles, cStr]);
      Application.ProcessMessages;
      if UserCancel then break;

      imgManipulation.getAllBitmaps(@SaveCallBack, 0, True);
      SourceFiles[iSourceFiles].fCrop:= True;
      inc(iSourceFiles);

      Finished:= CheckEndOfFiles;
      if not(Finished)
      then if not(LoadImage(SourceFiles[iSourceFiles].fName, False))
           then begin
                  { #todo -oMaxM : do something if LoadImage Fails? }
                  UserCancel:= True;
                end;

      lvCaptured.Selected:= lvCaptured.Items[iCapturedFiles];

      DigIt_Progress.progressTotal.Position:= iSourceFiles+1;
      DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [iSourceFiles-1, cStr]);
      Application.ProcessMessages;
    Until Finished or UserCancel;

  finally
    //XML_SaveWork([xmlSourceIndexes, xmlSourceFiles, xmlCapturedIndexes, xmlCapturedFiles]);
    XML_SaveSource_CapturedFiles(nil);
    UI_FillCounter(nil);
    UI_ToolBar;

    DigIt_Progress.Hide;
    UserCancel:= False;
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
    XML_SaveSource_CapturedFiles(nil);
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
  if (captItem <> nil) then HintInfo^.HintStr:=CapturedFiles[captItem.Index].fName;
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

procedure TDigIt_Main.ProgressCancelClick(Sender: TObject);
begin
  UserCancel:= True;
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

procedure TDigIt_Main.actCapturedDeleteAllExecute(Sender: TObject);
var
   captItem: TListItem;
   curFileName: String;
   i: Integer;
   nofileBMP: TBitmap;

begin
  if (MessageDlg('DigIt', rsDeleteAll, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  try
     nofileBMP:=TBitmap.Create;

     //Delete all File in FileSystem ?
     if (MessageDlg('DigIt', rsDeleteAllFiles, mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     then for i:=0 to Length(CapturedFiles)-1 do DeleteFile(CapturedFiles[i].fName);

     //Clear Array and ListView
     iCapturedFiles:= -1;
     CapturedFiles:= nil;
     lvCaptured.Clear;

     //Reset Counters
     Counters.Reset;

     //Get the Fixed Thumbnail NoFile and re-add it at position 0
     imgListThumb.GetBitmap(0, nofileBMP);
     imgListThumb.Clear;
     i:= imgListThumb.Add(noFileBMP, nil);
     imgListThumb_Changed:= True;

     XML_SaveWork;
     UI_ToolBar;
     UI_FillCounter(nil);

  finally
    noFileBMP.Free;
  end;
end;

procedure TDigIt_Main.actSessionNewExecute(Sender: TObject);
begin
  try
     if TDigIt_Templates.Execute then
     begin

     end;
   finally
      DigIt_Templates.Free; DigIt_Templates:= Nil;
   end;
end;

procedure TDigIt_Main.actSessionOpenExecute(Sender: TObject);
begin
  try
     if (Path_Session <> Path_Config) then
     case MessageDlg('DigIt', rsSaveWork, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
     mrYes : actSessionSave.Execute;
     mrCancel: exit;
     end;

     if OpenProject.Execute then XML_LoadSessionFile(OpenProject.FileName);
  finally
  end;
end;

procedure TDigIt_Main.actSessionSaveAsExecute(Sender: TObject);
begin
  if SaveProject.Execute
  then XML_SaveSessionFile(SaveProject.FileName);
end;

procedure TDigIt_Main.actSessionSaveExecute(Sender: TObject);
begin
  try
     if (Path_Session = Path_Config)
     then begin
            //Save As...
            if SaveProject.Execute
            then XML_SaveSessionFile(SaveProject.FileName);
          end
     else XML_SaveSessionFile('');

  finally
  end;
end;

procedure TDigIt_Main.actCapturedRotateLeftExecute(Sender: TObject);
var
   captItem: TListItem;
   sourceBitmap,
   rotatedBitmap: TBGRABitmap;
   curFileName: String;

begin
  captItem:= lvCaptured.Selected;

  if (captItem <> nil) then
  try
     curFileName:= CapturedFiles[captItem.Index].fName;
     sourceBitmap:= TBGRABitmap.Create(curFileName);
     rotatedBitmap:= sourceBitmap.RotateCCW(True);
     rotatedBitmap.SaveToFile(curFileName);

     UI_ThumbnailUpdate(captItem.Index, rotatedBitmap);

     //Update the Thumbnail in ListView
     lvCaptured.Selected:= nil;
     lvCaptured.Selected:= captItem;

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

begin
  captItem:= lvCaptured.Selected;

  if (captItem <> nil) then
  try
     curFileName:= CapturedFiles[captItem.Index].fName;
     sourceBitmap:= TBGRABitmap.Create(curFileName);
     rotatedBitmap:= sourceBitmap.RotateCW(True);
     rotatedBitmap.SaveToFile(curFileName);

     UI_ThumbnailUpdate(captItem.Index, rotatedBitmap);

     //Update the Thumbnail in ListView
     lvCaptured.Selected:= nil;
     lvCaptured.Selected:= captItem;

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
                savedFile:=SavePath+CropArea.Name+'.'+SaveExt;
                Bitmap.SaveToFile(savedFile, SaveWriter);
              end
         else begin
                //Increment the Counter Value
                cropCounter :=TDigIt_Counter(Counters.items[CropArea.UserData]);
                cropCounter.Value:=cropCounter.Value+1;
                inc(iCapturedFiles);

                //Save File
                savedFile:=SavePath+cropCounter.GetValue+'.'+SaveExt;
                Bitmap.SaveToFile(savedFile, SaveWriter);
              end;
       end
  else begin
         //Increment the Counter Value
         cropCounter :=TDigIt_Counter(Counters.items[0]);
         cropCounter.Value:=cropCounter.Value+1;
         inc(iCapturedFiles);

         //Save File
         savedFile:=SavePath+cropCounter.GetValue+'.'+SaveExt;
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
  UI_FillCounter(Counters_GetCurrent);
end;

function TDigIt_Main.LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
var
   Bitmap,
   BitmapR :TBGRABitmap;
   curFileName: String;

begin
  Result:= False;

  curFileName:= AImageFile;
  RelativePathToFullPath(Path_Session, curFileName);

  if (curFileName<>'') and FileExists(curFileName) then
  try
     BitmapR:= Nil;

     Bitmap:= TBGRABitmap.Create;
     Bitmap.LoadFromFile(curFileName);

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

     imgManipulation.Bitmap.InvalidateBitmap;

     LoadedFile:= AImageFile;

     if saveToXML then XML_SaveLoadedImage(nil);

     Result:= True;

  finally
     if (Bitmap <> Nil) then Bitmap.Free;
     if (BitmapR <> Nil) then BitmapR.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadWork;
var
   newSourceI,
   newDestinationI: Integer;
   XMLWork: TXMLConfig;

begin
  try
    XML_Loading:= True;
    XMLWork:= TXMLConfig.Create(Path_Session+Session_File);

    newSourceI:= XML_LoadSource(XMLWork);
    XML_LoadSourceFiles(XMLWork);
    newDestinationI:= XML_LoadDestination(XMLWork);
    XML_LoadCapturedFiles(XMLWork);
    XML_LoadPageSettings(XMLWork);
    LoadImage(XMLWork.GetValue('LoadedFile', ''), False);
    Counters.Load(XMLWork, True);
    XML_LoadCropAreas(XMLWork);
    XML_LoadUserInterface(XMLWork);

    UI_MenuItemsChecks(newSourceI, newDestinationI);
    UI_FillCounter(Counters_GetCurrent);
    UI_ToolBar;

  finally
    XML_Loading:= False;
    XMLWork.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveWork;
var
   XMLWork: TXMLConfig;

begin
  if (rSource <> Nil) and (rSource^.Inst <> Nil) then
  try
     XMLWork:= TXMLConfig.Create(Path_Session+Session_File);

     XML_SaveSource(XMLWork);
     XML_SaveSourceFiles(XMLWork);
     XML_SaveDestination(XMLWork);
     XML_SaveCapturedFiles(XMLWork);
     XML_SavePageSettings(XMLWork);
     XML_SaveLoadedImage(XMLWork);

     Counters.Save(XMLWork, True);
     if (cbCounterList.ItemIndex > -1) then XMLWork.SetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);

     XML_SaveCropAreas(XMLWork);
     XML_SaveUserInterface(XMLWork);

     XMLWork.Flush;
     XMLWork.Free;

     //FPC Bug?
     //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
     //So we do it after destroying XMLWork.

     if (rSource <> nil) then rSource^.Inst.Params.Save(PChar(Path_Session+Session_File), 'Source/Params');
//     if (rDestination <> nil) then rDestination^.Inst.Params.Save(PChar(Path_Session+Session_File), 'Destination/Params');

  finally
  end;
end;

procedure TDigIt_Main.XML_ClearWork(AFromStartup: Boolean);
var
   i: Integer;
   nofileBMP: TBitmap;

begin
  DeleteFile(Path_Config+File_SessionDefault);
  DeleteFile(Path_Config+File_CapturedThumbs);
  DeleteDirectory(Path_Temp, True);

  Path_Session:= Path_Config;
  Path_Session_Temp:= Path_Temp;
  Session_File:= File_SessionDefault;

  rDestinationName:= '';
  SaveFormat:= ifJpeg;
  SavePath:= Path_Pictures;

  try
     if (SaveWriter <> nil) then SaveWriter.Free;
     SaveWriter:= BGRABitmapTypes.CreateBGRAImageWriter(SaveFormat, True);
     SaveExt:= BGRABitmapTypes.SuggestImageExtension(SaveFormat);

  except
    SaveWriter:= nil;
    SaveExt:= 'jpg';
  end;

  if not(AFromStartup) then
  try
     //Clear Source Queque
     iSourceFiles:= -1;
     lastCropped:= -1;
     lastLenTaked:= 0;
     SourceFiles:= nil;
     //rSource^.Inst.Clear;

     nofileBMP:= TBitmap.Create;

     //Clear Captured Array and ListView
     iCapturedFiles:= -1;
     CapturedFiles:= nil;
     lvCaptured.Clear;

     //Reset Counters
     Counters.Reset;

     //Get the Fixed Thumbnail NoFile and re-add it at position 0
     imgListThumb.GetBitmap(0, nofileBMP);
     imgListThumb.Clear;
     i:= imgListThumb.Add(noFileBMP, nil);
     imgListThumb_Changed:= True;

  finally
    noFileBMP.Free;

    UI_FillCounters;
    UI_FillCounter(nil);
  end;

  setCropMode(diCropFull);

  UI_ToolBar;
end;


procedure TDigIt_Main.XML_LoadSessionFile(AFileName: String);
begin
  try
    Path_Session:= ExtractFilePath(AFileName);
    Path_Session_Temp:= Path_Session+'tmp'+DirectorySeparator;
    Session_File:= ExtractFileName(AFileName);

    XML_LoadWork;

    if (Path_Session = Path_Config)
    then Caption :='DigIt'
    else Caption :='DigIt'+' - '+Session_File;

  finally
  end;
end;

procedure TDigIt_Main.XML_SaveSessionFile(AFileName: String);
var
   newPath_Session: String;

begin
  try
     if (AFileName <> '') then
     begin
       newPath_Session:=ExtractFilePath(AFileName);
       ForceDirectories(newPath_Session);

       //Copy or Move temp/thumb Files from Current Work Session to New Session
       Case MessageDlg('DigIt', rsSaveWorkCopy, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
       mrYes: begin
                //Copy the files to new Location
                CopyDirTree(Path_Session+'tmp', newPath_Session+'tmp',
                            [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime]);

              end;
       mrNo: begin
                //Copy the files to new Location
                if CopyDirTree(Path_Session+'tmp', newPath_Session+'tmp',
                              [cffOverwriteFile, cffCreateDestDirectory, cffPreserveTime])
                then DeleteDirectory(Path_Session+'tmp', False);

                DeleteFile(Path_Session+File_CapturedThumbs);
                DeleteFile(Path_Session+Session_File);
             end;
       mrCancel: exit;
       end;
     end;

     Path_Session:= newPath_Session;
     Session_File:= ExtractFileName(AFileName); {#todo 10 -oMaxM : Really only the Name, the Thumb must have the same name but with .digt }
     imgListThumb_Changed:= True;

     XML_SaveWork;

     if (Path_Session = Path_Config)
     then Caption :='DigIt'
     else Caption :='DigIt'+' - '+Session_File;

  finally
  end;
end;

function TDigIt_Main.XML_LoadSource(aXML: TXMLConfig): Integer;
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     //Load rSource and its Params
     Result:= -1;
     if theBridge.SourcesImpl.Select(aXML.GetValue('Source/Name', '')) then
     begin
       rSource:= theBridge.SourcesImpl.Selected;
       rSourceName:= theBridge.SourcesImpl.SelectedName;
       rSourceParams:= theBridge.SourcesImpl.SelectedParams;
       Result:= theBridge.SourcesImpl.SelectedIndex;
       theBridge.SourcesImpl.LoadSelectedParams(aXML.Filename, 'Source/Params');
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSource(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     //Save rSource and its Params
     aXML.SetValue('Source/Name', rSourceName);
     aXML.DeletePath('Source/Params/');

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadSourceFiles(aXML: TXMLConfig);
var
   aFree: Boolean;
   curItemPath: String;
   i, iCount: Integer;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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
       SourceFiles[i].fName:= aXML.GetValue(curItemPath+'fName', '');
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSourceFiles(aXML: TXMLConfig);
var
   aFree: Boolean;
   i: Integer;
   curItemPath: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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
       aXML.SetValue(curItemPath+'fName', SourceFiles[i].fName);
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

function TDigIt_Main.XML_LoadDestination(aXML: TXMLConfig): Integer;
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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
            SavePath:= aXML.GetValue('Destination/Params/Path', '');

            //Load Format and Create Writer
            SaveFormat:= ifJpeg;
            aXML.GetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
            try
               if (SaveWriter <> nil) then SaveWriter.Free;
               SaveWriter:= BGRABitmapTypes.CreateBGRAImageWriter(SaveFormat, True);
               SaveExt:= BGRABitmapTypes.SuggestImageExtension(SaveFormat);
               { #todo 5 -oMaxM : Load Format Specific parameters, Find a way using RTI? }

            except
               SaveWriter:= nil;
               SaveExt:= 'jpg';
            end;

            Result:= -1;
 (*
          end
     else begin
            if theBridge.DestinationsImpl.Select(newDestinationName) then
            begin
              rDestination:= theBridge.DestinationsImpl.Selected;
              rDestinationName:= theBridge.DestinationsImpl.SelectedName;
              rDestinationParams:= theBridge.DestinationsImpl.SelectedParams;
              Result:= theBridge.DestinationsImpl.SelectedIndex;
              theBridge.DestinationsImpl.LoadSelectedParams(aXML.Filename, 'Destination/Params');
            end;
          end;
 *)

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveDestination(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     //Save rDestination and its Params
(*     aXML.SetValue('Destination/Name', rDestinationName);
     if (rDestination = nil) then
     begin
*)
       aXML.DeletePath('Destination/Params/');
       aXML.SetValue('Destination/Params/Path', SavePath);

       aXML.SetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
       { #todo 5 -oMaxM : Save Format Specific parameters, Find a way using RTI? }

//     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadCapturedFiles(aXML: TXMLConfig);
var
   i,
   imgCount,
   newCount,
   newSelected: Integer;
   curAge: Longint;
   cuFileName,
   curItemPath: String;
   curItem: TListItem;
   imgListCountChanged: Boolean;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     newCount := aXML.GetValue(XML_CapturedFiles+'Count', 0);
     iCapturedFiles:= aXML.GetValue(XML_CapturedFiles+'iCapturedFiles', -1);
     newSelected :=aXML.GetValue(XML_CapturedFiles+'Selected', -1);

     lvCaptured.BeginUpdate;
     lvCaptured.Clear;

     if FileExists(Path_Session+File_CapturedThumbs)
     then try
             imgListThumb.Clear;
             imgListThumb.LoadFromFile(Path_Session+File_CapturedThumbs);
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
         CapturedFiles[i].fName:= aXML.GetValue(curItemPath+'fName', '');
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
              end
         else CapturedFiles[i].iIndex :=0;

         curItem.ImageIndex:= CapturedFiles[i].iIndex;
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

procedure TDigIt_Main.XML_SaveCapturedFiles(aXML: TXMLConfig);
var
   i,
   lenCapturedFiles: Integer;
   curItemPath: String;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     if imgListThumb_Changed then
     try
       imgListThumb.SaveToFile(Path_Session+File_CapturedThumbs);
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
         aXML.SetValue(curItemPath+'fName', CapturedFiles[i].fName);
         aXML.SetValue(curItemPath+'iIndex', CapturedFiles[i].iIndex);
       end;
     end;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadLoadedImage(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     LoadImage(aXML.GetValue('LoadedFile', ''), False); //DON'T set to True, if you do not want infinite recursion

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveLoadedImage(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     aXML.SetValue('LoadedFile', LoadedFile);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSource_CapturedFiles(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     XML_SaveSourceFiles(aXML);
     XML_SaveCapturedFiles(aXML);
     aXML.SetValue('LoadedFile', LoadedFile);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveSource_CapturedIndexes(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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

     aXML.SetValue('LoadedFile', LoadedFile);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadCropAreas(aXML: TXMLConfig);
var
   aFree: Boolean;
   newCropMode: TDigItCropMode;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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

procedure TDigIt_Main.XML_SaveCropAreas(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     aXML.SetValue('CropMode', Integer(CropMode));
     if (CropMode = diCropCustom)
     then imgManipulation.CropAreas.Save(aXML, 'CropAreas')
     else aXML.DeletePath('CropAreas');

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadPageSettings(aXML: TXMLConfig);
var
   selButton: Integer;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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

procedure TDigIt_Main.XML_SavePageSettings(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

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

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadUserInterface(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     //User Interface
     rollCrops.Collapsed:=aXML.GetValue('UI/rollCrops_Collapsed', False);
     rollPages.Collapsed:=aXML.GetValue('UI/rollPages_Collapsed', True);
     rollCounters.Collapsed:=aXML.GetValue('UI/rollCounters_Collapsed', True);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Main.XML_SaveUserInterface(aXML: TXMLConfig);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TXMLConfig.Create(Path_Session+Session_File);

     //User Interface
     aXML.SetValue('UI/rollCrops_Collapsed', rollCrops.Collapsed);
     aXML.SetValue('UI/rollPages_Collapsed', rollPages.Collapsed);
     aXML.SetValue('UI/rollCounters_Collapsed', rollCounters.Collapsed);

  finally
    if aFree then aXML.Free;
  end;
end;

function TDigIt_Main.Source_Select(newSourceIndex: Integer): Boolean;
begin
  Result:= False;
  try
     if theBridge.SourcesImpl.Select(newSourceIndex, True) then
     begin
       if (theBridge.SourcesImpl.Selected <> rSource) then
       begin
         { #note -oMaxM : rSource Switched...Do something? }
       end;

       rSource:= theBridge.SourcesImpl.Selected;
       rSourceName:= theBridge.SourcesImpl.SelectedName;
       rSourceParams:= theBridge.SourcesImpl.SelectedParams;
       Result:= True;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Main.Destination_Select(newDestinationIndex: Integer): Boolean;
begin
  Result:= False;
  try
    (*
     if (newDestinationIndex = -1)
     then begin
    *)
            { #note 10 -oMaxM : Use of this function should be removed when SaveFiles is implemented as a descendant of IDigIt_Destination }
            if TDest_SaveFiles_Settings.Execute(SaveFormat, SavePath) then
            begin
              try
                 if (SaveWriter <> nil) then SaveWriter.Free;
                 SaveWriter:= BGRABitmapTypes.CreateBGRAImageWriter(SaveFormat, True);
                 SaveExt:= BGRABitmapTypes.SuggestImageExtension(SaveFormat);

              except
                SaveWriter:= nil;
                SaveExt:= 'jpg';
              end;
              { #todo 5 -oMaxM : Load Specific Format Settings from Form? }

            end;
            //rDestination:= nil;
            //rDestinationParams:= nil;
            rDestinationName:= '';
     (*     end
     else begin
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
          end;
     *)

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
    0: XML_LoadWork;
    1: XML_SaveWork;
    2: XML_ClearWork(False);
  end;
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
Var
  PDF: TPDFDocument;
  PDF_File: TFileStream;
  P: TPDFPage;
  S: TPDFSection;
  paper: TPDFPaper;
  IDX,
  i, W, H: Integer;
  cStr,
  curFileName,
  pdfFileName: String;
  saved: Boolean;

begin
  if SavePDF.Execute then
  try
     pdfFileName:= SavePDF.FileName;

     PDF:= TPDFDocument.Create(Nil);

     PDF.Infos.Title := Application.Title;
     PDF.Infos.Author := 'MaxM';
     PDF.Infos.Producer := Application.Title;
     PDF.Infos.ApplicationName := Application.Title+' ver '+DigIt_Version;
     PDF.Infos.CreationDate := Now;
     PDF.Options := [poCompressImages, poUseRawJPEG];

     if TDigIt_PDF.Execute(PDF.Infos) then
     try
        UserCancel:= False;
        saved:= False;

        with DigIt_Progress do
        begin
          labTotal.Caption:= '';
          capTotal.Caption:= '';
          progressTotal.Style:= pbstNormal;
          progressTotal.Min:= 0;
          progressTotal.Max:= lvCaptured.Items.Count-1;
          progressTotal.Position:= 0;
          panelCurrent.Visible:= False;
          Show(PChar(rsConvertPDF));
          cStr:= IntToStr(progressTotal.Max);
        end;

        PDF.StartDocument;
        S := PDF.Sections.AddSection;

        for i := 0 to Length(CapturedFiles)-1 do
        begin
          DigIt_Progress.progressTotal.Position:= i;
          DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [i, cStr]);
          Application.ProcessMessages;
          if UserCancel then break;

          curFileName:= CapturedFiles[i].fName;

          if FileExists(curFileName) then
          begin
            P := PDF.Pages.AddPage;
            P.PaperType := ptCustom; //ptCustom; //ptA4;
            P.UnitOfMeasure := uomPixels;//uomMillimeters;

            IDX := PDF.Images.AddFromFile(curFileName, False);
            if (IDX >= 0) then
            begin
              W := PDF.Images[IDX].Width;
              H := PDF.Images[IDX].Height;

              //Set Paper to Full image size
              paper.W:=W;
              paper.H:=H;
              P.Paper:=paper;

              P.AddObject(TPDFImage.Create(PDF, 0, 0, W, H, IDX));

              S.AddPage(P);
            end;
          end;

          DigIt_Progress.progressTotal.Position:= i+1;
          DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [i, cStr]);
          Application.ProcessMessages;
          if UserCancel then break;
        end;

        if not(UserCancel) then
        begin
          PDF_File:= TFileStream.Create(pdfFileName, fmCreate);
          PDF.SaveToStream(PDF_File);
          saved:= True;
        end;

     finally
       DigIt_Progress.Hide;
       UserCancel:= False;

       if (PDF_File <> nil) then PDF_File.Free;

       if saved and FileExists(pdfFileName)
       then if (MessageDlg('DigIt', rsOpenSavedPDF, mtConfirmation, mbYesNo, 0) = mrYes)
            then OpenDocument(pdfFileName);
     end;

  finally
    PDF.Free;
  end;
end;

procedure TDigIt_Main.setSession_File(AValue: String);
begin
  Path_Session:= ExtractFilePath(AValue);
  Path_Session_Temp:= Path_Session+'tmp'+DirectorySeparator;
  Session_File:= ExtractFileName(AValue);

  if (Path_Session = Path_Config)
  then Caption :='DigIt'
  else Caption :='DigIt'+' - '+Session_File;
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

        panelCounterList.Enabled:= False;

        if (Counters.Count = 0)
        then Counters.Add('Counter 0')
        else Counters.RemoveAllButFirst;

       (* if not(XML_Loading) then
        begin
          UI_FillCounters;
          UI_FillCounter(Counters[0]);
        end; *)

        tbCropMode.Caption:= rsCropFull;
      end;
      diCropCustom: begin
        tbCrop.Visible:= True;
        imgManipulation.Opacity:= 128;
        imgManipulation.Enabled:= True;
        rollCrops.Enabled:= True; rollCrops.Collapsed:= False;

        panelCounterList.Enabled:= True;

       (* if not(XML_Loading) then
        begin
          UI_FillCounters;
          UI_FillCounter(Counters_GetCurrent);
        end; *)

        tbCropMode.Caption:= rsCropCust;
      end;
    end;

    CropMode:= ANewCropMode;

    if not(XML_Loading) then
    begin
      UI_FillCounters;
      UI_FillCounter(Counters_GetCurrent);
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

       UserCancel:= UserCancel or not(CropFile_Full(SourceFiles[i].fName));
       if UserCancel then break;

       //SourceFiles[i].fCrop:= True;
       //if (i > lastCropped) then lastCropped:= i;
       //iSourceFiles:= i;

       DigIt_Progress.progressTotal.Position:= i+1;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [i, cStr]);

       Application.ProcessMessages; if UserCancel then break;
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

    UserCancel:= False;
  end;
end;

procedure TDigIt_Main.ProgressImagesShow(TotalMin, TotalMax: Integer);
begin
  UserCancel:= False;
  with DigIt_Progress do
  begin
    labTotal.Caption:= '';
    capTotal.Caption:= '';
    progressTotal.Style:= pbstNormal;
    progressTotal.Min:= TotalMin;
    progressTotal.Max:= TotalMax;
    progressTotal.Position:= TotalMin;
    panelCurrent.Visible:= False;
    Show(PChar(rsProcessingImages));
  end;
end;

procedure TDigIt_Main.Pages_InsertMiddle(ASourceFileIndex: Integer);
begin
  { #todo 5 -oMaxM : Re index the crops starting from the lastCropped }
  MessageDlg(rsNotImpl, mtInformation, [mbOk], 0);
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
                       DirectoryExists(SavePath);

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
         actTake.Enabled:= bCommonCond and DirectoryExists(SavePath);

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

