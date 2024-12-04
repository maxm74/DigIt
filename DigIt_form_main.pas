(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Main Form                                                                **
*******************************************************************************)

unit DigIt_Form_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtDlgs, ExtCtrls, Menus, ComCtrls,
  ActnList, Spin, ShellCtrls, EditBtn,
  Digit_Bridge_Intf, Digit_Bridge_Impl,
  FPImage, BGRAImageManipulation, BGRABitmap, BGRABitmapTypes, BGRASpeedButton, BCPanel, BCLabel, BCListBox,
  BGRAImageList, BCExpandPanels, Laz2_XMLCfg, SpinEx, BGRAPapers, DigIt_Types, DigIt_Utils,  DigIt_Counters, LCLType;

type
  TSourceFile = packed record
    fCrop: Boolean;
    fName: String;
  end;
  TSourcesArray = array of TSourceFile;

  { TDigIt_Main }

  TDigIt_Main = class(TForm)
    actCropGoBack: TAction;
    actCropAll: TAction;
    actCrop: TAction;
    actClearQueue: TAction;
    actCropGoNext: TAction;
    actTakeBuildDuplex: TAction;
    actTakeAgain: TAction;
    actTimerTake: TAction;
    actProjectSaveAs: TAction;
    actPreview: TAction;
    actRotateRight: TAction;
    actRotateLeft: TAction;
    ActionListC: TActionList;
    actOptions: TAction;
    actProjectOpen: TAction;
    actProjectSave: TAction;
    actProjectNew: TAction;
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
    itemTake: TMenuItem;
    itemTakeAgain: TMenuItem;
    itemBuildDuplex: TMenuItem;
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
    imgListCaptured: TImageList;
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
    OpenCropList: TOpenDialog;
    OpenPicture: TOpenPictureDialog;
    MenuMain: TPopupMenu;
    SavePDF: TSaveDialog;
    SavePicture: TSavePictureDialog;
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

    procedure actOptionsExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actRotateLeftExecute(Sender: TObject);
    procedure actRotateRightExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actTakeAgainExecute(Sender: TObject);
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
    procedure lvCapturedCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure lvCapturedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lvCapturedDblClick(Sender: TObject);
    procedure lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure edCounterNameEditingDone(Sender: TObject);
    procedure edCounterValueChange(Sender: TObject);
    procedure edCounterValueStringDigitsChange(Sender: TObject);
    procedure edCounterValueStringPreEditingDone(Sender: TObject);
    procedure edCounterValueStringPostEditingDone(Sender: TObject);
    procedure edCropNameEditingDone(Sender: TObject);
    procedure edCropUnit_TypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure btBox_AddClick(Sender: TObject);
    procedure btBox_DelClick(Sender: TObject);
    procedure cbBoxListChange(Sender: TObject);
    procedure edCropHeightChange(Sender: TObject);
    procedure edCropLeftChange(Sender: TObject);
    procedure edCropTopChange(Sender: TObject);
    procedure edCropWidthChange(Sender: TObject);
    procedure rgCropAspectSelectionChanged(Sender: TObject);
    procedure btCropApplyAspectRatioClick(Sender: TObject);

    procedure AddedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure DeletedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure ChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure SelectedChangedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
    procedure tbWorkSaveClick(Sender: TObject);

    procedure TestClick(Sender: TObject);
    procedure TestRClick(Sender: TObject);
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
    UserCancel: Boolean;
    Counters: TDigIt_CounterList;
    XMLWork,
    XMLProject: TXMLConfig;

    rSource: PSourceInfo;
    rSourceName: String;
    rSourceParams: IDigIt_Params;

    rDestination: PDestinationInfo;
    rDestinationName: String;
    rDestinationParams: IDigIt_Params;

    CropMode: TDigItCropMode;
    SaveExt,
    SavePath,
    rProject_File,
    LoadedFile: String;
    testI,
    lastCropped,
    lastLenTaked,     //Used in Take Again
    iSourceFiles: Integer;
    SourceFiles: TSourcesArray;
    DestinationFiles: TStringArray; { #todo 10 -oMaxM : Use this array and not ListItems }

    function GetCurrentCropArea: TCropArea;
    function Counters_GetCurrent: TDigIt_Counter;
    procedure Counters_Dec;
    procedure Counters_Inc;
    procedure UI_FillBox(ABox :TCropArea);
    procedure UI_FillCounter(ACounter :TDigIt_Counter);
    procedure UI_FillCounters;
    procedure UI_FillPageSizes;
    procedure UI_ToolBar;
    procedure UI_ToolBarAddShortcuts;
    procedure UI_MenuItemsChecks(newSourceI, newDestinationI: Integer);
    procedure DestinationMenuClick(Sender: TObject);
    procedure SourceMenuClick(Sender: TObject);
    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer);
    procedure UpdateBoxList;
    procedure UpdateCropAreaCountersList(ACounter :TDigIt_Counter);
    procedure UpdateCounterExampleLabel(ACounter :TDigIt_Counter);
    procedure CounterSelect(AIndex:Integer);
    function LoadImage(AImageFile: String): Boolean;
    procedure XML_LoadWork;
    procedure XML_SaveWork;
    procedure XML_LoadCapturedFiles;
    procedure XML_SaveCapturedFile(AItem:TFileListItem);
    procedure XML_LoadProject(AFileName:String);
    procedure XML_SaveProject(AFileName:String);
    procedure XML_LoadPageSettings;
    procedure XML_SavePageSettings;
    procedure Default_Work;
    function Source_Select(newSourceIndex: Integer): Boolean;
    function Destination_Select(newDestinationIndex: Integer): Boolean;

    procedure setProject_File(AValue: String);
    procedure setCropMode(ANewCropMode: TDigItCropMode);

//    function WaitForAFile(AFileName: String; ATimeOut: Integer): Boolean;

    function SourceFiles_Add(AArray: IDigIt_ROArray): Integer; overload;
    function SourceFiles_Add(AFileName: String): Integer; overload;

    function CropFile_Full(AFileName: String): Boolean; overload;
    procedure CropFile_Full(AStartIndex: Integer); overload;

    procedure ProgressImagesShow(TotalMin, TotalMax: Integer);

    procedure Pages_InsertMiddle(ASourceFileIndex: Integer);

    property Project_File:String read rProject_File write setProject_File;
  public
    procedure ItemSizesClick(Sender: TObject);
    procedure PageSizesClick(Sender: TObject);

    property Source: PSourceInfo read rSource;
    property SourceName: String read rSourceName;
    property SourceParams: IDigIt_Params read rSourceParams;

    property Destination: PDestinationInfo read rDestination;
    property DestinationName: String read rDestinationName;
    property DestinationParams: IDigIt_Params read rDestinationParams;

  end;

var
  DigIt_Main: TDigIt_Main;

implementation

{$R *.lfm}

uses
  {$ifopt D+}
  lazlogger,
  {$endif}
  LCLIntf, LCLProc, fppdf,
  Digit_Destinations, DigIt_Form_Progress, DigIt_Form_Templates, DigIt_Form_BuildDuplex;


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
  if (curCounter<>nil)
  then curCounter.Value :=edCounterValue.Value;
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
    UpdateCounterExampleLabel(curCounter);
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
    UpdateCounterExampleLabel(curCounter);
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
    UpdateCounterExampleLabel(curCounter);
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
  XMLWork:=nil;
  XMLProject:=nil;
  Project_File:='';
  LoadedFile:='';

  Counters :=TDigIt_CounterList.Create('Counters');

  Closing :=False;
  XML_Loading:= False;
  changingAspect :=False;
  inFillCounterUI :=False;
  inFillBoxUI :=False;
  inFillPagesUI :=False;

  lastNewBoxNum :=0;
  TStringList(cbBoxList.Items).OwnsObjects:=False;
  TStringList(cbCounterList.Items).OwnsObjects:=False;

  rSource:= nil;
  rSourceName:= '';;
  rSourceParams:= nil;
  SourceFiles:= nil;
  iSourceFiles:= -1;
  lastCropped:= -1;
  lastLenTaked:= 0;
  BuildSourcesMenu(Self, menuSources, @SourceMenuClick);

  rDestination:= nil;
  rDestinationName:= '';
  rDestinationParams:= nil;
  BuildDestinationsMenu(Self, menuDestinations, @DestinationMenuClick);

  UI_ToolBarAddShortcuts;

  CropMode:= diCropNull; //setCropMode works only if there are changes

  {$ifopt D+}
  {$endif}
end;

procedure TDigIt_Main.FormDestroy(Sender: TObject);
begin
  XML_SaveWork;
  if (XMLProject<>nil) then XMLProject.Free;
  Counters.Free;
  theBridge.Free;
  SourceFiles:= nil;
end;

procedure TDigIt_Main.FormShow(Sender: TObject);
var
   i:Integer;

begin
  for i:=0 to tbCaptured.ButtonCount-1 do
     tbCaptured.Buttons[i].Hint:=tbCaptured.Buttons[i].Caption;

  if FileExists(Path_Config+Config_XMLWork)
     {$ifopt D-}
      and (MessageDlg('DigIt', 'Continue from last Session?', mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     {$endif}
  then begin
         XML_LoadWork;
         {$ifopt D-}
         actPreview.Execute;
         {$endif}
       end
  else begin
         Default_Work;
         UI_MenuItemsChecks(-1, -1);
       end;

  UI_ToolBar;
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
            LoadImage(curImageFile);
            StrDispose(curImageFile);
            XML_SaveWork;
          end;
        end;
      end
      else MessageDlg('NO Files Downloaded ', mtError, [mbOk], 0);

      UI_ToolBar;

  finally
  end;
end;

procedure TDigIt_Main.actTakeAgainExecute(Sender: TObject);
var
   lenSources: Integer;

begin
  if (SourceFiles <> nil) and
     (MessageDlg('DigIt', 'Replace the last '+IntToStr(lastLenTaked)+' taked files with a new Take?',
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
                     //Add files to the array instead of processing them directly,
                     //so if the user closes the application before the end when he reopens it we restart
                     SourceFiles_Add(curArray);
                     { #todo 5 -oMaxM : Check User Cancel the Operation }
                     CropFile_Full(0);
                   end;
              SourceFiles:= nil; iSourceFiles:= -1;
              rSource^.Inst.Clear;

              UI_ToolBar;
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
                LoadImage(SourceFiles[0].fName);
              end;

              UI_ToolBar;

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

          Application.ProcessMessages;
          XML_SaveWork;
        end;

        UI_FillCounter(nil);
      end
      else MessageDlg('NO Files Downloaded ', mtError, [mbOk], 0);

  finally
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
      if (MessageDlg('DigIt', 'There are no more files to process, should I clear the queue?', mtConfirmation,
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
//    if CheckEndOfFiles then exit;

//    if LoadImage(SourceFiles[iSourceFiles]) then
    begin
      if (iSourceFiles = lenSources)
      then begin
             ////We've already cut the last one, Re Crop the Last File, dec the Counters first
             Counters_Dec;
             imgManipulation.getAllBitmaps(@SaveCallBack, 1, True);
           end
      else begin
             if SourceFiles[iSourceFiles].fCrop
             then imgManipulation.getAllBitmaps(@SaveCallBack, 1, True) //Re Crop, Counters is already decremented by GoBack
             else begin  //(lastCropped < iSourceFiles)
                    if (lastCropped > iSourceFiles) then
                    begin
                      if (MessageDlg('DigIt', 'The page is within the already processed range'+#13#10+
                                    'Should I put it in the middle?', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel)
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
           then if not(LoadImage(SourceFiles[iSourceFiles].fName))
                then begin
                       { #todo -oMaxM : do something if LoadImage Fails? }
                     end;

      UI_FillCounter(nil);
      UI_ToolBar;
      XML_SaveWork;
    end;

  finally
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
       if LoadImage(SourceFiles[new_iSourceFiles].fName) then
       begin
         if SourceFiles[new_iSourceFiles].fCrop //(iSourceFiles <= lastCropped)
         then Counters_Inc;
         iSourceFiles:= new_iSourceFiles;

         UI_FillCounter(nil);
         UI_ToolBar;
         XML_SaveWork;
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
       if (new_iSourceFiles >= Length(SourceFiles)) then new_iSourceFiles:= Length(SourceFiles)-1;

       dec(new_iSourceFiles);

       if LoadImage(SourceFiles[new_iSourceFiles].fName) then
       begin
         if SourceFiles[new_iSourceFiles].fCrop //(new_iSourceFiles <= lastCropped)
         then Counters_Dec;
         iSourceFiles:= new_iSourceFiles;

         UI_FillCounter(nil);
         UI_ToolBar;
         XML_SaveWork;
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
       if (MessageDlg('DigIt', 'There are no more files to process, should I clear the queue?', mtConfirmation,
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
    UserCancel:= False;
    Finished:= False;
    c:= Length(SourceFiles);
    cStr:= IntToStr(c);
    ProgressImagesShow(iSourceFiles, c);

    repeat
      DigIt_Progress.progressTotal.Position:= iSourceFiles;
      DigIt_Progress.capTotal.Caption:= 'Processing '+IntToStr(iSourceFiles)+' / '+cStr;
      Application.ProcessMessages;

      //lvCaptured.BeginUpdate; { #todo 2 -oMaxM : Refresh only new captured Images not all the list }
      imgManipulation.getAllBitmaps(@SaveCallBack, 0, True);
      //lvCaptured.Refresh;
      //lvCaptured.EndUpdate;
      SourceFiles[iSourceFiles].fCrop:= True;
      inc(iSourceFiles);

      Finished:= CheckEndOfFiles;
      if not(Finished)
      then if not(LoadImage(SourceFiles[iSourceFiles].fName))
           then begin
                  { #todo -oMaxM : do something if LoadImage Fails? }
                  UserCancel:= True;
                end;

      UI_FillCounter(nil);
      UI_ToolBar;
      XML_SaveWork;

      DigIt_Progress.progressTotal.Position:= iSourceFiles+1;
      DigIt_Progress.capTotal.Caption:= 'Processed '+IntToStr(iSourceFiles-1)+' / '+cStr;
      Application.ProcessMessages;
    Until Finished or UserCancel;

  finally
    DigIt_Progress.Hide;
    UserCancel:= False;
  end;
end;

procedure TDigIt_Main.actClearQueueExecute(Sender: TObject);
begin
  if (MessageDlg('DigIt', 'Should I clear the queue?', mtConfirmation,
                 [mbYes, mbNo], 0) = mrYes)
  then begin
         iSourceFiles:= -1;
         lastCropped:= -1;
         lastLenTaked:= 0;
         SourceFiles:= nil;
         rSource^.Inst.Clear;

         UI_ToolBar;
         XML_SaveWork;
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
         raise Exception.Create('Failed to Create the Counter');
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

procedure TDigIt_Main.lvCapturedCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass :=TFileListItem;
end;

procedure TDigIt_Main.lvCapturedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
   itemRect, destRect:TRect;
   Bitmap, BitmapR :TBGRABitmap;
   newWidth, newHeight:Integer;

begin
  try
    //Stage=prePaint
    itemRect :=Item.DisplayRect(drBounds);
    if FileExists(TFileListItem(Item).FileName)
    then begin
           Bitmap := TBGRABitmap.Create;
           Bitmap.LoadFromFile(TFileListItem(Item).FileName);
           GetThumnailSize(itemRect.Width, itemRect.Height, Bitmap.Width, Bitmap.Height, newWidth, newHeight);
           destRect.Left:=itemRect.Left+((itemRect.Width-newWidth) div 2);
           destRect.Top:=itemRect.Top+((itemRect.Height-newHeight) div 2);
           destRect.Width:=newWidth;
           destRect.Height:=newHeight;
           BitmapR :=Bitmap.Resample(newWidth, newHeight);
           BitmapR.Draw(lvCaptured.Canvas, destRect, True);
           Bitmap.Free;
           BitmapR.Free;
         end
    else Item.ImageIndex:=0;
  except
    Item.ImageIndex:=0;
  end;
  DefaultDraw:=True;
end;

procedure TDigIt_Main.lvCapturedDblClick(Sender: TObject);
var
   captItem: TFileListItem;

begin
  captItem:= TFileListItem(lvCaptured.Selected);
  if (captItem <> nil)
  then OpenDocument(captItem.FileName);
end;

procedure TDigIt_Main.lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
var
   captItem: TFileListItem;

begin
  captItem:= TFileListItem(lvCaptured.GetItemAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y));
  if (captItem <> nil)
  then HintInfo^.HintStr:=captItem.FileName;
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

procedure TDigIt_Main.actProjectNewExecute(Sender: TObject);
begin
  try
     if TDigIt_Templates.Execute then
     begin

     end;
   finally
      DigIt_Templates.Free; DigIt_Templates:= Nil;
   end;
end;

procedure TDigIt_Main.actProjectOpenExecute(Sender: TObject);
begin
  try
     if (Project_File<>'') then
     case MessageDlg('DigIt Project', 'Save Current Project?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
     mrYes : actProjectSave.Execute;
     mrCancel: exit;
     end;

     if OpenProject.Execute then XML_LoadProject(OpenProject.FileName);
  finally
  end;
end;

procedure TDigIt_Main.actProjectSaveAsExecute(Sender: TObject);
begin
  if SaveProject.Execute then
  try
    XML_SaveProject(SaveProject.FileName);
    Project_File:=SaveProject.FileName;
  finally
  end;
end;

procedure TDigIt_Main.actProjectSaveExecute(Sender: TObject);
begin
  try
     if (Project_File='')
     then actProjectSaveAs.Execute
     else XML_SaveProject(Project_File);
  finally
  end;
end;

procedure TDigIt_Main.actRotateLeftExecute(Sender: TObject);
var
   captItem: TFileListItem;
   sourceBitmap,
   rotatedBitmap: TBGRABitmap;

begin
  captItem:= TFileListItem(lvCaptured.Selected);

  if (captItem <> nil) then
  try
     sourceBitmap:= TBGRABitmap.Create(captItem.FileName);
     rotatedBitmap:= sourceBitmap.RotateCCW(True);
     rotatedBitmap.SaveToFile(captItem.FileName);

     lvCaptured.Selected:= nil;
     lvCaptured.Selected:= captItem;
  finally
     if (sourceBitmap <> nil) then sourceBitmap.Free;
     if (rotatedBitmap <> nil) then rotatedBitmap.Free;
  end;
end;

procedure TDigIt_Main.actRotateRightExecute(Sender: TObject);
var
   captItem: TFileListItem;
   sourceBitmap,
   rotatedBitmap: TBGRABitmap;

begin
  captItem:= TFileListItem(lvCaptured.Selected);

  if (captItem <> nil) then
  try
     sourceBitmap:= TBGRABitmap.Create(captItem.FileName);
     rotatedBitmap:= sourceBitmap.RotateCW(True);
     rotatedBitmap.SaveToFile(captItem.FileName);

     lvCaptured.Selected:= nil;
     lvCaptured.Selected:= captItem;

  finally
     if (sourceBitmap <> nil) then sourceBitmap.Free;
     if (rotatedBitmap <> nil) then rotatedBitmap.Free;
  end;
end;

procedure TDigIt_Main.SaveCallBack(Bitmap: TBGRABitmap; CropArea: TCropArea; AUserData: Integer);
var
  cropCounter:TDigIt_Counter;
  savedFile:String;
  captItem:TFileListItem;

begin
  if (CropArea <> nil)
  then begin
         if (CropArea.UserData<0)
         then begin
                //No Counter, Save File with CropArea Name
                savedFile:=SavePath+CropArea.Name+'.'+SaveExt;
                Bitmap.SaveToFile(savedFile);
              end
         else begin
                //Increment the Counter Value
                cropCounter :=TDigIt_Counter(Counters.items[CropArea.UserData]);
                cropCounter.Value:=cropCounter.Value+1;

                //Save File
                savedFile:=SavePath+cropCounter.GetValue+'.'+SaveExt;
                Bitmap.SaveToFile(savedFile);
              end;
       end
  else begin
         //Increment the Counter Value
         cropCounter :=TDigIt_Counter(Counters.items[0]);
         cropCounter.Value:=cropCounter.Value+1;

         //Save File
         savedFile:=SavePath+cropCounter.GetValue+'.'+SaveExt;
         Bitmap.SaveToFile(savedFile);
       end;

  if (AUserData=0)
  then begin
         //Take, add file to Captured List
         captItem:= TFileListItem(lvCaptured.Items.Add);
         captItem.FileName:= savedFile;
         XML_SaveCapturedFile(captItem);
       end
  else begin
         //ReTake, search file in Captured List and update the image
         captItem:= FindFileListItem(lvCaptured.Items, savedFile);
         if (captItem = nil)
         then begin
                //if not found (?) add file to Captured List
                captItem:= TFileListItem(lvCaptured.Items.Add);
                captItem.FileName:= savedFile;
                XML_SaveCapturedFile(captItem);
              end
         else begin
                captItem.FileName:= savedFile;

                lvCaptured.Selected:= nil;
                lvCaptured.Selected:= captItem;
              end;
       end;

  //Go to last Item in Captured List
  captItem.MakeVisible(False);
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

procedure TDigIt_Main.UpdateCounterExampleLabel(ACounter: TDigIt_Counter);
begin
  if (ACounter.Value=-1)
  then lbCounterExample.Caption:=ACounter.Value_StringPre+
                            Format('%.'+IntToStr(ACounter.Value_StringDigits)+'d', [1])+
                            ACounter.Value_StringPost
  else lbCounterExample.Caption:=ACounter.GetValue;
end;

procedure TDigIt_Main.CounterSelect(AIndex: Integer);
begin
  cbCounterList.ItemIndex:=AIndex;
  UI_FillCounter(Counters_GetCurrent);
end;

function TDigIt_Main.LoadImage(AImageFile: String): Boolean;
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

     LoadedFile:= AImageFile;
     Result:= True;

  finally
     if (Bitmap <> Nil) then Bitmap.Free;
     if (BitmapR <> Nil) then BitmapR.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadWork;
var
   newDestinationName,
   curItemPath: String;
   i, iCount,
   newDestinationI: Integer;
   newCropMode: TDigItCropMode;

begin
  try
    XML_Loading:= True;

    newDestinationI:= -1;

    XMLWork:=TXMLConfig.Create(Path_Config+Config_XMLWork);

    //Load rSource and its Params
    if theBridge.SourcesImpl.Select(XMLWork.GetValue('Source/Name', '')) then
    begin
      rSource:= theBridge.SourcesImpl.Selected;
      rSourceName:= theBridge.SourcesImpl.SelectedName;
      rSourceParams:= theBridge.SourcesImpl.SelectedParams;
      theBridge.SourcesImpl.LoadSelectedParams(Path_Config+Config_XMLWork, 'Source/Params');
    end;

    //Load SourceFiles
    LoadImage(XMLWork.GetValue('SourceFiles/LoadedFile', ''));
    SourceFiles:= nil; //Avoid possible data overlaps by eliminating any existing array
    iCount:= XMLWork.GetValue('SourceFiles/Count', 0);
    iSourceFiles:= XMLWork.GetValue('SourceFiles/iSourceFiles', 0);
    lastCropped:= XMLWork.GetValue('SourceFiles/lastCropped', -1);
    lastLenTaked:= XMLWork.GetValue('SourceFiles/lastLenTaked', 0);
    SetLength(SourceFiles, iCount);
    for i:=0 to iCount-1 do
    begin
      curItemPath :='SourceFiles/Item' + IntToStr(i)+'/';
      SourceFiles[i].fCrop:= XMLWork.GetValue(curItemPath+'fCrop', False);
      SourceFiles[i].fName:= XMLWork.GetValue(curItemPath+'fName', '');
    end;

    //Load rDestination and its Params
    newDestinationName:= XMLWork.GetValue('Destination/Name', '');
    if (newDestinationName = '')
    then begin
           rDestination:= nil;
           rDestinationName:= '';
           rDestinationParams:= nil;
           SaveExt:= XMLWork.GetValue('Destination/Params/Format', 'jpg');
           SavePath:= XMLWork.GetValue('Destination/Params/Path', '');
           newDestinationI:= -1;
         end
    else begin
           if theBridge.DestinationsImpl.Select(newDestinationName) then
           begin
             rDestination:= theBridge.DestinationsImpl.Selected;
             rDestinationName:= theBridge.DestinationsImpl.SelectedName;
             rDestinationParams:= theBridge.DestinationsImpl.SelectedParams;
             theBridge.DestinationsImpl.LoadSelectedParams(Path_Config+Config_XMLWork, 'Destination/Params');
             newDestinationI:= theBridge.DestinationsImpl.SelectedIndex;
           end;
         end;

    XML_LoadPageSettings;

    Counters.Load(XMLWork, True);

    newCropMode:= TDigItCropMode(XMLWork.GetValue('CropMode', 0));
    setCropMode(newCropMode);
    if (newCropMode = diCropCustom) then
    begin
      UI_FillCounters;
      imgManipulation.CropAreas.Load(XMLWork, 'CropAreas');
      cbCounterList.ItemIndex :=XMLWork.GetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);
    end;

    XML_LoadCapturedFiles;

    UI_MenuItemsChecks(theBridge.SourcesImpl.SelectedIndex, newDestinationI);

    //User Interface
    rollCrops.Collapsed:=XMLWork.GetValue('UI/rollCrops_Collapsed', False);
    rollPages.Collapsed:=XMLWork.GetValue('UI/rollPages_Collapsed', True);
    rollCounters.Collapsed:=XMLWork.GetValue('UI/rollCounters_Collapsed', True);

  finally
    XML_Loading:= False;
    XMLWork.Free; XMLWork:= Nil;
  end;
end;

procedure TDigIt_Main.XML_SaveWork;
var
   i: Integer;
   curItemPath: String;

begin
  if (rSource <> Nil) and (rSource^.Inst <> Nil) then
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(Path_Config+Config_XMLWork);

     XMLWork.SetValue('CropMode', Integer(CropMode));

     XML_SavePageSettings;
     Counters.Save(XMLWork, True);
     if (cbCounterList.ItemIndex > -1)
     then XMLWork.SetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);

     if (CropMode = diCropCustom) then imgManipulation.CropAreas.Save(XMLWork, 'CropAreas');

     //User Interface
     XMLWork.SetValue('UI/rollCrops_Collapsed', rollCrops.Collapsed);
     XMLWork.SetValue('UI/rollPages_Collapsed', rollPages.Collapsed);
     XMLWork.SetValue('UI/rollCounters_Collapsed', rollCounters.Collapsed);

     //Save rSource and its Params
     XMLWork.SetValue('Source/Name', rSourceName);
     XMLWork.DeletePath('Source/Params/');

     //Save SourceFiles array
     XMLWork.DeletePath('SourceFiles/');
     XMLWork.SetValue('SourceFiles/LoadedFile', LoadedFile);
     XMLWork.SetValue('SourceFiles/Count', Length(SourceFiles));
     XMLWork.SetValue('SourceFiles/iSourceFiles', iSourceFiles);
     XMLWork.SetValue('SourceFiles/lastCropped', lastCropped);
     XMLWork.SetValue('SourceFiles/lastLenTaked', lastLenTaked);
     for i:=0 to Length(SourceFiles)-1 do
     begin
       curItemPath :='SourceFiles/Item' + IntToStr(i)+'/';
       XMLWork.SetValue(curItemPath+'fCrop', SourceFiles[i].fCrop);
       XMLWork.SetValue(curItemPath+'fName', SourceFiles[i].fName);
     end;

     //Save rDestination and its Params
     XMLWork.SetValue('Destination/Name', rDestinationName);
     if (rDestination = nil) then
     begin
       XMLWork.DeletePath('Destination/Params/');
       XMLWork.SetValue('Destination/Params/Format', SaveExt);
       XMLWork.SetValue('Destination/Params/Path', SavePath);
     end;

     XMLWork.Flush;
     XMLWork.Free; XMLWork:= Nil;

     //FPC Bug?
     //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
     //So we do it after destroying XMLWork.

     if (rSource <> nil) then rSource^.Inst.Params.Save(PChar(Path_Config+Config_XMLWork), 'Source/Params');
     if (rDestination <> nil) then rDestination^.Inst.Params.Save(PChar(Path_Config+Config_XMLWork), 'Destination/Params');
  finally
  end;
end;

procedure TDigIt_Main.XML_LoadCapturedFiles;
var
   i, newCount, newSelected:Integer;
   curFileName:String;

begin
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(Path_Config+Config_XMLWork);

     newCount := XMLWork.GetValue(XMLWork_Captured+'Count', 0);

     lvCaptured.BeginUpdate;

     lvCaptured.Clear;
     newSelected :=XMLWork.GetValue(XMLWork_Captured+'Selected', -1);

     for i:=0 to newCount-1 do
     begin
       curFileName :=XMLWork.GetValue(XMLWork_Captured+'Item' + IntToStr(i)+'/FileName', '');
       if (curFileName<>'')  { #todo -oMaxM : if null? there is a problem? }
       then TFileListItem(lvCaptured.Items.Add).FileName :=curFileName;
     end;

     if (newSelected>-1) and (newSelected<newCount)
     then lvCaptured.Selected:=lvCaptured.Items[newSelected];

     lvCaptured.EndUpdate;
  finally
  end;
end;

procedure TDigIt_Main.XML_SaveCapturedFile(AItem: TFileListItem);
begin
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(Path_Config+Config_XMLWork);

     XMLWork.SetValue(XMLWork_Captured+'Count', lvCaptured.Items.Count);
     if lvCaptured.Selected=nil
     then XMLWork.DeleteValue(XMLWork_Captured+'Selected')
     else XMLWork.SetValue(XMLWork_Captured+'Selected', lvCaptured.Selected.Index);
     XMLWork.SetValue(XMLWork_Captured+'Item'+IntToStr(AItem.Index)+'/FileName', AItem.FileName);
  finally
  end;
end;

procedure TDigIt_Main.XML_LoadProject(AFileName: String);
begin
  try
    if (XMLProject=nil) then XMLProject:=TXMLConfig.Create(AFileName);

    Counters.Load(XMLProject, False);
    imgManipulation.CropAreas.Load(XMLProject, 'CropAreas');
    UI_FillCounters;
    cbCounterList.ItemIndex :=XMLProject.GetValue(Counters.Name+'/Selected', -1);
    UI_FillCounter(Counters_GetCurrent);
    UI_ToolBar;

  finally
  end;
end;

procedure TDigIt_Main.XML_SaveProject(AFileName: String);
begin
  try
     if (XMLProject=nil) then XMLProject:=TXMLConfig.Create(AFileName);

     Counters.Save(XMLProject, False);
     XMLProject.SetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);
     imgManipulation.CropAreas.Save(XMLProject, 'CropAreas');
     XMLProject.Flush;
  finally
  end;
end;

procedure TDigIt_Main.XML_LoadPageSettings;
var
   selButton: Integer;

begin
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(Path_Config+Config_XMLWork);

     selButton := XMLWork.GetValue(XMLWork_PageSettings+'Rotate', -1);
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

     selButton := XMLWork.GetValue(XMLWork_PageSettings+'Flip', -1);
     Case selButton of
     0: btPFlipV.Down:= True;
     1: btPFlipH.Down:= True;
     else begin
            btPFlipV.Down:=  False;
            btPFlipH.Down:= False;
          end;
     end;

  finally
  end;
end;

procedure TDigIt_Main.XML_SavePageSettings;
begin
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(Path_Config+Config_XMLWork);

     if btPRotateLeft.Down
     then XMLWork.SetValue(XMLWork_PageSettings+'Rotate', 0)
     else
     if btPRotateRight.Down
     then XMLWork.SetValue(XMLWork_PageSettings+'Rotate', 1)
     else
     if btPRotate180.Down
     then XMLWork.SetValue(XMLWork_PageSettings+'Rotate', 2)
     else XMLWork.SetValue(XMLWork_PageSettings+'Rotate', -1);

     if btPFlipV.Down
     then XMLWork.SetValue(XMLWork_PageSettings+'Flip', 0)
     else
     if btPFlipH.Down
     then XMLWork.SetValue(XMLWork_PageSettings+'Flip', 1)
     else XMLWork.SetValue(XMLWork_PageSettings+'Flip', -1);

  finally
  end;
end;

procedure TDigIt_Main.Default_Work;
begin
  rDestinationName:= '';
  SaveExt:= 'jpg';
  SavePath:= Path_Pictures;
  (*
  rollCrops.Collapsed:=False;

  rollCounters.Collapsed:=True;*)
  setCropMode(diCropFull);
  rollPages.Collapsed:= False;
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
     if (newDestinationIndex = -1)
     then begin
            { #note 10 -oMaxM : Use of this function should be removed when SaveFiles is implemented as a descendant of IDigIt_Destination }
            if Destination_SaveFiles_Settings_Execute(SaveExt, SavePath) then
            begin
            end;
            rDestination:= nil;
            rDestinationName:= '';
            rDestinationParams:= nil;
          end
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

procedure TDigIt_Main.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
     closing :=True;
     { #todo 1 -oMaxM : Project Save Confirmation }
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

   if (CropArea.Name='')
   then CropArea.Name:='Name '+IntToStr(curIndex);

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
  if (cbBoxList.Items.Objects[cbBoxList.ItemIndex] = CropArea) then
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

procedure TDigIt_Main.tbWorkSaveClick(Sender: TObject);
begin
  Self.XML_SaveWork;
end;

procedure TDigIt_Main.TestClick(Sender: TObject);
var
   astr:String;

begin
  try
     astr :=theBridge.Settings.Path_Config;
     MessageDlg(astr, mtInformation, [mbOk], 0);
  finally
  end;
end;

procedure TDigIt_Main.TestRClick(Sender: TObject);
begin
  try
  finally
  end;
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
  curFileName: String;
  captItem: TFileListItem;  { #todo 10 -oMaxM : Use DestinationFiles array intestead }

begin
  if SavePDF.Execute then
  try
     with DigIt_Progress do
     begin
       labTotal.Caption:= '';
       capTotal.Caption:= '';
       progressTotal.Style:= pbstNormal;
       progressTotal.Min:= 0;
       progressTotal.Max:= lvCaptured.Items.Count-1;
       progressTotal.Position:= 0;
       panelCurrent.Visible:= False;
       Show('Converting Images to PDF...');
       cStr:= IntToStr(progressTotal.Max);
     end;

     PDF:= TPDFDocument.Create(Nil);

     PDF.Infos.Title := Application.Title;
     PDF.Infos.Author := 'MaxM';
     PDF.Infos.Producer := Application.Title;
     PDF.Infos.ApplicationName := ApplicationName;
     PDF.Infos.CreationDate := Now;
     PDF.Options := [poCompressImages, poUseRawJPEG];

     PDF.StartDocument;
     S := PDF.Sections.AddSection;

     for i := 0 to lvCaptured.Items.Count-1 do
     begin
       DigIt_Progress.progressTotal.Position:= i;
       DigIt_Progress.capTotal.Caption:= 'Processing '+IntToStr(i)+' / '+cStr;
       Application.ProcessMessages;

       captItem:= TFileListItem(lvCaptured.Items[i]);
       curFileName:= captItem.FileName;
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
       DigIt_Progress.capTotal.Caption:= 'Processed '+IntToStr(i)+' / '+cStr;
       Application.ProcessMessages;
     end;

     PDF_File:= TFileStream.Create(SavePDF.FileName, fmCreate);
     PDF.SaveToStream(PDF_File);

  finally
     DigIt_Progress.Hide;

     PDF.Free;
     PDF_File.Free;
  end;
end;

procedure TDigIt_Main.setProject_File(AValue: String);
begin
  if rProject_File=AValue then Exit;
  rProject_File:=AValue;
  if (rProject_File='')
  then Caption :='DigIt'
  else Caption :='DigIt'+' - '+ExtractFileName(rProject_File);
end;

procedure TDigIt_Main.setCropMode(ANewCropMode: TDigItCropMode);
begin
  if (ANewCropMode <> CropMode) then
  begin
    { #todo -oMaxM : Code when switching from one mode to another }
    Case ANewCropMode of
      diCropFull: begin
        if (CropMode = diCropCustom) then
        begin
          if (imgManipulation.CropAreas.Count > 0)
          then if (MessageDlg('DigIt', 'Clear Crop Areas ?', mtConfirmation, mbYesNo, 0) = mrNo)
               then exit;
        end;

       (* tbSepCrop.Visible:= False;
        actCrop.Visible:= False;
        actCropNext.Visible:= False;
        actCropGoBack.Visible:= False;
        actCropAll.Visible:= False;
        tbCropSummary.Visible:= False;
        *)
        tbCrop.Visible:= False;
        imgManipulation.clearCropAreas;
        imgManipulation.Opacity:= 0;
        imgManipulation.Enabled:= False;
        rollCrops.Enabled:= False; rollCrops.Collapsed:= True;

        panelCounterList.Enabled:= False;

        if (Counters.Count = 0)
        then Counters.Add('Counter 0')
        else Counters.RemoveAllButFirst;

        if not(XML_Loading) then
        begin
          UI_FillCounters;
          UI_FillCounter(Counters[0]);
        end;

        tbCropMode.Caption:= 'Full Area';
      end;
      diCropCustom: begin
        (*
        tbSepCrop.Visible:= True;
        actCrop.Visible:= True;
        actCropNext.Visible:= True;
        actCropGoBack.Visible:= True;
        actCropAll.Visible:= True;
        *)
        tbCrop.Visible:= True;
        imgManipulation.Opacity:= 128;
        imgManipulation.Enabled:= True;
        rollCrops.Enabled:= True; rollCrops.Collapsed:= False;

        panelCounterList.Enabled:= True;

        (*
        if not(XML_Loading) then
        begin
          UI_FillCounters;
          UI_FillCounter(Counters_GetCurrent);
        end;
        *)

        tbCropMode.Caption:= 'Custom';
      end;
    end;

    CropMode:= ANewCropMode;

    if not(XML_Loading) then UI_ToolBar;
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
  Result:= (AFileName <> '') and LoadImage(AFileName);
  if Result then
  begin
//    Counters.CopyValuesToPrevious;
    SaveCallBack(imgManipulation.Bitmap, nil, 0);
  end;
end;

procedure TDigIt_Main.CropFile_Full(AStartIndex: Integer);
var
   i,
   c: Integer;
   cStr: String;

begin
  try
     c:= Length(SourceFiles);
     cStr:= IntToStr(c);
     DigIt_Progress.progressTotal.Min:= AStartIndex;
     DigIt_Progress.progressTotal.Max:= c;

     for i:=AStartIndex to c-1 do
     begin
       DigIt_Progress.progressTotal.Position:= i;
       DigIt_Progress.capTotal.Caption:= 'Processing '+IntToStr(i)+' / '+cStr;
       Application.ProcessMessages;

       CropFile_Full(SourceFiles[i].fName);
       SourceFiles[i].fCrop:= True;

       if (i > lastCropped) then lastCropped:= i;
       iSourceFiles:= i;

       DigIt_Progress.progressTotal.Position:= i+1;
       DigIt_Progress.capTotal.Caption:= 'Processed '+IntToStr(i)+' / '+cStr;
       Application.ProcessMessages;
     end;

  finally
    UserCancel:= False;
  end;
end;

procedure TDigIt_Main.ProgressImagesShow(TotalMin, TotalMax: Integer);
begin
  with DigIt_Progress do
  begin
    labTotal.Caption:= '';
    capTotal.Caption:= '';
    progressTotal.Style:= pbstNormal;
    progressTotal.Min:= TotalMin;
    progressTotal.Max:= TotalMax;
    progressTotal.Position:= TotalMin;
    panelCurrent.Visible:= False;
    Show('Processing Images');
  end;
end;

procedure TDigIt_Main.Pages_InsertMiddle(ASourceFileIndex: Integer);
begin
  { #todo 5 -oMaxM : Re index the crops starting from the lastCropped }
  MessageDlg('Not yet implemented', mtInformation, [mbOk], 0);
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
  end;
end;

procedure TDigIt_Main.UI_ToolBar;
var
   bCommonCond: Boolean;
   cropSumStr: String;
   lenSources,
   remSources: Integer;

begin
  bCommonCond:= (rSource<>nil) and (rSource^.Inst <> nil);
  actPreview.Enabled:= bCommonCond;

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
           tbCropSummary.Caption:= IntToStr(remSources)+' files to do';
         end;

       end
  else actTake.Enabled:= bCommonCond and DirectoryExists(SavePath);

  actTimerTake.Enabled:= actTake.Enabled;
  actTakeBuildDuplex.Enabled:= actTake.Enabled;
  actTakeAgain.Enabled:= actTake.Enabled and (lastLenTaked > 0);
end;

procedure TDigIt_Main.UI_ToolBarAddShortcuts;
var
   i: Integer;
   curBtn: TToolButton;
   st:String;

begin
  for i:=0 to tbMain.ButtonCount-1 do
  begin
    curBtn:= tbMain.Buttons[i];
    if (curBtn.Action <> nil) and (TAction(curBtn.Action).ShortCut <> 0)
    then curBtn.Caption:= curBtn.Caption+' ('+ShortCutToText(TAction(curBtn.Action).ShortCut)+')';
  end;
  tbMain.AdjustSize;

  for i:=0 to tbCrop.ButtonCount-1 do
  begin
    curBtn:= tbCrop.Buttons[i];
    if (curBtn.Action <> nil) and (TAction(curBtn.Action).ShortCut <> 0)
    then curBtn.Caption:= curBtn.Caption+' ('+ShortCutToText(TAction(curBtn.Action).ShortCut)+')';
  end;
  tbCrop.AdjustSize;
  tbCrop.Left:= tbMain.Left+tbMain.Width+40;
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
           edCounterValue.Value:=ACounter.Value;
           edCounterValueStringDigits.Value:=ACounter.Value_StringDigits;
           edCounterValueStringPre.Text:=ACounter.Value_StringPre;
           edCounterValueStringPost.Text:=ACounter.Value_StringPost;
           lbPrevious.Caption:='Value Previous:'+IntToStr(ACounter.Value_Previous);
           UpdateCounterExampleLabel(ACounter);
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

