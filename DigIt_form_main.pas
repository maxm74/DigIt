(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Main Form                                                                **
*******************************************************************************)

unit DigIt_Form_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtDlgs, ExtCtrls, Menus, ComCtrls,
  ActnList, Spin, ShellCtrls, EditBtn, ExpandPanels, Digit_Bridge, Digit_Taker_Folder,
  FPImage, BGRAImageManipulation, BGRABitmap, BGRABitmapTypes, BGRASpeedButton, BCPanel, BCLabel, BCListBox,
  BGRAImageList, Laz2_XMLCfg, SpinEx, BGRAPapers, DigIt_Types, DigIt_Utils,  DigIt_Counters;

type

  { TDigIt_Main }

  TDigIt_Main = class(TForm)
    actProjectSaveAs: TAction;
    actPreview: TAction;
    actRotateRight: TAction;
    actRotateLeft: TAction;
    ActionListC: TActionList;
    actOptions: TAction;
    actProjectOpen: TAction;
    actProjectSave: TAction;
    actProjectNew: TAction;
    actReTake: TAction;
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
    BCLabel9: TBCLabel;
    btPageSizesToCrops: TSpeedButton;
    imgListMenu: TImageList;
    imgListThumb: TBGRAImageList;
    lvCaptured: TListView;
    panelPageSize: TBCPanel;
    btBox_Add: TBGRASpeedButton;
    btCFlipHLeft: TSpeedButton;
    btCFlipHRight: TSpeedButton;
    btCFlipVDown: TSpeedButton;
    btCFlipVUp: TSpeedButton;
    btCounter_Add: TBGRASpeedButton;
    btBox_Del: TBGRASpeedButton;
    btCounter_Del: TBGRASpeedButton;
    btCropCounter_Add: TBGRASpeedButton;
    btCropCounter_Del: TBGRASpeedButton;
    btCropDuplicate: TSpeedButton;
    btCropDuplicateOp: TSpeedButton;
    btCRotateLeft: TSpeedButton;
    btCRotateRight: TSpeedButton;
    btPageSizes: TSpeedButton;
    btZBack: TSpeedButton;
    btZDown: TSpeedButton;
    btZFront: TSpeedButton;
    btZUp: TSpeedButton;
    cbBoxList: TComboBox;
    cbCounterList: TComboBox;
    cbCropCounterList: TComboBox;
    cbSaveFormat: TComboBox;
    dirDestination: TDirectoryEdit;
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
    edPageHeight: TFloatSpinEdit;
    edPageWidth: TFloatSpinEdit;
    edPage_UnitType: TComboBox;
    edCropWidth: TFloatSpinEdit;
    imgListCaptured: TImageList;
    imgManipulation: TBGRAImageManipulation;
    imgListMain: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbPrevious: TLabel;
    lbTakerSummary: TLabel;
    lbCounterExample: TLabel;
    menuOptions: TMenuItem;
    OpenProject: TOpenDialog;
    panelCounter: TBCPanel;
    panelCropArea: TBCPanel;
    panelMainToolbar: TPanel;
    menuPaperSizes: TPopupMenu;
    menuTakers: TPopupMenu;
    rgCropAspect: TRadioGroup;
    rollCounters: TMyRollOut;
    SaveProject: TSaveDialog;
    Separator1: TMenuItem;
    menuProjectOpen: TMenuItem;
    menuProjectSave: TMenuItem;
    menuProjectNew: TMenuItem;
    rollCrops: TMyRollOut;
    rollPages: TMyRollOut;
    OpenCropList: TOpenDialog;
    OpenPicture: TOpenPictureDialog;
    MenuMain: TPopupMenu;
    SaveCropList: TSaveDialog;
    SavePicture: TSavePictureDialog;
    SelectDirectory: TSelectDirectoryDialog;
    btPaperSizes: TSpeedButton;
    tbCaptured: TToolBar;
    tbCapturedRotateLeft: TToolButton;
    tbMain: TToolBar;
    tbTake: TToolButton;
    tbReTake: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    tbMenu: TToolButton;
    ToolButton6: TToolButton;
    tbPreview: TToolButton;
    procedure actOptionsExecute(Sender: TObject);
    procedure actProjectNewExecute(Sender: TObject);
    procedure actProjectOpenExecute(Sender: TObject);
    procedure actProjectSaveAsExecute(Sender: TObject);
    procedure actProjectSaveExecute(Sender: TObject);
    procedure actRotateLeftExecute(Sender: TObject);
    procedure actRotateRightExecute(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure actTakeExecute(Sender: TObject);
    procedure actReTakeExecute(Sender: TObject);
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
    procedure cbSaveFormatChange(Sender: TObject);
    procedure dirDestinationChange(Sender: TObject);
    procedure edPageHeightChange(Sender: TObject);
    procedure edPageWidthChange(Sender: TObject);
    procedure edPage_UnitTypeChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvCapturedCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure lvCapturedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lvCapturedDblClick(Sender: TObject);
    procedure lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure TakerMenuClick(Sender: TObject);
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

    procedure TestClick(Sender: TObject);
    procedure TestRClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { private declarations }
    lastNewBoxNum: Word;
    changingAspect, closing,
    inFillCounterUI,
    inFillBoxUI,
    inFillPagesUI: Boolean;
    Counters: TDigIt_CounterList;
    XMLWork,
    XMLProject: TXMLConfig;
    takerClass: TDigIt_TakerClasses;
    takerInst:TDigIt_Taker;
    takerParams: TPersistent;
    SaveExt,
    SavePath,
    rProject_File: String;
    testI: Integer;

    function GetCurrentCropArea: TCropArea;
    function GetCurrentCounter: TDigIt_Counter;
    procedure UI_FillBox(ABox :TCropArea);
    procedure UI_FillCounter(ACounter :TDigIt_Counter);
    procedure UI_FillPageSizes;
    procedure UI_FillTaker;
    procedure UI_Load;
    function UI_FindSaveFormat(AExt:String):Integer;
    procedure SaveCallBack(Bitmap :TBGRABitmap; CropArea: TCropArea; AUserData:Integer);
    procedure UpdateBoxList;
    procedure UpdateCropAreaCountersList(ACounter :TDigIt_Counter);
    procedure UpdateCounterExampleLabel(ACounter :TDigIt_Counter);
    procedure CounterSelect(AIndex:Integer);
    procedure BuildSaveFormats;
    procedure LoadImage(AImageFile:String);
    procedure XML_LoadWork;
    procedure XML_SaveWork;
    procedure XML_LoadCapturedFiles;
    procedure XML_SaveCapturedFile(AItem:TFileListItem);
    procedure XML_LoadProject(AFileName:String);
    procedure XML_SaveProject(AFileName:String);
    procedure Taker_SelectUserParams(curClass :TDigIt_TakerClasses);
    procedure Taker_SelectWithParams(curClass :TDigIt_TakerClasses; curParams:TPersistent);
    procedure setProject_File(AValue: String);

    procedure WaitForAFile(AFileName: String; ATimeOut: Integer);

    property Project_File:String read rProject_File write setProject_File;
  public
    procedure ItemSizesClick(Sender: TObject);
    procedure PageSizesClick(Sender: TObject);
  end;

var
  DigIt_Main: TDigIt_Main;

implementation

{$R *.lfm}

uses
  {$ifopt D+}
  lazlogger,
  {$endif}
  LCLIntf, DigIt_Form_Templates, DelphiTwain, DelphiTwain_VCL;


{ TDigIt_Main }

procedure TDigIt_Main.edCounterNameEditingDone(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  curCounter :=GetCurrentCounter;
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

  curCounter :=GetCurrentCounter;
  if (curCounter<>nil)
  then curCounter.Value :=edCounterValue.Value;
end;

procedure TDigIt_Main.edCounterValueStringDigitsChange(Sender: TObject);
var
   curCounter :TDigIt_Counter;

begin
  if inFillCounterUI then exit;

  curCounter :=GetCurrentCounter;
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
  curCounter :=GetCurrentCounter;
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
  curCounter :=GetCurrentCounter;
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

  Counters :=TDigIt_CounterList.Create('Counters');

  closing :=False;
  changingAspect :=False;
  inFillCounterUI :=False;
  inFillBoxUI :=False;
  inFillPagesUI :=False;

  lastNewBoxNum :=0;
  TStringList(cbBoxList.Items).OwnsObjects:=False;
  TStringList(cbCounterList.Items).OwnsObjects:=False;

  takerClass:=nil;
  takerInst:=nil;
  takerParams:=nil;
  BuildTakersMenu(Self, menuTakers, @TakerMenuClick);
  lbTakerSummary.Caption:='';
  BuildSaveFormats;
end;

procedure TDigIt_Main.FormDestroy(Sender: TObject);
begin
  Counters.Free;

  if (takerInst<>nil) then takerInst.Free;
  if (XMLWork<>nil) then XMLWork.Free;
  if (XMLProject<>nil) then XMLProject.Free;
end;

procedure TDigIt_Main.FormShow(Sender: TObject);
var
   i:Integer;

begin
  for i:=0 to tbCaptured.ButtonCount-1 do
     tbCaptured.Buttons[i].Hint:=tbCaptured.Buttons[i].Caption;

  if FileExists(ConfigDir+Config_XMLWork)
     {$ifopt D-}
      and (MessageDlg('DigIt', 'Continue from last Session?', mtConfirmation, [mbYes, mbNo], 0)=mrYes)
     {$endif}
  then begin
         XML_LoadWork;
         actPreview.Execute;
       end
  else begin
         //rollCrops.Collapsed:=True;
         rollPages.Collapsed:=True;
         //rollCounters.Collapsed:=True;
       end;
end;

procedure TDigIt_Main.actPreviewExecute(Sender: TObject);
var
  curImageFile:String;

begin
  try
    if (takerInst<>nil) then
    begin
      curImageFile :=takerInst.Preview;
      if (curImageFile<>'') then
      begin
        WaitForAFile(curImageFile, 30000);
        LoadImage(curImageFile);
      end;
      UI_FillTaker;
    end;
  finally
  end;
end;

procedure TDigIt_Main.actTakeExecute(Sender: TObject);
var
  curImageFile:String;

begin
  try
    if (takerInst<>nil) and not(imgManipulation.Empty) then
    begin
      curImageFile :=takerInst.Take;
      if (curImageFile<>'') then
      begin
        WaitForAFile(curImageFile, 30000);
        LoadImage(curImageFile);
        Counters.CopyValuesToPrevious;
        //lvCaptured.BeginUpdate;
        imgManipulation.getAllBitmaps(@SaveCallBack);
        //lvCaptured.EndUpdate;
        XML_SaveWork;
      end;
      UI_FillTaker;
    end;
  finally
  end;
end;

procedure TDigIt_Main.actReTakeExecute(Sender: TObject);
var
  curImageFile:String;

begin
  try
    if (takerInst<>nil) and not(imgManipulation.Empty) then
    begin
      curImageFile :=takerInst.ReTake;
      if (curImageFile<>'') then
      begin
        WaitForAFile(curImageFile, 30000);
        LoadImage(curImageFile);
        Counters.CopyPreviousToValues;
        //lvCaptured.BeginUpdate;
        imgManipulation.getAllBitmaps(@SaveCallBack, 1);
        //lvCaptured.EndUpdate;
        XML_SaveWork;
      end;
      UI_FillTaker;
    end;
  finally
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
   Paper: TPaperSize;
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
   Paper: TPaperSize;

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

procedure TDigIt_Main.cbSaveFormatChange(Sender: TObject);
var
   i:Integer;

begin
  SaveExt:=ImageHandlers.Extensions[cbSaveFormat.Items[cbSaveFormat.ItemIndex]];
  i :=Pos(';', SaveExt);
  if (i>0) then SaveExt :=Copy(SaveExt, 1, i-1);
end;

procedure TDigIt_Main.dirDestinationChange(Sender: TObject);
begin
  SavePath :=dirDestination.Directory;
  UI_FillTaker;
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

procedure TDigIt_Main.FormResize(Sender: TObject);
begin
  panelMainToolbar.Left:=tbMain.Width div 2;
  panelMainToolbar.Width:=tbMain.Width div 2;
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
    Bitmap := TBGRABitmap.Create;
    Bitmap.LoadFromFile(TFileListItem(Item).FileName);
    GetThumnailSize(itemRect.Width, itemRect.Height, Bitmap.Width, Bitmap.Height, newWidth, newHeight);
    destRect.Left:=itemRect.Left+((itemRect.Width-newWidth) div 2);
    destRect.Top:=itemRect.Top+((itemRect.Height-newHeight) div 2);
    destRect.Width:=newWidth;
    destRect.Height:=newHeight;
    BitmapR :=Bitmap.Resample(newWidth, newHeight);
    BitmapR.Draw(lvCaptured.Canvas, destRect, True);
    DefaultDraw:=True;
    {$ifopt D+}
      DebugLn('draw='+IntToStr(Item.Index));
    {$endif}
  finally
    Bitmap.Free;
    BitmapR.Free;
  end;
end;

procedure TDigIt_Main.lvCapturedDblClick(Sender: TObject);
var
   tt:TFileListItem;

begin
  tt:=TFileListItem(lvCaptured.Selected);
  if (tt<>nil)
  then OpenDocument(tt.FileName);
end;

procedure TDigIt_Main.lvCapturedShowHint(Sender: TObject; HintInfo: PHintInfo);
var
   tt:TFileListItem;

begin
  tt:=TFileListItem(lvCaptured.GetItemAt(HintInfo^.CursorPos.X, HintInfo^.CursorPos.Y));
  if (tt<>nil)
  then HintInfo^.HintStr:=tt.FileName;
end;

procedure TDigIt_Main.TakerMenuClick(Sender: TObject);
var
   curClass :TDigIt_TakerClasses;
   newInst :TDigIt_Taker;

begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  begin
    curClass :=TDigIt_TakerClasses(theBridge.Takers.GetTaker(TMenuItem(Sender).Tag));
    Taker_SelectUserParams(curClass);
    UI_FillTaker;
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
      FreeAndNil(DigIt_Templates);
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
begin
  //
end;

procedure TDigIt_Main.actRotateRightExecute(Sender: TObject);
begin
  //
end;

procedure TDigIt_Main.SaveCallBack(Bitmap: TBGRABitmap; CropArea: TCropArea; AUserData: Integer);
var
  cropCounter:TDigIt_Counter;
  savedFile:String;
  captItem:TFileListItem;

begin
  if (CropArea<>nil) then
  begin
    if (CropArea.UserData<0)
    then begin
           savedFile:=SavePath+DirectorySeparator+CropArea.Name+'.'+SaveExt;
           Bitmap.SaveToFile(savedFile);
         end
    else begin
           cropCounter :=TDigIt_Counter(Counters.items[CropArea.UserData]);
           cropCounter.Value:=cropCounter.Value+1;
           savedFile:=SavePath+DirectorySeparator+cropCounter.GetValue+'.'+SaveExt;
           Bitmap.SaveToFile(savedFile);
         end;

    if (AUserData=0)
    then begin   //Take, add file to Captured List
           captItem :=TFileListItem(lvCaptured.Items.Add);
           captItem.FileName:=savedFile;
           XML_SaveCapturedFile(captItem);
         end
    else begin
           captItem :=FindFileListItem(lvCaptured.Items, savedFile);
           if (captItem=nil)
           then begin
                  captItem :=TFileListItem(lvCaptured.Items.Add);
                  captItem.FileName:=savedFile;
                  XML_SaveCapturedFile(captItem);
                end;
           //else lvCaptured;   { #todo 2 -oMaxM : Update Item }
         end;
    captItem.MakeVisible(False);
  end;
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
  UI_FillCounter(GetCurrentCounter);
end;

procedure TDigIt_Main.BuildSaveFormats;
var
   i,j :Integer;
   t,e:String;

begin
  j:=0;
  for i :=0 to ImageHandlers.Count-1 do
  begin
    t :=ImageHandlers.TypeNames[i];
    e :=ImageHandlers.Extensions[t];
    if (ImageHandlers.ImageWriter[t]<>nil) then
    begin
      cbSaveFormat.Items.Add(t);
      if (Pos('jpg', e)>0) then j:=i;
    end;
  end;
  cbSaveFormat.ItemIndex:=j-1;
  SaveExt :='jpg';
end;

procedure TDigIt_Main.LoadImage(AImageFile: String);
var
   Bitmap :TBGRABitmap;

begin
  try
     Bitmap := TBGRABitmap.Create;
     //DetectFileFormat(AImageFile);
     Bitmap.LoadFromFile(AImageFile);
     imgManipulation.Bitmap := Bitmap;
  finally
     Bitmap.Free;
  end;
end;

procedure TDigIt_Main.XML_LoadWork;
var
   takerName:String;
   aClass:TDigIt_TakerClasses;
   aClassInstParams:TPersistent;

begin
  try
    if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(ConfigDir+Config_XMLWork);

    takerName :=XMLWork.GetValue('Taker', '');
    SaveExt :=XMLWork.GetValue('Format', 'jpg');
    SavePath :=XMLWork.GetValue('Destination', '');
    if (takerName<>'') then
    begin
      aClass :=theBridge.Takers.Taker[takerName];
      if assigned(aClass) then
      begin
        //Read Params
        aClassInstParams :=aClass.Params_GetClass.Create;
        ReadPersistentFromXMLConfig(XMLWork, 'Params', '', aClassInstParams);

        Taker_SelectWithParams(aClass, aClassInstParams);
      end;
    end;
    Counters.Load(XMLWork, True);
    imgManipulation.CropAreas.Load(XMLWork, 'CropAreas');
    XML_LoadCapturedFiles;
    UI_Load;
    cbCounterList.ItemIndex :=XMLWork.GetValue(Counters.Name+'/Selected', -1);
    UI_FillCounter(GetCurrentCounter);

    //User Interface
    rollCrops.Collapsed:=XMLWork.GetValue('UI/rollCrops_Collapsed', True);
    rollPages.Collapsed:=XMLWork.GetValue('UI/rollPages_Collapsed', True);
    rollCounters.Collapsed:=XMLWork.GetValue('UI/rollCounters_Collapsed', True);

  finally
  end;
end;

procedure TDigIt_Main.XML_SaveWork;
begin
  if (takerInst<>nil) then
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(ConfigDir+Config_XMLWork);

     XMLWork.SetValue('Taker', takerInst.RegisterName);
     XMLWork.SetValue('Format', SaveExt);
     XMLWork.SetValue('Destination', SavePath);
     WritePersistentToXMLConfig(XMLWork, 'Params', '', takerInst.Params_Get);

     Counters.Save(XMLWork, True);
     XMLWork.SetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);
     imgManipulation.CropAreas.Save(XMLWork, 'CropAreas');

     //User Interface
     XMLWork.SetValue('UI/rollCrops_Collapsed', rollCrops.Collapsed);
     XMLWork.SetValue('UI/rollPages_Collapsed', rollPages.Collapsed);
     XMLWork.SetValue('UI/rollCounters_Collapsed', rollCounters.Collapsed);

     XMLWork.Flush;
  finally
  end;
end;

procedure TDigIt_Main.XML_LoadCapturedFiles;
var
   i, newCount, newSelected:Integer;
   curFileName:String;

begin
  try
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(ConfigDir+Config_XMLWork);

     newCount := XMLWork.GetValue(XMLWork_Captured+'Count', -1);
     if (newCount=-1)
     then raise Exception.Create('XML Path not Found - '+XMLWork_Captured+'Count');

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
     if (XMLWork=nil) then XMLWork:=TXMLConfig.Create(ConfigDir+Config_XMLWork);

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
    UI_Load;
    cbCounterList.ItemIndex :=XMLProject.GetValue(Counters.Name+'/Selected', -1);
    UI_FillCounter(GetCurrentCounter);
  finally
  end;
end;

procedure TDigIt_Main.XML_SaveProject(AFileName: String);
begin
  try
     if (XMLProject=nil) then XMLProject:=TXMLConfig.Create(AFileName);

     //XMLProject.SetValue('Taker', takerInst.RegisterName);
     //XMLProject.SetValue('Format', SaveExt);
     //XMLProject.SetValue('Destination', SavePath);
     //WritePersistentToXMLConfig(XMLProject, 'Params', '', takerInst.Params_Get);
     Counters.Save(XMLProject, False);
     XMLProject.SetValue(Counters.Name+'/Selected', cbCounterList.ItemIndex);
     imgManipulation.CropAreas.Save(XMLProject, 'CropAreas');
     XMLProject.Flush;
  finally
  end;
end;

procedure TDigIt_Main.Taker_SelectUserParams(curClass :TDigIt_TakerClasses);
var
   newInst:TDigIt_Taker;

begin
  if assigned(curClass) then
  begin
    if (curClass<>takerClass)
    then begin
           //New Taker is different from actual, Create a new Instance and GetParams from User
           newInst :=curClass.Create(nil);
           if newInst.Params_GetFromUser
           then begin
                  //If User say Ok then use this New Instance as Taker
                  if (takerInst<>nil) then takerInst.Free;
                  takerClass :=curClass;
                  takerInst :=newInst;
                end
           else newInst.Free;
         end
    else begin
           //GetParams from User only
           if (takerInst=nil) { #todo 2 -oMaxM : anomaly }
           then takerInst :=takerClass.Create(nil);

           takerInst.Params_GetFromUser; { #todo 2 -oMaxM : if False? }
         end;
  end;
end;

procedure TDigIt_Main.Taker_SelectWithParams(curClass :TDigIt_TakerClasses; curParams:TPersistent);
var
   newInst:TDigIt_Taker;

begin
  if assigned(curClass) then
  begin
    if (curClass<>takerClass)
    then begin
           //New Taker is different from actual, Create a new Instance
           newInst :=curClass.Create(curParams);

           if (takerInst<>nil) then takerInst.Free;
           takerClass :=curClass;
           takerInst :=newInst;
         end
    else begin
           { #todo 2 -oMaxM : anomaly }
           if (takerInst=nil)
           then takerInst :=takerClass.Create(curParams);
         end;

    takerInst.Params_Set(curParams);
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

   UI_FillBox(CropArea);
end;

procedure TDigIt_Main.DeletedCrop(Sender: TBGRAImageManipulation; CropArea: TCropArea);
var
   delIndex :Integer;
begin
  try
    if not(closing) then
    begin
         delIndex :=cbBoxList.Items.IndexOfObject(CropArea);
         if (delIndex<>-1)
         then cbBoxList.Items.Delete(delIndex);
         panelCropArea.Enabled:=(cbBoxList.Items.Count>0);
    end;
  except
  end;
  //MessageDlg('Deleting Crop Area', 'Deleting '+CropArea.Name, mtInformation, [mbOk], 0);
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
   then newIndex :=cbBoxList.Items.IndexOfObject(imgManipulation.SelectedCropArea) //BGRAImageManipulation.SelectedCropArea.Index;
   else newIndex :=-1;

   cbBoxList.ItemIndex:=newIndex;
   UI_FillBox(imgManipulation.SelectedCropArea);
end;

procedure TDigIt_Main.TestClick(Sender: TObject);
var
   Twain: TDelphiTwain=nil;
   Twain_Source:TTwainSource;
   Twain_SourceI, i:Integer;
   astr:String;

begin
  try
    //Create Twain
   Twain := TDelphiTwain.Create;
   if Twain.LoadLibrary then
   begin
     Twain.SourceManagerLoaded := TRUE;
     for i:=0 to Twain.SourceCount-1 do
     begin
       aStr:=Twain.Source[i].ProductName;
     end;
   end;
  finally
     Twain.Free
  end;
(*
    if Twain = nil then
    begin
      Twain := TDelphiTwain.Create;
      Twain.OnTwainAcquire := @TwainTwainAcquire;
    end;

    //Load Twain Library dynamically
    if Twain.LoadLibrary then
    begin
      //Load source manager
      Twain.SourceManagerLoaded := TRUE;

    (*  Twain_Source:=TTwainSource.Create(Twain);
      ReadPersistentFromXMLConfig(XMLWork, 'ScanTest', '', Twain_Source);
      Twain_SourceI:=Twain.FindSource(Twain_Source);
      Twain.SelectedSourceIndex:=Twain_SourceI;  *)

      //Allow user to select source -> only the first time
      if not Assigned(Twain.SelectedSource) then
        Twain.SelectSource;

      if Assigned(Twain.SelectedSource) then
      begin
        //Load source, select transference method and enable (display interface)}
        Twain.SelectedSource.Loaded := TRUE;
        //Twain.SelectedSource.ShowUI := TRUE;//display interface
        Twain.SelectedSource.Enabled := True;

//        WritePersistentToXMLConfig(XMLWork, 'ScanTest', '', Twain.SelectedSource);
      end;

    end else begin
      ShowMessage('Twain is not installed.');
    end;
  finally
  end;
  *)
end;

procedure TDigIt_Main.TestRClick(Sender: TObject);
begin
  try
  finally
  end;
end;

procedure TDigIt_Main.ToolButton2Click(Sender: TObject);
var
   tt:TListItem;

begin
  //tt :=lvCaptured.Items.Add;
  //tt.Caption:='Test Image.jpg';
end;

procedure TDigIt_Main.setProject_File(AValue: String);
begin
  if rProject_File=AValue then Exit;
  rProject_File:=AValue;
  if (rProject_File='')
  then Caption :='DigIt'
  else Caption :='DigIt'+' - '+ExtractFileName(rProject_File);
end;

procedure TDigIt_Main.WaitForAFile(AFileName:String; ATimeOut: Integer);
var
   timeStart, timeCur:DWord;

begin
  if (AFileName='') then exit;

  timeStart :=GetTickCount;
  repeat
    Application.ProcessMessages;
    timeCur :=GetTickCount;
  until FileExists(AFileName) or ((timeCur-timeStart)>=ATimeOut);
end;

function TDigIt_Main.GetCurrentCropArea: TCropArea;
begin
  if (cbBoxList.ItemIndex<0)
  then Result :=nil
  else Result :=TCropArea(cbBoxList.Items.Objects[cbBoxList.ItemIndex]);
end;

function TDigIt_Main.GetCurrentCounter: TDigIt_Counter;
begin
  if (cbCounterList.ItemIndex<0)
  then Result :=nil
  else Result :=TDigIt_Counter(cbCounterList.Items.Objects[cbCounterList.ItemIndex]);
end;

procedure TDigIt_Main.UI_FillTaker;
begin
  actPreview.Enabled:=(takerInst<>nil);
  actTake.Enabled:=(takerInst<>nil) and not(imgManipulation.Empty) and DirectoryExists(SavePath);
  actReTake.Enabled:=actTake.Enabled;
  if (takerInst<>nil)
  then lbTakerSummary.Caption:=takerInst.UI_Title+':'+#13#10+takerInst.UI_Params_Summary
  else lbTakerSummary.Caption:='';
end;

procedure TDigIt_Main.UI_Load;
var
   i:Integer;
   curCounter:TDigIt_Counter;
   curCropArea:TCropArea;

begin
  UI_FillTaker;

  dirDestination.Directory:=SavePath;
  cbSaveFormat.ItemIndex:=UI_FindSaveFormat(SaveExt);

  //UI for Crop Areas is filled in Events

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
  if (curCropArea=nil)
  then cbCropCounterList.ItemIndex:=-1
  else cbCropCounterList.ItemIndex:=curCropArea.UserData;
end;

function TDigIt_Main.UI_FindSaveFormat(AExt: String): Integer;
var
   i:Integer;

begin
  Result:=-1;

  if (AExt<>'') then
  for i:=0 to cbSaveFormat.Items.Count-1 do
  begin
    if (Pos(AExt, ImageHandlers.Extensions[cbSaveFormat.Items[i]]) > 0) then
    begin
      Result :=i; break;
    end;
  end;
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
           if (ABox.UserData>-1)
           then CounterSelect(ABox.UserData);

           inFillBoxUI :=False;
        end
   else panelCropArea.Enabled :=False;
end;

procedure TDigIt_Main.UI_FillCounter(ACounter: TDigIt_Counter);
begin
   if (ACounter<>nil)
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

