(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Wizard to Build Front/Back Pages from a Front Only Source                **
*******************************************************************************)

unit DigIt_Form_BuildDuplex;

{$mode objfpc}{$H+}

//{$define Test}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls,
  Digit_Bridge_Intf, Digit_Bridge_Impl, BGRAAnimatedGif;

const
     CAPTION_PREV ='&Back';
     CAPTION_NEXT ='&Next';
     CAPTION_END ='&End';
     CAPTION_CANCEL ='&Cancel';
     MSG_CONFIRM_CANCELSTEP ='Cancel the Operation?';
     MSG_ERR_CANCELSTEP ='Operation aborted by User';

type
  { TWizardBuildDuplex }
  Exception_CancelledOp = class (Exception);

  TWizardBuildDuplex = class(TForm, IDigIt_ROArray, IDigIt_ProgressCallback)
    Image1: TImage;
    Image2: TImage;
    animTurn: TImage;
    Image4: TImage;
    Image5: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lbBackSide: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbBackSide1: TLabel;
    Panel1: TPanel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    btNext: TBitBtn;
    btPrev: TBitBtn;
    btCancel: TBitBtn;
    rbOneSided: TRadioButton;
    rbTwoSided: TRadioButton;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    tbDestination: TToolButton;
    tbSource: TToolButton;
    tbWizard: TToolBar;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btPrevClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    StartPageIndex,
    CurPageIndex,
    MaxPageIndex: Integer;
    InStepCode: Boolean;
    CancelledOp: Boolean;

    FrontFiles,
    BackFiles,
    AllFiles: TStringArray;
    OneSided: Boolean;

    procedure GUI_Adjust;
    procedure InternalNext;
    procedure InternalPrev;
    function BeforeNextStep(index: Integer): Boolean;
    procedure NextStep(index: Integer);
    function PrevStep(index: Integer): Boolean;
    function CancelStep: Boolean;
    function EndStep: Boolean;

    function Files_Add(var AFiles: TStringArray; AArray: IDigIt_ROArray): Integer; overload;
    function Files_Add(var AFiles: TStringArray; AFileName: String): Integer; overload;

  public
    //IDigIt_ROArray
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;

    //IDigIt_ProgressCallback
    procedure ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;

    function Execute(out aDataType: TDigItDataType; out aData: Pointer): TModalResult;
  end;

var
  WizardBuildDuplex: TWizardBuildDuplex = nil;

function WizardBuildDuplexExecute(out aDataType: TDigItDataType; out aData: Pointer): Integer;

implementation

{$R *.lfm}

uses DigIt_Form_Main;

procedure TWizardBuildDuplex.GUI_Adjust;
begin
     btPrev.Enabled := (CurPageIndex > StartPageIndex);
     btNext.Enabled :=True;

     if (CurPageIndex = MaxPageIndex)
     then btNext.Caption :=CAPTION_END
     else btNext.Caption :=CAPTION_NEXT;

     PageControl1.ActivePage :=PageControl1.Pages[CurPageIndex-1];
end;

procedure TWizardBuildDuplex.InternalNext;
begin
     if BeforeNextStep(CurPageIndex+1)
     then begin
            if (CurPageIndex = MaxPageIndex)
            then begin
                    if EndStep
                    then ModalResult :=mrOk;
                  end
            else begin
                    inc(CurPageIndex);
                    GUI_Adjust;
                    NextStep(CurPageIndex);
                  end;
           end
    else if (CurPageIndex = (StartPageIndex-1)) //Cancelled Before the First Step Close with Cancel
         then ModalResult :=mrCancel;
end;

procedure TWizardBuildDuplex.InternalPrev;
begin
  if PrevStep(CurPageIndex-1) then
  begin
       dec(CurPageIndex);
       GUI_Adjust;
   end;
end;

function TWizardBuildDuplex.BeforeNextStep(index: Integer): Boolean;
var
   btNext_Enabled,
   btPrev_Enabled :Boolean;

   curData: Pointer;
   curDataType: TDigItDataType;
   res, i,
   oldLength: Integer;
   curImageFile: PChar;
   curArray: IDigIt_ROArray;

   procedure CheckCancelledOp;
   begin
     if CancelledOp then raise Exception_CancelledOp.Create(MSG_ERR_CANCELSTEP);
   end;

begin
  InStepCode := True;

  //Avoid User Can Click during Step
  btNext_Enabled := btNext.Enabled;
  btPrev_Enabled := btPrev.Enabled;
  btNext.Enabled := False;
  btPrev.Enabled := False;

  Result :=True;
  try
     (* Put YOUR Code Here : index is the Page about to be Showed, return False to block *)
     Case index of
     1:try
       except
         on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     2:try
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;

     3:try //Before EndStep if index=(MaxPageIndex+1)
       res:= DigIt_Main.Source^.Inst.Take(takeActTake, curDataType, curData);
       Result:= (res > 0) and (curData <> nil);
       if Result then
       begin
         if (curDataType = diDataType_FileName) then
         begin
           if (res = 1)
           then begin
                  curImageFile:= PChar(curData);
                  Files_Add(FrontFiles, curImageFile);
                  StrDispose(curImageFile);
                end
           else
           if (res > 1)
           then begin
                  curArray:= IDigIt_ROArray(curData);
                  Files_Add(FrontFiles, curArray);
                end;
         end;
        end;
        except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;

     4:try
        res:= DigIt_Main.Source^.Inst.Take(takeActTake, curDataType, curData);
        Result:= (res > 0) and (curData <> nil);
        if Result then
        begin
          if (curDataType = diDataType_FileName) then
          begin
            if (res = 1)
            then begin
                   curImageFile:= PChar(curData);
                   Files_Add(BackFiles, curImageFile);
                   StrDispose(curImageFile);
                 end
            else
            if (res > 1)
            then begin
                   curArray:= IDigIt_ROArray(curData);
                   Files_Add(BackFiles, curArray);
                 end;
          end;
         end;
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;

     end;
  except
     on E:Exception_CancelledOp do
     begin
          (* do something to Cancel the Operation *)
          Case index of
          1:try
            except
            end;
          2:try
            except
            end;
          end;

          btNext_Enabled :=False;
          Dialogs.MessageDlg(E.Message, mtError, [mbOk], 0);
     end;
     on E:Exception do
     begin
          btNext_Enabled :=False;
          Dialogs.MessageDlg(E.Message, mtError, [mbOk], 0);
     end;
  end;


  InStepCode := False;

  //User Can Now Click (return to Previous Button State)
  btNext.Enabled := btNext_Enabled;
  btPrev.Enabled := btPrev_Enabled;
end;

procedure TWizardBuildDuplex.NextStep(index: Integer);
var
   btNext_Enabled,
   btPrev_Enabled :Boolean;

   procedure CheckCancelledOp;
   begin
     if CancelledOp then raise Exception_CancelledOp.Create(MSG_ERR_CANCELSTEP);
   end;

begin
  InStepCode := True;

  //Avoid User Can Click during Step
  btNext_Enabled := btNext.Enabled;
  btPrev_Enabled := btPrev.Enabled;
  btNext.Enabled := False;
  btPrev.Enabled := False;

  try
     (* Put YOUR Code Here : index is the Page Showed *)
     Case index of
     1:try
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     2:try
          OneSided:= rbOneSided.Checked;
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     3:try
        animTurn.Visible:= not(OneSided);
        lbBackSide.Visible:= not(OneSided);
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     end;

  except
     on E:Exception_CancelledOp do
     begin
          (* do something to Cancel the Operation *)
          Case index of
          1:try
            except
            end;
          2:try
            except
            end;
          end;

          btNext_Enabled :=False;
          Dialogs.MessageDlg(E.Message, mtError, [mbOk], 0);
     end;
     on E:Exception do
     begin
          btNext_Enabled :=False;
          Dialogs.MessageDlg(E.Message, mtError, [mbOk], 0);
     end;
  end;

  InStepCode := False;

  //User Can Now Click (return to Previous Button State)
  btNext.Enabled := btNext_Enabled;
  btPrev.Enabled := btPrev_Enabled;
end;

function TWizardBuildDuplex.PrevStep(index: Integer): Boolean;
begin
  Result := (index >= StartPageIndex);
  if Result then
  try
     (* Put YOUR Code Here : index is the Page about to be Showed, return False to block *)
     Case index of
     1:begin
        end;
     2:begin
         if (Length(FrontFiles) > 0) then
         begin
            Case MessageDlg('DigIt', 'Do I cancel the Front Side acquisition I just made?',
                            mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
              mrYes: begin
                       FrontFiles:= nil;
                       DigIt_Main.Source^.Inst.Clear;
                     end;
              mrCancel: Result:= False;
            end;
         end;
        end;
     3:begin
         if (Length(BackFiles) > 0) then
         begin
           Case MessageDlg('DigIt', 'Do I cancel the Back Side acquisition I just made?',
                           mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
             mrYes: begin
                      BackFiles:= nil;
                      DigIt_Main.Source^.Inst.Clear;
                    end;
             mrCancel: Result:= False;
           end;
         end;
        end;
     4:begin
       end;
     end;
  except
     on E:Exception do begin
             Dialogs.MessageDlg('Error :'+#13#10+'['+E.Message+']', mtError, [mbOk], 0);
             Result :=False;   //True if User can Go Back in each case, False to Stop it
     end;
  end;
end;

function TWizardBuildDuplex.CancelStep: Boolean;
begin
  try
     (* Put YOUR Code Here : User Click the Cancel Button, return False to block *)

     Result :=True;
  except
     on E:Exception do begin
          Dialogs.MessageDlg('Error :'+#13#10+
                             '['+E.Message+']', mtError, [mbOk], 0);
          Result :=False;
     end;
  end;
end;

function TWizardBuildDuplex.EndStep: Boolean;
var
   i,
   iOver,
   last,
   lenFront,
   lenBack,
   len,
   lenOver: Integer;

begin
  try
     {$ifdef Test}
     OneSided:= False;
     SetLength(FrontFiles, 7);
     SetLength(BackFiles, 4);
     for i:=0 to Length(FrontFiles)-1 do FrontFiles[i]:=IntToStr(i)+' front';
     for i:=0 to Length(BackFiles)-1 do BackFiles[i]:=IntToStr(Length(BackFiles)-i-1)+' back';
     {$endif}

     AllFiles:= nil;
     lenFront:= Length(FrontFiles);
     lenBack:= Length(BackFiles);
     if (lenFront=0) or (lenBack=0) then raise Exception.Create('No Pages in Front or Back Side');

     if (lenFront <= lenBack)
     then begin
            len:= lenFront;
            lenOver:= lenBack-lenFront;
          end
     else begin
            len:= lenBack;
            lenOver:= lenFront-lenBack;
          end;

     SetLength(AllFiles, (len*2)+lenOver);

     if OneSided
     then begin
            for i:=0 to len-1 do
            begin
              AllFiles[i*2]:= FrontFiles[i];
              AllFiles[(i*2)+1]:= BackFiles[i];
            end;
            last:= (i+1)*2;

            if (lenFront < lenBack)
            then for iOver:=0 to lenOver-1 do AllFiles[last+iOver]:= BackFiles[len+iOver]
            else for iOver:=0 to lenOver-1 do AllFiles[last+iOver]:= FrontFiles[len+iOver];
          end
     else begin
            for i:=0 to len-1 do
            begin
              AllFiles[i*2]:= FrontFiles[i];
              AllFiles[(i*2)+1]:= BackFiles[lenBack-i-1];
            end;
            last:= (i+1)*2;

            if (lenFront < lenBack)
            then for iOver:=1 to lenOver do AllFiles[last+iOver-1]:= BackFiles[lenOver-iOver]
            else for iOver:=0 to lenOver-1 do AllFiles[last+iOver]:= FrontFiles[len+iOver];
          end;

     Result :=True;
  except
     on E:Exception do begin
          Dialogs.MessageDlg('Error :'+#13#10+
                             '['+E.Message+']', mtError, [mbOk], 0);
          Result :=False;
     end;
  end;
end;

function TWizardBuildDuplex.Files_Add(var AFiles: TStringArray; AArray: IDigIt_ROArray): Integer;
var
   oldLength, i: Integer;
   curImageFile: PChar;

begin
  Result:= 0;
  if (AArray <> nil) then
  begin
    //Add files to end of Array AFiles
    oldLength:= Length(AFiles);
    Result:= AArray.GetCount;
    if (Result > 0) then
    begin
      SetLength(AFiles, oldLength+Result);
      for i:=0 to Result-1 do
      begin
        if AArray.Get(i, curImageFile) then
        begin
          AFiles[oldLength+i]:= curImageFile;
          StrDispose(curImageFile);
        end;
      end;
    end;
  end;
end;

function TWizardBuildDuplex.Files_Add(var AFiles: TStringArray; AFileName: String): Integer;
var
   oldLength: Integer;

begin
  Result:= 0;
  if (AFileName <> '') then
  begin
    //Add files to end of Array AFiles
    oldLength:= Length(AFiles);
    SetLength(AFiles, oldLength+1);
    AFiles[oldLength]:= AFileName;
    Result:= 1;
  end;
end;

procedure TWizardBuildDuplex.FormCreate(Sender: TObject);
begin
     StartPageIndex :=1;
     InStepCode :=False;
     CancelledOp :=False;
     FrontFiles:= nil;
     BackFiles:= nil;
     AllFiles:= nil;
end;

procedure TWizardBuildDuplex.FormDestroy(Sender: TObject);
begin
  FrontFiles:= nil;
  BackFiles:= nil;
  AllFiles:= nil;
end;

procedure TWizardBuildDuplex.FormShow(Sender: TObject);
begin
  try
     MaxPageIndex :=PageControl1.PageCount;
     btPrev.Caption :=CAPTION_PREV;
     btNext.Caption :=CAPTION_NEXT;
     btCancel.Caption :=CAPTION_CANCEL;

     CurPageIndex :=StartPageIndex-1;
     InternalNext;
  except
     on E:Exception do
          Dialogs.MessageDlg('Error Opening the Wizard :'+#13#10+
                             '['+E.Message+']', mtError, [mbOk], 0);
  end;
end;

procedure TWizardBuildDuplex.btNextClick(Sender: TObject);
begin
  InternalNext;
end;

procedure TWizardBuildDuplex.btPrevClick(Sender: TObject);
begin
  InternalPrev;
end;

procedure TWizardBuildDuplex.btCancelClick(Sender: TObject);
begin
  if InStepCode
  then begin
         if (CancelledOp = False)
         then CancelledOp := (MessageDlg(Self.Caption, MSG_CONFIRM_CANCELSTEP, mtConfirmation, mbYesNo, 0) = mrYes);
       end
  else if not(CancelStep)
       then begin
            { TODO 10 : Cancel the Modal Click Event }
            end;
end;

function TWizardBuildDuplex.GetCount: DWord; stdcall;
begin
  Result:= Length(AllFiles);
end;

function TWizardBuildDuplex.Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;
begin
  Result:= (aIndex < Length(AllFiles));
  if Result
  then aData:= StrNew(PChar(AllFiles[aIndex]))
  else aData:= nil;
end;

procedure TWizardBuildDuplex.ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;
begin
  CancelledOp:= True;
end;

function TWizardBuildDuplex.Execute(out aDataType: TDigItDataType; out aData: Pointer): TModalResult;
begin
    StartPageIndex :=1;

    {$ifdef Test} EndStep; {$endif}

    if (DigIt_Main.Source <> nil) and (DigIt_Main.Source^.Inst <> nil)
    then Result:= Self.ShowModal
    else Result:= mrCancel;
end;

function WizardBuildDuplexExecute(out aDataType: TDigItDataType; out aData: Pointer): Integer;
begin
  Result:= 0;
  aDataType:= diDataType_FileName;

  if (WizardBuildDuplex = nil)
  then WizardBuildDuplex :=TWizardBuildDuplex.Create(Application);

  if (WizardBuildDuplex <> nil) and
     (WizardBuildDuplex.Execute(aDataType, aData) = mrOk) then
  begin
    Result:= Length(WizardBuildDuplex.AllFiles);
    aData:= WizardBuildDuplex as IDigIt_ROArray;
  end;
end;

end.
