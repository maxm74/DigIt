(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
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
  BGRAAnimatedGif,
  DigIt_Types, Digit_Bridge_Intf, Digit_Bridge_Impl;

resourcestring
     CAPTION_PREV ='&Back';
     CAPTION_NEXT ='&Next';
     CAPTION_END ='&End';
     CAPTION_CANCEL ='&Cancel';
     MSG_CONFIRM_CANCELSTEP ='Cancel the Operation?';
     MSG_ERR_CANCELSTEP ='Operation aborted by User';

     rsCancelFront = 'Do I cancel the Front Side acquisition I just made?';
     rsCancelBack = 'Do I cancel the Back Side acquisition I just made?';
     rsNoPagesFront = 'No Pages in Front Side';
     rsNoPagesBack = 'No Pages in Back Side';

type
  { TWizardBuildDuplex }
  Exception_CancelledOp = class (Exception);

  TWizardBuildDuplex = class(TForm)
    cbFirstPageBlank: TCheckBox;
    cbLastPageBlank: TCheckBox;
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
    tbProfiles: TToolButton;
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
    AllFiles: TSourceFileArray;
    OneSided: Boolean;
    curtakeAction: DigIt_Source_TakeAction;

    procedure GUI_Adjust;
    procedure InternalNext;
    procedure InternalPrev;
    function BeforeNextStep(index: Integer): Boolean;
    procedure NextStep(index: Integer);
    function PrevStep(index: Integer): Boolean;
    function CancelStep: Boolean;
    function EndStep: Boolean;

  public
    class function Execute(takeAction: DigIt_Source_TakeAction;
                           var AFiles: TSourceFileArray; AStartIndex: Integer): DWord;
  end;

var
  WizardBuildDuplex: TWizardBuildDuplex = nil;

implementation

{$R *.lfm}

uses DigIt_Sources;

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
   res: Integer;

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
     Case index of
     1:try
       except
         on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     2:try
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;

     3:try //Front
          res:= Sources.Take(curtakeAction, FrontFiles, 0);
          Result:= (res > 0) and (FrontFiles <> nil);
        except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;

     4:try //Back
          animTurn.Visible:= False;

          res:= Sources.Take(curtakeAction, BackFiles, 0);
          Result:= (res > 0) and (BackFiles <> nil);
          if not(Result) then animTurn.Visible:= not(OneSided);
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
     Case index of
     1:try
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     2:try
          OneSided:= rbOneSided.Checked;
          cbLastPageBlank.Visible:= OneSided;
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
            Case MessageDlg('DigIt', rsCancelFront,
                            mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
              mrYes: begin
                       FrontFiles:= nil;
                       Sources.Selected^.Inst.Clear;
                     end;
              mrCancel: Result:= False;
            end;
         end;
        end;
     3:begin
         if (Length(BackFiles) > 0) then
         begin
           Case MessageDlg('DigIt', rsCancelBack,
                           mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
             mrYes: begin
                      BackFiles:= nil;
                      Sources.Selected^.Inst.Clear;
                    end;
             mrCancel: Result:= False;
           end;
         end;
         if Result then
         begin
           animTurn.Visible:= not(OneSided);
           lbBackSide.Visible:= not(OneSided);
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
   iAll,
   lenFront,
   lenBack,
   len,
   lenOver: Integer;

begin
  try
     {$ifdef Test}
     OneSided:= False;
     SetLength(FrontFiles, 4);
     SetLength(BackFiles, 7);
     for i:=0 to Length(FrontFiles)-1 do FrontFiles[i]:=IntToStr(i)+' front';
     for i:=0 to Length(BackFiles)-1 do BackFiles[i]:=IntToStr(Length(BackFiles)-i-1)+' back';
     {$endif}

     AllFiles:= nil;
     lenFront:= Length(FrontFiles);
     lenBack:= Length(BackFiles);

     //Delete the Last Page from FrontFiles, no need to physically delete it we always use lenFront not Length(FrontFiles)
     if OneSided and cbLastPageBlank.Checked then dec(lenFront);

     //Delete the First Page from BackFiles moving elements back by 1
     if cbFirstPageBlank.Checked then
     begin
       for i:=1 to lenBack-1 do BackFiles[i-1]:= BackFiles[i];
       dec(lenBack);
     end;

     if (lenFront <= 0) then raise Exception.Create(rsNoPagesFront);
     if (lenBack <= 0) then raise Exception.Create(rsNoPagesBack);

     //Take the smallest length in len and the difference with the largest in lenOver
     if (lenFront <= lenBack)
     then begin
            len:= lenFront;
            lenOver:= lenBack-lenFront;
          end
     else begin
            len:= lenBack;
            lenOver:= lenFront-lenBack;
          end;

     //Allocates space for two pages at a time plus any extra pages
     SetLength(AllFiles, (len*2)+lenOver);

     if OneSided
     then begin
            //Take Front and Back in the same order
            iAll:= 0;
            for i:=0 to len-1 do
            begin
              AllFiles[iAll]:= FrontFiles[i];
              inc(iAll);
              AllFiles[iAll]:= BackFiles[i];
              inc(iAll);
            end;

            //Take Extra Pages
            if (lenFront < lenBack)
            then for i:=0 to lenOver-1 do AllFiles[iAll+i]:= BackFiles[len+i]
            else for i:=0 to lenOver-1 do AllFiles[iAll+i]:= FrontFiles[len+i];
          end
     else begin
            //Take Front in the Same order, Back in reverse order
            iAll:= 0;
            for i:=0 to len-1 do
            begin
              AllFiles[iAll]:= FrontFiles[i];
              inc(iAll);
              AllFiles[iAll]:= BackFiles[lenBack-i-1];
              inc(iAll);
            end;

            //Take Extra Pages, Front in the Same order, Back in reverse order
            if (lenFront < lenBack)
            then for i:=1 to lenOver do AllFiles[iAll+i-1]:= BackFiles[lenOver-i]
            else for i:=0 to lenOver-1 do AllFiles[iAll+i]:= FrontFiles[len+i];
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

class function TWizardBuildDuplex.Execute(takeAction: DigIt_Source_TakeAction;
                                          var AFiles: TSourceFileArray; AStartIndex: Integer): DWord;
var
   oldLength, i: Integer;

begin
  Result:= 0;

  if (Sources.Selected <> nil) and (Sources.Selected^.Inst <> nil) then
  try
    if (WizardBuildDuplex = nil)
    then WizardBuildDuplex :=TWizardBuildDuplex.Create(Application);

    if (WizardBuildDuplex <> nil) then
    with WizardBuildDuplex do
    begin
      StartPageIndex:= 1;
      curTakeAction:= takeAction;

      {$ifdef Test} EndStep; {$endif}

      if (ShowModal = mrOk) then
      begin
        Result:= Length(AllFiles);

        if (AFiles <> nil) and (AStartIndex >= 0)
        then begin
               oldLength:= Length(AFiles);
               if (AStartIndex > oldLength) then AStartIndex:= oldLength;

               //Add more space, if needed, to end of Array
               if (AStartIndex+Result > oldLength) then SetLength(AFiles, AStartIndex+Result);

               for i:=0 to Result-1 do
               begin
                 AFiles[AStartIndex+i].fName:= AllFiles[i].fName;
               end;
             end
        else AFiles:= AllFiles;
      end;
    end;

  finally
    WizardBuildDuplex.Free; WizardBuildDuplex:= nil;
  end;
end;

end.
