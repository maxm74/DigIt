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

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls,
  Digit_Bridge_Intf, Digit_Bridge_Impl;

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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    PageControl1: TPageControl;
    Panel2: TPanel;
    btNext: TBitBtn;
    btPrev: TBitBtn;
    btCancel: TBitBtn;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;

    procedure FormCreate(Sender: TObject);
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

    DownloadedFiles: TStringArray;

    procedure GUI_Adjust;
    procedure InternalNext;
    procedure InternalPrev;
    function BeforeNextStep(index: Integer): Boolean;
    procedure NextStep(index: Integer);
    function PrevStep(index: Integer): Boolean;
    function CancelStep: Boolean;
    function EndStep: Boolean;
  public
    //IDigIt_ROArray
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;

    //IDigIt_ProgressCallback
    procedure ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;

     function Execute(Source: PSourceInfo; out aDataType: TDigItDataType; out aData: Pointer): TModalResult;
  end;

var
  WizardBuildDuplex: TWizardBuildDuplex = nil;

function WizardBuildDuplexExecute(Source: PSourceInfo; out aDataType: TDigItDataType; out aData: Pointer): Integer;

implementation

{$R *.lfm}

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
begin
  Result :=True;
  try
     (* Put YOUR Code Here : index is the Page about to be Showed, return False to block *)
     Case index of
     1:begin
        end;
     2:begin
        end;
     3:begin //Before EndStep if index=(MaxPageIndex+1)
        end;
     end;
  except
     on E:Exception do begin
             Dialogs.MessageDlg('Error :'+#13#10+'['+E.Message+']', mtError, [mbOk], 0);
             Result :=False;
     end;
  end;
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
          (* Use CheckCancelledOp in various point of you code *)
       except
          on E:Exception do raise Exception.Create('Error on Step:'+#13#10+'['+E.Message+']');
       end;
     2:try
          (* Use CheckCancelledOp in various point of you code *)
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
begin
  try
     (* Put YOUR Code Here : User Click "End", return False to block *)

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
  Result:= Length(DownloadedFiles);
end;

function TWizardBuildDuplex.Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;
begin
  Result:= (aIndex < Length(DownloadedFiles));
  if Result
  then aData:= StrNew(PChar(DownloadedFiles[aIndex]))
  else aData:= nil;
end;

procedure TWizardBuildDuplex.ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;
begin
  CancelledOp:= True;
end;

function TWizardBuildDuplex.Execute(Source: PSourceInfo; out aDataType: TDigItDataType; out aData: Pointer): TModalResult;
begin
    StartPageIndex :=1;

    (* Put YOUR Code Here : GUI Initializations *)
    DownloadedFiles:= nil;

    Result :=Self.ShowModal

    (* Put YOUR Code Here : After the Wizard ends (mrOk or mrCancel) *)
end;

function WizardBuildDuplexExecute(Source: PSourceInfo; out aDataType: TDigItDataType; out aData: Pointer): Integer;
begin
  Result:= 0;
  aDataType:= diDataType_FileName;

  if (WizardBuildDuplex = nil)
  then WizardBuildDuplex :=TWizardBuildDuplex.Create(Application);

  if (WizardBuildDuplex <> nil) and
     (WizardBuildDuplex.Execute(Source, aDataType, aData) = mrOk) then
  begin
    Result:= Length(WizardBuildDuplex.DownloadedFiles);
    aData:= WizardBuildDuplex as IDigIt_ROArray;
  end;
end;

end.
