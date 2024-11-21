(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Main Form                                                                **
*******************************************************************************)

unit DigIt_Form_Progress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Buttons,
  Digit_Bridge_Intf;

type

  { TDigIt_Progress }

  TDigIt_Progress = class(TForm, IDigIt_Progress)
    btCancel: TBitBtn;
    capTotal: TLabel;
    labTotal: TLabel;
    capCurrent: TLabel;
    panelCancel: TPanel;
    panelTotal: TPanel;
    panelCurrent: TPanel;
    progressTotal: TProgressBar;
    progressCurrent: TProgressBar;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    curEventCallBack: IDigIt_ProgressCallback;

  public
    procedure SetTotalVisible(AVisible: Boolean); stdcall;
    procedure SetTotalLabel(const ALabel: PChar); stdcall;
    procedure SetTotal(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
    procedure SetTotalCaption(const ACaption: PChar); stdcall;
    procedure SetTotalValue(AValue: Integer); stdcall;

    procedure SetCurrentVisible(AVisible: Boolean); stdcall;
    procedure SetCurrent(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
    procedure SetCurrentCaption(const ACaption: PChar); stdcall;
    procedure SetCurrentValue(AValue: Integer); stdcall;

    procedure SetEventCallBack(const AEventCallBack: IDigIt_ProgressCallback); stdcall;

    procedure Show(const ACaption: PChar); stdcall;
    procedure Hide; stdcall;
  end;

var
  DigIt_Progress: TDigIt_Progress;

implementation

{$R *.lfm}

{ TDigIt_Progress }

procedure TDigIt_Progress.FormCreate(Sender: TObject);
begin
  curEventCallBack:= nil;
end;

procedure TDigIt_Progress.btCancelClick(Sender: TObject);
begin
  if (curEventCallBack <> nil)
  then curEventCallBack.ProgressCancelClick(progressTotal.Position, progressCurrent.Position);
end;

procedure TDigIt_Progress.SetTotalVisible(AVisible: Boolean); stdcall;
begin
  panelTotal.Visible:= AVisible;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetTotalLabel(const ALabel: PChar); stdcall;
begin
  labTotal.Caption:= ALabel;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetTotal(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
begin
  if isMarquee
  then begin
         progressTotal.Smooth:= False;
         progressTotal.Style:= pbstMarquee;
       end
  else begin
         progressTotal.Smooth:= True;
         progressTotal.Style:= pbstNormal;
       end;

  progressTotal.Min:= AMin;
  progressTotal.Max:= AMax;
  progressTotal.Position:= AValue;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetTotalCaption(const ACaption: PChar); stdcall;
begin
  capTotal.Caption:= ACaption;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetTotalValue(AValue: Integer); stdcall;
begin
  progressTotal.Position:= AValue;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetCurrentVisible(AVisible: Boolean); stdcall;
begin
  panelCurrent.Visible:= AVisible;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetCurrent(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
begin
  if isMarquee
  then begin
         progressCurrent.Smooth:= False;
         progressCurrent.Style:= pbstMarquee;
       end
  else begin
         progressCurrent.Smooth:= True;
         progressCurrent.Style:= pbstNormal;
       end;

  progressCurrent.Min:= AMin;
  progressCurrent.Max:= AMax;
  progressCurrent.Position:= AValue;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetCurrentCaption(const ACaption: PChar); stdcall;
begin
  capCurrent.Caption:= ACaption;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetCurrentValue(AValue: Integer); stdcall;
begin
  progressCurrent.Position:= AValue;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.SetEventCallBack(const AEventCallBack: IDigIt_ProgressCallback); stdcall;
begin
  curEventCallBack:= AEventCallBack;
end;

procedure TDigIt_Progress.Show(const ACaption: PChar); stdcall;
begin
  Caption:= ACaption;
  Visible:= True;
  Application.ProcessMessages;
end;

procedure TDigIt_Progress.Hide; stdcall;
begin
  Visible:= False;
  Application.ProcessMessages;
end;

end.

