unit Digit_Taker_Twain_SettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Spin, Twain, DelphiTwain;

type
  { TTwainSettingsSource }

  TTwainSettingsSource = class(TForm)
    btCancel: TBitBtn;
    btOrientation: TSpeedButton;
    btRefresh: TBitBtn;
    btOk: TBitBtn;
    cbPaperFeeding: TComboBox;
    cbPaperFeeding1: TComboBox;
    cbPaperSize: TComboBox;
    cbUseNativeUI: TCheckBox;
    edBrightness: TSpinEdit;
    edContrast: TSpinEdit;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    panelUI: TPanel;
    panelButtons: TPanel;
    trBrightness: TTrackBar;
    trContrast: TTrackBar;
  private
    Twain: TCustomDelphiTwain;

  public
     class function Execute(ATwain: TCustomDelphiTwain):Integer;
  end;

var
  TwainSettingsSource: TTwainSettingsSource=nil;

implementation

{$R *.lfm}

{ TTwainSettingsSource }

class function TTwainSettingsSource.Execute(ATwain: TCustomDelphiTwain): Integer;
begin
  if (TwainSettingsSource=nil)
  then TwainSettingsSource :=TTwainSettingsSource.Create(nil);

  with TwainSettingsSource do
  begin
    Twain :=ATwain;

    if (ShowModal=mrOk)
    then Result :=1
    else Result :=-1;
  end;
end;

end.

