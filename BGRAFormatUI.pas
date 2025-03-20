unit BGRAFormatUI;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons,
  FpImage, Laz2_XMLCfg,
  BCPanel, BCTrackbarUpdown,
  BGRABitmapTypes;


type

  { TBGRAFormatUIContainer }

  TBGRAFormatUIContainer = class(TForm)
    ifTiff: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    CompressionQuality: TBCTrackbarUpdown;
    GrayScale: TCheckBox;
    ifJpeg: TBCPanel;
    Label1: TLabel;
    panelButtons: TPanel;
    ProgressiveEncoding: TCheckBox;
    RadioButton1: TRadioButton;
  private
    curFormat: TBGRAImageFormat;
    curWriter: TFPCustomImageWriter;

    function AdjustPanels: Boolean;
    function SelectPanel: TBCPanel;

     procedure GetWriterProperties;

  public
     class function Execute(const AFormat: TBGRAImageFormat;
                            var AWriter: TFPCustomImageWriter): Boolean;

     class function GetUI(const AFormat: TBGRAImageFormat;
                          var AWriter: TFPCustomImageWriter;
                          var APanel: TBCPanel): Boolean;

     procedure SetWriterProperties(var AWriter: TFPCustomImageWriter);
  end;

var
  BGRAFormatUIContainer: TBGRAFormatUIContainer = nil;


implementation

{$R *.lfm}

uses TypInfo;

{ TBGRAFormatUIContainer }

class function TBGRAFormatUIContainer.Execute(const AFormat: TBGRAImageFormat;
                                              var AWriter: TFPCustomImageWriter): Boolean;
begin
  Result:= False;
  if (AFormat = ifUnknown) or
     ((AWriter = nil) and (DefaultBGRAImageWriter[AFormat] = nil))
  then exit;

  if (BGRAFormatUIContainer = nil)
  then BGRAFormatUIContainer :=TBGRAFormatUIContainer.Create(nil);

  if (BGRAFormatUIContainer <> nil) then
  with BGRAFormatUIContainer do
  try
     if (AWriter = nil) then AWriter:= CreateBGRAImageWriter(AFormat, True);

     curFormat:= AFormat;
     curWriter:= AWriter;

     AdjustPanels;
     GetWriterProperties;

     if (ShowModal = mrOk) then
     begin
       SetWriterProperties(AWriter);
       Result:= True;
     end;

  finally
     BGRAFormatUIContainer.Free; BGRAFormatUIContainer:= nil;
  end;
end;

class function TBGRAFormatUIContainer.GetUI(const AFormat: TBGRAImageFormat;
                                            var AWriter: TFPCustomImageWriter;
                                            var APanel: TBCPanel): Boolean;
begin
  Result:= False;
  if (AFormat = ifUnknown) or
     ((AWriter = nil) and (DefaultBGRAImageWriter[AFormat] = nil))
  then exit;

  if (BGRAFormatUIContainer = nil)
  then BGRAFormatUIContainer :=TBGRAFormatUIContainer.Create(nil);

  if (BGRAFormatUIContainer <> nil) then
  with BGRAFormatUIContainer do
  try
     if (AWriter = nil) then AWriter:= CreateBGRAImageWriter(AFormat, True);

     curFormat:= AFormat;
     curWriter:= AWriter;

     APanel:= SelectPanel;
     GetWriterProperties;

  finally
  end;
end;

procedure TBGRAFormatUIContainer.SetWriterProperties(var AWriter: TFPCustomImageWriter);
begin
  //Set Writer Properties from UI
end;

procedure TBGRAFormatUIContainer.GetWriterProperties;
begin
  //Set UI Control Values from Writer Properties
end;

function TBGRAFormatUIContainer.AdjustPanels: Boolean;
var
   pName: String;
   curControl: TControl;
   pFormat: TBCPanel;
   i: Integer;

begin
  Result:= False;

  pName:= GetEnumName(TypeInfo(TBGRAImageFormat), Integer(curFormat));

  for i:=0 to ControlCount-1 do
  begin
    curControl:= Controls[i];

    if (curControl <> nil) and
       (curControl is TBCPanel) then
    begin
      if (CompareText(curControl.Name, pName) = 0) then
      begin
        pFormat:= TBCPanel(curControl);
        Result:= True;
      end;

      curControl.Visible:= False;
    end;
  end;

  if Result then
  begin
    pFormat.Top:= 0; pFormat.Left:= 0;
    {$ifopt D-}
    pFormat.BevelInner:= bvNone;
    pFormat.BevelOuter:= bvNone;
    pFormat.Caption:='';
    {$endif}
    Self.Width:= pFormat.Width;
    Self.Height:= pFormat.Height+panelButtons.Height;

    pFormat.Visible:= True;
  end;
end;

function TBGRAFormatUIContainer.SelectPanel: TBCPanel;
var
   pName: String;
   curControl: TControl;
   i: Integer;

begin
  Result:= nil;

  pName:= GetEnumName(TypeInfo(TBGRAImageFormat), Integer(curFormat));

  for i:=0 to ControlCount-1 do
  begin
    curControl:= Controls[i];

    if (curControl <> nil) and
       (curControl is TBCPanel) and
       (CompareText(curControl.Name, pName) = 0) then
    begin
        Result:= TBCPanel(curControl);
        break;
    end;
  end;

  if (Result <> nil) then
  begin
    Result.Top:= 0; Result.Left:= 0;
    {$ifopt D-}
    Result.BevelInner:= bvNone;
    Result.BevelOuter:= bvNone;
    Result.Caption:='';
    {$endif}
  end;
end;



end.

