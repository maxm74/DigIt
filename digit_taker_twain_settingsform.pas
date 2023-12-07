unit Digit_Taker_Twain_SettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, Spin, Twain, DelphiTwain, Digit_Taker_Twain_Types;

type
  { TTwainSettingsSource }

  TTwainSettingsSource = class(TForm)
    btCancel: TBitBtn;
    btOrientation: TSpeedButton;
    btRefresh: TBitBtn;
    btOk: TBitBtn;
    cbPaperFeeding: TComboBox;
    cbResolution: TComboBox;
    cbPaperSize: TComboBox;
    cbBitDepth: TComboBox;
    cbUseNativeUI: TCheckBox;
    edBrightness: TSpinEdit;
    edContrast: TSpinEdit;
    imgList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    panelUI: TPanel;
    panelButtons: TPanel;
    trBrightness: TTrackBar;
    trContrast: TTrackBar;
    procedure edBrightnessChange(Sender: TObject);
    procedure edContrastChange(Sender: TObject);
    procedure trBrightnessChange(Sender: TObject);
    procedure trContrastChange(Sender: TObject);
  private
    Twain: TCustomDelphiTwain;
    SelectedSourceIndex:Integer;

  public
     class function Execute(ATwain: TCustomDelphiTwain;
                            ASelectedSourceIndex:Integer;
                            var AParams:TDigIt_Taker_TwainParams): Boolean;
  end;

var
  TwainSettingsSource: TTwainSettingsSource=nil;

implementation

{$R *.lfm}

{ TTwainSettingsSource }

procedure TTwainSettingsSource.trBrightnessChange(Sender: TObject);
begin
  edBrightness.Value:=trBrightness.Position;
end;

procedure TTwainSettingsSource.edBrightnessChange(Sender: TObject);
begin
  trBrightness.Position:=edBrightness.Value;
end;

procedure TTwainSettingsSource.edContrastChange(Sender: TObject);
begin
  trContrast.Position:=edContrast.Value;
end;

procedure TTwainSettingsSource.trContrastChange(Sender: TObject);
begin
  edContrast.Value:=trContrast.Position;
end;

class function TTwainSettingsSource.Execute(ATwain: TCustomDelphiTwain;
                                            ASelectedSourceIndex:Integer;
                                            var AParams:TDigIt_Taker_TwainParams): Boolean;
var
  ItemType: TW_UINT16;
  List: TStringArray;
  Current, Default: Integer;
  tCurrent, tDefault, tList: TTwainPaperSize;
  capRet:TCapabilityRet;
  paperFeed:TTwainPaperFeedingSet;
  paperList:TTwainPaperSizeSet;
  resolutionList:TTwainResolution;
  resolutionCurrent:Extended;
  i, cbSelected: Integer;
  t:TTwainOrientation;
  test:Boolean;
  bitArray:TArrayInteger;

begin
  if (TwainSettingsSource=nil)
  then TwainSettingsSource :=TTwainSettingsSource.Create(nil);

  with TwainSettingsSource do
  begin
    Twain :=ATwain;

    //Fill UI getting values from scanner
    if AParams.IPC_Scanner
    then begin
           { #note 10 -oMaxM : Implement the Capabilities in IPC Server or switch the Project to 32bit? }
         end
    else begin
           //capRet :=Twain.SelectedSource.GetAutofeed(test);
           //capRet :=Twain.SelectedSource.SetAutoFeed(False);

           //Twain.SelectedSource.GetOrientation(t);

           cbPaperFeeding.Clear;
           paperFeed :=Twain.SelectedSource.GetPaperFeeding;
           if (pfFlatbed in paperFeed) then cbPaperFeeding.Items.AddObject('Flatbed', TObject(PtrUInt(pfFlatbed)));
           if (pfFeeder in paperFeed) then cbPaperFeeding.Items.AddObject('Feeder', TObject(PtrUInt(pfFeeder)));
           cbPaperFeeding.ItemIndex:=cbPaperFeeding.Items.IndexOfObject(TObject(PtrUInt(AParams.PaperFeed)));

           //Get List of Papers
           cbPaperSize.Clear;
           cbSelected :=0;
           Twain.SelectedSource.GetPaperSizeSet(paperList, tCurrent, tDefault);
           cbPaperSize.Items.AddObject('Full Scanner size', TObject(PtrUInt(tpsNONE)));
           for tList in paperList do
           begin
             if (tList<>tpsNONE) and (tList<>tpsMAXSIZE)
             then cbPaperSize.Items.AddObject(PaperSizesTwain[tList].name+
                      ' ('+FloatToStrF(PaperSizesTwain[tList].w, ffFixed, 15, 2)+' x '+
                      FloatToStrF(PaperSizesTwain[tList].h, ffFixed, 15, 2)+')',
                      TObject(PtrUInt(tList)));

             //if (tList=tCurrent) then cbSelected :=cbPaperSize.Items.Count-1;
             if (tList=AParams.PaperSize) then cbSelected :=cbPaperSize.Items.Count-1;
           end;
           cbPaperSize.ItemIndex:=cbSelected;

           //Get List of Bit Depth
           cbBitDepth.Clear;
           capRet :=Twain.SelectedSource.GetIBitDepth(Current, Default, bitArray);
           if capRet=crSuccess then
           begin
             for i:=Low(bitArray) to High(bitArray) do
             begin
               cbBitDepth.Items.AddObject(IntToStr(bitArray[i]), TObject(PtrUInt(i)));
               //if (bitArray[i]=Current) then cbSelected :=cbBitDepth.Items.Count-1;
               if (bitArray[i]=AParams.BitDepth) then cbSelected :=cbBitDepth.Items.Count-1;
             end;
             cbBitDepth.ItemIndex:=cbSelected;
           end;

           //Get List of Resolution (Y Resolution=X Resolution)
           cbResolution.Clear;
           cbSelected :=0;
           capRet :=Twain.SelectedSource.GetIXResolution(resolutionCurrent, resolutionList);
           if capRet=crSuccess then
           begin
             for i:=Low(resolutionList) to High(resolutionList) do
             begin
               cbResolution.Items.AddObject(FloatToStr(resolutionList[i]), TObject(PtrUInt(i)));

               //if (resolutionList[i]=resolutionCurrent) then cbSelected :=cbResolution.Items.Count-1;
               if (resolutionList[i]=AParams.Resolution) then cbSelected :=cbResolution.Items.Count-1;
             end;
             cbResolution.ItemIndex:=cbSelected;
           end;
    end;

    { #todo -oMaxM 2 : is an Extended or an Integer? }
    trContrast.Position:=Trunc(AParams.Contrast);
    edContrast.Value:=Trunc(AParams.Contrast);
    trBrightness.Position:=Trunc(AParams.Brightness);
    edBrightness.Value:=Trunc(AParams.Brightness);

    Result := (ShowModal=mrOk);

    if Result then
    begin
      //Fill AParams with new values
      if (cbPaperFeeding.ItemIndex>-1)
      then AParams.PaperFeed:=TTwainPaperFeeding(PtrUInt(cbPaperFeeding.Items.Objects[cbPaperFeeding.ItemIndex]));
      { #todo -oMaxM : else Predefined Value }

      if (cbPaperSize.ItemIndex>-1)
      then AParams.PaperSize:=TTwainPaperSize(PtrUInt(cbPaperSize.Items.Objects[cbPaperSize.ItemIndex]));

      if (cbBitDepth.ItemIndex>-1)
      then AParams.BitDepth:=bitArray[PtrUInt(cbBitDepth.Items.Objects[cbBitDepth.ItemIndex])];

      if (cbResolution.ItemIndex>-1)
      then AParams.Resolution:=resolutionList[PtrUInt(cbResolution.Items.Objects[cbResolution.ItemIndex])];

      AParams.Contrast:=edContrast.Value;
      AParams.Brightness:=edBrightness.Value;
    end;
  end;
end;

end.

