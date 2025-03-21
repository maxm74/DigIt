(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   SaveFiles Settings Form                                                  **
*******************************************************************************)

unit DigIt_Destination_SaveFiles_SettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons,
  FpImage,
  BGRABitmapTypes,
  BCPanel, BCLabel;

type
  { TDest_SaveFiles_Settings }

  TDest_SaveFiles_Settings = class(TForm)
    BCLabel9: TBCLabel;
    panelMain: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    cbSaveFormat: TComboBox;
    dirDestination: TDirectoryEdit;
    Label6: TLabel;
    panelButtons: TBCPanel;
    procedure cbSaveFormatChange(Sender: TObject);
    procedure dirDestinationChange(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    SaveFormat,
    UserSaveFormat: TBGRAImageFormat;
    SaveWriter,
    UserSaveWriter: TFPCustomImageWriter;
    SavePath: String;
    panelFormatUI: TBCPanel;
    createdWriter: Boolean;

    procedure BuildSaveFormats;
    procedure AdjustFormatPanel;

  public
    class function Execute(var ASaveFormat: TBGRAImageFormat;
                           var ASaveWriter: TFPCustomImageWriter;
                           var ASavePath: String): Boolean;

  end;

var
  Dest_SaveFiles_Settings: TDest_SaveFiles_Settings = nil;

implementation

{$R *.lfm}

uses Math, BGRAFormatUI;

{ TDest_SaveFiles_Settings }

procedure TDest_SaveFiles_Settings.BuildSaveFormats;
var
  iFormat: TBGRAImageFormat;

begin
  cbSaveFormat.Clear;

  for iFormat:=Low(TBGRAImageFormat) to High(TBGRAImageFormat) do
  begin
    if (iFormat <> ifUnknown) and (DefaultBGRAImageWriter[iFormat] <> nil) then
    begin
      cbSaveFormat.Items.AddObject(BGRAImageFormat[iFormat].TypeName+' ('+SuggestImageExtension(iFormat)+')',
                                   TObject(PTRUInt(iFormat)));
    end;
  end;

  if (cbSaveFormat.Items.Count > 0) then cbSaveFormat.ItemIndex:= 0;
end;

procedure TDest_SaveFiles_Settings.AdjustFormatPanel;
begin
  if (panelFormatUI <> nil)
  then begin
         panelFormatUI.Top:= 90; panelFormatUI.Left:= 100;
         Width:= Max(510, panelFormatUI.Width+110);
         Height:= Max(150, panelFormatUI.Height+100+panelButtons.Height);

         panelFormatUI.Parent:= panelMain;
         panelFormatUI.Visible:= True;
       end
  else begin
         Width:= 510;
         Height:= 150;
       end;
end;

procedure TDest_SaveFiles_Settings.cbSaveFormatChange(Sender: TObject);
begin
  SaveFormat:= TBGRAImageFormat(PTRUInt(cbSaveFormat.Items.Objects[cbSaveFormat.ItemIndex]));

  //I Have created the old Writer, Free It
  if createdWriter then
  begin
    SaveWriter.Free;
    createdWriter:= False;
  end;

  //If the user has selected the initial format assign it, else create a new one
  if (SaveFormat = UserSaveFormat)
  then SaveWriter:= UserSaveWriter
  else begin
         SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
         createdWriter:= True;
       end;

  if (panelFormatUI <> nil) then panelFormatUI.Visible:= False;

  TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);
  AdjustFormatPanel;
end;

procedure TDest_SaveFiles_Settings.dirDestinationChange(Sender: TObject);
begin
  SavePath :=dirDestination.Directory;
end;

procedure TDest_SaveFiles_Settings.FormShow(Sender: TObject);
begin
  AdjustFormatPanel
end;

class function TDest_SaveFiles_Settings.Execute(var ASaveFormat: TBGRAImageFormat;
                                                var ASaveWriter: TFPCustomImageWriter;
                                                var ASavePath: String): Boolean;
begin
  try
     if (Dest_SaveFiles_Settings = nil)
     then Dest_SaveFiles_Settings:= TDest_SaveFiles_Settings.Create(nil);

     with Dest_SaveFiles_Settings do
     begin
       UserSaveFormat:= ASaveFormat;
       UserSaveWriter:= ASaveWriter;
       createdWriter:= False;
       panelFormatUI:= nil;
       BuildSaveFormats;

       dirDestination.Directory:= ASavePath;

       //Select Current Format, if not found Select Jpeg
       cbSaveFormat.ItemIndex:= cbSaveFormat.Items.IndexOfObject(TObject(PTRUInt(ASaveFormat)));
       if (cbSaveFormat.ItemIndex = -1)
       then begin
              SaveFormat:= ifJpeg;
              SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
              createdWriter:= True;

              if (cbSaveFormat.Items.Count > 0)
              then cbSaveFormat.ItemIndex:= cbSaveFormat.Items.IndexOfObject(TObject(PTRUInt(ifJpeg)));
            end
       else begin
              SaveFormat:= ASaveFormat;
              SaveWriter:= ASaveWriter;
            end;

       TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);

       Result:= (ShowModal=mrOk);

       if Result then
       begin
         ASaveFormat:= SaveFormat;

         if (BGRAFormatUIContainer <> nil) and
            (panelFormatUI <> nil) then BGRAFormatUIContainer.SetWriterProperties(SaveWriter);

         ASaveWriter:= SaveWriter;
         ASavePath:= SavePath;
         //Last Char of the Path MUST be the Directory Separator
         if (ASavePath[Length(ASavePath)] <> DirectorySeparator)
         then ASavePath:= ASavePath+DirectorySeparator;
       end
       else if createdWriter then SaveWriter.Free;
     end;


  finally
    if (BGRAFormatUIContainer <> nil) then FreeAndNil(BGRAFormatUIContainer);
    FreeAndNil(Dest_SaveFiles_Settings);
  end;
end;

end.

