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
  BGRABitmapTypes, BCPanel, BCLabel;

type
  { TDest_SaveFiles_Settings }

  TDest_SaveFiles_Settings = class(TForm)
    BCLabel9: TBCLabel;
    BCPanel1: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    cbSaveFormat: TComboBox;
    dirDestination: TDirectoryEdit;
    Label6: TLabel;
    panelButtons: TBCPanel;
    procedure cbSaveFormatChange(Sender: TObject);
    procedure dirDestinationChange(Sender: TObject);

  private
    SaveFormat: TBGRAImageFormat;
    SavePath: String;

    procedure BuildSaveFormats;

  public
    class function Execute(var ASaveFormat: TBGRAImageFormat; var ASavePath: String): Boolean;

  end;

var
  Dest_SaveFiles_Settings: TDest_SaveFiles_Settings = nil;

implementation

{$R *.lfm}

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

procedure TDest_SaveFiles_Settings.cbSaveFormatChange(Sender: TObject);
begin
  SaveFormat:= TBGRAImageFormat(PTRUInt(cbSaveFormat.Items.Objects[cbSaveFormat.ItemIndex]));

  { #to-do 5 -oMaxM : Select Page with the Format Options }
end;

procedure TDest_SaveFiles_Settings.dirDestinationChange(Sender: TObject);
begin
  SavePath :=dirDestination.Directory;
end;

class function TDest_SaveFiles_Settings.Execute(var ASaveFormat: TBGRAImageFormat; var ASavePath: String): Boolean;
begin
  try
     Dest_SaveFiles_Settings:= TDest_SaveFiles_Settings.Create(nil);

     with Dest_SaveFiles_Settings do
     begin
       BuildSaveFormats;

       dirDestination.Directory:=ASavePath;

       //Select Current Format, if not found Select Jpeg
       cbSaveFormat.ItemIndex:= cbSaveFormat.Items.IndexOfObject(TObject(PTRUInt(ASaveFormat)));
       if (cbSaveFormat.ItemIndex = -1) and (cbSaveFormat.Items.Count > 0)
       then cbSaveFormat.ItemIndex:= cbSaveFormat.Items.IndexOfObject(TObject(PTRUInt(ifJpeg)));

       Result:= (ShowModal=mrOk);

       if Result then
       begin
         ASaveFormat:= SaveFormat;
         ASavePath:= SavePath;
         //Last Char of the Path MUST be the Directory Separator
         if (ASavePath[Length(ASavePath)] <> DirectorySeparator)
         then ASavePath:= ASavePath+DirectorySeparator;
       end;
     end;


  finally
    FreeAndNil(Dest_SaveFiles_Settings);
  end;
end;

end.

