unit DigIt_Dest_SaveFiles_SettingsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons, BCPanel, BCLabel;

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
    SaveExt,
    SavePath: String;

    function UI_FindSaveFormat(AExt: String):Integer;
    procedure BuildSaveFormats;

  public
    class function Execute(var ASaveExt, ASavePath: String): Boolean;

  end;

var
  Dest_SaveFiles_Settings: TDest_SaveFiles_Settings = nil;

implementation

{$R *.lfm}

uses fpimage;

{ TDest_SaveFiles_Settings }

procedure TDest_SaveFiles_Settings.BuildSaveFormats;
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

function TDest_SaveFiles_Settings.UI_FindSaveFormat(AExt: String): Integer;
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

procedure TDest_SaveFiles_Settings.cbSaveFormatChange(Sender: TObject);
var
   i:Integer;

begin
  SaveExt:=ImageHandlers.Extensions[cbSaveFormat.Items[cbSaveFormat.ItemIndex]];
  i :=Pos(';', SaveExt);
  if (i>0) then SaveExt :=Copy(SaveExt, 1, i-1);
end;

procedure TDest_SaveFiles_Settings.dirDestinationChange(Sender: TObject);
begin
  SavePath :=dirDestination.Directory;
  //UI_FillTaker;
end;

class function TDest_SaveFiles_Settings.Execute(var ASaveExt, ASavePath: String): Boolean;
begin
  if (Dest_SaveFiles_Settings=nil)
  then Dest_SaveFiles_Settings :=TDest_SaveFiles_Settings.Create(nil);

  with Dest_SaveFiles_Settings do
  begin
    BuildSaveFormats;

    dirDestination.Directory:=ASavePath;
    cbSaveFormat.ItemIndex:=UI_FindSaveFormat(ASaveExt);

    Result:= (ShowModal=mrOk);

    if Result then
    begin
      ASaveExt:= SaveExt;
      ASavePath:= SavePath;
    end;
  end;

end;

end.

