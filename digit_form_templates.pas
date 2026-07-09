unit DigIt_Form_Templates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, ExtCtrls, StdCtrls,
  BCPanel, BCListBox, BCRoundedImage, BCLabel, MM_UI_EnumFiles,
  DigIt_Session, DigIt_Templates;

type

  { TDigIt_Templates_Form }

  TDigIt_Templates_Form = class(TForm)
    edDescription: TMemo;
    edDescriptionSave: TMemo;
    imgPreview: TBCRoundedImage;
    imgPreviewSave: TBCRoundedImage;
    lbSummary: TBCLabel;
    lbSummarySave: TBCLabel;
    lbTitle: TBCLabel;
    edTitle: TEdit;
    PageControl1: TPageControl;
    tabLoad: TTabSheet;
    tabSave: TTabSheet;
    tvEnum: TMM_UI_EnumFilesINTreeView;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    panelButtons: TBCPanel;
    tvTemplates: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function tvEnumCreateNode(Sender: TMM_UI_EnumFiles;
      ParentComponentNode: TObject; AIndex: Integer; IsDir: Boolean;
      FileName: String; var ACaption: String): Boolean;
    procedure tvEnumNodeClick(Sender: TMM_UI_EnumFiles; ComponentNode: TObject;
      Node: TMM_UI_EnumFilesNode);
  private
    Templates: TDigIt_Templates;

  public
    class function Execute(var aSelectedFileName: String): Boolean; overload;
    class function Execute(aSession: TDigIt_Session; var aFileName: String;
                           var whatSave: TDigIt_Templates_Sections; var aSave: TDigIt_Templates_Data): Boolean; overload;
  end;

var
  DigIt_Templates_Form: TDigIt_Templates_Form=nil;

implementation

{$R *.lfm}

uses DigIt_Types, BGRABitmap;

{ TDigIt_Templates_Form }

procedure TDigIt_Templates_Form.FormCreate(Sender: TObject);
begin
  Templates:= TDigIt_Templates.Create;
end;

procedure TDigIt_Templates_Form.FormDestroy(Sender: TObject);
begin
  Templates.Free;
end;

procedure TDigIt_Templates_Form.FormShow(Sender: TObject);
begin
  tvEnum.BasePaths.Add(Path_Templates);
  tvEnum.BasePaths.Add(Path_Templates_User);
  tvEnum.UpdateOnBasePath(False);
end;

function TDigIt_Templates_Form.tvEnumCreateNode(Sender: TMM_UI_EnumFiles;
                                                ParentComponentNode: TObject; AIndex: Integer; IsDir: Boolean;
                                                FileName: String; var ACaption: String): Boolean;
var
   aData: TDigIt_Templates_Data;

begin
  if not(IsDir) and Templates.Load(FileName, [tempSect_Title], aData) and
     (aData.Title <> '') then ACaption:= aData.Title;

  Result:= True;
end;

procedure TDigIt_Templates_Form.tvEnumNodeClick(Sender: TMM_UI_EnumFiles; ComponentNode: TObject; Node: TMM_UI_EnumFilesNode);
var
   aData: TDigIt_Templates_Data;

begin
  lbTitle.Caption:= aData.Title;

  if Node.IsDir
  then begin
       end
  else try
         aData.Preview:= TBGRABitmap.Create;

         if Templates.Load(Node.FullPath, [tempSect_Description, tempSect_SettingsSummary, tempSect_Preview], aData) then
         with aData do begin
           lbSummary.Caption:= SettingsSummary;
           edDescription.Lines.Text:= Description;
           imgPreview.Bitmap:= Preview;
          end;
       finally
         aData.Preview.Free;
       end;
end;

class function TDigIt_Templates_Form.Execute(var aSelectedFileName: String): Boolean;
begin
  Result:= False;
  try
     if (DigIt_Templates_Form=nil) then DigIt_Templates_Form:= TDigIt_Templates_Form.Create(nil);

     if (DigIt_Templates_Form <> nil) then
     with DigIt_Templates_Form do
     begin
       PageControl1.ActivePage:= tabLoad;
       Result:= (ShowModal = mrOk);

       if Result then aSelectedFileName:= tvEnum.SelectedPath;
     end;

  finally
    if (DigIt_Templates_Form <> nil) then
    begin
      DigIt_Templates_Form.Free;
      DigIt_Templates_Form:= nil;
    end;
  end;
end;

class function TDigIt_Templates_Form.Execute(aSession: TDigIt_Session; var aFileName: String;
                                             var whatSave: TDigIt_Templates_Sections; var aSave: TDigIt_Templates_Data): Boolean;
begin
  Result:= False;
  try
     if (DigIt_Templates_Form=nil) then DigIt_Templates_Form:= TDigIt_Templates_Form.Create(nil);

     if (DigIt_Templates_Form <> nil) then
     with DigIt_Templates_Form do
     begin
       PageControl1.ActivePage:= tabSave;
       Result:= (ShowModal = mrOk);
     end;

  finally
    if (DigIt_Templates_Form <> nil) then
    begin
      DigIt_Templates_Form.Free;
      DigIt_Templates_Form:= nil;
    end;
  end;
end;

end.

