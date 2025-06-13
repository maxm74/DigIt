unit DigIt_Form_Profiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, ExtCtrls, Laz2_XMLCfg,
  BCPanel, BCListBox;

const
  PROF_Item = 'Profiles/Profile_';

type
  { TDigIt_Profiles }

  TDigIt_Profiles = class(TForm)
    BCPanel1: TBCPanel;
    btAddFiles1: TSpeedButton;
    btCancel: TBitBtn;
    btDel: TSpeedButton;
    btDelAll: TSpeedButton;
    btDown: TSpeedButton;
    btEditProfile: TSpeedButton;
    btFirst: TSpeedButton;
    btLast: TSpeedButton;
    btOk: TBitBtn;
    btUp: TSpeedButton;
    imgList: TImageList;
    lvProfiles: TListView;
    panelButtons: TBCPanel;
    procedure btUpDownClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvProfilesEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure lvProfilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    XMLFilename: String;

    procedure UI_EnableButtons;
    procedure UI_LoadFromXML;

  public
    class function Execute(const AFilename: String; var ATitleArray: TStringArray):Boolean;
    class function LoadFromXML(const AFilename: String; var ATitleArray: TStringArray): Boolean;
  end;

var
  DigIt_Profiles: TDigIt_Profiles=nil;

implementation

{$R *.lfm}

uses Laz2_DOM, FileUtil;

{ TDigIt_Profiles }

procedure TDigIt_Profiles.btUpDownClick(Sender: TObject);
var
   curItem: TListItem;
   oldIndex, newIndex: Integer;
   fileStr: RTLString;
   theFile: TStringStream;

begin
  curItem:= lvProfiles.Selected;

  if (curItem <> nil) then
  try
    oldIndex:= curItem.Index;

    lvProfiles.BeginUpdate;

    Case TSpeedButton(Sender).Tag of
      0: lvProfiles.Items.Move(curItem.Index, 0);
      1: lvProfiles.Items.Move(curItem.Index, lvProfiles.Items.Count-1);
      2: lvProfiles.Items.Move(curItem.Index, curItem.Index-1);
      3: lvProfiles.Items.Move(curItem.Index, curItem.Index+1);
    end;

    lvProfiles.EndUpdate;

    try
      newIndex:= curItem.Index;

      theFile:= TStringStream.Create();
      theFile.LoadFromFile(XMLFilename);
      fileStr:= theFile.DataString;
      fileStr:= StringReplace(fileStr, 'Profile_'+IntToStr(newIndex), 'Profile_X', [rfReplaceAll, rfIgnoreCase]);
      fileStr:= StringReplace(fileStr, 'Profile_'+IntToStr(oldIndex), 'Profile_'+IntToStr(newIndex), [rfReplaceAll, rfIgnoreCase]);
      fileStr:= StringReplace(fileStr, 'Profile_X', 'Profile_'+IntToStr(oldIndex), [rfReplaceAll, rfIgnoreCase]);
      theFile.Seek(0, soFromBeginning);
      {$IF SIZEOF(CHAR)=1}
        theFile.WriteAnsiString(fileStr);
      {$ELSE}
        theFile.WriteUnicodeString(fileStr);
      {$ENDIF}
      theFile.SaveToFile(XMLFilename);

    finally
      theFile.Free;
    end;

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_Profiles.FormShow(Sender: TObject);
begin
  if (lvProfiles.Items.Count > 0) then lvProfiles.ItemIndex:= 0;
end;

procedure TDigIt_Profiles.lvProfilesEdited(Sender: TObject; Item: TListItem; var AValue: string);
var
   i: Integer;
   dup: Boolean;
   curPath: String;
   XML: TRttiXMLConfig;

begin
  //Avoid Duplicates
  dup:= False;
  for i:=0 to lvProfiles.Items.Count-1 do
    if (lvProfiles.Items[i].Caption = AValue) then begin dup:= True; break; end;

  if dup
  then AValue:= Item.Caption
  else try
          XML:= TRttiXMLConfig.Create(XMLFilename);
          XML.SetValue(PROF_Item+IntToStr(Item.Index)+'/'+'Name', AValue);
       finally
          XML.Free;
       end;
end;

procedure TDigIt_Profiles.lvProfilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UI_EnableButtons;
end;

procedure TDigIt_Profiles.UI_EnableButtons;
var
   lvCount: Integer;

begin
  lvCount:= lvProfiles.Items.Count;
  btDel.Enabled:= (lvCount > 0);
  btDelAll.Enabled:= (lvCount > 0);
  btUp.Enabled:= (lvCount > 1) and (lvProfiles.Selected <> nil) and (lvProfiles.Selected.Index > 0);
  btDown.Enabled:= (lvCount > 1) and (lvProfiles.Selected <> nil) and (lvProfiles.Selected.Index < lvCount-1);
  btFirst.Enabled:= btUp.Enabled;
  btLast.Enabled:= btDown.Enabled;
end;

procedure TDigIt_Profiles.UI_LoadFromXML;
var
   i, iCount: Integer;
   curItem: TListItem;
   XML: TRttiXMLConfig;

begin
  try
     XML:= TRttiXMLConfig.Create(XMLFilename);

     lvProfiles.Clear;

     //Load Profiles
     iCount:= XML.GetValue('Profiles/Count', 0);
     for i:=0 to iCount-1 do
     begin
       curItem:= lvProfiles.Items.Add;
       curItem.Caption:= XML.GetValue(PROF_Item+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
       curItem.SubItems.Add(XML.GetValue(PROF_Item+IntToStr(i)+'/Source/Name', ''));
     end;

  finally
    XML.Free;
    UI_EnableButtons;
  end;
end;

class function TDigIt_Profiles.Execute(const AFilename: String; var ATitleArray: TStringArray): Boolean;
var
   i: Integer;

begin
  Result:= False;
  try
     if (DigIt_Profiles=nil)
     then DigIt_Profiles :=TDigIt_Profiles.Create(nil);

     if (DigIt_Profiles <> nil) then
     with DigIt_Profiles do
     try
        if CopyFile(AFilename, AFilename+'.tmp') then
        begin
          XMLFilename:= AFilename+'.tmp';
          UI_LoadFromXML;

          Result:= (ShowModal = mrOk);

          if Result then
          begin
            Result:= CopyFile(AFilename+'.tmp', AFilename);
            DeleteFile(AFilename+'.tmp');
            Result:= LoadFromXML(AFilename, ATitleArray);
          end;
        end;

     finally
     end;

  finally
    DigIt_Profiles.Free; DigIt_Profiles:= nil;
  end;
end;

class function TDigIt_Profiles.LoadFromXML(const AFilename: String; var ATitleArray: TStringArray): Boolean;
var
   aXML: TRttiXMLConfig;
   i, iCount: Integer;

begin
  Result:= False;
  try
     aXML:= TRttiXMLConfig.Create(AFilename);

     //Load Profiles
     ATitleArray:= nil;
     iCount:= aXML.GetValue('Profiles/Count', 0);
     SetLength(ATitleArray, iCount);
     for i:=0 to iCount-1 do
     begin
       ATitleArray[i]:= aXML.GetValue(PROF_Item+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
     end;

     Result:= True;

  finally
    aXML.Free;
  end;
end;

end.

