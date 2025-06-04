unit DigIt_Form_Profiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, ExtCtrls, Laz2_XMLCfg,
  BCPanel, BCListBox;

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
    XML: TRttiXMLConfig;

    procedure UI_EnableButtons;
    procedure UI_LoadFromXML(const AFilename: String);

    function SaveToXML(const AFilename: String): Boolean;

  public
    class function Execute(const AFilename: String):Boolean;
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
   oldIndex: Integer;
   curNodeStr: String;
   curNode, newNode: TDomNode;
   itemLineNodeList, TempNodeList: TDOMNodeList;

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

  (* FIND A WAY To Exchange Items or Works directly with String File

    itemLineNodeList:= XML.FindNode('Profiles/', True).GetChildNodes;

    curNode:= itemLineNodeList[curItem.Index];
    curNodeStr:= curNode.TextContent;

//    itemLineNodeList[curItem.Index]:= itemLineNodeList[oldIndex];
//    itemLineNodeList[oldIndex]:= curNode;

    XML.SetValue('Profiles/Item'+IntToStr(curItem.Index)+'/', 'Profiles/ItemXXX/');
    XML.SetValue('Profiles/Item'+IntToStr(oldIndex)+'/', 'Profiles/Item'+IntToStr(curItem.Index)+'/');
    XML.SetValue('Profiles/ItemXXX/', 'Profiles/Item'+IntToStr(oldIndex)+'/');
  *)
    XML.Flush;

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

begin
  //Avoid Duplicates
  dup:= False;
  for i:=0 to lvProfiles.Items.Count-1 do
    if (lvProfiles.Items[i].Caption = AValue) then begin dup:= True; break; end;

  if dup
  then AValue:= Item.Caption
  else begin
        curPath:= 'Profiles/Item'+IntToStr(Item.Index)+'/';
        XML.SetValue(curPath+'Name', AValue);
        XML.Flush;
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

procedure TDigIt_Profiles.UI_LoadFromXML(const AFilename: String);
var
   i, iCount: Integer;
   curItem: TListItem;

begin
  try
     XML:= TRttiXMLConfig.Create(AFilename);

     lvProfiles.Clear;

     //Load Profiles
     iCount:= XML.GetValue('Profiles/Count', 0);
     for i:=0 to iCount-1 do
     begin
       curItem:= lvProfiles.Items.Add;
       curItem.Caption:= XML.GetValue('Profiles/Item'+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
       curItem.SubItems.Add(XML.GetValue('Profiles/Item'+IntToStr(i)+'/Source/Name', ''));
     end;

  finally
    UI_EnableButtons;
  end;
end;

class function TDigIt_Profiles.Execute(const AFilename: String): Boolean;
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
          UI_LoadFromXML(AFilename+'.tmp');

          Result:= (ShowModal = mrOk);

          if Result then
          begin
            Result:= CopyFile(AFilename+'.tmp', AFilename);
            DeleteFile(AFilename+'.tmp');
          end;
        end;

     finally
       XML.Free;
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
       ATitleArray[i]:= aXML.GetValue('Profiles/Item'+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
     end;

     Result:= True;

  finally
    aXML.Free;
  end;
end;

function TDigIt_Profiles.SaveToXML(const AFilename: String): Boolean;
var
   i, iCount: Integer;
   curPath: String;

begin
  try
 (*   //Test Only REMOVE IT
    SetLength(Profiles, 5);
    Profiles[0]:= 'Test Profile 0';
    Profiles[1]:= 'Test Profile 1';
    Profiles[2]:= 'Test Caio';
    Profiles[3]:= 'Test Sempronio';
    Profiles[4]:= 'Test Pluto';

     XML:= TRttiXMLConfig.Create(AFilename);

     //Save SourceFiles array
     aXML.DeletePath('Profiles/');
     aXML.SetValue('Profiles/Count', Length(Profiles));
     for i:=0 to Length(Profiles)-1 do
     begin
       curPath:= 'Profiles/Item'+IntToStr(i)+'/';
       aXML.SetValue(curPath+'Name', Profiles[i]);
     end;
     aXML.Free; aXML:= nil;

     //Test Only REMOVE IT
     for i:=0 to Length(Profiles)-1 do
     begin
       curPath:= 'Profiles/Item'+IntToStr(i)+'/';
       SES_SaveSource(nil, False, curPath, Path_Config+File_Profiles);
     end;
   *)
  finally
  //  if (aXML<>nil) then aXML.Free;
  end;
end;

end.

