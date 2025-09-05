(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Profiles Editing Form                                                    **
*******************************************************************************)

unit DigIt_Form_Profiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, Buttons, ComCtrls, ExtCtrls, Dialogs,
  Menus, Laz2_XMLCfg, BCPanel, BCListBox,
  Digit_Bridge_Intf, Digit_Bridge_Impl, DigIt_Sources, DigIt_Profiles;

resourcestring
  rsProfiles_AddCurrent = 'Add Profile...';
  rsProfiles_Del = 'Delete Profile?'#13#10'%s';
  rsProfiles_DelAll = 'Delete All Profiles?';
  rsProfiles_Title = 'Profile Title :';
  rsProfiles_InvalidTitle_Exists = 'Profile Title already exists';
  rsProfiles_SourceErr = 'Cannot Open Source'#13#10'%s';

type
  { TDigIt_Profiles_Form }

  TDigIt_Profiles_Form = class(TForm)
    BCPanel1: TBCPanel;
    btAddProfile: TSpeedButton;
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
    menuSources: TPopupMenu;
    panelButtons: TBCPanel;
    procedure btAddProfileClick(Sender: TObject);
    procedure btDelAllClick(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure btEditProfileClick(Sender: TObject);
    procedure btUpDownClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvProfilesEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure lvProfilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    newProfiles: TDigIt_Profiles;

    procedure UI_EnableButtons;
    procedure UI_LoadFromXML;
    procedure UI_SourceMenuClick(Sender: TObject);


  public
    class function Execute(const AFilename: String): Boolean;

    class function Add(const ASource: PSourceInfo; const AParams: IDigIt_Params;
                       const ASourceName, AFilename: String; const ATitleArray: TStringArray): String; overload;
    function Add(const ASource: PSourceInfo; const AParams: IDigIt_Params; const ASourceName: String): String; overload;
  end;

var
  DigIt_Profiles_Form: TDigIt_Profiles_Form=nil;

implementation

{$R *.lfm}

uses Laz2_DOM, FileUtil,
     MM_StrUtils, MM_Form_EditText,
     DigIt_Utils;

{ TDigIt_Profiles_Form }

procedure TDigIt_Profiles_Form.btUpDownClick(Sender: TObject);
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

    Case TSpeedButton(Sender).Tag of
      0: newIndex:= 0;
      1: newIndex:= lvProfiles.Items.Count-1;
      2: newIndex:= curItem.Index-1;
      3: newIndex:= curItem.Index+1;
    end;

    if newProfiles.Move(oldIndex, newIndex) then
    begin
      lvProfiles.BeginUpdate;
      lvProfiles.Items.Move(oldIndex, newIndex);
      lvProfiles.EndUpdate;
    end;

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_Profiles_Form.btDelAllClick(Sender: TObject);
begin
  try
     if (MessageDlg('DigIt', rsProfiles_DelAll, mtConfirmation, [mbYes, mbNo], 0) = mrYes) and
         newProfiles.Clear(True)
     then lvProfiles.Clear;

  finally
     UI_EnableButtons;
  end;
end;

procedure TDigIt_Profiles_Form.btDelClick(Sender: TObject);
var
   curItem: TListItem;

begin
  try
     curItem:= lvProfiles.Selected;
     if (curItem <> nil) and
        (MessageDlg('DigIt', Format(rsProfiles_Del, [curItem.Caption]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) and
        newProfiles.Del(curItem.Index)
     then lvProfiles.Items.Delete(curItem.Index);

  finally
     UI_EnableButtons;
  end;
end;

procedure TDigIt_Profiles_Form.btEditProfileClick(Sender: TObject);
var
   aXML: TRttiXMLConfig;
   curItem: TListItem;
   ASource: PSourceInfo=nil;
   AParams: IDigIt_Params=nil;
   curName,
   curXMPath: String;
   connected: Boolean;

begin
  curItem:= lvProfiles.Selected;
  try
     if (curItem <> nil) and newProfiles.Get(curItem.Index, ASource, AParams, curName) then
     begin
       repeat
         try
            connected:= (AParams <> nil) and AParams.Select and AParams.GetFromUser;
         except
            connected:= False;
         end;
       until connected or (MessageDlg('DigIt', Format(rsProfiles_SourceErr, [curItem.Caption]),
                           mtError, [mbRetry, mbCancel], 0) = mrCancel);

       if connected then newProfiles.Put(curItem.Index, curName, curItem.Caption, AParams);
     end;

  finally
     if (AParams <> nil) then
     begin
       AParams.Release;
       if (ASource <> nil) then ASource^.Inst.Params_Set(nil);
     end;
  end;
end;

procedure TDigIt_Profiles_Form.UI_SourceMenuClick(Sender: TObject);
var
   newSourceIndex,
   newSourceSubIndex: Integer;
   curItem: TListItem;
   ASource: PSourceInfo=nil;
   AParams: IDigIt_Params=nil;
   curName,
   curTitle: String;
   connected: Boolean;

begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  begin
    SourcesMenuTag_decode(TMenuItem(Sender).Tag, newSourceIndex, newSourceSubIndex);

    repeat
      try
         if (AParams <> nil) then
         begin
           AParams.Release;
           ASource^.Inst.Params_Set(nil);
         end;

         connected:= Sources.Get(newSourceIndex, newSourceSubIndex, True, True, ASource, AParams);
      except
         connected:= False;
      end;
    until connected or (MessageDlg('DigIt', Format(rsProfiles_SourceErr, [TMenuItem(Sender).Caption]),
                        mtError, [mbRetry, mbCancel], 0) = mrCancel);

    if connected then
    try
      curName:= Sources.Key[newSourceIndex];
      curTitle:= Add(ASource, AParams, curName, newProfiles.XMLFilename, newProfiles.List);
      if (curTitle <> '') then
      begin
        newProfiles.Add(curTitle);
        curItem:= lvProfiles.Items.Add;
        curItem.Caption:= curTitle;
        curItem.SubItems.Add(curName);
        lvProfiles.Selected:= curItem;
      end;

    finally
      if (AParams <> nil) then
      begin
        AParams.Release;
        if (ASource <> nil) then ASource^.Inst.Params_Set(nil);
      end;
    end;
  end;
end;

procedure TDigIt_Profiles_Form.btAddProfileClick(Sender: TObject);
var
   Pt: TPoint;

begin
  BuildSourcesMenu(Self, menuSources, @UI_SourceMenuClick, nil);
  Pt:= btAddProfile.ClientToScreen(Point(0, btAddProfile.Height));
  menuSources.PopUp(Pt.X, Pt.Y);
end;

procedure TDigIt_Profiles_Form.FormShow(Sender: TObject);
begin
  if (lvProfiles.Items.Count > 0) then lvProfiles.ItemIndex:= 0;
end;

procedure TDigIt_Profiles_Form.lvProfilesEdited(Sender: TObject; Item: TListItem; var AValue: string);
var
   i: Integer;
   dup: Boolean;

begin
  //Avoid Duplicates
  dup:= False;
  for i:=0 to lvProfiles.Items.Count-1 do
    if (lvProfiles.Items[i].Caption = AValue) then begin dup:= True; break; end;

  if dup
  then AValue:= Item.Caption
  else newProfiles.SetTitle(Item.Index, AValue);
end;

procedure TDigIt_Profiles_Form.lvProfilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UI_EnableButtons;
end;

procedure TDigIt_Profiles_Form.UI_EnableButtons;
var
   lvCount,
   lvIndex: Integer;

begin
  lvCount:= lvProfiles.Items.Count;
  if (lvProfiles.Selected <> nil)
  then lvIndex:= lvProfiles.Selected.Index
  else lvIndex:= -1;

  btEditProfile.Enabled:= (lvCount > 0) and (lvIndex >= 0);
  btDel.Enabled:= btEditProfile.Enabled;
  btDelAll.Enabled:= (lvCount > 0);
  btUp.Enabled:= (lvCount > 1) and (lvIndex > 0);
  btDown.Enabled:= (lvCount > 1) and (lvIndex < lvCount-1);
  btFirst.Enabled:= btUp.Enabled;
  btLast.Enabled:= btDown.Enabled;
end;

procedure TDigIt_Profiles_Form.UI_LoadFromXML;
var
   i: Integer;
   curItem: TListItem;
   XML: TRttiXMLConfig;

begin
  try
     lvProfiles.Clear;

     XML:= TRttiXMLConfig.Create(newProfiles.XMLFilename);

     //Load Profiles Source Names
     for i:=0 to newProfiles.Count-1 do
     try
       curItem:= lvProfiles.Items.Add;
       curItem.Caption:= newProfiles.Data[i];
       curItem.SubItems.Add(XML.GetValue(PROFILE_Item+IntToStr(i)+'/Source/Name', ''));

     except
     end;

  finally
    XML.Free;
    UI_EnableButtons;
  end;
end;

class function TDigIt_Profiles_Form.Execute(const AFilename: String): Boolean;
var
   XMLFileName: String;
   oldSource: PSourceInfo;
   oldParams: IDigIt_Params;

begin
  Result:= False;
  try
     if (DigIt_Profiles_Form=nil) then DigIt_Profiles_Form:= TDigIt_Profiles_Form.Create(nil);

     if (DigIt_Profiles_Form <> nil) then
     with DigIt_Profiles_Form do
     begin
       oldSource:= DigIt_Sources.Sources.Selected;
       oldParams:= DigIt_Sources.Sources.SelectedParams;

       XMLFileName:= AFilename+'.tmp';

       if FileExists(AFilename) then CopyFile(AFilename, XMLFileName);

       newProfiles:= TDigIt_Profiles.Create(XMLFileName);

       UI_LoadFromXML;

       Result:= (ShowModal = mrOk);

       newProfiles.Free; newProfiles:= nil;

       //Restore Original Params of Selected Source, The Selected is not changed (we use only Sources.Get methods)
       DigIt_Sources.Sources.Select(oldSource, oldParams);

       if Result then Result:= CopyFile(XMLFilename, AFilename);

       DeleteFile(XMLFilename);
     end;

  finally
    if (DigIt_Profiles_Form <> nil) then
    begin
      if (DigIt_Profiles_Form.newProfiles <> nil) then DigIt_Profiles_Form.newProfiles.Free;
      DigIt_Profiles_Form.Free;
      DigIt_Profiles_Form:= nil;
    end;
  end;
end;

//I do it this way to not enable nestedprocvars switch
var
   chkTitleArray: ^TStringArray;

function AddCheck(const AText: String; var AStatusText: String; var AStatusColor: TColor): Boolean;
begin
  Result:= not(StringArrayFind(AText, chkTitleArray^, True));
  if Result
  then begin
         AStatusColor:= clDefault;
         AStatusText:= '';
       end
  else begin
         AStatusColor:= clRed;
         AStatusText:= rsProfiles_InvalidTitle_Exists;
       end;
end;

class function TDigIt_Profiles_Form.Add(const ASource: PSourceInfo; const AParams: IDigIt_Params;
                                        const ASourceName, AFilename: String;
                                        const ATitleArray: TStringArray): String;
var
   newProfileTitle,
   curXMPath: String;
   aXML: TRttiXMLConfig;
   res, iCount: Integer;

begin
  Result:= '';
  if (ASource <> nil) then
  begin
    chkTitleArray:= @ATitleArray;

    newProfileTitle:= Sources.GetTitle(ASource, AParams, True);

    if TFormEditText.Execute(rsProfiles_AddCurrent, rsProfiles_Title, '', newProfileTitle, @AddCheck) then
    begin
      try
         aXML:= TRttiXMLConfig.Create(AFilename);

         //Load Profiles Count
         iCount:= aXML.GetValue('Profiles/Count', 0);

         curXMPath:= PROFILE_Item+IntToStr(iCount)+'/';

         //Add new Profile as Last
         aXML.SetValue(curXMPath+'Name', newProfileTitle);
         aXML.SetValue('Profiles/Count', iCount+1);

         //Save Source
         aXML.SetValue(curXMPath+'Source/Name', ASourceName);
         aXML.DeletePath(curXMPath+'Source/Params/');

      finally
         aXML.Free;
      end;

      //FPC Bug?
      //If a key like "Source/Params" is written to the same open file, even after a flush, it is ignored.
      //So we do it after destroying XML.

      if (AParams <> nil) then AParams.Save(PChar(AFilename), PChar(curXMPath+'Source/Params'));

      Result:= newProfileTitle;
    end;
  end;
end;

function TDigIt_Profiles_Form.Add(const ASource: PSourceInfo; const AParams: IDigIt_Params; const ASourceName: String): String;
begin
  Result:= Add(ASource, AParams, ASourceName, newProfiles.XMLFilename, newProfiles.List);
  if (Result <> '') then newProfiles.Add(Result);
end;

end.

