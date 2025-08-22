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
  Classes, SysUtils, Graphics, Forms, Controls, Buttons, ComCtrls, ExtCtrls,
  Menus, Laz2_XMLCfg, BCPanel, BCListBox,
  Digit_Bridge_Intf, Digit_Bridge_Impl, DigIt_Sources, DigIt_Session;

resourcestring
  rsProfiles_AddCurrent = 'Add Current Source...';
  rsProfiles_Title = 'Profile Title :';
  rsProfiles_InvalidTitle_Exists = 'Profile Title already exists';

const
  PROF_Item = 'Profiles/Profile_';

type
  { TDigIt_Profiles_Form }

  {# TO-DO : SEPARATE GRAPHICS FROM Profiles Logic (new DigIt_Profiles class and file)}

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
    procedure btEditProfileClick(Sender: TObject);
    procedure btUpDownClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvProfilesEdited(Sender: TObject; Item: TListItem;
      var AValue: string);
    procedure lvProfilesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    XMLFilename: String;
    TitleArray: TStringArray;

    procedure UI_EnableButtons;
    procedure UI_LoadFromXML;
    procedure UI_SourceMenuClick(Sender: TObject);


  public
    class function Execute(ASession: TDigIt_Session; const AFilename: String; var ATitleArray: TStringArray): Boolean;
    class function LoadFromXML(const AFilename: String; var ATitleArray: TStringArray): Boolean;

    class function Add(const ASource: PSourceInfo; const AParams: IDigIt_Params;
                       const ASourceName, AFilename: String; var ATitleArray: TStringArray): Boolean;
    class function AddCurrent(const AFilename: String; var ATitleArray: TStringArray): Boolean;

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

      { #note : Work directly with the text file, Maybe there is a better way to exchange two items}
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

procedure TDigIt_Profiles_Form.btDelAllClick(Sender: TObject);
var
   aXML: TRttiXMLConfig;

begin
  try
     lvProfiles.Clear;

     aXML:= TRttiXMLConfig.CreateClean(XMLFilename);

     //Set Selected Profile
     aXML.SetValue('Profiles/Count', 0);

  finally
    aXML.Free;
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

begin
  curItem:= lvProfiles.Selected;
  if (curItem <> nil) then
  try
     aXML:= TRttiXMLConfig.Create(XMLFilename);

     curXMPath:= PROF_Item+IntToStr(curItem.Index)+'/';

     if Sources.Get(ASource, AParams, curName, aXML, curXMPath) then
     begin
       if (AParams <> nil) then
       begin
         try
            //Rewrite Profile Title
            aXML.SetValue(curXMPath+'Name', curItem.Caption);

            //Save Source
            aXML.SetValue(curXMPath+'Source/Name', curName);
            aXML.DeletePath(curXMPath+'Source/Params/');

         finally
           aXML.Free; aXML:= nil;
         end;

         AParams.Save(PChar(XMLFilename), PChar(curXMPath+'Source/Params'));
       end;
     end;
  finally
     if (AParams <> nil) then AParams.Release;
  end;
end;

procedure TDigIt_Profiles_Form.UI_SourceMenuClick(Sender: TObject);
var
   newSourceIndex,
   newSourceSubIndex: Integer;
   curItem: TListItem;
   ASource: PSourceInfo=nil;
   AParams: IDigIt_Params=nil;
   curName: String;

begin
  if (Sender<>nil) and (Sender is TMenuItem) then
  begin
    SourcesMenuTag_decode(TMenuItem(Sender).Tag, newSourceIndex, newSourceSubIndex);

    if Sources.Get(ASource, AParams, newSourceIndex, newSourceSubIndex, True) then
    try
      curName:= Sources.Key[newSourceIndex];
      if Add(ASource, AParams, curName, XMLFilename, TitleArray) then
      begin
        curItem:= lvProfiles.Items.Add;
        curItem.Caption:= TitleArray[Length(TitleArray)-1];
        curItem.SubItems.Add(curName);
      end;

    finally
      if (AParams <> nil) then AParams.Release;
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

procedure TDigIt_Profiles_Form.lvProfilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UI_EnableButtons;
end;

procedure TDigIt_Profiles_Form.UI_EnableButtons;
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

procedure TDigIt_Profiles_Form.UI_LoadFromXML;
var
   i, iCount: Integer;
   curItem: TListItem;
   XML: TRttiXMLConfig;

begin
  try
  try
     XML:= TRttiXMLConfig.Create(XMLFilename);

     lvProfiles.Clear;

     //Load Profiles
     iCount:= XML.GetValue('Profiles/Count', 0);
     SetLength(TitleArray, iCount);
     for i:=0 to iCount-1 do
     begin
       curItem:= lvProfiles.Items.Add;
       TitleArray[i]:= XML.GetValue(PROF_Item+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
       curItem.Caption:= TitleArray[i];
       curItem.SubItems.Add(XML.GetValue(PROF_Item+IntToStr(i)+'/Source/Name', ''));
     end;

  except
  end;

  finally
    XML.Free;
    UI_EnableButtons;
  end;
end;

class function TDigIt_Profiles_Form.Execute(ASession: TDigIt_Session; const AFilename: String; var ATitleArray: TStringArray): Boolean;
var
   fCreate: Boolean;
   fHandle: THandle;

begin
  Result:= False;
  try
     if (DigIt_Profiles_Form=nil) then DigIt_Profiles_Form:= TDigIt_Profiles_Form.Create(nil);

     if (DigIt_Profiles_Form <> nil) then
     with DigIt_Profiles_Form do
     try
        XMLFilename:= AFilename+'.tmp';

        if not(FileExists(AFilename))
        then try
               fHandle:= FileCreate(XMLFilename);
               FileClose(fHandle);
               fCreate:= True;
             except
               fCreate:= False;
             end
        else fCreate:= CopyFile(AFilename, XMLFilename);

        if fCreate then
        begin
          UI_LoadFromXML;

          Result:= (ShowModal = mrOk);

          if Result then
          begin
            Result:= CopyFile(XMLFilename, AFilename);
            DeleteFile(XMLFilename);
            Result:= LoadFromXML(AFilename, ATitleArray);
          end;

          //Reload Old Source
          if (ASession <> nil) then
          begin
            ASession.LoadSource(nil, True);
          end;
        end;

     finally
     end;

  finally
    DigIt_Profiles_Form.Free; DigIt_Profiles_Form:= nil;
  end;
end;

class function TDigIt_Profiles_Form.LoadFromXML(const AFilename: String;
  var ATitleArray: TStringArray): Boolean;
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
                                        const ASourceName, AFilename: String; var ATitleArray: TStringArray): Boolean;
var
   newProfileTitleP: PChar;
   newProfileTitle,
   curXMPath: String;
   aXML: TRttiXMLConfig;
   res, iCount: Integer;

begin
  if (ASource <> nil) then
  try
     chkTitleArray:= @ATitleArray;

     //First tell to Params a Description then to Source
     newProfileTitleP:= ''; res:= 0;
     if (AParams <> nil) then
     begin
       res:= AParams.Summary(newProfileTitleP);
       if (res >0 ) and (newProfileTitleP <> '') then
       begin
         newProfileTitle:= newProfileTitleP;
         StrDispose(newProfileTitleP);
         newProfileTitleP:= '';
       end;
     end;

     res:= ASource^.Inst.UI_Title(newProfileTitleP);
     if (res > 0) and (newProfileTitleP <> '') then
     begin
       if (newProfileTitle = '')
       then newProfileTitle:= newProfileTitleP
       else newProfileTitle:= newProfileTitle+' - '+newProfileTitleP;

       StrDispose(newProfileTitleP);
       newProfileTitleP:= '';
     end;

     Result:= TFormEditText.Execute(rsProfiles_AddCurrent, rsProfiles_Title, '', newProfileTitle, @AddCheck);
     if Result then
     begin
       Result:= False;
       try
          aXML:= TRttiXMLConfig.Create(AFilename);

          //Load Profiles Count
          iCount:= aXML.GetValue('Profiles/Count', 0);

          curXMPath:= PROF_Item+IntToStr(iCount)+'/';

          //Add new Profile as Last
          aXML.SetValue(curXMPath+'Name', newProfileTitle);
          aXML.SetValue('Profiles/Count', iCount+1);

          //Save Source
          aXML.SetValue(curXMPath+'Source/Name', ASourceName);
          aXML.DeletePath(curXMPath+'Source/Params/');

       finally
         aXML.Free; aXML:= nil;
       end;

       //FPC Bug?
       //If a key like "Source/Params" is written to the same open file, even after a flush, it is ignored.
       //So we do it after destroying XML.

       if (AParams <> nil)
       then AParams.Save(PChar(AFilename), PChar(curXMPath+'Source/Params'));

       SetLength(ATitleArray, iCount+1);
       ATitleArray[iCount]:= newProfileTitle;
       Result:= True;
     end;

  finally
    if (newProfileTitleP <> '') then StrDispose(newProfileTitleP);
  end;
end;

class function TDigIt_Profiles_Form.AddCurrent(const AFilename: String; var ATitleArray: TStringArray): Boolean;
var
   ASourceParams: IDigIt_Params;
   newProfileTitleP: PChar;
   newProfileTitle,
   curXMPath: String;
   aXML: TRttiXMLConfig;
   res, iCount: Integer;

begin
  Result:= False;
  if (Sources.Selected <> nil) then Result:= Add(Sources.Selected, Sources.SelectedParams, Sources.SelectedName,
                                                       AFilename, ATitleArray);

(*oldcode
  if (Sources.Selected <> nil) then
  try
     ASourceParams:= Sources.SelectedParams;

     chkTitleArray:= @ATitleArray;

     //First tell to Params a Description then to Source
     newProfileTitleP:= ''; res:= 0;
     if (ASourceParams <> nil) then
     begin
       res:= ASourceParams.Summary(newProfileTitleP);
       if (res >0 ) and (newProfileTitleP <> '') then
       begin
         newProfileTitle:= newProfileTitleP;
         StrDispose(newProfileTitleP);
         newProfileTitleP:= '';
       end;
     end;

     res:= Sources.Selected^.Inst.UI_Title(newProfileTitleP);
     if (res >0 ) and (newProfileTitleP <> '') then
     begin
       if (newProfileTitle = '')
       then newProfileTitle:= newProfileTitleP
       else newProfileTitle:= newProfileTitle+' - '+newProfileTitleP;

       StrDispose(newProfileTitleP);
       newProfileTitleP:= '';
     end;

     Result:= TFormEditText.Execute(rsProfiles_AddCurrent, rsProfiles_Title, '', newProfileTitle, @AddCheck);
     if Result then
     begin
       Result:= False;
       try
          aXML:= TRttiXMLConfig.Create(AFilename);

          //Load Profiles Count
          iCount:= aXML.GetValue('Profiles/Count', 0);

          curXMPath:= PROF_Item+IntToStr(iCount)+'/';

          //AddCurrent new Profile as Last
          aXML.SetValue(curXMPath+'Name', newProfileTitle);
          aXML.SetValue('Profiles/Count', iCount+1);

          //Save Source
          aXML.SetValue(curXMPath+'Source/Name', Sources.SelectedName);
          aXML.DeletePath(curXMPath+'Source/Params/');

       finally
         aXML.Free; aXML:= nil;
       end;

       //FPC Bug?
       //If a key like "Source/Params" is written to the same open file, even after a flush, it is ignored.
       //So we do it after destroying XML.

       if (ASourceParams <> nil)
       then ASourceParams.Save(PChar(AFilename), PChar(curXMPath+'Source/Params'));

       SetLength(ATitleArray, iCount+1);
       ATitleArray[iCount]:= newProfileTitle;
       Result:= True;
     end;

  finally
    if (newProfileTitleP <> '') then StrDispose(newProfileTitleP);
  end;
  *)
end;

end.

