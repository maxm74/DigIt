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
  Classes, SysUtils, Graphics, Forms, Controls, Buttons, ComCtrls, ExtCtrls, Laz2_XMLCfg,
  BCPanel, BCListBox,
  Digit_Bridge_Intf, Digit_Bridge_Impl, DigIt_Sources;

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
    procedure btDelAllClick(Sender: TObject);
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
    class function Execute(const AFilename: String; var ATitleArray: TStringArray): Boolean;
    class function LoadFromXML(const AFilename: String; var ATitleArray: TStringArray): Boolean;

    class function Add(const AFilename: String; var ATitleArray: TStringArray;
                       ASource: PSourceInfo; ASourceParams: IDigIt_Params; const ASourceName: String): Boolean;
  end;

var
  DigIt_Profiles_Form: TDigIt_Profiles_Form=nil;

implementation

{$R *.lfm}

uses Laz2_DOM, FileUtil, MM_StrUtils, MM_Form_EditText;

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

procedure TDigIt_Profiles_Form.FormShow(Sender: TObject);
begin
  if (lvProfiles.Items.Count > 0) then lvProfiles.ItemIndex:= 0;
end;

procedure TDigIt_Profiles_Form.lvProfilesEdited(Sender: TObject; Item: TListItem; var AValue: string);
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

class function TDigIt_Profiles_Form.Execute(const AFilename: String; var ATitleArray: TStringArray): Boolean;
var
   aSelected: Integer;

begin
  Result:= False;
  try
     if (DigIt_Profiles_Form=nil)
     then DigIt_Profiles_Form :=TDigIt_Profiles_Form.Create(nil);

     if (DigIt_Profiles_Form <> nil) then
     with DigIt_Profiles_Form do
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

class function TDigIt_Profiles_Form.Add(const AFilename: String;
  var ATitleArray: TStringArray; ASource: PSourceInfo;
  ASourceParams: IDigIt_Params; const ASourceName: String): Boolean;

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

     res:= ASource^.Inst.UI_Title(newProfileTitleP);
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

       if (ASourceParams <> nil)
       then ASourceParams.Save(PChar(AFilename), PChar(curXMPath+'Source/Params'));

       SetLength(ATitleArray, iCount+1);
       ATitleArray[iCount]:= newProfileTitle;
       Result:= True;
     end;

  finally
    if (newProfileTitleP <> '') then StrDispose(newProfileTitleP);
  end;
end;

end.

