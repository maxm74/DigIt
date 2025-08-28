(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (c) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Profiles  ( maybe tomorrow implementation of IDigIt_Profiles )           **
*******************************************************************************)
unit DigIt_Profiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg,
  MM_OpenArrayList,
  Digit_Bridge_Intf, DigIt_Sources;

const
  PROFILE_Item = 'Profiles/Profile_';

type
  { TDigIt_Profiles }

  TDigIt_Profiles = class(TOpenArrayString)
  private
    rXMLFilename: String;

  public
    constructor Create(const AXMLFilename: String);

    function LoadFromXML: Boolean;

    class function Add(const AXMLFilename, ASourceName, ATitle: String; const ASourceParams: IDigIt_Params): Boolean; overload;
    function Add(const ASourceName, ATitle: String; const ASourceParams: IDigIt_Params): Boolean; overload;

    function Get(const AIndex: Integer;
                 out ASource: PSourceInfo; out AParams: IDigIt_Params; out ASourceName: String): Boolean; overload;

    function Put(const AIndex: Integer;
                 const ASourceName, ATitle: String; const ASourceParams: IDigIt_Params): Boolean; overload;

    function Move(const AFromIndex, AToIndex: Integer): Boolean;

    function SetTitle(const AIndex: Integer; const ATitle: String): Boolean;

    function Clear(AClearXML: Boolean): Boolean; overload;

    property XMLFilename: String read rXMLFilename;
    property List: TStringArray read rList;
  end;

var
   Profiles: TDigIt_Profiles=nil;

implementation

uses Laz2_DOM, FileUtil;

{ TDigIt_Profiles }

constructor TDigIt_Profiles.Create(const AXMLFilename: String);
begin
  inherited Create;

  rXMLFilename:= AXMLFilename;
  LoadFromXML;
end;

function TDigIt_Profiles.LoadFromXML: Boolean;
var
   aXML: TRttiXMLConfig;
   i, iCount: Integer;

begin
  Result:= False;
  rList:= nil;

  if FileExists(rXMLFilename) then
  try
     aXML:= TRttiXMLConfig.Create(rXMLFilename);

     //Load Profiles
     iCount:= aXML.GetValue('Profiles/Count', 0);
     SetLength(rList, iCount);
     for i:=0 to iCount-1 do
     begin
       rList[i]:= aXML.GetValue(PROFILE_Item+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
     end;

  finally
    aXML.Free;
  end;

  Result:= True;
end;

class function TDigIt_Profiles.Add(const AXMLFilename, ASourceName, ATitle: String; const ASourceParams: IDigIt_Params): Boolean;
var
   curXMPath: String;
   aXML: TRttiXMLConfig;
   iCount: Integer;

begin
  Result:= False;
  try
     aXML:= TRttiXMLConfig.Create(AXMLFilename);

     //Load Profiles Count
     iCount:= aXML.GetValue('Profiles/Count', 0);

     curXMPath:= PROFILE_Item+IntToStr(iCount)+'/';

     //Add new Profile as Last
     aXML.SetValue(curXMPath+'Name', ATitle);
     aXML.SetValue('Profiles/Count', iCount+1);

     //Save Source
     aXML.SetValue(curXMPath+'Source/Name', ASourceName);
     aXML.DeletePath(curXMPath+'Source/Params/');

  finally
     aXML.Free;
  end;

  if (ASourceParams <> nil) then ASourceParams.Save(PChar(AXMLFilename), PChar(curXMPath+'Source/Params'));

  Result:= True;
end;

function TDigIt_Profiles.Add(const ASourceName, ATitle: String; const ASourceParams: IDigIt_Params): Boolean;
begin
  Result:= Add(rXMLFilename, ASourceName, ATitle, ASourceParams);
  if Result then
  begin
    Add(ATitle);
    Result:= (Length(rList) > 0);
  end;
end;

function TDigIt_Profiles.Get(const AIndex: Integer;
                             out ASource: PSourceInfo; out AParams: IDigIt_Params; out ASourceName: String): Boolean;
var
   aXML: TRttiXMLConfig;

begin
  try
     aXML:= TRttiXMLConfig.Create(rXMLFilename);

     Result:= Sources.Get(aXML, PROFILE_Item+IntToStr(AIndex)+'/', True, ASource, AParams, ASourceName);

  finally
    aXML.Free;
  end;
end;

function TDigIt_Profiles.Put(const AIndex: Integer;
                             const ASourceName, ATitle: String; const ASourceParams: IDigIt_Params): Boolean;
var
   curXMPath: String;
   aXML: TRttiXMLConfig;

begin
  Result:= False;
  try
     aXML:= TRttiXMLConfig.Create(rXMLFilename);

     curXMPath:= PROFILE_Item+IntToStr(AIndex)+'/';

     //Rewrite Profile Title
     aXML.SetValue(curXMPath+'Name', ATitle);

     //Save Source
     aXML.SetValue(curXMPath+'Source/Name', ASourceName);
     aXML.DeletePath(curXMPath+'Source/Params/');

  finally
     aXML.Free;
  end;

  if (ASourceParams <> nil) then ASourceParams.Save(PChar(rXMLFilename), PChar(curXMPath+'Source/Params'));

  rList[AIndex]:= ATitle;
  Result:= True;
end;

function TDigIt_Profiles.Move(const AFromIndex, AToIndex: Integer): Boolean;
var
   fileStr: RTLString;
   theFile: TStringStream;

begin
  try
    { #note : Work directly with the text file, Maybe there is a better way to exchange two items}
    theFile:= TStringStream.Create();
    theFile.LoadFromFile(rXMLFilename);
    fileStr:= theFile.DataString;
    fileStr:= StringReplace(fileStr, 'Profile_'+IntToStr(AToIndex), 'Profile_X', [rfReplaceAll, rfIgnoreCase]);
    fileStr:= StringReplace(fileStr, 'Profile_'+IntToStr(AFromIndex), 'Profile_'+IntToStr(AToIndex), [rfReplaceAll, rfIgnoreCase]);
    fileStr:= StringReplace(fileStr, 'Profile_X', 'Profile_'+IntToStr(AFromIndex), [rfReplaceAll, rfIgnoreCase]);
    theFile.Seek(0, soFromBeginning);
    {$IF SIZEOF(CHAR)=1}
      theFile.WriteAnsiString(fileStr);
    {$ELSE}
      theFile.WriteUnicodeString(fileStr);
    {$ENDIF}
    theFile.SaveToFile(rXMLFilename);

  finally
    theFile.Free;
  end;
end;

function TDigIt_Profiles.SetTitle(const AIndex: Integer; const ATitle: String): Boolean;
var
   aXML: TRttiXMLConfig;

begin
  Result:= False;
  try
     aXML:= TRttiXMLConfig.Create(rXMLFilename);

     aXML.SetValue(PROFILE_Item+IntToStr(AIndex)+'/'+'Name', ATitle);
  finally
     aXML.Free;
  end;
end;

function TDigIt_Profiles.Clear(AClearXML: Boolean): Boolean;
var
   aXML: TRttiXMLConfig;

begin
  Result:= inherited Clear;

  if Result then
  try
     Result:= False;

     aXML:= TRttiXMLConfig.CreateClean(rXMLFilename);

     //Set Selected Profile
     aXML.SetValue('Profiles/Count', 0);

     Result:= True;
  finally
    aXML.Free;
  end;
end;

end.

