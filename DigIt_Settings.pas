unit DigIt_Settings;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Laz2_XMLCfg,
     DigIt_Types, Digit_Bridge_Intf;

const
  SET_Path = 'Settings/';

type
  { TDigIt_Settings }
  TDigIt_Settings = class(TNoRefCountObject, IDigIt_Settings)
  protected
    rStartupSession_Path,
    rStartupSession_File: String;
    rStartupProfile: Integer;

  public
    constructor Create;

    procedure Default;

    function Load(aXML: TRttiXMLConfig): Boolean;
    function Save(aXML: TRttiXMLConfig): Boolean;

    //Useful to avoid having to read/write everything
    procedure Load_StartupSession(aXML: TRttiXMLConfig; var APath, AFile: String);
    procedure Save_StartupSession(aXML: TRttiXMLConfig; const APath, AFile: String);

    //IDigIt_Settings implementation
    //Path consts: High Byte = Category, Low Byte = Path
    function Path(const APathID: Word): PChar; stdcall; { #note 10 -oMaxM : Test in External LIBRARY }

  published
    property StartupSession_Path: String read rStartupSession_Path write rStartupSession_Path;
    property StartupSession_File: String read rStartupSession_File write rStartupSession_File;
    property StartupProfile: Integer read rStartupProfile write rStartupProfile;
  end;

implementation

{ TDigIt_Settings }

constructor TDigIt_Settings.Create;
begin
  inherited Create;

  Default;
end;

procedure TDigIt_Settings.Default;
begin
  rStartupSession_Path:= '';
  rStartupSession_File:= '';
  rStartupProfile:= -1;
end;

function TDigIt_Settings.Load(aXML: TRttiXMLConfig): Boolean;
var
   aFree: Boolean;

begin
  Result:= False;
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(DigIt_Types.Path_Config+File_Config);

     aXML.ReadObject(SET_Path, Self);

     Result:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

function TDigIt_Settings.Save(aXML: TRttiXMLConfig): Boolean;
var
   aFree: Boolean;

begin
  Result:= False;
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(DigIt_Types.Path_Config+File_Config);

     aXML.WriteObject(SET_Path, Self);

     Result:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Settings.Load_StartupSession(aXML: TRttiXMLConfig; var APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(DigIt_Types.Path_Config+File_Config);

     APath:= SetDirSeparators(aXML.GetValue(SET_Path+'StartupSession_Path', ''));
     AFile:= SetDirSeparators(aXML.GetValue(SET_Path+'StartupSession_File', ''));

     rStartupSession_Path:= APath;
     rStartupSession_File:= AFile;

  finally
     if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Settings.Save_StartupSession(aXML: TRttiXMLConfig; const APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(DigIt_Types.Path_Config+File_Config);

     rStartupSession_Path:= SetDirSeparators(APath);
     rStartupSession_File:= SetDirSeparators(AFile);

     aXML.SetValue(SET_Path+'StartupSession_Path', rStartupSession_Path);
     aXML.SetValue(SET_Path+'StartupSession_File', rStartupSession_File);

  finally
     if aFree then aXML.Free;
  end;
end;


{ TDigIt_Settings }

function TDigIt_Settings.Path(const APathID: Word): PChar; stdcall;
begin
  Case APathID of
  ID_Path_Application: Result:= PChar(Path_Application);
  ID_Path_Config: Result:= PChar(Path_Config);
  ID_Path_Temp: Result:= PChar(Path_Temp);

  ID_Path_Session: Result:= PChar(Path_Session);
  ID_Path_Session_Scan: Result:= PChar(Path_Session_Scan);
  ID_Path_Session_Pictures: Result:= PChar(Path_Session_Pictures);
  else Result:= nil;
  end
end;

end.

