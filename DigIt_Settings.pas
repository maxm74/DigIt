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
    //Path consts                 { #note 10 -oMaxM : Test in External LIBRARY }
    function Path_Application: PChar; stdcall;
    function Path_Config: PChar; stdcall;
    function Path_Temp: PChar; stdcall;
    function Path_Session: PChar; stdcall;
    function Path_Session_Scan: PChar; stdcall;
    function Path_Session_Pictures: PChar; stdcall;

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

function TDigIt_Settings.Path_Application: PChar; stdcall;
begin
  Result:= PChar(DigIt_Types.Path_Application);
end;

function TDigIt_Settings.Path_Config: PChar; stdcall;
begin
  Result:= PChar(DigIt_Types.Path_Config);
end;

function TDigIt_Settings.Path_Temp: PChar; stdcall;
begin
  Result:= PChar(DigIt_Types.Path_Temp);
end;

function TDigIt_Settings.Path_Session_Pictures: PChar; stdcall;
begin
  Result:= PChar(DigIt_Types.Path_Session_Pictures);
end;

function TDigIt_Settings.Path_Session: PChar; stdcall;
begin
  Result:= PChar(DigIt_Types.Path_Session);
end;

function TDigIt_Settings.Path_Session_Scan: PChar; stdcall;
begin
  Result:= PChar(DigIt_Types.Path_Session_Scan);
end;


end.

