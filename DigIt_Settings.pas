unit DigIt_Settings;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Laz2_XMLCfg,
     DigIt_Types, Digit_Bridge_Intf;

const
  SET_Path = 'Settings/';

type

  { TSessionSettings }

  TSessionSettings = class(TPersistent)
  protected
    rStartup_Path,
    rStartup_File: String;
    rConfirmSaveOnClose: Boolean;

  published
    property Startup_Path: String read rStartup_Path write rStartup_Path;
    property Startup_File: String read rStartup_File write rStartup_File;
    property ConfirmSaveOnClose: Boolean read rConfirmSaveOnClose write rConfirmSaveOnClose;
  end;

  { TDigIt_Settings }
  TDigIt_Settings = class(TNoRefCountObject, IDigIt_Settings)
  protected
    rSession: TSessionSettings;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Default;

    function Load(aXML: TRttiXMLConfig): Boolean;
    function Save(aXML: TRttiXMLConfig): Boolean;

    //Useful to avoid having to read/write everything
    //procedure Load_StartupSession(aXML: TRttiXMLConfig; var APath, AFile: String);
    procedure Save_StartupSession(aXML: TRttiXMLConfig; const APath, AFile: String);

    //IDigIt_Settings implementation
    //Path consts: High Byte = Category, Low Byte = Path
    function Path(const APathID: Word): PChar; stdcall; { #note 10 -oMaxM : Test in External LIBRARY }

  published
    property Session: TSessionSettings read rSession write rSession;
  end;

implementation

{ TDigIt_Settings }

constructor TDigIt_Settings.Create;
begin
  inherited Create;

  rSession:= TSessionSettings.Create;

  Default;
end;

destructor TDigIt_Settings.Destroy;
begin
  rSession.Free;

  inherited Destroy;
end;

procedure TDigIt_Settings.Default;
begin
  rSession.Startup_Path:= '';
  rSession.Startup_File:= '';
  rSession.ConfirmSaveOnClose:= True;
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

(* oldcode
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
*)

procedure TDigIt_Settings.Save_StartupSession(aXML: TRttiXMLConfig; const APath, AFile: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then aXML:= TRttiXMLConfig.Create(DigIt_Types.Path_Config+File_Config);

     rSession.Startup_Path:= SetDirSeparators(APath);
     rSession.Startup_File:= SetDirSeparators(AFile);

     aXML.SetValue(SET_Path+'Session/Startup_Path', rSession.Startup_Path);
     aXML.SetValue(SET_Path+'Session/Startup_File', rSession.Startup_File);

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

