(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Bridge Engine Implementation                                             **
*******************************************************************************)

unit Digit_Bridge_Impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Digit_Bridge_Intf;

type
  TPluginInfo = record
    Info: TDigIt_PluginInfo;
    LibFile: String;
    LibHandle: TLibHandle;  //If =0 then InitProc and ReleaseProc refer to statically linked procedures
    InfoProc: TDigIt_PluginInfoProc;
    InitProc,
    ReleaseProc: TDigIt_PluginInitProc;
  end;

  { TDigIt_Plugins }

  TDigIt_Plugins = class(TObject)
  protected
    rPluginsList: array of TPluginInfo;

    function Find(const aName: String): Integer;

    function GetPlugin(const aName: String): TPluginInfo; overload;
    function GetPlugin(Index : Integer): TPluginInfo; overload;
    function GetPluginName(Index: Integer): String;
    function GetCount: Integer;

    function FreeElement(Index: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Register(const aFileName: String): Boolean; overload;
    function Register(const aPluginInfo: TPluginInfo): Boolean; overload;
    function UnRegister(const aName: String): Boolean;

    function RegisterInPath(aPath: String): Integer;

    property Count: Integer read GetCount;

    property PluginByName [const aName: String]: TPluginInfo read GetPlugin;
    property Plugin [const aIndex: Integer]: TPluginInfo read GetPlugin;
    property Name [const aIndex: Integer]: String read GetPluginName;
  end;

  TTakerInfo = record
    Name: String;
    Inst: IDigIt_Taker;
  end;

  { TDigIt_Takers }

  TDigIt_Takers = class(TNoRefCountObject, IDigIt_Takers)
  protected
    rTakersList: array of TTakerInfo;

    function Find(const aName: String): Integer; overload;
    function Find(const aClass: IDigIt_Taker): Integer; overload;

    function GetTaker(const aName: String) : IDigIt_Taker; overload;
    function GetTaker(Index: Integer) : IDigIt_Taker; overload;
    function GetTakerName(Index: Integer) : String;
    function GetCount: Integer;

    function FreeElement(Index: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Register(const aName: PChar; const aClass: IDigIt_Taker): Boolean; stdcall;
    function UnRegister(const aName: String): Boolean; overload;
    function UnRegister(const aClass: IDigIt_Taker): Boolean; overload;

    property Count: Integer read GetCount;

    property TakerByName [const aName: String]: IDigIt_Taker read GetTaker;
    property Taker [const aIndex: Integer]: IDigIt_Taker read GetTaker;
    property Name [const aIndex: Integer]: String read GetTakerName;
  end;

  { TDigIt_Bridge_Settings }

  { TDigIt_Settings }

  TDigIt_Settings = class(TNoRefCountObject, IDigIt_Settings)
  protected
    rMaxPCharSize: DWord;
    rPath_Temp,
    rPath_Config,
    rPath_Application: PChar;

  public
    constructor Create;
    destructor Destroy; override;

    //Buffers Limits Variables
    function GetMaxPCharSize: DWord; stdcall;
    function SetMaxPCharSize(NewSize: DWord): DWord; stdcall;

    //Path consts
    function Path_Temp: PChar; stdcall;
    function Path_Config: PChar; stdcall;
    function Path_Application: PChar; stdcall;
  end;

  { TDigIt_Bridge }

  TDigIt_Bridge = class(TNoRefCountObject, IDigIt_Bridge)
  protected
    rSettings: TDigIt_Settings;
    rTakers: TDigIt_Takers;
    rPlugins: TDigIt_Plugins;

  public
    constructor Create;
    destructor Destroy; override;

    function Takers: IDigIt_Takers; stdcall;
    function Settings: IDigIt_Settings; stdcall;

    (*function Register(const aDisplayName: PChar;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean; stdcall;
    //function UnRegister(const aDisplayName: PChar) :Boolean; stdcall;
    *)

    //Internal Use only
    function TakersImpl: TDigIt_Takers;
    function SettingsImpl: TDigIt_Settings;
    function Plugins: TDigIt_Plugins;
  end;

var
   theBridge: TDigIt_Bridge = nil;

implementation

uses DigIt_Types, dynlibs, Masks;

{ TDigIt_Plugins }

function TDigIt_Plugins.Find(const aName: String): Integer;
var
  i : Integer;

begin
  Result:= -1;
  for i:=0 to Length(rPluginsList)-1 do
    if (rPluginsList[i].Info.Name = aName) then
    begin
      Result:= i; break;
    end;
end;

constructor TDigIt_Plugins.Create;
begin
  inherited Create;

  rPluginsList:= Nil;
end;

destructor TDigIt_Plugins.Destroy;
var
   i:Integer;
   res: Boolean;

begin
  for i:=0 to Length(rPluginsList)-1 do
  begin
    try
       res:= FreeElement(i);

    except
    end;
  end;

  try
     rPluginsList:= Nil;
  except
  end;

  inherited Destroy;
end;

function TDigIt_Plugins.Register(const aFileName: String): Boolean;
var
   newPlugin: TPluginInfo;
   res: Boolean;

begin
  Result:= False;

  try
     with newPlugin do
     begin
       LibFile:= aFileName;
       LibHandle:= LoadLibrary(aFileName);

       if not(LibHandle = dynlibs.NilHandle) then
       begin
         InitProc:= TDigIt_PluginInitProc(GetProcedureAddress(LibHandle, DigIt_PluginInitProcName));

         //Plugin MUST Have InitProc
         if Assigned(InitProc) then
         begin
           ReleaseProc:= TDigIt_PluginInitProc(GetProcedureAddress(LibHandle, DigIt_PluginReleaseProcName));
           InfoProc :=TDigIt_PluginInfoProc(GetProcedureAddress(LibHandle, DigIt_PluginInfoProcName));

           res:= Assigned(InfoProc);
           if res
           then res:= InfoProc(Info);

           if not(res)
           then Info.Name:= ExtractFileName(aFileName);

           Result:= Register(newPlugin);
         end;

         if not(Result)
         then FreeLibrary(newPlugin.LibHandle);

       end;
     end;

  except
    if not(newPlugin.LibHandle = dynlibs.NilHandle)
    then FreeLibrary(newPlugin.LibHandle);
    Result:= False;
  end;
end;

function TDigIt_Plugins.Register(const aPluginInfo: TPluginInfo): Boolean;
begin
  Result:= False;

  if (Find(aPluginInfo.Info.Name) = -1) then
  begin
    SetLength(rPluginsList, Length(rPluginsList)+1);

    rPluginsList[Length(rPluginsList)-1]:= aPluginInfo;

    if Assigned(aPluginInfo.InitProc)
    then Result:= aPluginInfo.InitProc(theBridge);
  end;
end;

function TDigIt_Plugins.UnRegister(const aName: String): Boolean;
var
   r : Integer;
   res: Boolean;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    Result:= FreeElement(r);

    Delete(rPluginsList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Plugins.RegisterInPath(aPath: String): Integer;
var
   fileInfo: TSearchRec;
   err: Integer;
   theCaption,
   theExt: String;
   IsDir: Boolean;

begin
  Result:= 0;

  //if Last char is Separator, Delete it
  if (aPath[Length(aPath)] in AllowDirectorySeparators)
  then SetLength(aPath, Length(aPath)-1);

  if DirectoryExists(aPath) then
  try
     err:= FindFirst(aPath+DirectorySeparator+'*', faAnyFile, fileInfo);
     while (err = 0) do
     begin
       if (fileInfo.Name[1] <> '.') then  //not [.] or [..]
       begin
         theCaption :=ExtractFileName(fileInfo.Name);
         theExt     :=ExtractFileExt(fileInfo.Name);
         IsDir  :=((fileInfo.Attr and faDirectory)<>0);

         if not(IsDir) and MatchesMask(fileInfo.Name, '*.'+SharedSuffix)
         then if Register(aPath+DirectorySeparator+fileInfo.Name)
              then Inc(Result);
       end;

       err :=FindNext(fileInfo);
    end;

  finally
    FindClose(fileInfo);
  end;
end;

function TDigIt_Plugins.GetPlugin(const aName: String): TPluginInfo;
var
  i: Integer;

begin
  { #note 2 -oMaxM : Return a Copy or a Pointer to List element? }
  FillChar(Result, SizeOf(TPluginInfo), 0);

  for i:=0 to Length(rPluginsList)-1 do
    if (rPluginsList[i].Info.Name = aName) then
    begin
      Result:= rPluginsList[i]; break;
    end;
end;

function TDigIt_Plugins.GetPlugin(Index: Integer): TPluginInfo;
begin
  { #note 2 -oMaxM : Return a Copy or a Pointer to List element? }
  FillChar(Result, SizeOf(TPluginInfo), 0);

  if (Index >= 0) and (Index < Length(rPluginsList))
  then Result:= rPluginsList[Index];
end;

function TDigIt_Plugins.GetPluginName(Index: Integer): String;
begin
  if (Index >= 0) and (Index < Length(rPluginsList))
  then Result:= rPluginsList[Index].Info.Name
  else Result:= '';
end;

function TDigIt_Plugins.GetCount: Integer;
begin
  Result:= Length(rPluginsList);
end;

function TDigIt_Plugins.FreeElement(Index: Integer): Boolean;
begin
  Result:= False;

  if Assigned(rPluginsList[Index].ReleaseProc)
  then Result:= rPluginsList[Index].ReleaseProc(theBridge);

  if not(rPluginsList[Index].LibHandle = dynlibs.NilHandle)
  then Result:= FreeLibrary(rPluginsList[Index].LibHandle);
end;

{ TDigIt_Takers }

function TDigIt_Takers.Find(const aName: String): Integer;
var
  i: Integer;

begin
  Result:= -1;
  for i:=0 to Length(rTakersList)-1 do
    if (rTakersList[i].Name = aName) then
    begin
      Result:= i; break;
    end;
end;

function TDigIt_Takers.Find(const aClass: IDigIt_Taker): Integer;
var
  i: Integer;

begin
  Result:= -1;
  for i:=0 to Length(rTakersList)-1 do
    if (rTakersList[i].Inst = aClass) then
    begin
      Result:= i; break;
    end;
end;

function TDigIt_Takers.GetTaker(const aName: String): IDigIt_Taker;
var
  i: Integer;

begin
  Result:= Nil;
  for i:=0 to Length(rTakersList)-1 do
    if (rTakersList[i].Name = aName) then
    begin
      Result:= rTakersList[i].Inst; break;
    end;
end;

function TDigIt_Takers.GetTaker(Index: Integer): IDigIt_Taker;
begin
  if (Index >= 0) and (Index < Length(rTakersList))
  then Result:= rTakersList[Index].Inst
  else Result:= Nil;
end;

function TDigIt_Takers.GetTakerName(Index: Integer): String;
begin
  if (Index >= 0) and (Index < Length(rTakersList))
  then Result:= rTakersList[Index].Name
  else Result:= '';
end;

function TDigIt_Takers.GetCount: Integer;
begin
  Result:= Length(rTakersList);
end;

function TDigIt_Takers.FreeElement(Index: integer): Boolean;
begin
  if (rTakersList[Index].Inst <> nil)
  then rTakersList[Index].Inst.Release;

  //rTakersList[Index].Name:='';
  //FillChar(rTakersList[Index], SizeOf(rTakersList[Index]), 0);
end;

constructor TDigIt_Takers.Create;
begin
  inherited Create;

  rTakersList:= Nil;
end;

destructor TDigIt_Takers.Destroy;
var
   i: Integer;

begin
  { #todo 10 -oMaxM : Free the Instances?  }
  for i:=0 to Length(rTakersList)-1 do
  begin
    try
       FreeElement(i);

    except
    end;
  end;

  try
     rTakersList:= Nil;
  except
  end;

  inherited Destroy;
end;

function TDigIt_Takers.Register(const aName: PChar; const aClass: IDigIt_Taker): Boolean; stdcall;
begin
  Result:= False;

  if (Find(aName) = -1) then
  begin
    SetLength(rTakersList, Length(rTakersList)+1);

    rTakersList[Length(rTakersList)-1].Name:= aName;
    rTakersList[Length(rTakersList)-1].Inst:= aClass;

    Result:= True;
  end;
end;

function TDigIt_Takers.UnRegister(const aName: String): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rTakersList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Takers.UnRegister(const aClass: IDigIt_Taker): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aClass);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rTakersList, r, 1);
    Result:= True;
  end;
end;

{ TDigIt_Settings }

constructor TDigIt_Settings.Create;
begin
  inherited Create;

  rMaxPCharSize:= DigIt_MaxPCharSize;
  rPath_Temp:= Nil;
  rPath_Config:= Nil;
  rPath_Application:= Nil;
end;

destructor TDigIt_Settings.Destroy;
begin
  StrDispose(rPath_Temp);
  StrDispose(rPath_Config);
  StrDispose(rPath_Application);

  inherited Destroy;
end;

function TDigIt_Settings.GetMaxPCharSize: DWord; stdcall;
begin
  Result:= rMaxPCharSize;
end;

function TDigIt_Settings.SetMaxPCharSize(NewSize: DWord): DWord; stdcall;
begin
  if (NewSize > rMaxPCharSize) then rMaxPCharSize:= NewSize;
  Result:= rMaxPCharSize;
end;

function TDigIt_Settings.Path_Temp: PChar; stdcall;
begin
  if (rPath_Temp = Nil) then rPath_Temp:= StrAlloc(rMaxPCharSize);

  StrPLCopy(rPath_Temp, DigIt_Types.Path_Temp, rMaxPCharSize);
  Result:= rPath_Temp;  { #note 10 -oMaxM : Test internal plugin and LIBRARY }
end;

function TDigIt_Settings.Path_Config: PChar; stdcall;
begin
  if (rPath_Config = Nil) then rPath_Config:= StrAlloc(rMaxPCharSize);

  StrPLCopy(rPath_Config, DigIt_Types.Path_Config, rMaxPCharSize);
  Result:= rPath_Config;  { #note 10 -oMaxM : Test internal plugin and LIBRARY }
end;

function TDigIt_Settings.Path_Application: PChar; stdcall;
begin
  if (rPath_Application = Nil) then rPath_Application:= StrAlloc(rMaxPCharSize);

  StrPLCopy(rPath_Application, DigIt_Types.Path_Application, rMaxPCharSize);
  Result:= rPath_Application;  { #note 10 -oMaxM : Test internal plugin and LIBRARY }
end;

{ TDigIt_Bridge }

constructor TDigIt_Bridge.Create;
begin
  inherited Create;

  rPlugins:= TDigIt_Plugins.Create;
  rSettings:= TDigIt_Settings.Create;
  rTakers:= TDigIt_Takers.Create;
end;

destructor TDigIt_Bridge.Destroy;
begin
  rTakers.Free;
  rSettings.Free;
  rPlugins.Free;

  inherited Destroy;
end;

function TDigIt_Bridge.Takers: IDigIt_Takers; stdcall;
begin
  Result:= rTakers;
end;

function TDigIt_Bridge.Settings: IDigIt_Settings; stdcall;
begin
  Result:= rSettings;
end;

function TDigIt_Bridge.TakersImpl: TDigIt_Takers;
begin
  Result:= rTakers;
end;

function TDigIt_Bridge.SettingsImpl: TDigIt_Settings;
begin
  Result:= rSettings;
end;

function TDigIt_Bridge.Plugins: TDigIt_Plugins;
begin
  Result:= rPlugins;
end;

initialization
  theBridge:= TDigIt_Bridge.Create;

finalization
  theBridge.Free;

end.

