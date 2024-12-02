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
  Classes, SysUtils, Digit_Bridge_Intf, MM_OpenArrayList;

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

  { TDigIt_Sources }

  TSourceInfo = record
    Flags: DWord;
    Inst: IDigIt_Source;
  end;
  PSourceInfo = ^TSourceInfo;

  TDigIt_Sources = class(specialize TOpenArrayList<TSourceInfo, String>, IDigIt_Sources)
    function Register(const aName: PChar; const aClass: IDigIt_Source): Boolean; stdcall;
  protected
    rSelected: PSourceInfo;
    rSelectedName: String;
    rSelectedIndex: Integer;
    rSelectedParams: IDigIt_Params;

    function FreeElement(var aData: TSourceInfo): Boolean; override;

  public
    constructor Create;

    function Select(SourceName: String; GetUserParams: Boolean=False): Boolean; overload;
    function Select(SourceIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;

    function LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;

    property Selected: PSourceInfo read rSelected;
    property SelectedIndex: Integer read rSelectedIndex;
    property SelectedName: String read rSelectedName;
    property SelectedParams: IDigIt_Params read rSelectedParams;
  end;

  { TDigIt_Destinations }

  TDestinationInfo = record
    Flags: DWord;
    Inst: IDigIt_Destination;
  end;
  PDestinationInfo = ^TDestinationInfo;

  TDigIt_Destinations = class(specialize TOpenArrayList<TDestinationInfo, String>, IDigIt_Destinations)
    function Register(const aName: PChar; const aClass: IDigIt_Destination): Boolean; stdcall;
  protected
    rSelected: PDestinationInfo;
    rSelectedName: String;
    rSelectedIndex: Integer;
    rSelectedParams: IDigIt_Params;

    function FreeElement(var aData: TDestinationInfo): Boolean; override;

  public
    constructor Create;

    function Select(DestinationName: String; GetUserParams: Boolean=False): Boolean; overload;
    function Select(DestinationIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;

    function LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;

    property Selected: PDestinationInfo read rSelected;
    property SelectedIndex: Integer read rSelectedIndex;
    property SelectedName: String read rSelectedName;
    property SelectedParams: IDigIt_Params read rSelectedParams;
  end;

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
    rSources: TDigIt_Sources;
    rDestinations: TDigIt_Destinations;
    rPlugins: TDigIt_Plugins;

  public
    constructor Create;
    destructor Destroy; override;

    function Sources: IDigIt_Sources; stdcall;
    function Destinations: IDigIt_Destinations; stdcall;
    function Settings: IDigIt_Settings; stdcall;
    function Progress: IDigIt_Progress; stdcall;

    (*function Register(const aDisplayName: PChar;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean; stdcall;
    //function UnRegister(const aDisplayName: PChar) :Boolean; stdcall;
    *)

    //Internal Use only
    function SourcesImpl: TDigIt_Sources;
    function DestinationsImpl: TDigIt_Destinations;
    function SettingsImpl: TDigIt_Settings;
    function Plugins: TDigIt_Plugins;
  end;

var
   theBridge: TDigIt_Bridge = nil;

implementation

uses dynlibs, Masks, DigIt_Types, DigIt_Form_Progress;

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

{ TDigIt_Sources }

function TDigIt_Sources.FreeElement(var aData: TSourceInfo): Boolean;
begin
  Result:= False;
  try
     if (aData.Inst <> nil)
     then aData.Inst.Release;

     Result:= True;

  finally
    //MaxM: When the open array is freed the compiler frees the contents of the record using rtti,
    //      a very dangerous thing for us.
    FillChar(aData, SizeOf(aData), 0);
  end;
end;

constructor TDigIt_Sources.Create;
begin
  inherited Create;

  rSelected:= nil;
  rSelectedName:= '';
  rSelectedIndex:= -1;
  rSelectedParams:= nil;
end;

function TDigIt_Sources.Select(SourceName: String; GetUserParams: Boolean): Boolean;
var
   newSourceI: Integer;
   newSource: PSourceInfo =nil;

begin
  Result:= False;

  if (SourceName <> '') then
  try
     newSourceI:= FindByKey(SourceName);
     newSource:= Data[newSourceI];

     if (newSource <> nil) then
     begin
       if GetUserParams and
          not((newSource^.Inst.Params = nil) or newSource^.Inst.Params.GetFromUser)
       then exit;

       rSelected:= newSource;
       rSelectedParams :=newSource^.Inst.Params;
       rSelectedName:= SourceName;
       rSelectedIndex:= newSourceI;
       Result:= True;
      end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(SourceIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newSource: PSourceInfo =nil;

begin
  Result:= False;

  try
     newSource:= Data[SourceIndex];

     if (newSource <> nil) then
     begin
       if GetUserParams and
          not((newSource^.Inst.Params = nil) or newSource^.Inst.Params.GetFromUser)
       then exit;

       rSelected:= newSource;
       rSelectedParams :=newSource^.Inst.Params;
       rSelectedName:= rList[SourceIndex].Key;
       rSelectedIndex:= SourceIndex;
       Result:= True;
      end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;
begin
  Result:= False;
  if (rSelectedParams <> nil) and (XMLFileName <> '') and (XMLPath <> '') then
  try
     Result:= rSelectedParams.Load(PChar(XMLFileName), PChar(XMLPath));
     if Result then Result:= rSelectedParams.OnSet;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Register(const aName: PChar; const aClass: IDigIt_Source): Boolean; stdcall;
var
   newData: TSourceInfo;

begin
  Result:= False;
  if (aClass = nil) then exit;

  //If the Class cannot Init don't register it and Release
  if (aClass.Init)
  then begin
         newData.Inst:= aClass;
         Result:= (Add(aName, newData) > -1);
       end
  else aClass.Release;
end;
(*
function TDigIt_Sources.UnRegister(const aName: String): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rSourcesList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Sources.UnRegister(const aClass: IDigIt_Source): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aClass);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rSourcesList, r, 1);
    Result:= True;
  end;
end;
*)

{ TDigIt_Destinations }

function TDigIt_Destinations.FreeElement(var aData: TDestinationInfo): Boolean;
begin
  Result:= False;
  try
     if (aData.Inst <> nil)
     then aData.Inst.Release;

     Result:= True;

  finally
    //MaxM: When the open array is freed the compiler frees the contents of the record using rtti,
    //      a very dangerous thing for us.
    FillChar(aData, SizeOf(aData), 0);
  end;
end;

constructor TDigIt_Destinations.Create;
begin
  inherited Create;

  rSelected:= nil;
  rSelectedName:= '';
  rSelectedIndex:= -1;
  rSelectedParams:= nil;
end;

function TDigIt_Destinations.Select(DestinationName: String; GetUserParams: Boolean): Boolean;
var
   newDestinationI: Integer;
   newDestination: PDestinationInfo =nil;

begin
  Result:= False;

  if (DestinationName <> '') then
  try
     newDestinationI:= FindByKey(DestinationName);
     newDestination:= Data[newDestinationI];

     if (newDestination <> nil) then
     begin
       if GetUserParams and
          not((newDestination^.Inst.Params = nil) or newDestination^.Inst.Params.GetFromUser)
       then exit;

       rSelected:= newDestination;
       rSelectedParams :=newDestination^.Inst.Params;
       rSelectedName:= DestinationName;
       rSelectedIndex:= newDestinationI;
       Result:= True;
      end;

  except
    Result:= False;
  end;
end;

function TDigIt_Destinations.Select(DestinationIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newDestination: PDestinationInfo =nil;

begin
  Result:= False;

  try
     newDestination:= Data[DestinationIndex];

     if (newDestination <> nil) then
     begin
       if GetUserParams and
          not((newDestination^.Inst.Params = nil) or newDestination^.Inst.Params.GetFromUser)
       then exit;

       rSelected:= newDestination;
       rSelectedParams :=newDestination^.Inst.Params;
       rSelectedName:= rList[DestinationIndex].Key;
       rSelectedIndex:= DestinationIndex;
       Result:= True;
      end;

  except
    Result:= False;
  end;
end;

function TDigIt_Destinations.LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;
begin
  Result:= False;
  if (rSelectedParams <> nil) and (XMLFileName <> '') and (XMLPath <> '') then
  try
     Result:= rSelectedParams.Load(PChar(XMLFileName), PChar(XMLPath));
     if Result then Result:= rSelectedParams.OnSet;

  except
    Result:= False;
  end;
end;

function TDigIt_Destinations.Register(const aName: PChar; const aClass: IDigIt_Destination): Boolean; stdcall;
var
   newData: TDestinationInfo;

begin
  Result:= False;
  if (aClass = nil) then exit;

  //If the Class cannot Init don't register it and Release
  if (aClass.Init)
  then begin
         newData.Inst:= aClass;
         Result:= (Add(aName, newData) > -1);
       end
  else aClass.Release;
end;

(*
function TDigIt_Destinations.UnRegister(const aName: String): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rDestinationsList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Destinations.UnRegister(const aClass: IDigIt_Destination): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aClass);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rDestinationsList, r, 1);
    Result:= True;
  end;
end;
*)

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
  rSources:= TDigIt_Sources.Create;
  rDestinations:= TDigIt_Destinations.Create;
end;

destructor TDigIt_Bridge.Destroy;
begin
  rSources.Free;
  rSettings.Free;
  rPlugins.Free;
  rDestinations.free;

  inherited Destroy;
end;

function TDigIt_Bridge.Sources: IDigIt_Sources; stdcall;
begin
  Result:= rSources as IDigIt_Sources;
end;

function TDigIt_Bridge.Destinations: IDigIt_Destinations; stdcall;
begin
  Result:= rDestinations as IDigIt_Destinations;
end;

function TDigIt_Bridge.Settings: IDigIt_Settings; stdcall;
begin
  Result:= rSettings as IDigIt_Settings;
end;

function TDigIt_Bridge.Progress: IDigIt_Progress; stdcall;
begin
  Result:= DigIt_Progress as IDigIt_Progress;
end;

function TDigIt_Bridge.SourcesImpl: TDigIt_Sources;
begin
  Result:= rSources;
end;

function TDigIt_Bridge.DestinationsImpl: TDigIt_Destinations;
begin
  Result:= rDestinations;
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
  //theBridge.Free;   { #note -oMaxM : MainForm Free the Bridge otherwise the TwainSource does not send the message STOP to 32 bit Comm }

end.

