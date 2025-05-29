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
  Classes, SysUtils, Laz2_XMLCfg, Digit_Bridge_Intf, MM_OpenArrayList, DigIt_Settings;

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
    function Select(SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;
    function Select(aXML: TRttiXMLConfig; XMLRoot_Path: String; var newSourceName: String): Boolean; overload;

    function Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;
    function Save(aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;
    //function LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;

    property Selected: PSourceInfo read rSelected;
    property SelectedIndex: Integer read rSelectedIndex;
    property SelectedName: String read rSelectedName;
    property SelectedParams: IDigIt_Params read rSelectedParams;
  end;

  { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
  (*

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

  *)

  { TDigIt_Bridge }

  TDigIt_Bridge = class(TNoRefCountObject, IDigIt_Bridge)
  protected
    rSettings: TDigIt_Settings;
    rSources: TDigIt_Sources;
    //rDestinations: TDigIt_Destinations;
    rPlugins: TDigIt_Plugins;

  public
    constructor Create;
    destructor Destroy; override;

    function Sources: IDigIt_Sources; stdcall;
    //function Destinations: IDigIt_Destinations; stdcall;
    function Settings: IDigIt_Settings; stdcall;
    function Progress: IDigIt_Progress; stdcall;

    //Internal Use only
    property SourcesImpl: TDigIt_Sources read rSources;
    property SettingsImpl: TDigIt_Settings read rSettings;
    property Plugins: TDigIt_Plugins read rPlugins;
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
     if (newSourceI > -1) then Result:= Select(newSourceI, -1, GetUserParams);

     (* oldcode
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
      *)

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newSource: PSourceInfo =nil;
   curSourceItems: IDigIt_Source_Items;

begin
  Result:= False;

  try
     newSource:= Data[SourceIndex];

     if (newSource <> nil) then
     begin
       if GetUserParams then
       begin
         if (newSource^.Inst is IDigIt_Source_Items)
         then Result:= (newSource^.Inst as IDigIt_Source_Items).Select(newSourceSubIndex)
         else Result:= True;

         if Result then Result:= (newSource^.Inst.Params <> nil) and newSource^.Inst.Params.GetFromUser;
       end
       else Result:= True;

       if Result then
       begin
         rSelected:= newSource;
         rSelectedParams :=newSource^.Inst.Params;
         rSelectedName:= rList[SourceIndex].Key;
         rSelectedIndex:= SourceIndex;
       end;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(aXML: TRttiXMLConfig; XMLRoot_Path: String; var newSourceName: String): Boolean;
begin
  Result:= False;

  if (aXML <> nil) then
  try
     //Load a New Source and its Params
     newSourceName:= aXML.GetValue(XMLRoot_Path+'Source/Name', '');

     if Select(newSourceName) then
     begin
        Result:= True;

        if (rSelectedParams <> nil) then
        begin
          Result:= rSelectedParams.Load(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));
          if Result then Result:= rSelectedParams.OnSet;
        end;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean;
var
   curSource: PSourceInfo =nil;

begin
  Result:= False;

  if (aXML <> nil) then
  try
     curSource:= Data[SourceIndex];

     if (curSource <> nil) then
     begin
       //Save ASource Source and its Params
       aXML.SetValue(XMLRoot_Path+'Source/Name', rList[SourceIndex].Key);
       aXML.DeletePath(XMLRoot_Path+'Source/Params/');

       if SaveParams and (curSource^.Inst <> nil)
       then curSource^.Inst.Params.Save(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));

       Result:= True;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Save(aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean;
begin
  Result:= False;

  if (aXML <> nil) then
  try
     //Save Selected Source and its Params
     aXML.SetValue(XMLRoot_Path+'Source/Name', rSelectedName);
     aXML.DeletePath(XMLRoot_Path+'Source/Params/');

     if SaveParams and
       (rSelected <> nil) and (rSelected^.Inst <> nil)
     then rSelected^.Inst.Params.Save(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));

     Result:= True;

  except
    Result:= False;
  end;
end;

(* oldcode
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
*)

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

{ #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
(*

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
*)

{ TDigIt_Bridge }

constructor TDigIt_Bridge.Create;
begin
  inherited Create;

  rPlugins:= TDigIt_Plugins.Create;
  rSettings:= TDigIt_Settings.Create;
  rSources:= TDigIt_Sources.Create;
  //rDestinations:= TDigIt_Destinations.Create;
end;

destructor TDigIt_Bridge.Destroy;
begin
  rSources.Free;
  rSettings.Free;
  rPlugins.Free;
  //rDestinations.free;

  inherited Destroy;
end;

function TDigIt_Bridge.Sources: IDigIt_Sources; stdcall;
begin
  Result:= rSources as IDigIt_Sources;
end;

(*
function TDigIt_Bridge.Destinations: IDigIt_Destinations; stdcall;
begin
  Result:= rDestinations as IDigIt_Destinations;
end;
*)

function TDigIt_Bridge.Settings: IDigIt_Settings; stdcall;
begin
  Result:= rSettings as IDigIt_Settings;
end;

function TDigIt_Bridge.Progress: IDigIt_Progress; stdcall;
begin
  Result:= DigIt_Progress as IDigIt_Progress;
end;

initialization
  theBridge:= TDigIt_Bridge.Create;

finalization
  //theBridge.Free;   { #note -oMaxM : MainForm Free the Bridge otherwise the TwainSource does not send the message STOP to 32 bit Comm }

end.

