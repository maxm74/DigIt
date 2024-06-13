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
    Name: String;
    LibHandle: TLibHandle;  //If =0 then InitProc and ReleaseProc refer to statically linked procedures
    InitProc,
    ReleaseProc: TDigIt_PluginInitProc;
  end;

  { TDigIt_Bridge_Plugins }

  TDigIt_Bridge_Plugins = class(TObject)
  protected
    rPluginsList: array of TPluginInfo;

    function Find(const aName:String): Integer;

  public
    constructor Create;
    destructor Destroy; override;

    function Register(const aDisplayName :String;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean;
    function UnRegister(const aDisplayName :String) :Boolean;

    function GetPlugin(const aName: String) : TPluginInfo;
    function GetPlugin(index : integer) : TPluginInfo;
    function GetCount : Integer;

    property Count : integer read GetCount;
    property Plugin [const aName:String] : TPluginInfo read GetPlugin;
  end;

  TTakerInfo = record
    Name: String;
    Inst: IDigIt_Taker;
  end;

  { TDigIt_Bridge_Takers }

  TDigIt_Bridge_Takers = class(TNoRefCountObject, IDigIt_Takers)
  protected
    rTakersList: array of TTakerInfo;

    function Find(const aName:String): Integer; overload;
    function Find(const aClass : IDigIt_Taker): Integer; overload;

    function GetTaker(const aName:String) : IDigIt_Taker;
    function GetTaker(index : Integer) : IDigIt_Taker;
    function GetCount : Integer;

  public
    constructor Create;
    destructor Destroy; override;

    function Register(const aName :PChar; const aClass : IDigIt_Taker) :Boolean; stdcall;
    function UnRegister(const aName :String) :Boolean;
    function UnRegister(const aClass : IDigIt_Taker) :Boolean;

    property Count : integer read GetCount;

    property TakerByName [const aName:String] : IDigIt_Taker read GetTaker;
    property Taker [const aIndex:Integer] : IDigIt_Taker read GetTaker;
  end;

  { TDigIt_Bridge }

  TDigIt_Bridge = class(TNoRefCountObject, IDigIt_Bridge)
  protected
    rTakers : TDigIt_Bridge_Takers;
    rPlugins: TDigIt_Bridge_Plugins;

  public
    constructor Create;
    destructor Destroy; override;

    function Takers :IDigIt_Takers; stdcall;

    function Register(const aDisplayName :PChar;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean; stdcall;
    function UnRegister(const aDisplayName :PChar) :Boolean; stdcall;
  end;

var
   theBridge : TDigIt_Bridge = nil;

implementation

{ TDigIt_Bridge_Plugins }

{ #todo 10 -oMaxM : Same kind of implementation of Takers List }
function TDigIt_Bridge_Plugins.Find(const aName: String): Integer;
begin

end;

constructor TDigIt_Bridge_Plugins.Create;
begin

end;

destructor TDigIt_Bridge_Plugins.Destroy;
begin
  inherited Destroy;
end;

function TDigIt_Bridge_Plugins.Register(const aDisplayName: String; const InitProc: TDigIt_PluginInitProc;
  const ReleaseProc: TDigIt_PluginInitProc): Boolean;
begin

end;

function TDigIt_Bridge_Plugins.UnRegister(const aDisplayName: String): Boolean;
begin

end;

function TDigIt_Bridge_Plugins.GetPlugin(const aName: String): TPluginInfo;
begin

end;

function TDigIt_Bridge_Plugins.GetPlugin(index: integer): TPluginInfo;
begin

end;

function TDigIt_Bridge_Plugins.GetCount: Integer;
begin

end;

{ TDigIt_Bridge_Takers }

function TDigIt_Bridge_Takers.Find(const aName: String): Integer;
var
  i : integer;

begin
  Result :=-1;
  for i:=0 to Length(rTakersList)-1 do
    if (rTakersList[i].Name = aName) then
    begin
      Result :=i; break;
    end;
end;

function TDigIt_Bridge_Takers.Find(const aClass: IDigIt_Taker): Integer;
var
  i : integer;

begin
  Result :=-1;
  for i:=0 to Length(rTakersList)-1 do
    if (rTakersList[i].Inst = aClass) then
    begin
      Result :=i; break;
    end;
end;

function TDigIt_Bridge_Takers.GetTaker(const aName: String): IDigIt_Taker;
var
  i : integer;

begin
  Result :=nil;
  for i:=0 to Length(rTakersList)-1 do
    if (rTakersList[i].Name = aName) then
    begin
      Result :=rTakersList[i].Inst; break;
    end;
end;

function TDigIt_Bridge_Takers.GetTaker(Index: integer): IDigIt_Taker;
begin
  if (index >= 0) and (index < Length(rTakersList))
  then Result := rTakersList[index].Inst
  else Result := nil;
end;

function TDigIt_Bridge_Takers.GetCount: Integer;
begin
  Result := Length(rTakersList);
end;

constructor TDigIt_Bridge_Takers.Create;
begin
  inherited Create;

  SetLength(rTakersList, 0);
end;

destructor TDigIt_Bridge_Takers.Destroy;
var
   i:Integer;

begin
  { #todo 10 -oMaxM : Free the Instances?  }
  for i:=0 to Length(rTakersList)-1 do
    try
    if Assigned(rTakersList[i].Inst)
    then rTakersList[i].Inst.Release;
    except
    end;

  try
     SetLength(rTakersList, 0);
  except
  end;

  inherited Destroy;
end;

function TDigIt_Bridge_Takers.Register(const aName: PChar; const aClass: IDigIt_Taker): Boolean; stdcall;
begin
  result :=False;

  (*
  if not(aClass is IDigIt_Taker)
  then raise Exception.Create('Taker '+aName+' invalid Class');
  *)

  if (Find(aName) > -1)
  then raise Exception.Create('Taker '+aName+' already registered');

  SetLength(rTakersList, Length(rTakersList)+1);

  rTakersList[Length(rTakersList)-1].Name:=aName;
  rTakersList[Length(rTakersList)-1].Inst :=aClass;

  result :=True;
end;

function TDigIt_Bridge_Takers.UnRegister(const aName: String): Boolean;
var
   r  :integer;

begin
  result :=false;

  r :=Find(aName);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    if Assigned(rTakersList[r].Inst)
    then rTakersList[r].Inst.Release;

    Delete(rTakersList, r, 1);
  end;

  result :=true;
end;

function TDigIt_Bridge_Takers.UnRegister(const aClass: IDigIt_Taker): Boolean;
var
   r  :integer;

begin
  result :=false;

  r :=Find(aClass);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    if Assigned(rTakersList[r].Inst)
    then rTakersList[r].Inst.Release;

    Delete(rTakersList, r, 1);
  end;

  result :=true;
end;

{ TDigIt_Bridge }

constructor TDigIt_Bridge.Create;
begin
  inherited Create;

  rTakers := TDigIt_Bridge_Takers.Create;
end;

destructor TDigIt_Bridge.Destroy;
begin
  FreeAndNil(rTakers);

  inherited Destroy;
end;

function TDigIt_Bridge.Takers: IDigIt_Takers; stdcall;
begin
  Result := rTakers;
end;

function TDigIt_Bridge.Register(const aDisplayName: PChar; const InitProc: TDigIt_PluginInitProc;
  const ReleaseProc: TDigIt_PluginInitProc): Boolean; stdcall;
begin

end;

function TDigIt_Bridge.UnRegister(const aDisplayName: PChar): Boolean; stdcall;
begin

end;

initialization
  theBridge := TDigIt_Bridge.Create;

finalization
  FreeAndNil(theBridge);
  //theBridge :=nil;

end.

