library DigIt_tests_plugin;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Digit_Bridge_Intf;

type

  { TTestLibTaker }

  TTestLibTaker = class(TNoRefCountObject, IDigIt_Taker)
  protected
    Digit_Bridge: IDigIt_Bridge;

  public
    function Init: Boolean; stdcall;
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

    function Params: IDigIt_Params; stdcall;
    function UI_Title(const AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

     //Take a Picture and returns FileName
    function Preview(const AFileName: PChar): Integer; stdcall;
    function Take(const AFileName: PChar): Integer; stdcall;
    function ReTake(const AFileName: PChar): Integer; stdcall;

    constructor Create(ADigit_Bridge: IDigIt_Bridge);
    destructor Destroy; override;
  end;

var
   TestLibTaker: TTestLibTaker=nil;

function DigIt_Plugin_Info(var PluginInfo: TDigIt_PluginInfo): Boolean; stdcall;
begin
  PluginInfo.BridgeMinVer:= 0;
  PluginInfo.Name:= 'Test Plugin Lib';
  PluginInfo.Ver:= '1.0';
  Result:= True;
end;

function DigIt_Plugin_Init(const digitBridge: IDigIt_Bridge): Boolean; stdcall;
begin
  Result:= False;
  if (digitBridge <> nil) then
  begin
    TestLibTaker:= TTestLibTaker.Create(digitBridge);
    digitBridge.Takers.Register('Test Taker in Lib', TestLibTaker);

    Result:= True;
  end;
end;

function DigIt_Plugin_Release(const digitBridge: IDigIt_Bridge): Boolean; stdcall;
begin
  Result:= False;
  if (digitBridge <> nil) then
  begin
    if (TestLibTaker <> nil) then TestLibTaker.Free;

    Result:= True;
  end;
end;

exports
  DigIt_Plugin_Release,
  DigIt_Plugin_Init,
  DigIt_Plugin_Info;

{ TTestLibTaker }

function TTestLibTaker.Init: Boolean; stdcall;
begin
  Result :=True;
end;

function TTestLibTaker.Enabled(AEnabled: Boolean): Boolean; stdcall;
begin
  Result :=True;
end;

function TTestLibTaker.Release: Boolean; stdcall;
begin
  Result :=True;
end;

function TTestLibTaker.Params: IDigIt_Params; stdcall;
begin
  Result:= Nil;
end;

function TTestLibTaker.UI_Title(const AUI_Title: PChar): Integer; stdcall;
begin
  StrCopy(AUI_Title, 'Test Plugin in Lib Menu');
  Result:= Length(AUI_Title);
end;

function TTestLibTaker.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 4;
end;

function TTestLibTaker.Preview(const AFileName: PChar): Integer; stdcall;
var
   test: String;

begin
  test:= Digit_Bridge.Settings.Path_Config+'testlib.jpg';
  StrPCopy(AFileName, test);
  Result:= Length(AFileName);
end;

function TTestLibTaker.Take(const AFileName: PChar): Integer; stdcall;
var
   test: String;

begin
  test:= Digit_Bridge.Settings.Path_Config+'testlib.jpg';
  StrPCopy(AFileName, test);
  Result:= Length(AFileName);
end;

function TTestLibTaker.ReTake(const AFileName: PChar): Integer; stdcall;
var
   test: String;

begin
  test:= Digit_Bridge.Settings.Path_Config+'testlib.jpg';
  StrPCopy(AFileName, test);
  Result:= Length(AFileName);
end;

constructor TTestLibTaker.Create(ADigit_Bridge: IDigIt_Bridge);
begin
  inherited Create;

  Digit_Bridge:= ADigit_Bridge;
end;

destructor TTestLibTaker.Destroy;
begin
  inherited Destroy;
end;

begin
end.

