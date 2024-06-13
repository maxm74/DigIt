unit digit_tests_interfaces_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Digit_Bridge_Intf, Digit_Bridge_Impl;

const
  TestTakerName = 'Test Internal Taker';

type

  { TTestTakerParams }

  TTestTakerParams = class(TNoRefCountObject, IDigIt_Params)
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Summary: PChar; stdcall;
  end;

  TTestTaker = class(TNoRefCountObject, IDigIt_Taker)
    function Init: Boolean; stdcall;
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

    function RegisterName: PChar; stdcall;
    function Params: IDigIt_Params; stdcall;
    function UI_Title: PChar; stdcall;
    function UI_ImageIndex: Integer; stdcall;

     //Take a Picture and returns FileName
    function Preview: PChar; stdcall;
    function Take: PChar; stdcall;
    function ReTake: PChar; stdcall;

    constructor Create;
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btReg: TButton;
    btUnreg: TButton;
    btLoad: TButton;
    btSave: TButton;
    Button1: TButton;
    Button3: TButton;
    Memo1: TMemo;
    procedure btRegClick(Sender: TObject);
    procedure btUnregClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    test:TTestTaker;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TTestTakerParams }

function TTestTakerParams.GetFromUser: Boolean; stdcall;
begin

end;

function TTestTakerParams.Duplicate: IDigIt_Params; stdcall;
begin

end;

function TTestTakerParams.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
begin

end;

function TTestTakerParams.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
begin

end;

function TTestTakerParams.Summary: PChar; stdcall;
begin

end;

{ TTestTakerParams }

function TTestTaker.Init: Boolean; stdcall;
begin
  Result :=True;
end;

function TTestTaker.Enabled(AEnabled: Boolean): Boolean; stdcall;
begin

end;

function TTestTaker.Release: Boolean; stdcall;
begin
  Result :=False;

  try
     Free;

     Result :=True;
  except

  end;
end;

function TTestTaker.RegisterName: PChar; stdcall;
begin
  Result :=TestTakerName;
end;

function TTestTaker.Params: IDigIt_Params; stdcall;
begin
  Result:=nil;
end;

function TTestTaker.UI_Title: PChar; stdcall;
begin
  Result:=nil;
end;

function TTestTaker.UI_ImageIndex: Integer; stdcall;
begin
  Result:=-1;
end;

function TTestTaker.Preview: PChar; stdcall;
begin
  Result :='C:\ReTake\AFilePath\'+TestTakerName;
end;

function TTestTaker.Take: PChar; stdcall;
begin
  Result :='C:\ReTake\AFilePath\'+TestTakerName;
end;

function TTestTaker.ReTake: PChar; stdcall;
begin
  Result :='C:\ReTake\AFilePath\'+TestTakerName;
end;

constructor TTestTaker.Create;
begin
  inherited Create;
end;

destructor TTestTaker.Destroy;
begin
  inherited Destroy;
end;

{ TForm1 }

procedure TForm1.btRegClick(Sender: TObject);
var
   rf:longint;

begin
  theBridge.Takers.Register(TestTakerName, test);

  //rf :=test.RefCount;
end;

procedure TForm1.btUnregClick(Sender: TObject);
var
   rf:longint;

begin
  //theBridge.Takers.UnRegister(test);

  //rf :=test.RefCount;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
   i:Integer;
   objTakers: TDigIt_Bridge_Takers;
   curTaker: IDigIt_Taker;

begin
  objTakers := theBridge.Takers as TDigIt_Bridge_Takers;

  if Assigned(objTakers)
  then for i:=0 to objTakers.Count-1 do
       begin
         curTaker :=objTakers.Taker[i];
         if Assigned(curTaker)
         then Memo1.Lines.Add(Concat(curTaker.RegisterName, ' ', curTaker.Take));
       end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  test :=TTestTaker.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //test.Free;
end;

end.

