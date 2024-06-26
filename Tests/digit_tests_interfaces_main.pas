unit digit_tests_interfaces_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Digit_Bridge_Intf, Digit_Bridge_Impl;

const
  TestTakerName = 'Test Internal Taker';

type

  { TTestTakerParams }

  TTestTakerParams = class(TInterfacedObject, IDigIt_Params)
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Summary: PChar; stdcall;
  end;

  TTestTaker = class(TInterfacedObject, IDigIt_Taker)
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

    constructor Create;
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btReg: TButton;
    btUnreg: TButton;
    btLoad: TButton;
    btSave: TButton;
    btTest: TButton;
    btRegisterLibs: TButton;
    Memo1: TMemo;
    selDirDialog: TSelectDirectoryDialog;
    procedure btRegClick(Sender: TObject);
    procedure btUnregClick(Sender: TObject);
    procedure btTestClick(Sender: TObject);
    procedure btRegisterLibsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private


  public

  end;

var
  Form1: TForm1;
  test:TTestTaker;


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
     //Free;

     Result :=True;
  except

  end;
end;

function TTestTaker.Params: IDigIt_Params; stdcall;
begin
  Result:=nil;
end;

function TTestTaker.UI_Title(const AUI_Title: PChar): Integer; stdcall;
begin
  StrPCopy(AUI_Title, 'Test Internal Plugin Menu');
  Result:= Length(AUI_Title);
end;

function TTestTaker.UI_ImageIndex: Integer; stdcall;
begin
  Result:=-1;
end;

function TTestTaker.Preview(const AFileName: PChar): Integer; stdcall;
begin
  StrPCopy(AFileName, 'C:\Preview\AFilePath\'+TestTakerName);
  Result:= Length(AFileName);
end;

function TTestTaker.Take(const AFileName: PChar): Integer; stdcall;
begin
  StrPCopy(AFileName, 'C:\Take\AFilePath\'+TestTakerName);
  Result:= Length(AFileName);
end;

function TTestTaker.ReTake(const AFileName: PChar): Integer; stdcall;
begin
  StrPCopy(AFileName, 'C:\ReTake\AFilePath\'+TestTakerName);
  Result:= Length(AFileName);
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
  test :=TTestTaker.Create;
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

procedure TForm1.btTestClick(Sender: TObject);
var
   i:Integer;
   objTakers: TDigIt_Takers;
   curTaker: IDigIt_Taker;
   curImageFile: PChar;
   res: Integer;

begin
  objTakers := theBridge.Takers as TDigIt_Takers;

  if Assigned(objTakers)
  then for i:=0 to objTakers.Count-1 do
       begin
         curTaker :=objTakers.Taker[i];
         if Assigned(curTaker) then
         begin
           curImageFile:= StrAlloc(theBridge.Settings.GetMaxPCharSize);
           res :=curTaker.Take(curImageFile);
           Memo1.Lines.Add(objTakers.Name[i]+' '+curImageFile);
           StrDispose(curImageFile);
         end;
       end;
end;

procedure TForm1.btRegisterLibsClick(Sender: TObject);
begin
  if selDirDialog.Execute then
  begin
    Memo1.Lines.Add('***RegisterInPath***');
    Memo1.Lines.Add('   '+IntToStr(theBridge.Plugins.RegisterInPath(selDirDialog.FileName))+' New Plugins Registered');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //test :=TTestTaker.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //test.Free;
  //theBridge.Free;
end;

end.

