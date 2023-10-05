(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Bridge to the Engine                                                     **
*******************************************************************************)

unit Digit_Bridge;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, DigIt_types;

type

  { TDigIt_Taker }

  TDigIt_Taker = class
  protected
    rName   :String;
    rParams :TPersistent;

  public
    constructor Create(aParams :TPersistent); virtual;
    destructor Destroy; override;

    class function RegisterName: String; virtual; abstract;
    class function Params_GetClass: TPersistentClass; virtual; abstract;
    function Params_GetFromUser: Boolean; virtual; abstract;
    procedure Params_Set(newParams: TPersistent); virtual; abstract;
    function Params_Get:TPersistent;
    class function UI_Title: String; virtual; abstract;
    class function UI_ImageIndex: Integer; virtual; abstract;
    function UI_Params_Summary: String; virtual; abstract;

     //Take a Picture and returns Filename
    function Preview: String; virtual; abstract;
    function Take: String; virtual; abstract;
    function ReTake: String; virtual; abstract;
  end;
  TDigIt_TakerClasses = class of TDigIt_Taker;

  { TDigIt_Bridge_Takers }

  TDigIt_Bridge_Takers = class
  protected
    rTakersList:TStringList;

    function GetTaker(const aName:String) : TDigIt_TakerClasses;
 //   function GetTaker(const aName:String; var takerIndex:Integer) : TDigIt_Taker;
    function GetCount : integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Register(aName :String; aClass : TDigIt_TakerClasses) :boolean;
    function UnRegister(aName :String) :boolean;

    function GetTaker(index : integer) : TDigIt_TakerClasses;

    property Count : integer read GetCount;
    property Taker [const aName:String] : TDigIt_TakerClasses read GetTaker;
  end;

  TDigIt_Bridge = class
  protected
    rTakers : TDigIt_Bridge_Takers;

  public
    constructor Create;
    destructor Destroy; override;

    property Takers : TDigIt_Bridge_Takers read rTakers;
  end;


var
   theBridge : TDigIt_Bridge = nil;

implementation

{ TDigIt_Taker }

constructor TDigIt_Taker.Create(aParams: TPersistent);
begin
  inherited Create;
  if (aParams=nil)
  then begin
         if assigned(Self.Params_GetClass)
         then rParams :=Self.Params_GetClass.Create;
       end
  else rParams :=aParams;
end;

destructor TDigIt_Taker.Destroy;
begin
  if (rParams<>nil)
  then rParams.Free;

  inherited Destroy;
end;

function TDigIt_Taker.Params_Get: TPersistent;
begin
  Result :=rParams;
end;

{ TDigIt_Bridge_Takers }

function TDigIt_Bridge_Takers.GetTaker(const aName: String): TDigIt_TakerClasses;
var
   r : integer;

begin
  if rTakersList.Find(aName, r)
  then Result :=TDigIt_TakerClasses(rTakersList.Objects[r])
  else Result :=nil;
end;

(*
function TDigIt_Bridge_Takers.GetTaker(const aName: String;  var takerIndex: Integer): TDigIt_Taker;
begin
  takerIndex := rTakersList.Count;
  repeat
    dec (takerIndex);
  until (takerIndex < 0) or (compareText(TDigIt_Taker(rTakersList[takerIndex]).rName, aName) = 0);
  if (takerIndex >= 0)
  then result := TDigIt_Taker(rTakersList[takerIndex])
  else result := nil;
end;
*)

function TDigIt_Bridge_Takers.GetTaker(index: integer): TDigIt_TakerClasses;
begin
  if (index >= 0) and (index < rTakersList.count)
  then result := TDigIt_TakerClasses(rTakersList.Objects[index])
  else result := nil;
end;

function TDigIt_Bridge_Takers.GetCount: integer;
begin
  result := rTakersList.Count;
end;

constructor TDigIt_Bridge_Takers.Create;
begin
  inherited Create;

  rTakersList :=TStringList.Create;
  rTakersList.Sorted:=True;
  rTakersList.OwnsObjects:=False;
end;

destructor TDigIt_Bridge_Takers.Destroy;
begin
  rTakersList.Free;

  inherited Destroy;
end;

function TDigIt_Bridge_Takers.Register(aName :String; aClass : TDigIt_TakerClasses): boolean;
begin
  result :=false;

  if assigned(Taker[aName])
  then raise Exception.Create('Taker '+aName+' already registered');

  rTakersList.AddObject(aName, TObject(aClass));

  result :=true;
end;

function TDigIt_Bridge_Takers.UnRegister(aName: String): boolean;
var
   it :TDigIt_Taker;
   r  :integer;
begin
  result :=false;

  if rTakersList.Find(aName, r) then
  begin
    rTakersList.Delete(r);
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
  rTakers.Free;

  inherited Destroy;
end;

initialization
  theBridge := TDigIt_Bridge.Create;

finalization
  if (theBridge<>nil) then FreeAndNil(theBridge);

end.

