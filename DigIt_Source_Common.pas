(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Common Source, Internal Source is derived from this classes              **
*******************************************************************************)

unit DigIt_Source_Common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Digit_Bridge_Intf;

type
  TDigIt_Source_Common = class;

  { TDigIt_Source_Common_Params }

  TDigIt_Source_Common_Params  = class(TNoRefCountObject, IDigIt_Params)
  protected
    rOwner: TDigIt_Source_Common;

    function GetSummary: String; virtual;

  public
    function Init: Boolean; stdcall; virtual;
    function Release: Boolean; stdcall; virtual;

    function GetFromUser: Boolean; stdcall; virtual;
    function Duplicate: IDigIt_Params; stdcall; virtual;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; virtual;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; virtual;
    function Summary(out ASummary: PChar): Integer; stdcall; virtual;

    function OnSelected: Boolean; stdcall; virtual;

    constructor Create(AOwner: TDigIt_Source_Common); virtual;
  end;
  TDigIt_Source_Common_ParamsClass = class of TDigIt_Source_Common_Params;

  { TDigIt_Source_Common }

  TDigIt_Source_Common = class(TNoRefCountObject, IDigIt_Interface, IDigIt_Source)
  protected
    rParams: IDigIt_Params;
    rEnabled: Boolean;

    function GetParamsClass: TDigIt_Source_Common_ParamsClass; virtual;
    function CreateParamsFromUser: IDigIt_Params; virtual;

    function GetName: String; virtual;

  public
    countTakes: Integer;

    constructor Create;
    destructor Destroy; override;

    //IDigIt_Interface Implementation
    function Flags: TDigItInterfaceKind; stdcall; virtual;
    function Init: Boolean; stdcall; virtual;
    function Release: Boolean; stdcall; virtual;
    function Enabled: Boolean; stdcall; virtual;
    function setEnabled(AEnabled: Boolean): Boolean; stdcall; virtual;

    function Params: IDigIt_Params; stdcall; virtual;
    function Params_New: IDigIt_Params; stdcall; virtual;
    function Params_Set(const AParams: IDigIt_Params): Boolean; stdcall; virtual;

    function UI_Title(out AUI_Title: PChar): Integer; stdcall; virtual;
    function UI_ImageIndex: Integer; stdcall; virtual;

    //IDigIt_Source
                                                       //Take a Picture and returns FileName/s
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall; virtual;
    procedure Clear; stdcall; virtual;
 end;


implementation

{ TDigIt_Source_Common_Params }

function TDigIt_Source_Common_Params.GetSummary: String;
begin
  Result:= '';
end;

function TDigIt_Source_Common_Params.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Common_Params.Release: Boolean; stdcall;
begin
  Result:= True;
  Free;
end;

function TDigIt_Source_Common_Params.GetFromUser: Boolean; stdcall;
begin
  Result:= False;
end;

function TDigIt_Source_Common_Params.Duplicate: IDigIt_Params; stdcall;
begin
  Result:= nil;
end;

function TDigIt_Source_Common_Params.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Common_Params.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Common_Params.Summary(out ASummary: PChar): Integer; stdcall;
begin
  ASummary:= StrNew(PChar(GetSummary));
  Result:= Length(ASummary);
end;

function TDigIt_Source_Common_Params.OnSelected: Boolean; stdcall;
begin
  Result:= True;
end;

constructor TDigIt_Source_Common_Params.Create(AOwner: TDigIt_Source_Common);
begin
  inherited Create;

  rOwner:= AOwner;
end;

{ TDigIt_Source_Common }

function TDigIt_Source_Common.GetParamsClass: TDigIt_Source_Common_ParamsClass;
begin
  Result:= TDigIt_Source_Common_Params;
end;

function TDigIt_Source_Common.CreateParamsFromUser: IDigIt_Params;
begin
  try
     //Create New Params Class
     Result:= Params_New;

     //Try to Set as Current Params if fail Release it
     if not(Params_Set(Result)) and (Result <> nil) then
     begin
       Result.Release;
       Result:= nil;
     end;

     if (Result <> nil) then
     begin
       //Try to Get Params from User if fail Release it
       if not(Result.GetFromUser) then
       begin
         Result.Release;
         Result:= nil;
       end;
     end;

  except
     Result:= nil;
  end;
end;

function TDigIt_Source_Common.GetName: String;
begin
  Result:= Self.ClassName;
end;

constructor TDigIt_Source_Common.Create;
begin
  inherited Create;

  rParams:= nil;
  rEnabled:= True;
  countTakes:= -1;
end;

destructor TDigIt_Source_Common.Destroy;
begin
  inherited Destroy;

  if (rParams <> nil) then rParams.Release;
end;

function TDigIt_Source_Common.Flags: TDigItInterfaceKind; stdcall;
begin
  Result:= diSourceStd;
end;

function TDigIt_Source_Common.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Common.Enabled: Boolean; stdcall;
begin
  Result:= rEnabled;
end;

function TDigIt_Source_Common.setEnabled(AEnabled: Boolean): Boolean; stdcall;
begin
  rEnabled:= AEnabled;
  Result:= rEnabled;
end;

function TDigIt_Source_Common.Release: Boolean; stdcall;
begin
  Free;
  Result:= True;
end;

function TDigIt_Source_Common.Params: IDigIt_Params; stdcall;
begin
  Result:= rParams;
end;

function TDigIt_Source_Common.Params_New: IDigIt_Params; stdcall;
begin
  Result:= GetParamsClass.Create(Self);
end;

function TDigIt_Source_Common.Params_Set(const AParams: IDigIt_Params): Boolean; stdcall;
begin
  rParams:= AParams;
  Result:= True;
end;

function TDigIt_Source_Common.UI_Title(out AUI_Title: PChar): Integer; stdcall;
begin
  AUI_Title:= StrNew(PChar(GetName));
  Result:= Length(AUI_Title);
end;

function TDigIt_Source_Common.UI_ImageIndex: Integer; stdcall;
begin
  Result:= -1;
end;

function TDigIt_Source_Common.Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;
begin
  Result:= 0;
  aData:= nil;
  if (rParams = nil) then rParams:= CreateParamsFromUser;
end;

procedure TDigIt_Source_Common.Clear; stdcall;
begin
end;

end.

