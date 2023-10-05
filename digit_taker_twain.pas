(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Folder Taker                                                             **
*******************************************************************************)

unit Digit_Taker_Twain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Digit_Bridge, Twain;

type

  { TDigIt_Taker_Twain }
  TDigIt_Taker_Twain = class(TDigIt_Taker)
  protected
    xFiles :TStringList; //A Citation
    lastFile :Integer;
    lastFolder:String;

  public
    constructor Create(aParams :TPersistent); override;
    destructor Destroy; override;

    class function RegisterName: String; override;
    class function Params_GetClass : TPersistentClass; override;
    function Params_GetFromUser: Boolean; override;
    procedure Params_Set(newParams: TPersistent); override;
    class function UI_Title: String; override;
    class function UI_ImageIndex: Integer; override;
    function UI_Params_Summary: String; override;

    function Preview: String; override;
    function Take: String; override;
    function ReTake: String; override;
  end;

  TDigIt_Taker_TwainParams = class(TPersistent)
  protected
    rLastSource :TW_IDENTITY;

  published
  end;

implementation

uses Dialogs, DelphiTwain, BGRABitmapTypes;

{ TDigIt_Taker_Twain }

constructor TDigIt_Taker_Twain.Create(aParams: TPersistent);
begin
  inherited Create(aParams);
end;

destructor TDigIt_Taker_Twain.Destroy;
begin
  inherited Destroy;
end;

class function TDigIt_Taker_Twain.RegisterName: String;
begin
  Result :='TDigIt_Taker_Twain';
end;

class function TDigIt_Taker_Twain.Params_GetClass: TPersistentClass;
begin
  result :=TDigIt_Taker_TwainParams;
end;

class function TDigIt_Taker_Twain.UI_Title: String;
begin
  Result :='Twain Source'; { #todo 2 -oMaxM : Usare Risorse per la Traduzione }
end;

class function TDigIt_Taker_Twain.UI_ImageIndex: Integer;
begin
  Result :=2;
end;

function TDigIt_Taker_Twain.UI_Params_Summary: String;
begin
  Result :='';
end;

function TDigIt_Taker_Twain.Preview: String;
begin
end;

function TDigIt_Taker_Twain.Take: String;
begin
end;

function TDigIt_Taker_Twain.ReTake: String;
begin
end;

function TDigIt_Taker_Twain.Params_GetFromUser: Boolean;
begin
  Result :=False;
  if (rParams=nil) then exit;
  try
     with TDigIt_Taker_TwainParams(rParams) do
     begin
     end;
  finally
  end;
end;

procedure TDigIt_Taker_Twain.Params_Set(newParams: TPersistent);
begin
  rParams :=newParams;
end;

initialization
  theBridge.Takers.Register(TDigIt_Taker_Twain.RegisterName, TDigIt_Taker_Twain);

end.

