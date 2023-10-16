(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Taker                                                             **
*******************************************************************************)

unit Digit_Taker_Twain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Digit_Bridge, Digit_Taker_Twain_Types,
  Twain, DelphiTwain, DelphiTwain_VCL;

type
  { TDigIt_Taker_Twain }
  TDigIt_Taker_Twain = class(TDigIt_Taker)
  private
    rTwain:TDelphiTwain;
    rTwain_SourceI:Integer;
    i:Integer;
  protected

    function getTwain: TDelphiTwain;
    procedure FreeTwain;
    procedure SetTwainTranferMode;
   // procedure TwainTwainAcquire(Sender: TObject; const Index: Integer; Image: TBitmap; var Cancel: Boolean);

    property Twain: TDelphiTwain read getTwain;
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

implementation

uses Digit_Types, Dialogs, BGRABitmapTypes;

{ TDigIt_Taker_Twain }

function TDigIt_Taker_Twain.getTwain: TDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TDelphiTwain.Create;
   // rTwain.OnTwainAcquire := @TwainTwainAcquire;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
  end;

  Result :=rTwain;
end;

procedure TDigIt_Taker_Twain.FreeTwain;
begin
  if (rTwain<>nil)
  then rTwain.Free;
end;

procedure TDigIt_Taker_Twain.SetTwainTranferMode;
begin
end;

(*
procedure TDigIt_Taker_Twain.TwainTwainAcquire(Sender: TObject; const Index: Integer; Image: TBitmap;
  var Cancel: Boolean);
begin
  //  ImageHolder.Picture.Bitmap.Assign(Image);
    Cancel := True;//Only want one image
end;
*)

constructor TDigIt_Taker_Twain.Create(aParams: TPersistent);
begin
  rTwain:=nil;
  inherited Create(aParams);
end;

destructor TDigIt_Taker_Twain.Destroy;
begin
  FreeTwain;

  //Delete previous scanned file
  if FileExists(TempDir+'twain_'+IntToStr(i-1)+'.bmp')
  then DeleteFile(TempDir+'twain_'+IntToStr(i-1)+'.bmp');

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
  try
     Result :=Take;
  finally
  end;
end;

function TDigIt_Taker_Twain.Take: String;
begin
  try
     Result :='';

     //Delete previous scanned file
     if FileExists(TempDir+'twain_'+IntToStr(i-1)+'.bmp')
     then DeleteFile(TempDir+'twain_'+IntToStr(i-1)+'.bmp');

     if Assigned(Twain.SelectedSource) then
     begin
       Twain.SelectedSource.Loaded := TRUE;
       Twain.SelectedSource.ShowUI := False;//display interface
       Twain.SelectedSource.Modal:=False;
       Twain.SelectedSource.TransferMode:=ttmFile;
       Twain.SelectedSource.SetupFileTransfer(TempDir+'twain_'+IntToStr(i)+'.bmp', tfBMP);
       Twain.SelectedSource.EnableSource(False, True);
       Result :=TempDir+'twain_'+IntToStr(i)+'.bmp';
       Inc(i);
     end;
  finally
  end;
end;

function TDigIt_Taker_Twain.ReTake: String;
begin
  Result :=Take;
end;

function TDigIt_Taker_Twain.Params_GetFromUser: Boolean;
begin
  Result :=False;
  if (rParams=nil) then exit;
  try
     //Load source manager
     Twain.SourceManagerLoaded :=True;
     Twain.SelectSource;  { #todo 10 -oMaxM : Use an internal Method so we can select 32bit Scanners via IPC }
     Result :=Assigned(Twain.SelectedSource);

     if Result then
     with TDigIt_Taker_TwainParams(rParams) do
     begin
       IPC_Scanner:=False;
       Manufacturer :=Twain.SelectedSource.Manufacturer;
       ProductFamily :=Twain.SelectedSource.ProductFamily;
       ProductName :=Twain.SelectedSource.ProductName;
     end;
  finally
  end;
end;

procedure TDigIt_Taker_Twain.Params_Set(newParams: TPersistent);
begin
  rParams :=newParams;
  Twain.SourceManagerLoaded :=True;
  with TDigIt_Taker_TwainParams(rParams) do
  begin
    { #todo 10 -oMaxM : Use an internal Method so we can select 32bit Scanners via IPC }
    rTwain_SourceI:=Twain.FindSource(Manufacturer, ProductFamily, ProductName);

    if (rTwain_SourceI=-1)
    then begin
           { #todo 1 -oMaxM : Scanner not find, A Message to User with Retry }
           Twain.SelectSource;
         end
    else Twain.SelectedSourceIndex:=rTwain_SourceI;

    if Assigned(Twain.SelectedSource) then
    begin
      Manufacturer :=Twain.SelectedSource.Manufacturer;
      ProductFamily :=Twain.SelectedSource.ProductFamily;
      ProductName :=Twain.SelectedSource.ProductName;
    end;
  end;
end;

initialization
  theBridge.Takers.Register(TDigIt_Taker_Twain.RegisterName, TDigIt_Taker_Twain);

end.

