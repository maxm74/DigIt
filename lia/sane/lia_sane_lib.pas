(*
*********************************
**           DigIt             **
** (s) 2013 Massimo Magnano    **
**                             **
*********************************
**  L.I.A. Sane Impl.          **
**    As External Module       **
*********************************
*) unit lia_sane_lib;

{$mode objfpc}{$H+}

interface

uses
  Digit_Bridge,
  lia_sane_devices;

procedure Init(digitBridge :TDigIt_Bridge);
procedure Done(digitBridge :TDigIt_Bridge);

implementation

procedure Init(digitBridge: TDigIt_Bridge);
begin
     digitBridge.Devices.Register_Device_Enum('Sane Linux', 'sane', TLia_Sane_Device_enum)
end;

procedure Done(digitBridge: TDigIt_Bridge);
begin
     //
end;

end.

