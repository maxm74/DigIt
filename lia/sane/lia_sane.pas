(*
*********************************
**           DigIt             **
** (s) 2013 Massimo Magnano    **
**                             **
*********************************
**  L.I.A. Sane Impl.          **
**    As Internal Module       **
*********************************
*)unit lia_sane;

{$mode objfpc}{$H+}

interface

uses
  Digit_Bridge,
  lia_sane_devices;

implementation

initialization
              Digit_Bridge.theBridge.Devices.Register_Device_Enum('Sane Linux', 'sane', TLia_Sane_Device_enum);

end.

