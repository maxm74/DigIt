(*
*********************************
**           DigIt             **
** (s) 2013 Massimo Magnano    **
**                             **
*********************************
**  L.I.A. Sane Devices Impl.  **
*********************************
*)
unit lia_sane_devices;

{$mode objfpc}{$H+}

interface
uses
  DigIt_types, lia_devices, lia_properties;

type
    { TLia_Sane_Device }

    TLia_Sane_Device = class(TLia_Device)
    public
          constructor Create(); override;
          destructor Destroy(); override;

          function Properties : TLia_Property_enum; override;
          function Capabilities : TLia_Capabilities; override;

          procedure Take(); override;
    end;

    { TLia_Sane_Device_enum }

    TLia_Sane_Device_enum = class(TLia_Device_enum)
    public
      constructor Create(); override;
      destructor Destroy(); override;

      function Find_First : TLia_Sane_Device; override;
      function Find_Next  : TLia_Sane_Device; override;
    end;

implementation

{ TLia_Sane_Device }

constructor TLia_Sane_Device.Create;
begin

end;

destructor TLia_Sane_Device.Destroy;
begin

end;

function TLia_Sane_Device.Properties: TLia_Property_enum;
begin

end;

function TLia_Sane_Device.Capabilities: TLia_Capabilities;
begin

end;

procedure TLia_Sane_Device.Take;
begin

end;

{ TLia_Sane_Device_enum }

constructor TLia_Sane_Device_enum.Create;
begin

end;

destructor TLia_Sane_Device_enum.Destroy;
begin

end;

function TLia_Sane_Device_enum.Find_First: TLia_Sane_Device;
begin

end;

function TLia_Sane_Device_enum.Find_Next: TLia_Sane_Device;
begin

end;

end.

