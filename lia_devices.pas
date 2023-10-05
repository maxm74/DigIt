(*
*********************************
**           DigIt             **
** (s) 2013 Massimo Magnano    **
**                             **
*********************************
**  L.I.A. Abstarct Devices    **
*********************************
*)
unit lia_devices;

{$mode objfpc}{$H+}

interface

uses
  DigIt_types, lia_properties;

type
    TLia_Capabilities = set of (canTake);

    { TLia_Device }

    TLia_Device = class
    protected
          rName :TDigIt_Name;
          rUID  :TDigIt_Name;

    public
          function Name :TDigIt_Name;
          function UID  :TDigIt_Name;

          constructor Create(); virtual; abstract;
          destructor Destroy(); virtual; abstract;

          function Properties : TLia_Property_enum; virtual; abstract;
          function Capabilities : TLia_Capabilities; virtual; abstract;

          procedure Take(); virtual; abstract;
    end;

    { TLia_Device_enum }

    TLia_Device_enum = class
    public
      constructor Create(); virtual; abstract;
      destructor Destroy(); virtual; abstract;

      function Find_First : TLia_Device; virtual; abstract;
      function Find_Next  : TLia_Device; virtual; abstract;
    end;
    TLia_Device_enum_Classes = class of TLia_Device_enum;

implementation

{ TLia_Device_enum }


{ TLia_Device }

function TLia_Device.Name: TDigIt_Name;
begin
     result := Self.rName;
end;

function TLia_Device.UID: TDigIt_Name;
begin
     result := Self.rUID;
end;

end.

