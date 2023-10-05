(*
*********************************
**           DigIt             **
** (s) 2013 Massimo Magnano    **
**                             **
*********************************
**  L.I.A. Abstarct Properties **
*********************************
*)
unit lia_properties;

{$mode objfpc}{$H+}

interface
uses
  DigIt_types;

type
    { TLia_Property }

    TLia_Property = class
      protected
            rName : TDigIt_Name;

      public
            function Name : TDigIt_Name;

            constructor Create(); virtual; abstract;
            destructor Destroy(); virtual; abstract;

            function getValue(): Variant; virtual; abstract;
            procedure setValue(aValue :Variant); virtual; abstract;
            function Valid(aValue :Variant): boolean; virtual; abstract;
    end;

    { TLia_Property_enum }

    TLia_Property_enum = class
    public
          constructor Create(); virtual; abstract;
          destructor Destroy(); virtual; abstract;

          function Find_First : TLia_Property; virtual; abstract;
          function Find_Next  : TLia_Property; virtual; abstract;

          function Serialize  : PChar; virtual; abstract;
          procedure DeSerialize(aValue : PChar); virtual; abstract;
    end;


implementation


{ TLia_Property }

function TLia_Property.Name: TDigIt_Name;
begin
     result := Self.rName;
end;

end.

