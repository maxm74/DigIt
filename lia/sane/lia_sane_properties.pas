(*
**********************************
**           DigIt              **
** (s) 2013 Massimo Magnano     **
**                              **
**********************************
**  L.I.A. Sane Properties Impl.**
**********************************
*)
unit lia_sane_properties;

{$mode objfpc}{$H+}

interface
uses
  DigIt_types, lia_properties;

type
    { TLia_Property }

    { TLia_Sane_Property }

    TLia_Sane_Property = class(TLia_Property)
      public
            constructor Create(); override;
            destructor Destroy(); override;

            function getValue(): Variant; override;
            procedure setValue(aValue :Variant); override;
            function Valid(aValue :Variant): boolean; override;
    end;

    { TLia_Property_enum }

    { TLia_Sane_Property_enum }

    TLia_Sane_Property_enum = class(TLia_Property_enum)
    public
          constructor Create(); override;
          destructor Destroy(); override;

          function Find_First : TLia_Property; override;
          function Find_Next  : TLia_Property; override;

          function Serialize  : PChar; override;
          procedure DeSerialize(aValue : PChar); override;
    end;


implementation


{ TLia_Sane_Property }

constructor TLia_Sane_Property.Create;
begin

end;

destructor TLia_Sane_Property.Destroy;
begin

end;

function TLia_Sane_Property.getValue: Variant;
begin

end;

procedure TLia_Sane_Property.setValue(aValue: Variant);
begin

end;

function TLia_Sane_Property.Valid(aValue: Variant): boolean;
begin

end;

{ TLia_Sane_Property_enum }

constructor TLia_Sane_Property_enum.Create;
begin

end;

destructor TLia_Sane_Property_enum.Destroy;
begin

end;

function TLia_Sane_Property_enum.Find_First: TLia_Property;
begin

end;

function TLia_Sane_Property_enum.Find_Next: TLia_Property;
begin

end;

function TLia_Sane_Property_enum.Serialize: PChar;
begin

end;

procedure TLia_Sane_Property_enum.DeSerialize(aValue: PChar);
begin

end;

end.

