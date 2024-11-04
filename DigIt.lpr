program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, DigIt_Bridge_Intf, DigIt_Bridge_Impl, DigIt_Types, DigIt_Counters, DigIt_Utils,
  DigIt_Form_Main, DigIt_Form_Templates,
DigIt_Sources, DigIt_Destinations;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

