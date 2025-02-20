program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LCLVersion, DefaultTranslator, LCLTranslator,
  Forms, lazcontrols, DigIt_Bridge_Intf, DigIt_Bridge_Impl, DigIt_Types, DigIt_Counters, DigIt_Utils,
  DigIt_Form_Main, DigIt_Form_Templates,
  DigIt_Sources, DigIt_Destinations, DigIt_Form_Progress, DigIt_Form_BuildDuplex, DigIt_Source_WIA, Digit_Source_Folder;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.CreateForm(TDigIt_Progress, DigIt_Progress);
  Application.Run;
end.

