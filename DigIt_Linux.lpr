program DigIt_Linux;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LCLVersion, DefaultTranslator, LCLTranslator, Forms, lazcontrols,
  DigIt_Types, DigIt_Bridge_Intf, DigIt_Bridge_Impl,
  DigIt_Settings, DigIt_Sources, DigIt_Utils,
  Digit_Source_Folder,
  DigIt_Destinations,
  DigIt_Form_Main, DigIt_Form_Progress;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='DigIt';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.CreateForm(TDigIt_Progress, DigIt_Progress);
  Application.Run;
end.

