program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols,
  DigIt_Types, Digit_Bridge, DigIt_Counters, DigIt_Utils,
  Digit_Taker_Folder, Digit_Taker_Twain_Types, Digit_Taker_Twain,
  DigIt_Form_Main, DigIt_Form_Templates, DigIt_Form_AnimAcquiring;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

