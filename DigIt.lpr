program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, pexpandpanels,
  DigIt_Form_Main, Digit_Bridge, Digit_Taker_Twain, DigIt_Utils, DigIt_Counters, DigIt_Types, Digit_Taker_Folder, 
  DigIt_Form_Templates, Digit_Taker_Twain_Types, Digit_Taker_Twain_SelectForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

