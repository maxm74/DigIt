program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Digit_Bridge_Intf, Digit_Bridge_Impl, DigIt_Types, DigIt_Counters, DigIt_Utils,
  Digit_Source_Folder,
  Digit_Source_Twain, Digit_Source_Twain_Types,
  DigIt_Form_Main, DigIt_Form_Templates, DigIt_Form_AnimAcquiring, DigIt_Dest_SaveFiles_SettingsForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

