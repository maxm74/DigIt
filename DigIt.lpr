program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LCLVersion, SysUtils,
  Forms, lazcontrols,
  DigIt_Types, DigIt_Bridge_Intf, DigIt_Bridge_Impl,
  DigIt_Settings, DigIt_Sources, DigIt_Session, DigIt_Utils,
  DigIt_Source_Common,
  Digit_Source_WIA, Digit_Source_Twain,
  Digit_Source_Folder,
  DigIt_Destinations,
  DigIt_Form_Main, Digit_Bridge_Impl_UI_Form;

{$R *.res}

begin
  TranslateLanguage(false);
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

