program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, Digit_Bridge_Intf, Digit_Bridge_Impl, DigIt_Types, DigIt_Counters, DigIt_Utils,
  Digit_Taker_Folder, DigIt_Form_Main, DigIt_Form_Templates, DigIt_Form_AnimAcquiring, Digit_Taker_Twain,
  Digit_Taker_Twain_SelectForm, Digit_Taker_Twain_SettingsForm, Digit_Taker_Twain_Types;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

