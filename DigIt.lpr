program DigIt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  LCLVersion, DefaultTranslator, LCLTranslator, gettext, Translations, SysUtils,
  Forms, lazcontrols,
  DigIt_Types, DigIt_Bridge_Intf, DigIt_Bridge_Impl,
  DigIt_Settings, DigIt_Sources, DigIt_Session, DigIt_Utils,
  Digit_Source_WIA, Digit_Source_Twain,
  Digit_Source_Folder,
  DigIt_Destinations,
  DigIt_Form_Main, Digit_Bridge_Impl_UI_Form;

procedure TranslateLCL;
var
  PODirectory, Lang, FallbackLang: String;
begin
  PODirectory:=  ExtractFilePath(Application.ExeName) +'languages/';
  Lang:='';
  FallbackLang:='';
  GetLanguageIDs(Lang,FallbackLang);
  Translations.TranslateUnitResourceStrings('LCLStrConsts',
                      PODirectory+'lclstrconsts.%s.po',Lang,FallbackLang);
(*  Translations.TranslateUnitResourceStrings('lr_const',
                      PODirectory+'lr_const.%s.po',Lang,FallbackLang);*)
end;

{$R *.res}

begin
  TranslateLCL;
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDigIt_Main, DigIt_Main);
  Application.Run;
end.

