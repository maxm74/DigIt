(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Bridge to the Engine                                                     **
*******************************************************************************)

unit Digit_Bridge_Intf;

{$mode objfpc}{$H+}
//{$interfaces corba}

interface

type
  IDigIt_Bridge =interface;

  // Library in plugins subdirectory must export this procedures
  //   "DigIt_Init" when the plugin is initialized
  //   "DigIt_Release" when the plugin is released
  TDigIt_PluginInitProc = function (const digitBridge: IDigIt_Bridge): Boolean; stdcall;

  // ans this to "GetDisplayName"
  TDigIt_PluginNameProc = function :PChar; stdcall;

  IDigIt_Params = Interface
  ['{D101FEDE-C69C-473C-9C36-4E30A587BCB3}']
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Summary: PChar; stdcall;
  end;

  IDigIt_Taker = Interface
  ['{D101FEDE-C69C-4595-8676-699AC76DEE84}']
    function Init: Boolean; stdcall;
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

    function RegisterName: PChar; stdcall;
    function Params: IDigIt_Params; stdcall;
    function UI_Title: PChar; stdcall;
    function UI_ImageIndex: Integer; stdcall;

     //Take a Picture and returns FileName
    function Preview: PChar; stdcall;
    function Take: PChar; stdcall;
    function ReTake: PChar; stdcall;
  end;

  IDigIt_Takers = Interface
  ['{D101FEDE-C69C-4929-A8DF-8344B540E2A2}']
    function Register(const aName :PChar; const aClass : IDigIt_Taker) :Boolean; stdcall;
    //function UnRegister(const aClass : IDigIt_Taker) :Boolean; stdcall; { #note 5 -oMaxM : Implementer unregist the Class}
  end;

  IDigIt_Bridge = Interface
  ['{D101FEDE-C69C-4CD6-BD14-C779BE1D5762}']
    function Takers :IDigIt_Takers; stdcall;
    //ToolBar, Menù, PostProcessing Image, Export Image format, etc... { #todo -oMaxM : Maybe Tomorrow }


    //General Register/Unregister, inside the InitProc/ReleaseProc you can Register/UnRegister in Takers, etc...
    function Register(const aDisplayName :PChar;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean; stdcall;
    //function UnRegister(const aDisplayName :PChar) :Boolean; stdcall; { #note 5 -oMaxM : This way each library can uninstall  the others ???? }
  end;

implementation

end.

