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

const
  //Default Max PChar Size when passing/receiving parameters to the interface
  //use SetMaxPCharSize to increase the value
  DigIt_MaxPCharSize = 512;

  DigIt_PluginInfoProcName = 'DigIt_Plugin_Info';
  DigIt_PluginInitProcName = 'DigIt_Plugin_Init';
  DigIt_PluginReleaseProcName = 'DigIt_Plugin_Release';

type
  IDigIt_Bridge =interface;

  TDigIt_PluginInfo = packed record
    BridgeMinVer: Byte;
    Flags: Word;        { #note -oMaxM : Future use, like kind of Plugin, etc... }
    Name: String[32];
    Ver: String[5];
  end;

  // Library in plugins subdirectory must export this procedures:

    //   "DigIt_Plugin_Info" when get the plugin Info
    TDigIt_PluginInfoProc = function (var PluginInfo: TDigIt_PluginInfo): Boolean; stdcall;

    //   "DigIt_Plugin_Init" when the plugin is initialized
    //   "DigIt_Plugin_Release" when the plugin is released
    TDigIt_PluginInitProc = function (const digitBridge: IDigIt_Bridge): Boolean; stdcall;



  // ans this to "GetDisplayName"
  TDigIt_PluginNameProc = function :PChar; stdcall;

  IDigIt_Params = Interface
  ['{D101CADE-C69C-4929-A8DF-4E30A587BCB3}']
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Summary(const ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;
  end;

  IDigIt_Taker = Interface
  ['{D101CADE-C69C-4929-A8DF-699AC76DEE84}']
    function Flags: Word; stdcall; { #note -oMaxM : Future use, like kind of Taker, etc... }
    function Init: Boolean; stdcall;
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

    function Params: IDigIt_Params; stdcall;
    function UI_Title(const AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

     //Take a Picture and returns FileName
    function Preview(const AFileName: PChar): Integer; stdcall;
    function Take(const AFileName: PChar): Integer; stdcall;
    function ReTake(const AFileName: PChar): Integer; stdcall;
  end;

  IDigIt_Takers = Interface
  ['{D101CADE-C69C-4929-A8DF-8344B540E2A2}']
    function Register(const aName: PChar; const aClass: IDigIt_Taker) :Boolean; stdcall;
    //function UnRegister(const aClass : IDigIt_Taker) :Boolean; stdcall; { #note 5 -oMaxM : Implementer unregist the Class}
  end;

  IDigIt_Settings = Interface
  ['{D101CADE-C69C-4929-A8DF-6B103B8BCBDF}']
    //Buffers Limits Variables
    function GetMaxPCharSize: DWord; stdcall;
    function SetMaxPCharSize(NewSize: DWord): DWord; stdcall;

    //Path consts
    function Path_Temp: PChar; stdcall;
    function Path_Config: PChar; stdcall;
    function Path_Application: PChar; stdcall;
  end;

  IDigIt_Bridge = Interface
  ['{D101CADE-C69C-4929-A8DF-C779BE1D5762}']
    function Takers: IDigIt_Takers; stdcall;
    function Settings: IDigIt_Settings; stdcall;
    //ToolBar, Menù, PostProcessing Image, Export Image format, etc... { #todo -oMaxM : Maybe Tomorrow }

    (*
    //General Register/Unregister, inside the InitProc/ReleaseProc you can Register/UnRegister in Takers, etc...
    function Register(const aDisplayName: PChar;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean; stdcall;
    //function UnRegister(const aDisplayName :PChar) :Boolean; stdcall; { #note 5 -oMaxM : This way each library can uninstall  the others ???? }
    *)
  end;

implementation

end.

