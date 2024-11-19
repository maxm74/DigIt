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
  DigIt_Source_Kind         = $0000000F;
  DigIt_Source_TakeDataType = $00000FF0;

  //
  //DigIt_Sources_Kind constants
  //

  //Standard Source
  DigIt_Source_Kind_STD      = $00000000;

  //
  //DigIt_Source_TakeDataType constants
  //

  //Source return a Filename in Preview/Take/ReTake as a PChar
  TakeData_FileName = $00000000;

  //Source return a Filename in Preview/Take/ReTake as a PChar (IDigIt_Settings.GetMaxPCharSize maximum size)
  TakeData_ARRAY = $00000001;

  (* Maybe Tomorrow
  //Source return a Pointer to a Bitmap in Preview/Take/ReTake as a Pointer (IDigIt_Settings.GetMaxBufferSize maximum size)
  DigIt_Source_TakeData_BITMAP   = $00000002;
  *)

  //Default Max PChar Size when passing/receiving parameters to the interface
  //use SetMaxPCharSize to increase the value
  DigIt_MaxPCharSize = 512;

  DigIt_PluginInfoProcName = 'DigIt_Plugin_Info';
  DigIt_PluginInitProcName = 'DigIt_Plugin_Init';
  DigIt_PluginReleaseProcName = 'DigIt_Plugin_Release';

type
  //Interface Kind
  TDigItInterfaceKind = (
    diSourceStd, //Standard Source
    diDestinationStd
  );

  //Data Type
  TDigItDataType = (
    diDataType_FileName, //Filename as a PChar
    diDataType_Bitmap    //Pointer to a Bitmap
  );

  IDigIt_Bridge =interface;

  TDigIt_PluginInfo = packed record
    BridgeMinVer: Byte;
    Flags: DWord;        { #note -oMaxM : Future use, like kind of Plugin, etc... }
    Name: String[32];
    Ver: String[5];
  end;

  // Library in plugins subdirectory must export this procedures:

    //   "DigIt_Plugin_Info" when get the plugin Info
    TDigIt_PluginInfoProc = function (var PluginInfo: TDigIt_PluginInfo): Boolean; stdcall;

    //   "DigIt_Plugin_Init" when the plugin is initialized
    //   "DigIt_Plugin_Release" when the plugin is released
    TDigIt_PluginInitProc = function (const digitBridge: IDigIt_Bridge): Boolean; stdcall;



    //   "GetDisplayName"
    TDigIt_PluginNameProc = function :PChar; stdcall;

  IDigIt_ROArray = Interface
  ['{D101CADE-C69C-4929-A8DF-1386A8BF4D21}']
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;
  end;

  IDigIt_RWArray = Interface(IDigIt_ROArray)
  ['{D101CADE-C69C-4929-A8DF-1386A8BF4D22}']
    function Put(const aIndex: DWord; var aData: Pointer): Boolean; stdcall;
  end;

  IDigIt_Params = Interface
  ['{D101CADE-C69C-4929-A8DF-4E30A587BCB3}']
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Summary(const ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;
  end;

  IDigIt_Interface = Interface
    function Flags: TDigItInterfaceKind; stdcall;
    function Init: Boolean; stdcall;
    function Release: Boolean; stdcall;
    function Enabled: Boolean; stdcall;
    function setEnabled(AEnabled: Boolean): Boolean; stdcall;

    function Params: IDigIt_Params; stdcall;
    function UI_Title(const AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;
  end;

  //
  //DigIt_Source_TakeAction enum
  //
  DigIt_Source_TakeAction = (takeActPreview, takeActTake, takeActReTake);

  IDigIt_Source = Interface(IDigIt_Interface)
  ['{D101CADE-C69C-4929-A8DF-699AC76DEE00}']
    //Take a Picture and returns it on aData according with aDataType
    // (if Result is > 1 then aData is a IDigIt_ROArray interface)
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;

    //Clear internal Data, usually used when we finish processing files
    procedure Clear; stdcall;
  end;

  IDigIt_Sources = Interface
  ['{D101CADE-C69C-4929-A8DF-8344B540E20F}']
    function Register(const aName: PChar; const aClass: IDigIt_Source) :Boolean; stdcall;
    //function UnRegister(const aClass : IDigIt_Source) :Boolean; stdcall; { #note 5 -oMaxM : Implementer unregist the Class}
  end;


  TDigit_CounterData = packed record
      rValue,
      rValue_Previous: Integer;
      rValue_StringPost,
      rValue_StringPre: PChar;
      rValue_StringDigits: Byte;
    end;

  IDigIt_Destination = Interface(IDigIt_Interface)
  ['{D101CADE-C69C-4929-A8DF-699AC76DEE10}']
    //Put cropped image (stored in the temporary file AFileName) and
    //return a preview File name in APreviewFileName (Result is the String size)
    { #todo 5 -oMaxM : should also return a value to refer to this cropped area in future operations such as rotate/etc  }
    function Put(counterValue: TDigit_CounterData;
                 MaxDataSize: DWord; const AFileName, APreviewFileName: PChar): DWord; stdcall;
  end;

  IDigIt_Destinations = Interface
  ['{D101CADE-C69C-4929-A8DF-8344B540E21F}']
    function Register(const aName: PChar; const aClass: IDigIt_Destination) :Boolean; stdcall;
    //function UnRegister(const aClass : IDigIt_Destination) :Boolean; stdcall; { #note 5 -oMaxM : Implementer unregist the Class}
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
    function Sources: IDigIt_Sources; stdcall;
    function Destinations: IDigIt_Destinations; stdcall;
    function Settings: IDigIt_Settings; stdcall;
    //ToolBar, Menù, PostProcessing Image, etc... { #todo -oMaxM : Maybe Tomorrow }

    (*
    //General Register/Unregister, inside the InitProc/ReleaseProc you can Register/UnRegister in Sources, etc...
    function Register(const aDisplayName: PChar;
                      const InitProc: TDigIt_PluginInitProc;
                      const ReleaseProc: TDigIt_PluginInitProc) :Boolean; stdcall;
    //function UnRegister(const aDisplayName :PChar) :Boolean; stdcall; { #note 5 -oMaxM : This way each library can uninstall  the others ???? }
    *)
  end;

implementation

end.

