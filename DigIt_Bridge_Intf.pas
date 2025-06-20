(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Interfaces of Main Bridge                                                **
*******************************************************************************)

unit Digit_Bridge_Intf;

{$mode objfpc}{$H+}

interface

uses MM_OpenArrayList;

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

  //Source return a Filename in Preview/Take/ReTake as a PChar
  TakeData_ARRAY = $00000001;

  (* Maybe Tomorrow
  //Source return a Pointer to a Bitmap in Preview/Take/ReTake as a Pointer (IDigIt_Settings.GetMaxBufferSize maximum size)
  DigIt_Source_TakeData_BITMAP   = $00000002;
  *)

  DigIt_PluginInfoProcName = 'DigIt_Plugin_Info';
  DigIt_PluginInitProcName = 'DigIt_Plugin_Init';
  DigIt_PluginReleaseProcName = 'DigIt_Plugin_Release';

  //Settings Path Consts: High Byte = Category, Low Byte = Path
  ID_Path_Application = $00;
  ID_Path_Config      = $10;
  ID_Path_Temp        = $20;

  ID_Path_Session          = $30;
  ID_Path_Session_Scan     = $31;
  ID_Path_Session_Pictures = $32;


type
  //Interface Kind
  TDigItInterfaceKind = (
    diSourceStd, //Standard Source
    diDestinationStd
  );

  //Data Type
  TDigItDataType = (
    diDataType_FileName,      //A Single Filename (PChar)
    diDataType_FileNameArray, //An IDigIt_Array of Filenames (PChar)
    diDataType_Bitmap         //Pointer to a Bitmap
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

  //ALWAYS Return a Copy (using StrNew, etc) of your internal String DigIt will free after using it
  IDigIt_ArrayR_PChars = interface(specialize IOpenArrayR<PChar>)
  ['{D101CADE-C69C-4929-A8DF-1386A8BF4D21}']
  end;

  IDigIt_ArrayW_PChars = Interface(specialize IOpenArrayW<PChar>)
  ['{D101CADE-C69C-4929-A8DF-1386A8BF4D22}']
  end;

  IDigIt_Params = Interface
  ['{D101CADE-C69C-4929-A8DF-4E30A587BCB3}']
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
    function Summary(out ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;
  end;

  IDigIt_Interface = Interface
    function Flags: TDigItInterfaceKind; stdcall;
    function Init: Boolean; stdcall;
    function Release: Boolean; stdcall;
    function Enabled: Boolean; stdcall;
    function setEnabled(AEnabled: Boolean): Boolean; stdcall;

    function Params: IDigIt_Params; stdcall;
    function UI_Title(out AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;
  end;

  //
  //DigIt_Source_TakeAction enum
  //
  DigIt_Source_TakeAction = (takeActPreview, takeActTake);

  IDigIt_Source = Interface(IDigIt_Interface)
  ['{D101CADE-C69C-4929-A8DF-699AC76DEE00}']
    //Take a Picture and returns it on aData according with aDataType
    // (if Result is > 1 then aData is a IDigIt_ROArray interface)
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;

    //Clear internal Data, usually used when we finish processing files
    procedure Clear; stdcall;
  end;

  //Get Sub Items so the User can select directly the Device from Menù
  IDigIt_Source_Items = interface(specialize IOpenArrayR<PChar>)
  ['{D101CADE-C69C-4929-A8DF-699AC76DEE01}']

    //Select Sub Item, if called with aIndex=-1 then Show User Dialog to Select it
    function Select(aIndex: Integer): Boolean; stdcall;

    //Is aIndex Sub Item the Current Selected?
    function Selected(aIndex: Integer): Boolean; stdcall;
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

  { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
  (*
  IDigIt_Destination = Interface(IDigIt_Interface)
  ['{D101CADE-C69C-4929-A8DF-699AC76DEE10}']
    //Put cropped image (stored in the temporary file AFileName) and
    //return a preview File name in APreviewFileName (Result is the String size) ??
    { #todo 5 -oMaxM : should also return a value to refer to this cropped area in future operations such as rotate/etc  }
    function Put(counterValue: TDigit_CounterData;
                 MaxDataSize: DWord; const AFileName, APreviewFileName: PChar): DWord; stdcall;
  end;

  IDigIt_Destinations = Interface
  ['{D101CADE-C69C-4929-A8DF-8344B540E21F}']
    function Register(const aName: PChar; const aClass: IDigIt_Destination) :Boolean; stdcall;
    //function UnRegister(const aClass : IDigIt_Destination) :Boolean; stdcall; { #note 5 -oMaxM : Implementer unregist the Class}
  end;
  *)

  IDigIt_Settings = Interface
  ['{D101CADE-C69C-4929-A8DF-6B103B8BCBDF}']
    //Path Consts: High Byte = Category, Low Byte = Path
    function Path(const APathID: Word): PChar; stdcall;
  end;

  IDigIt_ProgressCallback = Interface
  ['{D101CADE-C69C-4929-A8DF-2344BF2350B0}']
    procedure ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;
  end;

  IDigIt_Progress = Interface
  ['{D101CADE-C69C-4929-A8DF-2344BF2350B1}']
    procedure SetTotalVisible(AVisible: Boolean); stdcall;
    procedure SetTotalLabel(const ALabel: PChar); stdcall;
    procedure SetTotal(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
    procedure SetTotalCaption(const ACaption: PChar); stdcall;
    procedure SetTotalValue(AValue: Integer); stdcall;

    procedure SetCurrentVisible(AVisible: Boolean); stdcall;
    procedure SetCurrent(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
    procedure SetCurrentCaption(const ACaption: PChar); stdcall;
    procedure SetCurrentValue(AValue: Integer); stdcall;

    function Cancelled: Boolean; stdcall;

    procedure SetEventCallBack(const AEventCallBack: IDigIt_ProgressCallback); stdcall;

    procedure Show(const ACaption: PChar); stdcall;
    procedure Hide; stdcall;
  end;

  IDigIt_Bridge = Interface
  ['{D101CADE-C69C-4929-A8DF-C779BE1D5762}']
    function Sources: IDigIt_Sources; stdcall;

    { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
    //function Destinations: IDigIt_Destinations; stdcall;

    function Settings: IDigIt_Settings; stdcall;
    function Progress: IDigIt_Progress; stdcall;

    //ToolBar, Menù, Pre/PostProcessing Image, etc... { #todo -oMaxM : Maybe Tomorrow }
  end;

implementation

end.

