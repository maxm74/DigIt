(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Source Data Types and Inter Process Comunication                    **
*******************************************************************************)
unit Digit_Source_Twain_Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Twain, DelphiTwain;

const
  TWAIN32_SERVER_EXE = 'DigIt_Twain32Comm';
  TWAIN32_SERVER_NAME = 'DigIt_Twain32CommServer';

  MSG_TWAIN32_TIMEOUT = 100; //Input=mtData_Integer Output=mtData_Integer (Boolean)
  MSG_TWAIN32_STOP = 101; //Input=mtData_Null Output=mtData_Integer (Magic Mess)
  MSG_TWAIN32_LIST = 102; //Input=mtData_Null Output=mtData_Pointer (array of TW_IDENTITY)
  MSG_TWAIN32_FIND = 103; //Input=mtData_Var (TW_IDENTITY) Output=mtData_Integer
  MSG_TWAIN32_OPEN = 104; //Input=mtData_Var (TW_IDENTITY) Output=mtData_Integer (Boolean)
  MSG_TWAIN32_USERINTERFACE = 105; //Input=mtData_Var (TW_USERINTERFACE) Output=mtData_Integer (Boolean)
  MSG_TWAIN32_PARAMS_SET = 106; //Input=mtData_Var (TTwainParams) Output=mtData_Integer (Boolean)
  MSG_TWAIN32_PARAMS_GET = 107; //Input=mtData_Null Output=mtData_Stream (TTwainParamsCapabilities)
  MSG_TWAIN32_PREVIEW = 108; //Input=mtData_String  Output=mtData_Integer (Boolean)
  MSG_TWAIN32_TAKE = 109; //Input=mtData_String  Output=mtData_Integer (Boolean)

type
  TTwainDeviceInfo = record
    IPC: Boolean;
    Manufacturer,          { Manufacturer name, e.g. "Hewlett-Packard" }
    ProductFamily,         { Product family name, e.g. "ScanJet" }
    ProductName: TW_STR32; { Product name, e.g. "ScanJet Plus" }
  end;

  TTwainParams = packed record
    PaperFeed: TTwainPaperFeeding;
    PaperSize: TTwainPaperSize;
    Resolution,
    Contrast,
    Brightness: Single;
    BitDepth: Integer;
    PixelType:TTwainPixelType;
  end;

  TTwainParamsCapabilities = packed record
    PaperFeedingSet: TTwainPaperFeedingSet;
    //PaperFeedDefault: TTwainPaperFeeding;
    PaperSizeSet: TTwainPaperSizeSet;
    PixelType:TTwainPixelTypeSet;
    PixelTypeDefault:TTwainPixelType;
    PaperSizeDefault: TTwainPaperSize;
    ResolutionDefault: Single;
    BitDepthDefault,
    ResolutionArraySize,
    BitDepthArraySize: Integer;

    //Array MUST be at the end so then 32bit server can write up to BitDepthArraySize with a single write
    ResolutionArray: TTwainResolution;
    BitDepthArray: TArrayInteger;
  end;

//Comparing by "Unique Id" given by Twain It's useless because it's not unique and it always changes
function DeviceInfoDifferent(A, B: TTwainDeviceInfo): Boolean; overload;
function DeviceInfoDifferent(A, B: TW_IDENTITY): Boolean; overload;
function DeviceInfoDifferent(A: TTwainDeviceInfo; B: TW_IDENTITY): Boolean; overload;

implementation

function DeviceInfoDifferent(A, B: TTwainDeviceInfo): Boolean;
begin
  Result:= (A.IPC <> B.IPC) or (A.ProductName <> B.ProductName) or
           (A.ProductFamily <> B.ProductFamily) or (A.Manufacturer <> B.Manufacturer);
end;

function DeviceInfoDifferent(A, B: TW_IDENTITY): Boolean;
begin
  Result:= (A.ProductName <> B.ProductName) or
           (A.ProductFamily <> B.ProductFamily) or (A.Manufacturer <> B.Manufacturer);
end;

function DeviceInfoDifferent(A: TTwainDeviceInfo; B: TW_IDENTITY): Boolean;
begin
  Result:= (A.ProductName <> B.ProductName) or
           (A.ProductFamily <> B.ProductFamily) or (A.Manufacturer <> B.Manufacturer);
end;

end.

