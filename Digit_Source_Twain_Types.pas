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
  Classes, SysUtils, DelphiTwain;

const
  TWAIN32_SERVER_EXE = 'DigIt_Twain32Comm';
  TWAIN32_SERVER_NAME = 'DigIt_Twain32CommServer';

  MSG_TWAIN32_TIMEOUT = 100; //Input=mtSync_Integer Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_STOP = 101; //Input=mtSync_Null Output=mtSync_Integer (Magic Mess)
  MSG_TWAIN32_LIST = 102; //Input=mtSync_Null Output=mtSync_Pointer (array of TW_IDENTITY)
  MSG_TWAIN32_FIND = 103; //Input=mtSync_Var (TW_IDENTITY) Output=mtSync_Integer
  MSG_TWAIN32_OPEN = 104; //Input=mtSync_Integer Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_USERINTERFACE = 105; //Input=mtSync_Var (TW_USERINTERFACE) Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_PARAMS_SET = 106; //Input=mtSync_Var (TTwainParams) Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_PARAMS_GET = 107; //Input=mtSync_Null Output=mtSync_Stream (TTwainParamsCapabilities)
  MSG_TWAIN32_PREVIEW = 108; //Input=mtSync_String  Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_TAKE = 109; //Input=mtSync_String  Output=mtSync_Integer (Boolean)

  RES_TWAIN32_STOPPED = $0CACA; //:-( A message for you in Italian...

type
  TTwainScannerInfo = record
    IPC_Scanner: Boolean;
    Manufacturer,
    ProductFamily,
    ProductName: String;
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

implementation

end.
