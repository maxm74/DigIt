(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Taker Data Types and Inter Process Comunication                    **
*******************************************************************************)
unit Digit_Taker_Twain_Types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DelphiTwain;

const
  TWAIN32_SERVER_EXE = 'DigIt_Twain32Comm.exe';
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
  TTwainParams = packed record
    PaperFeed: TTwainPaperFeeding;
    PaperSize: TTwainPaperSize;
    Resolution,
    Contrast,
    Brightness: Extended;
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
    ResolutionDefault: Extended;
    BitDepthDefault,
    ResolutionArraySize,
    BitDepthArraySize: Integer;

    //Array MUST be at the end so then 32bit server can write up to BitDepthArraySize with a single write
    ResolutionArray: TTwainResolution;
    BitDepthArray: TArrayInteger;
  end;

  TDigIt_Taker_TwainParams = class(TPersistent)
  protected
    rIPC_Scanner: Boolean;
    rManufacturer,
    rProductFamily,
    rProductName: String;
    rTwainParams: TTwainParams;

  public
    property TwainParams: TTwainParams read rTwainParams; //Used in IPC to set 32bit Scanner capabilities

  published
    property IPC_Scanner:Boolean read rIPC_Scanner write rIPC_Scanner;
    property Manufacturer: String read rManufacturer write rManufacturer;
    property ProductFamily: String read rProductFamily write rProductFamily;
    property ProductName: String read rProductName write rProductName;

    property PaperFeed:TTwainPaperFeeding read rTwainParams.PaperFeed write rTwainParams.PaperFeed;
    property PaperSize:TTwainPaperSize read rTwainParams.PaperSize write rTwainParams.PaperSize;
    property PixelType:TTwainPixelType read rTwainParams.PixelType write rTwainParams.PixelType;
    property Resolution:Extended read rTwainParams.Resolution write rTwainParams.Resolution;
    property Contrast:Extended read rTwainParams.Contrast write rTwainParams.Contrast;
    property Brightness:Extended read rTwainParams.Brightness write rTwainParams.Brightness;
    property BitDepth: Integer read rTwainParams.BitDepth write rTwainParams.BitDepth;
  end;


implementation

end.

