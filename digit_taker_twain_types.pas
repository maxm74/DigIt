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
  Classes, SysUtils;

const
  TWAIN32_SERVER_EXE = 'DigIt_Twain32Comm.exe';
  TWAIN32_SERVER_NAME = 'DigIt_Twain32CommServer';

  MSG_TWAIN32_TIMEOUT = 100; //Input=mtSync_Integer Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_STOP = 101; //Input=mtSync_Null Output=mtSync_Integer (Magic Mess)
  MSG_TWAIN32_LIST = 102; //Input=mtSync_Null Output=mtSync_Pointer (array of TW_IDENTITY)
  MSG_TWAIN32_FIND = 103; //Input=mtSync_Var (TW_IDENTITY) Output=mtSync_Integer
  MSG_TWAIN32_OPEN = 104; //Input=mtSync_Integer Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_USERINTERFACE = 105; //Input=mtSync_Var (TW_USERINTERFACE) Output=mtSync_Integer (Boolean)
  MSG_TWAIN32_TAKE = 106; //Input=mtSync_String  Output=mtSync_Integer (Boolean)

  RES_TWAIN32_STOPPED = $0CACA; //:-( A message for you in Italian...

type
  TDigIt_Taker_TwainParams = class(TPersistent)
  protected
    rIPC_Scanner: Boolean;
    rManufacturer,
    rProductFamily,
    rProductName: String;
  published
    property IPC_Scanner:Boolean read rIPC_Scanner write rIPC_Scanner;
    property Manufacturer: String read rManufacturer write rManufacturer;
    property ProductFamily: String read rProductFamily write rProductFamily;
    property ProductName: String read rProductName write rProductName;
end;


implementation

end.

