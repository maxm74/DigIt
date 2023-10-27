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
  TWAIN32_SERVER_NAME = 'DigIt_Twain32CommServer';

  MSG_TWAIN32_STOP = 101;
  MSG_TWAIN32_LIST = 102;
  MSG_TWAIN32_FIND = 103;
  MSG_TWAIN32_OPEN = 104;
  MSG_TWAIN32_TAKE = 105;

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

