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

