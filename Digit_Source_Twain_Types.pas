(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Twain Source Data Types and Inter Process Comunication                   **
*******************************************************************************)
unit Digit_Source_Twain_Types;

{$mode ObjFPC}{$H+}

interface

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
  MSG_TWAIN32_CAPABILITIES_GET = 107; //Input=mtData_Null Output=mtData_Stream (TTwainParamsCapabilities)
  MSG_TWAIN32_PREVIEW = 108; //Input=mtData_String  Output=mtData_Integer
  MSG_TWAIN32_TAKE = 109; //Input=mtData_String  Output=mtData_Integer

  TwainFileBase = 'twain';  //File is a Bitmap so Ext is .bmp

implementation

end.

