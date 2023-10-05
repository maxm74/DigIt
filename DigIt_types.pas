(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Common Types\Consts                                                      **
*******************************************************************************)

unit DigIt_Types;

{$mode objfpc}{$H+}

interface

uses SysUtils;

const
  Config_XMLWork='digit.xml';
  Config_Options='digit.ini';

var
   ConfigDir:String;

implementation

initialization
   ConfigDir :=GetAppConfigDir(False);
   ForceDirectories(ConfigDir);


end.

