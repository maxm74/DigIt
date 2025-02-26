(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Common Types\Consts                                                      **
*******************************************************************************)

unit DigIt_Types;

{$mode objfpc}{$H+}

interface

uses SysUtils;

const
  DigIt_Version = '0.1.0';
  Config_XMLWork = 'digit.xml';
  Config_Options = 'digit.ini';
  Config_CapturedThumbs = 'c_thumbs.img';
  XML_SourceFiles   = 'SourceFiles/';
  XML_CapturedFiles = 'CapturedFiles/';
  XML_PageSettings  = 'PageSettings/';

type
  TDigItCropMode = (
    diCropNull = -1, //Used during the creation phase
    diCropFull,      //All captured pages are processed in bulk as they are
    diCropCustom     //All captured pages are processed with the cut chosen the first time in preview
  );

var
   Path_Application,
   Path_Config,
   Path_Temp,
   Path_Pictures: String;

implementation

initialization
   Path_Application :=ExtractFilePath(ParamStr(0));
   Path_Config :=GetAppConfigDir(False);
   Path_Temp :=Path_Config+'tmp'+DirectorySeparator; //GetTempDir(False)+'DigIt'+DirectorySeparator;
   Path_Pictures :=GetUserDir+'Pictures'+DirectorySeparator+'DigIt'+DirectorySeparator;
   //ForceDirectories(Path_Config);
   ForceDirectories(Path_Temp);
   ForceDirectories(Path_Pictures);

end.

