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

  Ext_Sess  = 'digs';
  Ext_Thumb = 'digt';

  File_SessionDefault = 'digit.'+Ext_Sess;
  File_Options = 'digit.xml';
  File_CapturedThumbs = 'digit.'+Ext_Thumb;

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
   Path_Pictures,
   Path_Session,
   Path_Session_Temp: String;

implementation

initialization
   Path_Application:= ExtractFilePath(ParamStr(0));
   Path_Config:= GetAppConfigDir(False);
   Path_Temp:= Path_Config+'tmp'+DirectorySeparator; //GetTempDir(False)+'DigIt'+DirectorySeparator;
   Path_Pictures:= GetUserDir+'Pictures'+DirectorySeparator+'DigIt'+DirectorySeparator;
   Path_Session:= Path_Config;
   Path_Session_Temp:= Path_Temp;

   ForceDirectories(Path_Temp);
   ForceDirectories(Path_Pictures);

end.

