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

resourcestring
  rsProcessingImages = 'Processing Images';
  rsProcessing = 'Processing %d / %s';
  rsProcessed = 'Processed %d / %s';

const
  DigIt_Version = '0.2.0';

  Ext_Sess  = '.digs';
  Ext_Thumb = '.digt';
  Ext_AutoSess = '.digas';
  Ext_AutoThumb = '.digat';

  File_DefSession = 'digit';
  File_Options = 'digit.cfg';

  XML_SourceFiles   = 'SourceFiles/';
  XML_CapturedFiles = 'CapturedFiles/';
  XML_PageSettings  = 'PageSettings/';

type
  TDigItCropMode = (
    diCropNull = -1, //Used during the creation phase
    diCropFull,      //All captured pages are processed in bulk as they are
    diCropCustom     //All captured pages are processed with the cut chosen the first time in preview
  );

  TDigItFilter_Rotate = (
    rotNone,
    rotLeft90,
    rotRight90,
    rot180
  );

  TDigItFilter_Flip = (
    flipNone,
    flipHorizontal,
    flipVertical
  );

  TDigItFilter_Flip4 = (
    flipNull,
    flipHLeft,
    flipHRight,
    flipVDown,
    flipVUp
  );

  TDigItFilter_Resize = (
    resFixedWidth,
    resFixedHeight,
    resBoth,
    resNone
  );

  TSourceFile = packed record
    cCount,
    cStart: DWord;
    fName: String;
  end;
  TSourceFileArray = array of TSourceFile;

  TCapturedFile = packed record
    fAge: LongInt;
    fName: String;
    iIndex: Integer;
  end;
  TCapturedFileArray = array of TCapturedFile;

var
   Path_Application,
   Path_Config,
   Path_Temp,
   Path_DefSession,
   Path_DefSession_Scan,
   Path_DefSession_Pictures,
   Path_Session,
   Path_Session_Scan,
   Path_Session_Pictures: String;

implementation

uses BGRAOpenRaster, BGRAPaintNet;

initialization
   Path_Application:= ExtractFilePath(ParamStr(0));
   Path_Config:= GetAppConfigDir(False);
   Path_Temp:= GetTempDir(False)+'DigIt'+DirectorySeparator;
//   Path_Temp:= Path_Config+'tmp'+DirectorySeparator;

   Path_DefSession:= Path_Config+'Session'+DirectorySeparator;
   Path_DefSession_Scan:= Path_DefSession+'Scan'+DirectorySeparator;
   Path_DefSession_Pictures:= Path_DefSession+'Pictures'+DirectorySeparator;

   Path_Session:= Path_DefSession;
   Path_Session_Scan:= Path_DefSession_Scan;
   Path_Session_Pictures:= Path_DefSession_Pictures;

   ForceDirectories(Path_Session_Scan);
   ForceDirectories(Path_Session_Pictures);
   ForceDirectories(Path_Temp);

   RegisterPaintNetFormat;
   RegisterOpenRasterFormat;

end.

