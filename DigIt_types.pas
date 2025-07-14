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

uses Classes, SysUtils, Laz2_XMLCfg, FPImage,
     BGRAUnits, BGRABitmapTypes, BGRABitmap,
     Digit_Bridge_Intf;

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
  File_Config = 'digit.cfg';
  File_Profiles = 'profiles.cfg';

  SES_SourceFiles   = 'SourceFiles/';
  SES_CapturedFiles = 'CapturedFiles/';
  SES_PageSettings  = 'PageSettings/';
  SES_CropAreas = 'CropAreas/';

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
    resFullSize,
    resFixedWidth,
    resFixedHeight,
    resBoth
  );

  { TDigItPhysicalSize }

  TPhysicalUnit = (
    puPixel,
    puCentimeter,
    puMillimeter,
    puInch,
    puPercent
  );


  TDigItPhysicalSize = class(TPersistent)
    private
      rPhysicalUnit: TPhysicalUnit;
      rWidth,
      rHeight: Single;

      procedure SetPhysicalUnit(AValue: TPhysicalUnit);

    public
      PixelsPerInch: Integer; //Used only to convert to pixels

      constructor Create(APixelsPerInch: Integer=96);
      procedure SetValues(APhysicalUnit: TPhysicalUnit; AWidth, AHeight: Single);

    published
      property PhysicalUnit: TPhysicalUnit read rPhysicalUnit write SetPhysicalUnit;
      property Width: Single read rWidth write rWidth;
      property Height: Single read rHeight write rHeight;
    end;

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

  //Events Types
  TLoadSaveSourceXMLEvent = procedure (Sender: TObject; aXML: TRttiXMLConfig; IsAutoSave: Boolean;
                                       XMLRoot_Path: String) of object;
  TLoadSaveXMLEvent = procedure (Sender: TObject; aXML: TRttiXMLConfig; IsAutoSave: Boolean) of object;

  TFileNameEvent = procedure (Sender: TObject; AFileName: String) of object;

  TCropFullEvent = procedure (Sender: TObject; UserCancel, KeepFiles: Boolean;
                              old_CounterValue, old_CapturedFilesIndex: Integer) of object;

  TCropImageEvent = procedure (Sender: TObject; ABitmap: TBGRABitmap;
                               iCapturedFiles: Integer; IsReCrop: Boolean) of object;

  TCustomTakeMethod = function (takeAction: DigIt_Source_TakeAction;
                                var AFiles: TSourceFileArray; AStartIndex: Integer): DWord of object;


{** Convert PhysicalSize to/from Cm/Inch}
function PhysicalSizeConvert(ASourceUnit: TPhysicalUnit; ASourceSize: Single;
                             ATargetUnit: TPhysicalUnit; AResolution: Single): Single;

function PhysicalToCSSUnit(ASourceUnit: TPhysicalUnit): TCSSUnit;
function CSSToPhysicalUnit(ASourceUnit: TCSSUnit): TPhysicalUnit;

function CSSToResolutionUnit(ASourceUnit: TCSSUnit): TResolutionUnit;

function PhysicalToResolutionUnit(ASourceUnit: TPhysicalUnit): TResolutionUnit;
function ResolutionToPhysicalUnit(ASourceUnit: TResolutionUnit): TPhysicalUnit;

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

function PhysicalSizeConvert(ASourceUnit: TPhysicalUnit; ASourceSize: Single;
                             ATargetUnit: TPhysicalUnit; AResolution: Single): Single;
begin
  Result:= ASourceSize;
  // already in expected unit
  if ASourceUnit = ATargetUnit then exit;

  // checks if resolution is ill-defined
  if (ATargetUnit = puPixel) and (AResolution < 2) then AResolution:= 96; // assume legacy 96 DPI

  case ASourceUnit of
  puPixel: if (ATargetUnit = puInch)
           then Result:= ASourceSize/AResolution                // from Pixel to Inch
           else Result:= (ASourceSize/AResolution)*2.54;        // from Pixel to Cm
  puInch: if (ATargetUnit = puCentimeter)
          then Result:= ASourceSize*2.54                        // from Inch to Cm
          else Result:= ASourceSize*AResolution;                // form Inch to Pixel
  puCentimeter: if (ATargetUnit = puInch)
                then Result:= ASourceSize/2.54                  // from Cm to Inch
                else Result:= (ASourceSize/2.54)*AResolution;   // form Cm to Pixel
  end;
end;

function PhysicalToCSSUnit(ASourceUnit: TPhysicalUnit): TCSSUnit;
begin
  case ASourceUnit of
  puPixel: Result:= cuPixel;
  puCentimeter: Result:= cuCentimeter;
  puMillimeter: Result:= cuMillimeter;
  puInch: Result:= cuInch;
  puPercent: Result:= cuPercent;
  end;
end;

function CSSToPhysicalUnit(ASourceUnit: TCSSUnit): TPhysicalUnit;
begin
  case ASourceUnit of
  cuPixel: Result:= puPixel;
  cuCentimeter: Result:= puCentimeter;
  cuMillimeter: Result:= puMillimeter;
  cuInch: Result:= puInch;
  cuPercent: Result:= puPercent;
  end;
end;

function CSSToResolutionUnit(ASourceUnit: TCSSUnit): TResolutionUnit;
begin
  case ASourceUnit of
  cuPixel: Result:= ruNone;
  cuCentimeter: Result:= ruPixelsPerCentimeter;
  cuInch: Result:= ruPixelsPerInch;
  end;
end;

function PhysicalToResolutionUnit(ASourceUnit: TPhysicalUnit): TResolutionUnit;
begin
  case ASourceUnit of
  puPixel: Result:= ruNone;
  puInch: Result:= ruPixelsPerInch;
  puCentimeter: Result:= ruPixelsPerCentimeter;
  end;
end;

function ResolutionToPhysicalUnit(ASourceUnit: TResolutionUnit): TPhysicalUnit;
begin
  case ASourceUnit of
  ruNone: Result:= puPixel;
  ruPixelsPerInch: Result:= puInch;
  ruPixelsPerCentimeter: Result:= puCentimeter;
  end;
end;


{ TDigItPhysicalSize }

procedure TDigItPhysicalSize.SetPhysicalUnit(AValue: TPhysicalUnit);
begin
  if (AValue<>rPhysicalUnit) then
  begin
    rWidth:= PhysicalSizeConvert(rPhysicalUnit, rWidth, AValue, PixelsPerInch);
    rHeight:= PhysicalSizeConvert(rPhysicalUnit, rHeight, AValue, PixelsPerInch);
    rPhysicalUnit:= AValue;
  end;
end;

constructor TDigItPhysicalSize.Create(APixelsPerInch: Integer);
begin
  inherited Create;

  PixelsPerInch:= APixelsPerInch;
end;

procedure TDigItPhysicalSize.SetValues(APhysicalUnit: TPhysicalUnit; AWidth,
  AHeight: Single);
begin
  rPhysicalUnit:= APhysicalUnit;
  rWidth:= AWidth;
  rHeight:= AHeight;
end;

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

