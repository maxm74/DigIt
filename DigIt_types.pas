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
  SES_Counter = 'Counter/';

type
  TDigItCropMode = (
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
    cuPixel,
    cuCentimeter,
    cuMillimeter,
    cuInch,
    cuPica,
    cuPoint,
    cuPercent
  );


  TDigItPhysicalSize = class(TPersistent)
    private
      rPhysicalRect: TPhysicalRect;
      Bitmap: TBGRABitmap; //Used only to convert to pixels

      function GetPhysicalUnit: TPhysicalUnit;
      procedure SetPhysicalUnit(AValue: TPhysicalUnit);

    public
      constructor Create(ABitmap: TBGRABitmap);
      procedure SetValues(APhysicalUnit: TPhysicalUnit; AWidth, AHeight: Single);

      property PhysicalRect: TPhysicalRect read rPhysicalRect;

    published
      property PhysicalUnit: TPhysicalUnit read GetPhysicalUnit write SetPhysicalUnit;
      property Width: Single read rPhysicalRect.Right write rPhysicalRect.Right;
      property Height: Single read rPhysicalRect.Bottom write rPhysicalRect.Bottom;
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

  TCropModeEvent = procedure (Sender: TObject; old_Mode: TDigItCropMode) of object;

  TCropFullEvent = procedure (Sender: TObject; UserCancel, KeepFiles: Boolean;
                              old_CounterValue, old_CapturedFilesIndex: Integer) of object;

  TCropImageEvent = procedure (Sender: TObject; ABitmap: TBGRABitmap;
                               iCapturedFiles: Integer; IsReCrop: Boolean) of object;

  TCustomTakeMethod = function (takeAction: DigIt_Source_TakeAction;
                                var AFiles: TSourceFileArray; AStartIndex: Integer): DWord of object;


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

function PhysicalToCSSUnit(ASourceUnit: TPhysicalUnit): TCSSUnit;
begin
  case ASourceUnit of
  cuPixel: Result:= TCSSUnit.cuPixel;
  cuCentimeter: Result:= TCSSUnit.cuCentimeter;
  cuMillimeter: Result:= TCSSUnit.cuMillimeter;
  cuInch: Result:= TCSSUnit.cuInch;
  cuPica: Result:= TCSSUnit.cuPica;
  cuPoint: Result:= TCSSUnit.cuPoint;
  cuPercent: Result:= TCSSUnit.cuPercent;
  end;
end;

function CSSToPhysicalUnit(ASourceUnit: TCSSUnit): TPhysicalUnit;
begin
  case ASourceUnit of
  TCSSUnit.cuPixel: Result:= cuPixel;
  TCSSUnit.cuCentimeter: Result:= cuCentimeter;
  TCSSUnit.cuMillimeter: Result:= cuMillimeter;
  TCSSUnit.cuInch: Result:= cuInch;
  TCSSUnit.cuPica: Result:= cuPica;
  TCSSUnit.cuPoint: Result:= cuPoint;
  TCSSUnit.cuPercent: Result:= cuPercent;
  end;
end;

function CSSToResolutionUnit(ASourceUnit: TCSSUnit): TResolutionUnit;
begin
  case ASourceUnit of
  TCSSUnit.cuPixel: Result:= ruNone;
  TCSSUnit.cuCentimeter: Result:= ruPixelsPerCentimeter;
  TCSSUnit.cuInch: Result:= ruPixelsPerInch;
  end;
end;

function PhysicalToResolutionUnit(ASourceUnit: TPhysicalUnit): TResolutionUnit;
begin
  case ASourceUnit of
  cuPixel: Result:= ruNone;
  cuInch: Result:= ruPixelsPerInch;
  cuCentimeter: Result:= ruPixelsPerCentimeter;
  end;
end;

function ResolutionToPhysicalUnit(ASourceUnit: TResolutionUnit): TPhysicalUnit;
begin
  case ASourceUnit of
  ruNone: Result:= cuPixel;
  ruPixelsPerInch: Result:= cuInch;
  ruPixelsPerCentimeter: Result:= cuCentimeter;
  end;
end;


{ TDigItPhysicalSize }

procedure TDigItPhysicalSize.SetPhysicalUnit(AValue: TPhysicalUnit);
begin
  if (AValue<>CSSToPhysicalUnit(rPhysicalRect.PhysicalUnit)) then
  begin
    PhysicalSizeConvert(rPhysicalRect, PhysicalToCSSUnit(AValue), Bitmap);
  end;
end;

function TDigItPhysicalSize.GetPhysicalUnit: TPhysicalUnit;
begin
  Result:= CSSToPhysicalUnit(rPhysicalRect.PhysicalUnit);
end;

constructor TDigItPhysicalSize.Create(ABitmap: TBGRABitmap);
begin
  inherited Create;

  Bitmap:= ABitmap;
end;

procedure TDigItPhysicalSize.SetValues(APhysicalUnit: TPhysicalUnit; AWidth, AHeight: Single);
begin
  rPhysicalRect.PhysicalUnit:= PhysicalToCSSUnit(APhysicalUnit);
  rPhysicalRect.Right:= AWidth;
  rPhysicalRect.Bottom:= AHeight;
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

