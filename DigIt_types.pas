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
      rCSSUnit: TCSSUnit;
      rWidth,
      rHeight: Single;
      rResolution: TImageResolutionInfo;
      rBitmapWidth, rBitmapHeight: Integer;

      function GetPhysicalUnit: TPhysicalUnit;
      procedure SetPhysicalUnit(AValue: TPhysicalUnit);

    public
      constructor Create(AResolution: TImageResolutionInfo;
                         ABitmapWidth, ABitmapHeight: Integer); overload;
      constructor Create(AResolution: TImageResolutionInfo;
                         ABitmapPhysicalUnit: TPhysicalUnit; ABitmapWidth, ABitmapHeight: Single); overload;

      procedure SetValues(APhysicalUnit: TPhysicalUnit; AWidth, AHeight: Single);

      procedure SetBitmapValues(ABitmap: TBGRABitmap); overload;
      procedure SetBitmapValues(AResolution: TImageResolutionInfo;
                                ABitmapPhysicalUnit: TPhysicalUnit; ABitmapWidth, ABitmapHeight: Single); overload;

      procedure GetPixelsSize(var pWidth, pHeight: Integer);

      property BitmapWidth: Integer read rBitmapWidth write rBitmapWidth;
      property BitmapHeight: Integer read rBitmapHeight write rBitmapHeight;
      property Resolution: TImageResolutionInfo read rResolution write rResolution;

    published
      property PhysicalUnit: TPhysicalUnit read GetPhysicalUnit write SetPhysicalUnit;
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

  TCropModeEvent = procedure (Sender: TObject; old_Mode: TDigItCropMode) of object;

  TCropFullEvent = procedure (Sender: TObject; UserCancel, KeepFiles: Boolean;
                              old_CounterValue, old_CapturedFilesIndex: Integer) of object;

  TCropImageEvent = procedure (Sender: TObject; ABitmap: TBGRABitmap;
                               iCapturedFiles: Integer; IsReCrop: Boolean) of object;

  TCustomTakeMethod = function (takeAction: DigIt_Source_TakeAction;
                                var AFiles: TSourceFileArray; AStartIndex: Integer): DWord of object;


function PhysicalToCSSUnit(ASourceUnit: TPhysicalUnit): TCSSUnit;
function CSSToPhysicalUnit(ASourceUnit: TCSSUnit): TPhysicalUnit;


//THIS FUNCTIONS ARE TEMPORARY
//until pull request #297 in BGRABitmap is approved and subsequent work on ImageManipulation to change the unit of measurement
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
var
   newUnit: TCSSUnit;
   tmpWidth,
   tmpHeight: Single;

begin
  if (AValue<>CSSToPhysicalUnit(rCSSUnit)) then
  begin
    newUnit:= PhysicalToCSSUnit(AValue);

    if (rCSSUnit = TCSSUnit.cuPercent)
    then begin
           rHeight:= PixelsToPhysicalSize(rHeight * rBitmapHeight / 100,
                                          rResolution.ResolutionUnit, rResolution.ResolutionY, newUnit);
           rWidth:= PixelsToPhysicalSize(rWidth * rBitmapWidth / 100,
                                         rResolution.ResolutionUnit, rResolution.ResolutionX, newUnit);
         end
    else begin
           if (newUnit = TCSSUnit.cuPercent)
           then begin
                  rHeight:= rHeight * 100 / rBitmapHeight;
                  rWidth:= rWidth * 100 / rBitmapWidth;
                end
           else PhysicalSizeConvert(rCSSUnit, rWidth, rHeight, newUnit, rResolution);
         end;
  end;
end;

function TDigItPhysicalSize.GetPhysicalUnit: TPhysicalUnit;
begin
  Result:= CSSToPhysicalUnit(rCSSUnit);
end;

constructor TDigItPhysicalSize.Create(AResolution: TImageResolutionInfo; ABitmapWidth, ABitmapHeight: Integer);
begin
  inherited Create;

  rResolution:= AResolution;
  rBitmapWidth:= ABitmapWidth;
  rBitmapHeight:= ABitmapHeight;
end;

constructor TDigItPhysicalSize.Create(AResolution: TImageResolutionInfo;
                                      ABitmapPhysicalUnit: TPhysicalUnit; ABitmapWidth, ABitmapHeight: Single);
begin
  inherited Create;

  SetBitmapValues(AResolution, ABitmapPhysicalUnit, ABitmapWidth, ABitmapHeight);
end;

procedure TDigItPhysicalSize.SetValues(APhysicalUnit: TPhysicalUnit; AWidth, AHeight: Single);
begin
  rCSSUnit:= PhysicalToCSSUnit(APhysicalUnit);
  rWidth:= AWidth;
  rHeight:= AHeight;
end;

procedure TDigItPhysicalSize.SetBitmapValues(ABitmap: TBGRABitmap);
begin
  if (ABitmap <> nil) then
  begin
    rBitmapWidth:= ABitmap.Width;
    rBitmapHeight:= ABitmap.Height;
    rResolution:= ABitmap.ResolutionInfo;
  end;
end;

procedure TDigItPhysicalSize.SetBitmapValues(AResolution: TImageResolutionInfo;
                                             ABitmapPhysicalUnit: TPhysicalUnit; ABitmapWidth, ABitmapHeight: Single);
begin
  rResolution:= AResolution;
  rBitmapHeight:= HalfUp(PhysicalSizeToPixels(ABitmapHeight,
                                              rResolution.ResolutionUnit, rResolution.ResolutionY,
                                              PhysicalToCSSUnit(ABitmapPhysicalUnit)));
  rBitmapWidth:= HalfUp(PhysicalSizeToPixels(ABitmapWidth,
                                             rResolution.ResolutionUnit, rResolution.ResolutionX,
                                             PhysicalToCSSUnit(ABitmapPhysicalUnit)));
end;

procedure TDigItPhysicalSize.GetPixelsSize(var pWidth, pHeight: Integer);
var
   sWidth, sHeight: Single;

begin
  if (rCSSUnit = TCSSUnit.cuPercent)
  then begin
         pHeight:= HalfUp(rHeight * 100 / rBitmapHeight);
         pWidth:= HalfUp(rWidth * 100 / rBitmapWidth);
       end
  else begin
         pHeight:= HalfUp(PhysicalSizeToPixels(rHeight, rResolution.ResolutionUnit, rResolution.ResolutionY, rCSSUnit));
         pWidth:= HalfUp(PhysicalSizeToPixels(rWidth, rResolution.ResolutionUnit, rResolution.ResolutionX, rCSSUnit));
       end;
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

