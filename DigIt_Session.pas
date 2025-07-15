(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Session Class                                                            **
*******************************************************************************)
unit DigIt_Session;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg, FPImage,
  BGRABitmap, BGRABitmapTypes,
  DigIt_Types, DigIt_Bridge_Intf;

resourcestring
  rsSavingWork = 'Saving the Work Session';
  rsSavingSources = 'Saving Sources Files';
  rsSavingCaptured = 'Saving Captured Files';
  rsSavingSessionFiles = 'Saving Session Files';
  rsSavingSwitch = 'Switching to New Work Session';
  rsSavingDone = 'Saved Done';
  rsSaveWorkCopy = 'Create a Copy of the Work Session or Move it?';
  rsNoFilesDownloaded = 'NO Files Downloaded ';
  rsTakeAgain = 'Replace the last %d taked files with a new Take?';
  rsNoMoreFiles = 'There are no more files to process, should I clear the Work Queue?';
  rsClearQueue = 'Clear the Work Queue?';
  rsDeleteCaptured = 'Delete Captured Page %s ?';
  rsDeleteAll = 'Delete All Captured Pages?';
  rsDeleteAllFiles = 'Do I also Delete Files from the disk?';

  rsErrIncomplete = 'Operation not completed, do I keep the processed files?';
  rsErrLoadWork = 'Cannot Load Work Session'#13#10'%s';

type

  { TDigIt_Session }

  TDigIt_Session = class
  private
  protected
    rLoading,
    rModified: Boolean;
    (*rPath,
    rPath_Scan,
    rPath_Pictures,*)
    rLoadedImageFile,
    rFileName: String;

    rPageSize: TDigItPhysicalSize;

    rBitmap: TBGRABitmap;

    rLastCroppedIndex,
    rLastTakedLength,     //Used in Take Again
    rSourceFilesIndex,
    rCapturedFilesIndex,
    rCapturedFilesSelected: Integer;

    rCropMode: TDigItCropMode;

    { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
    (*    rDestination: PDestinationInfo;
    rDestinationParams: IDigIt_Params;
    *)
    rDestinationName: String;

    procedure SetCropMode(AValue: TDigItCropMode);
    procedure SetCapturedFilesSelected(AValue: Integer);

    procedure CropImage(ABitmap: TBGRABitmap; IsReCrop: Boolean);
    procedure CropFile_Full(AStartIndex: Integer; isReTake: Boolean);
    procedure CropFiles(ASourceFileIndex: Integer; isReTake: Boolean);

  public
    SourceFiles: TSourceFileArray;
    CapturedFiles: TCapturedFileArray;

    PageResize: TDigItFilter_Resize;
    PageRotate: TDigItFilter_Rotate;
    PageFlip: TDigItFilter_Flip;

    SaveFormat: TBGRAImageFormat;
    SaveWriter: TFPCustomImageWriter;
    SaveExt: String;

    CropAreas: TPhysicalRectArray;

    //Load Save from XML Events
    OnLoadSource,
    OnSaveSource: TLoadSaveSourceXMLEvent;
    OnLoadXML,
    OnSaveXML,
    OnLoadSourceFiles,
    OnSaveSourceFiles,
    OnLoadDestination,
    OnSaveDestination,
    OnLoadCapturedFiles,
    OnSaveCapturedFiles,
    OnLoadLoadedImage,
    OnSaveLoadedImage,
    OnSaveSource_CapturedIndexes,
    OnLoadCropAreas,
    OnSaveCropAreas,
    OnLoadPageSettings,
    OnSavePageSettings: TLoadSaveXMLEvent;

    //Image Events
    OnLoadImage,
    OnEmptyImage: TNotifyEvent;

    //Crop Events
    OnCropModeChange: TCropModeEvent;
    OnCropImage: TCropImageEvent;
    OnCropFile_Full: TCropFullEvent;

    constructor Create;
    procedure SetDefaultStartupValues;
    destructor Destroy; override;

    procedure Counter_Dec(AValue: Integer);
    procedure Counter_Inc(AValue: Integer);
    procedure Counter_Assign(AValue: Integer);

    function LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
    procedure EmptyImage(saveToXML: Boolean);
                                            //Only the File Part, Path and Ext are added automatically
    function SaveImage(ABitmap: TBGRABitmap; AFileName: String): String;

    function ResizeImage(ABitmap :TBGRABitmap; APageResize: TDigItFilter_Resize): TBGRABitmap;
    function RotateImage(ABitmap :TBGRABitmap; APageRotate: TDigItFilter_Rotate): TBGRABitmap;
    procedure FlipImage(ABitmap :TBGRABitmap; APageFlip: TDigItFilter_Flip);

    function LoadSessionFile(APath, AFile: String; IsAutoSave: Boolean=False): Boolean; overload;
    function LoadSessionFile(AFileName: String): Boolean; overload;
    function SaveSessionFile(isMove: Boolean; AFileName: String): Boolean;

    procedure Load(IsAutoSave: Boolean);
    procedure Save(IsAutoSave: Boolean);
    procedure ClearAutoSave(AFromStartup: Boolean);

    function LoadSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean;
                        XMLRoot_Path: String=''; XML_File: String=''): Integer;
    procedure SaveSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean;
                         XMLRoot_Path: String=''; XML_File: String='');
    procedure LoadSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    function LoadDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
    procedure SaveDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure LoadLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveSource_CapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveSource_CapturedIndexes(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SavePageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);

    procedure Clear_SourceFiles(ClearSourceInst: Boolean);
    procedure Clear_Captured;

    procedure SetSaveWriter(AFormat: TBGRAImageFormat);

    //Actions
    function actPreview: Boolean;
    function actTake(isReTake: Boolean; CustomTake: TCustomTakeMethod=nil): DWord;
    procedure actTimerTake;
    procedure actCropNext;
    procedure actGoNext;
    procedure actGoBack;
    procedure actCropAll;
    procedure actClearQueue;
    procedure actCapturedDeleteAll(UserConfirm: Boolean);
    procedure actCapturedDelete(UserConfirm: Boolean; AIndex: Integer);

    procedure GetEnabledActions(out actPreview_Enabled, actTake_Enabled, actTakeRe_Enabled,
                                actCropNext_Enabled,
                                actGoNext_Enabled, actGoBack_Enabled,
                                actCropAll_Enabled, actClearQueue_Enabled: Boolean);

    procedure GetEnabledActions_Captured(out actCapturedDeleteAll_Enabled, actCapturedDelete_Enabled: Boolean);

    //property Path: String read rPath; It should be like this but the Bridge Setting reads it from Path_Session
    property FileName: String read rFileName;

    property Loading: Boolean read rLoading;
    property Modified: Boolean read rModified write rModified;
    property CropMode: TDigItCropMode read rCropMode write SetCropMode;
    property LoadedImageFile: String read rLoadedImageFile;
    property PageSize: TDigItPhysicalSize read rPageSize;
    property Bitmap: TBGRABitmap read rBitmap;

    property LastCroppedIndex: Integer read rLastCroppedIndex;
    property LastTakedLength: Integer read rLastTakedLength;
    property SourceFilesIndex: Integer read rSourceFilesIndex;
    property CapturedFilesIndex: Integer read rCapturedFilesIndex;
    property CapturedFilesSelected: Integer read rCapturedFilesSelected write SetCapturedFilesSelected;
  end;

implementation

uses Graphics, FileUtil, LazFileUtils,
     MM_StrUtils, MM_Interface_MessageDlg,
     BGRAUnits, BGRAWriteJPeg, BGRAWriteTiff,
     Digit_Bridge_Impl, DigIt_Utils, DigIt_Sources, DigIt_Counter;

procedure TDigIt_Session.SetCapturedFilesSelected(AValue: Integer);
begin
  if (rCapturedFilesSelected <> AValue) then
  begin
    if (AValue >= Length(CapturedFiles)) then AValue:= Length(CapturedFiles)-1;
    rCapturedFilesSelected:= AValue;
  end;
end;

procedure TDigIt_Session.SetCropMode(AValue: TDigItCropMode);
var
   old_Mode: TDigItCropMode;

begin
  if (rCropMode <> AValue) then
  begin
    old_Mode:= rCropMode;
    rCropMode:= AValue;
    if Assigned(OnCropModeChange) then OnCropModeChange(Self, old_Mode);
  end;
end;

constructor TDigIt_Session.Create;
begin
  inherited Create;

  // Create the Image Bitmap
  rBitmap := TBGRABitmap.Create;

  SetDefaultStartupValues;
end;

procedure TDigIt_Session.SetDefaultStartupValues;
begin
  //Set Default Startup Values
  SourceFiles:= nil;
  rSourceFilesIndex:= -1;
  CapturedFiles:= nil;
  rCapturedFilesIndex:= -1;
  rLastCroppedIndex:= -1;
  rLastTakedLength:= 0;

  rLoadedImageFile:= '';

  CropAreas:= nil;

  rPageSize:= TDigItPhysicalSize.Create;
  rPageSize.SetValues(TPhysicalUnit.cuCentimeter, 21, 29.7);

  PageResize:= resFullSize;
  PageRotate:= rotNone;
  PageFlip:= flipNone;

  rCropMode:= diCropFull;

  //Set Default Session Values
  Path_Session:= Path_DefSession;
  Path_Session_Scan:= Path_DefSession_Scan;
  Path_Session_Pictures:= Path_DefSession_Pictures;
  rFileName:= File_DefSession;
  rModified:= False;
  SetSaveWriter(ifJpeg);
end;

destructor TDigIt_Session.Destroy;
begin
  CropAreas:= nil;
  SourceFiles:= nil;
  CapturedFiles:= nil;
  rBitmap.Free; rBitmap:= nil;

  inherited Destroy;
end;

procedure TDigIt_Session.Counter_Dec(AValue: Integer);
begin
  if (AValue > 0) then
  begin
    Counter.Value:= Counter.Value-AValue;
    dec(rCapturedFilesIndex, AValue);
    if (rCapturedFilesIndex < 0) then rCapturedFilesIndex:= -1;
  end;
end;

procedure TDigIt_Session.Counter_Inc(AValue: Integer);
begin
  if (AValue > 0) then
  begin
    Counter.Value:= Counter.Value+AValue;
    inc(rCapturedFilesIndex, AValue);
    if (rCapturedFilesIndex > Length(CapturedFiles)-1) then rCapturedFilesIndex:= Length(CapturedFiles)-1;
  end;
end;

procedure TDigIt_Session.Counter_Assign(AValue: Integer);
begin
  Counter.Value:= AValue;
  rCapturedFilesIndex:= AValue-1;
  if (rCapturedFilesIndex > Length(CapturedFiles)-1) then rCapturedFilesIndex:= Length(CapturedFiles)-1;
end;

function TDigIt_Session.LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
var
   BitmapN,
   BitmapR: TBGRABitmap;

begin
  Result:= False;

  if (AImageFile<>'') and FileExists(AImageFile) then
  try
     BitmapR:= nil;
     BitmapN:= TBGRABitmap.Create;
     BitmapN.LoadFromFile(AImageFile);

     //Pre processing Filters

     { #todo -oMaxM : In future Preprocessing filters as Interfaces Here }

     //Rotate
     if (PageRotate <> rotNone) then
     begin
       BitmapR:= RotateImage(BitmapN, PageRotate);
       if (BitmapR<>nil) then
       begin
         BitmapN.Free;
         BitmapN:= BitmapR;
         BitmapR:= nil;
       end;
     end;

     //Flip
     if (PageFlip <> flipNone) then FlipImage(BitmapN, PageFlip);

     //Resize
     if (PageResize <> resFullSize) then
     begin
       BitmapR:= ResizeImage(BitmapN, PageResize);
       if (BitmapR<>nil) then
       begin
         BitmapN.Free;
         BitmapN:= BitmapR;
         BitmapR:= nil;
       end;
     end;

     rBitmap.Assign(BitmapN, True); // Associate the new bitmap

     rLoadedImageFile:= AImageFile;

     if saveToXML then SaveLoadedImage(nil, True);

     if Assigned(OnLoadImage) then OnLoadImage(Self);

     Result:= True;

  finally
     if (BitmapN <> Nil) then BitmapN.Free;
  end;
end;

procedure TDigIt_Session.EmptyImage(saveToXML: Boolean);
begin
  FreeAndNil(rBitmap);
  rLoadedImageFile:= '';
  if saveToXML then SaveLoadedImage(nil, True);

  if Assigned(OnLoadImage) then OnEmptyImage(Self);
end;

function TDigIt_Session.SaveImage(ABitmap: TBGRABitmap; AFileName: String): String;
begin
  Result:=Path_Session_Pictures+AFileName+ExtensionSeparator+SaveExt;

  { #todo -oMaxM : In future Presaving filters as Interfaces Here }

  //Adjust some Writers
  if (SaveWriter is TBGRAWriterTiff) then TBGRAWriterTiff(SaveWriter).Clear;

  ABitmap.SaveToFile(Result, SaveWriter);
end;

function TDigIt_Session.ResizeImage(ABitmap: TBGRABitmap; APageResize: TDigItFilter_Resize): TBGRABitmap;
var
   newWidth, newHeight: Single;
   pixelWidth, pixelHeight: Integer;

begin
  Result:= nil;

  if (rPageSize.PhysicalUnit = TPhysicalUnit.cuPixel)
  then begin
         pixelWidth:= Trunc(rPageSize.Width);
         pixelHeight:= Trunc(rPageSize.Height);
        end
  else begin
    (*
        newWidth:= PhysicalSizeConvert(rPageSize.PhysicalUnit,
                                       rPageSize.Width,
                                       ResolutionToPhysicalUnit(ABitmap.ResolutionUnit), ABitmap.ResolutionX);
        newHeight:= PhysicalSizeConvert(rPageSize.PhysicalUnit,
                                        rPageSize.Height,
                                        ResolutionToPhysicalUnit(ABitmap.ResolutionUnit), ABitmap.ResolutionY);

        pixelWidth:= HalfUp(newWidth*ABitmap.ResolutionX);
        pixelHeight:= HalfUp(newHeight*ABitmap.ResolutionY);
*)      pixelWidth:= HalfUp(PhysicalSizeToPixels(rPageSize.Width, ABitmap.ResolutionUnit, Abitmap.ResolutionX,
                            PhysicalToCSSUnit(rPageSize.PhysicalUnit)));
        pixelHeight:= HalfUp(PhysicalSizeToPixels(rPageSize.Height, ABitmap.ResolutionUnit, Abitmap.ResolutionY,
                             PhysicalToCSSUnit(rPageSize.PhysicalUnit)));
      end;

  Case APageResize of
  resFixedWidth: pixelHeight:= GetProportionalSide(pixelWidth, ABitmap.Width, ABitmap.Height);
  resFixedHeight: pixelWidth:= GetProportionalSide(pixelHeight, ABitmap.Height, ABitmap.Width);
  end;

  Result:= ABitmap.Resample(pixelWidth, pixelHeight, rmFineResample, True);
end;

function TDigIt_Session.RotateImage(ABitmap: TBGRABitmap; APageRotate: TDigItFilter_Rotate): TBGRABitmap;
begin
  Case APageRotate of
    rotNone: Result:= nil;
    rotLeft90: Result:= ABitmap.RotateCCW(True);
    rotRight90: Result:= ABitmap.RotateCW(True);
    rot180: Result:= ABitmap.RotateUD(True);
  end;
end;

procedure TDigIt_Session.FlipImage(ABitmap :TBGRABitmap; APageFlip: TDigItFilter_Flip);
begin
  Case APageFlip of
    flipHorizontal: ABitmap.HorizontalFlip;
    flipVertical: ABitmap.VerticalFlip;
  end;
end;

function TDigIt_Session.LoadSessionFile(APath, AFile: String; IsAutoSave: Boolean): Boolean;
var
   oldPath_Session,
   oldPath_Session_Scan,
   oldPath_Session_Pictures,
   oldSession_File: String;

begin
  try
     Result:= False;

     oldPath_Session:= Path_Session;
     oldPath_Session_Scan:= Path_Session_Scan;
     oldPath_Session_Pictures:= Path_Session_Pictures;
     oldSession_File:= rFileName;

     Path_Session:= APath;
     Path_Session_Scan:= Path_Session+'Scan'+DirectorySeparator;
     Path_Session_Pictures:= Path_Session+'Pictures'+DirectorySeparator;
     //rPath:= APath;
     rFileName:= AFile;

     Load(IsAutoSave);

     Result:= True;

  except
     Result:= False;
  end;

  if not(Result) then
  begin
    theBridge.MessageDlg('DigIt', Format(rsErrLoadWork, [APath+AFile]), mtError, [mbOk], 0);

    Path_Session:= oldPath_Session;
    Path_Session_Scan:= oldPath_Session_Scan;
    Path_Session_Pictures:= oldPath_Session_Pictures;
    {#note : it would be more complicated than that because we would have to reread everything from the auto save}
  end;
end;

function TDigIt_Session.LoadSessionFile(AFileName: String): Boolean;
begin
  Result:= LoadSessionFile(ExtractFilePath(AFileName),
                           ExtractFileNameWithoutExt(ExtractFileName(AFileName)));
end;

function TDigIt_Session.SaveSessionFile(isMove: Boolean; AFileName: String): Boolean;
var
   newPath_Session,
   newSession_File,
   curFileName,
   curFileNameR: String;
   fileSources,
   fileCaptured: TStringArray;
   isRelative: Boolean;
   i,
   lenSources,
   lenCaptured: Integer;

begin
  Result:= False;
  try
  if (AFileName <> '') then
  begin
     //Save(True);

     newPath_Session:= ExtractFilePath(AFileName);
     newSession_File:= ExtractFileNameWithoutExt(ExtractFileName(AFileName));
     ForceDirectories(newPath_Session);

     theBridge.ProgressShow(rsSavingWork, 0, 5);

     lenSources:= Length(SourceFiles);
     lenCaptured:= Length(CapturedFiles);

     SetLength(fileSources, lenSources);
     SetLength(fileCaptured, lenCaptured);

     //Copy Files to new Session, if is Relative convert Names else leave as is
     if theBridge.ProgressSetTotal(rsSavingSources, 1) then exit;
     for i:=0 to lenSources-1 do
     begin
       curFileNameR:= FullPathToRelativePath(Path_Session, SourceFiles[i].fName, isRelative);

       if isRelative
       then begin
              curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
              ForceDirectories(ExtractFilePath(curFileName));
              CopyFile(SourceFiles[i].fName, curFileName, True, True);
              if isMove then DeleteFile(SourceFiles[i].fName);

              fileSources[i]:= curFileName;
            end
       else begin
              {#todo -oMaxM : User may select if copy the file inside the Session Scan Folder Yes,No,YesAll,NoAll }
              fileSources[i]:= SourceFiles[i].fName;
            end;

       if theBridge.ProgressCancelled then exit;
     end;

     if theBridge.ProgressSetTotal(rsSavingCaptured, 2) then exit;
     for i:=0 to lenCaptured-1 do
     begin
       curFileNameR:= FullPathToRelativePath(Path_Session, CapturedFiles[i].fName, isRelative);

       if isRelative
       then begin
              curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
              ForceDirectories(ExtractFilePath(curFileName));
              CopyFile(CapturedFiles[i].fName, curFileName, True, True);
              if isMove then DeleteFile(CapturedFiles[i].fName);

              fileCaptured[i]:= curFileName;
            end
       else begin
              {#todo -oMaxM : User may select if copy the file inside the Session Pictures Folder Yes,No,YesAll,NoAll }
              fileCaptured[i]:= CapturedFiles[i].fName;
            end;

       if theBridge.ProgressCancelled then exit;
     end;

     if theBridge.ProgressSetTotal(rsSavingSessionFiles, 3) then exit;

     //Copy Loaded File
     curFileNameR:= FullPathToRelativePath(Path_Session, rLoadedImageFile, isRelative);
     if isRelative then
     begin
       curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
       ForceDirectories(ExtractFilePath(curFileName));
       CopyFile(rLoadedImageFile, curFileName, True, False);
       if isMove then DeleteFile(rLoadedImageFile);

       rLoadedImageFile:= curFileName;
     end;

     if theBridge.ProgressCancelled then exit;

     //Copy AutoSave Files
     curFileName:=Path_Session+rFileName+Ext_AutoSess;
     if FileExists(curFileName) then
     begin
       CopyFile(curFileName, newPath_Session+newSession_File+Ext_AutoSess, True, False);

       //if Path is Default then we are saving a new Session, we must move the autoSave Files
       if isMove then DeleteFile(curFileName);
     end;
     curFileName:=Path_Session+rFileName+Ext_AutoThumb;
     if FileExists(curFileName) then
     begin
       CopyFile(curFileName, newPath_Session+newSession_File+Ext_AutoThumb, True, False);
       if isMove then DeleteFile(curFileName);
     end;

     //if is Move then Delete old Session Files
     if isMove then
     begin
       DeleteFile(Path_Session+rFileName+Ext_Thumb);
       DeleteFile(Path_Session+rFileName+Ext_Sess);
     end;

     if theBridge.ProgressSetTotal(rsSavingSwitch, 4) then exit;

     //Populate the Files Arrays with new Filenames
     for i:=0 to lenSources-1 do
       SourceFiles[i].fName:= fileSources[i];

     for i:=0 to lenCaptured-1 do
       CapturedFiles[i].fName:= fileCaptured[i];

     //If is Relative convert Paths to new Session, else leave as is
     curFileNameR:= FullPathToRelativePath(Path_Session, Path_Session_Pictures, isRelative);
     if isRelative then Path_Session_Pictures:= RelativePathToFullPath(newPath_Session, curFileNameR);

     curFileNameR:= FullPathToRelativePath(Path_Session, Path_Session_Scan, isRelative);
     if isRelative then Path_Session_Scan:= RelativePathToFullPath(newPath_Session, curFileNameR);

     //Switch to New Session
     Path_Session:= newPath_Session;
     rFileName:= ExtractFileNameWithoutExt(ExtractFileName(AFileName));

     Save(False);

     theBridge.ProgressSetTotal(rsSavingDone, 5);

     Result:= True;
  end;

  finally
    fileSources:= nil;
    fileCaptured:= nil;

    theBridge.ProgressHide;
  end;
end;

procedure TDigIt_Session.SetSaveWriter(AFormat: TBGRAImageFormat);
begin
  SaveFormat:= AFormat;
  try
     if (SaveWriter <> nil) then SaveWriter.Free;
     SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
     SaveExt:= SuggestImageExtension(SaveFormat);

  except
    SaveFormat:= ifJpeg;
    SaveWriter:= nil;
    SaveExt:= 'jpg';
  end;

  if (SaveWriter <> nil) then
  begin
    if (SaveWriter is TBGRAWriterJPEG) then TBGRAWriterJPEG(SaveWriter).CompressionQuality:= 100;
  end;
end;

procedure TDigIt_Session.Load(IsAutoSave: Boolean);
var
   newSourceI,
   newDestinationI: Integer;
   aXML: TRttiXMLConfig;

begin
  try
     rLoading:= True;

     if IsAutoSave
     then aXML:= TRttiXMLConfig.Create(Path_Session+rFilename+Ext_AutoSess)
     else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     newSourceI:= LoadSource(aXML, IsAutoSave);
     LoadSourceFiles(aXML, IsAutoSave);
     newDestinationI:= LoadDestination(aXML, IsAutoSave);
     LoadCapturedFiles(aXML, IsAutoSave);
     LoadPageSettings(aXML, IsAutoSave);
     LoadLoadedImage(aXML, IsAutoSave);
     Counter.Load(aXML, SES_Counter, True);

     rCropMode:= diCropFull;
     aXML.GetValue('CropMode', rCropMode, TypeInfo(TDigItCropMode));

     if (rCropMode = diCropCustom)
     then LoadCropAreas(aXML, IsAutoSave)
     else CropAreas:= nil;

     if Assigned(OnLoadXML) then OnLoadXML(Self, aXML, IsAutoSave);

     rModified:= IsAutoSave;

  finally
     rLoading:= False;
     aXML.Free;
  end;
end;

procedure TDigIt_Session.Save(IsAutoSave: Boolean);
var
   aXML: TRttiXMLConfig;
   curExt: String;

begin
  try
     if IsAutoSave
     then curExt:= Ext_AutoSess
     else curExt:= Ext_Sess;

     aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+curExt);

     SaveSource(aXML, IsAutoSave);
     SaveSourceFiles(aXML, IsAutoSave);
     SaveDestination(aXML, IsAutoSave);
     SaveCapturedFiles(aXML, IsAutoSave);
     SavePageSettings(aXML, IsAutoSave);
     SaveLoadedImage(aXML, IsAutoSave);

     Counter.Save(aXML, SES_Counter, True);

     aXML.SetValue('CropMode', rCropMode, TypeInfo(TDigItCropMode));

     if (rCropMode = diCropCustom)
     then SaveCropAreas(aXML, IsAutoSave)
     else aXML.DeletePath(SES_CropAreas);

     if Assigned(OnSaveXML) then OnSaveXML(Self, aXML, IsAutoSave);

     aXML.Flush;
     aXML.Free;

     //FPC Bug?
     //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
     //So we do it after destroying XML.

     if (Sources.Selected <> nil) and
        (Sources.Selected^.Inst <> Nil)
     then Sources.Selected^.Inst.Params.Save(PChar(Path_Session+rFileName+curExt), 'Source/Params');

//     if (rDestination <> nil) then rDestination^.Inst.Params.Save(PChar(Path_Session+rFileName+curExt), 'Destination/Params');

     if not(IsAutoSave) then rModified:= False;

  finally
  end;
end;

procedure TDigIt_Session.ClearAutoSave(AFromStartup: Boolean);
var
   i: Integer;
   nofileBMP: TBitmap;

begin
  DeleteFile(Path_DefSession+File_DefSession+Ext_AutoSess);
  DeleteFile(Path_DefSession+File_DefSession+Ext_AutoThumb);
  DeleteDirectory(Path_DefSession_Scan, True);
  DeleteDirectory(Path_DefSession_Pictures, True);

  if not(AFromStartup) then
  try
     SetDefaultStartupValues;

     //Clear Source Queque
     //rSource^.Inst.Clear;

     //Clear Captured Array and ListView
     Clear_Captured;

     //Clear the Bitmap
     EmptyImage(False);

     //Reset Counter
     Counter.Reset;

     setCropMode(diCropFull);

  finally
  end;
end;

function TDigIt_Session.LoadSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean;
                                   XMLRoot_Path: String; XML_File: String): Integer;
var
   aFree: Boolean;
   oldSourceName,
   newSourceName: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree then
     begin
       if (XML_File = '') then //Use Session File
       begin
         if IsAutoSave
         then XML_File:= Path_Session+rFileName+Ext_AutoSess
         else XML_File:= Path_Session+rFileName+Ext_Sess;
       end;

       aXML:= TRttiXMLConfig.Create(XML_File);
     end
     else XML_File:= aXML.Filename;

     Result:= -1;

     oldSourceName:= Sources.SelectedName;
     if Sources.Select(aXML, XMLRoot_Path, newSourceName)
     then begin
            if (oldSourceName <> newSourceName) then
            begin
              { #note -oMaxM : rSource Switched...Do something? }
            end;

            Result:= Sources.SelectedIndex;
          end
     else if (newSourceName <> '') then
          begin
            //MessageDlg('DigIt', Format(rsSourceNotFound, [newSourceName]), mtInformation, [mbOk], 0);
            Result:= Sources.SelectedIndex;
          end;

     if Assigned(OnLoadSource) then OnLoadSource(Self, aXML, IsAutoSave, XMLRoot_Path);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveSource(aXML: TRttiXMLConfig; IsAutoSave: Boolean;
                                    XMLRoot_Path: String; XML_File: String);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree then
     begin
       if (XML_File = '') then //Use Session File
       begin
         if IsAutoSave
         then XML_File:= Path_Session+rFileName+Ext_AutoSess
         else XML_File:= Path_Session+rFileName+Ext_Sess;
       end;

       aXML:= TRttiXMLConfig.Create(XML_File);
     end
     else XML_File:= aXML.Filename;

     if Sources.Save(aXML, XMLRoot_Path, False) then
     begin
       if Assigned(OnSaveSource) then OnSaveSource(Self, aXML, IsAutoSave, XMLRoot_Path);

       if aFree then
       begin
         aXML.Free; aXML:= nil;
       end;

       //Cannot use SaveParams=True in Sources.Save
       //FPC Bug?
       //If a key like "Source/Params" is written to the same open file, even after a flush, it is ignored.
       //So we do it after destroying XML.

       if (Sources.Selected <> nil) and
          (Sources.Selected^.Inst <> nil)
       then Sources.Selected^.Inst.Params.Save(PChar(XML_File), PChar(XMLRoot_Path+'Source/Params'));

       if IsAutoSave then rModified:= True;
     end;

  finally
    if aFree and (aXML <> nil) then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   curItemPath: String;
   i, iCount: Integer;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     //Load SourceFiles
     SourceFiles:= nil; //Avoid possible data overlaps by eliminating any existing array
     iCount:= aXML.GetValue(SES_SourceFiles+'Count', 0);
     rSourceFilesIndex:= aXML.GetValue(SES_SourceFiles+'SourceFilesIndex', -1);
     rLastCroppedIndex:= aXML.GetValue(SES_SourceFiles+'LastCroppedIndex', -1);
     rLastTakedLength:= aXML.GetValue(SES_SourceFiles+'LastTakedLength', 0);
     SetLength(SourceFiles, iCount);
     for i:=0 to iCount-1 do
     begin
       curItemPath:= SES_SourceFiles+'Item' + IntToStr(i)+'/';
       SourceFiles[i].cCount:= aXML.GetValue(curItemPath+'cCount', 0);
       SourceFiles[i].cStart:= aXML.GetValue(curItemPath+'cStart', 0);
       SourceFiles[i].fName:= RelativePathToFullPath(Path_Session, aXML.GetValue(curItemPath+'fName', ''));
     end;

     if Assigned(OnLoadSourceFiles) then OnLoadSourceFiles(Self, aXML, IsAutoSave);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveSourceFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   i: Integer;
   curItemPath: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     //Save SourceFiles array
     aXML.DeletePath(SES_SourceFiles);
     aXML.SetValue(SES_SourceFiles+'Count', Length(SourceFiles));
     aXML.SetValue(SES_SourceFiles+'SourceFilesIndex', rSourceFilesIndex);
     aXML.SetValue(SES_SourceFiles+'LastCroppedIndex', rLastCroppedIndex);
     aXML.SetValue(SES_SourceFiles+'LastTakedLength', rLastTakedLength);
     for i:=0 to Length(SourceFiles)-1 do
     begin
       curItemPath:= SES_SourceFiles+'Item' + IntToStr(i)+'/';
       aXML.SetValue(curItemPath+'cCount', SourceFiles[i].cCount);
       aXML.SetValue(curItemPath+'cStart', SourceFiles[i].cStart);
       aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(Path_Session, SourceFiles[i].fName));
     end;

     if Assigned(OnSaveSourceFiles) then OnSaveSourceFiles(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

function TDigIt_Session.LoadDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean): Integer;
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     //Load Destination and its Params
     { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
 (*
     newDestinationName:= aXML.GetValue('Destination/Name', '');
     if (newDestinationName = '')
     then begin
            rDestination:= nil;
            rDestinationParams:= nil;
 *)
            rDestinationName:= '';
            Path_Session_Pictures:= RelativePathToFullPath(Path_Session, aXML.GetValue('Destination/Params/Path', ''));

            //Load Format and Create Writer
            SaveFormat:= ifJpeg;
            aXML.GetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
            SetSaveWriter(SaveFormat);
            aXML.ReadObject('Destination/Params/Writer/', SaveWriter);
            Result:= 0;
 (*
          end
     else begin
            if theBridge.DestinationsImpl.Select(newDestinationName) then
            begin
              rDestination:= theBridge.DestinationsImpl.Selected;
              rDestinationName:= theBridge.DestinationsImpl.SelectedName;
              rDestinationParams:= theBridge.DestinationsImpl.SelectedParams;
              Result:= theBridge.DestinationsImpl.SelectedIndex+1;
              theBridge.DestinationsImpl.LoadSelectedParams(aXML.Filename, 'Destination/Params');
            end;
          end;
 *)
     if Assigned(OnLoadDestination) then OnLoadDestination(Self, aXML, IsAutoSave);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveDestination(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     //Save rDestination and its Params
(*     aXML.SetValue('Destination/Name', rDestinationName);
     if (rDestination = nil) then
     begin
*)
       aXML.DeletePath('Destination/Params/');
       aXML.SetValue('Destination/Params/Path', FullPathToRelativePath(Path_Session, Path_Session_Pictures));

       aXML.SetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
       aXML.WriteObject('Destination/Params/Writer/', SaveWriter);

//     end;

     if Assigned(OnSaveDestination) then OnSaveDestination(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   i,
   newCount: Integer;
   curAge: Longint;
   cuFileName,
   curItemPath: String;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     newCount := aXML.GetValue(SES_CapturedFiles+'Count', 0);
     rCapturedFilesIndex:= aXML.GetValue(SES_CapturedFiles+'CapturedFilesIndex', -1);
     rCapturedFilesSelected :=aXML.GetValue(SES_CapturedFiles+'Selected', -1);

     SetLength(CapturedFiles, newCount);
     for i:=0 to newCount-1 do
     begin
         curItemPath :=SES_CapturedFiles+'Item' + IntToStr(i)+'/';
         CapturedFiles[i].fAge:= aXML.GetValue(curItemPath+'fAge', 0);
         CapturedFiles[i].fName:= RelativePathToFullPath(Path_Session, aXML.GetValue(curItemPath+'fName', ''));
         CapturedFiles[i].iIndex:= aXML.GetValue(curItemPath+'iIndex', 0);

         cuFileName:= CapturedFiles[i].fName;

         //if FileExists leave it's Index else set to 0, -1 if exists but has a different Age
         if FileExists(cuFileName)
         then begin
                curAge:= FileAge(cuFileName);

                //File is Changed
                if (CapturedFiles[i].fAge <> curAge) then
                begin
                  CapturedFiles[i].fAge:= curAge;
                  CapturedFiles[i].iIndex:= -1;
                end;
              end
        else CapturedFiles[i].iIndex:= 0;
     end;

     if Assigned(OnLoadCapturedFiles) then OnLoadCapturedFiles(Self, aXML, IsAutoSave);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   i,
   lenCapturedFiles: Integer;
   curExt,
   curItemPath: String;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     if IsAutoSave
     then curExt:= Ext_AutoThumb
     else curExt:= Ext_Thumb;

     //Save CapturedFiles array
     aXML.DeletePath(SES_CapturedFiles);
     lenCapturedFiles:= Length(CapturedFiles);

     if (lenCapturedFiles > 0) then
     begin
       aXML.SetValue(SES_CapturedFiles+'Count', Length(CapturedFiles));
       aXML.SetValue(SES_CapturedFiles+'CapturedFilesIndex', rCapturedFilesIndex);

       aXML.SetValue(SES_CapturedFiles+'Selected', rCapturedFilesSelected);

       for i:=0 to Length(CapturedFiles)-1 do
       begin
         curItemPath :=SES_CapturedFiles+'Item' + IntToStr(i)+'/';
         aXML.SetValue(curItemPath+'fAge', CapturedFiles[i].fAge);
         aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(Path_Session, CapturedFiles[i].fName));
         aXML.SetValue(curItemPath+'iIndex', CapturedFiles[i].iIndex);
       end;
     end;

     if Assigned(OnSaveCapturedFiles) then OnSaveCapturedFiles(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     LoadImage(RelativePathToFullPath(Path_Session, aXML.GetValue('LoadedFile', '')), False); //DON'T set to True, if you do not want infinite recursion

     if Assigned(OnLoadLoadedImage) then OnLoadLoadedImage(Self, aXML, IsAutoSave);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveLoadedImage(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     aXML.SetValue('LoadedFile', FullPathToRelativePath(Path_Session, rLoadedImageFile));

     if Assigned(OnSaveLoadedImage) then OnSaveLoadedImage(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveSource_CapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     SaveSourceFiles(aXML, IsAutoSave);
     SaveCapturedFiles(aXML, IsAutoSave);
     SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveSource_CapturedIndexes(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     if (Length(SourceFiles) > 0) then
     begin
       aXML.SetValue(SES_SourceFiles+'SourceFilesIndex', rSourceFilesIndex);
       aXML.SetValue(SES_SourceFiles+'LastCroppedIndex', rLastCroppedIndex);
       aXML.SetValue(SES_SourceFiles+'LastTakedLength', rLastTakedLength);
     end;

     if (Length(CapturedFiles) > 0) then aXML.SetValue(SES_SourceFiles+'CapturedFilesIndex', rCapturedFilesIndex);

     aXML.SetValue(SES_CapturedFiles+'Selected', rCapturedFilesSelected);

     SaveLoadedImage(aXML, IsAutoSave);

     if Assigned(OnSaveSource_CapturedIndexes) then OnSaveSource_CapturedIndexes(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   i, newCount, newSelected: integer;
   curItemPath: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
             then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
             else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     newCount:= aXML.GetValue(SES_CropAreas+'Count', -1);
     //newSelected:= aXML.GetValue(SES_CropAreas+'Selected', -1);  IN UI See TCropAreaList.Load

     CropAreas:= nil;
     SetLength(CropAreas, newCount);

     for i:=0 to newCount-1 do
     begin
       curItemPath:= SES_CropAreas+'Item'+IntToStr(i)+'/';

       //Area Unit
       CropAreas[i].PhysicalUnit:= cuCentimeter;
       aXML.GetValue(curItemPath+'AreaUnit', CropAreas[i].PhysicalUnit, TypeInfo(TPhysicalUnit));

       //Area Coordinates
       CropAreas[i].Left:= StrToFloat(aXML.GetValue(curItemPath+'Area/Left', '0'));
       CropAreas[i].Top:= StrToFloat(aXML.GetValue(curItemPath+'Area/Top', '0'));
       CropAreas[i].Width:= StrToFloat(aXML.GetValue(curItemPath+'Area/Width', '0'));
       CropAreas[i].Height:= StrToFloat(aXML.GetValue(curItemPath+'Area/Height', '0'));
     end;

     if Assigned(OnLoadCropAreas) then OnLoadCropAreas(Self, aXML, IsAutoSave);

  finally
     if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   i: integer;
   curItemPath: String;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     aXML.DeletePath(SES_CropAreas);

     aXML.SetValue(SES_CropAreas+'Count', Length(CropAreas));
     //aXML.SetValue(SES_CropAreas+'Selected', fOwner.SelectedCropArea.Index); IN UI See TCropAreaList.Save

     for i:=0 to Length(CropAreas)-1 do
     begin
       curItemPath:= SES_CropAreas+'Item' + IntToStr(i)+'/';

       //Area Unit
       aXML.SetValue(curItemPath+'AreaUnit', CropAreas[i].PhysicalUnit, TypeInfo(TPhysicalUnit));

       //Area Coordinates
       aXML.SetValue(curItemPath+'Area/Left', FloatToStr(CropAreas[i].Left));
       aXML.SetValue(curItemPath+'Area/Top', FloatToStr(CropAreas[i].Top));
       aXML.SetValue(curItemPath+'Area/Width', FloatToStr(CropAreas[i].Width));
       aXML.SetValue(curItemPath+'Area/Height', FloatToStr(CropAreas[i].Height));
     end;

     if Assigned(OnSaveCropAreas) then OnSaveCropAreas(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     PageResize:= resFullsize;
     aXML.GetValue(SES_PageSettings+'PageResize', PageResize, TypeInfo(TDigItFilter_Resize));

     aXML.ReadObject(SES_PageSettings+'PageSize', rPageSize);

     PageRotate:= rotNone;
     aXML.GetValue(SES_PageSettings+'PageRotate', PageRotate, TypeInfo(TDigItFilter_Rotate));

     PageFlip:= flipNone;
     aXML.GetValue(SES_PageSettings+'PageFlip', PageFlip, TypeInfo(TDigItFilter_Flip));

     if Assigned(OnLoadPageSettings) then OnLoadPageSettings(Self, aXML, IsAutoSave);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SavePageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(Path_Session+rFileName+Ext_Sess);

     if (rPageSize.Width = 0) or
        (rPageSize.Height = 0) then PageResize:= resFullsize;

     aXML.SetValue(SES_PageSettings+'PageResize', PageResize, TypeInfo(TDigItFilter_Resize));
     aXML.WriteObject(SES_PageSettings+'PageSize', rPageSize);

     aXML.SetValue(SES_PageSettings+'PageRotate', PageRotate, TypeInfo(TDigItFilter_Rotate));
     aXML.SetValue(SES_PageSettings+'PageFlip', PageFlip, TypeInfo(TDigItFilter_Flip));

     if Assigned(OnSavePageSettings) then OnSavePageSettings(Self, aXML, IsAutoSave);

     if IsAutoSave then rModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.Clear_SourceFiles(ClearSourceInst: Boolean);
begin
  try
     rSourceFilesIndex:= -1;
     rLastCroppedIndex:= -1;
     rLastTakedLength:= 0;
     SourceFiles:= nil;
     if ClearSourceInst and (Sources.Selected <> nil) then Sources.Selected^.Inst.Clear;

  finally
    SaveSource_CapturedFiles(nil, True);
  end;
end;

procedure TDigIt_Session.Clear_Captured;
begin
  rCapturedFilesIndex:= -1;
  CapturedFiles:= nil;
end;

procedure TDigIt_Session.CropImage(ABitmap: TBGRABitmap; IsReCrop: Boolean);
var
  savedFile: String;

begin
  //Increment the Counter Value
  Counter.Value:= Counter.Value+1;

  inc(rCapturedFilesIndex);

  //Save File
  savedFile:= SaveImage(ABitmap, Counter.GetValue);

  if IsReCrop
  then begin
         if (rCapturedFilesIndex > -1) and (rCapturedFilesIndex < Length(CapturedFiles)) then
         begin
           //ReCrop, Update the image in Captured List
           CapturedFiles[rCapturedFilesIndex].fName:= savedFile;
           CapturedFiles[rCapturedFilesIndex].fAge:= FileAge(savedFile);
         end;
       end
  else begin
         //Crop, add file to Captured List
         rCapturedFilesIndex:= Length(CapturedFiles);

         SetLength(CapturedFiles, rCapturedFilesIndex+1);
         CapturedFiles[rCapturedFilesIndex].fName:= savedFile;
         CapturedFiles[rCapturedFilesIndex].fAge:= FileAge(savedFile);
         CapturedFiles[rCapturedFilesIndex].iIndex:= 0;
       end;

   if Assigned(OnCropImage) then OnCropImage(Self, ABitmap, rCapturedFilesIndex, IsReCrop);
end;

procedure TDigIt_Session.CropFile_Full(AStartIndex: Integer; isReTake: Boolean);
var
   i,
   c,
   old_CounterValue,
   old_CapturedFilesIndex: Integer;
   cStr: String;
   UserCancel,
   KeepFiles: Boolean;

begin
  try
     UserCancel:= False;

     //Store old Values so if user Cancel Operation we can rollback
     old_CapturedFilesIndex:= rCapturedFilesIndex;
     old_CounterValue:= Counter.Value;

     c:= Length(SourceFiles);
     cStr:= IntToStr(c);
     theBridge.ProgressSetTotal(AStartIndex, c, AStartIndex);

     for i:=AStartIndex to c-1 do
     begin
       UserCancel:= theBridge.ProgressSetTotal(Format(rsProcessing, [i, cStr]), i);
       if UserCancel then break;

       UserCancel:= not(LoadImage(SourceFiles[i].fName, False));
       if UserCancel then break;

       try
          //SaveCallBack(imgManipulation.Bitmap, nil, Integer( (isReTake and (i < lastLenTaked)) ));
          CropImage(rBitmap, isReTake and (i < rLastTakedLength));
       except
         UserCancel:= True;
         break;
       end;

       UserCancel:= theBridge.ProgressSetTotal(Format(rsProcessed, [i, cStr]), i+1);
       if UserCancel then break;
     end;

     if UserCancel
     then KeepFiles:= not(theBridge.MessageDlg('DigIt', rsErrIncomplete, mtConfirmation, [mbYes, mbNo], 0) = mrNo)
     else KeepFiles:= True;

     if Assigned(OnCropFile_Full) then OnCropFile_Full(Self, UserCancel, KeepFiles, old_CounterValue, old_CapturedFilesIndex);

     if UserCancel and not(KeepFiles) then
     begin
       if (Length(CapturedFiles) > old_CapturedFilesIndex+1) then
         for i:=old_CapturedFilesIndex+1 to rCapturedFilesIndex do
           DeleteFile(CapturedFiles[i].fName);

       SetLength(CapturedFiles, old_CapturedFilesIndex+1);
       rCapturedFilesIndex:= old_CapturedFilesIndex;
       Counter.Value:= old_CounterValue;
     end;

  finally
  end;
end;

procedure TDigIt_Session.CropFiles(ASourceFileIndex: Integer; isReTake: Boolean);
var
   oldCount: DWord;
   i,
   lenCropAreas: Integer;
   curBitmap: TBGRABitmap;
   curRect: TRect;

begin
  rSourceFilesIndex:= ASourceFileIndex;
  lenCropAreas:= Length(CropAreas);

  if isReTake
  then begin
         oldCount:= SourceFiles[rSourceFilesIndex].cCount;

         if (oldCount <> lenCropAreas) then
         begin
           if (oldCount < lenCropAreas)
           then begin
                  { #todo 10 -oMaxM : Re index - add space for more files }
                  theBridge.MessageDlg('DigIt', 'To-Do: add space for more files', mtInformation, [mbOk], 0);
                end
           else begin
                  { #todo 10 -oMaxM : Re index - delete extra files }
                  theBridge.MessageDlg('DigIt', 'To-Do: delete extra files', mtInformation, [mbOk], 0);
                end;
         end;

         Counter_Assign(SourceFiles[rSourceFilesIndex].cStart);
       end
  else begin
         if (rSourceFilesIndex < rLastCroppedIndex) then
         begin
           { #todo 10 -oMaxM : Re index - delete extra files }
           theBridge.MessageDlg('DigIt', 'To-Do: insert files', mtInformation, [mbOk], 0);
         end;

         SourceFiles[rSourceFilesIndex].cStart:= Counter.Value;
       end;

  //imgManipulation.getAllBitmaps(@SaveCallBack, Integer(isReTake), True);
  for i:=0 to lenCropAreas-1 do
  try
     //ONLY PIXELS Wait for BGRABitmap Pull Request
    curRect.Top:= Trunc(CropAreas[i].Top);
    curRect.Left:= Trunc(CropAreas[i].Left);
    curRect.Bottom:= HalfUp(CropAreas[i].Bottom);
    curRect.Right:= HalfUp(CropAreas[i].Right);

    curBitmap:= rBitmap.GetPart(curRect, True, False);
    CropImage(curBitmap, isReTake);

  finally
    if (curBitmap<>nil) then curBitmap.Free;
  end;

  SourceFiles[rSourceFilesIndex].cCount:= lenCropAreas;
end;

function TDigIt_Session.actPreview: Boolean;
var
  curImageFile: String;

begin
  try
     Result:= Sources.Take(takeActPreview, curImageFile);
     if Result then Result:= LoadImage(curImageFile, True);
     if not(Result) then theBridge.MessageDlg(rsNoFilesDownloaded, mtError, [mbOk], 0);

  finally
  end;
end;

function TDigIt_Session.actTake(isReTake: Boolean; CustomTake: TCustomTakeMethod): DWord;
var
   StartIndex,
   oldLength,
   numTaked, i: Integer;
   hasCropped: Boolean;

begin
  Result:= 0;

  if isReTake and (theBridge.MessageDlg('DigIt', Format(rsTakeAgain, [rLastTakedLength]),
                                        mtConfirmation, [mbYes, mbNo], 0) = mrNo) then exit;
  try
    Case CropMode of
      diCropFull: if isReTake then Counter_Dec(rLastTakedLength);
      diCropCustom: begin
        oldLength:= Length(SourceFiles);
        numTaked:= rLastTakedLength;
        hasCropped:= (oldLength-numTaked <= rLastCroppedIndex);

        //If we have processed all queue files automatically clear it
        if not(isRetake) and
           (oldLength > 0) and
           (rSourceFilesIndex >= 0) and (rSourceFilesIndex >= oldLength) then
        begin
          Clear_SourceFiles(False);
          oldLength:= 0;
        end;
      end;
    end;

    if isRetake
    then StartIndex:= oldLength-rLastTakedLength
    else StartIndex:= oldLength;

    if Assigned(CustomTake)
    then Result:= CustomTake(takeActTake, SourceFiles, StartIndex)
    else Result:= Sources.Take(takeActTake, SourceFiles, StartIndex);

    if (Result > 0) then
    begin
      Case CropMode of
        diCropFull: begin
          theBridge.ProgressShow(rsProcessingImages, 1, Result);

          CropFile_Full(0, isRetake);

          rLastTakedLength:= Result;
          SourceFiles:= nil; rSourceFilesIndex:= -1;
          Sources.Selected^.Inst.Clear;
        end;
        diCropCustom: begin
          if isRetake
          then begin
                 if (rSourceFilesIndex > Length(SourceFiles))
                 then rSourceFilesIndex:= Length(SourceFiles); //Repos rSourceFilesIndex if outside
               end
          else begin
                 //The queue was empty, Start from the first file
                 if (oldLength = 0) and (Length(SourceFiles) > 0) then
                 begin
                   rSourceFilesIndex:= 0;
                   LoadImage(SourceFiles[0].fName, False);

                   //Crop the First File directly
                   actCropNext;
                 end;
               end;

            rLastTakedLength:= Result;
        end;
      end;

      if isRetake and hasCropped then
      begin
        rSourceFilesIndex:= oldLength-numTaked;
        for i:=oldLength-numTaked to oldLength-1 do
        begin
          if (SourceFiles[i].cCount > 0) then
          begin
            rSourceFilesIndex:= i;
            LoadImage(SourceFiles[i].fName, False);
            CropFiles(i, True);
          end;
        end;
      end;
   end
   else theBridge.MessageDlg(rsNoFilesDownloaded, mtError, [mbOk], 0);

  finally
    Save(True);
    theBridge.ProgressHide;
  end;
end;

procedure TDigIt_Session.actTimerTake;
begin
  //
end;

procedure TDigIt_Session.actCropNext;
var
   lenSources: Integer;

  function CheckEndOfFiles: Boolean;
  begin
    Result:= (rSourceFilesIndex >= lenSources);

    if Result and (lenSources > 1) then
    begin
      if not(theBridge.MessageDlg('DigIt', rsNoMoreFiles, mtConfirmation, [mbYes, mbNo], 0) = mrNo)
      then Clear_SourceFiles(True);
    end;
  end;

begin
  try
    lenSources:= Length(SourceFiles);
    if (rSourceFilesIndex = -1) then rSourceFilesIndex:= 0;

    if (rSourceFilesIndex = lenSources) or
       (SourceFiles[rSourceFilesIndex].cCount > 0)
    then CropFiles(rSourceFilesIndex, True) //Re Crop
    else begin
           CropFiles(rSourceFilesIndex, False);

           if (rSourceFilesIndex > rLastCroppedIndex) then rLastCroppedIndex:= rSourceFilesIndex;

           inc(rSourceFilesIndex);
         end;

    if not(CheckEndOfFiles)
    then if (rSourceFilesIndex > -1) and (rSourceFilesIndex < Length(SourceFiles))
         then if not(LoadImage(SourceFiles[rSourceFilesIndex].fName, False))
              then begin
                     { #todo -oMaxM : do something if LoadImage Fails? }
                   end;

  finally
    SaveSource_CapturedFiles(nil, True);
  end;
end;

procedure TDigIt_Session.actGoNext;
var
   new_SourceFilesIndex: Integer;

begin
  try
     new_SourceFilesIndex:= rSourceFilesIndex;
     if (new_SourceFilesIndex >= 0) and (new_SourceFilesIndex < Length(SourceFiles)-1) then
     begin
       inc(new_SourceFilesIndex);
       if LoadImage(SourceFiles[new_SourceFilesIndex].fName, False) then
       begin
         if (SourceFiles[new_SourceFilesIndex].cCount > 0)
         then Counter_Assign(SourceFiles[new_SourceFilesIndex].cStart);
         rSourceFilesIndex:= new_SourceFilesIndex;
       end;
     end;

  finally
    SaveSource_CapturedFiles(nil, True);
  end;
end;

procedure TDigIt_Session.actGoBack;
var
   new_SourceFilesIndex: Integer;

begin
  try
     new_SourceFilesIndex:= rSourceFilesIndex;
     if (new_SourceFilesIndex > 0) then
     begin
       //We've already cut the last one, start with the penultimate one
       if (new_SourceFilesIndex >= Length(SourceFiles)) then new_SourceFilesIndex:= Length(SourceFiles)-1;

       dec(new_SourceFilesIndex);

       if LoadImage(SourceFiles[new_SourceFilesIndex].fName, False) then
       begin
         if (SourceFiles[new_SourceFilesIndex].cCount > 0)
         then Counter_Assign(SourceFiles[new_SourceFilesIndex].cStart);
         rSourceFilesIndex:= new_SourceFilesIndex;
       end;
     end;

  finally
    SaveSource_CapturedFiles(nil, True);
  end;
end;

procedure TDigIt_Session.actCropAll;
var
   c: Integer;
   cStr: String;
   Finished: Boolean;

   function CheckEndOfFiles: Boolean;
   begin
     Result:= (rSourceFilesIndex >= Length(SourceFiles));
     if Result then
     begin
       //User Confirmation
       if not(theBridge.MessageDlg('DigIt', rsNoMoreFiles, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
       begin
         rSourceFilesIndex:= -1;
         SourceFiles:= nil;
         Sources.Selected^.Inst.Clear;
       end;
     end;
   end;

begin
  try
    Finished:= False;
    c:= Length(SourceFiles);

    if (rLastCroppedIndex > -1) then
    begin
      rSourceFilesIndex:= rLastCroppedIndex+1;
      LoadImage(SourceFiles[rSourceFilesIndex].fName, False);
      Counter_Assign(SourceFiles[rLastCroppedIndex].cStart+SourceFiles[rLastCroppedIndex].cCount);
    end;

    cStr:= IntToStr(c);
    theBridge.ProgressShow(rsProcessingImages, rSourceFilesIndex, c);

    repeat
      if theBridge.ProgressSetTotal(Format(rsProcessing, [rSourceFilesIndex, cStr]), rSourceFilesIndex) then break;

      CropFiles(rSourceFilesIndex, False);
      inc(rSourceFilesIndex);

      Finished:= CheckEndOfFiles;
      if not(Finished)
      then if not(LoadImage(SourceFiles[rSourceFilesIndex].fName, False))
           then begin
                  { #todo -oMaxM : do something if LoadImage Fails? }
                  break;
                end;

//      lvCaptured.Selected:= lvCaptured.Items[rCapturedFilesIndex];

      if theBridge.ProgressSetTotal(Format(rsProcessed, [rSourceFilesIndex-1, cStr]), rSourceFilesIndex+1) then break;
    Until Finished;

  finally
    SaveSource_CapturedFiles(nil, True);
    theBridge.ProgressHide;
  end;
end;

procedure TDigIt_Session.actClearQueue;
begin
  if (theBridge.MessageDlg('DigIt', rsClearQueue, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then exit;

  try
    Clear_SourceFiles(True);

  finally
    SaveSource_CapturedFiles(nil, True);
  end;
end;

procedure TDigIt_Session.actCapturedDeleteAll(UserConfirm: Boolean);
var
   i: Integer;

begin
  if UserConfirm and
     (theBridge.MessageDlg('DigIt', rsDeleteAll, mtConfirmation, [mbYes, mbNo], 0) = mrNo) then exit;

  //Delete all File in FileSystem ?
  if (theBridge.MessageDlg('DigIt', rsDeleteAllFiles, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then for i:=0 to Length(CapturedFiles)-1 do
         DeleteFile(CapturedFiles[i].fName);

  //Clear Array and ListView
  Clear_Captured;

  //Reset Counter
  Counter.Reset;

  EmptyImage(False);

  Save(True);
end;

procedure TDigIt_Session.actCapturedDelete(UserConfirm: Boolean; AIndex: Integer);
var
   iCur, iSelected: Integer;

begin
  { #todo -oMaxM : this works ONLY in FullArea Mode, in Custom? Delete only the Item?  }
  if UserConfirm and
     (theBridge.MessageDlg('DigIt', Format(rsDeleteCaptured, [ExtractFileName(CapturedFiles[AIndex].fName)]),
                           mtConfirmation, [mbYes, mbNo], 0) = mrNo) then exit;
  try
     //Delete File and Thumb
     DeleteFile(CapturedFiles[AIndex].fName);

     if (AIndex < Length(CapturedFiles)-1) then
       for iCur:=AIndex+1 to Length(CapturedFiles)-1 do
       begin
         RenameFile(CapturedFiles[iCur].fName, CapturedFiles[iCur-1].fName);

         CapturedFiles[iCur-1].fAge:= CapturedFiles[iCur].fAge;
       end;

     //Delete Last Item
     SetLength(CapturedFiles, Length(CapturedFiles)-1);
     rCapturedFilesIndex:= Length(CapturedFiles)-1;

  finally
    if (Counter.Value >= Length(CapturedFiles)) then Counter.Value:= Length(CapturedFiles)-1;
  end;
end;

procedure TDigIt_Session.GetEnabledActions(out actPreview_Enabled, actTake_Enabled, actTakeRe_Enabled,
                                           actCropNext_Enabled,
                                           actGoNext_Enabled, actGoBack_Enabled,
                                           actCropAll_Enabled, actClearQueue_Enabled: Boolean);
var
   bCommonCond,
   bCropEnabled: Boolean;
   lenSources: Integer;

begin
  bCommonCond:= (Sources.Selected<>nil) and (Sources.Selected^.Inst <> nil);

  actPreview_Enabled:= bCommonCond;
  actTake_Enabled:= bCommonCond; // and DirectoryExists(Path_Session_Pictures)
  actTakeRe_Enabled:= bCommonCond and (rLastTakedLength > 0);

  if (rCropMode = diCropCustom) then
  begin
    lenSources:= Length(SourceFiles);
    bCommonCond:= bCommonCond and
                  (rBitmap<>nil) and not(rBitmap.Empty) and
                  (Length(CropAreas) > 0)
                  (*and DirectoryExists(Path_Session_Pictures)*);

    bCropEnabled:= bCommonCond and
                   ((lenSources = 1) or (rSourceFilesIndex >= 0) and (rSourceFilesIndex >= lenSources-1));
    actCropNext_Enabled:= bCommonCond and (lenSources > 0);
                          //bCommonCond and (lenSources > 1) and (rSourceFilesIndex >= 0) and (rSourceFilesIndex < lenSources-1);
    actGoNext_Enabled:= actCropNext_Enabled and not(bCropEnabled);
    actGoBack_Enabled:= bCommonCond and (lenSources > 1) and (rSourceFilesIndex > 0) and (rSourceFilesIndex <= lenSources);
    actCropAll_Enabled:= actCropNext_Enabled and not(bCropEnabled);

    actClearQueue_Enabled:= bCommonCond and (lenSources > 0);
  end;
end;

procedure TDigIt_Session.GetEnabledActions_Captured(out actCapturedDeleteAll_Enabled, actCapturedDelete_Enabled: Boolean);
var
   lenCaptured: Integer;

begin
  lenCaptured:= Length(CapturedFiles);

  actCapturedDeleteAll_Enabled:= (lenCaptured > 0);
  actCapturedDelete_Enabled:= (lenCaptured > 0) and
                              (rCropMode = diCropFull) and { #todo 2 -oMaxM : evaluate the complexity in case of multiple crop areas}
                              (rCapturedFilesSelected in [0..lenCaptured-1]);
end;


end.

