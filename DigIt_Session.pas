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
  {$ifdef BGRAControls} BGRAImageManipulation, {$endif}
  DigIt_Types;

resourcestring
  rsSavingWork = 'Saving the Work Session';
  rsSavingSources = 'Saving Sources Files';
  rsSavingCaptured = 'Saving Captured Files';
  rsSavingSessionFiles = 'Saving Session Files';
  rsSavingSwitch = 'Switching to New Work Session';
  rsSavingDone = 'Saved Done';

type

  { TDigIt_Session }

  TDigIt_Session = class
  protected
    rLoading,
    rSessionModified: Boolean;
    rPath,
    rPath_Scan,
    rPath_Pictures,
    rFileName: String;

    testI,
    lastCropped,
    lastLenTaked,     //Used in Take Again
    iSourceFiles,
    iCapturedFiles,
    sCapturedFiles: Integer;
    SourceFiles: TSourceFileArray;

    CapturedFiles: TCapturedFileArray;
    CropMode: TDigItCropMode;

    rPageResizeUnitType: TDigItResizeUnitType;
    PageResize: TDigItFilter_Resize;
    PageResizeInfo: TImageResolutionInfo;
    PageRotate: TDigItFilter_Rotate;
    PageFlip: TDigItFilter_Flip;

    { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
    (*    rDestination: PDestinationInfo;
    rDestinationParams: IDigIt_Params;
    *)
    rDestinationName: String;

    SaveFormat: TBGRAImageFormat;
    SaveWriter: TFPCustomImageWriter;
    SaveExt: String;

    rLoadedFile: String;

    rBitmap: TBGRABitmap;

    {$ifdef BGRAControls}
    rImageManipulation: TBGRAImageManipulation;
    {$else}
    {$endif}

    rAdditionalLoad,
    rAdditionalSave: TLoadSaveXMLMethod;

  public
    constructor Create;
    destructor Destroy; override;

    function LoadImage(AImageFile: String; saveToXML: Boolean): Boolean;
    procedure EmptyImage(saveToXML: Boolean);

    function ResizeImage(ABitmap :TBGRABitmap;
                         APageResize: TDigItFilter_Resize; APageResizeInfo: TImageResolutionInfo): TBGRABitmap;
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
(* oldcode    procedure LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    procedure SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
    *)

    (*
    procedure setCropMode(ANewCropMode: TDigItCropMode);

    procedure SourceFiles_Clear(ClearSourceInst: Boolean);

    procedure CropFile_Full(AStartIndex: Integer; isReTake: Boolean); overload;
    procedure CropFiles(ASourceFileIndex: Integer; isReTake: Boolean);
    *)
    procedure SetSaveWriter(AFormat: TBGRAImageFormat);

    property Loading: Boolean read rLoading;

    property AdditionalLoad: TLoadSaveXMLMethod read rAdditionalLoad write rAdditionalLoad;
    property AdditionalSave: TLoadSaveXMLMethod read rAdditionalSave write rAdditionalSave;
  end;

implementation

uses Graphics, FileUtil, LazFileUtils,
     MM_StrUtils,
     BGRAWriteJPeg,
     Digit_Bridge_Impl, DigIt_Utils, DigIt_Sources, DigIt_Counter;

constructor TDigIt_Session.Create;
begin
  inherited Create;

  //Set Default Startup Values
  SourceFiles:= nil;
  iSourceFiles:= -1;
  CapturedFiles:= nil;
  iCapturedFiles:= -1;
  lastCropped:= -1;
  lastLenTaked:= 0;

  rLoadedFile:= '';

  rPageResizeUnitType:= ruFullsize;
  PageResize:= resNone;
  PageRotate:= rotNone;
  PageFlip:= flipNone;

  CropMode:= diCropNull; //setCropMode works only if there are changes

  //Set Default Session Values
  Path_Session:= Path_DefSession;
  Path_Session_Scan:= Path_DefSession_Scan;
  Path_Session_Pictures:= Path_DefSession_Pictures;
  rFileName:= File_DefSession;
  rSessionModified:= False;
end;

destructor TDigIt_Session.Destroy;
begin
  inherited Destroy;
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
     if (PageResize <> resNone) then
     begin
       BitmapR:= ResizeImage(BitmapN, PageResize, PageResizeInfo);
       if (BitmapR<>nil) then
       begin
         BitmapN.Free;
         BitmapN:= BitmapR;
         BitmapR:= nil;
       end;
     end;

     rBitmap:= BitmapN;

     rLoadedFile:= AImageFile;

     if saveToXML then SaveLoadedImage(nil, True);

     Result:= True;

  finally
     if (BitmapN <> Nil) then BitmapN.Free;
  end;
end;

procedure TDigIt_Session.EmptyImage(saveToXML: Boolean);
begin
  rBitmap:= nil;
  rLoadedFile:= '';
  if saveToXML then SaveLoadedImage(nil, True);
end;

function TDigIt_Session.ResizeImage(ABitmap: TBGRABitmap;
                                    APageResize: TDigItFilter_Resize; APageResizeInfo: TImageResolutionInfo): TBGRABitmap;
var
   newWidth, newHeight: Single;
   pixelWidth, pixelHeight: Integer;

begin
  Result:= nil;

  if (APageResize <> resNone) then
  begin
    if (APageResizeInfo.ResolutionUnit = ruNone)
    then begin
           pixelWidth:= Trunc(APageResizeInfo.ResolutionX);
           pixelHeight:= Trunc(APageResizeInfo.ResolutionY);
         end
    else begin
           newWidth:= PhysicalSizeConvert(APageResizeInfo.ResolutionUnit,
                                          APageResizeInfo.ResolutionX,
                                          ABitmap.ResolutionUnit);
           newHeight:= PhysicalSizeConvert(APageResizeInfo.ResolutionUnit,
                                           APageResizeInfo.ResolutionY,
                                           ABitmap.ResolutionUnit);

           pixelWidth:= HalfUp(newWidth*ABitmap.ResolutionX);
           pixelHeight:= HalfUp(newHeight*ABitmap.ResolutionY);
         end;

    Case APageResize of
    resFixedWidth: pixelHeight:= GetProportionalSide(pixelWidth, ABitmap.Width, ABitmap.Height);
    resFixedHeight: pixelWidth:= GetProportionalSide(pixelHeight, ABitmap.Height, ABitmap.Width);
    end;

    Result:= ABitmap.Resample(pixelWidth, pixelHeight, rmFineResample, True);
  end;
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
     rFileName:= AFile;

     Load(IsAutoSave);

     Result:= True;

  except
     Result:= False;
  end;

  if not(Result) then
  begin
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

       if theBridge.Progress.Cancelled then exit;
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

       if theBridge.Progress.Cancelled then exit;
     end;

     if theBridge.ProgressSetTotal(rsSavingSessionFiles, 3) then exit;

     //Copy Loaded File
     curFileNameR:= FullPathToRelativePath(Path_Session, rLoadedFile, isRelative);
     if isRelative then
     begin
       curFileName:= RelativePathToFullPath(newPath_Session, curFileNameR);
       ForceDirectories(ExtractFilePath(curFileName));
       CopyFile(rLoadedFile, curFileName, True, False);
       if isMove then DeleteFile(rLoadedFile);

       rLoadedFile:= curFileName;
     end;

     if theBridge.Progress.Cancelled then exit;

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
     then aXML:= TRttiXMLConfig.Create(rPath+rFilename+Ext_AutoSess)
     else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     newSourceI:= LoadSource(aXML, IsAutoSave);
     LoadSourceFiles(aXML, IsAutoSave);
     newDestinationI:= LoadDestination(aXML, IsAutoSave);
     LoadCapturedFiles(aXML, IsAutoSave);
     LoadPageSettings(aXML, IsAutoSave);
     LoadLoadedImage(aXML, IsAutoSave);
     Counter.Load(aXML, 'Counter', True);
     LoadCropAreas(aXML, IsAutoSave);

     if Assigned(rAdditionalLoad) then rAdditionalLoad(aXML, IsAutoSave);

     rSessionModified:= IsAutoSave;

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

     aXML:= TRttiXMLConfig.Create(rPath+rFileName+curExt);

     SaveSource(aXML, IsAutoSave);
     SaveSourceFiles(aXML, IsAutoSave);
     SaveDestination(aXML, IsAutoSave);
     SaveCapturedFiles(aXML, IsAutoSave);
     SavePageSettings(aXML, IsAutoSave);
     SaveLoadedImage(aXML, IsAutoSave);

     Counter.Save(aXML, 'Counter', True);

     SaveCropAreas(aXML, IsAutoSave);

     if Assigned(rAdditionalSave) then rAdditionalSave(aXML, IsAutoSave);

     aXML.Flush;
     aXML.Free;

     //FPC Bug?
     //If a key like "rSource/Params" is written to the same open file, even after a flush, it is ignored.
     //So we do it after destroying XML.

     if (Sources.Selected <> nil) and
        (Sources.Selected^.Inst <> Nil)
     then Sources.Selected^.Inst.Params.Save(PChar(rPath+rFileName+curExt), 'Source/Params');

//     if (rDestination <> nil) then rDestination^.Inst.Params.Save(PChar(rPath+rFileName+curExt), 'Destination/Params');

     if not(IsAutoSave) then rSessionModified:= False;

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
         then XML_File:= rPath+rFileName+Ext_AutoSess
         else XML_File:= rPath+rFileName+Ext_Sess;
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
            Result:= Sources.SelectedIndex;
          end;

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
         then XML_File:= rPath+rFileName+Ext_AutoSess
         else XML_File:= rPath+rFileName+Ext_Sess;
       end;

       aXML:= TRttiXMLConfig.Create(XML_File);
     end
     else XML_File:= aXML.Filename;

     if Sources.Save(aXML, XMLRoot_Path, False) then
     begin
       if aFree then
       begin
         aXML.Free; aXML:= nil;

         //Cannot use SaveParams=True in Sources.Save
         //FPC Bug?
         //If a key like "Source/Params" is written to the same open file, even after a flush, it is ignored.
         //So we do it after destroying XML.

         if (Sources.Selected <> nil) and
            (Sources.Selected^.Inst <> nil)
         then Sources.Selected^.Inst.Params.Save(PChar(XML_File), PChar(XMLRoot_Path+'Source/Params'));
       end;

       if IsAutoSave then rSessionModified:= True;
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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     //Load SourceFiles
     SourceFiles:= nil; //Avoid possible data overlaps by eliminating any existing array
     iCount:= aXML.GetValue(SES_SourceFiles+'Count', 0);
     iSourceFiles:= aXML.GetValue(SES_SourceFiles+'iSourceFiles', -1);
     lastCropped:= aXML.GetValue(SES_SourceFiles+'lastCropped', -1);
     lastLenTaked:= aXML.GetValue(SES_SourceFiles+'lastLenTaked', 0);
     SetLength(SourceFiles, iCount);
     for i:=0 to iCount-1 do
     begin
       curItemPath:= SES_SourceFiles+'Item' + IntToStr(i)+'/';
       SourceFiles[i].cCount:= aXML.GetValue(curItemPath+'cCount', 0);
       SourceFiles[i].cStart:= aXML.GetValue(curItemPath+'cStart', 0);
       SourceFiles[i].fName:= RelativePathToFullPath(rPath, aXML.GetValue(curItemPath+'fName', ''));
     end;

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     //Save SourceFiles array
     aXML.DeletePath(SES_SourceFiles);
     aXML.SetValue(SES_SourceFiles+'Count', Length(SourceFiles));
     aXML.SetValue(SES_SourceFiles+'iSourceFiles', iSourceFiles);
     aXML.SetValue(SES_SourceFiles+'lastCropped', lastCropped);
     aXML.SetValue(SES_SourceFiles+'lastLenTaked', lastLenTaked);
     for i:=0 to Length(SourceFiles)-1 do
     begin
       curItemPath:= SES_SourceFiles+'Item' + IntToStr(i)+'/';
       aXML.SetValue(curItemPath+'cCount', SourceFiles[i].cCount);
       aXML.SetValue(curItemPath+'cStart', SourceFiles[i].cStart);
       aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(rPath, SourceFiles[i].fName));
     end;

     if IsAutoSave then rSessionModified:= True;

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

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
            rPath_Pictures:= RelativePathToFullPath(rPath, aXML.GetValue('Destination/Params/Path', ''));

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     //Save rDestination and its Params
(*     aXML.SetValue('Destination/Name', rDestinationName);
     if (rDestination = nil) then
     begin
*)
       aXML.DeletePath('Destination/Params/');
       aXML.SetValue('Destination/Params/Path', FullPathToRelativePath(rPath, rPath_Pictures));

       aXML.SetValue('Destination/Params/Format', SaveFormat, TypeInfo(TBGRAImageFormat));
       aXML.WriteObject('Destination/Params/Writer/', SaveWriter);

//     end;

    if IsAutoSave then rSessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadCapturedFiles(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   i,
   imgCount,
   newCount: Integer;
   curAge: Longint;
   curExt,
   cuFileName,
   curItemPath: String;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     if IsAutoSave
     then curExt:= Ext_AutoThumb
     else curExt:= Ext_Thumb;

     newCount := aXML.GetValue(SES_CapturedFiles+'Count', 0);
     iCapturedFiles:= aXML.GetValue(SES_CapturedFiles+'iCapturedFiles', -1);
     sCapturedFiles :=aXML.GetValue(SES_CapturedFiles+'Selected', -1);

     SetLength(CapturedFiles, newCount);
     for i:=0 to newCount-1 do
     begin
         curItemPath :=SES_CapturedFiles+'Item' + IntToStr(i)+'/';
         CapturedFiles[i].fAge:= aXML.GetValue(curItemPath+'fAge', 0);
         CapturedFiles[i].fName:= RelativePathToFullPath(rPath, aXML.GetValue(curItemPath+'fName', ''));
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
        else CapturedFiles[i].iIndex :=0;
     end;


(* oldcode
     lvCaptured.BeginUpdate;
     lvCaptured.Clear;

     if FileExists(rPath+rFileName+curExt)
     then try
             imgListThumb.Clear;
             imgListThumb.LoadFromFile(rPath+rFileName+curExt);
             imgListCountChanged:= ((imgListThumb.Count-1) <> newCount); //0 is reserved for No File

          except
            imgListCountChanged:= True;
          end
     else imgListCountChanged:= True;

     SetLength(CapturedFiles, newCount);
     for i:=0 to newCount-1 do
     begin
         curItemPath :=CapturedFiles+'Item' + IntToStr(i)+'/';
         CapturedFiles[i].fAge:= aXML.GetValue(curItemPath+'fAge', 0);
         CapturedFiles[i].fName:= RelativePathToFullPath(rPath, aXML.GetValue(curItemPath+'fName', ''));
         CapturedFiles[i].iIndex:= aXML.GetValue(curItemPath+'iIndex', 0);

         cuFileName:= CapturedFiles[i].fName;
         curItem:= lvCaptured.Items.Add;
         curItem.Caption:= ExtractFileName(cuFileName);

         if FileExists(cuFileName)
         then begin
                curAge:= FileAge(cuFileName);

                //Thumbs file don't exists we must add the image
                if imgListCountChanged then CapturedFiles[i].iIndex:= -1;

                //File is Changed, update the ImageList
                if (CapturedFiles[i].iIndex <= 0) or (CapturedFiles[i].fAge <> curAge) then
                begin
                  CapturedFiles[i].fAge:= curAge;

                  UI_ThumbnailUpdate(i, cuFileName);
                end;

                curItem.ImageIndex:= CapturedFiles[i].iIndex;
              end
        else curItem.ImageIndex:= 0; //CapturedFiles[i].iIndex :=0;
     end;

     if (newSelected > -1) and (newSelected < newCount)
     then begin
            lvCaptured.Selected:= lvCaptured.Items[newSelected];
            lvCaptured.Selected.MakeVisible(False);
          end
     else if (lvCaptured.Items.Count > 0) then lvCaptured.Items[lvCaptured.Items.Count-1].MakeVisible(False);

     lvCaptured.EndUpdate;
     *)

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     if IsAutoSave
     then curExt:= Ext_AutoThumb
     else curExt:= Ext_Thumb;

(* oldcode
     if imgListThumb_Changed then
     try
       imgListThumb.SaveToFile(rPath+rFileName+curExt);
       imgListThumb_Changed:= False;

     except
     end;
*)
     //Save CapturedFiles array
     aXML.DeletePath(SES_CapturedFiles);
     lenCapturedFiles:= Length(CapturedFiles);

     if (lenCapturedFiles > 0) then
     begin
       aXML.SetValue(SES_CapturedFiles+'Count', Length(CapturedFiles));
       aXML.SetValue(SES_CapturedFiles+'iCapturedFiles', iCapturedFiles);

(* oldcode       if lvCaptured.Selected=nil
       then aXML.DeleteValue(CapturedFiles+'Selected')
       else aXML.SetValue(SES_CapturedFiles+'Selected', lvCaptured.Selected.Index);
*)
       aXML.SetValue(SES_CapturedFiles+'Selected', sCapturedFiles);

       for i:=0 to Length(CapturedFiles)-1 do
       begin
         curItemPath :=SES_CapturedFiles+'Item' + IntToStr(i)+'/';
         aXML.SetValue(curItemPath+'fAge', CapturedFiles[i].fAge);
         aXML.SetValue(curItemPath+'fName', FullPathToRelativePath(rPath, CapturedFiles[i].fName));
         aXML.SetValue(curItemPath+'iIndex', CapturedFiles[i].iIndex);
       end;
     end;

     if IsAutoSave then rSessionModified:= True;

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     LoadImage(RelativePathToFullPath(rPath, aXML.GetValue('LoadedFile', '')), False); //DON'T set to True, if you do not want infinite recursion

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     aXML.SetValue('LoadedFile', FullPathToRelativePath(rPath, rLoadedFile));

     if IsAutoSave then rSessionModified:= True;

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     SaveSourceFiles(aXML, IsAutoSave);
     SaveCapturedFiles(aXML, IsAutoSave);
     SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then rSessionModified:= True;

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     if (Length(SourceFiles) > 0) then
     begin
       aXML.SetValue(SES_SourceFiles+'iSourceFiles', iSourceFiles);
       aXML.SetValue(SES_SourceFiles+'lastCropped', lastCropped);
       aXML.SetValue(SES_SourceFiles+'lastLenTaked', lastLenTaked);
     end;

     if (Length(CapturedFiles) > 0) then aXML.SetValue(SES_SourceFiles+'iCapturedFiles', iCapturedFiles);

(* oldcode
     if lvCaptured.Selected=nil
     then aXML.DeleteValue(CapturedFiles+'Selected')
     else aXML.SetValue(CapturedFiles+'Selected', lvCaptured.Selected.Index);
*)
     aXML.SetValue(SES_CapturedFiles+'Selected', sCapturedFiles);

     SaveLoadedImage(aXML, IsAutoSave);

     if IsAutoSave then rSessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;
   newCropMode: TDigItCropMode;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
             then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
             else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     newCropMode:= TDigItCropMode(aXML.GetValue('CropMode', 0));

     if (newCropMode = diCropCustom)
     then rImageManipulation.CropAreas.Load(aXML, 'CropAreas')
     else rImageManipulation.CropAreas.Clear;

(* oldcode
     setCropMode(newCropMode);
     if (newCropMode = diCropCustom) then
     begin
       UI_FillCounter;
       imgManipulation.CropAreas.Load(aXML, 'CropAreas');
     end;
*)

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveCropAreas(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     aXML.SetValue('CropMode', Integer(CropMode));
     if (CropMode = diCropCustom)
     then rImageManipulation.CropAreas.Save(aXML, 'CropAreas')
     else aXML.DeletePath('CropAreas');

     if IsAutoSave then rSessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.LoadPageSettings(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   selButton: Integer;
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     aXML.ReadObject(SES_PageSettings+'Page/', rImageManipulation.EmptyImage);

     rPageResizeUnitType:= ruFullsize;
     aXML.GetValue(SES_PageSettings+'ResizeUnitType', rPageResizeUnitType, TypeInfo(TDigItResizeUnitType));
     if (rImageManipulation.EmptyImage.Width = 0) or
        (rImageManipulation.EmptyImage.Height = 0) then rPageResizeUnitType:= ruFullsize;

     PageResize:= resFixedWidth;
     aXML.GetValue(SES_PageSettings+'Resize', PageResize, TypeInfo(TDigItFilter_Resize));

     PageRotate:= rotNone;
     aXML.GetValue(SES_PageSettings+'Rotate', PageRotate, TypeInfo(TDigItFilter_Rotate));

     PageFlip:= flipNone;
     aXML.GetValue(SES_PageSettings+'Flip', PageFlip, TypeInfo(TDigItFilter_Flip));

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
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     aXML.WriteObject(SES_PageSettings+'Page/', rImageManipulation.EmptyImage);

     aXML.SetValue(SES_PageSettings+'ResizeUnitType', rPageResizeUnitType, TypeInfo(TDigItResizeUnitType));
     aXML.SetValue(SES_PageSettings+'Resize', PageResize, TypeInfo(TDigItFilter_Resize));
     aXML.SetValue(SES_PageSettings+'Rotate', PageRotate, TypeInfo(TDigItFilter_Rotate));
     aXML.SetValue(SES_PageSettings+'Flip', PageFlip, TypeInfo(TDigItFilter_Flip));

     if IsAutoSave then rSessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;

(* oldcode
procedure TDigIt_Session.LoadUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     //User Interface
     rollCrops.Collapsed:=aXML.GetValue('UI/rollCrops_Collapsed', False);
     rollPages.Collapsed:=aXML.GetValue('UI/rollPages_Collapsed', True);
     rollCounters.Collapsed:=aXML.GetValue('UI/rollCounters_Collapsed', True);

  finally
    if aFree then aXML.Free;
  end;
end;

procedure TDigIt_Session.SaveUserInterface(aXML: TRttiXMLConfig; IsAutoSave: Boolean);
var
   aFree: Boolean;

begin
  try
     aFree:= (aXML = nil);
     if aFree
     then if IsAutoSave
          then aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_AutoSess)
          else aXML:= TRttiXMLConfig.Create(rPath+rFileName+Ext_Sess);

     //User Interface
     aXML.SetValue('UI/rollCrops_Collapsed', rollCrops.Collapsed);
     aXML.SetValue('UI/rollPages_Collapsed', rollPages.Collapsed);
     aXML.SetValue('UI/rollCounters_Collapsed', rollCounters.Collapsed);

     if IsAutoSave then rSessionModified:= True;

  finally
    if aFree then aXML.Free;
  end;
end;
*)

end.

