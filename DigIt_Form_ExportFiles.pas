(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Export Files Settings Form                                               **
*******************************************************************************)

unit DigIt_Form_ExportFiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, BCPanel, BCLabel, BGRADialogs,
  FPImage, BGRABitmap, BGRABitmapTypes,
  DigIt_Types;

resourcestring
  rsConvertPDF = 'Converting Images to PDF...';
  rsConvertIMG = 'Converting Images to Images...';
  rsOpenSavedPDF = 'Conversion to PDF completed, do i Open it?';
  rsOpenDirectory = 'Conversion to Images completed, do i Open it?';
  rsLandscape = 'Landscape';
  rsPortrait = 'Portrait';

type
  { TDigIt_ExportFiles }

  TDigIt_ExportFiles = class(TForm)
    BCLabel9: TBCLabel;
    btLast: TSpeedButton;
    btDown: TSpeedButton;
    btFirst: TSpeedButton;
    btUp: TSpeedButton;
    cbResursive: TCheckBox;
    cbSaveFormat: TComboBox;
    OpenDlg: TBGRAOpenPictureDialog;
    btPaperOrientation: TSpeedButton;
    cbPaperSize: TComboBox;
    cbUseJpgAsIs: TCheckBox;
    edAuthor: TEdit;
    edKeywords: TMemo;
    edProducer: TEdit;
    edTitle: TEdit;
    imgList: TImageList;
    Label1: TBCLabel;
    Label2: TBCLabel;
    Label3: TBCLabel;
    Label4: TBCLabel;
    Label5: TBCLabel;
    lvFiles: TListView;
    pageOptions: TPageControl;
    panelFiles: TGroupBox;
    panelInfos: TGroupBox;
    panelJPeg: TGroupBox;
    panelMain: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    panelButtons: TBCPanel;
    SavePDF: TSaveDialog;
    btAddFiles: TSpeedButton;
    btAddFolder: TSpeedButton;
    btDel: TSpeedButton;
    btDelAll: TSpeedButton;
    OpenFolder: TSelectDirectoryDialog;
    SaveFolder: TSelectDirectoryDialog;
    tabPDF: TTabSheet;
    tabIMG: TTabSheet;
    procedure btAddFilesClick(Sender: TObject);
    procedure btAddFolderClick(Sender: TObject);
    procedure btDelAllClick(Sender: TObject);
    procedure btDelClick(Sender: TObject);
    procedure btUpDownClick(Sender: TObject);
    procedure btPaperOrientationClick(Sender: TObject);
    procedure cbResursiveChange(Sender: TObject);
    procedure cbSaveFormatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

  private
    SaveFormat: TBGRAImageFormat;
    SaveWriter: TFPCustomImageWriter;
    SavePath: String;
    panelFormatUI: TBCPanel;
    createdWriter: Boolean;

    CapturedFiles: TCapturedFileArray;

    procedure UI_AdjustFormatPanel;
    procedure UI_EnableButtons;

    procedure AddFolder(BaseDir: String; Recursive: Boolean);
    procedure GenerateCapturedFiles;

    function SaveAsPDF(out pdfFileName: String): Boolean;
    function SaveAsIMG(out destPath: String): Boolean;

  public
    class function Execute(const ATitle: String;
                           const ACapturedFiles: TCapturedFileArray;
                           asPDF: Boolean): Boolean;

  end;

var
  DigIt_ExportFiles: TDigIt_ExportFiles = nil;

implementation

{$R *.lfm}

uses LCLIntf, LazFileUtils,
     MM_StrUtils, MM_FilesUtils,
     BGRAReadJpeg, BGRAWriteJpeg, fpPDF, BGRAPdf,
     BGRAFormatUI, DigIt_Form_Progress;

const
  IMG_FILE   = 2;
  IMG_FOLDER = 3;

{ TDigIt_ExportFiles }

procedure TDigIt_ExportFiles.btPaperOrientationClick(Sender: TObject);
begin
  if btPaperOrientation.Down
  then begin btPaperOrientation.ImageIndex:= 1; btPaperOrientation.Hint:= rsLandscape; end
  else begin btPaperOrientation.ImageIndex:= 0; btPaperOrientation.Hint:= rsPortrait; end;
end;

procedure TDigIt_ExportFiles.cbResursiveChange(Sender: TObject);
begin
  if (lvFiles.Selected <> nil)
  then lvFiles.Selected.Data:= Pointer(Integer(cbResursive.Checked));
end;

procedure TDigIt_ExportFiles.cbSaveFormatChange(Sender: TObject);
begin
  SaveFormat:= TBGRAImageFormat(PTRUInt(cbSaveFormat.Items.Objects[cbSaveFormat.ItemIndex]));
  SaveWriter.Free; SaveWriter:= nil;

  //Create the new Writer
  SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);

  if (panelFormatUI <> nil) then panelFormatUI.Visible:= False;

  TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);
  UI_AdjustFormatPanel;
end;

procedure TDigIt_ExportFiles.btAddFilesClick(Sender: TObject);
var
   i: Integer;
   newItem: TListItem;

begin
  if OpenDlg.Execute then
  try
     lvFiles.BeginUpdate;

     for i:=0 to OpenDlg.Files.Count-1 do
     begin
       newItem:= lvFiles.Items.Add;
       newItem.Caption:= ExtractFileName(OpenDlg.Files[i]);
       newItem.ImageIndex:= IMG_FILE;
       newItem.SubItems.Add(OpenDlg.Files[i]);
       newItem.Data:= nil;
     end;

     lvFiles.EndUpdate;
     if (newItem <> nil) then lvFiles.Selected:= newItem;

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_ExportFiles.btAddFolderClick(Sender: TObject);
var
   i: Integer;
   newItem: TListItem;

begin
  if OpenFolder.Execute then
  try
     lvFiles.BeginUpdate;

     for i:=0 to OpenFolder.Files.Count-1 do
     begin
       newItem:= lvFiles.Items.Add;
       newItem.Caption:= ExtractFileName(OpenFolder.Files[i]);
       newItem.ImageIndex:= IMG_FOLDER;
       newItem.SubItems.Add(OpenFolder.Files[i]);
       newItem.Data:= Pointer(Integer(cbResursive.Checked));
     end;

     lvFiles.EndUpdate;
     if (newItem <> nil) then lvFiles.Selected:= newItem;

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_ExportFiles.btDelAllClick(Sender: TObject);
begin
  try
    lvFiles.Clear;

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_ExportFiles.btDelClick(Sender: TObject);
begin
  if (lvFiles.Selected <> nil) then
  try
     lvFiles.Items.Delete(lvFiles.Selected.Index);

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_ExportFiles.btUpDownClick(Sender: TObject);
var
   curItem: TListItem;

begin
  curItem:= lvFiles.Selected;

  if (curItem <> nil) then
  try
    lvFiles.BeginUpdate;

    Case TSpeedButton(Sender).Tag of
      0: lvFiles.Items.Move(curItem.Index, 0);
      1: lvFiles.Items.Move(curItem.Index, lvFiles.Items.Count-1);
      2: lvFiles.Items.Move(curItem.Index, curItem.Index-1);
      3: lvFiles.Items.Move(curItem.Index, curItem.Index+1);
    end;

    lvFiles.EndUpdate;

  finally
    UI_EnableButtons;
  end;
end;

procedure TDigIt_ExportFiles.FormCreate(Sender: TObject);
var
   i: Integer;

begin
  SaveWriter:= nil;

  //Fill PaperSize, the first Item is 255 = Custom with Image Size in Pixels
  cbPaperSize.Items.Objects[0]:= TObject(PtrUInt(255));
  for i:=1 to cbPaperSize.Items.Count-1 do
    cbPaperSize.Items.Objects[i]:= TObject(PtrUInt(i-1));
end;

procedure TDigIt_ExportFiles.FormDestroy(Sender: TObject);
begin
  if (SaveWriter <> nil) then SaveWriter.Free;
end;

procedure TDigIt_ExportFiles.FormShow(Sender: TObject);
begin
  UI_AdjustFormatPanel;
end;

procedure TDigIt_ExportFiles.lvFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and (Item <> nil) then
  begin
    cbResursive.Enabled:= (Item.ImageIndex = IMG_FOLDER);
    cbResursive.Checked:= (Item.Data <> nil);
  end;
  UI_EnableButtons;
end;

procedure TDigIt_ExportFiles.UI_AdjustFormatPanel;
begin
  if (panelFormatUI <> nil) then
  begin
    if (pageOptions.ActivePage = tabPDF)
    then begin
           panelFormatUI.Top:= 68; panelFormatUI.Left:= 107;
           panelFormatUI.Parent:= panelJPeg;
         end
    else begin
           panelFormatUI.Top:= 50; panelFormatUI.Left:= 100;
           panelFormatUI.Parent:= tabIMG;
         end;

    panelFormatUI.Color:= clDefault;
    panelFormatUI.Visible:= True;
  end;
end;

procedure TDigIt_ExportFiles.UI_EnableButtons;
var
   lvCount: Integer;

begin
  lvCount:= lvFiles.Items.Count;
  btDel.Enabled:= (lvCount > 0);
  btDelAll.Enabled:= (lvCount > 0);
  btUp.Enabled:= (lvCount > 1) and (lvFiles.Selected <> nil) and (lvFiles.Selected.Index > 0);
  btDown.Enabled:= (lvCount > 1) and (lvFiles.Selected <> nil) and (lvFiles.Selected.Index < lvCount-1);
  btFirst.Enabled:= btUp.Enabled;
  btLast.Enabled:= btDown.Enabled;
end;

procedure TDigIt_ExportFiles.AddFolder(BaseDir: String; Recursive: Boolean);
var
   i,
   len, lenC: Integer;
   xFiles: TStringList;

begin
  try
     xFiles:= GetFilesInDir(BaseDir, Recursive,
                            faAnyFile, BGRARegisteredImageReaderExtension,
                            flsSortName, False, True);

     len:= xFiles.Count;
     if (len > 0) then
     begin
       lenC:= Length(CapturedFiles);
       SetLength(CapturedFiles, lenC+len);
       for i:=0 to len-1 do
       begin
         CapturedFiles[lenC+i].fName:= xFiles[i];
       end;
     end;

  finally
    xFiles.Free;
  end;
end;

procedure TDigIt_ExportFiles.GenerateCapturedFiles;
var
   i: Integer;
   curItem: TListItem;

begin
  CapturedFiles:= nil;

  for i:=0 to lvFiles.Items.Count-1 do
  begin
    curItem:= lvFiles.Items[i];

    if (curItem.ImageIndex = IMG_FOLDER)
    then AddFolder(curItem.SubItems[0], (curItem.Data <> nil))
    else begin
           SetLength(CapturedFiles, Length(CapturedFiles)+1);
           CapturedFiles[Length(CapturedFiles)-1].fName:= curItem.SubItems[0];
         end;
  end;
end;

function TDigIt_ExportFiles.SaveAsPDF(out pdfFileName: String): Boolean;
var
  PDF: TBGRAPDFDocument;
  P: TPDFPage;
  S: TPDFSection;
  paper: TPDFPaper;
  curImg: TBGRAPDFImageItem;
  IDX, i,
  lenCaptured,
  curPaper: Integer;
  cStr,
  curExt,
  curFileName: String;
  srcBitmap: TBGRABitmap;
  MemStream: TMemoryStream;
  loadImg,
  ImgInStream: Boolean;
  jpgInfo: TJPEGInfo;

begin
  Result:= False;
  pdfFileName:= '';

  if SavePDF.Execute then
  try
     pdfFileName:= SavePDF.FileName;

     srcBitmap:= TBGRABitmap.Create;
     MemStream:= TMemoryStream.Create;

     PDF:= TBGRAPDFDocument.Create(Nil);
     PDF.Infos.Title := edTitle.Text;
     PDF.Infos.Author := edAuthor.Text;
     PDF.Infos.Producer := edProducer.Text;
     PDF.Infos.Keywords:= edKeyWords.Text;
     PDF.Infos.ApplicationName := Application.Title+' ver '+DigIt_Version;
     PDF.Infos.CreationDate := Now;
     PDF.Options := [poCompressImages, poUseRawJPEG];

     lenCaptured:= Length(CapturedFiles)-1;
     cStr:= IntToStr(lenCaptured+1);
     DigIt_Progress.ProgressShow(rsConvertPDF, 0, lenCaptured);

     PDF.StartDocument;
     S := PDF.Sections.AddSection;

     curPaper:= Integer(PTRUInt(cbPaperSize.Items.Objects[cbPaperSize.ItemIndex]));

     for i:=0 to lenCaptured do
     begin
       DigIt_Progress.progressTotal.Position:= i;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [i, cStr]);
       Application.ProcessMessages;
       if DigIt_Progress.Cancelled then break;

       curFileName:= CapturedFiles[i].fName;

       if FileExists(curFileName) then
       try
         curExt:= Uppercase(ExtractFileExt(curFileName));
         loadImg:= True;
         ImgInStream:= False;

         if cbUseJpgAsIs.Checked and ((curExt = '.JPG') or (curExt = '.JPEG')) then
         try
           MemStream.Position:= 0;
           MemStream.Size:= 0;
           MemStream.LoadFromFile(curFileName);
           ImgInStream:= True;

           //Get Jpeg Info, if False try to read the Image with srcBitmap
           loadImg:= not(TBGRAReaderJpeg.GetJpegInfo(MemStream, jpgInfo));

         except
           loadImg:= True;
         end;

         if loadImg then
         begin
           //Load Image and Convert To JPeg in Memory (Options is already in SaveWriter)
           if ImgInStream
           then srcBitmap.LoadFromStream(MemStream)  //I deliberately do not use the file extension, because if we are here
                                                     //the file has a fake extension and we let DetectFileFormat determine the format
           else srcBitmap.LoadFromFile(curFileName);

           //Reuse the Same Stream to Write converted JPeg
           MemStream.Position:= 0;
           MemStream.Size:= 0;
           srcBitmap.SaveToStream(MemStream, SaveWriter);
           MemStream.Position:= 0;

           jpgInfo.Width:= srcBitmap.Width;
           jpgInfo.Height:= srcBitmap.Height;
           jpgInfo.GrayScale:= TBGRAWriterJPEG(SaveWriter).GrayScale;
         end;

         P := PDF.Pages.AddPage;
         IDX:= PDF.Images.AddJPEGStream(MemStream, jpgInfo.Width, jpgInfo.Height);
         if (IDX >= 0) then
         begin
           curImg:= TBGRAPDFImageItem(PDF.Images[IDX]);

           if jpgInfo.GrayScale
           then curImg.ColorSpace:= csDeviceGray
           else curImg.ColorSpace:= csDeviceRGB;

           (*case curPaper of   Add In Next Version
             Integer(ptCustom): begin
               //P.PaperType:= ptCustom;
               P.UnitOfMeasure:= From UI;
               //Set Paper to Full image size
               paper.W:= From UI;
               paper.H:= From UI;
               P.Paper:=paper;

             end;
             255:  begin*)
               //P.PaperType:= ptCustom;
               P.UnitOfMeasure:= uomPixels;
               //Set Paper to Full image size
               paper.W:= jpgInfo.Width;
               paper.H:= jpgInfo.Height;
               P.Paper:=paper;
             (*end;
             else P.PaperType:= TPDFPaperType(curPaper);
           end;*)

           P.AddObject(TPDFImage.Create(PDF, 0, 0, jpgInfo.Width, jpgInfo.Height, IDX));
           S.AddPage(P);
         end;

       except
         //Some Error Loading Ignore It
       end;

       DigIt_Progress.progressTotal.Position:= i+1;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [i, cStr]);
       Application.ProcessMessages;
       if DigIt_Progress.Cancelled then break;
     end;

     if not(DigIt_Progress.Cancelled) then
     begin
       PDF.SaveToFile(pdfFileName);
       Result:= True;
     end;

  finally
     DigIt_Progress.Hide;

     srcBitmap.Free;
     MemStream.Free;
     PDF.Free;
  end;
end;

function TDigIt_ExportFiles.SaveAsIMG(out destPath: String): Boolean;
var
  i,
  lenCaptured: Integer;
  cStr,
  curFileName,
  destDir,
  destExt,
  destFilename: String;
  srcBitmap: TBGRABitmap;

begin
  Result:= False;
  destPath:= '';

  if SaveFolder.Execute then
  try
     destPath:= SaveFolder.FileName;
     destExt:= ExtensionSeparator+SuggestImageExtension(SaveFormat);

     srcBitmap:= TBGRABitmap.Create;

     lenCaptured:= Length(CapturedFiles)-1;
     cStr:= IntToStr(lenCaptured+1);
     DigIt_Progress.ProgressShow(rsConvertIMG, 0, lenCaptured);

(*  Recursive cicle like this...
for i:=0 to lvFiles.Items.Count-1 do
begin
  curItem:= lvFiles.Items[i];

  if (curItem.ImageIndex = IMG_FOLDER)
  then AddFolder(curItem.SubItems[0], (curItem.Data <> nil))
  else begin
         SetLength(CapturedFiles, Length(CapturedFiles)+1);
         CapturedFiles[Length(CapturedFiles)-1].fName:= curItem.SubItems[0];
       end;
end;

using  FullPathToRelativePath() and RelativePathToFullPath()

*)
     for i:=0 to lenCaptured do
     begin
       DigIt_Progress.progressTotal.Position:= i;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [i, cStr]);
       Application.ProcessMessages;
       if DigIt_Progress.Cancelled then break;

       curFileName:= CapturedFiles[i].fName;

       if FileExists(curFileName) then
       try
         srcBitmap.LoadFromFile(curFileName);
         destFilename:= GetFileFreeName(destPath, ExtractFileNameOnly(curFileName), destExt, True, 3);

         srcBitmap.SaveToFile(destPath+DirectorySeparator+destFilename, SaveWriter);

       except
         //Some Error Loading Ignore It
       end;

       DigIt_Progress.progressTotal.Position:= i+1;
       DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [i, cStr]);
       Application.ProcessMessages;
       if DigIt_Progress.Cancelled then break;
     end;

     Result:= not(DigIt_Progress.Cancelled);

  finally
     DigIt_Progress.Hide;

     srcBitmap.Free;
  end;
end;

class function TDigIt_ExportFiles.Execute(const ATitle: String;
                                          const ACapturedFiles: TCapturedFileArray;
                                          asPDF: Boolean): Boolean;
var
   Finished: Boolean;
   Dest: String;

begin
  try
     if (DigIt_ExportFiles = nil)
     then DigIt_ExportFiles:= TDigIt_ExportFiles.Create(nil);

     if (DigIt_ExportFiles <> nil) then
     with DigIt_ExportFiles do
     try
       Result:= False;

       panelFiles.Visible:= (ACapturedFiles = nil);

       SaveFormat:= ifJpeg;
       SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
       TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);

       if asPDF
       then begin
              Caption:= Caption+'PDF';
              pageOptions.ActivePage:= tabPDF;

              edTitle.Text:= ATitle;
              edAuthor.Text:= 'MaxM';
              edProducer.Text:= Application.Title;
              edKeyWords.Text:= '';
            end
       else begin
              Caption:= Caption+'Image';
              pageOptions.ActivePage:= tabIMG;

              if not(TBGRAFormatUIContainer.BuildSaveFormats(cbSaveFormat, SaveFormat) > 0)
              then raise Exception.Create('No Writers Registered...');
            end;

       repeat
         Finished:= False;
         Result:= False;

         if (ShowModal = mrOk) then
         begin
           if (BGRAFormatUIContainer <> nil) and
              (panelFormatUI <> nil) then BGRAFormatUIContainer.SetWriterProperties(SaveWriter);

           if (ACapturedFiles = nil)
           then GenerateCapturedFiles
           else CapturedFiles:= ACapturedFiles;

           if asPDF
           then begin
                  Result:= SaveAsPDF(Dest);
                  Finished:= True
                end
           else begin
                  Result:= SaveAsImg(Dest);
                  Finished:= True
                end;

         end
         else Finished:= True;
       until Finished;

       if Result then
       begin
         if asPDF
         then begin
                if FileExists(Dest) and
                   (MessageDlg('DigIt', rsOpenSavedPDF, mtConfirmation, mbYesNo, 0) = mrYes)
                then OpenDocument(Dest);
              end
         else begin
                if DirectoryExists(Dest) and
                     (MessageDlg('DigIt', rsOpenDirectory, mtConfirmation, mbYesNo, 0) = mrYes)
                then OpenDocument(Dest);
              end;

       end;

     finally
       if (ACapturedFiles = nil) then CapturedFiles:= nil;
     end;

  finally
    DigIt_ExportFiles.Free; DigIt_ExportFiles:= nil;
  end;
end;

end.

