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
  Buttons, ComCtrls, BCPanel, BCLabel,
  FPImage, BGRABitmap, BGRABitmapTypes,
  DigIt_Types;

resourcestring
  rsConvertPDF = 'Converting Images to PDF...';
  rsOpenSavedPDF = 'Conversion to PDF completed, do i Open it?';
  rsLandscape = 'Landscape';
  rsPortrait = 'Portrait';

type
  { TDigIt_ExportFiles }

  TDigIt_ExportFiles = class(TForm)
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
    pageOptions: TPageControl;
    panelFiles: TGroupBox;
    panelInfos: TGroupBox;
    panelJPeg: TGroupBox;
    panelMain: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    panelButtons: TBCPanel;
    SavePDF: TSaveDialog;
    tabPDF: TTabSheet;
    tabIMG: TTabSheet;
    procedure btPaperOrientationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    SaveFormat: TBGRAImageFormat;
    SaveWriter: TFPCustomImageWriter;
    SavePath: String;
    panelFormatUI: TBCPanel;

    procedure AdjustFormatPanel;

  public
    class function Execute(const ATitle: String; const CapturedFiles: TCapturedFileArray;
                           asPDF: Boolean): Boolean;

  end;

var
  DigIt_ExportFiles: TDigIt_ExportFiles = nil;

implementation

{$R *.lfm}

uses LCLIntf, BGRAReadJpeg, BGRAWriteJpeg, fpPDF, BGRAPdf, BGRAFormatUI, DigIt_Form_Progress;

{ TDigIt_ExportFiles }

procedure TDigIt_ExportFiles.btPaperOrientationClick(Sender: TObject);
begin
  if btPaperOrientation.Down
  then begin btPaperOrientation.ImageIndex:= 1; btPaperOrientation.Hint:= rsLandscape; end
  else begin btPaperOrientation.ImageIndex:= 0; btPaperOrientation.Hint:= rsPortrait; end;
end;

procedure TDigIt_ExportFiles.FormCreate(Sender: TObject);
var
   i: Integer;

begin
  //Fill PaperSize, the first Item is 255 = Custom with Image Size in Pixels
  cbPaperSize.Items.Objects[0]:= TObject(PtrUInt(255));
  for i:=1 to cbPaperSize.Items.Count-1 do
    cbPaperSize.Items.Objects[i]:= TObject(PtrUInt(i-1));
end;

procedure TDigIt_ExportFiles.FormShow(Sender: TObject);
begin
  AdjustFormatPanel;
end;

procedure TDigIt_ExportFiles.AdjustFormatPanel;
begin
  if (panelFormatUI <> nil) then
  begin
    if (pageOptions.ActivePage = tabPDF)
    then begin
           panelFormatUI.Top:= 68; panelFormatUI.Left:= 107;
           panelFormatUI.Parent:= panelJPeg;
           panelFormatUI.Color:= clDefault;
           panelFormatUI.Visible:= True;
         end
    else begin
         end;
  end;
end;

class function TDigIt_ExportFiles.Execute(const ATitle: String; const CapturedFiles: TCapturedFileArray;
                                          asPDF: Boolean): Boolean;
var
   Finished: Boolean;

   function SaveAsPDF: Boolean;
   var
     PDF: TBGRAPDFDocument;
     P: TPDFPage;
     S: TPDFSection;
     paper: TPDFPaper;
     curImg: TBGRAPDFImageItem;
     curColorSpace: TPDFColorSpace;
     IDX, i,
     lenCaptured,
     curPaper: Integer;
     cStr,
     curExt,
     curFileName,
     pdfFileName: String;
     srcBitmap: TBGRABitmap;
     MemStream: TMemoryStream;
     loadImg,
     ImgInStream: Boolean;
     jpgInfo: TJPEGInfo;

   begin
     Result:= False;

     with DigIt_ExportFiles do
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
          begin
            P := PDF.Pages.AddPage;

            curExt:= Uppercase(ExtractFileExt(curFileName));
            loadImg:= True;
            ImgInStream:= False;

            if cbUseJpgAsIs.Checked and ((curExt = '.JPG') or (curExt = '.JPEG')) then
            try
              MemStream.Position:= 0;
              MemStream.Size:= 0;
              MemStream.LoadFromFile(curFileName);
              ImgInStream:= True;

              curColorSpace:= csDeviceRGB;

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
          end;

          DigIt_Progress.progressTotal.Position:= i+1;
          DigIt_Progress.capTotal.Caption:= Format(rsProcessed, [i, cStr]);
          Application.ProcessMessages;
          if DigIt_Progress.Cancelled then break;
        end;

        if not(DigIt_Progress.Cancelled) then
        begin
          PDF.SaveToFile(pdfFileName);
          Finished:= True;
          Result:= True;
        end;

     finally
        DigIt_Progress.Hide;

        srcBitmap.Free;
        MemStream.Free;
        PDF.Free;

        if Result and FileExists(pdfFileName)
        then if (MessageDlg('DigIt', rsOpenSavedPDF, mtConfirmation, mbYesNo, 0) = mrYes)
             then OpenDocument(pdfFileName);
     end;
   end;

begin
  try
     if (DigIt_ExportFiles = nil)
     then DigIt_ExportFiles:= TDigIt_ExportFiles.Create(nil);

     if (DigIt_ExportFiles <> nil) then
     with DigIt_ExportFiles do
     begin
       Result:= False;

       panelFiles.Visible:= (CapturedFiles=nil);

       SaveFormat:= ifJpeg;
       SaveWriter:= CreateBGRAImageWriter(SaveFormat, True);
       TBGRAFormatUIContainer.GetUI(SaveFormat, SaveWriter, panelFormatUI);

       if asPDF
       then begin
              pageOptions.ActivePage:= tabPDF;

              edTitle.Text:= ATitle;
              edAuthor.Text:= 'MaxM';
              edProducer.Text:= Application.Title;
              edKeyWords.Text:= '';
            end
       else begin
              pageOptions.ActivePage:= tabIMG;

            end;

       repeat
         Finished:= False;
         Result:= False;

         if (ShowModal = mrOk) then
         begin
           if (BGRAFormatUIContainer <> nil) and
              (panelFormatUI <> nil) then BGRAFormatUIContainer.SetWriterProperties(SaveWriter);

           if asPDF
           then SaveAsPDF
           else begin
                end;

         end
         else Finished:= True;
       until Finished;

       if Result then
       begin

       end;
     end;

  finally
    DigIt_ExportFiles.Free; DigIt_ExportFiles:= nil;
  end;
end;

end.

