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
  Buttons, ComCtrls, fppdf, BCPanel, BCLabel,
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
    Panel1: TPanel;
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

uses LCLIntf, BGRAWriteJpeg, BGRAFormatUI, DigIt_Form_Progress;

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
           panelFormatUI.Top:= 34; panelFormatUI.Left:= 107;
           panelFormatUI.Parent:= panelJPeg;
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
     PDF: TPDFDocument;
     P: TPDFPage;
     S: TPDFSection;
     paper: TPDFPaper;
     IDX,
     i, W, H,
     curPaper: Integer;
     cStr,
     curFileName,
     pdfFileName: String;
     srcBitmap: TBGRABitmap;
     MemStream: TMemoryStream;
     PDF_File: TFileStream;

   begin
     Result:= False;

     with DigIt_ExportFiles do
     if SavePDF.Execute then
     try
        srcBitmap:= TBGRABitmap.Create;
        MemStream:= TMemoryStream.Create;

        pdfFileName:= SavePDF.FileName;

        PDF:= TPDFDocument.Create(Nil);

        PDF.Infos.Title := edTitle.Text;
        PDF.Infos.Author := edAuthor.Text;
        PDF.Infos.Producer := edProducer.Text;
        PDF.Infos.Keywords:= edKeyWords.Text;
        PDF.Infos.ApplicationName := Application.Title+' ver '+DigIt_Version;
        PDF.Infos.CreationDate := Now;

        PDF.Options := [poCompressImages, poUseRawJPEG];

        cStr:= IntToStr(Length(CapturedFiles)-1);

        DigIt_Progress.ProgressShow(rsConvertPDF, 0, Length(CapturedFiles)-1);

        PDF.StartDocument;
        S := PDF.Sections.AddSection;

        curPaper:= Integer(PTRUInt(cbPaperSize.Items.Objects[cbPaperSize.ItemIndex]));

        for i := 0 to Length(CapturedFiles)-1 do
        begin
          DigIt_Progress.progressTotal.Position:= i;
          DigIt_Progress.capTotal.Caption:= Format(rsProcessing, [i, cStr]);
          Application.ProcessMessages;
          if DigIt_Progress.Cancelled then break;

          curFileName:= CapturedFiles[i].fName;

          if FileExists(curFileName) then
          begin
            P := PDF.Pages.AddPage;

            //Directly from File but we cannot use File Options
            //IDX:= PDF.Images.AddFromFile(curFileName, False);

            //Load Image and Convert To JPeg in Memory (Options is already in SaveWriter)
            srcBitmap.LoadFromFile(curFileName);
            MemStream.Position:= 0;
            MemStream.Size:= 0;
            srcBitmap.SaveToStream(MemStream, SaveWriter);
            IDX:= PDF.Images.AddJPEGStream(MemStream, srcBitmap.Width, srcBitmap.Height);

            if (IDX >= 0) then
            begin
              W := PDF.Images[IDX].Width;
              H := PDF.Images[IDX].Height;

              (*case curPaper of
                Integer(ptCustom): begin

                end;
                255:  begin*)
                  P.PaperType := ptCustom;
                  P.UnitOfMeasure := uomPixels;
                  //Set Paper to Full image size
                  paper.W:=W;
                  paper.H:=H;
                (*end;
                else begin
                       P.PaperType:= TPDFPaperType(curPaper);

                       case Img.ResolutionUnit of
                         ruPixelsPerCentimeter: begin
                           P.UnitOfMeasure := uomCentimeters;
                           paper.W:= Round(Img.ResolutionWidth);
                           paper.H:= Round(Img.ResolutionHeight);
                         end;
                         ruPixelsPerInch: begin
                           P.UnitOfMeasure := uomInches;
                           paper.W:= Round(Img.ResolutionWidth);
                           paper.H:= Round(Img.ResolutionHeight);
                         end;
                         ruNone: begin
                            P.UnitOfMeasure := uomPixels;
                            paper.W:= PDFPaperSizes[P.PaperType, 1];
                            paper.H:= PDFPaperSizes[P.PaperType, 0];
                         end;
                       end;
                end;
              end;*)

              P.Paper:=paper;
              P.AddObject(TPDFImage.Create(PDF, 0, 0, W, H, IDX));

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
          //PDF_File:= TFileStream.Create(pdfFileName, fmCreate);
          //PDF.SaveToStream(PDF_File);
          PDF.SaveToFile(pdfFileName);
          Finished:= True;
          Result:= True;
        end;

     finally
        DigIt_Progress.Hide;

        srcBitmap.Free;
        MemStream.Free;

        //if (PDF_File <> nil) then PDF_File.Free;

        if Result and FileExists(pdfFileName)
        then if (MessageDlg('DigIt', rsOpenSavedPDF, mtConfirmation, mbYesNo, 0) = mrYes)
             then OpenDocument(pdfFileName);

        PDF.Free;
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

              //PDF does not support GrayScale JPeg?????
              if (BGRAFormatUIContainer <> nil) then BGRAFormatUIContainer.ifJpeg_GrayScale.Enabled:= False;

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

