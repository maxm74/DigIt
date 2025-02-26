(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   SaveFiles Settings Form                                                  **
*******************************************************************************)

unit DigIt_Form_PDF;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, Buttons,
  fppdf, BCPanel, BCLabel;

type
  { TDigIt_PDF }

  TDigIt_PDF = class(TForm)
    BCPanel1: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    edTitle: TEdit;
    edAuthor: TEdit;
    edProducer: TEdit;
    Label1: TBCLabel;
    Label2: TBCLabel;
    Label3: TBCLabel;
    Label4: TBCLabel;
    edKeywords: TMemo;
    panelButtons: TBCPanel;
  private

  public
    class function Execute(APDFInfo: TPDFInfos): Boolean;

  end;

var
  DigIt_PDF: TDigIt_PDF = nil;

implementation

{$R *.lfm}

{ TDigIt_PDF }

class function TDigIt_PDF.Execute(APDFInfo: TPDFInfos): Boolean;
begin
  try
     DigIt_PDF :=TDigIt_PDF.Create(nil);

     with DigIt_PDF do
     begin
       edTitle.Text:= APDFInfo.Title;
       edAuthor.Text:= APDFInfo.Author;
       edProducer.Text:= APDFInfo.Producer;
       edKeyWords.Text:= APDFInfo.Keywords;

       Result:= (ShowModal=mrOk);

       if Result then
       begin
         APDFInfo.Title:= edTitle.Text;
         APDFInfo.Author:= edAuthor.Text;
         APDFInfo.Producer:= edProducer.Text;
         APDFInfo.Keywords:= edKeyWords.Text;
       end;
     end;

  finally
    FreeAndNil(DigIt_PDF);
  end;
end;

end.

