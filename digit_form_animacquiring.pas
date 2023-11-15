unit DigIt_Form_AnimAcquiring;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls;

type

  { TFormAnimAcquiring }

  TFormAnimAcquiring = class(TForm)
    btAbort: TBitBtn;
    ProgressBar1: TProgressBar;
    procedure btAbortClick(Sender: TObject);
  private
    rAborted:Boolean;
  public
    class procedure Execute;

    property Aborted:Boolean read rAborted;
  end;

var
  FormAnimAcquiring: TFormAnimAcquiring=nil;

implementation

{$R *.lfm}

{ TFormAnimAcquiring }

procedure TFormAnimAcquiring.btAbortClick(Sender: TObject);
begin
  rAborted:=True;
  btAbort.Enabled:=False;
end;

class procedure TFormAnimAcquiring.Execute;
begin
  if (FormAnimAcquiring=nil)
  then FormAnimAcquiring :=TFormAnimAcquiring.Create(nil);

  with FormAnimAcquiring do
  begin
    rAborted :=False;
    btAbort.Enabled:=True;
    Visible :=True;
  end;
  Application.ProcessMessages;
end;

end.

