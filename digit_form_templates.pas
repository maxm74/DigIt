unit DigIt_Form_Templates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, ComCtrls, ExtCtrls, BCPanel, BCListBox;

type

  { TDigIt_Templates }

  TDigIt_Templates = class(TForm)
    BCPanel1: TBCPanel;
    btCancel: TBitBtn;
    btOk: TBitBtn;
    panelButtons: TBCPanel;
    TreeView1: TTreeView;
  private

  public
    class function Execute:Boolean;
  end;

var
  DigIt_Templates: TDigIt_Templates=nil;

implementation

{$R *.lfm}

{ TDigIt_Templates }

class function TDigIt_Templates.Execute: Boolean;
begin
  if (DigIt_Templates=nil)
  then DigIt_Templates :=TDigIt_Templates.Create(nil);

  with DigIt_Templates do
  begin
    Result :=(ShowModal=mrOk);
  end;
end;

end.

