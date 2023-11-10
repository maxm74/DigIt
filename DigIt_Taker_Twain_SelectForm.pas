unit Digit_Taker_Twain_SelectForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Buttons,
  ComCtrls, Twain, DelphiTwain;

type

  { TTwainSelectSource }

  TTwainSelectSource = class;
  TRefreshNotify = procedure(ASender:TTwainSelectSource) of object;

  TTwainSelectSource = class(TForm)
    btCancel: TBitBtn;
    btRefresh: TBitBtn;
    btOk: TBitBtn;
    lvSources: TListView;
    Panel1: TPanel;
    panelButtons: TPanel;
    procedure btRefreshClick(Sender: TObject);
  private
    Twain: TCustomDelphiTwain;
    rRefreshClick:TRefreshNotify;

  public
     class function Execute(ARefreshClick:TRefreshNotify; ATwain: TCustomDelphiTwain;
                            const ipcList:array of TW_IDENTITY; selIPC:Boolean; selectedIndex:Integer):Integer;
    procedure FillList(const ipcList: array of TW_IDENTITY);
  end;

var
  TwainSelectSource: TTwainSelectSource=nil;

implementation

{$R *.lfm}

{ TTwainSelectSource }

procedure TTwainSelectSource.btRefreshClick(Sender: TObject);
begin
  if Assigned(rRefreshClick) then rRefreshClick(Self);
end;

procedure TTwainSelectSource.FillList(const ipcList: array of TW_IDENTITY);
var
   i, countTwain_Source:Integer;
   curItem:TListItem;

begin
  lvSources.Clear;
  countTwain_Source:=Twain.SourceCount;
  for i:=0 to countTwain_Source-1 do
  begin
    curItem :=lvSources.Items.Add;
    curItem.Caption:=Twain.Source[i].ProductName;
    //curItem.SubItems.Add(Twain.Source[i].ProductFamily);
    curItem.SubItems.Add(Twain.Source[i].Manufacturer);
  end;

  for i:=Low(ipcList) to High(ipcList) do
  begin
    curItem :=lvSources.Items.Add;
    curItem.Caption:=ipcList[i].ProductName;
    //curItem.SubItems.Add(ipcList[i].ProductFamily);
    curItem.SubItems.Add(ipcList[i].Manufacturer);
    curItem.SubItems.Add('(32bit)');
  end;
end;

class function TTwainSelectSource.Execute(ARefreshClick:TRefreshNotify; ATwain: TCustomDelphiTwain;
  const ipcList: array of TW_IDENTITY; selIPC: Boolean; selectedIndex: Integer): Integer;
var
   i, countTwain_Source:Integer;
   curItem:TListItem;

begin
  if (TwainSelectSource=nil)
  then TwainSelectSource :=TTwainSelectSource.Create(nil);

  with TwainSelectSource do
  begin
    Twain :=ATwain;
    FillList(ipcList);

    if (lvSources.Items.Count=0)
    then begin
           MessageDlg('DigIt', 'No Twain Scanner present...', mtError, [mbOk], 0);
           Result :=-1;
         end
    else begin
           if (selectedIndex>-1)
           then begin
                  if selIPC
                  then lvSources.ItemIndex :=selectedIndex+countTwain_Source
                  else lvSources.ItemIndex :=selectedIndex;
                end
           else lvSources.ItemIndex :=0;

           rRefreshClick:=ARefreshClick;
           btRefresh.Visible :=Assigned(rRefreshClick);

           if (ShowModal=mrOk)
           then Result :=lvSources.ItemIndex
           else Result :=-1;
         end;
  end;
end;

end.

