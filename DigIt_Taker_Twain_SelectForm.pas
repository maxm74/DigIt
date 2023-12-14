unit Digit_Taker_Twain_SelectForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Buttons,
  ComCtrls, Twain, DelphiTwain, Digit_Taker_Twain_Types;

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
    countTwain_Source:Integer;
    rRefreshClick:TRefreshNotify;
    rParams:TDigIt_Taker_TwainParams;

  public
     class function Execute(ARefreshClick:TRefreshNotify; ATwain: TCustomDelphiTwain;
                            const ipcList:array of TW_IDENTITY; AParams:TDigIt_Taker_TwainParams):Integer;
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
   i, selectedIndex:Integer;
   curItem:TListItem;
   curSource:TTwainSource;

begin
  selectedIndex:=-1;
  countTwain_Source:=Twain.SourceCount;

  lvSources.Clear;
  for i:=0 to countTwain_Source-1 do
  begin
    curSource :=Twain.Source[i];
    curItem :=lvSources.Items.Add;
    curItem.Caption:=curSource.ProductName;
    //curItem.SubItems.Add(curSource.ProductFamily);
    curItem.SubItems.Add(curSource.Manufacturer);

    //if is Current Selected Scanner set selectedIndex
    if not(rParams.IPC_Scanner) and
       (curSource.Manufacturer=rParams.Manufacturer) and
       (curSource.ProductFamily=rParams.ProductFamily) and
       (curSource.ProductName=rParams.ProductName) then selectedIndex :=curItem.Index;
  end;

  for i:=Low(ipcList) to High(ipcList) do
  begin
    curItem :=lvSources.Items.Add;
    curItem.Caption:=ipcList[i].ProductName;
    //curItem.SubItems.Add(ipcList[i].ProductFamily);
    curItem.SubItems.Add(ipcList[i].Manufacturer);
    curItem.SubItems.Add('(32bit)');

    //if is Current Selected Scanner set selectedIndex
    if (rParams.IPC_Scanner) and
       (ipcList[i].Manufacturer=rParams.Manufacturer) and
       (ipcList[i].ProductFamily=rParams.ProductFamily) and
       (ipcList[i].ProductName=rParams.ProductName) then selectedIndex :=curItem.Index;
  end;

  //Select Current Scanner
  if (selectedIndex>-1)
  then lvSources.ItemIndex :=selectedIndex
  else lvSources.ItemIndex :=0;
end;

class function TTwainSelectSource.Execute(ARefreshClick: TRefreshNotify; ATwain: TCustomDelphiTwain;
  const ipcList: array of TW_IDENTITY; AParams: TDigIt_Taker_TwainParams): Integer;
begin
  Result :=-1;
  if (TwainSelectSource=nil)
  then TwainSelectSource :=TTwainSelectSource.Create(nil);

  with TwainSelectSource do
  begin
    Twain :=ATwain;
    rParams:=AParams;
    FillList(ipcList);

    if (lvSources.Items.Count=0)
    then MessageDlg('DigIt', 'No Twain Scanner present...', mtError, [mbOk], 0)
    else begin
           rRefreshClick:=ARefreshClick;
           btRefresh.Visible :=Assigned(rRefreshClick);

           if (ShowModal=mrOk) then Result :=lvSources.ItemIndex
         end;
  end;
end;

end.

