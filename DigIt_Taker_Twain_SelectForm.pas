unit Digit_Taker_Twain_SelectForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls, Buttons,
  ComCtrls, Twain, DelphiTwain;

type

  { TTwainSelectSource }

  TTwainSelectSource = class(TForm)
    btCancel: TBitBtn;
    btOk: TBitBtn;
    lvSources: TListView;
    Panel1: TPanel;
    panelButtons: TPanel;
  private

  public
     class function Execute(Twain: TCustomDelphiTwain; const ipcList:array of TW_IDENTITY;
                            selIPC:Boolean; selectedIndex:Integer):Integer;
  end;

var
  TwainSelectSource: TTwainSelectSource=nil;

implementation

{$R *.lfm}

{ TTwainSelectSource }

class function TTwainSelectSource.Execute(Twain: TCustomDelphiTwain; const ipcList: array of TW_IDENTITY;
  selIPC: Boolean; selectedIndex: Integer): Integer;
var
   i, countTwain_Source:Integer;
   curItem:TListItem;

begin
  if (TwainSelectSource=nil)
  then TwainSelectSource :=TTwainSelectSource.Create(nil);

  with TwainSelectSource do
  begin
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

           if (ShowModal=mrOk)
           then Result :=lvSources.ItemIndex
           else Result :=-1;
         end;
  end;
end;

end.

