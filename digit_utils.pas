(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Utils Functions                                                          **
*******************************************************************************)

unit DigIt_Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, DOM, XMLConf, Laz2_DOM, Laz_XMLStreaming, Laz2_XMLCfg,
  FPImage, Menus, DigIt_types, BGRAPapers;

resourcestring
  rsDestination_Default = 'Save as Files';
  rsVertical = '   Vertical';
  rsHorizontal = '    Horizontal';

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string; Append: Boolean; var DestroyDriver: boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string; var DestroyDriver: boolean): TReader;

procedure WritePersistentToXMLConfig(XMLConfig: TXMLConfig; const Path, AName: string; AData: TPersistent);
procedure ReadPersistentFromXMLConfig(XMLConfig: TXMLConfig; const Path, AName: string;
                                      AData: TPersistent; OnFindComponentClass: TFindComponentClassEvent=nil);

procedure BuildPaperSizesMenu(ResUnit: TResolutionUnit;
                              AOwner: TComponent; menuPaperSizes: TMenu; menuOnClick: TNotifyEvent;
                              VImageIndex, HImageIndex: Integer);

procedure PaperSizesMenuTag_decode(ATag:Integer; var ResUnit: TResolutionUnit; var Paper: TPaperSize);
function PaperSizesMenuTag_encode(ResUnit: TResolutionUnit; vert: Boolean; pIndex, iIndex: Byte): Integer;

procedure BuildSourcesMenu(AOwner: TComponent; menuSources: TMenu; menuOnClick: TNotifyEvent);
procedure BuildDestinationsMenu(AOwner: TComponent; menuDestinations: TMenu; menuOnClick: TNotifyEvent);
function FindMenuItemByTag(AMenu: TMenu; ATag: PtrInt): TMenuItem;

procedure GetThumnailSize(thumbWidth, thumbHeight, imgWidth, imgHeight:Integer;
                          var newWidth, newHeight:Integer);

function FindFileListItem(AList:TListItems; AFileName:String):TFileListItem;

implementation

uses Digit_Bridge_Intf, Digit_Bridge_Impl;

type
    //Workaround class so we can write a TPersistent using TWriter.WriteRootComponent
    TPersistentComponent = class(TComponent)
    protected
       rData:TPersistent;
    published
       property Data:TPersistent read rData write rData;
    end;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string; Append: Boolean; var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=TXMLObjectWriter.Create(ADoc,Path,Append);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
end;

function CreateXMLReader(ADoc: TDOMDocument; const Path: string; var DestroyDriver: boolean): TReader;
var
  p: Pointer;
  Driver: TAbstractObjectReader;
  DummyStream: TMemoryStream;
begin
  DummyStream:=TMemoryStream.Create;
  try
    Result:=TReader.Create(DummyStream,256);
    DestroyDriver:=false;
    // hack to set a write protected variable.
    // DestroyDriver:=true; TReader will free it
    Driver:=TXMLObjectReader.Create(ADoc,Path);
    p:=@Result.Driver;
    Result.Driver.Free;
    TAbstractObjectReader(p^):=Driver;
  finally
    DummyStream.Free;
  end;
end;

procedure WritePersistentToXMLConfig(XMLConfig: TXMLConfig; const Path, AName: string; AData: TPersistent);
var
  Writer: TWriter;
  DestroyDriver: boolean;
  tt:TPersistentComponent;

begin
  Writer:=nil;
  DestroyDriver:=false;
  try
    tt:=TPersistentComponent.Create(nil);
    tt.Name:=AName;
    tt.Data :=AData;

    Writer:=CreateXMLWriter(XMLConfig.Document,Path,false,DestroyDriver);
    XMLConfig.Modified:=true;
    Writer.WriteRootComponent(tt);
    XMLConfig.Flush;
  finally
    if DestroyDriver and (Writer<>nil)
    then Writer.Driver.Free;
    Writer.Free;
    tt.Free;
  end;
end;

procedure ReadPersistentFromXMLConfig(XMLConfig: TXMLConfig; const Path, AName: string;
                                      AData: TPersistent; OnFindComponentClass: TFindComponentClassEvent=nil);
var
  DestroyDriver: Boolean;
  Reader: TReader;
  IsInherited: Boolean;
  AClassName: String;
  AClass: TComponentClass;
  tt:TPersistentComponent;

begin
  if (AData=nil)
  then raise Exception.Create('ReadPersistentFromXMLConfig AData=nil');

  Reader:=nil;
  try
    Reader:=CreateXMLReader(XMLConfig.Document,Path,DestroyDriver);
    Reader.OnFindComponentClass:=OnFindComponentClass;

    // get root class
    AClassName:=(Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
    if (AClassName<>'TPersistentComponent')
    then raise Exception.Create('Invalid ClassName '+AClassName+' in XML (maybe TPersistentComponent)');

    tt :=TPersistentComponent.Create(nil);
    tt.Name:=AName;
    tt.Data:=AData;
    Reader.ReadRootComponent(tt);
  finally
    if DestroyDriver
    then Reader.Driver.Free;
    Reader.Free;
    tt.Free;
  end;
end;

procedure BuildPaperSizesMenu(ResUnit: TResolutionUnit; AOwner: TComponent; menuPaperSizes: TMenu;
  menuOnClick: TNotifyEvent; VImageIndex, HImageIndex: Integer);
var
   p,
   i :Integer;
   newItem, newItem2 :TMenuItem;
   curPapers: array of TPaperSizes;
   u: String[5];
   preCaption: String;

begin
  if ResUnit=ruPixelsPerInch
  then begin
         curPapers:=PaperSizes_inch;
         u:=' in';
       end
  else begin
         curPapers:=PaperSizes_cm;
         u:=' cm';
         ResUnit:=ruPixelsPerCentimeter;
       end;

  //Vertical
  newItem :=TMenuItem.Create(AOwner);
  newItem.Caption:= rsVertical;
  newItem.ImageIndex:=VImageIndex;
  menuPaperSizes.Items.Add(newItem);
  for p:=Low(curPapers) to High(curPapers) do
  begin
    newItem :=TMenuItem.Create(AOwner);
    newItem.Caption:=PaperSizes_Names[p];
    newItem.Tag:=p;
    //newItem.ImageIndex:=VImageIndex;
    menuPaperSizes.Items.Add(newItem);

    for i:=Low(curPapers[p]) to High(curPapers[p]) do
    begin
      newItem2 :=TMenuItem.Create(AOwner);

      preCaption :=curPapers[p][i].name;
      if (preCaption<>'')
      then preCaption :=preCaption+' - ';

      newItem2.Caption:=preCaption+
        FloatToStrF(curPapers[p][i].w, ffFixed, 15, 2)+' x '+
        FloatToStrF(curPapers[p][i].h, ffFixed, 15, 2)+u;
      newItem2.OnClick:=menuOnClick;
      newItem2.Tag:=PaperSizesMenuTag_encode(ResUnit, True, p, i);
      newItem.Add(newItem2);
   end;
  end;

  menuPaperSizes.Items.AddSeparator;

  //Horizontal
  newItem :=TMenuItem.Create(AOwner);
  newItem.Caption:= rsHorizontal;
  newItem.ImageIndex:=HImageIndex;
  menuPaperSizes.Items.Add(newItem);
  for p:=Low(curPapers) to High(curPapers) do
  begin
    newItem :=TMenuItem.Create(AOwner);
    newItem.Caption:=PaperSizes_Names[p];
    newItem.Tag:=p;
    //newItem.ImageIndex:=HImageIndex;
    menuPaperSizes.Items.Add(newItem);

    for i:=Low(curPapers[p]) to High(curPapers[p]) do
    begin
      newItem2 :=TMenuItem.Create(AOwner);

      preCaption :=curPapers[p][i].name;
      if (preCaption<>'')
      then preCaption :=preCaption+' - ';

      newItem2.Caption:=preCaption+
        FloatToStrF(curPapers[p][i].h, ffFixed, 15, 2)+' x '+
        FloatToStrF(curPapers[p][i].w, ffFixed, 15, 2)+u;
      newItem2.OnClick:=menuOnClick;
      newItem2.Tag:=PaperSizesMenuTag_encode(ResUnit, False, p, i);
      newItem.Add(newItem2);
    end;
  end;
end;

procedure PaperSizesMenuTag_decode(ATag: Integer; var ResUnit: TResolutionUnit; var Paper: TPaperSize);
var
   p, i :Integer;
   vert :Boolean;
   t :Single;

begin
     // ResUnit - vert(1bit) - pIndex(8bit) - iIndex(8bit)
  i :=(ATag and $00FF);
  p :=(ATag and $FF00) shr 8;
  vert :=Boolean((ATag and $10000) shr 16);
  ResUnit :=TResolutionUnit((ATag and $E0000) shr 17);

  if (ResUnit=ruPixelsPerCentimeter)
  then Paper :=PaperSizes_cm[p][i]
  else Paper :=PaperSizes_inch[p][i];

  if not(vert) then
  begin
    //Swap w and h
    t :=Paper.h;
    Paper.h:=Paper.w;
    Paper.w:=t;
  end;
end;

function PaperSizesMenuTag_encode(ResUnit: TResolutionUnit; vert: Boolean; pIndex, iIndex: Byte): Integer;
begin
         // ResUnit               -   vert(1bit)         -   pIndex(8bit)  -  iIndex(8bit)
  Result :=(Byte(ResUnit) shl 17) or (Byte(vert) shl 16) or (pIndex shl 8) or iIndex;
end;


procedure BuildSourcesMenu(AOwner: TComponent; menuSources: TMenu; menuOnClick: TNotifyEvent);
var
   i, res  :Integer;
   newItem :TMenuItem;
   curSource:PSourceInfo;
   curTitle:PChar;

begin
  for i:=0 to theBridge.SourcesImpl.Count-1 do
  begin
    curSource:= theBridge.SourcesImpl.Data[i];
    if (curSource<>nil) then
    begin
      curTitle:= '';
      newItem:= TMenuItem.Create(AOwner);

      res:= curSource^.Inst.UI_Title(curTitle);
      if (res >0 ) and (curTitle <> '')
      then begin
             newItem.Caption:= curTitle;
             StrDispose(curTitle);
           end
      else newItem.Caption:= theBridge.SourcesImpl.Key[i];

      newItem.ImageIndex:= curSource^.Inst.UI_ImageIndex;
      newItem.Tag:= i;
      newItem.OnClick:= menuOnClick;
      newItem.Enabled:= curSource^.Inst.Enabled;
      menuSources.Items.Add(newItem);
    end;
  end;
end;

procedure BuildDestinationsMenu(AOwner: TComponent; menuDestinations: TMenu; menuOnClick: TNotifyEvent);
var
   i, res  :Integer;
   newItem :TMenuItem;
   curDestination:PDestinationInfo;
   curTitle:PChar;

begin
  //Add SaveAsFile Destination
  newItem:= TMenuItem.Create(AOwner);
  newItem.Caption:= rsDestination_Default;
  newItem.ImageIndex:= 6;
  newItem.Tag:= -1;
  newItem.OnClick:= menuOnClick;
  menuDestinations.Items.Add(newItem);

  for i:=0 to theBridge.DestinationsImpl.Count-1 do
  begin
    curDestination:= theBridge.DestinationsImpl.Data[i];
    if (curDestination<>nil) then
    begin
      curTitle:= '';
      newItem:= TMenuItem.Create(AOwner);

      res:= curDestination^.Inst.UI_Title(curTitle);
      if (res >0 ) and (curTitle <> '')
      then begin
             newItem.Caption:= curTitle;
             StrDispose(curTitle);
           end
      else newItem.Caption:= theBridge.DestinationsImpl.Key[i];

      newItem.ImageIndex:= curDestination^.Inst.UI_ImageIndex;
      newItem.Tag:= i;
      newItem.OnClick:= menuOnClick;
      newItem.Enabled:= curDestination^.Inst.Enabled;
      menuDestinations.Items.Add(newItem);
    end;
  end;
end;

function FindMenuItemByTag(AMenu: TMenu; ATag: PtrInt): TMenuItem;
var
   i: Integer;

begin
  Result:= Nil;

  //No Need to recurvilly find in subitems
  for i:=0 to AMenu.Items.Count-1 do
    if (AMenu.Items[i].Tag = ATag)
    then begin
           Result:= AMenu.Items[i];
           break;
         end;
end;

procedure GetThumnailSize(thumbWidth, thumbHeight, imgWidth, imgHeight:Integer;
                          var newWidth, newHeight:Integer);
var
   rW, rH:Single;

begin
  if (thumbWidth=0)
  then rW:=1
  else rW := imgWidth / thumbWidth;
  if (thumbHeight=0)
  then rH:=1
  else rH := imgHeight / thumbHeight;

  if (rW > rH)
  then begin
         newHeight := round(imgHeight / rW);
         newWidth := thumbWidth;
       end
  else begin
         newWidth := round(imgWidth / rH);
         newHeight := thumbHeight;
       end;
end;

function FindFileListItem(AList: TListItems; AFileName: String): TFileListItem;
var
   i:Integer;

begin
  Result:=nil;
  for i:=AList.Count-1 downto 0 do
  begin
    if (TFileListItem(AList[i]).FileName=AFileName)
    then begin Result:=TFileListItem(AList[i]); break; end;
  end;
end;


end.

