(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Utils Functions                                                          **
*******************************************************************************)

unit DigIt_Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FPImage, Menus,
  BGRAPapers,
  {$ifdef BGRAControls}
  BGRAImageManipulation,
  {$endif}
  DigIt_Types, Digit_Bridge_Intf, Digit_Bridge_Impl, DigIt_Sources;

resourcestring
  rsDestination_Default = 'Save as Files';
  rsVertical = '   Vertical';
  rsHorizontal = '    Horizontal';
  rsBusinessCard = 'Business Card';

procedure BuildPaperSizesMenu(PhysicalUnit: TPhysicalUnit;
                              AOwner: TComponent; menuPaperSizes: TMenu; menuOnClick: TNotifyEvent;
                              VImageIndex, HImageIndex: Integer);

procedure PaperSizesMenuTag_decode(ATag:Integer; out PhysicalUnit: TPhysicalUnit; out Paper: TPaperSize);
function PaperSizesMenuTag_encode(PhysicalUnit: TPhysicalUnit; vert: Boolean; pIndex, iIndex: Byte): Integer;

procedure BuildSourcesMenu(AOwner: TComponent;
                           menuSources: TMenu; menuSourcesOnClick: TNotifyEvent;
                           ASelectedSource: PSourceInfo);

procedure SourcesMenuTag_decode(ATag: Integer; out Index, SubIndex: Integer);
function SourcesMenuTag_encode(Index, SubIndex: Integer): Integer;

procedure BuildDestinationsMenu(AOwner: TComponent; menuDestinations: TMenu; menuOnClick: TNotifyEvent);

procedure BuildProfilesMenu(AOwner: TComponent;
                            menuProfiles: TMenu; itemOnClick: TNotifyEvent;
                            Profiles: TStringArray);

function FindMenuItemByTag(AMenu: TMenu; ATag: PtrInt): TMenuItem;

procedure GetProportionalSize(Width, Height, imgWidth, imgHeight: Integer;
                              var newWidth, newHeight: Integer);
function GetProportionalSide(ASide, imgSide, imgOtherSide: Integer): Integer;

function GetUserName: String;

procedure ConvertCmPaperTo(PhysicalUnit: TPhysicalUnit; var Paper: TPaperSize);


implementation

{$ifdef Windows}
uses Windows;
{$else}
uses users, baseunix;
{$endif}

type
  TPaperSizesArray = array of TPaperSizes;

var
   PaperSizes: TPaperSizesArray=nil;

procedure AddPapers(const NewPapersName: TStringArray; const NewPapers: TPaperSizesArray; var APaperArray: TPaperSizesArray);
var
   i,
   oldLength,
   oldLenPapersName,
   lenNewPapers,
   lenNewPapersName: Integer;

begin
  oldLength:= Length(APaperArray);
  lenNewPapers:= Length(NewPapers);
  oldLenPapersName:= Length(PaperSizes_Names);
  lenNewPapersName:= Length(NewPapersName);

  SetLength(APaperArray, oldLength+lenNewPapers);
  SetLength(PaperSizes_Names, oldLenPapersName+lenNewPapersName);
  for i:=0 to lenNewPapers-1 do
  begin
    APaperArray[oldLength+i]:= NewPapers[i];
    if (i<lenNewPapersName) then PaperSizes_Names[oldLenPapersName+i]:= NewPapersName[i];
  end;
end;

function GetPapers(var PhysicalUnit: TPhysicalUnit): TPaperSizesArray;
var
   p, i: Integer;

begin
  Case PhysicalUnit of
    cuPixel,
    cuPercent,
    cuCentimeter: begin
      Result:= PaperSizes_cm;
      PhysicalUnit:= cuCentimeter;
    end;
    cuMillimeter: begin
      Result:= PaperSizes_cm;
      for p:=Low(Result) to High(Result) do
      for i:=Low(Result[p]) to High(Result[p]) do
      begin
        Result[p][i].w:= Result[p][i].w * 10;
        Result[p][i].h:= Result[p][i].h * 10;
      end;
    end;
    cuInch: Result:= PaperSizes_inch;
    cuPica: begin
      Result:= PaperSizes_inch;
      for p:=Low(Result) to High(Result) do
      for i:=Low(Result[p]) to High(Result[p]) do
      begin
        Result[p][i].w:= Result[p][i].w * 6;
        Result[p][i].h:= Result[p][i].h * 6;
      end;
    end;
    cuPoint: begin
      Result:= PaperSizes_inch;
      for p:=Low(Result) to High(Result) do
      for i:=Low(Result[p]) to High(Result[p]) do
      begin
        Result[p][i].w:= Result[p][i].w * 72;
        Result[p][i].h:= Result[p][i].h * 72;
      end;
    end;
  end;
end;

procedure ConvertCmPaperTo(PhysicalUnit: TPhysicalUnit; var Paper: TPaperSize);
begin
  Paper.w:= PhysicalSizeConvert(cuCentimeter, Paper.w, PhysicalUnit);
  Paper.h:= PhysicalSizeConvert(cuCentimeter, Paper.h, PhysicalUnit);
(*oldcode  Case PhysicalUnit of
    cuPixel,
    cuPercent,
    cuCentimeter: begin
      Result:= ruPixelsPerCentimeter;
    end;
  cuMillimeter: begin
      Paper.w:= Paper.w / 10;
      Paper.h:= Paper.h / 10;
    end;
    cuInch: begin
      Paper.w:= Paper.w / 2.54;
      Paper.h:= Paper.h / 2.54;
    end;
    cuPica: begin
      Paper.w:= Paper.w / 6;
      Paper.h:= Paper.h / 6;

      Result:= ruPixelsPerInch;
    end;
    cuPoint: begin
      Paper.w:= Paper.w / 72;
      Paper.h:= Paper.h / 72;

      Result:= ruPixelsPerInch;
    end;
  end;
  *)
end;

procedure BuildPaperSizesMenu(PhysicalUnit: TPhysicalUnit; AOwner: TComponent; menuPaperSizes: TMenu;
  menuOnClick: TNotifyEvent; VImageIndex, HImageIndex: Integer);
var
   p,
   i :Integer;
   newItem, newItem2 :TMenuItem;
//   curPapers: array of TPaperSizes;
   u,
   preCaption: String;

begin
  PaperSizes:= GetPapers(PhysicalUnit);
  u:= ' '+PhysicalUnitShortName[PhysicalUnit];

  //Vertical
  newItem :=TMenuItem.Create(AOwner);
  newItem.Caption:= rsVertical;
  newItem.ImageIndex:=VImageIndex;
  menuPaperSizes.Items.Add(newItem);
  for p:=Low(PaperSizes) to High(PaperSizes) do
  begin
    newItem :=TMenuItem.Create(AOwner);
    newItem.Caption:=PaperSizes_Names[p];
    newItem.Tag:=p;
    //newItem.ImageIndex:=VImageIndex;
    menuPaperSizes.Items.Add(newItem);

    for i:=Low(PaperSizes[p]) to High(PaperSizes[p]) do
    begin
      newItem2 :=TMenuItem.Create(AOwner);

      preCaption :=PaperSizes[p][i].name;
      if (preCaption<>'')
      then preCaption :=preCaption+' - ';

      newItem2.Caption:=preCaption+
        FloatToStrF(PaperSizes[p][i].w, ffFixed, 15, 2)+' x '+
        FloatToStrF(PaperSizes[p][i].h, ffFixed, 15, 2)+u;
      newItem2.OnClick:=menuOnClick;
      newItem2.Tag:=PaperSizesMenuTag_encode(PhysicalUnit, True, p, i);
      newItem.Add(newItem2);
   end;
  end;

  menuPaperSizes.Items.AddSeparator;

  //Horizontal
  newItem :=TMenuItem.Create(AOwner);
  newItem.Caption:= rsHorizontal;
  newItem.ImageIndex:=HImageIndex;
  menuPaperSizes.Items.Add(newItem);
  for p:=Low(PaperSizes) to High(PaperSizes) do
  begin
    newItem :=TMenuItem.Create(AOwner);
    newItem.Caption:=PaperSizes_Names[p];
    newItem.Tag:=p;
    //newItem.ImageIndex:=HImageIndex;
    menuPaperSizes.Items.Add(newItem);

    for i:=Low(PaperSizes[p]) to High(PaperSizes[p]) do
    begin
      newItem2 :=TMenuItem.Create(AOwner);

      preCaption :=PaperSizes[p][i].name;
      if (preCaption<>'')
      then preCaption :=preCaption+' - ';

      newItem2.Caption:=preCaption+
        FloatToStrF(PaperSizes[p][i].h, ffFixed, 15, 2)+' x '+
        FloatToStrF(PaperSizes[p][i].w, ffFixed, 15, 2)+u;
      newItem2.OnClick:=menuOnClick;
      newItem2.Tag:=PaperSizesMenuTag_encode(PhysicalUnit, False, p, i);
      newItem.Add(newItem2);
    end;
  end;
end;

procedure PaperSizesMenuTag_decode(ATag: Integer; out PhysicalUnit: TPhysicalUnit; out Paper: TPaperSize);
var
   p, i :Integer;
   vert :Boolean;
   t :Single;

begin
     // PhysicalUnit - vert(1bit) - pIndex(8bit) - iIndex(8bit)
  i:= (ATag and $00FF);
  p:= (ATag and $FF00) shr 8;
  vert:= Boolean((ATag and $10000) shr 16);
  PhysicalUnit:= TPhysicalUnit((ATag and $E0000) shr 17);

  if (PaperSizes <> nil) then Paper:= PaperSizes[p][i];

  if not(vert) then
  begin
    //Swap w and h
    t :=Paper.h;
    Paper.h:=Paper.w;
    Paper.w:=t;
  end;
end;

function PaperSizesMenuTag_encode(PhysicalUnit: TPhysicalUnit; vert: Boolean; pIndex, iIndex: Byte): Integer;
begin
              // PhysicalUnit               -   vert(1bit)    -   pIndex(8bit)  -  iIndex(8bit)
  Result:= (Byte(PhysicalUnit) shl 17) or (Byte(vert) shl 16) or (pIndex shl 8) or iIndex;
end;

procedure BuildSourcesMenu(AOwner: TComponent;
                           menuSources: TMenu; menuSourcesOnClick: TNotifyEvent;
                           ASelectedSource: PSourceInfo);
var
   i, iSub,
   cSub, res: Integer;
   newItem,
   newSep: TMenuItem;
   curSource: PSourceInfo;
   curSourceItems: IDigIt_Source_Items;
   curTitle: PChar;
   curSelected: Boolean;

begin
  menuSources.Items.Clear;

  for i:=0 to Sources.Count-1 do
  begin
    curSource:= Sources.Data[i];
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
      else newItem.Caption:= Sources.Key[i];

      newItem.ImageIndex:= curSource^.Inst.UI_ImageIndex;
      newItem.Tag:= SourcesMenuTag_encode(i, -1);
      newItem.OnClick:= menuSourcesOnClick;
      newItem.Enabled:= curSource^.Inst.Enabled;

      curSelected:= (curSource = ASelectedSource);
      menuSources.Items.Add(newItem);
      newItem.Default:= curSelected;

      if newItem.Enabled and (curSource^.Inst is IDigIt_Source_Items) then
      begin
        //Get Sub Items so the User can select directly the Device from Menù
        curSourceItems:= (curSource^.Inst as IDigIt_Source_Items);
        cSub:= curSourceItems.GetCount;
        if (cSub > 0) then
        begin
          newSep:= TMenuItem.Create(AOwner);
          newSep.Caption:= '-';
          menuSources.Items.Add(newSep);

          res:= 0;
          for iSub:=0 to cSub-1 do
          begin
            curTitle:= '';
            if curSourceItems.Get(iSub, curTitle) and (curTitle <> '') then
            begin
              inc(res);
              newItem:= TMenuItem.Create(AOwner);
              newItem.Caption:= curTitle;
              StrDispose(curTitle);
              newItem.Tag:= SourcesMenuTag_encode(i, iSub);
              newItem.OnClick:= menuSourcesOnClick;
              menuSources.Items.Add(newItem);
              newItem.Default:= curSelected and curSourceItems.Selected(iSub);
            end;
          end;

          if (res > 0)
          then begin
                 newSep:= TMenuItem.Create(AOwner);
                 newSep.Caption:= '-';
                 menuSources.Items.Add(newSep);
               end
          else menuSources.Items.Delete(menuSources.Items.IndexOf(newSep));
        end;
      end;
    end;
  end;
end;

procedure SourcesMenuTag_decode(ATag: Integer; out Index, SubIndex: Integer);
begin
  SubIndex:= (ATag and $FFFF);
  Index:= (ATag and $FFFF0000) shr 16;
  if (Index = $FFFF) then Index:= -1;
  if (SubIndex = $FFFF) then SubIndex:= -1;
end;

function SourcesMenuTag_encode(Index, SubIndex: Integer): Integer;
begin
  if (Index < 0) then Index:= $FFFF;
  if (SubIndex < 0) then SubIndex:= $FFFF;

  Result:= (Index shl 16) or SubIndex;
end;

procedure BuildDestinationsMenu(AOwner: TComponent; menuDestinations: TMenu; menuOnClick: TNotifyEvent);
var
   i, res  :Integer;
   newItem :TMenuItem;
   //curDestination:PDestinationInfo;
   curTitle:PChar;

begin
  //Add SaveAsFile Destination
  newItem:= TMenuItem.Create(AOwner);
  newItem.Caption:= rsDestination_Default;
  newItem.ImageIndex:= 6;
  newItem.Tag:= 0;
  newItem.OnClick:= menuOnClick;
  menuDestinations.Items.Add(newItem);

  { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
  (*
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
      newItem.Tag:= i+1;
      newItem.OnClick:= menuOnClick;
      newItem.Enabled:= curDestination^.Inst.Enabled;
      menuDestinations.Items.Add(newItem);
    end;
  end;
  *)
end;

procedure BuildProfilesMenu(AOwner: TComponent; menuProfiles: TMenu;
                            itemOnClick: TNotifyEvent; Profiles: TStringArray);
var
   i: Integer;
   newItem: TMenuItem;

begin
  //Delete old Profiles MenuItem if any
  for i:=1 to menuProfiles.Items.Count-3 do
    menuProfiles.Items.Delete(0);

  //Hide or Show Separator whether or not there are Profiles
  menuProfiles.Items[0].Visible:= (Length(Profiles) > 0);

  for i:=Length(Profiles)-1 downto 0 do
  begin
    newItem:= TMenuItem.Create(AOwner);

    newItem.Caption:= Profiles[i];
    //newItem.ImageIndex:= ;
    newItem.Tag:= i;
    newItem.OnClick:= itemOnClick;
    menuProfiles.Items.Insert(0, newItem);
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

procedure GetProportionalSize(Width, Height, imgWidth, imgHeight: Integer;
                              var newWidth, newHeight: Integer);
var
   rW, rH:Single;

begin
  if (Width = 0)
  then rW:= 1
  else rW:= imgWidth/Width;

  if (Height = 0)
  then rH:= 1
  else rH:= imgHeight/Height;

  if (rW > rH)
  then begin
         newHeight:= Round(imgHeight/rW);
         newWidth:= Width;
       end
  else begin
         newWidth:= Round(imgWidth/rH);
         newHeight:= Height;
       end;
end;

function GetProportionalSide(ASide, imgSide, imgOtherSide: Integer): Integer;
var
   r: Single;

begin
  if (ASide = 0)
  then r:= 1
  else r:= imgSide/ASide;

  Result:= Round(imgOtherSide/r);
end;

function GetUserName: String;
{$ifdef WINDOWS}
var
  nSize: DWord;
{$endif}

begin
  {$ifdef WINDOWS}
  nSize := 1024;
  SetLength(Result, nSize);
  if Windows.GetUserName(PChar(Result), nSize) then
  begin
    SetLength(Result, nSize - 1)
  end
  else Result:= '';
  {$else}
  Result:= users.GetUserName(fpgetuid);
  {$endif}
end;

initialization
   AddPapers(['US', rsBusinessCard], [Paper_US_cm, Paper_BUSINESS_CARD_cm], PaperSizes_cm);
   AddPapers(['US', rsBusinessCard], [Paper_US_inch, Paper_BUSINESS_CARD_inch], PaperSizes_inch);

end.

