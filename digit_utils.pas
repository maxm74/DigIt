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
  FPImage, Menus, BGRAPapers,
  DigIt_types, Digit_Bridge_Intf, Digit_Bridge_Impl;

resourcestring
  rsDestination_Default = 'Save as Files';
  rsVertical = '   Vertical';
  rsHorizontal = '    Horizontal';

procedure BuildPaperSizesMenu(ResUnit: TResolutionUnit;
                              AOwner: TComponent; menuPaperSizes: TMenu; menuOnClick: TNotifyEvent;
                              VImageIndex, HImageIndex: Integer);

procedure PaperSizesMenuTag_decode(ATag:Integer; var ResUnit: TResolutionUnit; var Paper: TPaperSize);
function PaperSizesMenuTag_encode(ResUnit: TResolutionUnit; vert: Boolean; pIndex, iIndex: Byte): Integer;

procedure BuildSourcesMenu(AOwner: TComponent;
                           menuSources: TMenu; menuSourcesOnClick: TNotifyEvent;
                           ASelectedSource: PSourceInfo);

procedure SourcesMenuTag_decode(ATag: Integer; var Index, SubIndex: Integer);
function SourcesMenuTag_encode(Index, SubIndex: Integer): Integer;

procedure BuildDestinationsMenu(AOwner: TComponent; menuDestinations: TMenu; menuOnClick: TNotifyEvent);

procedure BuildProfilesMenu(AOwner: TComponent;
                            itemProfiles: TMenuItem; itemOnClick: TNotifyEvent;
                            selectedProfile: Integer; Profiles: TStringArray);

function FindMenuItemByTag(AMenu: TMenu; ATag: PtrInt): TMenuItem;

procedure GetProportionalSize(Width, Height, imgWidth, imgHeight: Integer;
                              var newWidth, newHeight: Integer);
function GetProportionalSide(ASide, imgSide, imgOtherSide: Integer): Integer;

{** Convert PhysicalSize to/from Cm/Inch}
function PhysicalSizeConvert(ASourceUnit: TResolutionUnit; ASourceSize: Single;
                             ATargetUnit: TResolutionUnit=ruPixelsPerInch;
                             AResolution: Single = 96): Single;

implementation

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
  for i:=2 to menuSources.Items.Count-1 do
    menuSources.Items.Delete(2);

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

procedure SourcesMenuTag_decode(ATag: Integer; var Index, SubIndex: Integer);
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

procedure BuildProfilesMenu(AOwner: TComponent;
                            itemProfiles: TMenuItem; itemOnClick: TNotifyEvent;
                            selectedProfile: Integer; Profiles: TStringArray);
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
  for i:=3 to itemProfiles.Count-1 do
    itemProfiles.Delete(3);

  for i:=0 to Length(Profiles)-1 do
  begin
    newItem:= TMenuItem.Create(AOwner);

    newItem.Caption:= Profiles[i];
    //newItem.ImageIndex:= ;
    newItem.Tag:= i;
    newItem.OnClick:= itemOnClick;
    itemProfiles.Add(newItem);
    newItem.Default:= (i = selectedProfile);
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

function PhysicalSizeConvert(ASourceUnit: TResolutionUnit; ASourceSize: Single;
                             ATargetUnit: TResolutionUnit; AResolution: Single): Single;
begin
  Result:= ASourceSize;
  // already in expected unit
  if ASourceUnit = ATargetUnit then exit;

  // checks if resolution is ill-defined
  if (ATargetUnit = ruNone) and (AResolution < 2) then AResolution:= 96; // assume legacy 96 DPI

  case ASourceUnit of
    ruPixelsPerCentimeter: Result:= ASourceSize/2.54; // from Cm to Inch
    ruPixelsPerInch: Result:= ASourceSize*2.54;       // from Inch to Cm
   else {ruNone}
    Result:= ASourceSize / AResolution                // from Pixels
  end;
end;

end.

