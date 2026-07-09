(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2026 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Templates Class                                                          **
*******************************************************************************)
unit DigIt_Templates;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg,
  BGRABitmapTypes, BGRABitmap,
  DigIt_Types, DigIt_Session;

type

  { TDigIt_Templates }

  TDigIt_Templates_Section = (
    tempSect_Title,
    tempSect_Description,
    tempSect_SettingsSummary,
    tempSect_Icon,
    tempSect_Preview,
    tempSect_CropAreas,       //from this point aSession must be <> nil in Load/Save
    tempSect_PageSettings,
    tempSect_Counter          //...Others maybe tomorrow
  );
  TDigIt_Templates_Sections = set of TDigIt_Templates_Section;

  TDigIt_Templates_Data = record
    Title,
    Description,
    SettingsSummary: String;
    Icon,
    Preview: TBGRABitmap;
  end;

  TDigIt_Templates = class
  protected
    procedure LoadBGRABitmap(aXML: TRttiXMLConfig; const AXMLPath: String;
                             ADest: TCustomUniversalBitmap; AOptions: TBGRALoadingOptions);

    procedure SaveBGRABitmap(aXML: TRttiXMLConfig; const AXMLPath: String;
                             ASource: TCustomUniversalBitmap; AFormat: TBGRAImageFormat);
  public
    function Load(aFileName: String;
                  whatRead: TDigIt_Templates_Sections;
                  var aRead: TDigIt_Templates_Data;
                  aSession: TDigIt_Session=nil): Boolean; overload;
    function Load(aSession: TDigIt_Session;
                  aXML: TRttiXMLConfig; whatRead: TDigIt_Templates_Sections;
                  var aRead: TDigIt_Templates_Data): Boolean; overload;

    function Save(aFileName: String;
                  whatSave: TDigIt_Templates_Sections;
                  aSave: TDigIt_Templates_Data;
                  aSession: TDigIt_Session=nil): Boolean; overload;
    function Save(aSession: TDigIt_Session;
                  aXML: TRttiXMLConfig; whatSave: TDigIt_Templates_Sections;
                  aSave: TDigIt_Templates_Data): Boolean; overload;

    function LoadTitleIco(aXML: TRttiXMLConfig;
                          var aTitle, aDescription: String; var aIcon: TBGRABitmap): Boolean; overload;
    function LoadTitleIco(aFileName: String;
                          var aTitle, aDescription: String; var aIcon: TBGRABitmap): Boolean; overload;
  end;

implementation

uses base64, Graphics, FpImage;

{ TDigIt_Templates }

function TDigIt_Templates.Load(aFileName: String;
                               whatRead: TDigIt_Templates_Sections; var aRead: TDigIt_Templates_Data;
                               aSession: TDigIt_Session=nil): Boolean;
var
   aXML: TRttiXMLConfig;

begin
  Result:= False;

  if FileExists(aFileName) then
  try
     aXML:= TRttiXMLConfig.Create(aFileName);

     Result:= Load(aSession, aXML, whatRead, aRead);

  finally
    aXML.Free;
  end;
end;

function TDigIt_Templates.Load(aSession: TDigIt_Session;
                               aXML: TRttiXMLConfig; whatRead: TDigIt_Templates_Sections;
                               var aRead: TDigIt_Templates_Data): Boolean;
var
   curSelected: Integer;

begin
  Result:= False;
  if (aXML <> nil) then
  with aRead do
  try
     if (tempSect_Title in whatRead) then Title:= aXML.GetValue('Title', '');

     if (tempSect_Description in whatRead) then Description:= aXML.GetValue('Description', '');
     if (tempSect_SettingsSummary in whatRead) then SettingsSummary:= aXML.GetValue('SettingsSummary', '');

     if (tempSect_Icon in whatRead) and (Icon <> nil) then LoadBGRABitmap(aXML, 'Icon', Icon, []);
     if (tempSect_Preview in whatRead) and (Preview <> nil) then LoadBGRABitmap(aXML, 'Preview', Preview, []);

     if (aSession <> nil) then
     begin
       if (tempSect_CropAreas in whatRead) then aSession.LoadCropAreas(aXML, False, curSelected);

(*       if (tempSect_PageSettings in whatRead)
       then
       if (tempSect_Counter in whatRead)
       then*)
     end;

     Result:= True;
  finally
  end;
end;

function TDigIt_Templates.Save(aFileName: String;
                               whatSave: TDigIt_Templates_Sections; aSave: TDigIt_Templates_Data;
                               aSession: TDigIt_Session=nil): Boolean;
var
   aXML: TRttiXMLConfig;

begin
  Result:= False;

  try
     aXML:= TRttiXMLConfig.Create(aFileName);

     Result:= Save(aSession, aXML, whatSave, aSave);

  finally
    aXML.Free;
  end;
end;

function TDigIt_Templates.Save(aSession: TDigIt_Session;
                               aXML: TRttiXMLConfig; whatSave: TDigIt_Templates_Sections;
                               aSave: TDigIt_Templates_Data): Boolean;
begin
  Result:= False;
  if (aXML <> nil) then
  with aSave do
  try
     if (tempSect_Title in whatSave) then Title:= aXML.GetValue('Title', '');

     if (tempSect_Description in whatSave) then aXML.SetValue('Description', Description);
     if (tempSect_SettingsSummary in whatSave) then aXML.SetValue('SettingsSummary', SettingsSummary);

     if (tempSect_Icon in whatSave) and (Icon <> nil) then SaveBGRABitmap(aXML, 'Icon', Icon, ifJpeg);
     if (tempSect_Preview in whatSave) and (Preview <> nil) then SaveBGRABitmap(aXML, 'Preview', Preview, ifJpeg);

     if (aSession <> nil) then
     begin
       if (tempSect_CropAreas in whatSave) then aSession.SaveCropAreas(aXML, False, -1);

(*       if (tempSect_PageSettings in whatSave)
       then
       if (tempSect_Counter in whatSave)
       then*)
     end;

     Result:= True;
  finally
  end;
end;

procedure TDigIt_Templates.LoadBGRABitmap(aXML: TRttiXMLConfig; const AXMLPath: String;
                                          ADest: TCustomUniversalBitmap; AOptions: TBGRALoadingOptions);
var
  Str: TStringStream;
  format: TBGRAImageFormat;
  reader: TFPCustomImageReader;
  data64: String;

begin
  format:= ifUnknown;
  aXML.GetValue(AXMLPath+'/format', format, TypeInfo(TBGRAImageFormat));
  data64:= aXML.GetValue(AXMLPath+'/data', '');

  if (Length(Trim(data64)) > 0) then
  try
     Str:= TStringStream.Create(DecodeStringBase64(data64));
     Str.Position:= 0;

     reader:= BGRABitmapTypes.CreateBGRAImageReader(format);

     ADest.LoadFromStream(Str, reader, AOptions);
  finally
    reader.Free;
    Str.Free;
  end;
end;

procedure TDigIt_Templates.SaveBGRABitmap(aXML: TRttiXMLConfig; const AXMLPath: String;
                                          ASource: TCustomUniversalBitmap;  AFormat: TBGRAImageFormat);
var
  Str: TStringStream;
  writer: TFPCustomImageWriter;
  data64: String;

begin
  try
     Str:= TStringStream.Create();

     writer:= CreateBGRAImageWriter(AFormat, ASource.HasTransparentPixels);
     TFPCustomImage(ASource).SaveToStream(Str, writer);

     data64:= EncodeStringBase64(Str.AnsiDataString);

     aXML.SetValue(AXMLPath+'/format', AFormat, TypeInfo(TBGRAImageFormat));
     aXML.SetValue(AXMLPath+'/data', data64);

  finally
    writer.Free;
    Str.Free;
  end;
end;

function TDigIt_Templates.LoadTitleIco(aXML: TRttiXMLConfig;
                                       var aTitle, aDescription: String; var aIcon: TBGRABitmap): Boolean;
begin
  Result:= False;

  if (aXML <> nil) then
  try
     aTitle:= aXML.GetValue('Title', '');
     aDescription:= aXML.GetValue('Description', '');
     if (aIcon <> nil) then LoadBGRABitmap(aXML, 'Icon', aIcon, []);

     Result:= True;
  finally
  end;
end;

function TDigIt_Templates.LoadTitleIco(aFileName: String;
                                       var aTitle, aDescription: String; var aIcon: TBGRABitmap): Boolean;
var
   aXML: TRttiXMLConfig;

begin
  Result:= False;

  if FileExists(aFileName) then
  try
     aXML:= TRttiXMLConfig.Create(aFileName);

     Result:= LoadTitleIco(aXML, aTitle, aDescription, aIcon);

  finally
    aXML.Free;
  end;
end;

end.

