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
  BGRABitmap,
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

    function LoadTitleIco(aFileName: String;
                          var aTitle, aDescription: String; var aIcon: TBGRABitmap): Boolean; overload;
  end;

implementation

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
begin

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

end;

function TDigIt_Templates.LoadTitleIco(aFileName: String;
                                       var aTitle, aDescription: String; var aIcon: TBGRABitmap): Boolean;
begin

end;

end.

