(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (c) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Profiles  ( maybe tomorrow implementation of IDigIt_Profiles )           **
*******************************************************************************)
unit DigIt_Profiles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg,
  Digit_Bridge_Intf, Digit_Bridge_Impl;

const
  PROF_Item = 'Profiles/Profile_';

type
  { TDigIt_Profiles }

  TDigIt_Profiles = class(TNoRefCountObject)
  private
    XMLFilename: String;

  public
    function LoadFromXML(const AFilename: String; var ATitleArray: TStringArray): Boolean;

    function Add(const AFilename: String; var ATitleArray: TStringArray;
                 ASource: PSourceInfo; ASourceParams: IDigIt_Params; const ASourceName: String): Boolean;
  end;

var
  DigIt_Profiles: TDigIt_Profiles=nil;

implementation

{$R *.lfm}

uses Laz2_DOM, FileUtil, MM_StrUtils;

{ TDigIt_Profiles }

function TDigIt_Profiles.LoadFromXML(const AFilename: String;
                                     var ATitleArray: TStringArray): Boolean;
var
   aXML: TRttiXMLConfig;
   i, iCount: Integer;

begin
  Result:= False;
  try
     aXML:= TRttiXMLConfig.Create(AFilename);

     //Load Profiles
     ATitleArray:= nil;
     iCount:= aXML.GetValue('Profiles/Count', 0);
     SetLength(ATitleArray, iCount);
     for i:=0 to iCount-1 do
     begin
       ATitleArray[i]:= aXML.GetValue(PROF_Item+IntToStr(i)+'/Name', 'Profile '+IntToStr(i));
     end;

     Result:= True;

  finally
    aXML.Free;
  end;
end;

function TDigIt_Profiles.Add(const AFilename: String;
                             var ATitleArray: TStringArray; ASource: PSourceInfo;
                             ASourceParams: IDigIt_Params; const ASourceName: String): Boolean;
var
   newProfileTitleP: PChar;
   newProfileTitle,
   curXMPath: String;
   aXML: TRttiXMLConfig;
   res, iCount: Integer;

begin
  if (ASource <> nil) then
  try
     chkTitleArray:= @ATitleArray;

     //First tell to Params a Description then to Source
     newProfileTitleP:= ''; res:= 0;
     if (ASourceParams <> nil) then
     begin
       res:= ASourceParams.Summary(newProfileTitleP);
       if (res >0 ) and (newProfileTitleP <> '') then
       begin
         newProfileTitle:= newProfileTitleP;
         StrDispose(newProfileTitleP);
         newProfileTitleP:= '';
       end;
     end;

     res:= ASource^.Inst.UI_Title(newProfileTitleP);
     if (res >0 ) and (newProfileTitleP <> '') then
     begin
       if (newProfileTitle = '')
       then newProfileTitle:= newProfileTitleP
       else newProfileTitle:= newProfileTitle+' - '+newProfileTitleP;

       StrDispose(newProfileTitleP);
       newProfileTitleP:= '';
     end;

//     Result:= TFormEditText.Execute(rsProfiles_AddCurrent, rsProfiles_Title, '', newProfileTitle, @AddCheck);
//     if Result then
     begin
       Result:= False;
       try
          aXML:= TRttiXMLConfig.Create(AFilename);

          //Load Profiles Count
          iCount:= aXML.GetValue('Profiles/Count', 0);

          curXMPath:= PROF_Item+IntToStr(iCount)+'/';

          //Add new Profile as Last
          aXML.SetValue(curXMPath+'Name', newProfileTitle);
          aXML.SetValue('Profiles/Count', iCount+1);

          //Save Source
          aXML.SetValue(curXMPath+'Source/Name', ASourceName);
          aXML.DeletePath(curXMPath+'Source/Params/');

       finally
         aXML.Free; aXML:= nil;
       end;

       //FPC Bug?
       //If a key like "Source/Params" is written to the same open file, even after a flush, it is ignored.
       //So we do it after destroying XML.

       if (ASourceParams <> nil)
       then ASourceParams.Save(PChar(AFilename), PChar(curXMPath+'Source/Params'));

       SetLength(ATitleArray, iCount+1);
       ATitleArray[iCount]:= newProfileTitle;
       Result:= True;
     end;

  finally
    if (newProfileTitleP <> '') then StrDispose(newProfileTitleP);
  end;
end;

end.

