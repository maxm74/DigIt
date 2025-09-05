(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Sources List ( Implementation of IDigIt_Sources )                        **
*******************************************************************************)
unit DigIt_Sources;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg,
  MM_OpenArrayList,
  DigIt_Types, DigIt_Bridge_Intf;

type
  { TDigIt_Sources }

  TSourceInfo = record
    Flags: DWord;
    Inst: IDigIt_Source;
  end;
  PSourceInfo = ^TSourceInfo;

  TDigIt_Sources = class(specialize TOpenArrayList<TSourceInfo, String>, IDigIt_Sources)
  protected
    rSelected: PSourceInfo;
    rSelectedName: String;
    rSelectedIndex: Integer;
    rSelectedParams: IDigIt_Params;

    function FreeElement(var aData: TSourceInfo): Boolean; override;
    function CompData(aData1, aData2: TSourceInfo): Integer; override;

  public
    //IDigIt_Sources implementation
    function Register(const aName: PChar; const aClass: IDigIt_Source): Boolean; stdcall;

    constructor Create;

    function Get(SourceName: String; CreateParams, UserParams: Boolean;
                 out ASource: PSourceInfo; var AParams: IDigIt_Params): Boolean; overload;
    function Get(SourceIndex, SourceSubIndex: Integer; CreateParams, UserParams: Boolean;
                 out ASource: PSourceInfo; var AParams: IDigIt_Params): Boolean; overload;
    function Get(aXML: TRttiXMLConfig; XMLRoot_Path: String; CreateParams: Boolean;
                 out ASource: PSourceInfo; var AParams: IDigIt_Params; out ASourceName: String): Boolean; overload;

    function Select(SourceName: String; UserParams: Boolean=False): Boolean; overload;
    function Select(SourceIndex, SourceSubIndex: Integer; UserParams: Boolean=False): Boolean; overload;
    function Select(aXML: TRttiXMLConfig; XMLRoot_Path: String; out ASourceName: String): Boolean; overload;
    function Select(ASelected: PSourceInfo; AParams: IDigIt_Params): Boolean; overload;

    function Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;
    function Save(aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;

    function Take(takeAction: DigIt_Source_TakeAction; var AFiles: TSourceFileArray; AStartIndex: Integer;
                  ASource: PSourceInfo=nil): DWord; overload;
    function Take(takeAction: DigIt_Source_TakeAction; var AFileName: String;
                  ASource: PSourceInfo=nil): Boolean; overload;

    function GetTitle(const ASource: PSourceInfo; const AParams: IDigIt_Params; const AddSourceName: Boolean): String;

    property Selected: PSourceInfo read rSelected;
    property SelectedIndex: Integer read rSelectedIndex;
    property SelectedName: String read rSelectedName;
    property SelectedParams: IDigIt_Params read rSelectedParams;
  end;

var
   Sources: TDigIt_Sources = nil;
   //The Bridge Implementation Create It, there is no point in relying on the initialization/finalization mechanism

implementation

{ TDigIt_Sources }

function TDigIt_Sources.Register(const aName: PChar; const aClass: IDigIt_Source): Boolean; stdcall;
var
   newData: TSourceInfo;

begin
  Result:= False;
  if (aClass = nil) then exit;

  //If the Class cannot Init don't register it and Release
  if (aClass.Init)
  then begin
         newData.Inst:= aClass;
         Result:= (Add(aName, newData) > -1);
       end
  else aClass.Release;
end;
(*
function TDigIt_Sources.UnRegister(const aName: String): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rSourcesList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Sources.UnRegister(const aClass: IDigIt_Source): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aClass);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rSourcesList, r, 1);
    Result:= True;
  end;
end;
*)

function TDigIt_Sources.FreeElement(var aData: TSourceInfo): Boolean;
begin
  Result:= False;
  try
     if (aData.Inst <> nil)
     then aData.Inst.Release;

     Result:= True;

  finally
    //MaxM: When the open array is freed the compiler frees the contents of the record using rtti,
    //      a very dangerous thing for us.
    FillChar(aData, SizeOf(aData), 0);
  end;
end;

function TDigIt_Sources.CompData(aData1, aData2: TSourceInfo): Integer;
begin
  if (aData1.Inst = aData2.Inst)
  then Result:= 0
  else Result:= -1;
end;

constructor TDigIt_Sources.Create;
begin
  inherited Create;

  rSelected:= nil;
  rSelectedName:= '';
  rSelectedIndex:= -1;
  rSelectedParams:= nil;
end;

function TDigIt_Sources.Get(SourceName: String; CreateParams, UserParams: Boolean;
                            out ASource: PSourceInfo; var AParams: IDigIt_Params): Boolean;
var
   newSourceI: Integer;

begin
  Result:= False;

  if (SourceName <> '') then
  try
     newSourceI:= FindByKey(SourceName);
     if (newSourceI > -1) then Result:= Get(newSourceI, -1, CreateParams, UserParams, ASource, AParams);

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Get(SourceIndex, SourceSubIndex: Integer; CreateParams, UserParams: Boolean;
                            out ASource: PSourceInfo; var AParams: IDigIt_Params): Boolean;
begin
  Result:= False;

  try
     ASource:= Data[SourceIndex];

     if (ASource <> nil) then
     begin
       if CreateParams then
       begin
         AParams:= ASource^.Inst.Params_New;
         Result:= ASource^.Inst.Params_Set(AParams);
         if not(Result) and (AParams <> nil) then
         begin
           AParams.Release;
           AParams:= nil;
         end;
       end;

       if UserParams then
       begin
         if (ASource^.Inst is IDigIt_Source_Items)
         then begin
                Result:= (ASource^.Inst as IDigIt_Source_Items).Select(SourceSubIndex);
                if (AParams <> nil) then Result:= AParams.GetFromUser;
              end
         else if (AParams <> nil) then Result:= AParams.GetFromUser;
       end
       else Result:= True;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Get(aXML: TRttiXMLConfig; XMLRoot_Path: String; CreateParams: Boolean;
                            out ASource: PSourceInfo; var AParams: IDigIt_Params; out ASourceName: String): Boolean;
begin
  Result:= False;

  if (aXML <> nil) then
  try
     //Load a New Source and its Params
     ASourceName:= aXML.GetValue(XMLRoot_Path+'Source/Name', '');

     Result:= Get(ASourceName, CreateParams, False, ASource, AParams);
     if Result and (AParams <> nil)
     then Result:= AParams.Load(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(SourceName: String; UserParams: Boolean): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= Get(SourceName, True, UserParams, newSource, newParams);

  if Result then
  begin
    if (newParams <> nil) and not(UserParams) then Result:= newParams.Select;

    if Result then
    begin
      rSelected:= newSource;

      //Release old Params and replace with new one
      if (rSelectedParams <> nil) then rSelectedParams.Release;
      rSelectedParams:= newParams;

      rSelectedName:= SourceName;
      rSelectedIndex:= FindByKey(rSelectedName);
    end;
  end
  else if (newParams <> nil) then newParams.Release;
end;

function TDigIt_Sources.Select(SourceIndex, SourceSubIndex: Integer; UserParams: Boolean): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= Get(SourceIndex, SourceSubIndex, True, UserParams, newSource, newParams);

  if Result then
  begin
    if (newParams <> nil) and not(UserParams) then Result:= newParams.Select;

    if Result then
    begin
      rSelected:= newSource;

      //Release old Params and replace with new one
      if (rSelectedParams <> nil) then rSelectedParams.Release;
      rSelectedParams:= newParams;

      rSelectedName:= rList[SourceIndex].Key;
      rSelectedIndex:= SourceIndex;
    end;
  end
  else if (newParams <> nil) then newParams.Release;
end;

function TDigIt_Sources.Select(aXML: TRttiXMLConfig; XMLRoot_Path: String; out ASourceName: String): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= Get(aXML, XMLRoot_Path, True, newSource, newParams, ASourceName);

  if Result then
  begin
    if (newParams <> nil) then Result:= newParams.Select;

    if Result then
    begin
      rSelected:= newSource;

      //Release old Params and replace with new one
      if (rSelectedParams <> nil) then rSelectedParams.Release;
      rSelectedParams:= newParams;

      rSelectedName:= ASourceName;
      rSelectedIndex:= FindByKey(rSelectedName);
    end;
  end
  else if (newParams <> nil) then newParams.Release;
end;

function TDigIt_Sources.Select(ASelected: PSourceInfo; AParams: IDigIt_Params): Boolean;
var
   newIndex: Integer;

begin
  Result:= False;
  if (ASelected = nil) then exit;

  //Check if ASelected is really on our list
  newIndex:= Find(ASelected^);
  if (newIndex >= 0) and ASelected^.Inst.Params_Set(AParams) then
  begin
    Result:= True;
    if (AParams <> nil) then Result:= AParams.Select;

    if Result then
    begin
      rSelected:= ASelected;

      //Release old Params and replace with new one
      if (rSelectedParams <> nil) and (rSelectedParams <> AParams) then rSelectedParams.Release;
      rSelectedParams:= AParams;

      rSelectedIndex:= newIndex;
      rSelectedName:= rList[newIndex].Key;
    end;
  end;
end;

function TDigIt_Sources.Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean;
var
   curSource: PSourceInfo =nil;
   curParams: IDigIt_Params;

begin
  Result:= False;

  if (aXML <> nil) then
  try
     curSource:= Data[SourceIndex];

     if (curSource <> nil) then
     begin
       //Save ASource Source and its Params
       aXML.SetValue(XMLRoot_Path+'Source/Name', rList[SourceIndex].Key);
       aXML.DeletePath(XMLRoot_Path+'Source/Params/');

       if SaveParams and (curSource^.Inst <> nil) then
       begin
         curParams:= curSource^.Inst.Params;
         if (curParams <> nil) then curParams.Save(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));
       end;

       Result:= True;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Save(aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean;
begin
  Result:= False;

  if (aXML <> nil) then
  try
     //Save Selected Source and its Params
     aXML.SetValue(XMLRoot_Path+'Source/Name', rSelectedName);
     aXML.DeletePath(XMLRoot_Path+'Source/Params/');

     if SaveParams and
       (rSelected <> nil) and (rSelectedParams <> nil)
     then rSelectedParams.Save(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));

     Result:= True;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Take(takeAction: DigIt_Source_TakeAction; var AFiles: TSourceFileArray; AStartIndex: Integer;
                             ASource: PSourceInfo=nil): DWord;
var
   curData: Pointer;
   curDataType: TDigItDataType;
   oldLength, i: Integer;
   curImageFile: PChar;

begin
  Result:= 0;
  if (ASource = nil) then ASource:= rSelected;

  if (ASource <> nil) and (ASource^.Inst <> nil) then
  begin
    curData:= nil;

    Result:= ASource^.Inst.Take(takeAction, curDataType, curData);
    if (curData <> nil) then
    begin
      //Add files to end of Array AFiles
      oldLength:= Length(AFiles);
      Case curDataType of
        diDataType_FileName: begin
          if (AStartIndex < 0) or (AStartIndex > oldLength) then AStartIndex:= oldLength;

          //Add more space, if needed, to end of Array
          if (AStartIndex+1 > oldLength) then SetLength(AFiles, AStartIndex+1);

          AFiles[AStartIndex].fName:= PChar(curData);
          StrDispose(PChar(curData));
          Result:= 1;
        end;
        diDataType_FileNameArray: begin
          Result:= IDigIt_ArrayR_PChars(curData).GetCount;
          if (Result > 0) then
          begin
            if (AStartIndex < 0) or (AStartIndex > oldLength) then AStartIndex:= oldLength;

            //Add more space, if needed, to end of Array
            if (AStartIndex+Result > oldLength) then SetLength(AFiles, AStartIndex+Result);

            for i:=0 to Result-1 do
              if IDigIt_ArrayR_PChars(curData).Get(i, curImageFile) then
              begin
                AFiles[AStartIndex+i].fName:= curImageFile;
                StrDispose(curImageFile);
              end;
          end;
        end;
        else Result:= 0;
      end;
   end;
  end;
end;

function TDigIt_Sources.Take(takeAction: DigIt_Source_TakeAction; var AFileName: String;
                             ASource: PSourceInfo=nil): Boolean;
var
   curData: Pointer;
   curDataType: TDigItDataType;
   res: Integer;
   curImageFile: PChar;

begin
  Result:= False;
  if (ASource = nil) then ASource:= rSelected;

  if (ASource <> nil) and (ASource^.Inst <> nil) then
  begin
    curData:= nil;

    res:= ASource^.Inst.Take(takeAction, curDataType, curData);
    if (res > 0) and (curData <> nil) then
    begin
      Case curDataType of
        diDataType_FileName: begin
          AFileName:= PChar(curData);
          StrDispose(PChar(curData));
          Result:= True;
        end;
        diDataType_FileNameArray: begin
          if IDigIt_ArrayR_PChars(curData).Get(0, curImageFile)
          then begin
                 AFileName:= curImageFile;
                 StrDispose(curImageFile);
               end
          else AFileName:= '';
          Result:= True;
        end;
      end;
    end;
  end;
end;

function TDigIt_Sources.GetTitle(const ASource: PSourceInfo; const AParams: IDigIt_Params; const AddSourceName: Boolean): String;
var
   newProfileTitleP: PChar;
   res: Integer;

begin
  Result:= '';
  if (ASource <> nil) then
  try
  try
     //First tell to Params a Description then to Source
     newProfileTitleP:= ''; res:= 0;
     if (AParams <> nil) then
     begin
       res:= AParams.Summary(newProfileTitleP);
       if (res >0 ) and (newProfileTitleP <> '') then
       begin
         Result:= newProfileTitleP;
         StrDispose(newProfileTitleP);
         newProfileTitleP:= '';
       end;
     end;

     if AddSourceName then
     begin
       res:= ASource^.Inst.UI_Title(newProfileTitleP);
       if (res > 0) and (newProfileTitleP <> '') then
       begin
         if (Result = '')
         then Result:= newProfileTitleP
         else Result:= Result+' - '+newProfileTitleP;

         StrDispose(newProfileTitleP);
         newProfileTitleP:= '';
       end;
     end;

  except
    Result:= '';
  end;
  finally
    if (newProfileTitleP <> '') then StrDispose(newProfileTitleP);
  end;
end;

end.

