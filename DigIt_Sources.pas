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

  public
    //IDigIt_Sources implementation
    function Register(const aName: PChar; const aClass: IDigIt_Source): Boolean; stdcall;

    constructor Create;

    function Get(var ASource: PSourceInfo; var AParams: IDigIt_Params;
                 SourceName: String; GetUserParams: Boolean=False): Boolean; overload;
    function Get(var ASource: PSourceInfo; var AParams: IDigIt_Params;
                 SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;
    function Get(var ASource: PSourceInfo; var AParams: IDigIt_Params; var ASourceName: String;
                 aXML: TRttiXMLConfig; XMLRoot_Path: String): Boolean; overload;

    function Select(SourceName: String; GetUserParams: Boolean=False): Boolean; overload;
    function Select(SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;
    function Select(out ASourceName: String; aXML: TRttiXMLConfig; XMLRoot_Path: String): Boolean; overload;

    function Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;
    function Save(aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;

    function Take(takeAction: DigIt_Source_TakeAction; var AFiles: TSourceFileArray; AStartIndex: Integer;
                  ASource: PSourceInfo=nil): DWord; overload;
    function Take(takeAction: DigIt_Source_TakeAction; var AFileName: String;
                  ASource: PSourceInfo=nil): Boolean; overload;

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

constructor TDigIt_Sources.Create;
begin
  inherited Create;

  rSelected:= nil;
  rSelectedName:= '';
//oldcode  rSelectedIndex:= -1;
  rSelectedParams:= nil;
end;

function TDigIt_Sources.Get(var ASource: PSourceInfo; var AParams: IDigIt_Params;
                            SourceName: String; GetUserParams: Boolean): Boolean;
var
   newSourceI: Integer;

begin
  Result:= False;

  if (SourceName <> '') then
  try
     newSourceI:= FindByKey(SourceName);
     if (newSourceI > -1) then Result:= Get(ASource, AParams, newSourceI, -1, GetUserParams);

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Get(var ASource: PSourceInfo; var AParams: IDigIt_Params;
                            SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= False;

  try
     newSource:= Data[SourceIndex];

     if (newSource <> nil) then
     begin
       newParams:= newSource^.Inst.Params;

       if GetUserParams then
       begin
         if (newParams = nil) then
         begin
           newParams:= newSource^.Inst.Params_New;
           Result:= newSource^.Inst.Params_Set(newParams);
           if not(Result) and (newParams <> nil) then
           begin
             newParams.Release;
             newParams:= nil;
           end;
         end;

         if (newSource^.Inst is IDigIt_Source_Items)
         then begin
                Result:= (newSource^.Inst as IDigIt_Source_Items).Select(newSourceSubIndex);
                if (newParams <> nil) then Result:= newParams.GetFromUser;
              end
         else if (newParams <> nil) then Result:= newParams.Select and newParams.GetFromUser;
       end
       else Result:= True;

       if Result then
       begin
         ASource:= newSource;
         AParams:= newParams;
       end;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Get(var ASource: PSourceInfo; var AParams: IDigIt_Params; var ASourceName: String;
                            aXML: TRttiXMLConfig; XMLRoot_Path: String): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= False;

  if (aXML <> nil) then
  try
     //Load a New Source and its Params
     ASourceName:= aXML.GetValue(XMLRoot_Path+'Source/Name', '');

     Result:= Get(newSource, newParams, ASourceName);
     if Result then
     begin
        if (newParams = nil) then
        begin
          newParams:= newSource^.Inst.Params_New;
          Result:= newSource^.Inst.Params_Set(newParams);
          if not(Result) and (newParams <> nil) then
          begin
            newParams.Release;
            newParams:= nil;
          end;
        end;

        if (newParams <> nil) then
        begin
          Result:= newParams.Load(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));
//          Result:= newParams.Select;
        end;

        if Result then
        begin
          ASource:= newSource;
          AParams:= newParams;
        end;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(SourceName: String; GetUserParams: Boolean): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= Get(newSource, newParams, SourceName, GetUserParams);

  if Result then
  begin
    if (newParams <> nil) and not(GetUserParams) then Result:= newParams.Select;
    if Result then
    begin
      rSelected:= newSource;
      rSelectedParams:= newParams;
      rSelectedName:= SourceName;
      rSelectedIndex:= FindByKey(rSelectedName);
    end;
  end;
end;

function TDigIt_Sources.Select(SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;

begin
  Result:= Get(newSource, newParams, SourceIndex, newSourceSubIndex, GetUserParams);

  if Result then
  begin
    if (newParams <> nil) and not(GetUserParams) then Result:= newParams.Select;
    if Result then
    begin
      rSelected:= newSource;
      rSelectedParams:= newParams;
      rSelectedName:= rList[SourceIndex].Key;
      rSelectedIndex:= SourceIndex;
    end;
  end;
end;

function TDigIt_Sources.Select(out ASourceName: String; aXML: TRttiXMLConfig; XMLRoot_Path: String): Boolean;
var
   newSource: PSourceInfo=nil;
   newParams: IDigIt_Params=nil;
   newSourceName: String;

begin
  Result:= Get(newSource, newParams, newSourceName, aXML, XMLRoot_Path);
  ASourceName:= newSourceName;

  if Result then
  begin
    if (newParams <> nil) then Result:= newParams.Select;
    if Result then
    begin
      rSelected:= newSource;
      rSelectedParams:= newParams;
      rSelectedName:= newSourceName;
      rSelectedIndex:= FindByKey(rSelectedName);
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

end.

