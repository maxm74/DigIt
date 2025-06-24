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
  DigIt_Types, Digit_Bridge_Intf;

type
  { TDigIt_Sources }

  TSourceInfo = record
    Flags: DWord;
    Inst: IDigIt_Source;
  end;
  PSourceInfo = ^TSourceInfo;

  TDigIt_Sources = class(specialize TOpenArrayList<TSourceInfo, String>, IDigIt_Sources)
    function Register(const aName: PChar; const aClass: IDigIt_Source): Boolean; stdcall;
  protected
    rSelected: PSourceInfo;
    rSelectedName: String;
    rSelectedIndex: Integer;
    rSelectedParams: IDigIt_Params;

    function FreeElement(var aData: TSourceInfo): Boolean; override;

  public
    constructor Create;

    function Select(SourceName: String; GetUserParams: Boolean=False): Boolean; overload;
    function Select(SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;
    function Select(aXML: TRttiXMLConfig; XMLRoot_Path: String; var newSourceName: String): Boolean; overload;

    function Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;
    function Save(aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean; overload;
    //function LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;

    function Take(takeAction: DigIt_Source_TakeAction;
                  var AFiles: TSourceFileArray; AStartIndex: Integer): DWord; overload;
    function Take(takeAction: DigIt_Source_TakeAction; var AFileName: String): Boolean; overload;

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
  rSelectedIndex:= -1;
  rSelectedParams:= nil;
end;

function TDigIt_Sources.Select(SourceName: String; GetUserParams: Boolean): Boolean;
var
   newSourceI: Integer;
   newSource: PSourceInfo =nil;

begin
  Result:= False;

  if (SourceName <> '') then
  try
     newSourceI:= FindByKey(SourceName);
     if (newSourceI > -1) then Result:= Select(newSourceI, -1, GetUserParams);

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(SourceIndex, newSourceSubIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newSource: PSourceInfo =nil;
   curSourceItems: IDigIt_Source_Items;

begin
  Result:= False;

  try
     newSource:= Data[SourceIndex];

     if (newSource <> nil) then
     begin
       if GetUserParams then
       begin
         if (newSource^.Inst is IDigIt_Source_Items)
         then Result:= (newSource^.Inst as IDigIt_Source_Items).Select(newSourceSubIndex)
         else Result:= True;

         if Result then Result:= (newSource^.Inst.Params <> nil) and newSource^.Inst.Params.GetFromUser;
       end
       else Result:= True;

       if Result then
       begin
         rSelected:= newSource;
         rSelectedParams :=newSource^.Inst.Params;
         rSelectedName:= rList[SourceIndex].Key;
         rSelectedIndex:= SourceIndex;
       end;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Select(aXML: TRttiXMLConfig; XMLRoot_Path: String; var newSourceName: String): Boolean;
begin
  Result:= False;

  if (aXML <> nil) then
  try
     //Load a New Source and its Params
     newSourceName:= aXML.GetValue(XMLRoot_Path+'Source/Name', '');

     if Select(newSourceName) then
     begin
        Result:= True;

        if (rSelectedParams <> nil) then
        begin
          Result:= rSelectedParams.Load(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));
          if Result then Result:= rSelectedParams.OnSet;
        end;
     end;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Save(SourceIndex: Integer; aXML: TRttiXMLConfig; XMLRoot_Path: String; SaveParams: Boolean): Boolean;
var
   curSource: PSourceInfo =nil;

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

       if SaveParams and (curSource^.Inst <> nil)
       then curSource^.Inst.Params.Save(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));

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
       (rSelected <> nil) and (rSelected^.Inst <> nil)
     then rSelected^.Inst.Params.Save(PChar(aXML.Filename), PChar(XMLRoot_Path+'Source/Params'));

     Result:= True;

  except
    Result:= False;
  end;
end;

function TDigIt_Sources.Take(takeAction: DigIt_Source_TakeAction;
                             var AFiles: TSourceFileArray; AStartIndex: Integer): DWord;
var
   curData: Pointer;
   curDataType: TDigItDataType;
   oldLength, i: Integer;
   curImageFile: PChar;

begin
  Result:= 0;
  if (rSelected <> nil) and (rSelected^.Inst <> nil) then
  begin
    curData:= nil;

    Result:= rSelected^.Inst.Take(takeActTake, curDataType, curData);
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

function TDigIt_Sources.Take(takeAction: DigIt_Source_TakeAction; var AFileName: String): Boolean;
var
   curData: Pointer;
   curDataType: TDigItDataType;
   res: Integer;
   curImageFile: PChar;

begin
  Result:= False;
  if (rSelected <> nil) and (rSelected^.Inst <> nil) then
  begin
    curData:= nil;

    res:= rSelected^.Inst.Take(takeAction, curDataType, curData);
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

end.

