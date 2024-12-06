unit DigIt_Counters;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Contnrs, Laz2_XMLCfg;

type
    { TDigIt_Counter }
    TDigIt_Counter = class(TPersistent)
    private
      rName: String;
      rValue,
      rUserData,
      rValue_Previous: Integer;
      rValue_StringPost: String;
      rValue_StringPre: String;
      rValue_StringDigits: Byte;

      function GetValueNext: Integer;
      procedure SetValueNext(AValue: Integer);

    public
      constructor Create(AName:String; AValue:Integer=-1); overload;
      function GetValue(nextValue: Boolean=False): String;

      procedure Load(const XMLConf: TXMLConfig; APath: String; LoadValues:Boolean);
      procedure Save(const XMLConf: TXMLConfig; APath: String; SaveValues:Boolean);

    published
      property Name: String read rName write rName;
      property Value: Integer read rValue write rValue;
      property Value_Next: Integer read GetValueNext write SetValueNext;
      property Value_Previous: Integer read rValue_Previous write rValue_Previous;
      property Value_StringDigits: Byte read rValue_StringDigits write rValue_StringDigits;
      property Value_StringPre: String read rValue_StringPre write rValue_StringPre;
      property Value_StringPost: String read rValue_StringPost write rValue_StringPost;
      property UserData: Integer read rUserData write rUserData;
    end;

    { TDigIt_CounterList }
    TDigIt_CounterList = class;

    TCounterLoadEvent = function (AOwner: TDigIt_CounterList; Counter: TDigIt_Counter;
                                 const XMLConf: TXMLConfig; const Path:String):Integer of object;
    TCounterSaveEvent = procedure (AOwner: TDigIt_CounterList; Counter: TDigIt_Counter;
                                 const XMLConf: TXMLConfig; const Path:String) of object;

    TDigIt_CounterList = class(TObjectList)
    protected
      rName: String;
      rOnCounterLoad: TCounterLoadEvent;
      rOnCounterSave: TCounterSaveEvent;

      function getCounter(aIndex: Integer): TDigIt_Counter;
      procedure setCounter(aIndex: Integer; const AValue: TDigIt_Counter);

      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      constructor Create(AName:String; AOwnsObjects:Boolean=True);

      function add(aCounter: TDigIt_Counter): integer; overload;
      function add(AName:String; AValue:Integer=-1): integer; overload;

      //Remove all Items except the First
      procedure RemoveAllButFirst;

      //Create List From File/Stream/XML
      procedure Load(const XMLConf: TXMLConfig; LoadValues:Boolean);
      procedure LoadFromStream(Stream: TStream; LoadValues:Boolean);
      procedure LoadFromFile(const FileName: string; LoadValues:Boolean);

      //Save List To File/Stream/XML
      procedure Save(const XMLConf: TXMLConfig; SaveValues:Boolean);
      procedure SaveToStream(Stream: TStream; SaveValues:Boolean);
      procedure SaveToFile(const FileName: string; SaveValues:Boolean);

      //Load/Save only Values
      procedure LoadValues(const XMLConf: TXMLConfig);
      procedure SaveValues(const XMLConf: TXMLConfig);

      procedure CopyValues(CopyFrom:TDigIt_CounterList);
      procedure CopyValuesToPrevious;
      procedure CopyPreviousToValues;

      property items[aIndex: integer] : TDigIt_Counter read getCounter write setCounter; default;
      property Name:String read rName write rName;

      //Events
      //property OnCropAreaAdded:TCropAreaEvent read rOnCropAreaAdded write rOnCropAreaAdded;
      //property OnCropAreaDeleted:TCropAreaEvent read rOnCropAreaDeleted write rOnCropAreaDeleted;
      //property OnCropAreaChanged:TCropAreaEvent read rOnCropAreaChanged write rOnCropAreaChanged;
      property OnCropAreaLoad:TCounterLoadEvent read rOnCounterLoad write rOnCounterLoad;
      property OnCropAreaSave:TCounterSaveEvent read rOnCounterSave write rOnCounterSave;
   end;

implementation

{ TDigIt_Counter }

function TDigIt_Counter.GetValueNext: Integer;
begin
  Result:= rValue+1;
end;

procedure TDigIt_Counter.SetValueNext(AValue: Integer);
begin
  if (AValue >= 0)
  then rValue:= AValue-1
  else rValue:= -1;
end;

constructor TDigIt_Counter.Create(AName: String; AValue: Integer);
begin
  inherited Create;
  rName :=AName;
  rValue :=AValue;
  rValue_Previous:=AValue;
  rValue_StringDigits:=2;
end;

function TDigIt_Counter.GetValue(nextValue: Boolean): String;
begin
  if nextValue
  then Result :=rValue_StringPre+Format('%.'+IntToStr(rValue_StringDigits)+'d', [GetValueNext])+rValue_StringPost
  else Result :=rValue_StringPre+Format('%.'+IntToStr(rValue_StringDigits)+'d', [rValue])+rValue_StringPost;
end;

procedure TDigIt_Counter.Load(const XMLConf: TXMLConfig; APath: String; LoadValues: Boolean);
begin
  //Loading:=True;
  Name :=XMLConf.GetValue(APath+'Name', '');
  if LoadValues
  then begin
         Value:=XMLConf.GetValue(APath+'Value', -1);
         Value_Previous:=XMLConf.GetValue(APath+'Value_Previous', -1);
       end
  else begin
         Value:=-1;
         Value_Previous:=-1;
       end;
  Value_StringPre:=XMLConf.GetValue(APath+'Value_StringPre', '');
  Value_StringPost:=XMLConf.GetValue(APath+'Value_StringPost', '');
  Value_StringDigits:=XMLConf.GetValue(APath+'Value_StringDigits', 2);
  UserData:=XMLConf.GetValue(APath+'UserData', -1);
  //Loading:=False;
end;

procedure TDigIt_Counter.Save(const XMLConf: TXMLConfig; APath: String; SaveValues: Boolean);
begin
  XMLConf.SetValue(APath+'Name', Name);
  if SaveValues
  then begin
         XMLConf.SetValue(APath+'Value', Value);
         XMLConf.SetValue(APath+'Value_Previous', Value_Previous);
       end
  else begin
         XMLConf.DeleteValue(APath+'Value');
         XMLConf.DeleteValue(APath+'Value_Previous');
       end;
  XMLConf.SetValue(APath+'Value_StringPre', Value_StringPre);
  XMLConf.SetValue(APath+'Value_StringPost', Value_StringPost);
  XMLConf.SetValue(APath+'Value_StringDigits', Value_StringDigits);
  XMLConf.SetValue(APath+'UserData', UserData);
end;

{ TDigIt_CounterList }

procedure TDigIt_CounterList.setCounter(aIndex: Integer; const AValue: TDigIt_Counter);
begin
  inherited Items[aIndex] := AValue;
end;

function TDigIt_CounterList.getCounter(aIndex: Integer): TDigIt_Counter;
begin
  Result := inherited Items[aIndex] as TDigIt_Counter;
end;

procedure TDigIt_CounterList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
end;

constructor TDigIt_CounterList.Create(AName: String; AOwnsObjects: Boolean);
begin
  inherited Create;
  Self.OwnsObjects:=AOwnsObjects;
  rName:=AName;
end;

function TDigIt_CounterList.add(aCounter: TDigIt_Counter): integer;
begin
  Result := inherited Add(aCounter);
end;

function TDigIt_CounterList.add(AName: String; AValue: Integer): integer;
var
   newCounter :TDigIt_Counter;

begin
  newCounter:= TDigIt_Counter.Create(AName, AValue);
  Result := inherited Add(newCounter);
end;

procedure TDigIt_CounterList.RemoveAllButFirst;
var
   i, c: Integer;

begin
  c:= Count;
  if (c > 1)
  then for i:=1 to c-1 do Delete(1);
end;

procedure TDigIt_CounterList.Load(const XMLConf: TXMLConfig; LoadValues: Boolean);
var
  i, newCount: integer;
  curItemPath, curPath: String;
  newCounter: TDigIt_Counter;

begin
  try
    curPath :=Self.Name+'/';
    newCount := XMLConf.GetValue(curPath+'Count', -1);
    if (newCount=-1)
    then raise Exception.Create('XML Path not Found');

    Clear;

    for i :=0 to newCount-1 do
    begin
      curItemPath :=curPath+'Item' + IntToStr(i)+'/';

      newCounter :=TDigIt_Counter.Create;
      newCounter.Load(XMLConf, curItemPath, LoadValues);

      if assigned(rOnCounterLoad)
      then newCounter.UserData :=rOnCounterLoad(Self, newCounter, XMLConf, curItemPath);

      add(newCounter);
    end;

   finally
   end;
end;

procedure TDigIt_CounterList.LoadFromStream(Stream: TStream; LoadValues: Boolean);
var
   FXMLConf: TXMLConfig;

begin
  try
    FXMLConf := TXMLConfig.Create(nil);
    FXMLConf.ReadFromStream(Stream);
    Load(FXMLConf, LoadValues);
  finally
    FXMLConf.Free;
  end;
end;

procedure TDigIt_CounterList.LoadFromFile(const FileName: string; LoadValues: Boolean);
var
   FXMLConf: TXMLConfig;

begin
  try
     FXMLConf := TXMLConfig.Create(FileName);
     Load(FXMLConf, LoadValues);
  finally
     FXMLConf.Free;
  end;
end;

procedure TDigIt_CounterList.Save(const XMLConf: TXMLConfig; SaveValues: Boolean);
var
  i: integer;
  curItemPath, curPath: String;
  curCounter: TDigIt_Counter;

begin
  curPath :=Self.Name+'/';
  XMLConf.DeletePath(curPath);
  XMLConf.SetValue(curPath+'Count', Count);

  for i :=0 to Count-1 do
  begin
    curItemPath :=curPath+'Item' + IntToStr(i)+'/';

    curCounter :=Items[i];
    curCounter.Save(XMLConf, curItemPath, SaveValues);

    if assigned(rOnCounterSave)
    then rOnCounterSave(Self, curCounter, XMLConf, curItemPath);
  end;
end;

procedure TDigIt_CounterList.SaveToStream(Stream: TStream; SaveValues: Boolean);
var
  FXMLConf: TXMLConfig;

begin
  try
    FXMLConf := TXMLConfig.Create(nil);
    Save(FXMLConf, SaveValues);
    FXMLConf.WriteToStream(Stream);
  finally
    FXMLConf.Free;
  end;
end;

procedure TDigIt_CounterList.SaveToFile(const FileName: string; SaveValues: Boolean);
var
  FXMLConf: TXMLConfig;

begin
  try
    FXMLConf := TXMLConfig.Create(FileName);
    Save(FXMLConf, SaveValues);
    FXMLConf.Flush;
  finally
    FXMLConf.Free;
  end;
end;

procedure TDigIt_CounterList.LoadValues(const XMLConf: TXMLConfig);
var
  i, newCount: integer;
  curItemPath, curPath: String;
  curCounter: TDigIt_Counter;

begin
  try
    curPath :=Self.Name+'/';
    newCount := XMLConf.GetValue(curPath+'Count', -1);
    if (newCount=-1)
    then raise Exception.Create('XML Path not Found');

    for i :=0 to newCount-1 do
    begin
      curItemPath :=curPath+'Item' + IntToStr(i)+'/';

      curCounter :=Items[i];
      curCounter.Value:=XMLConf.GetValue(curItemPath+'Value', -1);
      curCounter.Value_Previous:=XMLConf.GetValue(curItemPath+'Value_Previous', -1);;
    end;

   finally
   end;
end;

procedure TDigIt_CounterList.SaveValues(const XMLConf: TXMLConfig);
var
  i: integer;
  curItemPath, curPath: String;
  curCounter: TDigIt_Counter;

begin
  curPath :=Self.Name+'/';
  XMLConf.SetValue(curPath+'Count', Count);

  for i :=0 to Count-1 do
  begin
    curItemPath :=curPath+'Item' + IntToStr(i)+'/';

    curCounter :=Items[i];
    XMLConf.SetValue(curItemPath+'Value', curCounter.Value);
    XMLConf.SetValue(curItemPath+'Value_Previous', curCounter.Value_Previous);
  end;
end;

procedure TDigIt_CounterList.CopyValues(CopyFrom: TDigIt_CounterList);
var
  i: integer;
  curCounter: TDigIt_Counter;

begin
  if (CopyFrom.Count<>Self.Count)
  then raise Exception.Create(Self.Name+' Count('+IntToStr(Self.Count)+') <> '+CopyFrom.Name+' Count('+IntToStr(CopyFrom.Count)+')');

  for i :=0 to CopyFrom.Count-1 do
  begin
    curCounter :=Items[i];

    if (Self.items[i].Name<>CopyFrom.items[i].Name)
    then raise Exception.Create(Self.Name+' Counter('+Self.items[i].Name+') <> '+CopyFrom.Name+' Counter('+CopyFrom.items[i].Name+')');

    Self.items[i].Value:=curCounter.Value;
  end;
end;

procedure TDigIt_CounterList.CopyValuesToPrevious;
var
  i: integer;
  curCounter: TDigIt_Counter;

begin
  for i :=0 to Count-1 do
  begin
    curCounter :=Items[i];
    curCounter.Value_Previous:=curCounter.Value;
  end;
end;

procedure TDigIt_CounterList.CopyPreviousToValues;
var
  i: integer;
  curCounter: TDigIt_Counter;

begin
  for i :=0 to Count-1 do
  begin
    curCounter :=Items[i];
    curCounter.Value:=curCounter.Value_Previous;
  end;
end;

end.

