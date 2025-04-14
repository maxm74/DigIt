unit DigIt_Counters;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, Contnrs, Laz2_XMLCfg;

type
    { TDigIt_Counter }
    TDigIt_Counter = class(TPersistent)
    private
      rName: String;
      MinValue,
      rValue,
      rUserData,
      rValue_Previous: Integer;
      rValue_StringPost,
      rValue_StringPre,
      formatString: String;
      rValue_StringDigits: Byte;

      function GetValueNext: Integer;
      procedure SetValue(AValue: Integer);
      procedure SetValueNext(AValue: Integer);
      procedure SetValue_StringDigits(AValue: Byte);

    public
      constructor Create(AName:String; AMinValue:Integer=0); overload;

      procedure Reset;

      function GetValue(nextValue: Boolean=False): String; overload;
      function GetValue(AValue: Integer): String; overload;

      procedure Load(const XMLConf: TRttiXMLConfig; APath: String; LoadValues:Boolean);
      procedure Save(const XMLConf: TRttiXMLConfig; APath: String; SaveValues:Boolean);

    published
      property Name: String read rName write rName;
      property Value: Integer read rValue write SetValue;
      property Value_Next: Integer read GetValueNext write SetValueNext;
      property Value_Previous: Integer read rValue_Previous write rValue_Previous;
      property Value_StringDigits: Byte read rValue_StringDigits write SetValue_StringDigits;
      property Value_StringPre: String read rValue_StringPre write rValue_StringPre;
      property Value_StringPost: String read rValue_StringPost write rValue_StringPost;
      property UserData: Integer read rUserData write rUserData;
    end;

implementation

{ TDigIt_Counter }

function TDigIt_Counter.GetValueNext: Integer;
begin
  Result:= rValue+1;
end;

procedure TDigIt_Counter.SetValue(AValue: Integer);
begin
  if (rValue <> AValue) then
  begin
    rValue_Previous:= rValue;

    if (AValue < MinValue)
    then rValue:= MinValue
    else rValue:= AValue;
  end;
end;

procedure TDigIt_Counter.SetValueNext(AValue: Integer);
begin
  if (AValue > 0)
  then rValue:= AValue-1
  else rValue:= 0;
end;

procedure TDigIt_Counter.SetValue_StringDigits(AValue: Byte);
begin
  if (rValue_StringDigits <> AValue) then
  begin
    rValue_StringDigits:= AValue;
    formatString:= '%s%.'+IntToStr(AValue)+'d%s';
  end;
end;

constructor TDigIt_Counter.Create(AName: String; AMinValue: Integer);
begin
  inherited Create;

  rName:= AName;
  MinValue:= AMinValue;
  rValue:= AMinValue;
  rValue_Previous:= rValue;
  rValue_StringDigits:= 3;
  formatString:= '%s%.3d%s';
end;

procedure TDigIt_Counter.Reset;
begin
  rValue:= MinValue;
end;

function TDigIt_Counter.GetValue(nextValue: Boolean): String;
begin
  if nextValue
  then Result := Format(formatString, [rValue_StringPre, GetValueNext, rValue_StringPost])
  else Result := Format(formatString, [rValue_StringPre, rValue, rValue_StringPost]);
end;

function TDigIt_Counter.GetValue(AValue: Integer): String;
begin
  Result := Format(formatString, [rValue_StringPre, AValue, rValue_StringPost]);
end;

procedure TDigIt_Counter.Load(const XMLConf: TRttiXMLConfig; APath: String; LoadValues: Boolean);
begin
  //Loading:=True;
  Name :=XMLConf.GetValue(APath+'Name', '');
  if LoadValues
  then begin
         Value:=XMLConf.GetValue(APath+'Value', 0);
         Value_Previous:=XMLConf.GetValue(APath+'Value_Previous', 0);
       end
  else begin
         Value:= 0;
         Value_Previous:= 0;
       end;
  Value_StringPre:=XMLConf.GetValue(APath+'Value_StringPre', '');
  Value_StringPost:=XMLConf.GetValue(APath+'Value_StringPost', '');
  Value_StringDigits:=XMLConf.GetValue(APath+'Value_StringDigits', 3);
  UserData:=XMLConf.GetValue(APath+'UserData', -1);
  //Loading:=False;
end;

procedure TDigIt_Counter.Save(const XMLConf: TRttiXMLConfig; APath: String; SaveValues: Boolean);
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

end.

