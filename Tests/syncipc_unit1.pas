unit syncipc_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  simpleipc, syncipc, Twain, DelphiTwain_VCL, Digit_Taker_Twain_Types;

type
  { TTestSyncIPCServer }

  TTestSyncIPCServer = class(TSyncIPCServer)
  protected
    rTwain:TDelphiTwain;
    rTwain_SourceI:Integer;
    function TWAIN32_LIST:Boolean;
    function getTwain: TDelphiTwain;
    procedure FreeTwain;


    function MessageReceived(AMsgID:Integer):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AInteger:Integer; IntegerSize:Byte):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const APointer:Pointer; Count: LongInt):Boolean; override; overload;

    property Twain: TDelphiTwain read getTwain;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btInt1: TButton;
    btString: TButton;
    btRectVar: TButton;
    btInt: TButton;
    btStream: TButton;
    btPRect: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    lbServer: TLabel;
    lbClient: TLabel;
    Memo2: TMemo;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure btInt1Click(Sender: TObject);
    procedure btStringClick(Sender: TObject);
    procedure btRectVarClick(Sender: TObject);
    procedure btIntClick(Sender: TObject);
    procedure btStreamClick(Sender: TObject);
    procedure btPRectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    CommsServer:TTestSyncIPCServer;
    CommsClient:TSyncIPCClient;

  public
  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}


{ TTestSyncIPCServer }

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer): Boolean;
var
   resBuf:array of TRect;

begin
    Form1.Memo2.Lines.Add('MessageReceived (mtSync_Null) : '+IntToStr(AMsgID));

    Case AMsgID of
      7: begin
            SetLength(resBuf, 2);
            resBuf[0].Left:=1;
            resBuf[0].Top:=100;
            resBuf[0].Right:=1000;
            resBuf[0].Bottom:=10000;
            resBuf[1].Left:=2;
            resBuf[1].Top:=200;
            resBuf[1].Right:=2000;
            resBuf[1].Bottom:=20000;
            Result :=MessageResult(Pointer(resBuf), 2*sizeof(TRect));
            SetLength(resBuf, 0);
    end;

    MSG_TWAIN32_LIST : Result:=TWAIN32_LIST;

    end;
end;

function TTestSyncIPCServer.TWAIN32_LIST: Boolean;
var
  i, listCount: Integer;
  theList:array of TW_IDENTITY;

begin
//  Twain.SourceManagerLoaded :=True;
  listCount :=Twain.SourceCount; //This LoadLibrary+SourceManager+Enumerate the devices
  Result :=(listCount>0);
  if Result then
  begin
    SetLength(theList, listCount);
    for i:=0 to listCount-1 do
       theList[i] :=Twain.Source[i].SourceIdentity^;

    //Copy theList on result Stream
    { #todo 10 : Test if Work, else write directly to ResultStream }
    Result:=MessageResult(Pointer(theList), listCount*Sizeof(TW_IDENTITY));
    SetLength(theList, 0); //we can free it, is already on the ResultStream
  end;
end;

function TTestSyncIPCServer.getTwain: TDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TDelphiTwain.Create;
   // rTwain.OnTwainAcquire := @TwainTwainAcquire;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
  end;

  Result :=rTwain;
end;

procedure TTestSyncIPCServer.FreeTwain;
begin
  if (rTwain<>nil)
  then rTwain.Free;
end;

constructor TTestSyncIPCServer.Create(AOwner: TComponent);
begin
  rTwain:=nil;
  inherited Create(AOwner);
end;

destructor TTestSyncIPCServer.Destroy;
begin
  FreeTwain;
  inherited Destroy;
end;




function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; AInteger: Integer; IntegerSize:Byte): Boolean;
var
   resRect:TRect;

begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Integer) : '+IntToStr(AMsgID)+'-'+IntToHex(AInteger)+' ('+IntToStr(IntegerSize)+')');

  Case AMsgID of
  3: Result :=MessageResult($ABCDEF0);
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Stream) : '+IntToStr(AMsgID)+' ('+IntToStr(AStream.Size)+')');

  Case AMsgID of
  4: begin
        AStream.WriteAnsiString('Reply to SyncMessage 4 as Stream');
        Result :=MessageResult(AStream);
  end;
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_String) : '+IntToStr(AMsgID)+'-'+Msg);

  Case AMsgID of
  1: Result :=MessageResult('Ciao son Sync Result for '+IntToStr(AMsgID));
  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
var
   resRect:TRect;

begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Var) : '+IntToStr(AMsgID)+' ('+IntToStr(Count)+')');

  Case AMsgID of
    2: begin
      resRect.Top:=TRect(Buffer).Top+33;
      resRect.Left:=TRect(Buffer).Left+66;
      resRect.Bottom:=TRect(Buffer).Bottom+100;
      resRect.Right:=TRect(Buffer).Right+200;
      Result :=MessageResult(resRect, sizeof(TRect));
  end;

  end;
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const APointer: Pointer; Count: LongInt): Boolean;
type PRect=^TRect;
begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Pointer) : '+IntToStr(AMsgID)+' ('+IntToStr(Count)+')');

  Case AMsgID of
    5: begin
      PRect(APointer)^.Top:=PRect(APointer)^.Top+33;
      PRect(APointer)^.Left:=PRect(APointer)^.Left+66;
      PRect(APointer)^.Bottom:=PRect(APointer)^.Bottom+100;
      PRect(APointer)^.Right:=PRect(APointer)^.Right+200;
      Result :=MessageResult(APointer, sizeof(TRect));
  end;

  end;
end;


{ TForm1 }


procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if CommsServer=nil then
  begin
    CommsServer  := TTestSyncIPCServer.Create(Nil);
    CommsServer.ServerID:=Edit2.Text {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
    CommsServer.StartServer(True);  // start listening, threaded
    if CommsServer.Active
    then MessageDlg('TestSyncIPC', 'Server Running', mtInformation, [mbOk], 0)
    else MessageDlg('TestSyncIPC', 'Server NOT Running', mtError, [mbOk], 0);

    lbServer.Visible:=CommsServer.Active;
  end;
end;

procedure TForm1.btStringClick(Sender: TObject);
Var
   recStr:String;
   resType:TMessageType;

begin
  Memo2.Lines.Add('SendSyncMessage 1 (mtSync_String):');
  resType :=CommsClient.SendSyncMessage(30000, 1, 'Ciao SyncMsg1', recStr);
  Memo2.Lines.Add('SendSyncMessage 1 Return ('+IntToStr(resType)+'):'+recStr);
end;

procedure TForm1.btInt1Click(Sender: TObject);
Var
   recSize, recBuf:Longint;
   resType:TMessageType;

begin
  Memo2.Lines.Add('SendSyncMessage STOP (mtSync_Null):');
  resType :=CommsClient.SendSyncMessage(30000, 101, mtSync_Null, recBuf, 0, recBuf, recSize);
  if (resType=mtSync_Integer) then
  begin
    Memo2.Lines.Add('SendSyncMessage STOP Return ('+IntToStr(resType)+'):'+IntToHex(recBuf)+'-'+IntToStr(recSize));
  end;
end;

procedure TForm1.btRectVarClick(Sender: TObject);
Var
   recBuf:TRect;
   recSize:Integer;
   resType:TMessageType;

begin
  recBuf.Top:=666;
  recBuf.Left:=999;
  recBuf.Bottom:=789;
  recBuf.Right:=456;
  recSize:=sizeof(TRect);
  Memo2.Lines.Add('SendSyncMessage 2 (mtSync_Var):'+#13#10+
        IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  resType :=CommsClient.SendSyncMessage(30000, 2, mtSync_Var, recBuf, recSize, recBuf, recSize);
  if (resType=mtSync_Var) then
  begin
    Memo2.Lines.Add('SendSyncMessage 2 Return ('+IntToStr(resType)+'):'+#13#10+
          IntToStr(recBuf.Top)+'-'+IntToStr(recBuf.Left)+'-'+IntToStr(recBuf.Bottom)+'-'+IntToStr(recBuf.Right));
  end;
end;

procedure TForm1.btIntClick(Sender: TObject);
Var
   recSize, recBuf, msg:Longint;
   resType:TMessageType;

begin
  msg:=$1BCDEF23;
  Memo2.Lines.Add('SendSyncMessage 3 (mtSync_Integer):'+IntToHex(msg));
  resType :=CommsClient.SendSyncMessage(30000, 3, mtSync_Integer, msg, 0, recBuf, recSize);
  if (resType=mtSync_Integer) then
  begin
    Memo2.Lines.Add('SendSyncMessage 3 Return ('+IntToStr(resType)+'):'+IntToHex(recBuf)+'-'+IntToStr(recSize));
  end;
end;

procedure TForm1.btStreamClick(Sender: TObject);
Var
   recSize:Integer;
   recBuf:TMemoryStream;
   res:TMemoryStream=nil;
   resType:TMessageType;
   retStr:String;

begin
  recBuf:=TMemoryStream.Create;
  recBuf.WriteAnsiString('SyncMessage 4 as Stream25');
  Memo2.Lines.Add('SendSyncMessage 4 (mtSync_Stream): "SyncMessage 4 as Stream25"');
  recSize:=recBuf.Size;
  (*  //Test with Result on a new Stream
  resType :=CommsClient.SendSyncMessage(30000, 4, mtSync_Stream, recBuf, 0, res, recSize);
  if (resType=mtSync_Stream) then
  begin
    res.Position:=0;
    retStr:=res.ReadAnsiString;
    Memo2.Lines.Add('SendSyncMessage 4 Return ('+IntToStr(resType)+' - '+IntToStr(recSize)+'):'+retStr+' - '+IntToStr(Integer(res.Size)));
  end;
  *)
  //Test with Result on the same stream
  resType :=CommsClient.SendSyncMessage(30000, 4, mtSync_Stream, recBuf, 0, recBuf, recSize);
  if (resType=mtSync_Stream) then
  begin
    retStr:=recBuf.ReadAnsiString;
    retStr:=recBuf.ReadAnsiString;
    Memo2.Lines.Add('SendSyncMessage 4 Return ('+IntToStr(resType)+' - '+IntToStr(recSize)+'):'+retStr+' - '+IntToStr(Integer(recBuf.Size)));
  end;
  recBuf.Free;
  if res<>nil then res.Free;
end;

procedure TForm1.btPRectClick(Sender: TObject);
Var
   recBuf:^TRect;
   recSize, msg:Integer;
   resType:TMessageType;

begin
  GetMem(recBuf, SizeOf(TRect));
  recBuf^.Top:=666;
  recBuf^.Left:=999;
  recBuf^.Bottom:=789;
  recBuf^.Right:=456;
  recSize:=sizeof(TRect);
  Memo2.Lines.Add('SendSyncMessage 5 (mtSync_Pointer):'+#13#10+
        IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  resType :=CommsClient.SendSyncMessage(30000, 5, mtSync_Pointer, recBuf, recSize, recBuf, recSize);
  if (resType=mtSync_Pointer) then
  begin
    Memo2.Lines.Add('SendSyncMessage 5 Return ('+IntToStr(resType)+'):'+#13#10+
          IntToStr(recBuf^.Top)+'-'+IntToStr(recBuf^.Left)+'-'+IntToStr(recBuf^.Bottom)+'-'+IntToStr(recBuf^.Right));
  end;
  FreeMem(recBuf, recSize);
end;

procedure TForm1.Button1Click(Sender: TObject);
Var
   recBuf:pTW_IDENTITY=nil;
   curBuf:pTW_IDENTITY;
   recSize, i, count:Integer;
   resType:TMessageType;

begin
  Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_LIST :');
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_LIST, mtSync_Null, recBuf, 0, recBuf, recSize);
  if (resType=mtSync_Pointer) then
  begin
    count :=SizeOf(TW_IDENTITY);
    count :=Trunc(recSize/SizeOf(TW_IDENTITY));
    curBuf:=recBuf;
    for i:=0 to count-1 do
    begin
      Memo2.Lines.Add('['+IntToStr(i)+']: '+curBuf^.ProductFamily+'-'+curBuf^.ProductName);
      Inc(curBuf);
    end;
  end;
  FreeMem(recBuf, recSize);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if CommsClient<>nil then CommsClient.Free;
  if CommsServer<>nil then CommsServer.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Caption:=ExtractFileName(ParamStr(0))+' - '+IntToStr(Sizeof(Integer));
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if CommsClient=nil then
  begin
    CommsClient  := TSyncIPCClient.Create(Nil);
    CommsClient.ServerID:=Edit1.Text {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
    CommsClient.Connect;
    if CommsClient.ServerRunning
    then MessageDlg('TestSyncIPC', 'Client Running', mtInformation, [mbOk], 0)
    else MessageDlg('TestSyncIPC', 'Client - Server NOT Running', mtError, [mbOk], 0);

    lbClient.Visible:=CommsClient.ServerRunning;
  end;
end;

end.

