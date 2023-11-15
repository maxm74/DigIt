unit DigIt_tests_main;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  simpleipc, syncipc, Twain, DelphiTwain, DelphiTwain_VCL, Digit_Taker_Twain_Types;

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
    btStop: TButton;
    btList: TButton;
    btIntCapture: TButton;
    btCapture: TButton;
    btIntCap: TButton;
    btIntList: TButton;
    cbModalCapture: TCheckBox;
    cbShowUI: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    edDevTest: TEdit;
    Label1: TLabel;
    lbServer: TLabel;
    lbClient: TLabel;
    Memo2: TMemo;
    btServer: TSpeedButton;
    btClient: TSpeedButton;
    procedure btStopClick(Sender: TObject);
    procedure btListClick(Sender: TObject);
    procedure btIntCaptureClick(Sender: TObject);
    procedure btCaptureClick(Sender: TObject);
    procedure btIntCapClick(Sender: TObject);
    procedure btIntListClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btServerClick(Sender: TObject);
    procedure btClientClick(Sender: TObject);
  private
     rTwain:TDelphiTwain;
     Twain_Source:TTwainSource;
     Twain_SourceI:Integer;
     astr:String;

    function getTwain: TDelphiTwain;
  private
    CommsServer:TTestSyncIPCServer;
    CommsClient:TSyncIPCClient;

    property Twain: TDelphiTwain read getTwain;

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

(*  Case AMsgID of
  end; *)
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Stream) : '+IntToStr(AMsgID)+' ('+IntToStr(AStream.Size)+')');

  (*  Case AMsgID of
    end; *)
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_String) : '+IntToStr(AMsgID)+'-'+Msg);

  (*  Case AMsgID of
    end; *)
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
var
   resRect:TRect;

begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Var) : '+IntToStr(AMsgID)+' ('+IntToStr(Count)+')');

  (*  Case AMsgID of
    end; *)
end;

function TTestSyncIPCServer.MessageReceived(AMsgID: Integer; const APointer: Pointer; Count: LongInt): Boolean;
type PRect=^TRect;
begin
  Form1.Memo2.Lines.Add('MessageReceived (mtSync_Pointer) : '+IntToStr(AMsgID)+' ('+IntToStr(Count)+')');

  (*  Case AMsgID of
    end; *)
end;


{ TForm1 }


procedure TForm1.btServerClick(Sender: TObject);
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

procedure TForm1.btStopClick(Sender: TObject);
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

procedure TForm1.btListClick(Sender: TObject);
Var
   recBuf:pTW_IDENTITY=nil;
   curBuf:pTW_IDENTITY;
   recSize, count, i:Integer;
   resType:TMessageType;

begin
  Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_LIST :');
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_LIST, mtSync_Null, recBuf, 0, recBuf, recSize);
  if (resType=mtSync_Pointer) and (recBuf<>nil) then
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

procedure TForm1.btIntCaptureClick(Sender: TObject);
var
   listCount,  AIndex, i: Integer;
   aPath:String;

begin
  AIndex:=StrToInt(edDevTest.Text);
  Memo2.Lines.Add(' Test Twain on Device '+IntToStr(AIndex));
  Twain.SourceManagerLoaded :=True;
  Memo2.Lines.Add('    Twain.SourceManagerLoaded='+BoolToStr(Twain.SourceManagerLoaded, true));
  listCount :=Twain.SourceCount;
  Memo2.Lines.Add('    Twain.SourceCount='+IntToStr(listCount));
  if (listCount>0) and (AIndex>=0) and (AIndex<listCount) then
  begin
    Twain.SelectedSourceIndex:=AIndex;
    if Assigned(Twain.SelectedSource) then
    begin
      Memo2.Lines.Add(' Take from '+Twain.SelectedSource.ProductName);
      aPath:=ExtractFilePath(ParamStr(0))+'test_0.bmp';
      Memo2.Lines.Add(' Path='+aPath);

      if FileExists(aPath)
      then DeleteFile(aPath);

      Twain.SelectedSource.Loaded := TRUE;
     // Twain.SelectedSource.ShowUI := False;//display interface
     // Twain.SelectedSource.Modal:=False;
      Twain.SelectedSource.TransferMode:=ttmFile;
      Twain.SelectedSource.SetupFileTransfer(aPath, tfBMP);
      Memo2.Lines.Add('  ActiveFormHandle='+Screen.ActiveCustomForm.name);
      Twain.SelectedSource.EnableSource(cbShowUI.Checked, cbModalCapture.Checked, Application.ActiveFormHandle);

      i:=0;
      repeat
        Memo2.Lines.Add('.');
        CheckSynchronize(10);
        Application.ProcessMessages;
        inc(i);
      until FileExists(APath) or (i>500);

      if FileExists(aPath)
      then Memo2.Lines.Add(' Take DONE ')
      else Memo2.Lines.Add(' Take NOT DONE ');
    end;
  end
  else Memo2.Lines.Add('    AIndex out of Bounds');

  if rTwain<>nil then FreeAndNil(rTwain);
end;

procedure TForm1.btCaptureClick(Sender: TObject);
Var
     recBuf:pTW_IDENTITY=nil;
     curBuf:pTW_IDENTITY;
     recSize, i, count:Integer;
     resType:TMessageType;
     aPath:String;
     res:Boolean;

begin
    i:=StrToInt(edDevTest.Text);
    Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_OPEN : '+IntToStr(i));
    resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_OPEN, mtSync_Integer, i, 0, res, recSize);
    if (resType=mtSync_Integer) and (res=True) then
    begin
      aPath:=ExtractFilePath(ParamStr(0))+'test_take.bmp';
      Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_TAKE : '+aPath);
      resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_TAKE, mtSync_String, aPath, 0, res, recSize);
      if (resType=mtSync_Integer) and (res=True)
      then Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_TAKE : DONE')
      else Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_TAKE : FAIL');
    end
    else Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_OPEN : FAIL')
end;

procedure TForm1.btIntCapClick(Sender: TObject);
var
   listCount,  AIndex, i: Integer;
   aPath:String;
   capHandle: HGLOBAL;
   capContainer: TW_UINT16;
   capRet: TCapabilityRet;
   ArrayV:pTW_ENUMERATION;

   ItemType: TW_UINT16;
   List: TGetCapabilityList;
   Current, Default: Integer;
   capOps:TCapabilityOperations;

begin
  AIndex:=StrToInt(edDevTest.Text);
  Memo2.Lines.Add(' Test Twain on Device '+IntToStr(AIndex));
  Twain.SourceManagerLoaded :=True;
  Memo2.Lines.Add('    Twain.SourceManagerLoaded='+BoolToStr(Twain.SourceManagerLoaded, true));
  listCount :=Twain.SourceCount;
  Memo2.Lines.Add('    Twain.SourceCount='+IntToStr(listCount));
  if (listCount>0) and (AIndex>=0) and (AIndex<listCount) then
  begin
    Twain.SelectedSourceIndex:=AIndex;
    if Assigned(Twain.SelectedSource) then
    begin
     // capRet :=Twain.SelectedSource.GetCapabilityRec(ICAP_SUPPORTEDSIZES, capHandle, rcGet, capContainer);
      Twain.SelectedSource.Loaded:=True;
      //ItemType:=TWTY_UINT16;
      capRet :=Twain.SelectedSource.GetEnumerationValue(ICAP_SUPPORTEDSIZES, ItemType, List, Current, Default, rcGet, 0);
 //     ArrayV := GlobalLock(capHandle);

      {Unlock memory and unallocate}
 //     GlobalUnlock(capHandle);
 //     GlobalFree(capHandle);
       Memo2.Lines.Add('ICAP_SUPPORTEDSIZES Current='+IntToStr(Current)+' Default='+IntToStr(Default));
       for i:=Low(List) to High(List) do
         Memo2.Lines.Add(' ['+IntToStr(i)+']='+List[i]);

       capOps :=Twain.SelectedSource.GetCapabilitySupportedOp(ICAP_SUPPORTEDSIZES);

       Twain.SelectedSource.Loaded:=False;
    end;
  end;
end;

procedure TForm1.btIntListClick(Sender: TObject);
var
   i:Integer;

begin
  Twain.SourceManagerLoaded := TRUE;
  for i:=0 to Twain.SourceCount-1 do
  begin
    aStr:=Twain.Source[i].ProductName;
    Memo2.Lines.Add('['+IntToStr(i)+']: '+aStr);
  end;
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

procedure TForm1.btClientClick(Sender: TObject);
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

function TForm1.getTwain: TDelphiTwain;
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

end.

