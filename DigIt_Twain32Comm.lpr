program DigIt_Twain32Comm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Windows, Classes, SysUtils, Digit_Taker_Twain_Types, CustApp,
  syncipc, Twain, DelphiTwain;

type

  { TTwain32SyncIPCServer }

  TTwain32SyncIPCServer=class(TSyncIPCServer)
  private
    rTwain:TCustomDelphiTwain;

  protected
    function MessageReceived(AMsgID:Integer):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AInteger:Integer; IntegerSize:Byte):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const APointer:Pointer; Count: LongInt):Boolean; override; overload;

    function TWAIN32_TIMEOUT(ATimeout:Integer):Boolean; //Input=mtSync_Integer Output=mtSync_Integer (Boolean)
    function TWAIN32_LIST:Boolean; //Input=mtSync_Null Output=mtSync_Pointer (array of TW_IDENTITY)
    function TWAIN32_FIND(AIdentity:TW_IDENTITY):Boolean; //Input=mtSync_Var (TW_IDENTITY) Output=mtSync_Integer
    function TWAIN32_OPEN(AIndex:Integer):Boolean; //Input=mtSync_Integer Output=mtSync_Integer (Boolean)
    function TWAIN32_TAKE(APath:String):Boolean; //Input=mtSync_String  Output=mtSync_Integer (Boolean)

    function getTwain: TCustomDelphiTwain;
    procedure FreeTwain;

    procedure Test(AIndex:Integer);

    property Twain: TCustomDelphiTwain read getTwain;

  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;

  { TDigIt_Twain32Comm }

  TDigIt_Twain32Comm = class(TCustomApplication)
  protected
    CommServer : TTwain32SyncIPCServer;

    procedure DoRun; override;
    procedure MsgRunLoop;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  Timeout:Integer=3000;
  DoStop : Boolean=False;
  Application: TDigIt_Twain32Comm;


{ TTwain32SyncIPCServer }

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer): Boolean;
begin
  Case AMsgID of
  MSG_TWAIN32_STOP : begin
                       {$ifopt D+}
                       Writeln('MSG_TWAIN32_STOP:'+IntToStr(AMsgID));
                       {$endif}
                       Result:=MessageResult(RES_TWAIN32_STOPPED);
                       DoStop:=True;
                     end;
  MSG_TWAIN32_LIST : Result:=TWAIN32_LIST;
  else begin
         {$ifopt D+}
         Writeln('MSG_Unknown:'+IntToStr(AMsgID));
         {$endif}
         Result :=False;
       end;
  end;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; AInteger: Integer; IntegerSize: Byte): Boolean;
begin
  Case AMsgID of
  MSG_TWAIN32_TIMEOUT : Result:=TWAIN32_TIMEOUT(AInteger);
  MSG_TWAIN32_OPEN : Result:=TWAIN32_OPEN(AInteger);
  else begin
         {$ifopt D+}
         Writeln('MSG_Unknown:'+IntToStr(AMsgID));
         {$endif}
         Result :=False;
       end;
  end;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin
  {$ifopt D+}
  Writeln('MSG_Unknown:'+IntToStr(AMsgID));
  {$endif}
  Result :=False;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  Case AMsgID of
  MSG_TWAIN32_TAKE : Result:=TWAIN32_TAKE(Msg);
  else begin
         {$ifopt D+}
         Writeln('MSG_Unknown:'+IntToStr(AMsgID));
         {$endif}
         Result :=False;
       end;
  end;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
begin
  Case AMsgID of
  MSG_TWAIN32_FIND : Result:=TWAIN32_FIND(TW_IDENTITY(Buffer));
  else begin
         {$ifopt D+}
         Writeln('MSG_Unknown:'+IntToStr(AMsgID));
         {$endif}
         Result :=False;
       end;
  end;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; const APointer: Pointer; Count: LongInt): Boolean;
begin
  {$ifopt D+}
  Writeln('MSG_Unknown:'+IntToStr(AMsgID));
  {$endif}
  Result :=False;
end;

function TTwain32SyncIPCServer.TWAIN32_TIMEOUT(ATimeout: Integer): Boolean;
begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_TIMEOUT: '+IntToStr(ATimeout));
     {$endif}

     Result :=(ATimeout>0);
     if Result then Timeout:=ATimeout;

     MessageResult(Integer(Result));

     {$ifopt D+}
      Writeln(' TWAIN32_TIMEOUT Result: '+BoolToStr(Result, True));
     {$endif}

  except
    On E:Exception do
    begin
      Result :=False;
      {$ifopt D+}
      Application.ShowException(E);
      {$endif}
    end;
  end;
end;

function TTwain32SyncIPCServer.TWAIN32_LIST: Boolean;
var
  i, listCount: Integer;
  theList:array of TW_IDENTITY;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_LIST:');
     {$endif}

     Twain.SourceManagerLoaded :=True;
     listCount :=Twain.SourceCount;
     Result :=(listCount>0);
     if Result then
     begin
       SetLength(theList, listCount);
       for i:=0 to listCount-1 do
          theList[i] :=Twain.Source[i].SourceIdentity^;

       //Copy theList on result Stream
       Result:=MessageResult(Pointer(theList), listCount*Sizeof(TW_IDENTITY));
       SetLength(theList, 0); //we can free it, is already on the ResultStream
     end;

     {$ifopt D+}
      Writeln(' TWAIN32_LIST Result: '+IntToStr(listCount));
     {$endif}

  except
   On E:Exception do
   begin
     SetLength(theList, 0);
     Result :=False;
     {$ifopt D+}
     Application.ShowException(E);
     {$endif}
   end;
  end;
end;

function TTwain32SyncIPCServer.TWAIN32_FIND(AIdentity: TW_IDENTITY): Boolean;
begin

end;

function TTwain32SyncIPCServer.TWAIN32_OPEN(AIndex:Integer): Boolean;
var
   listCount: Integer;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_OPEN: '+IntToStr(AIndex));
     {$endif}

     Twain.SourceManagerLoaded :=True;
     listCount :=Twain.SourceCount;
     Result :=(listCount>0) and (AIndex>=0) and (AIndex<listCount);
     if Result then
     begin
       Twain.SelectedSourceIndex:=AIndex;
       Result :=(Twain.SelectedSource<>nil);
       if Result then MessageResult(1);
     end;

     {$ifopt D+}
      Writeln(' TWAIN32_OPEN Result: '+BoolToStr(Result, True));
     {$endif}

  except
    On E:Exception do
    begin
      Result :=False;
      {$ifopt D+}
      Application.ShowException(E);
      {$endif}
    end;
  end;
end;

function TTwain32SyncIPCServer.TWAIN32_TAKE(APath: String): Boolean;
var
   i:Integer;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_TAKE: '+APath);
     {$endif}

     if Assigned(Twain.SelectedSource) then
     begin
       Twain.SelectedSource.Loaded := TRUE;
       Twain.SelectedSource.ShowUI := False;//display interface
       Twain.SelectedSource.Modal:=False;
       Twain.SelectedSource.TransferMode:=ttmFile;
       Twain.SelectedSource.SetupFileTransfer(APath, tfBMP);
       Twain.SelectedSource.EnableSource(False, True);

       i:=0;
       repeat
         CheckSynchronize(10);
         Application.MsgRunLoop;
         inc(i);
         Result :=FileExists(APath);
       until Result or DoStop or (i>Timeout);

       MessageResult(Integer(Result));
     end;

     {$ifopt D+}
      Writeln(' TWAIN32_TAKE Result: '+BoolToStr(Result, True));
     {$endif}

  except
    On E:Exception do
    begin
      Result :=False;
      {$ifopt D+}
      Application.ShowException(E);
      {$endif}
    end;
  end;
end;

function TTwain32SyncIPCServer.getTwain: TCustomDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TCustomDelphiTwain.Create;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
  end;

  Result :=rTwain;
end;

procedure TTwain32SyncIPCServer.FreeTwain;
begin
  if (rTwain<>nil)
  then rTwain.Free;
end;

procedure TTwain32SyncIPCServer.Test(AIndex: Integer);
var
   listCount, i: Integer;
   aPath:String;

begin
  Writeln(' Test Twain on Device '+IntToStr(AIndex));
  Twain.SourceManagerLoaded :=True;
  Writeln('    Twain.SourceManagerLoaded='+BoolToStr(Twain.SourceManagerLoaded));
  listCount :=Twain.SourceCount;
  Writeln('    Twain.SourceCount='+IntToStr(listCount));
  if (listCount>0) and (AIndex>=0) and (AIndex<listCount) then
  begin
    Twain.SelectedSourceIndex:=AIndex;
    if Assigned(Twain.SelectedSource) then
    begin
      Writeln(' Take from '+Twain.SelectedSource.ProductName);
      aPath:=ExtractFilePath(ParamStr(0))+'test_0.bmp';
      Writeln(' Path='+aPath);

      if FileExists(aPath)
      then DeleteFile(aPath);

      Twain.SelectedSource.Loaded := TRUE;
      Twain.SelectedSource.ShowUI := False;//display interface
      Twain.SelectedSource.Modal:=False;
      Twain.SelectedSource.TransferMode:=ttmFile;
      Twain.SelectedSource.SetupFileTransfer(aPath, tfBMP);
      Twain.SelectedSource.EnableSource(False, True);

      i:=0;
      repeat
        CheckSynchronize(10);
        Application.MsgRunLoop;
        inc(i);
      until FileExists(aPath) or DoStop or (i>Timeout);

      if FileExists(aPath)
      then Writeln(' Take DONE ')
      else Writeln(' Take NOT DONE ');
    end;
  end
  else Writeln('    AIndex out of Bounds');
end;

constructor TTwain32SyncIPCServer.Create(AOwner: TComponent);
begin
  rTwain:=nil;
  inherited Create(AOwner);
end;

destructor TTwain32SyncIPCServer.Destroy;
begin
  FreeTwain;
  inherited Destroy;
end;



{ TDigIt_Twain32Comm }

procedure TDigIt_Twain32Comm.DoRun;
var
  ErrorMsg: String;
  stopClient: TSyncIPCClient;
  recSize, recBuf:Longint;

begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h s t', 'help stop test');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('s', 'stop') then
  try
     stopClient :=TSyncIPCClient.Create(nil);
     stopClient.ServerID:=TWAIN32_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
     stopClient.Connect;
     if stopClient.ServerRunning
     then stopClient.SendSyncMessage(10000, MSG_TWAIN32_STOP, mtSync_Null, recBuf, 0, recBuf, recSize);
     //    else ShowException(Exception.Create('Server '+stopClient.ServerID+' NOT Running'));

     stopClient.Free;
     Terminate;
     Exit;
  except
     On E:Exception do begin
        ShowException(E);
        stopClient.Free;
        Terminate;
        Exit;
     end;
  end;

  if HasOption('t', 'test') then
  try
     CommServer  := TTwain32SyncIPCServer.Create(Nil);
     CommServer.Test(StrToInt(GetOptionValue('t')));
     CommServer.Free;
     Terminate;
     Exit;
  except
     On E:Exception do begin
        ShowException(E);
        CommServer.Free;
        Terminate;
        Exit;
     end;
  end;


  try
     CommServer  := TTwain32SyncIPCServer.Create(Nil);
     CommServer.ServerID:=TWAIN32_SERVER_NAME {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
     CommServer.StartServer(True);  // start listening, threaded

     if CommServer.Active then
     repeat
        CheckSynchronize(10);
        MsgRunLoop;
     until DoStop;

  finally
     CommServer.Free;
     Terminate;
  end;
end;

procedure TDigIt_Twain32Comm.MsgRunLoop;
var
  AMessage: TMsg;

begin
    while PeekMessage(AMessage, HWnd(nil), 0, 0, PM_REMOVE) do
    begin
      if AMessage.message = WM_QUIT then
      begin
        PostQuitMessage(AMessage.wParam);
        DoStop :=True;
      end;

      TranslateMessage(@AMessage);
      DispatchMessageW(@AMessage);

      CheckSynchronize;
    end;
end;

constructor TDigIt_Twain32Comm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDigIt_Twain32Comm.Destroy;
begin
  inherited Destroy;
end;

procedure TDigIt_Twain32Comm.WriteHelp;
begin
  writeln('Usage: ', ExtractFileName(ExeName), ' options');
  writeln(' options:');
  writeln('         -h [--help] ', 'Show This Help');
  writeln('         -s [--stop] ', 'Stop Server');
end;

begin
  Application:=TDigIt_Twain32Comm.Create(nil);
  Application.Title:='DigIt_Twain32Comm';
  Application.Initialize;
  Application.DoRun;
  Application.Free;
end.

