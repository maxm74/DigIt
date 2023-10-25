program DigIt_Twain32Comm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Digit_Taker_Twain_Types, CustApp,
  syncipc;

type

  { TTwain32SyncIPCServer }

  TTwain32SyncIPCServer=class(TSyncIPCServer)
  protected
    function MessageReceived(AMsgID:Integer):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AInteger:Integer; IntegerSize:Byte):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const APointer:Pointer; Count: LongInt):Boolean; override; overload;
  end;

  { TDigIt_Twain32Comm }

  TDigIt_Twain32Comm = class(TCustomApplication)
  protected
    CommServer : TTwain32SyncIPCServer;

    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  DoStop : Boolean=False;


{ TTwain32SyncIPCServer }

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer): Boolean;
begin
    Writeln('Message Received :'+IntToStr(AMsgID));
    Result:=MessageResult(RES_TWAIN32_STOPPED);
    DoStop:=True;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; AInteger: Integer; IntegerSize: Byte): Boolean;
begin
    Result:=inherited MessageReceived(AMsgID, AInteger, IntegerSize);
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin
    Result:=inherited MessageReceived(AMsgID, AStream);
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  Result:=inherited MessageReceived(AMsgID, Msg);
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
begin
  Result:=inherited MessageReceived(AMsgID, Buffer, Count);
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer; const APointer: Pointer; Count: LongInt): Boolean;
begin
  Result:=inherited MessageReceived(AMsgID, APointer, Count);
end;


{ TDigIt_Twain32Comm }

procedure TDigIt_Twain32Comm.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  try
     CommServer  := TTwain32SyncIPCServer.Create(Nil);
     CommServer.ServerID:='DigIt_Twain32CommServer' {$ifdef UNIX} + '-' + GetEnvironmentVariable('USER'){$endif};
     CommServer.StartServer(True);  // start listening, threaded

     if CommServer.Active then
     repeat
        Sleep(10);
        CheckSynchronize;
     until DoStop;

  finally
     CommServer.Free;
     Terminate;
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
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TDigIt_Twain32Comm;
begin
  Application:=TDigIt_Twain32Comm.Create(nil);
  Application.Title:='DigIt_Twain32Comm';
  Application.Run;
  Application.Free;
end.

