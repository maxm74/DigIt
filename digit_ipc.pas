unit Digit_IPC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, simpleipc;

const
  mtSync_ResultServerID = 2;  // {GUID}AMsgID
  mtSync_Integer = 3;
  mtSync_Stream = 4;
  mtSync_String = 5;
  mtSync_Buffer = 6;

type

  { TSyncIPCServer }

  TSyncIPCServer = class(TSimpleIPCServer)
  private
    resultClient:TSimpleIPCClient;
    curMsgID:Integer;

  protected
    procedure InternalMessageRecevied(Sender: TObject);

    function MessageResult(ResultInteger:Integer):Boolean; overload;
    function MessageResult(ResultStream:TStream):Boolean; overload;
    function MessageResult(const ResultString:String):Boolean; overload;
    function MessageResult(const Buffer; Count: LongInt):Boolean; overload;

    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; virtual; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; virtual; overload;

  public
    Constructor Create(AOwner : TComponent); override;
  end;

  { TSyncIPCClient }

  TSyncIPCClient = class(TSimpleIPCClient)
  protected
    resultServer:TSimpleIPCServer;

    function preSendSyncMessage(AMsgID:Integer): Boolean;
    procedure postSendSyncMessage;

  public
    function SendSyncMessage(ATimeOut:Integer; AMsgID:Integer;
                             AStream:TStream; ResultStream:TStream;
                             MsgType: TMessageType=mtSync_Stream):TMessageType; overload;
    function SendSyncMessage(ATimeOut:Integer; AMsgID:Integer;
                             const Msg: String; var ResultString:String):Boolean; overload;
    function SendSyncMessage(ATimeOut:Integer; AMsgID:Integer;
                             const Buffer; Count: LongInt;
                             var ADataPointer:Pointer; var ADataSize:Longint):Boolean; overload;

  end;

function DataPointerToStream(const Buffer; Count: LongInt):TMemoryStream;
procedure StreamToDataPointer(AStream:TMemoryStream; var Buffer:Pointer; var ADataSize:Longint);

implementation

function DataPointerToStream(const Buffer; Count: LongInt): TMemoryStream;
begin
  Result:=TMemoryStream.Create;
  Result.Write(Buffer, Count);
end;

procedure StreamToDataPointer(AStream: TMemoryStream; var Buffer:Pointer; var ADataSize: Longint);
begin
  AStream.Position:=0;
  ADataSize:=AStream.Size;
  Buffer:=AStream.Memory;
end;

{ TSyncIPCServer }

procedure TSyncIPCServer.InternalMessageRecevied(Sender: TObject);
var
  posGraf :Integer;
  curMsgType:TMessageType;
  resultServerID, strMsg:String;
  curBuffer:TMemoryStream;
  AResult:Boolean;

begin
  ReadMessage;
  curMsgType :=Self.MsgType;
  if (curMsgType=mtSync_ResultServerID) then
  begin
    strMsg:=Self.StringMessage;

    FreeAndNil(resultClient);

    if (strMsg<>'') and (strMsg[1]='{') then
    begin
      posGraf :=Pos('}', strMsg);
      if (posGraf>1) then
      begin
        resultServerID :=Copy(strMsg, 1, posGraf);
        if (resultServerID<>'') then
        try
          curMsgID :=StrToInt(Copy(strMsg, posGraf+1, 255));

          curBuffer:=nil;
          resultClient:=TSimpleIPCClient.Create(Nil);
          resultClient.ServerID:=resultServerID;
          resultClient.Connect;
        except
          FreeAndNil(resultClient);
        end;
      end;
     end;
   end
   else if (resultClient<>nil) and resultClient.ServerRunning then
        try
            Case curMsgType of
            mtSync_Stream: begin
                             curBuffer:=TMemoryStream.Create;
                             Self.GetMessageData(curBuffer);
                             curBuffer.Position:=0;
                             AResult :=MessageReceived(curMsgID, curBuffer);
                           end;
            mtSync_String: AResult :=MessageReceived(curMsgID, Self.StringMessage);
            mtSync_Buffer: begin
                             curBuffer:=TMemoryStream.Create;
                             Self.GetMessageData(curBuffer);
                             curBuffer.Position:=0;
                             AResult :=MessageReceived(curMsgID, curBuffer.Memory, curBuffer.Size);
                           end;
            end;

            if not(AResult)
            then resultClient.SendStringMessage(mtSync_Integer, '0');

          finally
            if (curBuffer<>nil) then curBuffer.Free;
            FreeAndNil(resultClient);
          end;
end;

function TSyncIPCServer.MessageResult(ResultInteger: Integer): Boolean;
begin
  Result:=False;
  resultClient.SendStringMessage(mtSync_Integer, IntToStr(ResultInteger));
  Result:=True;
end;

function TSyncIPCServer.MessageResult(ResultStream: TStream): Boolean;
begin
  Result:=False;
  resultClient.SendMessage(mtSync_Stream, ResultStream);
  Result:=True;
end;

function TSyncIPCServer.MessageResult(const ResultString: String): Boolean;
begin
  Result:=False;
  resultClient.SendStringMessage(mtSync_String, ResultString);
  Result:=True;
end;

function TSyncIPCServer.MessageResult(const Buffer; Count: LongInt): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;
     curResBuffer:=DataPointerToStream(Buffer, Count);
     resultClient.SendMessage(mtSync_Buffer, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;
  end;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin

end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  Case AMsgID of
  12: Result :=MessageResult('Ciao son Sync Result for '+IntToStr(AMsgID));
  end;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
begin

end;

constructor TSyncIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  resultClient:=nil;
  Global:=True;
  Self.MaxQueue:=2;
  Self.OnMessageQueued:=@InternalMessageRecevied;
end;

{ TSyncIPCClient }

function TSyncIPCClient.preSendSyncMessage(AMsgID: Integer): Boolean;
var
   myID:TGUID;

begin
  Result :=False;

  resultServer  :=TSimpleIPCServer.Create(Nil);
  if (CreateGUID(myID)=0)
  then resultServer.ServerID:=GUIDToString(myID)
  else resultServer.ServerID:='{'+IntToStr(AMsgID)+IntToStr(GetTickCount64)+'}';
  resultServer.Global:=True;
  resultServer.StartServer(False);
  if resultServer.Active then
  begin
    Connect;
    Result :=ServerRunning;
    if Result then SendStringMessage(mtSync_ResultServerID, resultServer.ServerID+IntToStr(AMsgID));
  end;
end;

procedure TSyncIPCClient.postSendSyncMessage;
begin
  FreeAndNil(resultServer);
end;

function TSyncIPCClient.SendSyncMessage(ATimeOut: Integer; AMsgID: Integer;
                                        AStream: TStream; ResultStream: TStream;
                                        MsgType: TMessageType): TMessageType;
var
   myTickStart, curTick:QWord;

begin
  try
     Result :=mtUnknown;
     if preSendSyncMessage(AMsgID) then
     begin
       SendMessage(MsgType, AStream);

       myTickStart :=GetTickCount64;
       repeat
         Application.ProcessMessages;

         if resultServer.PeekMessage(0, True) then
         begin
           Result:=resultServer.MsgType;
           resultServer.GetMessageData(ResultStream);

           break;
         end;

         curTick :=GetTickCount64;
       until ((curTick-myTickStart)>ATimeOut);
     end;

  finally
    postSendSyncMessage;
  end;
end;


function TSyncIPCClient.SendSyncMessage(ATimeOut: Integer; AMsgID: Integer;
                                        const Msg: String; var ResultString: String): Boolean;
var
   S:TStringStream;

begin
  try
     Result :=False;
     S:=TStringStream.Create(Msg);
     SendSyncMessage(ATimeOut, AMsgID, S, S, mtSync_String);
     ResultString:=S.DataString;
     Result :=True;
  finally
    S.Free;
  end;
end;

function TSyncIPCClient.SendSyncMessage(ATimeOut: Integer; AMsgID: Integer;
                                        const Buffer; Count: LongInt;
                                        var ADataPointer: Pointer; var ADataSize: Longint): Boolean;
begin

end;

end.

