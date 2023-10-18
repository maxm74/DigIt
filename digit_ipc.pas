unit Digit_IPC;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, simpleipc;

const
  //mtSync_ResultServerID = 2;  // {GUID}AMsgID
  mtSync_Integer = 3;
  mtSync_Stream = 4;
  mtSync_String = 5;
  mtSync_Buffer = 6;

type

  { TSyncIPCServer }

  TSyncIPCServer = class(TSimpleIPCServer)
  private
    resultClient:TSimpleIPCClient;

  protected
    procedure InternalMessageRecevied(Sender: TObject);

    function MessageResult(ResultInteger:Integer):Boolean; overload;
    function MessageResult(ResultStream:TStream):Boolean; overload;
    function MessageResult(const ResultString:String):Boolean; overload;
    function MessageResult(const Buffer; Count: LongInt):Boolean; overload;

    //Derived Classes must implement this methods using MessageResult to send back the Result
    function MessageReceived(AMsgID:Integer; AInteger:Integer):Boolean; virtual; overload;
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

    function preSendSyncMessage(var MsgStream: TMemoryStream; AMsgID:Integer): Boolean;
    procedure postSendSyncMessage;
    function SendSyncMessage(ATimeOut:Integer; AMsgID:Integer;
                             AStream:TStream; ResultStream:TStream;
                             MsgType: TMessageType=mtSync_Stream):TMessageType; overload;

  public
    function SendSyncMessage(ATimeOut:Integer; AMsgID:Integer;
                             const Msg: String; var ResultString:String):TMessageType; overload;

// AData depends on the type of Result:
//        mtSync_Integer -> An Integer
//        mtSync_Stream  -> A Stream, if AData initially is nil then a new TMemoryStream is returned (user must free it)
//                                    else the result is appended in AData Stream.
//        mtSync_String  -> A String
//        mtSync_Buffer  -> A Pointer, if AData initially is nil then a new Pointer with Size=Result Size is allocated
//                                     else the Data is copied in user AData (there must be sufficient space)
    function SendSyncMessage(ATimeOut:Integer; AMsgID:Integer; MsgDataType:TMessageType;
                             const Buffer; Count: LongInt;
                             var AData; var ADataSize:Longint):TMessageType; overload;
  end;

implementation

{ TSyncIPCServer }

procedure TSyncIPCServer.InternalMessageRecevied(Sender: TObject);
var
  posGraf, curMsgID, msgInteger:Integer;
  curMsgType:TMessageType;
  resultServerID, strMsg:String;
  msgStream:TMemoryStream;
  AResult:Boolean;

begin
  ReadMessage;
  curMsgType :=Self.MsgType;
  if (curMsgType in [mtSync_Integer..mtSync_Buffer]) then
  begin
    msgStream:=TMemoryStream(Self.MsgData);
    msgStream.Position:=0;
    strMsg:=msgStream.ReadAnsiString;
    msgStream.ReadData(curMsgID);

    FreeAndNil(resultClient);

    if (strMsg<>'') and (strMsg[1]='{') then
    begin
      posGraf :=Pos('}', strMsg);
      if (posGraf>1) then
      begin
        resultServerID :=Copy(strMsg, 1, posGraf);
        if (resultServerID<>'') then
        try
          resultClient:=TSimpleIPCClient.Create(Nil);
          resultClient.ServerID:=resultServerID;
          resultClient.Connect;

          if resultClient.ServerRunning then
          begin
             Case curMsgType of
             mtSync_Integer: begin
               msgStream.ReadData(msgInteger);
               AResult :=MessageReceived(curMsgID, msgInteger);
             end;
             mtSync_Stream: AResult :=MessageReceived(curMsgID, msgStream);
             mtSync_String: AResult :=MessageReceived(curMsgID, msgStream.ReadAnsiString);
             mtSync_Buffer: AResult :=MessageReceived(curMsgID, msgStream.Memory, msgStream.Size-msgStream.Position);
             end;

             //Send something to avoid TimeOut
             if not(AResult) then MessageResult(0);
           end;

        finally
          //if (msgStream<>nil) then msgStream.Free;
          FreeAndNil(resultClient);
        end;
      end;
     end;
   end;
end;

function TSyncIPCServer.MessageResult(ResultInteger: Integer): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;
     curResBuffer:=TMemoryStream.Create;
     curResBuffer.WriteData(ResultInteger);
     resultClient.SendMessage(mtSync_Integer, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;  //Client Free the Stream ??
  end;
end;

function TSyncIPCServer.MessageResult(ResultStream: TStream): Boolean;
begin
  Result:=False;
  resultClient.SendMessage(mtSync_Stream, ResultStream);
  Result:=True;
end;

function TSyncIPCServer.MessageResult(const ResultString: String): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;
     curResBuffer:=TMemoryStream.Create;
     curResBuffer.WriteAnsiString(ResultString);
     resultClient.SendMessage(mtSync_String, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;  //Client Free the Stream ??
  end;
end;

function TSyncIPCServer.MessageResult(const Buffer; Count: LongInt): Boolean;
var
   curResBuffer:TMemoryStream;

begin
  try
     Result:=False;
     curResBuffer:=TMemoryStream.Create;
     curResBuffer.Write(Buffer, Count);
     resultClient.SendMessage(mtSync_Buffer, curResBuffer);
     Result:=True;

  finally
    curResBuffer.Free;  //Client Free the Stream ??
  end;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; AInteger: Integer): Boolean;
var
   resRect:TRect;

begin
  { #todo 10 -oMaxM : Tests...delete it }
  Case AMsgID of
  10: Result :=MessageResult($ABCDEF0);
  11: begin
      resRect.Top:=AInteger;
      resRect.Left:=AInteger+33;
      resRect.Bottom:=AInteger+66;
      resRect.Right:=AInteger+99;
      Result :=MessageResult(resRect, sizeof(TRect));
  end;

  end;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; AStream: TStream): Boolean;
begin

end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const Msg: String): Boolean;
begin
  { #todo 10 -oMaxM : Tests...delete it }
  Case AMsgID of
  12: Result :=MessageResult('Ciao son Sync Result for '+IntToStr(AMsgID));
  end;
end;

function TSyncIPCServer.MessageReceived(AMsgID: Integer; const Buffer; Count: LongInt): Boolean;
begin
  { #todo 10 -oMaxM : Test THIS }
end;

constructor TSyncIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  resultClient:=nil;
  Global:=True;
  Self.OnMessageQueued:=@InternalMessageRecevied;
end;

{ TSyncIPCClient }

function TSyncIPCClient.preSendSyncMessage(var MsgStream: TMemoryStream; AMsgID: Integer): Boolean;
var
   myID:TGUID;

   function randCreateGuid:TGUID;
   var
      i:Integer;
      P : PByte;

   begin
     Randomize;
     P:=@Result;
     for i:=0 to SizeOf(TGuid)-1 do P[i]:=Random(256);
     Result.clock_seq_hi_and_reserved:=(Result.clock_seq_hi_and_reserved and $3F) + 64;
     Result.time_hi_and_version      :=(Result.time_hi_and_version and $0FFF)+ $4000;
   end;

begin
  Result :=False;

  resultServer  :=TSimpleIPCServer.Create(Nil);
  if (CreateGUID(myID)=0)
  then resultServer.ServerID:=GUIDToString(myID)
  else resultServer.ServerID:=GUIDToString(randCreateGuid);
  resultServer.Global:=True;
  resultServer.StartServer(False);
  if resultServer.Active then
  begin
    Connect;
    if ServerRunning then
    begin
      MsgStream:=TMemoryStream.Create;
      MsgStream.WriteAnsiString(resultServer.ServerID);
      MsgStream.WriteBufferData(AMsgID);
      Result:=True;
    end;
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
   MsgStream:TMemoryStream=nil;

begin
  try
     Result :=mtUnknown;
     if preSendSyncMessage(MsgStream, AMsgID) then
     begin
       MsgStream.CopyFrom(AStream, 0);
       SendMessage(MsgType, MsgStream);

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
                                        const Msg: String; var ResultString: String): TMessageType;
var
   msgStr, resStr:TMemoryStream;

begin
  try
     Result :=mtUnknown;
     resStr:=TMemoryStream.Create;
     msgStr:=TMemoryStream.Create;
     msgStr.WriteAnsiString(Msg);
     Result :=SendSyncMessage(ATimeOut, AMsgID, msgStr, resStr, mtSync_String);

     if (Result=mtSync_String)
     then begin
            resStr.Position:=0;
            ResultString :=resStr.ReadAnsiString;
          end
     else ResultString:='';

  finally
    msgStr.Free;
    resStr.Free;
  end;
end;

function TSyncIPCClient.SendSyncMessage(ATimeOut: Integer; AMsgID: Integer; MsgDataType: TMessageType;
                                        const Buffer; Count: LongInt;
                                        var AData; var ADataSize: Longint): TMessageType;
var
   msgStr, resStr:TMemoryStream;
   resInt:Integer;

begin
  try
     msgStr:=nil;
     Result :=mtUnknown;
     resStr:=TMemoryStream.Create;

     Case MsgDataType of
     mtSync_Integer: begin
        msgStr:=TMemoryStream.Create;
        msgStr.WriteData(Integer(Buffer));
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStr, resStr, MsgDataType);
     end;
     mtSync_Stream : Result :=SendSyncMessage(ATimeOut, AMsgID, TStream(Buffer), resStr, MsgDataType);
     mtSync_String : begin
        msgStr:=TMemoryStream.Create;
        msgStr.WriteAnsiString(String(Buffer));
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStr, resStr, MsgDataType);
     end;
     mtSync_Buffer : begin
        msgStr:=TMemoryStream.Create;
        msgStr.Write(Buffer, Count);
        Result :=SendSyncMessage(ATimeOut, AMsgID, msgStr, resStr, MsgDataType);
     end;
     { #todo -oMaxM : User defined Type with callback(Buffer, Count, msgStr) }
     end;

     resStr.Position:=0;

     Case Result of
     mtSync_Integer: begin
        resStr.ReadData(resInt);
        Integer(AData) :=resInt;
        ADataSize:=sizeof(resInt);
        resStr.Free;
     end;
     mtSync_Stream : begin
        if (TStream(AData)=nil)
        then begin
               TStream(AData) :=resStr;
               ADataSize :=resStr.Size;
             end
        else begin
               ADataSize :=TStream(AData).CopyFrom(resStr, 0);
               resStr.Free;
             end;
     end;
     mtSync_String : begin
        String(AData) :=resStr.ReadAnsiString;
        resStr.Free;
     end;
     mtSync_Buffer : begin
        ADataSize:=resStr.Size;

        if (Pointer(AData)=nil)
        then GetMem(Pointer(AData), ADataSize);

        ADataSize :=resStr.Read(Pointer(AData)^, ADataSize);
        resStr.Free;
     end;
     { #todo -oMaxM : User defined Type with callback(resStr, AData, ADataSize) }
     end;

  finally
     if (msgStr<>nil) then msgStr.Free;
  end;
end;

end.

