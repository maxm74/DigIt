program DigIt_Twain32Comm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Windows, Classes, SysUtils, CustApp,
  SyncIPC, Twain, DelphiTwain, DelphiTwainUtils, DelphiTwainTypes,
  Digit_Source_Twain_Types;

type

  { TTwain32SyncIPCServer }

  TTwain32SyncIPCServer=class(TSyncIPCServer)
  private
    rTwain: TCustomDelphiTwain;
    TwainSource:TTwainSource;
    rScannerInfo: TW_IDENTITY;
    rParams: TTwainParams;
    TwainCapGetted: Boolean;
    TwainCap: TTwainParamsCapabilities;
    rUserInterface: TW_USERINTERFACE;
    curTimeout: Integer;

    function GetTwainSource(Load: Boolean): TTwainSource;

    function InternalTake(AResolution: Single; APath:String): Integer;

    function TwainProcessMessages(Sender: TObject; const Index: Integer): Boolean;

    procedure TwainAcquireNative(Sender: TObject; const Index: Integer;
                                 nativeHandle: TW_UINT32; var Cancel: Boolean);

  protected
    function MessageReceived(AMsgID:Integer):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AInteger:Integer; IntegerSize:Byte):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; AStream:TStream):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Msg: String):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const Buffer; Count: LongInt):Boolean; override; overload;
    function MessageReceived(AMsgID:Integer; const APointer:Pointer; Count: LongInt):Boolean; override; overload;

    function TWAIN32_TIMEOUT(ATimeout:Integer):Boolean; //Input=mtData_Integer Output=mtData_Integer (Boolean)
    function TWAIN32_LIST:Boolean; //Input=mtData_Null Output=mtData_Pointer (array of TW_IDENTITY)
    function TWAIN32_FIND(AIdentity:TW_IDENTITY):Boolean; //Input=mtData_Var (TW_IDENTITY) Output=mtData_Integer
    function TWAIN32_OPEN(AIdentity:TW_IDENTITY):Boolean; //Input=mtData_Var (TW_IDENTITY) Output=mtData_Integer (Boolean)
    function TWAIN32_USERINTERFACE(AUserInterface:TW_USERINTERFACE):Boolean; //Input=mtData_Var (TW_USERINTERFACE) Output=mtData_Integer (Boolean)
    function TWAIN32_PARAMS_SET(AParams:TTwainParams):Boolean; //Input=mtData_Var (TW_USERINTERFACE) Output=mtData_Integer (Boolean)
    function TWAIN32_CAPABILITIES_GET:Boolean; //Input=mtData_Null Output=mtData_Var (TTwainParamsCapabilities)
    function TWAIN32_PREVIEW(APath:String):Boolean; //Input=mtData_String  Output=mtData_Integer
    function TWAIN32_TAKE(APath:String):Boolean; //Input=mtData_String  Output=mtData_Integer

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

function TTwain32SyncIPCServer.GetTwainSource(Load: Boolean): TTwainSource;
begin
  Result:= Twain.SelectedSource;
  { #note 10 -oMaxM : For some reason Twain change the device order, so we MUST check if is our Selected Device }
  if (Result = nil) or (DeviceInfoDifferent(rScannerInfo, Result.SourceIdentity^))
  then begin
         {$ifopt D+}
         if (Result = nil)
         then Writeln('  ERROR SelectedSource = NIL')
         else Writeln('  ERROR SelectedSource Name:'+rScannerInfo.ProductName+'<>'+Result.SourceIdentity^.ProductName);
         {$endif}

         Result:= Twain.SelectSource(rScannerInfo.Manufacturer,
                                     rScannerInfo.ProductFamily,
                                     rScannerInfo.ProductName, Load);
       end
  else if Load then Result.Loaded:= True;

  TwainSource:= Result;
end;

function TTwain32SyncIPCServer.InternalTake(AResolution: Single; APath: String): Integer;
var
   capRet:TCapabilityRet;

begin
  Result:= 0;
  if (GetTwainSource(True) <> nil) then
  begin
    {$ifopt D+}
     Writeln('  InternalTake ['+IntToStr(TwainSource.Index)+'] ID:'+IntToStr(TwainSource.Id)+'-'+TwainSource.ProductName);
    {$endif}

    //Set Parameters, (after a capture the scanner reset it to default???)
    capRet :=TwainSource.SetPaperFeeding(rParams.PaperFeed);
    capRet :=TwainSource.SetDuplexEnabled(False);
    capRet :=TwainSource.SetPaperSize(rParams.PaperSize);
    capRet :=TwainSource.SetIPixelType(rParams.PixelType);
    capRet :=TwainSource.SetIXResolution(AResolution);
    capRet :=TwainSource.SetIYResolution(AResolution);
    capRet :=TwainSource.SetContrast(rParams.Contrast);
    capRet :=TwainSource.SetBrightness(rParams.Brightness);
    TwainSource.SetIndicators(True);

    { #note 10 -oMaxM : Switched to ttmNative Mode (my office Scanner fail if paper=tpsNone and dpi>150) see Tests}
    TwainSource.TransferMode:=ttmNative;
    Twain.OnTwainAcquireNative:=@TwainAcquireNative;
    Twain.OnTwainAcquireBitmap:=nil;

    curTimeout:= 0;
    DoStop:= False;
    {$ifopt D+}
     Writeln('    downloading on '+APath+' '+TwainFileBase);
    {$endif}
    Result:= TwainSource.Download(rUserInterface, APath, TwainFileBase, '.bmp', tfBMP);

(*
    repeat
       {$ifopt D+}
        Write('.');
       {$endif}
      CheckSynchronize(10);
      Application.MsgRunLoop;
      inc(i);
    until Result or DoStop or (i>Timeout);
*)
  end;
  {$ifopt D+}
   Writeln('  InternalTake Result: '+IntToStr(Result));
  {$endif}
end;

function TTwain32SyncIPCServer.TwainProcessMessages(Sender: TObject; const Index: Integer): Boolean;
begin
  {$ifopt D+}
   Write('.');
  {$endif}
  Application.MsgRunLoop;
  inc(curTimeout);
  Result:= not(DoStop or (curTimeout > Timeout));
end;

procedure TTwain32SyncIPCServer.TwainAcquireNative(Sender: TObject; const Index: Integer;
                                                   nativeHandle: TW_UINT32; var Cancel: Boolean);
begin
  with TCustomDelphiTwain(Sender).Source[Index] do
  try
    if (Download_Count = 0)
    then WriteBitmapToFile(Download_Path+Download_FileName+Download_Ext, nativeHandle)
    else WriteBitmapToFile(Download_Path+Download_FileName+
                           '-'+IntToStr(Download_Count)+Download_Ext, nativeHandle);

    Cancel := DoStop;
  except
  end;
end;

function TTwain32SyncIPCServer.MessageReceived(AMsgID: Integer): Boolean;
begin
  Case AMsgID of
  MSG_TWAIN32_STOP : begin
                       {$ifopt D+}
                       Writeln('MSG_TWAIN32_STOP:'+IntToStr(AMsgID));
                       {$endif}
                       //Result:=MessageResult(RES_TWAIN32_STOPPED);
                       Result:= True;
                       DoStop:=True;
                     end;
  MSG_TWAIN32_LIST : Result:=TWAIN32_LIST;
  MSG_TWAIN32_CAPABILITIES_GET: Result:=TWAIN32_CAPABILITIES_GET;
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
  MSG_TWAIN32_PREVIEW : Result:=TWAIN32_PREVIEW(Msg);
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
  MSG_TWAIN32_OPEN : Result:=TWAIN32_OPEN(TW_IDENTITY(Buffer));
  MSG_TWAIN32_USERINTERFACE : Result:=TWAIN32_USERINTERFACE(TW_USERINTERFACE(Buffer));
  MSG_TWAIN32_PARAMS_SET: Result:=TWAIN32_PARAMS_SET(TTwainParams(Buffer));
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

     Result:=MessageResult(Integer(Result));

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

     Twain.SourceManagerLoaded :=False; //So we Refresh Devices List
     Application.MsgRunLoop;
     Twain.SourceManagerLoaded :=True;
     listCount :=Twain.SourceCount;
     Result :=(listCount>0);
     if Result then
     begin
       SetLength(theList, listCount);
       for i:=0 to listCount-1 do
       begin
          theList[i] :=Twain.Source[i].SourceIdentity^;
          {$ifopt D+}
          Writeln('    ['+IntToStr(i)+'] = ID:'+IntToStr(theList[i].Id)+'-'+theList[i].Manufacturer+' - '+theList[i].ProductName);
          {$endif}
       end;

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
var
  res, i: Integer;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_FIND: '+AIdentity.Manufacturer);
      Writeln('               '+AIdentity.ProductFamily);
      Writeln('               '+AIdentity.ProductName);
      Writeln('    list before:');
      for i:=0 to Twain.SourceCount-1 do
        Writeln('    ['+IntToStr(i)+'] = ID:'+IntToStr(Twain.Source[i].SourceIdentity^.ID)+' - '+Twain.Source[i].SourceIdentity^.ProductName);
     {$endif}

     Twain.SourceManagerLoaded :=False; //So we Refresh Devices List
     Application.MsgRunLoop;
     Twain.SourceManagerLoaded :=True;
     {$ifopt D+}
     Writeln('    list after:');
     for i:=0 to Twain.SourceCount-1 do
       Writeln('    ['+IntToStr(i)+'] = ID:'+IntToStr(Twain.Source[i].SourceIdentity^.ID)+' - '+Twain.Source[i].SourceIdentity^.ProductName);
    {$endif}
     res :=Twain.FindSource(AIdentity.Manufacturer, AIdentity.ProductFamily, AIdentity.ProductName);
     Result:=MessageResult(res);

     {$ifopt D+}
      Writeln(' TWAIN32_FIND Result: '+IntToStr(res));
     {$endif}

  except
   On E:Exception do
   begin
     Result :=MessageResult(-1);
     {$ifopt D+}
     Application.ShowException(E);
     {$endif}
   end;
  end;
end;

function TTwain32SyncIPCServer.TWAIN32_OPEN(AIdentity:TW_IDENTITY): Boolean;
var
   listCount: Integer;
   oldTwainSource: TTwainSource;

begin
  Result :=False;
  try
    {$ifopt D+}
     Writeln(' TWAIN32_OPEN: '+IntToStr(AIdentity.Id)+'-'+AIdentity.ProductName);
    {$endif}

    if Twain.SourceManagerLoaded
    then oldTwainSource:= Twain.SelectedSource
    else oldTwainSource:= nil;

    Twain.SourceManagerLoaded :=True;
    TwainSource:= Twain.SelectSource(AIdentity.Manufacturer,
                                     AIdentity.ProductFamily,
                                     AIdentity.ProductName, False);
    Result:= (TwainSource <> nil);
    if Result then
    begin
       //Close Current Scanner if any
       if (oldTwainSource <> nil) then
       begin
         oldTwainSource.Loaded:= False;
         TwainCapGetted:= False;
       end;

       TwainSource.Loaded:= True;

       rScannerInfo:= AIdentity;

       Result:=MessageResult(Integer(TwainSource.Loaded));
       {$ifopt D+}
        Writeln('  Opened ID:'+IntToStr(TwainSource.ID)+'-'+TwainSource.ProductName);
       {$endif}
     end;

     {$ifopt D+}
      Writeln(' TWAIN32_OPEN: Result: '+BoolToStr(Result, True));
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

function TTwain32SyncIPCServer.TWAIN32_USERINTERFACE(AUserInterface: TW_USERINTERFACE): Boolean;
begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_USERINTERFACE: ShowUI='+BoolToStr(AUserInterface.ShowUI, True));
      Writeln('                        ModalUI='+BoolToStr(AUserInterface.ModalUI, True));
      Writeln('                        hParent='+IntToStr(AUserInterface.hParent));
     {$endif}

     rUserInterface:=AUserInterface;
     Result:=MessageResult(Integer(True));

     {$ifopt D+}
      Writeln(' TWAIN32_USERINTERFACE Result: '+BoolToStr(Result, True));
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

function TTwain32SyncIPCServer.TWAIN32_PARAMS_SET(AParams: TTwainParams): Boolean;
begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_PARAMS_SET: PaperSize='+IntToStr(Integer(AParams.PaperSize)));
      Writeln('                     Resolution='+FloatToStr(AParams.Resolution));
      Writeln('                     PixelType='+IntToStr(Integer(AParams.PixelType)));
     {$endif}

     rParams:=AParams;
     Result:=True;
     MessageResult(Integer(Result));

     {$ifopt D+}
      Writeln(' TWAIN32_PARAMS_SET Result: '+BoolToStr(Result, True));
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

function TTwain32SyncIPCServer.TWAIN32_CAPABILITIES_GET: Boolean;
var
   curResBuffer:TMemoryStream;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_PARAMS_GET:');
     {$endif}

     Result:= (GetTwainSource(True) <> nil);
     if Result and TwainSource.GetParamsCapabilities(TwainCap) then
     begin
       TwainCapGetted:= True;
(*       {$ifopt D+}
        Writeln('   Sizeof(TwainCap):'+IntToStr(Sizeof(TwainCap)));
        Writeln('   Sizeof(Single):'+IntToStr(Sizeof(Single)));
        Writeln('   Sizeof(ResolutionArray):'+IntToStr(Sizeof(TwainCap.ResolutionArray)));
        Writeln('   Length(ResolutionArray):'+IntToStr(TwainCap.ResolutionArraySize));
        Writeln('   Sizeof(BitDepthArray):'+IntToStr(Sizeof(TwainCap.BitDepthArray)));
        Writeln('   Length(BitDepthArray):'+IntToStr(TwainCap.BitDepthArraySize));
       {$endif}
*)
       //Create a MemoryStream to send back result and write the Buffer
       curResBuffer:=TMemoryStream.Create;
       curResBuffer.Write(TwainCap,
                          Sizeof(TwainCap)
                          -Sizeof(TwainCap.ResolutionArray)
                          -Sizeof(TwainCap.BitDepthArray));

       //Respect the order in TTwainParamsCapabilities Type
       curResBuffer.Write(Pointer(TwainCap.ResolutionArray), TwainCap.ResolutionArraySize*Sizeof(Single));
       curResBuffer.Write(Pointer(TwainCap.BitDepthArray), TwainCap.BitDepthArraySize*Sizeof(Integer));

       {$ifopt D+}
        Writeln('   Result Size:'+IntToStr(curResBuffer.Size));
       {$endif}

       Result:=MessageResult(curResBuffer);

       //we can free it, is already on the ResultStream
       curResBuffer.Free;
     end;

     {$ifopt D+}
      Writeln(' TWAIN32_PARAMS_GET Result: '+BoolToStr(Result, True));
     {$endif}

  except
   On E:Exception do
   begin
     if (curResBuffer<>nil) then curResBuffer.Free;
     Result :=False;
     {$ifopt D+}
     Application.ShowException(E);
     {$endif}
   end;
  end;
end;

function TTwain32SyncIPCServer.TWAIN32_PREVIEW(APath: String): Boolean;
var
   res: Integer;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_PREVIEW: '+APath);
     {$endif}

     if (GetTwainSource(True)<>nil) then
     begin
       if not(TwainCapGetted) then TwainCapGetted:= TwainSource.GetParamsCapabilities(TwainCap);

       if TwainCapGetted
       then res:= InternalTake(TwainCap.ResolutionMin, APath)
       else res:= InternalTake(100, APath);

       Result:=MessageResult(res);
     end;

     {$ifopt D+}
      Writeln(' TWAIN32_PREVIEW Result: '+IntToStr(res));
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
   res: Integer;

begin
  Result :=False;
  try
     {$ifopt D+}
      Writeln(' TWAIN32_TAKE: '+APath);
     {$endif}

     res:= InternalTake(rParams.Resolution, APath);
     Result:=MessageResult(res);

     {$ifopt D+}
      Writeln;
      Writeln(' TWAIN32_TAKE Result: '+IntToStr(res));
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
    rTwain.OnProcessMessages:= @TwainProcessMessages;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;

    TwainCapGetted:= False;
  end;

  Result :=rTwain;
end;

procedure TTwain32SyncIPCServer.FreeTwain;
begin
  if (rTwain<>nil) then rTwain.Free;

  TwainCap.BitDepthArray:= nil;
  TwainCap.ResolutionArray:= nil;
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

      rParams.PaperSize :=tpsNone;
      rParams.PixelType :=tbdRgb;
      rParams.Resolution :=200;
      InternalTake(rParams.Resolution, aPath);

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
  rUserInterface.ShowUI:=False;
  rUserInterface.ModalUI:=True;
  rUserInterface.hParent:=0;
  TwainCapGetted:= False;

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
     then stopClient.SendMessage(MSG_TWAIN32_STOP, mtData_Null, recBuf);
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
      TranslateMessage(@AMessage);
      DispatchMessageW(@AMessage);

      if AMessage.message = WM_QUIT then
      begin
        PostQuitMessage(AMessage.wParam);
        DoStop :=True;
      end;

      //TranslateMessage(@AMessage);
      //DispatchMessageW(@AMessage);

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

{$R *.res}

begin
  Application:=TDigIt_Twain32Comm.Create(nil);
  Application.Title:='DigIt_Twain32Comm';
  Application.Initialize;
  Application.DoRun;
  Application.Free;
end.

