unit DigIt_tests_main;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  simpleipc, syncipc, Twain, DelphiTwain, DelphiTwain_VCL, Digit_Taker_Twain_Types,
  Digit_Taker_Twain_SettingsForm, FPImage, BGRABitmap;

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
    btScanSettings1: TButton;
    btStop: TButton;
    btList: TButton;
    btIntCapture: TButton;
    btCapture: TButton;
    btIntCap: TButton;
    btIntList: TButton;
    Button1: TButton;
    Button2: TButton;
    cbModalCapture: TCheckBox;
    cbShowUI: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    edDevTest: TEdit;
    edOth1: TEdit;
    edOth2: TEdit;
    Label1: TLabel;
    lbServer: TLabel;
    lbClient: TLabel;
    Memo2: TMemo;
    btServer: TSpeedButton;
    btClient: TSpeedButton;
    procedure btScanSettings1Click(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btListClick(Sender: TObject);
    procedure btIntCaptureClick(Sender: TObject);
    procedure btCaptureClick(Sender: TObject);
    procedure btIntCapClick(Sender: TObject);
    procedure btIntListClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btServerClick(Sender: TObject);
    procedure btClientClick(Sender: TObject);
  private
     rTwain:TCustomDelphiTwain;
     Twain_Source:TTwainSource;
     Twain_SourceI:Integer;
     astr:String;
     rParams:TDigIt_Taker_TwainParams;
     CommsServer:TTestSyncIPCServer;
     CommsClient:TSyncIPCClient;

     function getTwain: TCustomDelphiTwain;

     function IPC_ParamsGet(var TwainCap:TTwainParamsCapabilities):Boolean;

     procedure TwainAcquire(Sender: TObject; const Index: Integer; Image:HBitmap;
                                 var Cancel: Boolean);

     property Twain: TCustomDelphiTwain read getTwain;
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

procedure TForm1.btScanSettings1Click(Sender: TObject);
var
   TwainCap:TTwainParamsCapabilities;
   recSize, i:Integer;
   resType:TMessageType;
   res:Boolean;

begin
  i:=StrToInt(edDevTest.Text);
  Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_OPEN : '+IntToStr(i));
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_OPEN, mtSync_Integer, i, 0, res, recSize);
  if (resType=mtSync_Integer) and (res=True) then
  begin
    Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_OPEN Return ('+IntToStr(resType)+'):'+BoolToStr(res, True));

    rParams:=TDigIt_Taker_TwainParams.Create;
    IPC_ParamsGet(TwainCap);

    TTwainSettingsSource.Execute(TwainCap, rParams);

    recSize :=SizeOf(rParams.TwainParams);
    Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_PARAMS_SET : '+IntToStr(recSize));
    resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_SET, mtSync_Var, rParams.TwainParams, recSize, res, recSize);
    if (resType=mtSync_Integer) and (res=True) then
    begin
    end;
    Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_PARAMS_SET Return ('+IntToStr(resType)+'):'+BoolToStr(res, True));

    rParams.Free;
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
   capRet: TCapabilityRet;
   TwainSource:TTwainSource;

begin
  try
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
      TwainSource :=Twain.SelectedSource;

      Memo2.Lines.Add(' Take from '+TwainSource.ProductName);
      aPath:=ExtractFilePath(ParamStr(0))+'test_0.bmp';
      Memo2.Lines.Add(' Path='+aPath);

      if FileExists(aPath)
      then DeleteFile(aPath);

      TwainSource.Loaded := True;

      capRet :=TwainSource.SetPaperFeeding(rParams.PaperFeed);
      Memo2.Lines.Add('Capability Set (PaperFeeding)='+IntToStr(Integer(capRet)));
      capRet :=TwainSource.SetDuplexEnabled(False);
      Memo2.Lines.Add('Capability Set (DuplexEnabled)='+IntToStr(Integer(capRet)));

//      rParams.Resolution:=300;
      capRet :=TwainSource.SetPaperSize(rParams.PaperSize);
      Memo2.Lines.Add('Capability Set (PaperSize)='+IntToStr(Integer(capRet)));
      capRet :=TwainSource.SetIPixelType(rParams.PixelType);
      Memo2.Lines.Add('Capability Set (PixelType)='+IntToStr(Integer(capRet)));
      capRet :=TwainSource.SetIXResolution(rParams.Resolution);
      capRet :=TwainSource.SetIYResolution(rParams.Resolution);
      Memo2.Lines.Add('Capability Set (Resolution)='+IntToStr(Integer(capRet)));
      capRet :=TwainSource.SetContrast(rParams.Contrast);
      Memo2.Lines.Add('Capability Set (Contrast)='+IntToStr(Integer(capRet)));
      capRet :=TwainSource.SetBrightness(rParams.Brightness);
      Memo2.Lines.Add('Capability Set (Brightness)='+IntToStr(Integer(capRet)));

      capRet :=TwainSource.SetIndicators(True);

      { #todo 10 -oMaxM : Brother MFC-6490 Exception with tpsNONE and ttmFile}
      TwainSource.TransferMode:=ttmNative; //ttmFile;
      Twain.OnTwainAcquire:=@TwainAcquire;
      //TwainSource.SetupFileTransfer(aPath, tfBMP);
      Memo2.Lines.Add('  ActiveFormHandle='+Screen.ActiveCustomForm.name);
      TwainSource.EnableSource(cbShowUI.Checked, cbModalCapture.Checked, Application.ActiveFormHandle);

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
  except
    TwainSource.Loaded := False;
  end;
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
   List: TStringArray;
   Current, Default: Integer;
   tCurrent, tDefault, tList: TTwainPaperSize;
   capOps:TCapabilityOperationSet;
   paperList:TTwainPaperSizeSet;
   resolutionList:TTwainResolution;
   resolutionCurrent, resolutionDefault:Single;

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
      Twain.SelectedSource.Loaded:=True;
      (*
      capRet :=Twain.SelectedSource.GetEnumerationValue(ICAP_SUPPORTEDSIZES, ItemType, List, Current, Default, rcGet, 0);

      tCurrent :=TWSS_ToTwainPaperSize(StrToInt(List[Current]));
      tDefault :=TWSS_ToTwainPaperSize(StrToInt(List[Default]));
      Memo2.Lines.Add('ICAP_SUPPORTEDSIZES'+#13#10+
         'Default='+IntToStr(Default)+' ->'+PaperSizesTwain[tDefault].name+'('+FloatToStr(PaperSizesTwain[tDefault].w)+' x '+FloatToStr(PaperSizesTwain[tDefault].h)+')'+#13#10+
         'Current='+IntToStr(Current)+' ->'+PaperSizesTwain[tCurrent].name+'('+FloatToStr(PaperSizesTwain[tCurrent].w)+' x '+FloatToStr(PaperSizesTwain[tCurrent].h)+')'+#13#10+'List:');

       for i:=Low(List) to High(List) do
       begin
         tList :=TWSS_ToTwainPaperSize(StrToInt(List[i]));
         Memo2.Lines.Add(' ['+IntToStr(i)+']='+List[i]+' ->'+PaperSizesTwain[tList].name+'('+FloatToStr(PaperSizesTwain[tList].w)+' x '+FloatToStr(PaperSizesTwain[tList].h)+')')
       end;

       //capOps :=Twain.SelectedSource.GetCapabilitySupportedOp(ICAP_SUPPORTEDSIZES);
       *)

       Twain.SelectedSource.GetPaperSizeSet(tCurrent, tDefault, paperList);
       Memo2.Lines.Add(#13#10+'SUPPORTED SIZES:'+#13#10+
          'Default='+PaperSizesTwain[tDefault].name+'('+FloatToStr(PaperSizesTwain[tDefault].w)+' x '+FloatToStr(PaperSizesTwain[tDefault].h)+')'+#13#10+
          'Current='+PaperSizesTwain[tCurrent].name+'('+FloatToStr(PaperSizesTwain[tCurrent].w)+' x '+FloatToStr(PaperSizesTwain[tCurrent].h)+')'+#13#10+'List:');

        for tList in paperList do
        begin
          Memo2.Lines.Add(IntToStr(Integer(tList))+'='+PaperSizesTwain[tList].name+'('+FloatToStr(PaperSizesTwain[tList].w)+' x '+FloatToStr(PaperSizesTwain[tList].h)+')')
        end;

        tCurrent :=GetTwainPaperSize(StrToFloat(edOth1.Text), StrToFloat(edOth2.Text), paperList);
        Memo2.Lines.Add('--- '+edOth1.Text+' x '+edOth2.Text+' closest to : '+PaperSizesTwain[tCurrent].name);

        capRet :=Twain.SelectedSource.GetIXResolution(resolutionCurrent, resolutionDefault, resolutionList);
        Memo2.Lines.Add(#13#10+'Resolutions X:');
        for i:=Low(resolutionList) to High(resolutionList) do
         Memo2.Lines.Add('['+IntToStr(i)+'] = '+FloatToStr(resolutionList[i]));

        capRet :=Twain.SelectedSource.GetIYResolution(resolutionCurrent, resolutionDefault, resolutionList);
        Memo2.Lines.Add(#13#10+'Resolutions Y:');
        for i:=Low(resolutionList) to High(resolutionList) do
         Memo2.Lines.Add('['+IntToStr(i)+'] = '+FloatToStr(resolutionList[i]));

       //Twain.SelectedSource.Loaded:=False;
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

procedure TForm1.Button1Click(Sender: TObject);
var
   r:TTwainPaperSize;

begin
(*  r :=GetTwainPaperSize(StrToFloat(edOth1.Text), StrToFloat(edOth2.Text));
  Memo2.Lines.Add('Paper closest to '+edOth1.Text+' x '+edOth2.Text+' is :'+
     PaperSizesTwain[r].name+'('+FloatToStr(PaperSizesTwain[r].w)+' x '+FloatToStr(PaperSizesTwain[r].h)+')');*)
  Memo2.Lines.Add('Sizeof(Single)='+IntToStr(Sizeof(Single)));
  Memo2.Lines.Add('Sizeof(Single)='+IntToStr(Sizeof(Single)));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
   listCount,  AIndex, i: Integer;
   capRet:TCapabilityRet;
   capBool:Boolean;
   TwainSource:TTwainSource;
   TwainCap:TTwainParamsCapabilities;
   Current: Integer;
   paperCurrent: TTwainPaperSize;
   pixelCurrent:TTwainPixelType;
   resolutionCurrent:Single;

begin
  AIndex:=StrToInt(edDevTest.Text);
  Memo2.Lines.Add(' Setting of Twain Device '+IntToStr(AIndex));
  Twain.SourceManagerLoaded :=True;
  Memo2.Lines.Add('    Twain.SourceManagerLoaded='+BoolToStr(Twain.SourceManagerLoaded, true));
  listCount :=Twain.SourceCount;
  Memo2.Lines.Add('    Twain.SourceCount='+IntToStr(listCount));
  if (listCount>0) and (AIndex>=0) and (AIndex<listCount) then
  begin
    Twain.SelectedSourceIndex:=AIndex;
    if Assigned(Twain.SelectedSource) then
    begin
      Twain.SelectedSource.Loaded:=True;


      rParams.IPC_Scanner:=False;
      rParams.Manufacturer :=Twain.SelectedSource.Manufacturer;
      rParams.ProductFamily :=Twain.SelectedSource.ProductFamily;
      rParams.ProductName :=Twain.SelectedSource.ProductName;

      //Fill UI getting values from scanner
      if rParams.IPC_Scanner
      then begin
             IPC_ParamsGet(TwainCap);
             TwainSource:=nil;
           end
      else begin
             //capRet :=Twain.SelectedSource.GetAutofeed(test);
             //capRet :=Twain.SelectedSource.SetAutoFeed(False);

             //Twain.SelectedSource.GetOrientation(t);

             TwainSource:=Twain.SelectedSource;
             TwainCap.PaperFeedingSet:=TwainSource.GetPaperFeeding;
             capRet :=TwainSource.GetPaperSizeSet(paperCurrent, TwainCap.PaperSizeDefault, TwainCap.PaperSizeSet);
             capRet :=TwainSource.GetIBitDepth(Current, TwainCap.BitDepthDefault, TwainCap.BitDepthArray);
             TwainCap.BitDepthArraySize :=Length(TwainCap.BitDepthArray);
             capRet :=TwainSource.GetIPixelType(pixelCurrent, TwainCap.PixelTypeDefault, TwainCap.PixelType);
             capRet :=TwainSource.GetIXResolution(resolutionCurrent, TwainCap.ResolutionDefault, TwainCap.ResolutionArray);
             TwainCap.ResolutionArraySize :=Length(TwainCap.ResolutionArray);
           end;


      TTwainSettingsSource.Execute(TwainCap, rParams);

//      capRet :=TwainSource.GetAutoScan(capBool);
//      Memo2.Lines.Add('Capability AutoScan='+BoolToStr(capBool, True)+' canSet='+BoolToStr(TwainSource.CapabilityCanSet(CAP_AUTOSCAN)));
//      capRet :=TwainSource.GetAutoFeed(capBool);
//      Memo2.Lines.Add('Capability AutoFeed='+BoolToStr(capBool, True)+' canSet='+BoolToStr(TwainSource.CapabilityCanSet(CAP_AUTOFEED)));

       FreeAndNil(TwainSettingsSource);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  rParams:=TDigIt_Taker_TwainParams.Create;
  rParams.PaperSize :=tpsNone;
  rParams.PixelType :=tbdRgb;
  rParams.Resolution :=300;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if CommsClient<>nil then CommsClient.Free;
  if CommsServer<>nil then CommsServer.Free;
  rParams.Free;
  if rTwain<>nil then rTwain.Free;
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

function TForm1.getTwain: TCustomDelphiTwain;
begin
  //Create Twain
  if (rTwain = nil) then
  begin
    rTwain := TCustomDelphiTwain.Create;
   // rTwain.OnTwainAcquire := @TwainTwainAcquire;

    //Load Twain Library dynamically
    rTwain.LoadLibrary;
  end;

  Result :=rTwain;
end;

function TForm1.IPC_ParamsGet(var TwainCap: TTwainParamsCapabilities): Boolean;
Var
   recStream:TMemoryStream=nil;
   recSize, i:Integer;
   resType:TMessageType;
   curBufSingle, recBufSingle:PSingle;
   curBufInteger, recBufInteger:PInteger;

begin
  Memo2.Lines.Add('SendSyncMessage MSG_TWAIN32_PARAMS_GET :');
  FillChar(TwainCap, Sizeof(TwainCap), 0);
  resType :=CommsClient.SendSyncMessage(30000, MSG_TWAIN32_PARAMS_GET, mtSync_Null, i, 0, recStream, recSize);
  if (resType=mtSync_Stream) and (recStream<>nil) then
  try
    recStream.Position:=0;
    recStream.Read(TwainCap,
                   Sizeof(TwainCap)
                   -Sizeof(TwainCap.ResolutionArray)
                   -Sizeof(TwainCap.BitDepthArray));

    //Respect the order in TTwainParamsCapabilities Type
    GetMem(recBufSingle, TwainCap.ResolutionArraySize*Sizeof(Single));
    recStream.Read(recBufSingle^, TwainCap.ResolutionArraySize*Sizeof(Single));

    Memo2.Lines.Add('ResolutionArray ('+IntToStr(TwainCap.ResolutionArraySize)+'): ');
    SetLength(TwainCap.ResolutionArray, TwainCap.ResolutionArraySize);
    curBufSingle:=recBufSingle;
    for i:=0 to  TwainCap.ResolutionArraySize-1 do
    begin
      TwainCap.ResolutionArray[i] :=curBufSingle^;
      Memo2.Lines.Add('['+IntToStr(i)+']: '+FloatToStr(TwainCap.ResolutionArray[i]));
      Inc(curBufSingle);
    end;
    FreeMem(recBufSingle, TwainCap.ResolutionArraySize*Sizeof(Single));

    GetMem(recBufInteger, TwainCap.BitDepthArraySize*Sizeof(Integer));
    recStream.Read(recBufInteger^, TwainCap.BitDepthArraySize*Sizeof(Integer));

    Memo2.Lines.Add('BitDepthArray ('+IntToStr(TwainCap.BitDepthArraySize)+'): ');
    SetLength(TwainCap.BitDepthArray, TwainCap.BitDepthArraySize);
    curBufInteger:=recBufInteger;
    for i:=0 to  TwainCap.BitDepthArraySize-1 do
    begin
      TwainCap.BitDepthArray[i] :=curBufInteger^;
      Memo2.Lines.Add('['+IntToStr(i)+']: '+IntToStr(TwainCap.BitDepthArray[i]));
      Inc(curBufInteger);
    end;
    FreeMem(recBufInteger, TwainCap.BitDepthArraySize*Sizeof(Integer));
  finally
    recStream.Free;
  end;
end;

procedure TForm1.TwainAcquire(Sender: TObject; const Index: Integer; Image: HBitmap; var Cancel: Boolean);
var
   Bitmap :TBGRABitmap;
   BitmapObj:TBitmap;

begin
  try
     BitmapObj := TBitmap.Create;
     Bitmap := TBGRABitmap.Create;
     BitmapObj.Handle := Image;
     Bitmap.Assign(BitmapObj);
     Bitmap.ResolutionUnit:=ruPixelsPerInch;
     Bitmap.ResolutionX:=rParams.Resolution;
     Bitmap.ResolutionY:=rParams.Resolution;
     Bitmap.SaveToFile('test_0.bmp');
  finally
     BitmapObj.Free;
     Bitmap.Free;
  end;
end;

end.

