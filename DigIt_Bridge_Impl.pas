(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Main Bridge Implementation                                               **
*******************************************************************************)

unit Digit_Bridge_Impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MM_Interface_Progress, MM_Interface_MessageDlg,
  DigIt_Types, Digit_Bridge_Intf, DigIt_Sources, DigIt_Settings, DigIt_Counter;

type
  TPluginInfo = record
    Info: TDigIt_PluginInfo;
    LibFile: String;
    LibHandle: TLibHandle;  //If =0 then InitProc and ReleaseProc refer to statically linked procedures
    InfoProc: TDigIt_PluginInfoProc;
    InitProc,
    ReleaseProc: TDigIt_PluginInitProc;
  end;

  { TDigIt_Plugins }

  TDigIt_Plugins = class(TObject)
  protected
    rPluginsList: array of TPluginInfo;

    function Find(const aName: String): Integer;

    function GetPlugin(const aName: String): TPluginInfo; overload;
    function GetPlugin(Index : Integer): TPluginInfo; overload;
    function GetPluginName(Index: Integer): String;
    function GetCount: Integer;

    function FreeElement(Index: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function Register(const aFileName: String): Boolean; overload;
    function Register(const aPluginInfo: TPluginInfo): Boolean; overload;
    function UnRegister(const aName: String): Boolean;

    function RegisterInPath(aPath: String): Integer;

    property Count: Integer read GetCount;

    property PluginByName [const aName: String]: TPluginInfo read GetPlugin;
    property Plugin [const aIndex: Integer]: TPluginInfo read GetPlugin;
    property Name [const aIndex: Integer]: String read GetPluginName;
  end;

  { #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
  (*

  { TDigIt_Destinations }

  TDestinationInfo = record
    Flags: DWord;
    Inst: IDigIt_Destination;
  end;
  PDestinationInfo = ^TDestinationInfo;

  TDigIt_Destinations = class(specialize TOpenArrayList<TDestinationInfo, String>, IDigIt_Destinations)
    function Register(const aName: PChar; const aClass: IDigIt_Destination): Boolean; stdcall;
  protected
    rSelected: PDestinationInfo;
    rSelectedName: String;
    rSelectedIndex: Integer;
    rSelectedParams: IDigIt_Params;

    function FreeElement(var aData: TDestinationInfo): Boolean; override;

  public
    constructor Create;

    function Select(DestinationName: String; GetUserParams: Boolean=False): Boolean; overload;
    function Select(DestinationIndex: Integer; GetUserParams: Boolean=False): Boolean; overload;

    function LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;

    property Selected: PDestinationInfo read rSelected;
    property SelectedIndex: Integer read rSelectedIndex;
    property SelectedName: String read rSelectedName;
    property SelectedParams: IDigIt_Params read rSelectedParams;
  end;

  *)

  { TDigIt_Bridge }

  TDigIt_Bridge = class(TNoRefCountObject, IDigIt_Bridge)
  protected
    //rDestinations: TDigIt_Destinations;
    rPlugins: TDigIt_Plugins;
    rProgress: IMM_Progress;
    rMsg: IMM_MessageDlg;

  public
    constructor Create;
    destructor Destroy; override;

    function Sources: IDigIt_Sources; stdcall;
    //function Destinations: IDigIt_Destinations; stdcall;
    function Settings: IDigIt_Settings; stdcall;
    function Progress: IMM_Progress; stdcall;
    function Msg: IMM_MessageDlg; stdcall;

    //***************************  Internal Use only ***************************
    procedure SetProgressInterface(const AProgressInterface: IMM_Progress);
    procedure SetMsgInterface(const AMsgInterface: IMM_MessageDlg);

    //Useful functions
    procedure ProgressShow(ACaption: String; TotalMin, TotalMax: Integer; CurrentMarquee: Boolean=False); overload;
    procedure ProgressShow(ACaption: String; TotalMarquee: Boolean; CurrentMin, CurrentMax: Integer); overload;
    procedure ProgressShow(ACaption: String; TotalMin, TotalMax, CurrentMin, CurrentMax: Integer); overload;
    procedure ProgressHide;
    function ProgressCancelled: Boolean;

    function ProgressSetTotal(TotalCaption: String; TotalMin, TotalMax, TotalVal: Integer): Boolean; overload;
    function ProgressSetTotal(TotalCaption: String; TotalVal: Integer): Boolean; overload;

    function ProgressSetCurrent(CurrentCaption: String; CurrentVal: Integer): Boolean; overload;
    function ProgressSetCurrent(CurrentCaption: String; CurrentMin, CurrentMax, CurrentVal: Integer): Boolean; overload;

    function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; overload;
    function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; overload;
    function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; overload;
    function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; overload;
    function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; const HelpKeyword: string): TModalResult; overload;
    function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                         Buttons: array of const; HelpCtx: Longint): TModalResult; overload;
    function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                         Buttons: array of const; const HelpKeyword: string): TModalResult; overload;

    property Plugins: TDigIt_Plugins read rPlugins;
  end;

var
   theBridge: TDigIt_Bridge = nil;

implementation

uses dynlibs, Masks;

{ TDigIt_Plugins }

function TDigIt_Plugins.Find(const aName: String): Integer;
var
  i : Integer;

begin
  Result:= -1;
  for i:=0 to Length(rPluginsList)-1 do
    if (rPluginsList[i].Info.Name = aName) then
    begin
      Result:= i; break;
    end;
end;

constructor TDigIt_Plugins.Create;
begin
  inherited Create;

  rPluginsList:= Nil;
end;

destructor TDigIt_Plugins.Destroy;
var
   i:Integer;
   res: Boolean;

begin
  for i:=0 to Length(rPluginsList)-1 do
  begin
    try
       res:= FreeElement(i);

    except
    end;
  end;

  try
     rPluginsList:= Nil;
  except
  end;

  inherited Destroy;
end;

function TDigIt_Plugins.Register(const aFileName: String): Boolean;
var
   newPlugin: TPluginInfo;
   res: Boolean;

begin
  Result:= False;

  try
     with newPlugin do
     begin
       LibFile:= aFileName;
       LibHandle:= LoadLibrary(aFileName);

       if not(LibHandle = dynlibs.NilHandle) then
       begin
         InitProc:= TDigIt_PluginInitProc(GetProcedureAddress(LibHandle, DigIt_PluginInitProcName));

         //Plugin MUST Have InitProc
         if Assigned(InitProc) then
         begin
           ReleaseProc:= TDigIt_PluginInitProc(GetProcedureAddress(LibHandle, DigIt_PluginReleaseProcName));
           InfoProc :=TDigIt_PluginInfoProc(GetProcedureAddress(LibHandle, DigIt_PluginInfoProcName));

           res:= Assigned(InfoProc);
           if res
           then res:= InfoProc(Info);

           if not(res)
           then Info.Name:= ExtractFileName(aFileName);

           Result:= Register(newPlugin);
         end;

         if not(Result)
         then FreeLibrary(newPlugin.LibHandle);

       end;
     end;

  except
    if not(newPlugin.LibHandle = dynlibs.NilHandle)
    then FreeLibrary(newPlugin.LibHandle);
    Result:= False;
  end;
end;

function TDigIt_Plugins.Register(const aPluginInfo: TPluginInfo): Boolean;
begin
  Result:= False;

  if (Find(aPluginInfo.Info.Name) = -1) then
  begin
    SetLength(rPluginsList, Length(rPluginsList)+1);

    rPluginsList[Length(rPluginsList)-1]:= aPluginInfo;

    if Assigned(aPluginInfo.InitProc)
    then Result:= aPluginInfo.InitProc(theBridge);
  end;
end;

function TDigIt_Plugins.UnRegister(const aName: String): Boolean;
var
   r : Integer;
   res: Boolean;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    Result:= FreeElement(r);

    Delete(rPluginsList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Plugins.RegisterInPath(aPath: String): Integer;
var
   fileInfo: TSearchRec;
   err: Integer;
   theCaption,
   theExt: String;
   IsDir: Boolean;

begin
  Result:= 0;

  //if Last char is Separator, Delete it
  if (aPath[Length(aPath)] in AllowDirectorySeparators)
  then SetLength(aPath, Length(aPath)-1);

  if DirectoryExists(aPath) then
  try
     err:= FindFirst(aPath+DirectorySeparator+'*', faAnyFile, fileInfo);
     while (err = 0) do
     begin
       if (fileInfo.Name[1] <> '.') then  //not [.] or [..]
       begin
         theCaption :=ExtractFileName(fileInfo.Name);
         theExt     :=ExtractFileExt(fileInfo.Name);
         IsDir  :=((fileInfo.Attr and faDirectory)<>0);

         if not(IsDir) and MatchesMask(fileInfo.Name, '*.'+SharedSuffix)
         then if Register(aPath+DirectorySeparator+fileInfo.Name)
              then Inc(Result);
       end;

       err :=FindNext(fileInfo);
    end;

  finally
    FindClose(fileInfo);
  end;
end;

function TDigIt_Plugins.GetPlugin(const aName: String): TPluginInfo;
var
  i: Integer;

begin
  { #note 2 -oMaxM : Return a Copy or a Pointer to List element? }
  FillChar(Result, SizeOf(TPluginInfo), 0);

  for i:=0 to Length(rPluginsList)-1 do
    if (rPluginsList[i].Info.Name = aName) then
    begin
      Result:= rPluginsList[i]; break;
    end;
end;

function TDigIt_Plugins.GetPlugin(Index: Integer): TPluginInfo;
begin
  { #note 2 -oMaxM : Return a Copy or a Pointer to List element? }
  FillChar(Result, SizeOf(TPluginInfo), 0);

  if (Index >= 0) and (Index < Length(rPluginsList))
  then Result:= rPluginsList[Index];
end;

function TDigIt_Plugins.GetPluginName(Index: Integer): String;
begin
  if (Index >= 0) and (Index < Length(rPluginsList))
  then Result:= rPluginsList[Index].Info.Name
  else Result:= '';
end;

function TDigIt_Plugins.GetCount: Integer;
begin
  Result:= Length(rPluginsList);
end;

function TDigIt_Plugins.FreeElement(Index: Integer): Boolean;
begin
  Result:= False;

  if Assigned(rPluginsList[Index].ReleaseProc)
  then Result:= rPluginsList[Index].ReleaseProc(theBridge);

  if not(rPluginsList[Index].LibHandle = dynlibs.NilHandle)
  then Result:= FreeLibrary(rPluginsList[Index].LibHandle);
end;

{ #note -oMaxM : Not enabled for now until I figure out how to pass the image data and make the thumbnails }
(*

{ TDigIt_Destinations }

function TDigIt_Destinations.FreeElement(var aData: TDestinationInfo): Boolean;
begin
  Result:= False;
  try
     if (aData.Inst <> nil)
     then aData.Inst.Release;

     Result:= True;

  finally
    //MaxM: When the open array is freed the compiler frees the contents of the record using rtti,
    //      a very dangerous thing for us.
    FillChar(aData, SizeOf(aData), 0);
  end;
end;

constructor TDigIt_Destinations.Create;
begin
  inherited Create;

  rSelected:= nil;
  rSelectedName:= '';
  rSelectedIndex:= -1;
  rSelectedParams:= nil;
end;

function TDigIt_Destinations.Select(DestinationName: String; GetUserParams: Boolean): Boolean;
var
   newDestinationI: Integer;
   newDestination: PDestinationInfo =nil;

begin
  Result:= False;

  if (DestinationName <> '') then
  try
     newDestinationI:= FindByKey(DestinationName);
     newDestination:= Data[newDestinationI];

     if (newDestination <> nil) then
     begin
       if GetUserParams and
          not((newDestination^.Inst.Params = nil) or newDestination^.Inst.Params.GetFromUser)
       then exit;

       rSelected:= newDestination;
       rSelectedParams :=newDestination^.Inst.Params;
       rSelectedName:= DestinationName;
       rSelectedIndex:= newDestinationI;
       Result:= True;
      end;

  except
    Result:= False;
  end;
end;

function TDigIt_Destinations.Select(DestinationIndex: Integer; GetUserParams: Boolean): Boolean;
var
   newDestination: PDestinationInfo =nil;

begin
  Result:= False;

  try
     newDestination:= Data[DestinationIndex];

     if (newDestination <> nil) then
     begin
       if GetUserParams and
          not((newDestination^.Inst.Params = nil) or newDestination^.Inst.Params.GetFromUser)
       then exit;

       rSelected:= newDestination;
       rSelectedParams :=newDestination^.Inst.Params;
       rSelectedName:= rList[DestinationIndex].Key;
       rSelectedIndex:= DestinationIndex;
       Result:= True;
      end;

  except
    Result:= False;
  end;
end;

function TDigIt_Destinations.LoadSelectedParams(XMLFileName, XMLPath: String): Boolean;
begin
  Result:= False;
  if (rSelectedParams <> nil) and (XMLFileName <> '') and (XMLPath <> '') then
  try
     Result:= rSelectedParams.Load(PChar(XMLFileName), PChar(XMLPath));
     if Result then Result:= rSelectedParams.OnSet;

  except
    Result:= False;
  end;
end;

function TDigIt_Destinations.Register(const aName: PChar; const aClass: IDigIt_Destination): Boolean; stdcall;
var
   newData: TDestinationInfo;

begin
  Result:= False;
  if (aClass = nil) then exit;

  //If the Class cannot Init don't register it and Release
  if (aClass.Init)
  then begin
         newData.Inst:= aClass;
         Result:= (Add(aName, newData) > -1);
       end
  else aClass.Release;
end;

(*
function TDigIt_Destinations.UnRegister(const aName: String): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aName);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rDestinationsList, r, 1);
    Result:= True;
  end;
end;

function TDigIt_Destinations.UnRegister(const aClass: IDigIt_Destination): Boolean;
var
   r : Integer;

begin
  Result:= False;

  r:= Find(aClass);
  if (r > -1) then
  begin
    { #todo 10 -oMaxM : Free the Instances?  }
    Result:= FreeElement(r);

    Delete(rDestinationsList, r, 1);
    Result:= True;
  end;
end;
*)
*)

{ TDigIt_Bridge }

constructor TDigIt_Bridge.Create;
begin
  inherited Create;

  rPlugins:= TDigIt_Plugins.Create;
  DigIt_Counter.Counter:= TDigIt_Counter.Create('Counter0');
  DigIt_Settings.Settings:= TDigIt_Settings.Create;
  DigIt_Sources.Sources:= TDigIt_Sources.Create;
  //rDestinations:= TDigIt_Destinations.Create;
end;

destructor TDigIt_Bridge.Destroy;
begin
  DigIt_Sources.Sources.Free; DigIt_Sources.Sources:= nil;
  DigIt_Settings.Settings.Free; DigIt_Settings.Settings:= nil;
  rPlugins.Free;
  //rDestinations.free;

  inherited Destroy;
end;

function TDigIt_Bridge.Sources: IDigIt_Sources; stdcall;
begin
  Result:= DigIt_Sources.Sources as IDigIt_Sources;
end;

(*
function TDigIt_Bridge.Destinations: IDigIt_Destinations; stdcall;
begin
  Result:= rDestinations as IDigIt_Destinations;
end;
*)

function TDigIt_Bridge.Settings: IDigIt_Settings; stdcall;
begin
  Result:= DigIt_Settings.Settings as IDigIt_Settings;
end;

function TDigIt_Bridge.Progress: IMM_Progress; stdcall;
begin
  Result:= rProgress;
end;

function TDigIt_Bridge.Msg: IMM_MessageDlg; stdcall;
begin
  Result:= rMsg;
end;

procedure TDigIt_Bridge.SetProgressInterface(
  const AProgressInterface: IMM_Progress);
begin
  rProgress:= AProgressInterface;
end;

procedure TDigIt_Bridge.SetMsgInterface(const AMsgInterface: IMM_MessageDlg);
begin
  rMsg:= AMsgInterface;
end;

procedure TDigIt_Bridge.ProgressShow(ACaption: String; TotalMin, TotalMax: Integer; CurrentMarquee: Boolean=False);
begin
  if (rProgress <> nil) then
  begin
    rProgress.SetTotal(TotalMin, TotalMax, TotalMin, False);

    if CurrentMarquee
    then rProgress.SetCurrent(0, 100, 0, True)
    else rProgress.SetCurrentVisible(False);

    rProgress.Show(PChar(ACaption));
  end;
end;

procedure TDigIt_Bridge.ProgressShow(ACaption: String; TotalMarquee: Boolean; CurrentMin, CurrentMax: Integer);
begin
  if (rProgress <> nil) then
  begin
    if TotalMarquee
    then rProgress.SetTotal(0, 100, 0, True)
    else rProgress.SetTotalVisible(False);

    rProgress.SetCurrent(CurrentMin, CurrentMax, CurrentMin, False);

    rProgress.Show(PChar(ACaption));
  end;
end;

procedure TDigIt_Bridge.ProgressShow(ACaption: String; TotalMin, TotalMax, CurrentMin, CurrentMax: Integer);
begin
  if (rProgress <> nil) then
  begin
    rProgress.SetTotal(TotalMin, TotalMax, TotalMin, False);
    rProgress.SetCurrent(CurrentMin, CurrentMax, CurrentMin, False);
    rProgress.SetTotalVisible(True);
    rProgress.SetCurrentVisible(True);
    rProgress.Show(PChar(ACaption));
  end;
end;

procedure TDigIt_Bridge.ProgressHide;
begin
  if (rProgress <> nil) then rProgress.Hide;
end;

function TDigIt_Bridge.ProgressCancelled: Boolean;
begin
  Result:= False;
  if (rProgress <> nil) then Result:= rProgress.Cancelled;
end;

function TDigIt_Bridge.ProgressSetTotal(TotalCaption: String; TotalMin, TotalMax, TotalVal: Integer): Boolean;
begin
  if (rProgress <> nil) then
  begin
    Result:= rProgress.Cancelled;
    if Result then exit;

    rProgress.SetTotal(TotalMin, TotalMax, TotalVal, False);
    rProgress.SetTotalCaption(PChar(TotalCaption));
    Result:= rProgress.Cancelled;
  end;
end;

function TDigIt_Bridge.ProgressSetTotal(TotalCaption: String; TotalVal: Integer): Boolean;
begin
  if (rProgress <> nil) then
  begin
    Result:= rProgress.Cancelled;
    if Result then exit;

    rProgress.SetTotalValue(TotalVal);
    rProgress.SetTotalCaption(PChar(TotalCaption));
    Result:= rProgress.Cancelled;
  end;
end;

function TDigIt_Bridge.ProgressSetCurrent(CurrentCaption: String; CurrentVal: Integer): Boolean;
begin
  if (rProgress <> nil) then
  begin
    Result:= rProgress.Cancelled;
    if Result then exit;

    rProgress.SetCurrentValue(CurrentVal);
    rProgress.SetCurrentCaption(PChar(CurrentCaption));
    Result:= rProgress.Cancelled;
  end;
end;

function TDigIt_Bridge.ProgressSetCurrent(CurrentCaption: String; CurrentMin, CurrentMax, CurrentVal: Integer): Boolean;
begin
  if (rProgress <> nil) then
  begin
    Result:= rProgress.Cancelled;
    if Result then exit;

    rProgress.SetCurrent(CurrentMin, CurrentMax, CurrentVal, False);
    rProgress.SetCurrentCaption(PChar(CurrentCaption));
    Result:= rProgress.Cancelled;
  end;
end;

function TDigIt_Bridge.MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.MessageDlg(PChar(aMsg), DlgType, Buttons, HelpCtx);
end;

function TDigIt_Bridge.MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.MessageDlg(PChar(aCaption), PChar(aMsg), DlgType, Buttons, HelpCtx);
end;

function TDigIt_Bridge.MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.MessageDlg(PChar(aCaption), PChar(aMsg), DlgType, Buttons, HelpCtx, DefaultButton);
end;

function TDigIt_Bridge.MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.MessageDlg(PChar(aMsg), DlgType, Buttons, HelpCtx, DefaultButton);
end;

function TDigIt_Bridge.MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: TMsgDlgButtons; const HelpKeyword: string): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.MessageDlg(PChar(aCaption), PChar(aMsg), DlgType, Buttons, PChar(HelpKeyword));
end;

function TDigIt_Bridge.QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: array of const; HelpCtx: Longint): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.QuestionDlg(PChar(aCaption), PChar(aMsg), DlgType, Buttons, HelpCtx);
end;

function TDigIt_Bridge.QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
                                  Buttons: array of const; const HelpKeyword: string): TModalResult;
begin
  Result:= mrNone;
  if (rMsg <> nil) then Result:= rMsg.QuestionDlg(PChar(aCaption), PChar(aMsg), DlgType, Buttons, PChar(HelpKeyword));
end;

initialization
  theBridge:= TDigIt_Bridge.Create;

finalization
  //theBridge.Free;   { #note -oMaxM : MainForm Free the Bridge otherwise the TwainSource does not send the message STOP to 32 bit Comm }

end.

