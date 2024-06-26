(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Folder Taker                                                             **
*******************************************************************************)

unit Digit_Taker_Folder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Digit_Bridge_Intf;

type
  { TDigIt_Taker_FolderParams }

  TDigIt_Taker_FolderParams = class(TNoRefCountObject, IDigIt_Params)
  protected
    Folder,
    LastTaked :String;

  public
//    property Folder:String read rFolder write rFolder;
//    property LastTaked:String read rLastTaked write rLastTaked;
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Summary: PChar; stdcall;

    destructor Destroy; override;
  end;

  { TDigIt_Taker_Folder }
  TDigIt_Taker_Folder = class(TNoRefCountObject, IDigIt_Taker) { #note 10 -oMaxM : Meglio TNoRefCountObject }
  protected
    rParams: TDigIt_Taker_FolderParams;
    xFiles : TStringList; //A Citation
    lastFile: Integer;
    lastFolder: String;

  public
  (*  constructor Create(aParams :TPersistent); override;
    destructor Destroy; override;

    class function RegisterName: String; override;
    class function Params_GetClass : TPersistentClass; override;
    function Params_GetFromUser: Boolean; override;
    procedure Params_Set(newParams: TPersistent); override;
    class function UI_Title: String; override;
    class function UI_ImageIndex: Integer; override;
    function UI_Params_Summary: String; override;

    function Preview(var Data:Variant):TDigIt_TakerResultType; override;
    function Take(var Data:Variant):TDigIt_TakerResultType; override;
    function ReTake(var Data:Variant):TDigIt_TakerResultType; override;
    *)
    constructor Create;
    destructor Destroy; override;

    function Init: Boolean; stdcall;
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

    //function RegisterName: PChar; stdcall;
    function Params: IDigIt_Params; stdcall;
    function UI_Title(const AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

    //Take a Picture and returns FileName
    function Preview(const AFileName: PChar): Integer; stdcall;
    function Take(const AFileName: PChar): Integer; stdcall;
    function ReTake(const AFileName: PChar): Integer; stdcall;
 end;


implementation

uses Laz2_XMLCfg, Digit_Bridge_Impl, Dialogs, masks, BGRABitmapTypes;

const
  DigIt_Taker_Folder_Name = 'Folder Source';  { #todo 2 -oMaxM : Usare Risorse per la Traduzione }

var
   Taker_Folder : TDigIt_Taker_Folder = nil;

procedure SearchOnPath(xItems: TStringList; BaseDir, EnumFilter: String; EnumAttr:Integer; Recursive:Boolean);
var
   fileInfo: TSearchRec;
   err, i, dupIndex: Integer;
   theCaption,
   insCaption,
   theExt: String;
   isDefault,
   CanAdd,
   IsDir: Boolean;

begin
  //if Last char is Separator, Delete it
  if (BaseDir[Length(BaseDir)] in AllowDirectorySeparators)
  then SetLength(BaseDir, Length(BaseDir)-1);

  if DirectoryExists(BaseDir) then
  try
     err :=FindFirst(BaseDir+DirectorySeparator+'*', faAnyFile, fileInfo);
     while (err=0) do
     begin
       if (fileInfo.Name[1] <> '.') then  //non è [.] o [..]
       begin
         theCaption :=ExtractFileName(fileInfo.Name);
         theExt     :=ExtractFileExt(fileInfo.Name);
         IsDir  :=((fileInfo.Attr and faDirectory)<>0);
         CanAdd :=((fileInfo.Attr and EnumAttr) <>0) and
                    MatchesMask(fileInfo.Name, EnumFilter) and
                    (DetectFileFormat(nil, theExt)<>ifUnknown);

         if IsDir and Recursive
         then SearchOnPath(xItems, BaseDir+DirectorySeparator+fileInfo.Name, EnumFilter, EnumAttr, Recursive)
         else if CanAdd
              then if (Recursive)
                   then xItems.Add(BaseDir+DirectorySeparator+fileInfo.Name)
                   else xItems.Add(fileInfo.Name);

       end;

       err :=FindNext(fileInfo);
      end;

  finally
    FindClose(fileInfo);
  end;
end;

{ TDigIt_Taker_FolderParams }

function TDigIt_Taker_FolderParams.GetFromUser: Boolean; stdcall;
var
   openDialog: TSelectDirectoryDialog;

begin
  Result :=False;
  try
     openDialog:=TSelectDirectoryDialog.Create(nil);
     openDialog.InitialDir:=Folder;
     Result :=openDialog.Execute;
     if Result then
     begin
       Folder :=openDialog.FileName;
       LastTaked:='';
     end;

  finally
    openDialog.Free;
  end;
end;

function TDigIt_Taker_FolderParams.Duplicate: IDigIt_Params; stdcall;
begin

end;

function TDigIt_Taker_FolderParams.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);

     Folder:= XMLWork.GetValue(xml_RootPath+'/Folder', '');
     LastTaked:= XMLWork.GetValue(xml_RootPath+'/LastTaked', '');

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Taker_FolderParams.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
var
   XMLWork: TXMLConfig;

begin
  try
     Result:= False;
     XMLWork:= TXMLConfig.Create(xml_File);

     XMLWork.SetValue(xml_RootPath+'/Folder', Folder);
     XMLWork.SetValue(xml_RootPath+'/LastTaked', LastTaked);
     XMLWork.Flush;

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Taker_FolderParams.Summary: PChar; stdcall;
begin
  Result :=PChar('Folder = '+Folder+#13#10+'Taked = '+LastTaked);
end;

destructor TDigIt_Taker_FolderParams.Destroy;
begin
  inherited Destroy;
end;

{ TDigIt_Taker_Folder }

constructor TDigIt_Taker_Folder.Create;
begin
  inherited Create;

  xFiles :=TStringList.Create;
  lastFile :=-1;
  lastFolder :='';
  rParams :=TDigIt_Taker_FolderParams.Create;
end;

destructor TDigIt_Taker_Folder.Destroy;
begin
  inherited Destroy;

  xFiles.Free;
  rParams.Free;
end;

function TDigIt_Taker_Folder.Init: Boolean; stdcall;
begin
  Result :=True;
end;

function TDigIt_Taker_Folder.Enabled(AEnabled: Boolean): Boolean; stdcall;
begin
  Result :=True;
end;

function TDigIt_Taker_Folder.Release: Boolean; stdcall;
begin
  Free;
  Result :=True;
end;

(*function TDigIt_Taker_Folder.RegisterName: PChar; stdcall;
begin
  Result :=DigIt_Taker_Folder_Name;
end;*)

function TDigIt_Taker_Folder.Params: IDigIt_Params; stdcall;
begin
  Result :=rParams;
end;

function TDigIt_Taker_Folder.UI_Title(const AUI_Title: PChar): Integer; stdcall;
begin
  StrPCopy(AUI_Title, DigIt_Taker_Folder_Name);
  Result:= Length(AUI_Title);
end;

function TDigIt_Taker_Folder.UI_ImageIndex: Integer; stdcall;
begin
  Result :=3;
end;

//function TDigIt_Taker_Folder.Preview(AFileName: PChar):Integer; stdcall;
function TDigIt_Taker_Folder.Preview(const AFileName: PChar):Integer; stdcall;
begin
  Result:= 0;
  if (lastFolder<>rParams.Folder)
  then xFiles.Clear;

  if (xFiles.Count=0)
  then begin
         SearchOnPath(xFiles, rParams.Folder, '*.*', faAnyFile, False);
         lastFolder:= rParams.Folder;
         lastFile:= xFiles.IndexOf(rParams.LastTaked);
         //if (lastFile=-1) then lastFile:=0;
       end;

  if (xFiles.Count=0) or ((lastFile+1)>=xFiles.Count)
  then Result:= 0
  else begin
        StrPCopy(AFileName, rParams.Folder+DirectorySeparator+xFiles[lastFile+1]);
        Result:= Length(AFileName);
       end;
end;

function TDigIt_Taker_Folder.Take(const AFileName: PChar):Integer; stdcall;
begin
  Result:= 0;
  if (lastFolder<>rParams.Folder)
  then xFiles.Clear;

  if (xFiles.Count=0)
  then begin
         SearchOnPath(xFiles, rParams.Folder, '*.*', faAnyFile, False);
         lastFolder :=rParams.Folder;
         lastFile :=xFiles.IndexOf(rParams.LastTaked);
         //if (lastFile=-1) then lastFile:=0;
       end;

  if (xFiles.Count=0) or ((lastFile+1)>=xFiles.Count)
  then Result :=0
  else begin
         Inc(lastFile);
         rParams.LastTaked:=xFiles[lastFile];
         //Result :=PChar(rParams.Folder+DirectorySeparator+rParams.LastTaked);

         StrPCopy(AFileName, rParams.Folder+DirectorySeparator+rParams.LastTaked);
         Result:= Length(AFileName);
       end;
end;

function TDigIt_Taker_Folder.ReTake(const AFileName: PChar):Integer; stdcall;
begin
  Result :=0;
  if (lastFolder<>rParams.Folder)
  then xFiles.Clear;

  if (xFiles.Count=0)
  then begin
         SearchOnPath(xFiles, rParams.Folder, '*.*', faAnyFile, False);
         lastFolder :=rParams.Folder;
         lastFile :=xFiles.IndexOf(rParams.LastTaked);
         if (lastFile=-1) then
         begin
           lastFile:=0;
           rParams.LastTaked :=xFiles[lastFile];
         end;
       end;

  if (xFiles.Count=0) or (lastFile=xFiles.Count)
  then Result :=0
  else begin
         //Result :=PChar(rParams.Folder+DirectorySeparator+rParams.LastTaked);
         StrPCopy(AFileName, rParams.Folder+DirectorySeparator+rParams.LastTaked);
         Result:= Length(AFileName);
       end;
end;

{ TDigIt_Taker_Folder }
(*
constructor TDigIt_Taker_Folder.Create(aParams: TPersistent);
begin
  inherited Create(aParams);
  xFiles :=TStringList.Create;
  lastFile :=-1;
  lastFolder :='';
end;

destructor TDigIt_Taker_Folder.Destroy;
begin
  inherited Destroy;
  xFiles.Free;
end;

class function TDigIt_Taker_Folder.RegisterName: String;
begin
  Result :=TDigIt_Taker_Folder_Name;
end;

class function TDigIt_Taker_Folder.Params_GetClass: TPersistentClass;
begin
  result :=TDigIt_Taker_FolderParams;
end;

class function TDigIt_Taker_Folder.UI_Title: String;
begin
  Result :=TDigIt_Taker_Folder_Name;
end;

class function TDigIt_Taker_Folder.UI_ImageIndex: Integer;
begin
  Result :=3;
end;

function TDigIt_Taker_Folder.UI_Params_Summary: String;
begin
  if (rParams<>nil)
  then Result :='Folder = '+TDigIt_Taker_FolderParams(rParams).Folder+#13#10+
                'Taked = '+TDigIt_Taker_FolderParams(rParams).LastTaked
  else Result :='';
end;

function TDigIt_Taker_Folder.Preview(var Data:Variant):TDigIt_TakerResultType;
begin
  Result :=trtFilename;
  with TDigIt_Taker_FolderParams(rParams) do
  begin
    if (lastFolder<>Folder)
    then xFiles.Clear;

    if (xFiles.Count=0)
    then begin
           SearchOnPath(xFiles, Folder, '*.*', faAnyFile, False);
           lastFolder :=Folder;
           lastFile :=xFiles.IndexOf(LastTaked);
           //if (lastFile=-1) then lastFile:=0;
         end;

    if (xFiles.Count=0) or ((lastFile+1)>=xFiles.Count)
    then Data :=''
    else Data :=Folder+DirectorySeparator+xFiles[lastFile+1];
  end;
end;

function TDigIt_Taker_Folder.Take(var Data:Variant):TDigIt_TakerResultType;
begin
  Result :=trtFilename;
  with TDigIt_Taker_FolderParams(rParams) do
  begin
    if (lastFolder<>Folder)
    then xFiles.Clear;

    if (xFiles.Count=0)
    then begin
           SearchOnPath(xFiles, Folder, '*.*', faAnyFile, False);
           lastFolder :=Folder;
           lastFile :=xFiles.IndexOf(LastTaked);
           //if (lastFile=-1) then lastFile:=0;
         end;

    if (xFiles.Count=0) or ((lastFile+1)>=xFiles.Count)
    then Data :=''
    else begin
           Inc(lastFile);
           LastTaked:=xFiles[lastFile];
           Data :=Folder+DirectorySeparator+LastTaked;
         end;
  end;
end;

function TDigIt_Taker_Folder.ReTake(var Data:Variant):TDigIt_TakerResultType;
begin
  Result :=trtFilename;
  with TDigIt_Taker_FolderParams(rParams) do
  begin
    if (lastFolder<>Folder)
    then xFiles.Clear;

    if (xFiles.Count=0)
    then begin
           SearchOnPath(xFiles, Folder, '*.*', faAnyFile, False);
           lastFolder :=Folder;
           lastFile :=xFiles.IndexOf(LastTaked);
           if (lastFile=-1) then
           begin
             lastFile:=0;
             LastTaked :=xFiles[lastFile];
           end;
         end;

    if (xFiles.Count=0) or (lastFile=xFiles.Count)
    then Data :=''
    else Data :=Folder+DirectorySeparator+LastTaked;
  end;
end;

function TDigIt_Taker_Folder.Params_GetFromUser: Boolean;
var
   openDialog:TSelectDirectoryDialog;

begin
  Result :=False;
  if (rParams=nil) then exit;
  try
     with TDigIt_Taker_FolderParams(rParams) do
     begin
       openDialog:=TSelectDirectoryDialog.Create(nil);
       openDialog.InitialDir:=Folder;
       Result :=openDialog.Execute;
       if Result then
       begin
         Folder :=openDialog.FileName;
         LastTaked:='';
       end;
     end;
  finally
    openDialog.Free;
  end;
end;

procedure TDigIt_Taker_Folder.Params_Set(newParams: TPersistent);
begin
  rParams :=newParams;
end;
*)

initialization
  try
     Taker_Folder :=TDigIt_Taker_Folder.Create;
     theBridge.Takers.Register(DigIt_Taker_Folder_Name, Taker_Folder);
  except
  end;

end.

