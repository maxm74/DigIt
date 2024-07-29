(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Folder Source                                                             **
*******************************************************************************)

unit Digit_Source_Folder;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Digit_Bridge_Intf;

const
  DigIt_Source_Folder_Name = 'Folder Pictures';

resourcestring
  DigIt_Source_Folder_NameL = 'Folder Pictures';

type
  { TDigIt_Source_FolderParams }

  TDigIt_Source_FolderParams = class(TNoRefCountObject, IDigIt_Params)
  protected
    Folder,
    LastTaked :String;

  public
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Summary(const ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;
  end;

  { TDigIt_Source_Folder }
  TDigIt_Source_Folder = class(TNoRefCountObject, IDigIt_Source) { #note 10 -oMaxM : Meglio TNoRefCountObject }
  protected
    rParams: TDigIt_Source_FolderParams;
    xFiles : TStringList; //A Citation
    lastFile: Integer;
    lastFolder: String;

  public
    constructor Create;
    destructor Destroy; override;

    function Flags: DWord; stdcall;
    function Init: Boolean; stdcall;
    function Enabled(AEnabled: Boolean): Boolean; stdcall;
    function Release: Boolean; stdcall;

    //function RegisterName: PChar; stdcall;
    function Params: IDigIt_Params; stdcall;
    function UI_Title(const AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

    //Take a Picture and returns FileName
    function Take(takeAction: DigIt_Source_TakeAction; MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;
 end;


implementation

uses Laz2_XMLCfg, Digit_Bridge_Impl, Dialogs, masks, BGRABitmapTypes;

var
   Source_Folder : TDigIt_Source_Folder = nil;

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

{ TDigIt_Source_FolderParams }

function TDigIt_Source_FolderParams.GetFromUser: Boolean; stdcall;
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

function TDigIt_Source_FolderParams.Duplicate: IDigIt_Params; stdcall;
begin
  Result:= nil;
end;

function TDigIt_Source_FolderParams.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

function TDigIt_Source_FolderParams.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

function TDigIt_Source_FolderParams.Summary(const ASummary: PChar): Integer; stdcall;
begin
  StrPCopy(ASummary, 'Folder= '+Folder+#13#10+'Taked= '+LastTaked);
  Result:= Length(ASummary);
end;

function TDigIt_Source_FolderParams.OnSet: Boolean; stdcall;
begin
  Result:= True;
end;

{ TDigIt_Source_Folder }

constructor TDigIt_Source_Folder.Create;
begin
  inherited Create;

  xFiles :=TStringList.Create;
  lastFile :=-1;
  lastFolder :='';
  rParams :=TDigIt_Source_FolderParams.Create;
end;

destructor TDigIt_Source_Folder.Destroy;
begin
  inherited Destroy;

  xFiles.Free;
  rParams.Free;
end;

function TDigIt_Source_Folder.Flags: DWord; stdcall;
begin
  Result:= DigIt_Source_TakeData_PICTUREFILE;
end;

function TDigIt_Source_Folder.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Folder.Enabled(AEnabled: Boolean): Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Folder.Release: Boolean; stdcall;
begin
  Free;
  Result:= True;
end;

function TDigIt_Source_Folder.Params: IDigIt_Params; stdcall;
begin
  Result:= rParams;
end;

function TDigIt_Source_Folder.UI_Title(const AUI_Title: PChar): Integer; stdcall;
begin
  StrPCopy(AUI_Title, DigIt_Source_Folder_NameL);
  Result:= Length(AUI_Title);
end;

function TDigIt_Source_Folder.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 3;
end;

(*
function TDigIt_Source_Folder.Preview(MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;
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
        StrPLCopy(PChar(AData), rParams.Folder+DirectorySeparator+xFiles[lastFile+1], MaxDataSize);
        Result:= Length(PChar(AData));
       end;
end;
*)
function TDigIt_Source_Folder.Take(takeAction: DigIt_Source_TakeAction; MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;
begin
  Result:= 0;
  if (lastFolder<>rParams.Folder)
  then xFiles.Clear;

  if (xFiles.Count=0) then
  begin
    SearchOnPath(xFiles, rParams.Folder, '*.*', faAnyFile, False);
    lastFolder:= rParams.Folder;
    lastFile :=xFiles.IndexOf(rParams.LastTaked);

    if (takeAction = takeActReTake) and
       (lastFile=-1) then
    begin
      lastFile:=0;
      rParams.LastTaked :=xFiles[lastFile];
    end;
  end;

  Case takeAction of
  takeActPreview: begin
    if (xFiles.Count=0) or ((lastFile+1)>=xFiles.Count)
    then Result:= 0
    else begin
          StrPLCopy(PChar(AData), rParams.Folder+DirectorySeparator+xFiles[lastFile+1], MaxDataSize);
          Result:= Length(PChar(AData));
         end;
  end;
  takeActTake: begin
    if (xFiles.Count=0) or ((lastFile+1)>=xFiles.Count)
    then Result :=0
    else begin
           Inc(lastFile);
           rParams.LastTaked:=xFiles[lastFile];

           StrPLCopy(PChar(AData), rParams.Folder+DirectorySeparator+rParams.LastTaked, MaxDataSize);
           Result:= Length(PChar(AData));
         end;
  end;
  takeActReTake: begin
    if (xFiles.Count=0) or (lastFile=xFiles.Count)
    then Result :=0
    else begin
           StrPLCopy(PChar(AData), rParams.Folder+DirectorySeparator+rParams.LastTaked, MaxDataSize);
           Result:= Length(PChar(AData));
         end;
  end;
  end;
end;
(*
function TDigIt_Source_Folder.ReTake(MaxDataSize: DWord; const AData: Pointer): DWord; stdcall;
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
         StrPLCopy(PChar(AData), rParams.Folder+DirectorySeparator+rParams.LastTaked, MaxDataSize);
         Result:= Length(PChar(AData));
       end;
end;
*)
initialization
  try
     Source_Folder:= TDigIt_Source_Folder.Create;
     theBridge.Sources.Register(DigIt_Source_Folder_Name, Source_Folder);
  except
  end;

end.

