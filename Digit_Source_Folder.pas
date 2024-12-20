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
  { TDigIt_Source_Folder }
  TDigIt_Source_Folder = class(TNoRefCountObject, IDigIt_Params, IDigIt_ROArray, IDigIt_Source)
  protected
    xFiles : TStringList; //A Citation
    lastFile: Integer;
    lastFolder,
    Folder,
    LastTaked :String;

  public
    constructor Create;
    destructor Destroy; override;

    //IDigIt_Interface
    function Flags: TDigItInterfaceKind; stdcall;
    function Init: Boolean; stdcall;
    function Release: Boolean; stdcall;
    function Enabled: Boolean; stdcall;
    function setEnabled(AEnabled: Boolean): Boolean; stdcall;

    //function RegisterName: PChar; stdcall;
    function Params: IDigIt_Params; stdcall;
    function UI_Title(out AUI_Title: PChar): Integer; stdcall;
    function UI_ImageIndex: Integer; stdcall;

    //IDigIt_Params
    function GetFromUser: Boolean; stdcall;
    function Duplicate: IDigIt_Params; stdcall;
    function Load(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Save(const xml_File: PChar; const xml_RootPath: PChar):Boolean; stdcall;
    function Summary(const ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;

    //IDigIt_ROArray
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;

    //IDigIt_Source
    //Take a Picture and returns FileNames
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;

    procedure Clear; stdcall;
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

{ TDigIt_Source_Folder }

function TDigIt_Source_Folder.GetFromUser: Boolean; stdcall;
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

function TDigIt_Source_Folder.Duplicate: IDigIt_Params; stdcall;
begin
  Result:= nil;
end;

function TDigIt_Source_Folder.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

function TDigIt_Source_Folder.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

function TDigIt_Source_Folder.Summary(const ASummary: PChar): Integer; stdcall;
begin
  StrPCopy(ASummary, 'Folder= '+Folder+#13#10+'Taked= '+LastTaked);
  Result:= Length(ASummary);
end;

function TDigIt_Source_Folder.OnSet: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Folder.GetCount: DWord; stdcall;
begin
  Result:= xFiles.Count;
end;

function TDigIt_Source_Folder.Get(const aIndex: DWord; out aData: Pointer): Boolean; stdcall;
begin
  Result:= (aIndex < xFiles.Count);
  if Result
  then aData:= StrNew(PChar(Folder+DirectorySeparator+xFiles[aIndex]))
  else aData:= nil;
end;

constructor TDigIt_Source_Folder.Create;
begin
  inherited Create;

  xFiles :=TStringList.Create;
  lastFile :=-1;
  lastFolder :='';
end;

destructor TDigIt_Source_Folder.Destroy;
begin
  inherited Destroy;

  xFiles.Free;
end;

function TDigIt_Source_Folder.Flags: TDigItInterfaceKind; stdcall;
begin
  Result:= diSourceStd;
end;

function TDigIt_Source_Folder.Init: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Folder.Enabled: Boolean; stdcall;
begin
  Result:= True;
end;

function TDigIt_Source_Folder.setEnabled(AEnabled: Boolean): Boolean; stdcall;
begin
  //Always return True, Predefined Source cannot be disabled
  Result:= True;
end;

function TDigIt_Source_Folder.Release: Boolean; stdcall;
begin
  Free;
  Result:= True;
end;

function TDigIt_Source_Folder.Params: IDigIt_Params; stdcall;
begin
  Result:= Self;
end;

function TDigIt_Source_Folder.UI_Title(out AUI_Title: PChar): Integer; stdcall;
begin
  AUI_Title:= StrNew(PChar(DigIt_Source_Folder_NameL));
  Result:= Length(AUI_Title);
end;

function TDigIt_Source_Folder.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 6;
end;

function TDigIt_Source_Folder.Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType;
                                   out aData: Pointer): DWord; stdcall;
begin
  Result:= 0;
  aData:= nil;
  aDataType:= diDataType_FileName;

  (* old code
  if (lastFolder <> Folder) then xFiles.Clear;

  if (xFiles.Count = 0) then
  begin
    SearchOnPath(xFiles, Folder, '*.*', faAnyFile, False);
    xFiles.Sort;
    lastFolder:= Folder;
    lastFile:= xFiles.IndexOf(LastTaked);
    if (lastFile = -1) then
    begin
      lastFile:= 0;
      LastTaked:= xFiles[lastFile];
    end;
  end;

  Case takeAction of
  takeActPreview: begin
    if (xFiles.Count = 0) or ((lastFile+1) >= xFiles.Count)
    then Result:= 0
    else begin
           aData:= StrNew(PChar(Folder+DirectorySeparator+xFiles[lastFile+1]));
           Result:= 1;
         end;
  end;
  takeActTake: begin
    if (xFiles.Count = 0) or ((lastFile+1) >= xFiles.Count)
    then Result :=0
    else begin
           Inc(lastFile);
           LastTaked:= xFiles[lastFile];

           aData:= Self as IDigIt_ROArray;
           Result:= xFiles.Count;
         end;
  end;
  takeActReTake: begin
    xFiles.Clear;
    SearchOnPath(xFiles, Folder, '*.*', faAnyFile, False);
    lastFolder:= Folder;
    lastFile:= xFiles.IndexOf(LastTaked);
    if (lastFile = -1) then
    begin
      lastFile:= 0;
      LastTaked:= xFiles[lastFile];
    end;

    aData:= Self as IDigIt_ROArray;
    Result:= xFiles.Count;
  end;
  end; *)

  if (lastFolder <> Folder) then xFiles.Clear;

  if (xFiles.Count = 0) then
  begin
    SearchOnPath(xFiles, Folder, '*.*', faAnyFile, False);
    xFiles.Sort;
    lastFolder:= Folder;
  end;

  if (xFiles.Count > 0) then
  Case takeAction of
    takeActPreview: begin
       aData:= StrNew(PChar(Folder+DirectorySeparator+xFiles[0]));
       Result:= 1;
    end;
    takeActTake: begin
       aData:= Self as IDigIt_ROArray;
       Result:= xFiles.Count;
    end;
  end;
end;

procedure TDigIt_Source_Folder.Clear; stdcall;
begin
  LastTaked:= '';
  xFiles.Clear;
end;

initialization
  try
     Source_Folder:= TDigIt_Source_Folder.Create;
     theBridge.Sources.Register(DigIt_Source_Folder_Name, Source_Folder);
  except
  end;

end.

