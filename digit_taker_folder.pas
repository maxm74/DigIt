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
  Classes, SysUtils, Digit_Bridge;

type

  { TDigIt_Taker_Folder }
  TDigIt_Taker_Folder = class(TDigIt_Taker)
  protected
    xFiles :TStringList; //A Citation
    lastFile :Integer;
    lastFolder:String;

  public
    constructor Create(aParams :TPersistent); override;
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
  end;

  TDigIt_Taker_FolderParams = class(TPersistent)
  protected
    rFolder,
    rLastTaked :String;

  published
    property Folder:String read rFolder write rFolder;
    property LastTaked:String read rLastTaked write rLastTaked;
  end;

implementation

uses Dialogs, masks, BGRABitmapTypes;

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

{ TDigIt_Taker_Folder }

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
  Result :='Folder';
end;

class function TDigIt_Taker_Folder.Params_GetClass: TPersistentClass;
begin
  result :=TDigIt_Taker_FolderParams;
end;

class function TDigIt_Taker_Folder.UI_Title: String;
begin
  Result :='Folder Source'; { #todo 2 -oMaxM : Usare Risorse per la Traduzione }
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

initialization
  theBridge.Takers.Register(TDigIt_Taker_Folder.RegisterName, TDigIt_Taker_Folder);

end.

