(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
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
  rsFoldNotFound = 'Folder not found...'#13#10'%s';
  rsFoldSelect = 'Select a Folder';

type
  { TDigIt_Source_Folder }
  TDigIt_Source_Folder = class(TNoRefCountObject, IDigIt_Params, IDigIt_ArrayR_PChars, IDigIt_Source)
  protected
    xFiles: TStringList; //A Citation
    Folder: String;

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
    function Summary(out ASummary: PChar): Integer; stdcall;

    function OnSet: Boolean; stdcall;

    //IDigIt_ROArray
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;

    //IDigIt_Source
    //Take a Picture and returns FileNames
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall;

    procedure Clear; stdcall;
 end;


implementation

uses Digit_Bridge_Impl, BGRABitmapTypes,
     Controls, Dialogs, Laz2_XMLCfg,
     MM_FilesUtils;

var
   Source_Folder : TDigIt_Source_Folder = nil;

{ TDigIt_Source_Folder }

function TDigIt_Source_Folder.GetFromUser: Boolean; stdcall;
var
   openDialog: TSelectDirectoryDialog;

begin
  Result:= False;
  try
     openDialog:=TSelectDirectoryDialog.Create(nil);
     openDialog.InitialDir:= Folder;
     Result:= openDialog.Execute;
     if Result then
     begin
       Folder:= openDialog.FileName;
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
     XMLWork.Flush;

     Result:= True;

  finally
    XMLWork.Free;
  end;
end;

function TDigIt_Source_Folder.Summary(out ASummary: PChar): Integer; stdcall;
begin
  ASummary:= StrNew(PChar('Folder= '+Folder));
  Result:= Length(ASummary);
end;

function TDigIt_Source_Folder.OnSet: Boolean; stdcall;
begin
  repeat
    Result:= DirectoryExists(Folder);
    if not(Result) then
      Case QuestionDlg('DigIt '+DigIt_Source_Folder_NameL, Format(rsFoldNotFound, [Folder]),
                       mtError, [mrYes, rsFoldSelect, 'IsDefault',
                       mrRetry, mrAbort], 0) of
      mrYes: Result:= GetFromUser;
      mrAbort: break;
      end;
  until Result;
end;

function TDigIt_Source_Folder.GetCount: DWord; stdcall;
begin
  Result:= 0;
  if (xFiles <> nil) then Result:= xFiles.Count;
end;

function TDigIt_Source_Folder.Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;
begin
  aData:= nil;
  Result:= (xFiles <> nil) and (aIndex < xFiles.Count);

  if Result then
  try
     aData:= StrNew(PChar(xFiles[aIndex]));
     Result:= True;
  except
     Result:= False;
  end;
end;

constructor TDigIt_Source_Folder.Create;
begin
  inherited Create;

  xFiles:= nil;
end;

destructor TDigIt_Source_Folder.Destroy;
begin
  inherited Destroy;

  if (xFiles <> nil) then xFiles.Free;
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

  if (xFiles <> nil) then xFiles.Free;

  xFiles:= GetFilesInDir(Folder, False,
                         faAnyFile, BGRARegisteredImageReaderExtension,
                         flsSortNatural, False, True);

  if (xFiles <> nil) and (xFiles.Count > 0) then
  Case takeAction of
    takeActPreview: begin
       aData:= StrNew(PChar(xFiles[0]));
       Result:= 1;
       aDataType:= diDataType_FileName;
    end;
    takeActTake: begin
       Result:= xFiles.Count;

       if (Result = 1 )
       then aData:= StrNew(PChar(xFiles[0]))
       else aData:= Self as IDigIt_ArrayR_PChars;

       aDataType:= diDataType_FileNameArray;
    end;
  end;
end;

procedure TDigIt_Source_Folder.Clear; stdcall;
begin
  if (xFiles <> nil) then
  begin
    xFiles.Free;
    xFiles:= nil;
  end;
end;

initialization
  try
     Source_Folder:= TDigIt_Source_Folder.Create;
     theBridge.Sources.Register(DigIt_Source_Folder_Name, Source_Folder);
  except
  end;

end.

