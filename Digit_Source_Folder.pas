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
  Classes, SysUtils, Digit_Bridge_Intf, DigIt_Source_Common;

const
  DigIt_Source_Folder_Name = 'Folder Pictures';

resourcestring
  DigIt_Source_Folder_NameL = 'Folder Pictures';
  rsFoldNotFound = 'Folder not found...'#13#10'%s';
  rsFoldSelect = 'Select a Folder';

type
  { TDigIt_Source_Folder_Params }
  TDigIt_Source_Folder_Params = class(TDigIt_Source_Common_Params)
  protected
    Folder: String;

    function GetSummary: String; override;

  public
    function GetFromUser: Boolean; stdcall; override;
    function Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; override;
    function Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall; override;

    function Select: Boolean; stdcall; override;
  end;

  { TDigIt_Source_Folder }
  TDigIt_Source_Folder = class(TDigIt_Source_Common, IDigIt_ArrayR_PChars)
  protected
    xFiles: TStringList; //A Citation

    function GetParamsClass: TDigIt_Source_Common_ParamsClass; override;

    function GetName: String; override;

  public
    constructor Create;
    destructor Destroy; override;

    //IDigIt_Interface Implementation
    function UI_ImageIndex: Integer; stdcall; override;

    //IDigIt_ROArray
    function GetCount: DWord; stdcall;
    function Get(const aIndex: DWord; out aData: PChar): Boolean; stdcall;

    //IDigIt_Source
                                                       //Take a Picture and returns FileName/s
    function Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType; out aData: Pointer): DWord; stdcall; override;
    procedure Clear; stdcall; override;
 end;


implementation

uses Digit_Bridge_Impl, BGRABitmapTypes,
     Controls, Dialogs, Laz2_XMLCfg,
     MM_FilesUtils;

var
   Source_Folder : TDigIt_Source_Folder = nil;

{ TDigIt_Source_Folder_Params }

function TDigIt_Source_Folder_Params.GetSummary: String;
begin
  Result:= 'Folder= '+Folder;
end;

function TDigIt_Source_Folder_Params.GetFromUser: Boolean; stdcall;
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

function TDigIt_Source_Folder_Params.Load(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

function TDigIt_Source_Folder_Params.Save(const xml_File: PChar; const xml_RootPath: PChar): Boolean; stdcall;
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

function TDigIt_Source_Folder_Params.Select: Boolean; stdcall;
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

{ TDigIt_Source_Folder }

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

function TDigIt_Source_Folder.GetParamsClass: TDigIt_Source_Common_ParamsClass;
begin
  Result:= TDigIt_Source_Folder_Params;
end;

function TDigIt_Source_Folder.GetName: String;
begin
  Result:= DigIt_Source_Folder_NameL;
end;

constructor TDigIt_Source_Folder.Create;
begin
  inherited Create;

  xFiles:= nil;
end;

destructor TDigIt_Source_Folder.Destroy;
begin
  if (xFiles <> nil) then xFiles.Free;

  inherited Destroy;
end;

function TDigIt_Source_Folder.UI_ImageIndex: Integer; stdcall;
begin
  Result:= 6;
end;

function TDigIt_Source_Folder.Take(takeAction: DigIt_Source_TakeAction; out aDataType: TDigItDataType;
                                   out aData: Pointer): DWord; stdcall;
begin
  Result:= inherited Take(takeAction, aDataType, aData);
  if (rParams = nil) then exit;

  if (xFiles <> nil) then xFiles.Free;

  xFiles:= GetFilesInDir((rParams as TDigIt_Source_Folder_Params).Folder, False,
                         faAnyFile, BGRARegisteredImageReaderExtension,
                         flsSortNatural, False, True);

  if (xFiles <> nil) and (xFiles.Count > 0) then
  Case takeAction of
    takeActPreview: Result:= 1;
    takeActTake:    Result:= xFiles.Count;
  end;

  if (Result = 1 )
  then begin
         aData:= StrNew(PChar(xFiles[0]));
         aDataType:= diDataType_FileName;
       end
  else begin
         aData:= Self as IDigIt_ArrayR_PChars;
         aDataType:= diDataType_FileNameArray;
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

