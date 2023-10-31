(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2023 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Common Types\Consts                                                      **
*******************************************************************************)

unit DigIt_Types;

{$mode objfpc}{$H+}

interface

uses SysUtils, ComCtrls;

type

  { TFileListItem }

  TFileListItem=class(TListItem)
  protected
    rFileName:String;

    procedure setFileName(AValue: String);
  public
    constructor Create(AOwner: TListItems; AFileName:String);

    property FileName:String read rFileName write setFileName;
  end;

const
  Config_XMLWork='digit.xml';
  Config_Options='digit.ini';

  XMLWork_Captured='CapturedFile/';

var
   ApplicationDir,
   ConfigDir,
   TempDir:String;

implementation

{ TFileListItem }

procedure TFileListItem.setFileName(AValue: String);
begin
  if rFileName=AValue then Exit;
  rFileName:=AValue;
  Caption:=ExtractFileName(AValue);
end;

constructor TFileListItem.Create(AOwner: TListItems; AFileName: String);
begin
  inherited Create(AOwner);
  setFileName(AFileName);
end;

initialization
   ApplicationDir :=ExtractFilePath(ParamStr(0));
   ConfigDir :=GetAppConfigDir(False);
   TempDir :=GetTempDir(False)+'DigIt'+DirectorySeparator;
   ForceDirectories(ConfigDir);
   ForceDirectories(TempDir);

end.

