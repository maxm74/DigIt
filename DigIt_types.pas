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
  TDigItCropMode = (
    diCropNull = -1, //Used during the creation phase
    diCropFull,      //All captured pages are processed in bulk as they are
    diCropCustom     //All captured pages are processed with the cut chosen the first time in preview
  );

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
  Config_XMLWork = 'digit.xml';
  Config_Options = 'digit.ini';

  XMLWork_Captured = 'CapturedFile/';
  XMLWork_PageSettings = 'PageSettings/';

var
   Path_Application,
   Path_Config,
   Path_Temp,
   Path_Pictures: String;

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
   Path_Application :=ExtractFilePath(ParamStr(0));
   Path_Config :=GetAppConfigDir(False);
   Path_Temp :=Path_Config+'tmp'+DirectorySeparator; //GetTempDir(False)+'DigIt'+DirectorySeparator;
   Path_Pictures :=GetUserDir+'Pictures'+DirectorySeparator+'DigIt'+DirectorySeparator;
   //ForceDirectories(Path_Config);
   ForceDirectories(Path_Temp);
   ForceDirectories(Path_Pictures);

end.

