(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Internal Destinations depending on the OS                                **
*******************************************************************************)
unit DigIt_Destinations;

{$mode ObjFPC}{$H+}

interface

uses
  DigIt_Destination_SaveFiles_SettingsForm
  {$ifdef WINDOWS}
  {$endif}
  {$ifdef LINUX}
  {$endif}
  ;

{ #note 10 -oMaxM : This function is temporary and should be removed when SaveFiles is implemented as a descendant of IDigIt_Destination }
function Destination_SaveFiles_Settings_Execute(var ASaveExt, ASavePath: String): Boolean;

implementation

function Destination_SaveFiles_Settings_Execute(var ASaveExt, ASavePath: String): Boolean;
begin
  Result:= TDest_SaveFiles_Settings.Execute(ASaveExt, ASavePath);
end;

end.

