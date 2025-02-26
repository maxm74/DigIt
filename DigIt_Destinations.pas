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
  BGRABitmapTypes, DigIt_Destination_SaveFiles_SettingsForm
  {$ifdef WINDOWS}
  {$endif}
  {$ifdef LINUX}
  {$endif}
  ;

implementation

end.

