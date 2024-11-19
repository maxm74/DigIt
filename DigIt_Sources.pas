(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2024 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Internal Sources depending on the OS                                     **
*******************************************************************************)
unit DigIt_Sources;

{$mode ObjFPC}{$H+}

interface

uses
  {$ifdef WINDOWS}
    Digit_Source_WIA,
  //Digit_Source_Twain,
  {$endif}
  {$ifdef LINUX}
  {$endif}
  Digit_Source_Folder
  ;

implementation

end.

