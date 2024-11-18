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
  Digit_Source_Folder
  {$ifdef WINDOWS}
  //, Digit_Source_Twain
  , Digit_Source_WIA
  {$endif}
  {$ifdef LINUX}
  {$endif}
  ;

implementation

end.

