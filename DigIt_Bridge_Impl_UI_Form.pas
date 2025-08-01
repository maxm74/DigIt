(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Bridge User Interfaces Implementation for Forms                          **
*******************************************************************************)

unit Digit_Bridge_Impl_UI_Form;

{$mode objfpc}{$H+}

interface

uses
  Forms,
  MM_Interface_MessageDlg, MM_MessageDlg_Impl_Dialogs,
  Digit_Bridge_Intf, Digit_Bridge_Impl;

implementation

initialization
  MessageDlg_Impl_Dialogs:= TMM_MessageDlg_Impl_Dialogs.Create;
  theBridge.SetMsgInterface(MessageDlg_Impl_Dialogs as IMM_MessageDlg);

finalization
  if (MessageDlg_Impl_Dialogs<>nil) then MessageDlg_Impl_Dialogs.Free;

end.

