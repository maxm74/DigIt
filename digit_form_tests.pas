unit digit_form_tests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Spin, ExtDlgs, LMessages;

type

  { TDigIt_Tests }

  TDigIt_Tests = class(TForm)
    btPreview: TSpeedButton;
    Button1: TButton;
    diaCalendar: TCalendarDialog;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    imgsMain: TImageList;
    ListBox1: TListBox;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    pgMain: TPageControl;
    rbTakeAt: TRadioButton;
    rbTakeEvery: TRadioButton;
    rbTimerOff: TRadioButton;
    btTake: TSpeedButton;
    btReTake: TSpeedButton;
    edTimerEvery: TSpinEdit;
    btTakeAt_Add: TSpeedButton;
    btTakeAt_Del: TSpeedButton;
    btTakeAt_Clear: TSpeedButton;
    Tests: TTabSheet;
    tbTimer: TTabSheet;
    tbCapture: TTabSheet;
    tbWorktop: TTabSheet;
    TakeTimer: TTimer;
    procedure btPreviewClick(Sender: TObject);
    procedure btTakeClick(Sender: TObject);
    procedure btReTakeClick(Sender: TObject);
    procedure btTakeAt_AddClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure KeyHandler(var Msg: TLMKey; var Handled: Boolean);
  end;

var
  DigIt_Tests: TDigIt_Tests;

implementation


{$R *.lfm}

uses MMT_Types, MMT_DeviceList, MMT_Device, MMT_Property;


{ TDigIt_Tests }

type

  { T_MMT_Test_Property }

  T_MMT_Test_Property = class(TInterfacedObject, I_MMT_Property)
     function getName: ShortString;
     function getUID_Part: MMT_UID_Part;
     function getValue: ShortString;
     procedure setValue(AValue: ShortString);
  end;

    { T_MMT_Test_Device }

    T_MMT_Test_Device = class(TInterfacedObject, I_MMT_Device)
     function getName: ShortString;
     function getUID_Part: MMT_UID_Part;

     function FindFirst: I_MMT_Property;
     function FindNext: I_MMT_Property;
     function Locate(AUID: MMT_UID_Part): I_MMT_Property;
  end;

    { T_MMT_Test_DeviceList }

    T_MMT_Test_DeviceList = class(TInterfacedObject, I_MMT_DeviceList)
    private
       rDeviceList :TStringList;

    public
       function getName: ShortString;
       function getUID_Part: MMT_UID_Part;

       function FindFirst: I_MMT_Device;
       function FindNext: I_MMT_Device;
       function Locate(AUID: MMT_UID_Part): I_MMT_Device;

       constructor Create;
       destructor Destroy; override;
  end;

{ T_MMT_Test_Property }

function T_MMT_Test_Property.getName: ShortString;
begin

end;

function T_MMT_Test_Property.getUID_Part: MMT_UID_Part;
begin

end;

function T_MMT_Test_Property.getValue: ShortString;
begin

end;

procedure T_MMT_Test_Property.setValue(AValue: ShortString);
begin

end;

function T_MMT_Test_Device.getName: ShortString;
begin
  result :='Test Device';
end;

function T_MMT_Test_Device.getUID_Part: MMT_UID_Part;
begin
  result :='01010202';
end;

function T_MMT_Test_Device.FindFirst: I_MMT_Property;
begin

end;

function T_MMT_Test_Device.FindNext: I_MMT_Property;
begin

end;

function T_MMT_Test_Device.Locate(AUID: MMT_UID_Part): I_MMT_Property;
begin

end;

{ T_MMT_Test_DeviceList }

function T_MMT_Test_DeviceList.getName: ShortString;
begin
   result :='Test Device List';
end;

function T_MMT_Test_DeviceList.getUID_Part: MMT_UID_Part;
begin
   result :='CFC0655C';
end;

function T_MMT_Test_DeviceList.FindFirst: I_MMT_Device;
begin
  Result :=T_MMT_Test_Device(rDeviceList.Objects[0]);
end;

function T_MMT_Test_DeviceList.FindNext: I_MMT_Device;
begin

end;

function T_MMT_Test_DeviceList.Locate(AUID: MMT_UID_Part): I_MMT_Device;
begin
  rDeviceList.IndexOfName(AUID);
end;

constructor T_MMT_Test_DeviceList.Create;
begin
  inherited Create;

  rDeviceList :=TStringList.Create;
  rDeviceList.AddPair('CA221', 'Capra');
  rDeviceList.AddPair('CA220', 'Cacocciula');
end;

destructor T_MMT_Test_DeviceList.Destroy;
begin
  rDeviceList.Free;

  inherited Destroy;
end;







procedure TDigIt_Tests.btPreviewClick(Sender: TObject);
begin
     if (Sender=nil)
     then pgMain.ActivePage :=tbWorktop;
end;

procedure TDigIt_Tests.btTakeClick(Sender: TObject);
begin
     if (Sender=nil)
     then pgMain.ActivePage :=tbCapture;
end;

procedure TDigIt_Tests.btReTakeClick(Sender: TObject);
begin
     if (Sender=nil)
     then pgMain.ActivePage :=tbCapture;
end;

procedure TDigIt_Tests.btTakeAt_AddClick(Sender: TObject);
begin
     //
end;

procedure TDigIt_Tests.Button1Click(Sender: TObject);
begin

end;


procedure TDigIt_Tests.FormCreate(Sender: TObject);
begin
     Application.OnShortcut:=@KeyHandler;
     pgMain.ActivePage :=tbWorktop;
end;


procedure TDigIt_Tests.KeyHandler(var Msg: TLMKey; var Handled: Boolean);
begin
     //Dialogs.MessageDlg('Key', IntToHex(Msg.CharCode, 8), mtInformation, [mbOk], '');
     case Msg.CharCode of
     $074 :begin
                btPreviewClick(nil);
                Handled :=True;
          end;
     $075 :begin
                btTakeClick(nil);
                Handled :=True;
          end;
     $076 :begin
                btRetakeClick(nil);
                Handled :=True;
          end;
     end;
end;

end.

