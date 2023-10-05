unit ImagesCutter_types;

{$mode objfpc}{$H+}

interface

uses Classes, Graphics;

const
                 //v,h - A,B,C
  PaperSizes :array['A'..'C', 0..10] of TPoint =
    (
      (//A
       (x:841; y:1189), (x:594; y:841), (x:420; y:594), (x:297; y:420), (x:210; y:297),
       (x:210; y:297), (x:841; y:1189), (x:594; y:841), (x:420; y:594), (x:297; y:420), (x:210; y:297)
      ),
      (//B
       (x:841; y:1189), (x:594; y:841), (x:420; y:594), (x:297; y:420), (x:210; y:297),
       (x:210; y:297), (x:841; y:1189), (x:594; y:841), (x:420; y:594), (x:297; y:420), (x:210; y:297)
      ),
      (//C
       (x:841; y:1189), (x:594; y:841), (x:420; y:594), (x:297; y:420), (x:210; y:297),
       (x:210; y:297), (x:841; y:1189), (x:594; y:841), (x:420; y:594), (x:297; y:420), (x:210; y:297)
      )
     );

type
  TImgCutter_UnitType = (utPixels=0, utMM=1);
  PImgCutter_Box =^TImgCutter_Box;
  TImgCutter_Box = record
    UnitType :TImgCutter_UnitType;
//    Left, Top, Right, Bottom :Longint;
    //BoxList :Byte;   //0..32
  end;

var
   BoxLists :array[0..32] of TFPList;

implementation

initialization
   fillchar(BoxLists, sizeof(BoxLists), 0);

end.

