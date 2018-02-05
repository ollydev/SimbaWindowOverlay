unit liboverlay.types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PBox = ^TBox;
  TBox = record X1, Y1, X2, Y2: Int32; end;

  PPoint = ^TPoint;

  PPointArray = ^TPointArray;
  TPointArray = array of TPoint;

  P2DPointArray = ^T2DPointArray;
  T2DPointArray = array of TPointArray;

  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  PRGB32 = ^TRGB32;

  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

function ColorToRGB(Color: Int32): TRGB32; inline;
function RGBToColor(RGB: TRGB32): Int32; inline;

procedure TPALine(var TPA: TPointArray; const P1: TPoint; const P2: TPoint); inline;

implementation

function ColorToRGB(Color: Int32): TRGB32;
begin
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
end;

function RGBToColor(RGB: TRGB32): Int32;
begin
  Result := RGB.R or RGB.G shl 8 or RGB.B shl 16;
end;

// Slacky!
procedure TPALine(var TPA: TPointArray; const P1: TPoint; const P2: TPoint);
var
  dx,dy,step,I,H: Int32;
  rx,ry,x,y: Extended;
begin
  H := Length(TPA);
  if (P1.X = P2.X) and (P1.Y = P2.Y) then
  begin
    SetLength(TPA, H+1);
    TPA[H] := P1;
    Exit;
  end;

  dx := (P2.x - P1.x);
  dy := (P2.y - P1.y);
  if (Abs(dx) > Abs(dy)) then
    step := Abs(dx)
  else
    step := Abs(dy);
  SetLength(TPA, (H+step+1));

  rx := dx / step;
  ry := dy / step;
  x := P1.x;
  y := P1.y;

  TPA[H].X := P1.X;
  TPA[H].Y := P1.Y;
  for I:=1 to step do
  begin
    x := x + rx;
    y := y + ry;
    TPA[(H+i)].X := Round(x);
    TPA[(H+i)].Y := Round(y);
  end;
end;

end.

