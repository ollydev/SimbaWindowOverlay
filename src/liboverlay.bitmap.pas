unit liboverlay.bitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows,
  liboverlay.Types;

type
  POverlayBitmap = ^TOverlayBitmap;

  TOverlayBitmap = class
  private
    FFonts: array of record Name: String; Size: Int32; Handle: HFONT; Bold: Int32; Quaility: Int32; end;
    FDC: HDC;
    FBitmap: HBitmap;
    FData: PRGB32;
    FLeft, FTop: Int32;
    FWidth, FHeight: Int32;
    FDrawing: Boolean;

    function SetFont(Name: String; Size: Int32; Bold, Smooth: Boolean): HGDIOBJ;
  public
    property Data: PRGB32 read FData;
    property DC: HDC read FDC;
    property Left: Int32 read FLeft;
    property Top: Int32 read FTop;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;
    property Drawing: Boolean read FDrawing write FDrawing;

    procedure Draw(P: TPoint; FilePath: PChar); overload;
    procedure Draw(P: TPoint; Src: PRGB32; SrcW, SrcH: Int32); overload;
    procedure Draw(P: TPoint; BMP: TOverlayBitmap); overload;
    procedure DrawLine(Start, Stop: TPoint; Color: Int32);
    procedure DrawCircle(P: TPoint; Radius: Int32; Fill: Boolean; Color: Int32);
    procedure DrawBox(Box: TBox; Fill: Boolean; Color: Int32);
    procedure DrawTPARainbow(TPA: TPointArray);
    procedure DrawTPA(TPA: TPointArray; Color: Int32);
    procedure DrawATPA(ATPA: T2DPointArray);
    procedure DrawText(Text, Font: String; Size: Int32; Bold, Smooth: Boolean; Position: TPoint; Color: Int32); overload;
    procedure DrawText(Text, Font: String; Size: Int32; Bold, Smooth: Boolean; WordBreak: Boolean; Box: TBox; Color: Int32); overload;

    procedure CalculateText(Text, Font: String; Size: Int32; Bold: Boolean; var W, H: Int32); overload;
    procedure CalculateText(Text, Font: String; Size: Int32; Bold: Boolean; WordBreak: Boolean; var Box: TBox); overload;

    procedure Clear(Color: Int32 = 0); overload;
    procedure Clear(Box: TBox; Color: Int32 = 0); overload;

    procedure SetPixel(X, Y: Int32; Color: Int32); inline;
    function GetPixel(X, Y: Int32): Int32; inline;

    constructor Create(ALeft, ATop, AWidth, AHeight: Int32);
    constructor Create(FilePath: PChar);
    destructor Destroy; override;
  end;

  POverlayBitmaps = ^TOverlayBitmaps;
  TOverlayBitmaps = array of TOverlayBitmap;

implementation

procedure TOverlayBitmap.DrawCircle(P: TPoint; Radius: Int32; Fill: Boolean; Color: Int32);
var
  x, y: Integer;
  min, max, dist: Single;
  B: TBox;
  RGB: TRGB32;
begin
  RGB := ColorToRGB(Color);

  max := Trunc(Sqr(Radius + 0.5));
  min := Trunc(Sqr(Radius - 0.5));
  if Fill then
    min := 0;

  B.X1 := p.x - Radius;
  if (B.X1 < 0) then
    B.X1 := 0;
  B.Y1 := p.y - Radius;
  if (B.Y1 < 0) then
    B.Y1 := 0;
  B.X2 := p.x + Radius;
  if (B.X2 >= FWidth) then
    B.X2 := FWidth - 1;
  B.Y2 := p.y + Radius;
  if (B.Y2 >= FWidth) then
    B.Y2 := FHeight - 1;

  for y := B.Y1 to B.Y2 do
    for x := B.X1 to B.X2 do
    begin
      dist := Sqr(x - p.x) + Sqr(y - p.y);
      if (dist >= min) and (dist < max) then
        FData[y * FWidth + x] := RGB;
    end;
end;

procedure TOverlayBitmap.DrawBox(Box: TBox; Fill: Boolean; Color: Int32);
var
  RGB: TRGB32;
  y: Int32;
begin
  RGB := ColorToRGB(Color);

  if Fill then
  begin
    for y := Box.Y1 to Box.Y2 do
      FillDWord(FData[y * FWidth + Box.X1], (Box.X2 - Box.X1) + 1, Int32(RGB));
  end else
  begin
    FillDWord(FData[Box.Y1 * FWidth + Box.X1], (Box.X2 - Box.X1) + 1, Int32(RGB));
    FillDWord(FData[Box.Y2 * FWidth + Box.X1], (Box.X2 - Box.X1) + 1, Int32(RGB));

    for y := Box.Y1 to Box.Y2 do
    begin
      FData[y * FWidth + Box.X1] := RGB;
      FData[y * FWidth + Box.X2] := RGB;
    end;
  end;
end;

procedure TOverlayBitmap.DrawTPARainbow(TPA: TPointArray);
var
  Color: TRGB32;
  fraction, m: Extended;
  i, len: Int32;
begin
  len := Length(TPA);

  for i := 0 to len - 1 do
    if (TPA[i].x >= 0) and (TPA[i].y >= 0) and (TPA[i].x < FWidth) and (TPA[i].y < FHeight) then
    begin
      fraction := i / len;

      if (fraction <= 0) then
        m := 0
      else if (fraction >= 1) then
        m := 6
      else
        m := fraction * 6;

      case Trunc(m) of
        0:
          begin
            Color.R := $FF;
            Color.G := round(frac(m) * $FF);
            Color.B := 0;
          end;
        1:
          begin
            Color.R := $FF - round(frac(m) * $FF);
            Color.G := $FF;
            Color.B := 0;
          end;
        2:
          begin
            Color.R := 0;
            Color.G := $FF;
            Color.B := round(frac(m) * $FF);
          end;
        3:
          begin
            Color.R := 0;
            Color.G := $FF - round(frac(m) * $FF);
            Color.B := $FF;
          end;
        4:
          begin
            Color.R := round(frac(m) * $FF);
            Color.G := 0;
            Color.B := $FF;
          end;
        5:
          begin
            Color.R := $FF;
            Color.G := 0;
            Color.B := $FF - round(frac(m) * $FF);
          end;
      end;

      FData[TPA[i].y * FWidth + TPA[i].x] := Color;
    end;
end;

procedure TOverlayBitmap.DrawTPA(TPA: TPointArray; Color: Int32);
var
  i: Int32;
  RGB: TRGB32;
begin
  RGB := ColorToRGB(Color);

  for i := 0 to High(TPA) do
    if (TPA[i].X >= 0) and (TPA[i].Y >= 0) and (TPA[i].X < FWidth) and (TPA[i].Y < FHeight) then
      FData[TPA[i].Y * FWidth + TPA[i].X] := RGB;
end;

procedure TOverlayBitmap.DrawATPA(ATPA: T2DPointArray);
var
  i: Int32;
begin
  for i := 0 to High(ATPA) do
    DrawTPA(ATPA[i], Random($FFFFFF));
end;

function TOverlayBitmap.SetFont(Name: String; Size: Int32; Bold, Smooth: Boolean): HGDIOBJ;
var
  i: Int32;
  Quality, isBold: Int32;
begin
  if Smooth then
    Quality := ANTIALIASED_QUALITY
  else
    Quality := NONANTIALIASED_QUALITY;

  if Bold then
    isBold := FW_BOLD
  else
    isBold := FW_NORMAL;

  for i := 0 to High(FFonts) do
    if (FFonts[i].Name = Name) and (FFonts[i].Size = Size) and (FFonts[i].Quaility = Quality) and (FFonts[i].Bold = isBold) then
      Exit(SelectObject(FDC, FFonts[i].Handle));

  SetLength(FFonts, Length(FFonts) + 1);

  FFonts[High(FFonts)].Bold := isBold;
  FFonts[High(FFonts)].Quaility := Quality;
  FFonts[High(FFonts)].Size := Size;
  FFonts[High(FFonts)].Name := Name;
  FFonts[High(FFonts)].Handle := CreateFont(-Trunc((Size * GetDeviceCaps(FDC, LOGPIXELSY) / 72) + 0.5), 0, 0, 0, FFonts[High(FFonts)].Bold, 0, 0 , 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, FFonts[High(FFonts)].Quaility, DEFAULT_PITCH, PChar(Name));

  Result := SelectObject(FDC, FFonts[High(FFonts)].Handle);
end;

procedure TOverlayBitmap.Draw(P: TPoint; FilePath: PChar);
var
  BMP: TOverlayBitmap;
begin
  BMP := TOverlayBitmap.Create(FilePath);
  Draw(P, BMP.Data, BMP.Width, BMP.Height);
  BMP.Free();
end;

procedure TOverlayBitmap.Draw(P: TPoint; Src: PRGB32; SrcW, SrcH: Int32);
var
  Y, W, H: Int32;
begin
  if (P.X < 0) or (P.Y < 0) or (P.X >= FWidth) or (P.Y >= FHeight) then
    Exit;

  W := SrcW;
  if (P.X + SrcW) > FWidth then
    W := FWidth - P.X;
  H := P.Y + SrcH;
  if (H > FHeight) then
    H := FHeight;

  for Y := P.Y to H - 1 do
    Move(Src[(Y - P.Y) * SrcW], FData[Y * FWidth + P.X], W * SizeOf(TRGB32));
end;

procedure TOverlayBitmap.Draw(P: TPoint; BMP: TOverlayBitmap);
begin
  Draw(P, BMP.Data, BMP.Width, BMP.Height);
end;

procedure TOverlayBitmap.DrawLine(Start, Stop: TPoint; Color: Int32);
var
  TPA: TPointArray;
begin
  TPALine(TPA, Start, Stop);

  DrawTPA(TPA, Color);
end;

procedure TOverlayBitmap.DrawText(Text, Font: String; Size: Int32; Bold, Smooth: Boolean; Position: TPoint; Color: Int32);
var
  R: TRect;
  F: HGDIOBJ;
begin
  F := SetFont(Font, Size, Bold, Smooth);

  SetBkMode(FDC, TRANSPARENT);
  SetTextColor(FDC, Color);

  R.Left := Position.X;
  R.Top := Position.Y;
  R.Right := $FFFFFF;
  R.Bottom := $FFFFFF;

  Windows.DrawText(FDC, PChar(Text), Length(Text), R, DT_LEFT);

  SelectObject(FDC, F);
end;

procedure TOverlayBitmap.DrawText(Text, Font: String; Size: Int32; Bold, Smooth: Boolean; WordBreak: Boolean; Box: TBox; Color: Int32);
var
  R: TRect;
  F: HGDIOBJ;
begin
  F := SetFont(Font, Size, Bold, Smooth);

  SetBkMode(FDC, TRANSPARENT);
  SetTextColor(FDC, Color);

  R.Left := Box.X1;
  R.Top := Box.Y1;
  R.Right := Box.X2;
  R.Bottom := Box.Y2;

  if WordBreak then
    Windows.DrawText(FDC, PChar(Text), Length(Text), R, DT_LEFT or DT_WORDBREAK)
  else
    Windows.DrawText(FDC, PChar(Text), Length(Text), R, DT_LEFT);

  SelectObject(FDC, F);
end;

procedure TOverlayBitmap.CalculateText(Text, Font: String; Size: Int32; Bold: Boolean; var W, H: Int32);
var
  F: HGDIOBJ;
  S: TSize;
begin
  F := SetFont(Font, Size, Bold, False);
  S := Default(TSize);
  GetTextExtentPoint(FDC, PChar(Text), Length(Text), S);

  W := S.Width;
  H := S.Height;

  SelectObject(FDC, F);
end;

procedure TOverlayBitmap.CalculateText(Text, Font: String; Size: Int32; Bold: Boolean; WordBreak: Boolean; var Box: TBox);
var
  F: HGDIOBJ;
  R: TRect;
begin
  F := SetFont(Font, Size, Bold, False);

  R.Left := Box.X1;
  R.Top := Box.Y1;
  R.Right := Box.X2;
  R.Bottom := Box.Y2;

  if WordBreak then
    Windows.DrawText(FDC, PChar(Text), Length(Text), R, DT_LEFT or DT_CALCRECT or DT_WORDBREAK)
  else
    Windows.DrawText(FDC, PChar(Text), Length(Text), R, DT_LEFT or DT_CALCRECT);

  Box.X1 := R.Left;
  Box.Y1 := R.Top;
  Box.X2 := R.Right;
  Box.Y2 := R.Bottom;

  SelectObject(FDC, F);
end;

procedure TOverlayBitmap.Clear(Color: Int32);
begin
  FillDWord(FData[0], FWidth * FHeight, Int32(ColorToRGB(Color)));
end;

procedure TOverlayBitmap.Clear(Box: TBox; Color: Int32);
begin
  DrawBox(Box, True, Color);
end;

procedure TOverlayBitmap.SetPixel(X, Y: Int32; Color: Int32);
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    raise Exception.Create(Format('Pixel(%d, %d) out of bounds',[X, Y]));

  FData[Y * FWidth + X] := ColorToRGB(Color);
end;

function TOverlayBitmap.GetPixel(X, Y: Int32): Int32;
begin
  if (X < 0) or (Y < 0) or (X >= FWidth) or (Y >= FHeight) then
    raise Exception.Create(Format('Pixel(%d, %d) out of bounds',[X, Y]));

  Result := RGBToColor(FData[Y * FWidth + X]);
end;

constructor TOverlayBitmap.Create(ALeft, ATop, AWidth, AHeight: Int32);
var
  bi: Windows.BITMAPINFO;
begin
  FDrawing := True;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
  FData := nil;

  ZeroMemory(@bi, SizeOf(BITMAPINFO));
  bi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  bi.bmiHeader.biWidth := AWidth;
  bi.bmiHeader.biHeight := -AHeight;
  bi.bmiHeader.biPlanes := 1;
  bi.bmiHeader.biBitCount := 32;
  bi.bmiHeader.biCompression := BI_RGB;
  bi.bmiHeader.biSizeImage := 0;
  bi.bmiHeader.biClrUsed := 0;

  FDC := Windows.CreateCompatibleDC(0);
  FBitmap := Windows.CreateDIBSection(FDC, bi, DIB_RGB_COLORS, FData, 0, 0);

  SelectObject(FDC, FBitmap);
end;

constructor TOverlayBitmap.Create(FilePath: PChar);
var
  bmp: HBitmap;
  bi: Windows.BITMAPINFO;
  Brush: HBRUSH;
  R: RECT;
begin
  if (not FileExists(FilePath)) then
    raise Exception.Create(Format('File "%s" not found', [FilePath]));
  if (ExtractFileExt(FilePath) <> '.bmp') then
    raise Exception.Create(Format('File "%s" isn''t a bitmap', [FilePath]));

  bmp := LoadImage(0, FilePath, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);
  GetObject(bmp, SizeOf(bi), @bi);

  R.Left := 0;
  R.Top := 0;
  R.Right := bi.bmiHeader.biWidth;
  R.Bottom := bi.bmiHeader.biHeight;

  Create(r.Left, r.Top, R.Right, R.Bottom);

  Brush := CreatePatternBrush(bmp);

  FillRect(FDC, R, Brush);

  DeleteObject(bmp);
  DeleteObject(Brush);
end;

destructor TOverlayBitmap.Destroy;
var
  i: Int32;
begin
  for i := 0 to High(FFonts) do
    DeleteObject(FFonts[i].Handle);

  DeleteObject(FBitmap);
end;

end.

