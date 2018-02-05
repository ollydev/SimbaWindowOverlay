library libWindowOverlay;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  liboverlay.Window, liboverlay.Bitmap, liboverlay.Types;

// Lape Wrappers \\

var
  Overlay: ^TWindowOverlay;
  OverlayTarget: PtrUInt;
  OverlayScriptThread: PtrUInt;

procedure OverlayCreate;
begin
  Overlay^ := TWindowOverlay.Create(OverlayTarget, OverlayScriptThread);
end;

procedure Lape_Overlay_Init(const Params: PParamArray); cdecl;
var
  Sync: TOverlaySync;
begin
  Overlay := PWindowOverlay(Params^[0]);
  OverlayTarget := PPtrUInt(Params^[1])^;
  OverlayScriptThread := GetCurrentThreadId();

  Sync := TOverlaySync(Params^[2]^);
  Sync(@OverlayCreate);
end;

procedure Lape_Overlay_Free(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.Free();
end;

procedure Lape_Overlay_Paint(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.Paint();
end;

procedure Lape_Overlay_PaintInterval(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.PaintInterval := PInt32(Params^[1])^;
end;

procedure Lape_Overlay_BeginUpdate(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.BeginUpdate();
end;

procedure Lape_Overlay_EndUpdate(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.EndUpdate();
end;

procedure Lape_Overlay_Add(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PPointer(Result)^ := PWindowOverlay(Params^[0])^.Add(PBox(Params^[1])^.X1, PBox(Params^[1])^.Y1, (PBox(Params^[1])^.X2 - PBox(Params^[1])^.X1), (PBox(Params^[1])^.Y2 - PBox(Params^[1])^.Y1));
end;

procedure Lape_Overlay_DrawMouse(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.DrawMouse := PBoolean(Params^[1])^;
end;

procedure Lape_Overlay_DrawMouseManual(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.DrawMouseManual := PBoolean(Params^[1])^;
end;

procedure Lape_Overlay_DrawMouseCursor(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.DrawMouseCursor := PBoolean(Params^[1])^;
end;

procedure Lape_Overlay_SetOnClick(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.OnClick := TOverlayClickEvent(Params^[1]^);
end;

procedure Lape_Overlay_SetOnKeyPress(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.OnKey := TOverlayKeyEvent(Params^[1]^);
end;

procedure Lape_Overlay_Bitmaps(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  POverlayBitmaps(Result)^ := PWindowOverlay(Params^[0])^.Bitmaps;
end;

procedure Lape_Overlay_AddMouseHistory(const Params: PParamArray); cdecl;
begin
  PWindowOverlay(Params^[0])^.AddMouseHistory(PInt32(Params^[1])^, PInt32(Params^[2])^, PBoolean(Params^[3])^);
end;

// procedure DrawText(Text, Font: String; Size: Int32; Smooth: Boolean; Color: Int32; Position: TPoint);
procedure Lape_Bitmap_DrawText(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PPoint(Params^[5])^, PInt32(Params^[6])^);
end;

// procedure DrawCircle(P: TPoint; Radius: Int32; Fill: Boolean; Color: Int32);
procedure Lape_Bitmap_DrawCircle(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawCircle(PPoint(Params^[1])^, PInt32(Params^[2])^, PBoolean(Params^[3])^, PInt32(Params^[4])^);
end;

// procedure DrawBox(Box: TBox; Fill: Boolean; Color: Int32);
procedure Lape_Bitmap_DrawBox(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawBox(PBox(Params^[1])^, PBoolean(Params^[2])^, PInt32(Params^[3])^);
end;

// procedure DrawTPA(TPA: TPointArray; Color: Int32);
procedure Lape_Bitmap_DrawTPA(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawTPA(PPointArray(Params^[1])^, PInt32(Params^[2])^);
end;

// procedure DrawATPA(ATPA: T2DPointArray);
procedure Lape_Bitmap_DrawATPA(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawATPA(P2DPointArray(Params^[1])^);
end;

// procedure Clear(Color: Int32); overload;
procedure Lape_Bitmap_Clear(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.Clear(PInt32(Params^[1])^);
end;

//procedure Clear(Box: TBox; Color: Int32); overload;
procedure Lape_Bitmap_ClearEx(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.Clear(PBox(Params^[1])^, PInt32(Params^[2])^);
end;

// procedure CalculateText(Text, Font: String; Size: Int32; var W, H: Int32);
procedure Lape_Bitmap_CalculateText(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.CalculateText(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^);
end;

// procedure DrawText(Text, Font: String; Size: Int32; Smooth: Boolean; WordBreak: Boolean; Box: TBox; Color: Int32);
procedure Lape_Bitmap_DrawTextEx(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawText(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PBoolean(Params^[5])^, PBox(Params^[6])^, PInt32(Params^[7])^);
end;

// procedure CalculateText(Text, Font: String; Size: Int32; WordBreak: Boolean; var Box: TBox)
procedure Lape_Bitmap_CalculateTextEx(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.CalculateText(PString(Params^[1])^, PString(Params^[2])^, PInt32(Params^[3])^, PBoolean(Params^[4])^, PBox(Params^[5])^);
end;

procedure Lape_Bitmap_Width(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := POverlayBitmap(Params^[0])^.Width;
end;

procedure Lape_Bitmap_Height(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := POverlayBitmap(Params^[0])^.Height;
end;

procedure Lape_Bitmap_Data(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PPointer(Result)^ := POverlayBitmap(Params^[0])^.Data;
end;

// procedure Draw(P: TPoint; Src: PRGB32; SrcW, SrcH: Int32);
procedure Lape_Bitmap_Draw(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.Draw(PPoint(Params^[1])^, PPointer(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

// procedure Draw(P: TPoint; FilePath: String);
procedure Lape_Bitmap_DrawEx(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.Draw(PPoint(Params^[1])^, PPointer(Params^[2])^);
end;

procedure Lape_Bitmap_Drawing(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.Drawing := PBoolean(Params^[1])^;
end;

// procedure DrawLine(Start, Stop: TPoint; Color: Int32);
procedure Lape_Bitmap_DrawLine(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawLine(PPoint(Params^[1])^, PPoint(Params^[2])^, PInt32(Params^[3])^);
end;

// procedure Draw(P: TPoint; BMP: TLayerBitmap);
procedure Lape_Bitmap_DrawExEx(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.Draw(PPoint(Params^[1])^, TOverlayBitmap(Params^[2]^));
end;

// procedure DrawTPARainbow(TPA: TPointArray);
procedure Lape_Bitmap_DrawTPARainbow(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.DrawTPARainbow(PPointArray(Params^[1])^);
end;

// procedure SetPixel(X, Y: Int32; Color: Int32);
procedure Lape_Bitmap_SetPixel(const Params: PParamArray); cdecl;
begin
  POverlayBitmap(Params^[0])^.SetPixel(PInt32(Params^[1])^, PInt32(Params^[2])^,  PInt32(Params^[3])^);
end;

// function GetPixel(X, Y: Int32): Int32;
procedure Lape_Bitmap_GetPixel(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := POverlayBitmap(Params^[0])^.GetPixel(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

// Plugin Exports \\

function GetPluginABIVersion: Int32; cdecl; export;
begin
  Result := 2;
end;

function GetFunctionCount: Int32; cdecl; export;
begin
  Result := 35;
end;

function GetFunctionInfo(Index: Int32; var Addr: Pointer; var Decl: PChar): Int32; cdecl; export;
begin
  case Index of
    0:
      begin
        Addr := @Lape_Overlay_Init;
        StrPCopy(Decl, 'procedure TWindowOverlay.Init(TargetWindow: PtrUInt; Sync: Pointer); native;');
      end;
    1:
      begin
        Addr := @Lape_Overlay_Free;
        StrPCopy(Decl, 'procedure TWindowOverlay.Free; native;');
      end;
    2:
      begin
        Addr := @Lape_Overlay_Paint;
        StrPCopy(Decl, 'procedure TWindowOverlay.Paint; native;');
      end;
    3:
      begin
        Addr := @Lape_Overlay_PaintInterval;
        StrPCopy(Decl, 'procedure TWindowOverlay.PaintInterval(Interval: Int32); native;');
      end;
    4:
      begin
        Addr := @Lape_Overlay_BeginUpdate;
        StrPCopy(Decl, 'procedure TWindowOverlay.BeginUpdate; native;');
      end;
    5:
      begin
        Addr := @Lape_Overlay_EndUpdate;
        StrPCopy(Decl, 'procedure TWindowOverlay.EndUpdate; native;');
      end;
    6:
      begin
        Addr := @Lape_Overlay_Add;
        StrPCopy(Decl, 'function TWindowOverlay.Add(Area: TBox): TOverlayBitmap; native;');
      end;
    7:
      begin
        Addr := @Lape_Overlay_SetOnClick;
        StrPCopy(Decl, 'procedure TWindowOverlay.SetOnClick(Event: TOverlayClickEvent); native;');
      end;
    8:
      begin
        Addr := @Lape_Overlay_SetOnKeyPress;
        StrPCopy(Decl, 'procedure TWindowOverlay.SetOnKeyPress(Toggle: TOverlayKeyEvent); native;');
      end;
    9:
      begin
        Addr := @Lape_Overlay_DrawMouse;
        StrPCopy(Decl, 'procedure TWindowOverlay.DrawMouse(Toggle: Boolean); native;');
      end;
    10:
      begin
        Addr := @Lape_Overlay_DrawMouseManual;
        StrPCopy(Decl, 'procedure TWindowOverlay.DrawMouseManual(Toggle: Boolean); native;');
      end;
    11:
      begin
        Addr := @Lape_Overlay_DrawMouseCursor;
        StrPCopy(Decl, 'procedure TWindowOverlay.DrawMouseCursor(Toggle: Boolean); native;');
      end;
    12:
      begin
        Addr := @Lape_Overlay_AddMouseHistory;
        StrPCopy(Decl, 'procedure TWindowOverlay.AddMouseHistory(X, Y: Int32; Manual: Boolean); constref; native;');
      end;
    13:
      begin
        Addr := @Lape_Overlay_Bitmaps;
        StrPCopy(Decl, 'function TWindowOverlay.Bitmaps: array of TOverlayBitmap; constref; native;');
      end;
    14:
      begin
        Addr := @Lape_Bitmap_DrawText;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawText(Text, Font: String; Size: Int32; Smooth: Boolean; Position: TPoint; Color: Int32); constref; native;');
      end;
    15:
      begin
        Addr := @Lape_Bitmap_DrawCircle;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawCircle(P: TPoint; Radius: Int32; Fill: Boolean; Color: Int32); constref; native;');
      end;
    16:
      begin
        Addr := @Lape_Bitmap_DrawBox;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawBox(Box: TBox; Fill: Boolean; Color: Int32); constref; native;');
      end;
    17:
      begin
        Addr := @Lape_Bitmap_DrawTPA;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawTPA(TPA: TPointArray; Color: Int32); constref; native;');
      end;
    18:
      begin
        Addr := @Lape_Bitmap_DrawATPA;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawATPA(ATPA: T2DPointArray); constref; native;');
      end;
    19:
      begin
        Addr := @Lape_Bitmap_Clear;
        StrPCopy(Decl, 'procedure TOverlayBitmap.Clear(Color: Int32 = 0); constref; overload; native;');
      end;
    20:
      begin
        Addr := @Lape_Bitmap_ClearEx;
        StrPCopy(Decl, 'procedure TOverlayBitmap.Clear(Box: TBox; Color: Int32 = 0); constref; overload; native;');
      end;
    21:
      begin
        Addr := @Lape_Bitmap_CalculateText;
        StrPCopy(Decl, 'procedure TOverlayBitmap.CalculateText(Text, Font: String; Size: Int32; var W, H: Int32); constref; overload; native;');
      end;
    22:
      begin
        Addr := @Lape_Bitmap_DrawTextEx;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawText(Text, Font: String; Size: Int32; Smooth: Boolean; WordBreak: Boolean; Box: TBox; Color: Int32); constref; overload; native;');
      end;
    23:
      begin
        Addr := @Lape_Bitmap_CalculateTextEx;
        StrPCopy(Decl, 'procedure TOverlayBitmap.CalculateText(Text, Font: String; Size: Int32; WordBreak: Boolean; var Box: TBox); constref; overload; native;');
      end;
    24:
      begin
        Addr := @Lape_Bitmap_Width;
        StrPCopy(Decl, 'function TOverlayBitmap.Width: Int32; constref; native;');
      end;
    25:
      begin
        Addr := @Lape_Bitmap_Height;
        StrPCopy(Decl, 'function TOverlayBitmap.Height: Int32; constref; native;');
      end;
    26:
      begin
        Addr := @Lape_Bitmap_Data;
        StrPCopy(Decl, 'function TOverlayBitmap.Data: PRGB32; constref; native;');
      end;
    27:
      begin
        Addr := @Lape_Bitmap_Draw;
        StrPCopy(Decl, 'procedure TOverlayBitmap.Draw(P: TPoint; Src: PRGB32; SrcW, SrcH: Int32); constref; overload; native;');
      end;
    28:
      begin
        Addr := @Lape_Bitmap_DrawEx;
        StrPCopy(Decl, 'procedure TOverlayBitmap.Draw(P: TPoint; FilePath: PChar); constref; overload; native;');
      end;
    29:
      begin
        Addr := @Lape_Bitmap_DrawExEx;
        StrPCopy(Decl, 'procedure TOverlayBitmap.Draw(P: TPoint; BMP: TOverlayBitmap); constref; overload; native;');
      end;
    30:
      begin
        Addr := @Lape_Bitmap_Drawing;
        StrPCopy(Decl, 'procedure TOverlayBitmap.SetDrawing(Toggle: Boolean); constref; native;');
      end;
    31:
      begin
        Addr := @Lape_Bitmap_DrawLine;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawLine(Start, Stop: TPoint; Color: Int32); constref; native;');
      end;
    32:
      begin
        Addr := @Lape_Bitmap_DrawTPARainbow;
        StrPCopy(Decl, 'procedure TOverlayBitmap.DrawTPARainbow(TPA: TPointArray); constref; native;');
      end;
    33:
      begin
        Addr := @Lape_Bitmap_SetPixel;
        StrPCopy(Decl, 'procedure TOverlayBitmap.SetPixel(X, Y: Int32; Color: Int32); constref; native;');
      end;
    34:
      begin
        Addr := @Lape_Bitmap_GetPixel;
        StrPCopy(Decl, 'function TOverlayBitmap.GetPixel(X, Y: Int32): Int32; constref; native;');
      end;
  end;

  Result := Index;
end;

function GetTypeCount(): Int32; cdecl; export;
begin
  Result := 8;
end;

function GetTypeInfo(Index: Int32; var Name, Def: PChar): Int32; cdecl; export;
begin
  case Index of
    0:
      begin
        StrPCopy(Name, 'TOverlayBitmap');
        StrPCopy(Def, 'type Pointer');
      end;
    1:
      begin
        StrPCopy(Name, 'TWindowOverlay');
        StrPCopy(Def, 'type Pointer');
      end;
    2:
      begin
        StrPCopy(Name, '_TOverlayClickEvent');
        StrPCopy(Def, 'procedure(Sender: TObject; X, Y: Int32; Button: Int32; var Block: Boolean)');
      end;
    3:
      begin
        StrPCopy(Name, 'TOverlayClickEvent');
        StrPCopy(Def, 'native(_TOverlayClickEvent, FFI_CDECL)');
      end;
    4:
      begin
        StrPCopy(Name, '_TOverlaySync');
        StrPCopy(Def, 'procedure(Method: Pointer)');
      end;
    5:
      begin
        StrPCopy(Name, 'TOverlaySync');
        StrPCopy(Def, 'native(_TOverlaySync, FFI_CDECL)');
      end;
    6:
      begin
        StrPCopy(Name, '_TOverlayKeyEvent');
        StrPCopy(Def, ' procedure(Sender: TObject; VirtualKey: Int32; S: Char; var Block: Boolean);');
      end;
    7:
      begin
        StrPCopy(Name, 'TOverlayKeyEvent');
        StrPCopy(Def, 'native(_TOverlayKeyEvent, FFI_CDECL)');
      end;
  end;

  Result := Index;
end;

procedure SetPluginMemManager(MemoryManager: TMemoryManager); cdecl; export;
begin
  SetMemoryManager(MemoryManager);
end;

procedure OnDetach; cdecl; export;
begin
  DestroyOverlays();
end;

exports GetPluginABIVersion;
exports GetFunctionCount;
exports GetFunctionInfo;
exports GetTypeCount;
exports GetTypeInfo;
exports SetPluginMemManager;
exports OnDetach;

begin
end.

