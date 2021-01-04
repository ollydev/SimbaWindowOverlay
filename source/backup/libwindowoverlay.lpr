library libwindowoverlay;

{$mode objfpc}{$H+}

uses
  classes, sysutils, syncobjs,
  jwawintype, jwawinuser, jwawingdi, dwmapi;

{$I simbaplugin.inc}

function WindowRect(Handle: HWND): RECT;
begin
  Result := Default(RECT);

  if DwmCompositionEnabled() and (GetAncestor(Handle, GA_ROOT) = Handle) then
    DwmGetWindowAttribute(Handle, DWMWA_EXTENDED_FRAME_BOUNDS, @Result, SizeOf(Result))
  else
    GetWindowRect(Handle, Result);
end;

type
  PWindowOverlay = ^TWindowOverlay;
  TWindowOverlay = class(TThread)
  protected
    FOverlay: HWND;
    FTarget: HWND;
    FData: Pointer;
    FWidth: Int32;
    FHeight: Int32;
    FDC: HDC;
    FBitmap: HBITMAP;
    FEvent: TSimpleEvent;
    FUpdateCount: Int32;
    FOffset: TPoint;
    FPaintInterval: Int32;
    FMufasaBitmap: Pointer;

    procedure Execute; override;
  public
    property MufasaBitmap: Pointer read FMufasaBitmap;
    property Data: Pointer read FData;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;

    procedure Paint;
    procedure WaitInitialized;

    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create(Target: HWND; PaintInterval: Int32; AMufasaBitmap: Pointer);
    destructor Destroy; override;
  end;

threadvar
  OverlayWindow: HWND;
  OverlayTarget: HWND;

procedure Hook(hWinEventHook: HWINEVENTHOOK; event: DWORD; Window: HWND; idObject, idChild: LONG; idEventThread, dwmsEventTime: DWORD); stdcall;
var
  R: TRect;
begin
  case Event of
    EVENT_OBJECT_HIDE:
      begin
        if (Window = OverlayTarget) then
          SetWindowPos(OverlayWindow, GetForegroundWindow(), 0, 0, 0, 0, SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOSENDCHANGING or SWP_HIDEWINDOW);
      end;

    EVENT_OBJECT_SHOW:
      begin
        if (Window = OverlayTarget) then
          SetWindowPos(OverlayWindow, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOSENDCHANGING);
      end;

    EVENT_OBJECT_REORDER:
      begin
        if (GetForegroundWindow() = OverlayTarget) then
          SetWindowPos(OverlayWindow, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOSENDCHANGING or SWP_SHOWWINDOW)
        else
        if IsWindowVisible(OverlayWindow) and (GetForegroundWindow() <> OverlayTarget) and ((GetWindowLong(OverlayWindow, GWL_EXSTYLE) and WS_EX_TOPMOST) <> 0) then
          SetWindowPos(OverlayWindow, GetForegroundWindow(), 0, 0, 0, 0, SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOSENDCHANGING);
      end;

    EVENT_OBJECT_LOCATIONCHANGE:
      begin
        if (Window = OverlayTarget) then
        begin
          R := WindowRect(OverlayTarget);
          SetWindowPos(OverlayWindow, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOSENDCHANGING);
        end;
      end;
  end;
end;

constructor TWindowOverlay.Create(Target: HWND; PaintInterval: Int32; AMufasaBitmap: Pointer);
begin
  inherited Create(False);

  FEvent := TSimpleEvent.Create();
  FTarget := Target;
  FPaintInterval := PaintInterval;
  FMufasaBitmap := AMufasaBitmap;

  if (Win32MajorVersion >= 6) then
    InitDwmLibrary();
end;

destructor TWindowOverlay.Destroy;
begin
  PostMessage(FOverlay, WM_CLOSE, 0, 0);

  inherited Destroy();
end;

procedure TWindowOverlay.Execute;

  procedure CreateBuffer(W, H: Int32);
  var
    bi: BITMAPINFO;
  begin
    bi := Default(BITMAPINFO);
    bi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth := W;
    bi.bmiHeader.biHeight := -H;
    bi.bmiHeader.biPlanes := 1;
    bi.bmiHeader.biBitCount := 32;
    bi.bmiHeader.biCompression := BI_RGB;
    bi.bmiHeader.biSizeImage := 0;
    bi.bmiHeader.biClrUsed := 0;

    FData := nil;
    FDC := CreateCompatibleDC(0);
    FBitmap := CreateDIBSection(FDC, @bi, DIB_RGB_COLORS, @FData, 0, 0);

    SelectObject(FDC, FBitmap);
  end;

var
  WindowClass: WNDCLASSEX;
  R: RECT;
  Msg: TMsg;
  Root: HWND;
begin
  WindowClass := Default(WNDCLASSEX);
  with WindowClass do
  begin
    cbSize := SizeOf(WNDCLASSEX);
    style := 0;
    lpfnWndProc := @DefWindowProc;
    cbClsExtra := 0;
    cbWndExtra := 0;
    hInstance := System.HINSTANCE;
    hIcon := 0;
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := GetStockObject(BLACK_BRUSH);
    lpszMenuName := nil;
    lpszClassName := PChar('WindowOverlay');
  end;

  RegisterClassEx(WindowClass);

  R := WindowRect(FTarget);

  Root := GetAncestor(FTarget, GA_ROOT);

  if (Root <> FTarget) then // Target is a child window, so we will offset painting
  begin
    FOffset.X := WindowRect(FTarget).Left - WindowRect(Root).Left;
    FOffset.Y := WindowRect(FTarget).Top - WindowRect(Root).Top;

    FTarget := Root;
  end;

  FWidth := R.Right - R.Left;
  FHeight := R.Bottom - R.Top;

  OverlayTarget := FTarget;
  OverlayWindow := CreateWindowEx(
    WS_EX_LAYERED or WS_EX_TRANSPARENT,
    WindowClass.lpszClassName, WindowClass.lpszClassName,
    WS_POPUP or WS_SYSMENU or WS_OVERLAPPED,
    R.Left, R.Top,
    FWidth, FHeight,
    0, 0,
    System.HINSTANCE,
    nil
  );

  CreateBuffer(FWidth, FHeight);

  FOverlay := OverlayWindow;

  ShowWindow(OverlayWindow, SW_SHOW);

  if (FPaintInterval > 0) then
   SetTimer(OverlayWindow, 0, FPaintInterval, nil);

  SetLayeredWindowAttributes(OverlayWindow, 0, 0, LWA_COLORKEY);
  SetWinEventHook(EVENT_OBJECT_SHOW, EVENT_OBJECT_LOCATIONCHANGE, 0, @Hook, 0, 0, WINEVENT_OUTOFCONTEXT);

  Hook(0, EVENT_OBJECT_REORDER, FTarget, 0, 0, 0, 0);
  Hook(0, EVENT_OBJECT_LOCATIONCHANGE, FTarget, 0, 0, 0, 0);

  FEvent.SetEvent();

  while GetMessage(@Msg, 0, 0, 0) do
  begin
    case Msg.Message of
      WM_CLOSE: PostQuitMessage(0);
      WM_TIMER: Paint();
      else
      begin
        TranslateMessage(@Msg);
        DispatchMessage(@Msg);
      end;
    end;
  end;

  FEvent.Free();

  UnregisterClass(WindowClass.lpszClassName, WindowClass.hInstance);
  DeleteObject(FBitmap);
end;

procedure TWindowOverlay.Paint;
var
  WindowDC: HDC;
begin
  if (FUpdateCount = 0) then
  begin
    WindowDC := GetWindowDC(FOverlay);
    BitBlt(WindowDC, FOffset.X, FOffset.Y, FWidth, FHeight, FDC, 0, 0, SRCCOPY);
    ReleaseDC(FOverlay, WindowDC);
  end;
end;

procedure TWindowOverlay.WaitInitialized;
begin
  FEvent.WaitFor(INFINITE);
end;

procedure TWindowOverlay.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TWindowOverlay.EndUpdate;
begin
  Dec(FUpdateCount);
  if (FUpdateCount = 0) then
    Paint();
end;

procedure TWindowOverlay_Setup(const Params: PParamArray; const Result: Pointer); cdecl;
var
  Overlay: TWindowOverlay;
begin
  Overlay := TWindowOverlay.Create(PPtrUInt(Params^[0])^, PInt32(Params^[1])^, PPointer(Params^[2])^);
  Overlay.WaitInitialized();

  PPointer(Result)^ := Overlay;
end;

procedure TWindowOverlay_Free(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PWindowOverlay(Params^[0])^.Free();
end;

procedure TWindowOverlay_Data(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PPointer(Result)^ := PWindowOverlay(Params^[0])^.Data;
end;

procedure TWindowOverlay_Width(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := PWindowOverlay(Params^[0])^.Width;
end;

procedure TWindowOverlay_Height(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := PWindowOverlay(Params^[0])^.Height;
end;

procedure TWindowOverlay_BeginUpdate(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PWindowOverlay(Params^[0])^.BeginUpdate();
end;

procedure TWindowOverlay_EndUpdate(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PWindowOverlay(Params^[0])^.EndUpdate();
end;

procedure TWindowOverlay_Paint(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PWindowOverlay(Params^[0])^.Paint();
end;

procedure TWindowOverlay_MufasaBitmap(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PPointer(Result)^ := PWindowOverlay(Params^[0])^.MufasaBitmap;
end;

begin
  addGlobalType('type Pointer', 'TWindowOverlay');

  addGlobalFunc('function TWindowOverlay.InternalCreate(TargetWindow: PtrUInt; PaintInterval: Int32 = 100; Bitmap: TMufasaBitmap = nil): TWindowOverlay; static; native;', @TWindowOverlay_Setup);
  addGlobalFunc('procedure TWindowOverlay.Free; native;', @TWindowOverlay_Free);
  addGlobalFunc('function TWindowOverlay.Data: Pointer; native;', @TWindowOverlay_Data);
  addGlobalFunc('function TWindowOverlay.Width: Int32; native;', @TWindowOverlay_Width);
  addGlobalFunc('function TWindowOverlay.Height: Int32; native;', @TWindowOverlay_Height);
  addGlobalFunc('procedure TWindowOverlay.BeginUpdate; native;', @TWindowOverlay_BeginUpdate);
  addGlobalFunc('procedure TWindowOverlay.EndUpdate; native;', @TWindowOverlay_EndUpdate);
  addGlobalFunc('procedure TWindowOverlay.Paint; native;', @TWindowOverlay_Paint);
  addGlobalFunc('function TWindowOverlay.Bitmap: TMufasaBitmap; constref; native;', @TWindowOverlay_MufasaBitmap);

  addCode('procedure TWindowOverlay.Free; override;'                                                                       + LineEnding +
          'begin'                                                                                                          + LineEnding +
          '  Self.Bitmap.Free();'                                                                                          + LineEnding +
          ''                                                                                                               + LineEnding +
          '  inherited();'                                                                                                 + LineEnding +
          'end;'                                                                                                           + LineEnding +
          ''                                                                                                               + LineEnding +
          'function TWindowOverlay.Create(TargetWindow: PtrUInt = 0; PaintInterval: Int32 = 100): TWindowOverlay; static;' + LineEnding +
          'var'                                                                                                            + LineEnding +
          '  Bitmap: TMufasaBitmap;'                                                                                       + LineEnding +
          'begin'                                                                                                          + LineEnding +
          '  if TargetWindow = 0 then'                                                                                     + LineEnding +
          '    TargetWindow := GetNativeWindow();'                                                                         + LineEnding +
          ''                                                                                                               + LineEnding +
          '  Bitmap.Init(Client.GetMBitmaps());'                                                                           + LineEnding +
          '  Result := TWindowOverlay.InternalCreate(TargetWindow, PaintInterval, Bitmap);'                                + LineEnding +
          '  Bitmap.SetPersistentMemory(PtrUInt(Result.Data), Result.Width, Result.Height);'                               + LineEnding +
          'end;');
end.

