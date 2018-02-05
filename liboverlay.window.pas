unit liboverlay.window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JwaWindows,
  liboverlay.Bitmap, liboverlay.PaintThread;

type
  TOverlayClickEvent = procedure(Sender: TObject; X, Y: Int32; Button: Int32; var Block: Boolean); cdecl;
  TOverlayKeyEvent = procedure(Sender: TObject; VirtualKey: Int32; S: Char; var Block: Boolean); cdecl;
  TOverlaySync = procedure(Method: Pointer); cdecl;

  TWindowOverlay = class
  private
    FBitmaps: TOverlayBitmaps;
    FChildren: array of HWND;
    FPaintInterval: Int32;
    FPaintThread: TPaintThread;
    FWindow: HWND;
    FTarget: HWND;
    FOffset: POINT;
    FOnClick: TOverlayClickEvent;
    FOnKey: TOverlayKeyEvent;
    FWidth: Int32;
    FHeight: Int32;
    FScriptThread: HANDLE;
    FMouseHistory: array of record Point: TPoint; LifeSpan: UInt64; end;
    FDrawMousePath: Boolean;

    procedure SetPaintInterval(Interval: Int32);
  public
    property OnKey: TOverlayKeyEvent read FOnKey write FOnKey;
    property OnClick: TOverlayClickEvent read FOnClick write FOnClick;
    property Window: HWND read FWindow;
    property Target: HWND read FTarget;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;
    property PaintInterval: Int32 read FPaintInterval write SetPaintInterval;
    property Offset: POINT read FOffset;
    property DrawMousePath: Boolean read FDrawMousePath write FDrawMousePath;
    property Bitmaps: TOverlayBitmaps read FBitmaps;

    function ScriptActive: Boolean;

    procedure AddMouseHistory(X, Y: Int32);
    function Add(X, Y, AWidth, AHeight: Int32): TOverlayBitmap;

    procedure AddChild(Handle: HWND);
    function HasChild(Handle: HWND): Boolean;

    procedure SetLayered;
    procedure SetParent(Parent: HWND);
    procedure SetTransparentColor(Color: Int32);
    procedure SetBounds(Rect: RECT);
    procedure Show;
    procedure Hide;
    procedure Close;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Paint;

    constructor Create(ATargetWindow: HWND; AScriptThread: UInt32);
    destructor Destroy; override;
  end;

  PWindowOverlay = ^TWindowOverlay;

  function WindowRect(Handle: HWND): RECT;

  function GetOverlayHandle(Handle: HWND): HWND;
  function GetOverlay(Handle: HWND): TWindowOverlay;

  procedure DestroyOverlays;

implementation

uses
  DwmApi,
  liboverlay.Hooks, liboverlay.Types;

function GetOverlayHandle(Handle: HWND): HWND;
const
  CLASS_NAME = 'TWindowOverlay';
  CLASS_SIZE = 14;
var
  Arr: array[0..CLASS_SIZE + 1] of Char;
begin
  Handle := GetWindow(Handle, GW_ENABLEDPOPUP);
  if (Handle <> 0) and isWindow(Handle) and (GetClassName(Handle, @Arr[0], Length(Arr)) = CLASS_SIZE) and (UTF8Encode(Arr) = CLASS_NAME) then
    Exit(Handle);

  Exit(0);
end;

function GetOverlay(Handle: HWND): TWindowOverlay;
var
  Ptr: Pointer;
begin
  Handle := GetOverlayHandle(Handle);
  if (Handle > 0) then
  begin
    Ptr := Pointer(PtrUInt(GetWindowLongPtr(Handle, GWL_USERDATA)));
    if (not IsBadReadPtr(Ptr, SizeOf(Pointer))) then // Make sure it's in our memory space
      Exit(TWindowOverlay(Ptr));
  end;

  Exit(nil);
end;

function DestroyOverlay(Handle: HWND; Param: LPARAM): BOOL; stdcall;
var
  Overlay: TWindowOverlay;
begin
  Overlay := GetOverlay(Handle);
  if (Overlay <> nil) then
    Overlay.Close();

  Exit(True);
end;

procedure DestroyOverlays;
begin
  EnumWindows(@DestroyOverlay, 0);
end;

function GetChildren(Handle: HWND; Param: LPARAM): BOOL; stdcall;
begin
  PWindowOverlay(PtrUInt(Param))^.addChild(Handle);

  Exit(True);
end;

function WindowRect(Handle: HWND): RECT;
begin
  Result := Default(RECT);

  if (Win32MajorVersion >= 6) and (DwmCompositionEnabled) and (GetAncestor(Handle, GA_ROOT) = Handle) then
    DwmGetWindowAttribute(Handle, DWMWA_EXTENDED_FRAME_BOUNDS, @Result, SizeOf(Result))
  else
    GetWindowRect(Handle, Result);
end;

function WindowProc(Handle: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if (Message = WM_ACTIVATEAPP) and (not IsWindow(GetWindowLongPtr(Handle, GWL_HWNDPARENT))) then // seems to catch target close
    PostMessage(Handle, WM_CLOSE, 0, 0);

  if (Message = WM_SHOWWINDOW) and (lParam = SW_PARENTCLOSING) then
    Result := 0
  else
    Result := DefWindowProc(Handle, Message, wParam, lParam);
end;

function TWindowOverlay.ScriptActive: Boolean;
var
  Code: DWord = 0;
begin
  Result := GetExitCodeThread(FScriptThread, Code) and (Code = STILL_ACTIVE);
end;

procedure TWindowOverlay.AddMouseHistory(X, Y: Int32);
var
  i: Int32;
begin
  if (Length(FMouseHistory) > 0) and ((Abs(FMouseHistory[0].Point.X - X) + Abs(FMouseHistory[0].Point.Y - Y)) < 10) then
    Exit;

  if (Length(FMouseHistory) < 150) then
    SetLength(FMouseHistory, Length(FMouseHistory) + 1);

  for i := High(FMouseHistory) downto 1 do
    FMouseHistory[i] := FMouseHistory[i - 1];

  FMouseHistory[0].Point.X := X;
  FMouseHistory[0].Point.Y := Y;
  FMouseHistory[0].LifeSpan := GetTickCount64() + 1250;
end;

function TWindowOverlay.Add(X, Y, AWidth, AHeight: Int32): TOverlayBitmap;
begin
  Result := TOverlayBitmap.Create(X, Y, AWidth, AHeight);

  SetLength(FBitmaps, Length(FBitmaps) + 1);
  FBitmaps[High(FBitmaps)] := Result;
end;

procedure TWindowOverlay.SetPaintInterval(Interval: Int32);
begin
  if (FPaintInterval = Interval) then
    Exit;
  FPaintInterval := Interval;

  if (FPaintThread <> nil) then
  begin
    FPaintThread.Terminate();
    FPaintThread.WaitFor();
    FPaintThread.Free();
    FPaintThread := nil;
  end;

  if (FPaintInterval > 0) then
    FPaintThread := TPaintThread.Create(FPaintInterval, @Paint);
end;

procedure TWindowOverlay.AddChild(Handle: HWND);
begin
  SetLength(FChildren, Length(FChildren) + 1);
  FChildren[High(FChildren)] := Handle;
end;

function TWindowOverlay.HasChild(Handle: HWND): Boolean;
var
  i: Int32;
begin
  for i := 0 to High(FChildren) do
    if (FChildren[i] = Handle) then
      Exit(True);

  Exit(False);
end;

procedure TWindowOverlay.SetLayered;
begin
  SetWindowLong(FWindow, GWL_EXSTYLE, WS_EX_LAYERED or WS_EX_TRANSPARENT);
end;

procedure TWindowOverlay.SetParent(Parent: HWND);
begin
  SetWindowLong(FWindow, GWL_HWNDPARENT, Parent);
end;

procedure TWindowOverlay.SetTransparentColor(Color: Int32);
begin
  SetLayeredWindowAttributes(FWindow, Color, 0, LWA_COLORKEY);
end;

procedure TWindowOverlay.SetBounds(Rect: RECT);
begin
  SetWindowPos(FWindow, 0, Rect.Left, Rect.Top, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

procedure TWindowOverlay.Show;
begin
  ShowWindow(FWindow, SW_SHOWNOACTIVATE);
end;

procedure TWindowOverlay.Hide;
begin
  ShowWindow(FWindow, SW_HIDE);
end;

procedure TWindowOverlay.Close;
begin
  SendMessage(FWindow, WM_CLOSE, 0, 0);
end;

procedure TWindowOverlay.BeginUpdate;
begin
  if (FPaintThread <> nil) then
    FPaintThread.Paused := True;
end;

procedure TWindowOverlay.EndUpdate;
begin
  if (FPaintThread <> nil) then
    FPaintThread.Paused := False;
end;

procedure TWindowOverlay.Paint;
var
  DC: HDC;
  i: Int32;
  T: UInt64;
  Path: TPointArray;
begin
  DC := GetWindowDC(FWindow);

  // Copy the window bitmap to our buffer
  Move(FBitmaps[0].Data[0], FBitmaps[1].Data[0], FWidth * FHeight * SizeOf(TRGB32));

  // Draw mouse history
  if FDrawMousePath and (FPaintInterval > 0) then
  begin
    T := GetTickCount64();
    for i := 0 to High(FMouseHistory) do
      if (T > FMouseHistory[i].LifeSpan) then
        Break;

    if (i > 1) then
    begin
      for i := 0 to i - 2 do
        TPALine(Path, FMouseHistory[i].Point, FMouseHistory[i + 1].Point);

      FBitmaps[1].DrawTPARainbow(Path);
    end;
  end;

  // Draw bitmaps
  for i := 2 to High(FBitmaps) do
    if FBitmaps[i].Drawing then
      FBitmaps[1].Draw(Classes.Point(FBitmaps[i].Left, FBitmaps[i].Top), FBitmaps[i]);

  BitBlt(DC, FOffset.X, FOffset.Y, FBitmaps[1].Width, FBitmaps[1].Height, FBitmaps[1].DC, 0, 0, SRCCOPY);

  ReleaseDC(FWindow, DC);
end;

constructor TWindowOverlay.Create(ATargetWindow: HWND; AScriptThread: UInt32);

  procedure CreateWindow;
  var
    WindowClass: WNDCLASS;
    R: RECT;
  begin
    with WindowClass do
    begin
      Style := 0;
      lpfnWndProc := @WindowProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := System.HINSTANCE;
      hIcon := 0;
      hCursor := LoadCursor(0, IDC_ARROW);
      hbrBackground := GetStockObject(BLACK_BRUSH);
      lpszMenuName := nil;
      lpszClassName := PChar(String(ClassName));
    end;

    RegisterClass(WindowClass);

    R := WindowRect(FTarget);

    FWidth := R.Right - R.Left;
    FHeight := R.Bottom - R.Top;
    FWindow := JwaWindows.CreateWindow(
                 PChar(String(ClassName)), PChar(String(ClassName)), WS_POPUP or WS_SYSMENU,
                 R.Left, R.Top, FWidth, FHeight, 0, 0, System.HINSTANCE, nil
               );

    SetWindowLongPtr(FWindow, GWL_USERDATA, PtrUInt(Self));
  end;

var
  Root: HWND;
begin
  FTarget := ATargetWindow;
  FPaintThread := nil;
  FPaintInterval := 0;
  FWidth := 0;
  FHeight := 0;
  FOffset.X := 0;
  FOffset.Y := 0;

  Root := GetAncestor(FTarget, GA_ROOT);

  if (Root <> FTarget) then // Target is a child window, so we will offset painting
  begin
    FOffset.X := WindowRect(FTarget).Left - WindowRect(Root).Left;
    FOffset.Y := WindowRect(FTarget).Top - WindowRect(Root).Top;

    FTarget := Root;
  end;

  // Store script thread
  FScriptThread := OpenThread(THREAD_QUERY_INFORMATION, False, AScriptThread);

  // Get children
  EnumChildWindows(FTarget, @GetChildren, PtrUInt(@Self));

  // Close old layer if needed
  if (GetOverlayHandle(FTarget) > 0) then
    SendMessage(GetOverlayHandle(FTarget), WM_CLOSE, 0, 0);

  // Create new layer
  CreateWindow();

  // Setup layer window
  Show();
  SetLayered();
  SetTransparentColor(0);
  SetParent(FTarget);

  // Focus the target window, else sometimes the layer wont show.
  BringWindowToTop(FTarget);

  // Add hooks
  LayerHooks.IncRef();

  // Create Image
  Add(0, 0, FWidth, FHeight); // Bitmap
  Add(0, 0, FWidth, FHeight); // Buffer
end;

destructor TWindowOverlay.Destroy;
var
  i: Int32;
begin
  Close();
  CloseHandle(FScriptThread);
  LayerHooks.DecRef();

  if (FPaintThread <> nil) then
  begin
    FPaintThread.Terminate();
    FPaintThread.WaitFor();
    FPaintThread.Free();
  end;

  for i := 0 to High(FBitmaps) do
    FBitmaps[i].Free();

  inherited Destroy();
end;

initialization
  if (Win32MajorVersion >= 6) then
    InitDwmLibrary();

end.

