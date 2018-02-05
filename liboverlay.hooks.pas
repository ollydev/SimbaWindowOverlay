unit liboverlay.hooks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JwaWindows,
  liboverlay.Window;

type
  TLayerHooks = class
  private
    FEventHook: HHOOK;
    FMouseHook: HHOOK;
    FKeyboardHook: HHOOK;
    FReferenceCount: Int32;

    procedure AddHooks;
    procedure RemoveHooks;
  public
    procedure IncRef;
    procedure DecRef;

    constructor Create;
    destructor Destroy; override;
  end;

var
  LayerHooks: TLayerHooks;

implementation

uses
  Types;

procedure OnEventHook(Hook: HWINEVENTHOOK; Event: DWORD; Window: HWND; idObject: LONG; idChild: LONG; EventThread: DWORD; EventTime: DWORD); stdcall;
var
  Layer: TWindowOverlay;
begin
  if (idObject = OBJID_WINDOW) and (Event = EVENT_OBJECT_LOCATIONCHANGE) then
  begin
    Layer := GetOverlay(Window);

    if (Layer <> nil) then
      Layer.SetBounds(WindowRect(Window));
  end;
end;

function OnMouseHook(Code: Int32; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  P: JwaWindows.POINT;
  Layer: TWindowOverlay;
  Block: Boolean = False;
  Handle: HWND;
  Button: Int32 = -1;
begin
  if (wParam = WM_LBUTTONDOWN) or (wParam = WM_RBUTTONDOWN) or (wParam = WM_MOUSEMOVE) then
  begin
    P := MSLLHOOKSTRUCT(Pointer(lParam)^).Pt;
    Handle := WindowFromPoint(P);
    Layer := GetOverlay(GetAncestor(Handle, GA_ROOT));

    if (Layer <> nil) and ((Handle = Layer.Target) or Layer.HasChild(Handle)) then
    begin
      P.X -= WindowRect(Layer.Target).Left;
      P.Y -= WindowRect(Layer.Target).Top;
      P.X -= Layer.Offset.X;
      P.Y -= Layer.Offset.Y;

      case wParam of
        WM_RBUTTONDOWN:
          Button := 0;
        WM_LBUTTONDOWN:
          Button := 1;
        WM_MOUSEMOVE:
          if (not Layer.DrawMouseManual) then
            Layer.AddMouseHistory(P.X, P.Y);
      end;

      if (Button > -1) and (Layer.OnClick <> nil) and Layer.ScriptActive then
      begin
        Layer.OnClick(Layer, P.X, P.Y, Button, Block);

        if Block then
          Exit(-1);
      end;
    end;
  end;

  Exit(CallNextHookEx(0, Code, wParam, lParam));
end;

function OnKeyboardHook(Code: Int32; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Handle: HWND;
  Layer: TWindowOverlay;
  Struct: ^KBDLLHOOKSTRUCT absolute lParam;
  State: array [0..255] of Byte;
  Chars: array[0..1] of WideChar;
  Block: Boolean = False;
begin
  if (wParam = WM_KEYDOWN) then
  begin
    Handle := GetFocus();
    Layer := GetOverlay(GetAncestor(Handle, GA_ROOT));

    if (Layer <> nil) and (Layer.OnKey <> nil) and ((Handle = Layer.Target) or Layer.HasChild(Handle)) then
    begin
      GetKeyboardState(State);

      if (ToUnicodeEx(Struct^.vkCode, Struct^.ScanCode, @State, @Chars, SizeOf(Chars), 0, GetKeyboardLayout(0)) > 0) then
      begin
        Layer.OnKey(Layer, Struct^.vkCode, UTF8Encode(UnicodeString(Chars))[1], Block);
        if Block then
          Exit(-1);
      end;
    end;
  end;

  Exit(CallNextHookEx(0, Code, wParam, lParam));
end;

procedure TLayerHooks.AddHooks;
begin
  FEventHook := SetWinEventHook(EVENT_OBJECT_LOCATIONCHANGE, EVENT_OBJECT_LOCATIONCHANGE, 0, @OnEventHook, 0, 0, WINEVENT_OUTOFCONTEXT);
  FMouseHook := SetWindowsHookEx(WH_MOUSE_LL, @OnMouseHook, 0, 0);
  FKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @OnKeyboardHook, 0, 0);
end;

procedure TLayerHooks.RemoveHooks;
begin
  UnhookWinEvent(FEventHook);
  UnhookWindowsHookEx(FMouseHook);
  UnhookWindowsHookEx(FKeyboardHook);
end;

procedure TLayerHooks.IncRef;
begin
  if (FReferenceCount = 0) then
    AddHooks();

  Inc(FReferenceCount);
end;

procedure TLayerHooks.DecRef;
begin
  if (FReferenceCount > 0) then
    Dec(FReferenceCount);

  if (FReferenceCount = 0) then
    RemoveHooks();
end;

constructor TLayerHooks.Create;
begin
  FReferenceCount := 0;
end;

destructor TLayerHooks.Destroy;
begin
  while (FReferenceCount > 0) do
    DecRef();

  inherited Destroy;
end;

initialization
  LayerHooks := TLayerHooks.Create();

finalization
  LayerHooks.Free();

end.

