# WindowOverlay
A (windows only) plugin to give your Simba script ability to draw on top of any window:

Basic example:
```pascal
{$loadlib libwindowoverlay}

var
  Overlay: TWindowOverlay;
begin
  Overlay := TWindowOverlay.Create(); // `TargetWindow` not specified. Will Simba's current target.
  AddOnTerminate(@Overlay.Free);

  // Overlay.Bitmap is a TMufasaBitmap, let's draw a red rectangle.
  Overlay.Bitmap.Rectangle([100, 100, 200, 200], 255);

  while True do 
    Sleep(1000);
end.
```

- The overlay automatically paints every 100 milliseconds, this can be changed with the `PaintInterval` parameter in the `TWindowOverlay.Create` method.
- If doing lots of drawing at once, do your drawing between a `BeginUpdate` and `EndUpdate` call to reduce flickering.

------

All available methods:
```pascal
function TWindowOverlay.Create(TargetWindow: PtrUInt = 0; PaintInterval: Int32 = 100): TWindowOverlay; static;
procedure TWindowOverlay.Free;
procedure TWindowOverlay.BeginUpdate;
procedure TWindowOverlay.EndUpdate;
procedure TWindowOverlay.Paint;
function TWindowOverlay.Width: Int32; 
function TWindowOverlay.Height: Int32;
function TWindowOverlay.Data: Pointer;
function TWindowOverlay.Bitmap: TMufasaBitmap; constref;
```