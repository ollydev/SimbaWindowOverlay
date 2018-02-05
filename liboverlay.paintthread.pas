unit liboverlay.paintthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPaintProc = procedure of object;

  TPaintThread = class(TThread)
  private
    FInterval: Int32;
    FPaused: Boolean;
    FPaintProc: TPaintProc;
  protected
    procedure Execute; override;
  public
    property Paused: Boolean read FPaused write FPaused;

    constructor Create(Interval: Int32; PaintProc: TPaintProc);
  end;

implementation

procedure TPaintThread.Execute;
begin
  try
    while (not Terminated) do
    begin
      while (FPaused) do
      begin
        if (Terminated) then
          Exit;

        Sleep(1);
      end;

      FPaintProc();

      Sleep(FInterval);
    end;
  except
    on e: Exception do
      WriteLn('[PAINT THREAD]:: ' + e.ClassName + '::' + e.Message);
  end;
end;

constructor TPaintThread.Create(Interval: Int32; PaintProc: TPaintProc);
begin
  inherited Create(False);

  FInterval := Interval;
  FPaintProc := PaintProc;
  FPaused := False;
end;

end.

