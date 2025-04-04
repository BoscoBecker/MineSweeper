program MineSweeper;

uses
  Vcl.Forms,
  Vcl.Graphics,
  View.Game in 'View\View.Game.pas' {FormGame},
  View.Effect in 'View\View.Effect.pas' {FormEffectStart},
  View.Result in 'View\View.Result.pas' {FormResult},
  Sound.Game in 'Engine\Sound.Game.pas',
  View.HighScore in 'View\View.HighScore.pas' {FormHighScore},
  View.Share in 'View\View.Share.pas' {FormShare},
  SQLite.Connection in 'Connection\SQLite.Connection.pas',
  Score.Controller in 'Controller\Score.Controller.pas',
  Game.Types in 'Enum\Game.Types.pas',
  Consts.Game in 'Const\Consts.Game.pas',
  View.Splash in 'View\View.Splash.pas' {FormSplash},
  System.SysUtils,
  System.Threading,
  Winapi.Windows;

{$R *.res}

var Rgn :HRGN;

procedure EffectDownBomb;
begin
  with FormSplash do
  begin
    while ImageBomb1.Top < 41 do
    begin
      ImageBomb1.Top:= ImageBomb1.Top + 5;
      ImageBomb2.Top:= ImageBomb2.Top + 7;
      ImageBomb3.Top:= ImageBomb3.Top + 8;
      Sleep(50);
      Application.ProcessMessages;
    end;
  end;
end;

procedure ShakeWindow;
begin
  var OriginalLeft := FormSplash.Left;
  var OriginalTop := FormSplash.Top;

  for var i := 1 to 10 do
  begin
    FormSplash.Left := OriginalLeft + Random(10) - 5;
    FormSplash.Top := OriginalTop + Random(10) - 5;
    Sleep(30);
    Application.ProcessMessages;
  end;
  FormSplash.Left := OriginalLeft;
  FormSplash.Top := OriginalTop;
end;

procedure BorderRadiusEffect;

begin
  with FormSplash do
  begin
    Rgn:= CreateRoundRectRgn(0, 0, Width, Height, 30, 30);
    SetWindowRgn(Handle, Rgn, True);
  end;
end;

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  FormSplash := TFormSplash.Create(nil);
  FormSplash.Show;
  BorderRadiusEffect;
  EffectDownBomb;
  FormSplash.PanelCloud.Color:= clRed;
  FormSplash.Repaint;
  FormSplash.Update; // força a renderização
  ShakeWindow;
  FormSplash.Close;
  DeleteObject(Rgn);
  FormSplash.Free;
  TPlaySoundGame.VerifyFilesWav; // Files exists in folder
  TFormEffectStart.Execute(Application, TFormGame);
  Application.Run;
end.
