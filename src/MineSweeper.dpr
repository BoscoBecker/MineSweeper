program MineSweeper;

uses
  Vcl.Forms,
  View.Game in 'View\View.Game.pas' {FormGame},
  View.Effect.Start in 'View\View.Effect.Start.pas' {FormEffectStart},
  View.Result in 'View\View.Result.pas' {FormResult},
  Sound.Game in 'Engine\Sound.Game.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  FormEffectStart:= TFormEffectStart.Create(Application);
  FormEffectStart.ShowModal;
  Application.Run;
end.
