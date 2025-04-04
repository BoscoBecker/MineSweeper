unit View.Effect.Start;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

const WM_AFTER_SHOW = WM_USER + 1;
type
  TFormEffectStart = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    procedure WMAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
  public
  end;

var
  FormEffectStart: TFormEffectStart;

implementation

{$R *.dfm}

uses View.Game;

procedure TFormEffectStart.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TFormEffectStart.WMAfterShow(var Msg: TMessage);
begin
  var Game:= TFormGame.Create(Self);
  Game.ShowModal;
  FormEffectStart.Close;
end;

end.
