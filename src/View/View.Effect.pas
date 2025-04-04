unit View.Effect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

const WM_AFTER_SHOW = WM_USER + 1;
type
  TFormEffectStart = class(TForm)
    published procedure FormShow(Sender: TObject);
    private FTargetFormClass: TFormClass;
    private procedure WMAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    public class procedure Execute(AOwner: TComponent; TargetFormClass: TFormClass); static;
  end;

var
  FormEffectStart: TFormEffectStart;

implementation

{$R *.dfm}

uses View.Game;

class procedure TFormEffectStart.Execute(AOwner: TComponent; TargetFormClass: TFormClass);
begin
  var EffectForm := TFormEffectStart.Create(AOwner);
  try
    EffectForm.FTargetFormClass := TargetFormClass;
    EffectForm.ShowModal;
  finally
    FreeAndNil(EffectForm);
  end;
end;

procedure TFormEffectStart.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TFormEffectStart.WMAfterShow(var Msg: TMessage);
begin
  var TargetForm := FTargetFormClass.Create(Self);
  try
    TargetForm.ShowModal;
  finally
    FreeAndNil(TargetForm);
    Close;
  end;
end;

end.
