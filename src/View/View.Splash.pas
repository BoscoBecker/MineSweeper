unit View.Splash;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia, Vcl.ExtCtrls;

type
  TFormSplash = class(TForm)
    SkAnimatedImage4: TSkAnimatedImage;
    LabelMineSweeper: TSkLabel;
    ImageHighScore: TSkAnimatedImage;
    ImageTrophy: TSkAnimatedImage;
    SkAnimatedImage1: TSkAnimatedImage;
    PanelCloud: TPanel;
    ImageCloud: TSkAnimatedImage;
    ImageBomb2: TSkAnimatedImage;
    ImageBomb3: TSkAnimatedImage;
    ImageBomb1: TSkAnimatedImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSplash: TFormSplash;

implementation

{$R *.dfm}


Uses System.Threading;

end.
