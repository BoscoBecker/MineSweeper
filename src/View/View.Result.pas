unit View.Result;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls;

type ResultGame = (Winner, Loser);

type
  TFormResult = class(TForm)
    ImageTryAgain: TImage;
    PanelCloud: TPanel;
    ImageCloud: TSkAnimatedImage;
    ImageWinEffect: TSkAnimatedImage;
    ImageBack: TImage;
    SkAnimatedImage4: TSkAnimatedImage;
    ImageSoundON: TSkAnimatedImage;
    ImageClose: TSkAnimatedImage;
    ImageSoundOFF: TSkAnimatedImage;
    ImageShare: TSkAnimatedImage;
    ImageTrophy: TSkAnimatedImage;
    ImageTime: TSkAnimatedImage;
    ImageLose: TSkAnimatedImage;
    LabelWinner: TLabel;
    LabelTime: TLabel;
    LabelTryAgain: TLabel;
    LabelMineSweeper: TSkLabel;
    procedure FormShow(Sender: TObject);
    procedure ImageCloseClick(Sender: TObject);
    procedure ImageTryAgainClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageSoundOFFClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageSoundONClick(Sender: TObject);
  private
    Rgn: HRGN;
    FResultGame: ResultGame;
    FStatus: string;
    FTime: Integer;
    FSoundActive: Boolean;
    function GetResultGame: ResultGame;
    procedure PlayGameWinner;
    procedure PlayGameOver;
    procedure PlayOff;
    procedure PlayOn;
    function GetSoundActive: Boolean;
  public
    procedure SetSoundActive(const Value: Boolean);
    procedure SetResultGame(const Value: ResultGame);
    procedure SetTime(const Value: Integer);
    procedure SetStatus(const Value: string);
    property ResultGame: ResultGame read GetResultGame write SetResultGame;
    property Time: Integer read FTime write SetTime;
    property Status: string read FStatus write SetStatus;
    property SoundActive : Boolean read GetSoundActive write SetSoundActive default True;
  end;

var
  FormResult: TFormResult;

implementation

{$R *.dfm}

{ TFormResult }

uses Sound.Game;

procedure TFormResult.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteObject(Rgn);
end;

procedure TFormResult.FormCreate(Sender: TObject);
begin
  /// border radius effect
  Rgn := CreateRoundRectRgn(0, 0, Width, Height, 30, 30);
  SetWindowRgn(Handle, Rgn, True);
  SetSoundActive(True);
end;

procedure TFormResult.FormShow(Sender: TObject);
begin
  if GetSoundActive then
  begin
    ImageSoundON.visible:= True;
    ImageSoundOff.visible:= False;
  end else
  begin
    ImageSoundON.visible:= False;
    ImageSoundOff.visible:= True;
  end;
  case GetResultGame of
    Winner:
      begin
        ImageTrophy.Visible:= True;
        ImageTrophy.BringToFront;
        ImageTime.Visible:= True;
        ImageTime.BringToFront;

        ImageWinEffect.Visible:= True;
        LabelWinner.Visible:= True;
        LabelWinner.Visible:= True;
        LabelTime.Visible:= True;
        LabelWinner.caption:= 'You Winner';
        LabelTime.caption:= FTime.ToString+' seconds';
        LabelTryAgain.Caption:= '  You Win Amazing !!!';

        ImageBack.Visible:= False;
        ImageLose.Visible:= False;
        ImageLose.SendToBack;
      end;

    Loser:
      begin
        ImageLose.Visible:= True;
        ImageLose.BringToFront;
        ImageBack.Visible:= True;
        LabelWinner.caption:= 'You Lost ';
        LabelTime.caption:= FTime.ToString+' seconds';
        LabelTryAgain.Caption:= 'Try Again';

        ImageTrophy.Visible:= False;
        ImageWinEffect.Visible:= False;
        ImageWinEffect.SendToBack;
      end;
  end;
end;

function TFormResult.GetResultGame: ResultGame;
begin
  result:= FResultGame;
end;

function TFormResult.GetSoundActive: Boolean;
begin
  result:= FSoundActive;
end;

procedure TFormResult.ImageTryAgainClick(Sender: TObject);
begin
  Close;
end;

procedure TFormResult.PlayGameOver;
begin
  TPlaySoundGame.GameOver;
end;

procedure TFormResult.PlayGameWinner;
begin
  TPlaySoundGame.GameWinner;
end;

procedure TFormResult.PlayOff;
begin
  TPlaySoundGame.Off;
end;

procedure TFormResult.PlayOn;
begin
  PlayOff;
  ImageSoundOff.visible:= True;
  ImageSoundON.visible:= False;
end;

procedure TFormResult.SetResultGame(const Value: ResultGame);
begin
  FResultGame := Value;
end;

procedure TFormResult.SetSoundActive(const Value: Boolean);
begin
  FSoundActive := Value;
end;

procedure TFormResult.SetStatus(const Value: string);
begin
  FStatus:= Value;
end;

procedure TFormResult.SetTime(const Value: Integer);
begin
 FTime:= Value;
end;

procedure TFormResult.ImageCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormResult.ImageSoundOFFClick(Sender: TObject);
begin
  case GetResultGame of
    Winner: PlayGameWinner;
    Loser: PlayGameOver;
  end;

  ImageSoundON.visible:= True;
  ImageSoundOff.visible:= False;
  SetSoundActive(True);
end;

procedure TFormResult.ImageSoundONClick(Sender: TObject);
begin
  PlayOff;
  ImageSoundON.visible:= False;
  ImageSoundOff.visible:= True;
  SetSoundActive(False);
end;

end.
