unit View.Result;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Game.Types;
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
    EditPlayerName: TEdit;
    LabelInfoName: TLabel;
    ImageYourName: TImage;
    LabelYourName: TLabel;
    LabelMineSweeper: TSkLabel;
    ImageHighScore: TSkAnimatedImage;
    procedure FormShow(Sender: TObject);
    procedure ImageCloseClick(Sender: TObject);
    procedure ImageTryAgainClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageSoundOFFClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageSoundONClick(Sender: TObject);
    procedure ImageShareClick(Sender: TObject);
    procedure ImageHighScoreClick(Sender: TObject);
    procedure EditPlayerNameChange(Sender: TObject);
  private
    FResultGame: ResultGame;
    FStatus: string;
    FSoundActive: Boolean;
    FTime: Integer;
    Rgn: HRGN;
    FDifficulty: TLevel;
    function GetResultGame: ResultGame;
    function GetSoundActive: Boolean;
    procedure PlayGameWinner;
    procedure PlayGameOver;
    procedure PlayOff;
    procedure Share;
    function GetStatus: string;
    function GetTime: Integer;
    procedure PopulateScore;
  public
    function GetDifficulty: TLevel;
    procedure BorderRadiusEffect;
    procedure SetDifficulty(const Value: TLevel);
    procedure SetSoundActive(const Value: Boolean);
    procedure SetResultGame(const Value: ResultGame);
    procedure SetTime(const Value: Integer);
    procedure SetStatus(const Value: string);
    procedure ViewHighScore;
    property ResultGame: ResultGame read GetResultGame write SetResultGame;
    property Time: Integer read GetTime write SetTime;
    property Status: string read GetStatus write SetStatus;
    property SoundActive : Boolean read GetSoundActive write SetSoundActive default True;
    property Difficulty : TLevel read GetDifficulty write SetDifficulty;
  end;

var
  FormResult: TFormResult;

implementation

{$R *.dfm}

uses Sound.Game, View.Effect, View.Share, View.HighScore, Score.Controller;

procedure TFormResult.BorderRadiusEffect;
begin
  Rgn := CreateRoundRectRgn(0, 0, Width, Height, 30, 30);
  SetWindowRgn(Handle, Rgn, True);
  SetSoundActive(True);

  with PanelCloud do
  begin
    var R := ClientRect;
    Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 10, 10);
    SetWindowRgn(Handle, Rgn, True);
  end;
end;

procedure TFormResult.EditPlayerNameChange(Sender: TObject);
begin
  LabelYourName.caption:= EditPlayerName.Text;
end;

procedure TFormResult.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteObject(Rgn);
  Action:= TCloseAction.caFree;
end;

procedure TFormResult.FormCreate(Sender: TObject);
begin
  BorderRadiusEffect;
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
  LabelYourName.BringToFront;
  ImageHighScore.Visible:= True;
  ImageHighScore.BringToFront;
end;

function TFormResult.GetDifficulty: TLevel;
begin
  result:= FDifficulty;
end;

function TFormResult.GetResultGame: ResultGame;
begin
  result:= FResultGame;
end;

function TFormResult.GetSoundActive: Boolean;
begin
  result:= FSoundActive;
end;

function TFormResult.GetStatus: string;
begin
  result:= FStatus;
end;

function TFormResult.GetTime: Integer;
begin
  result:= FTime;
end;

procedure TFormResult.PopulateScore;
begin
  if String(Trim(EditPlayerName.Text)).Equals('') then Exit;
  var Status := Ord(GetResultGame) = 1;
  var ScorePlayerGame : TScore;
      ScorePlayerGame.PlayerName:= EditPlayerName.Text;
      ScorePlayerGame.Score:= GetTime;
      ScorePlayerGame.TimePlayed:= GetTime;
      ScorePlayerGame.level:= GetDifficulty;
      ScorePlayerGame.StatusPlayer:= Status;
      TScoreController.SaveScore(ScorePlayerGame);
end;

procedure TFormResult.ImageTryAgainClick(Sender: TObject);
begin
  PopulateScore;
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

procedure TFormResult.SetDifficulty(const Value: TLevel);
begin
  FDifficulty := Value;
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

procedure TFormResult.Share;
begin
  TFormEffectStart.Execute(Application, TFormShare);
end;

procedure TFormResult.ViewHighScore;
begin
  TFormEffectStart.Execute(Application, TFormHighScore);
end;

procedure TFormResult.ImageCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormResult.ImageHighScoreClick(Sender: TObject);
begin
  ViewHighScore;
end;

procedure TFormResult.ImageShareClick(Sender: TObject);
begin
  Share;
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
