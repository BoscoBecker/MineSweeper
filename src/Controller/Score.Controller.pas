unit Score.Controller;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, FireDAC.Stan.Param,
  SQLite.Connection, Game.Types, Forms, Vcl.Controls;
type
  TScore = record
    ID: Integer;
    PlayerName: string;
    Score: Integer;
    TimePlayed: Integer;
    Level: TLevel;
    StatusPlayer: boolean;
  end;

  TScoreController = class
    public class var Difficulty: TLevel;
    public class procedure SetDifficulty(const level :TLevel); static;
    public class procedure SaveScore(const ScorePlayer: TScore);  static;
    public class procedure ClearScores; static;
  end;

implementation

uses System.Threading;

class procedure TScoreController.SaveScore(const ScorePlayer: TScore);
begin
  Screen.Cursor:= crHandPoint;
  TTask.Run(
  procedure
  begin
    var FinalScore:=0;
    try
      case ScorePlayer.level of
        Easy: FinalScore := ScorePlayer.Score * 1 ;
        Medium: FinalScore := Round(ScorePlayer.Score * 1.5) ;
        Hard: FinalScore := ScorePlayer.Score * 2;
      end;

      TSQLiteConnection
        .Connection
          .ExecSQL('INSERT INTO Scores (player_name, score, time, status_player) VALUES (:player, :score, :time, :status_player)',
            [ScorePlayer.PlayerName,FinalScore,ScorePlayer.TimePlayed,ScorePlayer.StatusPlayer.ToInteger]);
    finally
      TSQLiteConnection.CloseConnection;
      Screen.Cursor:= crHandPoint;
    end;
  end);
end;

class procedure TScoreController.SetDifficulty(const level :TLevel);
begin
  Difficulty:= level;
end;

class procedure TScoreController.ClearScores;
begin
  Screen.Cursor:= crHandPoint;
  TTask.Run(
  procedure
  begin
    try
      TSQLiteConnection
        .Connection
          .ExecSQL('DELETE FROM scores');
    finally
      TSQLiteConnection.CloseConnection;
      Screen.Cursor:= crHandPoint;
    end;
  end);
end;

end.

