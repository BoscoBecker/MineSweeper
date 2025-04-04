unit View.HighScore;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Skia, Vcl.Skia, Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, SQlite.Connection, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.VCLUI.Wait, Vcl.StdCtrls,
  Vcl.Imaging.pngimage;

type
  TFormHighScore = class(TForm)
    DBGridHighScore: TDBGrid;
    dsScore: TDataSource;
    PanelCloud: TPanel;
    ImageCloud: TSkAnimatedImage;
    ImageClose: TSkAnimatedImage;
    LabelMineSweeper: TSkLabel;
    ImageBaloonExplosion: TSkAnimatedImage;
    ImageHighScore: TSkAnimatedImage;
    HighScore: TFDQuery;
    ImageYourName: TImage;
    LabelInfoName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ImageCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DBGridHighScoreKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LabelInfoNameClick(Sender: TObject);
  private
    Rgn: HRGN;
    procedure ClearScores;
    procedure BorderRadiusEffect;
  end;

var
  FormHighScore: TFormHighScore;

implementation

{$R *.dfm}

Uses Score.Controller;

procedure TFormHighScore.BorderRadiusEffect;
begin
  Rgn:= CreateRoundRectRgn(0, 0, Width, Height, 30, 30);
  SetWindowRgn(Handle, Rgn, True);
  with DBGridHighScore do
  begin
    var R := ClientRect;
    Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 10, 10);
    SetWindowRgn(Handle, Rgn, True);
  end;

  with PanelCloud do
  begin
    var R := ClientRect;
    Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 10, 10);
    SetWindowRgn(Handle, Rgn, True);
  end;
end;

procedure TFormHighScore.ClearScores;
begin
  TScoreController.ClearScores;
  LabelInfoName.Visible:= false;
  ImageYourname.Visible:= false;
end;

procedure TFormHighScore.DBGridHighScoreKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (ssCtrl in Shift) then Key := 0; // Why so serious ?
end;

procedure TFormHighScore.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TSQLiteConnection.CloseConnection;
  DeleteObject(Rgn);
  Action:= TCloseAction.caFree;
end;

procedure TFormHighScore.FormCreate(Sender: TObject);
begin
  BorderRadiusEffect;
  Screen.Cursor:= crDefault;
  try
    HighScore.Connection:=  TSQLiteConnection.GetConnection;
    HighScore.Open;
    HighScore.First;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFormHighScore.ImageCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHighScore.LabelInfoNameClick(Sender: TObject);
begin
  ClearScores;
end;

end.
