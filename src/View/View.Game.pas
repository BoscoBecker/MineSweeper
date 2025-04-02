unit View.Game;

interface

uses
  Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
  Vcl.Samples.Spin, Vcl.MPlayer, Vcl.Imaging.pngimage, System.Types,
  System.ImageList, Vcl.ImgList;

type TLevel = ( Easy, Medium, Hard);
type TImageBoard = ( Bomb, Flag);

type
  TFormGame = class(TForm)
    GameGrid: TStringGrid;
    PanelOptions: TPanel;
    TimerLabel: TLabel;
    GameTimer: TTimer;
    ImageFlag: TImage;
    ImageClock: TImage;
    ImageClose: TImage;
    LabelFlag: TLabel;
    ImageShare: TImage;
    ImageSoundOn: TImage;
    ImageSoundOff: TImage;
    ComboBoxLevel: TComboBox;
    procedure GameTimerTimer(Sender: TObject);
    procedure GameGridDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState);
    procedure GameGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImageCloseClick(Sender: TObject);
    procedure ComboBoxLevelChange(Sender: TObject);
    procedure ImageSoundOnClick(Sender: TObject);
    procedure ImageSoundOffClick(Sender: TObject);
  private
    Board: array of array of Integer;
    Revealed: array of array of Boolean;
    Flagged: array of array of Boolean;
    MineCount: Integer;
    GridSize: Integer;
    TimerCount: Integer;
    GameOver: Boolean;
    FSoundActive: boolean;
    FDifficulty: Tlevel;
    BombImage: TPngImage;
    FlagImage: TPngImage;
    Rgn: HRGN;
    procedure InitializeBoard;
    procedure CalculateAdjacentNumbers;
    procedure CheckWinCondition;
    procedure CreateImages;
    procedure CreateBordEffect;
    procedure EndGame(const Lost: Boolean);
    procedure FloodFill(X, Y: Integer);
    procedure NewGame(level: Tlevel);
    procedure PlaceMines;
    procedure PlaySoundGameOver;
    procedure PlaySoundExplosion;
    procedure PlaySoundWinner;
    procedure PlaySoundOff;
    procedure PlayClickSound;
    procedure RevealedMines;
    procedure SetDifficulty(level: Tlevel);
    procedure SetSoundActive(const Value: boolean);
    procedure RevealCell(X, Y: Integer);
    procedure UpdateLabelFlag(const CountFlag: Integer);

    function GetDifficulty: Tlevel;
    function GetImage(const Image: TImageBoard) : TPngImage;
    function GetSoundActive: boolean;
  public
    property SoundActive: boolean read GetSoundActive write SetSoundActive;
    property Difficulty: Tlevel read GetDifficulty write SetDifficulty;
  end;

var  FormGame: TFormGame;

implementation

{$R *.dfm}

{ TFormGame }

uses View.Result, Sound.Game;

procedure TFormGame.FormCreate(Sender: TObject);
begin
  CreateImages;
  CreateBordEffect;
  NewGame(Easy);
end;

procedure TFormGame.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if BombImage <> nil then
    FreeAndNil(BombImage);
  if FlagImage <> nil then
    FreeAndNil(FlagImage);
  DeleteObject(Rgn);
  Action := caFree;
end;

procedure TFormGame.NewGame(level: Tlevel);
begin
  case level of
    Easy: UpdateLabelFlag(10);
    Medium: UpdateLabelFlag(40);
    Hard: UpdateLabelFlag(99);
  end;
  case ComboBoxLevel.ItemIndex of
    0: MineCount:=  10;  //'Easy'   10 Flags
    1: MineCount:=  40;  //'Medium' 40 Flags
    2: MineCount:=  99;  //'Hard'   99 Flags
  end;

  GridSize := 17;
  SetLength(Board, GridSize, GridSize);
  SetLength(Revealed, GridSize, GridSize);
  SetLength(Flagged, GridSize, GridSize);

  GameGrid.ColCount := GridSize;
  GameGrid.RowCount := GridSize;
  GameGrid.DefaultColWidth := (ClientWidth div GridSize);
  GameGrid.DefaultRowHeight := (ClientHeight div GridSize);
  GameGrid.FixedCols := 0;
  GameGrid.FixedRows := 0;

  GameGrid.DefaultColWidth := 30;
  GameGrid.DefaultRowHeight := 30;
  GameGrid.FixedCols := 0;
  GameGrid.FixedRows := 0;
  InitializeBoard;
end;

procedure TFormGame.EndGame(const Lost: Boolean);
begin
  Formresult:= TFormresult.Create(nil);
  try
    GameOver := True;
    GameTimer.Enabled := False;
    if Lost then
    begin
      GameTimer.Enabled := False;
      PlaySoundExplosion;
      RevealedMines;
      PlaySoundGameOver;
      Formresult.SetTime(TimerCount);
      Formresult.SetResultGame(ResultGame.Loser);
      Formresult.SetSoundActive(GetSoundActive);
      Formresult.ShowModal;
    end else
    begin
      PlaySoundWinner;
      Formresult.SetTime(TimerCount);
      Formresult.SetResultGame(ResultGame.Winner);
      Formresult.SetSoundActive(GetSoundActive);
      Formresult.ShowModal;
    end;
  finally
    GameTimer.Enabled := False;
    SetSoundActive(FormResult.SoundActive);

    if GetSoundActive then
    begin
      ImageSoundON.visible:= True;
      ImageSoundOff.visible:= False;
    end else
    begin
      ImageSoundOff.visible:= True;
      ImageSoundON.visible:= False;
    end;
    Formresult.Close;
    Formresult.Free;
    PlaySoundOff;
    NewGame(TLevel(ComboBoxLevel.ItemIndex));
  end;
end;

procedure TFormGame.GameGridDrawCell(Sender: TObject; ACol, ARow: LongInt; Rect: TRect; State: TGridDrawState);
begin
  with GameGrid.Canvas do
  begin
    // Background Color
    if Revealed[ACol, ARow] then
    begin
      if Board[ACol, ARow] = -1 then
        Brush.Color := clRed
      else
        Brush.Color := clWhite;
    end else if Flagged[ACol, ARow] then
      Brush.Color := clYellow
    else
    begin
      if (ACol + ARow) mod 2 = 0 then
        Brush.Color := $0051D7AA
      else
        Brush.Color := $0032A388;
    end;

    FillRect(Rect);

    // color of font
    Font.Color := clBlack;
    Font.Size := 10;
    Font.Style := [fsBold];

    if Revealed[ACol, ARow] then
    begin
      if Board[ACol, ARow] = -1 then
      begin
        Font.Color := clWhite;
        Draw(Rect.Left + 10, Rect.Top + 5, GetImage(Bomb)); // Draw the bomb image
      end
      else if Board[ACol, ARow] > 0 then
      begin
        case Board[ACol, ARow] of
          1: Font.Color := clBlue;
          2: Font.Color := clGreen;
          3: Font.Color := clRed;
          4: Font.Color := clNavy;
          5: Font.Color := clMaroon;
          6: Font.Color := clTeal;
          7: Font.Color := clPurple;
          8: Font.Color := clBlack;
        end;
        TextOut(Rect.Left + 10, Rect.Top + 5, IntToStr(Board[ACol, ARow]));
      end;
    end
    else if Flagged[ACol, ARow] then
      Draw(Rect.Left + 5, Rect.Top + 5, GetImage(Flag)); // Draw the help flag
  end;
end;

procedure TFormGame.GameGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PlayClickSound;
  var Col, Row: Integer;
  if GameOver then Exit;
  GameGrid.MouseToCell(X, Y, Col, Row);
  if Button = mbLeft then
  begin
    RevealCell(Col, Row);
    if not GameTimer.Enabled then
      GameTimer.Enabled:= True;
  end
  else if Button = mbRight then
    Flagged[Col, Row] := not Flagged[Col, Row];

  GameGrid.Invalidate;
  CheckWinCondition;
end;

function TFormGame.GetDifficulty: Tlevel;
begin
  result:= FDifficulty;
end;

procedure TFormGame.InitializeBoard;
begin
  for var x := 0 to GridSize - 1 do
    for var y := 0 to GridSize - 1 do
    begin
      Board[x, y] := 0;
      Revealed[x, y] := False;
      Flagged[x, y] := False;
    end;

  GameOver := False;
  TimerCount := 0;
  TimerLabel.Caption := '0';

  PlaceMines;
  CalculateAdjacentNumbers;
end;

procedure TFormGame.PlaceMines;
begin
  var Positions: array of TPoint;
  Randomize;
  SetLength(Positions, GridSize * GridSize);

  // Fill the list with all possible positions
  for var x := 0 to GridSize - 1 do
    for var y := 0 to GridSize - 1 do
      Positions[x * GridSize + y] := Point(x, y);

  // Sort the position
  for var i := High(Positions) downto 1 do
  begin
     var x := Random(i + 1);
     var y := Positions[i].X;
    Positions[i].X := Positions[x].X;
    Positions[x].X := y;

    y := Positions[i].Y;
    Positions[i].Y := Positions[x].Y;
    Positions[x].Y := y;
  end;

  // Selects the first `MineCount` positions as mines
  for var i := 0 to MineCount - 1 do
  begin
    var x := Positions[i].X;
    var y := Positions[i].Y;
    Board[x, y] := -1;
  end;
end;

procedure TFormGame.CalculateAdjacentNumbers;
begin
  var count: Integer;
  for var x := 0 to GridSize - 1 do
    for var y := 0 to GridSize - 1 do
      if Board[x, y] <> -1 then
      begin
        count := 0;
        for var dx := -1 to 1 do
          for var dy := -1 to 1 do
            if (x + dx >= 0) and (x + dx < GridSize) and (y + dy >= 0) and (y + dy < GridSize) then
              if Board[x + dx, y + dy] = -1 then
                Inc(count);
        Board[x, y] := count;
      end;
end;

procedure TFormGame.CheckWinCondition;
begin
  for var x := 0 to GridSize - 1 do
    for var y := 0 to GridSize - 1 do
      if (Board[x, y] <> -1) and (not Revealed[x, y]) then Exit;
  EndGame(False);
end;

procedure TFormGame.FloodFill(X, Y: Integer);
begin
  for var dx := -1 to 1 do
    for var dy := -1 to 1 do
      RevealCell(X + dx, Y + dy);
end;

procedure TFormGame.RevealCell(X, Y: Integer);
begin
  if (X < 0) or (X >= GridSize) or (Y < 0) or (Y >= GridSize) or Revealed[X, Y]
                                           or Flagged[X, Y] then Exit;

  Revealed[X, Y] := True;
  if Board[X, Y] = -1 then
  begin
    EndGame(True);
    Exit;
  end;
  if Board[X, Y] = 0 then
    FloodFill(X, Y);
end;

procedure TFormGame.RevealedMines;
begin
  for var x := 0 to GridSize - 1 do
    for var y := 0 to GridSize - 1 do
      if Board[x, y] = -1 then
        Revealed[x, y] := True;
  GameGrid.Repaint;
  sleep(2000);
end;

procedure TFormGame.SetDifficulty(Level: TLevel);
begin
  FDifficulty:= level;
end;

procedure TFormGame.ComboBoxLevelChange(Sender: TObject);
begin
  SetDifficulty(TLevel(ComboBoxLevel.ItemIndex));
end;

/// Screen Controls
procedure TFormGame.UpdateLabelFlag(const CountFlag: Integer);
begin
  LabelFlag.Caption:= CountFlag.ToString();
end;

procedure TFormGame.GameTimerTimer(Sender: TObject);
begin
  try
    Inc(TimerCount);
    TimerLabel.Caption := Format('%d', [TimerCount]);
  except
    GameTimer.Enabled:= False;
  end;
end;

procedure TFormGame.CreateBordEffect;
var
  R : TRect;
begin
  Rgn := CreateRoundRectRgn(0, 0, Width, Height, 30, 30);
  SetWindowRgn(Handle, Rgn, True);

  SetDifficulty(Easy);
  SetSoundActive(True);

  with ComboBoxLevel do
  begin
    R := ClientRect;
    Rgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 10, 10);
    SetWindowRgn(Handle, Rgn, True);
  end;
end;

/// Sound Control
procedure TFormGame.PlaySoundWinner;
begin
  if FSoundActive then
    TPlaySoundGame.GameWinner;
end;

procedure TFormGame.PlayClickSound;
begin
  if FSoundActive then
    TPlaySoundGame.Click;
end;

procedure TFormGame.PlaySoundExplosion;
begin
  if FSoundActive then
    TPlaySoundGame.Explosion;
end;

procedure TFormGame.PlaySoundGameOver;
begin
  if FSoundActive then
    TPlaySoundGame.GameOver;
end;

procedure TFormGame.PlaySoundOff;
begin
  TPlaySoundGame.Off;
end;

procedure TFormGame.SetSoundActive(const Value: boolean);
begin
  FSoundActive:= value;
end;

procedure TFormGame.ImageSoundOffClick(Sender: TObject);
begin
  SetSoundActive(True);
  PlaySoundGameOver;
  ImageSoundON.visible:= True;
  ImageSoundOff.visible:= False;
end;

procedure TFormGame.ImageSoundOnClick(Sender: TObject);
begin
  SetSoundActive(False);
  PlaySoundOff;
  ImageSoundOff.visible:= True;
  ImageSoundON.visible:= False;
end;

/// Images Control
function TFormGame.GetImage(const Image : TImageBoard): TPngImage;
begin
  Result:= FlagImage;
  case Image of
    Bomb: Result := BombImage;
    Flag: Result := FlagImage;
  end;
end;

function TFormGame.GetSoundActive: boolean;
begin
  result:= FSoundActive;
end;

procedure TFormGame.CreateImages;
begin
   BombImage := TPngImage.Create;
   FlagImage := TPngImage.Create;
   BombImage.LoadFromFile(ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\')+'assets\Bomb.png');
   FlagImage.LoadFromFile(ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\')+'assets\FlagGame.png');
end;

procedure TFormGame.ImageCloseClick(Sender: TObject);
begin
  Close;
end;

end.


