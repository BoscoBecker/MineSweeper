unit Sound.Game;

interface
type
  TPlaySoundGame = class
    private class function GetBasePath: string; static;
    public class procedure GameOver; static;
    public class procedure GameWinner; static;
    public class procedure Explosion; static;
    public class procedure Off; static;
    public class procedure Click; static;
    public class procedure VerifyFilesWav; static;
  end;

implementation

uses Consts.Game, MMSystem, System.SysUtils, System.Classes, System.Threading;

class function TPlaySoundGame.GetBasePath: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');
end;

class procedure TPlaySoundGame.Click;
begin
   var SoundPath:= GetBasePath + SOUND_FOLDER + SOUND_CLICK;
   if FileExists(SoundPath) then
     TTask.Run(procedure begin PlaySound(Pchar(SoundPath), 0, SND_ASYNC or SND_ASYNC); end);
end;

class procedure TPlaySoundGame.Explosion;
begin
   var SoundPath:= GetBasePath + SOUND_FOLDER + SOUND_EXPLOSION;
   if FileExists(SoundPath) then
     TTask.Run(procedure begin PlaySound(Pchar(SoundPath), 0, SND_ASYNC or SND_ASYNC); end);
end;

class procedure TPlaySoundGame.GameOver;
begin
   var SoundPath := GetBasePath + SOUND_FOLDER + SOUND_GAME_OVER;
   if FileExists(SoundPath) then
     TTask.Run(procedure begin  PlaySound(Pchar(SoundPath), 0, SND_LOOP or SND_ASYNC); end);
end;

class procedure TPlaySoundGame.GameWinner;
begin
   var SoundPath:= GetBasePath + SOUND_FOLDER + SOUND_CLAPS;
   if FileExists(SoundPath) then
    TTask.Run(procedure begin PlaySound(Pchar(SoundPath), 0, SND_LOOP or SND_ASYNC); end);
end;

class procedure TPlaySoundGame.Off;
begin
  TTask.Run(procedure begin PlaySound(nil, 0, 0); end );
end;
class procedure TPlaySoundGame.VerifyFilesWav;
begin
  if not FileExists(TPlaySoundGame.GetBasePath + SOUND_FOLDER + SOUND_CLICK) then
    raise Exception.Create('File explosion.wav not Found ');
  if not FileExists(TPlaySoundGame.GetBasePath + SOUND_FOLDER + SOUND_EXPLOSION) then
    raise Exception.Create('File gameover.wav not Found');
  if not FileExists(TPlaySoundGame.GetBasePath + SOUND_FOLDER + SOUND_GAME_OVER) then
    raise Exception.Create('File clapsWinner.wav not Found');
  if not FileExists(TPlaySoundGame.GetBasePath + SOUND_FOLDER + SOUND_CLAPS) then
    raise Exception.Create('File click.wav not Found');
end;

end.
