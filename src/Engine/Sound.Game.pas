unit Sound.Game;

interface

const SOUND_FOLDER = 'Sound\';
const SOUND_EXPLOSION = 'explosion.wav';
const SOUND_GAME_OVER = 'gameover.wav';
const SOUND_CLAPS = 'clapsWinner.wav';
const SOUND_CLICK = 'click.wav';

type
  TPlaySoundGame = class
   private class function GetBasePath: string; static;
   public class procedure GameOver; static;
   public class procedure GameWinner; static;
   public class procedure Explosion; static;
   public class procedure Off; static;
   public class procedure Click; static;
  end;

implementation

{ TPlaySound }

uses MMSystem, System.SysUtils, System.Threading;

class function TPlaySoundGame.GetBasePath: string;
begin
  Result := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\');
end;

class procedure TPlaySoundGame.Click;
begin
   var SoundPath:= GetBasePath + SOUND_FOLDER + SOUND_CLICK;
   if FileExists(SoundPath) then
     TTask.Run(procedure begin PlaySound(Pchar(SoundPath), 0, SND_NOWAIT or SND_ASYNC); end);
end;

class procedure TPlaySoundGame.Explosion;
begin
   var SoundPath:= GetBasePath + SOUND_FOLDER + SOUND_EXPLOSION;
   if FileExists(SoundPath) then
     TTask.Run(procedure begin PlaySound(Pchar(SoundPath), 0, SND_NOWAIT or SND_ASYNC); end);
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
    TTask.Run(procedure begin PlaySound(Pchar(SoundPath), 0, SND_NOWAIT or SND_ASYNC); end);
end;

class procedure TPlaySoundGame.Off;
begin
  TTask.Run(procedure begin PlaySound(nil, 0, 0); end );
end;
end.
