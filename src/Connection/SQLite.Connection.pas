unit SQLite.Connection;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, FireDAC.Stan.Def,
  FireDAC.Stan.Async, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.Intf, FireDAC.UI.Intf, FireDAC.VCLUI.Wait,
  FireDAC.Phys, FireDAC.Comp.UI, FireDAC.DApt, FireDAC.Stan.Option;

type
  TSQLiteConnection = class
    strict private class var FConnection: TFDConnection;
    public class function GetConnection: TFDConnection; static;
    public class procedure CloseConnection; static;
    public class property Connection: TFDConnection read GetConnection;
  end;

implementation

uses Consts.Game;

class function TSQLiteConnection.GetConnection: TFDConnection;
begin
  var FullPathDB := ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\')+ DATA_FOLDER + DATABASE_FILENAME;
  if not Assigned(FConnection) then
  begin
    FConnection := TFDConnection.Create(nil);
    try
      FConnection.DriverName := 'SQLite';
      FConnection.Params.Add('Database=' + FullPathDB);
      FConnection.Params.Add('LockingMode=Normal');
      FConnection.Params.Add('Synchronous=Full');
      FConnection.Params.Add('SharedCache=False');
      FConnection.LoginPrompt := False;
      FConnection.Connected := True;
    except
      FreeAndNil(FConnection);
      raise;
    end;
  end;
  Result := FConnection;
end;

class procedure TSQLiteConnection.CloseConnection;
begin
  if Assigned(FConnection) then
  begin
    FConnection.Close;
    FreeAndNil(FConnection);
  end;
end;

initialization

finalization
  TSQLiteConnection.CloseConnection;
end.

