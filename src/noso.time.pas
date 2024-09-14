unit Noso.Time;

{
Nosotime 1.3
September 20th, 2023
Noso Time Unit for time synchronization on Noso project.
Requires indy package. (To-do: remove this dependancy)

Changes:
- Random use of NTP servers.
- Async process limited to every 5 seconds.
- Block time related functions.
- Test NTPs.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IdSNTP, DateUtils, strutils;

type
  TThreadUpdateOffset = class(TThread)
  private
    Hosts: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const CreatePaused: Boolean; const THosts: String);
  end;

function GetNetworkTimestamp(hostname: String): Int64;
function TimestampToDate(timestamp: Int64): String;
function GetTimeOffset(NTPServers: String): Int64;
function UTCTime: Int64;
function UTCTimeStr: String;
procedure UpdateOffset(NTPServers: String);
function TimeSinceStamp(Lvalue: Int64): String;
function BlockAge(): Integer;
function NextBlockTimeStamp(): Int64;
function IsBlockOpen(): Boolean;

var
  NosoT_TimeOffset: Int64 = 0;
  NosoT_LastServer: String = '';
  NosoT_LastUpdate: Int64 = 0;

implementation

constructor TThreadUpdateOffset.Create(const CreatePaused: Boolean; const THosts: String);
begin
  inherited Create(CreatePaused);
  Hosts := THosts;
end;

procedure TThreadUpdateOffset.Execute;
begin
  GetTimeOffset(Hosts);
end;

{Returns the data from the specified NTP server [Hostname]}
function GetNetworkTimestamp(hostname: String): Int64;
var
  NTPClient: TIdSNTP;
begin
  Result := 0;
  NTPClient := TIdSNTP.Create(nil);
  try
    NTPClient.Host := hostname;
    NTPClient.Active := True;
    NTPClient.ReceiveTimeout := 500;
    Result := DateTimeToUnix(NTPClient.DateTime);
    if Result < 0 then Result := 0;
  except
    on E: Exception do
      Result := 0;
  end; {TRY}
  NTPClient.Free;
end;

{Returns a UNIX timestamp in a human readeable format}
function TimestampToDate(timestamp: Int64): String;
begin
  Result := DateTimeToStr(UnixToDateTime(TimeStamp));
end;

{
Uses a random NTP server from the list provided to set the value of the local variables.
NTPservers string must use NosoCFG format: server1:server2:server3:....serverX:
If directly invoked, will block the main thread until finish. (not recommended except on app launchs)
}
function GetTimeOffset(NTPServers: String): Int64;
var
  Counter: Integer = 0;
  ThisNTP: Int64;
  MyArray: array of String;
  RanNumber: Integer;
begin
  Result := 0;
  NTPServers := StringReplace(NTPServers, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
  NTPServers := Trim(NTPServers);
  MyArray := SplitString(NTPServers, ' ');
  Rannumber := Random(length(MyArray));
  for Counter := 0 to length(MyArray) - 1 do
  begin
    ThisNTP := GetNetworkTimestamp(MyArray[Rannumber]);
    if ThisNTP > 0 then
    begin
      Result := ThisNTP - DateTimeToUnix(Now);
      NosoT_LastServer := MyArray[Rannumber];
      NosoT_LastUpdate := UTCTime;
      break;
    end;
    Inc(RanNumber);
    if RanNumber >= length(MyArray) - 1 then RanNumber := 0;
  end;
  NosoT_TimeOffset := Result;
end;

{Returns the UTC UNIX timestamp}
function UTCTime: Int64;
begin
  Result := DateTimeToUnix(Now, False) + NosoT_TimeOffset;
end;

{Implemented for easy compatibility with nosowallet}
function UTCTimeStr: String;
begin
  Result := IntToStr(DateTimeToUnix(Now, False) + NosoT_TimeOffset);
end;

{Implemented to allow an async update of the offset; can be called every 5 seconds max}
procedure UpdateOffset(NTPServers: String);
const
  LastRun: Int64 = 0;
var
  LThread: TThreadUpdateOffset;
begin
  if UTCTime <= LastRun + 4 then exit;
  LastRun := UTCTime;
  LThread := TThreadUpdateOffset.Create(True, NTPservers);
  LThread.FreeOnTerminate := True;
  LThread.Start;
end;

{Tool: returns a simple string with the time elapsed since the provided timestamp [LValue]}
function TimeSinceStamp(Lvalue: Int64): String;
var
  Elapsed: Int64 = 0;
begin
  Elapsed := UTCTime - Lvalue;
  if Elapsed div 60 < 1 then Result := IntToStr(Elapsed) + 's'
  else if Elapsed div 3600 < 1 then Result := IntToStr(Elapsed div 60) + 'm'
  else if Elapsed div 86400 < 1 then Result := IntToStr(Elapsed div 3600) + 'h'
  else if Elapsed div 2592000 < 1 then Result := IntToStr(Elapsed div 86400) + 'd'
  else if Elapsed div 31536000 < 1 then Result := IntToStr(Elapsed div 2592000) + 'M'
  else
    Result := IntToStr(Elapsed div 31536000) + ' Y';
end;

{Return the current block age}
function BlockAge(): Integer;
begin
  Result := UTCtime mod 600;
end;

{Returns the expected timestamp for next block}
function NextBlockTimeStamp(): Int64;
var
  currTime: Int64;
  Remains: Int64;
begin
  CurrTime := UTCTime;
  Remains := 600 - (CurrTime mod 600);
  Result := CurrTime + Remains;
end;

{Returns if the current block is in operation period}
function IsBlockOpen(): Boolean;
begin
  Result := True;
  if ((BlockAge < 10) or (BlockAge > 585)) then Result := False;
end;

end. // END UNIT
