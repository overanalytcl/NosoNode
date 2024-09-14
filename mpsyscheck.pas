unit mpSysCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient{$IFDEF Unix} ,Linux {$ENDIF}, nosodebug,
  nosocrypto;

type
  TThreadHashtest = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

function Sys_HashSpeed(cores: Integer = 4): Int64;
function AllocateMem(UpToMb: Integer = 1024): Int64;
function TestDownloadSpeed(): Int64;

var
  OpenHashThreads: Integer;

implementation

constructor TThreadHashtest.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TThreadHashtest.Execute;
var
  counter: Integer;
begin
  for counter := 1 to 10000 do
    HashSha256String(IntToStr(counter));
  Dec(OpenHashThreads);
end;

function Sys_HashSpeed(cores: Integer = 4): Int64;
var
  counter: Integer;
  StartTime, EndTime: Int64;
  ThisThread: TThreadHashtest;
begin
  OpenHashThreads := cores;
  for counter := 1 to cores do
  begin
    ThisThread := TThreadHashtest.Create(True);
    ThisThread.FreeOnTerminate := True;
    ThisThread.Start;
  end;
  StartTime := GetTickCount64;
  repeat
    sleep(1);
  until OpenHashThreads = 0;
  EndTime := GetTickCount64;
  Result := (cores * 10000) div (EndTime - StartTime);
end;

function AllocateMem(UpToMb: Integer = 1024): Int64;
var
  h: TFPCHeapStatus;
  i: Cardinal;
  LastHeapFails: Boolean;
  Z: Pointer;
  {$IFDEF Unix}
 Info : TSysInfo;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF WINDOWS}
Result := 0;
LastHeapFails := ReturnNilIfGrowHeapFails;
ReturnNilIfGrowHeapFails := True;
for i := 1 to $FFFF do
begin
  Z := GetMem(i * $10000);
  if Z = nil then
    break;
  h := GetFPCHeapStatus;
  Result := h.MaxHeapSize div 1048576;
  Freemem(Z);
end;
ReturnNilIfGrowHeapFails := LastHeapFails;
  {$ENDIF}
  {$IFDEF Unix}
SysInfo(@Info);
result := Info.freeram div 1048576;
  {$ENDIF}
end;

function TestDownloadSpeed(): Int64;
var
  MS: TMemoryStream;
  DownLink: String = '';
  Conector: TFPHttpClient;
  Sucess: Boolean = False;
  timeStart, timeEnd: Int64;
  trys: Integer = 0;
begin
  Result := 0;
  DownLink := 'https://raw.githubusercontent.com/nosocoin/NosoNode/main/1mb.dat';
  MS := TMemoryStream.Create;
  Conector := TFPHttpClient.Create(nil);
  conector.ConnectTimeout := 1000;
  conector.IOTimeout := 1000;
  conector.AllowRedirect := True;
  repeat
    timeStart := GetTickCount64;
  try
    Conector.Get(DownLink, MS);
    MS.SaveToFile('NOSODATA' + DirectorySeparator + '1mb.dat');
    timeEnd := GetTickCount64;
    //DeleteFile('NOSODATA'+DirectorySeparator+'1mb.dat');
    Sucess := True;
  except
    ON E: Exception do
      ToLog('events', TimeToStr(now) + e.Message);
  end{Try};
    Inc(Trys);
  until ((sucess) or (trys = 5));
  MS.Free;
  conector.Free;
  if Sucess then Result := 1048576 div (TimeEnd - TimeStart);
end;

end. // END UNIT
