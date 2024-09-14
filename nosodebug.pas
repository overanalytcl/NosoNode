unit nosodebug;

{
Nosodebug 1.3
September 22th, 2023
Unit to implement debug functionalities on noso project apps.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  Tperformance = record
    tag: String;
    Start: Int64;
    Average: Int64;
    Max: Int64;
    Min: Int64;
    Count: Int64;
    Total: Int64;
  end;

  TLogND = record
    tag: String;
    Count: Integer;
    ToDisk: Boolean;
    Filename: String;
  end;

  TCoreManager = record
    ThName: String;
    ThStart: Int64;
    ThLast: Int64;
  end;

  TFileManager = record
    FiType: String;
    FiFile: String;
    FiPeer: String;
    FiLast: Int64;
  end;

  TProcessCopy = array of TCoreManager;
  TFileMCopy = array of TFileManager;

procedure BeginPerformance(Tag: String);
function EndPerformance(Tag: String): Int64;
function PerformanceToFile(Destination: String): Boolean;

procedure CreateNewLog(LogName: String; LogFileName: String = '');
procedure ToLog(LogTag, NewLine: String);
function GetLogLine(LogTag: String; out LineContent: String): Boolean;

procedure AddNewOpenThread(ThName: String; TimeStamp: Int64);
procedure UpdateOpenThread(ThName: String; TimeStamp: Int64);
procedure CloseOpenThread(ThName: String);
function GetProcessCopy(): TProcessCopy;

procedure AddFileProcess(FiType, FiFile, FiPeer: String; TimeStamp: Int64);
function CloseFileProcess(FiType, FiFile, FiPeer: String; TimeStamp: Int64): Int64;
function GetFileProcessCopy(): TFileMCopy;

procedure InitDeepDeb(LFileName: String; SysInfo: String = '');
procedure ToDeepDeb(LLine: String);
function GetDeepDebLine(out LineContent: String): Boolean;

var
  ArrPerformance: array of TPerformance;
  NosoDebug_UsePerformance: Boolean = False;
  ArrNDLogs: array of TLogND;
  ArrNDCSs: array of TRTLCriticalSection;
  ArrNDSLs: array of TStringList;
  ArrProcess: array of TCoreManager;
  CS_ThManager: TRTLCriticalSection;

  ArrFileMgr: array of TFileManager;
  CS_FileManager: TRTLCriticalSection;

  SLDeepDebLog: TStringList;
  CS_DeepDeb: TRTLCriticalSection;
  DeepDebFilename: String = '';

implementation

{$REGION Performance}

{Starts a performance measure}
procedure BeginPerformance(Tag: String);
var
  counter: Integer;
  NewData: TPerformance;
begin
  if not NosoDebug_UsePerformance then exit;
  for counter := 0 to high(ArrPerformance) do
  begin
    if Tag = ArrPerformance[counter].tag then
    begin
      ArrPerformance[counter].Start := GetTickCount64;
      Inc(ArrPerformance[counter].Count);
      exit;
    end;
  end;
  NewData := default(TPerformance);
  NewData.tag := tag;
  NewData.Min := 99999;
  NewData.Start := GetTickCount64;
  NewData.Count := 1;
  Insert(NewData, ArrPerformance, length(ArrPerformance));
end;

{Ends a performance}
function EndPerformance(Tag: String): Int64;
var
  counter: Integer;
  duration: Int64 = 0;
begin
  Result := 0;
  if not NosoDebug_UsePerformance then exit;
  for counter := 0 to high(ArrPerformance) do
  begin
    if tag = ArrPerformance[counter].tag then
    begin
      duration := GetTickCount64 - ArrPerformance[counter].Start;
      ArrPerformance[counter].Total := ArrPerformance[counter].Total + Duration;
      ArrPerformance[counter].Average := ArrPerformance[counter].Total div
        ArrPerformance[counter].Count;
      if duration > ArrPerformance[counter].Max then
        ArrPerformance[counter].Max := duration;
      if duration < ArrPerformance[counter].Min then
        ArrPerformance[counter].Min := duration;
      break;
    end;
  end;
  Result := duration;
end;

function PerformanceToFile(Destination: String): Boolean;
var
  counter: Integer;
  Lines: TStringList;
  ThisLine: String;
  Tag, Count, max, average: String;
begin
  Result := True;
  Lines := TStringList.Create;
  Lines.Add('TAG                                           Count        Max    Average');
  for counter := 0 to high(ArrPerformance) do
  begin
    Tag := Format('%0:-40s', [ArrPerformance[counter].tag]);
    Count := Format('%0:10s', [IntToStr(ArrPerformance[counter].Count)]);
    Max := Format('%0:10s', [IntToStr(ArrPerformance[counter].Max)]);
    Average := Format('%0:10s', [IntToStr(ArrPerformance[counter].Average)]);
    ThisLine := Format('%s %s %s %s', [Tag, Count, max, average]);
    Lines.Add(ThisLine);
  end;
  Lines.SaveToFile(Destination);
  Lines.Free;
end;

{$ENDREGION}

{$REGION Logs}

{private: verify that the file for the log exists}
function InitializeLogFile(Filename: String; OptionText: String = ''): Boolean;
var
  LFile: textfile;
begin
  Result := True;
  if not fileexists(Filename) then
  begin
    try
      Assignfile(LFile, Filename);
      rewrite(LFile);
      if OptionText <> '' then
        Writeln(LFile, OptionText);
      Closefile(LFile);
    except
      on E: Exception do
      begin
        ToDeepDeb('Nosodebug,InitializeLogFile,' + E.Message);
        Result := False;
      end;
    end; {Try}
  end;
end;

{private: if enabled, saves the line to the log file}
procedure SaveTextToDisk(TextLine, Filename: String);
var
  LFile: textfile;
  IOCode: Integer;
begin
  Assignfile(LFile, Filename);
  {$I-}
  Append(LFile)
  {$I+}
  ;
  IOCode := IOResult;
  if IOCode = 0 then
  begin
    try
      Writeln(LFile, TextLine);
    except
      on E: Exception do
        ToDeepDeb('Nosodebug,SaveTextToDisk,' + E.Message);
    end; {Try}
    Closefile(LFile);
  end
  else if IOCode = 5 then
    {$I-}
    Closefile(LFile)
  {$I+}
  ;
end;

{Creates a new log and assigns an optional file to save it}
procedure CreateNewLog(LogName: String; LogFileName: String = '');
var
  NewData: TLogND;
begin
  NewData := Default(TLogND);
  NewData.tag := Uppercase(Logname);
  NewData.Filename := LogFileName;
  SetLength(ArrNDCSs, length(ArrNDCSs) + 1);
  InitCriticalSection(ArrNDCSs[length(ArrNDCSs) - 1]);
  SetLEngth(ArrNDSLs, length(ArrNDSLs) + 1);
  ArrNDSLs[length(ArrNDSLs) - 1] := TStringList.Create;
  if LogFileName <> '' then
  begin
    InitializeLogFile(LogFileName);
    NewData.ToDisk := True;
  end;
  Insert(NewData, ArrNDLogs, length(ArrNDLogs));
end;

{Adds one line to the specified log}
procedure ToLog(LogTag, NewLine: String);
var
  counter: Integer;
begin
  for counter := 0 to length(ArrNDLogs) - 1 do
  begin
    if ArrNDLogs[counter].tag = Uppercase(LogTag) then
    begin
      EnterCriticalSection(ArrNDCSs[counter]);
      ArrNDSLs[counter].Add(NewLine);
      Inc(ArrNDLogs[counter].Count);
      LeaveCriticalSection(ArrNDCSs[counter]);
    end;
  end;
end;

{Retireves the oldest line in the specified log, assigning value to LineContent}
function GetLogLine(LogTag: String; out LineContent: String): Boolean;
var
  counter: Integer;
begin
  Result := False;
  for counter := 0 to length(ArrNDLogs) - 1 do
  begin
    if ArrNDLogs[counter].tag = Uppercase(LogTag) then
    begin
      if ArrNDSLs[counter].Count > 0 then
      begin
        EnterCriticalSection(ArrNDCSs[counter]);
        LineContent := ArrNDSLs[counter][0];
        Result := True;
        ArrNDSLs[counter].Delete(0);
        if ArrNDLogs[counter].ToDisk then
          SaveTextToDisk(LineContent, ArrNDLogs[counter].Filename);
        LeaveCriticalSection(ArrNDCSs[counter]);
        break;
      end;
    end;
  end;
end;

{Private: Free all data at close}
procedure FreeAllLogs;
var
  counter: Integer;
begin
  for counter := 0 to length(ArrNDLogs) - 1 do
  begin
    ArrNDSLs[counter].Free;
    DoneCriticalsection(ArrNDCSs[counter]);
  end;
end;

{$ENDREGION}

{$REGION Thread manager}

procedure AddNewOpenThread(ThName: String; TimeStamp: Int64);
var
  NewValue: TCoreManager;
begin
  NewValue := Default(TCoreManager);
  NewValue.ThName := ThName;
  NewValue.ThStart := TimeStamp;
  NewValue.ThLast := TimeStamp;
  EnterCriticalSection(CS_ThManager);
  Insert(NewValue, ArrProcess, Length(ArrProcess));
  LeaveCriticalSection(CS_ThManager);
end;

procedure UpdateOpenThread(ThName: String; TimeStamp: Int64);
var
  counter: Integer;
begin
  EnterCriticalSection(CS_ThManager);
  for counter := 0 to High(ArrProcess) do
  begin
    if UpperCase(ArrProcess[counter].ThName) = UpperCase(ThName) then
    begin
      ArrProcess[counter].ThLast := TimeStamp;
      Break;
    end;
  end;
  LeaveCriticalSection(CS_ThManager);
end;

procedure CloseOpenThread(ThName: String);
var
  counter: Integer;
begin
  EnterCriticalSection(CS_ThManager);
  for counter := 0 to High(ArrProcess) do
  begin
    if UpperCase(ArrProcess[counter].ThName) = UpperCase(ThName) then
    begin
      Delete(ArrProcess, Counter, 1);
      Break;
    end;
  end;
  LeaveCriticalSection(CS_ThManager);
end;

function GetProcessCopy(): TProcessCopy;
begin
  Setlength(Result, 0);
  EnterCriticalSection(CS_ThManager);
  Result := copy(ArrProcess, 0, length(ArrProcess));
  LeaveCriticalSection(CS_ThManager);
end;

{$ENDREGION}

{$REGION Files manager}

procedure AddFileProcess(FiType, FiFile, FiPeer: String; TimeStamp: Int64);
var
  NewValue: TFileManager;
begin
  NewValue := Default(TFileManager);
  NewValue.FiType := FiType;
  NewValue.FiFile := FiFile;
  NewValue.FiPeer := FiPeer;
  NewValue.FiLast := TimeStamp;
  EnterCriticalSection(CS_FileManager);
  Insert(NewValue, ArrFileMgr, Length(ArrFileMgr));
  LeaveCriticalSection(CS_FileManager);
end;

function CloseFileProcess(FiType, FiFile, FiPeer: String; TimeStamp: Int64): Int64;
var
  counter: Integer;
begin
  Result := 0;
  EnterCriticalSection(CS_FileManager);
  for counter := 0 to High(ArrFileMgr) do
  begin
    if ((UpperCase(ArrFileMgr[counter].FiType) = UpperCase(FiType)) and
      (UpperCase(ArrFileMgr[counter].FiFile) = UpperCase(FiFile)) and
      (UpperCase(ArrFileMgr[counter].FiPeer) = UpperCase(FiPeer))) then
    begin
      Result := TimeStamp - ArrFileMgr[counter].FiLast;
      Delete(ArrFileMgr, Counter, 1);
      Break;
    end;
  end;
  LeaveCriticalSection(CS_FileManager);
end;

function GetFileProcessCopy(): TFileMCopy;
begin
  Setlength(Result, 0);
  EnterCriticalSection(CS_FileManager);
  Result := copy(ArrFileMgr, 0, length(ArrFileMgr));
  LeaveCriticalSection(CS_FileManager);
end;

{$ENDREGION}

{$REGION Deep debug control}

procedure InitDeepDeb(LFileName: String; SysInfo: String = '');
begin
  if DeepDebFilename <> '' then Exit;
  if InitializeLogFile(LFileName, SysInfo) then
  begin
    DeepDebFilename := LFileName;
    //ToDeepDeb(SysInfo);
  end;
end;


procedure ToDeepDeb(LLine: String);
begin
  EnterCriticalSection(CS_DeepDeb);
  SLDeepDebLog.Add(LLine);
  LeaveCriticalSection(CS_DeepDeb);
end;

function GetDeepDebLine(out LineContent: String): Boolean;
begin
  Result := False;
  if SLDeepDebLog.Count > 0 then
  begin
    EnterCriticalSection(CS_DeepDeb);
    LineContent := SLDeepDebLog[0];
    SLDeepDebLog.Delete(0);
    Result := True;
    if DeepDebFilename <> '' then
      SaveTextToDisk(DateTimeToStr(Now) + ' ' + LineContent, DeepDebFilename);
    LeaveCriticalSection(CS_DeepDeb);
  end;
end;

{$ENDREGION}

initialization
  SLDeepDebLog := TStringList.Create;
  Setlength(ArrPerformance, 0);
  Setlength(ArrNDLogs, 0);
  Setlength(ArrNDCSs, 0);
  Setlength(ArrNDSLs, 0);
  Setlength(ArrProcess, 0);
  Setlength(ArrFileMgr, 0);
  InitCriticalSection(CS_ThManager);
  InitCriticalSection(CS_FileManager);
  InitCriticalSection(CS_DeepDeb);


finalization
  DoneCriticalSection(CS_ThManager);
  DoneCriticalSection(CS_FileManager);
  DoneCriticalSection(CS_DeepDeb);
  FreeAllLogs;
  SLDeepDebLog.Free;

end. {END UNIT}
