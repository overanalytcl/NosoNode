{ Unit to implement debug functionalities on Noso. }
unit Noso.Debug;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  /// <summary>
  /// Record to hold performance statistics
  /// </summary>
  TPerformanceStats = record

    /// <summary>
    /// Identifier for the performance measurement
    /// </summary>
    Tag: String;

    /// <summary>
    /// Timestamp when the measurement started
    /// </summary>
    StartTime: Int64;

    /// <summary>
    /// Average time recorded
    /// </summary>
    AverageTime: Int64;

    /// <summary>
    /// Maximum time recorded
    /// </summary>
    MaxTime: Int64;

    /// <summary>
    /// Minimum time recorded
    /// </summary>
    MinTime: Int64;

    /// <summary>
    /// Number of measurements
    /// </summary>
    Count: Int64;

    /// <summary>
    /// Total time recorded
    /// </summary>
    TotalTime: Int64;
  end;

  /// <summary>
  /// Record for logging non-disk operations.
  /// </summary>
  TLogNonDisk = record
    /// <summary>
    /// Identifier for the log entry.
    /// </summary>
    Tag: String;

    /// <summary>
    /// Number of log entries.
    /// </summary>
    Count: Integer;

    /// <summary>
    /// Flag indicating if logs should be written to disk.
    /// </summary>
    LogToDisk: Boolean;

    /// <summary>
    /// Name of the log file.
    /// </summary>
    Filename: String;
  end;

  /// <summary>
  /// Record for thread management.
  /// </summary>
  TThreadManager = record
    /// <summary>
    /// Name of the thread.
    /// </summary>
    ThreadName: String;

    /// <summary>
    /// Timestamp when the thread started.
    /// </summary>
    StartTime: Int64;

    /// <summary>
    /// Timestamp when the thread was last active.
    /// </summary>
    LastActiveTime: Int64;
  end;

  /// <summary>
  /// Record for file management.
  /// </summary>
  TFileManager = record
    /// <summary>
    /// Type of the file (e.g., 'text', 'binary').
    /// </summary>
    FileType: String;

    /// <summary>
    /// Name of the file.
    /// </summary>
    FileName: String;

    /// <summary>
    /// Peer associated with the file.
    /// </summary>
    Peer: String;

    /// <summary>
    /// Timestamp when the file was last accessed.
    /// </summary>
    LastAccessTime: Int64;
  end;

  /// <summary>
  /// Array of thread management records.
  /// </summary>
  TThreadManagerArray = specialize TArray<TThreadManager>;

  /// <summary>
  /// Array of file management records.
  /// </summary>
  TFileManagerArray = specialize TArray<TFileManager>;

/// <summary>
/// Starts performance measurement for a given tag by storing the current timestamp.
/// If the tag already exists, it updates the start time and increments the count.
/// </summary>
/// <param name="Tag">Identifier for the performance measurement.</param>
procedure StartPerformanceMeasurement(const Tag: String);

/// <summary>
/// Stops performance measurement for a given tag and calculates the duration.
/// Updates the total, max, min, and average times for the tag.
/// </summary>
/// <param name="Tag">Identifier for the performance measurement.</param>
/// <returns>The duration of the measurement in milliseconds.</returns>
function StopPerformanceMeasurement(const Tag: String): Int64;

/// <summary>
/// Logs performance statistics to a specified file.
/// The output includes the tag, count, maximum time, and average time for each tag.
/// </summary>
/// <param name="Destination">File path where the performance statistics will be saved.</param>
/// <returns>@true if the logging was successful, otherwise @false.</returns>
function LogPerformanceToFile(const Destination: String): Boolean;

/// <summary>
/// Creates a new log with a specified name and optional file.
/// </summary>
/// <param name="Name">Name of the log.</param>
/// <param name="FileName">Optional log file name.</param>
procedure CreateNewLog(const Name: String; const FileName: String = '');

/// <summary>
/// Appends a new line to a specified log.
/// </summary>
/// <param name="Tag">Identifier for the log.</param>
/// <param name="Line">Content of the new line to append.</param>
procedure ToLog(const Tag, Line: String);

/// <summary>
/// Retrieves the oldest log line for a specified log.
/// </summary>
/// <param name="Tag">Identifier for the log.</param>
/// <param name="Line">Output parameter containing the content of the log line.</param>
/// <returns>True if the line was successfully retrieved, otherwise False.</returns>
function GetLogLine(const Tag: String; out Line: String): Boolean;

/// <summary>
/// Adds a new open thread to the thread management list.
/// </summary>
/// <param name="ThreadName">Name of the thread.</param>
/// <param name="TimeStamp">Timestamp when the thread was created.</param>
procedure AddNewOpenThread(const ThreadName: String; const TimeStamp: Int64);

/// <summary>
/// Updates the timestamp of an existing thread.
/// </summary>
/// <param name="ThreadName">Name of the thread.</param>
/// <param name="TimeStamp">New timestamp for the thread.</param>
procedure UpdateOpenThread(const ThreadName: String; const TimeStamp: Int64);

/// <summary>
/// Closes an open thread.
/// </summary>
/// <param name="ThreadName">Name of the thread to close.</param>
procedure CloseOpenThread(const ThreadName: String);

/// <summary>
/// Retrieves the current list of open threads.
/// </summary>
/// <returns>An array of thread management records.</returns>
function GetProcessCopy(): TThreadManagerArray;

/// <summary>
/// Adds a new file process to the file management list.
/// </summary>
/// <param name="FileType">Type of the file.</param>
/// <param name="FileName">Name of the file.</param>
/// <param name="Peer">Peer associated with the file.</param>
/// <param name="TimeStamp">Timestamp when the file process started.</param>
procedure AddFileProcess(const FileType, FileName, Peer: String; const TimeStamp: Int64);

/// <summary>
/// Closes a file process and returns the elapsed time.
/// </summary>
/// <param name="FileType">Type of the file.</param>
/// <param name="FileName">Name of the file.</param>
/// <param name="Peer">Peer associated with the file.</param>
/// <param name="TimeStamp">Timestamp when the file process ended.</param>
/// <returns>The elapsed time in milliseconds.</returns>
function CloseFileProcess(const FileType, FileName, Peer: String;
  const TimeStamp: Int64): Int64;

/// <summary>
/// Retrieves the current list of file processes.
/// </summary>
/// <returns>An array of file management records.</returns>
function GetFileProcessCopy(): TFileManagerArray;

/// <summary>
/// Initializes deep debugging with a specified log file and optional system information.
/// </summary>
/// <param name="LogFileName">Filename for the deep debug log.</param>
/// <param name="SysInfo">Optional system information.</param>
procedure InitializeDeepDebug(const LogFileName: String; const SysInfo: String = '');

/// <summary>
/// Adds a new line to the deep debug log.
/// </summary>
/// <param name="Line">Content of the line to append.</param>
procedure ToDeepDebug(const Line: String);

/// <summary>
/// Retrieves the oldest line from the deep debug log.
/// </summary>
/// <param name="Line">Output parameter containing the content of the log line.</param>
/// <returns>@true if the line was successfully retrieved, otherwise @false.</returns>
function GetDeepDebugLine(out Line: String): Boolean;

var
  /// <summary>
  /// Array to store performance statistics.
  /// </summary>
  PerformanceStats: array of TPerformanceStats;

  /// <summary>
  /// Flag to enable or disable performance logging.
  /// </summary>
  EnablePerformanceLogging: Boolean = False;

  /// <summary>
  /// Array of non-disk log entries.
  /// </summary>
  NonDiskLogs: array of TLogNonDisk;

  /// <summary>
  /// Array of critical sections for managing non-disk logs.
  /// </summary>
  CSNonDisks: array of TRTLCriticalSection;

  /// <summary>
  /// Array of string lists associated with non-disk logs.
  /// </summary>
  NonDiskStringLists: array of TStringList;

  /// <summary>
  /// Array of thread management records.
  /// </summary>
  ProcessList: array of TThreadManager;

  /// <summary>
  /// Critical section for thread management.
  /// </summary>
  CSThreadManager: TRTLCriticalSection;

  /// <summary>
  /// Array of file management records.
  /// </summary>
  FileManagers: array of TFileManager;

  /// <summary>
  /// Critical section for file management.
  /// </summary>
  CSFileManager: TRTLCriticalSection;

  /// <summary>
  /// String list for deep debugging logs.
  /// </summary>
  DeepDebugLogStringList: TStringList;

  /// <summary>
  /// Critical section for deep debugging logs.
  /// </summary>
  CSDeepDebug: TRTLCriticalSection;

  /// <summary>
  /// Filename for deep debug logs.
  /// </summary>
  DeepDebugFilename: String = '';

implementation

procedure StartPerformanceMeasurement(const Tag: String);
var
  i: Integer;
  Stats: TPerformanceStats;
begin
  if not EnablePerformanceLogging then
    Exit;

  for i := 0 to High(PerformanceStats) do
  begin
    if Tag = PerformanceStats[i].Tag then
    begin
      PerformanceStats[i].StartTime := GetTickCount64;
      Inc(PerformanceStats[i].Count);
      Exit;
    end;
  end;

  Stats := Default(TPerformanceStats);
  Stats.Tag := Tag;
  Stats.MinTime := 99999;
  Stats.StartTime := GetTickCount64;
  Stats.Count := 1;

  Insert(Stats, PerformanceStats, Length(PerformanceStats));
end;

function StopPerformanceMeasurement(const Tag: String): Int64;
var
  i: Integer;
  Duration: Int64 = 0;
begin
  Result := 0;
  if not EnablePerformanceLogging then Exit;
  for i := 0 to High(PerformanceStats) do
  begin
    if Tag = PerformanceStats[i].Tag then
    begin
      Duration := GetTickCount64 - PerformanceStats[i].StartTime;

      Inc(PerformanceStats[i].TotalTime, Duration);

      PerformanceStats[i].AverageTime :=
        PerformanceStats[i].TotalTime div PerformanceStats[i].Count;

      PerformanceStats[i].MaxTime :=
        Max(PerformanceStats[i].MaxTime, Duration);

      PerformanceStats[i].MinTime :=
        Min(PerformanceStats[i].MinTime, Duration);

      Break;
    end;
  end;
  Result := Duration;
end;

function LogPerformanceToFile(const Destination: String): Boolean;
var
  i: Integer;
  Lines: TStringList;
  Line: String;
begin
  Result := True;

  Lines := TStringList.Create;
  try
    try
      Lines.Add(Format('%-40s %10s %10s %10s', ['TAG', 'Count', 'Max', 'Average']));

      for i := 0 to High(PerformanceStats) do
      begin
        Line := Format('%-40s %10d %10d %10d',
          [PerformanceStats[i].Tag, PerformanceStats[i].Count,
          PerformanceStats[i].MaxTime, PerformanceStats[i].AverageTime]);

        Lines.Add(Line);
      end;

      // Try saving all lines to the specified file
      Lines.SaveToFile(Destination);
    except
      on E: Exception do
      begin
        Result := False;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

/// <summary>
/// Initializes the log file by creating it if it does not exist.
/// Optionally, writes an initial header to the log file.
/// </summary>
/// <param name="Filename">The name of the log file to be created or initialized.</param>
/// <param name="Header">Optional header to write into the log file.</param>
/// <returns>@true if the file was successfully created or already exists; @false if an error occurs.</returns>
function InitializeLogFile(const Filename: String; const Header: String = ''): Boolean;
var
  LogFile: TextFile;
begin
  Result := True;
  if not FileExists(Filename) then
  begin
    try
      AssignFile(LogFile, Filename);
      Rewrite(LogFile);
      try
        if Header <> '' then
          WriteLn(LogFile, Header);
      finally
        CloseFile(LogFile);
      end;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoDebug,InitializeLogFile, ' + E.Message);
        Result := False;
      end;
    end;
  end;
end;

/// <summary>
/// Appends a line of text to the specified log file.
/// Logs any errors encountered during the operation.
/// </summary>
/// <param name="TextLine">The line of text to be written to the log file.</param>
/// <param name="Filename">The name of the log file to which the text should be written.</param>
procedure SaveTextToDisk(const TextLine, Filename: String);
var
  LogFile: TextFile;
begin
  try
    if not FileExists(Filename) then
      if not InitializeLogFile(Filename) then
        Exit;  // Exit if log file can't be initialized

    AssignFile(LogFile, Filename);
    Append(LogFile);
    try
      WriteLn(LogFile, TextLine);
    finally
      CloseFile(LogFile);
    end;
  except
    on E: Exception do
      ToDeepDebug('NosoDebug, SaveTextToDisk, ' + E.Message);
  end;
end;

procedure CreateNewLog(const Name: String; const FileName: String = '');
var
  Log: TLogNonDisk;
  i: Integer;
begin
  Log := Default(TLogNonDisk);
  Log.Tag := UpperCase(Name);
  Log.Filename := FileName;

  // Increase the size of the critical section array and string list
  i := Length(CSNonDisks);
  SetLength(CSNonDisks, i + 1);
  InitCriticalSection(CSNonDisks[i]);

  SetLength(NonDiskStringLists, i + 1);
  NonDiskStringLists[i] := TStringList.Create;

  // If a file is provided, initialize the log file and mark it for disk logging
  if FileName <> '' then
  begin
    if InitializeLogFile(FileName) then
      Log.LogToDisk := True
    else
      ToDeepDebug('NosoDebug,CreateNewLog,Failed to initialize log file: ' + FileName);
  end;

  // Add the new log to the log array
  Insert(Log, NonDiskLogs, Length(NonDiskLogs));
end;

procedure ToLog(const Tag, Line: String);
var
  i: Integer;
begin
  for i := 0 to Length(NonDiskLogs) - 1 do
  begin
    if NonDiskLogs[i].Tag = UpperCase(Tag) then
    begin
      EnterCriticalSection(CSNonDisks[i]);
      try
        // Add the log line to the in-memory list
        NonDiskStringLists[i].Add(Line);
        Inc(NonDiskLogs[i].Count);

        // If logging to disk is enabled, save to the file
        if NonDiskLogs[i].LogToDisk then
          SaveTextToDisk(Line, NonDiskLogs[i].Filename);
      finally
        LeaveCriticalSection(CSNonDisks[i]);
      end;
      Exit; // Exit once the matching log is found
    end;
  end;
end;

function GetLogLine(const Tag: String; out Line: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to Length(NonDiskLogs) - 1 do
  begin
    if NonDiskLogs[i].Tag = UpperCase(Tag) then
    begin
      EnterCriticalSection(CSNonDisks[i]);
      try
        // Check if there are any lines in the log
        if NonDiskStringLists[i].Count > 0 then
        begin
          // Retrieve the first line from the log
          Line := NonDiskStringLists[i][0];
          NonDiskStringLists[i].Delete(0); // Remove it after retrieval
          Result := True;

          // If disk logging is enabled, save the line to the file
          if NonDiskLogs[i].LogToDisk then
            SaveTextToDisk(Line, NonDiskLogs[i].Filename);
        end;
      finally
        LeaveCriticalSection(CSNonDisks[i]);
      end;
      Exit; // Exit once the matching log is found
    end;
  end;
end;


/// <summary>
/// Free all logs, along with their associated resources.
/// </summary>
procedure FreeAllLogs;
var
  i: Integer;
begin
  // Iterate through each log and free the associated resources
  for i := 0 to High(NonDiskLogs) do
  begin
    // Free the TStringList used for in-memory logging
    if Assigned(NonDiskStringLists[i]) then
    begin
      NonDiskStringLists[i].Free;
      NonDiskStringLists[i] := nil;
    end;

    // Clean up the critical section associated with each log
    DoneCriticalSection(CSNonDisks[i]);
  end;

  // Clear the arrays after all resources are freed
  SetLength(NonDiskLogs, 0);
  SetLength(CSNonDisks, 0);
  SetLength(NonDiskStringLists, 0);
end;

procedure AddNewOpenThread(const ThreadName: String; const TimeStamp: Int64);
var
  NewThread: TThreadManager;
begin
  NewThread := Default(TThreadManager);
  NewThread.ThreadName := ThreadName;
  NewThread.StartTime := TimeStamp;
  NewThread.LastActiveTime := TimeStamp;

  EnterCriticalSection(CSThreadManager);
  try
    Insert(NewThread, ProcessList, Length(ProcessList));
  finally
    LeaveCriticalSection(CSThreadManager);
  end;
end;

procedure UpdateOpenThread(const ThreadName: String; const TimeStamp: Int64);
var
  i: Integer;
begin
  EnterCriticalSection(CSThreadManager);
  try
    for i := 0 to High(ProcessList) do
    begin
      if SameText(ProcessList[i].ThreadName, ThreadName) then
      begin
        ProcessList[i].LastActiveTime := TimeStamp;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(CSThreadManager);
  end;
end;

procedure CloseOpenThread(const ThreadName: String);
var
  i: Integer;
begin
  EnterCriticalSection(CSThreadManager);
  try
    for i := 0 to High(ProcessList) do
    begin
      if SameText(ProcessList[i].ThreadName, ThreadName) then
      begin
        Delete(ProcessList, i, 1);
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(CSThreadManager);
  end;
end;

function GetProcessCopy(): TThreadManagerArray;
begin
  SetLength(Result, 0);

  EnterCriticalSection(CSThreadManager);
  try
    Result := Copy(ProcessList, 0, Length(ProcessList));
  finally
    LeaveCriticalSection(CSThreadManager);
  end;
end;

procedure AddFileProcess(const FileType, FileName, Peer: String; const TimeStamp: Int64);
var
  NewFile: TFileManager;
begin
  NewFile := Default(TFileManager);
  NewFile.FileType := FileType;
  NewFile.FileName := FileName;
  NewFile.Peer := Peer;
  NewFile.LastAccessTime := TimeStamp;

  EnterCriticalSection(CSFileManager);
  try
    Insert(NewFile, FileManagers, Length(FileManagers));
  finally
    LeaveCriticalSection(CSFileManager);
  end;
end;

function CloseFileProcess(const FileType, FileName, Peer: String;
  const TimeStamp: Int64): Int64;
var
  i: Integer;
begin
  Result := 0;

  EnterCriticalSection(CSFileManager);
  try
    for i := 0 to High(FileManagers) do
    begin
      if (SameText(FileManagers[i].FileType, FileType) and
        SameText(FileManagers[i].FileName, FileName) and
        SameText(FileManagers[i].Peer, Peer)) then
      begin
        Result := TimeStamp - FileManagers[i].LastAccessTime;
        Delete(FileManagers, i, 1);
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(CSFileManager);
  end;
end;

function GetFileProcessCopy(): TFileManagerArray;
begin
  SetLength(Result, 0);

  EnterCriticalSection(CSFileManager);
  try
    Result := Copy(FileManagers, 0, Length(FileManagers));
  finally
    LeaveCriticalSection(CSFileManager);
  end;
end;

procedure InitializeDeepDebug(const LogFileName: String; const SysInfo: String = '');
begin
  if DeepDebugFilename <> '' then
    Exit;

  if InitializeLogFile(LogFileName, SysInfo) then
    DeepDebugFilename := LogFileName;
end;


procedure ToDeepDebug(const Line: String);
begin
  EnterCriticalSection(CSDeepDebug);
  try
    DeepDebugLogStringList.Add(Line);
  finally
    LeaveCriticalSection(CSDeepDebug);
  end;
end;

function GetDeepDebugLine(out Line: String): Boolean;
begin
  Result := False;

  EnterCriticalSection(CSDeepDebug);
  try
    if DeepDebugLogStringList.Count > 0 then
    begin
      Line := DeepDebugLogStringList[0];
      DeepDebugLogStringList.Delete(0);
      Result := True;

      if DeepDebugFilename <> '' then
        SaveTextToDisk(DateTimeToStr(Now) + ' ' + Line, DeepDebugFilename);
    end;
  finally
    LeaveCriticalSection(CSDeepDebug);
  end;
end;

initialization
  // Initialize global variables
  DeepDebugLogStringList := TStringList.Create;

  // Initialize dynamic arrays
  SetLength(PerformanceStats, 0);
  SetLength(NonDiskLogs, 0);
  SetLength(CSNonDisks, 0);
  SetLength(NonDiskStringLists, 0);
  SetLength(ProcessList, 0);
  SetLength(FileManagers, 0);

  // Initialize critical sections
  InitCriticalSection(CSThreadManager);
  InitCriticalSection(CSFileManager);
  InitCriticalSection(CSDeepDebug);

finalization
  // Free critical sections
  DoneCriticalSection(CSDeepDebug);
  DoneCriticalSection(CSFileManager);
  DoneCriticalSection(CSThreadManager);

  // Free all log-related resources
  FreeAllLogs;

  // Free dynamically allocated string list
  DeepDebugLogStringList.Free;

end.
