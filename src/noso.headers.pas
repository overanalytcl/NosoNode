/// Standalone unit to control headers file.
unit Noso.Headers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Noso.Debug, Noso.Crypto, Noso.General;

type
  /// <summary>
  /// Record to hold summary data for a block
  /// </summary>
  TBlockSummary = packed record
    /// <summary>
    /// The current block number.
    /// </summary>
    BlockNumber: Integer;

    /// <summary>
    /// The block's hash.
    /// </summary>
    BlockHash: String[32];

    /// <summary>
    /// The sum hash.
    /// </summary>
    SumHash: String[32];
  end;

/// <summary>
/// Sets the hash of the summary file to reflect the current file state.
/// </summary>
procedure SetSummaryFileHash();

/// <summary>
/// Retrieves the current hash of the summary file.
/// </summary>
/// <returns>The hash of the summary file.</returns>
function GetSummaryFileHash(): String;

/// <summary>
/// Sets the path and name of the headers file.
/// </summary>
/// <param name="Filename">The path and name of the headers file.</param>
/// <returns>@true if the file name was set successfully; otherwise, @false.</returns>
function SetHeadersFileName(const Filename: String): Boolean;

/// <summary>
/// Creates a new headers file or overwrites an existing one.
/// </summary>
/// <returns>@true if the headers file was created successfully; otherwise, @false.</returns>
function CreateHeadersFile(): Boolean;

/// <summary>
/// Adds a new record to the headers file.
/// </summary>
/// <param name="BlockNumber">The block number to be added.</param>
/// <param name="BlockHash">The hash of the block.</param>
/// <param name="SumHash">The sum hash associated with the block.</param>
/// <returns>@true if the record was added successfully; otherwise, @false.</returns>
function AddRecordToHeaders(const BlockNumber: Int64;
  const BlockHash, SumHash: String): Boolean;

/// <summary>
/// Removes the last record from the headers file.
/// </summary>
/// <returns>@true if the record was removed successfully; otherwise, @false.</returns>
function RemoveLastHeaderRecord(): Boolean;

/// <summary>
/// Retrieves the number of records in the headers file.
/// </summary>
/// <returns>The number of records in the headers file.</returns>
function GetHeadersCount(): Integer;

/// <summary>
/// Retrieves the block number of the last record in the headers file.
/// </summary>
/// <returns>The block number of the last record.</returns>
function GetLastHeaderBlock(): Integer;

/// <summary>
/// Loads the headers file into a memory stream.
/// </summary>
/// <param name="Stream">The memory stream to load the headers into.</param>
/// <returns>The size of the loaded headers in bytes.</returns>
function GetHeadersAsMemoryStream(var Stream: TMemoryStream): Int64;

/// <summary>
/// Saves the contents of a memory stream as the headers file.
/// </summary>
/// <param name="Stream">The memory stream containing the headers data.</param>
/// <returns>@true if the stream was saved successfully; otherwise, @false.</returns>
function SaveMemoryStreamAsHeaders(var Stream: TMemoryStream): Boolean;

/// <summary>
/// Retrieves headers data as a string starting from a specified block.
/// </summary>
/// <param name="FromBlock">The block number from which to start retrieving headers.</param>
/// <returns>A string representing the headers data from the specified block.</returns>
function GetHeadersAsStringFromBlock(FromBlock: Integer): String;

var
  /// <summary>
  /// The file handle for the summary file.
  /// </summary>
  SummaryFile: file of TBlockSummary;

  /// <summary>
  /// The current hash of the summary file.
  /// </summary>
  CurrentSummaryFileHash: String = '';

  /// <summary>
  /// The path and name of the summary file.
  /// </summary>
  SummaryFilename: String = {'NOSODATA'+DirectorySeparator+}'blchhead.nos';

  /// <summary>
  /// Critical section for synchronizing access to the headers file.
  /// </summary>
  HeadersFileCriticalSection: TRTLCriticalSection;

implementation

{$REGION Headers file access}

procedure SetSummaryFileHash();
begin
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    CurrentSummaryFileHash := HashMD5File(SummaryFilename);
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function GetSummaryFileHash(): String;
begin
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    Result := CurrentSummaryFileHash;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function SetHeadersFileName(const Filename: String): Boolean;
begin
  Result := True;
  SummaryFilename := Filename;
  AssignFile(SummaryFile, SummaryFilename);

  if not FileExists(SummaryFilename) then
    CreateEmptyFile(SummaryFilename);

  SetSummaryFileHash();
end;

function CreateHeadersFile(): Boolean;
var
  TempStream: TMemoryStream;
begin
  Result := True;
  TempStream := TMemoryStream.Create;
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      TempStream.SaveToFile(SummaryFilename);
    except
      on E: Exception do
      begin
        Result := False;
        ToDeepDebug('NosoHeaders,CreateHeadersFile,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
    TempStream.Free;
  end;
end;

function AddRecordToHeaders(const BlockNumber: Int64;
  const BlockHash, SumHash: String): Boolean;
var
  BlockSummary: TBlockSummary;
begin
  Result := True;
  BlockSummary.BlockNumber := BlockNumber;
  BlockSummary.BlockHash := BlockHash;
  BlockSummary.SumHash := SumHash;
  FileMode := fmOpenWrite;
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Reset(SummaryFile);
      Seek(SummaryFile, FileSize(SummaryFile));
      Write(SummaryFile, BlockSummary);
    except
      on E: Exception do
      begin
        Result := False;
        ToDeepDebug('NosoHeaders,AddRecordToHeaders,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function RemoveLastHeaderRecord(): Boolean;
begin
  Result := True;
  FileMode := fmOpenWrite;
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Reset(SummaryFile);
      Seek(SummaryFile, FileSize(SummaryFile) - 1);
      Truncate(SummaryFile);
    except
      on E: Exception do
      begin
        Result := False;
        ToDeepDebug('NosoHeaders,RemoveLastHeaderRecord,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function GetHeadersCount(): Integer;
begin
  Result := -1;
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Reset(SummaryFile);
      Result := FileSize(SummaryFile);
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoHeaders,GetHeadersCount,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function GetLastHeaderBlock(): Integer;
var
  LastRecord: TBlockSummary;
begin
  Result := 0;
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Reset(SummaryFile);
      if FileSize(SummaryFile) > 0 then
      begin
        Seek(SummaryFile, FileSize(SummaryFile) - 1);
        Read(SummaryFile, LastRecord);
        Result := LastRecord.BlockNumber;
      end;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoHeaders,GetLastHeaderBlock,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function GetHeadersAsMemoryStream(var Stream: TMemoryStream): Int64;
begin
  Result := 0;
  StartPerformanceMeasurement('GetHeadersAsMemoryStream');
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Stream.LoadFromFile(SummaryFilename);
      Result := Stream.Size;
      Stream.Position := 0;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoHeaders,GetHeadersAsMemoryStream,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
    StopPerformanceMeasurement('GetHeadersAsMemoryStream');
  end;
end;

function SaveMemoryStreamAsHeaders(var Stream: TMemoryStream): Boolean;
begin
  Result := False;
  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Stream.SaveToFile(SummaryFilename);
      Result := True;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoHeaders,SaveMemoryStreamAsHeaders,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
  end;
end;

function GetHeadersAsStringFromBlock(FromBlock: Integer): String;
var
  CurrentRecord: TBlockSummary;
begin
  Result := '';

  if FromBlock < GetLastHeaderBlock() - 1008 then
    Exit;

  EnterCriticalSection(HeadersFileCriticalSection);
  try
    try
      Reset(SummaryFile);
      Seek(SummaryFile, FromBlock - 100);
      while not EOF(SummaryFile) do
      begin
        Read(SummaryFile, CurrentRecord);
        Result := Result + Format('%d:%s:%s ',
          [CurrentRecord.BlockNumber, CurrentRecord.BlockHash, CurrentRecord.SumHash]);
      end;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoHeaders,GetHeadersAsStringFromBlock,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(HeadersFileCriticalSection);
    Result := Trim(Result);
  end;
end;

{$ENDREGION}

initialization
  InitCriticalSection(HeadersFileCriticalSection);
  AssignFile(SummaryFile, SummaryFilename);

finalization
  DoneCriticalSection(HeadersFileCriticalSection);

end.
