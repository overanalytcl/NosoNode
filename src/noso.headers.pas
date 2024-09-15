unit Noso.Headers;

{
nosoHeaders 1.1
Jan 31th, 2024
Stand alone unit to control headers file.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Noso.Debug, Noso.Crypto, Noso.General;

type

  ResumenData = packed record
    block: Integer;
    blockhash: String[32];
    SumHash: String[32];
  end;

procedure SetResumenHash();
function GetResumenHash(): String;
function SetHeadersFileName(Filename: String): Boolean;
function CreateHeadersFile(): Boolean;
function AddRecordToHeaders(BlockNumber: Int64; BlockHash, SumHash: String): Boolean;
function RemoveHeadersLastRecord(): Boolean;
function GetHeadersHeigth(): Integer;
function GetHeadersLastBlock(): Integer;
function GetHeadersAsMemStream(var LMs: TMemoryStream): Int64;
function SaveStreamAsHeaders(var LStream: TMemoryStream): Boolean;
function LastHeadersString(FromBlock: Integer): String;



var
  FileResumen: file of ResumenData;
  MyResumenHash: String = '';
  ResumenFilename: String = {'NOSODATA'+DirectorySeparator+}'blchhead.nos';
  CS_HeadersFile: TRTLCriticalSection;

implementation

{$REGION Headers file access}

// Sets the hash value
procedure SetResumenHash();
begin
  EnterCriticalSection(CS_HeadersFile);
  MyResumenHash := HashMD5File(ResumenFilename);
  LeaveCriticalSection(CS_HeadersFile);
end;

// Returns the hash of the file
function GetResumenHash(): String;
begin
  EnterCriticalSection(CS_HeadersFile);
  Result := MyResumenHash;
  LeaveCriticalSection(CS_HeadersFile);
end;

// Sets the headers file path and name
function SetHeadersFileName(Filename: String): Boolean;
begin
  Result := True;
  ResumenFilename := Filename;
  assignfile(FileResumen, ResumenFilename);
  if not Fileexists(ResumenFilename) then CreateEmptyFile(ResumenFilename);
  SetResumenHash();
end;

// Creates a headers file. If it already exists, rewrite a new empty one.
function CreateHeadersFile(): Boolean;
var
  MyStream: TMemoryStream;
begin
  Result := True;
  MyStream := TMemoryStream.Create;
  EnterCriticalSection(CS_HeadersFile);
  try
    MYStream.SaveToFile(ResumenFilename);
  except
    ON E: Exception do
    begin
      Result := False;
      ToDeepDebug('NosoHeaders,CreateHeadersFile,' + E.Message);
    end;
  end;
  LeaveCriticalSection(CS_HeadersFile);
  MyStream.Free;
end;

function AddRecordToHeaders(BlockNumber: Int64; BlockHash, SumHash: String): Boolean;
var
  NewData: ResumenData;
  Opened: Boolean = False;
  PorperlyClosed: Boolean = False;
begin
  Result := True;
  NewData := Default(ResumenData);
  NewData.block := BlockNumber;
  NewData.blockhash := BlockHash;
  NewData.SumHash := SumHash;
  filemode := 2;
  EnterCriticalSection(CS_HeadersFile);
  try
    reset(FileResumen);
    Opened := True;
    seek(fileResumen, filesize(fileResumen));
    Write(fileResumen, NewData);
    closefile(FileResumen);
    PorperlyClosed := True;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoHeaders,AddRecordToHeaders,' + E.Message);
      Result := False;
    end;
  end;
  if ((opened) and (not PorperlyClosed)) then closefile(FileResumen);
  LeaveCriticalSection(CS_HeadersFile);
end;

function RemoveHeadersLastRecord(): Boolean;
var
  Opened: Boolean = False;
  PorperlyClosed: Boolean = False;
begin
  Result := True;
  filemode := 2;
  EnterCriticalSection(CS_HeadersFile);
  try
    reset(FileResumen);
    Opened := True;
    seek(fileResumen, filesize(fileResumen) - 1);
    truncate(fileResumen);
    closefile(FileResumen);
    PorperlyClosed := True;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoHeaders,RemoveHeadersLastRecord,' + E.Message);
      Result := False;
    end;
  end;
  if ((opened) and (not PorperlyClosed)) then closefile(FileResumen);
  LeaveCriticalSection(CS_HeadersFile);
end;

function GetHeadersHeigth(): Integer;
var
  Opened: Boolean = False;
  PorperlyClosed: Boolean = False;
begin
  Result := -1;
  EnterCriticalSection(CS_HeadersFile);
  try
    reset(FileResumen);
    Opened := True;
    Result := filesize(fileResumen) - 1;
    closefile(FileResumen);
    PorperlyClosed := True;
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoHeaders,GetHeadersHeigth,' + E.Message);
    end;
  end;
  if ((opened) and (not PorperlyClosed)) then closefile(FileResumen);
  LeaveCriticalSection(CS_HeadersFile);
end;

// Returns the block number of the last record on headers
function GetHeadersLastBlock(): Integer;
var
  ThisData: ResumenData;
  Opened: Boolean = False;
  PorperlyClosed: Boolean = False;
begin
  Result := 0;
  ThisData := Default(ResumenData);
  EnterCriticalSection(CS_HeadersFile);
  try
    reset(FileResumen);
    Opened := True;
    if filesize(FileResumen) > 0 then
    begin
      seek(fileResumen, filesize(FileResumen) - 1);
      Read(fileResumen, ThisData);
      Result := ThisData.block;
    end;
    CloseFile(FileResumen);
    PorperlyClosed := True;
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoHeaders,GetHeadersLastBlock,' + E.Message);
    end;
  end;
  if ((opened) and (not PorperlyClosed)) then closefile(FileResumen);
  LeaveCriticalSection(CS_HeadersFile);
end;

// Returns the headers file as a STREAM
function GetHeadersAsMemStream(var LMs: TMemoryStream): Int64;
begin
  Result := 0;
  BeginPerformance('GetHeadersAsMemStream');
  EnterCriticalSection(CS_HeadersFile);
  try
    LMs.LoadFromFile(ResumenFilename);
    Result := LMs.Size;
    LMs.Position := 0;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoHeaders,GetHeadersAsMemStream,' + E.Message);
    end;
  end;
  LeaveCriticalSection(CS_HeadersFile);
  EndPerformance('GetHeadersAsMemStream');
end;

// Save a provided stream as the headers file
function SaveStreamAsHeaders(var LStream: TMemoryStream): Boolean;
begin
  Result := False;
  EnterCriticalSection(CS_HeadersFile);
  try
    LStream.SaveToFile(ResumenFilename);
    Result := True;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoHeaders,SaveStreamAsHeaders,' + E.Message);
    end;
  end{Try};
  LeaveCriticalSection(CS_HeadersFile);
end;

// Returns the string for headers updates
function LastHeadersString(FromBlock: Integer): String;
var
  ThisData: ResumenData;
  Opened: Boolean = False;
  PorperlyClosed: Boolean = False;
begin
  Result := '';
  if FromBlock < GetHeadersLastBlock - 1008 then exit;
  EnterCriticalSection(CS_HeadersFile);
  try
    reset(FileResumen);
    Opened := True;
    ThisData := Default(ResumenData);
    seek(fileResumen, FromBlock - 100);
    while not EOF(fileResumen) do
    begin
      Read(fileResumen, ThisData);
      Result := Result + ThisData.block.ToString + ':' + ThisData.blockhash +
        ':' + ThisData.SumHash + ' ';
    end;
    closefile(FileResumen);
    PorperlyClosed := True;
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoHeaders,LastHeadersString,' + E.Message);
    end;
  end;
  //if ( (opened) and (not PorperlyClosed) ) then closefile(FileResumen);
  LeaveCriticalSection(CS_HeadersFile);
  Result := Trim(Result);
end;

{$ENDREGION}

initialization
  InitCriticalSection(CS_HeadersFile);
  assignfile(FileResumen, ResumenFilename);

finalization
  DoneCriticalSection(CS_HeadersFile);

end.
