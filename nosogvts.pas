unit nosogvts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, nosodebug, nosocrypto;

type
  TGVT = packed record
    number: String[2];
    owner: String[32];
    Hash: String[64];
    control: Integer;
  end;

function CreateGVTsFile(): Boolean;
function GetGVTsAsStream(out LStream: TMemoryStream): Int64;
function SaveStreamAsGVTs(const LStream: TMemoryStream): Boolean;
procedure GetGVTsFileData();
procedure SaveGVTs();
function ChangeGVTOwner(Lnumber: Integer; OldOwner, NewOWner: String): Integer;
function GetGVTIndex(Index: Integer): TGVT;
function GetGVTLength: Integer;
function CountAvailableGVTs(): Integer;
function GetGVTPrice(available: Integer; ToSell: Boolean = False): Int64;

const
  GVTBaseValue = 70000000000;

var
  GVTsFilename: String = 'NOSODATA' + DirectorySeparator + 'gvts.psk';
  FileGVTs: file of TGVT;
  ArrGVTs: array of TGVT;
  CSGVTsArray: TRTLCriticalSection;
  CSGVTsFile: TRTLCriticalSection;
  MyGVTsHash: String = '';

implementation

// Creates a GVTs file. If it already exists, rewrite a new empty one.
function CreateGVTsFile(): Boolean;
var
  MyStream: TMemoryStream;
begin
  Result := True;
  MyStream := TMemoryStream.Create;
  EnterCriticalSection(CSGVTsFile);
  try
    MYStream.SaveToFile(GVTsFilename);
  except
    ON E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGVTs,CreateGVTsFile,' + E.Message);
    end;
  end;
  LeaveCriticalSection(CSGVTsFile);
  MyStream.Free;
end;

// Loads the GVTs file into a stream
function GetGVTsAsStream(out LStream: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(CSGVTsFile);
  try
    LStream.LoadFromFile(GVTsFilename);
    Result := LStream.Size;
    LStream.Position := 0;
  except
    ON E: Exception do
    begin
      ToDeepDeb('NosoGVTs,GetGVTsAsStream,' + E.Message);
    end;
  end{Try};
  LeaveCriticalSection(CSGVTsFile);
end;

// Save a stream as the GVT file
function SaveStreamAsGVTs(const LStream: TMemoryStream): Boolean;
begin
  Result := False;
  EnterCriticalSection(CSGVTsFile);
  try
    LStream.SaveToFile(GVTsFilename);
    Result := True;
  except
    ON E: Exception do
    begin
      ToDeepDeb('NosoGVTs,SaveStreamAsGVTs,' + E.Message);
    end;
  end{Try};
  LeaveCriticalSection(CSGVTsFile);
  MyGVTsHash := HashMD5File(GVTsFilename);
end;

procedure GetGVTsFileData();
var
  counter: Integer;
begin
  EnterCriticalSection(CSGVTsFile);
  EnterCriticalSection(CSGVTsArray);
  Assignfile(FileGVTs, GVTsFilename);
  try
    reset(FileGVTs);
    Setlength(ArrGVTs, filesize(FileGVTs));
    for counter := 0 to filesize(FileGVTs) - 1 do
    begin
      seek(FileGVTs, counter);
      Read(FileGVTs, ArrGVTs[counter]);
    end;
    Closefile(FileGVTs);
  except
    ON E: Exception do
    begin
      ToDeepDeb('NosoGVTs,GetGVTsFileData,' + E.Message);
    end;
  end;
  LeaveCriticalSection(CSGVTsArray);
  LeaveCriticalSection(CSGVTsFile);
  MyGVTsHash := HashMD5File(GVTsFilename);
end;

procedure SaveGVTs();
var
  counter: Integer;
begin
  EnterCriticalSection(CSGVTsFile);
  EnterCriticalSection(CSGVTsArray);
  try
    rewrite(FileGVTs);
    for counter := 0 to length(ArrGVTs) - 1 do
    begin
      seek(FileGVTs, counter);
      Write(FileGVTs, ArrGVTs[counter]);
    end;
    Closefile(FileGVTs);
  except
    ON E: Exception do
      ToDeepDeb('NosoGVTs,SaveGVTs,' + E.Message);
  end;
  LeaveCriticalSection(CSGVTsArray);
  LeaveCriticalSection(CSGVTsFile);
  MyGVTsHash := HashMD5File(GVTsFilename);
end;

function GetGVTIndex(Index: Integer): TGVT;
begin
  Result := Default(TGVT);
  if index > GetGVTLength - 1 then exit;
  EnterCriticalSection(CSGVTsArray);
  Result := ArrGVTs[index];
  LeaveCriticalSection(CSGVTsArray);
end;

function GetGVTLength: Integer;
begin
  EnterCriticalSection(CSGVTsArray);
  Result := length(ArrGVTs);
  LeaveCriticalSection(CSGVTsArray);
end;

function ChangeGVTOwner(Lnumber: Integer; OldOwner, NewOWner: String): Integer;
var
  LData: TGVT;
begin
  Result := 0;
  if LNumber > 99 then Result := 1;
  LData := GetGVTIndex(Lnumber);
  if LData.owner <> OldOwner then Result := 2;
  if not IsValidHashAddress(NewOWner) then Result := 3;
  if Result = 0 then
  begin
    EnterCriticalSection(CSGVTsArray);
    ArrGVTs[Lnumber].owner := NewOWner;
    LeaveCriticalSection(CSGVTsArray);
  end;
end;

function CountAvailableGVTs(): Integer;
var
  counter: Integer;
begin
  Result := 0;
  EnterCriticalSection(CSGVTsArray);
  for counter := 0 to length(ArrGVTs) - 1 do
    if ArrGVTs[counter].owner = 'NpryectdevepmentfundsGE' then Inc(Result);
  LeaveCriticalSection(CSGVTsArray);
end;

function GetGVTPrice(available: Integer; ToSell: Boolean = False): Int64;
var
  counter: Integer;
begin
  Result := GVTBaseValue;
  available := 40 - available;
  for counter := 1 to available do
    Result := (Result * 110) div 100;
  if Result < GVTBaseValue then Result := GVTBaseValue;
  if ToSell then Result := (Result * 85) div 100;
end;

initialization
  InitCriticalSection(CSGVTsArray);
  InitCriticalSection(CSGVTsFile);
  SetLength(ArrGVTs, 0);
  Assignfile(FileGVTs, GVTsFilename);

finalization
  DoneCriticalSection(CSGVTsArray);
  DoneCriticalSection(CSGVTsFile);

end. // End unit
