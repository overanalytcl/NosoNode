/// Unit for managing a GVT (Governance Token)
unit Noso.Gvt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Noso.Debug, Noso.Crypto;

type
  /// <summary>
  /// Record representing a Governance Token (GVT)
  /// </summary>
  TGovernanceToken = packed record
    /// <summary>
    /// Number associated with the governance token (TBD)
    /// </summary>
    Number: String[2];

    /// <summary>
    /// Address of the current owner.
    /// </summary>
    Owner: String[32];

    /// <summary>
    /// Hash associated with the governance token.
    /// </summary>
    Hash: String[64];

    /// <summary>
    /// Control code for the governance token (?, TBD)
    /// </summary>
    Control: Integer;
  end;

/// <summary>
/// Creates an empty GVT file.
/// If a file already exists, it is overwritten with an empty one.
/// </summary>
/// <returns>@true if the file was successfully created; @false otherwise.</returns>
function CreateGVTFile(): Boolean;

/// <summary>
/// Loads the GVT file into a memory stream.
/// </summary>
/// <param name="Stream">The memory stream to load the GVT file into.</param>
/// <returns>Size of the loaded stream in bytes.</returns>
function GetGVTAsStream(out Stream: TMemoryStream): Int64;

/// <summary>
/// Saves the provided memory stream as the GVT file.
/// </summary>
/// <param name="Stream">The memory stream to save.</param>
/// <returns>@true if the file was successfully saved; @false otherwise.</returns>
function SaveStreamAsGVTs(Stream: TMemoryStream): Boolean;

/// <summary>
/// Loads the GVT file data into an array.
/// The array holds all GVT records from the file.
/// </summary>
procedure LoadGVTsFileData();

/// <summary>
/// Saves the GVT array data back to the file.
/// </summary>
procedure SaveGVTsAsData();

/// <summary>
/// Changes the owner of a Governance Token given its number.
/// <summary>
/// <param name="Number">The number of the GVT to change.</param>
/// <param name="OldOwner">The current owner's address. </param>
/// <param name="NewOwner">The new owner's address.</param>

/// <returns>0 for success, or an error code (1, 2, 3).</returns>
function ChangeGVTOwner(Number: Integer; OldOwner, NewOwner: String): Integer;

/// <summary>
/// Retrieves a Governance Token record by its index in the array.
/// </summary>
/// <param name="Index">The index of the GVT to retrieve.</param>
/// <returns>The requested GVT record.</returns>    .
function GetGVTByIndex(Index: Integer): TGovernanceToken;

/// <summary>
/// Returns the number of Governance Token records currently loaded in the array.
/// </summary>
/// <returns>The number of GVT records.</returns>
function GetGVTCount: Integer;

/// <summary>
/// Counts the number of available Governance Tokens (GVTs belonging to the development fund).
/// </summary>
/// <returns>The count of available GVTs.</returns>
function CountAvailableGVTs(): Integer;

/// <summary>
/// Calculates the price of a Governance Token based on the number of available GVTs.
/// </summary>
/// <param name="Available">The number of GVTs available.</param>
/// <param name="ForSale">Whether the price is for selling (@true) or buying (@false).</param>
/// <returns>The calculated price of the GVT.</returns>
function CalculateGVTPrice(Available: Integer; ForSale: Boolean = False): Int64;

const
  /// Base value of a Governance Token
  GVTBaseValue = 70000000000;

var
  // Filename for GVT storage
  GVTFilename: String = 'NOSODATA' + DirectorySeparator + 'gvts.psk';
  // File variable for GVT file handling
  GVTFile: file of TGovernanceToken;
  // Array to hold loaded GVT records
  GVTArray: array of TGovernanceToken;
  // Critical section for synchronizing array access
  GVTArrayLock: TRTLCriticalSection;
  // Critical section for synchronizing file access
  GVTFileLock: TRTLCriticalSection;
  // MD5 hash of the Governance Token file
  GVTHashMD5: String = '';

implementation

function CreateGVTFile(): Boolean;
var
  GVTStream: TMemoryStream;
begin
  Result := True;
  GVTStream := TMemoryStream.Create;
  EnterCriticalSection(GVTFileLock);
  try
    try
      GVTStream.SaveToFile(GVTFilename);
    except
      on E: Exception do
      begin
        Result := False;
        ToDeepDebug('NosoGVTs,CreateGVTFile,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(GVTFileLock);
    GVTStream.Free;
  end;
end;

function GetGVTAsStream(out Stream: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(GVTFileLock);
  try
    try
      Stream.LoadFromFile(GVTFilename);
      Result := Stream.Size;
      Stream.Position := 0;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoGVTs,GetGVTsAsStream,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(GVTFileLock);
  end;
end;

function SaveStreamAsGVTs(Stream: TMemoryStream): Boolean;
begin
  Result := False;
  EnterCriticalSection(GVTFileLock);
  try
    try
      Stream.SaveToFile(GVTFilename);
      Result := True;
    except
      on E: Exception do
      begin
        ToDeepDebug('NosoGVTs,SaveStreamAsGVTs,' + E.Message);
      end;
    end;
  finally
    LeaveCriticalSection(GVTFileLock);
    GVTHashMD5 := HashMD5File(GVTFilename);
  end;
end;

procedure LoadGVTsFileData();
var
  i: Integer;
begin
  EnterCriticalSection(GVTFileLock);
  EnterCriticalSection(GVTArrayLock);
  AssignFile(GVTFile, GVTFilename);
  try
    try
      Reset(GVTFile);
      SetLength(GVTArray, FileSize(GVTFile));
      for i := 0 to FileSize(GVTFile) - 1 do
      begin
        Seek(GVTFile, i);
        Read(GVTFile, GVTArray[i]);
      end;
      CloseFile(GVTFile);
    except
      on E: Exception do
        ToDeepDebug('NosoGVTs,LoadGVTsFileData,' + E.Message);
    end;
  finally
    LeaveCriticalSection(GVTArrayLock);
    LeaveCriticalSection(GVTFileLock);
    GVTHashMD5 := HashMD5File(GVTFilename);
  end;
end;

procedure SaveGVTsAsData();
var
  i: Integer;
begin
  EnterCriticalSection(GVTFileLock);
  EnterCriticalSection(GVTArrayLock);
  AssignFile(GVTFile, GVTFilename);
  try
    try
      Rewrite(GVTFile);
      for i := 0 to Length(GVTArray) - 1 do
      begin
        Seek(GVTFile, i);
        Write(GVTFile, GVTArray[i]);
      end;
      CloseFile(GVTFile);
    except
      on E: Exception do
        ToDeepDebug('NosoGVTs,SaveGVTsAsData,' + E.Message);
    end;
  finally
    LeaveCriticalSection(GVTArrayLock);
    LeaveCriticalSection(GVTFileLock);
    GVTHashMD5 := HashMD5File(GVTFilename);
  end;
end;

function GetGVTByIndex(Index: Integer): TGovernanceToken;
begin
  Result := Default(TGovernanceToken);
  if (Index < 0) or (Index >= GetGVTCount) then
    Exit;

  EnterCriticalSection(GVTArrayLock);
  try
    Result := GVTArray[Index];
  finally
    LeaveCriticalSection(GVTArrayLock);
  end;
end;

function GetGVTCount: Integer;
begin
  EnterCriticalSection(GVTArrayLock);
  try
    Result := Length(GVTArray);
  finally
    LeaveCriticalSection(GVTArrayLock);
  end;
end;

function ChangeGVTOwner(Number: Integer; OldOwner, NewOwner: String): Integer;
var
  GVTData: TGovernanceToken;
begin
  Result := 0;
  if Number >= GetGVTCount then Exit(1); // Invalid GVT number

  GVTData := GetGVTByIndex(Number);
  if GVTData.Owner <> OldOwner then Exit(2); // Ownership mismatch
  if not IsValidHashAddress(NewOwner) then Exit(3); // Invalid new owner

  // Update ownership
  EnterCriticalSection(GVTArrayLock);
  try
    GVTArray[Number].Owner := NewOwner;
  finally
    LeaveCriticalSection(GVTArrayLock);
  end;
end;

function CountAvailableGVTs(): Integer;
const
  DevelopmentAddress = 'NpryectdevepmentfundsGE';
var
  i: Integer;
begin
  Result := 0;
  EnterCriticalSection(GVTArrayLock);
  try
    for i := 0 to Length(GVTArray) - 1 do
    begin
      if GVTArray[i].Owner = DevelopmentAddress then
        Inc(Result);
    end;
  finally
    LeaveCriticalSection(GVTArrayLock);
  end;
end;

function CalculateGVTPrice(Available: Integer; ForSale: Boolean = False): Int64;
var
  i: Integer;
begin
  Result := GVTBaseValue;
  Available := 40 - Available;

  // Increment price by 10% for each sold token
  for i := 1 to Available do
  begin
    Result := (Result * 110) div 100;
  end;

  // Ensure the price does not fall below the base value
  if Result < GVTBaseValue then
    Result := GVTBaseValue;

  // Apply a 15% reduction if the token is being sold
  if ForSale then
    Result := (Result * 85) div 100;
end;

initialization
  InitCriticalSection(GVTArrayLock);
  InitCriticalSection(GVTFileLock);
  SetLength(GVTArray, 0);
  AssignFile(GVTFile, GVTFilename);

finalization
  DoneCriticalSection(GVTArrayLock);
  DoneCriticalSection(GVTFileLock);

end.
