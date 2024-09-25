unit Noso.WallCon;

{
nosowallcon 1.1
January 26th, 2024
Stand alone unit to control wallet addresses file and array
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fileutil, Noso.Debug, Noso.Crypto, Noso.General,
  Noso.Headers, Noso.Network;

type

  WalletData = packed record
    Hash: String[40];        // El hash publico o direccion
    Custom: String[40];      // En caso de que la direccion este personalizada
    PublicKey: String[255];  // clave publica
    PrivateKey: String[255]; // clave privada
    Balance: Int64;          // el ultimo saldo conocido de la direccion
    Pending: Int64;          // el ultimo saldo de pagos pendientes
    Score: Int64;            // estado del registro de la direccion.
    LastOP: Int64;           // tiempo de la ultima operacion en UnixTime.
  end;

function SetWalletFileName(Fname: String): Boolean;
procedure ClearWalletArray();
function InsertToWallArr(LData: WalletData): Boolean;
function GetWallArrIndex(Index: Integer): WalletData;
function WallAddIndex(Address: String): Integer;
function LenWallArr(): Integer;
function ChangeWallArrPos(PosA, PosB: Integer): Boolean;
procedure ClearWallPendings();
procedure SetPendingForAddress(Index: Integer; Value: Int64);
function GetAddressFromFile(FileLocation: String; out WalletInfo: WalletData): Boolean;
function ImportAddressesFromBackup(BakFolder: String): Integer;
function SaveAddresstoFile(FileName: String; LData: WalletData): Boolean;

function CreateNewWallet(): Boolean;
function GetWalletAsStream(out LStream: TMemoryStream): Int64;
function SaveWalletToFile(): Boolean;
function LoadWallet(wallet: String): Boolean;
function VerifyAddressOnDisk(HashAddress: String): Boolean;



var
  WalletArray: array of walletData; // Wallet addresses
  FileWallet: file of WalletData;
  WalletFilename: String = 'NOSODATA' + DirectorySeparator + 'wallet.pkw';
  WalletFileLock: TRTLCriticalSection;
  WalletArrayLocks: TRTLCriticalSection;

implementation

// Set the wallet filename; if not exists, returns false
function SetWalletFileName(Fname: String): Boolean;
begin
  Result := True;
  WalletFilename := Fname;//'NOSODATA'+DirectorySeparator+'wallet.pkw';
  if not FileExists(WalletFilename) then
  begin
    CreateNewWallet;
    Result := False;
  end
  else
    LoadWallet(WalletFilename);
end;

procedure ClearWalletArray();
begin
  EnterCriticalSection(WalletArrayLocks);
  setlength(WalletArray, 0);
  LeaveCriticalSection(WalletArrayLocks);
end;

function InsertToWallArr(LData: WalletData): Boolean;
begin
  Result := False;
  if WallAddIndex(LData.Hash) < 0 then
  begin
    EnterCriticalSection(WalletArrayLocks);
    Insert(LData, WalletArray, length(WalletArray));
    LeaveCriticalSection(WalletArrayLocks);
    Result := True;
  end;
end;

function GetWallArrIndex(Index: Integer): WalletData;
begin
  EnterCriticalSection(WalletArrayLocks);
  if Index <= Length(WalletArray) - 1 then
    Result := WalletArray[Index]
  else
    Result := Default(WalletData);
  LeaveCriticalSection(WalletArrayLocks);
end;

function WallAddIndex(Address: String): Integer;
var
  counter: Integer;
begin
  Result := -1;
  if ((Address = '') or (length(Address) < 5)) then exit;
  EnterCriticalSection(WalletArrayLocks);
  for counter := 0 to high(WalletArray) do
    if ((WalletArray[counter].Hash = Address) or
      (WalletArray[counter].Custom = Address)) then
    begin
      Result := counter;
      break;
    end;
  LeaveCriticalSection(WalletArrayLocks);
end;

function LenWallArr(): Integer;
begin
  EnterCriticalSection(WalletArrayLocks);
  Result := Length(WalletArray);
  LeaveCriticalSection(WalletArrayLocks);
end;

function ChangeWallArrPos(PosA, PosB: Integer): Boolean;
var
  oldData, NewData: WalletData;
begin
  Result := False;
  if posA > LenWallArr - 1 then exit;
  if posB > LenWallArr - 1 then exit;
  if posA = posB then Exit;
  OldData := GetWallArrIndex(posA);
  NewData := GetWallArrIndex(posB);
  EnterCriticalSection(WalletArrayLocks);
  WalletArray[posA] := NewData;
  WalletArray[posB] := OldData;
  LeaveCriticalSection(WalletArrayLocks);
  Result := True;
end;

procedure ClearWallPendings();
var
  counter: Integer;
begin
  EnterCriticalSection(WalletArrayLocks);
  for counter := 0 to length(WalletArray) - 1 do
    WalletArray[counter].pending := 0;
  LeaveCriticalSection(WalletArrayLocks);
end;

procedure SetPendingForAddress(Index: Integer; Value: Int64);
begin
  if Index > LenWallArr - 1 then exit;
  EnterCriticalSection(WalletArrayLocks);
  WalletArray[Index].pending := Value;
  LeaveCriticalSection(WalletArrayLocks);
end;

// Import an address data from a file
function GetAddressFromFile(FileLocation: String; out WalletInfo: WalletData): Boolean;
var
  TempFile: file of WalletData;
  Opened: Boolean = False;
  Closed: Boolean = False;
begin
  Result := True;
  AssignFile(TempFile, FileLocation);
  try
    Reset(TempFile);
    Opened := True;
    Read(TempFile, WalletInfo);
    CloseFile(TempFile);
    Closed := True;
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDebug('NosoWallcon,GetAddressFromFile,' + E.Message);
    end;
  end;
  if ((opened) and (not Closed)) then CloseFile(TempFile);
end;

// Verify if all baked up keys are present on active wallet
function ImportAddressesFromBackup(BakFolder: String): Integer;
var
  BakFiles: TStringList;
  Counter: Integer = 0;
  ThisData: WalletData;
begin
  Result := 0;
  StartPerformanceMeasurement('ImportAddressesFromBackup');
  BakFiles := TStringList.Create;
  try
    FindAllFiles(BakFiles, BakFolder, '*.pkw', True);
    while Counter < BakFiles.Count do
    begin
      if GetAddressFromFile(BakFiles[Counter], ThisData) then
      begin
        if InsertToWallArr(ThisData) then Inc(Result);
      end;
      Inc(Counter);
    end;
    if Result > 0 then ToDeepDebug(
        format('Imported %d addresses from backup files', [Result]));
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoWallcon,ImportAddressesFromBackup,' + E.Message);
    end;
  end;
  BakFiles.Free;
  StopPerformanceMeasurement('ImportAddressesFromBackup');
end;

// Saves an address info to a specific file
function SaveAddresstoFile(FileName: String; LData: WalletData): Boolean;
var
  TempFile: file of WalletData;
  opened: Boolean = False;
  Closed: Boolean = False;
begin
  Result := True;
  AssignFile(TempFile, FileName);
  try
    rewrite(TempFile);
    opened := True;
    Write(TempFile, Ldata);
    CloseFile(TempFile);
    Closed := True;
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDebug('NosoWallcon,SaveAddresstoFile,' + E.Message);
    end;
  end;
  if ((opened) and (not Closed)) then CloseFile(TempFile);
end;

// Creates a new wallet file with a new generated address
function CreateNewWallet(): Boolean;
var
  NewAddress: WalletData;
  PubKey, PriKey: String;
begin
  try
    if not fileexists(WalletFilename) then // Check to avoid delete an existing file
    begin
      ClearWalletArray;
      NewAddress := Default(WalletData);
      NewAddress.Hash := GenerateNewAddress(PubKey, PriKey);
      NewAddress.PublicKey := pubkey;
      NewAddress.PrivateKey := PriKey;
      InsertToWallArr(NewAddress);
      SaveWalletToFile;
    end;
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoWallcon,CreateNewWallet,' + E.Message);
    end;
  end; {TRY}
end;

// Load the wallet file into a memory stream
function GetWalletAsStream(out LStream: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(WalletFileLock);
  try
    LStream.LoadFromFile(WalletFilename);
    Result := LStream.Size;
    LStream.Position := 0;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoWallcon,GetWalletAsStream,' + E.Message);
    end;
  end{Try};
  LeaveCriticalSection(WalletFileLock);
end;

// Save the wallet array to the file
function SaveWalletToFile(): Boolean;
var
  MyStream: TMemoryStream;
  Counter: Integer;
begin
  Result := True;
  TryCopyFile(WalletFilename, WalletFilename + '.bak');
  MyStream := TMemoryStream.Create;
  MyStream.Position := 0;
  EnterCriticalSection(WalletFileLock);
  EnterCriticalSection(WalletArrayLocks);
  for Counter := 0 to length(WalletArray) - 1 do
  begin
    MyStream.Write(WalletArray[counter], SizeOf(WalletData));
  end;
  try
    MyStream.SaveToFile(WalletFilename);
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoWallcon,SaveWalletToFile,' + E.Message);
      Result := False;
    end;
  end;
  LeaveCriticalSection(WalletArrayLocks);
  LeaveCriticalSection(WalletFileLock);
  MyStream.Free;
  if Result = True then TryCopyFile(WalletFilename, WalletFilename + '.bak')
  else
    TryCopyFile(WalletFilename + '.bak', WalletFilename);
end;

function LoadWallet(wallet: String): Boolean;
var
  MyStream: TMemoryStream;
  ThisAddress: WalletData;
  Counter: Integer;
  Records: Integer;
begin
  Result := True;
  MyStream := TMemoryStream.Create;
  if fileExists(wallet) then
  begin
    Records := GetWalletAsStream(MyStream) div sizeof(WalletData);
    if Records > 0 then
    begin
      ClearWalletArray;
      for counter := 0 to records - 1 do
      begin
        MyStream.Read(ThisAddress, Sizeof(WalletData));
        InsertToWallArr(ThisAddress);
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
  MyStream.Free;
end;

function VerifyAddressOnDisk(HashAddress: String): Boolean;
var
  MyStream: TMemoryStream;
  ThisAddress: WalletData;
  Counter: Integer;
  Records: Integer;
begin
  Result := False;
  MyStream := TMemoryStream.Create;
  if fileExists(WalletFilename) then
  begin
    Records := GetWalletAsStream(MyStream) div sizeof(WalletData);
    if Records > 0 then
    begin
      for counter := 0 to records - 1 do
      begin
        MyStream.Read(ThisAddress, Sizeof(WalletData));
        if ThisAddress.Hash = HashAddress then
        begin
          Result := True;
          break;
        end;
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
  MyStream.Free;
end;

{$REGION Summary related}



{$ENDREGION Summary related}

initialization
  InitCriticalSection(WalletArrayLocks);
  InitCriticalSection(WalletFileLock);
  SetLength(WalletArray, 0);

finalization
  DoneCriticalSection(WalletArrayLocks);
  DoneCriticalSection(WalletFileLock);
end.
