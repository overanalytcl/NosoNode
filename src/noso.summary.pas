unit Noso.Summary;

{
Nosounit 1.0
January 8th 2023
Noso project unit to handle summary
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Zipper,
  Noso.Crypto, Noso.Debug, Noso.General;

type
  TSummaryData = packed record
    Hash: String[40];     // Public hash
    CustomAlias: String[40]; // Custom alias
    Balance: Int64;       // Noso balance
    Score: Int64;         // Token balance
    LastOperation: Int64; // Last operation block
  end;

  TOrderGroup = packed record
    Block: Integer;
    TimeStamp: Int64;
    OrderID: String[64];
    OrderType: String[6];
    OrderLineCount: Integer;
    Reference: String[64];
    Sender: String;
    Receiver: String[40];
    AmmountFee: Int64;
    AmountTransferred: Int64;
  end;

  TIndexRecord = array of Integer;

  TBlockRecord = record
    DiskSlot: Int64;
    SummaryData: TSummaryData;
  end;

{Protocol utilitys}
function CreateProtocolOrder(BlockNumber: Integer;
  OrderType, Sender, Receiver, Signature: String; TimeStamp, Amount: Int64): TOrderData;

{Sumary management}
procedure CreateNewSummaryFile(AddBlockZero: Boolean);
function ZipSummary(): Boolean;
function CreateSummaryIndex(): Int64;
function GetSummaryAsMemoryStream(out Stream: TMemoryStream): Int64;
function GetZipSummaryAsMemoryStream(out Stream: TMemoryStream): Int64;
function SaveSummaryToFile(const Stream: TMemoryStream): Boolean;
function CreateSummaryBackup(): Boolean;
function RestoreSummaryBackup(): Boolean;
function SummaryIndexLength(): Int64;
procedure ResetBlockRecords();
function FindSummaryIndexPosition(Text: String; out RecordData: TSummaryData;
  IsAlias: Boolean = False): Int64;
function IsSummaryValidPayment(Address: String; Amount, BlockNumber: Int64): Boolean;
procedure ProcessSummaryPayment(Address: String; Amount, BlockNumber: Int64);
procedure CreditTo(Address: String; Amount, BlockNumber: Int64);
function IsCustomizationValid(Address, Custom: String; BlockNumber: Int64;
  ForceCustom: Boolean = False): Boolean;
procedure UpdateSummaryChanges();
function GetAddressBalanceIndexed(Address: String): Int64;
function GetAddressAlias(Address: String): String;
function GetAddressLastOperation(Address: String): Int64;

// Summary hash related
procedure SetSummaryHash();
function ComputeSummaryHash: String;

var
  {Overall variables}
  WorkingDirectory: String = '';

  {Summary related}
  SummaryFileName: String = 'NOSODATA' + DirectorySeparator + 'sumary.psk';
  ZipSummaryFileName: String = 'NOSODATA' + DirectorySeparator + 'sumary.zip';
  SummaryLastOperation: Int64;
  SummaryHashValue: String = '';

implementation

var
  IndexLength: Int64 = 10;
  SummaryIndex: array of TindexRecord;
  SummaryDiskLock: TRTLCriticalSection;    {Disk access to summary}
  SummaryIndexLock: TRTLCriticalSection;       {Access to index}
  BlockRecords: array of TBlockRecord;
  BlockRecordsLock: TRTLCriticalSection;
  SummaryHashLock: TRTLCriticalSection;

  {$REGION Protocol utilitys}

function CreateProtocolOrder(BlockNumber: Integer;
  OrderType, Sender, Receiver, Signature: String; TimeStamp, Amount: Int64): TOrderData;
begin
  Result := Default(TOrderData);
  Result.Block := BlockNumber;
  Result.OrderLineCount := 1;
  Result.OrderType := OrderType;
  Result.TimeStamp := TimeStamp;
  Result.Reference := 'null';
  Result.TransferLine := 1;
  Result.Sender := Sender;
  Result.Address := Sender;
  Result.Receiver := Receiver;
  Result.AmountFee := 0;
  Result.AmountTransferred := amount;
  Result.Signature := Signature;
  Result.TransferID := GetTransferHash(Result.TimeStamp.ToString +
    Sender + Receiver + IntToStr(amount) + IntToStr(BlockNumber - 1));
  Result.OrderID := GetOrderHash('1' + Result.TransferID);
end;

{$ENDREGION}

{$REGION Sumary management}

{Creates a new summary file}
procedure CreateNewSummaryFile(AddBlockZero: Boolean);
var
  SummaryFile: file;
begin
  try
    AssignFile(SummaryFile, SummaryFileName);
    Rewrite(SummaryFile);
    CloseFile(SummaryFile);

    CreateSummaryIndex;
    if AddBlockZero then
    begin
      CreditTo('N4PeJyqj8diSXnfhxSQdLpo8ddXTaGd', 1030390730000, 0);
      UpdateSummaryChanges;
      ResetBlockRecords;
      SummaryLastOperation := 0;
    end;
  except
    on E: Exception do

  end;
  SetSummaryHash;
end;

{Create the zipped summary file}
{Must be replaced with new stream compression methods}
function ZipSummary(): Boolean;
var
  Zipper: TZipper;
  ArchiveName: String;
begin
  Result := False;
  Zipper := TZipper.Create;
  try

    Zipper.FileName := ZipSummaryFileName;
    EnterCriticalSection(SummaryDiskLock);

    try
      {$IFDEF WINDOWS}
      ArchiveName := StringReplace(SummaryFileName,'\','/',[rfReplaceAll]);
      {$ENDIF}
      {$IFDEF UNIX}
      ArchiveName := SummaryFileName;
      {$ENDIF}

      ArchiveName := StringReplace(ArchiveName, 'NOSODATA', 'data', [rfReplaceAll]);

      Zipper.Entries.AddFileEntry(SummaryFileName, ArchiveName);
      Zipper.ZipAllFiles;
      Result := True;
    except
      ON E: Exception do
    end;
  finally
    Zipper.Free;
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

function GetSummaryAsMemoryStream(out Stream: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(SummaryDiskLock);
  try
    Stream.LoadFromFile(SummaryFileName);
    Result := Stream.Size;
    Stream.Position := 0;
  except
    ON E: Exception do
  end{Try};
  LeaveCriticalSection(SummaryDiskLock);
end;

function GetZipSummaryAsMemoryStream(out Stream: TMemoryStream): Int64;
begin
  Result := 0;
  Stream := TMemoryStream.Create;

  try
    EnterCriticalSection(SummaryDiskLock);
    try
      Stream.LoadFromFile(SummaryFileName);
      Result := Stream.Size;
      Stream.Position := 0;
    except
      on E: Exception do
        LogError('Failed to load summary into memory stream: ' + E.Message);
    end
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

function SaveSummaryToFile(const Stream: TMemoryStream): Boolean;
begin
  Result := 0;
  Stream := TMemoryStream.Create;

  try
    EnterCriticalSection(SummaryDiskLock);
    try
      Stream.LoadFromFile(ZipSummaryFileName);
      Result := Stream.Size;
      Stream.Position := 0;
    except
      on E: Exception do
        LogError('Failed to load zipped summary into memory stream: ' + E.Message);
    end;
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

function CreateSummaryBackup(): Boolean;
begin
  EnterCriticalSection(SummaryDiskLock);
  try
    Result := TryCopyFile(SummaryFileName, SummaryFileName + '.bak');
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

function RestoreSummaryBackup(): Boolean;
begin
  EnterCriticalSection(SummaryDiskLock);
  try
    Result := TryCopyFile(SummaryFileName + '.bak', SummaryFileName);
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

function GetIndexSize(RecordCount: Integer): Integer;
begin
  Result := 10;
  repeat
    Result := Result * 10;
  until Result > RecordCount;

  Result := Result div 10;
end;

function IndexFunction(AddressHash: String; IndexSize: Int64): Int64;
var
  SubStr: String;
begin
  AddressHash := HashMD5String(AddressHash);
  AddressHash := B16ToB58(AddressHash);
  SubStr := Copy(AddressHash, 2, 6);
  Result := StrToInt64(B58ToB10(SubStr)) mod IndexSize;
end;

{Reads a specific summary record position from disk}
function ReadSummaryRecordFromDisk(Index: Integer): TSummaryData;
var
  SummaryFile: file;
begin
  Result := Default(TSummaryData);
  AssignFile(SummaryFile, SummaryFileName);

  EnterCriticalSection(SummaryDiskLock);
  try
    try
      Reset(SummaryFile, 1);
      try
        Seek(SummaryFile, Index * SizeOf(Result));
        BlockRead(SummaryFile, Result, SizeOf(Result));
      except
        on E: Exception do
          LogError('Error reading summary record from disk: ' + E.Message);
      end;
      CloseFile(SummaryFile);
    except
      on E: Exception do
        LogError('Error assigning summary file: ' + E.Message);
    end
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

{Add a pointer to the summary index}
procedure InsertIndexData(RecordData: TSummaryData; DiskPos: Int64);
var
  Index: Int64;
begin
  Index := IndexFunction(RecordData.Hash, IndexLength);
  Insert(DiskPos, SummaryIndex[Index], Length(SummaryIndex[Index]));

  if RecordData.CustomAlias <> '' then
  begin
    Index := IndexFunction(RecordData.CustomAlias, IndexLength);
    Insert(DiskPos, SummaryIndex[Index], Length(SummaryIndex[Index]));
  end;
end;

{Creates the summary index from the disk}
function CreateSummaryIndex(): Int64;
var
  SummaryFile: file;
  CurrentRecord: TSummaryData;
  CurrentPos: Int64 = 0;
begin
  StartPerformanceMeasurement('CreateSummaryIndex');
  AssignFile(SummaryFile, SummaryFileName);

  EnterCriticalSection(SummaryIndexLock);
  try
    SetLength(SummaryIndex, 0, 0);

    EnterCriticalSection(SummaryDiskLock);
    try
      try
        Reset(SummaryFile, 1);
        IndexLength := GetIndexSize(FileSize(SummaryFile) div SizeOf(TSummaryData));
        SetLength(SummaryIndex, IndexLength);

        while not EOF(SummaryFile) do
        begin
          BlockRead(SummaryFile, CurrentRecord, SizeOf(CurrentRecord));
          InsertIndexData(CurrentRecord, CurrentPos);
          Inc(CurrentPos);
        end;

        SummaryLastOperation := ReadSummaryRecordFromDisk(0).LastOperation;
      except
        on E: Exception do
          LogError('Error creating summary index: ' + E.Message);
      end;

    finally
      CloseFile(SummaryFile);
      LeaveCriticalSection(SummaryDiskLock);
    end
  finally
    LeaveCriticalSection(SummaryIndexLock);
  end;
  Result := StopPerformanceMeasurement('CreateSummaryIndex');
  SetSummaryHash;
end;

{Returns the summary index length}
function SummaryIndexLength(): Int64;
begin
  EnterCriticalSection(SummaryIndexLock);
  try
    Result := IndexLength;
  finally
    LeaveCriticalSection(SummaryIndexLock);
  end;
end;

{If found, returns the record}
function FindSummaryIndexPosition(Text: String; out RecordData: TSummaryData;
  IsAlias: Boolean = False): Int64;
var
  IndexPos: Int64;
  i: Integer;
  CurrentRecord: TSummaryData;
begin
  Result := -1;
  RecordData := Default(TSummaryData);
  IndexPos := IndexFunction(Text, IndexLength);

  if Length(SummaryIndex[IndexPos]) > 0 then
  begin
    EnterCriticalSection(SummaryIndexLock);
    try
      for i := 0 to High(SummaryIndex[IndexPos]) do
      begin
        CurrentRecord := ReadSummaryRecordFromDisk(SummaryIndex[IndexPos][i]);
        if (((CurrentRecord.Hash = Text) and not IsAlias) or
          ((CurrentRecord.CustomAlias = Text) and IsAlias)) then
        begin
          RecordData := CurrentRecord;
          Result := SummaryIndex[IndexPos][i];
          Break;
        end;
      end;
    finally
      LeaveCriticalSection(SummaryIndexLock);
    end;
  end;
end;


{Returns the balance of a specific address}
function GetAddressBalanceIndexed(Address: String): Int64;
var
  IndexPos: Integer;
  i: Integer;
  CurrentRecord: TSummaryData;
begin
  Result := 0;
  IndexPos := IndexFunction(Address, Length(SummaryIndex));

  if IndexPos >= Length(SummaryIndex) then Exit;

  if Length(SummaryIndex[IndexPos]) > 0 then
  begin
    EnterCriticalSection(SummaryIndexLock);
    try
      for i := 0 to High(SummaryIndex[IndexPos]) do
      begin
        CurrentRecord := ReadSummaryRecordFromDisk(SummaryIndex[IndexPos][i]);
        if CurrentRecord.Hash = Address then
        begin
          Result := CurrentRecord.Balance;
          Break;
        end;
      end;
    finally
      LeaveCriticalSection(SummaryIndexLock);
    end;
  end;
end;

{Reset the block records}
procedure ResetBlockRecords();
begin
  EnterCriticalSection(BlockRecordsLock);
  try
    SetLength(BlockRecords, 0);
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;
end;

{Insert a block record}
procedure InsBlockRecord(LRecord: TSummaryData; SLot: Int64);
begin
  EnterCriticalSection(BlockRecordsLock);
  try
    SetLength(BlockRecords, Length(BlockRecords) + 1);
    BlockRecords[High(BlockRecords)].DiskSlot := DiskSlot;
    BlockRecords[High(BlockRecords)].SummaryData := RecordData;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;
end;

{Verify if a sender address have enough funds}
function IsSummaryValidPayment(Address: String; Amount, BlockNumber: Int64): Boolean;
var
  i: Integer;
  SendPos: Int64;
  Summary: TSummaryData;
begin
  Result := False;

  EnterCriticalSection(BlockRecordsLock);
  try
    for i := 0 to High(BlockRecords) do
    begin
      if BlockRecords[i].SummaryData.Hash = Address then
      begin
        if BlockRecords[i].SummaryData.Balance < Amount then Exit(False);

        Dec(BlockRecords[i].SummaryData.Balance, Amount);
        BlockRecords[i].SummaryData.LastOperation := BlockNumber;
        Exit(True);
      end;
    end;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;

  SendPos := FindSummaryIndexPosition(Address, Summary);
  if SendPos < 0 then Exit(False);

  if Summary.Balance < Amount then
    Exit(False);

  Dec(Summary.Balance, Amount);
  Summary.LastOperation := BlockNumber;
  InsBlockRecord(Summary, SendPos);

  Exit(True);
end;

procedure ProcessSummaryPayment(Address: String; Amount, BlockNumber: Int64);
var
  i: Integer;
  SendPos: Int64;
  Summary: TSummaryData;
begin
  EnterCriticalSection(BlockRecordsLock);
  try
    for i := 0 to High(BlockRecords) do
    begin
      if BlockRecords[i].SummaryData.Hash = Address then
      begin
        Dec(BlockRecords[i].SummaryData.Balance, Amount);
        BlockRecords[i].SummaryData.LastOperation := BlockNumber;
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;

  SendPos := FindSummaryIndexPosition(Address, Summary);
  if SendPos < 0 then
  begin
    Summary.Hash := Address;
  end
  else
  begin
    Dec(Summary.Balance, Amount);
    Summary.LastOperation := BlockNumber;
    InsBlockRecord(Summary, SendPos);
  end;
end;

{Set an ammount to be credited to an specific address}
procedure CreditTo(Address: String; Amount, BlockNumber: Int64);
var
  i: Integer;
  SummaryPos: Int64;
  Summary: TSummaryData;
begin
  EnterCriticalSection(BlockRecordsLock);
  try
    for i := 0 to High(BlockRecords) do
    begin
      if BlockRecords[i].SummaryData.Hash = Address then
      begin
        Inc(BlockRecords[i].SummaryData.Balance, Amount);
        BlockRecords[i].SummaryData.LastOperation := BlockNumber;
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;

  SummaryPos := FindSummaryIndexPosition(Address, Summary);
  Inc(Summary.Balance, Amount);
  Summary.LastOperation := BlockNumber;

  if SummaryPos < 0 then
    Summary.Hash := Address;

  InsBlockRecord(Summary, SummaryPos);
end;

{Process if an address customization is valid}
function IsCustomizationValid(Address, Custom: String; BlockNumber: Int64;
  ForceCustom: Boolean = False): Boolean;
const
  CustomizationCost = 25000;
var
  counter: Integer;
  SumPos: Int64;
  ThisRecord: TSummaryData;
begin
  Result := False;

  EnterCriticalSection(BlockRecordsLock);
  try
    for counter := 0 to High(BlockRecords) do
    begin
      if BlockRecords[counter].SummaryData.Hash = Address then
      begin
        if (BlockRecords[counter].SummaryData.CustomAlias <> '') and not ForceCustom then
          Exit(False);

        if (BlockRecords[counter].SummaryData.Balance < CustomizationCost) and
          not ForceCustom then
          Exit(False);

        BlockRecords[counter].SummaryData.CustomAlias := Custom;
        Dec(BlockRecords[counter].SummaryData.Balance, CustomizationCost);
        Exit(True);
      end;
    end;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;

  SumPos := FindSummaryIndexPosition(Address, ThisRecord);
  if SumPos < 0 then Exit(False);

  if (ThisRecord.Balance < CustomizationCost) and not ForceCustom then Exit(False);
  if (ThisRecord.CustomAlias <> '') and not ForceCustom then Exit(False);

  ThisRecord.CustomAlias := Custom;
  Dec(ThisRecord.Balance, CustomizationCost);
  ThisRecord.LastOperation := BlockNumber;
  InsBlockRecord(ThisRecord, SumPos);
  Result := True;
end;

{Process the changes of the block to the summary on disk}
procedure UpdateSummaryChanges();
var
  i: Integer;
  SummaryFile: file;
begin
  AssignFile(SummaryFile, SummaryFileName);

  EnterCriticalSection(SummaryDiskLock);
  try
    Reset(SummaryFile, 1);
    try
      for i := 0 to High(BlockRecords) do
      begin
        if BlockRecords[i].DiskSlot < 0 then
        begin
          BlockRecords[i].DiskSlot := FileSize(SummaryFile) div SizeOf(TSummaryData);
          InsertIndexData(BlockRecords[i].SummaryData, BlockRecords[i].DiskSlot);
        end;

        Seek(SummaryFile, BlockRecords[i].DiskSlot * SizeOf(TSummaryData));
        BlockWrite(SummaryFile, BlockRecords[i].SummaryData, SizeOf(TSummaryData));
      end;
    except
      on E: Exception do
        LogError('Error updating summary changes: ' + E.Message);
    end;
    CloseFile(SummaryFile);
  except
    on E: Exception do
      LogError('Error assigning summary file: ' + E.Message);
  end;

  LeaveCriticalSection(SummaryDiskLock);
  SummaryLastOperation := ReadSummaryRecordFromDisk(0).LastOperation;
  SetSummaryHash;
end;

{Returns the address alias name if exists}
function GetAddressAlias(Address: String): String;
var
  SummaryPos: Int64;
  Summary: TSummaryData;
begin
  Result := '';
  SummaryPos := FindSummaryIndexPosition(Address, Summary);

  if ((SummaryPos >= 0) and (Summary.CustomAlias <> '')) then
    Result := Summary.CustomAlias;
end;

{Returns the address last operation block}
function GetAddressLastOperation(Address: String): Int64;
var
  SummaryPos: Int64;
  Summary: TSummaryData;
begin
  Result := 0;

  SummaryPos := FindSummaryIndexPosition(Address, Summary);
  if SummaryPos >= 0 then
    Result := Summary.LastOperation;
end;

{$ENDREGION}

procedure SetSummaryHash();
begin
  EnterCriticalSection(SummaryHashLock);
  try
    SummaryHashValue := HashMD5File(SummaryFileName);
  finally
    LeaveCriticalSection(SummaryHashLock);
  end;
end;

function ComputeSummaryHash: String;
begin
  EnterCriticalSection(SummaryHashLock);
  try
    Result := SummaryHashValue;
  finally
    LeaveCriticalSection(SummaryHashLock);
  end;
end;

initialization
  SetLength(SummaryIndex, 0, 0);
  SetLength(BlockRecords, 0);
  InitCriticalSection(SummaryDiskLock);
  InitCriticalSection(BlockRecordsLock);
  InitCriticalSection(SummaryHashLock);
  InitCriticalSection(SummaryIndexLock);

finalization
  DoneCriticalSection(SummaryDiskLock);
  DoneCriticalSection(BlockRecordsLock);
  DoneCriticalSection(SummaryHashLock);
  DoneCriticalSection(SummaryIndexLock);

end. {End unit}
