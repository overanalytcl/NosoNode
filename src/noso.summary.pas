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
  { Holds the data for each address' summary }
  TSummaryData = packed record
    Hash: String[40];        //< Public hash representing the address
    CustomAlias: String[40]; //< Optional custom alias for the address
    Balance: Int64;          //< Balance associated with the address
    Score: Int64;            //< Token balance related to the address
    LastOperation: Int64;    //< Block number of the last operation for the address
  end;

  { Manages the details of an order within the system. }
  TOrderGroup = packed record
    Block: Integer;           //< Block number associated with the order
    TimeStamp: Int64;         //< Timestamp of the order
    OrderID: String[64];      //< Unique identifier for the order
    OrderType: String[6];     //< Type of order
    OrderLineCount: Integer;  //< Number of lines in the order
    Reference: String[64];    //< Reference for the order
    Sender: String;           //< Address of the sender
    Receiver: String[40];     //< Address of the receiver
    AmmountFee: Int64;        //< Fee associated with the order
    AmountTransferred: Int64; //< Amount transferred in the order
  end;

  TIndexRecord = array of Integer;

  { Holds information about a block, including its disk position and associated summary data. }
  TBlockRecord = record
    DiskSlot: Int64;
    SummaryData: TSummaryData;
  end;

{
  Initializes a new order with the specified parameters.

  @param BlockNumber The block number where the order is recorded.
  @param OrderType Type of order.
  @param Sender The address of the sender.
  @param Receiver The address of the receiver.
  @param Signature Digital signature of the order.
  @param TimeStamp Timestamp of when the order is created.
  @param Amount Amount related to the order.
  @returns A TOrderData structure containing the initialized order information.
}
function CreateProtocolOrder(BlockNumber: Integer;
  OrderType, Sender, Receiver, Signature: String; TimeStamp, Amount: Int64): TOrderData;

{
  Initializes a new summary file on disk.
  @param AddBlockZero If @true, a zero block is added to the summary file and updates the summary.
}
procedure CreateNewSummaryFile(AddBlockZero: Boolean);
{
  Compresses the current summary file into a ZIP archive.

  @returns @true if the zipping operation succeeds, otherwise @false.
}
function ZipSummary(): Boolean;
{
  Creates the summary index from the disk summary file.
  @returns The time taken to create the summary index in milliseconds.
}
function CreateSummaryIndex(): Int64;
{
  Loads the summary file into a memory stream.

  @param Stream The output memory stream that will contain the summary data.
  @returns The size of the summary data loaded into the stream.
}
function GetSummaryAsMemoryStream(out Stream: TMemoryStream): Int64;

{
  Loads the zipped summary file into a memory stream.
  @param Stream The output memory stream that will contain the zipped summary data.
  @returns The size of the zipped summary data loaded into the stream.
}
function GetZipSummaryAsMemoryStream(out Stream: TMemoryStream): Int64;
{
  Saves the content of a memory stream to a specified file.
  @param Stream The memory stream containing the data to save.
  @returns The size of the stream.
}
function SaveSummaryToFile(Stream: TMemoryStream): Int64;
{
  Creates a backup of the current summary file.
  @returns @true if the backup operation is successful, otherwise @false.
}
function CreateSummaryBackup(): Boolean;
{
  Restores the summary file from a backup.
  @returns @true if the restore operation is successful, otherwise @false.
}
function RestoreSummaryBackup(): Boolean;
{
  Returns the length of the summary index.
  @returns The length of the summary index.
}
function SummaryIndexLength(): Int64;
{ Resets the block records by clearing the block records array. }
procedure ResetBlockRecords();
{
  Searches for a summary record in the index based on the provided text.
  @param Text The text (hash or alias) to search for in the index.
  @param RecordData Output parameter that will hold the found record data.
  @param IsAlias If true, searches for a custom alias instead of the hash.
  @returns The index position of the found record, or -1 if not found.
}
function FindSummaryIndexPosition(Text: String; out RecordData: TSummaryData;
  IsAlias: Boolean = False): Int64;
{
  Verifies if a sender address has enough funds for a specified payment amount.
  @param Address The address of the sender.
  @param Amount The amount to be paid.
  @param BlockNumber The block number in which the payment is being processed.
  @returns @true if the payment is valid, otherwise @false.
}
function IsSummaryValidPayment(Address: String; Amount, BlockNumber: Int64): Boolean;
{
  Processes a payment for a specified address, updating its balance.

  @param Address The address to process the payment for.
  @param Amount The amount to be deducted from the address's balance.
  @param BlockNumber The block number in which the payment is processed.
}
procedure ProcessSummaryPayment(Address: String; Amount, BlockNumber: Int64);
{
  Credits a specified amount to a specific address, updating its balance.
  @param Address The address to which the amount will be credited.
  @param Amount The amount to be credited.
  @param BlockNumber The block number in which the credit is applied.
}
procedure CreditTo(Address: String; Amount, BlockNumber: Int64);
{
  Checks if an address can customize its alias based on current balance and conditions.
  @param Address The address to verify customization for.
  @param Custom The new custom alias to be set.
  @param BlockNumber The block number in which the customization is processed.
  @param ForceCustom If true, bypasses balance checks for customization.
  @returns @true if the customization is valid, otherwise @false.
}
function IsCustomizationValid(Address, Custom: String; BlockNumber: Int64;
  ForceCustom: Boolean = False): Boolean;

{ Processes and writes changes made to the summary in memory to the disk. }
procedure UpdateSummaryChanges();

{
  Retrieves the balance of a specific address from the summary index.
  @param Address The address whose balance is to be retrieved.
  @returns The balance of the specified address, or 0 if not found.
}
function GetAddressBalanceIndexed(Address: String): Int64;
{
  Retrieves the custom alias for a specified address, if it exists.

  @param Address The address to look up the alias for.
  @returns The custom alias associated with the address, or an empty string if none exists.
}
function GetAddressAlias(Address: String): String;
{
  Retrieves the last operation block number associated with a specified address.

  @param Address The address to look up.
  @returns The block number of the last operation, or 0 if not found.
}
function GetAddressLastOperation(Address: String): Int64;

{ Calculates and sets the hash value for the summary file. }
procedure SetSummaryHash();

{
  Retrieves the current hash value of the summary file.
  @returns The hash value as a string.
}
function ComputeSummaryHash: String;

var
  { The current working directory for the application. }
  WorkingDirectory: String = '';

  { The file name for the summary data, located in the 'NOSODATA' directory. }
  SummaryFileName: String = 'NOSODATA' + DirectorySeparator + 'sumary.psk';
  { The file name for the zipped summary data, located in the 'NOSODATA' directory. }
  ZipSummaryFileName: String = 'NOSODATA' + DirectorySeparator + 'sumary.zip';
  { Holds the block number of the last operation performed on the summary data. }
  SummaryLastOperation: Int64;
  { The MD5 hash value of the summary file for integrity checks. }
  SummaryHashValue: String = '';

implementation

var
  { Represents the length of the summary index. }
  IndexLength: Int64 = 10;
  { An array of TindexRecord that stores the index of the summary records. }
  SummaryIndex: array of TindexRecord;
  { Critical section for managing access to the summary data on disk. }
  SummaryDiskLock: TRTLCriticalSection;
  { Critical section for managing access to the summary index. }
  SummaryIndexLock: TRTLCriticalSection;
  { An array of TBlockRecord that stores block records associated with the summary. }
  BlockRecords: array of TBlockRecord;
  { Critical section for managing access to the block records. }
  BlockRecordsLock: TRTLCriticalSection;
  { Critical section for managing access to the summary hash value. }
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
        WriteLn('Failed to load summary into memory stream: ' + E.Message);
    end
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

function SaveSummaryToFile(Stream: TMemoryStream): Int64;
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
        Writeln('Failed to load zipped summary into memory stream: ' + E.Message);
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

{
  Calculates the size of the index based on the number of records.
  @param RecordCount The total number of records to determine the index size.
  @returns The calculated index size.
}

function GetIndexSize(RecordCount: Integer): Integer;
begin
  Result := 10;
  repeat
    Result := Result * 10;
  until Result > RecordCount;

  Result := Result div 10;
end;

{
  Computes an index based on the hash of the given address.
  @param AddressHash The hash of the address to index.
  @param IndexSize The maximum size for the index.
  @returns The computed index.
}

function IndexFunction(AddressHash: String; IndexSize: Int64): Int64;
var
  SubStr: String;
begin
  AddressHash := HashMD5String(AddressHash);
  AddressHash := B16ToB58(AddressHash);
  SubStr := Copy(AddressHash, 2, 6);
  Result := StrToInt64(B58ToB10(SubStr)) mod IndexSize;
end;

{
  Reads a specific summary record from disk based on the provided index.
  @param Index The index position of the summary record to read.
  @returns The summary record data read from the disk.
}
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
          Writeln('Error reading summary record from disk: ' + E.Message);
      end;
      CloseFile(SummaryFile);
    except
      on E: Exception do
        Writeln('Error assigning summary file: ' + E.Message);
    end
  finally
    LeaveCriticalSection(SummaryDiskLock);
  end;
end;

{
  Adds a pointer to the summary index for the given summary record data.
  @param RecordData The summary record data to be indexed.
  @param DiskPos The disk position of the record to be inserted.
}
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
          Writeln('Error creating summary index: ' + E.Message);
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

function SummaryIndexLength(): Int64;
begin
  EnterCriticalSection(SummaryIndexLock);
  try
    Result := IndexLength;
  finally
    LeaveCriticalSection(SummaryIndexLock);
  end;
end;

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

procedure ResetBlockRecords();
begin
  EnterCriticalSection(BlockRecordsLock);
  try
    SetLength(BlockRecords, 0);
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;
end;

{
  Inserts a new block record with the specified summary data at the given slot.
  @param Summary The summary data to be inserted into the block record.
  @param Slot The disk slot where the block record will be stored.
}
procedure InsertBlockRecord(Summary: TSummaryData; Slot: Int64);
begin
  EnterCriticalSection(BlockRecordsLock);
  try
    SetLength(BlockRecords, Length(BlockRecords) + 1);
    BlockRecords[High(BlockRecords)].DiskSlot := Slot;
    BlockRecords[High(BlockRecords)].SummaryData := Summary;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;
end;

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
  InsertBlockRecord(Summary, SendPos);

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
    InsertBlockRecord(Summary, SendPos);
  end;
end;

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

  InsertBlockRecord(Summary, SummaryPos);
end;


function IsCustomizationValid(Address, Custom: String; BlockNumber: Int64;
  ForceCustom: Boolean = False): Boolean;
const
  CustomizationCost = 25000;
var
  i: Integer;
  SummaryPos: Int64;
  Summary: TSummaryData;
begin
  Result := False;

  EnterCriticalSection(BlockRecordsLock);
  try
    for i := 0 to High(BlockRecords) do
    begin
      if BlockRecords[i].SummaryData.Hash = Address then
      begin
        if (BlockRecords[i].SummaryData.CustomAlias <> '') and not ForceCustom then
          Exit(False);

        if (BlockRecords[i].SummaryData.Balance < CustomizationCost) and
          not ForceCustom then
          Exit(False);

        BlockRecords[i].SummaryData.CustomAlias := Custom;
        Dec(BlockRecords[i].SummaryData.Balance, CustomizationCost);
        Exit(True);
      end;
    end;
  finally
    LeaveCriticalSection(BlockRecordsLock);
  end;

  SummaryPos := FindSummaryIndexPosition(Address, Summary);
  if SummaryPos < 0 then
    Exit(False);

  if (Summary.Balance < CustomizationCost) and not ForceCustom then
    Exit(False);

  if (Summary.CustomAlias <> '') and not ForceCustom then
    Exit(False);

  Summary.CustomAlias := Custom;
  Dec(Summary.Balance, CustomizationCost);
  Summary.LastOperation := BlockNumber;
  InsertBlockRecord(Summary, SummaryPos);
  Result := True;
end;

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
        Writeln('Error updating summary changes: ' + E.Message);
    end;
    CloseFile(SummaryFile);
  except
    on E: Exception do
      Writeln('Error assigning summary file: ' + E.Message);
  end;

  LeaveCriticalSection(SummaryDiskLock);
  SummaryLastOperation := ReadSummaryRecordFromDisk(0).LastOperation;
  SetSummaryHash;
end;

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
