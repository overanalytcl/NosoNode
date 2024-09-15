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
    Hash: String[40];     {Public hash}
    Custom: String[40];   {Custom alias}
    Balance: Int64;       {Noso balance}
    Score: Int64;         {token balance}
    LastOP: Int64;        {Last operation block}
  end;

  TOrderGroup = packed record
    Block: Integer;
    TimeStamp: Int64;
    OrderID: String[64];
    OrderType: String[6];
    OrderLines: Integer;
    Reference: String[64];
    Sender: String;
    Receiver: String[40];
    AmmountFee: Int64;
    AmmountTrf: Int64;
  end;

  TIndexRecord = array of Integer;

  TBlockRecords = record
    DiskSlot: Int64;
    VRecord: TSummaryData;
  end;

{Protocol utilitys}
function CreateProtocolOrder(BlockN: Integer;
  OrType, Sender, receiver, signature: String; TimeStamp, Amount: Int64): TOrderData;

{Sumary management}
procedure CreateNewSummaryFile(AddBlockZero: Boolean);
function ZipSumary(): Boolean;
function CreateSumaryIndex(): Int64;
function GetSummaryAsMemStream(out LMs: TMemoryStream): Int64;
function GetZIPSummaryAsMemStream(out LMs: TMemoryStream): Int64;
function SaveSummaryToFile(const LStream: TMemoryStream): Boolean;
function CreateSumaryBackup(): Boolean;
function RestoreSumaryBackup(): Boolean;
function SumIndexLength(): Int64;
procedure ResetBlockRecords();
function GetIndexPosition(LText: String; out RecordData: TSummaryData;
  IsAlias: Boolean = False): Int64;
function SummaryValidPay(Address: String; amount, blocknumber: Int64): Boolean;
procedure SummaryPay(Address: String; amount, blocknumber: Int64);
procedure CreditTo(Address: String; amount, blocknumber: Int64);
function IsCustomizacionValid(address, custom: String; blocknumber: Int64;
  forceCustom: Boolean = False): Boolean;
procedure UpdateSummaryChanges();
function GetAddressBalanceIndexed(Address: String): Int64;
function GetAddressAlias(Address: String): String;
function GetAddressLastOP(Address: String): Int64;

// Summary hash related
procedure SetSummaryHash();
function MySumarioHash: String;

var
  {Overall variables}
  WorkingPath: String = '';

  {Summary related}
  SummaryFileName: String = 'NOSODATA' + DirectorySeparator + 'sumary.psk';
  ZipSumaryFileName: String = 'NOSODATA' + DirectorySeparator + 'sumary.zip';
  SummaryLastop: Int64;
  SummaryHashValue: String = '';

implementation

var
  IndexLength: Int64 = 10;
  SumaryIndex: array of TindexRecord;
  CS_SummaryDisk: TRTLCriticalSection;    {Disk access to summary}
  CS_SumIndex: TRTLCriticalSection;       {Access to index}
  BlockRecords: array of TBlockRecords;
  CS_BlockRecs: TRTLCriticalSection;
  CS_SummaryHashV: TRTLCriticalSection;

  {$REGION Protocol utilitys}

function CreateProtocolOrder(BlockN: Integer;
  OrType, Sender, receiver, signature: String; TimeStamp, Amount: Int64): TOrderData;
begin
  Result := Default(TOrderData);
  Result.Block := BlockN;
  Result.OrderLines := 1;
  Result.OrderType := OrType;
  Result.TimeStamp := TimeStamp;
  Result.Reference := 'null';
  Result.TrxLine := 1;
  Result.Sender := Sender;
  Result.Address := Sender;
  Result.Receiver := receiver;
  Result.AmountFee := 0;
  Result.AmountTransferred := amount;
  Result.Signature := Signature;
  Result.TransferID := GetTransferHash(Result.TimeStamp.ToString +
    Sender + Receiver + IntToStr(amount) + IntToStr(BlockN - 1));
  Result.OrderID := GetOrderHash('1' + Result.TransferID);
end;

{$ENDREGION}

{$REGION Sumary management}

{Creates a new summary file}
procedure CreateNewSummaryFile(AddBlockZero: Boolean);
var
  lFile: file;
begin
  try
    assignfile(lFile, SummaryFileName);
    Rewrite(lFile);
    CloseFile(lFile);
    CreateSumaryIndex;
    if AddBlockZero then
    begin
      CreditTo('N4PeJyqj8diSXnfhxSQdLpo8ddXTaGd', 1030390730000, 0);
      UpdateSummaryChanges;
      ResetBlockRecords;
      SummaryLastop := 0;
    end;
  except
    on E: Exception do

  end; {TRY}
  SetSummaryHash;
end;

 {Create the zipped summary file}
 {Must be replaced with new stream compression methods}
function ZipSumary(): Boolean;
var
  MyZipFile: TZipper;
  archivename: String;
begin
  Result := False;
  MyZipFile := TZipper.Create;
  MyZipFile.FileName := ZipSumaryFileName;
  EnterCriticalSection(CS_SummaryDisk);
  try
    {$IFDEF WINDOWS}
   archivename:= StringReplace(SummaryFileName,'\','/',[rfReplaceAll]);
    {$ENDIF}
    {$IFDEF UNIX}
   archivename:= SummaryFileName;
    {$ENDIF}
    archivename := StringReplace(archivename, 'NOSODATA', 'data', [rfReplaceAll]);
    MyZipFile.Entries.AddFileEntry(SummaryFileName, archivename);
    MyZipFile.ZipAllFiles;
    Result := True;
  except
    ON E: Exception do
  end{Try};
  MyZipFile.Free;
  LeaveCriticalSection(CS_SummaryDisk);
end;

function GetSummaryAsMemStream(out LMs: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(CS_SummaryDisk);
  try
    LMs.LoadFromFile(SummaryFileName);
    Result := LMs.Size;
    LMs.Position := 0;
  except
    ON E: Exception do
  end{Try};
  LeaveCriticalSection(CS_SummaryDisk);
end;

function GetZIPSummaryAsMemStream(out LMs: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(CS_SummaryDisk);
  try
    LMs.LoadFromFile(ZipSumaryFileName);
    Result := LMs.Size;
    LMs.Position := 0;
  except
    ON E: Exception do
  end{Try};
  LeaveCriticalSection(CS_SummaryDisk);
end;

function SaveSummaryToFile(const LStream: TMemoryStream): Boolean;
begin
  Result := False;
  EnterCriticalSection(CS_SummaryDisk);
  try
    LStream.SaveToFile(SummaryFileName);
    Result := True;
  except
    ON E: Exception do
  end{Try};
  LeaveCriticalSection(CS_SummaryDisk);
  SetSummaryHash;
end;

function CreateSumaryBackup(): Boolean;
begin
  EnterCriticalSection(CS_SummaryDisk);
  Result := TryCopyFile(SummaryFileName, SummaryFileName + '.bak');
  LeaveCriticalSection(CS_SummaryDisk);
end;

function RestoreSumaryBackup(): Boolean;
begin
  EnterCriticalSection(CS_SummaryDisk);
  Result := Trycopyfile(SummaryFileName + '.bak', SummaryFileName);
  LeaveCriticalSection(CS_SummaryDisk);
end;

function GetIndexSize(LRecords: Integer): Integer;
begin
  Result := 10;
  repeat
    Result := Result * 10;
  until Result > Lrecords;
  Result := Result div 10;
end;

function IndexFunction(LAddressHash: String; indexsize: Int64): Int64;
var
  SubStr: String;
begin
  LAddressHash := Hashmd5String(LAddressHash);
  LAddressHash := B16toB58(LAddressHash);
  SubStr := copy(LAddressHash, 2, 6);
  Result := StrToInt64(b58toB10(SubStr)) mod indexsize;
end;

{Reads a specific summary record position from disk}
function ReadSumaryRecordFromDisk(index: Integer): TSummaryData;
var
  SumFile: file;
begin
  Result := Default(TSummaryData);
  AssignFile(SumFile, SummaryFileName);
  EnterCriticalSection(CS_SummaryDisk);
  try
    Reset(SumFile, 1);
    try
      seek(Sumfile, index * (sizeof(Result)));
      blockread(sumfile, Result, sizeof(Result));
    except
    end;{Try}
    CloseFile(SumFile);
  except
  end;{Try}
  LeaveCriticalSection(CS_SummaryDisk);
end;

{Add a pointer to the summary index}
procedure InsertIndexData(LRecord: TSummaryData; DiskPos: Int64);
var
  IndexValue: Int64;
begin
  IndexValue := IndexFunction(LRecord.Hash, IndexLength);
  Insert(DiskPos, SumaryIndex[IndexValue], length(SumaryIndex[IndexValue]));
  if LRecord.Custom <> '' then
  begin
    IndexValue := IndexFunction(LRecord.custom, IndexLength);
    Insert(DiskPos, SumaryIndex[IndexValue], length(SumaryIndex[IndexValue]));
  end;
end;

{Creates the summary index from the disk}
function CreateSumaryIndex(): Int64;
var
  SumFile: file;
  ThisRecord: TSummaryData;
  CurrPos: Int64 = 0;
  Opened: Boolean = False;
  Closed: Boolean = False;
begin
  beginperformance('CreateSumaryIndex');
  AssignFile(SumFile, SummaryFileName);
  EnterCriticalSection(CS_SumIndex);
  SetLength(SumaryIndex, 0, 0);
  EnterCriticalSection(CS_SummaryDisk);
  try
    Reset(SumFile, 1);
    Opened := True;
    IndexLength := GetIndexSize(FileSize(SumFile) div Sizeof(TSummaryData));
    SetLength(SumaryIndex, IndexLength);
    while not EOF(SumFile) do
    begin
      blockread(sumfile, ThisRecord, sizeof(ThisRecord));
      InsertIndexData(ThisRecord, CurrPos);
      Inc(currpos);
    end;
    CloseFile(SumFile);
    Closed := True;
  except
  end;{Try}
  LeaveCriticalSection(CS_SummaryDisk);
  LeaveCriticalSection(CS_SumIndex);
  Result := EndPerformance('CreateSumaryIndex');
  SummaryLastop := ReadSumaryRecordFromDisk(0).LastOp;
  SetSummaryHash;
end;

{Returns the summary index length}
function SumIndexLength(): Int64;
begin
  EnterCriticalSection(CS_SumIndex);
  Result := IndexLength;
  LeaveCriticalSection(CS_SumIndex);
end;

{If found, returns the record}
function GetIndexPosition(LText: String; out RecordData: TSummaryData;
  IsAlias: Boolean = False): Int64;
var
  IndexPos: Int64;
  counter: Integer = 0;
  ThisRecord: TSummaryData;
begin
  Result := -1;
  RecordData := Default(TSummaryData);
  IndexPos := IndexFunction(LText, IndexLength);
  if length(SumaryIndex[IndexPos]) > 0 then
  begin
    EnterCriticalSection(CS_SumIndex);
    for counter := 0 to high(SumaryIndex[IndexPos]) do
    begin
      ThisRecord := ReadSumaryRecordFromDisk(SumaryIndex[IndexPos][counter]);
      if (((Thisrecord.Hash = LText) and (not isAlias)) or
        ((ThisRecord.Custom = LText) and (IsAlias))) then
      begin
        RecordData := ThisRecord;
        Result := SumaryIndex[IndexPos][counter];
        break;
      end;
    end;
    LeaveCriticalSection(CS_SumIndex);
  end;
end;

{Returns the balance of a specific address}
function GetAddressBalanceIndexed(Address: String): Int64;
var
  IndexPos: Integer;
  counter: Integer = 0;
  ThisRecord: TSummaryData;
begin
  Result := 0;
  IndexPos := IndexFunction(address, length(SumaryIndex));
  if IndexPos > Length(SumaryIndex) then Exit;
  if length(SumaryIndex[IndexPos]) > 0 then
  begin
    EnterCriticalSection(CS_SumIndex);
    for counter := 0 to high(SumaryIndex[IndexPos]) do
    begin
      ThisRecord := ReadSumaryRecordFromDisk(SumaryIndex[IndexPos][counter]);
      if Thisrecord.Hash = address then
      begin
        Result := ThisRecord.Balance;
        break;
      end;
    end;
    LeaveCriticalSection(CS_SumIndex);
  end;
end;

{Reset the block records}
procedure ResetBlockRecords();
begin
  EnterCriticalSection(CS_BlockRecs);
  SetLength(BlockRecords, 0);
  LeaveCriticalSection(CS_BlockRecs);
end;

{Insert a block record}
procedure InsBlockRecord(LRecord: TSummaryData; SLot: Int64);
begin
  EnterCriticalSection(CS_BlockRecs);
  SetLength(BlockRecords, Length(BlockRecords) + 1);
  BlockRecords[Length(BlockRecords) - 1].DiskSlot := SLot;
  BlockRecords[Length(BlockRecords) - 1].VRecord := LRecord;
  LeaveCriticalSection(CS_BlockRecs);
end;

{Verify if a sender address have enough funds}
function SummaryValidPay(Address: String; amount, blocknumber: Int64): Boolean;
var
  counter: Integer;
  SendPos: Int64;
  ThisRecord: TSummaryData;
begin
  Result := False;
  EnterCriticalSection(CS_BlockRecs);
  try
    for counter := 0 to high(BlockRecords) do
    begin
      if BlockRecords[counter].VRecord.Hash = Address then
      begin
        if BlockRecords[counter].VRecord.Balance < amount then Exit(False)
        else
        begin
          Dec(BlockRecords[counter].VRecord.Balance, amount);
          BlockRecords[counter].VRecord.LastOP := BlockNumber;
          Exit(True);
        end;
      end;
    end;
  finally
    LeaveCriticalSection(CS_BlockRecs);
  end;
  SendPos := GetIndexPosition(Address, ThisRecord);
  if SendPos < 0 then exit(False)
  else
  begin
    if ThisRecord.Balance < amount then Exit(False)
    else
    begin
      Dec(ThisRecord.Balance, amount);
      ThisRecord.LastOP := Blocknumber;
      InsBlockRecord(ThisRecord, sendpos);
      Exit(True);
    end;
  end;
end;

procedure SummaryPay(Address: String; amount, blocknumber: Int64);
var
  counter: Integer;
  SendPos: Int64;
  ThisRecord: TSummaryData;
begin
  EnterCriticalSection(CS_BlockRecs);
  try
    for counter := 0 to high(BlockRecords) do
    begin
      if BlockRecords[counter].VRecord.Hash = Address then
      begin
        Dec(BlockRecords[counter].VRecord.Balance, amount);
        BlockRecords[counter].VRecord.LastOP := BlockNumber;
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection(CS_BlockRecs);
  end;
  SendPos := GetIndexPosition(Address, ThisRecord);
  if SendPos < 0 then ThisRecord.Hash := address
  else
  begin
    Dec(ThisRecord.Balance, amount);
    ThisRecord.LastOP := Blocknumber;
    InsBlockRecord(ThisRecord, sendpos);
    Exit;
  end;
end;

{Set an ammount to be credited to an specific address}
procedure CreditTo(Address: String; amount, blocknumber: Int64);
var
  counter: Integer;
  SummPos: Int64;
  ThisRecord: TSummaryData;
begin
  EnterCriticalSection(CS_BlockRecs);
  try
    for counter := 0 to high(BlockRecords) do
    begin
      if BlockRecords[counter].VRecord.Hash = Address then
      begin
        Inc(BlockRecords[counter].VRecord.Balance, amount);
        BlockRecords[counter].VRecord.LastOP := BlockNumber;
        Exit;
      end;
    end;
  finally
    LeaveCriticalSection(CS_BlockRecs);
  end;
  SummPos := GetIndexPosition(Address, ThisRecord);
  Inc(ThisRecord.Balance, amount);
  ThisRecord.LastOP := BlockNumber;
  if SummPos < 0 then ThisRecord.Hash := Address;
  InsBlockRecord(ThisRecord, SummPos);
end;

{Process if an address customization is valid}
function IsCustomizacionValid(address, custom: String; blocknumber: Int64;
  forceCustom: Boolean = False): Boolean;
var
  counter: Integer;
  SumPos: Int64;
  ThisRecord: TSummaryData;
begin
  Result := False;
  EnterCriticalSection(CS_BlockRecs);
  try
    for counter := 0 to high(BlockRecords) do
    begin
      if BlockRecords[counter].VRecord.Hash = Address then
      begin
        if ((BlockRecords[counter].VRecord.Custom <> '') and (not forceCustom)) then
          exit(False);
        if ((BlockRecords[counter].VRecord.Balance < 25000) and (not forceCustom)) then
          Exit(False);
        BlockRecords[counter].VRecord.Custom := custom;
        Dec(BlockRecords[counter].VRecord.Balance, 25000);
        exit(True);
      end;
    end;
  finally
    LeaveCriticalSection(CS_BlockRecs);
  end;
  SumPos := GetIndexPosition(Address, ThisRecord);
  if SumPos < 0 then Exit(False);
  if ((ThisRecord.Balance < 25000) and (not forceCustom)) then Exit(False);
  if ((thisRecord.Custom <> '') and (not forceCustom)) then Exit(False);
  ThisRecord.Custom := custom;
  Dec(ThisRecord.Balance, 25000);
  ThisRecord.LastOP := BlockNumber;
  InsBlockRecord(ThisRecord, SumPos);
  Result := True;
end;

{Process the changes of the block to the summary on disk}
procedure UpdateSummaryChanges();
var
  counter: Integer;
  SumFile: file;
begin
  AssignFile(SumFile, SummaryFileName);
  EnterCriticalSection(CS_SummaryDisk);
  try
    Reset(SumFile, 1);
    try
      for counter := 0 to high(BlockRecords) do
      begin
        if BlockRecords[counter].DiskSlot < 0 then
        begin
          BlockRecords[counter].DiskSlot := FileSize(SumFile) div Sizeof(TSummaryData);
          InsertIndexData(BlockRecords[counter].VRecord, BlockRecords[counter].DiskSlot);
        end;
        seek(Sumfile, BlockRecords[counter].DiskSlot * (sizeof(TSummaryData)));
        blockwrite(sumfile, BlockRecords[counter].VRecord, sizeof(TSummaryData));
      end;
    except
    end;{Try}
    CloseFile(SumFile);
  except
  end;{Try}
  LeaveCriticalSection(CS_SummaryDisk);
  SummaryLastop := ReadSumaryRecordFromDisk(0).LastOp;
  SetSummaryHash;
end;

{Returns the address alias name if exists}
function GetAddressAlias(Address: String): String;
var
  sumpos: Int64;
  LRecord: TSummaryData;
begin
  Result := '';
  sumpos := GetIndexPosition(Address, LRecord);
  if ((sumpos >= 0) and (LRecord.Custom <> '')) then Result := LRecord.Custom;
end;

{Returns the address last operation block}
function GetAddressLastOP(Address: String): Int64;
var
  sumpos: Int64;
  LRecord: TSummaryData;
begin
  Result := 0;
  sumpos := GetIndexPosition(Address, LRecord);
  if (sumpos >= 0) then Result := LRecord.LastOP;
end;

{$ENDREGION}

procedure SetSummaryHash();
begin
  EnterCriticalSection(CS_SummaryHashV);
  SummaryHashValue := HashMD5File(SummaryFileName);
  LeaveCriticalSection(CS_SummaryHashV);
end;

function MySumarioHash: String;
begin
  EnterCriticalSection(CS_SummaryHashV);
  Result := SummaryHashValue;
  LeaveCriticalSection(CS_SummaryHashV);
end;

initialization
  SetLength(SumaryIndex, 0, 0);
  SetLength(BlockRecords, 0);
  InitCriticalSection(CS_SummaryDisk);
  InitCriticalSection(CS_BlockRecs);
  InitCriticalSection(CS_SummaryHashV);
  InitCriticalSection(CS_SumIndex);

finalization
  DoneCriticalSection(CS_SummaryDisk);
  DoneCriticalSection(CS_BlockRecs);
  DoneCriticalSection(CS_SummaryHashV);
  DoneCriticalSection(CS_SumIndex);

end. {End unit}
