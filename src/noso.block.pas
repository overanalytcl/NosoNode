unit Noso.Block;

{
NosoNosoCFG 1.1
Febraury 5, 2024
All block related controls
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Zipper,
  Noso.Debug, Noso.Summary, Noso.Crypto, Noso.General;

type
  TDBRecord = record
    block: Integer;
    orderID: Integer;
    Source: Integer;
    Target: Integer;
  end;

  BlockHeaderData = packed record
    Number: Int64;
    TimeStart: Int64;
    TimeEnd: Int64;
    TimeTotal: Integer;
    TimeLast20: Integer;
    TrxTotales: Integer;
    Difficult: Integer;
    TargetHash: String[32];
    Solution: String[200]; // 180 necessary
    LastBlockHash: String[32];
    NxtBlkDiff: Integer;
    AccountMiner: String[40];
    MinerFee: Int64;
    Reward: Int64;
  end;

  IntArray = array of Integer;

procedure SetBlockDirectory(NewFolder: String);

procedure CreateDBFile();
function GetDBRecords(): Integer;
function AddRecordToDBFile(block, order, Source, target: Integer): Boolean;
function GetDBLastBlock(): Integer;
function UpdateBlockDatabase(): Boolean;
function InsertToIndex(LData: TDBRecord): Boolean;
function CreateOrderIDIndex(): Boolean;
function GetBlockFromOrder(OrderID: String): Integer;
function GetOrderFromDB(OrderID: String; out OrderInfo: TOrderData): Boolean;

function GetMyLastUpdatedBlock(): Int64;
function GetBlockTransfers(BlockNumber: Integer): TBlockOrders;
function LoadBlockDataHeader(BlockNumber: Integer): BlockHeaderData;

function SaveStreamAsZipBlocks(const LStream: TMemoryStream): Boolean;
function GetBlocksAsStream(out LMs: TMemoryStream;
  firstblock, CurrentLastblock: Integer): Int64;

var
  BlockDirectory: String = 'NOSODATA' + DirectorySeparator + 'BLOCKS' +
    DirectorySeparator;
  BlocksZipFile: String = 'blocks.zip';
  DBDirectory: String = 'DB' + DirectorySeparator;
  DataBaseFilename: String = 'blocks_db.nos';
  DBFile: file of TDBRecord;
  CSDBFile: TRTLCriticalSection;
  CSDBIndex: TRTLCriticalSection;
  OrderIDIndex: array of TindexRecord;

implementation

procedure SetBlockDirectory(NewFolder: String);
begin
  BlockDirectory := NewFolder;
  Assignfile(DBFile, BlockDirectory + DBDirectory + DataBaseFilename);
end;

{$REGION blocks database}

// Creates an empty DB file
procedure CreateDBFile();
begin
  try
    Rewrite(DBFile);
    Closefile(DBFile);
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoBlock,CreateDBFile,' + E.Message);
    end;
  end;
end;

// Returns the records count on file
function GetDBRecords(): Integer;
var
  opened: Boolean = False;
  Closed: Boolean = False;
begin
  Result := 0;
  EnterCriticalSection(CSDBFile);
  try
    Reset(DBFile);
    opened := True;
    Result := Filesize(DBFile);
    Closefile(DBFile);
    Closed := True;
  except
    ON E: Exception do
    begin
    end;
  end;
  if ((opened) and (not closed)) then Closefile(DBfile);
  LeaveCriticalSection(CSDBFile);
end;

// Add a new record to the File and also to the index
function AddRecordToDBFile(block, order, Source, target: Integer): Boolean;
var
  NewData: TDBRecord;
  opened: Boolean = False;
  Closed: Boolean = False;
begin
  Result := True;
  NewData := Default(TDBRecord);
  NewData.block := Block;
  NewData.orderID := order;
  NewData.Source := Source;
  NewData.Target := target;
  EnterCriticalSection(CSDBFile);
  try
    Reset(DBFile);
    opened := True;
    Seek(DBFile, Filesize(DBFile));
    Write(DBFile, NewData);
    Closefile(DBFile);
    Closed := True;
  except
    ON E: Exception do
    begin
      Result := False;
    end;
  end;
  if ((opened) and (not closed)) then Closefile(DBfile);
  LeaveCriticalSection(CSDBFile);
  InsertToIndex(NewData);
end;

// Returns the last block on file
function GetDBLastBlock(): Integer;
var
  NewData: TDBRecord;
  opened: Boolean = False;
  Closed: Boolean = False;
begin
  Result := -1;
  EnterCriticalSection(CSDBFile);
  try
    Reset(DBFile);
    opened := True;
    Result := 0;
    if Filesize(DBFile) > 0 then
    begin
      Seek(DBFile, Filesize(DBFile) - 1);
      Read(DBFile, NewData);
      Result := NewData.Block;
    end;
    Closefile(DBFile);
    Closed := True;
  except
    ON E: Exception do
    begin

    end;
  end;
  if ((opened) and (not closed)) then Closefile(DBfile);
  LeaveCriticalSection(CSDBFile);
end;

// Calculates the integer for the value
function DBIndex(Text: String): Integer;
var
  SubStr: String;
begin
  Text := Hashmd5String(Text);
  Text := B16toB58(Text);
  SubStr := copy(Text, 2, 6);
  Result := StrToInt64(b58toB10(SubStr)) mod 100000;
end;

// updates file and database to most recent block on disk
function UpdateBlockDatabase(): Boolean;
var
  LastUpdated: Integer;
  UntilBlock: Integer;
  counter, counter2: Integer;
  ArrayOrders: TBlockOrders;
  ThisOrder: TOrderData;
begin
  Result := True;
  LastUpdated := GetDBLastBlock;
  UntilBlock := LastUpdated + 1000;
  if untilblock > GetMyLastUpdatedBlock then untilblock := GetMyLastUpdatedBlock;
  for counter := LastUpdated + 1 to untilblock do
  begin
    ArrayOrders := Default(TBlockOrders);
    ArrayOrders := GetBlockTransfers(counter);
    for counter2 := 0 to length(ArrayOrders) - 1 do
    begin
      ThisOrder := ArrayOrders[counter2];
      if ThisOrder.OrderType <> '' then
      begin
        AddRecordToDBFile(Counter, DBIndex(ThisOrder.OrderID), DBIndex(
          ThisOrder.Address), DBIndex(ThisOrder.Receiver));
      end;
    end;
  end;
end;

// Insert a value on index
function InsertToIndex(LData: TDBRecord): Boolean;
begin
  Result := True;
  EnterCriticalSEction(CSDBIndex);
  Insert(LData.block, OrderIDIndex[LData.orderID], length(OrderIDIndex[LData.orderID]));
  LeaveCriticalSEction(CSDBIndex);
end;

// Creates the INDEX from the file
function CreateOrderIDIndex(): Boolean;
var
  ThisData: TDBRecord;
begin
  StartPerformanceMeasurement('CreateOrderIDIndex');
  SetLength(OrderIDIndex, 0, 0);
  SetLength(OrderIDIndex, 100000);
  try
    Reset(DBFile);
    while not EOF(DBFile) do
    begin
      ThisData := Default(TDBRecord);
      Read(DBFile, ThisData);
      Insert(ThisData.block, OrderIDIndex[ThisData.orderID], length(
        OrderIDIndex[ThisData.orderID]));
    end;
  except
    ON E: Exception do
    begin

    end;
  end;
  StopPerformanceMeasurement('CreateOrderIDIndex');
end;

// Returns the array of integer of the specified index value
function GetDBArray(Value: Integer; out LArray: IntArray): Boolean;
begin
  Result := False;
  SetLength(LArray, 0);
  EnterCriticalSection(CSDBIndex);
  if length(OrderIDIndex[Value]) > 0 then
  begin
    LArray := copy(OrderIDIndex[Value], 0, length(OrderIDIndex[Value]));
    Result := True;
  end;
  LeaveCriticalSection(CSDBIndex);
end;

// Returns the block number where the order is found, or -1 if none
function GetBlockFromOrder(OrderID: String): Integer;
var
  LValue: Integer;
  ThisArray: IntArray;
  counter: Integer;
  counter2: Integer;
  ArrayOrders: TBlockOrders;
begin
  Result := -1;
  LValue := DBIndex(OrderID);
  if GetDBArray(LValue, ThisArray) then
  begin
    for counter := 0 to length(ThisArray) - 1 do
    begin
      ArrayOrders := Default(TBlockOrders);
      ArrayOrders := GetBlockTransfers(ThisArray[counter]);
      for counter2 := 0 to length(ArrayOrders) - 1 do
      begin
        if Arrayorders[counter2].OrderID = OrderID then
        begin
          Exit(Arrayorders[counter2].Block);
        end;
      end;
    end;
  end;
end;

// Returns the order data from its orderID
function GetOrderFromDB(OrderID: String; out OrderInfo: TOrderData): Boolean;
var
  IndexValue: Integer;
  Counter, counter2: Integer;
  ThisArray: IntArray;
  ArrayOrders: TBlockOrders;
begin
  Result := False;
  OrderInfo := Default(TOrderData);
  IndexValue := DBIndex(OrderID);
  if GetDBArray(IndexValue, ThisArray) then
    //if length(OrderIDIndex[IndexValue]) > 0 then
  begin
    for counter := 0 to length(ThisArray) - 1 do
    begin
      ArrayOrders := Default(TBlockOrders);
      ArrayOrders := GetBlockTransfers(ThisArray[counter]);
      for counter2 := 0 to length(ArrayOrders) - 1 do
      begin
        if Arrayorders[counter2].OrderID = OrderID then
        begin
          OrderInfo := Arrayorders[counter2];
          Exit(True);
        end;
      end;
    end;
  end;
end;

{$ENDREGION blocks database}

{$REGION Blocks Information}

// Returns the last downloaded block
function GetMyLastUpdatedBlock(): Int64;
var
  BlockFiles: TStringList;
  contador: Int64 = 0;
  LastBlock: Int64 = 0;
  OnlyNumbers: String;
  IgnoredChars: Integer;
begin
  IgNoredChars := Length(BlockDirectory) + 1;
  BlockFiles := TStringList.Create;
  try
    FindAllFiles(BlockFiles, BlockDirectory, '*.blk', True);
    while contador < BlockFiles.Count do
    begin
      OnlyNumbers := copy(BlockFiles[contador], IgNoredChars,
        length(BlockFiles[contador]) - (ignoredchars + 3));
      if StrToInt64Def(OnlyNumbers, 0) > Lastblock then
        LastBlock := StrToInt64Def(OnlyNumbers, 0);
      Inc(contador);
    end;
    Result := LastBlock;
  except
    on E: Exception do
    begin

    end;
  end; {TRY}
  BlockFiles.Free;
end;

// Return the array containing orders in the specified block
function GetBlockTransfers(BlockNumber: Integer): TBlockOrders;
var
  ArrTrxs: TBlockOrders;
  MemStr: TMemoryStream;
  Header: BlockHeaderData;
  ArchData: String;
  counter: Integer;
  TotalTrxs, totalposes: Integer;
  posreward: Int64;
begin
  Setlength(ArrTrxs, 0);
  ArchData := BlockDirectory + IntToStr(BlockNumber) + '.blk';
  MemStr := TMemoryStream.Create;
  try
    MemStr.LoadFromFile(ArchData);
    MemStr.Position := 0;
    MemStr.Read(Header, SizeOf(Header));
    TotalTrxs := header.TrxTotales;
    SetLength(ArrTrxs, TotalTrxs);
    for Counter := 0 to TotalTrxs - 1 do
      MemStr.Read(ArrTrxs[Counter], Sizeof(ArrTrxs[Counter])); // read each record
  except
    on E: Exception do
    begin
      ToDeepDebug('Nosoblock,GetBlockTrxs,' + E.Message);
    end;
  end;
  MemStr.Free;
  Result := ArrTrxs;
end;

function LoadBlockDataHeader(BlockNumber: Integer): BlockHeaderData;
var
  MemStr: TMemoryStream;
  Header: BlockHeaderData;
  ArchData: String;
begin
  Header := Default(BlockHeaderData);
  ArchData := BlockDirectory + IntToStr(BlockNumber) + '.blk';
  MemStr := TMemoryStream.Create;
  try
    MemStr.LoadFromFile(ArchData);
    MemStr.Position := 0;
    MemStr.Read(Header, SizeOf(Header));
  except
    ON E: Exception do
    begin
      ToLog('console', 'Error loading Header from block ' + IntToStr(
        BlockNumber) + ':' + E.Message);
    end;
  end{Try};
  MemStr.Free;
  Result := header;
end;

{$ENDREGION Blocks Information}

{$REGION Blocks Files management}

function SaveStreamAsZipBlocks(const LStream: TMemoryStream): Boolean;
begin
  Result := False;
  try
    LStream.SaveToFile(BlockDirectory + BlocksZipFile);
    Result := True;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoBlock,SaveStreamAsZipBlocks,' + E.Message);
    end;
  end{Try};
end;

// Creates the zip block file
function GetBlocksAsStream(out LMs: TMemoryStream;
  firstblock, CurrentLastblock: Integer): Int64;
var
  MyZipFile: TZipper;
  ZipFileName: String;
  LastBlock: Integer;
  contador: Integer;
  filename, archivename: String;
begin
  Result := 0;
  LastBlock := FirstBlock + 100;
  if LastBlock > CurrentLastblock then LastBlock := CurrentLastblock;
  MyZipFile := TZipper.Create;
  ZipFileName := BlockDirectory + 'Blocks_' + IntToStr(FirstBlock) +
    '_' + IntToStr(LastBlock) + '.zip';
  MyZipFile.FileName := ZipFileName;
  try
    for contador := FirstBlock to LastBlock do
    begin
      filename := BlockDirectory + IntToStr(contador) + '.blk';
      {$IFDEF WINDOWS}
      archivename:= StringReplace(filename,'\','/',[rfReplaceAll]);
      {$ENDIF}
      {$IFDEF UNIX}
      archivename:= filename;
      {$ENDIF}
      MyZipFile.Entries.AddFileEntry(filename, archivename);
    end;
    MyZipFile.ZipAllFiles;
    //result := ZipFileName;
  except
    ON E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error zipping block files: ' + E.Message);
    end;
  end;
  MyZipFile.Free;
  try
    LMs.LoadFromFile(ZipFileName);
    Result := LMs.Size;
    LMs.Position := 0;
  except
    ON E: Exception do
  end{Try};
  Trydeletefile(ZipFileName);
end;

{$ENDREGION Blocks Files management}

initialization
  Assignfile(DBFile, BlockDirectory + DBDirectory + DataBaseFilename);
  InitCriticalSection(CSDBFile);
  InitCriticalSection(CSDBIndex);
  SetLength(OrderIDIndex, 0, 0);
  SetLength(OrderIDIndex, 100000);

finalization
  DoneCriticalSection(CSDBFile);
  DoneCriticalSection(CSDBIndex);

end.
