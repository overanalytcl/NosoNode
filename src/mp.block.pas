unit MP.Block;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPForm, fileutil, MP.Coin, Dialogs, Math,
  Noso.Time, Noso.Debug, Noso.General, Noso.Crypto, Noso.Summary, strutils,
  Noso.Pso, Noso.WallCon, Noso.Headers, Noso.Block, Noso.Config, Noso.Network, Noso.Gvt,
  Noso.Masternodes;

procedure CrearBloqueCero();
procedure BuildNewBlock(Numero, TimeStamp: Int64; TargetHash, Minero, Solucion: String);
 //Function GetDiffHashrate(bestdiff:String):integer;
 //Function BestHashReadeable(BestDiff:String):string;
//function GetDiffForNextBlock(UltimoBloque,Last20Average,lastblocktime,previous:integer):integer;
//function GetLast20Time(LastBlTime:integer):integer;
function GetBlockReward(BlNumber: Int64): Int64;
function GuardarBloque(NombreArchivo: String; Cabezera: BlockHeaderData;
  Ordenes: array of TOrderData; PosPay: Int64; PoSnumber: Integer;
  PosAddresses: array of TArrayPos; MNsPay: Int64; MNsNumber: Integer;
  MNsAddresses: array of TArrayPos): Boolean;
 {function LoadBlockDataHeader(BlockNumber:integer):BlockHeaderData;}
 {function GetBlockTrxs(BlockNumber:integer):TBlockOrdersArray;}
procedure UndoneLastBlock();
function GetBlockPoSes(BlockNumber: Integer): BlockArraysPos;
function GetBlockMNs(BlockNumber: Integer): BlockArraysPos;
function RemoveBlocks(UpToBlock: Int64): Integer;
function GEtNSLBlkOrdInfo(LineText: String): String;

implementation

uses
  MP.Disk, MP.Protocol, MP.Gui, MP.Parser, MP.Red;

function CreateDevPaymentOrder(number: Integer; timestamp, amount: Int64): TOrderData;
begin
  Result := Default(TOrderData);
  Result.Block := number;
  //Result.OrderID    :='';
  Result.OrderLineCount := 1;
  Result.OrderType := 'PROJCT';
  Result.TimeStamp := timestamp - 1;
  Result.Reference := 'null';
  Result.TransferLine := 1;
  Result.Sender := 'COINBASE';
  Result.Address := 'COINBASE';
  Result.Receiver := 'NpryectdevepmentfundsGE';
  Result.AmountFee := 0;
  Result.AmountTransferred := amount;
  Result.Signature := 'COINBASE';
  Result.TransferId := GetTransferHash(Result.TimeStamp.ToString +
    'COINBASE' + 'NpryectdevepmentfundsGE' + IntToStr(amount) + IntToStr(LastBlockIndex));
  Result.OrderID := {GetOrderHash(}'1' + Result.TransferID{)};
end;

function CreateNosoPayOrder(number: Integer; AddSend, AddReceive: String;
  timestamp, amount: Int64): TOrderData;
begin
  Result := Default(TOrderData);
  Result.Block := number;
  Result.OrderLineCount := 1;
  Result.OrderType := 'TRFR';
  Result.TimeStamp := timestamp - 1;
  Result.Reference := 'null';
  Result.TransferLine := 1;
  Result.Sender := AddSend;
  Result.Address := AddSend;
  Result.Receiver := AddReceive;
  Result.AmountFee := 0;
  Result.AmountTransferred := amount;
  Result.Signature := 'Directive';
  Result.TransferId := GetTransferHash(Result.TimeStamp.ToString + 'TRFR' +
    AddSend + IntToStr(amount) + IntToStr(LastBlockIndex));
  Result.OrderID := GetOrderHash('1' + Result.TransferId);
end;

// Build the default block 0
procedure CrearBloqueCero();
begin
  BuildNewBlock(0, GenesysTimeStamp, '', adminhash, '');
  if G_Launching then ToLog('console', 'Block GENESYS (0) created.');
  //'Block 0 created.'
  if G_Launching then OutText('✓ Block 0 created', False, 1);
end;

// Crea un bloque nuevo con la informacion suministrada
procedure BuildNewBlock(Numero, TimeStamp: Int64; TargetHash, Minero, Solucion: String);
var
  BlockHeader: BlockHeaderData;
  StartBlockTime: Int64 = 0;
  MinerFee: Int64 = 0;
  ListaOrdenes: array of TOrderData;
  IgnoredTrxs: array of TOrderData;
  Filename: String;
  Contador: Integer = 0;
  OperationAddress: String = '';
  errored: Boolean = False;
  PoWTotalReward: Int64;
  ArrayLastBlockTrxs: TBlockOrders;
  ExistsInLastBlock: Boolean;
  Count2: Integer;
  NewMNs, ExpiredMNs: Integer;

  DevsTotalReward: Int64 = 0;
  DevOrder: TOrderData;

  PoScount: Integer = 0;
  PosRequired, PosReward: Int64;
  PoSTotalReward: Int64 = 0;
  PoSAddressess: array of TArrayPos;

  MNsCount: Integer;
  MNsReward: Int64;
  MNsTotalReward: Int64 = 0;
  MNsAddressess: array of TArrayPos;
  ThisParam: String;

  MNsFileText: String = '';
  GVTsTransfered: Integer = 0;
  NosoPayData: String = '';
  NPDOrder: TOrderData;
  NPDBlock: Integer;
  NPDSource: String;
  NPDTarget: String;
  NPDAmount: Int64;
  BlockTrfrs: Integer = 0;
begin
  if WO_skipBlocks then exit;
  if GetCFGDataStr(0) = 'STOP' then
  begin
    ClearAllPendingTransactions;
    exit;
  end;
  if AnsiContainsStr(GetCFGDataStr(0), 'EMPTY') then ClearAllPendingTransactions;
  BuildingBlock := Numero;
  StartPerformanceMeasurement('BuildNewBlock');
  if ((numero > 0) and (Timestamp < lastblockdata.TimeEnd)) then
  begin
    ToLog('console', 'New block ' + IntToStr(numero) + ' : Invalid timestamp');
    ToLog('console', 'Blocks can not be added until ' +
      TimestampToDate(GenesysTimeStamp));
    errored := True;
  end;
  if TimeStamp > UTCTime + 5 then
  begin
    ToLog('console', 'New block ' + IntToStr(numero) + ' : Invalid timestamp');
    ToLog('console', 'Timestamp ' + IntToStr(TimeStamp) + ' is ' +
      IntToStr(TimeStamp - UTCTime) + ' seconds in the future');
    errored := True;
  end;
  if not errored then
  begin
    if Numero = 0 then StartBlockTime := 1531896783
    else
      StartBlockTime := LastBlockData.TimeEnd + 1;
    FileName := BlockDirectory + IntToStr(Numero) + '.blk';
    SetLength(ListaOrdenes, 0);
    SetLength(IgnoredTrxs, 0);
    // Generate summary copy
    CreateSummaryBackup();

    // Generate GVT copy
    EnterCriticalSection(GVTArrayLock);
    trydeletefile(GVTFilename + '.bak');
    copyfile(GVTFilename, GVTFilename + '.bak');
    LeaveCriticalSection(GVTArrayLock);

    // Processs pending orders
    EnterCriticalSection(CSPendingTransactions);
    StartPerformanceMeasurement('NewBLOCK_PENDING');
    ArrayLastBlockTrxs := Default(TBlockOrders);
    ArrayLastBlockTrxs := GetBlockTransfers(LastBlockIndex);
    ResetBlockRecords;
    for contador := 0 to length(PendingTransactionsPool) - 1 do
    begin
      // Version 0.2.1Ga1 reverification starts
      if PendingTransactionsPool[contador].TimeStamp < LastBlockData.TimeStart then
        continue;
      //{
      ExistsInLastBlock := False;
      for count2 := 0 to length(ArrayLastBlockTrxs) - 1 do
      begin
        if ArrayLastBlockTrxs[count2].TransferId = PendingTransactionsPool[contador].TransferId then
        begin
          ExistsInLastBlock := True;
          break;
        end;
      end;
      if ExistsInLastBlock then continue;
      if ((PendingTransactionsPool[contador].TimeStamp + 60 > TimeStamp) or
        (BlockTrfrs >= 2000)) then
      begin
        if PendingTransactionsPool[contador].TimeStamp < TimeStamp + 600 then
          insert(PendingTransactionsPool[contador], IgnoredTrxs, length(IgnoredTrxs));
        continue;
      end;
      if PendingTransactionsPool[contador].OrderType = 'CUSTOM' then
      begin
        OperationAddress := GetAddressFromPublicKey(PendingTransactionsPool[contador].Sender);
        if IsCustomizationValid(OperationAddress, PendingTransactionsPool[contador].Receiver,
          numero) then
        begin
          minerfee := minerfee + PendingTransactionsPool[contador].AmountFee;
          PendingTransactionsPool[contador].Block := numero;
          PendingTransactionsPool[contador].Sender := OperationAddress;
          insert(PendingTransactionsPool[contador], ListaOrdenes, length(listaordenes));
          Inc(BlockTrfrs);
        end;
      end;
      if PendingTransactionsPool[contador].OrderType = 'TRFR' then
      begin
        OperationAddress := PendingTransactionsPool[contador].Address;
        if IsSummaryValidPayment(OperationAddress, PendingTransactionsPool[contador].AmountFee +
          PendingTransactionsPool[contador].AmountTransferred, numero) then
        begin
          minerfee := minerfee + PendingTransactionsPool[contador].AmountFee;
          CreditTo(PendingTransactionsPool[contador].Receiver,
            PendingTransactionsPool[contador].AmountTransferred, numero);
          PendingTransactionsPool[contador].Block := numero;
          PendingTransactionsPool[contador].Sender := OperationAddress;
          insert(PendingTransactionsPool[contador], ListaOrdenes, length(listaordenes));
          Inc(BlockTrfrs);
        end;
      end;
      if ((PendingTransactionsPool[contador].OrderType = 'SNDGVT') and
        (PendingTransactionsPool[contador].Sender = AdminPubKey)) then
      begin
        OperationAddress := GetAddressFromPublicKey(PendingTransactionsPool[contador].Sender);
        if GetAddressBalanceIndexed(OperationAddress) <
          PendingTransactionsPool[contador].AmountFee then continue;
        if ChangeGVTOwner(StrToIntDef(PendingTransactionsPool[contador].Reference, 100),
          OperationAddress, PendingTransactionsPool[contador].Receiver) = 0 then
        begin
          minerfee := minerfee + PendingTransactionsPool[contador].AmountFee;
          Inc(GVTsTransfered);
          IsSummaryValidPayment(OperationAddress, PendingTransactionsPool[contador].AmountFee, numero);
          PendingTransactionsPool[contador].Block := numero;
          PendingTransactionsPool[contador].Sender := OperationAddress;
          insert(PendingTransactionsPool[contador], ListaOrdenes, length(listaordenes));
        end;
      end;
    end;
    // Proyect payments
    if GetCFGDataStr(6) <> '' then
    begin
      NosoPayData := GetCFGDataStr(6);
      NosoPayData := StringReplace(NosoPayData, ':', '', [rfReplaceAll, rfIgnoreCase]);
      NosoPayData := StringReplace(NosoPayData, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
      NPDBlock := StrToIntdef(GetParameter(NosoPayData, 0), 0);
      if NPDBlock = numero then
      begin
        NPDSource := GetParameter(NosoPayData, 1);
        NPDTarget := GetParameter(NosoPayData, 2);
        if ((IsValidHashAddress(NPDTarget)) and (IsValidHashAddress(NPDSource))) then
        begin
          NPDAmount := StrToInt64def(GetParameter(NosoPayData, 3), 0);
          if NPDAmount > 0 then
          begin
            if IsSummaryValidPayment(NPDSource, NPDamount, NPDBlock) then
            begin
              CreditTo(NPDTarget, NPDAmount, NPDBlock);
              NPDOrder :=
                CreateNosoPayOrder(NPDBlock, NPDSource, NPDTarget, TimeStamp, NPDAmount);
              insert(NPDOrder, ListaOrdenes, length(listaordenes));
              RemoveCFGData(GetCFGDataStr(6), 6);
            end;
          end;
        end;
      end;
    end;
    // Project funds payment
    if numero >= PoSBlockEnd then
    begin
      DevsTotalReward := ((GetBlockReward(Numero) + MinerFee) *
        GetDevPercentage(Numero)) div 10000;
      DevORder := CreateDevPaymentOrder(numero, TimeStamp, DevsTotalReward);
      CreditTo('NpryectdevepmentfundsGE', DevsTotalReward, numero);
      insert(DevORder, ListaOrdenes, length(listaordenes));
    end;
    if GVTsTransfered > 0 then
    begin
      SaveGVTsAsData;
      UpdateMyGVTsList;
    end;
    try
      SetLength(PendingTransactionsPool, 0);
      PendingTransactionsPool := copy(IgnoredTrxs, 0, length(IgnoredTrxs));
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
          ' -> ' + 'Error asigning pending to Ignored');
      end;
    end; {TRY}
    SetLength(IgnoredTrxs, 0);
    StopPerformanceMeasurement('NewBLOCK_PENDING');
    LeaveCriticalSection(CSPendingTransactions);

    //PoS payment
    StartPerformanceMeasurement('NewBLOCK_PoS');
    if numero >= PoSBlockStart then
    begin
      SetLength(PoSAddressess, 0);
      PoSReward := 0;
      PosCount := 0;
      PosTotalReward := 0;
      if numero < PosBlockEnd then
      begin
        PosRequired := (GetCirculatingSupply(numero) * PosStackCoins) div 10000;
        PoScount := length(PoSAddressess);
        PosTotalReward := ((GetBlockReward(Numero) + MinerFee) *
          GetPoSPercentage(Numero)) div 10000;
        PosReward := PosTotalReward div PoScount;
        PosTotalReward := PoSCount * PosReward;
        //pay POS
        for contador := 0 to length(PoSAddressess) - 1 do
          CreditTo(PoSAddressess[contador].address, PosReward, numero);
      end;
    end;
    StopPerformanceMeasurement('NewBLOCK_PoS');
    // Masternodes processing
    StartPerformanceMeasurement('NewBLOCK_MNs');
    CreditMNVerifications();
    MNsFileText := GetMNsAddresses(LastBlockIndex);
    SaveMNsFile(MNsFileText);
    PopulateNodeList;
    ClearMNsChecks();
    ClearMNsList();
    ClearReceivedMNs();
    if numero >= MNBlockStart then
    begin
      SetLength(MNsAddressess, 0);
      Contador := 1;
      repeat
        begin
          ThisParam := GetParameter(MNsFileText, contador);
          if ThisParam <> '' then
          begin
            ThisParam := StringReplace(ThisParam, ':', ' ', [rfReplaceAll]);
            ThisParam := GetParameter(ThisParam, 1);
            SetLength(MNsAddressess, length(MNsAddressess) + 1);
            MNsAddressess[length(MNsAddressess) - 1].address := ThisParam;
          end;
          Inc(contador);
        end;
      until ThisParam = '';

      MNsCount := Length(MNsAddressess);
      MNsTotalReward := ((GetBlockReward(Numero) + MinerFee) *
        GetMasterNodesPercentage(Numero, GetCFGDataStr(0))) div 10000;
      if MNsCount > 0 then MNsReward := MNsTotalReward div MNsCount
      else
        MNsReward := 0;
      MNsTotalReward := MNsCount * MNsReward;
      NewMNs := 0;
      for contador := 0 to length(MNsAddressess) - 1 do
      begin
        CreditTo(MNsAddressess[contador].address, MNsReward, numero);
        if AddLockedMM(MNsAddressess[contador].address, numero) then Inc(NewMNs);
      end;
      StopPerformanceMeasurement('NewBLOCK_MNs');
    end;// End of MNS payment procecessing

    // ***END MASTERNODES PROCESSING***

    // Reset Order hashes received
    ClearReceivedOrdersIDs;

    // Miner payment
    PoWTotalReward := (GetBlockReward(Numero) + MinerFee) - PosTotalReward -
      MNsTotalReward - DevsTotalReward;
    CreditTo(Minero, PoWTotalReward, numero);
    // Update summary lastblock
    CreditTo(AdminHash, 0, numero);
    // Save summary file
    StartPerformanceMeasurement('NewBLOCK_SaveSum');
    UpdateSummaryChanges();
    StopPerformanceMeasurement('NewBLOCK_SaveSum');
    SummaryLastOperation := numero;
    // Limpiar las pendientes
    ClearWallPendings;
    // Definir la cabecera del bloque *****
    BlockHeader := Default(BlockHeaderData);
    BlockHeader.Number := Numero;
    BlockHeader.TimeStart := StartBlockTime;
    BlockHeader.TimeEnd := timeStamp;
    BlockHeader.TimeTotal := TimeStamp - StartBlockTime;
    BlockHeader.TimeLast20 := 0;//GetLast20Time(BlockHeader.TimeTotal);
    BlockHeader.TrxTotales := length(ListaOrdenes);
    if numero = 0 then BlockHeader.Difficult := InitialBlockDiff
    else if ((numero > 0) and (numero < 53000)) then BlockHeader.Difficult := 0
    else
      BlockHeader.Difficult := PoSCount;
    BlockHeader.TargetHash := TargetHash;
    //if ProtocolVersion = 1 then BlockHeader.Solution:= Solucion
    BlockHeader.Solution := Solucion + ' ' +
      {GetNMSData.Diff}'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1' + ' ' +
      PoWTotalReward.ToString + ' ' + MNsTotalReward.ToString + ' ' +
      PosTotalReward.ToString;
    if numero = 0 then BlockHeader.Solution := '';
    if numero = 0 then BlockHeader.LastBlockHash := 'NOSO GENESYS BLOCK'
    else
      BlockHeader.LastBlockHash := LastBlockHash;
    if numero < 53000 then BlockHeader.NxtBlkDiff :=
        0{MNsReward}//GetDiffForNextBlock(numero,BlockHeader.TimeLast20,BlockHeader.TimeTotal,BlockHeader.Difficult);
    else
      BlockHeader.NxtBlkDiff := MNsCount;
    BlockHeader.AccountMiner := Minero;
    BlockHeader.MinerFee := MinerFee;
    BlockHeader.Reward := GetBlockReward(Numero);
    // Fin de la cabecera -----
    // Guardar bloque al disco
    if not GuardarBloque(FileName, BlockHeader, ListaOrdenes, PosReward,
      PosCount, PoSAddressess, MNsReward, MNsCount, MNsAddressess) then
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + '*****CRITICAL*****' + slinebreak + 'Error building block: ' +
        numero.ToString);

    BuildNMSBlock := 0;
    ZipSummary;

    SetLength(ListaOrdenes, 0);
    SetLength(PoSAddressess, 0);
    // Actualizar informacion
    LastBlockIndex := Numero;
    LastBlockHash := HashMD5File(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
    LastBlockData := LoadBlockDataHeader(LastBlockIndex);
    SetSummaryHash;
    SetMNsHash;
    // Actualizar el arvhivo de cabeceras
    AddRecordToHeaders(Numero, LastBlockHash, ComputeSummaryHash);
    SetSummaryFileHash;
    if ((Numero > 0) and (form1.Server.Active)) then
    begin
      OutgoingMsjsAdd(GetProtocolLineFromCode(ping));
    end;
    CheckForMyPending;

    U_DirPanel := True;
    ExpiredMNs := ClearExpiredLockedMNs(numero);
    SavePSOFileToDisk(Numero);
    OutText(format('Block built: %d (%d ms) MNs: + %d / - %d',
      [numero, StopPerformanceMeasurement('BuildNewBlock'), NewMNs, ExpiredMNs]), True);
  end
  else
  begin
    OutText('Failed to build the block', True);
  end;
  BuildingBlock := 0;
  U_DataPanel := True;
end;

{
Function GetDiffHashrate(bestdiff:String):integer;
var
  counter :integer= 0;
Begin
repeat
  counter := counter+1;
until bestdiff[counter]<> '0';
Result := (Counter-1)*100;
if bestdiff[counter]='1' then Result := Result+50;
if bestdiff[counter]='2' then Result := Result+25;
if bestdiff[counter]='3' then Result := Result+12;
if bestdiff[counter]='4' then Result := Result+6;
//if bestdiff[counter]='5' then Result := Result+3;
End;
}
{
Function BestHashReadeable(BestDiff:String):string;
var
  counter :integer = 0;
Begin
if bestdiff = '' then BestDiff := 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
repeat
  counter := counter+1;
until bestdiff[counter]<> '0';
Result := (Counter-1).ToString+'.';
if counter<length(BestDiff) then Result := Result+bestdiff[counter];
End;
}
{
// Devuelve cuantos caracteres compondran el targethash del siguiente bloque
function GetDiffForNextBlock(UltimoBloque,Last20Average, lastblocktime,previous:integer):integer;
Begin
result := previous;
if UltimoBloque < 21 then result := InitialBlockDiff
else
   begin
   if Last20Average < SecondsPerBlock then
      begin
      if lastblocktime<SecondsPerBlock then result := Previous+1
      end
   else if Last20Average > SecondsPerBlock then
      begin
      if lastblocktime>SecondsPerBlock then result := Previous-1
      end
   else result := previous;
   end;
End;
}
{
// Hace el calculo del tiempo promedio empleado en los ultimos 20 bloques
function GetLast20Time(LastBlTime:integer):integer;
var
  Part1, Part2 : integer;
Begin
if LastBlockData.Number<21 then result := SecondsPerBlock
else
   begin
   Part1 := LastBlockData.TimeLast20 * 19 div 20;
   Part2 := LastBlTime div 20;
   result := Part1 + Part2;
   end;
End;
}
// RETURNS THE MINING REWARD FOR A BLOCK
function GetBlockReward(BlNumber: Int64): Int64;
var
  NumHalvings: Int64;
begin
  if BlNumber = 0 then Result := PremineAmount
  else if ((BlNumber > 0) and (blnumber < BlockHalvingInterval *
    (HalvingSteps + 1))) then
  begin
    numHalvings := BlNumber div BlockHalvingInterval;
    Result := InitialReward div (2 ** NumHalvings);
  end
  else
    Result := 0;
end;

// Guarda el archivo de bloque en disco
function GuardarBloque(NombreArchivo: String; Cabezera: BlockHeaderData;
  Ordenes: array of TOrderData; PosPay: Int64; PoSnumber: Integer;
  PosAddresses: array of TArrayPos; MNsPay: Int64; MNsNumber: Integer;
  MNsAddresses: array of TArrayPos): Boolean;
var
  MemStream: TMemoryStream;
  NumeroOrdenes: Int64;
  counter: Integer;
begin
  Result := True;
  StartPerformanceMeasurement('GuardarBloque');
  NumeroOrdenes := Cabezera.TrxTotales;
  MemStream := TMemoryStream.Create;
  try
    MemStream.Write(Cabezera, Sizeof(Cabezera));
    for counter := 0 to NumeroOrdenes - 1 do
      MemStream.Write(Ordenes[counter], Sizeof(Ordenes[Counter]));
    if Cabezera.Number >= PoSBlockStart then
    begin
      MemStream.Write(PosPay, Sizeof(PosPay));
      MemStream.Write(PoSnumber, Sizeof(PoSnumber));
      for counter := 0 to PoSnumber - 1 do
        MemStream.Write(PosAddresses[counter], Sizeof(PosAddresses[Counter]));
    end;
    if Cabezera.Number >= MNBlockStart then
    begin
      MemStream.Write(MNsPay, Sizeof(MNsPay));
      MemStream.Write(MNsnumber, Sizeof(MNsnumber));
      for counter := 0 to MNsNumber - 1 do
      begin
        MemStream.Write(MNsAddresses[counter], Sizeof(MNsAddresses[Counter]));
      end;
    end;
    MemStream.SaveToFile(NombreArchivo);
  except
    On E: Exception do
    begin
      ToLog('console', 'Error saving block to disk: ' + E.Message);
      Result := False;
    end;
  end{Try};
  MemStream.Free;
  StopPerformanceMeasurement('GuardarBloque');
end;

function GetBlockPoSes(BlockNumber: Integer): BlockArraysPos;
var
  resultado: BlockArraysPos;
  ArrTrxs: TBlockOrders;
  ArchData: String;
  MemStr: TMemoryStream;
  Header: BlockHeaderData;
  TotalTrxs, totalposes: Integer;
  posreward: Int64;
  counter: Integer;
begin
  Setlength(resultado, 0);
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
    MemStr.Read(posreward, SizeOf(Int64));
    MemStr.Read(totalposes, SizeOf(Integer));
    SetLength(resultado, totalposes);
    for Counter := 0 to totalposes - 1 do
      MemStr.Read(resultado[Counter].address, Sizeof(resultado[Counter]));
    SetLength(resultado, totalposes + 1);
    resultado[length(resultado) - 1].address := IntToStr(posreward);
  except
    on E: Exception do // nothing, the block is not found
  end;
  MemStr.Free;
  Result := resultado;
end;

function GetBlockMNs(BlockNumber: Integer): BlockArraysPos;
var
  resultado: BlockArraysPos;
  ArrayPos: BlockArraysPos;
  ArrTrxs: TBlockOrders;
  ArchData: String;
  MemStr: TMemoryStream;
  Header: BlockHeaderData;
  TotalTrxs, totalposes, totalMNs: Integer;
  posreward, MNreward: Int64;
  counter: Integer;
begin
  Setlength(resultado, 0);
  Setlength(ArrayPos, 0);
  if blocknumber < MNBlockStart then
  begin
    Result := resultado;
    exit;
  end;
  ArchData := BlockDirectory + IntToStr(BlockNumber) + '.blk';
  MemStr := TMemoryStream.Create;
  try
    // HEADERS
    MemStr.LoadFromFile(ArchData);
    MemStr.Position := 0;
    MemStr.Read(Header, SizeOf(Header));
    // TRXS LIST
    TotalTrxs := header.TrxTotales;
    SetLength(ArrTrxs, TotalTrxs);
    for Counter := 0 to TotalTrxs - 1 do
      MemStr.Read(ArrTrxs[Counter], Sizeof(ArrTrxs[Counter])); // read each record
    // POS INFO
    MemStr.Read(posreward, SizeOf(Int64));
    MemStr.Read(totalposes, SizeOf(Integer));
    SetLength(ArrayPos, totalposes);
    for Counter := 0 to totalposes - 1 do
      MemStr.Read(ArrayPos[Counter].address, Sizeof(ArrayPos[Counter]));
    // MNS INFO
    MemStr.Read(MNReward, SizeOf(MNReward));
    MemStr.Read(totalMNs, SizeOf(totalMNs));
    SetLength(resultado, totalMNs);
    for Counter := 0 to totalMNs - 1 do
    begin
      MemStr.Read(resultado[Counter].address, Sizeof(resultado[Counter]));
    end;
    SetLength(resultado, totalMNs + 1);
    resultado[length(resultado) - 1].address := IntToStr(MNReward);
  except
    on E: Exception do // nothing, the block is not founded
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'EXCEPTION on MNs file data:' + E.Message)
  end; {TRY}
  MemStr.Free;
  Result := resultado;
end;

// Deshacer el ultimo bloque
procedure UndoneLastBlock();
const
  Highest: Integer = 0;
var
  blocknumber: Integer;
begin
  blocknumber := LastBlockIndex;
  if BlockNumber < Highest then
  begin
    ToLog('Console', 'Can not undo block ' + LastBlockIndex.ToString);
    exit;
  end
  else
    Highest := BlockNumber;
  if BlockNumber = 0 then exit;
  if MyConStatus = 3 then
  begin
    MyConStatus := 2;
    //if Form1.Server.Active then Form1.Server.Active := false;
    ClearMNsChecks();
    ClearMNsList();
    ClearAllPendingTransactions;
    ClearReceivedOrdersIDs;
  end;
  // recover summary
  RestoreSummaryBackup();

  CreateSummaryIndex;
  // recover GVTs file
  EnterCriticalSection(GVTArrayLock);
  trydeletefile(GVTFilename);
  copyfile(GVTFilename + '.bak', GVTFilename);
  LeaveCriticalSection(GVTArrayLock);
  LoadGVTsFileData();
  UpdateMyGVTsList;

  // Actualizar la cartera
  UpdateWalletFromSumario();
  // actualizar el archivo de cabeceras
  RemoveLastHeaderRecord;
  // Borrar archivo del ultimo bloque
  trydeletefile(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
  // Actualizar mi informacion
  LastBlockIndex := GetMyLastUpdatedBlock;
  LastBlockHash := HashMD5File(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
  LastBlockData := LoadBlockDataHeader(LastBlockIndex);
  SetSummaryFileHash;
  ToLog('console', '****************************');
  ToLog('console', 'Block undone: ' + IntToStr(blocknumber)); //'Block undone: '
  ToLog('console', '****************************');
  ToLog('events', TimeToStr(now) + 'Block Undone: ' + IntToStr(blocknumber));
  ToDeepDebug('Block undone: ' + Blocknumber.ToString);
  U_DataPanel := True;
end;

function RemoveBlocks(UpToBlock: Int64): Integer;
var
  Last: Int64;
begin
  Result := 0;
  repeat
    Last := GetMyLastUpdatedBlock;
    if Last >= UpToBlock then
    begin
      trydeletefile(BlockDirectory + IntToStr(Last) + '.blk');
      Inc(Result);
    end;
  until Last < UpToBlock;
  LastBlockIndex := GetMyLastUpdatedBlock;
end;

function GEtNSLBlkOrdInfo(LineText: String): String;
var
  ParamBlock: String;
  BlkNumber: Integer;
  OrdersArray: TBlockOrders;
  Cont: Integer;
  ThisOrder: String = '';
begin
  StartPerformanceMeasurement('GEtNSLBlkOrdInfo');
  Result := 'NSLBLKORD ';
  ParamBlock := UpperCase(GetParameter(LineText, 1));
  if paramblock = 'LAST' then BlkNumber := LastBlockIndex
  else
    BlkNumber := StrToIntDef(ParamBlock, -1);
  if ((BlkNumber < 0) or (BlkNumber < LastBlockIndex - 4000) or
    (BlkNumber > LastBlockIndex)) then
    Result := Result + 'ERROR'
  else
  begin
    Result := Result + BlkNumber.ToString + ' ';
    OrdersArray := Default(TBlockOrders);
    OrdersArray := GetBlockTransfers(BlkNumber);
    if Length(OrdersArray) > 0 then
    begin
      for Cont := 0 to LEngth(OrdersArray) - 1 do
      begin
        if OrdersArray[cont].OrderType = 'TRFR' then
        begin
          ThisOrder := ThisOrder + OrdersArray[Cont].Sender + ':' +
            OrdersArray[Cont].Receiver + ':' + OrdersArray[Cont].AmountTransferred.ToString +
            ':' + OrdersArray[Cont].Reference + ':' + OrdersArray[Cont].OrderID + ' ';
        end;
      end;
    end;
    Result := Result + ThisOrder;
    Result := Trim(Result);
  end;
  StopPerformanceMeasurement('GEtNSLBlkOrdInfo');
end;

end. // END UNIT
