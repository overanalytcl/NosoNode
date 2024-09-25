unit MP.Protocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MP.Red, MPForm, MP.Parser, StrUtils,
  MP.Disk, Noso.Time, MP.Block,
  Zipper, MP.Coin, Noso.Debug, Noso.General, Noso.Crypto, Noso.Summary, Noso.Consensus, Noso.Pso,
  Noso.Headers, Noso.Config, Noso.Block, Noso.Network, Noso.Gvt, Noso.Masternodes,
  Noso.IP.Control;

//function GetPTCEcn():String;
function GetOrderFromString(textLine: String; out ThisData: TOrderData): Boolean;
//function GetStringFromOrder(order:Torderdata):String;
function GetStringFromBlockHeader(blockheader: BlockHeaderdata): String;
//Function ProtocolLine(tipo:integer):String;
procedure ParseProtocolLines();
//function IsValidProtocol(line:String):Boolean;
procedure PTC_SendLine(Slot: Int64; Message: String);
 //Procedure ClearOutTextToSlot(slot:integer);
 //Function GetTextToSlot(slot:integer):string;
 //Procedure ProcessPing(LineaDeTexto: string; Slot: integer; Responder:boolean);
function GetPingString(): String;
//procedure PTC_SendPending(Slot:int64);
procedure PTC_SendResumen(Slot: Int64);
procedure PTC_SendSumary(Slot: Int64);
procedure PTC_SendPSOS(Slot: Int64);
procedure PTC_SendGVTs(Slot: Int64);
function ZipHeaders(): Boolean;
function CreateZipBlockfile(firstblock: Integer): String;
procedure PTC_SendBlocks(Slot: Integer; TextLine: String);
procedure INC_PTC_Custom(TextLine: String; connection: Integer);
function PTC_Custom(TextLine: String): Integer;
function ValidateTrfr(order: TOrderData; Origen: String): Integer;
function IsAddressLocked(LAddress: String): Boolean;
function IsOrderIDAlreadyProcessed(OrderText: String): Boolean;
procedure INC_PTC_Order(TextLine: String; connection: Integer);
function PTC_Order(TextLine: String): String;
procedure INC_PTC_SendGVT(TextLine: String; connection: Integer);
function PTC_SendGVT(TextLine: String): Integer;
procedure PTC_AdminMSG(TextLine: String);
procedure PTC_CFGData(Linea: String);
//Procedure PTC_SendMNsList(slot:integer);
procedure PTC_SendUpdateHeaders(Slot: Integer; Linea: String);
procedure PTC_ProcessMNFileIncoming(LText: String);
procedure PTC_HeadUpdate(linea: String);

procedure AddCriptoOp(tipo: Integer; proceso, resultado: String);
procedure DeleteCriptoOp();


const
  OnlyHeaders = 0;
  Getnodes = 1;
  Nodes = 2;
  Ping = 3;
  Pong = 4;
  GetPending = 5;
  GetSumary = 6;
  GetResumen = 7;
  LastBlock = 8;
  Custom = 9;
  //NodeReport = 10;
  GetMNs = 11;
  BestHash = 12;
  MNReport = 13;
  MNCheck = 14;
  GetChecks = 15;
  GetMNsFile = 16;
  MNFile = 17;
  GetHeadUpdate = 18;
  HeadUpdate = 19;

  GetGVTs = 20;
  GetCFG = 30;
  SETCFG = 31;
  GetPSOs = 32;

implementation

uses
  MP.Gui;

// convierte los datos de la cadena en una order
function GetOrderFromString(textLine: String; out ThisData: TOrderData): Boolean;
var
  orderinfo: TOrderData;
begin
  StartPerformanceMeasurement('GetOrderFromString');
  Result := True;
  ThisData := Default(TOrderData);
  try
    ThisData.OrderID := GetParameter(textline, 1);
    ThisData.OrderLineCount := StrToInt(GetParameter(textline, 2));
    ThisData.OrderType := GetParameter(textline, 3);
    ThisData.TimeStamp := StrToInt64(GetParameter(textline, 4));
    ThisData.reference := GetParameter(textline, 5);
    ThisData.TransferLine := StrToInt(GetParameter(textline, 6));
    ThisData.Sender := GetParameter(textline, 7);
    ThisData.Address := GetParameter(textline, 8);
    ThisData.Receiver := GetParameter(textline, 9);
    ThisData.AmountFee := StrToInt64(GetParameter(textline, 10));
    ThisData.AmountTransferred := StrToInt64(GetParameter(textline, 11));
    ThisData.Signature := GetParameter(textline, 12);
    ThisData.TransferID := GetParameter(textline, 13);
  except
    ON E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error GetOrderFromString : ' + E.Message);
      Result := False;
    end;
  end;{TRY}
  StopPerformanceMeasurement('GetOrderFromString');
end;

// devuelve una cadena con los datos de la cabecera de un bloque
function GetStringFromBlockHeader(BlockHeader: blockheaderdata): String;
begin
  Result := 'Number:' + IntToStr(BlockHeader.Number) + ' ' +
    'Start:' + IntToStr(BlockHeader.TimeStart) + ' ' +
    'End:' + IntToStr(BlockHeader.TimeEnd) + ' ' +
    'Total:' + IntToStr(BlockHeader.TimeTotal) + ' ' +
    '20:' + IntToStr(BlockHeader.TimeLast20) + ' ' +
    'Trxs:' + IntToStr(BlockHeader.TrxTotales) + ' ' +
    'Diff:' + IntToStr(BlockHeader.Difficult) + ' ' +
    'Target:' + BlockHeader.TargetHash + ' ' +
    'Solution:' + BlockHeader.Solution + ' ' +
    'NextDiff:' + IntToStr(BlockHeader.NxtBlkDiff) + ' ' +
    'Miner:' + BlockHeader.AccountMiner + ' ' +
    'Fee:' + IntToStr(BlockHeader.MinerFee) + ' ' +
    'Reward:' + IntToStr(BlockHeader.Reward);

end;

// Procesa todas las lineas procedentes de las Connections
procedure ParseProtocolLines();
var
  contador: Integer = 0;
  UsedProtocol: Integer = 0;
  UsedVersion: String = '';
  PeerTime: String = '';
  Linecomando: String = '';
  ProcessLine: String;
  ValidMNCheck: String;
  OrderError: Boolean = False;
begin
  for contador := 1 to MaxConnections do
  begin
    while GetIncomingMessageLength(contador) > 0 do
    begin
      ProcessLine := GetIncomingMessage(contador);
      UsedProtocol := StrToIntDef(GetParameter(ProcessLine, 1), 1);
      UsedVersion := GetParameter(ProcessLine, 2);
      PeerTime := GetParameter(ProcessLine, 3);
      LineComando := GetParameter(ProcessLine, 4);
      if ((not IsValidProtocol(ProcessLine)) and
        (not GetConnectionData(contador).IsAuthenticated)) then
        // La linea no es valida y proviene de una conexion no autentificada
      begin
        ToLog('console', 'CONNECTION REJECTED: INVALID PROTOCOL -> ' +
          GetConnectionData(contador).IpAddress + '->' + ProcessLine); //CONNECTION REJECTED: INVALID PROTOCOL ->
        UpdateBotData(GetConnectionData(contador).IpAddress);
        CloseConnectionSlot(contador);
      end
      else if UpperCase(LineComando) = 'DUPLICATED' then
      begin
        ToLog('Console', 'You are already connected to ' + GetConnectionData(contador).IpAddress);
        //CONNECTION REJECTED: INVALID PROTOCOL ->
        CloseConnectionSlot(contador);
      end
      else if UpperCase(LineComando) = 'OLDVERSION' then
      begin
        ToLog('Console', 'You need update your node to connect to ' +
          GetConnectionData(contador).IpAddress); //CONNECTION REJECTED: INVALID PROTOCOL ->
        CloseConnectionSlot(contador);
      end
      else if UpperCase(LineComando) = '$PING' then
        ProcessPingCommand(ProcessLine, contador, True)                        // Done
      else if UpperCase(LineComando) = '$PONG' then
        ProcessPingCommand(ProcessLine, contador, False)                       // Done
      else if ((blockage < 10) or (BlockAge > 585)) then
      begin
        continue;
      end
      else if UpperCase(LineComando) = '$GETPENDING' then
        SendPendingTransactionsToPeer(contador)//PTC_SendPending(contador) // Done
      else if UpperCase(LineComando) = '$GETMNS' then
        SendMasternodeListToPeer(contador) //SendMNsList(contador)         // Done
      else if UpperCase(LineComando) = '$GETRESUMEN' then PTC_SendResumen(contador)
      else if UpperCase(LineComando) = '$LASTBLOCK' then
        PTC_SendBlocks(contador, ProcessLine)
      else if UpperCase(LineComando) = '$CUSTOM' then
      begin
        if addipcontrol(GetConnectionData(contador).IpAddress) < 100 then
          INC_PTC_Custom(GetOpData(ProcessLine), contador)
        else
        begin
          UpdateBotData(GetConnectionData(contador).IpAddress);
          ToLog('console', 'IP spammer: ' + GetConnectionData(contador).IpAddress);
          CloseConnectionSlot(contador);
        end;
      end
      else if UpperCase(LineComando) = 'ORDER' then
      begin
        if addipcontrol(GetConnectionData(contador).IpAddress) < 100 then
          INC_PTC_Order(ProcessLine, contador)
        else
        begin
          UpdateBotData(GetConnectionData(contador).IpAddress);
          ToLog('console', 'IP spammer: ' + GetConnectionData(contador).IpAddress);
          CloseConnectionSlot(contador);
        end;
      end
      else if UpperCase(LineComando) = 'ADMINMSG' then PTC_AdminMSG(ProcessLine)
      else if UpperCase(LineComando) = '$MNREPO' then AddWaitingMNs(ProcessLine)
      else if UpperCase(LineComando) = '$MNCHECK' then
      begin
        ValidMNCheck := ValidateMasternodeCheck(ProcessLine);
        if ValidMNCheck <> '' then outGOingMsjsAdd(GetProtocolHeader + ValidMNCheck);
        //PTC_MNCheck(ProcessLine)
      end
      else if UpperCase(LineComando) = '$GETCHECKS' then SendMasternodeChecksToPeer(contador)
      else if UpperCase(LineComando) = 'GETMNSFILE' then
        PTC_SendLine(contador, GetProtocolLineFromCode(MNFILE) + ' $' + LoadMNsFile)
      else if UpperCase(LineComando) = 'GETCFGDATA' then
        PTC_SendLine(contador, GetProtocolLineFromCode(SETCFG) + GetCFGDataStr)

      else if UpperCase(LineComando) = 'MNFILE' then
        PTC_ProcessMNFileIncoming(ProcessLine)
      //SaveMNsFile(ExtractMasternodesText(ProcessLine))//PTC_MNFile(ProcessLine)
      else if UpperCase(LineComando) = 'SETCFGDATA' then PTC_CFGData(ProcessLine)

      else if UpperCase(LineComando) = 'GETHEADUPDATE' then
        PTC_SendUpdateHeaders(contador, ProcessLine)
      else if UpperCase(LineComando) = 'HEADUPDATE' then PTC_HeadUpdate(ProcessLine)
      else if UpperCase(LineComando) = '$GETSUMARY' then PTC_SendSumary(contador)
      //else if UpperCase(LineComando) = '$SNDGVT' then INC_PTC_SendGVT(GetOpData(ProcessLine), contador)
      else if UpperCase(LineComando) = '$GETPSOS' then PTC_SendPSOS(contador)
      else if UpperCase(LineComando) = '$GETGVTS' then PTC_SendGVTs(contador)

      else
      begin  // El comando recibido no se reconoce. Verificar protocolos posteriores.
        ToLog('Console', 'Unknown command () in slot: (' + ProcessLine +
          ') ' + IntToStr(contador)); //Unknown command () in slot: (
      end;
    end;
  end;
end;

// Envia una linea a un determinado slot
procedure PTC_SendLine(Slot: Int64; Message: String);
begin
  if ((slot >= 1) and (Slot <= MaxConnections)) then
  begin
    if ((GetConnectionData(Slot).ConnectionType = 'CLI') and (not GetConnectionData(Slot).IsBusy)) then
    begin
      AddTextToSlot(slot, message);
    end;
    if ((GetConnectionData(Slot).ConnectionType = 'SER') and (not GetConnectionData(Slot).IsBusy)) then
    begin
      AddTextToSlot(slot, message);
    end;
  end
  else
    ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) + ' -> ' +
      'Invalid PTC_SendLine slot: ' + IntToStr(slot));
end;

// Devuelve la informacion contenida en un ping
function GetPingString(): String;
var
  Port: Integer = 0;
begin
  if Form1.Server.Active then port := Form1.Server.DefaultPort
  else
    port := -1;
  Result := IntToStr(GetTotalConnections()) + ' ' + IntToStr(LastBlockIndex) + ' ' +
    LastBlockHash + ' ' + ComputeSummaryHash + ' ' +
    GetPendingTransactionCount.ToString + ' ' + GetSummaryFileHash + ' ' +
    IntToStr(NodeConnectionStatus) + ' ' + IntToStr(port) + ' ' +
    copy(GetMNsHash, 0, 5) + ' ' + IntToStr(GetMNsListLength) + ' ' +
    'null' + ' ' + //GetNMSData.Diff
    GetMasternodeCheckCount.ToString + ' ' + GVTHashMD5 + ' ' +
    Copy(HashMD5String(GetCFGDataStr), 0, 5) + ' ' + Copy(PSOFileHash, 0, 5);
end;


// Send cHeaders file to peer
procedure PTC_SendResumen(Slot: Int64);
const
  LastRequest: Int64 = 0;
var
  MemStream: TMemoryStream;
begin
  if UTCTime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  MemStream := TMemoryStream.Create;
  GetHeadersAsMemoryStream(MemStream);
  if GetConnectionData(slot).ConnectionType = 'CLI' then
  begin
    try
      GetConnectionData(slot).Context.Connection.IOHandler.WriteLn('RESUMENFILE');
      GetConnectionData(slot).Context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConnectionData(Slot).Context);
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'SERVER: Error sending headers file (' + E.Message + ')');
      end;
    end; {TRY}
  end;
  if GetConnectionData(slot).ConnectionType = 'SER' then
  begin
    try
      ClientChannels[slot].IOHandler.WriteLn('RESUMENFILE');
      ClientChannels[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'CLIENT: Error sending Headers file (' + E.Message + ')');
        CloseConnectionSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

procedure PTC_SendSumary(Slot: Int64);
const
  LastRequest: Int64 = 0;
var
  MemStream: TMemoryStream;
begin
  if UTCtime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  MemStream := TMemoryStream.Create;
  GetSummaryAsMemoryStream(MemStream);
  if GetConnectionData(slot).ConnectionType = 'CLI' then
  begin
    try
      GetConnectionData(slot).Context.Connection.IOHandler.WriteLn('SUMARYFILE');
      GetConnectionData(slot).Context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConnectionData(Slot).Context);
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'SERVER: Error sending sumary file (' + E.Message + ')');
      end;
    end; {TRY}
  end;
  if GetConnectionData(slot).ConnectionType = 'SER' then
  begin
    try
      ClientChannels[slot].IOHandler.WriteLn('SUMARYFILE');
      ClientChannels[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'CLIENT: Error sending Sumary file (' + E.Message + ')');
        CloseConnectionSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

procedure PTC_SendPSOS(Slot: Int64);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  GetPSOsAsMemStream(MemStream);
  if GetConnectionData(slot).ConnectionType = 'CLI' then
  begin
    try
      GetConnectionData(slot).Context.Connection.IOHandler.WriteLn('PSOSFILE');
      GetConnectionData(slot).Context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConnectionData(Slot).Context);
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'SERVER: Error sending PSOs file (' + E.Message + ')');
      end;
    end; {TRY}
  end;
  if GetConnectionData(slot).ConnectionType = 'SER' then
  begin
    try
      ClientChannels[slot].IOHandler.WriteLn('PSOSFILE');
      ClientChannels[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'CLIENT: Error sending PSOs file (' + E.Message + ')');
        CloseConnectionSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

procedure PTC_SendGVTs(Slot: Int64);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  GetGVTAsStream(MemStream);
  if GetConnectionData(slot).ConnectionType = 'CLI' then
  begin
    try
      GetConnectionData(slot).Context.Connection.IOHandler.WriteLn('GVTSFILE');
      GetConnectionData(slot).Context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConnectionData(Slot).Context);
        ToDeepDebug('Error sending GVTs file: ' + E.Message);
      end;
    end; {TRY}
  end;
  if GetConnectionData(slot).ConnectionType = 'SER' then
  begin
    try
      ClientChannels[slot].IOHandler.WriteLn('GVTSFILE');
      ClientChannels[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToDeepDebug('Error sending GVTs file: ' + E.Message);
        CloseConnectionSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

// Zips the cHeaders file. Uses deprecated methods, to be removed...
function ZipHeaders(): Boolean;
var
  MyZipFile: TZipper;
  archivename: String;
begin
  Result := False;
  MyZipFile := TZipper.Create;
  MyZipFile.FileName := BlockHeadersZipFileName;
  try
    {$IFDEF WINDOWS}
   archivename:= StringReplace(SummaryFilename,'\','/',[rfReplaceAll]);
    {$ENDIF}
    {$IFDEF UNIX}
   archivename:= ResumenFilename;
    {$ENDIF}
    archivename := StringReplace(archivename, 'NOSODATA', 'data', [rfReplaceAll]);
    MyZipFile.Entries.AddFileEntry(SummaryFilename, archivename);
    MyZipFile.ZipAllFiles;
    Result := True;
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on Zip Headers file: ' + E.Message);
  end{Try};
  MyZipFile.Free;
end;

// Creates the zip block file
function CreateZipBlockfile(firstblock: Integer): String;
var
  MyZipFile: TZipper;
  ZipFileName: String;
  LastBlock: Integer;
  contador: Integer;
  filename, archivename: String;
begin
  Result := '';
  LastBlock := FirstBlock + 100;
  if LastBlock > LastBlockIndex then LastBlock := LastBlockIndex;
  MyZipFile := TZipper.Create;
  ZipFileName := BlockDirectory + 'Blocks_' + IntToStr(FirstBlock) + '_' +
    IntToStr(LastBlock) + '.zip';
  MyZipFile.FileName := ZipFileName;
  EnterCriticalSection(BlocksAccessLock);
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
    Result := ZipFileName;
  except
    ON E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error zipping block files: ' + E.Message);
    end;
  end;
  LeaveCriticalSection(BlocksAccessLock);
  MyZipFile.Free;
end;

// Send Zipped blocks to peer
procedure PTC_SendBlocks(Slot: Integer; TextLine: String);
const
  LastRequest: Int64 = 0;
var
  FirstBlock, LastBlock: Integer;
  MyZipFile: TZipper;
  contador: Integer;
  MemStream: TMemoryStream;
  filename, archivename: String;
  GetFileOk: Boolean = False;
  FileSentOk: Boolean = False;
  ZipFileName: String;
begin
  if UTCTime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  ToLog('Console', '********** DEBUG CHECK **********');
  FirstBlock := StrToIntDef(GetParameter(textline, 5), -1) + 1;
  ZipFileName := CreateZipBlockfile(FirstBlock);
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromFile(ZipFileName);
    GetFileOk := True;
  except
    on E: Exception do
    begin
      GetFileOk := False;
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on PTC_SendBlocks: ' + E.Message);
    end;
  end; {TRY}
  if GetFileOk then
  begin
    if GetConnectionData(Slot).ConnectionType = 'CLI' then
    begin
      try
        GetConnectionData(Slot).Context.Connection.IOHandler.WriteLn('BLOCKZIP');
        GetConnectionData(Slot).Context.connection.IOHandler.Write(MemStream, 0, True);
        FileSentOk := True;
      except
        on E: Exception do
        begin
          Form1.TryCloseServerConnection(GetConnectionData(Slot).Context);
          ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
            Now) + ' -> ' + 'SERVER: Error sending ZIP blocks file (' + E.Message + ')');
        end;
      end; {TRY}
    end;
    if GetConnectionData(Slot).ConnectionType = 'SER' then
    begin
      try
        ClientChannels[Slot].IOHandler.WriteLn('BLOCKZIP');
        ClientChannels[Slot].IOHandler.Write(MemStream, 0, True);
        FileSentOk := True;
      except
        on E: Exception do
        begin
          ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
            Now) + ' -> ' + 'CLIENT: Error sending ZIP blocks file (' + E.Message + ')');
          CloseConnectionSlot(slot);
        end; {TRY}
      end;
    end;
  end;
  MemStream.Free;
  Trydeletefile(ZipFileName);
end;

procedure INC_PTC_Custom(TextLine: String; connection: Integer);
begin
  AddCriptoOp(4, TextLine, '');
end;

// Procesa una solicitud de customizacion
function PTC_Custom(TextLine: String): Integer;
var
  OrderInfo: TOrderData;
  Address: String = '';
  OpData: String = '';
  ErrorCode: Integer = 0;
begin
  Result := 0;
  if not GetOrderFromString(TextLine, OrderInfo) then exit;
  Address := GetAddressFromPublicKey(OrderInfo.Sender);
  if address <> OrderInfo.Address then ErrorCode := 1;
  // La direccion no dispone de fondos
  if GetAddressBalanceIndexed(Address) - GetAddressPendingPays(Address) <
    GetCustomFee(LastBlockIndex) then ErrorCode := 2;
  if TransactionAlreadyPending(OrderInfo.TransferID) then ErrorCode := 3;
  if OrderInfo.TimeStamp < LastBlockData.TimeStart then ErrorCode := 4;
  if TransferExistsInLastBlock(OrderInfo.TransferID) then ErrorCode := 5;
  if AddressAlreadyCustomized(Address) then ErrorCode := 6;
  if AliasAlreadyExists(OrderInfo.Receiver) then ErrorCode := 7;
  if not VerifySignedString('Customize this ' + Address + ' ' + OrderInfo.Receiver,
    OrderInfo.Signature, OrderInfo.Sender) then ErrorCode := 8;
  if ErrorCode = 0 then
  begin
    OpData := GetOpData(TextLine); // Eliminar el encabezado
    AddTransactionToPool(OrderInfo);
    if form1.Server.Active then OutgoingMsjsAdd(GetProtocolHeader + opdata);
  end;
  Result := ErrorCode;
end;

function IsAddressLocked(LAddress: String): Boolean;
begin
  Result := False;
  if AnsiContainsSTR(GetCFGDataStr(5), LAddress) then Result := True;
end;

// Verify a transfer
function ValidateTrfr(order: TOrderData; Origen: String): Integer;
begin
  Result := 0;
  if GetAddressBalanceIndexed(Origen) - GetAddressPendingPays(Origen) <
    Order.AmountFee + order.AmountTransferred then
    Result := 1
  else if TransactionAlreadyPending(order.TransferID) then
    Result := 2
  else if Order.TimeStamp < LastBlockData.TimeStart then
    Result := 3
  else if Order.TimeStamp > LastBlockData.TimeEnd + 600 then
    Result := 4
  else if TransferExistsInLastBlock(Order.TransferID) then
    Result := 5
  else if not VerifySignedString(IntToStr(order.TimeStamp) + origen +
    order.Receiver + IntToStr(order.AmountTransferred) + IntToStr(order.AmountFee) +
    IntToStr(order.TransferLine), Order.Signature, Order.Sender) then
    Result := 6
  else if Order.AmountTransferred < 0 then
    Result := 7
  else if Order.AmountFee < 0 then
    Result := 8
  else if not IsValidHashAddress(Origen) then
    Result := 9
  else if ((order.OrderType = 'TRFR') and (not IsValidHashAddress(Order.Receiver))) then
    Result := 10
  else if IsAddressLocked(Order.Address) then
    Result := 11
  else if ((AnsiContainsStr(GetCFGDataStr(0), 'EMPTY')) or
    (AnsiContainsStr(GetCFGDataStr(0), 'STOP'))) then
    Result := 12
  else if origen <> Order.Address then
    Result := 13
  else
    Result := 0;
end;

function IsOrderIDAlreadyProcessed(OrderText: String): Boolean;
var
  OrderID: String;
  counter: Integer;
begin
  Result := False;
  OrderId := GetParameter(OrderText, 7);
  EnterCriticalSection(IdsProcessedLock);
  if length(ProcessedOrderIDs) > 0 then
  begin
    for counter := 0 to length(ProcessedOrderIDs) - 1 do
    begin
      if ProcessedOrderIDs[counter] = OrderID then
      begin
        Result := True;
        break;
      end;
    end;
  end;
  if Result = False then Insert(OrderID, ProcessedOrderIDs, length(
      ProcessedOrderIDs));
  LeaveCriticalSection(IdsProcessedLock);
end;

procedure INC_PTC_Order(TextLine: String; connection: Integer);
begin
  if not IsOrderIDAlreadyProcessed(TextLine) then
    AddCriptoOp(5, TextLine, '');
end;

function PTC_Order(TextLine: String): String;
var
  NumTransfers: Integer;
  TrxArray: array of TOrderData;
  senderTrx: array of String;
  cont: Integer;
  Textbak: String;
  sendersString: String = '';
  TodoValido: Boolean = True;
  Proceder: Boolean = True;
  ErrorCode: Integer = -1;
  TotalSent: Int64 = 0;
  TotalFee: Int64 = 0;
  RecOrderID: String = '';
  GenOrderID: String = '';
begin
  Result := '';
  try
    NumTransfers := StrToInt(GetParameter(TextLine, 5));
    ToLog('events', format('Order with %d transfers', [Numtransfers]));
    ToLog('events', format('Complete line: %s', [TextLine]));
    RecOrderId := GetParameter(TextLine, 7);
    GenOrderID := GetParameter(TextLine, 5) + GetParameter(TextLine, 10);
    Textbak := GetOpData(TextLine);
    if NumTransfers > 30 then
    begin
      Proceder := False;
      ErrorCode := 89;
    end;
    SetLength(TrxArray, 0);
    SetLength(senderTrx, 0);
    for cont := 0 to NumTransfers - 1 do
    begin
      if not Proceder then Break;
      SetLength(TrxArray, length(TrxArray) + 1);
      SetLength(senderTrx, length(senderTrx) + 1);
      if not GetOrderFromString(Textbak, TrxArray[cont]) then
      begin
        Proceder := False;
        ErrorCode := 96;
      end;
      Inc(TotalSent, TrxArray[cont].AmountTransferred);
      Inc(TotalFee, TrxArray[cont].AmountFee);
      GenOrderID := GenOrderID + TrxArray[cont].TransferID;
      if TransactionAlreadyPending(TrxArray[cont].TransferID) then
      begin
        Proceder := False;
        ErrorCode := 98;
      end;
      senderTrx[cont] := GetAddressFromPublicKey(TrxArray[cont].Sender);
      if senderTrx[cont] <> TrxArray[cont].Address then
      begin
        proceder := False;
        ErrorCode := 97;
        //ToLog('console',format('error: %s <> %s',[senderTrx[cont],TrxArray[cont].Address ]))
      end;
      if AnsiContainsstr(sendersString, senderTrx[cont]) then
      begin
        Proceder := False; // sender duplicated
        ErrorCode := 99;
      end;
      sendersString := sendersString + senderTrx[cont];
      Textbak := copy(textBak, 2, length(textbak));
      Textbak := GetOpData(Textbak);
    end;
    GenOrderID := GetOrderHash(GenOrderID);
    if TotalFee < GetMinimumFeeForAmount(TotalSent) then
    begin
      TodoValido := False;
      ErrorCode := 100;
    end;
    if RecOrderId <> GenOrderID then
    begin
      TodoValido := False;
      ErrorCode := 101;
    end;
    if TodoValido then
    begin
      for cont := 0 to NumTransfers - 1 do
      begin
        ErrorCode := ValidateTrfr(TrxArray[cont], senderTrx[cont]);
        if ErrorCode > 0 then
        begin
          TodoValido := False;
          break;
        end;
      end;
    end;
    if not todovalido then Proceder := False;
    if proceder then
    begin
      Textbak := GetOpData(TextLine);
      Textbak := GetProtocolHeader + 'ORDER ' + IntToStr(NumTransfers) + ' ' + Textbak;
      for cont := 0 to NumTransfers - 1 do
        AddTransactionToPool(TrxArray[cont]);
      if form1.Server.Active then OutgoingMsjsAdd(Textbak);
      UpdateDirPanel := True;
      Result := GetParameter(Textbak, 7); // send order ID as result
    end
    else
    begin
      if ErrorCode > 0 then Result := 'ERROR ' + ErrorCode.ToString;
    end;
  except
    ON E: Exception do
    begin
      ToLog('Console', '****************************************' + slinebreak +
        'PTC_Order:' + E.Message);
    end;
  end; {TRY}
end;

procedure INC_PTC_SendGVT(TextLine: String; connection: Integer);
begin
  if not IsOrderIDAlreadyProcessed(TextLine) then
    AddCriptoOp(7, TextLine, '');
end;

function PTC_SendGVT(TextLine: String): Integer;
var
  OrderInfo: TOrderData;
  Address: String = '';
  OpData: String = '';
  ErrorCode: Integer = 0;
  StrTosign: String = '';
begin
  if not GetOrderFromString(TextLine, OrderInfo) then exit;
  Address := GetAddressFromPublicKey(OrderInfo.Sender);
  if address <> OrderInfo.Address then ErrorCode := 1;
  // La direccion no dispone de fondos
  if GetAddressBalanceIndexed(Address) - GetAddressPendingPays(Address) <
    GetCustomFee(LastBlockIndex) then ErrorCode := 2;
  if TransactionAlreadyPending(OrderInfo.TransferID) then ErrorCode := 3;
  if OrderInfo.TimeStamp < LastBlockData.TimeStart then ErrorCode := 4;
  if TransferExistsInLastBlock(OrderInfo.TransferID) then ErrorCode := 5;
  if GVTAlreadyTransfered(OrderInfo.Reference) then ErrorCode := 6;
  StrTosign := 'Transfer GVT ' + OrderInfo.Reference + ' ' + OrderInfo.Receiver +
    OrderInfo.TimeStamp.ToString;
  if not VerifySignedString(StrToSign, OrderInfo.Signature, OrderInfo.Sender) then
    ErrorCode := 7;
  if OrderInfo.Sender <> AdminPublicKey then ErrorCode := 8;
  if ErrorCode = 0 then
  begin
    OpData := GetOpData(TextLine); // remove trx header
    AddTransactionToPool(OrderInfo);
    if form1.Server.Active then OutgoingMsjsAdd(GetProtocolHeader + opdata);
  end;
  Result := ErrorCode;
  if ErrorCode > 0 then
    ToLog('events', TimeToStr(now) + 'SendGVT error: ' + ErrorCode.ToString);
end;

procedure PTC_AdminMSG(TextLine: String);
const
  MsgsReceived: String = '';
var
  msgtime, mensaje, firma, hashmsg: String;
  msgtoshow: String = '';
  contador: Integer = 1;
  errored: Boolean = False;
  TCommand: String;
  TParam: String;

  procedure LaunchDirectiveThread(LParameter: String);
  var
    ThDirect: TDirectiveThread;
  begin
    if not EnableAutoUpdate then exit;
    ThDirect := TDirectiveThread.Create(True, LParameter);
    ThDirect.FreeOnTerminate := True;
    ThDirect.Start;
    ToLog('events', TimeToStr(now) + Format('Directive: %s', [LParameter]));
  end;

begin
  msgtime := GetParameter(TextLine, 5);
  mensaje := GetParameter(TextLine, 6);
  firma := GetParameter(TextLine, 7);
  hashmsg := GetParameter(TextLine, 8);
  if AnsiContainsStr(MsgsReceived, hashmsg) then errored := True;
  mensaje := StringReplace(mensaje, '_', ' ', [rfReplaceAll, rfIgnoreCase]);
  if not VerifySignedString(msgtime + mensaje, firma, AdminPublicKey) then
  begin
    ToLog('events', TimeToStr(now) + 'Directive wrong sign');
    errored := True;
  end;
  if HashMD5String(msgtime + mensaje + firma) <> Hashmsg then
  begin
    ToLog('events', TimeToStr(now) + 'Directive wrong hash');
    errored := True;
  end;
  if not errored then
  begin
    MsgsReceived := MsgsReceived + hashmsg;
    TCommand := GetParameter(mensaje, 0);
    TParam := GetParameter(mensaje, 1);
    if UpperCase(TCommand) = 'UPDATE' then LaunchDirectiveThread('update ' + TParam);
    if UpperCase(TCommand) = 'RESTART' then LaunchDirectiveThread('restart');
    if UpperCase(TCommand) = 'SETMODE' then SetCFGData(TParam, 0);
    if UpperCase(TCommand) = 'ADDNODE' then
    begin
      AddCFGData(TParam, 1);
      PopulateNodeList;
      SetNodeList(GetCFGDataStr(1));
    end;
    if UpperCase(TCommand) = 'DELNODE' then
    begin
      RemoveCFGData(TParam, 1);
      PopulateNodeList;
      SetNodeList(GetCFGDataStr(1));
    end;
    if UpperCase(TCommand) = 'ADDNTP' then AddCFGData(TParam, 2);
    if UpperCase(TCommand) = 'DELNTP' then RemoveCFGData(TParam, 2);
   {
   if UpperCase(TCommand) = 'ADDPOOLADDRESS' then AddCFGData(TParam,3);
   if UpperCase(TCommand) = 'DELPOOLADDRESS' then RemoveCFGData(TParam,3);
   if UpperCase(TCommand) = 'ADDPOOLHOST' then AddCFGData(TParam,4);
   if UpperCase(TCommand) = 'DELPOOLHOST' then RemoveCFGData(TParam,4);
   }
    if UpperCase(TCommand) = 'ADDLOCKED' then AddCFGData(TParam, 5);
    if UpperCase(TCommand) = 'DELLOCKED' then RemoveCFGData(TParam, 5);
    if UpperCase(TCommand) = 'ADDNOSOPAY' then AddCFGData(TParam, 6);
    if UpperCase(TCommand) = 'DELNOSOPAY' then RemoveCFGData(TParam, 6);
    if UpperCase(TCommand) = 'CLEARCFG' then ClearCFGData(TParam);
    if UpperCase(TCommand) = 'RESTORECFG' then RestoreCFGData;
    OutgoingMsjsAdd(TextLine);
  end;
end;

procedure PTC_CFGData(Linea: String);
var
  startpos: Integer;
  content: String;
begin
  startpos := Pos('$', Linea);
  Content := Copy(Linea, Startpos + 1, Length(linea));
  if Copy(HAshMD5String(Content), 0, 5) = GetConsensusData(19) then
  begin
    SaveCFGToFile(content);
    SetCFGDataStr(content);
    PopulateNodeList;
    ToLog('events', 'Noso CFG updated!');
  end
  else
    ToLog('events', Format('Failed CFG: %s <> %s',
      [Copy(HAshMD5String(Content), 0, 5), GetConsensusData(19)]));
end;

procedure PTC_SendUpdateHeaders(Slot: Integer; Linea: String);
const
  LastRequest: Int64 = 0;
var
  Block: Integer;
begin
  if UTCTime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  Block := StrToIntDef(GetParameter(Linea, 5), 0);
  PTC_SendLine(slot, GetProtocolLineFromCode(headupdate) + ' $' + GetHeadersAsStringFromBlock(Block));
end;

procedure PTC_ProcessMNFileIncoming(LText: String);
var
  MNText: String;
begin
  MNText := ExtractMasternodesText(LText);
  if copy(HashMD5String(MNText + #13#10), 1, 5) = GetConsensusData(8) then
  begin
    //ToLog('console','Received MNs hash match!');
    SaveMNsFile(MNText);
    PopulateNodeList;
  end
  else
  begin
    //ToLog('console',' Wrong MN hash received');
    //ToLog('console',MNText);
  end;
end;

// This function must go to NosoHeaders
procedure PTC_HeadUpdate(linea: String);
var
  startpos: Integer;
  content: String;
  ThisHeader, blockhash, sumhash: String;
  Counter: Integer = 0;
  Numero: Integer;
  LastBlockOnSummary: Integer;
  TotalErrors: Integer = 0;
  TotalReceived: Integer = 0;
begin
  if GetSummaryFileHash = GetConsensusData(5) then exit;
  startpos := Pos('$', Linea);
  Content := Copy(Linea, Startpos + 1, Length(linea));
  //ToLog('console','Content: '+Linea);
  repeat
    ThisHeader := GetParameter(Content, counter);
    if thisheader <> '' then
    begin
      Inc(TotalReceived);
      ThisHeader := StringReplace(ThisHeader, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
      Numero := StrToIntDef(GetParameter(ThisHeader, 0), 0);
      blockhash := GetParameter(ThisHeader, 1);
      sumhash := GetParameter(ThisHeader, 2);
      LastBlockOnSummary := GetLastHeaderBlock();
      if numero = LastBlockOnSummary + 1 then
        AddRecordToHeaders(Numero, blockhash, sumhash)
      else
      begin
        Inc(TotalErrors);
      end;
    end;
    Inc(counter);
  until ThisHeader = '';
  SetSummaryFileHash;
  if copy(GetSummaryFileHash, 0, 5) <> GetConsensusData(5) then
  begin
    ForceHeadersDownload := True;
    ToLog('Console', Format('Update headers failed (%d) : %s <> %s',
      [TotalErrors, Copy(GetSummaryFileHash, 0, 5), GetConsensusData(5)]));
  end
  else
  begin
    ToLog('Console', 'Headers Updated!');
    ForceHeadersDownload := False;
  end;
end;

// Añade una operacion a la espera de cripto
procedure AddCriptoOp(tipo: Integer; proceso, resultado: String);
var
  NewOp: TCryptoOperation;
begin
  NewOp.OperationType := tipo;
  NewOp.Data := proceso;
  NewOp.Result := resultado;
  EnterCriticalSection(CryptoThreadLock);
  try
    Insert(NewOp, ArrayCriptoOp, length(ArrayCriptoOp));

  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error adding Operation to crypto thread:' + proceso);
  end{Try};
  LeaveCriticalSection(CryptoThreadLock);
end;

// Elimina la operacion cripto
procedure DeleteCriptoOp();
begin
  EnterCriticalSection(CryptoThreadLock);
  if Length(ArrayCriptoOp) > 0 then
  begin
    try
      Delete(ArrayCriptoOp, 0, 1);
    except
      ON E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
          ' -> ' + 'Error removing Operation from crypto thread:' + E.Message);
      end;
    end{Try};
  end;
  LeaveCriticalSection(CryptoThreadLock);
end;


end. // END UNIT
