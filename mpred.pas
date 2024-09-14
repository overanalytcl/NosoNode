unit mpRed;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, SysUtils, MasterPaskalForm, nosotime, IdContext,
  IdGlobal, mpGUI, mpDisk,
  mpBlock, fileutil, Graphics, Dialogs, strutils, mpcoin, fphttpclient,
  opensslsockets, translation, IdHTTP, IdComponent, IdSSLOpenSSL, IdTCPClient,
  nosodebug, nosogeneral, nosocrypto, nosounit, nosoconsensus, nosopsos, nosowallcon,
  nosoheaders, nosoblock, nosonosocfg, nosonetwork, nosogvts, nosomasternodes;

function GetSlotFromIP(Ip: String): Int64;
function GetSlotFromContext(Context: TidContext): Int64;
 //function BotExists(IPUser:String):Boolean;
 //function NodeExists(IPUser,Port:String):integer;
function SaveConection(tipo, ipuser: String; contextdata: TIdContext;
  toSlot: Integer = -1): Integer;
procedure ForceServer();
procedure StartServer();
function StopServer(): Boolean;
//procedure CloseSlot(Slot:integer);
function IsSlotFree(number: Integer): Boolean;
//Function IsSlotConnected(number:integer):Boolean;
function GetFreeSlot(): Integer;
function ReserveSlot(): Integer;
function ConnectClient(Address, Port: String): Integer;
 //function GetTotalConexiones():integer;
 //function GetTotalVerifiedConnections():Integer;
function GetTotalSyncedConnections(): Integer;
function CerrarClientes(ServerToo: Boolean = True): String;
procedure LeerLineasDeClientes();
procedure VerifyConnectionStatus();
 //Function IsAllSynced():integer;
 //Procedure UpdateMyData();
procedure SyncWithMainnet();
function GetOutGoingConnections(): Integer;
function GetIncomingConnections(): Integer;
function GetSeedConnections(): Integer;
function GetValidSlotForSeed(out Slot: Integer): Boolean;
function GetOrderDetails(orderid: String): TOrderGroup;
function GetOrderSources(orderid: String): String;
function GetNodeStatusString(): String;
function IsSafeIP(IP: String): Boolean;
function GetLastRelease(): String;
function GetRepoFile(LurL: String): String;
function GetOS(): String;
function GetLastVerZipFile(version, LocalOS: String): Boolean;
//Function GetSyncTus():String;
function GetMiIP(): String;
function NodeServerInfo(): String;
procedure ClearReceivedOrdersIDs();
function SendOrderToNode(OrderString: String): String;

implementation

uses
  mpParser, mpProtocol;

// RETURNS THE SLOT OF THE GIVEN IP
function GetSlotFromIP(Ip: String): Int64;
var
  contador: Integer;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if GetConexIndex(contador).ip = ip then
    begin
      Result := contador;
      break;
    end;
  end;
end;

// RETURNS THE SLOT OF THE GIVEN CONTEXT
function GetSlotFromContext(Context: TidContext): Int64;
var
  contador: Integer;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if GetConexIndex(contador).context = Context then
    begin
      Result := contador;
      break;
    end;
  end;
end;

{
// Devuelve si un bot existe o no en la base de datos
function BotExists(IPUser:String):Boolean;
var
  contador : integer = 0;
Begin
Result := false;
for contador := 0 to length(ListadoBots)-1 do
   if ListadoBots[contador].ip = IPUser then result := true;
End;
}

{
// Devuelve si un Nodo existe o no en la base de datos
function NodeExists(IPUser,Port:String):integer;
var
  contador : integer = 0;
Begin
  Result := -1;
  for contador := 0 to length(ListadoBots)-1 do
    if ((ListaNodos[contador].ip = IPUser) and (ListaNodos[contador].port = port)) then result := contador;
End;
}
// Almacena una conexion con sus datos en el array Conexiones
function SaveConection(tipo, ipuser: String; contextdata: TIdContext;
  toSlot: Integer = -1): Integer;
var
  counter: Integer = 1;
  Slot: Int64 = 0;
  FoundSlot: Boolean = False;
  NewValue: Tconectiondata;
begin
  NewValue := Default(Tconectiondata);
  NewValue.Autentic := False;
  NewValue.Connections := 0;
  NewValue.tipo := tipo;
  NewValue.ip := ipuser;
  NewValue.lastping := UTCTimeStr;
  NewValue.context := contextdata;
  NewValue.Lastblock := '0';
  NewValue.LastblockHash := '';
  NewValue.SumarioHash := '';
  NewValue.ListeningPort := -1;
  NewValue.Pending := 0;
  NewValue.ResumenHash := '';
  NewValue.ConexStatus := 0;
  if ToSLot < 0 then
  begin
    for counter := 1 to MaxConecciones do
    begin
      if GetConexIndex(counter).tipo = '' then
      begin
        SetConexIndex(counter, NewValue);
        ClearIncoming(counter);
        FoundSlot := True;
        Result := counter;
        break;
      end;
    end;
    if not FoundSlot then Result := 0;
  end
  else
  begin
    SetConexIndex(toSlot, NewValue);
    ClearIncoming(ToSLot);
    Result := ToSLot;
  end;
end;

procedure ForceServer();
var
  PortNumber: Integer;
begin
  KeepServerOn := True;
  PortNumber := StrToIntDef(LocalMN_Port, 8080);
  if Form1.Server.Active then
  begin
    ToLog('console', 'Server Already active'); //'Server Already active'
  end
  else
  begin
    try
      LastTryServerOn := UTCTime;
      Form1.Server.Bindings.Clear;
      Form1.Server.DefaultPort := PortNumber;
      Form1.Server.Active := True;
      ToLog('console', 'Server ENABLED. Listening on port ' + PortNumber.ToString);
      //Server ENABLED. Listening on port
      ServerStartTime := UTCTime;
      U_DataPanel := True;
    except
      on E: Exception do
        ToLog('events', TimeToStr(now) + 'Unable to start Server');
         //Unable to start Server
    end; {TRY}
  end;
end;

// Activa el servidor
procedure StartServer();
var
  PortNumber: Integer;
begin
  PortNumber := StrToIntDef(LocalMN_Port, 8080);
  if WallAddIndex(LocalMN_Sign) < 0 then
  begin
    ToLog('console', rs2000); //Sign address not valid
    exit;
  end;
  if MyConStatus < 3 then // rs2001 = 'Wallet not updated';
  begin
    ToLog('console', rs2001);
    exit;
  end;
  KeepServerOn := True;
  if Form1.Server.Active then
  begin
    ToLog('console', 'Server Already active'); //'Server Already active'
  end
  else
  begin
    try
      LastTryServerOn := UTCTime;
      Form1.Server.Bindings.Clear;
      Form1.Server.DefaultPort := PortNumber;
      Form1.Server.Active := True;
      ToLog('console', 'Server ENABLED. Listening on port ' + PortNumber.ToString);
      //Server ENABLED. Listening on port
      ServerStartTime := UTCTime;
      U_DataPanel := True;
    except
      on E: Exception do
        ToLog('events', TimeToStr(now) + 'Unable to start Server: ' + e.Message);
      //Unable to start Server
    end;
  end;
end;

// Apaga el servidor
function StopServer(): Boolean;
var
  Contador: Integer;
begin
  Result := True;
  if not Form1.Server.Active then exit;
  KeepServerOn := False;
  try
    Form1.Server.Active := False;
    U_DataPanel := True;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end{Try};
end;

{
// Cierra la conexion del slot especificado
Procedure CloseSlot(Slot:integer);
Begin
  BeginPerformance('CloseSlot');
  TRY
  if GetConexIndex(Slot).tipo='CLI' then
    begin
    ClearIncoming(slot);
    GetConexIndex(Slot).context.Connection.Disconnect;
    Sleep(10);
    //Conexiones[Slot].Thread.terminate; // free ? WaitFor??
    end;
  if GetConexIndex(Slot).tipo='SER' then
    begin
    ClearIncoming(slot);
    CanalCliente[Slot].IOHandler.InputBuffer.Clear;
    CanalCliente[Slot].Disconnect;
    end;
  EXCEPT on E:Exception do
    ToLog('exceps',FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now)+' -> '+'Error: Closing slot '+IntToStr(Slot)+SLINEBREAK+E.Message);
  END;{Try}
  SetConexIndex(Slot,Default(Tconectiondata));
  EndPerformance('CloseSlot');
End;
}

function IsSlotFree(number: Integer): Boolean;
begin
  Result := True;
  if GetConexIndex(number).tipo <> '' then Result := False;
end;

{
Function IsSlotConnected(number:integer):Boolean;
Begin
  result := false;
  if ((GetConexIndex(number).tipo = 'SER') or (GetConexIndex(number).tipo = 'CLI')) then result := true;
End;
}

// Returns first available slot
function GetFreeSlot(): Integer;
var
  contador: Integer = 1;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if IsSlotFree(Contador) then
    begin
      Result := contador;
      break;
    end;
  end;
end;

// Reserves the first available slot
function ReserveSlot(): Integer;
var
  contador: Integer = 1;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if IsSlotFree(Contador) then
    begin
      SetConexReserved(contador, True);
      Result := contador;
      break;
    end;
  end;
end;

// Reserves the first available slot
procedure UnReserveSlot(number: Integer);
begin
  if GetConexIndex(number).tipo = 'RES' then
  begin
    SetConexReserved(number, False);
    CloseSlot(Number);
  end
  else
  begin
    ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
      ' -> ' + 'Error un-reserving slot ' + number.ToString);
  end;
end;

// Connects a client and returns the slot
function ConnectClient(Address, Port: String): Integer;
var
  Slot: Integer = 0;
  ConContext: TIdContext; // EMPTY
  Errored: Boolean = False;
  SavedSlot: Integer;
  ConnectOk: Boolean = False;
begin
  Result := 0;
  ConContext := Default(TIdContext);
  Slot := ReserveSlot();
  if Slot = 0 then exit;  // No free slots
  CanalCliente[Slot].Host := Address;
  CanalCliente[Slot].Port := StrToIntDef(Port, 8080);
  CanalCliente[Slot].ConnectTimeout := 1000;
  ClearOutTextToSlot(slot);
  try
    CanalCliente[Slot].Connect;
    ConnectOk := True;
  except
    on E: Exception do
    begin
      ConnectOk := False;
    end;
  end;{TRY}
  if connectok then
  begin
    SavedSlot := SaveConection('SER', Address, ConContext, slot);
    ToLog('events', TimeToStr(now) + 'Connected TO: ' + Address);
    //Connected TO:
    StartConexThread(Slot);
    Result := Slot;
    try
      CanalCliente[Slot].IOHandler.WriteLn('PSK ' + Address + ' ' +
        MainnetVersion + NodeRelease + ' ' + UTCTimeStr);
      CanalCliente[Slot].IOHandler.WriteLn(ProtocolLine(3));   // Send PING
    except
      on E: Exception do
      begin
        Result := 0;
        CloseSlot(slot);
      end;
    end;{TRY}
  end
  else
  begin
    Result := 0;
    UnReserveSlot(Slot);
  end;
end;

{
// Retuns the number of active peers connections
function GetTotalConexiones():integer;
var
  counter:integer;
Begin
  BeginPerformance('GetTotalConexiones');
  result := 0;
  for counter := 1 to MaxConecciones do
    if IsSlotConnected(Counter) then result := result + 1;
  EndPerformance('GetTotalConexiones');
End;
}

{
function GetTotalVerifiedConnections():Integer;
var
  counter:integer;
Begin
result := 0;
for counter := 1 to MaxConecciones do
   if conexiones[Counter].Autentic then result := result + 1;
End;
}

function GetTotalSyncedConnections(): Integer;
var
  counter: Integer;
begin
  Result := 0;
  for counter := 1 to MaxConecciones do
    if GetConexIndex(Counter).MerkleHash = GetCOnsensus(0) then Result := Result + 1;
end;

// Close all outgoing connections
function CerrarClientes(ServerToo: Boolean = True): String;
var
  Contador: Integer;
begin
  Result := '';
  try
    for contador := 1 to MaxConecciones do
    begin
      if GetConexIndex(contador).tipo = 'SER' then CloseSlot(contador);
    end;
    Result := 'Clients connections closed'
  except
    on E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error closing client');
      Result := 'Error closing clients';
    end;
  end; {TRY}
  if ServerToo then
  begin
    if form1.Server.active then StopServer;
  end;
end;

// This needs to be included on peers threads
procedure LeerLineasDeClientes();
var
  contador: Integer = 0;
begin
  for contador := 1 to Maxconecciones do
  begin
    if IsSlotConnected(contador) then
    begin
      if ((UTCTime > StrToInt64Def(GetConexIndex(contador).lastping, 0) + 15) and
        (not GetConexIndex(contador).IsBusy) and (not REbuildingSumary)) then
      begin
        ToLog('events', TimeToStr(now) + 'Conection closed: Time Out Auth -> ' +
          GetConexIndex(contador).ip);   //Conection closed: Time Out Auth ->
        CloseSlot(contador);
      end;
      if GetConexIndex(contador).IsBusy then SetConexIndexLastPing(contador, UTCTimeStr);
    end;
  end;
end;

// Checks the current connection status (0-3)
procedure VerifyConnectionStatus();
var
  NumeroConexiones: Integer = 0;
  ValidSlot: Integer;
begin
  try
    NumeroConexiones := GetTotalConexiones;
    if NumeroConexiones = 0 then  // Desconectado
    begin
      EnterCriticalSection(CSCriptoThread);
      SetLength(ArrayCriptoOp, 0); // Delete operations from crypto thread
      LeaveCriticalSection(CSCriptoThread);
      EnterCriticalSection(CSIdsProcessed);
      Setlength(ArrayOrderIDsProcessed, 0); // clear processed Orders
      LeaveCriticalSection(CSIdsProcessed);
      ClearMNsChecks();
      ClearMNsList();
      MyConStatus := 0;
      if STATUS_Connected then
      begin
        STATUS_Connected := False;
        ToLog('console', 'Disconnected.');       //Disconnected
        G_TotalPings := 0;
        U_Datapanel := True;
        ClearAllPending; //THREADSAFE
      end;
      // Resetear todos los valores
    end;
    if ((NumeroConexiones > 0) and (NumeroConexiones < 3) and (MyConStatus = 0)) then
      // Conectando
    begin
      MyConStatus := 1;
      G_LastPing := UTCTime;
      ToLog('console', 'Connecting...'); //Connecting...
    end;
    if MyConStatus > 0 then
    begin
      if (G_LastPing + 5) < UTCTime then
      begin
        G_LastPing := UTCTime;
        OutgoingMsjsAdd(ProtocolLine(ping));
      end;
    end;
    if ((NumeroConexiones >= 3) and (MyConStatus < 2) and (not STATUS_Connected)) then
    begin
      STATUS_Connected := True;
      MyConStatus := 2;
      ToLog('console', 'Connected.');     //Connected
    end;
    if STATUS_Connected then
    begin
      //UpdateNetworkData();
      if Last_SyncWithMainnet + 4 < UTCTime then SyncWithMainnet();
    end;
    if ((MyConStatus = 2) and (STATUS_Connected) and
      (IntToStr(MyLastBlock) = Getconsensus(2)) and
      (copy(MySumarioHash, 0, 5) = GetConsensus(17)) and
      (copy(GetResumenHash, 0, 5) = GetConsensus(5))) then
    begin
      GetValidSlotForSeed(ValidSlot);
      ClearReceivedOrdersIDs;
      MyConStatus := 3;
      ToLog('console', 'Updated!');   //Updated!
      //if RPCAuto then  ProcessLinesAdd('RPCON');
      if WO_AutoServer then ProcessLinesAdd('serveron');
      if StrToIntDef(GetConsensus(3), 0) < GetPendingCount then
      begin
        setlength(ArrayPoolTXs, 0);
      end;
      // Get MNS
      PTC_SendLine(ValidSlot, ProtocolLine(11));  // Get MNs
      LastTimeMNsRequested := UTCTime;
      OutgoingMsjsAdd(ProtocolLine(ping));
    end;
    if MyConStatus = 3 then
    begin
      GetValidSlotForSeed(ValidSlot);
      if ((RPCAuto) and (not Form1.RPCServer.Active)) then  ProcessLinesAdd('RPCON');
      if ((not RPCAuto) and (Form1.RPCServer.Active)) then  ProcessLinesAdd('RPCOFF');
      if ((StrToIntDef(GetConsensus(3), 0) > GetPendingCount) and
        (LastTimePendingRequested + 5 < UTCTime) and (length(ArrayCriptoOp) = 0)) then
      begin
        ClearReceivedOrdersIDs();
        PTC_SendLine(ValidSlot, ProtocolLine(5));  // Get pending
        LastTimePendingRequested := UTCTime;
      end;
      if GetAddressBalanceIndexed(LocalMN_Funds) < GetStackRequired(MyLastBlock) then
        LastTimeReportMyMN := NextBlockTimeStamp + 5;
      if ((not IsMyMNListed(LocalMN_IP)) and (Form1.Server.Active) and
        (UTCTime > LastTimeReportMyMN + 5) and (BlockAge > 10 + MNsRandomWait) and
        (BlockAge < 495) and (1 = 1)) then
      begin
        OutGoingMsjsAdd(ProtocolLine(MNReport));
        ToLog('events', TimeToStr(now) + 'My Masternode reported');
        LastTimeReportMyMN := UTCTime;
      end;
    end;
  except
    ON E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + format(rs2002, [E.Message]));
    end;
  end{Try};
end;

{
Function IsAllSynced():integer;
Begin
result := 0;
if MyLastBlock     <> StrToIntDef(GetConsensus(cLastBlock),0) then result := 1;
if MyLastBlockHash <> GetConsensus(cLBHash) then result := 2;
if Copy(MySumarioHash,0,5)   <> GetConsensus(cSumHash) then result := 3;
if Copy(GetResumenHash,0,5)   <> GetConsensus(cHeaders) then result := 4;
{
if Copy(GetMNsHash,1,5) <>  NetMNsHash.value then result := 5;
if MyGVTsHash <> NetGVTSHash.Value then result := 6;
if MyCFGHash <> NETCFGHash.Value then result := 7;
}
End;
}
{
// Actualiza mi informacion para compoartirla en la red
Procedure UpdateMyData();
Begin
MySumarioHash := HashMD5File(SummaryFileName);
MyLastBlockHash := HashMD5File(BlockDirectory+IntToStr(MyLastBlock)+'.blk');
LastBlockData := LoadBlockDataHeader(MyLastBlock);
SetResumenHash := HashMD5File(ResumenFilename);
  if SetResumenHash = GetConsensus(5) then ForceCompleteHeadersDownload := false;
MyMNsHash     := HashMD5File(MasterNodesFilename);
MyCFGHash     := Copy(HashMD5String(GetCFGDataStr),1,5);
End;
}



// Request necessary files/info to update
procedure SyncWithMainnet();
var
  NLBV: Integer = 0; // network last block value
  LastDownBlock: Integer = 0;
  ValidSlot: Integer;
  ConsenLB: Int64;
  Removed: Integer = 0;
begin
  if BuildingBlock > 0 then exit;
  if GetConsensus = '' then exit;
  if ((BlockAge < 10) or (blockAge > 595)) then exit;
  ConsenLB := StrToIntDef(GetConsensus(2), -1);
  if ((MyLastBlock > ConsenLB) and (ConsenLB >= 0)) then
  begin
    Removed := RemoveBlocks(ConsenLB);
    ToLog('console', format('%d blocks deleted', [Removed]));
    UndoneLastBlock;
    //RestartNoso;
    Exit;
  end;
  NLBV := StrToIntDef(GetConsensus(cLastBlock), 0);

  // *** New Synchronization methods

  // *** Update CFG file.
  if ((GetConsensus(19) <> Copy(GetCFGHash, 0, 5)) and
    (LasTimeCFGRequest + 5 < UTCTime) and (GetConsensus(19) <> '')) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      PTC_SendLine(ValidSlot, ProtocolLine(GetCFG));
      LasTimeCFGRequest := UTCTime;
      ToLog('console', 'Noso CFG file requested');
    end;
  end;

  // *** Update MNs file
  if ((GetConsensus(8) <> Copy(GetMNsHash, 1, 5)) and
    (LastTimeMNHashRequestes + 5 < UTCTime) and (GetConsensus(8) <> '')) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      PTC_SendLine(ValidSlot, ProtocolLine(GetMNsFile));  // Get MNsFile
      LastTimeMNHashRequestes := UTCTime;
      ToLog('console', 'Mns File requested to ' + GetConexIndex(ValidSlot).ip);
    end;
  end;

  // *** update headers
  if Copy(GetResumenhash, 0, 5) <> GetConsensus(cHeaders) then  // Request headers
  begin
    ClearAllPending;
    ClearMNsChecks();
    ClearMNsList();
    if ((LastTimeRequestResumen + 10 < UTCTime) and (not DownloadHeaders)) then
    begin
      if ((NLBV - mylastblock >= 144) or (ForceCompleteHeadersDownload)) then
      begin
        if GetValidSlotForSeed(ValidSlot) then
        begin
          PTC_SendLine(ValidSlot, ProtocolLine(7)); // GetResumen
          ToLog('console', 'Headers file requested to ' + GetConexIndex(ValidSlot).ip);
          //'Headers file requested'
          LastTimeRequestResumen := UTCTime;
        end;
      end
      else // If less than 144 block just update headers
      begin
        if GetValidSlotForSeed(ValidSlot) then
        begin
          PTC_SendLine(ValidSlot, ProtocolLine(18)); // GetResumen
          ToLog('console', Format('Headers update (%d) requested from %s',
            [mylastblock, GetConexIndex(ValidSlot).ip]));
          LastTimeRequestResumen := UTCTime;
        end;
      end;
    end;
  end;

  // *** Update blocks
  if ((Copy(GetResumenhash, 0, 5) = GetConsensus(5)) and (mylastblock < NLBV)) then
    // request up to 100 blocks
  begin
    ClearAllPending;
    ClearMNsChecks();
    ClearMNsList();
    if ((LastTimeRequestBlock + 5 < UTCTime) and (not DownLoadBlocks)) then
    begin
      if GetValidSlotForSeed(ValidSlot) then
      begin
        PTC_SendLine(ValidSlot, ProtocolLine(8)); // lastblock
        if WO_FullNode then
          ToLog('console', 'LastBlock requested from block ' +
            IntToStr(mylastblock) + ' to ' + GetConexIndex(ValidSlot).ip)
        //'LastBlock requested from block '
        else
        begin
          LastDownBlock := NLBV - SecurityBlocks;
          if LastDownBlock < MyLastBlock then LastDownBlock := MyLastBlock;
          ToLog('console', 'LastBlock requested from block ' + IntToStr(LastDownBlock));
        end;
        LastTimeRequestBlock := UTCTime;
      end;
    end;
  end;

  // Update summary
  if ((copy(GetResumenhash, 0, 5) = GetConsensus(5)) and (mylastblock = NLBV) and
    (MySumarioHash <> GetConsensus(17)) {and (SummaryLastop < mylastblock)}) then
  begin  // complete or download summary
    if (SummaryLastop + (2 * SumMarkInterval) < mylastblock) then
    begin
      if ((LastTimeRequestsumary + 5 < UTCTime) and (not DownloadSumary)) then
      begin
        if GetValidSlotForSeed(ValidSlot) then
        begin
          PTC_SendLine(ValidSlot, ProtocolLine(6)); // Getsumary
          ToLog('console', rs2003); //'sumary file requested'
          LastTimeRequestsumary := UTCTime;
        end;
      end;
    end
    else
    begin
      CompleteSumary();
    end;
  end;

  // Update GVTs file
  if ((GetConsensus(18) <> Copy(MyGVTsHash, 0, 5)) and
    (LasTimeGVTsRequest + 5 < UTCTime) and (GetConsensus(18) <> '') and
    (not DownloadGVTs)) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      PTC_SendLine(ValidSlot, ProtocolLine(GetGVTs));
      LasTimeGVTsRequest := UTCTime;
      ToLog('console', 'GVTs File requested to ' + GetConexIndex(ValidSlot).ip);
    end;
  end;

  // Update PSOs file
  if ((GetConsensus(20) <> Copy(PSOFileHash, 0, 5)) and
    (LasTimePSOsRequest + 5 < UTCTime) and (GetConsensus(20) <> '') and
    (not DownloadPSOs)) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      PTC_SendLine(ValidSlot, ProtocolLine(GetPSOs));
      LasTimePSOsRequest := UTCTime;
      ToLog('console', 'Requested PSOs to: ' + GetConexIndex(ValidSlot).ip);
    end;
  end;

  // *** Request reported MNs
  if ((StrToIntDef(GetConsensus(9), 0) > GetMNsListLength) and
    (LastTimeMNsRequested + 15 < UTCTime) and (LengthWaitingMNs = 0) and
    (BlockAge > 30) and (IsAllSynced = 0)) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      ClearReceivedMNs();
      ClearMNIPProcessed;
      PTC_SendLine(ValidSlot, ProtocolLine(11));  // Get MNsList
      LastTimeMNsRequested := UTCTime;
      ToLog('console', 'MNs reports requested to ' + GetConexIndex(ValidSlot).ip);
    end;
  end;

  // *** Request MNs verifications
  if ((StrToIntDef(GetConsensus(14), 0) > GetMNsChecksCount) and
    (LastTimeChecksRequested + 5 < UTCTime) and (IsAllSynced = 0)) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      PTC_SendLine(ValidSlot, ProtocolLine(GetChecks));  // Get MNsChecks
      LastTimeChecksRequested := UTCTime;
      ToLog('console', 'Checks requested to ' + GetConexIndex(ValidSlot).ip);
    end;
  end;

  // Blockchain status issues starts here
  if ((copy(GetResumenhash, 0, 5) = GetConsensus(5)) and (mylastblock = NLBV) and
    (copy(MySumarioHash, 0, 5) <> GetConsensus(17)) and
    (SummaryLastop = mylastblock) and (LastTimeRequestsumary + 5 < UTCTime)) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      ToLog('console', format('%s <> %s', [copy(MySumarioHash, 0, 5),
        GetConsensus(17)]));
      PTC_SendLine(ValidSlot, ProtocolLine(6)); // Getsumary
      ToLog('console', rs2003); //'sumary file requested'
      LastTimeRequestsumary := UTCTime;
    end;
  end
  else if ((mylastblock = NLBV) and ((copy(GetResumenhash, 0, 5) <> GetConsensus(5)) or
    (MyLastBlockHash <> GetConsensus(10)))) then
  begin
    ToLog('console', MyLastBlockHash + ' ' + MyLastBlockHash);
    UndoneLastBlock();
  end
  // Update headers
  else if ((copy(GetResumenhash, 0, 5) <> GetConsensus(5)) and
    (NLBV = mylastblock) and (MyLastBlockHash = GetConsensus(10)) and
    (copy(MySumarioHash, 0, 5) = GetConsensus(17)) and (not DownloadHeaders)) then
  begin
    if GetValidSlotForSeed(ValidSlot) then
    begin
      ClearAllPending;
      PTC_SendLine(ValidSlot, ProtocolLine(7));
      ToLog('console', 'Headers file requested');
      LastTimeRequestResumen := UTCTime;
    end;
  end;

  if IsAllSynced = 0 then Last_SyncWithMainnet := UTCTime;
end;

function GetOutGoingConnections(): Integer;
var
  contador: Integer;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if GetConexIndex(contador).tipo = 'SER' then
      Inc(Result);
  end;
end;

function GetIncomingConnections(): Integer;
var
  contador: Integer;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if GetConexIndex(contador).tipo = 'CLI' then
      Inc(Result);
  end;
end;

function GetValidSlotForSeed(out Slot: Integer): Boolean;
const
  SlotCount: Integer = 0;
var
  counter: Integer;
begin
  Result := False;
  for counter := 1 to MaxConecciones do
  begin
    Inc(SlotCount);
    if SlotCount > MaxConecciones then SlotCount := 1;
    if ((GetConexIndex(SlotCount).MerkleHash = GetConsensus(0))) then
    begin
      Result := True;
      slot := SlotCount;
      break;
    end;
  end;
end;

function GetSeedConnections(): Integer;
var
  contador: Integer;
begin
  Result := 0;
  for contador := 1 to MaxConecciones do
  begin
    if IsSeedNode(GetConexIndex(contador).ip) then
      Inc(Result);
  end;
end;

function GetOrderDetails(orderid: String): TOrderGroup;
var
  counter, counter2: Integer;
  orderfound: Boolean = False;
  resultorder: TOrderGroup;
  ArrTrxs: TBlockOrdersArray;
  LastBlockToCheck: Integer = 0;
  FirstBlockToCheck: Integer;
  TryonIndex: Integer = -1;
  CopyPendings: array of Torderdata;
begin
  BeginPerformance('GetOrderDetails');
  resultorder := default(TOrderGroup);
  Result := resultorder;
  if GetPendingCount > 0 then
  begin
    EnterCriticalSection(CSPending);
    SetLength(CopyPendings, 0);
    CopyPendings := copy(ArrayPoolTXs, 0, length(ArrayPoolTXs));
    LeaveCriticalSection(CSPending);
    for counter := 0 to length(CopyPendings) - 1 do
    begin
      if CopyPendings[counter].OrderID = orderid then
      begin
        resultorder.OrderID := CopyPendings[counter].OrderID;
        resultorder.Block := -1;
        resultorder.reference := CopyPendings[counter].reference;
        resultorder.TimeStamp := CopyPendings[counter].TimeStamp;
        resultorder.receiver := CopyPendings[counter].receiver;
        if CopyPendings[counter].OrderLines = 1 then
          resultorder.Sender := CopyPendings[counter].address
        else
          resultorder.Sender :=
            resultorder.Sender + format('[%s,%d,%d]',
            [CopyPendings[counter].Address, CopyPendings[counter].AmmountTrf,
            CopyPendings[counter].AmmountFee]);
        resultorder.AmmountTrf :=
          resultorder.AmmountTrf + CopyPendings[counter].AmmountTrf;
        resultorder.AmmountFee :=
          resultorder.AmmountFee + CopyPendings[counter].AmmountFee;
        resultorder.OrderLines += 1;
        resultorder.OrderType := CopyPendings[counter].OrderType;
        orderfound := True;
      end;
    end;
  end;
  if orderfound then Result := resultorder
  else
  begin
    TryonIndex := GetBlockFromOrder(orderid);
    if TryonIndex >= 0 then
    begin
      ToLog('console', 'Order found on index: ' + TryOnIndex.ToString());
      ArrTrxs := GetBlockTrxs(TryonIndex);
      if length(ArrTrxs) > 0 then
      begin
        for counter2 := 0 to length(ArrTrxs) - 1 do
        begin
          if ArrTrxs[counter2].OrderID = orderid then
          begin
            resultorder.OrderID := ArrTrxs[counter2].OrderID;
            resultorder.Block := ArrTrxs[counter2].Block;
            resultorder.reference := ArrTrxs[counter2].reference;
            resultorder.TimeStamp := ArrTrxs[counter2].TimeStamp;
            resultorder.receiver := ArrTrxs[counter2].receiver;
            if ArrTrxs[counter2].OrderLines = 1 then
              resultorder.Sender := ArrTrxs[counter2].Sender
            else
              resultorder.Sender :=
                resultorder.Sender + format('[%s,%d,%d]',
                [ArrTrxs[counter2].Address, ArrTrxs[counter2].AmmountTrf,
                ArrTrxs[counter2].AmmountFee]);
            resultorder.AmmountTrf :=
              resultorder.AmmountTrf + ArrTrxs[counter2].AmmountTrf;
            resultorder.AmmountFee :=
              resultorder.AmmountFee + ArrTrxs[counter2].AmmountFee;
            resultorder.OrderLines += 1;
            resultorder.OrderType := ArrTrxs[counter2].OrderType;
            orderfound := True;
          end;
        end;
      end;
      SetLength(ArrTrxs, 0);
    end
    else
    begin
      FirstBlockToCheck := mylastblock;
      ToLog('console', 'Order not on index');
    end;
  end;
  Result := resultorder;
  EndPerformance('GetOrderDetails');
end;

function GetOrderSources(orderid: String): String;
var
  LastBlockToCheck: Integer;
  Counter: Integer;
  counter2: Integer;
  ArrTrxs: TBlockOrdersArray;
  orderfound: Boolean = False;
begin
  Result := '';
  if WO_FullNode then LastBlockToCheck := 1
  else
    LastBlockToCheck := mylastblock - SecurityBlocks;
  if LastBlockToCheck < 1 then LastBlockToCheck := 1;
  for counter := mylastblock downto mylastblock - 4000 do
  begin
    ArrTrxs := GetBlockTrxs(counter);
    if length(ArrTrxs) > 0 then
    begin
      for counter2 := 0 to length(ArrTrxs) - 1 do
      begin
        if ArrTrxs[counter2].OrderID = orderid then
        begin
          Result := Result + Format('[%s,%d,%d]',
            [ArrTrxs[counter2].Sender, ArrTrxs[counter2].AmmountTrf,
            ArrTrxs[counter2].AmmountFee]);
          orderfound := True;
        end;
      end;
    end;
    if orderfound then break;
    SetLength(ArrTrxs, 0);
  end;
  if not orderfound then Result := 'Order Not Found';
end;

function GetNodeStatusString(): String;
begin
  if BuildingBlock > 0 then Result := '';
  //NODESTATUS 1{Peers} 2{LastBlock} 3{Pendings} 4{Delta} 5{headers} 6{version} 7{UTCTime} 8{MNsHash}
  //           9{MNscount} 10{LasBlockHash} 11{BestHashDiff} 12{LastBlockTimeEnd} 13{LBMiner}
  //           14{ChecksCount} 15{LastBlockPoW} 16{LastBlockDiff} 17{summary} 18{GVTs} 19{nosoCFG}
  //           20{PSOHash}
  Result := {1}IntToStr(GetTotalConexiones) + ' ' +{2}IntToStr(MyLastBlock) + ' ' +
    {3}GetPendingCount.ToString + ' ' +
    {4}IntToStr(UTCTime - EngineLastUpdate) + ' ' +{5}copy(GetResumenHash, 0, 5) + ' ' +
    {6}MainnetVersion + NodeRelease + ' ' +{7}UTCTimeStr + ' ' +{8}copy(
    GetMnsHash, 0, 5) + ' ' +{9}GetMNsListLength.ToString + ' ' +
    {10}MyLastBlockHash + ' ' +{11}{GetNMSData.Diff}'null' + ' ' +
    {12}IntToStr(LastBlockData.TimeEnd) + ' ' +
    {13}LastBlockData.AccountMiner + ' ' +{14}GetMNsChecksCount.ToString +
    ' ' +{15}Parameter(LastBlockData.Solution, 2) + ' ' +
    {16}Parameter(LastBlockData.Solution, 1) + ' ' +{17}copy(
    MySumarioHash, 0, 5) + ' ' +{18}copy(MyGVTsHash, 0, 5) + ' ' +
    {19}Copy(GetCFGHash, 0, 5) + ' ' +{20}copy(PSOFileHash, 0, 5);
end;

function IsSafeIP(IP: String): Boolean;
begin
  if Pos(IP, Parameter(GetCFGDataStr, 1)) > 0 then Result := True
  else
    Result := False;
end;

function GetLastRelease(): String;
var
  readedLine: String = '';
  Conector: TFPHttpClient;
begin
  Conector := TFPHttpClient.Create(nil);
  conector.ConnectTimeout := 1000;
  conector.IOTimeout := 1000;
  try
    readedLine := Conector.SimpleGet(
      'https://raw.githubusercontent.com/nosocoin/NosoNode/main/lastrelease.txt');
    // Binance API example
    //readedLine := Conector.SimpleGet('https://api.binance.com/api/v3/ticker/price?symbol=LTCUSDT');
  except
    on E: Exception do
    begin
      ToLog('console', 'ERROR RETRIEVING LAST RELEASE DATA: ' + E.Message);
    end;
  end;//TRY
  Conector.Free;
  Result := readedLine;
end;

function GetRepoFile(LurL: String): String;
var
  readedLine: String = '';
  Conector: TFPHttpClient;
begin
  Conector := TFPHttpClient.Create(nil);
  conector.ConnectTimeout := 1000;
  conector.IOTimeout := 1000;
  try
    readedLine := Conector.SimpleGet(LurL);
  except
    on E: Exception do
    begin
      ToDeepDeb('mpRed,GetRepoFile,' + E.Message);
    end;
  end;//TRY
  Conector.Free;
  Result := readedLine;
end;

// Retrieves the OS for download the lastest version
function GetOS(): String;
begin
  {$IFDEF UNIX}
result := 'Linux';
  {$ENDIF}
  {$IFDEF WINDOWS}
result := 'Win';
  {$ENDIF}
end;

function GetLastVerZipFile(version, LocalOS: String): Boolean;
var
  MS: TMemoryStream;
  DownLink: String = '';
  Conector: TFPHttpClient;
  Trys: Integer = 0;
begin
  Result := False;
  if Uppercase(localOS) = 'WIN' then
    DownLink := 'https://github.com/nosocoin/NosoNode/releases/download/v' +
      version + '/noso-v' + version + '-x86_64-win64.zip';
  if Uppercase(localOS) = 'LINUX' then
    DownLink := 'https://github.com/nosocoin/NosoNode/releases/download/v' +
      version + '/noso-v' + version + '-x86_64-linux.zip';
  MS := TMemoryStream.Create;
  Conector := TFPHttpClient.Create(nil);
  conector.ConnectTimeout := 1000;
  conector.IOTimeout := 1000;
  conector.AllowRedirect := True;
  repeat
    Inc(Trys);
  try
    Conector.Get(DownLink, MS);
    MS.SaveToFile('NOSODATA' + DirectorySeparator + 'UPDATES' +
      DirectorySeparator + version + '_' + LocalOS + '.zip');
    Result := True;
  except
    ON E: Exception do
    begin
      ToLog('console', Format('Error downloading release (Try %d): %s',
        [Trys, E.Message]));
    end;
  end{Try};
  until ((Result = True) or (Trys = 3));
  MS.Free;
  conector.Free;
end;

{
Function GetSyncTus():String;
Begin
  result := '';
  TRY
    Result := MyLastBlock.ToString+Copy(GetResumenHash,1,3)+Copy(MySumarioHash,1,3)+Copy(MyLastBlockHash,1,3);
  EXCEPT ON E:EXCEPTION do
    begin
    ToLog('console','****************************************'+slinebreak+'GetSyncTus:'+e.Message);
    end;
  END; {TRY}
End;
}
function GetMiIP(): String;
var
  TCPClient: TidTCPClient;
  NodeToUse: Integer;
begin
  NodeToUse := Random(NodesListLen);
  Result := '';
  TCPClient := TidTCPClient.Create(nil);
  TCPclient.Host := NodesIndex(NodeToUse).ip;
  TCPclient.Port := StrToIntDef(NodesIndex(NodeToUse).port, 8080);
  TCPclient.ConnectTimeout := 1000;
  TCPclient.ReadTimeout := 1000;
  try
    TCPclient.Connect;
    TCPclient.IOHandler.WriteLn('GETMIIP');
    Result := TCPclient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
    TCPclient.Disconnect();
  except
    on E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on GetMiIP: ' + E.Message)
  end{try};
  TCPClient.Free;
end;

function NodeServerInfo(): String;
var
  TotalSeconds, days, hours, minutes, seconds, remain: Integer;
begin
  if not form1.Server.Active then Result := 'OFF'
  else
  begin
    Totalseconds := UTCTime - ServerStartTime;
    Days := Totalseconds div 86400;
    remain := Totalseconds mod 86400;
    hours := remain div 3600;
    remain := remain mod 3600;
    minutes := remain div 60;
    remain := remain mod 60;
    seconds := remain;
    if Days > 0 then Result :=
        Format('[%d] %dd %.2d:%.2d:%.2d', [G_MNVerifications, Days,
        Hours, Minutes, Seconds])
    else
      Result := Format('[%d] %.2d:%.2d:%.2d', [G_MNVerifications,
        Hours, Minutes, Seconds]);
  end;
end;

procedure ClearReceivedOrdersIDs();
begin
  EnterCriticalSection(CSIdsProcessed);
  Setlength(ArrayOrderIDsProcessed, 0); // clear processed Orders
  LeaveCriticalSection(CSIdsProcessed);
end;

// Sends a order to the mainnet
function SendOrderToNode(OrderString: String): String;
var
  Client: TidTCPClient;
  RanNode: Integer;
  TrysCount: Integer = 0;
  WasOk: Boolean = False;
begin
  Result := '';
  if MyConStatus < 3 then
  begin
    Result := 'ERROR 20';
    exit;
  end;
  Client := TidTCPClient.Create(nil);
  repeat
    Inc(TrysCount);
    if GetValidSlotForSeed(RanNode) then
    begin
      Client.Host := GetConexIndex(RanNode).ip;
      Client.Port := GetConexIndex(RanNode).ListeningPort;
      Client.ConnectTimeout := 3000;
      Client.ReadTimeout := 3000;
      try
        Client.Connect;
        Client.IOHandler.WriteLn(OrderString);
        Result := Client.IOHandler.ReadLn(IndyTextEncoding_UTF8);
        WasOK := True;
      except
        on E: Exception do
        begin
          Result := 'ERROR 19';
        end;
      end{Try};
    end;
  until ((WasOk) or (TrysCount = 3));
  if Result <> '' then U_DirPanel := True;
  if Result = '' then Result := 'ERROR 21';
  if client.Connected then Client.Disconnect();
  client.Free;
end;

end. // END UNIT
