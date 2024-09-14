unit MP.Parser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPForm, MP.Gui, MP.Red, MP.Disk, Noso.Time, MP.Block, MP.Coin,
  Dialogs, fileutil, Forms, idglobal, strutils, MP.Rpc, DateUtils, Clipbrd, translation,
  idContext, Math, MP.SysCheck, Noso.Debug, Noso.General, Noso.Crypto, Noso.Summary,
  Noso.Consensus, Noso.Pso, Noso.WallCon, Noso.Headers, Noso.Block, Noso.Config, Noso.Network,
  Noso.Gvt, Noso.Masternodes;

procedure ProcessLinesAdd(const ALine: String);
procedure OutgoingMsjsAdd(const ALine: String);
function OutgoingMsjsGet(): String;

procedure ProcesarLineas();
function GetOpData(textLine: String): String;
procedure ParseCommandLine(LineText: String);
procedure NuevaDireccion(linetext: String);
procedure ShowNodes();
procedure ShowBots();
procedure ShowUser_Options();
function GetWalletBalance(): Int64;
procedure ConnectTo(LineText: String);
procedure AutoServerON();
procedure AutoServerOFF();
procedure ShowWallet();
procedure ImportarWallet(LineText: String);
procedure ExportarWallet(LineText: String);
procedure ShowBlchHead(number: Integer);
function SetDefaultAddress(linetext: String): Boolean;
procedure ParseShowBlockInfo(LineText: String);
procedure ShowBlockInfo(numberblock: Integer);
procedure CustomizeAddress(linetext: String);
procedure Parse_SendFunds(LineText: String);
function SendFunds(LineText: String; showOutput: Boolean = True): String;
procedure Parse_SendGVT(LineText: String);
function SendGVT(LineText: String; showOutput: Boolean = True): String;
procedure ShowHalvings();
procedure SetServerPort(LineText: String);
procedure TestParser(LineText: String);
//Procedure DeleteBots(LineText:String);
procedure Parse_RestartNoso();
procedure GetOwnerHash(LineText: String);
procedure CheckOwnerHash(LineText: String);
function AvailableUpdates(): String;
procedure RunUpdate(linea: String);
procedure RunGetBeta(linea: String);
procedure SendAdminMessage(linetext: String);
//Procedure RequestSumary();
procedure ShowOrderDetails(LineText: String);
procedure ExportAddress(LineText: String);
procedure ShowAddressInfo(LineText: String);
procedure ShowAddressHistory(LineText: String);
procedure ShowTotalFees();
function ShowPrivKey(linea: String; ToConsole: Boolean = False): String;
procedure TestNetwork(LineText: String);
procedure ShowPendingTrxs();
procedure WebWallet();
procedure ExportKeys(linea: String);
procedure NewAddressFromKeys(inputline: String);
procedure TestHashGeneration(inputline: String);
procedure CompareHashes(inputline: String);
procedure CreateMultiAddress(Inputline: String);

// CONSULTING
procedure ListGVTs();

// 0.2.1 DEBUG
procedure ShowBlockPos(LineText: String);
procedure ShowBlockMNs(LineText: String);
procedure showgmts(LineText: String);
procedure ShowSystemInfo(Linetext: String);
procedure ShowMNsChecks();

// EXCHANGE
procedure PostOffer(LineText: String);

procedure DebugTest2(linetext: String);
procedure OrdInfo(linetext: String);

procedure totallocked();
procedure ShowSumary();

// CONSENSUS

procedure ShowConsensus();
procedure ShowConsensusStats();

// PSOs testing functions

procedure TestNewPSO(Dataline: String);
procedure GetPSOs();
procedure ShowGVTInfo();
procedure ClearPSOs();
procedure ShowMNsLocked();

// Specific Tests

procedure Test_Headers();

implementation

uses
  MP.Protocol;

  // **************************
  // *** CRITICIAL SECTIONS ***
  // **************************

// Adds a line to ProcessLines thread safe
procedure ProcessLinesAdd(const ALine: String);
begin
  EnterCriticalSection(CSProcessLines);
  try
    ProcessLines.Add(ALine);
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on PROCESSLINESADD: ' + E.Message);
  end; {TRY}
  LeaveCriticalSection(CSProcessLines);
end;

// Adds a line to OutgoingMsjs thread safe
procedure OutgoingMsjsAdd(const ALine: String);
begin
  EnterCriticalSection(CSOutgoingMsjs);
  try
    OutgoingMsjs.Add(ALine);
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on OutgoingMsjsAdd: ' + E.Message);
  end{Try};
  LeaveCriticalSection(CSOutgoingMsjs);
end;

// Gets a line from OutgoingMsjs thread safe
function OutgoingMsjsGet(): String;
var
  Linea: String;
begin
  Linea := '';
  EnterCriticalSection(CSOutgoingMsjs);
  try
    Linea := OutgoingMsjs[0];
    OutgoingMsjs.Delete(0);
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error extracting outgoing line: ' + E.Message);
  end{Try};
  LeaveCriticalSection(CSOutgoingMsjs);
  Result := linea;
end;

// Procesa las lineas de la linea de comandos
procedure ProcesarLineas();
begin
  while ProcessLines.Count > 0 do
  begin
    ParseCommandLine(ProcessLines[0]);
    if ProcessLines.Count > 0 then
    begin
      EnterCriticalSection(CSProcessLines);
      try
        ProcessLines.Delete(0);
      except
        on E: Exception do
        begin
          ShowMessage('Your wallet just exploded and we will close it for your security'
            +
            slinebreak + 'Error deleting line 0 from ProcessLines');
          halt(0);
        end;
      end;
      LeaveCriticalSection(CSProcessLines);
    end;
  end;
end;

// Elimina el encabezado de una linea de protocolo
function GetOpData(textLine: String): String;
var
  CharPos: Integer;
begin
  charpos := pos('$', textline);
  Result := copy(textline, charpos, length(textline));
end;

procedure ParseCommandLine(LineText: String);
var
  Command: String;
  Counter: Integer;
  LItem: TSummaryData;
begin
  Command := Parameter(Linetext, 0);
  if not AnsiContainsStr(HideCommands, Uppercase(command)) then
    ToLog('Console', '>> ' + Linetext);
  if UpperCase(Command) = 'VER' then ToLog('console', MainnetVersion + NodeRelease)
  else if UpperCase(Command) = 'SERVERON' then StartServer()
  else if UpperCase(Command) = 'SERVEROFF' then StopServer()
  else if UpperCase(Command) = 'FORCESERVER' then ForceServer()
  else if UpperCase(Command) = 'NODES' then ShowNodes()
  else if UpperCase(Command) = 'BOTS' then ShowBots()
  {else if UpperCase(Command) = 'CONNECT' then ConnectToServers()}
  else if UpperCase(Command) = 'DISCONNECT' then CerrarClientes()
  else if UpperCase(Command) = 'OFFSET' then
    ToLog('console', 'Server: ' + NosoT_LastServer + SLINEBREAK +
      'Time offset seconds: ' + IntToStr(NosoT_TimeOffset) + slinebreak +
      'Last update : ' + TimeSinceStamp(NosoT_LastUpdate))
  else if UpperCase(Command) = 'NEWADDRESS' then NuevaDireccion(linetext)
  else if UpperCase(Command) = 'USEROPTIONS' then ShowUser_Options()
  else if UpperCase(Command) = 'BALANCE' then
    ToLog('console', Int2Curr(GetWalletBalance) + ' ' + CoinSimbol)
  else if UpperCase(Command) = 'CONNECTTO' then ConnectTo(Linetext)
  else if UpperCase(Command) = 'AUTOSERVERON' then AutoServerON()
  else if UpperCase(Command) = 'AUTOSERVEROFF' then AutoServerOFF()
  else if UpperCase(Command) = 'SHOWWALLET' then ShowWallet()
  else if UpperCase(Command) = 'IMPWALLET' then ImportarWallet(LineText)
  else if UpperCase(Command) = 'EXPWALLET' then ExportarWallet(LineText)
  else if UpperCase(Command) = 'RESUMEN' then
    ShowBlchHead(StrToIntDef(Parameter(Linetext, 1), MyLastBlock))
  else if UpperCase(Command) = 'SETDEFAULT' then SetDefaultAddress(LineText)
  else if UpperCase(Command) = 'LBINFO' then ShowBlockInfo(MyLastBlock)
  else if UpperCase(Command) = 'TIMESTAMP' then ToLog('console', UTCTimeStr)
  else if UpperCase(Command) = 'UNDOBLOCK' then UndoneLastBlock()  // to be removed
  else if UpperCase(Command) = 'CUSTOMIZE' then CustomizeAddress(LineText)
  else if UpperCase(Command) = 'SENDTO' then Parse_SendFunds(LineText)
  else if UpperCase(Command) = 'SENDGVT' then Parse_SendGVT(LineText)
  else if UpperCase(Command) = 'HALVING' then ShowHalvings()
  else if UpperCase(Command) = 'SETPORT' then SetServerPort(LineText)
  else if UpperCase(Command) = 'SHA256' then
    ToLog('console', HashSha256String(Parameter(LineText, 1)))
  else if UpperCase(Command) = 'MD5' then
    ToLog('console', HashMD5String(Parameter(LineText, 1)))
  else if UpperCase(Command) = 'MD160' then
    ToLog('console', HashMD160String(Parameter(LineText, 1)))
  else if UpperCase(Command) = 'CLEAR' then form1.Memoconsola.Lines.Clear
  else if UpperCase(Command) = 'TP' then TestParser(LineText)
  else if UpperCase(Command) = 'BLOCK' then ParseShowBlockInfo(LineText)
  else if UpperCase(Command) = 'TESTNET' then TestNetwork(LineText)
  else if UpperCase(Command) = 'RESTART' then Parse_RestartNoso()
  else if UpperCase(Command) = 'OSVERSION' then ToLog('console', OsVersion)
  else if UpperCase(Command) = 'DIRECTIVE' then SendAdminMessage(linetext)
  else if UpperCase(Command) = 'MYHASH' then ToLog('console', HashMD5File('noso.exe'))
  else if UpperCase(Command) = 'STATUS' then ToLog('console', GetCurrentStatus(1))
  else if UpperCase(Command) = 'GETCERT' then GetOwnerHash(LineText)
  else if UpperCase(Command) = 'CHECKCERT' then CheckOwnerHash(LineText)
  else if UpperCase(Command) = 'UPDATE' then RunUpdate(LineText)
  else if UpperCase(Command) = 'GETBETA' then RunGetBeta(LineText)
  else if UpperCase(Command) = 'RESTOREBLOCKCHAIN' then RestoreBlockChain()
  else if UpperCase(Command) = 'RESTORESUMARY' then
    RestoreSumary(StrToIntDef(Parameter(LineText, 1), 0))
  //else if UpperCase(Command) = 'REQSUM' then RequestSumary()
  else if UpperCase(Command) = 'SAVEADV' then CreateADV(True)
  else if UpperCase(Command) = 'ORDER' then ShowOrderDetails(LineText)
  else if UpperCase(Command) = 'ORDERSOURCES' then
    ToLog('console', GetOrderSources(Parameter(LineText, 1)))
  else if UpperCase(Command) = 'EXPORTADDRESS' then ExportAddress(LineText)
  else if UpperCase(Command) = 'ADDRESS' then ShowAddressInfo(LineText)
  else if UpperCase(Command) = 'HISTORY' then ShowAddressHistory(LineText)
  else if UpperCase(Command) = 'TOTALFEES' then ShowTotalFees()
  else if UpperCase(Command) = 'SUPPLY' then
    ToLog('console', 'Current supply: ' + Int2Curr(GetSupply(MyLastBlock)))
  else if UpperCase(Command) = 'GMTS' then showgmts(LineText)
  else if UpperCase(Command) = 'SHOWKEYS' then ShowPrivKey(LineText, True)
  else if UpperCase(Command) = 'SHOWPENDING' then ShowPendingTrxs()
  else if UpperCase(Command) = 'WEBWAL' then WebWallet()
  else if UpperCase(Command) = 'EXPKEYS' then ExportKeys(LineText)
  else if UpperCase(Command) = 'CHECKUPDATES' then ToLog('console', GetLastRelease)
  else if UpperCase(Command) = 'ZIPSUMARY' then ZipSumary()
  else if UpperCase(Command) = 'GETPOS' then
    ToLog('console', GetPoSPercentage(StrToIntdef(Parameter(linetext, 1),
      Mylastblock)).ToString)
  else if UpperCase(Command) = 'GETMNS' then
    ToLog('console', GetMNsPercentage(StrToIntdef(Parameter(linetext, 1), Mylastblock),
      GetCFGDataStr(0)).ToString)
  else if UpperCase(Command) = 'CLOSESTARTON' then WO_CloseStart := True
  else if UpperCase(Command) = 'CLOSESTARTOFF' then WO_CloseStart := False
  else if UpperCase(Command) = 'TT' then DebugTest2(LineText)
  else if UpperCase(Command) = 'BASE58SUM' then
    ToLog('console', BMB58resumen(parameter(linetext, 1)))
  else if UpperCase(Command) = 'PENDING' then ToLog('console', PendingRawInfo)
  else if UpperCase(Command) = 'HEADSIZE' then
    ToLog('console', GetHeadersHeigth.ToString)
  else if UpperCase(Command) = 'NEWFROMKEYS' then NewAddressFromKeys(LineText)
  else if UpperCase(Command) = 'TESTHASH' then TestHashGeneration(LineText)
  else if UpperCase(Command) = 'COMPARE' then CompareHashes(LineText)
  else if UpperCase(Command) = 'GETREPOSEEDS' then
    ToLog('console', SendApiRequest(
      'https://raw.githubusercontent.com/nosocoin/NosoNode/main/defseeds.nos'))
  else if UpperCase(Command) = 'FORCEREPOSEEDS' then
  begin
    SetCFGData(SendApiRequest(
      'https://raw.githubusercontent.com/nosocoin/NosoNode/main/defseeds.nos'), 1);
  end
  else if UpperCase(Command) = 'SENDREPORT' then
    SEndFileViaTCP(ResumeLogFilename, 'REPORT', 'debuglogs.nosocoin.com:18081', 18081)
  else if UpperCase(Command) = 'GETDBLB' then ToLog('console', GetDBLastBlock.ToString)
  else if UpperCase(Command) = 'ORDINFO' then OrdInfo(LineText)
  else if UpperCase(Command) = 'GETMULTI' then CreateMultiAddress(LineText)
  else if UpperCase(Command) = 'DELBOTS' then DeleteBots

  // New system

  else if UpperCase(Command) = 'SUMARY' then ShowSumary()
  else if UpperCase(Command) = 'REBUILDSUM' then RebuildSummary()

  // CONSULTING
  else if UpperCase(Command) = 'LISTGVT' then ListGVTs()
  else if UpperCase(Command) = 'GVTINFO' then ShowGVTInfo()
  else if UpperCase(Command) = 'SYSTEM' then ShowSystemInfo(Linetext)
  else if UpperCase(Command) = 'NOSOCFG' then ToLog('console', GetCFGDataStr)
  else if UpperCase(Command) = 'FUNDS' then
    ToLog('console', 'Project funds ' + lineEnding + 'NpryectdevepmentfundsGE: ' +
      Int2curr(GetAddressAvailable('NpryectdevepmentfundsGE')) + lineEnding +
      'NPrjectPrtcRandmJacptE5: ' + Int2curr(
      GetAddressAvailable('NPrjectPrtcRandmJacptE5')))
  else if UpperCase(Command) = 'SUMINDEXSIZE' then
    ToLog('console', IntToStr(SumIndexLength))
  else if UpperCase(Command) = 'MNSCHECKS' then ShowMNsChecks()
  else if UpperCase(Command) = 'TESTHEAD' then Test_Headers()


  // 0.2.1 DEBUG
  else if UpperCase(Command) = 'BLOCKPOS' then ShowBlockPos(LineText)
  else if UpperCase(Command) = 'BLOCKMNS' then ShowBlockMNs(LineText)
  else if UpperCase(Command) = 'MYIP' then ToLog('console', GetMiIP)
  else if UpperCase(Command) = 'SETMODE' then SetCFGData(parameter(linetext, 1), 0)
  else if UpperCase(Command) = 'ADDNODE' then AddCFGData(parameter(linetext, 1), 1)
  else if UpperCase(Command) = 'DELNODE' then RemoveCFGData(parameter(linetext, 1), 1)
  else if UpperCase(Command) = 'ADDPOOL' then AddCFGData(parameter(linetext, 1), 3)
  else if UpperCase(Command) = 'DELPOOL' then RemoveCFGData(parameter(linetext, 1), 3)
  else if UpperCase(Command) = 'RESTORECFG' then RestoreCFGData()
  else if UpperCase(Command) = 'ADDNOSOPAY' then AddCFGData(parameter(linetext, 1), 6)
  else if UpperCase(Command) = 'DELNOSOPAY' then RemoveCFGData(parameter(linetext, 1), 6)
  else if UpperCase(Command) = 'ISALLSYNCED' then ToLog('console', IsAllsynced.ToString)
  else if UpperCase(Command) = 'FREEZED' then Totallocked()
  else if UpperCase(Command) = 'CLEARCFG' then ClearCFGData(parameter(linetext, 1))

  else if UpperCase(Command) = 'ADDFROMPUB' then
    ToLog('console', GetAddressFromPublicKey(parameter(linetext, 1)))

  // 0.4.0
  else if UpperCase(Command) = 'CONSENSUS' then ShowConsensus()
  else if UpperCase(Command) = 'VALIDATE' then
    ToLog('console', BoolToStr(VerifyAddressOnDisk(parameter(linetext, 1)), True))
  // P2P
  else if UpperCase(Command) = 'PEERS' then
    ToLog('console', 'Server list: ' + IntToStr(form1.ClientsCount) +
      '/' + IntToStr(GetIncomingConnections))

  // RPC
  else if UpperCase(Command) = 'SETRPCPORT' then SetRPCPort(LineText)
  else if UpperCase(Command) = 'RPCON' then SetRPCOn()
  else if UpperCase(Command) = 'RPCOFF' then SetRPCOff()

  // PSO
  else if UpperCase(Command) = 'NEWPSO' then TestNewPSO(parameter(linetext, 1))
  else if UpperCase(Command) = 'LISTPSOS' then GetPSOs()
  else if UpperCase(Command) = 'CLEARPSOS' then CLEARPSOS()
  else if UpperCase(Command) = 'SHOWPSOS' then ShowMNsLocked()

  else if UpperCase(Command) = 'CONSTATS' then ShowConsensusStats()



  //EXCHANGE
  else if UpperCase(Command) = 'POST' then PostOffer(LineText)

  else
    ToLog('console', 'Unknown command: ' + Command);  // Unknow command
end;

// Add a new address generation to the crypto thread
procedure NuevaDireccion(linetext: String);
var
  cantidad: Integer;
  cont: Integer;
begin
  AddCRiptoOp(1, '', '');
  sleep(1);
end;

// muestra los nodos
procedure ShowNodes();
var
  contador: Integer = 0;
begin
  for contador := 0 to NodesListLen - 1 do
    ToLog('console', IntToStr(contador) + '- ' + NodesIndex(contador).ip +
      ':' + NodesIndex(contador).port);
end;

// muestra los Bots
procedure ShowBots();
var
  contador: Integer = 0;
begin
  for contador := 0 to length(BotsList) - 1 do
    ToLog('console', IntToStr(contador) + '- ' + BotsList[contador].ip);
  ToLog('console', IntToStr(length(BotsList)) + ' bots registered.');  // bots registered
end;

// Muestras las opciones del usuario
procedure ShowUser_Options();
begin
  ToLog('console', 'Language    : ' + WO_Language);
  ToLog('console', 'Server Port : ' + LocalMN_Port);
  ToLog('console', 'Wallet      : ' + WalletFilename);
  ToLog('console', 'AutoServer  : ' + BoolToStr(WO_AutoServer, True));
end;

// Returns the total balance on the wallet
function GetWalletBalance(): Int64;
var
  counter: Integer = 0;
  Total: Int64 = 0;
begin
  for counter := 0 to LenWallArr - 1 do
  begin
    Total := Total + GetAddressBalanceIndexed(GetWallArrIndex(counter).Hash);
  end;
  Result := Total - MontoOutgoing;
end;

// Conecta a un server especificado
procedure ConnectTo(LineText: String);
var
  Ip, Port: String;
begin
  Ip := Parameter(Linetext, 1);
  Port := Parameter(Linetext, 2);
  if StrToIntDef(Port, -1) = -1 then Port := '8080';
  ConnectClient(ip, port);
end;

procedure AutoServerON();
begin
  WO_autoserver := True;
  S_AdvOpt := True;
  ToLog('console', 'AutoServer option is now ' + 'ACTIVE');   //autoserver //active
end;

procedure AutoServerOFF();
begin
  WO_autoserver := False;
  S_AdvOpt := True;
  ToLog('console', 'AutoServer option is now ' + 'INACTIVE');   //autoserver //inactive
end;

// Shows all the addresses on the wallet
procedure ShowWallet();
var
  contador: Integer = 0;
begin
  for contador := 0 to LenWallArr - 1 do
  begin
    ToLog('console', GetWallArrIndex(contador).Hash);
  end;
  ToLog('console', IntToStr(LenWallArr) + ' addresses.');
  ToLog('console', Int2Curr(GetWalletBalance) + ' ' + CoinSimbol);
end;

procedure ExportarWallet(LineText: String);
var
  destino: String = '';
begin
  destino := Parameter(linetext, 1);
  destino := StringReplace(destino, '*', ' ', [rfReplaceAll, rfIgnoreCase]);
  if fileexists(destino + '.pkw') then
  begin
    ToLog('console', 'Error: Can not overwrite existing wallets');
    exit;
  end;
  if copyfile(WalletFilename, destino + '.pkw', []) then
  begin
    ToLog('console', 'Wallet saved as ' + destino + '.pkw');
  end
  else
  begin
    ToLog('console', 'Failed');
  end;
end;

procedure ImportarWallet(LineText: String);
var
  Cartera: String = '';
  CarteraFile: file of WalletData;
  DatoLeido: Walletdata;
  Contador: Integer = 0;
  Nuevos: Integer = 0;
begin
  Cartera := Parameter(linetext, 1);
  Cartera := StringReplace(Cartera, '*', ' ', [rfReplaceAll, rfIgnoreCase]);
  if not FileExists(cartera) then
  begin
    ToLog('console', 'Specified wallet file do not exists.');
    //Specified wallet file do not exists.
    exit;
  end;
  assignfile(CarteraFile, Cartera);
  try
    reset(CarteraFile);
    seek(CarteraFile, 0);
    Read(CarteraFile, DatoLeido);
    if not IsValidHashAddress(DatoLeido.Hash) then
    begin
      closefile(CarteraFile);
      ToLog('console', 'The file is not a valid wallet');
      exit;
    end;
    for contador := 0 to filesize(CarteraFile) - 1 do
    begin
      seek(CarteraFile, contador);
      Read(CarteraFile, DatoLeido);
      if ((WallAddIndex(DatoLeido.Hash) < 0) and
        (IsValidHashAddress(DatoLeido.Hash))) then
      begin
        InsertToWallArr(DatoLeido);
        Nuevos := nuevos + 1;
      end;
    end;
    closefile(CarteraFile);
  except
    on E: Exception do
      ToLog('console', 'The file is not a valid wallet');
    //'The file is not a valid wallet'
  end;
  if nuevos > 0 then
  begin
    OutText('Addresses imported: ' + IntToStr(nuevos), False, 2);
    //'Addresses imported: '
    UpdateWalletFromSumario;
  end
  else
    ToLog('console', 'No new addreses found.');  //'No new addreses found.'
end;

procedure ShowBlchHead(number: Integer);
var
  Dato: ResumenData;
  Found: Boolean = False;
  StartBlock: Integer = 0;
  counter: Integer = 100000;
  Errors: Integer = 0;
  ProperlyClosed: Boolean = False;
begin
  StartBlock := number - 10;
  if StartBlock < 0 then StartBlock := 0;
  try
    assignfile(FileResumen, ResumenFilename);
    reset(FileResumen);
    repeat
      Seek(FileResumen, StartBlock);
      Read(fileresumen, dato);
      ToLog('console', IntToStr(dato.block) + ' ' + copy(dato.blockhash, 1, 5) +
        ' ' + copy(dato.SumHash, 1, 5));
      if dato.blockhash = 'MISS' then Inc(Errors);
      if dato.sumhash = 'MISS' then Inc(Errors);
      Inc(StartBlock);
    until EOF(fileresumen);
    closefile(FileResumen);
    ProperlyClosed := True;
    ToLog('Console', 'Errors : ' + Errors.ToString);
  except
    ON E: Exception do
      ToLog('console', 'Error: ' + E.Message)
  end;{TRY}
  if not ProperlyClosed then closefile(FileResumen);
end;

// Cambiar la primera direccion de la wallet
function SetDefaultAddress(linetext: String): Boolean;
var
  Address: String;
  Index: Integer;
  OldData, NewData: walletData;
begin
  Result := False;
  Address := Parameter(linetext, 1);
  index := WallAddIndex(Address);
  if ((index < 0) or (index > LenWallArr - 1)) then
    OutText('Invalid address.', False, 2)  //'Invalid address number.'
  else if index = 0 then
    OutText('Address is already the default.', False, 2)
  //'Address 0 is already the default.'
  else
  begin
    if ChangeWallArrPos(0, index) then
    begin
      S_Wallet := True;
      U_DirPanel := True;
      Result := True;
    end;
  end;
end;

procedure ParseShowBlockInfo(LineText: String);
var
  blnumber: Integer;
begin
  blnumber := StrToIntDef(Parameter(linetext, 1), -1);
  if (blnumber < 0) or (blnumber > MylastBlock) then
    outtext('Invalid block number')
  else
    ShowBlockInfo(blnumber);
end;

procedure ShowBlockInfo(numberblock: Integer);
var
  Header: BlockHeaderData;
  LOrders: TBlockOrdersArray;
  LPOSes: BlockArraysPos;
  PosReward: Int64;
  PosCount: Integer;
  Counter: Integer;
begin
  if fileexists(BlockDirectory + IntToStr(numberblock) + '.blk') then
  begin
    Header := LoadBlockDataHeader(numberblock);
    ToLog('console', 'Block info: ' + IntToStr(numberblock));
    ToLog('console', 'Hash  :       ' + HashMD5File(
      BlockDirectory + IntToStr(numberblock) + '.blk'));
    ToLog('console', 'Number:       ' + IntToStr(Header.Number));
    ToLog('console', 'Time start:   ' + IntToStr(Header.TimeStart) +
      ' (' + TimestampToDate(Header.TimeStart) + ')');
    ToLog('console', 'Time end:     ' + IntToStr(Header.TimeEnd) +
      ' (' + TimestampToDate(Header.TimeEnd) + ')');
    ToLog('console', 'Time total:   ' + IntToStr(Header.TimeTotal));
    ToLog('console', 'L20 average:  ' + IntToStr(Header.TimeLast20));
    ToLog('console', 'Transactions: ' + IntToStr(Header.TrxTotales));
    ToLog('console', 'Difficult:    ' + IntToStr(Header.Difficult));
    ToLog('console', 'Target:       ' + Header.TargetHash);
    ToLog('console', 'Solution:     ' + Header.Solution);
    ToLog('console', 'Last Hash:    ' + Header.LastBlockHash);
    ToLog('console', 'Next Diff:    ' + IntToStr(Header.NxtBlkDiff));
    ToLog('console', 'Miner:        ' + Header.AccountMiner);
    ToLog('console', 'Fees:         ' + IntToStr(Header.MinerFee));
    ToLog('console', 'Reward:       ' + IntToStr(Header.Reward));
    LOrders := GetBlockTrxs(numberblock);
    if length(LOrders) > 0 then
    begin
      ToLog('console', 'TRANSACTIONS');
      for Counter := 0 to length(LOrders) - 1 do
      begin
        ToLog('console', Format('%-8s %-35s -> %-35s : %s',
          [LOrders[counter].OrderType, LOrders[counter].Sender,
          LOrders[counter].Receiver, int2curr(LOrders[counter].AmmountTrf)]));
      end;
    end;
    if numberblock > PoSBlockStart then
    begin
      LPoSes := GetBlockPoSes(numberblock);
      PosReward := StrToInt64Def(LPoSes[length(LPoSes) - 1].address, 0);
      SetLength(LPoSes, length(LPoSes) - 1);
      PosCount := length(LPoSes);
      ToLog('console', Format('PoS Reward: %s  /  Addresses: %d  /  Total: %s',
        [int2curr(PosReward), PosCount, int2curr(PosReward * PosCount)]));
    end;
    if numberblock > MNBlockStart then
    begin
      LPoSes := GetBlockMNs(numberblock);
      PosReward := StrToInt64Def(LPoSes[length(LPoSes) - 1].address, 0);
      SetLength(LPoSes, length(LPoSes) - 1);
      PosCount := length(LPoSes);
      ToLog('console', Format('MNs Reward: %s  /  Addresses: %d  /  Total: %s',
        [int2curr(PosReward), PosCount, int2curr(PosReward * PosCount)]));
    end;
  end
  else
    ToLog('console', 'Block file do not exists: ' + numberblock.ToString);
end;

procedure CustomizeAddress(linetext: String);
var
  address, AddAlias, TrfrHash, OrderHash, CurrTime: String;
  cont: Integer;
  procesar: Boolean = True;
begin
  address := Parameter(linetext, 1);
  AddAlias := Parameter(linetext, 2);
  if WallAddIndex(address) < 0 then
  begin
    ToLog('console', 'Invalid address');  //'Invalid address'
    procesar := False;
  end;
  if GetWallArrIndex(WallAddIndex(address)).Custom <> '' then
  begin
    ToLog('console', 'Address already have a custom alias');
    //'Address already have a custom alias'
    procesar := False;
  end;
  if ((length(AddAlias) < 5) or (length(AddAlias) > 40)) then
  begin
    OutText('Alias must have between 5 and 40 chars', False, 2);
    //'Alias must have between 5 and 40 chars'
    procesar := False;
  end;
  if IsValidHashAddress(addalias) then
  begin
    ToLog('console', 'Alias can not be a valid address');
    //'Alias can not be a valid address'
    procesar := False;
  end;
  if GetWallArrIndex(WallAddIndex(address)).Balance < GetCustFee(MyLastBlock) then
  begin
    ToLog('console', 'Insufficient balance'); //'Insufficient balance'
    procesar := False;
  end;
  if AddressAlreadyCustomized(Address) then
  begin
    ToLog('console', 'Address already have a custom alias');
    //'Address already have a custom alias'
    procesar := False;
  end;
  if AliasAlreadyExists(addalias) then
  begin
    ToLog('console', 'Alias already exists');
    procesar := False;
  end;
  for cont := 1 to length(addalias) do
  begin
    if pos(addalias[cont], CustomValid) = 0 then
    begin
      ToLog('console', 'Invalid character in alias: ' + addalias[cont]);
      info('Invalid character in alias: ' + addalias[cont]);
      procesar := False;
    end;
  end;
  if procesar then
  begin
    CurrTime := UTCTimeStr;
    TrfrHash := GetTransferHash(CurrTime + Address + addalias);
    OrderHash := GetOrderHash('1' + currtime + TrfrHash);
    AddCriptoOp(2, 'Customize this ' + address + ' ' + addalias +
      '$' + GetWallArrIndex(WallAddIndex(address)).PrivateKey,
      ProtocolLine(9) +    // CUSTOM
      OrderHash + ' ' +    // OrderID
      '1' + ' ' +          // OrderLines
      'CUSTOM' + ' ' +     // OrderType
      CurrTime + ' ' +     // Timestamp
      'null' + ' ' +       // reference
      '1' + ' ' +          // Trxline
      GetWallArrIndex(WallAddIndex(address)).PublicKey + ' ' +    // sender
      GetWallArrIndex(WallAddIndex(address)).Hash + ' ' +    // address
      AddAlias + ' ' +     // receiver
      IntToStr(GetCustFee(MyLastBlock)) + ' ' +  // Amountfee
      '0' + ' ' +                         // amount trfr
      '[[RESULT]] ' + TrfrHash);      // trfrhash
  end;
end;

// Incluye una solicitud de envio de fondos a la cola de transacciones cripto
procedure Parse_SendFunds(LineText: String);
begin
  AddCriptoOp(3, linetext, '');
end;

// Ejecuta una orden de transferencia
function SendFunds(LineText: String; showOutput: Boolean = True): String;
var
  Destination, amount, reference: String;
  monto, comision: Int64;
  montoToShow, comisionToShow: Int64;
  contador: Integer;
  Restante: Int64;
  ArrayTrfrs: array of Torderdata;
  currtime: String;
  TrxLinea: Integer = 0;
  OrderHashString: String;
  OrderString: String;
  AliasIndex: Integer;
  Procesar: Boolean = True;
  ResultOrderID: String = '';
  CoinsAvailable: Int64;
  DestinationRecord: TSummaryData;
  SendersString: String = '';
begin
  Result := '';
  BeginPerformance('SendFunds');
  Destination := Parameter(Linetext, 1);
  amount := Parameter(Linetext, 2);
  reference := Parameter(Linetext, 3);
  if ((Destination = '') or (amount = '')) then
  begin
    if showOutput then ToLog('console', 'Invalid parameters.'); //'Invalid parameters.'
    Procesar := False;
  end;
  if not IsValidHashAddress(Destination) then
  begin
    AliasIndex := GetIndexPosition(Destination, DestinationRecord, True);
    if AliasIndex < 0 then
    begin
      if showOutput then ToLog('console', 'Invalid destination.');
      //'Invalid destination.'
      Procesar := False;
    end
    else
      Destination := DestinationRecord.Hash;
  end;
  monto := StrToInt64Def(amount, -1);
  if reference = '' then reference := 'null';
  if monto <= 10 then
  begin
    if showOutput then ToLog('console', 'Invalid ammount.'); //'Invalid ammount.'
    Procesar := False;
  end;
  if procesar then
  begin
    Comision := GetMinimumFee(Monto);
    montoToShow := Monto;
    comisionToShow := Comision;
    Restante := monto + comision;
    if WO_Multisend then CoinsAvailable :=
        GetAddressBalanceIndexed(GetWallArrIndex(0).Hash) -
        GetAddressPendingPays(GetWallArrIndex(0).Hash)
    else
      CoinsAvailable := GetWalletBalance;
    if Restante > CoinsAvailable then
    begin
      if showOutput then ToLog('console', 'Insufficient funds. Needed: ' +
          Int2curr(Monto + comision));//'Insufficient funds. Needed: '
      Procesar := False;
    end;
  end;
  // empezar proceso
  if procesar then
  begin
    currtime := UTCTimeStr;
    Setlength(ArrayTrfrs, 0);
    Contador := form1.DireccionesPAnel.Row - 1;
    OrderHashString := currtime;
    while monto > 0 do
    begin
      BeginPerformance('SendFundsVerify');
      if AnsiContainsstr(SendersString, GetWallArrIndex(contador).Hash) then
      begin
        ToLog('console', 'Duplicated address on order');
        Exit;
      end;
      SendersString := SendersString + GetWallArrIndex(contador).Hash;
      if GetAddressBalanceIndexed(GetWallArrIndex(contador).Hash) -
      GetAddressPendingPays(GetWallArrIndex(contador).Hash) > 0 then
      begin
        trxLinea := TrxLinea + 1;
        Setlength(ArrayTrfrs, length(arraytrfrs) + 1);
        ArrayTrfrs[length(arraytrfrs) - 1] :=
          SendFundsFromAddress(GetWallArrIndex(contador).Hash,
          Destination, monto, comision, reference, CurrTime, TrxLinea);
        comision := comision - ArrayTrfrs[length(arraytrfrs) - 1].AmmountFee;
        monto := monto - ArrayTrfrs[length(arraytrfrs) - 1].AmmountTrf;
        OrderHashString := OrderHashString + ArrayTrfrs[length(arraytrfrs) - 1].TrfrID;
      end;
      Inc(contador);
      if contador >= LenWallArr then contador := 0;
      EndPerformance('SendFundsVerify');
    end;
    for contador := 0 to length(ArrayTrfrs) - 1 do
    begin
      ArrayTrfrs[contador].OrderID := GetOrderHash(IntToStr(trxLinea) + OrderHashString);
      ArrayTrfrs[contador].OrderLines := trxLinea;
    end;
    ResultOrderID := GetOrderHash(IntToStr(trxLinea) + OrderHashString);
    if showOutput then ToLog('console', 'Send to: ' + Destination +
        slinebreak + 'Send ' + Int2Curr(montoToShow) + ' fee ' +
        Int2Curr(comisionToShow) + slinebreak + 'Order ID: ' + ResultOrderID);
    Result := ResultOrderID;

    OrderString := GetPTCEcn + 'ORDER ' + IntToStr(trxLinea) + ' $';
    for contador := 0 to length(ArrayTrfrs) - 1 do
    begin
      OrderString := orderstring + GetStringfromOrder(ArrayTrfrs[contador]) + ' $';
    end;
    Setlength(orderstring, length(orderstring) - 2);
    OrderString := StringReplace(OrderString, 'PSK', 'NSLORDER', []);
    ToLog('console', 'Send to Node ' + OrderString);
    Result := SendOrderToNode(OrderString);
    //ToLog('console','Node result: '+result);
    OutgoingMsjsAdd(OrderString);
    EndPerformance('SendFunds');
  end // End procesar
  else
  begin
    if showOutput then ToLog('console',
        'Syntax: sendto {destination} {ammount} {reference}');
  end;
end;

// Process a GVT sending
procedure Parse_SendGVT(LineText: String);
begin
  AddCriptoOp(6, linetext, '');
end;

function SendGVT(LineText: String; showOutput: Boolean = True): String;
var
  GVTNumber: Integer;
  GVTOwner: String;
  Destination: String = '';
  AliasIndex: Integer;
  Procesar: Boolean = True;
  OrderTime: String = '';
  TrfrHash: String = '';
  OrderHash: String = '';
  ResultStr: String = '';
  Signature: String = '';
  GVTNumStr: String = '';
  StrTosign: String = '';
  DestinationRecord: TSummaryData;
begin
  Result := '';
  BeginPerformance('SendGVT');
  GVTNumber := StrToIntDef(Parameter(Linetext, 1), -1);
  Destination := Parameter(Linetext, 2);
  if ((GVTnumber < 0) or (GVTnumber > length(ArrGVTs) - 1)) then
  begin
    if showOutput then ToLog('console', 'Invalid GVT number');
    exit;
  end;
  GVTNumStr := ArrGVTs[GVTnumber].number;
  GVTOwner := ArrGVTs[GVTnumber].owner;
  if WallAddIndex(GVTOwner) < 0 then
  begin
    if showOutput then ToLog('console', 'You do not own that GVT');
    exit;
  end;
  if GetAddressAvailable(GVTOwner) < GetCustFee(MyLastBlock) then
  begin
    if showOutput then ToLog('console', 'Inssuficient funds');
    exit;
  end;
  if not IsValidHashAddress(Destination) then
  begin
    AliasIndex := GetIndexPosition(Destination, DestinationRecord, True);
    if AliasIndex < 0 then
    begin
      if showOutput then ToLog('console', 'Invalid destination.');
      //'Invalid destination.'
      Exit;
    end
    else
      Destination := DestinationRecord.Hash;
  end;
  if GVTOwner = Destination then
  begin
    if showOutput then ToLog('console', 'Can not transfer GVT to same address');
    exit;
  end;
  OrderTime := UTCTimeStr;
  TrfrHash := GetTransferHash(OrderTime + GVTOwner + Destination);
  OrderHash := GetOrderHash('1' + OrderTime + TrfrHash);
  StrTosign := 'Transfer GVT ' + GVTNumStr + ' ' + Destination + OrderTime;
  Signature := GetStringSigned(StrTosign,
    GetWallArrIndex(WallAddIndex(GVTOwner)).PrivateKey);
  ResultStr := ProtocolLine(21) + // sndGVT
    OrderHash + ' ' +     // OrderID
    '1' + ' ' +           // OrderLines
    'SNDGVT' + ' ' +      // OrderType
    OrderTime + ' ' +     // Timestamp
    GVTNumStr + ' ' +     // reference
    '1' + ' ' +           // Trxline
    GetWallArrIndex(WallAddIndex(GVTOwner)).PublicKey + ' ' +    // sender
    GetWallArrIndex(WallAddIndex(GVTOwner)).Hash + ' ' +        // address
    Destination + ' ' +   // receiver
    IntToStr(GetCustFee(MyLastBlock)) + ' ' +  // Amountfee
    '0' + ' ' +                         // amount trfr
    Signature + ' ' + TrfrHash;      // trfrhash
  OutgoingMsjsAdd(ResultStr);
  if showoutput then
  begin
    ToLog('console', 'GVT ' + GVTNumStr + ' transfered from ' +
      GetWallArrIndex(WallAddIndex(GVTOwner)).Hash + ' to ' + Destination);
    ToLog('console', 'Order: ' + OrderHash);
    //ToLog('console',StrToSign);
  end;
  EndPerformance('SendGVT');
end;

// Muestra la escala de halvings
procedure ShowHalvings();
var
  contador: Integer;
  texto: String;
  block1, block2: Integer;
  reward: Int64;
  MarketCap: Int64 = 0;
begin
  for contador := 0 to HalvingSteps do
  begin
    block1 := BlockHalvingInterval * (contador);
    if block1 = 0 then block1 := 1;
    block2 := (BlockHalvingInterval * (contador + 1)) - 1;
    reward := InitialReward div (2 ** contador);
    MarketCap := marketcap + (reward * BlockHalvingInterval);
    Texto := Format('From block %7d until %7d : %11s',
      [block1, block2, Int2curr(reward)]);
    //Texto :='From block '+IntToStr(block1)+' until '+IntToStr(block2)+': '+Int2curr(reward); //'From block '+' until '
    ToLog('console', Texto);
  end;
  ToLog('console', 'And then ' + int2curr(0)); //'And then '
  MarketCap := MarketCap + PremineAmount - InitialReward;
  // descuenta una recompensa inicial x bloque 0
  ToLog('console', 'Final supply: ' + int2curr(MarketCap)); //'Final supply: '
end;

// cambia el puerto de escucha
procedure SetServerPort(LineText: String);
var
  NewPort: String = '';
begin
  ToLog('console', 'Deprecated');
  Exit;
  NewPort := parameter(linetext, 1);
  if ((StrToIntDef(NewPort, 0) < 1) or (StrToIntDef(NewPort, 0) > 65535)) then
  begin
    ToLog('console', 'Invalid Port');
  end
  else
  begin
    LocalMN_Port := NewPort;
    OutText('New listening port: ' + NewPort, False, 2);
  end;
end;

// prueba la lectura de parametros de la linea de comandos
procedure TestParser(LineText: String);
var
  contador: Integer = 1;
  continuar: Boolean;
  parametro: String;
begin
  ToLog('console', Parameter(linetext, 0));
  continuar := True;
  repeat
    begin
      parametro := Parameter(linetext, contador);
      if parametro = '' then continuar := False
      else
      begin
        ToLog('console', IntToStr(contador) + ' ' + parametro);
        contador := contador + 1;
      end;
    end;
  until not continuar;
end;

// Delete bots from server
{
Procedure DeleteBots(LineText:String);
Begin
  SetLength(BotsList,0);
  LastBotClear := UTCTimeStr;
End;
}

procedure Parse_RestartNoso();
begin
  RestartNosoAfterQuit := True;
  CloseeAppSafely();
end;

procedure GetOwnerHash(LineText: String);
var
  Direccion, Pubkey, privkey, currtime, Certificate: String;
  AddIndex: Integer;
begin
  direccion := parameter(linetext, 1);
  AddIndex := WallAddIndex(direccion);
  if ((AddIndex < 0) or (direccion = '')) then
  begin
    ToLog('console', 'Invalid address');
  end
  else
  begin
    currtime := UTCTimeStr;
    Pubkey := GetWallArrIndex(AddIndex).PublicKey;
    Privkey := GetWallArrIndex(AddIndex).PrivateKey;
    Certificate := GetCertificate(Pubkey, privkey, currtime);
    ToLog('console', direccion + ' owner cert: ' + slinebreak + Certificate);
  end;
end;

procedure CheckOwnerHash(LineText: String);
var
  Data, firmtime, Address, Lalias: String;
begin
  BeginPerformance('CheckOwnerHash');
  Data := parameter(LineText, 1);
  Address := CheckCertificate(Data, firmtime);
  if Address <> '' then
  begin
    Lalias := GetAddressAlias(Address);
    if Lalias <> '' then
      Address := Format('%s [%s]', [Address, Lalias]);
    ToLog('console', Address + ' verified ' +
      TimeSinceStamp(StrToInt64(firmtime)) + ' ago.');
  end
  else
  begin
    ToLog('console', 'Invalid verification');
  end;
  EndPerformance('CheckOwnerHash');
end;

// devuelve una cadena con los updates disponibles
function AvailableUpdates(): String;
var
  updatefiles: TStringList;
  contador: Integer = 0;
  version: String;
begin
  Result := '';
  updatefiles := TStringList.Create;
  FindAllFiles(updatefiles, UpdatesDirectory, '*.zip', False);
  while contador < updatefiles.Count do
  begin
    version := copy(updatefiles[contador], 18, 8);
    Result := Result + version + ' ';
    Inc(contador);
  end;
  updatefiles.Free;
  Result := Trim(Result);
end;

// Manual update the app
procedure RunUpdate(linea: String);
var
  Tversion: String;
  TArch: String;
  overRule: Boolean = False;
begin
  Tversion := parameter(linea, 1);
  if Tversion = '' then Tversion := Parameter(GetLastRelease, 0);
  TArch := Uppercase(parameter(linea, 2));
  if TArch = '' then TArch := GetOS;
  ToLog('console', Format('Trying upgrade to version %s (%s)', [TVersion, TArch]));
  if ansicontainsstr(linea, ' /or') then overRule := True;
  Application.ProcessMessages;
  if ((Tversion = MainnetVersion + NodeRelease) and (not overRule)) then
  begin
    ToLog('console', 'Version ' + TVersion + ' already installed');
    exit;
  end;
  if GetLastVerZipFile(Tversion, TArch) then
  begin
    ToLog('console', 'Version ' + Tversion + ' downloaded');
    if UnZipUpdateFromRepo(Tversion, TArch) then
    begin
      ToLog('console', 'Unzipped !');
      {$IFDEF WINDOWS}
Trycopyfile('NOSODATA/UPDATES/Noso.exe','nosonew');
      {$ENDIF}
      {$IFDEF UNIX}
Trycopyfile('NOSODATA/UPDATES/Noso','Nosonew');
      {$ENDIF}
      CreateLauncherFile(True);
      RunExternalProgram(RestartFilename);
      CloseeAppSafely();
    end;
  end
  else
  begin
    ToLog('console', 'Update Failed');
  end;
end;

// Manual update to last beta
procedure RunGetBeta(linea: String);
var
  Tversion: String;
  TArch: String;
  overRule: Boolean = False;
begin
  Tversion := parameter(linea, 1);
  if Tversion = '' then Tversion := Parameter(GetLastRelease, 1);
  TArch := Uppercase(parameter(linea, 2));
  if TArch = '' then TArch := GetOS;
  ToLog('console', Format('Trying Install beta %s (%s)', [TVersion, TArch]));
  if ansicontainsstr(linea, ' /or') then overRule := True;
  Application.ProcessMessages;
  if ((Tversion = MainnetVersion + NodeRelease) and (not overRule)) then
  begin
    ToLog('console', 'Version ' + TVersion + ' already installed');
    exit;
  end;
  if GetLastVerZipFile(Tversion, TArch) then
  begin
    ToLog('console', 'Beta Version ' + Tversion + ' downloaded');
    if UnZipUpdateFromRepo(Tversion, TArch) then
    begin
      ToLog('console', 'Unzipped !');
      {$IFDEF WINDOWS}
Trycopyfile('NOSODATA/UPDATES/Noso.exe','nosonew');
      {$ENDIF}
      {$IFDEF UNIX}
Trycopyfile('NOSODATA/UPDATES/Noso','Nosonew');
      {$ENDIF}
      CreateLauncherFile(True);
      RunExternalProgram(RestartFilename);
      CloseeAppSafely();
    end;
  end
  else
  begin
    ToLog('console', 'Beta instalation Failed');
  end;
end;

procedure SendAdminMessage(linetext: String);
var
  mensaje, currtime, firma, hashmsg: String;
begin
  if (WallAddIndex(AdminHash) < 0) then
    ToLog('console', 'Only the Noso developers can do this.')
  //Only the Noso developers can do this
  else
  begin
    mensaje := copy(linetext, 11, length(linetext));
    //Mensaje := parameter(linetext,1);
    currtime := UTCTimeStr;
    firma := GetStringSigned(currtime + mensaje, GetWallArrIndex(
      WallAddIndex(AdminHash)).PrivateKey);
    hashmsg := HashMD5String(currtime + mensaje + firma);
    mensaje := StringReplace(mensaje, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
    OutgoingMsjsAdd(GetPTCEcn + 'ADMINMSG ' + currtime + ' ' + mensaje +
      ' ' + firma + ' ' + hashmsg);
    mensaje := StringReplace(mensaje, '_', ' ', [rfReplaceAll, rfIgnoreCase]);
    ToLog('console', 'Directive sent: ' + mensaje);
  end;
end;

procedure ShowOrderDetails(LineText: String);
var
  orderid: String;
  orderdetails: String;
  ThisOrderdata: TOrderGroup;
begin
  orderid := parameter(LineText, 1);
  ThisOrderdata := GetOrderDetails(orderid);
  if thisorderdata.AmmountTrf <= 0 then
    ToLog('console', 'Order not found')
  else
  begin
    ToLog('console', 'Time     : ' + TimestampToDate(ThisOrderdata.TimeStamp));
    if ThisOrderdata.Block = -1 then ToLog('console', 'Block: Pending')
    else
      ToLog('console', 'Block    : ' + IntToStr(ThisOrderdata.Block));
    ToLog('console', 'Type     : ' + ThisOrderdata.OrderType);
    ToLog('console', 'Trfrs    : ' + IntToStr(ThisOrderdata.OrderLines));
    ToLog('console', 'sender   : ' + ThisOrderdata.Sender);
    ToLog('console', 'Receiver : ' + ThisOrderdata.receiver);
    ToLog('console', 'Ammount  : ' + Int2curr(ThisOrderdata.AmmountTrf));
    ToLog('console', 'Fee      : ' + Int2curr(ThisOrderdata.AmmountFee));
    ToLog('console', 'Reference: ' + ThisOrderdata.reference);
  end;
end;

// Exports a single address credentials of the wallet
procedure ExportAddress(LineText: String);
var
  addresshash: String;
  newfile: file of WalletData;
  Data: WalletData;
begin
  addresshash := parameter(LineText, 1);
  if WallAddIndex(addresshash) >= 0 then
  begin
    Assignfile(newfile, 'tempwallet.pkw');
    rewrite(newfile);
    Data := GetWallArrIndex(WallAddIndex(addresshash));
    Write(newfile, Data);
    closefile(newfile);
    ToLog('console', 'Address exported to tempwallet.pkw');
  end
  else
    ToLog('console', 'Address not found in wallet');
end;

// Shows all the info of a specified address
procedure ShowAddressInfo(LineText: String);
var
  addtoshow, addhash, addalias: String;
  sumposition: Integer;
  onsumary, pending: Int64;
  counter: Integer;
  OwnedGVTs: String = '';
  LRecord: TSummaryData;
begin
  addtoshow := parameter(LineText, 1);
  if IsValidHashAddress(addtoshow) then
  begin
    addhash := addtoshow;
    addalias := GetAddressAlias(addtoshow);
    sumposition := GetIndexPosition(AddToShow, LRecord, False);
  end
  else
  begin
    sumposition := GetIndexPosition(AddToShow, LRecord, True);
    if Sumposition >= 0 then
    begin
      addhash := LRecord.Hash;
      AddAlias := AddToShow;
    end;
  end;
  if sumposition < 0 then
    ToLog('console', 'Address do not exists in sumary.')
  else
  begin
    onsumary := GetAddressBalanceIndexed(addhash);
    pending := GetAddressPendingPays(addhash);
    ToLog('console', 'Address   : ' + addhash + slinebreak +
      'Alias     : ' + AddAlias + slinebreak +
      format('Summary   : %s (%d)', [Int2curr(onsumary), sumposition]) +
      slinebreak +
      //'Sumary    : '+Int2curr(onsumary)+slinebreak+
      'Incoming  : ' + Int2Curr(GetAddressIncomingpays(AddHash)) +
      slinebreak + 'Outgoing  : ' + Int2curr(pending) + slinebreak +
      'Available : ' + int2curr(onsumary - pending));
    if AnsiContainsStr(GetMN_FileText, addhash) then
      ToLog('console', 'Masternode: Active');
    EnterCriticalSection(CSGVTsArray);
    for counter := 0 to length(ArrGVTs) - 1 do
    begin
      if ArrGVTs[counter].owner = addhash then
      begin
        OwnedGVTs := OwnedGVTs + counter.ToString + ' ';
      end;
    end;
    LeaveCriticalSection(CSGVTsArray);
    OwnedGVTs := Trim(OwnedGVTs);
    if OwnedGVTs <> '' then
      ToLog('console', 'GVTs      : ' + OwnedGVTs);
  end;
end;

// Shows transaction history of the specified address
procedure ShowAddressHistory(LineText: String);
var
  BlockCount: Integer;
  addtoshow: String;
  counter, contador2: Integer;
  Header: BlockHeaderData;
  ArrTrxs: TBlockOrdersArray;
  incomingtrx: Integer = 0;
  minedblocks: Integer = 0;
  inccoins: Int64 = 0;
  outgoingtrx: Integer = 0;
  outcoins: Int64 = 0;
  inbalance: Int64;
  ArrayPos: BlockArraysPos;
  PosReward: Int64;
  PosCount: Integer;
  CounterPos: Integer;
  PosPAyments: Integer = 0;
  PoSEarnings: Int64 = 0;
  TransSL: TStringList;
  MinedBlocksStr: String = '';
  sumpool1: Int64 = 0;
  sumpool2: Int64 = 0;
  sumpool3: Int64 = 0;
  sumpool4: Int64 = 0;
begin
  BlockCount := StrToIntDef(Parameter(Linetext, 2), 0);
  if BlockCount = 0 then BlockCount := SecurityBlocks - 1;
  if BlockCount >= MyLastBlock then BlockCount := MyLastBlock - 1;
  TransSL := TStringList.Create;
  addtoshow := parameter(LineText, 1);
  for counter := MyLastBlock downto MyLastBlock - BlockCount do
  begin
    if counter mod 10 = 0 then
    begin
      info('History :' + IntToStr(Counter));
      application.ProcessMessages;
    end;
    Header := LoadBlockDataHeader(counter);
    if Header.AccountMiner = addtoshow then // address is miner
    begin
      minedblocks += 1;
      MinedBlocksStr := MinedBlocksStr + Counter.ToString + ' ';
      inccoins := inccoins + header.Reward + header.MinerFee;
    end;
    ArrTrxs := GetBlockTrxs(counter);
    if length(ArrTrxs) > 0 then
    begin
      for contador2 := 0 to length(ArrTrxs) - 1 do
      begin
        if ArrTrxs[contador2].Receiver = addtoshow then // incoming order
        begin
          incomingtrx += 1;
          inccoins := inccoins + ArrTrxs[contador2].AmmountTrf;
          transSL.Add(IntToStr(Counter) + '] ' + ArrTrxs[contador2].Sender +
            '<-- ' + Int2curr(ArrTrxs[contador2].AmmountTrf));
        end;
        if ArrTrxs[contador2].Sender = addtoshow then // outgoing order
        begin
          outgoingtrx += 1;
          outcoins := outcoins + ArrTrxs[contador2].AmmountTrf +
            ArrTrxs[contador2].AmmountFee;
          transSL.Add(IntToStr(Counter) + '] ' + ArrTrxs[contador2].Receiver +
            '--> ' + Int2curr(ArrTrxs[contador2].AmmountTrf));
        end;
      end;
    end;
    SetLength(ArrTrxs, 0);
    if counter >= PoSBlockStart then
    begin
      ArrayPos := GetBlockPoSes(counter);
      PosReward := StrToIntDef(Arraypos[length(Arraypos) - 1].address, 0);
      SetLength(ArrayPos, length(ArrayPos) - 1);
      PosCount := length(ArrayPos);
      for counterpos := 0 to PosCount - 1 do
      begin
        if ArrayPos[counterPos].address = addtoshow then
        begin
          PosPAyments += 1;
          PosEarnings := PosEarnings + PosReward;
        end;
      end;
      SetLength(ArrayPos, 0);
    end;
  end;
  inbalance := GetAddressBalanceIndexed(addtoshow);
  ToLog('console', 'Last block : ' + IntToStr(MyLastBlock));
  ToLog('console', 'Address    : ' + addtoshow);
  ToLog('console', 'INCOMINGS');
  ToLog('console', '  Mined        : ' + IntToStr(minedblocks));
  ToLog('console', '  Mined blocks : ' + MinedBlocksStr);
  ToLog('console', '  Transactions : ' + IntToStr(incomingtrx));
  ToLog('console', '  Coins        : ' + Int2Curr(inccoins));
  ToLog('console', '  PoS Payments : ' + IntToStr(PosPAyments));
  ToLog('console', '  PoS Earnings : ' + Int2Curr(PosEarnings));
  ToLog('console', 'OUTGOINGS');
  ToLog('console', '  Transactions : ' + IntToStr(outgoingtrx));
  ToLog('console', '  Coins        : ' + Int2Curr(outcoins));
  ToLog('console', 'TOTAL  : ' + Int2Curr(inccoins - outcoins + PoSearnings));
  ToLog('console', 'SUMARY : ' + Int2Curr(inbalance));
  ToLog('console', '');
  ToLog('console', 'Transactions');
  while TransSL.Count > 0 do
  begin
    ToLog('console', TransSL[0]);
    TransSL.Delete(0);
  end;
  TransSL.Free;
end;

// Shows the total fees paid in the whole blockchain
procedure ShowTotalFees();
var
  counter: Integer;
  Header: BlockHeaderData;
  totalcoins: Int64 = 0;
begin
  for counter := 1 to MyLastBlock do
  begin
    Header := LoadBlockDataHeader(counter);
    totalcoins := totalcoins + header.MinerFee;
    if counter mod 100 = 0 then
    begin
      info('TOTAL FEES ' + counter.ToString);
      application.ProcessMessages;
    end;
  end;
  ToLog('console', 'Blockchain total fees: ' + Int2curr(totalcoins));
  ToLog('console', 'Block average        : ' + Int2curr(totalcoins div MyLastBlock));
end;

 // *******************
 // *** DEBUG 0.2.1 ***
 // *******************

procedure ShowBlockPos(LineText: String);
var
  number: Integer;
  ArrayPos: BlockArraysPos;
  PosReward: Int64;
  PosCount, counterPos: Integer;
begin
  number := StrToIntDef(parameter(linetext, 1), 0);
  if ((number < PoSBlockStart) or (number > MyLastBlock)) then
  begin
    ToLog('console', 'Invalid block number: ' + number.ToString);
  end
  else
  begin
    ArrayPos := GetBlockPoSes(number);
    PosReward := StrToIntDef(Arraypos[length(Arraypos) - 1].address, 0);
    SetLength(ArrayPos, length(ArrayPos) - 1);
    PosCount := length(ArrayPos);
    for counterpos := 0 to PosCount - 1 do
      ToLog('console', ArrayPos[counterPos].address + ': ' + int2curr(PosReward));
    ToLog('console', 'Block:   : ' + IntToStr(number));
    ToLog('console', 'Addresses: ' + IntToStr(PosCount));
    ToLog('console', 'Reward   : ' + int2curr(PosReward));
    ToLog('console', 'Total    : ' + int2curr(PosCount * PosReward));
    SetLength(ArrayPos, 0);
  end;
end;

procedure ShowBlockMNs(LineText: String);
var
  number: Integer;
  ArrayMNs: BlockArraysPos;
  MNsReward: Int64;
  MNsCount, counterMNs: Integer;
begin
  number := StrToIntDef(parameter(linetext, 1), 0);
  if ((number < MNBlockStart) or (number > MyLastBlock)) then
  begin
    ToLog('console', 'Invalid block number: ' + number.ToString);
  end
  else
  begin
    ArrayMNs := GetBlockMNs(number);
    MNsReward := StrToIntDef(ArrayMNs[length(ArrayMNs) - 1].address, 0);
    SetLength(ArrayMNs, length(ArrayMNs) - 1);
    MNSCount := length(ArrayMNs);
    for counterMNs := 0 to MNsCount - 1 do
      ToLog('console', ArrayMNs[counterMNs].address);
    ToLog('console', 'MNs Block : ' + IntToStr(number));
    ToLog('console', 'Addresses : ' + IntToStr(MNsCount));
    ToLog('console', 'Reward    : ' + int2curr(MNsReward));
    ToLog('console', 'Total     : ' + int2curr(MNsCount * MNsReward));
    SetLength(ArrayMNs, 0);
  end;
end;

procedure showgmts(LineText: String);
var
  monto: Int64;
  gmts, fee: Int64;
begin
  monto := StrToInt64Def(Parameter(LineText, 1), 0);
  gmts := GetMaximunToSend(monto);
  fee := monto - gmts;
  if fee < 1000000{MinimunFee} then fee := 1000000{MinimunFee};
  if monto <= 1000000{MinimunFee} then
  begin
    gmts := 0;
    fee := 0;
  end;
  ToLog('console', 'Ammount         : ' + Int2Curr(monto));
  ToLog('console', 'Maximun to send : ' + Int2Curr(gmts));
  ToLog('console', 'Fee paid        : ' + Int2Curr(fee));
  if gmts + fee = monto then ToLog('console', '✓ Match')
  else
    ToLog('console', '✗ Error');
end;

// List all GVTs owners
procedure ListGVTs();
var
  counter: Integer;
begin
  ShowGVTInfo;
  ToLog('console', 'Existing: ' + Length(arrgvts).ToString);
  for counter := 0 to length(arrgvts) - 1 do
    ToLog('console', Format('%.2d %s', [counter, arrgvts[counter].owner]));
  UpdateMyGVTsList;
end;

function ShowPrivKey(linea: String; ToConsole: Boolean = False): String;
var
  addtoshow: String;
  sumposition: Integer;
begin
  Result := '';
  addtoshow := parameter(linea, 1);
  sumposition := WallAddIndex(addtoshow);
  if sumposition < 0 then
  begin
    if ToConsole then ToLog('console', rs1504);
  end
  else
  begin
    Result := GetWallArrIndex(sumposition).PublicKey + ' ' + GetWallArrIndex(
      sumposition).PrivateKey;
  end;
  if ToConsole then ToLog('console', Result);
end;

procedure TestNetwork(LineText: String);
var
  numero: Integer;
  monto: Integer;
  contador: Integer;
begin
  numero := StrToIntDef(Parameter(linetext, 1), 0);
  if ((numero < 1) or (numero > 1000)) then
    Outtext('Range must be 1-1000')
  else
  begin
    Randomize;
    for contador := 1 to numero do
    begin
      Monto := 1000000 + contador;
      ProcesslinesAdd('SENDTO devteam_donations ' + IntToStr(Monto) +
        ' ' + contador.ToString);
    end;
  end;
end;

procedure ShowPendingTrxs();
begin

end;

procedure WebWallet();
var
  contador: Integer;
  ToClipboard: String = '';
begin
  for contador := 0 to LenWallArr - 1 do
  begin
    ToClipboard := ToClipboard + (GetWallArrIndex(contador).Hash) + ',';
  end;
  Setlength(ToClipboard, length(ToClipboard) - 1);
  Clipboard.AsText := ToClipboard;
  ToLog('console', 'Web wallet data copied to clipboard');
end;

procedure ExportKeys(linea: String);
var
  sumposition: Integer;
  addtoshow: String = '';
  Resultado: String = '';
begin
  addtoshow := parameter(linea, 1);
  sumposition := WallAddIndex(addtoshow);
  if sumposition < 0 then
  begin
    ToLog('console', rs1504);
  end
  else
  begin
    Resultado := GetWallArrIndex(sumposition).PublicKey + ' ' +
      GetWallArrIndex(sumposition).PrivateKey;
    Clipboard.AsText := Resultado;
    ToLog('console', rs1505);
  end;
end;

procedure PostOffer(LineText: String);
var
  FromAddress: String = '';
  Amount: Int64 = 0;
  Market: String = '';
  Price: Int64;
  TotalPost: Int64;
  PAyAddress: String = '';
  Duration: Int64;
  FeeTotal: Int64;
  FeeTramos: Int64;

  ErrorCode: Integer = 0;
  errorMessage: String = '';
begin
  FromAddress := Parameter(LineText, 1);
  if UPPERCASE(FromAddress) = 'DEF' then FromAddress := GetWallArrIndex(0).Hash;
  if UPPERCASE(Parameter(linetext, 2)) = 'MAX' then
    Amount := GetMaximunToSend(GetAddressAvailable(FromAddress))
  else
    Amount := StrToInt64Def(Parameter(linetext, 2), 0);
  Market := UpperCase(Parameter(LineText, 3));
  Price := StrToInt64Def(Parameter(linetext, 4), 0);
  TotalPost := amount * price div 100000000;
  PayAddress := Parameter(LineText, 5);
  Duration := StrToInt64Def(Parameter(LineText, 6), 100);
  if duration > 1000 then duration := 1000;
  Feetramos := duration div 100;
  if duration mod 100 > 0 then feetramos += 1;
  FeeTotal := GetMinimumFee(amount) * feetramos;


  if FromAddress = '' then ErrorCode := -1
  else if WallAddIndex(FromAddress) < 0 then ErrorCode := 1
  else if ((amount = 0) or (amount + GetMinimumFee(amount) >
    GetAddressAvailable(FromAddress))) then ErrorCode := 2
  else if not AnsiContainsStr(AvailableMarkets, market) then ErrorCode := 3
  else if price <= 0 then ErrorCode := 4;

  if errorcode = -1 then ErrorMessage :=
      'post {address} {ammount} {market} {price} {payaddress}' + ' {duration}';
  if errorcode = 1 then ErrorMessage := 'Invalid Address';
  if errorcode = 2 then ErrorMessage := 'Invalid Ammount';
  if errorcode = 3 then ErrorMessage := 'Invalid market';
  if errorcode = 4 then ErrorMessage := 'Invalid price';

  if ErrorMessage <> '' then ToLog('console', ErrorMessage)
  else
  begin
    ToLog('console', 'Post Exchange Offer');
    ToLog('console', 'From Address: ' + FromAddress);
    ToLog('console', 'Ammount     : ' + Int2Curr(amount) + ' ' + CoinSimbol);
    ToLog('console', 'Market      : ' + Market);
    ToLog('console', 'Price       : ' + Int2Curr(price) + ' ' + Market);
    ToLog('console', 'Total       : ' + Int2Curr(TotalPost) + ' ' + Market);
    ToLog('console', 'Pay to      : ' + PayAddress);
    ToLog('console', 'Duration    : ' + IntToStr(Duration) + ' blocks');
    ToLog('console', 'Fee         : (' + IntToStr(Feetramos) + ') ' +
      Int2Curr(FeeTotal) + ' ' + CoinSimbol);

  end;

end;

procedure DebugTest2(linetext: String);
var
  total: Integer;
  verifis: Integer;
  counter: Integer;
begin
  Total := Length(ArrayMNsData);
  verifis := (total div 10) + 3;
  ToLog('console', GetVerificatorsText);
  ToLog('console', 'Masternodes  : ' + IntToStr(total));
  ToLog('console', 'Verificators : ' + IntToStr(verifis));
  for counter := 0 to verifis - 1 do
    ToLog('console', format('%s %s %d', [ArrayMNsData[counter].ipandport,
      copy(arrayMNsData[counter].address, 1, 5), ArrayMNsData[counter].age]));
end;

procedure OrdInfo(linetext: String);
var
  LOrder: TOrderData;
begin
  beginperformance('ordinfo');
  if GetOrderFromDB(parameter(linetext, 1), LOrder) then
  begin
    ToLog('console', Lorder.Block.ToString);
    ToLog('console', Lorder.Sender);
    ToLog('console', Lorder.receiver);
    ToLog('console', Lorder.AmmountTrf.ToString);
  end
  else
    ToLog('console', 'Order not found');
  Endperformance('ordinfo');
end;

procedure ShowSystemInfo(Linetext: String);
var
  DownSpeed: Int64;
  Param: String;
begin
  if MyConStatus > 0 then exit;
  Param := Uppercase(Parameter(Linetext, 1));
  if param = 'POWER' then
    ToLog('console', Format('Processing       : %d Trx/s', [Sys_HashSpeed]))
  else if param = 'MEM' then
    ToLog('console', Format('Available memory : %d MB', [AllocateMem]))
  else if param = 'DOWNSPEED' then
    ToLog('console', Format('Download speed   : %d Kb/s', [TestDownloadSpeed]))
  else
    ToLog('console', 'Invalid parameter: ' + Param + slinebreak +
      'Use: power, mem or downspeed');
end;

procedure totallocked();
var
  sourcestr: String;
  thisadd: String;
  counter: Integer = 0;
  total: Int64 = 0;
  Count: Integer = 0;
  MNMsg: String;
  ThisBal: Int64;
  LAstOP: Int64;
begin
  sourcestr := GetCFGDataStr(5);
  repeat
    Thisadd := Parameter(sourcestr, counter, ':');
    if thisadd <> '' then
    begin
      ThisBal := GetAddressBalanceIndexed(ThisAdd);
      Inc(Total, ThisBal);
      Inc(Count);
      LastOP := GetAddressLastOP(ThisAdd);
      if AnsiContainsStr(GetMN_FileText, Thisadd) then MNMsg := '[MN]'
      else
        MNMsg := '';
      ToLog('console', format('%-35s : %15s  [%5d] %s',
        [thisadd, int2curr(ThisBal), LastOP, MNMsg]));
    end;
    Inc(counter);
  until thisadd = '';
  ToLog('console', format('Freezed %d : %s', [Count, int2curr(Total)]));
end;

{ShowsSummary file info}
procedure ShowSumary();
var
  SumFile: file;
  Readed: Integer = 0;
  ThisRecord: TSummaryData;
  IndexPosition: Int64;
  CurrPos: Int64 = 0;
  TotalCoins: Int64 = 0;
  AsExpected: String = '';
  NegativeCount: Integer = 0;
  EmptyCount: Integer = 0;
  LastRecord: Integer = -1;
  NodeAddresses: Integer = 0;
  NodeAmount: Int64 = 0;
  SNAddresses: Integer = 0;
  SNAmount: Int64 = 0;
  NanoAddresses: Integer = 0;
  NanoAmount: Int64 = 0;
  ShortAdd: Integer = 0;
  TotalOld: Int64 = 0;
  NanoAddres: Integer = 0;
begin
  AssignFile(SumFile, SummaryFileName);
  try
    Reset(SumFile, 1);
    while not EOF(SumFile) do
    begin
      blockread(sumfile, ThisRecord, sizeof(ThisRecord));
      if lastrecord < 0 then LastRecord := ThisRecord.LastOP;
      if thisrecord.Balance < 0 then Inc(NegativeCount);
      if thisrecord.Balance = 0 then Inc(EmptyCount);
      Inc(TotalCoins, ThisRecord.Balance);
      if Length(Thisrecord.Hash) < 28 then
      begin
        Inc(ShortAdd);
        ToLog('console', ThisRecord.Hash);
      end;
      if ((ThisRecord.Balance > 0) and (ThisREcord.Balance < 101)) then Inc(NanoAddres);
      if ThisRecord.Balance >= 1050000000000 then
      begin
        Inc(NodeAddresses);
        Inc(NodeAmount, ThisRecord.Balance);
      end;
      if ((ThisRecord.Balance >= 10500000000) and
        (ThisRecord.Balance < 1050000000000)) then
      begin
        Inc(SNAddresses);
        Inc(SNAmount, ThisRecord.Balance);
      end;
      if ((ThisRecord.Balance > 0) and (ThisRecord.Balance < 10500000000)) then
      begin
        Inc(NanoAddresses);
        Inc(NanoAmount, ThisRecord.Balance);
      end;
      if ((ThisRecord.LastOP < 10000) and (ThisRecord.Balance > 100000000)) then
      begin
        Inc(TotalOld, ThisRecord.Balance);
      end;
      if not IsValidHashAddress(ThisRecord.Hash) then
        ToLog('console', ThisRecord.Hash);
      Inc(currpos);
    end;
    CloseFile(SumFile);
  except
  end;{Try}
  ToLog('console', 'Total old: ' + int2curr(totalold));
  if TotalCoins = GetSupply(MyLastBlock) then AsExpected := '✓'
  else
    AsExpected := '(' + Int2curr(TotalCoins - GetSupply(MyLastBlock)) + ')';
  ToLog('console', format('Block : %d (short: %d)', [LastRecord, shortadd]));
  ToLog('console', Int2Curr(Totalcoins) + ' ' + CoinSimbol + ' ' + AsExpected);
  ToLog('console', format('Addresses (%d): %d (%d empty)',
    [NegativeCount, currpos, EmptyCount]));
  ToLog('console', format('>= 10500      : %d (%s Noso)',
    [NodeAddresses, int2curr(NodeAmount)]));
  ToLog('console', format('105 - 10500   : %d (%s Noso)',
    [SNAddresses, int2curr(SNAmount)]));
  ToLog('console', format('<105          : %d (%s Noso)',
    [NanoAddresses, int2curr(NanoAmount)]));
  ToLog('console', format('Nano          : %d ', [NanoAddres]));
end;

procedure ShowConsensus();
var
  counter: Integer;
  LText: String;
begin
  ToLog('console', Format('(%d / %d) %d %%', [Css_ReachedNodes,
    Css_TotalNodes, Css_Percentage]));
  for counter := 0 to high(consensus) do
  begin
    LText := Format('%0:12s', [NConsensus[counter]]);
    ToLog('console', Format('%0:2s %s -> %s', [Counter.ToString,
      LText, Consensus[counter]]));
  end;
end;

procedure NewAddressFromKeys(inputline: String);
var
  Newadd: WalletData;
  PubKey, PrivKey: String;
begin
  Newadd := Default(WalletData);
  NewAdd.PublicKey := parameter(inputline, 1);
  NewAdd.PrivateKey := parameter(inputline, 2);
  NewAdd.Hash := GetAddressFromPublickey(NewAdd.PublicKey);
  InsertToWallArr(NewAdd);
  S_Wallet := True;
  U_DirPanel := True;
end;

procedure TestHashGeneration(inputline: String);
var
  NewAddress: WalletData;
  PubKey, PriKey: String;
  counter: Integer;
  FutureHAsh: String;
  Correct: Integer = 0;
  Fails1: Integer = 0;
begin
  BeginPerformance('TestHashGeneration');
  for counter := 1 to StrToIntDef(Parameter(inputline, 1), 100) do
  begin
    NewAddress := Default(WalletData);
    NewAddress.Hash := GenerateNewAddress(PubKey, PriKey);
    NewAddress.PublicKey := pubkey;
    NewAddress.PrivateKey := PriKey;
    FutureHash := FutureGetAddressFromPublicKey(pubkey);
    if NewAddress.Hash = FutureHash then Inc(Correct)
    else
    begin
      Inc(Fails1);
      ToLog('console', format('%s -> New %s', [NewAddress.Hash, FutureHash]));
      ToLog('console', format('Key -> %s', [pubkey]));
    end;
    Application.ProcessMessages;
    if counter mod 1000 = 0 then ToLog('console', format('Tested: %d', [counter]));
  end;
  ToLog('console', format('Correct: %d // Fails : %d ', [Correct, Fails1]));
  ToLog('console', format('%d ms', [EndPerformance('TestHashGeneration')]));
end;

procedure CompareHashes(inputline: String);
var
  pubkey: String;
  hashold, hashnew, hashfuture: String;
begin
  pubkey := parameter(inputline, 1);
  hashold := GetAddressFromPublicKey(pubkey);
  hashnew := NewGetAddressFromPublicKey(pubkey);
  hashfuture := FutureGetAddressFromPublicKey(pubkey);
  ToLog('console', format('Original : %s', [hashold]));
  ToLog('console', format('BaseXtoX : %s', [hashnew]));
  ToLog('console', format('Future   : %s', [hashfuture]));

end;

 // Creates a multiaddress
 // Example: >getmulti 2,3 Nxxx,Nxxxx,Nxxxx
procedure CreateMultiAddress(Inputline: String);
var
  Source: String = '';
  FullSource: String = '';
  AddType: String;
  NewAdd: String;
  AddsNeeded: Integer;
  AddsTotal: Integer;
  ErrorMsg: String = '';
  NewAddress: WalletData;
begin
  AddType := parameter(Inputline, 1);
  AddType := StringReplace(AddType, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
  AddsNeeded := StrToIntDef(Parameter(AddType, 0), -1);
  AddsTotal := StrToIntDef(Parameter(AddType, 1), -1);
  if Addtype = '' then ErrorMsg := 'getmulti needed,total list,of,addresses';
  if ((AddsTotal < 2) or (AddsTotal > 7)) then
    ErrorMsg := 'Wrong number of total addresses';
  if ((AddsNeeded < 1) or (AddsNeeded >= AddsTotal)) then
    ErrorMsg := 'Wrong number of needed addresses';
  if ErrorMsg <> '' then
  begin
    ToLog('Console', ErrorMsg);
    Exit;
  end;
  Source := parameter(Inputline, 2);
  if not GetMultiSource(Source, AddsTotal, FullSource) then
  begin
    ToLog('Console', 'Error: ' + FullSource);
    Exit;
  end;
  AddType := StringReplace(AddType, ' ', ',', [rfReplaceAll, rfIgnoreCase]);
  NewAdd := GetAddressFromPublicKey(AddType + ':' + FullSource, AddTypeMulti);
  if IsValidHashAddress(NewAdd) then
  begin
    ToLog('Console', 'New multiAddress: ' + NewAdd);
    NewAddress := Default(WalletData);
    NewAddress.Hash := NewAdd;
    NewAddress.PublicKey := AddType + ':' + FullSource;
    InsertToWallArr(NewAddress);
    S_Wallet := True;
    U_DirPanel := True;
  end
  else
    ToLog('Console', 'Something went wrong...');
end;

// PSOs testing functions

procedure TestNewPSO(Dataline: String);
var
  LocalParams: String;
  LOrder: TOrderData;
begin
  LocalPArams := '1:' + UTCtimeStr + ';' + '2:' + IntToStr(MyLastBlock) +
    ';' + '3:1;' + '4:500;' + '5:2016;';
  AddNewPSO(1, GetWallArrIndex(0).Hash, MyLastBlock + 2016, LocalPArams);
  SavePSOFileToDisk(MyLastBlock);
  ToLog('console', 'Added');
end;

procedure GetPSOs();
var
  Counter: Integer;
begin
  //GEtPSOHEadersFromFile;
  ToLog('console', 'PSOSs list');
  ToLog('console', 'Block: ' + GetPSOHeaders.Block.ToString);
  ToLog('console', 'MNs  : ' + GetPSOHeaders.MNsLock.ToString);
  ToLog('console', 'PSOs : ' + GetPSOHeaders.Count.ToString);
  for counter := 0 to length(PSOsArray) - 1 do
  begin
    ToLog('console', PSOSArray[counter].Mode.ToString + ',' +
      PSOSArray[counter].Hash + ',' + PSOSArray[counter].owner +
      ',' + PSOSArray[counter].Expire.ToString + ',' + PSOSArray[counter].Params);
  end;
end;

procedure ShowGVTInfo();
var
  counter: Integer;
  Availables: Integer;
begin
  Availables := CountAvailableGVTs;
  ToLog('console', format('Available: %d', [Availables]));
  ToLog('console', 'Buy  : ' + Int2Curr(GetGVTPrice(Availables)));
  ToLog('console', 'Sell : ' + Int2Curr(GetGVTPrice(Availables, True)));
  Exit;
  for counter := 40 downto 1 do
  begin
    ToLog('console', counter.tostring + ' : ' + Int2curr(GetGVTPrice(counter)));
  end;

end;

procedure ClearPSOs();
begin
  EnterCriticalSection(CS_PSOsArray);
  Setlength(PSOsArray, 0);
  LeaveCriticalSection(CS_PSOsArray);
end;

procedure ShowMNsLocked();
var
  counter: Integer;
begin
  EnterCriticalSection(CS_LockedMNs);
  for counter := 0 to length(MNSLockArray) - 1 do
  begin
    ToLog('console', MNSLockArray[counter].address + ' ' +
      MNSLockArray[counter].expire.ToString());
  end;
  LeaveCriticalSection(CS_LockedMNs);
end;

procedure ShowConsensusStats();
begin
  ToLog('Console', GetConsensus(8) + ' ' + Copy(GetMNsHash, 1, 5));
  ImportAddressesFromBackup(RPCBakDirectory);
end;

procedure ShowMNsChecks();
var
  counter: Integer;
begin
  for counter := 0 to length(ArrMNChecks) - 1 do
  begin
    ToLog('console', ArrMNChecks[counter].ValidatorIP + ',' +
      GetValidNodesCountOnCheck(ArrMNChecks[counter].ValidNodes).ToString);
  end;

end;

{$REGION Specific tests}

procedure Test_Headers();
var
  MyStream: TmemoryStream;
  Fsize: Int64;
begin
  MyStream := TMemoryStream.Create;
  FSize := GetHeadersAsMemStream(MyStream);
  ToLog('Console', 'File size: ' + Fsize.ToString());
  SaveStreamAsHeaders(MyStream);
  UpdateMyData();
  MyStream.Free;
end;

{$ENDREGION Specific tests}


end. // END UNIT
