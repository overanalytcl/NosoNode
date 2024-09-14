unit NosoNetwork;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  IdContext, IdGlobal, IdTCPClient, IdTCPServer,
  nosodebug, nosotime, nosogeneral, nosoheaders, nosocrypto, nosoblock, nosoconsensus,
  nosounit, nosonosoCFG, nosogvts, nosomasternodes, nosopsos;

type

  TThreadClientRead = class(TThread)
  private
    FSlot: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const CreatePaused: Boolean; const ConexSlot: Integer);
  end;

  TNodeData = packed record
    ip: String[15];
    port: String[8];
  end;

  Tconectiondata = packed record
    Autentic: Boolean;
    // si la conexion esta autenticada por un ping
    Connections: Integer;             // A cuantos pares esta conectado
    tipo: String[8];                   // Tipo: SER o CLI
    ip: String[20];                    // La IP del par
    lastping: String[15];              // UTCTime del ultimo ping
    context: TIdContext;               // Informacion para los canales cliente
    Lastblock: String[15];             // Numero del ultimo bloque
    LastblockHash: String[64];         // Hash del ultimo bloque
    SumarioHash: String[64];          // Hash del sumario de cuenta
    Pending: Integer;                  // Cantidad de operaciones pendientes
    Protocol: Integer;                // Numero de protocolo usado
    Version: String[8];
    ListeningPort: Integer;
    offset: Integer;                  // Segundos de diferencia a su tiempo
    ResumenHash: String[64];
    ConexStatus: Integer;
    IsBusy: Boolean;
    Thread: TThreadClientRead;
    MNsHash: String[5];
    MNsCount: Integer;
    BestHashDiff: String[32];
    MNChecksCount: Integer;
    GVTsHash: String[32];
    CFGHash: String[32];
    MerkleHash: String[32];
    PSOHash: String[32];
  end;

  TBotData = packed record
    ip: String[15];
    LastRefused: Int64;
  end;

  NodeServerEvents = class
    class procedure OnExecute(AContext: TIdContext);
    class procedure OnConnect(AContext: TIdContext);
    class procedure OnDisconnect(AContext: TIdContext);
    class procedure OnException(AContext: TIdContext);
  end;

function GetPendingCount(): Integer;
procedure ClearAllPending();
procedure SendPendingsToPeer(Slot: Int64);
function TranxAlreadyPending(TrxHash: String): Boolean;
function AddArrayPoolTXs(order: TOrderData): Boolean;
function TrxExistsInLastBlock(trfrhash: String): Boolean;

function GetPTCEcn(): String;
function IsValidProtocol(line: String): Boolean;
function GetPingString(): String;
function ProtocolLine(LCode: Integer): String;
procedure ProcessPing(LineaDeTexto: String; Slot: Integer; Responder: Boolean);
procedure ProcessIncomingLine(FSlot: Integer; LLine: String);
procedure SendMNsListToPeer(slot: Integer);
procedure SendMNChecksToPeer(Slot: Integer);
function GetVerificationMNLine(ToIp: String): String;
function IsAllSynced(): Integer;
function GetSyncTus(): String;

procedure ClearOutTextToSlot(slot: Integer);
function GetTextToSlot(slot: Integer): String;
procedure TextToSlot(Slot: Integer; LText: String);

function GetConexIndex(Slot: Integer): Tconectiondata;
procedure SetConexIndex(Slot: Integer; LData: Tconectiondata);
procedure SetConexIndexBusy(LSlot: Integer; Value: Boolean);
procedure SetConexIndexLastPing(LSlot: Integer; Value: String);
procedure SetConexReserved(LSlot: Integer; Reserved: Boolean);
procedure StartConexThread(LSlot: Integer);
procedure CloseSlot(Slot: Integer);
function GetTotalConexiones(): Integer;
function IsSlotConnected(number: Integer): Boolean;

procedure UpdateMyData();
function IsValidator(Ip: String): Boolean;
function ValidateMNCheck(Linea: String): String;

procedure InitNodeServer();
function ClientsCount: Integer;
function TryMessageToClient(AContext: TIdContext; message: String): Boolean;
function GetStreamFromClient(AContext: TIdContext; out LStream: TMemoryStream): Boolean;
procedure TryCloseServerConnection(AContext: TIdContext; closemsg: String = '');


procedure IncClientReadThreads();
procedure DecClientReadThreads();
function GetClientReadThreads(): Integer;

procedure AddToIncoming(Index: Integer; texto: String);
function GetIncoming(Index: Integer): String;
function LengthIncoming(Index: Integer): Integer;
procedure ClearIncoming(Index: Integer);

procedure UpdateBotData(IPUser: String);
procedure DeleteBots();
function BotExists(IPUser: String): Boolean;

procedure FillNodeList();
function NodesListLen(): Integer;
function NodesIndex(lIndex: Integer): TNodeData;

procedure InitializeElements();
procedure ClearElements();

const
  MaxConecciones = 99;
  Protocolo = 2;
  MainnetVersion = '0.4.4';

var
  // General
  Conexiones: array [1..MaxConecciones] of Tconectiondata;
  SlotLines: array [1..MaxConecciones] of TStringList;
  CanalCliente: array [1..MaxConecciones] of TIdTCPClient;
  ArrayOutgoing: array [1..MaxConecciones] of array of String;
  BotsList: array of TBotData;
  ArrayPoolTXs: array of TOrderData;
  ArrayMultiTXs: array of TMultiOrder;
  // Donwloading files
  DownloadHeaders: Boolean = False;
  DownloadSumary: Boolean = False;
  DownLoadBlocks: Boolean = False;
  DownLoadGVTs: Boolean = False;
  DownloadPSOs: Boolean = False;
  // Last time files request
  LastTimeMNHashRequestes: Int64 = 0;
  LastTimeBestHashRequested: Int64 = 0;
  LastTimeMNsRequested: Int64 = 0;
  LastTimeChecksRequested: Int64 = 0;
  LastRunMNVerification: Int64 = 0;
  LasTimeGVTsRequest: Int64 = 0;
  LastTimeRequestSumary: Int64 = 0;
  LastTimeRequestBlock: Int64 = 0;
  LastTimeRequestResumen: Int64 = 0;
  LastTimePendingRequested: Int64 = 0;
  LasTimePSOsRequest: Int64 = 0;
  LastBotClear: Int64 = 0;
  ForceCompleteHeadersDownload: Boolean = False;
  G_MNVerifications: Integer = 0;
  // Local data hashes
  MyLastBlock: Integer = 0;
  MyLastBlockHash: String = '';

  //MyGVTsHash      : string = '';
  //MyCFGHash       : string = '';
  MyPublicIP: String = '';
  // Local information
  LastBlockData: BlockHeaderData;
  OpenReadClientThreads: Integer = 0;
  // Critical sections
  CSClientReads: TRTLCriticalSection;
  CSIncomingArr: array[1..MaxConecciones] of TRTLCriticalSection;
  CSOutGoingArr: array[1..MaxConecciones] of TRTLCriticalSection;
  CSConexiones: TRTLCriticalSection;
  CSBotsList: TRTLCriticalSection;
  CSPending: TRTLCriticalSection;
  CS_MultiTRX: TRTLCriticalSection;
  // nodes list
  NodesList: array of TNodeData;
  CSNodesList: TRTLCriticalSection;
  // Node server
  NodeServer: TIdTCPServer;

implementation

uses
  MasterPaskalForm;
  // To be removed, only due to server dependancy until it is implemented

  {$REGION Pending Pool transactions}

function GetPendingCount(): Integer;
begin
  EnterCriticalSection(CSPending);
  Result := Length(ArrayPoolTXs);
  LeaveCriticalSection(CSPending);
end;

// Clear the pending transactions array safely
procedure ClearAllPending();
begin
  EnterCriticalSection(CSPending);
  SetLength(ArrayPoolTXs, 0);
  LeaveCriticalSection(CSPending);
end;

// Send pending transactions to peer, former PTC_SendPending
procedure SendPendingsToPeer(Slot: Int64);
var
  contador: Integer;
  Encab: String;
  Textline: String;
  TextOrder: String;
  CopyArrayPoolTXs: array of TOrderData;
begin
  Encab := GetPTCEcn;
  TextOrder := encab + 'ORDER ';
  if GetPendingCount > 0 then
  begin
    EnterCriticalSection(CSPending);
    SetLength(CopyArrayPoolTXs, 0);
    CopyArrayPoolTXs := copy(ArrayPoolTXs, 0, length(ArrayPoolTXs));
    LeaveCriticalSection(CSPending);
    for contador := 0 to Length(CopyArrayPoolTXs) - 1 do
    begin
      Textline := GetStringFromOrder(CopyArrayPoolTXs[contador]);
      if (CopyArrayPoolTXs[contador].OrderType = 'CUSTOM') then
      begin
        TextToSlot(slot, Encab + '$' + TextLine);
      end;
      if (CopyArrayPoolTXs[contador].OrderType = 'TRFR') then
      begin
        if CopyArrayPoolTXs[contador].TrxLine = 1 then
          TextOrder := TextOrder + IntToStr(CopyArrayPoolTXs[contador].OrderLines) + ' ';
        TextOrder := TextOrder + '$' + GetStringfromOrder(
          CopyArrayPoolTXs[contador]) + ' ';
        if CopyArrayPoolTXs[contador].OrderLines =
          CopyArrayPoolTXs[contador].TrxLine then
        begin
          Setlength(TextOrder, length(TextOrder) - 1);
          TextToSlot(slot, TextOrder);
          TextOrder := encab + 'ORDER ';
        end;
      end;
      if (CopyArrayPoolTXs[contador].OrderType = 'SNDGVT') then
      begin
        TextToSlot(slot, Encab + '$' + TextLine);
      end;
    end;
    SetLength(CopyArrayPoolTXs, 0);
  end;
end;

function TranxAlreadyPending(TrxHash: String): Boolean;
var
  cont: Integer;
begin
  Result := False;
  if GetPendingCount > 0 then
  begin
    EnterCriticalSection(CSPending);
    for cont := 0 to GetPendingCount - 1 do
    begin
      if TrxHash = ArrayPoolTXs[cont].TrfrID then
      begin
        Result := True;
        break;
      end;
    end;
    LeaveCriticalSection(CSPending);
  end;
end;

// Add a new trx to the pending pool
function AddArrayPoolTXs(order: TOrderData): Boolean;
var
  counter: Integer = 0;
  ToInsert: Boolean = False;
  LResult: Integer = 0;
begin
  BeginPerformance('AddArrayPoolTXs');
  if order.TimeStamp < LastBlockData.TimeStart then exit;
  if TrxExistsInLastBlock(order.TrfrID) then exit;
  if ((BlockAge > 585) and (order.TimeStamp < LastBlockData.TimeStart + 540)) then exit;
  if not TranxAlreadyPending(order.TrfrID) then
  begin
    EnterCriticalSection(CSPending);
    while counter < length(ArrayPoolTXs) do
    begin
      if order.TimeStamp < ArrayPoolTXs[counter].TimeStamp then
      begin
        ToInsert := True;
        LResult := counter;
        break;
      end
      else if order.TimeStamp = ArrayPoolTXs[counter].TimeStamp then
      begin
        if order.OrderID < ArrayPoolTXs[counter].OrderID then
        begin
          ToInsert := True;
          LResult := counter;
          break;
        end
        else if order.OrderID = ArrayPoolTXs[counter].OrderID then
        begin
          if order.TrxLine < ArrayPoolTXs[counter].TrxLine then
          begin
            ToInsert := True;
            LResult := counter;
            break;
          end;
        end;
      end;
      Inc(counter);
    end;
    if not ToInsert then LResult := length(ArrayPoolTXs);
    Insert(order, ArrayPoolTXs, LResult);
    LeaveCriticalSection(CSPending);
    Result := True;
    //VerifyIfPendingIsMine(order);
  end;
  EndPerformance('AddArrayPoolTXs');
end;

// Check if the TRxID exists in the last block
function TrxExistsInLastBlock(trfrhash: String): Boolean;
var
  ArrayLastBlockTrxs: TBlockOrdersArray;
  cont: Integer;
begin
  Result := False;
  ArrayLastBlockTrxs := Default(TBlockOrdersArray);
  ArrayLastBlockTrxs := GetBlockTrxs(MyLastBlock);
  for cont := 0 to length(ArrayLastBlockTrxs) - 1 do
  begin
    if ArrayLastBlockTrxs[cont].TrfrID = trfrhash then
    begin
      Result := True;
      break;
    end;
  end;
end;

{$ENDREGION Pending Pool transactions}

{$REGION Pending Multi transactions}

function GetMultiTrxCount(): Integer;
begin
  EnterCriticalSection(CS_MultiTRX);
  Result := Length(ArrayMultiTXs);
  LeaveCriticalSection(CS_MultiTRX);
end;

// Clear the pending transactions array safely
procedure ClearAllMultiTrx();
begin
  EnterCriticalSection(CS_MultiTRX);
  SetLength(ArrayMultiTXs, 0);
  LeaveCriticalSection(CS_MultiTRX);
end;

{$ENDREGION}

{$REGION Protocol}

// Returns protocolo message header
function GetPTCEcn(): String;
begin
  Result := 'PSK ' + IntToStr(protocolo) + ' ' + MainnetVersion +
    NodeRelease + ' ' + UTCTimeStr + ' ';
end;

function IsValidProtocol(line: String): Boolean;
begin
  if ((copy(line, 1, 4) = 'PSK ') or (copy(line, 1, 4) = 'NOS ')) then Result := True
  else
    Result := False;
end;

function GetPingString(): String;
var
  LPort: Integer = 0;
begin
  if Form1.Server.Active then Lport := Form1.Server.DefaultPort
  else
    Lport := -1;
  Result := IntToStr(GetTotalConexiones()) + ' ' + IntToStr(MyLastBlock) +
    ' ' + MyLastBlockHash + ' ' + MySumarioHash + ' ' + GetPendingCount.ToString +
    ' ' + GetResumenHash + ' ' + IntToStr(MyConStatus) + ' ' +
    IntToStr(Lport) + ' ' + copy(GetMNsHash, 0, 5) + ' ' +
    IntToStr(GetMNsListLength) + ' ' + 'null' + ' ' + //GetNMSData.Diff
    GetMNsChecksCount.ToString + ' ' + MyGVTsHash + ' ' +
    Copy(HashMD5String(GetCFGDataStr), 0, 5) + ' ' + Copy(PSOFileHash, 0, 5);
end;

function ProtocolLine(LCode: Integer): String;
var
  Specific: String = '';
  Header: String = '';
begin
  Header := 'PSK ' + IntToStr(protocolo) + ' ' + MainnetVersion +
    'zzz ' + UTCTimeStr + ' ';
  if LCode = 0 then Specific := '';                                 //OnlyHeaders
  if LCode = 3 then Specific := '$PING ' + GetPingString;             //Ping
  if LCode = 4 then Specific := '$PONG ' + GetPingString;             //Pong
  if LCode = 5 then Specific := '$GETPENDING';                      //GetPending
  if LCode = 6 then Specific := '$GETSUMARY';                       //GetSumary
  if LCode = 7 then Specific := '$GETRESUMEN';                      //GetResumen
  if LCode = 8 then Specific := '$LASTBLOCK ' + IntToStr(mylastblock);//LastBlock
  if LCode = 9 then Specific := '$CUSTOM ';                         //Custom
  if LCode = 11 then Specific := '$GETMNS';                         //GetMNs
  if LCode = 12 then Specific := '$BESTHASH';                       //BestHash
  if LCode = 13 then Specific := '$MNREPO ' + GetMNReportString(MyLastBlock);
  //MNReport
  if LCode = 14 then Specific := '$MNCHECK ';                       //MNCheck
  if LCode = 15 then Specific := '$GETCHECKS';                      //GetChecks
  if LCode = 16 then Specific := 'GETMNSFILE';                      //GetMNsFile
  if LCode = 17 then Specific := 'MNFILE';                              //MNFile
  if LCode = 18 then Specific := 'GETHEADUPDATE ' + MyLastBlock.ToString; //GetHeadUpdate
  if LCode = 19 then Specific := 'HEADUPDATE';                      //HeadUpdate
  if LCode = 20 then Specific := '$GETGVTS';                        //GetGVTs
  if LCode = 21 then Specific := '$SNDGVT ';
  if LCode = 30 then Specific := 'GETCFGDATA';                      //GetCFG
  if LCode = 31 then Specific := 'SETCFGDATA $';                    //SETCFG
  if LCode = 32 then Specific := '$GETPSOS';                        //GetPSOs
  Result := Header + Specific;
end;

procedure ProcessPing(LineaDeTexto: String; Slot: Integer; Responder: Boolean);
var
  NewData: Tconectiondata;
begin
  NewData := GetConexIndex(Slot);
  NewData.Autentic := True;
  NewData.Protocol := StrToIntDef(Parameter(LineaDeTexto, 1), 0);
  NewData.Version := Parameter(LineaDeTexto, 2);
  NewData.offset := StrToInt64Def(Parameter(LineaDeTexto, 3), UTCTime) - UTCTime;
  NewData.Connections := StrToIntDef(Parameter(LineaDeTexto, 5), 0);
  NewData.Lastblock := Parameter(LineaDeTexto, 6);
  NewData.LastblockHash := Parameter(LineaDeTexto, 7);
  NewData.SumarioHash := Parameter(LineaDeTexto, 8);
  NewData.Pending := StrToIntDef(Parameter(LineaDeTexto, 9), 0);
  NewData.ResumenHash := Parameter(LineaDeTexto, 10);
  NewData.ConexStatus := StrToIntDef(Parameter(LineaDeTexto, 11), 0);
  NewData.ListeningPort := StrToIntDef(Parameter(LineaDeTexto, 12), -1);
  NewData.MNsHash := Parameter(LineaDeTexto, 13);
  NewData.MNsCount := StrToIntDef(Parameter(LineaDeTexto, 14), 0);
  NewData.BestHashDiff := 'null'{15};
  NewData.MNChecksCount := StrToIntDef(Parameter(LineaDeTexto, 16), 0);
  NewData.lastping := UTCTimeStr;
  NewData.GVTsHash := Parameter(LineaDeTexto, 17);
  NewData.CFGHash := Parameter(LineaDeTexto, 18);
  NewData.PSOHash := Parameter(LineaDeTexto, 19);
  ;
  NewData.MerkleHash := HashMD5String(NewData.Lastblock + copy(
    NewData.ResumenHash, 0, 5) + copy(NewData.MNsHash, 0, 5) +
    copy(NewData.LastblockHash, 0, 5) + copy(NewData.SumarioHash, 0, 5) +
    copy(NewData.GVTsHash, 0, 5) + copy(NewData.CFGHash, 0, 5));
  SetConexIndex(Slot, NewData);
  if responder then
  begin
    TextToSlot(slot, ProtocolLine(4));
  end;
end;

procedure ProcessIncomingLine(FSlot: Integer; LLine: String);
var
  Protocol, PeerVersion, PeerTime, Command: String;
begin
  Protocol := Parameter(LLine, 1);
  PeerVersion := Parameter(LLine, 2);
  PeerTime := Parameter(LLine, 3);
  Command := ProCommand(LLine);
  if ((not IsValidProtocol(LLine)) and (not GetConexIndex(FSlot).Autentic)) then
  begin
    UpdateBotData(GetConexIndex(Fslot).ip);
    CloseSlot(FSlot);
  end
  else if UpperCase(LLine) = 'DUPLICATED' then CloseSlot(FSlot)
  else if Copy(UpperCase(LLine), 1, 10) = 'OLDVERSION' then CloseSlot(FSlot)
  else if Command = '$PING' then ProcessPing(LLine, FSlot, True)
  else if Command = '$PONG' then ProcessPing(LLine, FSlot, False)
  else if Command = '$GETPENDING' then SendPendingsToPeer(FSlot);
end;

procedure SendMNsListToPeer(slot: Integer);
var
  DataArray: array of String;
  counter: Integer;
begin
  if FillMnsListArray(DataArray) then
  begin
    for counter := 0 to length(DataArray) - 1 do
      TextToSlot(slot, GetPTCEcn + '$MNREPO ' + DataArray[counter]);
  end;
end;

procedure SendMNChecksToPeer(Slot: Integer);
var
  Counter: Integer;
  Texto: String;
begin
  if GetMNsChecksCount > 0 then
  begin
    EnterCriticalSection(CSMNsChecks);
    for counter := 0 to length(ArrMNChecks) - 1 do
    begin
      Texto := ProtocolLine(14) + GetStringFromMNCheck(ArrMNChecks[counter]);
      TextToSlot(slot, Texto);
    end;
    LeaveCriticalSection(CSMNsChecks);
  end;
end;

function GetVerificationMNLine(ToIp: String): String;
begin
  if IsAllSynced = 0 then
  begin
    Result := 'True ' + GetSyncTus + ' ' + LocalMN_Funds + ' ' +
      ToIp + ' ' + LocalMN_Sign;
    Inc(G_MNVerifications);
  end
  else
    Result := 'False';
end;

function IsAllSynced(): Integer;
begin
  Result := 0;
  if MyLastBlock <> StrToIntDef(GetConsensus(cLastBlock), 0) then Result := 1;
  if MyLastBlockHash <> GetConsensus(cLBHash) then Result := 2;
  if Copy(MySumarioHash, 0, 5) <> GetConsensus(cSumHash) then Result := 3;
  if Copy(GetResumenHash, 0, 5) <> GetConsensus(cHeaders) then Result := 4;
  {
  if Copy(GetMNsHash,1,5) <>  NetMNsHash.value then result := 5;
  if MyGVTsHash <> NetGVTSHash.Value then result := 6;
  if MyCFGHash <> NETCFGHash.Value then result := 7;
  }
end;

function GetSyncTus(): String;
begin
  Result := '';
  try
    Result := MyLastBlock.ToString + Copy(GetResumenHash, 1, 3) +
      Copy(MySumarioHash, 1, 3) + Copy(MyLastBlockHash, 1, 3);
  except
    ON E: Exception do
    begin
      ToDeepDeb('NosoNetwork,GetSyncTus,' + e.Message);
    end;
  end; {TRY}
end;

{$ENDREGION Protocol}

{$REGION ArrayOutgoing}

procedure ClearOutTextToSlot(slot: Integer);
begin
  EnterCriticalSection(CSOutGoingArr[slot]);
  SetLength(ArrayOutgoing[slot], 0);
  LeaveCriticalSection(CSOutGoingArr[slot]);
end;

function GetTextToSlot(slot: Integer): String;
begin
  Result := '';
  if ((Slot >= 1) and (slot <= MaxConecciones)) then
  begin
    EnterCriticalSection(CSOutGoingArr[slot]);
    if length(ArrayOutgoing[slot]) > 0 then
    begin
      Result := ArrayOutgoing[slot][0];
      Delete(ArrayOutgoing[slot], 0, 1);
    end;
    LeaveCriticalSection(CSOutGoingArr[slot]);
  end;
end;

procedure TextToSlot(Slot: Integer; LText: String);
begin
  if ((Slot >= 1) and (Slot <= 99)) then
  begin
    EnterCriticalSection(CSOutGoingArr[slot]);
    Insert(LText, ArrayOutgoing[slot], length(ArrayOutgoing[slot]));
    LeaveCriticalSection(CSOutGoingArr[slot]);
  end;
end;

{$ENDREGION ArrayOutgoing}

{$REGION Conexiones control}

function GetConexIndex(Slot: Integer): Tconectiondata;
begin
  if ((slot < 1) or (Slot > MaxConecciones)) then Result := Default(Tconectiondata);
  EnterCriticalSection(CSConexiones);
  Result := Conexiones[Slot];
  LeaveCriticalSection(CSConexiones);
end;

procedure SetConexIndex(Slot: Integer; LData: Tconectiondata);
begin
  if ((slot < 1) or (Slot > MaxConecciones)) then exit;
  EnterCriticalSection(CSConexiones);
  Conexiones[Slot] := LData;
  LeaveCriticalSection(CSConexiones);
end;

procedure SetConexIndexBusy(LSlot: Integer; Value: Boolean);
begin
  if ((Lslot < 1) or (LSlot > MaxConecciones)) then exit;
  EnterCriticalSection(CSConexiones);
  Conexiones[LSlot].IsBusy := Value;
  LeaveCriticalSection(CSConexiones);
end;

procedure SetConexIndexLastPing(LSlot: Integer; Value: String);
begin
  if ((Lslot < 1) or (LSlot > MaxConecciones)) then exit;
  EnterCriticalSection(CSConexiones);
  Conexiones[LSlot].lastping := Value;
  LeaveCriticalSection(CSConexiones);
end;

procedure SetConexReserved(LSlot: Integer; Reserved: Boolean);
var
  ToShow: String = '';
begin
  if ((Lslot < 1) or (LSlot > MaxConecciones)) then exit;
  if reserved then ToShow := 'RES';
  EnterCriticalSection(CSConexiones);
  Conexiones[LSlot].tipo := ToShow;
  LeaveCriticalSection(CSConexiones);
end;

procedure StartConexThread(LSlot: Integer);
begin
  if ((Lslot < 1) or (LSlot > MaxConecciones)) then exit;
  EnterCriticalSection(CSConexiones);
  Conexiones[Lslot].Thread := TThreadClientRead.Create(True, Lslot);
  Conexiones[Lslot].Thread.FreeOnTerminate := True;
  Conexiones[Lslot].Thread.Start;
  LeaveCriticalSection(CSConexiones);
end;

procedure CloseSlot(Slot: Integer);
begin
  if ((slot < 1) or (Slot > MaxConecciones)) then exit;
  BeginPerformance('CloseSlot');
  try
    if GetConexIndex(Slot).tipo = 'CLI' then
    begin
      ClearIncoming(slot);
      GetConexIndex(Slot).context.Connection.Disconnect;
      Sleep(10);
    end;
    if GetConexIndex(Slot).tipo = 'SER' then
    begin
      ClearIncoming(slot);
      CanalCliente[Slot].IOHandler.InputBuffer.Clear;
      CanalCliente[Slot].Disconnect;
    end;
  except
    on E: Exception do
      ToDeepDeb('NosoNetwork,CloseSlot,' + E.Message);
  end;{Try}
  SetConexIndex(Slot, Default(Tconectiondata));
  EndPerformance('CloseSlot');
end;

function GetTotalConexiones(): Integer;
var
  counter: Integer;
begin
  BeginPerformance('GetTotalConexiones');
  Result := 0;
  for counter := 1 to MaxConecciones do
    if IsSlotConnected(Counter) then Inc(Result);
  EndPerformance('GetTotalConexiones');
end;

function IsSlotConnected(number: Integer): Boolean;
begin
  Result := False;
  if ((number < 1) or (number > MaxConecciones)) then exit;
  if ((GetConexIndex(number).tipo = 'SER') or (GetConexIndex(number).tipo = 'CLI')) then
    Result := True;
end;

{$ENDREGION Conexiones control}

{$REGION General Data}

// Updates local data hashes
procedure UpdateMyData();
begin
  MyLastBlockHash := HashMD5File(BlockDirectory + IntToStr(MyLastBlock) + '.blk');
  LastBlockData := LoadBlockDataHeader(MyLastBlock);
  SetResumenHash;
  if GetResumenHash = GetConsensus(5) then
    ForceCompleteHeadersDownload := False;
  //MyMNsHash       := HashMD5File(MasterNodesFilename);
  //MyCFGHash       := Copy(HAshMD5String(GetCFGDataStr),1,5);
end;

function IsValidator(Ip: String): Boolean;
begin
  Result := False;
  if IsSeedNode(IP) then Result := True;
end;

// Verify if a validation report is correct
function ValidateMNCheck(Linea: String): String;
var
  CheckData: TMNCheck;
  StartPos: Integer;
  ReportInfo: String;
  ErrorCode: Integer = 0;
begin
  Result := '';
  StartPos := Pos('$', Linea);
  ReportInfo := copy(Linea, StartPos, length(Linea));
  CheckData := GetMNCheckFromString(Linea);
  if MnsCheckExists(CheckData.ValidatorIP) then exit;
  if not IsValidator(CheckData.ValidatorIP) then ErrorCode := 1;
  if CheckData.Block <> MyLastBlock then ErrorCode := 2;
  if GetAddressFromPublicKey(CheckData.PubKey) <> CheckData.SignAddress then
    ErrorCode := 3;
  if not VerifySignedString(CheckData.ValidNodes, CheckData.Signature,
    CheckData.PubKey) then
    ErrorCode := 4;
  if ErrorCode = 0 then
  begin
    Result := ReportInfo;
    AddMNCheck(CheckData);
    //if form1.Server.Active then
    //  outGOingMsjsAdd(GetPTCEcn+ReportInfo);
  end;
end;

{$ENDREGION General Data}

{$REGION Node Server}

procedure InitNodeServer();
begin
  NodeServer := TIdTCPServer.Create(nil);
  NodeServer.DefaultPort := 8080;
  NodeServer.Active := False;
  NodeServer.UseNagle := True;
  NodeServer.TerminateWaitTime := 10000;
  NodeServer.OnExecute := @NodeServerEvents.OnExecute;
  NodeServer.OnConnect := @NodeServerEvents.OnConnect;
  NodeServer.OnDisconnect := @form1.IdTCPServer1Disconnect;
  NodeServer.OnException := @Form1.IdTCPServer1Exception;
end;

// returns the number of active connections
function ClientsCount: Integer;
var
  Clients: TList;
begin
  Clients := Nodeserver.Contexts.LockList;
  try
    Result := Clients.Count;
  except
    ON E: Exception do
      ToDeepDeb('NosoNetwork,ClientsCount,' + E.Message);
  end; {TRY}
  Nodeserver.Contexts.UnlockList;
end;

// Try message to client safely
function TryMessageToClient(AContext: TIdContext; message: String): Boolean;
begin
  Result := True;
  try
    Acontext.Connection.IOHandler.WriteLn(message);
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;{Try}
end;

// Get stream from client
function GetStreamFromClient(AContext: TIdContext; out LStream: TMemoryStream): Boolean;
begin
  Result := False;
  LStream.Clear;
  try
    AContext.Connection.IOHandler.ReadStream(LStream);
    Result := True;
  except
    on E: Exception do
      ToDeepDeb('NosoNetwork,GetStreamFromClient,' + E.Message);
  end;
end;

// Trys to close a server connection safely
procedure TryCloseServerConnection(AContext: TIdContext; closemsg: String = '');
begin
  try
    if closemsg <> '' then
      Acontext.Connection.IOHandler.WriteLn(closemsg);
    AContext.Connection.Disconnect();
    Acontext.Connection.IOHandler.InputBuffer.Clear;
  except
    on E: Exception do
      ToDeepDeb('NosoNetwork,TryCloseServerConnection,' + E.Message);
  end; {TRY}
end;

class procedure NodeServerEvents.OnExecute(AContext: TIdContext);
var
  LLine: String = '';
  IPUser: String = '';
  slot: Integer = 0;
  UpdateZipName: String = '';
  UpdateVersion: String = '';
  UpdateHash: String = '';
  UpdateClavePublica: String = '';
  UpdateFirma: String = '';
  MemStream: TMemoryStream;
  BlockZipName: String = '';
  GetFileOk: Boolean = False;
  GoAhead: Boolean;
  NextLines: array of String;
  LineToSend: String;
  LinesSent: Integer = 0;
  FTPTime, FTPSize, FTPSpeed: Int64;
begin
end;

class procedure NodeServerEvents.OnConnect(AContext: TIdContext);
begin

end;

class procedure NodeServerEvents.OnDisconnect(AContext: TIdContext);
begin

end;

class procedure NodeServerEvents.OnException(AContext: TIdContext);
begin

end;

{$ENDREGION Node Server}

{$REGION Thread Client read}

constructor TThreadClientRead.Create(const CreatePaused: Boolean;
  const ConexSlot: Integer);
begin
  inherited Create(CreatePaused);
  FSlot := ConexSlot;
end;

function LineToClient(Slot: Integer; LLine: String): Boolean;
begin
  Result := False;
  try
    CanalCliente[Slot].IOHandler.WriteLn(LLine);
    Result := True;
  except
    on E: Exception do
      ToDeepDeb('NosoNetwork,LineToClient,' + E.Message);
  end;
end;

function GetStreamFromClient(Slot: Integer; out LStream: TMemoryStream): Boolean;
begin
  Result := False;
  LStream.Clear;
  try
    CanalCliente[Slot].IOHandler.ReadStream(LStream);
    Result := True;
  except
    on E: Exception do
      ToDeepDeb('NosoNetwork,GetStreamFromClient,' + E.Message);
  end;
end;

function SendLineToClient(FSlot: Integer; LLine: String): Boolean;
begin
  Result := True;
  try
    CanalCliente[FSlot].IOHandler.Writeln(LLine);
  except
    ON E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoNetwork,SendLineToClient,' + E.Message);
    end;
  end;
end;

procedure TThreadClientRead.Execute;
var
  LLine: String;
  MemStream: TMemoryStream;
  OnBuffer: Boolean = True;
  Errored: Boolean;
  downloaded: Boolean;
  LineToSend: String;
  KillIt: Boolean = False;
  SavedToFile: Boolean;
  ThreadName: String = '';
  LastActive: Int64 = 0;
begin
  ThreadName := 'ReadClient ' + FSlot.ToString + ' ' + UTCTimeStr;
  CanalCliente[FSlot].ReadTimeout := 1000;
  CanalCliente[FSlot].IOHandler.MaxLineLength := Maxint;
  IncClientReadThreads;
  AddNewOpenThread(ThreadName, UTCTime);
  LastActive := UTCTime;
  repeat
  try
    sleep(10);
    OnBuffer := True;
    if CanalCliente[FSlot].IOHandler.InputBufferIsEmpty then
    begin
      CanalCliente[FSlot].IOHandler.CheckForDataOnSource(1000);
      if CanalCliente[FSlot].IOHandler.InputBufferIsEmpty then
      begin
        OnBuffer := False;
        repeat
          LineToSend := GetTextToSlot(Fslot);
          if LineToSend <> '' then
          begin
            if not SendLineToClient(FSlot, LineToSend) then
            begin
              killit := True;
              Conexiones[FSlot].Thread.Terminate;
              break;
            end;
          end;
        until LineToSend = '';
      end;
    end;
    if OnBuffer then
    begin
      while not CanalCliente[FSlot].IOHandler.InputBufferIsEmpty do
      begin
        SetConexIndexBusy(FSlot, True);
        SetConexIndexLastPing(fSlot, UTCTimeStr);
        LLine := '';
        try
          LLine := CanalCliente[FSlot].IOHandler.ReadLn(IndyTextEncoding_UTF8);
        except
          on E: Exception do
          begin
            SetConexIndexBusy(FSlot, False);
            Conexiones[FSlot].Thread.Terminate;
            KillIt := True;
            break;
          end;
        end; {TRY}
        if LLine <> '' then
        begin
          LastActive := UTCTime;
          UpdateOpenThread(ThreadName, UTCTime);
          CanalCliente[FSlot].ReadTimeout := 10000;
          if Parameter(LLine, 0) = 'RESUMENFILE' then
          begin
            DownloadHeaders := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveStreamAsHeaders(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              UpdateMyData();
            end
            else
              killit := True;
            LastTimeRequestResumen := 0;
            MemStream.Free;
            DownloadHeaders := False;
          end

          else if Parameter(LLine, 0) = 'SUMARYFILE' then
          begin
            DownloadSumary := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveSummaryToFile(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              UpdateMyData();
              CreateSumaryIndex();
            end
            else
              killit := True;
            LastTimeRequestSumary := 0;
            MemStream.Free;
            DownloadSumary := False;
          end

          else if Parameter(LLine, 0) = 'PSOSFILE' then
          begin
            DownloadPSOs := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SavePSOsToFile(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              LoadPSOFileFromDisk;
              UpdateMyData();
            end
            else
              killit := True;
            LasTimePSOsRequest := 0;
            MemStream.Free;
            DownloadPSOs := False;
          end
          else if Parameter(LLine, 0) = 'GVTSFILE' then
          begin
            DownloadGVTs := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveStreamAsGVTs(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              GetGVTsFileData;
            end
            else
              killit := True;
            LasTimeGVTsRequest := 0;
            MemStream.Free;
            DownloadGVTs := False;
          end

          else if Parameter(LLine, 0) = 'BLOCKZIP' then
          begin
            DownLoadBlocks := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveStreamAsZipBlocks(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              if UnzipFile(BlockDirectory + 'blocks.zip', True) then
              begin
                MyLastBlock := GetMyLastUpdatedBlock();
                MyLastBlockHash :=
                  HashMD5File(BlockDirectory + IntToStr(MyLastBlock) + '.blk');
                UpdateMyData();
              end;
            end
            else
              killit := True;
            LastTimeRequestBlock := 0;
            MemStream.Free;
            DownLoadBlocks := False;
          end // END RECEIVING BLOCKS
          else
          begin
            //ProcessIncomingLine(FSlot,LLine);
            AddToIncoming(FSlot, LLine);
          end;
        end;
        SetConexIndexBusy(FSlot, False);
      end; // end while client is not empty
    end;   // End OnBuffer
    if LastActive + 30 < UTCTime then killit := True;
    if GetConexIndex(Fslot).tipo <> 'SER' then killit := True;
    if not CanalCliente[FSlot].Connected then killit := True;
  except
    ON E: Exception do
    begin
      ToDeepDeb('NosoNetwork,TThreadClientRead,' + E.Message);
      KillIt := True;
    end;
  end;
  until ((terminated) or (KillIt));
  CloseSlot(Fslot);
  DecClientReadThreads;
  CloseOpenThread(ThreadName);
end;

//Procedure ProcessLine(LLine:String;)

{$ENDREGION Thread Client read}

{$REGION ClientReadThreads}

procedure IncClientReadThreads();
begin
  EnterCriticalSection(CSClientReads);
  Inc(OpenReadClientThreads);
  LeaveCriticalSection(CSClientReads);
end;

procedure DecClientReadThreads();
begin
  EnterCriticalSection(CSClientReads);
  Dec(OpenReadClientThreads);
  LeaveCriticalSection(CSClientReads);
end;

function GetClientReadThreads(): Integer;
begin
  EnterCriticalSection(CSClientReads);
  Result := OpenReadClientThreads;
  LeaveCriticalSection(CSClientReads);
end;

{$ENDREGION ClientReadThreads}

{$REGION Incoming/outgoing info}

procedure AddToIncoming(Index: Integer; texto: String);
begin
  EnterCriticalSection(CSIncomingArr[Index]);
  SlotLines[Index].Add(texto);
  LeaveCriticalSection(CSIncomingArr[Index]);
end;

function GetIncoming(Index: Integer): String;
begin
  Result := '';
  if LengthIncoming(Index) > 0 then
  begin
    EnterCriticalSection(CSIncomingArr[Index]);
    Result := SlotLines[Index][0];
    SlotLines[index].Delete(0);
    LeaveCriticalSection(CSIncomingArr[Index]);
  end;
end;

function LengthIncoming(Index: Integer): Integer;
begin
  EnterCriticalSection(CSIncomingArr[Index]);
  Result := SlotLines[Index].Count;
  LeaveCriticalSection(CSIncomingArr[Index]);
end;

procedure ClearIncoming(Index: Integer);
begin
  EnterCriticalSection(CSIncomingArr[Index]);
  SlotLines[Index].Clear;
  LeaveCriticalSection(CSIncomingArr[Index]);
end;


{$ENDREGION Incoming/outgoing info}

{$REGION Bots array}

procedure UpdateBotData(IPUser: String);
var
  contador: Integer = 0;
  updated: Boolean = False;
begin
  if IsSeedNode(IPUser) then exit;
  EnterCriticalSection(CSBotsList);
  for contador := 0 to length(BotsList) - 1 do
  begin
    if BotsList[Contador].ip = IPUser then
    begin
      BotsList[Contador].LastRefused := UTCTime;
      Updated := True;
    end;
  end;
  LeaveCriticalSection(CSBotsList);
  if not updated then
  begin
    EnterCriticalSection(CSBotsList);
    SetLength(BotsList, Length(BotsList) + 1);
    BotsList[Length(BotsList) - 1].ip := IPUser;
    BotsList[Length(BotsList) - 1].LastRefused := UTCTime;
    LeaveCriticalSection(CSBotsList);
  end;
end;

procedure DeleteBots();
begin
  EnterCriticalSection(CSBotsList);
  SetLength(BotsList, 0);
  LeaveCriticalSection(CSBotsList);
  LastBotClear := UTCTime;
end;

function BotExists(IPUser: String): Boolean;
var
  contador: Integer = 0;
begin
  Result := False;
  EnterCriticalSection(CSBotsList);
  for contador := 0 to length(BotsList) - 1 do
    if BotsList[contador].ip = IPUser then
    begin
      Result := True;
      break;
    end;
  LeaveCriticalSection(CSBotsList);
end;

{$ENDREGION Bots array}

{$REGION Nodes list}

procedure FillNodeList();
var
  counter: Integer;
  ThisNode: String = '';
  Thisport: Integer;
  continuar: Boolean = True;
  NodeToAdd: TNodeData;
  SourceStr: String = '';
begin
  counter := 0;
  SourceStr := Parameter(GetCFGDataStr, 1) + GetVerificatorsText;
  SourceStr := StringReplace(SourceStr, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
  EnterCriticalSection(CSNodesList);
  SetLength(NodesList, 0);
  repeat
    ThisNode := parameter(SourceStr, counter);
    ThisNode := StringReplace(ThisNode, ';', ' ', [rfReplaceAll, rfIgnoreCase]);
    ThisPort := StrToIntDef(Parameter(ThisNode, 1), 8080);
    ThisNode := Parameter(ThisNode, 0);
    if thisnode = '' then continuar := False
    else
    begin
      NodeToAdd.ip := ThisNode;
      NodeToAdd.port := IntToStr(ThisPort);
      Insert(NodeToAdd, NodesList, Length(NodesList));
      counter += 1;
    end;
  until not continuar;
  LeaveCriticalSection(CSNodesList);
end;

function NodesListLen(): Integer;
begin
  EnterCriticalSection(CSNodesList);
  Result := Length(NodesList);
  LeaveCriticalSection(CSNodesList);
end;

function NodesIndex(lIndex: Integer): TNodeData;
begin
  Result := Default(TNodeData);
  if lIndex >= NodesListLen then exit;
  EnterCriticalSection(CSNodesList);
  Result := NodesList[lIndex];
  LeaveCriticalSection(CSNodesList);
end;

{$ENDREGION Nodes list}

{$REGION Unit related}

procedure InitializeElements();
var
  counter: Integer;
begin
  InitCriticalSection(CSClientReads);
  InitCriticalSection(CSConexiones);
  InitCriticalSection(CSBotsList);
  InitCriticalSection(CSPending);
  InitCriticalSection(CS_MultiTRX);
  InitCriticalSection(CSNodesList);
  SetLength(BotsList, 0);
  Setlength(ArrayPoolTXs, 0);
  SetLength(ArrayMultiTXs, 0);
  Setlength(NodesList, 0);
  for counter := 1 to MaxConecciones do
  begin
    InitCriticalSection(CSIncomingArr[counter]);
    SlotLines[counter] := TStringList.Create;
    CanalCliente[counter] := TIdTCPClient.Create(nil);
    InitCriticalSection(CSOutGoingArr[counter]);
    SetLength(ArrayOutgoing[counter], 0);
  end;
end;

procedure ClearElements();
var
  counter: Integer;
begin
  DoneCriticalSection(CSClientReads);
  DoneCriticalSection(CSConexiones);
  DoneCriticalSection(CSBotsList);
  DoneCriticalSection(CS_MultiTRX);
  DoneCriticalSection(CSNodesList);
  for counter := 1 to MaxConecciones do
  begin
    DoneCriticalSection(CSIncomingArr[counter]);
    SlotLines[counter].Free;
    CanalCliente[counter].Free;
    DoneCriticalSection(CSOutGoingArr[counter]);
  end;
end;

{$ENDREGION Unit related}

initialization
  InitializeElements();


finalization
  ClearElements;

end. // End unit
