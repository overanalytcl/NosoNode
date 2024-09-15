unit Noso.Network;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  IdContext, IdGlobal, IdTCPClient, IdTCPServer,
  Noso.Debug, Noso.Time, Noso.General, Noso.Headers, Noso.Crypto,
  Noso.Block, Noso.Consensus,
  Noso.Summary, Noso.Config, Noso.Gvt, Noso.Masternodes, Noso.Pso;

type

  // Thread class to handle client reading operations
  {
    @abstract(Thread for handling client read operations in a connection slot.)
    @member(FSlot The connection slot that this thread is responsible for.)
    @member(Execute The main procedure that runs the thread’s logic.)
    @member(Create Initializes the thread with a connection slot, optionally in a paused state.)
  }
  TClientReadThread = class(TThread)
  private
    FSlot: Integer;  //< Connection slot for the thread
  protected
    procedure Execute; override;  //< Core thread execution logic
  public
    {
      @param(CreatePaused If True, the thread will be created in a paused state.)
      @param(ConnectionSlot The slot number representing the connection this thread is handling.)
    }
    constructor Create(const CreatePaused: Boolean; const ConnectionSlot: Integer);
  end;

  {
    @abstract(Stores basic data for a network node.)
    @member(IpAddress The IP address of the node.)
    @member(Port The port the node is listening on.)
  }
  TNodeData = packed record
    IpAddress: String[15];
    Port: String[8];
  end;

  { @abstract(Type representing a UTC timestamp.) }
  TTimeStamp = String[15];
  { @abstract(Type representing a short 5-character hash.) }
  THashIdentifier = String[5];
  { @abstract(Type representing a 15-character block number.) }
  TBlockNumber = String[15];
  { @abstract(Type representing a hash string (MD5, 32 chars, 128 bits).) }
  THashString = String[32];
  { @abstract(Type representing a hash string (SHA256?, 64 chars, 256 bits).) }
  TLongHashString = String[64];

  {
    @abstract(Represents the data of a network connection.)

    @member(IsAuthenticated Whether the connection has been authenticated via a ping.)
    @member(ActiveConnections The number of peers currently connected.)
    @member(ConnectionType The type of connection, either 'SER' for server or 'CLI' for client.)
    @member(IPAddress The IP address of the peer.)
    @member(LastPingTime The UTC time of the last ping.)
    @member(Context Client context information used for communication channels.)
    @member(LastBlockNumber The number of the last known block.)
    @member(LastBlockHash The hash of the last known block.)
    @member(SummaryHash The hash of the account summary.)
    @member(PendingOperations The number of pending operations for this connection.)
    @member(ProtocolVersion The version of the protocol being used.)
    @member(ClientVersion The software version of the client.)
    @member(ListeningPort The port the peer is listening on.)
    @member(TimeOffset The time difference in seconds from this peer.)
    @member(SummaryBlockHash The hash of the summary block.)
    @member(ConnectionStatus The status of the connection.)
    @member(IsBusy Whether the peer is currently busy.)
    @member(ReadThread The thread used for client reading operations.)
    @member(MasternodeShortHash The shortened hash for identifying the masternode.)
    @member(MasternodeCount The number of masternodes connected.)
    @member(BestHashDifficulty The best hash difficulty found.)
    @member(MasternodeChecksCount The number of checks performed on masternodes.)
    @member(GVTHash The hash of the GVT.)
    @member(ConfigurationHash The hash of the configuration file.)
    @member(MerkleTreeHash The hash of the Merkle tree.)
    @member(PSOHash The hash of the Proof of Stake (PoS) state.)
  }
  TConnectionData = packed record
    IsAuthenticated: Boolean;
    ActiveConnections: Integer;
    ConnectionType: String[8];
    IpAddress: String[20];
    LastPingTime: TTimeStamp;
    Context: TIdContext;
    LastBlockNumber: String[15];
    LastBlockHash: TLongHashString;
    SummaryHash: TLongHashString;
    PendingOperations: Integer;
    ProtocolVersion: Integer;
    ClientVersion: String[8];
    ListeningPort: Integer;
    TimeOffset: Integer;
    SummaryBlockHash: TLongHashString;
    ConnectionStatus: Integer;
    IsBusy: Boolean;
    ReadThread: TClientReadThread;
    MasternodeShortHash: THashIdentifier;
    MasternodeCount: Integer;
    BestHashDifficulty: THashString;
    MasternodesChecksCount: Integer;
    GVTHash: THashString;
    ConfigHash: THashString;
    MerkleTreeHash: THashString;
    PSOHash: THashString;
  end;

  {
    @abstract(Stores data related to a bot, such as its IP address and the timestamp of the last refused connection.)
    @member(IpAddress The IP address of the bot.)
    @member(LastRefused The timestamp (in Unix time) of the last time the bot's connection was refused.)
  }
  TBotData = packed record
    IpAddress: String[15];
    LastRefused: Int64;
  end;

  {
    @abstract(Handles various TCP server events for the node, such as connect, disconnect, execution, and exceptions.)
    @member(OnExecute Handles the execution of an operation on the server.)
    @member(OnConnect Called when a client connects to the server.)
    @member(OnDisconnect Called when a client disconnects from the server.)
    @member(OnException Handles exceptions that occur during the server operations.)

    @note(This class, for all intents and purposes, is a no-op. The procedures don't do anything.)
  }
  TNodeServerEvents = class
    class procedure OnExecute(AContext: TIdContext);
    class procedure OnConnect(AContext: TIdContext);
    class procedure OnDisconnect(AContext: TIdContext);
    class procedure OnException(AContext: TIdContext);
  end;

  { @abstract(An enum representing the synchronization status. Used in IsAllSynchronized.) }
  TSyncStatus = (
    ssSynchronized,          //< Fully synchronized (= 0)
    ssBlockHeightMismatch,   //< Block height mismatch (= 1)
    ssBlockHashMismatch,     //< Block hash mismatch (= 2)
    ssSummaryHashMismatch,   //< Summary hash mismatch (= 3)
    ssResumenHashMismatch    //< Resumen hash mismatch (= 4)
    );

{
  @abstract(Return the number of pending transactions.)

  @returns(Return the number of pending transactions in the pool.)
}
function GetPendingTransactionCount(): Integer;
{
  @abstract(Clears all pending transactions.)
}
procedure ClearAllPendingTransactions();
{
  @abstract(Sends pending transactions to the specified peer slot.)

  It copies the pending transaction pool safely, and then sends each transaction to the peer based on its type.

  @param(Slot The slot number of the peer to send transactions to.)
}
procedure SendPendingTransactionsToPeer(const Slot: Int64);
{
  @abstract(Checks if a transaction with the given hash already exists in the pending pool.)
  @note(The function is thread-safe and locks the critical section during the search.)

  @param(Hash The transfer ID of the transaction to check.)
  @returns(@true if the transaction is already pending, otherwise @false.)
}
function TransactionAlreadyPending(const Hash: String): Boolean;
{
  @abstract(Adds a new transaction to the pending pool in a thread-safe manner.)

  The function finds the correct insertion position based on timestamps and order IDs.

  @param(Order The transaction data to add to the pending pool.)
  @returns(@true if the transaction was successfully added, otherwise @false.)
}
function AddTransactionToPool(const Order: TOrderData): Boolean;
{
  @abstract(Checks if a transfer with the given hash exists in the last block.)

  @param(TransferHash The transfer ID to check for.)
  @returns(@true if the transfer exists in the last block, otherwise @false.)
}
function TransferExistsInLastBlock(const TransferHash: String): Boolean;

{
  @abstract(Returns the protocol header.)

  The protocol header is of the form "PSK P MN U", where P is the protocol version
  (currently 2), M is the Mainnet version (0.4.4), N is the node release (Ab7) and
  U is UTCTimeStr. For instance, a valid header is: "PSK 2 0.4.4Ab7 1726431338".

  @returns(The protocol header in the given format.)
}
function GetProtocolHeader(): String;
{
  @abstract(Checks if the current line is a valid protocol line (if it starts with PSK or NOS))

  @param(Line The line to validate.)
  @returns(@true if Line is a valid protocol line, and @false otherwise.)
}
function IsValidProtocol(const Line: String): Boolean;
{
  @abstract(Generates a ping string.)

  Generates a ping string containing details about the current node's state, including
  connections, last block, and transaction information.

  @returns(A formatted ping string.)
}
function GetPingString(): String;
{
  @abstract(Generates a specific protocol line based on the provided code.)

  The protocol line varies according to the command type represented by the code.

  @param(Code The integer code representing a specific protocol command.)
  @returns(A formatted protocol line for the given command code.)
}
function GetProtocolLineFromCode(const Code: Integer): String;
{
  @abstract(Processes a ping command received from a peer.)

  It extracts the peer's data from the line,
  authenticates the peer, and optionally sends a pong response if ShouldRespond is True.

  @param(Line The ping command line from the peer.)
  @param(Slot The connection slot of the peer.)
  @param(ShouldRespond Whether the server should respond with a pong command.)
}
procedure ProcessPingCommand(const Line: String; const Slot: Integer;
  ShouldRespond: Boolean);
{
  @abstract(Processes an incoming command from the peer, such as ping, pong, or transaction requests.)

  If the protocol is invalid or the peer is not authenticated, the connection is closed.

  @param(Slot The connection slot of the peer.)
  @param(Line The incoming command line from the peer.)
}
procedure ProcessIncomingCommand(const Slot: Integer; const Line: String);

{
  Sends the current list of masternodes to the specified peer.
  Each masternode's data is sent as a separate message to the peer.

  @param(PeerSlot The identifier of the peer connection slot.)
}
procedure SendMasternodeListToPeer(const PeerSlot: Integer);
{
  Sends the list of masternode checks to the specified peer.
  Each check is sent as a separate message to the peer.

  @param(PeerSlot The identifier of the peer connection slot.)
}
procedure SendMasternodeChecksToPeer(const PeerSlot: Integer);
{
  Generates a masternode verification line for a given masternode IP.
  The verification includes synchronization status and other relevant data.

  @param(MasternodeIP The IP address of the target masternode.)
  @returns(A string representing the verification line.)
}
function GenerateMasternodeVerificationLine(const MasternodeIP: String): String;

{
  Checks if the local node is fully synchronized with the consensus network.

  @returns(A TSyncStatus representing the synchronization status:
           @unorderedList(
           @item(ssSynchronized (0) - Fully synchronized.)
           @item(ssBlockHeightMismatch (1) - Block height mismatch.)
           @item(ssBlockHashMismatch (2) - Block hash mismatch.)
           @item(ssSummaryHashMismatch (3) - Summary hash mismatch.)
           @item(ssResumenHashMismatch (4) - Resumen hash mismatch.)))
}
function IsAllSynchronized(): TSyncStatus;

{**
  Retrieves the current synchronization status of the node as a formatted string.
  This status is a combination of the last block index, resumen hash, summary hash,
  and last block hash.

  @returns(A string representing the synchronization status.)
*}
function GetSynchronizationStatus(): String;


procedure ClearOutgoingTextForSlot(const Slot: Integer);

function GetOutgoingTextForSlot(const Slot: Integer): String;

procedure AddTextToSlot(const Slot: Integer; const Text: String);


function GetConnectionData(const Slot: Integer): TConnectionData;

procedure SetConnectionData(const Slot: Integer; const Data: TConnectionData);

procedure SetConnectionBusy(const Slot: Integer; Value: Boolean);

procedure UpdateConnectionLastPing(const Slot: Integer; Value: String);

procedure ReserveConnectionSlot(const Slot: Integer; IsReserved: Boolean);

procedure StartConnectionThread(const Slot: Integer);

procedure CloseConnectionSlot(const Slot: Integer);

function GetTotalConnections(): Integer;

function IsSlotConnected(const Slot: Integer): Boolean;


procedure UpdateNodeData();

function IsNodeValidator(const Ip: String): Boolean;

function ValidateMasternodeCheck(const Line: String): String;


procedure InitializeNodeServer();

function GetClientCount: Integer;

function SendMessageToClient(AContext: TIdContext; Message: String): Boolean;

function RetrieveStreamFromClient(AContext: TIdContext;
  out Stream: TMemoryStream): Boolean;

procedure SafelyCloseClientConnection(AContext: TIdContext; CloseMessage: String = '');


procedure IncrementClientReadThreadCount();

procedure DecrementClientReadThreadCount();

function GetActiveClientReadThreadCount(): Integer;


procedure AddIncomingMessage(Index: Integer; Message: String);

function GetIncomingMessage(Index: Integer): String;

function GetIncomingMessageLength(Index: Integer): Integer;

procedure ClearIncomingMessages(Index: Integer);


procedure UpdateBotData(BotIP: String);

procedure RemoveAllBots();

function BotExists(BotIp: String): Boolean;


procedure PopulateNodeList();

function GetNodeListLength(): Integer;

function GetNodeDataAtIndex(Index: Integer): TNodeData;


procedure InitializeElements();

procedure ClearElements();

const
  MaxConnections = 99;
  ProtocolVersion = 2;
  MainnetVersion = '0.4.4';

type
  TConnectionList = array [1..MaxConnections] of TConnectionData;
  TConnectionStringListArray = array [1..MaxConnections] of TStringList;
  TConnectionChannelList = array [1..MaxConnections] of TIdTCPClient;
  TConnectionOutgoingMessagesList = array [1..MaxConnections] of TStringArray;
  TConnectionCriticalSectionList = array[1..MaxConnections] of TRTLCriticalSection;

  TTimestampInteger = Int64;

var
  Connections: TConnectionList;
  SlotTextLines: TConnectionStringListArray;
  ClientChannels: TConnectionChannelList;
  OutgoingMessages: TConnectionOutgoingMessagesList;

  BotList: specialize TArray<TBotData>;

  PendingTransactionsPool: specialize TArray<TOrderData>;
  MultiOrderTransactionsPool: specialize TArray<TMultiOrderData>;

  DownloadingHeaders: Boolean = False;
  DownloadingSummary: Boolean = False;
  DownloadingBlocks: Boolean = False;
  DownloadingGVTs: Boolean = False;
  DownloadingPSOs: Boolean = False;

  LastMasternodeHashRequestTime: TTimestampInteger = 0;
  LastBestHashRequestTime: TTimestampInteger = 0;
  LastMasternodeListRequestTime: TTimestampInteger = 0;
  LastMasternodeCheckRequestTime: TTimestampInteger = 0;
  LastMasternodeVerificationTime: TTimestampInteger = 0;
  LastGVTsRequestTime: TTimestampInteger = 0;
  LastSummaryRequestTime: TTimestampInteger = 0;
  LastBlockRequestTime: TTimestampInteger = 0;
  LastAccountSummaryRequestTime: TTimestampInteger = 0;
  LastPendingTransactionsRequestTime: TTimestampInteger = 0;
  LastPSOsRequestTime: TTimestampInteger = 0;
  LastBotClearTime: TTimestampInteger = 0;

  ForceHeadersDownload: Boolean = False;
  MasternodeVerificationCount: Integer = 0;

  // Local data hashes
  LastBlockIndex: Integer = 0;
  LastBlockHash: String = '';
  PublicIPAddress: String = '';

  // Local information
  LastBlockData: BlockHeaderData;

  ActiveClientReadThreads: Integer = 0;

  // Critical sections
  CSClientReadThreads: TRTLCriticalSection;
  CSIncomingMessages: TConnectionCriticalSectionList;
  CSOutgoingMessages: TConnectionCriticalSectionList;
  CSConnections: TRTLCriticalSection;
  CSBotList: TRTLCriticalSection;
  CSPendingTransactions: TRTLCriticalSection;
  CSMultiOrderTransactions: TRTLCriticalSection;

  // nodes list
  NodeList: specialize TArray<TNodeData>;
  CSNodeList: TRTLCriticalSection;

  // Node server
  NodeServer: TIdTCPServer;

implementation

uses
  MPForm;

function GetPendingTransactionCount(): Integer;
begin
  EnterCriticalSection(CSPendingTransactions);
  try
    Result := Length(PendingTransactionsPool);
  finally
    LeaveCriticalSection(CSPendingTransactions);
  end;
end;

procedure ClearAllPendingTransactions();
begin
  EnterCriticalSection(CSPendingTransactions);
  try
    SetLength(PendingTransactionsPool, 0);
  finally
    LeaveCriticalSection(CSPendingTransactions);
  end;
end;

procedure SendPendingTransactionsToPeer(const Slot: Int64);
var
  i: Integer;
  Header, Line, TextOrder: String;
  PendingTransactionsCopy: array of TOrderData;
  Transaction: TOrderData;
begin
  if GetPendingTransactionCount = 0 then Exit;

  Header := GetProtocolHeader;
  TextOrder := Header + 'ORDER ';

  EnterCriticalSection(CSPendingTransactions);
  try
    PendingTransactionsCopy := Copy(PendingTransactionsPool);
  finally
    LeaveCriticalSection(CSPendingTransactions);
  end;

  for i := 0 to High(PendingTransactionsCopy) do
  begin
    Transaction := PendingTransactionsCopy[i];
    Line := OrderToString(Transaction);

    case Transaction.OrderType of
      'CUSTOM', 'SNDGVT':
        AddTextToSlot(Slot, Header + '$' + Line);

      'TRFR':
      begin
        if Transaction.TransferLine = 1 then
          TextOrder := Format('%s%d ', [TextOrder, Transaction.OrderLineCount]);

        TextOrder := Format('%s$%s ', [TextOrder, Line]);

        if Transaction.OrderLineCount = Transaction.TransferLine then
        begin
          SetLength(TextOrder, Length(TextOrder) - 1);
          AddTextToSlot(Slot, TextOrder);
          TextOrder := Header + 'ORDER ';
        end;
      end;

      else
        ;
    end;
  end;
end;

function TransactionAlreadyPending(const Hash: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  if GetPendingTransactionCount > 0 then
  begin
    EnterCriticalSection(CSPendingTransactions);
    try
      for i := 0 to GetPendingTransactionCount - 1 do
        if Hash = PendingTransactionsPool[i].TransferId then
        begin
          Result := True;
          Break;
        end;

    finally
      LeaveCriticalSection(CSPendingTransactions);
    end;
  end;

end;

function AddTransactionToPool(const Order: TOrderData): Boolean;
var
  i, InsertPos: Integer;
  Transaction: TOrderData;
begin
  BeginPerformance('AddTransactionToPool');
  Result := False;

  if (Order.TimeStamp < LastBlockData.TimeStart) or
    TransferExistsInLastBlock(Order.TransferId) or
    ((BlockAge > 585) and (Order.TimeStamp < LastBlockData.TimeStart + 540)) or
    TransactionAlreadyPending(Order.TransferId) then Exit;

  EnterCriticalSection(CSPendingTransactions);
  try
    InsertPos := Length(PendingTransactionsPool);

    for i := 0 to High(PendingTransactionsPool) do
    begin
      Transaction := PendingTransactionsPool[i];

      if (Order.TimeStamp < Transaction.TimeStamp) or
        ((Order.TimeStamp = Transaction.TimeStamp) and
        ((Order.OrderID < Transaction.OrderID) or
        ((Order.OrderID = Transaction.OrderID) and
        (Order.TransferLine < Transaction.TransferLine)))) then
      begin
        InsertPos := i;
        Break;
      end;
    end;

    Insert(Order, PendingTransactionsPool, InsertPos);
    Result := True;
  finally
    LeaveCriticalSection(CSPendingTransactions);
  end;

  EndPerformance('AddTransactionToPool');
end;


function TransferExistsInLastBlock(const TransferHash: String): Boolean;
var
  BlockTransfers: TBlockOrders;
  Transfer: TOrderData;
begin
  Result := False;

  BlockTransfers := GetBlockTransfers(LastBlockIndex);

  for Transfer in BlockTransfers do
  begin
    if Transfer.TransferId = TransferHash then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function GetMultiTransferCount(): Integer;
begin
  EnterCriticalSection(CSMultiOrderTransactions);
  try
    Result := Length(MultiOrderTransactionsPool);
  finally
    LeaveCriticalSection(CSMultiOrderTransactions);
  end;
end;

procedure ClearAllMultiTransfers();
begin
  EnterCriticalSection(CSMultiOrderTransactions);
  try
    SetLength(MultiOrderTransactionsPool, 0);
  finally
    LeaveCriticalSection(CSMultiOrderTransactions);
  end;
end;

function GetProtocolHeader(): String;
begin
  Result := Format('PSK %d %s%s %s ', [ProtocolVersion, MainnetVersion,
    NodeRelease, UTCTimeStr]);
end;

function IsValidProtocol(const Line: String): Boolean;
var
  Prefix: String;
begin
  Prefix := Copy(Line, 1, 4);
  Result := (Prefix = 'PSK ') or (Prefix = 'NOS ');
end;

function GetPingString(): String;
var
  Port: Integer = 0;
begin
  if Form1.Server.Active then
    Port := Form1.Server.DefaultPort
  else
    Port := -1;

  Result := Format('%d %d %s %s %d %s %d %d %s %d null %d %s %s %s',
    [GetTotalConnections(), LastBlockIndex, LastBlockHash, MySumarioHash,
    GetPendingTransactionCount(), GetResumenHash, MyConStatus, Port,
    Copy(GetMNsHash, 0, 5), GetMNsListLength, GetMasternodeCheckCount(),
    MyGVTsHash, Copy(HashMD5String(GetCFGDataStr), 0, 5), Copy(PSOFileHash, 0, 5)]);
end;

function GetProtocolLineFromCode(const Code: Integer): String;
var
  Specific: String;
begin
  Specific := '';
  case Code of
    0: Specific := '';                                  // OnlyHeaders
    3: Specific := '$PING ' + GetPingString;            // Ping
    4: Specific := '$PONG ' + GetPingString;            // Pong
    5: Specific := '$GETPENDING';                       // GetPending
    6: Specific := '$GETSUMARY';                        // GetSummary
    7: Specific := '$GETRESUMEN';                       // GetResumen
    8: Specific := Format('$LASTBLOCK %d', [LastBlockIndex]); // LastBlock
    9: Specific := '$CUSTOM ';                          // Custom
    11: Specific := '$GETMNS';                          // GetMNs
    12: Specific := '$BESTHASH';                        // BestHash
    13: Specific := '$MNREPO ' + GetMNReportString(LastBlockIndex); // MNReport
    14: Specific := '$MNCHECK ';                        // MNCheck
    15: Specific := '$GETCHECKS';                       // GetChecks
    16: Specific := 'GETMNSFILE';                       // GetMNsFile
    17: Specific := 'MNFILE';                           // MNFile
    18: Specific := Format('GETHEADUPDATE %d', [LastBlockIndex]); // GetHeadUpdate
    19: Specific := 'HEADUPDATE';                       // HeadUpdate
    20: Specific := '$GETGVTS';                         // GetGVTs
    21: Specific := '$SNDGVT ';
    30: Specific := 'GETCFGDATA';                       // GetCFG
    31: Specific := 'SETCFGDATA $';                     // SETCFG
    32: Specific := '$GETPSOS';                         // GetPSOs
  end;

  Result := Format('PSK %d %szzz %s %s', [ProtocolVersion, MainnetVersion,
    UTCTimeStr, Specific]);
end;

procedure ProcessPingCommand(const Line: String; const Slot: Integer;
  ShouldRespond: Boolean);
var
  Data: TConnectionData;
begin
  Data := GetConnectionData(Slot);

  with Data do
  begin
    IsAuthenticated := True;
    ProtocolVersion := StrToIntDef(GetParameter(Line, 1), 0);
    ClientVersion := GetParameter(Line, 2);
    TimeOffset := StrToInt64Def(GetParameter(Line, 3), UTCTime) - UTCTime;
    ActiveConnections := StrToIntDef(GetParameter(Line, 5), 0);
    LastBlockNumber := GetParameter(Line, 6);
    LastBlockHash := GetParameter(Line, 7);
    SummaryHash := GetParameter(Line, 8);
    PendingOperations := StrToIntDef(GetParameter(Line, 9), 0);
    SummaryBlockHash := GetParameter(Line, 10);
    ConnectionStatus := StrToIntDef(GetParameter(Line, 11), 0);
    ListeningPort := StrToIntDef(GetParameter(Line, 12), -1);
    MasternodeShortHash := GetParameter(Line, 13);
    MasternodeCount := StrToIntDef(GetParameter(Line, 14), 0);
    BestHashDifficulty := 'null';
    MasternodesChecksCount := StrToIntDef(GetParameter(Line, 16), 0);
    LastPingTime := UTCTimeStr;
    GVTHash := GetParameter(Line, 17);
    ConfigHash := GetParameter(Line, 18);
    PSOHash := GetParameter(Line, 19);
    MerkleTreeHash := HashMD5String(LastBlockNumber +
      Copy(SummaryBlockHash, 0, 5) + Copy(MasternodeShortHash, 0, 5) +
      Copy(LastBlockHash, 0, 5) + Copy(SummaryHash, 0, 5) +
      Copy(GVTHash, 0, 5) + Copy(ConfigHash, 0, 5));
  end;

  SetConnectionData(Slot, Data);

  if ShouldRespond then
    // PONG
    AddTextToSlot(Slot, GetProtocolLineFromCode(4));
end;

procedure ProcessIncomingCommand(const Slot: Integer; const Line: String);
var
  Command: String;
begin
  if not IsValidProtocol(Line) and not GetConnectionData(Slot).IsAuthenticated then
  begin
    UpdateBotData(GetConnectionData(Slot).IpAddress);
    CloseConnectionSlot(Slot);
    Exit;
  end;

  Command := GetProtocolCommand(Line);

  case UpperCase(Command) of
    'DUPLICATED', 'OLDVERSION':
      CloseConnectionSlot(Slot);
    '$PING':
      ProcessPingCommand(Line, Slot, True);
    '$PONG':
      ProcessPingCommand(Line, Slot, False);
    '$GETPENDING':
      SendPendingTransactionsToPeer(Slot);
  end;
end;

procedure SendMasternodeListToPeer(const PeerSlot: Integer);
var
  MasternodeList: TStringArray;
  i: Integer;
begin
  if PopulateMasternodeList(MasternodeList) then
  begin
    for i := 0 to Length(MasternodeList) - 1 do
      AddTextToSlot(PeerSlot, GetProtocolHeader + '$MNREPO ' + MasternodeList[i]);
  end;
end;

procedure SendMasternodeChecksToPeer(const PeerSlot: Integer);
var
  i: Integer;
  MasternodeCheckString: String;
begin
  if GetMasternodeCheckCount > 0 then
  begin
    EnterCriticalSection(CSMNsChecks);
    try
      for i := 0 to Length(MasternodeChecks) - 1 do
      begin
        MasternodeCheckString :=
          Format('%s%s', [GetProtocolLineFromCode(14),
          FormatMasternodeCheck(MasternodeChecks[i])]);

        AddTextToSlot(PeerSlot, MasternodeCheckString);
      end;
    finally
      LeaveCriticalSection(CSMNsChecks);
    end;
  end;
end;

function GenerateMasternodeVerificationLine(const MasternodeIP: String): String;
begin
  if IsAllSynchronized = ssSynchronized then
  begin
    Result := Format('True %s %s %s %s', [GetSynchronizationStatus,
      LocalMasternodeFunds, MasternodeIP, LocalMasternodeSignature]);

    Inc(MasternodeVerificationCount);
  end
  else
    Result := 'False';
end;

function IsAllSynchronized(): TSyncStatus;
begin
  Result := ssSynchronized;

  if LastBlockIndex <> StrToIntDef(GetConsensus(cLastBlock), 0) then
    Result := ssBlockHeightMismatch;

  if LastBlockHash <> GetConsensus(cLBHash) then
    Result := ssBlockHashMismatch;

  if Copy(MySumarioHash, 0, 5) <> GetConsensus(cSumHash) then
    Result := ssSummaryHashMismatch;

  if Copy(GetResumenHash, 0, 5) <> GetConsensus(cHeaders) then
    Result := ssResumenHashMismatch;
end;

function GetSynchronizationStatus(): String;
begin
  Result := '';

  try
    Result := Format('%d%s%s%s', [LastBlockIndex, Copy(GetResumenHash, 1, 3),
      Copy(MySumarioHash, 1, 3), Copy(LastBlockHash, 1, 3)]);
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,GetSynchronizationStatus,' + e.Message);
  end;
end;

{$REGION OutgoingMessages}

procedure ClearOutgoingTextForSlot(const Slot: Integer);
begin
  EnterCriticalSection(CSOutgoingMessages[Slot]);
  SetLength(OutgoingMessages[Slot], 0);
  LeaveCriticalSection(CSOutgoingMessages[Slot]);
end;

function GetOutgoingTextForSlot(const Slot: Integer): String;
begin
  Result := '';
  if ((Slot >= 1) and (Slot <= MaxConnections)) then
  begin
    EnterCriticalSection(CSOutgoingMessages[Slot]);
    if Length(OutgoingMessages[Slot]) > 0 then
    begin
      Result := OutgoingMessages[Slot][0];
      Delete(OutgoingMessages[Slot], 0, 1);
    end;
    LeaveCriticalSection(CSOutgoingMessages[Slot]);
  end;
end;

procedure AddTextToSlot(const Slot: Integer; const Text: String);
begin
  if ((Slot >= 1) and (Slot <= 99)) then
  begin
    EnterCriticalSection(CSOutgoingMessages[Slot]);
    Insert(Text, OutgoingMessages[Slot], Length(OutgoingMessages[Slot]));
    LeaveCriticalSection(CSOutgoingMessages[Slot]);
  end;
end;

{$ENDREGION OutgoingMessages}

{$REGION Connections control}

function GetConnectionData(const Slot: Integer): TConnectionData;
begin
  if ((slot < 1) or (Slot > MaxConnections)) then Result := Default(TConnectionData);
  EnterCriticalSection(CSConnections);
  Result := Connections[Slot];
  LeaveCriticalSection(CSConnections);
end;

procedure SetConnectionData(const Slot: Integer; const Data: TConnectionData);
begin
  if ((slot < 1) or (Slot > MaxConnections)) then Exit;
  EnterCriticalSection(CSConnections);
  Connections[Slot] := Data;
  LeaveCriticalSection(CSConnections);
end;

procedure SetConnectionBusy(const Slot: Integer; Value: Boolean);
begin
  if ((Slot < 1) or (Slot > MaxConnections)) then Exit;
  EnterCriticalSection(CSConnections);
  Connections[Slot].IsBusy := Value;
  LeaveCriticalSection(CSConnections);
end;

procedure UpdateConnectionLastPing(const Slot: Integer; Value: String);
begin
  if ((Slot < 1) or (Slot > MaxConnections)) then Exit;
  EnterCriticalSection(CSConnections);
  Connections[Slot].LastPingTime := Value;
  LeaveCriticalSection(CSConnections);
end;

procedure ReserveConnectionSlot(const Slot: Integer; IsReserved: Boolean);
var
  ToShow: String = '';
begin
  if ((Slot < 1) or (Slot > MaxConnections)) then Exit;
  if IsReserved then ToShow := 'RES';
  EnterCriticalSection(CSConnections);
  Connections[Slot].ConnectionType := ToShow;
  LeaveCriticalSection(CSConnections);
end;

procedure StartConnectionThread(const Slot: Integer);
begin
  if ((Slot < 1) or (Slot > MaxConnections)) then Exit;
  EnterCriticalSection(CSConnections);
  Connections[Slot].ReadThread := TClientReadThread.Create(True, Slot);
  Connections[Slot].ReadThread.FreeOnTerminate := True;
  Connections[Slot].ReadThread.Start;
  LeaveCriticalSection(CSConnections);
end;

procedure CloseConnectionSlot(const Slot: Integer);
begin
  if ((slot < 1) or (Slot > MaxConnections)) then Exit;
  BeginPerformance('CloseSlot');
  try
    if GetConnectionData(Slot).ConnectionType = 'CLI' then
    begin
      ClearIncomingMessages(slot);
      GetConnectionData(Slot).Context.Connection.Disconnect;
      Sleep(10);
    end;
    if GetConnectionData(Slot).ConnectionType = 'SER' then
    begin
      ClearIncomingMessages(slot);
      ClientChannels[Slot].IOHandler.InputBuffer.Clear;
      ClientChannels[Slot].Disconnect;
    end;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,CloseSlot,' + E.Message);
  end;{Try}
  SetConnectionData(Slot, Default(TConnectionData));
  EndPerformance('CloseSlot');
end;

function GetTotalConnections(): Integer;
var
  counter: Integer;
begin
  BeginPerformance('GetTotalConexiones');
  Result := 0;
  for counter := 1 to MaxConnections do
    if IsSlotConnected(Counter) then Inc(Result);
  EndPerformance('GetTotalConexiones');
end;

function IsSlotConnected(const Slot: Integer): Boolean;
begin
  Result := False;
  if ((Slot < 1) or (Slot > MaxConnections)) then Exit;
  if ((GetConnectionData(Slot).ConnectionType = 'SER') or
    (GetConnectionData(Slot).ConnectionType = 'CLI')) then
    Result := True;
end;

{$ENDREGION Connections control}

{$REGION General Data}

// Updates local data hashes
procedure UpdateNodeData();
begin
  LastBlockHash := HashMD5File(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
  LastBlockData := LoadBlockDataHeader(LastBlockIndex);
  SetResumenHash;
  if GetResumenHash = GetConsensus(5) then
    ForceHeadersDownload := False;
end;

function IsNodeValidator(const Ip: String): Boolean;
begin
  Result := IsSeedNode(Ip);
end;

// Verify if a validation report is correct
function ValidateMasternodeCheck(const Line: String): String;
var
  CheckData: TMNCheck;
  StartPos: Integer;
  ReportInfo: String;
  ErrorCode: Integer = 0;
begin
  Result := '';
  StartPos := Pos('$', Line);
  ReportInfo := Copy(Line, StartPos, Length(Line));
  CheckData := GetMNCheckFromString(Line);
  if MnsCheckExists(CheckData.ValidatorIP) then Exit;
  if not IsNodeValidator(CheckData.ValidatorIP) then ErrorCode := 1;
  if CheckData.Block <> LastBlockIndex then ErrorCode := 2;
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
    //  outGOingMsjsAdd(GetProtocolHeader+ReportInfo);
  end;
end;

{$ENDREGION General Data}

{$REGION Node Server}

procedure InitializeNodeServer();
begin
  NodeServer := TIdTCPServer.Create(nil);
  NodeServer.DefaultPort := 8080;
  NodeServer.Active := False;
  NodeServer.UseNagle := True;
  NodeServer.TerminateWaitTime := 10000;
  NodeServer.OnExecute := @TNodeServerEvents.OnExecute;
  NodeServer.OnConnect := @TNodeServerEvents.OnConnect;
  NodeServer.OnDisconnect := @form1.IdTCPServer1Disconnect;
  NodeServer.OnException := @Form1.IdTCPServer1Exception;
end;

// returns the number of active connections
function GetClientCount: Integer;
var
  Clients: TList;
begin
  Clients := Nodeserver.Contexts.LockList;
  try
    Result := Clients.Count;
  except
    ON E: Exception do
      ToDeepDebug('NosoNetwork,ClientsCount,' + E.Message);
  end; {TRY}
  Nodeserver.Contexts.UnlockList;
end;

// Try message to client safely
function SendMessageToClient(AContext: TIdContext; Message: String): Boolean;
begin
  Result := True;
  try
    Acontext.Connection.IOHandler.WriteLn(Message);
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;{Try}
end;

// Get stream from client
function RetrieveStreamFromClient(AContext: TIdContext;
  out Stream: TMemoryStream): Boolean;
begin
  Result := False;
  Stream.Clear;
  try
    AContext.Connection.IOHandler.ReadStream(Stream);
    Result := True;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,GetStreamFromClient,' + E.Message);
  end;
end;

// Trys to close a server connection safely
procedure SafelyCloseClientConnection(AContext: TIdContext; CloseMessage: String = '');
begin
  try
    if CloseMessage <> '' then
      Acontext.Connection.IOHandler.WriteLn(CloseMessage);
    AContext.Connection.Disconnect();
    Acontext.Connection.IOHandler.InputBuffer.Clear;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,TryCloseServerConnection,' + E.Message);
  end; {TRY}
end;

class procedure TNodeServerEvents.OnExecute(AContext: TIdContext);
 //var
 //  LLine: String = '';
 //  IPUser: String = '';
 //  slot: Integer = 0;
 //  UpdateZipName: String = '';
 //  UpdateVersion: String = '';
 //  UpdateHash: String = '';
 //  UpdateClavePublica: String = '';
 //  UpdateFirma: String = '';
 //  MemStream: TMemoryStream;
 //  BlockZipName: String = '';
 //  GetFileOk: Boolean = False;
 //  GoAhead: Boolean;
 //  NextLines: array of String;
 //  LineToSend: String;
 //  LinesSent: Integer = 0;
 //  FTPTime, FTPSize, FTPSpeed: Int64;
begin
  AContext := AContext;
end;

class procedure TNodeServerEvents.OnConnect(AContext: TIdContext);
begin
  AContext := AContext;
end;

class procedure TNodeServerEvents.OnDisconnect(AContext: TIdContext);
begin
  AContext := AContext;
end;

class procedure TNodeServerEvents.OnException(AContext: TIdContext);
begin
  AContext := AContext;
end;

{$ENDREGION Node Server}

{$REGION Thread Client read}

constructor TClientReadThread.Create(const CreatePaused: Boolean;
  const ConnectionSlot: Integer);
begin
  inherited Create(CreatePaused);
  FSlot := ConnectionSlot;
end;

function LineToClient(Slot: Integer; LLine: String): Boolean;
begin
  Result := False;
  try
    ClientChannels[Slot].IOHandler.WriteLn(LLine);
    Result := True;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,LineToClient,' + E.Message);
  end;
end;

function GetStreamFromClient(Slot: Integer; out LStream: TMemoryStream): Boolean;
begin
  Result := False;
  LStream.Clear;
  try
    ClientChannels[Slot].IOHandler.ReadStream(LStream);
    Result := True;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,GetStreamFromClient,' + E.Message);
  end;
end;

function SendLineToClient(FSlot: Integer; LLine: String): Boolean;
begin
  Result := True;
  try
    ClientChannels[FSlot].IOHandler.Writeln(LLine);
  except
    ON E: Exception do
    begin
      Result := False;
      ToDeepDebug('NosoNetwork,SendLineToClient,' + E.Message);
    end;
  end;
end;

procedure TClientReadThread.Execute;
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
  ClientChannels[FSlot].ReadTimeout := 1000;
  ClientChannels[FSlot].IOHandler.MaxLineLength := Maxint;
  IncrementClientReadThreadCount;
  AddNewOpenThread(ThreadName, UTCTime);
  LastActive := UTCTime;
  repeat
  try
    Sleep(10);
    OnBuffer := True;
    if ClientChannels[FSlot].IOHandler.InputBufferIsEmpty then
    begin
      ClientChannels[FSlot].IOHandler.CheckForDataOnSource(1000);
      if ClientChannels[FSlot].IOHandler.InputBufferIsEmpty then
      begin
        OnBuffer := False;
        repeat
          LineToSend := GetOutgoingTextForSlot(Fslot);
          if LineToSend <> '' then
          begin
            if not SendLineToClient(FSlot, LineToSend) then
            begin
              killit := True;
              Connections[FSlot].ReadThread.Terminate;
              Break;
            end;
          end;
        until LineToSend = '';
      end;
    end;
    if OnBuffer then
    begin
      while not ClientChannels[FSlot].IOHandler.InputBufferIsEmpty do
      begin
        SetConnectionBusy(FSlot, True);
        UpdateConnectionLastPing(fSlot, UTCTimeStr);
        LLine := '';
        try
          LLine := ClientChannels[FSlot].IOHandler.ReadLn(IndyTextEncoding_UTF8);
        except
          on E: Exception do
          begin
            SetConnectionBusy(FSlot, False);
            Connections[FSlot].ReadThread.Terminate;
            KillIt := True;
            Break;
          end;
        end; {TRY}
        if LLine <> '' then
        begin
          LastActive := UTCTime;
          UpdateOpenThread(ThreadName, UTCTime);
          ClientChannels[FSlot].ReadTimeout := 10000;
          if GetParameter(LLine, 0) = 'RESUMENFILE' then
          begin
            DownloadingHeaders := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveStreamAsHeaders(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              UpdateNodeData();
            end
            else
              killit := True;
            LastAccountSummaryRequestTime := 0;
            MemStream.Free;
            DownloadingHeaders := False;
          end

          else if GetParameter(LLine, 0) = 'SUMARYFILE' then
          begin
            DownloadingSummary := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveSummaryToFile(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              UpdateNodeData();
              CreateSumaryIndex();
            end
            else
              killit := True;
            LastSummaryRequestTime := 0;
            MemStream.Free;
            DownloadingSummary := False;
          end

          else if GetParameter(LLine, 0) = 'PSOSFILE' then
          begin
            DownloadingPSOs := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SavePSOsToFile(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              LoadPSOFileFromDisk;
              UpdateNodeData();
            end
            else
              killit := True;
            LastPSOsRequestTime := 0;
            MemStream.Free;
            DownloadingPSOs := False;
          end
          else if GetParameter(LLine, 0) = 'GVTSFILE' then
          begin
            DownloadingGVTs := True;
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
            LastGVTsRequestTime := 0;
            MemStream.Free;
            DownloadingGVTs := False;
          end

          else if GetParameter(LLine, 0) = 'BLOCKZIP' then
          begin
            DownloadingBlocks := True;
            MemStream := TMemoryStream.Create;
            if GetStreamFromClient(FSlot, MemStream) then
              SavedToFile := SaveStreamAsZipBlocks(MemStream)
            else
              SavedToFile := False;
            if SavedToFile then
            begin
              if UnzipFile(BlockDirectory + 'blocks.zip', True) then
              begin
                LastBlockIndex := GetMyLastUpdatedBlock();
                LastBlockHash :=
                  HashMD5File(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
                UpdateNodeData();
              end;
            end
            else
              killit := True;
            LastBlockRequestTime := 0;
            MemStream.Free;
            DownloadingBlocks := False;
          end // END RECEIVING BLOCKS
          else
          begin
            //ProcessIncomingCommand(FSlot,LLine);
            AddIncomingMessage(FSlot, LLine);
          end;
        end;
        SetConnectionBusy(FSlot, False);
      end; // end while client is not empty
    end;   // End OnBuffer
    if LastActive + 30 < UTCTime then killit := True;
    if GetConnectionData(Fslot).ConnectionType <> 'SER' then killit := True;
    if not ClientChannels[FSlot].Connected then killit := True;
  except
    ON E: Exception do
    begin
      ToDeepDebug('NosoNetwork,TThreadClientRead,' + E.Message);
      KillIt := True;
    end;
  end;
  until ((terminated) or (KillIt));
  CloseConnectionSlot(Fslot);
  DecrementClientReadThreadCount;
  CloseOpenThread(ThreadName);
end;

//Procedure ProcessLine(LLine:String;)

{$ENDREGION Thread Client read}

{$REGION ClientReadThreads}

procedure IncrementClientReadThreadCount();
begin
  EnterCriticalSection(CSClientReadThreads);
  Inc(ActiveClientReadThreads);
  LeaveCriticalSection(CSClientReadThreads);
end;

procedure DecrementClientReadThreadCount();
begin
  EnterCriticalSection(CSClientReadThreads);
  Dec(ActiveClientReadThreads);
  LeaveCriticalSection(CSClientReadThreads);
end;

function GetActiveClientReadThreadCount(): Integer;
begin
  EnterCriticalSection(CSClientReadThreads);
  Result := ActiveClientReadThreads;
  LeaveCriticalSection(CSClientReadThreads);
end;

{$ENDREGION ClientReadThreads}

{$REGION Incoming/outgoing info}

procedure AddIncomingMessage(Index: Integer; Message: String);
begin
  EnterCriticalSection(CSIncomingMessages[Index]);
  SlotTextLines[Index].Add(Message);
  LeaveCriticalSection(CSIncomingMessages[Index]);
end;

function GetIncomingMessage(Index: Integer): String;
begin
  Result := '';
  if GetIncomingMessageLength(Index) > 0 then
  begin
    EnterCriticalSection(CSIncomingMessages[Index]);
    Result := SlotTextLines[Index][0];
    SlotTextLines[index].Delete(0);
    LeaveCriticalSection(CSIncomingMessages[Index]);
  end;
end;

function GetIncomingMessageLength(Index: Integer): Integer;
begin
  EnterCriticalSection(CSIncomingMessages[Index]);
  Result := SlotTextLines[Index].Count;
  LeaveCriticalSection(CSIncomingMessages[Index]);
end;

procedure ClearIncomingMessages(Index: Integer);
begin
  EnterCriticalSection(CSIncomingMessages[Index]);
  SlotTextLines[Index].Clear;
  LeaveCriticalSection(CSIncomingMessages[Index]);
end;


{$ENDREGION Incoming/outgoing info}

{$REGION Bots array}

procedure UpdateBotData(BotIP: String);
var
  contador: Integer = 0;
  updated: Boolean = False;
begin
  if IsSeedNode(BotIP) then Exit;
  EnterCriticalSection(CSBotList);
  for contador := 0 to Length(BotList) - 1 do
  begin
    if BotList[Contador].IpAddress = BotIP then
    begin
      BotList[Contador].LastRefused := UTCTime;
      Updated := True;
    end;
  end;
  LeaveCriticalSection(CSBotList);
  if not updated then
  begin
    EnterCriticalSection(CSBotList);
    SetLength(BotList, Length(BotList) + 1);
    BotList[Length(BotList) - 1].IpAddress := BotIP;
    BotList[Length(BotList) - 1].LastRefused := UTCTime;
    LeaveCriticalSection(CSBotList);
  end;
end;

procedure RemoveAllBots();
begin
  EnterCriticalSection(CSBotList);
  SetLength(BotList, 0);
  LeaveCriticalSection(CSBotList);
  LastBotClearTime := UTCTime;
end;

function BotExists(BotIp: String): Boolean;
var
  contador: Integer = 0;
begin
  Result := False;
  EnterCriticalSection(CSBotList);
  for contador := 0 to Length(BotList) - 1 do
    if BotList[contador].IpAddress = BotIp then
    begin
      Result := True;
      Break;
    end;
  LeaveCriticalSection(CSBotList);
end;

{$ENDREGION Bots array}

{$REGION Nodes list}

procedure PopulateNodeList();
var
  counter: Integer;
  ThisNode: String = '';
  Thisport: Integer;
  continuar: Boolean = True;
  NodeToAdd: TNodeData;
  SourceStr: String = '';
begin
  counter := 0;
  SourceStr := GetParameter(GetCFGDataStr, 1) + GetVerificatorsText;
  SourceStr := StringReplace(SourceStr, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
  EnterCriticalSection(CSNodeList);
  SetLength(NodeList, 0);
  repeat
    ThisNode := GetParameter(SourceStr, counter);
    ThisNode := StringReplace(ThisNode, ';', ' ', [rfReplaceAll, rfIgnoreCase]);
    ThisPort := StrToIntDef(GetParameter(ThisNode, 1), 8080);
    ThisNode := GetParameter(ThisNode, 0);
    if thisnode = '' then continuar := False
    else
    begin
      NodeToAdd.IpAddress := ThisNode;
      NodeToAdd.Port := IntToStr(ThisPort);
      Insert(NodeToAdd, NodeList, Length(NodeList));
      counter += 1;
    end;
  until not continuar;
  LeaveCriticalSection(CSNodeList);
end;

function GetNodeListLength(): Integer;
begin
  EnterCriticalSection(CSNodeList);
  Result := Length(NodeList);
  LeaveCriticalSection(CSNodeList);
end;

function GetNodeDataAtIndex(Index: Integer): TNodeData;
begin
  Result := Default(TNodeData);
  if Index >= GetNodeListLength then Exit;
  EnterCriticalSection(CSNodeList);
  Result := NodeList[Index];
  LeaveCriticalSection(CSNodeList);
end;

{$ENDREGION Nodes list}

{$REGION Unit related}

procedure InitializeElements();
var
  counter: Integer;
begin
  InitCriticalSection(CSClientReadThreads);
  InitCriticalSection(CSConnections);
  InitCriticalSection(CSBotList);
  InitCriticalSection(CSPendingTransactions);
  InitCriticalSection(CSMultiOrderTransactions);
  InitCriticalSection(CSNodeList);
  SetLength(BotList, 0);
  SetLength(PendingTransactionsPool, 0);
  SetLength(MultiOrderTransactionsPool, 0);
  SetLength(NodeList, 0);
  for counter := 1 to MaxConnections do
  begin
    InitCriticalSection(CSIncomingMessages[counter]);
    SlotTextLines[counter] := TStringList.Create;
    ClientChannels[counter] := TIdTCPClient.Create(nil);
    InitCriticalSection(CSOutgoingMessages[counter]);
    SetLength(OutgoingMessages[counter], 0);
  end;
end;

procedure ClearElements();
var
  counter: Integer;
begin
  DoneCriticalSection(CSClientReadThreads);
  DoneCriticalSection(CSConnections);
  DoneCriticalSection(CSBotList);
  DoneCriticalSection(CSMultiOrderTransactions);
  DoneCriticalSection(CSNodeList);
  for counter := 1 to MaxConnections do
  begin
    DoneCriticalSection(CSIncomingMessages[counter]);
    SlotTextLines[counter].Free;
    ClientChannels[counter].Free;
    DoneCriticalSection(CSOutgoingMessages[counter]);
  end;
end;

{$ENDREGION Unit related}

initialization
  InitializeElements();


finalization
  ClearElements;

end. // End unit
