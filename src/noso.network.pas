{ Unit for network utils. }
unit Noso.Network;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  IdContext, IdGlobal, IdTCPClient, IdTCPServer,
  Noso.Debug, Noso.Time, Noso.General, Noso.Headers, Noso.Crypto,
  Noso.Block, Noso.Consensus,
  Noso.Summary, Noso.Config, Noso.Gvt, Noso.Masternodes, Noso.Pso;

const
  { Maximum number of connections allowed. }
  MaxConnections = 99;
  { The protocol version used in communication between clients and servers. }
  ProtocolVersion = 2;
  { The main net version. }
  MainnetVersion = '0.4.4';

type
  {
    Thread for handling client read operations in a connection slot.
  }
  TClientReadThread = class(TThread)
  private
    FSlot: Integer;  //< The connection slot that this thread is responsible for.

  protected
    {
      The main procedure that runs the thread’s logic.
    }
    procedure Execute; override;
  public
    {
      Constructor for TClientReadThread. Initializes the thread with the specified connection slot.

      @param CreatePaused Determines whether the thread starts paused.
      @param ConnectionSlot The slot number associated with the connection this thread will handle.
    }
    constructor Create(const CreatePaused: Boolean; const ConnectionSlot: Integer);
  end;

  {
    Stores basic data for a network node.
  }
  TNodeData = packed record
    IpAddress: String[15]; //< The IP address of the node.
    Port: String[8];       //< The port the node is listening on.
  end;

  { Type representing a UTC timestamp. }
  TTimeStamp = String[15];
  { Type representing a short 5-character hash. }
  THashIdentifier = String[5];
  { Type representing a 15-character block number. }
  TBlockNumber = String[15];
  { Type representing a hash string (MD5, 32 chars, 128 bits). }
  THashString = String[32];
  { Type representing a hash string (SHA256?, 64 chars, 256 bits). }
  TLongHashString = String[64];

  {
    Represents the data of a network connection.

    @member IsAuthenticated Whether the connection has been authenticated via a ping.
    @member ActiveConnections The number of peers currently connected.
    @member ConnectionType The type of connection, either 'SER' for server or 'CLI' for client.
    @member IPAddress The IP address of the peer.
    @member LastPingTime The UTC time of the last ping.
    @member Context Client context information used for communication channels.
    @member LastBlockNumber The number of the last known block.
    @member LastBlockHash The hash of the last known block.
    @member SummaryHash The hash of the account summary.
    @member PendingOperations The number of pending operations for this connection.
    @member ProtocolVersion The version of the protocol being used.
    @member ClientVersion The software version of the client.
    @member ListeningPort The port the peer is listening on.
    @member TimeOffset The time difference in seconds from this peer.
    @member SummaryBlockHash The hash of the summary block.
    @member ConnectionStatus The status of the connection.
    @member IsBusy Whether the peer is currently busy.
    @member ReadThread The thread used for client reading operations.
    @member MasternodeShortHash The shortened hash for identifying the masternode.
    @member MasternodeCount The number of masternodes connected.
    @member BestHashDifficulty The best hash difficulty found.
    @member MasternodeChecksCount The number of checks performed on masternodes.
    @member GVTHashMD5 The hash of the GVT.
    @member ConfigurationHash The hash of the configuration file.
    @member MerkleTreeHash The hash of the Merkle tree.
    @member PSOHash The hash of the Proof of Stake (PoS) state.
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
    Stores data related to a bot, such as its IP address and the timestamp of the last refused connection.

    @member IpAddress The IP address of the bot.
    @member LastRefused The timestamp (in Unix time) of the last time the bot's connection was refused.
  }
  TBotData = packed record
    IpAddress: String[15];
    LastRefused: Int64;
  end;

  {
    Handles various TCP server events for the node, such as connect, disconnect, execution, and exceptions.

    @note This class, for all intents and purposes, is a no-op. The procedures don't do anything.
  }
  TNodeServerEvents = class
    { Handles the execution of an operation on the server. }    class
    procedure OnExecute(AContext: TIdContext);
    { Called when a client connects to the server. }    class procedure
      OnConnect(AContext: TIdContext);
    { Called when a client disconnects from the server. }    class
    procedure OnDisconnect(AContext: TIdContext);
    { OnException Handles exceptions that occur during the server operations. }
    class procedure OnException(AContext: TIdContext);
  end;

  { An enum representing the synchronization status. Used in IsAllSynchronized. }
  TSyncStatus = (
    ssSynchronized,          //< Fully synchronized (= 0)
    ssBlockHeightMismatch,   //< Block height mismatch (= 1)
    ssBlockHashMismatch,     //< Block hash mismatch (= 2)
    ssSummaryHashMismatch,   //< Summary hash mismatch (= 3)
    ssResumenHashMismatch    //< Resumen hash mismatch (= 4)
    );


  { A list that holds connection data for each client connection slot. }
  TConnectionList = array [1..MaxConnections] of TConnectionData;
  { An array of TStringList objects for holding incoming messages for each connection slot. }
  TConnectionStringListArray = array [1..MaxConnections] of TStringList;
  { A list of TCP client channels (connections) for each client slot. }
  TConnectionChannelList = array [1..MaxConnections] of TIdTCPClient;
  { An array of outgoing message lists, one per connection slot. }
  TConnectionOutgoingMessagesList = array [1..MaxConnections] of TStringArray;
  { A list of critical sections for synchronizing access to connection-specific resources.  }
  TConnectionCriticalSectionList = array[1..MaxConnections] of TRTLCriticalSection;
  { A type representing timestamps as integers. Used for tracking times in milliseconds since the epoch. }
  TTimestampInteger = Int64;


{
  Return the number of pending transactions.

  @returns Return the number of pending transactions in the pool.
}
function GetPendingTransactionCount(): Integer;
{
  Clears all pending transactions.
}
procedure ClearAllPendingTransactions();
{
  Sends pending transactions to the specified peer slot.

  It copies the pending transaction pool safely, and then sends each transaction to the peer based on its type.

  @param Slot The slot number of the peer to send transactions to.
}
procedure SendPendingTransactionsToPeer(const Slot: Int64);
{
  Checks if a transaction with the given hash already exists in the pending pool.

  @note The function is thread-safe and locks the critical section during the search.

  @param Hash The transfer ID of the transaction to check.
  @returns @true if the transaction is already pending, otherwise @false.
}
function TransactionAlreadyPending(const Hash: String): Boolean;
{
  Adds a new transaction to the pending pool in a thread-safe manner.

  The function finds the correct insertion position based on timestamps and order IDs.

  @param Order The transaction data to add to the pending pool.
  @returns @true if the transaction was successfully added, otherwise @false.
}
function AddTransactionToPool(const Order: TOrderData): Boolean;
{
  Checks if a transfer with the given hash exists in the last block.

  @param TransferHash The transfer ID to check for.
  @returns @true if the transfer exists in the last block, otherwise @false.
}
function TransferExistsInLastBlock(const TransferHash: String): Boolean;

{
  Returns the protocol header.

  The protocol header is of the form "PSK P MN U", where P is the protocol version
  (currently 2), M is the Mainnet version (0.4.4), N is the node release (Ab7) and
  U is UTCTimeStr. For instance, a valid header is: "PSK 2 0.4.4Ab7 1726431338".

  @returns The protocol header in the given format.
}
function GetProtocolHeader(): String;
{
  Checks if the current line is a valid protocol line (if it starts with PSK or NOS).

  @param Line The line to validate.
  @returns @true if Line is a valid protocol line, and @false otherwise.
}
function IsValidProtocol(const Line: String): Boolean;
{
  Generates a ping string.

  A ping string contains details about the current node's state, including
  connections, last block, and transaction information.

  @returns A formatted ping string.
}
function GetPingString(): String;
{
  Generates a specific protocol line based on the provided code.

  The protocol line varies according to the command type represented by the code.

  @param Code The integer code representing a specific protocol command.
  @returns A formatted protocol line for the given command code.
}
function GetProtocolLineFromCode(const Code: Integer): String;
{
  Processes a ping command received from a peer.

  It extracts the peer's data from the line,
  authenticates the peer, and optionally sends a pong response if ShouldRespond is True.

  @param Line The ping command line from the peer.
  @param Slot The connection slot of the peer.
  @param ShouldRespond Whether the server should respond with a pong command.
}
procedure ProcessPingCommand(const Line: String; const Slot: Integer;
  ShouldRespond: Boolean);
{
  Processes an incoming command from the peer, such as ping, pong, or transaction requests.

  If the protocol is invalid or the peer is not authenticated, the connection is closed.

  @param Slot The connection slot of the peer.
  @param Line The incoming command line from the peer.
}
procedure ProcessIncomingCommand(const Slot: Integer; const Line: String);

{
  Sends the current list of masternodes to the specified peer.
  Each masternode's data is sent as a separate message to the peer.

  @param PeerSlot The identifier of the peer connection slot.
}
procedure SendMasternodeListToPeer(const PeerSlot: Integer);
{
  Sends the list of masternode checks to the specified peer.
  Each check is sent as a separate message to the peer.

  @param PeerSlot The identifier of the peer connection slot.
}
procedure SendMasternodeChecksToPeer(const PeerSlot: Integer);
{
  Generates a masternode verification line for a given masternode IP.
  The verification includes synchronization status and other relevant data.

  @param MasternodeIP The IP address of the target masternode.
  @returns A string representing the verification line.
}
function GenerateMasternodeVerificationLine(const MasternodeIP: String): String;

{
  Checks if the local node is fully synchronized with the consensus network.

  @returns A TSyncStatus representing the synchronization status:
     @unorderedList(
         @item ssSynchronized (0) - Fully synchronized.
         @item ssBlockHeightMismatch (1) - Block height mismatch.
         @item ssBlockHashMismatch (2) - Block hash mismatch.
         @item ssSummaryHashMismatch (3) - Summary hash mismatch.
         @item ssResumenHashMismatch (4) - Resumen hash mismatch.
     )
}
function IsAllSynchronized(): TSyncStatus;

{
  Retrieves the current synchronization status of the node as a formatted string.
  This status is a combination of the last block index, resumen hash, summary hash,
  and last block hash.

  @returns A string representing the synchronization status.
}
function GetSynchronizationStatus(): String;

{
  Clears all outgoing text messages for the given slot.

  @param Slot The slot index for which the outgoing messages will be cleared.
}
procedure ClearOutgoingTextForSlot(const Slot: Integer);

{
  Retrieves and removes the first outgoing text message for the given slot.

  @param Slot The slot index from which to retrieve the outgoing message.
  @returns The first message for the slot, or an empty string if no messages exist.
}
function GetOutgoingTextForSlot(const Slot: Integer): String;

{
  Adds a text message to the outgoing message queue for the given slot.

  @param Slot The slot index to which the text message will be added.
  @param Text The text message to add to the slot's outgoing message queue.
}
procedure AddTextToSlot(const Slot: Integer; const Text: String);

{
  Retrieves the connection data for the given slot.

  @param Slot The index of the connection slot.
  @returns The connection data for the slot, or a default value if the slot is invalid.
}
function GetConnectionData(const Slot: Integer): TConnectionData;

{
  Sets the connection data for the given slot.

  @param Slot The index of the connection slot.
  @param Data The connection data to be set for the slot.
}
procedure SetConnectionData(const Slot: Integer; const Data: TConnectionData);


{
  Marks the connection slot as busy or not busy.

  @param Slot The index of the connection slot.
  @param Busy Boolean indicating whether the slot is busy.
}
procedure SetConnectionBusy(const Slot: Integer; Busy: Boolean);
{
  Updates the last ping time for the given connection slot.

  @param Slot The index of the connection slot.
  @param Value The timestamp of the last ping.
}
procedure UpdateConnectionLastPing(const Slot: Integer; Value: String);
{
  Reserves or releases a connection slot by marking its type.

  @param Slot The index of the connection slot.
  @param IsReserved Boolean indicating if the slot should be reserved.
}
procedure ReserveConnectionSlot(const Slot: Integer; IsReserved: Boolean);
{
  Starts a read thread for the given connection slot.

  @param Slot The index of the connection slot.
}
procedure StartConnectionThread(const Slot: Integer);

{
  Closes the connection for the specified slot.

  @param Slot The index of the connection slot.
}
procedure CloseConnectionSlot(const Slot: Integer);
{
  Retrieves the total number of active connections.

  @returns The count of active connections.
}
function GetTotalConnections(): Integer;
{
  Checks if the specified slot is currently connected.

  @param Slot The index of the connection slot.
  @returns True if the slot is connected, otherwise False.
}
function IsSlotConnected(const Slot: Integer): Boolean;

{
  Updates the node data by recalculating hashes and block data.
  Also determines whether to force a headers download.
}
procedure UpdateNodeData();
{
  Checks if the given IP belongs to a node validator.

  @param Ip The IP address of the node.
  @returns @true if the IP is a seed node, otherwise @false.
}
function IsNodeValidator(const Ip: String): Boolean;
{
  @abstract Validates the provided masternode check report.

  Verifies various aspects of the report including validator, block index,
  signature, and address consistency.

  @param Line The masternode check report as a string.
  @returns The validated report information if successful, or an empty string if validation fails.
}
function ValidateMasternodeCheck(const Line: String): String;

{
  Initializes the node server with the necessary configurations and event handlers.
  This sets up the TCP server but does not activate it.
}
procedure InitializeNodeServer();
{
  Retrieves the current number of active client connections to the node server.

  @returns The number of active client connections.
}
function GetClientCount: Integer;
{
  Sends a message to the specified client context in a safe manner.

  @param AContext The client context to send the message to.
  @param Message The message to be sent.
  @returns @true if the message was successfully sent, otherwise @false.
}
function SendMessageToClient(AContext: TIdContext; Message: String): Boolean;
{
  Retrieves a stream of data from the specified client context.

  @param AContext The client context to read the stream from.
  @param Stream The memory stream to store the received data.
  @returns @true if the stream was successfully retrieved, otherwise @false.
}
function RetrieveStreamFromClient(AContext: TIdContext;
  out Stream: TMemoryStream): Boolean;

{
  Safely closes the connection with a client, optionally sending a final message before disconnecting.

  @param AContext The client context to disconnect.
  @param CloseMessage An optional message to send to the client before closing the connection.
}
procedure SafelyCloseClientConnection(AContext: TIdContext; CloseMessage: String = '');

{ Increments the active client read thread count. }
procedure IncrementClientReadThreadCount();
{ Decrements the active client read thread count. }
procedure DecrementClientReadThreadCount();
{
  Retrieves the current number of active client read threads.

  @returns Number of active client read threads.
}
function GetActiveClientReadThreadCount(): Integer;

{
  Adds an incoming message to the message queue for the given connection slot.

  @param SlotIndex Index of the connection slot.
  @param IncomingMessage String The incoming message to be added.
}
procedure AddIncomingMessage(const SlotIndex: Integer; const IncomingMessage: String);

{
  Retrieves and removes the first incoming message from the message queue for the given connection slot.

  @param SlotIndex Index of the connection slot.
  @return String The incoming message. If no messages are available, returns an empty string.
}
function GetIncomingMessage(const SlotIndex: Integer): String;
{
  Retrieves the number of incoming messages in the message queue for the given connection slot.

  @param SlotIndex Index of the connection slot.
  @return The number of incoming messages.
}
function GetIncomingMessageLength(const SlotIndex: Integer): Integer;
{
  Clears all incoming messages from the message queue for the given connection slot.

  @param SlotIndex Index of the connection slot.
}
procedure ClearIncomingMessages(SlotIndex: Integer);

{
  Updates the bot information in the bot list if the bot exists.
  If the bot does not exist, it adds a new entry.

  @param BotIP String The IP address of the bot to update.
}
procedure UpdateBotData(const BotIP: String);

{
  Removes all bots from the bot list and resets the bot clear time.
}
procedure RemoveAllBots();
{
  Checks if a bot with the given IP address exists in the bot list.

  @param BotIP The IP address of the bot to check.
  @returns @true if the bot exists, @false otherwise.
}
function BotExists(const BotIp: String): Boolean;

{
  Populates the node list by extracting nodes from configuration data and verificators text.
  Each node is parsed from a string and added to the list with its IP address and port.
}
procedure PopulateNodeList();
{
  Returns the length of the node list.

  @return The length of the node list.
}
function GetNodeListLength(): Integer;
{
  Retrieves the node data at the specified index from the node list.

  @param Index The index of the node data to retrieve.
  @return The node data at the specified index, or a default value if the index is out of bounds.
}
function GetNodeDataAtIndex(const Index: Integer): TNodeData;

procedure InitializeElements();
procedure ClearElements();

var
  { Holds the connection data for each client, indexed by connection slot. }
  Connections: TConnectionList;

  { Holds the incoming text lines for each connection slot, indexed by connection slot. }
  SlotTextLines: TConnectionStringListArray;

  { Holds the TCP client channels for each connection slot, indexed by connection slot. }
  ClientChannels: TConnectionChannelList;

  { Holds the outgoing messages for each connection slot, indexed by connection slot. }
  OutgoingMessages: TConnectionOutgoingMessagesList;

  { A list of bot-related data, representing known bot nodes. }
  BotList: specialize TArray<TBotData>;

  { A pool of pending transactions awaiting processing. }
  PendingTransactionsPool: specialize TArray<TOrderData>;

  { A pool of multi-order transactions awaiting processing. }
  MultiOrderTransactionsPool: specialize TArray<TMultiOrderData>;

  { Indicates whether the headers are currently being downloaded. }
  DownloadingHeaders: Boolean = False;

  { Indicates whether the summary is currently being downloaded. }
  DownloadingSummary: Boolean = False;

  { Indicates whether the blocks are currently being downloaded. }
  DownloadingBlocks: Boolean = False;

  { Indicates whether the GVTs are currently being downloaded. }
  DownloadingGVTs: Boolean = False;

  { Indicates whether the PSOs are currently being downloaded. }
  DownloadingPSOs: Boolean = False;

  { Timestamp for the last time the masternode hash request was made. }
  LastMasternodeHashRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the best hash request was made. }
  LastBestHashRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the masternode list request was made. }
  LastMasternodeListRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the masternode check request was made. }
  LastMasternodeCheckRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the masternode verification was performed. }
  LastMasternodeVerificationTime: TTimestampInteger = 0;

  { Timestamp for the last time the GVTs request was made. }
  LastGVTsRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the summary request was made. }
  LastSummaryRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the block request was made. }
  LastBlockRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the account summary request was made. }
  LastAccountSummaryRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time pending transactions were requested. }
  LastPendingTransactionsRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the PSOs request was made. }
  LastPSOsRequestTime: TTimestampInteger = 0;

  { Timestamp for the last time the bot list was cleared. }
  LastBotClearTime: TTimestampInteger = 0;

  { Indicates whether to force a download of the headers. }
  ForceHeadersDownload: Boolean = False;

  { Count of masternode verification processes completed. }
  MasternodeVerificationCount: Integer = 0;

  { The last block index that was processed. }
  LastBlockIndex: Integer = 0;

  { The hash of the last processed block. }
  LastBlockHash: String = '';

  { The public IP address of the current node. }
  PublicIPAddress: String = '';

  { Data for the last block processed by the node. }
  LastBlockData: BlockHeaderData;

  { The count of active client read threads. }
  ActiveClientReadThreads: Integer = 0;

  { Critical section for managing access to client read threads. }
  CSClientReadThreads: TRTLCriticalSection;

  { Critical sections for synchronizing access to incoming messages for each connection slot. }
  CSIncomingMessages: TConnectionCriticalSectionList;

  { Critical sections for synchronizing access to outgoing messages for each connection slot. }
  CSOutgoingMessages: TConnectionCriticalSectionList;

  { Critical section for managing access to the connections list. }
  CSConnections: TRTLCriticalSection;

  { Critical section for managing access to the bot list. }
  CSBotList: TRTLCriticalSection;

  { Critical section for managing access to the pending transactions pool. }
  CSPendingTransactions: TRTLCriticalSection;

  { Critical section for managing access to the multi-order transactions pool. }
  CSMultiOrderTransactions: TRTLCriticalSection;

  { A list of known nodes in the network. }
  NodeList: specialize TArray<TNodeData>;

  { Critical section for managing access to the node list. }
  CSNodeList: TRTLCriticalSection;

  { The TCP server instance for managing node-to-node communications. }
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
  StartPerformanceMeasurement('AddTransactionToPool');
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

  StopPerformanceMeasurement('AddTransactionToPool');
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
    [GetTotalConnections(), LastBlockIndex, LastBlockHash, ComputeSummaryHash,
    GetPendingTransactionCount(), GetSummaryFileHash, MyConStatus, Port,
    Copy(GetMNsHash, 0, 5), GetMNsListLength, GetMasternodeCheckCount(),
    GVTHashMD5, Copy(HashMD5String(GetCFGDataStr), 0, 5), Copy(PSOFileHash, 0, 5)]);
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

  if Copy(ComputeSummaryHash, 0, 5) <> GetConsensus(cSumHash) then
    Result := ssSummaryHashMismatch;

  if Copy(GetSummaryFileHash, 0, 5) <> GetConsensus(cHeaders) then
    Result := ssResumenHashMismatch;
end;

function GetSynchronizationStatus(): String;
begin
  Result := '';

  try
    Result := Format('%d%s%s%s', [LastBlockIndex, Copy(GetSummaryFileHash, 1, 3),
      Copy(ComputeSummaryHash, 1, 3), Copy(LastBlockHash, 1, 3)]);
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,GetSynchronizationStatus,' + e.Message);
  end;
end;

procedure ClearOutgoingTextForSlot(const Slot: Integer);
begin
  if (Slot < Low(OutgoingMessages)) or (Slot > High(OutgoingMessages)) then
    Exit;

  EnterCriticalSection(CSOutgoingMessages[Slot]);
  try
    SetLength(OutgoingMessages[Slot], 0);
  finally
    LeaveCriticalSection(CSOutgoingMessages[Slot]);
  end;
end;

function GetOutgoingTextForSlot(const Slot: Integer): String;
begin
  Result := EmptyStr;

  if (Slot < Low(OutgoingMessages)) or (Slot > High(OutgoingMessages)) then Exit;

  EnterCriticalSection(CSOutgoingMessages[Slot]);
  try
    if Length(OutgoingMessages[Slot]) > 0 then
    begin
      Result := OutgoingMessages[Slot][0];
      Delete(OutgoingMessages[Slot], 0, 1);
    end
  finally
    LeaveCriticalSection(CSOutgoingMessages[Slot]);
  end;
end;

procedure AddTextToSlot(const Slot: Integer; const Text: String);
begin
  if (Slot < Low(OutgoingMessages)) or (Slot > High(OutgoingMessages)) then Exit;

  EnterCriticalSection(CSOutgoingMessages[Slot]);
  try
    SetLength(OutgoingMessages[Slot], Length(OutgoingMessages[Slot]) + 1);
    OutgoingMessages[Slot][High(OutgoingMessages[Slot])] := Text;
  finally
    LeaveCriticalSection(CSOutgoingMessages[Slot]);
  end;
end;

function GetConnectionData(const Slot: Integer): TConnectionData;
begin
  Result := Default(TConnectionData);
  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  EnterCriticalSection(CSConnections);
  try
    Result := Connections[Slot];
  finally
    LeaveCriticalSection(CSConnections);
  end;
end;

procedure SetConnectionData(const Slot: Integer; const Data: TConnectionData);
begin
  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  EnterCriticalSection(CSConnections);
  try
    Connections[Slot] := Data;
  finally
    LeaveCriticalSection(CSConnections);
  end;
end;

procedure SetConnectionBusy(const Slot: Integer; Busy: Boolean);
begin
  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  EnterCriticalSection(CSConnections);
  try
    Connections[Slot].IsBusy := Busy;
  finally
    LeaveCriticalSection(CSConnections);
  end;
end;

procedure UpdateConnectionLastPing(const Slot: Integer; Value: String);
begin
  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  EnterCriticalSection(CSConnections);
  try
    Connections[Slot].LastPingTime := Value;
  finally
    LeaveCriticalSection(CSConnections);
  end;
end;

procedure ReserveConnectionSlot(const Slot: Integer; IsReserved: Boolean);
var
  Status: String;
begin
  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  Status := IfThen(IsReserved, 'RES', '');

  EnterCriticalSection(CSConnections);
  try
    Connections[Slot].ConnectionType := Status;
  finally
    LeaveCriticalSection(CSConnections);
  end;
end;

procedure StartConnectionThread(const Slot: Integer);
begin
  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  EnterCriticalSection(CSConnections);
  try
    Connections[Slot].ReadThread := TClientReadThread.Create(True, Slot);
    Connections[Slot].ReadThread.FreeOnTerminate := True;
    Connections[Slot].ReadThread.Start;
  finally
    LeaveCriticalSection(CSConnections);
  end;
end;

procedure CloseConnectionSlot(const Slot: Integer);
begin
  if (Slot < Low(ClientChannels)) or (Slot > High(ClientChannels)) then Exit;

  StartPerformanceMeasurement('CloseConnectionSlot');
  try
    if GetConnectionData(Slot).ConnectionType = 'CLI' then
    begin
      ClearIncomingMessages(Slot);
      GetConnectionData(Slot).Context.Connection.Disconnect;
      Sleep(10);
    end
    else if GetConnectionData(Slot).ConnectionType = 'SER' then
    begin
      ClearIncomingMessages(Slot);
      ClientChannels[Slot].IOHandler.InputBuffer.Clear;
      ClientChannels[Slot].Disconnect;
    end;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,CloseConnectionSlot,' + E.Message);
  end;
  SetConnectionData(Slot, Default(TConnectionData));
  StopPerformanceMeasurement('CloseConnectionSlot');
end;

function GetTotalConnections(): Integer;
var
  Slot: Integer;
begin
  StartPerformanceMeasurement('GetTotalConnections');
  Result := 0;

  for Slot := 1 to MaxConnections do
  begin
    if IsSlotConnected(Slot) then
      Inc(Result);
  end;

  StopPerformanceMeasurement('GetTotalConnections');
end;

function IsSlotConnected(const Slot: Integer): Boolean;
var
  ConnectionType: String;
begin
  Result := False;

  if (Slot < Low(Connections)) or (Slot > High(Connections)) then Exit;

  ConnectionType := GetConnectionData(Slot).ConnectionType;
  Result := (ConnectionType = 'SER') or (ConnectionType = 'CLI');
end;

procedure UpdateNodeData();
begin
  LastBlockHash := HashMD5File(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
  LastBlockData := LoadBlockDataHeader(LastBlockIndex);
  SetSummaryFileHash;

  if GetSummaryFileHash = GetConsensus(5) then
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

  if MnsCheckExists(CheckData.ValidatorIP) then
    Exit;

  if not IsNodeValidator(CheckData.ValidatorIP) then
    ErrorCode := 1;

  if CheckData.Block <> LastBlockIndex then
    ErrorCode := 2;

  if GetAddressFromPublicKey(CheckData.PublicKey) <> CheckData.SignAddress then
    ErrorCode := 3;

  if not VerifySignedString(CheckData.ValidNodes, CheckData.Signature,
    CheckData.PublicKey) then
    ErrorCode := 4;

  if ErrorCode = 0 then
  begin
    Result := ReportInfo;
    AddMNCheck(CheckData);
  end;
end;

procedure InitializeNodeServer();
begin
  NodeServer := TIdTCPServer.Create(nil);
  NodeServer.DefaultPort := 8080;
  NodeServer.Active := False;
  NodeServer.UseNagle := True;
  NodeServer.TerminateWaitTime := 10000;

  // These are no-ops for now.
  NodeServer.OnExecute := @TNodeServerEvents.OnExecute;
  NodeServer.OnConnect := @TNodeServerEvents.OnConnect;

  NodeServer.OnDisconnect := @form1.IdTCPServer1Disconnect;
  NodeServer.OnException := @Form1.IdTCPServer1Exception;
end;

function GetClientCount: Integer;
var
  Clients: TList;
begin
  Clients := Nodeserver.Contexts.LockList;
  try
    try
      Result := Clients.Count;
    except
      on E: Exception do
        ToDeepDebug('NosoNetwork,ClientsCount,' + E.Message);
    end
  finally
    Nodeserver.Contexts.UnlockList;
  end;
end;

function SendMessageToClient(AContext: TIdContext; Message: String): Boolean;
begin
  Result := True;
  try
    AContext.Connection.IOHandler.WriteLn(Message);
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoNetwork,SendMessageToClient,' + E.Message);
      Result := False;
    end;
  end;
end;

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
      ToDeepDebug('NosoNetwork,RetrieveStreamFromClient,' + E.Message);
  end;
end;

procedure SafelyCloseClientConnection(AContext: TIdContext; CloseMessage: String = '');
begin
  try
    // Send a closing message if provided
    if CloseMessage <> '' then
      AContext.Connection.IOHandler.WriteLn(CloseMessage);

    // Disconnect the client
    AContext.Connection.Disconnect();
    // Clear input buffer after disconnecting
    AContext.Connection.IOHandler.InputBuffer.Clear;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,SafelyCloseClientConnection,' + E.Message);
  end;
end;

class procedure TNodeServerEvents.OnExecute(AContext: TIdContext);
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

{$REGION Thread Client read}

constructor TClientReadThread.Create(const CreatePaused: Boolean;
  const ConnectionSlot: Integer);
begin
  inherited Create(CreatePaused);
  FSlot := ConnectionSlot;
end;

{
  Sends a line of text to a client specified by the connection slot.

  @param Slot The slot number of the client to send the line to.
  @param Line The string of text to send to the client.
  @returns @true if the line was successfully sent, otherwise @false.
}
function LineToClient(const Slot: Integer; const Line: String): Boolean;
begin
  Result := False;
  try
    ClientChannels[Slot].IOHandler.WriteLn(Line);
    Result := True;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,LineToClient,' + E.Message);
  end;
end;

{
  Retrieves a stream of data from a client specified by the connection slot.

  @param Slot The slot number of the client to retrieve the stream from.
  @param Stream The memory stream to store the retrieved data.
  @returns @true if the stream was successfully retrieved, otherwise @false.
}
function GetStreamFromClient(const Slot: Integer; out Stream: TMemoryStream): Boolean;
begin
  Result := False;
  Stream.Clear;
  try
    ClientChannels[Slot].IOHandler.ReadStream(Stream);
    Result := True;
  except
    on E: Exception do
      ToDeepDebug('NosoNetwork,GetStreamFromClient,' + E.Message);
  end;
end;

{
  Sends a line of text to a client using the current thread's connection slot.

  @param Slot The slot number associated with the thread.
  @param Line The string of text to send to the client.
  @returns @true if the line was successfully sent, otherwise @false.
}
function SendLineToClient(const Slot: Integer; const Line: String): Boolean;
begin
  Result := True;
  try
    ClientChannels[Slot].IOHandler.Writeln(Line);
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDebug('NosoNetwork,SendLineToClient,' + E.Message);
    end;
  end;
end;

procedure TClientReadThread.Execute;
var
  IncomingLine: String;
  Stream: TMemoryStream;
  IsBufferAvailable, ShouldTerminate, IsSaved: Boolean;
  OutgoingLine, ReadThreadID: String;
  LastActivityTime: Int64;
begin
  ReadThreadID := Format('ReadClient %d %s', [FSlot, UTCTimeStr]);
  ClientChannels[FSlot].ReadTimeout := 1000;
  ClientChannels[FSlot].IOHandler.MaxLineLength := MaxInt;

  IncrementClientReadThreadCount;
  AddNewOpenThread(ReadThreadID, UTCTime);
  LastActivityTime := UTCTime;

  repeat
  try
    Sleep(10);
    IsBufferAvailable := True;

    // Check if buffer is empty and wait for data
    if ClientChannels[FSlot].IOHandler.InputBufferIsEmpty then
    begin
      ClientChannels[FSlot].IOHandler.CheckForDataOnSource(1000);
      if ClientChannels[FSlot].IOHandler.InputBufferIsEmpty then
      begin
        IsBufferAvailable := False;

        // Send any pending outgoing messages
        repeat
          OutgoingLine := GetOutgoingTextForSlot(FSlot);
          if OutgoingLine <> '' then
          begin
            if not SendLineToClient(FSlot, OutgoingLine) then
            begin
              ShouldTerminate := True;
              Break;
            end;
          end;
        until OutgoingLine = '';
      end;
    end;

    // Handle incoming data if available
    if IsBufferAvailable then
    begin
      while not ClientChannels[FSlot].IOHandler.InputBufferIsEmpty do
      begin
        SetConnectionBusy(FSlot, True);
        UpdateConnectionLastPing(FSlot, UTCTimeStr);
        IncomingLine := '';

        try
          IncomingLine := ClientChannels[FSlot].IOHandler.ReadLn(
            IndyTextEncoding_UTF8);
        except
          on E: Exception do
          begin
            SetConnectionBusy(FSlot, False);
            ShouldTerminate := True;
            Break;
          end;
        end;

        if IncomingLine <> '' then
        begin
          LastActivityTime := UTCTime;
          UpdateOpenThread(ReadThreadID, UTCTime);
          ClientChannels[FSlot].ReadTimeout := 10000;

          // RESUMENFILE
          if GetParameter(IncomingLine, 0) = 'RESUMENFILE' then
          begin
            DownloadingHeaders := True;
            Stream := TMemoryStream.Create;
            try
              IsSaved := GetStreamFromClient(FSlot, Stream) and
                SaveMemoryStreamAsHeaders(Stream);
              if IsSaved then
                UpdateNodeData()
              else
                ShouldTerminate := True;
            finally
              Stream.Free;
            end;
            DownloadingHeaders := False;
            LastAccountSummaryRequestTime := 0;
          end

          // SUMARYFILE
          else if GetParameter(IncomingLine, 0) = 'SUMARYFILE' then
          begin
            DownloadingSummary := True;
            Stream := TMemoryStream.Create;
            try
              IsSaved := GetStreamFromClient(FSlot, Stream) and
                (SaveSummaryToFile(Stream) <> 0);
              if IsSaved then
              begin
                UpdateNodeData();
                CreateSummaryIndex();
              end
              else
                ShouldTerminate := True;
            finally
              Stream.Free;
            end;
            DownloadingSummary := False;
            LastSummaryRequestTime := 0;
          end

          // PSOSFILE
          else if GetParameter(IncomingLine, 0) = 'PSOSFILE' then
          begin
            DownloadingPSOs := True;
            Stream := TMemoryStream.Create;
            try
              IsSaved := GetStreamFromClient(FSlot, Stream) and SavePSOsToFile(Stream);
              if IsSaved then
              begin
                LoadPSOFileFromDisk;
                UpdateNodeData();
              end
              else
                ShouldTerminate := True;
            finally
              Stream.Free;
            end;
            DownloadingPSOs := False;
            LastPSOsRequestTime := 0;
          end

          // GVTSFILE
          else if GetParameter(IncomingLine, 0) = 'GVTSFILE' then
          begin
            DownloadingGVTs := True;
            Stream := TMemoryStream.Create;
            try
              IsSaved := GetStreamFromClient(FSlot, Stream) and
                SaveStreamAsGVTs(Stream);
              if IsSaved then
                LoadGVTsFileData()
              else
                ShouldTerminate := True;
            finally
              Stream.Free;
            end;
            DownloadingGVTs := False;
            LastGVTsRequestTime := 0;
          end

          // BLOCKZIP
          else if GetParameter(IncomingLine, 0) = 'BLOCKZIP' then
          begin
            DownloadingBlocks := True;
            Stream := TMemoryStream.Create;
            try
              IsSaved := GetStreamFromClient(FSlot, Stream) and
                SaveStreamAsZipBlocks(Stream);
              if IsSaved and UnzipFile(BlockDirectory + 'blocks.zip', True) then
              begin
                LastBlockIndex := GetMyLastUpdatedBlock();
                LastBlockHash :=
                  HashMD5File(BlockDirectory + IntToStr(LastBlockIndex) + '.blk');
                UpdateNodeData();
              end
              else
                ShouldTerminate := True;
            finally
              Stream.Free;
            end;
            DownloadingBlocks := False;
            LastBlockRequestTime := 0;
          end

          // All other messages
          else
          begin
            AddIncomingMessage(FSlot, IncomingLine);
          end;
        end;

        SetConnectionBusy(FSlot, False);
      end;
    end;

    // Check for termination conditions
    if (LastActivityTime + 30 < UTCTime) or
      (GetConnectionData(FSlot).ConnectionType <> 'SER') or
      (not ClientChannels[FSlot].Connected) then
      ShouldTerminate := True;

  except
    on E: Exception do
    begin
      ToDeepDebug('NosoNetwork,TThreadClientRead,' + E.Message);
      ShouldTerminate := True;
    end;
  end;
  until Terminated or ShouldTerminate;

  // Cleanup after thread termination
  CloseConnectionSlot(FSlot);
  DecrementClientReadThreadCount;
  CloseOpenThread(ReadThreadID);
end;

procedure IncrementClientReadThreadCount();
begin
  EnterCriticalSection(CSClientReadThreads);
  try
    Inc(ActiveClientReadThreads);
  finally
    LeaveCriticalSection(CSClientReadThreads);
  end;
end;

procedure DecrementClientReadThreadCount();
begin
  EnterCriticalSection(CSClientReadThreads);
  try
    Dec(ActiveClientReadThreads);
  finally
    LeaveCriticalSection(CSClientReadThreads);
  end;
end;

function GetActiveClientReadThreadCount(): Integer;
begin
  EnterCriticalSection(CSClientReadThreads);
  try
    Result := ActiveClientReadThreads;
  finally
    LeaveCriticalSection(CSClientReadThreads);
  end;
end;

procedure AddIncomingMessage(const SlotIndex: Integer; const IncomingMessage: String);
begin
  EnterCriticalSection(CSIncomingMessages[SlotIndex]);
  try
    SlotTextLines[SlotIndex].Add(IncomingMessage);
  finally
    LeaveCriticalSection(CSIncomingMessages[SlotIndex]);
  end;
end;

function GetIncomingMessage(const SlotIndex: Integer): String;
begin
  Result := '';
  if GetIncomingMessageLength(SlotIndex) > 0 then
  begin
    EnterCriticalSection(CSIncomingMessages[SlotIndex]);
    try
      Result := SlotTextLines[SlotIndex][0];
      SlotTextLines[SlotIndex].Delete(0);
    finally
      LeaveCriticalSection(CSIncomingMessages[SlotIndex]);
    end;
  end;
end;

function GetIncomingMessageLength(const SlotIndex: Integer): Integer;
begin
  EnterCriticalSection(CSIncomingMessages[SlotIndex]);
  try
    Result := SlotTextLines[SlotIndex].Count;
  finally
    LeaveCriticalSection(CSIncomingMessages[SlotIndex]);
  end;
end;

procedure ClearIncomingMessages(SlotIndex: Integer);
begin
  EnterCriticalSection(CSIncomingMessages[SlotIndex]);
  try
    SlotTextLines[SlotIndex].Clear;
  finally
    LeaveCriticalSection(CSIncomingMessages[SlotIndex]);
  end;
end;


procedure UpdateBotData(const BotIP: String);
var
  Index: Integer;
  BotFound: Boolean;
begin
  if IsSeedNode(BotIP) then Exit;

  BotFound := False;
  EnterCriticalSection(CSBotList);
  try
    for Index := 0 to High(BotList) do
    begin
      if BotList[Index].IpAddress = BotIP then
      begin
        BotList[Index].LastRefused := UTCTime;
        BotFound := True;
        Break;
      end;
    end;

    if not BotFound then
    begin
      SetLength(BotList, Length(BotList) + 1);
      BotList[High(BotList)].IpAddress := BotIP;
      BotList[High(BotList)].LastRefused := UTCTime;
    end;
  finally
    LeaveCriticalSection(CSBotList);
  end;
end;

procedure RemoveAllBots();
begin
  EnterCriticalSection(CSBotList);
  try
    SetLength(BotList, 0);
    LastBotClearTime := UTCTime;
  finally
    LeaveCriticalSection(CSBotList);
  end;
end;

function BotExists(const BotIp: String): Boolean;
var
  Index: Integer;
begin
  Result := False;

  EnterCriticalSection(CSBotList);
  try
    for Index := 0 to High(BotList) do
    begin
      if BotList[Index].IpAddress = BotIP then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(CSBotList);
  end;
end;

procedure PopulateNodeList();
var
  NodeIndex, NodePort: Integer;
  NodeDataStr, SourceStr, NodeIP: String;
  IsValidNode: Boolean;
  NewNode: TNodeData;
begin
  NodeIndex := 0;
  IsValidNode := True;

  // Get source data (from config and verificators text)
  SourceStr := GetParameter(GetCFGDataStr, 1) + GetVerificatorsText;
  SourceStr := StringReplace(SourceStr, ':', ' ', [rfReplaceAll, rfIgnoreCase]);

  EnterCriticalSection(CSNodeList);
  try
    SetLength(NodeList, 0);

    // Loop to parse each node from source string
    repeat
      NodeDataStr := GetParameter(SourceStr, NodeIndex);
      NodeDataStr := StringReplace(NodeDataStr, ';', ' ', [rfReplaceAll, rfIgnoreCase]);

      // Extract port and IP address
      NodePort := StrToIntDef(GetParameter(NodeDataStr, 1), 8080);
      NodeIP := GetParameter(NodeDataStr, 0);

      if NodeIP = '' then
        IsValidNode := False
      else
      begin
        NewNode.IpAddress := NodeIP;
        NewNode.Port := IntToStr(NodePort);
        Insert(NewNode, NodeList, Length(NodeList));
        Inc(NodeIndex);
      end;
    until not IsValidNode;
  finally
    LeaveCriticalSection(CSNodeList);
  end;
end;

function GetNodeListLength(): Integer;
begin
  EnterCriticalSection(CSNodeList);
  try
    Result := Length(NodeList);
  finally
    LeaveCriticalSection(CSNodeList);
  end;
end;

function GetNodeDataAtIndex(const Index: Integer): TNodeData;
begin
  Result := Default(TNodeData);

  if (Index < 0) or (Index >= GetNodeListLength) then
    Exit;

  EnterCriticalSection(CSNodeList);
  try
    Result := NodeList[Index];
  finally
    LeaveCriticalSection(CSNodeList);
  end;
end;

procedure InitializeElements();
var
  ConnectionIndex: Integer;
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

  for ConnectionIndex := 1 to MaxConnections do
  begin
    InitCriticalSection(CSIncomingMessages[ConnectionIndex]);
    InitCriticalSection(CSOutgoingMessages[ConnectionIndex]);

    // Create string lists for incoming messages
    SlotTextLines[ConnectionIndex] := TStringList.Create;

    // Create TCP client channel for each slot
    ClientChannels[ConnectionIndex] := TIdTCPClient.Create(nil);

    // Initialize outgoing message array
    SetLength(OutgoingMessages[ConnectionIndex], 0);
  end;
end;

procedure ClearElements();
var
  ConnectionIndex: Integer;
begin
  DoneCriticalSection(CSClientReadThreads);
  DoneCriticalSection(CSConnections);
  DoneCriticalSection(CSBotList);
  DoneCriticalSection(CSMultiOrderTransactions);
  DoneCriticalSection(CSNodeList);

  for ConnectionIndex := 1 to MaxConnections do
  begin
    // Release critical sections for message handling
    DoneCriticalSection(CSIncomingMessages[ConnectionIndex]);
    DoneCriticalSection(CSOutgoingMessages[ConnectionIndex]);

    // Free the string lists for incoming messages
    SlotTextLines[ConnectionIndex].Free;

    // Free the TCP client channel for each slot
    ClientChannels[ConnectionIndex].Free;
  end;
end;

initialization
  InitializeElements;

finalization
  ClearElements;

end.
