// Noso Unit to get a consensus

unit Noso.Consensus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils,
  Noso.Debug, Noso.Time, Noso.General, Noso.Crypto;

type

  { This class represents a thread that communicates with a specific node to
    obtain its status. }
  TNodeStatusThread = class(TThread)
  private
    Slot: Integer; //< The index of the node in the node list
  protected
    { Executes the thread's main task: requesting the node's status. }
    procedure Execute; override;
  public
    {
      Creates a TNodeStatusThread.
      @param(CreatePaused Whether the thread should be created in a paused state.)
      @param(TSlot The index of the node in the node list.)
    }
    constructor Create(const CreatePaused: Boolean; TSlot: Integer);
  end;

  { This thread automatically computes consensus at regular intervals based on
    block age and timestamp. }
  TAutoConsensusThread = class(TThread)
  protected
    { Executes the automatic consensus computation process. }
    procedure Execute; override;
  public
    {
      Creates a TAutoConsensusThread.
      @param(CreateSuspended Whether the thread should be created in a suspended state.)
    }
    constructor Create(CreateSuspended: Boolean);
  end;

  { Array that stores consensus results. }
  TConsensus = array of String[32];

  { Record type to store information about a node in the network. }
  TNodeConsensus = record
    Host: String;               //< Node's hostname or IP address
    Port: Integer;              //< Node's port number
    Data: String;               //< Data returned from the node
    ConnectionHash: String[32]; //< A 32-character connection hash
    BlockNumber: Integer;       //< The block number reported by the node
    PeerCount: Integer;         //< The number of peers the node is connected to
  end;

  { Record to store consensus values and their occurrence count. }
  TConsensusData = record
    Value: String;  //< The consensus value
    Count: Integer; //< The number of occurrences of this value
  end;

{
  Returns the number of active threads.
  @returns the number of active threads.
}
function GetActiveThreadCount(): Integer;
{
  Compute consensus data from the node list.
  @param NodeList Optional node list as a string.
  @returns Array of consensus results.
}
function ComputeConsensus(NodeList: String = ''): TConsensus;
{
  Returns the consensus result for the given index.
  @param(Index The index of the consensus data.)
  @returns(A string representing the consensus value.)
}
function GetConsensusData(Index: Integer = 0): String;
{ Returns a randomly selected node from the node list as a string. }
function GetRandomNode(): String;
{ Computes a consensus hash based on the given status line.
  @param(Line The status line received from the node.)
  @returns(A string representing the computed hash.) }
function GetConsensusHash(Line: String): String;
{ Sets the node list based on the input string.
  @param(NodeListString The node list as a string.) }
procedure SetNodeList(NodeListString: String);
{ Returns the node data for the given index.
  @param(Index The index of the node.)
  @returns(The consensus data for the specified node.) }
function GetNodeDataByIndex(Index: Integer): TNodeConsensus;
{ Returns the number of nodes in the node list. }
function GetNodeCount(): Integer;
{ Starts the automatic consensus thread. }
procedure StartAutoConsensus();
{ Stops the automatic consensus thread. }
procedure StopAutoConsensus();

const
  cLastBlock = 2;    //< The index for last block in consensus results
  cHeaders = 5;      //< The index for headers in consensus results
  cMNsHash = 8;      //< The index for MNs hash in consensus results
  cMNsCount = 9;     //< The index for MNs count in consensus results
  cLBHash = 10;      //< The index for last block hash in consensus results
  cLBTimeEnd = 12;   //< The index for last block time end in consensus results
  cSumHash = 17;     //< The index for summary hash in consensus results
  cGVTsHash = 18;    //< The index for GVTs hash in consensus results
  cCFGHash = 19;     //< The index for Noso configuration hash in consensus results

var
  // Stores the computed consensus results
  ConsensusResults: TConsensus;
  // Stores the consensus labels
  ConsensusLabels: array[0..20] of
  String = ('Resume', 'Peers', 'LBlock', 'Pending', 'Delta', 'Headers',
    'Version', 'UTCTime', 'MNsHash', 'MNsCount', 'LBHash', 'BestDiff',
    'LBTimeEnd', 'LBMiner', 'ChecksCount', 'LBPoW', 'LBDiff', 'Summary',
    'GVTs', 'NosoCFG', 'PSOHash');
  // Total number of nodes
  TotalNodes: Integer = 0;
  // Nodes reached during consensus
  ReachedNodes: Integer = 0;
  // Valid nodes in the consensus process
  ValidNodes: Integer = 0;
  // Percentage of nodes reached
  ConsensusPercentage: Integer = 0;
  // Whether consensus is completed
  ConsensusCompleted: Boolean = False;
  // Timestamp of last consensus
  LastConsensusTimestamp: Int64 = 0;

implementation

var
  // Internal list of nodes
  NodeList: array of TNodeConsensus;
  // Critical section for node list access
  NodeListLock: TRTLCriticalSection;
  // Number of currently active threads
  ActiveThreadCount: Integer;
  // Number of nodes successfully reached
  NodesReached: Integer;
  // Critical section for thread management
  ThreadLock: TRTLCriticalSection;
  // Critical section for consensus management
  ConsensusLock: TRTLCriticalSection;
  // Whether auto-consensus is enabled
  AutoConsensusEnabled: Boolean = False;
  // Whether a consensus process is currently running
  IsConsensusRunning: Boolean = False;
  // ID of the current round of consensus
  ActiveRoundID: Int64 = 0;

  {$REGION Thread auto update}

procedure StartAutoConsensus();
var
  AutoThread: TAutoConsensusThread;
begin
  if AutoConsensusEnabled then
    Exit;

  AutoConsensusEnabled := True;
  AutoThread := TAutoConsensusThread.Create(True);
  AutoThread.FreeOnTerminate := True;
  AutoThread.Start;
end;

procedure StopAutoConsensus();
begin
  AutoConsensusEnabled := False;
end;

constructor TAutoConsensusThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TAutoConsensusThread.Execute;
begin
  repeat
    if (BlockAge >= 0) and (BlockAge < 5) then LastConsensusTimestamp := 0;
    if (BlockAge >= 5) and (BlockAge < 585) and
      (LastConsensusTimestamp + 60 < UTCTime) then
    begin
      LastConsensusTimestamp := UTCTime;
      ComputeConsensus();
    end;
    Sleep(100);
  until Terminated or not AutoConsensusEnabled;
end;

{$ENDREGION}

{$REGION Open threads}

{ Decreases the count of active threads and, if a node is successfully reached,
  increments the count of reached nodes.
  @param(NodeReached Whether the node was successfully reached.) }
procedure DecreaseActiveThreads(NodeReached: Boolean);
begin
  EnterCriticalSection(ThreadLock);
  try
    Dec(ActiveThreadCount);
    if NodeReached then
      Inc(NodesReached);
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

function GetActiveThreadCount(): Integer;
begin
  EnterCriticalSection(ThreadLock);
  try
    Result := ActiveThreadCount;
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

{ Returns the index of the node from the internal node list.
  @param(Index The index of the node.)
  @returns(The node at the given index.) }
function GetNodeIndex(Index: Integer): TNodeConsensus;
begin
  EnterCriticalSection(NodeListLock);
  try
    Result := NodeList[Index];
  finally
    LeaveCriticalSection(NodeListLock);
  end;
end;

{$ENDREGION}

{$REGION Thread consulting node}

constructor TNodeStatusThread.Create(const CreatePaused: Boolean; TSlot: Integer);
begin
  inherited Create(CreatePaused);
  Slot := TSlot;
  FreeOnTerminate := True;
end;

procedure TNodeStatusThread.Execute;
var
  CurrentNode: TNodeConsensus;
  ResponseLine: String;
  NodeReached: Boolean = False;
  ConnectionHash: String = '';
  RoundID: Int64;
begin
  RoundID := ActiveRoundID;
  CurrentNode := GetNodeIndex(slot);
  ResponseLine := RequestLineToPeer(CurrentNode.Host, CurrentNode.Port, 'NODESTATUS');
  if Copy(ResponseLine, 1, 10) = 'NODESTATUS' then
  begin
    ConnectionHash := GetConsensusHash(ResponseLine);
    ResponseLine := StringReplace(ResponseLine, 'NODESTATUS',
      ConnectionHash, [rfReplaceAll, rfIgnoreCase]);
    CurrentNode.Data := ResponseLine;
    CurrentNode.ConnectionHash := ConnectionHash;
    CurrentNode.PeerCount := StrToIntDef(GetParameter(ResponseLine, 1), 0);
    CurrentNode.BlockNumber := StrToIntDef(GetParameter(ResponseLine, 2), 0);
    NodeReached := True;
  end
  else
  begin
    CurrentNode.Data := '';
    CurrentNode.ConnectionHash := '';
    CurrentNode.PeerCount := 0;
    CurrentNode.BlockNumber := 0;
  end;

  if RoundID = ActiveRoundID then
  begin
    EnterCriticalSection(NodeListLock);
    try
      NodeList[slot] := CurrentNode;
    finally
      LeaveCriticalSection(NodeListLock);
      DecreaseActiveThreads(NodeReached);
    end;
  end;
end;

{$ENDREGION}

function GetConsensusHash(Line: String): String;
begin
  Result := HashMD5String(GetParameter(Line, 2) + Copy(GetParameter(Line, 5), 0, 5) +
    Copy(GetParameter(Line, 8), 0, 5) + Copy(GetParameter(Line, 10), 0, 5) +
    Copy(GetParameter(Line, 17), 0, 5) + Copy(GetParameter(Line, 18), 0, 5) +
    Copy(GetParameter(Line, 19), 0, 5));
end;

function GetRandomNode(): String;
var
  NodeIndex: Integer;
begin
  Result := '';
  EnterCriticalSection(NodeListLock);
  try
    NodeIndex := Random(Length(NodeList));
    Result := Format('%s %d', [NodeList[NodeIndex].Host, NodeList[NodeIndex].Port]);
  finally
    LeaveCriticalSection(NodeListLock);
  end;
end;

procedure SetNodeList(NodeListString: String);
var
  i: Integer;
  NodeEntries: array of String;
begin
  while IsConsensusRunning do Sleep(5); // ????????????

  SetLength(NodeList, 0);
  NodeListString := Trim(StringReplace(NodeListString, ':', ' ',
    [rfReplaceAll, rfIgnoreCase]));
  NodeEntries := SplitString(NodeListString, ' ');

  EnterCriticalSection(NodeListLock);
  try
    for i := 0 to high(NodeEntries) do
    begin
      NodeEntries[i] := StringReplace(NodeEntries[i], ';', ' ',
        [rfReplaceAll, rfIgnoreCase]);
      SetLength(NodeList, Length(NodeList) + 1);
      NodeList[High(NodeList)].Host := GetParameter(NodeEntries[I], 0);
      NodeList[High(NodeList)].Port :=
        StrToIntDef(GetParameter(NodeEntries[I], 1), 8080);
      NodeList[High(NodeList)].Data := '';
      NodeList[High(NodeList)].ConnectionHash := '';
      NodeList[High(NodeList)].BlockNumber := 0;
      NodeList[High(NodeList)].PeerCount := 0;
    end;
  finally
    LeaveCriticalSection(NodeListLock);
    LastConsensusTimestamp := 0;
  end;
end;

function GetNodeDataByIndex(Index: Integer): TNodeConsensus;
begin
  Result := Default(TNodeConsensus);
  EnterCriticalSection(NodeListLock);
  try
    if Index < Length(NodeList) then
      Result := NodeList[Index];
  finally
    LeaveCriticalSection(NodeListLock);
  end;
end;

function GetNodeCount(): Integer;
begin
  EnterCriticalSection(NodeListLock);
  try
    Result := Length(NodeList);
  finally
    LeaveCriticalSection(NodeListLock);
  end;
end;

function ComputeConsensus(NodeList: String = ''): TConsensus;
var
  I, ParamIndex: Integer;
  NodeStatusThread: TNodeStatusThread;
  EndTimestamp: Int64;
  ConsensusData: array of TConsensusData;
  NodeHash, HighestHash: String;
  ValidNodeCount: Integer = 0;
  Finished: Boolean = False;

  procedure AddConsensusValue(Value: String);
  var
    J: Integer;
  begin
    for J := 0 to High(ConsensusData) do
    begin
      if ConsensusData[J].Value = Value then
      begin
        Inc(ConsensusData[J].Count);
        Exit;
      end;
    end;
    SetLength(ConsensusData, Length(ConsensusData) + 1);
    ConsensusData[High(ConsensusData)].Value := Value;
    ConsensusData[High(ConsensusData)].Count := 1;
  end;

  function GetHighestValue(): String;
  var
    MaxCount, J: Integer;
  begin
    Result := '';
    MaxCount := 0;
    for J := 0 to High(ConsensusData) do
    begin
      if ConsensusData[J].Count > MaxCount then
      begin
        MaxCount := ConsensusData[J].Count;
        Result := ConsensusData[J].Value;
      end;
    end;
  end;

begin
  ConsensusCompleted := False;
  if NodeList <> '' then
    SetNodeList(NodeList);

  if GetNodeCount() = 0 then Exit;

  EnterCriticalSection(ConsensusLock);
  try
    IsConsensusRunning := True;
    SetLength(ConsensusResults, 21);
    for I := 0 to High(ConsensusResults) do
      ConsensusResults[I] := '';

    ReachedNodes := 0;
    ActiveThreadCount := GetNodeCount();
    for I := 0 to High(NodeList) do
    begin
      NodeStatusThread := TNodeStatusThread.Create(True, I);
      NodeStatusThread.Start;
    end;

    EndTimestamp := UTCTime + 20;
    repeat
      Sleep(10);
      if (GetActiveThreadCount() = 0) or (UTCTime > EndTimestamp) then
        Finished := True;
    until Finished;

    ValidNodes := 0;
    for ParamIndex := 0 to High(ConsensusLabels) do
    begin
      SetLength(ConsensusData, 0);
      for I := 0 to High(NodeList) do
      begin
        if NodeList[I] <> '' then
        begin
          NodeHash := GetParameter(NodeList[I], ParamIndex);
          AddConsensusValue(NodeHash);
          Inc(ValidNodes);
        end;
      end;

      HighestHash := GetHighestValue();
      ConsensusResults[ParamIndex] := HighestHash;
    end;

    if ValidNodes > 0 then
      ConsensusPercentage := (ReachedNodes * 100) div GetNodeCount()
    else
      ConsensusPercentage := 0;

    ConsensusCompleted := True;
    LastConsensusTimestamp := UTCTime;
  finally
    IsConsensusRunning := False;
    LeaveCriticalSection(ConsensusLock);
  end;

  Result := ConsensusResults;
end;

function GetConsensusData(Index: Integer = 0): String;
begin
  if (Index >= 0) and (Index <= High(ConsensusResults)) then
    Result := ConsensusResults[Index]
  else
    Result := '';
end;

initialization
  Randomize;
  SetLength(NodeList, 0);
  InitCriticalSection(NodeListLock);
  InitCriticalSection(ThreadLock);
  InitCriticalSection(ConsensusLock);


finalization
  DoneCriticalSection(NodeListLock);
  DoneCriticalSection(ThreadLock);
  DoneCriticalSection(ConsensusLock);

end. {END UNIT}
