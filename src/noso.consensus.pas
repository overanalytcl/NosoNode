unit Noso.Consensus;

{
nosoconsensus 1.0
January 20th, 2023
Noso Unit to get a consensus
Requires: nosodebug, nosotime, nosogeneral
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  Noso.Debug, Noso.Time, Noso.General, Noso.Crypto;

type

  TThreadNodeStatus = class(TThread)
  private
    Slot: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const CreatePaused: Boolean; TSlot: Integer);
  end;

  TThreadAutoConsensus = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  TConsensus = array of String[32];

  TNodeConsensus = record
    host: String;
    port: Integer;
    Data: String;
    ConStr: String[32];
    Block: Integer;
    Peers: Integer;
  end;

  TConsensusData = record
    Value: String;
    Count: Integer;
  end;

function OpenThreadsValue(): Integer;

function CalculateConsensus(NodesList: String = ''): TConsensus;
function GetConsensus(LData: Integer = 0): String;
function GetRandonNode(): String;
function GetConHash(ILine: String): String;
procedure SetNodesArray(NodesList: String);
function GetNodesArrayIndex(LIndex: Integer): TNodeConsensus;
function GetNodesArrayCount(): Integer;

procedure StartAutoConsensus();
procedure StopAutoConsensus();

const
  cLastBlock = 2;
  cHeaders = 5;
  cMNsHash = 8;
  cMNsCount = 9;
  cLBHash = 10;
  cLBTimeEnd = 12;
  cSumHash = 17;
  cGVTsHash = 18;
  cCFGHash = 19;

var
  Consensus: TConsensus;
  NConsensus: array[0..20] of
  String = ({0}'Resume', 'Peers', 'LBlock', 'Pending', 'Delta',
    {5}'Headers', 'Version', 'UTCTime', 'MNsHash', 'MNsCount',
    {10}'LBHash', 'BestDiff', 'LBTimeEnd', 'LBMiner', 'ChecksCount',
    {15}'LBPoW', 'LBDiff', 'Summary', 'GVTs', 'NosoCFG',
    {20}'PSOHash');
  Css_TotalNodes: Integer = 0;
  Css_ReachedNodes: Integer = 0;
  Css_ValidNodes: Integer = 0;
  Css_Percentage: Integer = 0;
  Css_Completed: Boolean = False;
  LastConsensusTime: Int64 = 0;

implementation

var
  NodesArray: array of TNodeConsensus;
  CSNodesArray: TRTLCriticalSection;
  OpenThreads: Integer;
  ReachedNodes: Integer;
  CSOpenThreads: TRTLCriticalSection;
  CSConsensus: TRTLCriticalSection;
  KeepAutoCon: Boolean = False;
  RunningConsensus: Boolean = False;
  ActiveRound: Int64 = 0;

  {$REGION Thread auto update}

procedure StartAutoConsensus();
var
  AutoThread: TThreadAutoConsensus;
begin
  if KeepAutoCon then exit;
  Keepautocon := True;
  AutoThread := TThreadAutoConsensus.Create(True);
  AutoThread.FreeOnTerminate := True;
  AutoThread.Start;
end;

procedure StopAutoConsensus();
begin
  Keepautocon := False;
end;

constructor TThreadAutoConsensus.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TThreadAutoConsensus.Execute;
begin
  repeat
    if ((BlockAge >= 0) and (BlockAge < 5)) then LastConsensusTime := 0;
    if (((BlockAge >= 5) and (BlockAge < 585)) and
      (LastConsensusTime + 60 < UTCTime)) then
    begin
      LastConsensusTime := UTCTime;
      CalculateConsensus();
    end;
    Sleep(100);
  until ((terminated) or (not KeepAutoCon));
end;

{$ENDREGION}

{$REGION Open threads}

procedure DecOpenThreads(Reached: Boolean);
begin
  EnterCriticalSection(CSOpenThreads);
  Dec(OpenThreads);
  if reached then Inc(ReachedNodes);
  LeaveCriticalSection(CSOpenThreads);
end;

function OpenThreadsValue(): Integer;
begin
  EnterCriticalSection(CSOpenThreads);
  Result := OpenThreads;
  LeaveCriticalSection(CSOpenThreads);
end;

function GetNodeIndex(index: Integer): TNodeConsensus;
begin
  EnterCriticalSection(CSNodesArray);
  Result := NodesArray[index];
  LeaveCriticalSection(CSNodesArray);
end;

{$ENDREGION}

{$REGION Thread consulting node}

constructor TThreadNodeStatus.Create(const CreatePaused: Boolean; TSlot: Integer);
begin
  inherited Create(CreatePaused);
  Slot := TSlot;
  FreeOnTerminate := True;
end;

procedure TThreadNodeStatus.Execute;
var
  ThisNode: TNodeConsensus;
  ReadedLine: String;
  Reached: Boolean = False;
  ConHash: String = '';
  MyRound: Int64;
begin
  MyRound := ActiveRound;
  ThisNode := GetNodeIndex(slot);
  ReadedLine := RequestLineToPeer(ThisNode.host, ThisNode.port, 'NODESTATUS');
  if copy(ReadedLine, 1, 10) = 'NODESTATUS' then
  begin
    ConHash := GetConHash(ReadedLine);
    ReadedLine := StringReplace(ReadedLine, 'NODESTATUS', ConHash,
      [rfReplaceAll, rfIgnoreCase]);
    ThisNode.Data := ReadedLine;
    ThisNode.ConStr := ConHash;
    ThisNode.Peers := StrToIntDef(Parameter(ReadedLine, 1), 0);
    ThisNode.Block := StrToIntDef(Parameter(ReadedLine, 2), 0);
    reached := True;
  end
  else
  begin
    ThisNode.Data := '';
    ThisNode.ConStr := '';
    ThisNode.Peers := 0;
    ThisNode.Block := 0;
  end;
  if MyRound = ActiveRound then
  begin
    EnterCriticalSection(CSNodesArray);
    NodesArray[slot] := ThisNode;
    LeaveCriticalSection(CSNodesArray);
    DecOpenThreads(Reached);
  end;
end;

{$ENDREGION}

function GetConHash(ILine: String): String;
begin
  Result := '';
  Result := HashMD5String(Parameter(ILine, 2) + copy(Parameter(ILine, 5), 0, 5) +
    copy(Parameter(ILine, 8), 0, 5) + copy(Parameter(ILine, 10), 0, 5) +
    copy(Parameter(ILine, 17), 0, 5) + copy(Parameter(ILine, 18), 0, 5) +
    copy(Parameter(ILine, 19), 0, 5));
end;

{Gets a random ip and port node}
function GetRandonNode(): String;
var
  LNumber: Integer;
begin
  Result := '';
  EnterCriticalSection(CSNodesArray);
  LNumber := random(length(NodesArray));
  Result := Format('%s %d', [NodesArray[LNumber].host, NodesArray[LNumber].port]);
  LeaveCriticalSection(CSNodesArray);
end;

{Set the values for the array of nodes}
procedure SetNodesArray(NodesList: String);
var
  counter: Integer;
  MyArray: array of String;
begin
  repeat
    sleep(5);
  until not RunningConsensus;
  setlength(NodesArray, 0);
  NodesList := Trim(StringReplace(NodesList, ':', ' ', [rfReplaceAll, rfIgnoreCase]));
  MyArray := SplitString(NodesList, ' ');
  EnterCriticalSection(CSNodesArray);
  for counter := 0 to high(MyArray) do
  begin
    MyArray[counter] := StringReplace(MyArray[counter], ';', ' ',
      [rfReplaceAll, rfIgnoreCase]);
    Setlength(NodesArray, length(NodesArray) + 1);
    NodesArray[length(NodesArray) - 1].host := Parameter(MyArray[counter], 0);
    NodesArray[length(NodesArray) - 1].port :=
      StrToIntDef(Parameter(MyArray[counter], 1), 8080);
    NodesArray[length(NodesArray) - 1].Data := '';
    NodesArray[length(NodesArray) - 1].ConStr := '';
    NodesArray[length(NodesArray) - 1].Block := 0;
    NodesArray[length(NodesArray) - 1].peers := 0;
  end;
  LeaveCriticalSection(CSNodesArray);
  LastConsensusTime := 0;
end;

function GetNodesArrayIndex(LIndex: Integer): TNodeConsensus;
begin
  Result := default(TNodeConsensus);
  EnterCriticalSection(CSNodesArray);
  if LIndex < length(NodesArray) then
    Result := NodesArray[LIndex];
  LeaveCriticalSection(CSNodesArray);
end;

function GetNodesArrayCount(): Integer;
begin
  EnterCriticalSection(CSNodesArray);
  Result := length(NodesArray);
  LeaveCriticalSection(CSNodesArray);
end;

function CalculateConsensus(NodesList: String = ''): TConsensus;
var
  counter: Integer;
  count2: Integer;
  ParamNumber: Integer = 1;
  ThisThread: TThreadNodeStatus;
  isFinished: Boolean = False;
  ArrayCon: array of TConsensusData;
  ThisHigh: String;
  ConHash: String;
  ValidNodes: Integer = 0;
  EndTime: Int64;

  procedure AddValue(Tvalue: String);
  var
    counter: Integer;
    ThisItem: TConsensusData;
  begin
    for counter := 0 to length(ArrayCon) - 1 do
    begin
      if Tvalue = ArrayCon[counter].Value then
      begin
        ArrayCon[counter].Count += 1;
        Exit;
      end;
    end;
    ThisItem.Value := Tvalue;
    ThisItem.Count := 1;
    Insert(ThisITem, ArrayCon, length(ArrayCon));
  end;

  function GetHighest(): String;
  var
    maximum: Integer = 0;
    counter: Integer;
    MaxIndex: Integer = 0;
  begin
    Result := '';
    if length(ArrayCon) > 0 then
    begin
      for counter := 0 to high(ArrayCon) do
      begin
        if ArrayCon[counter].Count > maximum then
        begin
          maximum := ArrayCon[counter].Count;
          MaxIndex := counter;
        end;
      end;
      Result := ArrayCon[MaxIndex].Value;
    end;
  end;

begin
  BeginPerformance('CalculateConsensus');
  RunningConsensus := True;
  SetLength(Result, 0);
  if NodesList <> '' then SetNodesArray(NodesList);
  OpenThreads := length(NodesArray);
  ReachedNodes := 0;
  ActiveRound := UTCTime;
  for counter := 0 to high(NodesArray) do
  begin
    ThisThread := TThreadNodeStatus.Create(True, counter);
    ThisThread.FreeOnTerminate := True;
    ThisThread.Start;
    Sleep(5);
  end;
  EndTime := UTCTime + 5;
  repeat
    sleep(5);
  until ((OpenThreadsValue <= 0) or (UTCTime >= EndTime));
  ActiveRound := 0;
  // Get the consensus hash
  SetLength(ArrayCon, 0);
  for counter := 0 to high(NodesArray) do
  begin
    if Parameter(NodesArray[counter].Data, 0) <> '' then
      AddValue(Parameter(NodesArray[counter].Data, 0));
  end;
  ConHash := GetHighest;
  if conhash = '' then
  begin
    for count2 := 0 to length(NConsensus) - 1 do
    begin
      insert('', Result, 0);
    end;
    Css_TotalNodes := length(NodesArray);
    Css_ReachedNodes := Reachednodes;
    Css_ValidNodes := ValidNodes;
    if ReachedNodes > 0 then Css_Percentage := (ValidNodes * 100) div ReachedNodes
    else
      Css_Percentage := 0;
    EnterCriticalSection(CSConsensus);
    setlength(consensus, 0);
    Consensus := copy(Result, 0, length(Result));
    LeaveCriticalSection(CSConsensus);
    Css_Completed := False;
    RunningConsensus := False;
    Dec(LastConsensusTime, 50);
    EndPerformance('CalculateConsensus');
    exit;
  end;
  insert(ConHash, Result, 0);
  // Fill the consensus
  repeat
    SetLength(ArrayCon, 0);
    for counter := 0 to high(NodesArray) do
    begin
      if Parameter(NodesArray[counter].Data, 0) = ConHash then
      begin
        AddValue(Parameter(NodesArray[counter].Data, paramnumber));
        if ParamNumber = 1 then Inc(ValidNodes);
      end;
    end;
    ThisHigh := GetHighest;
    if thishigh = '' then isFinished := True
    else
      insert(ThisHigh, Result, length(Result));
    Inc(ParamNumber);
  until isFinished;
  Css_TotalNodes := length(NodesArray);
  Css_ReachedNodes := Reachednodes;
  Css_ValidNodes := ValidNodes;
  if ReachedNodes > 0 then Css_Percentage := (ValidNodes * 100) div ReachedNodes
  else
    Css_Percentage := 0;
  //if StrToIntDef(result[cLastBlock],0) >= StrToIntDef(Consensus[cLastBlock],0) then
  //begin
  EnterCriticalSection(CSConsensus);
  setlength(consensus, 0);
  Consensus := copy(Result, 0, length(Result));
  LeaveCriticalSection(CSConsensus);
  //end;

  Css_Completed := True;
  RunningConsensus := False;
  EndPerformance('CalculateConsensus');
end;

function GetConsensus(LData: Integer = 0): String;
begin
  Result := '';
  EnterCriticalSection(CSConsensus);
  try
    Result := Consensus[LData];
  except
    on E: Exception do
    begin

    end;
  end;
  LeaveCriticalSection(CSConsensus);
end;

initialization
  Randomize;
  setlength(NodesArray, 0);
  InitCriticalSection(CSNodesArray);
  InitCriticalSection(CSOpenThreads);
  InitCriticalSection(CSConsensus);


finalization
  DoneCriticalSection(CSNodesArray);
  DoneCriticalSection(CSOpenThreads);
  DoneCriticalSection(CSConsensus);

end. {END UNIT}
