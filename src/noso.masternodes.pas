unit Noso.Masternodes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IdTCPClient, IdGlobal, strutils,
  Noso.Debug, Noso.Time, Noso.General, Noso.Crypto, Noso.Summary;

type

  TThreadMNVerificator = class(TThread)
  private
    FSlot: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(const CreatePaused: Boolean; const ConexSlot: Integer);
  end;

  TMNode = packed record
    Ip: String[15];
    Port: Integer;
    Sign: String[40];
    Fund: String[40];
    First: Integer;
    Last: Integer;
    Total: Integer;
    Validations: Integer;
    Hash: String[32];
  end;

  TMNCheck = record
    ValidatorIP: String;
    Block: Integer;
    SignAddress: String;
    PublicKey: String;
    ValidNodes: String;
    Signature: String;
  end;

  TMNsData = packed record
    ipandport: String;
    address: String;
    age: Integer;
  end;

procedure SetMasternodesFilename(LText: String);

procedure SetLocalIP(NewValue: String);
procedure SetMN_Sign(SignAddress, lPublicKey, lPrivateKey: String);
function GetMNReportString(block: Integer): String;
function VerifyThreadsCount: Integer;
function RunMNVerification(Block: Integer; LocSynctus: String;
  LocalIP: String; publicK, privateK: String): String;

function GetMNsListLength(): Integer;
procedure ClearMNsList();
function IsIPMNAlreadyProcessed(OrderText: String): Boolean;
procedure ClearMNIPProcessed();
function IsMyMNListed(LocalIP: String): Boolean;
function IsLegitNewNode(ThisNode: TMNode; block: Integer): Boolean;
function CheckMNReport(LineText: String; block: Integer): String;
function GetMNodeFromString(const StringData: String; out ToMNode: TMNode): Boolean;
function GetStringFromMN(Node: TMNode): String;
function PopulateMasternodeList(out LDataArray: TStringArray): Boolean;
function GetMNsAddresses(Block: Integer): String;
procedure CreditMNVerifications();

function GetMasternodeCheckCount(): Integer;
function GetValidNodesCountOnCheck(StringNodes: String): Integer;
function GetMNCheckFromString(Linea: String): TMNCheck;
procedure ClearMNsChecks();
function MnsCheckExists(Ip: String): Boolean;
procedure AddMNCheck(ThisData: TMNCheck);
function FormatMasternodeCheck(Data: TMNCheck): String;
function IsMyMNCheckDone(): Boolean;

procedure SetMNsHash();
function GetMNsHash(): String;

function LengthReceivedMNs(): Integer;
procedure ClearReceivedMNs();
function IsMNIPReceived(DataSource: String): Boolean;

function LengthWaitingMNs(): Integer;
procedure AddWaitingMNs(Linea: String);
function GetWaitingMNs(): String;

function GetMNAgeCount(TNode: TMNode): String;
function LoadMNsFile(): String;
procedure SaveMNsFile(GotText: String);
procedure SetMN_FileText(lvalue: String);
function GetMN_FileText(): String;
procedure FillMNsArray(TValue: String);
function GetVerificatorsText(): String;

var
  MasterNodesFilename: String = '';
  MNFileHandler: textfile;
  MNsFileLock: TRTLCriticalSection;

  MNsListCopy: array of TMnode;
  CurrSynctus: String;
  LocalMasternodeIP: String = '';
  LocalMasternodePort: String = '8080';
  LocalMasternodeSignature: String = '';
  LocalMasternodeFunds: String = '';
  LocalMasternodePublicKey: String = '';
  LocalMasternodePrivateKey: String = '';
  UnconfirmedIPs: Integer;

  MyMNsHash: String = '';
  MNsHashLock: TRTLCriticalSection;

  VerifiedNodes: String;
  VerifiedNodesLock: TRTLCriticalSection;

  OpenVerificators: Integer;
  VerifyThreadLock: TRTLCriticalSection;

  MNsList: array of TMnode;
  MNsListLock: TRTLCriticalSection;

  ArrayIPsProcessed: array of String;
  MNsIPProcLock: TRTLCriticalSection;

  MasternodeChecks: array of TMNCheck;
  MNsChecksLock: TRTLCriticalSection;

  ArrayMNsData: array of TMNsData;

  MN_FileText: String = '';
  MN_FileTextLock: TRTLCriticalSection;

  ArrWaitMNs: array of String;
  WaitingMNsLock: TRTLCriticalSection;

  ArrReceivedMNs: array of String;
  ReceivedMNsLock: TRTLCriticalSection;

implementation

procedure SetMasternodesFilename(LText: String);
begin
  MasterNodesFilename := LText;
  AssignFile(MNFileHandler, MasterNodesFilename);
  if not FileExists(MasterNodesFilename) then CreateEmptyFile(MasterNodesFilename);
  LoadMNsFile;
end;

procedure SetLocalIP(NewValue: String);
begin
  LocalMasternodeIP := NewValue;
end;

procedure SetMN_Sign(SignAddress, lPublicKey, lPrivateKey: String);
begin
  LocalMasternodeSignature := SignAddress;
  LocalMasternodePublicKey := lPublicKey;
  LocalMasternodePrivateKey := lPrivateKey;
end;

// Returns the string to send the own MN report
function GetMNReportString(block: Integer): String;
begin
  // {5}IP 6{Port} 7{SignAddress} 8{FundsAddress} 9{FirstBlock} 10{LastVerified}
  //    11{TotalVerified} 12{BlockVerifys} 13{hash}
  Result := LocalMasternodeIP + ' ' + LocalMasternodePort + ' ' + LocalMasternodeSignature +
    ' ' + LocalMasternodeFunds + ' ' + block.ToString + ' ' + block.ToString +
    ' ' + '0' + ' ' + '0' + ' ' + HashMD5String(LocalMasternodeIP + LocalMasternodePort +
    LocalMasternodeSignature + LocalMasternodeFunds);
end;

{$REGION ThreadVerificator}

constructor TThreadMNVerificator.Create(const CreatePaused: Boolean;
  const ConexSlot: Integer);
begin
  inherited Create(CreatePaused);
  FSlot := ConexSlot;
end;

procedure TThreadMNVerificator.Execute;
var
  TCPClient: TidTCPClient;
  Linea: String = '';
  WasPositive: Boolean;
  IP: String;
  Port: Integer;
  Success: Boolean;
  Trys: Integer = 0;
begin
  AddNewOpenThread('VerifyMN ' + FSlot.ToString, UTCTime);
  Sleep(1000);
  try {BIG}
    IP := MNsListCopy[FSlot].Ip;
    Port := MNsListCopy[FSlot].Port;
    TCPClient := TidTCPClient.Create(nil);
    TCPclient.Host := Ip;
    TCPclient.Port := Port;
    TCPclient.ConnectTimeout := 1000;
    TCPclient.ReadTimeout := 1000;
    repeat
      Inc(Trys);
    try
      TCPclient.Connect;
      TCPclient.IOHandler.WriteLn('MNVER');
      Linea := TCPclient.IOHandler.ReadLn(IndyTextEncoding_UTF8);
      TCPclient.Disconnect();
      Success := True;
    except
      on E: Exception do
      begin
        Success := False;
      end;
    end{try};
    until ((Success) or (trys = 3));
    TCPClient.Free;
    if success then
    begin
      WasPositive := StrToBoolDef(GetParameter(Linea, 0), False);
      if ((WasPositive) and (GetParameter(Linea, 1) = CurrSynctus)) then
      begin
        EnterCriticalSection(VerifiedNodesLock);
        VerifiedNodes := VerifiedNodes + Ip + ';' + Port.ToString + ':';
        LeaveCriticalSection(VerifiedNodesLock);
      end
      else if ((WasPositive) and (GetParameter(Linea, 1) <> CurrSynctus)) then
      begin
        // Wrong synctus returned
      end
      else
      begin
        // Was not possitive
      end;
    end;
    if GetParameter(Linea, 3) <> LocalMasternodeIP then Inc(UnconfirmedIPs);
  except
    on E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'CRITICAL MNs VERIFICATION (' + Ip + '): ' + E.Message);
    end;
  end{BIG TRY};
  EnterCriticalSection(VerifyThreadLock);
  Dec(OpenVerificators);
  LeaveCriticalSection(VerifyThreadLock);
  CloseOpenThread('VerifyMN ' + FSlot.ToString);
end;

function VerifyThreadsCount: Integer;
begin
  EnterCriticalSection(VerifyThreadLock);
  Result := OpenVerificators;
  LeaveCriticalSection(VerifyThreadLock);
end;

{$ENDREGION ThreadVerificator}

function RunMNVerification(Block: Integer; LocSynctus: String;
  LocalIP: String; publicK, privateK: String): String;
var
  counter: Integer;
  ThisThread: TThreadMNVerificator;
  Launched: Integer = 0;
  WaitCycles: Integer = 0;
  DataLine: String;
begin
  StartPerformanceMeasurement('RunMNVerification');
  Result := '';
  CurrSynctus := LocSynctus;
  SetLocalIP(LocalIP);
  VerifiedNodes := '';
  setlength(MNsListCopy, 0);
  EnterCriticalSection(MNsListLock);
  MNsListCopy := copy(MNsList, 0, length(MNsList));
  LeaveCriticalSection(MNsListLock);
  UnconfirmedIPs := 0;
  for counter := 0 to length(MNsListCopy) - 1 do
  begin
    if ((MNsListCopy[counter].ip <> LocalIP) and
      (IsValidIp(MNsListCopy[counter].ip))) then
    begin
      Inc(Launched);
      ThisThread := TThreadMNVerificator.Create(True, counter);
      ThisThread.FreeOnTerminate := True;
      ThisThread.Start;
    end;
  end;
  EnterCriticalSection(VerifyThreadLock);
  OpenVerificators := Launched;
  LeaveCriticalSection(VerifyThreadLock);
  repeat
    sleep(100);
    Inc(WaitCycles);
  until ((VerifyThreadsCount = 0) or (WaitCycles = 250));
  //ToDeepDebug(Format('MNs verification finish: %d launched, %d Open, %d cycles',[Launched,VerifyThreadsCount,WaitCycles ]));
  //ToDeepDebug(Format('Unconfirmed IPs: %d',[UnconfirmedIPs ]));
  if VerifyThreadsCount > 0 then
  begin
    EnterCriticalSection(VerifyThreadLock);
    OpenVerificators := 0;
    LeaveCriticalSection(VerifyThreadLock);
  end;
  Result := LocalIP + ' ' + Block.ToString + ' ' + LocalMasternodeSignature +
    ' ' + publicK + ' ' + VerifiedNodes + ' ' + GetStringSigned(VerifiedNodes, privateK);
  StopPerformanceMeasurement('RunMNVerification');
end;

{$REGION MNsList handling}

// Returns the count of reported MNs
function GetMNsListLength(): Integer;
begin
  EnterCriticalSection(MNsListLock);
  Result := Length(MNsList);
  LeaveCriticalSection(MNsListLock);
end;

procedure ClearMNsList();
begin
  EnterCriticalSection(MNsListLock);
  SetLength(MNsList, 0);
  LeaveCriticalSection(MNsListLock);
  EnterCriticalSection(MNsIPProcLock);
  Setlength(ArrayIPsProcessed, 0);
  LeaveCriticalSection(MNsIPProcLock);
end;

// Verify if an IP was already processed
function IsIPMNAlreadyProcessed(OrderText: String): Boolean;
var
  ThisIP: String;
  counter: Integer;
begin
  Result := False;
  ThisIP := GetParameter(OrderText, 5);
  EnterCriticalSection(MNsIPProcLock);
  if length(ArrayIPsProcessed) > 0 then
  begin
    for counter := 0 to length(ArrayIPsProcessed) - 1 do
    begin
      if ArrayIPsProcessed[counter] = ThisIP then
      begin
        Result := True;
        break;
      end;
    end;
  end;
  if Result = False then Insert(ThisIP, ArrayIPsProcessed, length(ArrayIPsProcessed));
  LeaveCriticalSection(MNsIPProcLock);
end;

procedure ClearMNIPProcessed();
begin
  EnterCriticalSection(MNsIPProcLock);
  Setlength(ArrayIPsProcessed, 0);
  LeaveCriticalSection(MNsIPProcLock);
end;

function IsMyMNListed(LocalIP: String): Boolean;
var
  counter: Integer;
begin
  Result := False;
  if GetMNsListLength > 0 then
  begin
    EnterCriticalSection(MNsListLock);
    for counter := 0 to length(MNsList) - 1 do
    begin
      if MNsList[counter].Ip = LocalIP then
      begin
        Result := True;
        break;
      end;
    end;
    LeaveCriticalSection(MNsListLock);
  end;
end;

function IsLegitNewNode(ThisNode: TMNode; block: Integer): Boolean;
var
  counter: Integer;
begin
  Result := True;
  if GetMNsListLength > 0 then
  begin
    EnterCriticalSection(MNsListLock);
    for counter := 0 to length(MNsList) - 1 do
    begin
      if ((ThisNode.Ip = MNsList[counter].Ip) or
        (ThisNode.Sign = MNsList[counter].Sign) or
        (ThisNode.Fund = MNsList[counter].Fund) or
        //(ThisNode.First>MyLastBlock) or
        //(ThisNode.Last>MyLastBlock) or
        //(ThisNode.Total<>0) or
        (GetAddressBalanceIndexed(ThisNode.Fund) < GetStackRequired(block + 1)) or
        (ThisNode.Validations <> 0)) then
      begin
        Result := False;
        break;
      end;
    end;
    LeaveCriticalSection(MNsListLock);
  end;
end;

function CheckMNReport(LineText: String; block: Integer): String;
var
  StartPos: Integer;
  ReportInfo: String = '';
  NewNode: TMNode;
  counter: Integer;
  Added: Boolean = False;
begin
  Result := '';
  StartPos := Pos('$', LineText);
  ReportInfo := copy(LineText, StartPos, length(LineText));
  if GetMNodeFromString(ReportInfo, NewNode) then
  begin
    if IsLegitNewNode(NewNode, block) then
    begin
      EnterCriticalSection(MNsListLock);
      if Length(MNsList) = 0 then
        Insert(NewNode, MNsList, 0)
      else
      begin
        for counter := 0 to length(MNsList) - 1 do
        begin
          if NewNode.Ip < MNsList[counter].ip then
          begin
            Insert(NewNode, MNsList, counter);
            Added := True;
            break;
          end;
        end;
        if not Added then Insert(NewNode, MNsList, Length(MNsList));
      end;
      LeaveCriticalSection(MNsListLock);
      Result := reportinfo;
    end
    else
    begin
      //No legit masternode
    end;
  end
  else
  begin
    //Invalid masternode
  end;
end;

// Converts a String into a MNNode data
function GetMNodeFromString(const StringData: String; out ToMNode: TMNode): Boolean;
var
  ErrCode: Integer = 0;
begin
  Result := True;
  ToMNode := Default(TMNode);
  ToMNode.Ip := GetParameter(StringData, 1);
  ToMNode.Port := StrToIntDef(GetParameter(StringData, 2), -1);
  ToMNode.Sign := GetParameter(StringData, 3);
  ToMNode.Fund := GetParameter(StringData, 4);
  ToMNode.First := StrToIntDef(GetParameter(StringData, 5), -1);
  ToMNode.Last := StrToIntDef(GetParameter(StringData, 6), -1);
  ToMNode.Total := StrToIntDef(GetParameter(StringData, 7), -1);
  ToMNode.Validations := StrToIntDef(GetParameter(StringData, 8), -1);
  ToMNode.hash := GetParameter(StringData, 9);
  if not IsValidIP(ToMNode.Ip) then Result := False
  else if ((ToMNode.Port < 0) or (ToMNode.Port > 65535)) then ErrCode := 1
  else if not IsValidHashAddress(ToMNode.Sign) then ErrCode := 2
  else if not IsValidHashAddress(ToMNode.Fund) then ErrCode := 3
  else if ToMNode.First < 0 then ErrCode := 4
  else if ToMNode.last < 0 then ErrCode := 5
  else if ToMNode.total < 0 then ErrCode := 6
  else if ToMNode.validations < 0 then ErrCode := 7
  else if ToMNode.hash <> HashMD5String(ToMNode.Ip + IntToStr(ToMNode.Port) +
    ToMNode.Sign + ToMNode.Fund) then ErrCode := 8;
  if ErrCode > 0 then
  begin
    Result := False;
    //Invalid Masternode
  end;
end;

// Converst a MNNode data into a string
function GetStringFromMN(Node: TMNode): String;
begin
  Result := Node.Ip + ' ' + Node.Port.ToString + ' ' + Node.Sign +
    ' ' + Node.Fund + ' ' + Node.First.ToString + ' ' + Node.Last.ToString +
    ' ' + Node.Total.ToString + ' ' + Node.Validations.ToString + ' ' + Node.Hash;
end;

// Fills the given array with the nodes reports to be sent to another peer
function PopulateMasternodeList(out LDataArray: TStringArray): Boolean;
var
  ThisLine: String;
  counter: Integer;
begin
  Result := False;
  SetLength(LDataArray, 0);
  if GetMNsListLength > 0 then
  begin
    EnterCriticalSection(MNsListLock);
    for counter := 0 to length(MNsList) - 1 do
    begin
      ThisLine := GetStringFromMN(MNsList[counter]);
      Insert(ThisLine, LDataArray, length(LDataArray));
    end;
    Result := True;
    LeaveCriticalSection(MNsListLock);
  end;
end;

// Returns the string to be stored on the masternodes.txt file
function GetMNsAddresses(Block: Integer): String;
var
  MinValidations: Integer;
  Counter: Integer;
  Resultado: String = '';
  AddAge: String = '';
begin
  MinValidations := (GetMasternodeCheckCount div 2) - 1;
  Resultado := Block.ToString + ' ';
  EnterCriticalSection(MNsListLock);
  for counter := 0 to length(MNsList) - 1 do
  begin
    if MNsList[counter].Validations >= MinValidations then
    begin
      AddAge := GetMNAgeCount(MNsList[counter]);
      Resultado := Resultado + MNsList[counter].Ip + ';' +
        MNsList[counter].Port.ToString + ':' + MNsList[counter].Fund + AddAge + ' ';
    end;
  end;
  LeaveCriticalSection(MNsListLock);
  SetLength(Resultado, Length(Resultado) - 1);
  Result := Resultado;
end;

procedure CreditMNVerifications();
var
  counter: Integer;
  NodesString: String;
  ThisIP: String;
  IPIndex: Integer = 0;
  CheckNodes: Integer;

  procedure AddCheckToIP(IP: String);
  var
    counter2: Integer;
  begin
    for counter2 := 0 to length(MNsList) - 1 do
    begin
      if MNsList[Counter2].Ip = IP then
      begin
        MNsList[Counter2].Validations := MNsList[Counter2].Validations + 1;
        Break;
      end;
    end;
  end;

begin
  EnterCriticalSection(MNsListLock);
  EnterCriticalSection(MNsChecksLock);
  for counter := 0 to length(MasternodeChecks) - 1 do
  begin
    NodesString := MasternodeChecks[counter].ValidNodes;
    NodesString := StringReplace(NodesString, ':', ' ', [rfReplaceAll]);
    CheckNodes := 0;
    IPIndex := 0;
    repeat
      begin
        ThisIP := GetParameter(NodesString, IPIndex);
        ThisIP := StringReplace(ThisIP, ';', ' ', [rfReplaceAll]);
        ThisIP := GetParameter(ThisIP, 0);
        if ThisIP <> '' then
        begin
          AddCheckToIP(ThisIP);
          Inc(CheckNodes);
        end;
        Inc(IPIndex);
      end;
    until ThisIP = '';
    //ToLog('Console',MasternodeChecks[counter].ValidatorIP+': '+Checknodes.ToString);
  end;
  LeaveCriticalSection(MNsChecksLock);
  LeaveCriticalSection(MNsListLock);
end;

{$ENDREGION MNsList handling}

{$REGION MNs check handling}

// Returns the number of MNs checks
function GetMasternodeCheckCount(): Integer;
begin
  EnterCriticalSection(MNsChecksLock);
  Result := Length(MasternodeChecks);
  LeaveCriticalSection(MNsChecksLock);
end;

function GetValidNodesCountOnCheck(StringNodes: String): Integer;
var
  ThisIP: String;
  IPIndex: Integer = 0;
begin
  Result := 0;
  StringNodes := StringReplace(StringNodes, ':', ' ', [rfReplaceAll]);
  IPIndex := 0;
  repeat
    begin
      ThisIP := GetParameter(StringNodes, IPIndex);
      if ThisIP <> '' then Inc(Result);
      Inc(IPIndex);
    end;
  until ThisIP = '';
end;

// Converts a string into a TMNChekc data
function GetMNCheckFromString(Linea: String): TMNCheck;
begin
  Result := Default(TMNCheck);
  Result.ValidatorIP := GetParameter(Linea, 5);
  Result.Block := StrToIntDef(GetParameter(Linea, 6), 0);
  Result.SignAddress := GetParameter(Linea, 7);
  Result.PublicKey := GetParameter(Linea, 8);
  Result.ValidNodes := GetParameter(Linea, 9);
  Result.Signature := GetParameter(Linea, 10);
end;

// Clears all the MNS checks
procedure ClearMNsChecks();
begin
  EnterCriticalSection(MNsChecksLock);
  SetLength(MasternodeChecks, 0);
  LeaveCriticalSection(MNsChecksLock);
end;

// Verify if an IP already sent a verification
function MnsCheckExists(Ip: String): Boolean;
var
  Counter: Integer;
begin
  Result := False;
  EnterCriticalSection(MNsChecksLock);
  for counter := 0 to length(MasternodeChecks) - 1 do
  begin
    if MasternodeChecks[counter].ValidatorIP = IP then
    begin
      Result := True;
      break;
    end;
  end;
  LeaveCriticalSection(MNsChecksLock);
end;

// Adds a new MNCheck
procedure AddMNCheck(ThisData: TMNCheck);
begin
  EnterCriticalSection(MNsChecksLock);
  Insert(ThisData, MasternodeChecks, Length(MasternodeChecks));
  LeaveCriticalSection(MNsChecksLock);
end;

function FormatMasternodeCheck(Data: TMNCheck): String;
begin
  Result := Data.ValidatorIP + ' ' + IntToStr(Data.Block) + ' ' +
    Data.SignAddress + ' ' + Data.PublicKey + ' ' + Data.ValidNodes + ' ' + Data.Signature;
end;

function IsMyMNCheckDone(): Boolean;
var
  counter: Integer;
begin
  Result := False;
  EnterCriticalSection(MNsChecksLock);
  for counter := 0 to length(MasternodeChecks) - 1 do
  begin
    if MasternodeChecks[counter].ValidatorIP = LocalMasternodeIP then
    begin
      Result := True;
      break;
    end;
  end;
  LeaveCriticalSection(MNsChecksLock);
end;

{$ENDREGION MNs check handling}

{$REGION MNs FileData handling}

function GetMNAgeCount(TNode: TMNode): String;
var
  TIpandPort: String;
  counter: Integer;
  Number: Integer = 0;
begin
  Result := '';
  TIpandPort := TNode.Ip + ';' + IntToStr(TNode.Port);
  for counter := 0 to length(ArrayMNsData) - 1 do
  begin
    if ((TIpandPort = ArrayMNsData[counter].ipandport) and
      (TNode.Fund = ArrayMNsData[counter].address)) then
    begin
      Number := ArrayMNsData[counter].age;
      break;
    end;
  end;
  Result := ':' + IntToStr(number + 1);
end;

{$ENDREGION MNs FileData handling}

{$REGION MNs hash}

procedure SetMNsHash();
begin
  EnterCriticalSection(MNsHashLock);
  MyMNsHash := HashMD5File(MasterNodesFilename);
  LeaveCriticalSection(MNsHashLock);
end;

function GetMNsHash(): String;
begin
  EnterCriticalSection(MNsHashLock);
  Result := HashMD5File(MasterNodesFilename);
  LeaveCriticalSection(MNsHashLock);
end;

{$ENDREGION MNs hash}

{$REGION Received Masternodes}

function LengthReceivedMNs(): Integer;
begin
  EnterCriticalSection(ReceivedMNsLock);
  Result := Length(ArrReceivedMNs);
  LeaveCriticalSection(ReceivedMNsLock);
end;

procedure ClearReceivedMNs();
begin
  EnterCriticalSection(ReceivedMNsLock);
  setlength(ArrReceivedMNs, 0);
  LeaveCriticalSection(ReceivedMNsLock);
end;

function IsMNIPReceived(DataSource: String): Boolean;
var
  counter: Integer;
begin
  Result := False;
  DataSource := GetParameter(DataSource, 5);
  EnterCriticalSection(ReceivedMNsLock);
  for counter := 0 to length(ArrReceivedMNs) - 1 do
  begin
    if ArrReceivedMNs[counter] = DataSource then
    begin
      Result := True;
      Break;
    end;
  end;
  if not Result then
  begin
    Insert(DataSource, ArrReceivedMNs, LEngth(ArrReceivedMNs));
  end;
  LeaveCriticalSection(ReceivedMNsLock);
end;

{$ENDREGION Received Masternodes}

{$REGION Waiting Masternodes}

function LengthWaitingMNs(): Integer;
begin
  EnterCriticalSection(WaitingMNsLock);
  Result := Length(ArrWaitMNs);
  LeaveCriticalSection(WaitingMNsLock);
end;

procedure AddWaitingMNs(Linea: String);
begin
  if IsMNIPReceived(linea) then exit;
  ;
  EnterCriticalSection(WaitingMNsLock);
  Insert(Linea, ArrWaitMNs, Length(ArrWaitMNs));
  LeaveCriticalSection(WaitingMNsLock);
end;

function GetWaitingMNs(): String;
begin
  Result := '';
  if LengthWaitingMNs > 0 then
  begin
    EnterCriticalSection(WaitingMNsLock);
    Result := ArrWaitMNs[0];
    Delete(ArrWaitMNs, 0, 1);
  end;
  LeaveCriticalSection(WaitingMNsLock);
end;

{$ENDREGION Waiting Masternodes}

function LoadMNsFile(): String;
var
  lText: String = '';
begin
  Result := '';
  EnterCriticalSection(MNsFileLock);
  try
    reset(MNFileHandler);
    Readln(MNFileHandler, Result);
    Closefile(MNFileHandler);
  except
    on E: Exception do
    begin
      ToDeepDebug('Nosomasternodes,LoadMNsFile,' + E.Message);
    end;
  end {TRY};
  LeaveCriticalSection(MNsFileLock);
  SetMN_FileText(Result);
  //SetMNsHash;
end;

procedure SaveMNsFile(GotText: String);
begin
  EnterCriticalSection(MNsFileLock);
  try
    rewrite(MNFileHandler);
    Write(MNFileHandler, GotText, #13#10);
    Closefile(MNFileHandler);
    SetMN_FileText(GotText);
  except
    on E: Exception do
    begin
      ToDeepDebug('Nosomasternodes,SaveMNsFile,' + E.Message);
      SetMN_FileText('');
    end;
  end {TRY};
  LeaveCriticalSection(MNsFileLock);
  SetMNsHash;
end;

procedure SetMN_FileText(lvalue: String);
begin
  EnterCriticalSection(MN_FileTextLock);
  MN_FileText := lvalue;
  FillMNsArray(lValue);
  //FillNodeList; <- Critical: needs to be redone
  LeaveCriticalSection(MN_FileTextLock);
  SetMNsHash;
end;

function GetMN_FileText(): String;
begin
  EnterCriticalSection(MN_FileTextLock);
  Result := MN_FileText;
  LeaveCriticalSection(MN_FileTextLock);
  FillMNsArray(Result);
end;

procedure FillMNsArray(TValue: String);
var
  counter: Integer = 1;
  count2: Integer = 0;
  ThisData: String = '';
  ThisMN: TMNsData;
  TempArray: array of TMNsData;
  Added: Boolean = False;
  VerificatorsCount: Integer;
begin
  StartPerformanceMeasurement('FillMNsArray');
  try
    SetLength(ArrayMNsData, 0);
    SetLength(TempArray, 0);
    repeat
      ThisData := GetParameter(Tvalue, counter);
      if ThisData <> '' then
      begin
        ThisData := StringReplace(ThisData, ':', ' ', [rfReplaceAll]);
        ThisMN.ipandport := GetParameter(ThisData, 0);
        ThisMN.address := GetParameter(ThisData, 1);
        ThisMN.age := StrToIntDef(GetParameter(ThisData, 2), 1);
        Insert(ThisMN, TempArray, length(TempArray));
      end;
      Inc(counter);
    until thisData = '';
    for counter := 0 to length(TempArray) - 1 do
    begin
      ThisMN := TempArray[counter];
      Added := False;
      if length(ArrayMNsData) = 0 then
        Insert(ThisMN, ArrayMNsData, 0)
      else
      begin
        for count2 := 0 to length(ArrayMNsData) - 1 do
        begin
          if ThisMN.age > ArrayMNsData[count2].age then
          begin
            Insert(ThisMN, ArrayMNsData, count2);
            added := True;
            break;
          end;
        end;
        if not added then Insert(ThisMN, ArrayMNsData, length(ArrayMNsData));
      end;
    end;
  except
    on E: Exception do
      ToDeepDebug('Nosomasternodes,FillMNsArray,' + E.Message);
  end;
  StopPerformanceMeasurement('FillMNsArray');
end;

function GetVerificatorsText(): String;
var
  counter: Integer;
  VerCount: Integer;
begin
  Result := '';
  if length(ArrayMNsData) < 3 then exit;
  VerCount := (length(ArrayMNsData) div 10) + 3;
  for counter := 0 to VerCount - 1 do
  begin
    Result := Result + ArrayMNsData[counter].ipandport + ':';
  end;
end;



initialization
  SetLength(MNsListCopy, 0);
  SetLength(MNsList, 0);
  SetLength(ArrayIPsProcessed, 0);
  SetLength(MasternodeChecks, 0);
  SetLength(ArrayMNsData, 0);
  Setlength(ArrWaitMNs, 0);
  Setlength(ArrReceivedMNs, 0);
  InitCriticalSection(MNsIPProcLock);
  InitCriticalSection(MNsListLock);
  InitCriticalSection(VerifiedNodesLock);
  InitCriticalSection(VerifyThreadLock);
  InitCriticalSection(MNsChecksLock);
  InitCriticalSection(MNsFileLock);
  InitCriticalSection(MNsHashLock);
  InitCriticalSection(WaitingMNsLock);
  InitCriticalSection(MN_FileTextLock);
  InitCriticalSection(MNsChecksLock);
  InitCriticalSection(ReceivedMNsLock);


finalization
  DoneCriticalSection(MNsIPProcLock);
  DoneCriticalSection(MNsListLock);
  DoneCriticalSection(VerifiedNodesLock);
  DoneCriticalSection(VerifyThreadLock);
  DoneCriticalSection(MNsChecksLock);
  DoneCriticalSection(MNsFileLock);
  DoneCriticalSection(MNsHashLock);
  DoneCriticalSection(WaitingMNsLock);
  DoneCriticalSection(MN_FileTextLock);
  DoneCriticalSection(MNsChecksLock);
  DoneCriticalSection(ReceivedMNsLock);

end. // End unit
