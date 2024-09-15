unit Noso.Config;

{
NosoNosoCFG 1.1
March 23, 2024
Stand alone unit to control nosocfg file and functionalitys
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  Noso.Debug, Noso.General, Noso.Time, Noso.Crypto;

procedure SetCFGHash();
function GetCFGHash(): String;

procedure SetCFGFilename(Fname: String);
function SaveCFGToFile(Content: String): Boolean;
procedure GetCFGFromFile();
procedure SetCFGDataStr(Content: String);
function GetCFGDataStr(LParam: Integer = -1): String;
procedure AddCFGData(DataToAdd: String; CFGIndex: Integer);
procedure RemoveCFGData(DataToRemove: String; CFGIndex: Integer);
procedure SetCFGData(DataToSet: String; CFGIndex: Integer);
procedure RestoreCFGData();
procedure ClearCFGData(Index: String);
function IsSeedNode(IP: String): Boolean;

var
  CFGFilename: String = 'nosocfg.psk';
  CFGFile: Textfile;
  MyCFGHash: String = '';
  CS_CFGFile: TRTLCriticalSection;
  CS_CFGData: TRTLCriticalSection;
  CS_CFGHash: TRTLCriticalSection;
  NosoCFGString: String = '';
  LasTimeCFGRequest: Int64 = 0;
  DefaultNosoCFG: String = // CFG parameters
  {0 Mainnet mode}'NORMAL ' +
    {1 Seed nodes  }
    '204.10.194.22;8080:204.10.194.29;8080:204.10.194.32;8080:204.10.194.36;8080:204.10.194.33;8080: '
    +
    {2 NTP servers }
    'ts2.aco.net:hora.roa.es:time.esa.int:time.stdtime.gov.tw:stratum-1.sjc02.svwh.net:ntp1.sp.se:1.de.pool.ntp.org:ntps1.pads.ufrj.br:utcnist2.colorado.edu:tick.usask.ca:ntp1.st.keio.ac.jp: ' +
    {3 DEPRECATED  }'null: ' +
    {4 DEPRECATED  }'null: ' +
    {5 FREZZED     }'NpryectdevepmentfundsGE:';


implementation

{$REGION CFG hash}

procedure SetCFGHash();
begin
  EnterCriticalSection(CS_CFGHash);
  MyCFGHash := HashMD5String(GetCFGDataStr);
  LeaveCriticalSection(CS_CFGHash);
end;

function GetCFGHash(): String;
begin
  EnterCriticalSection(CS_CFGHash);
  Result := MyCFGHash;
  LeaveCriticalSection(CS_CFGHash);
end;

{$ENDREGION CFG hash}

{$REGION File access}

procedure SetCFGFilename(Fname: String);
var
  defseeds: String = '';
begin
  CFGFilename := Fname;
  AssignFile(CFGFile, CFGFilename);
  if not fileexists(CFGFilename) then
  begin
    SaveCFGToFile(DefaultNosoCFG);
    GetCFGFromFile;
    Defseeds := SendApiRequest(
      'https://raw.githubusercontent.com/nosocoin/NosoNode/main/defseeds.nos');
    if defseeds <> '' then
    begin
      SetCFGData(Defseeds, 1);
      Tolog('console', 'Defaults seeds downloaded from trustable source');
    end
    else
    begin
      ToLog('console', 'Unable to download default seeds. Please, use a fallback');
    end;
  end;
  GetCFGFromFile;
  SetCFGHash();
end;

function SaveCFGToFile(Content: String): Boolean;
begin
  EnterCriticalSection(CS_CFGFile);
  Result := SaveTextToDisk(CFGFilename, Content);
  SetCFGDataStr(Content);
  LeaveCriticalSection(CS_CFGFile);
end;

procedure GetCFGFromFile();
begin
  EnterCriticalSection(CS_CFGFile);
  SetCFGDataStr(LoadTextFromDisk(CFGFilename));
  LeaveCriticalSection(CS_CFGFile);
end;

{$ENDREGION File access}

{$REGION Data access}

procedure SetCFGDataStr(Content: String);
begin
  EnterCriticalSection(CS_CFGData);
  NosoCFGString := Content;
  LeaveCriticalSection(CS_CFGData);
  SetCFGHash;
end;

function GetCFGDataStr(LParam: Integer = -1): String;
begin
  EnterCriticalSection(CS_CFGData);
  if LParam < 0 then Result := NosoCFGString
  else
    Result := GetParameter(NosoCFGString, LParam);
  LeaveCriticalSection(CS_CFGData);
end;

{$ENDREGION Data access}

{$REGION Management}

procedure AddCFGData(DataToAdd: String; CFGIndex: Integer);
var
  LCFGstr: String;
  LArrString: array of String;
  DataStr: String;
  thisData: String;
  Counter: Integer = 0;
  FinalStr: String = '';
begin
  if DataToAdd[Length(DataToAdd)] <> ':' then
    DataToAdd := DataToAdd + ':';
  LCFGStr := GetCFGDataStr();
  SetLength(LArrString, 0);
  repeat
    ThisData := GetParameter(LCFGStr, counter);
    if ThisData <> '' then
      Insert(ThisData, LArrString, LEngth(LArrString));
    Inc(Counter);
  until thisData = '';
  if CFGIndex + 1 > LEngth(LArrString) then
  begin
    repeat
      Insert('', LArrString, LEngth(LArrString));
    until CFGIndex + 1 = LEngth(LArrString);
  end;
  DataStr := LArrString[CFGIndex];
  DataStr := DataStr + DataToAdd;
  LArrString[CFGIndex] := DataStr;
  for counter := 0 to length(LArrString) - 1 do
    FinalStr := FinalStr + ' ' + LArrString[counter];
  if FinalStr[1] = ' ' then Delete(FinalStr, 1, 1);
  SaveCFGToFile(FinalStr);
  LasTimeCFGRequest := UTCTime + 5;
end;

procedure RemoveCFGData(DataToRemove: String; CFGIndex: Integer);
var
  LCFGstr: String;
  LArrString: array of String;
  DataStr: String;
  thisData: String;
  Counter: Integer = 0;
  FinalStr: String = '';
begin
  if ((Length(DataToRemove) > 0) and (DataToRemove[Length(DataToRemove)] <> ':')) then
    DataToRemove := DataToRemove + ':';
  LCFGStr := GetCFGDataStr();
  SetLength(LArrString, 0);
  repeat
    ThisData := GetParameter(LCFGStr, counter);
    if ThisData <> '' then
    begin
      Insert(ThisData, LArrString, LEngth(LArrString));
    end;
    Inc(Counter);
  until thisData = '';
  DataStr := LArrString[CFGIndex];
  DataStr := StringReplace(DataStr, DataToRemove, '', [rfReplaceAll, rfIgnoreCase]);
  LArrString[CFGIndex] := DataStr;
  for counter := 0 to length(LArrString) - 1 do
    FinalStr := FinalStr + ' ' + LArrString[counter];
  FinalStr := Trim(FinalStr);
  if FinalStr[1] = ' ' then Delete(FinalStr, 1, 1);
  LasTimeCFGRequest := UTCTime + 5;
  SaveCFGToFile(FinalStr);
end;

procedure SetCFGData(DataToSet: String; CFGIndex: Integer);
var
  LCFGstr: String;
  LArrString: array of String;
  DataStr: String;
  thisData: String;
  Counter: Integer = 0;
  FinalStr: String = '';
begin
  if ((Length(DataToSet) > 0) and (DataToSet[Length(DataToSet)] <> ':') and
    (CFGIndex > 0)) then
    DataToSet := DataToSet + ':';
  if ((CFGIndex = 0) and (DatatoSet = '')) then exit;
  LCFGStr := GetCFGDataStr();
  SetLength(LArrString, 0);
  repeat
    ThisData := GetParameter(LCFGStr, counter);
    if ThisData <> '' then
    begin
      Insert(ThisData, LArrString, LEngth(LArrString));
    end;
    Inc(Counter);
  until thisData = '';
  LArrString[CFGIndex] := DataToSet;
  for counter := 0 to length(LArrString) - 1 do
    FinalStr := FinalStr + ' ' + LArrString[counter];
  FinalStr := Trim(FinalStr);
  LasTimeCFGRequest := UTCTime + 5;
  SaveCFGToFile(FinalStr);
end;

procedure RestoreCFGData();
begin
  LasTimeCFGRequest := UTCTime + 5;
  SaveCFGToFile(DefaultNosoCFG);
end;

procedure ClearCFGData(Index: String);
var
  LIndex: Integer;
begin
  LIndex := StrToIntDef(Index, -1);
  if LIndex <= 0 then exit;
  SetCFGData('null:', LIndex);
end;

{$ENDREGION Management}

{$REGION Information}

// If the specified IP a seed node
function IsSeedNode(IP: String): Boolean;
var
  SeedNodesStr: String;
begin
  Result := False;
  SeedNodesStr := ':' + GetCFGDataStr(1);
  if AnsiContainsStr(SeedNodesStr, ':' + ip + ';') then Result := True;
end;

{$REGION Information}

initialization
  InitCriticalSection(CS_CFGFile);
  InitCriticalSection(CS_CFGData);
  InitCriticalSection(CS_CFGHash);


finalization
  DoneCriticalSection(CS_CFGFile);
  DoneCriticalSection(CS_CFGData);
  DoneCriticalSection(CS_CFGHash);

end.
