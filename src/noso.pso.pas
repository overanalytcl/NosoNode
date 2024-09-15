unit Noso.Pso;

{
nosopsos 1.0
May 30th, 2023
Stand alone unit to handle all PSOs (active and expired) on noso mainnet.
Required: Nosogeneral
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Noso.General, Noso.Crypto, Noso.Debug;

type

  TPSOData = record
    Mode: Integer;
    Hash: String;
    owner: String;
    Expire: Integer;
    Members: String;
    Params: String;
  end;

  TMNsLock = record
    address: String[32];
    expire: Integer;
  end;

  TPSOHeader = record
    Block: Integer;
    MNsLock: Integer;
    Count: Integer;
  end;

  TPSOsArray = array of TPSOData;

// File access
function GetPSOHeadersFromFile: Boolean;
function LoadPSOFileFromDisk(): Boolean;
function SavePSOFileToDisk(BlockNumber: Integer): Boolean;
function GetPSOsAsMemStream(out LMs: TMemoryStream): Int64;
function SavePSOsToFile(const LStream: TMemoryStream): Boolean;

// Locked Masternodes control
function GetLockedMNsCount(): Integer;
function GetLockedMNIndex(index: Integer): TMNsLock;
function AddLockedMM(Address: String; block: Integer): Boolean;
function ClearExpiredLockedMNs(BlockNumber: Integer): Integer;
function IsLockedMN(Address: String): Boolean;
function LockedMNsRawString(): String;

// PSOHeaders control
function GetPSOHeaders(): TPSOHeader;
procedure SetPSOHeaders(NewData: TPSOHeader);

function GetPSOValue(LValue: String; LParams: String): String;

function AddNewPSO(LMode: Integer; LOwner: String; LExpire: Integer;
  LParams: String): Boolean;
function GetPSOsCopy(): TPSOsArray;

const
  PSOsFileName: String = 'NOSODATA' + DirectorySeparator + 'psos.dat';
  MNsLockExpireLapse: Integer = 2016;
  PSOTimestamp: String = '1';
  PSOBlock: String = '2';
  PSOAction: String = '3';
  PSOFee: String = '4';
  PSODuration: String = '5';
  PSOTarget: String = '6';
  PSOMinSize: String = '7';
  PSOMaxSize: String = '8';
  PSOOverfill: String = '9';
  PSOSource: String = '10';

var
  PSOsArray: array of TPSOData;
  MNSLockArray: array of TMNsLock;
  PSOHeader: TPSOHeader;
  PSOFileHash: String = '';
  CS_PSOsArray: TRTLCriticalSection;
  CS_PSOFile: TRTLCriticalSection;
  CS_LockedMNs: TRTLCriticalSection;
  CS_PSOHeaders: TRTLCriticalSection;

implementation

{$REGION Internal functions}

{$ENDREGION}

{$REGION File access}

function GetPSOHeadersFromFile: Boolean;
var
  MyStream: TMemoryStream;
  counter: Integer;
  MNData: TMNsLock;
  LPSOHeader: TPSOHeader;
begin
  MyStream := TMemoryStream.Create;
  try
    MyStream.LoadFromFile(PSOsFileName);
    SetLEngth(MNSLockArray, 0);
    MyStream.Position := 0;
    MyStream.ReadBuffer(LPSOHeader, SizeOf(PSOHeader));
    SetPSOHeaders(LPSOHeader);
    for counter := 0 to GetPSOHeaders.MNsLock - 1 do
    begin
      MNData := Default(TMNsLock);
      MyStream.Read(MNData, sizeof(MNData));
      ToLog('console', counter.ToString + ' ' + MNData.address + ' ' +
        MNData.expire.ToString);
      Insert(MNData, MNSLockArray, length(MNSLockArray));
    end;

  except
    ON E: Exception do
  end;
  MyStream.Free;
end;

procedure InsertLockedMN(Data: TMNsLock);
begin
  EnterCriticalSection(CS_LockedMNs);
  Insert(Data, MNSLockArray, length(MNSLockArray));
  LeaveCriticalSection(CS_LockedMNs);
end;

procedure InsertPSO(Data: TPSOData);
begin
  EnterCriticalSection(CS_PSOsArray);
  Insert(Data, PSOsArray, Length(PSOsArray));
  LeaveCriticalSection(CS_PSOsArray);
end;

function LoadPSOFileFromDisk(): Boolean;
var
  MyStream: TMemoryStream;
  Counter: Integer;
  NewRec: TPSOData;
  MNData: TMNsLock;
  StrSize: Int64;
  NewHeader: TPSOHeader;
  Errored: Boolean = False;
begin
  Result := False;
  PSOHeader := Default(TPSOHeader);
  SetLength(PSOsArray, 0);
  SetLength(MNSLockArray, 0);
  MyStream := TMemoryStream.Create;
  StrSize := GetPSOsAsMemStream(MyStream);
  NewHeader := Default(TPSOHeader);
  if StrSize > 0 then
  begin
    MyStream.Position := 0;
    try
      MyStream.Read(NewHeader, SizeOf(NewHeader));
    except
      ON E: Exception do
        errored := True;
    end;
    if not errored then
    begin
      SetPSOHeaders(NewHeader);
      try
        for counter := 0 to NewHeader.MNsLock - 1 do
        begin
          MNData := Default(TMNsLock);
          MyStream.Read(MNData, sizeof(MNData));
          InsertLockedMN(MNData);
        end;
      except
        ON E: Exception do
        begin
          errored := True;
        end;
      end;
    end;
    if not errored then
    begin
      try
        for Counter := 0 to NewHeader.Count - 1 do
        begin
          NewRec.Mode := MyStream.ReadWord;
          NewRec.Hash := MyStream.GetString;
          NewRec.Owner := MyStream.GetString;
          NewRec.Expire := MyStream.ReadDWord;
          NewRec.Members := MyStream.GetString;
          NewRec.Params := MyStream.GetString;
          InsertPSO(NewRec);
        end;
      except
        ON E: Exception do
        begin
          errored := True;
        end;
      end;
    end;
  end;
  MyStream.Free;
  Result := errored;
  if ((not fileExists(PSOsFileName)) or (Errored)) then
    SavePSOFileToDisk(PSOHeader.Block)
  else
    PSOFileHash := HashMD5File(PSOsFileName);
end;

function SavePSOFileToDisk(BlockNumber: Integer): Boolean;
var
  MyStream: TMemoryStream;
  counter: Integer;
  NewHeader: TPSOHeader;
begin
  Result := False;
  MyStream := TMemoryStream.Create;
  NewHeader := GetPSOHeaders;
  NewHeader.Block := BlockNumber;
  NewHeader.Count := Length(PSOsArray);
  NewHeader.MNsLock := GetLockedMNsCount;
  SetPSOHeaders(NewHeader);
  EnterCriticalSection(CS_PSOsArray);
  try
    MyStream.Write(NewHeader, Sizeof(PSOHeader));
    for counter := 0 to NewHeader.MNsLock - 1 do
      MyStream.Write(GetLockedMNIndex(counter), Sizeof(TMNsLock));
    for counter := 0 to Length(PSOsArray) - 1 do
    begin
      MyStream.WriteWord(PSOsArray[counter].Mode);
      MyStream.SetString(PSOsArray[counter].hash);
      MyStream.SetString(PSOsArray[counter].owner);
      MyStream.WriteDWord(PSOsArray[counter].Expire);
      MyStream.SetString(PSOsArray[counter].Members);
      MyStream.SetString(PSOsArray[counter].Params);
    end;
  except
    ON E: Exception do
      ToDeepDebug('nosopsos,SavePSOFileToDisk,' + e.Message);
  end;
  LeaveCriticalSection(CS_PSOsArray);
  SavePSOsToFile(MyStream);
  MyStream.Free;
  Result := True;
  PSOFileHash := HashMD5File(PSOsFileName);
end;

function GetPSOsAsMemStream(out LMs: TMemoryStream): Int64;
begin
  Result := 0;
  EnterCriticalSection(CS_PSOFile);
  try
    LMs.LoadFromFile(PSOsFileName);
    Result := LMs.Size;
    LMs.Position := 0;
  except
    ON E: Exception do
  end{Try};
  LeaveCriticalSection(CS_PSOFile);
end;

function SavePSOsToFile(const LStream: TMemoryStream): Boolean;
begin
  Result := False;
  EnterCriticalSection(CS_PSOFile);
  try
    LStream.SaveToFile(PSOsFileName);
    Result := True;
  except
    ON E: Exception do
    begin
      ToDeepDebug('nosopsos,SavePSOsToFile,' + e.Message);
    end;
  end{Try};
  LeaveCriticalSection(CS_PSOFile);
end;

{$ENDREGION}

{$REGION Locked Masternodes}

function GetLockedMNsCount(): Integer;
begin
  EnterCriticalSection(CS_LockedMNs);
  Result := length(MNSLockArray);
  LeaveCriticalSection(CS_LockedMNs);
end;

function GetLockedMNIndex(index: Integer): TMNsLock;
begin
  Result := Default(TMNsLock);
  if index < GetLockedMNsCount then
  begin
    EnterCriticalSection(CS_LockedMNs);
    Result := MNSLockArray[index];
    LeaveCriticalSection(CS_LockedMNs);
  end;
end;

function AddLockedMM(Address: String; block: Integer): Boolean;
var
  counter: Integer;
  Exists: Boolean = False;
  NewRec: TMNsLock;
begin
  Result := False;
  EnterCriticalSection(CS_LockedMNs);
  for counter := 0 to length(MNSLockArray) - 1 do
  begin
    if MNSLockArray[counter].address = Address then
    begin
      Exists := True;
      Break;
    end;
  end;
  if not Exists then
  begin
    NewRec.address := address;
    NewRec.expire := Block + MNsLockExpireLapse;
    Insert(NewRec, MNSLockArray, length(MNSLockArray));
    Result := True;
  end;
  LeaveCriticalSection(CS_LockedMNs);
end;

function ClearExpiredLockedMNs(BlockNumber: Integer): Integer;
var
  counter: Integer = 0;
  IsDone: Boolean = False;
begin
  Result := 0;
  EnterCriticalSection(CS_LockedMNs);
  repeat
    if Counter >= Length(MNSLockArray) then IsDOne := True
    else
    begin
      if MNSLockArray[counter].expire <= blocknumber then
      begin
        Delete(MNSLockArray, counter, 1);
        Inc(Result);
      end
      else
        Inc(Counter);
    end;
  until IsDone;
  LeaveCriticalSection(CS_LockedMNs);
end;

function IsLockedMN(Address: String): Boolean;
var
  counter: Integer;
begin
  Result := False;
  EnterCriticalSection(CS_LockedMNs);
  for counter := 0 to length(MNSLockArray) - 1 do
  begin
    if MNSLockArray[counter].address = address then
    begin
      Result := True;
      break;
    end;
  end;
  LeaveCriticalSection(CS_LockedMNs);
end;

function LockedMNsRawString(): String;
var
  counter: Integer;
begin
  Result := '';
  EnterCriticalSection(CS_LockedMNs);
  for counter := 0 to length(MNSLockArray) - 1 do
  begin
    Result := Result + MNSLockArray[counter].address + ',' +
      MNSLockArray[counter].expire.ToString() + ' ';
  end;
  LeaveCriticalSection(CS_LockedMNs);
  Trim(Result);
end;

{$ENDREGION}

{$REGION PSOHeaders control}

function GetPSOHeaders(): TPSOHeader;
begin
  EnterCriticalSection(CS_PSOHeaders);
  Result := PSOHeader;
  LeaveCriticalSection(CS_PSOHeaders);
end;

procedure SetPSOHeaders(NewData: TPSOHeader);
begin
  EnterCriticalSection(CS_PSOHeaders);
  PSOHeader := NewData;
  LeaveCriticalSection(CS_PSOHeaders);
end;

{$ENDREGION}

{$REGION PSOs control}

function GetPSOValue(LValue: String; LParams: String): String;
var
  counter: Integer = 0;
  ThisItem: String;
  ILabel: String;
  IValue: String;
begin
  Result := '';
  LParams := StringReplace(LParams, ';', ' ', [rfReplaceAll, rfIgnoreCase]);
  repeat
    ThisItem := GetParameter(LParams, Counter);
    if ThisItem <> '' then
    begin
      ThisItem := StringReplace(ThisItem, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
      ILabel := GetParameter(ThisItem, 0);
      IValue := GetParameter(ThisItem, 1);
      if ILabel = LValue then Exit(IValue);
    end;
    Inc(counter);
  until thisItem = '';
end;

function AddNewPSO(LMode: Integer; LOwner: String; LExpire: Integer;
  LParams: String): Boolean;
var
  NewRec: TPSOData;
begin
  Result := True;
  NewRec := Default(TPSOData);
  NewRec.Mode := Lmode;
  NewRec.owner := LOwner;
  NewRec.Expire := LExpire;
  NewRec.Hash := HashMD5String(LOwner + LParams + IntToStr(LMode));
  NewRec.Members := '';
  NewRec.Params := LParams;
  EnterCriticalSection(CS_PSOsArray);
  Insert(NewRec, PSOsArray, Length(PSOsArray));
  LeaveCriticalSection(CS_PSOsArray);
end;

function GetPSOsCopy(): TPSOsArray;
begin
  SetLength(Result, 0);
  EnterCriticalSection(CS_PSOsArray);
  Result := copy(PSOsArray, 0, length(PSOsArray));
  LeaveCriticalSection(CS_PSOsArray);
end;

{$ENDREGION}

initialization
  InitCriticalSection(CS_PSOsArray);
  InitCriticalSection(CS_PSOFile);
  InitCriticalSection(CS_LockedMNs);
  InitCriticalSection(CS_PSOHeaders);

finalization
  DoneCriticalSection(CS_PSOsArray);
  DoneCriticalSection(CS_PSOFile);
  DoneCriticalSection(CS_LockedMNs);
  DoneCriticalSection(CS_PSOHeaders);

end. {END UNIT}
