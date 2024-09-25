unit MP.Coin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPForm, MP.Gui, Clipbrd, strutils, Noso.Debug, Noso.General,
  Noso.Crypto, Noso.Summary, Noso.Time, Noso.Pso, Noso.WallCon, Noso.Block, Noso.Network;

function GetAddressAvailable(address: String): Int64;
function GetAddressPendingPays(Address: String): Int64;
function GetAddressIncomingpays(Address: String): Int64;
 //function TranxAlreadyPending(TrxHash:string):boolean;
 //function TrxExistsInLastBlock(trfrhash:String):boolean;
 //function GetLastPendingTime():int64;
 //function AddArrayPoolTXs(order:TOrderData):boolean;
procedure VerifyIfPendingIsMine(order: TOrderData);
function AddressAlreadyCustomized(address: String): Boolean;
function GVTAlreadyTransfered(NumberStr: String): Boolean;
function AliasAlreadyExists(Addalias: String): Boolean;
//function GetFee(monto:int64):Int64;
function SendFundsFromAddress(Origen, Destino: String; monto, comision: Int64;
  reference, ordertime: String; linea: Integer): TOrderData;
procedure CheckForMyPending();
//function GetMaximunToSend(monto:int64):int64;
function GetCurrentStatus(mode: Integer): String;
function GetBlockHeaders(numberblock: Integer): String;
function ValidRPCHost(hoststr: String): Boolean;
function PendingRawInfo(ForRPC: Boolean = True): String;
{
Function GetPendingCount():integer;
Procedure ClearAllPending();
}

implementation

uses
  MP.Block, MP.Red, MP.Parser, MP.Disk, MP.Protocol;

function GetAddressAvailable(address: String): Int64;
begin
  Result := GetAddressBalanceIndexed(address) - GetAddressPendingPays(address);
end;

// Returns the balance an address already have committed to be paid.
function GetAddressPendingPays(Address: String): Int64;
var
  cont: Integer;
  CopyPendings: array of TOrderData;
begin
  Result := 0;
  if Address = '' then exit;
  if GetPendingTransactionCount > 0 then
  begin
    EnterCriticalSection(PendingTransactionsLock);
    SetLength(CopyPendings, 0);
    CopyPendings := copy(PendingTransactionsPool, 0, length(PendingTransactionsPool));
    LeaveCriticalSection(PendingTransactionsLock);
    for cont := 0 to length(CopyPendings) - 1 do
    begin
      if address = CopyPendings[cont].address then
        Result := Result + CopyPendings[cont].AmountFee + CopyPendings[cont].AmountTransferred;
    end;
  end;
  if LastBlockIndex >= ProtocolUpdateBlock then
    if IsLockedMN(Address) then Inc(Result, 1050000000000);
end;

// Returns the pending incomings for the specified address**ONLY HASH ACCEPTED
function GetAddressIncomingpays(Address: String): Int64;
var
  cont: Integer;
  CopyPendings: array of TOrderData;
begin
  Result := 0;
  if GetPendingTransactionCount > 0 then
  begin
    EnterCriticalSection(PendingTransactionsLock);
    SetLength(CopyPendings, 0);
    CopyPendings := copy(PendingTransactionsPool, 0, length(PendingTransactionsPool));
    LeaveCriticalSection(PendingTransactionsLock);
    for cont := 0 to length(CopyPendings) - 1 do
    begin
      if address = PendingTransactionsPool[cont].receiver then
        Result := Result + PendingTransactionsPool[cont].AmountTransferred;
    end;
  end;
end;

{
//Devuelve si una transaccion ya se encuentra pendiente
function TransactionAlreadyPending(TrxHash:string):boolean;
var
  cont : integer;
Begin
Result := false;
for cont := 0 to GetPendingTransactionCount-1 do
   begin
   if TrxHash = PendingTransactionsPool[cont].TrfrID then
      begin
      result := true;
      break;
      end;
   end;
End;
}

{
// Devuelve si una transaccion existe en el ultimo bloque
function TransferExistsInLastBlock(trfrhash:String):boolean;
var
  ArrayLastBlockTrxs : TBlockOrders;
  cont : integer;
Begin
Result := false;
ArrayLastBlockTrxs := Default(TBlockOrders);
ArrayLastBlockTrxs := GetBlockTransfers(LastBlockIndex);
for cont := 0 to length(ArrayLastBlockTrxs)-1 do
   begin
   if ArrayLastBlockTrxs[cont].TrfrID = trfrhash then
     begin
     result := true ;
     break
     end;
   end;
SetLength(ArrayLastBlockTrxs,0);
End;
}

{
function GetLastPendingTime():int64;
Begin
  result := 0;
  EnterCriticalSection(PendingTransactionsLock);
  if length(PendingTransactionsPool) > 0 then result := PendingTransactionsPool[length(PendingTransactionsPool)-1].TimeStamp;
  LeaveCriticalSection(PendingTransactionsLock);
End;
}

{
// Añade la transaccion pendiente en su lugar
function AddTransactionToPool(order:TOrderData):boolean;
var
  cont : integer = 0;
  insertar : boolean = false;
  resultado : integer = 0;
Begin
StartPerformanceMeasurement('AddArrayPoolTXs');
//if order.OrderType='FEE' then exit;
if order.TimeStamp < LastBlockData.TimeStart then exit;
if TransferExistsInLastBlock(order.TrfrID) then exit;
if ((BlockAge>585) and (order.TimeStamp < LastBlockData.TimeStart+540) ) then exit;
if not TransactionAlreadyPending(order.TrfrID) then
   begin
   EnterCriticalSection(PendingTransactionsLock);
   while cont < length(PendingTransactionsPool) do
     begin
     if order.TimeStamp < PendingTransactionsPool[cont].TimeStamp then
        begin
        insertar := true;
        resultado := cont;
        break;
        end
     else if order.TimeStamp = PendingTransactionsPool[cont].TimeStamp then
        begin
        if order.OrderID < PendingTransactionsPool[cont].OrderID then
           begin
           insertar := true;
           resultado := cont;
           break;
           end
        else if order.OrderID = PendingTransactionsPool[cont].OrderID then
           begin
           if order.TrxLine < PendingTransactionsPool[cont].TransferLine then
              begin
              insertar := true;
              resultado := cont;
              break;
              end;
           end;
        end;
     cont := cont+1;
     end;
   if not insertar then resultado := length(PendingTransactionsPool);
   Insert(order,PendingTransactionsPool,resultado);
   LeaveCriticalSection(PendingTransactionsLock);
   result := true;
   VerifyIfPendingIsMine(order);
   end;
StopPerformanceMeasurement('AddArrayPoolTXs');
End;
}

// Verifica si una orden especifica es del usuario
procedure VerifyIfPendingIsMine(order: TOrderData);
var
  DireccionEnvia: String;
  SendIndex: Integer;
begin
  DireccionEnvia := order.address;
  SendIndex := WallAddIndex(DireccionEnvia);
  if SendIndex >= 0 then
  begin
    SetPendingForAddress(SendIndex, GetWallArrIndex(SendIndex).Pending +
      Order.AmountFee + order.AmountTransferred);
    OutgoingAmount := OutgoingAmount + Order.AmountFee + order.AmountTransferred;
    if not form1.ImageOut.Visible then form1.ImageOut.Visible := True;
  end;
  if WallAddIndex(Order.Receiver) >= 0 then
  begin
    IncomingAmount := IncomingAmount + order.AmountTransferred;
    if not form1.ImageInc.Visible then form1.ImageInc.Visible := True;
  end;
  UpdateDirPanel := True;
end;

// Devuelve si una direccion ya posee un alias
function AddressAlreadyCustomized(address: String): Boolean;
var
  cont: Integer;
begin
  Result := False;
  if GetAddressAlias(address) <> '' then Exit(True);
  for cont := 0 to GetPendingTransactionCount - 1 do
    if ((PendingTransactionsPool[cont].Address = address) and
      (PendingTransactionsPool[cont].OrderType = 'CUSTOM')) then
      exit(True);
end;

function GVTAlreadyTransfered(NumberStr: String): Boolean;
var
  number: Integer;
  counter: Integer;
begin
  Result := False;
  Number := StrToIntDef(NumberStr, -1);
  if number < 0 then
  begin
    Result := True;
    exit;
  end;
  for counter := 0 to GetPendingTransactionCount - 1 do
  begin
    if ((PendingTransactionsPool[counter].reference = NumberStr) and
      (PendingTransactionsPool[counter].OrderType = 'SNDGVT')) then
    begin
      Result := True;
      break;
    end;
  end;
end;

// verify if an alias is already registered
function AliasAlreadyExists(Addalias: String): Boolean;
var
  cont: Integer;
  LRecord: TSummaryData;
begin
  Result := False;
  if FindSummaryIndexPosition(AddAlias, LRecord, True) >= 0 then Exit(True);
  for cont := 0 to GetPendingTransactionCount - 1 do
    if ((PendingTransactionsPool[cont].OrderType = 'CUSTOM') and
      (PendingTransactionsPool[cont].Receiver = Addalias)) then
      Exit(True);
end;

// Devuelve la comision por un monto
function GetFee(monto: Int64): Int64;
begin
  Result := monto div TransferFee;
  if Result < MinimumTransactionFee then Result := 1000000;//MinimumTransactionFee;
end;

// Obtiene una orden de envio de fondos desde una direccion
function SendFundsFromAddress(Origen, Destino: String; monto, comision: Int64;
  reference, ordertime: String; linea: Integer): TOrderData;
var
  MontoDisponible, Montotrfr, comisionTrfr: Int64;
  OrderInfo: TOrderData;
begin
  StartPerformanceMeasurement('SendFundsFromAddress');
  MontoDisponible := GetAddressBalanceIndexed(GetWallArrIndex(WallAddIndex(origen)).Hash) -
    GetAddressPendingPays(Origen);
  if MontoDisponible > comision then ComisionTrfr := Comision
  else
    comisiontrfr := montodisponible;
  if montodisponible > monto + comision then montotrfr := monto
  else
    montotrfr := montodisponible - comision;
  if montotrfr < 0 then montotrfr := 0;
  OrderInfo := Default(TOrderData);
  OrderInfo.OrderID := '';
  OrderInfo.OrderLineCount := 1;
  OrderInfo.OrderType := 'TRFR';
  OrderInfo.TimeStamp := StrToInt64(OrderTime);
  OrderInfo.reference := reference;
  OrderInfo.TransferLine := linea;
  OrderInfo.Sender := GetWallArrIndex(WallAddIndex(origen)).PublicKey;
  OrderInfo.Address := GetWallArrIndex(WallAddIndex(origen)).Hash;
  OrderInfo.Receiver := Destino;
  OrderInfo.AmountFee := ComisionTrfr;
  OrderInfo.AmountTransferred := montotrfr;
  OrderInfo.Signature := GetStringSigned(ordertime + origen + destino + IntToStr(montotrfr) +
    IntToStr(comisiontrfr) + IntToStr(linea),
    GetWallArrIndex(WallAddIndex(origen)).PrivateKey);
  OrderInfo.TransferId := GetTransferHash(ordertime + origen + destino +
    IntToStr(monto) + IntToStr(LastBlockIndex));
  Result := OrderInfo;
  StopPerformanceMeasurement('SendFundsFromAddress');
end;

// verifica si en las transaccione pendientes hay alguna de nuestra cartera
procedure CheckForMyPending();
var
  counter: Integer = 0;
  DireccionEnvia: String;
  AddIndex: Integer;
begin
  IncomingAmount := 0;
  OutgoingAmount := 0;
  if GetPendingTransactionCount = 0 then
  begin
    form1.ImageInc.Visible := False;
    form1.ImageOut.Visible := False;
  end
  else
  begin
    for counter := 0 to GetPendingTransactionCount - 1 do
    begin
      DireccionEnvia := PendingTransactionsPool[counter].Address;
      AddIndex := WallAddIndex(DireccionEnvia);
      if AddIndex >= 0 then
      begin
        OutgoingAmount := OutgoingAmount + PendingTransactionsPool[counter].AmountFee +
          PendingTransactionsPool[counter].AmountTransferred;
        SetPendingForAddress(AddIndex, GetWallArrIndex(AdDIndex).Pending +
          PendingTransactionsPool[counter].AmountFee + PendingTransactionsPool[counter].AmountTransferred);
        //WalletArray[WallAddIndex(DireccionEnvia)].Pending:=WalletArray[WallAddIndex(DireccionEnvia)].Pending+PendingTransactionsPool[counter].AmountFee+PendingTransactionsPool[counter].AmountTransferred;
      end;
      if WallAddIndex(PendingTransactionsPool[counter].Receiver) >= 0 then
        IncomingAmount := IncomingAmount + PendingTransactionsPool[counter].AmountTransferred;
    end;
    if IncomingAmount > 0 then form1.ImageInc.Visible := True
    else
      form1.ImageInc.Visible := False;
    if OutgoingAmount > 0 then form1.ImageOut.Visible := True
    else
      form1.ImageOut.Visible := False;
    UpdateDirPanel := True;
  end;
end;

// Retorna cuanto es lo maximo que se puede enviar
function GetMaximunToSend(monto: Int64): Int64;
var
  Disponible: Int64;
  maximo: Int64;
  comision: Int64;
  Envio: Int64;
  Diferencia: Int64;
begin
  Disponible := monto;
  if ((disponible < 1000000{MinimumTransactionFee}) or (Disponible < 0)) then
  begin
    Result := 0;
    exit;
  end;
  maximo := (Disponible * TransferFee) div (TransferFee + 1);
  comision := maximo div TransferFee;
  if Comision < 1000000{MinimumTransactionFee} then Comision := 1000000{MinimumTransactionFee};
  Envio := maximo + comision;
  Diferencia := Disponible - envio;
  Result := maximo + diferencia;
end;

function GetCurrentStatus(mode: Integer): String;
var
  Resultado: String = '';
begin
  resultado := resultado + 'ServerON    : ' + BoolToStr(Form1.Server.Active, True) + ' ';
  if mode = 1 then
  begin
    resultado := resultado + 'Date        : ' + FormatDateTime('dd MMMM YYYY HH:MM:SS.zzz',
      Now) + slinebreak;
    resultado := resultado + 'MyConStatus : ' + IntToStr(NodeConnectionStatus) + slinebreak;
    Resultado := resultado + 'OS          : ' + OSVersion + slinebreak;
    Resultado := resultado + 'WalletVer   : ' + MainnetVersion + NodeRelease + slinebreak;
  end;
  Result := resultado;
end;

function GetBlockHeaders(numberblock: Integer): String;
var
  Header: BlockHeaderData;
  blockhash: String;
begin
  Header := default(BlockHeaderData);
  if fileexists(BlockDirectory + IntToStr(numberblock) + '.blk') then
  begin
    Header := LoadBlockDataHeader(numberblock);
    blockhash := HashMD5File(BlockDirectory + IntToStr(numberblock) + '.blk');
    Header.Solution := StringReplace(Header.Solution, ' ', #0, [rfReplaceAll, rfIgnoreCase]);
    Header.LastBlockHash := StringReplace(Header.LastBlockHash, ' ',
      '', [rfReplaceAll, rfIgnoreCase]);
    Result := (format('%d'#127'%d'#127'%d'#127'%d'#127'%d'#127'%d'#127'%d'#127'%s'#127'%s'#127'%s'#127'%d'#127'%s'#127'%d'#127'%d'#127'%s',
      [Header.Number, Header.TimeStart, Header.TimeEnd, Header.TimeTotal,
      Header.TimeLast20, Header.TrxTotales, Header.Difficult,
      Header.TargetHash, Header.Solution, Header.LastBlockHash,
      Header.NxtBlkDiff, Header.AccountMiner, Header.MinerFee,
      Header.Reward, blockhash]));
  end;
end;

function ValidRPCHost(hoststr: String): Boolean;
var
  HostIP: String;
  whitelisted: String;
  thiswhitelist: String;
  counter: Integer = 0;
begin
  Result := False;
  HostIP := StringReplace(hoststr, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
  HostIP := GetParameter(HostIP, 0);
  whitelisted := StringReplace(RPCAllowedIPs, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
  repeat
    thiswhitelist := GetParameter(whitelisted, counter);
    if thiswhitelist = HostIP then Result := True;
    counter += 1;
  until thiswhitelist = '';
end;

// Returns the basic info of the pending orders
function PendingRawInfo(ForRPC: Boolean = True): String;
var
  CopyArrayPoolTXs: array of TOrderData;
  counter: Integer;
  ThisPending: String;
begin
  Result := '';
  if Length(PendingTransactionsPool) > 0 then
  begin
    EnterCriticalSection(PendingTransactionsLock);
    SetLength(CopyArrayPoolTXs, 0);
    CopyArrayPoolTXs := copy(PendingTransactionsPool, 0, length(PendingTransactionsPool));
    LeaveCriticalSection(PendingTransactionsLock);
    for counter := 0 to Length(CopyArrayPoolTXs) - 1 do
    begin
      if ForRPC then
      begin
        ThisPending := CopyArrayPoolTXs[counter].OrderID + ',' +
          CopyArrayPoolTXs[counter].TimeStamp.ToString + ',' +
          CopyArrayPoolTXs[counter].OrderType + ',' +
          CopyArrayPoolTXs[counter].Address + ',' +
          CopyArrayPoolTXs[counter].Receiver + ',' +
          CopyArrayPoolTXs[counter].AmountTransferred.ToString + ',' +
          CopyArrayPoolTXs[counter].AmountFee.ToString + ',' +
          CopyArrayPoolTXs[counter].Reference
        {+','+CopyArrayPoolTXs[counter].TimeStamp.ToString};

      end
      else
      begin
        ThisPending := CopyArrayPoolTXs[counter].OrderType + ',' +
          CopyArrayPoolTXs[counter].Address + ',' +
          CopyArrayPoolTXs[counter].Receiver + ',' +
          CopyArrayPoolTXs[counter].AmountTransferred.ToString + ',' +
          CopyArrayPoolTXs[counter].AmountFee.ToString
        {+','+CopyArrayPoolTXs[counter].TimeStamp.ToString};
      end;
      Result := Result + ThisPending + ' ';
    end;
    Trim(Result);
  end;
end;

{
// Returns the length of the pending transactions array safely
Function GetPendingTransactionCount():integer;
Begin
EnterCriticalSection(PendingTransactionsLock);
result := Length(PendingTransactionsPool);
LeaveCriticalSection(PendingTransactionsLock);
End;

// Clear the pending transactions array safely
Procedure ClearAllPendingTransactions();
Begin
EnterCriticalSection(PendingTransactionsLock);
SetLength(PendingTransactionsPool,0);
LeaveCriticalSection(PendingTransactionsLock);
End;
}

end. // END UNIT
