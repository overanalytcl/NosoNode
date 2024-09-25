unit MP.Rpc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MP.Gui, FPJSON, jsonparser, strutils, MP.Coin,
  MP.Red, MP.Block, Noso.Debug,
  Noso.General, Noso.Crypto, Noso.Summary, Noso.Consensus,
  Noso.WallCon, Noso.Pso, Noso.Network, Noso.Block,
  Noso.Gvt, Noso.Masternodes;

procedure SetRPCPort(LineText: String);
procedure setRPCpassword(newpassword: String);
procedure SetRPCOn();
procedure SetRPCOff();

// *** RPC PARSE FUNCTIONS ***

function IsValidJSON(MyJSONstring: String): Boolean;
function GetJSONErrorString(ErrorCode: Integer): String;
function GetJSONErrorCode(ErrorCode, JSONIdNumber: Integer): String;
function GetJSONResponse(ResultToSend: String; JSONIdNumber: Integer): String;
function ParseRPCJSON(jsonreceived: String): String;

function ObjectFromString(MyString: String): String;

function RPC_Restart(NosoPParams: String): String;
function RPC_Banned(NosoPParams: String): String;
function RPC_AddressBalance(NosoPParams: String): String;
function RPC_OrderInfo(NosoPParams: String): String;
function RPC_Blockinfo(NosoPParams: String): String;
function RPC_Mainnetinfo(NosoPParams: String): String;
function RPC_PendingOrders(NosoPParams: String): String;
function RPC_LockedMNs(NosoPParams: String): String;
function RPC_GetPeers(NosoPParams: String): String;
function RPC_BlockOrders(NosoPParams: String): String;
function RPC_Masternodes(NosoPParams: String): String;
function RPC_Blockmns(NosoPParams: String): String;
function RPC_WalletBalance(NosoPParams: String): String;
function RPC_NewAddress(NosoPParams: String): String;
function RPC_NewAddressFull(NosoPParams: String): String;
function RPC_ValidateAddress(NosoPParams: String): String;
function RPC_SetDefault(NosoPParams: String): String;
function RPC_GVTInfo(NosoPParams: String): String;
function RPC_CheckCertificate(NosoPParams: String): String;
function RPC_SubmitOrder(NosoPParams: String; waitresponse: Boolean = False): String;
function RPC_SendFunds(NosoPParams: String): String;


implementation

uses
  MPForm, MP.Parser, MP.Disk, MP.Protocol;

// Sets RPC port
procedure SetRPCPort(LineText: String);
var
  Value: Integer;
begin
  Value := StrToIntDef(GetParameter(LineText, 1), 0);
  if ((Value <= 0) or (Value > 65535)) then
  begin
    ToLog('console', 'Invalid value');
  end
  else if Form1.RPCServer.Active then
    ToLog('console', 'Can not change the RPC port when it is active')
  else
  begin
    RPCPort := Value;
    ToLog('console', 'RPC port set to: ' + IntToStr(Value));
    S_AdvOpt := True;
  end;
end;

procedure setRPCpassword(newpassword: String);
var
  counter: Integer;
  oldpassword: String;
begin
  oldpassword := RPCPass;
  trim(newpassword);
  RPCPass := newpassword;
end;

// Turn on RPC server
procedure SetRPCOn();
begin
  if not Form1.RPCServer.Active then
  begin
    try
      Form1.RPCServer.Bindings.Clear;
      Form1.RPCServer.DefaultPort := RPCPort;
      Form1.RPCServer.Active := True;
      G_Launching := True;
      G_Launching := False;
      ToLog('console', 'RPC server ENABLED');
    except
      on E: Exception do
      begin
        ToLog('console', 'Unable to start RPC port');
        G_Launching := True;
        G_Launching := False;
      end;
    end; {TRY}
  end
  else
    ToLog('console', 'RPC server already ENABLED');
end;

// Turns off RPC server
procedure SetRPCOff();
begin
  if Form1.RPCServer.Active then
  begin
    Form1.RPCServer.Active := False;
    ToLog('console', 'RPC server DISABLED');
    G_Launching := True;
    G_Launching := False;
  end
  else
    ToLog('console', 'RPC server already DISABLED');
end;

 // ***************************
 // *** RPC PARSE FUNCTIONS ***
 // ***************************

// Returns if a string is a valid JSON data
function IsValidJSON(MyJSONstring: String): Boolean;
var
  MyData: TJSONData;
begin
  Result := True;
  try
    MyData := GetJSON(MyJSONstring);
    Mydata.Free;
  except
    on E: ejsonparser do
      Result := False;
  end;
end;

// Returns the string of each error code
function GetJSONErrorString(ErrorCode: Integer): String;
begin
  if ErrorCode = 400 then Result := 'Bad Request'
  else if ErrorCode = 401 then Result := 'Invalid JSON request'
  else if ErrorCode = 402 then Result := 'Invalid method'
  else if ErrorCode = 407 then Result := 'Send funds failed'
  else if ErrorCode = 498 then Result := 'Not authorized'
  else if ErrorCode = 499 then Result := 'Unexpected error'

  {...}
  else
    Result := 'Unknown error code';
end;

// Returns a valid error JSON String
function GetJSONErrorCode(ErrorCode, JSONIdNumber: Integer): String;
var
  JSONResultado, JSONErrorObj: TJSONObject;
begin
  Result := '';
  JSONResultado := TJSONObject.Create;
  JSONErrorObj := TJSONObject.Create;
  try
    JSONResultado.Add('jsonrpc', TJSONString.Create('2.0'));
    JSONErrorObj.Add('code', TJSONIntegerNumber.Create(ErrorCode));
    JSONErrorObj.Add('message', TJSONString.Create(GetJSONErrorString(ErrorCode)));
    JSONResultado.Add('error', JSONErrorObj);
    JSONResultado.Add('id', TJSONIntegerNumber.Create(JSONIdNumber));
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on GetJSONErrorCode: ' + E.Message)
  end; {TRY}
  Result := JSONResultado.AsJSON;
  JSONResultado.Free;
end;

// Returns a valid response JSON string
function GetJSONResponse(ResultToSend: String; JSONIdNumber: Integer): String;
var
  JSONResultado, Resultado: TJSONObject;
  paramsarray: TJSONArray;
  myParams: TStringArray;
  counter: Integer;
  Errored: Boolean = False;
begin
  Result := '';
  paramsarray := TJSONArray.Create;
  if length(ResultToSend) > 0 then myParams := ResultToSend.Split(' ');
  JSONResultado := TJSONObject.Create;
  try
    JSONResultado.Add('jsonrpc', TJSONString.Create('2.0'));
    if length(myparams) > 0 then
      for counter := low(myParams) to high(myParams) do
        if myParams[counter] <> '' then
        begin
          paramsarray.Add(GetJSON(ObjectFromString(myParams[counter])));
        end;
    SetLength(MyParams, 0);
    JSONResultado.Add('result', paramsarray);
    JSONResultado.Add('id', TJSONIntegerNumber.Create(JSONIdNumber));
  except
    ON E: Exception do
    begin
      Result := GetJSONErrorCode(499, JSONIdNumber);
      JSONResultado.Free;
      paramsarray.Free;
      Errored := True;
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on GetJSONResponse: ' + E.Message);
    end;
  end; {TRY}
  if not errored then Result := JSONResultado.AsJSON;
  JSONResultado.Free;
end;

function ObjectFromString(MyString: String): String;
var
  resultado: TJSONObject;
  orderobject: TJSONObject;
  objecttype: String;
  blockorders, Newaddresses: Integer;
  ordersarray: TJSONArray;
  counter: Integer;
begin
  resultado := TJSONObject.Create;
  MyString := StringReplace(MyString, #127, ' ', [rfReplaceAll, rfIgnoreCase]);
  objecttype := GetParameter(mystring, 0);
  if objecttype = 'test' then
  begin
    resultado.Add('result', 'testok');
  end
  else if objecttype = 'banned' then
  begin
    resultado.Add('result', 'banned');
  end
  else if objecttype = 'restart' then
  begin
    resultado.Add('result', 'True');
  end
  else if objecttype = 'balance' then
  begin
    resultado.Add('valid', StrToBool(GetParameter(mystring, 1)));
    resultado.Add('address', TJSONString.Create(GetParameter(mystring, 2)));
    if GetParameter(mystring, 3) = 'null' then resultado.Add('alias', TJSONNull.Create)
    else
      resultado.Add('alias', GetParameter(mystring, 3));
    resultado.Add('balance', TJSONInt64Number.Create(StrToInt64(GetParameter(mystring, 4))));
    resultado.Add('incoming', TJSONInt64Number.Create(StrToInt64(GetParameter(mystring, 5))));
    resultado.Add('outgoing', TJSONInt64Number.Create(StrToInt64(GetParameter(mystring, 6))));
  end
  else if objecttype = 'orderinfo' then
  begin
    resultado.Add('valid', StrToBool(GetParameter(mystring, 1)));
    if StrToBool(GetParameter(mystring, 1)) then
    begin
      orderobject := TJSONObject.Create;
      orderobject.Add('orderid', GetParameter(mystring, 2));
      orderobject.Add('timestamp', StrToInt64(GetParameter(mystring, 3)));
      orderobject.Add('block', StrToInt64(GetParameter(mystring, 4)));
      orderobject.Add('type', GetParameter(mystring, 5));
      orderobject.Add('trfrs', StrToInt(GetParameter(mystring, 6)));
      orderobject.Add('receiver', GetParameter(mystring, 7));
      orderobject.Add('amount', StrToInt64(GetParameter(mystring, 8)));
      orderobject.Add('fee', StrToInt64(GetParameter(mystring, 9)));
      if GetParameter(mystring, 10) = 'null' then
        orderobject.Add('reference', TJSONNull.Create)
      else
        orderobject.Add('reference', GetParameter(mystring, 10));
      orderobject.Add('sender', GetParameter(mystring, 11));
      resultado.Add('order', orderobject);
    end
    else
      resultado.Add('order', TJSONNull.Create);
  end
  else if objecttype = 'blockinfo' then
  begin
    resultado.Add('valid', StrToBool(GetParameter(mystring, 1)));
    resultado.Add('number', StrToIntDef(GetParameter(mystring, 2), -1));
    resultado.Add('timestart', StrToInt64Def(GetParameter(mystring, 3), -1));
    resultado.Add('timeend', StrToInt64Def(GetParameter(mystring, 4), -1));
    resultado.Add('timetotal', StrToIntDef(GetParameter(mystring, 5), -1));
    resultado.Add('last20', StrToIntDef(GetParameter(mystring, 6), -1));
    resultado.Add('totaltransactions', StrToIntDef(GetParameter(mystring, 7), -1));
    resultado.Add('difficulty', StrToIntDef(GetParameter(mystring, 8), -1));
    resultado.Add('target', GetParameter(mystring, 9));
    resultado.Add('solution', GetParameter(mystring, 10));
    resultado.Add('lastblockhash', GetParameter(mystring, 11));
    resultado.Add('nextdifficult', StrToIntDef(GetParameter(mystring, 12), -1));
    resultado.Add('miner', GetParameter(mystring, 13));
    resultado.Add('feespaid', StrToInt64Def(GetParameter(mystring, 14), -1));
    resultado.Add('reward', StrToInt64Def(GetParameter(mystring, 15), -1));
    resultado.Add('hash', GetParameter(mystring, 16));
  end
  else if objecttype = 'pendingorders' then
  begin
    counter := 1;
    ordersarray := TJSONArray.Create;
    while GetParameter(mystring, counter) <> '' do
    begin
      ordersarray.Add(GetParameter(mystring, counter));
      Inc(Counter);
    end;
    resultado.Add('pendings', ordersarray);
  end
  else if objecttype = 'lockedmns' then
  begin
    counter := 1;
    ordersarray := TJSONArray.Create;
    while GetParameter(mystring, counter) <> '' do
    begin
      ordersarray.Add(GetParameter(mystring, counter));
      Inc(Counter);
    end;
    resultado.Add('lockedmns', ordersarray);
  end
  else if objecttype = 'peers' then
  begin
    counter := 1;
    ordersarray := TJSONArray.Create;
    while GetParameter(mystring, counter) <> '' do
    begin
      ordersarray.Add(GetParameter(mystring, counter));
      Inc(Counter);
    end;
    resultado.Add('peers', ordersarray);
  end

  else if objecttype = 'gvtinfo' then
  begin
    resultado.Add('available', StrToIntDef(GetParameter(mystring, 1), 0));
    resultado.Add('buy', StrToInt64Def(GetParameter(mystring, 2), 0));
    resultado.Add('sell', StrToInt64Def(GetParameter(mystring, 3), 0));
  end

  else if objecttype = 'mainnetinfo' then
  begin
    resultado.Add('lastblock', StrToIntDef(GetParameter(mystring, 1), 0));
    resultado.Add('lastblockhash', GetParameter(mystring, 2));
    resultado.Add('headershash', GetParameter(mystring, 3));
    resultado.Add('sumaryhash', GetParameter(mystring, 4));
    resultado.Add('pending', StrToInt(GetParameter(mystring, 5)));
    resultado.Add('supply', StrToInt64Def(GetParameter(mystring, 6), 0));
  end
  else if objecttype = 'blockorder' then
  begin
    resultado.Add('valid', StrToBool(GetParameter(mystring, 1)));
    resultado.Add('block', StrToIntDef(GetParameter(mystring, 2), -1));
    blockorders := StrToIntDef(GetParameter(mystring, 3), 0);
    ordersarray := TJSONArray.Create;
    if blockorders > 0 then
    begin
      for counter := 0 to blockorders - 1 do
      begin
        orderobject := TJSONObject.Create;
        orderobject.Add('orderid', GetParameter(mystring, 4 + (counter * 10)));
        orderobject.Add('timestamp', StrToIntDef(GetParameter(mystring, 5 + (counter * 10)), 0));
        orderobject.Add('block', StrToIntDef(GetParameter(mystring, 6 + (counter * 10)), 0));
        orderobject.Add('type', GetParameter(mystring, 7 + (counter * 10)));
        orderobject.Add('trfrs', StrToIntDef(GetParameter(mystring, 8 + (counter * 10)), 0));
        orderobject.Add('receiver', GetParameter(mystring, 9 + (counter * 10)));
        orderobject.Add('amount', StrToInt64Def(GetParameter(mystring, 10 + (counter * 10)), 0));
        orderobject.Add('fee', StrToIntDef(GetParameter(mystring, 11 + (counter * 10)), 0));
        orderobject.Add('reference', GetParameter(mystring, 12 + (counter * 10)));
        orderobject.Add('sender', GetParameter(mystring, 13 + (counter * 10)));
        ordersarray.Add(orderobject);
      end;
    end;
    resultado.Add('orders', ordersarray);
  end
  else if objecttype = 'blockmns' then
  begin
    resultado.Add('valid', StrToBool(GetParameter(mystring, 1)));
    resultado.Add('block', StrToIntDef(GetParameter(mystring, 2), -1));
    resultado.Add('count', StrToIntDef(GetParameter(mystring, 3), -1));
    resultado.Add('reward', StrToInt64Def(GetParameter(mystring, 4), -1));
    resultado.Add('total', StrToInt64Def(GetParameter(mystring, 5), -1));
    resultado.Add('addresses', GetParameter(mystring, 6));
  end
  else if objecttype = 'getmasternodes' then
  begin
    resultado.Add('block', StrToIntDef(GetParameter(mystring, 1), -1));
    resultado.Add('count', StrToIntDef(GetParameter(mystring, 2), -1));
    resultado.Add('nodes', GetParameter(mystring, 3));
  end
  else if objecttype = 'newaddressfull' then
  begin
    resultado.Add('hash', GetParameter(mystring, 1));
    resultado.Add('public', GetParameter(mystring, 2));
    resultado.Add('private', GetParameter(mystring, 3));
  end
  else if objecttype = 'newaddress' then
  begin
    //resultado.Add('valid',StrToBool(GetParameter(mystring,1)));
    Newaddresses := StrToIntDef(GetParameter(mystring, 2), 1);
    //resultado.Add('number',Newaddresses);
    ordersarray := TJSONArray.Create;
    for counter := 1 to Newaddresses do
    begin
      ordersarray.Add(GetParameter(mystring, 2 + counter));
    end;
    resultado.Add('addresses', ordersarray);
  end
  else if objecttype = 'checkcertificate' then
  begin
    if GetParameter(mystring, 1) = 'True' then
    begin
      resultado.Add('valid', True);
      resultado.Add('address', GetParameter(mystring, 2));
      resultado.Add('signtime', StrToInt64(GetParameter(mystring, 3)));
    end
    else
      resultado.Add('valid', False);
  end
  else if objecttype = 'sendfunds' then
  begin
    if GetParameter(mystring, 1) = 'ERROR' then
    begin
      resultado.Add('valid', False);
      resultado.add('result', StrToIntDef(GetParameter(mystring, 2), -1));
    end
    else
    begin
      resultado.Add('valid', True);
      resultado.Add('result', GetParameter(mystring, 1));
    end;
  end
  else if objecttype = 'islocaladdress' then
  begin
    resultado.Add('result', StrToBool(GetParameter(mystring, 1)));
  end

  else if objecttype = 'setdefault' then
  begin
    resultado.Add('result', StrToBool(GetParameter(mystring, 1)));
  end

  else if objecttype = 'walletbalance' then
  begin
    resultado.Add('balance', StrToInt64(GetParameter(mystring, 1)));
  end
  else if objecttype = 'submitorder' then
  begin
    resultado.Add('result', StrToBool(GetParameter(mystring, 1)));
  end
  else if objecttype = 'submitorderwr' then
  begin
    resultado.Add('result', StrToBool(GetParameter(mystring, 1)));
    resultado.Add('response', StringReplace(GetParameter(mystring, 2), '_',
      ' ', [rfReplaceAll, rfIgnoreCase]));
  end;

  Result := resultado.AsJSON;
  resultado.Free;
end;

// Parses a incoming JSON string
function ParseRPCJSON(jsonreceived: String): String;
var
  jData: TJSONData;
  jObject: TJSONObject;
  method: String;
  params: TJSONArray;
  jsonID: Integer;
  NosoPParams: String = '';
  counter: Integer;
begin
  Result := '';
  if not IsValidJSON(jsonreceived) then Result := GetJSONErrorCode(401, -1)
  else
  begin
    jData := GetJSON(jsonreceived);
    try
      jObject := TJSONObject(jData);
      method := jObject.Strings['method'];
      params := jObject.Arrays['params'];
      jsonid := jObject.Integers['id'];
      for counter := 0 to params.Count - 1 do
        NosoPParams := NosoPParams + ' ' + params[counter].AsString;
      NosoPParams := Trim(NosoPParams);
      //ToLog('console',jsonreceived);
      //ToLog('console','NosoPParams: '+NosoPParams);
      if AnsiContainsStr(RPCBanned, Method) then method := 'banned';
      if method = 'test' then Result := GetJSONResponse('test', jsonid)
      else if method = 'banned' then
        Result := GetJSONResponse(RPC_Banned(NosoPParams), jsonid)
      else if method = 'restart' then
        Result := GetJSONResponse(RPC_Restart(NosoPParams), jsonid)
      else if method = 'getaddressbalance' then
        Result := GetJSONResponse(RPC_AddressBalance(NosoPParams), jsonid)
      else if method = 'getorderinfo' then
        Result := GetJSONResponse(RPC_OrderInfo(NosoPParams), jsonid)
      else if method = 'getblocksinfo' then
        Result := GetJSONResponse(RPC_Blockinfo(NosoPParams), jsonid)
      else if method = 'getmainnetinfo' then
        Result := GetJSONResponse(RPC_Mainnetinfo(NosoPParams), jsonid)
      else if method = 'getpendingorders' then
        Result := GetJSONResponse(RPC_PendingOrders(NosoPParams), jsonid)
      else if method = 'lockedmns' then
        Result := GetJSONResponse(RPC_LockedMNs(NosoPParams), jsonid)
      else if method = 'getpeers' then
        Result := GetJSONResponse(RPC_GetPeers(NosoPParams), jsonid)
      else if method = 'getblockorders' then
        Result := GetJSONResponse(RPC_BlockOrders(NosoPParams), jsonid)
      else if method = 'getblockmns' then
        Result := GetJSONResponse(RPC_BlockMNs(NosoPParams), jsonid)
      else if method = 'getmasternodes' then
        Result := GetJSONResponse(RPC_Masternodes(NosoPParams), jsonid)
      else if method = 'getwalletbalance' then
        Result := GetJSONResponse(RPC_WalletBalance(NosoPParams), jsonid)
      else if method = 'getnewaddress' then
        Result := GetJSONResponse(RPC_NewAddress(NosoPParams), jsonid)
      else if method = 'getnewaddressfull' then
        Result := GetJSONResponse(RPC_NewAddressFull(NosoPParams), jsonid)
      else if method = 'islocaladdress' then
        Result := GetJSONResponse(RPC_ValidateAddress(NosoPParams), jsonid)
      else if method = 'setdefault' then
        Result := GetJSONResponse(RPC_SetDefault(NosoPParams), jsonid)
      else if method = 'getgvtinfo' then
        Result := GetJSONResponse(RPC_GVTInfo(NosoPParams), jsonid)
      else if method = 'sendfunds' then
        Result := GetJSONResponse(RPC_SendFunds(NosoPParams), jsonid)
      else if method = 'checkcertificate' then
        Result := GetJSONResponse(RPC_CheckCertificate(NosoPParams), jsonid)
      else if method = 'submitorder' then
        Result := GetJSONResponse(RPC_SubmitOrder(NosoPParams), jsonid)
      else if method = 'submitorderwr' then
        Result := GetJSONResponse(RPC_SubmitOrder(NosoPParams, True), jsonid)
      else
        Result := GetJSONErrorCode(402, -1);
    except
      on E: Exception do
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'JSON RPC error: ' + E.Message);
    end;
    jData.Free;
  end;
end;

// GET DATA FUNCTIONS

function RPC_Restart(NosoPParams: String): String;
var
  ThDirect: TThreadDirective;
begin
  ThDirect := TThreadDirective.Create(True, 'rpcrestart');
  ThDirect.FreeOnTerminate := True;
  ThDirect.Start;
  Result := 'restart';
end;

function RPC_Banned(NosoPParams: String): String;
begin
  Result := 'banned';
end;

function RPC_AddressBalance(NosoPParams: String): String;
var
  ThisAddress: String;
  counter: Integer = 0;
  Balance, incoming, outgoing: Int64;
  addalias: String = '';
  sumposition: Integer = 0;
  valid: String;
  LRecord: TSummaryData;
begin
  StartPerformanceMeasurement('RPC_AddressBalance');
  Result := '';
  if NosoPParams <> '' then
  begin
    repeat
      ThisAddress := GetParameter(NosoPParams, counter);
      if ThisAddress <> '' then
      begin
        if IsValidHashAddress(ThisAddress) then
          sumposition := GetIndexPosition(ThisAddress, LRecord)
        else
        begin
          sumposition := GetIndexPosition(ThisAddress, LRecord, True);
          ThisAddress := LRecord.Hash;
        end;
        if ThisAddress <> '' then
        begin
          if sumposition < 0 then
          begin
            balance := -1;
            incoming := -1;
            outgoing := -1;
            addalias := 'null';
            valid := 'false';
          end
          else
          begin
            Balance := GetAddressBalanceIndexed(ThisAddress);
            incoming := GetAddressIncomingpays(ThisAddress);
            outgoing := GetAddressPendingPays(ThisAddress);
            addalias := LRecord.CustomAlias;
            if addalias = '' then addalias := 'null';
            valid := 'true';
          end;
          Result := Result + format(
            'balance'#127'%s'#127'%s'#127'%s'#127'%d'#127'%d'#127'%d ',
            [valid, ThisAddress, addalias, balance, incoming, outgoing]);
        end;
      end;
      counter += 1;
    until ThisAddress = '';
    trim(Result);
  end;
  StopPerformanceMeasurement('RPC_AddressBalance');
end;

function RPC_OrderInfo(NosoPParams: String): String;
var
  thisOr: TOrderGroup;
  validID: String = 'true';
begin
  StartPerformanceMeasurement('RPC_OrderInfo');
  ToLog('events', TimeToStr(now) + 'GetOrderDetails requested: ' + NosoPParams);
  NosoPParams := Trim(NosoPParams);
  ThisOr := Default(TOrderGroup);
  if NosoPParams = '' then
  begin
    validID := 'false';
    Result := format('orderinfo'#127'%s'#127'%s'#127 +
      '%d'#127'%d'#127'%s'#127 + '%d'#127'%s'#127'%d'#127 +
      '%d'#127'%s'#127'%s'#127, [validid, NosoPParams,
      thisor.timestamp, thisor.block, thisor.OrderType,
      thisor.OrderLineCount, thisor.Receiver, thisor.AmountTransferred,
      thisor.AmmountFee, thisor.reference, thisor.Sender]);
    exit;
  end;
  thisor := GetOrderDetails(NosoPParams);
  if thisor.OrderID = '' then validID := 'false';
  Result := format('orderinfo'#127'%s'#127'%s'#127 +
    '%d'#127'%d'#127'%s'#127 + '%d'#127'%s'#127'%d'#127 +
    '%d'#127'%s'#127'%s'#127, [validid, NosoPParams,
    thisor.timestamp, thisor.block, thisor.OrderType,
    thisor.OrderLineCount, thisor.Receiver, thisor.AmountTransferred,
    thisor.AmmountFee, thisor.reference, thisor.Sender]);
  StopPerformanceMeasurement('RPC_OrderInfo');
end;

function RPC_Blockinfo(NosoPParams: String): String;
var
  thisblock: String;
  counter: Integer = 0;
begin
  StartPerformanceMeasurement('RPC_Blockinfo');
  Result := '';
  if NosoPParams <> '' then
  begin
    repeat
      thisblock := GetParameter(NosoPParams, counter);
      if thisblock <> '' then
      begin
        if ((StrToIntDef(thisblock, -1) >= 0) and
          (StrToIntDef(thisblock, -1) <= LastBlockIndex)) then
        begin
          Result := Result + 'blockinfo'#127'true'#127 + GetBlockHeaders(
            StrToIntDef(thisblock, -1)) + ' ';
        end
        else
          Result := Result + 'blockinfo'#127'false'#127 + thisblock +
            #127'-1'#127'-1'#127'-1'#127'-1'#127'-1'#127'-1'#127'-1'#127'null'#127'null'#127'null'#127'-1'#127'null'#127'-1'#127'-1'#127'null ';
      end;
      counter += 1;
    until thisblock = '';
    trim(Result);
  end;
  StopPerformanceMeasurement('RPC_Blockinfo');
end;

function RPC_Mainnetinfo(NosoPParams: String): String;
begin
  StartPerformanceMeasurement('RPC_Mainnetinfo');
  Result := format('mainnetinfo'#127'%s'#127'%s'#127'%s'#127'%s'#127'%s'#127'%d',
    [GetConsensus(2), Copy(GetConsensus(10), 0, 5), copy(GetConsensus(15), 0, 5),
    copy(GetConsensus(17), 0, 5), GetConsensus(3), GetCirculatingSupply(
    StrToIntDef(GetConsensus(2), 0))]);
  StopPerformanceMeasurement('RPC_Mainnetinfo');
end;

function RPC_PendingOrders(NosoPParams: String): String;
var
  LData: String;
begin
  StartPerformanceMeasurement('RPC_PendingOrders');
  LData := PendingRawInfo;
  LData := StringReplace(LData, ' ', #127, [rfReplaceAll, rfIgnoreCase]);
  Result := format('pendingorders'#127'%s', [LData]);
  StopPerformanceMeasurement('RPC_PendingOrders');
end;

function RPC_LockedMNs(NosoPParams: String): String;
var
  LData: String;
begin
  StartPerformanceMeasurement('RPC_LockedMNs');
  LData := LockedMNsRawString;
  LData := StringReplace(LData, ' ', #127, [rfReplaceAll, rfIgnoreCase]);
  Result := format('lockedmns'#127'%s', [LData]);
  StopPerformanceMeasurement('RPC_LockedMNs');
end;

function RPC_GetPeers(NosoPParams: String): String;
var
  LData: String;
begin
  StartPerformanceMeasurement('RPC_GetPeers');
  LData := GetConnectedPeers;
  LData := StringReplace(LData, ' ', #127, [rfReplaceAll, rfIgnoreCase]);
  Result := format('peers'#127'%s', [LData]);
  StopPerformanceMeasurement('RPC_GetPeers');
end;

function RPC_BlockOrders(NosoPParams: String): String;
var
  blocknumber: Integer;
  ArraTrxs: TBlockOrders;
  counter: Integer;
  Thisorderinfo: String;
  arrayOrds: array of TOrderGroup;

  procedure AddOrder(order: TOrderData);
  var
    cont: Integer;
    existed: Boolean = False;
  begin
    if length(arrayOrds) > 0 then
    begin
      for cont := 0 to length(arrayOrds) - 1 do
      begin
        if arrayords[cont].OrderID = order.OrderID then
        begin
          arrayords[cont].AmountTransferred := arrayords[cont].AmountTransferred + order.AmountTransferred;
          arrayords[cont].AmmountFee := arrayords[cont].AmmountFee + order.AmountFee;
          arrayords[cont].Sender := arrayords[cont].Sender +
            format('[%s,%d,%d]', [order.Address, order.AmountTransferred, order.AmountFee]);
          arrayords[cont].OrderLineCount += 1;
          existed := True;
          break;
        end;
      end;
    end;
    if not Existed then
    begin
      setlength(arrayords, length(arrayords) + 1);
      arrayords[length(arrayords) - 1].OrderID := order.OrderID;
      arrayords[length(arrayords) - 1].TimeStamp := order.TimeStamp;
      arrayords[length(arrayords) - 1].Block := order.Block;
      arrayords[length(arrayords) - 1].OrderType := order.OrderType;
      arrayords[length(arrayords) - 1].OrderLineCount := 1;
      arrayords[length(arrayords) - 1].Receiver := order.Receiver;
      arrayords[length(arrayords) - 1].AmountTransferred := order.AmountTransferred;
      arrayords[length(arrayords) - 1].AmmountFee := order.AmountFee;
      arrayords[length(arrayords) - 1].Reference := order.Reference;
      if order.OrderLineCount = 1 then
        arrayords[length(arrayords) - 1].Sender := order.Sender
      else
        arrayords[length(arrayords) - 1].Sender := arrayords[length(arrayords) - 1].Sender +
          format('[%s,%d,%d]', [order.Address, order.AmountTransferred, order.AmountFee]);
    end;
  end;

begin
  StartPerformanceMeasurement('RPC_BlockOrders');
  Result := '';
  setlength(arrayOrds, 0);
  blocknumber := StrToIntDef(NosoPParams, -1);
  if ((blocknumber < 0) or (blocknumber > LastBlockIndex)) then
    Result := 'blockorder'#127'false'#127 + NosoPParams + #127'0'
  else
  begin
    ArraTrxs := GetBlockTransfers(BlockNumber);
    Result := 'blockorder'#127'true'#127 + NosoPParams + #127;
    if length(ArraTrxs) > 0 then
    begin
      for counter := 0 to length(ArraTrxs) - 1 do
      begin

        AddOrder(ArraTrxs[counter]);
      end;
      Result := Result + IntToStr(length(arrayOrds)) + #127;
      for counter := 0 to length(arrayOrds) - 1 do
      begin
        thisorderinfo := format(
          '%s'#127'%d'#127'%d'#127'%s'#127'%d'#127'%s'#127'%d'#127'%d'#127'%s'#127'%s'#127,
          [arrayOrds[counter].OrderID, arrayOrds[counter].TimeStamp,
          arrayOrds[counter].Block, arrayOrds[counter].OrderType,
          arrayOrds[counter].OrderLineCount, arrayOrds[counter].Receiver,
          arrayOrds[counter].AmountTransferred, arrayOrds[counter].AmmountFee,
          arrayOrds[counter].Reference, arrayOrds[counter].Sender]);
        Result := Result + thisorderinfo;
      end;
    end
    else
      Result := Result + '0'#127;
    trim(Result);
  end;
  StopPerformanceMeasurement('RPC_BlockOrders');
end;

function RPC_Masternodes(NosoPParams: String): String;
var
  Source: String;
  counter: Integer = 1;
  Block: String;
  ThisData: String;
  Nodes: String = '';
  Total: Integer = 0;
  IpAndport, Ip, port, address, age: String;
begin
  StartPerformanceMeasurement('RPC_Masternodes');
  Result := '';
  Source := GetMN_FileText;
  Block := GetParameter(Source, 0);
  repeat
    ThisData := GetParameter(Source, counter);
    if thisData <> '' then
    begin
      ThisData := StringReplace(ThisData, ':', ' ', [rfReplaceAll]);
      ipandport := GetParameter(ThisData, 0);
      ipandport := StringReplace(ipandport, ';', ' ', [rfReplaceAll]);
      ip := GetParameter(ipandport, 0);
      port := GetParameter(ipandport, 1);
      address := GetParameter(ThisData, 1);
      age := GetParameter(ThisData, 2);
      nodes := nodes + format('[%s,%s,%s,%s]', [ip, port, address, age]);
      Inc(Total);
    end;
    Inc(counter);
  until thisdata = '';
  Result := 'getmasternodes'#127 + Block + #127 + IntToStr(Total) + #127 + Nodes;
  //Tolog('console',result);
  StopPerformanceMeasurement('RPC_Masternodes');
end;

function RPC_Blockmns(NosoPParams: String): String;
var
  blocknumber: Integer;
  ArrayMNs: BlockArraysPos;
  MNsReward: Int64;
  MNsCount, Totalpaid: Int64;
  counter: Integer;
  AddressesString: String = '';
begin
  StartPerformanceMeasurement('RPC_Blockmns');
  Result := '';
  blocknumber := StrToIntDef(NosoPParams, -1);
  if ((blocknumber < 48010) or (blocknumber > LastBlockIndex)) then
    Result := 'blockmns'#127'false'#127 + NosoPParams + #127'0'#127'0'#127'0'
  else
  begin
    ArrayMNs := GetBlockMNs(blocknumber);
    MNsReward := StrToInt64Def(ArrayMNs[length(ArrayMNs) - 1].address, 0);
    SetLength(ArrayMNs, length(ArrayMNs) - 1);
    MNSCount := length(ArrayMNs);
    TotalPAid := MNSCount * MNsReward;
    for counter := 0 to MNsCount - 1 do
      AddressesString := AddressesString + ArrayMNs[counter].address + ' ';
    AddressesString := Trim(AddressesString);
    AddressesString := StringReplace(AddressesString, ' ', ',', [rfReplaceAll,
      rfIgnoreCase]);
    Result := 'blockmns'#127'true'#127 + blocknumber.ToString + #127 + MNSCount.ToString + #127 +
      MNsReward.ToString + #127 + TotalPAid.ToString + #127 + AddressesString;
  end;
  StopPerformanceMeasurement('RPC_Blockmns');
end;

function RPC_WalletBalance(NosoPParams: String): String;
var
  LData: Int64;
begin
  StartPerformanceMeasurement('RPC_WalletBalance');
  LData := GetWalletBalance;
  Result := format('walletbalance'#127'%d', [LData]);
  StopPerformanceMeasurement('RPC_WalletBalance');
end;

function RPC_NewAddress(NosoPParams: String): String;
var
  TotalNumber: Integer;
  counter: Integer;
  NewAddress: WalletData;
  PubKey, PriKey: String;
begin
  StartPerformanceMeasurement('RPC_NewAddress');
  TotalNumber := StrToIntDef(NosoPParams, 1);
  if TotalNumber > 100 then TotalNumber := 100;
  Result := 'newaddress'#127'true'#127 + IntToStr(TotalNumber) + #127;
  for counter := 1 to totalnumber do
  begin
    NewAddress := Default(WalletData);
    NewAddress.Hash := GenerateNewAddress(PubKey, PriKey);
    NewAddress.PublicKey := pubkey;
    NewAddress.PrivateKey := PriKey;
    InsertToWallArr(NewAddress);
    if RPCSaveNew then SaveAddresstoFile(RPCBakDirectory + NewAddress.Hash +
        '.pkw', NewAddress);
    Result := Result + NewAddress.Hash + #127;
  end;
  trim(Result);
  S_Wallet := True;
  U_DirPanel := True;
  StopPerformanceMeasurement('RPC_NewAddress');
end;

function RPC_NewAddressFull(NosoPParams: String): String;
var
  counter: Integer;
  NewAddress: WalletData;
  PubKey, PriKey: String;
begin
  StartPerformanceMeasurement('RPC_NewAddressFull');
  Result := 'newaddressfull'#127;
  NewAddress := Default(WalletData);
  NewAddress.Hash := GenerateNewAddress(PubKey, PriKey);
  NewAddress.PublicKey := pubkey;
  NewAddress.PrivateKey := PriKey;
  InsertToWallArr(NewAddress);
  if RPCSaveNew then SaveAddresstoFile(RPCBakDirectory + NewAddress.Hash +
      '.pkw', NewAddress);
  Result := Result + NewAddress.Hash + #127 + NewAddress.PublicKey + #127 + NewAddress.PrivateKey;
  trim(Result);
  S_Wallet := True;
  U_DirPanel := True;
  StopPerformanceMeasurement('RPC_NewAddressFull');
end;

function RPC_ValidateAddress(NosoPParams: String): String;
begin
  StartPerformanceMeasurement('RPC_NewAddressFull');
  if VerifyAddressOnDisk(GetParameter(NosoPParams, 0)) then
    Result := 'islocaladdress'#127'True'
  else
    Result := 'islocaladdress'#127'False';
  StopPerformanceMeasurement('RPC_NewAddressFull');
end;

function RPC_SetDefault(NosoPParams: String): String;
var
  address: String;
begin
  StartPerformanceMeasurement('RPC_SetDefault');
  address := GetParameter(NosoPParams, 0);
  if SetDefaultAddress('SETDEFAULT ' + Address) then Result := 'setdefault'#127'True'
  else
    Result := 'setdefault'#127'False';
  StopPerformanceMeasurement('RPC_SetDefault');
end;

function RPC_GVTInfo(NosoPParams: String): String;
var
  available: Int64;
begin
  StartPerformanceMeasurement('RPC_GVTInfo');
  available := CountAvailableGVTs;
  Result := 'gvtinfo'#127 + IntToStr(available) + #127 + IntToStr(
    CalculateGVTPrice(Available)) + #127 + IntToStr(CalculateGVTPrice(Available, True));
  StopPerformanceMeasurement('RPC_GVTInfo');
end;

function RPC_CheckCertificate(NosoPParams: String): String;
var
  cert: String;
  SignTime: String;
  Address: String;
begin
  StartPerformanceMeasurement('RPC_CheckCertificate');
  Result := 'checkcertificate'#127;
  cert := GetParameter(NosoPParams, 0);
  Address := CheckCertificate(cert, SignTime);
  if Address <> '' then
  begin
    Result := Result + 'True'#127 + Address + #127 + SignTime;
  end
  else
  begin
    Result := Result + 'False';
  end;
  StopPerformanceMeasurement('RPC_CheckCertificate');
end;

function RPC_SubmitOrder(NosoPParams: String; waitresponse: Boolean = False): String;
var
  ResultLine: String;
begin
  StartPerformanceMeasurement('RPC_SubmitOrder');
  //ToLog('Console',NosoPParams);
  ResultLine := SendOrderToNode(NosoPParams);
  ResultLine := StringReplace(ResultLine, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
  if not waitresponse then Result := 'submitorder'#127'True'
  else
  begin
    Result := 'submitorderwr'#127'True'#127 + ResultLine;
  end;
  StopPerformanceMeasurement('RPC_SubmitOrder');
end;

function RPC_SendFunds(NosoPParams: String): String;
var
  destination, reference: String;
  amount: Int64;
  resultado: String;
  ErrorCode: Integer;
begin
  StartPerformanceMeasurement('RPC_SendFunds');
  destination := GetParameter(NosoPParams, 0);
  amount := StrToInt64Def(GetParameter(NosoPParams, 1), 0);
  reference := GetParameter(NosoPParams, 2);
  if reference = '' then reference := 'null';
  //ToLog('console','Send to '+destination+' '+IntToCurrency(amount)+' with reference: '+reference);
  Resultado := SendFunds('sendto ' + destination + ' ' + IntToStr(amount) + ' ' + Reference);
  if ((Resultado <> '') and (GetParameter(Resultado, 0) <> 'ERROR') and
    (copy(resultado, 0, 2) = 'OR')) then
  begin
    Result := 'sendfunds'#127 + resultado;
  end
  else if (GetParameter(Resultado, 0) = 'ERROR') then
  begin
    ErrorCode := StrToIntDef(GetParameter(Resultado, 1), 0);
    Result := 'sendfunds'#127 + 'ERROR'#127 + IntToStr(ErrorCode);
  end
  else
  begin
    Result := 'sendfunds'#127 + 'ERROR'#127 + '999';
  end;
  StopPerformanceMeasurement('RPC_SendFunds');
end;


end.  // END UNIT
