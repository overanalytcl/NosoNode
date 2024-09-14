unit mpProtocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mpRed, MasterPaskalForm, mpParser, StrUtils,
  mpDisk, nosotime, mpBlock,
  Zipper, mpcoin, nosodebug, nosogeneral, nosocrypto, nosounit, nosoconsensus, nosopsos,
  nosoheaders, NosoNosoCFG, nosoblock, nosonetwork, nosogvts, nosoMasternodes,
  NosoIPControl;

//function GetPTCEcn():String;
function GetOrderFromString(textLine: String; out ThisData: TOrderData): Boolean;
//function GetStringFromOrder(order:Torderdata):String;
function GetStringFromBlockHeader(blockheader: BlockHeaderdata): String;
//Function ProtocolLine(tipo:integer):String;
procedure ParseProtocolLines();
//function IsValidProtocol(line:String):Boolean;
procedure PTC_SendLine(Slot: Int64; Message: String);
 //Procedure ClearOutTextToSlot(slot:integer);
 //Function GetTextToSlot(slot:integer):string;
 //Procedure ProcessPing(LineaDeTexto: string; Slot: integer; Responder:boolean);
function GetPingString(): String;
//procedure PTC_SendPending(Slot:int64);
procedure PTC_SendResumen(Slot: Int64);
procedure PTC_SendSumary(Slot: Int64);
procedure PTC_SendPSOS(Slot: Int64);
procedure PTC_SendGVTs(Slot: Int64);
function ZipHeaders(): Boolean;
function CreateZipBlockfile(firstblock: Integer): String;
procedure PTC_SendBlocks(Slot: Integer; TextLine: String);
procedure INC_PTC_Custom(TextLine: String; connection: Integer);
function PTC_Custom(TextLine: String): Integer;
function ValidateTrfr(order: Torderdata; Origen: String): Integer;
function IsAddressLocked(LAddress: String): Boolean;
function IsOrderIDAlreadyProcessed(OrderText: String): Boolean;
procedure INC_PTC_Order(TextLine: String; connection: Integer);
function PTC_Order(TextLine: String): String;
procedure INC_PTC_SendGVT(TextLine: String; connection: Integer);
function PTC_SendGVT(TextLine: String): Integer;
procedure PTC_AdminMSG(TextLine: String);
procedure PTC_CFGData(Linea: String);
//Procedure PTC_SendMNsList(slot:integer);
procedure PTC_SendUpdateHeaders(Slot: Integer; Linea: String);
procedure PTC_ProcessMNFileIncoming(LText: String);
procedure PTC_HeadUpdate(linea: String);

procedure AddCriptoOp(tipo: Integer; proceso, resultado: String);
procedure DeleteCriptoOp();


const
  OnlyHeaders = 0;
  Getnodes = 1;
  Nodes = 2;
  Ping = 3;
  Pong = 4;
  GetPending = 5;
  GetSumary = 6;
  GetResumen = 7;
  LastBlock = 8;
  Custom = 9;
  //NodeReport = 10;
  GetMNs = 11;
  BestHash = 12;
  MNReport = 13;
  MNCheck = 14;
  GetChecks = 15;
  GetMNsFile = 16;
  MNFile = 17;
  GetHeadUpdate = 18;
  HeadUpdate = 19;

  GetGVTs = 20;
  GetCFG = 30;
  SETCFG = 31;
  GetPSOs = 32;

implementation

uses
  mpGui;

{
// Devuelve el puro encabezado con espacio en blanco al final
function GetPTCEcn():String;
Begin
result := 'PSK '+IntToStr(protocolo)+' '+MainnetVersion+NodeRelease+' '+UTCTimeStr+' ';
End;
}

// convierte los datos de la cadena en una order
function GetOrderFromString(textLine: String; out ThisData: TOrderData): Boolean;
var
  orderinfo: TOrderData;
begin
  BeginPerformance('GetOrderFromString');
  Result := True;
  ThisData := Default(TOrderData);
  try
    ThisData.OrderID := Parameter(textline, 1);
    ThisData.OrderLines := StrToInt(Parameter(textline, 2));
    ThisData.OrderType := Parameter(textline, 3);
    ThisData.TimeStamp := StrToInt64(Parameter(textline, 4));
    ThisData.reference := Parameter(textline, 5);
    ThisData.TrxLine := StrToInt(Parameter(textline, 6));
    ThisData.Sender := Parameter(textline, 7);
    ThisData.Address := Parameter(textline, 8);
    ThisData.Receiver := Parameter(textline, 9);
    ThisData.AmmountFee := StrToInt64(Parameter(textline, 10));
    ThisData.AmmountTrf := StrToInt64(Parameter(textline, 11));
    ThisData.Signature := Parameter(textline, 12);
    ThisData.TrfrID := Parameter(textline, 13);
  except
    ON E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error GetOrderFromString : ' + E.Message);
      Result := False;
    end;
  end;{TRY}
  EndPerformance('GetOrderFromString');
end;

{
// Convierte una orden en una cadena para compartir
function GetStringFromOrder(order:Torderdata):String;
Begin
result:= Order.OrderType+' '+
         Order.OrderID+' '+
         IntToStr(order.OrderLines)+' '+
         order.OrderType+' '+
         IntToStr(Order.TimeStamp)+' '+
         Order.reference+' '+
         IntToStr(order.TrxLine)+' '+
         order.sender+' '+
         Order.Address+' '+
         Order.Receiver+' '+
         IntToStr(Order.AmmountFee)+' '+
         IntToStr(Order.AmmountTrf)+' '+
         Order.Signature+' '+
         Order.TrfrID;
End;
}

// devuelve una cadena con los datos de la cabecera de un bloque
function GetStringFromBlockHeader(BlockHeader: blockheaderdata): String;
begin
  Result := 'Number:' + IntToStr(BlockHeader.Number) + ' ' +
    'Start:' + IntToStr(BlockHeader.TimeStart) + ' ' +
    'End:' + IntToStr(BlockHeader.TimeEnd) + ' ' +
    'Total:' + IntToStr(BlockHeader.TimeTotal) + ' ' +
    '20:' + IntToStr(BlockHeader.TimeLast20) + ' ' +
    'Trxs:' + IntToStr(BlockHeader.TrxTotales) + ' ' +
    'Diff:' + IntToStr(BlockHeader.Difficult) + ' ' +
    'Target:' + BlockHeader.TargetHash + ' ' +
    'Solution:' + BlockHeader.Solution + ' ' +
    'NextDiff:' + IntToStr(BlockHeader.NxtBlkDiff) + ' ' +
    'Miner:' + BlockHeader.AccountMiner + ' ' +
    'Fee:' + IntToStr(BlockHeader.MinerFee) + ' ' +
    'Reward:' + IntToStr(BlockHeader.Reward);

end;

{
//Devuelve la linea de protocolo solicitada
Function ProtocolLine(tipo:integer):String;
var
  Specific      : String = '';
  Header        : String = '';
Begin
  Header := 'PSK '+IntToStr(protocolo)+' '+MainnetVersion+NodeRelease+' '+UTCTimeStr+' ';
  if tipo = 0 then Specific := '';                                 //OnlyHeaders
  if tipo = 3 then Specific := '$PING '+GetPingString;             //Ping
  if tipo = 4 then Specific := '$PONG '+GetPingString;             //Pong
  if tipo = 5 then Specific := '$GETPENDING';                      //GetPending
  if tipo = 6 then Specific := '$GETSUMARY';                       //GetSumary
  if tipo = 7 then Specific := '$GETRESUMEN';                      //GetResumen
  if tipo = 8 then Specific := '$LASTBLOCK '+IntToStr(mylastblock) //LastBlock
  if tipo = 9 then Resultado := '$CUSTOM ';                        //Custom
  if tipo = 11 then Specific := '$GETMNS';                         //GetMNs
  if tipo = 12 then Specific := '$BESTHASH';                       //BestHash
  if tipo = 13 then Specific := '$MNREPO '+GetMNReportString;      //MNReport
  if tipo = 14 then Specific := '$MNCHECK ';                       //MNCheck
  if Tipo = 15 then Specific := '$GETCHECKS';                      //GetChecks
  if tipo = 16 then Specific := 'GETMNSFILE';                      //GetMNsFile
  if tipo = 17 then Specific := 'MNFILE';                              //MNFile
  if tipo = 18 then Specific := 'GETHEADUPDATE '+MyLastBlock.ToString; //GetHeadUpdate
  if tipo = 19 then Specific := 'HEADUPDATE';                      //HeadUpdate
  if tipo = 20 then Specific := '$GETGVTS';                        //GetGVTs
  if tipo = 21 then Specific := '$SNDGVT ';
  if tipo = 30 then Specific := 'GETCFGDATA';                      //GetCFG
  if tipo = 31 then Specific := 'SETCFGDATA $';                    //SETCFG
  if tipo = 32 then Specific := '$GETPSOS';                        //GetPSOs
Result := Header+Specific;
End;
}
// Procesa todas las lineas procedentes de las conexiones
procedure ParseProtocolLines();
var
  contador: Integer = 0;
  UsedProtocol: Integer = 0;
  UsedVersion: String = '';
  PeerTime: String = '';
  Linecomando: String = '';
  ProcessLine: String;
  ValidMNCheck: String;
  OrderError: Boolean = False;
begin
  for contador := 1 to MaxConecciones do
  begin
    while LengthIncoming(contador) > 0 do
    begin
      ProcessLine := GetIncoming(contador);
      UsedProtocol := StrToIntDef(Parameter(ProcessLine, 1), 1);
      UsedVersion := Parameter(ProcessLine, 2);
      PeerTime := Parameter(ProcessLine, 3);
      LineComando := Parameter(ProcessLine, 4);
      if ((not IsValidProtocol(ProcessLine)) and
        (not GetConexIndex(contador).Autentic)) then
        // La linea no es valida y proviene de una conexion no autentificada
      begin
        ToLog('console', 'CONNECTION REJECTED: INVALID PROTOCOL -> ' +
          GetConexIndex(contador).ip + '->' + ProcessLine); //CONNECTION REJECTED: INVALID PROTOCOL ->
        UpdateBotData(GetConexIndex(contador).ip);
        CloseSlot(contador);
      end
      else if UpperCase(LineComando) = 'DUPLICATED' then
      begin
        ToLog('Console', 'You are already connected to ' + GetConexIndex(contador).ip);
        //CONNECTION REJECTED: INVALID PROTOCOL ->
        CloseSlot(contador);
      end
      else if UpperCase(LineComando) = 'OLDVERSION' then
      begin
        ToLog('Console', 'You need update your node to connect to ' +
          GetConexIndex(contador).ip); //CONNECTION REJECTED: INVALID PROTOCOL ->
        CloseSlot(contador);
      end
      else if UpperCase(LineComando) = '$PING' then
        ProcessPing(ProcessLine, contador, True)                        // Done
      else if UpperCase(LineComando) = '$PONG' then
        ProcessPing(ProcessLine, contador, False)                       // Done
      else if ((blockage < 10) or (BlockAge > 585)) then
      begin
        continue;
      end
      else if UpperCase(LineComando) = '$GETPENDING' then
        SendPendingsToPeer(contador)//PTC_SendPending(contador) // Done
      else if UpperCase(LineComando) = '$GETMNS' then
        SendMNsListToPeer(contador) //SendMNsList(contador)         // Done
      else if UpperCase(LineComando) = '$GETRESUMEN' then PTC_SendResumen(contador)
      else if UpperCase(LineComando) = '$LASTBLOCK' then
        PTC_SendBlocks(contador, ProcessLine)
      else if UpperCase(LineComando) = '$CUSTOM' then
      begin
        if addipcontrol(GetConexIndex(contador).ip) < 100 then
          INC_PTC_Custom(GetOpData(ProcessLine), contador)
        else
        begin
          UpdateBotData(GetConexIndex(contador).ip);
          ToLog('console', 'IP spammer: ' + GetConexIndex(contador).ip);
          CloseSlot(contador);
        end;
      end
      else if UpperCase(LineComando) = 'ORDER' then
      begin
        if addipcontrol(GetConexIndex(contador).ip) < 100 then
          INC_PTC_Order(ProcessLine, contador)
        else
        begin
          UpdateBotData(GetConexIndex(contador).ip);
          ToLog('console', 'IP spammer: ' + GetConexIndex(contador).ip);
          CloseSlot(contador);
        end;
      end
      else if UpperCase(LineComando) = 'ADMINMSG' then PTC_AdminMSG(ProcessLine)
      else if UpperCase(LineComando) = '$MNREPO' then AddWaitingMNs(ProcessLine)
      else if UpperCase(LineComando) = '$MNCHECK' then
      begin
        ValidMNCheck := ValidateMNCheck(ProcessLine);
        if ValidMNCheck <> '' then outGOingMsjsAdd(GetPTCEcn + ValidMNCheck);
        //PTC_MNCheck(ProcessLine)
      end
      else if UpperCase(LineComando) = '$GETCHECKS' then SendMNChecksToPeer(contador)
      else if UpperCase(LineComando) = 'GETMNSFILE' then
        PTC_SendLine(contador, ProtocolLine(MNFILE) + ' $' + LoadMNsFile)
      else if UpperCase(LineComando) = 'GETCFGDATA' then
        PTC_SendLine(contador, ProtocolLine(SETCFG) + GetCFGDataStr)

      else if UpperCase(LineComando) = 'MNFILE' then
        PTC_ProcessMNFileIncoming(ProcessLine)
      //SaveMNsFile(ExtractMNsText(ProcessLine))//PTC_MNFile(ProcessLine)
      else if UpperCase(LineComando) = 'SETCFGDATA' then PTC_CFGData(ProcessLine)

      else if UpperCase(LineComando) = 'GETHEADUPDATE' then
        PTC_SendUpdateHeaders(contador, ProcessLine)
      else if UpperCase(LineComando) = 'HEADUPDATE' then PTC_HeadUpdate(ProcessLine)
      else if UpperCase(LineComando) = '$GETSUMARY' then PTC_SendSumary(contador)
      //else if UpperCase(LineComando) = '$SNDGVT' then INC_PTC_SendGVT(GetOpData(ProcessLine), contador)
      else if UpperCase(LineComando) = '$GETPSOS' then PTC_SendPSOS(contador)
      else if UpperCase(LineComando) = '$GETGVTS' then PTC_SendGVTs(contador)

      else
      begin  // El comando recibido no se reconoce. Verificar protocolos posteriores.
        ToLog('Console', 'Unknown command () in slot: (' + ProcessLine +
          ') ' + IntToStr(contador)); //Unknown command () in slot: (
      end;
    end;
  end;
end;

{
// Verifica si una linea recibida en una conexion es una linea valida de protocolo
function IsValidProtocol(line:String):Boolean;
Begin
  if copy(line,1,4) = 'PSK ' then result := true
  else result := false;
End;
}

// Envia una linea a un determinado slot
procedure PTC_SendLine(Slot: Int64; Message: String);
begin
  if ((slot >= 1) and (Slot <= MaxConecciones)) then
  begin
    if ((GetConexIndex(Slot).tipo = 'CLI') and (not GetConexIndex(Slot).IsBusy)) then
    begin
      TextToSlot(slot, message);
      {
      EnterCriticalSection(CSOutGoingArr[slot]);
      Insert(Message,ArrayOutgoing[slot],length(ArrayOutgoing[slot]));
      LeaveCriticalSection(CSOutGoingArr[slot]);
      }
    end;
    if ((GetConexIndex(Slot).tipo = 'SER') and (not GetConexIndex(Slot).IsBusy)) then
    begin
      TextToSlot(slot, message);
      {
      EnterCriticalSection(CSOutGoingArr[slot]);
      Insert(Message,ArrayOutgoing[slot],length(ArrayOutgoing[slot]));
      LeaveCriticalSection(CSOutGoingArr[slot]);
      }
      {
      TRY
      CanalCliente[Slot].IOHandler.WriteLn(Message);
      EXCEPT On E :Exception do
        begin
        ToLog('Console',E.Message);
        ToLog('exceps',FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now)+' -> '+'Error sending line: '+E.Message);
        CloseSlot(Slot);
        end;
      END;{TRY}
      }
    end;
  end
  else
    ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) + ' -> ' +
      'Invalid PTC_SendLine slot: ' + IntToStr(slot));
end;

{
Procedure ClearOutTextToSlot(slot:integer);
Begin
EnterCriticalSection(CSOutGoingArr[slot]);
SetLength(ArrayOutgoing[slot],0);
LeaveCriticalSection(CSOutGoingArr[slot]);
End;

Function GetTextToSlot(slot:integer):string;
Begin
result := '';
if ( (Slot>=1) and (slot<=MaxConecciones) ) then
   begin
   EnterCriticalSection(CSOutGoingArr[slot]);
   if length(ArrayOutgoing[slot])>0 then
      begin
      result:= ArrayOutgoing[slot][0];
      Delete(ArrayOutgoing[slot],0,1);
      end;
   LeaveCriticalSection(CSOutGoingArr[slot]);
   end;
End;
}
{
// Procesa un ping recibido y envia el PONG si corresponde.
Procedure ProcessPing(LineaDeTexto: string; Slot: integer; Responder:boolean);
var
  NewData : Tconectiondata;
Begin
  NewData               := GetConexIndex(Slot);
  NewData.Autentic      := true;
  NewData.Protocol      := StrToIntDef(Parameter(LineaDeTexto,1),0);
  NewData.Version       := Parameter(LineaDeTexto,2);
  NewData.offset        := StrToInt64Def(Parameter(LineaDeTexto,3),UTCTime)-UTCTime;
  NewData.Connections   := StrToIntDef(Parameter(LineaDeTexto,5),0);
  NewData.Lastblock     := Parameter(LineaDeTexto,6);
  NewData.LastblockHash := Parameter(LineaDeTexto,7);
  NewData.SumarioHash   := Parameter(LineaDeTexto,8);
  NewData.Pending       := StrToIntDef(Parameter(LineaDeTexto,9),0);
  NewData.ResumenHash   := Parameter(LineaDeTexto,10);
  NewData.ConexStatus   := StrToIntDef(Parameter(LineaDeTexto,11),0);
  NewData.ListeningPort := StrToIntDef(Parameter(LineaDeTexto,12),-1);
  NewData.MNsHash       := Parameter(LineaDeTexto,13);
  NewData.MNsCount      := StrToIntDef(Parameter(LineaDeTexto,14),0);
  NewData.BestHashDiff  := 'null'{15};
  NewData.MNChecksCount := StrToIntDef(Parameter(LineaDeTexto,16),0);
  NewData.lastping      := UTCTimeStr;
  NewData.GVTsHash      := Parameter(LineaDeTexto,17);
  NewData.CFGHash       := Parameter(LineaDeTexto,18);
  NewData.PSOHash       := Parameter(LineaDeTexto,19);;
  NewData.MerkleHash    := HashMD5String(NewData.Lastblock+copy(NewData.ResumenHash,0,5)+copy(NewData.MNsHash,0,5)+
                                copy(NewData.LastblockHash,0,5)+copy(NewData.SumarioHash,0,5)+
                                copy(NewData.GVTsHash,0,5)+copy(NewData.CFGHash,0,5));
  SetConexIndex(Slot,NewData);
  if responder then
    begin
    PTC_SendLine(slot,ProtocolLine(4));
    Inc(G_TotalPings);
    end;
End;
}
// Devuelve la informacion contenida en un ping
function GetPingString(): String;
var
  Port: Integer = 0;
begin
  if Form1.Server.Active then port := Form1.Server.DefaultPort
  else
    port := -1;
  Result := IntToStr(GetTotalConexiones()) + ' ' + IntToStr(MyLastBlock) + ' ' +
    MyLastBlockHash + ' ' + MySumarioHash + ' ' +
    GetPendingCount.ToString + ' ' + GetResumenHash + ' ' +
    IntToStr(MyConStatus) + ' ' + IntToStr(port) + ' ' +
    copy(GetMNsHash, 0, 5) + ' ' + IntToStr(GetMNsListLength) + ' ' +
    'null' + ' ' + //GetNMSData.Diff
    GetMNsChecksCount.ToString + ' ' + MyGVTsHash + ' ' +
    Copy(HashMD5String(GetCFGDataStr), 0, 5) + ' ' + Copy(PSOFileHash, 0, 5);
end;

{
// Envia las TXs pendientes al slot indicado
procedure PTC_SendPending(Slot:int64);
var
  contador : integer;
  Encab : string;
  Textline : String;
  TextOrder : String;
  CopyArrayPoolTXs : Array of TOrderData;
Begin
Encab := GetPTCEcn;
TextOrder := encab+'ORDER ';
if GetPendingCount > 0 then
   begin
   EnterCriticalSection(CSPending);
   SetLength(CopyArrayPoolTXs,0);
   CopyArrayPoolTXs := copy(ArrayPoolTXs,0,length(ArrayPoolTXs));
   LeaveCriticalSection(CSPending);
   for contador := 0 to Length(CopyArrayPoolTXs)-1 do
      begin
      Textline := GetStringFromOrder(CopyArrayPoolTXs[contador]);
      if (CopyArrayPoolTXs[contador].OrderType='CUSTOM') then
         begin
         PTC_SendLine(slot,Encab+'$'+TextLine);
         end;
      if (CopyArrayPoolTXs[contador].OrderType='TRFR') then
         begin
         if CopyArrayPoolTXs[contador].TrxLine=1 then TextOrder:= TextOrder+IntToStr(CopyArrayPoolTXs[contador].OrderLines)+' ';
         TextOrder := TextOrder+'$'+GetStringfromOrder(CopyArrayPoolTXs[contador])+' ';
         if CopyArrayPoolTXs[contador].OrderLines=CopyArrayPoolTXs[contador].TrxLine then
            begin
            Setlength(TextOrder,length(TextOrder)-1);
            PTC_SendLine(slot,TextOrder);
            TextOrder := encab+'ORDER ';
            end;
         end;
      if (CopyArrayPoolTXs[contador].OrderType='SNDGVT') then
         begin
         PTC_SendLine(slot,Encab+'$'+TextLine);
         end;
      end;
   SetLength(CopyArrayPoolTXs,0);
   end;
End;
}

// Send headers file to peer
procedure PTC_SendResumen(Slot: Int64);
const
  LastRequest: Int64 = 0;
var
  MemStream: TMemoryStream;
begin
  if UTCTime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  MemStream := TMemoryStream.Create;
  GetHeadersAsMemStream(MemStream);
  if GetConexIndex(slot).tipo = 'CLI' then
  begin
    try
      GetConexIndex(slot).context.Connection.IOHandler.WriteLn('RESUMENFILE');
      GetConexIndex(slot).context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'SERVER: Error sending headers file (' + E.Message + ')');
      end;
    end; {TRY}
  end;
  if GetConexIndex(slot).tipo = 'SER' then
  begin
    try
      CanalCliente[slot].IOHandler.WriteLn('RESUMENFILE');
      CanalCliente[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'CLIENT: Error sending Headers file (' + E.Message + ')');
        CloseSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

procedure PTC_SendSumary(Slot: Int64);
const
  LastRequest: Int64 = 0;
var
  MemStream: TMemoryStream;
begin
  if UTCtime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  MemStream := TMemoryStream.Create;
  GetSummaryAsMemStream(MemStream);
  if GetConexIndex(slot).tipo = 'CLI' then
  begin
    try
      GetConexIndex(slot).context.Connection.IOHandler.WriteLn('SUMARYFILE');
      GetConexIndex(slot).context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'SERVER: Error sending sumary file (' + E.Message + ')');
      end;
    end; {TRY}
  end;
  if GetConexIndex(slot).tipo = 'SER' then
  begin
    try
      CanalCliente[slot].IOHandler.WriteLn('SUMARYFILE');
      CanalCliente[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'CLIENT: Error sending Sumary file (' + E.Message + ')');
        CloseSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

procedure PTC_SendPSOS(Slot: Int64);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  GetPSOsAsMemStream(MemStream);
  if GetConexIndex(slot).tipo = 'CLI' then
  begin
    try
      GetConexIndex(slot).context.Connection.IOHandler.WriteLn('PSOSFILE');
      GetConexIndex(slot).context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'SERVER: Error sending PSOs file (' + E.Message + ')');
      end;
    end; {TRY}
  end;
  if GetConexIndex(slot).tipo = 'SER' then
  begin
    try
      CanalCliente[slot].IOHandler.WriteLn('PSOSFILE');
      CanalCliente[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
          Now) + ' -> ' + 'CLIENT: Error sending PSOs file (' + E.Message + ')');
        CloseSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

procedure PTC_SendGVTs(Slot: Int64);
var
  MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  GetGVTsAsStream(MemStream);
  if GetConexIndex(slot).tipo = 'CLI' then
  begin
    try
      GetConexIndex(slot).context.Connection.IOHandler.WriteLn('GVTSFILE');
      GetConexIndex(slot).context.connection.IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
        ToDeepDeb('Error sending GVTs file: ' + E.Message);
      end;
    end; {TRY}
  end;
  if GetConexIndex(slot).tipo = 'SER' then
  begin
    try
      CanalCliente[slot].IOHandler.WriteLn('GVTSFILE');
      CanalCliente[slot].IOHandler.Write(MemStream, 0, True);
    except
      on E: Exception do
      begin
        ToDeepDeb('Error sending GVTs file: ' + E.Message);
        CloseSlot(slot);
      end;
    end;{TRY}
  end;
  MemStream.Free;
end;

// Zips the headers file. Uses deprecated methods, to be removed...
function ZipHeaders(): Boolean;
var
  MyZipFile: TZipper;
  archivename: String;
begin
  Result := False;
  MyZipFile := TZipper.Create;
  MyZipFile.FileName := ZipHeadersFileName;
  try
    {$IFDEF WINDOWS}
   archivename:= StringReplace(ResumenFilename,'\','/',[rfReplaceAll]);
    {$ENDIF}
    {$IFDEF UNIX}
   archivename:= ResumenFilename;
    {$ENDIF}
    archivename := StringReplace(archivename, 'NOSODATA', 'data', [rfReplaceAll]);
    MyZipFile.Entries.AddFileEntry(ResumenFilename, archivename);
    MyZipFile.ZipAllFiles;
    Result := True;
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on Zip Headers file: ' + E.Message);
  end{Try};
  MyZipFile.Free;
end;

// Creates the zip block file
function CreateZipBlockfile(firstblock: Integer): String;
var
  MyZipFile: TZipper;
  ZipFileName: String;
  LastBlock: Integer;
  contador: Integer;
  filename, archivename: String;
begin
  Result := '';
  LastBlock := FirstBlock + 100;
  if LastBlock > MyLastBlock then LastBlock := MyLastBlock;
  MyZipFile := TZipper.Create;
  ZipFileName := BlockDirectory + 'Blocks_' + IntToStr(FirstBlock) + '_' +
    IntToStr(LastBlock) + '.zip';
  MyZipFile.FileName := ZipFileName;
  EnterCriticalSection(CSBlocksAccess);
  try
    for contador := FirstBlock to LastBlock do
    begin
      filename := BlockDirectory + IntToStr(contador) + '.blk';
      {$IFDEF WINDOWS}
      archivename:= StringReplace(filename,'\','/',[rfReplaceAll]);
      {$ENDIF}
      {$IFDEF UNIX}
      archivename:= filename;
      {$ENDIF}
      MyZipFile.Entries.AddFileEntry(filename, archivename);
    end;
    MyZipFile.ZipAllFiles;
    Result := ZipFileName;
  except
    ON E: Exception do
    begin
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error zipping block files: ' + E.Message);
    end;
  end;
  LeaveCriticalSection(CSBlocksAccess);
  MyZipFile.Free;
end;

// Send Zipped blocks to peer
procedure PTC_SendBlocks(Slot: Integer; TextLine: String);
const
  LastRequest: Int64 = 0;
var
  FirstBlock, LastBlock: Integer;
  MyZipFile: TZipper;
  contador: Integer;
  MemStream: TMemoryStream;
  filename, archivename: String;
  GetFileOk: Boolean = False;
  FileSentOk: Boolean = False;
  ZipFileName: String;
begin
  if UTCTime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  ToLog('Console', '********** DEBUG CHECK **********');
  FirstBlock := StrToIntDef(Parameter(textline, 5), -1) + 1;
  ZipFileName := CreateZipBlockfile(FirstBlock);
  MemStream := TMemoryStream.Create;
  try
    MemStream.LoadFromFile(ZipFileName);
    GetFileOk := True;
  except
    on E: Exception do
    begin
      GetFileOk := False;
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error on PTC_SendBlocks: ' + E.Message);
    end;
  end; {TRY}
  if GetFileOk then
  begin
    if GetConexIndex(Slot).tipo = 'CLI' then
    begin
      try
        GetConexIndex(Slot).context.Connection.IOHandler.WriteLn('BLOCKZIP');
        GetConexIndex(Slot).context.connection.IOHandler.Write(MemStream, 0, True);
        FileSentOk := True;
      except
        on E: Exception do
        begin
          Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
          ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
            Now) + ' -> ' + 'SERVER: Error sending ZIP blocks file (' + E.Message + ')');
        end;
      end; {TRY}
    end;
    if GetConexIndex(Slot).tipo = 'SER' then
    begin
      try
        CanalCliente[Slot].IOHandler.WriteLn('BLOCKZIP');
        CanalCliente[Slot].IOHandler.Write(MemStream, 0, True);
        FileSentOk := True;
      except
        on E: Exception do
        begin
          ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
            Now) + ' -> ' + 'CLIENT: Error sending ZIP blocks file (' + E.Message + ')');
          CloseSlot(slot);
        end; {TRY}
      end;
    end;
  end;
  MemStream.Free;
  Trydeletefile(ZipFileName);
end;

procedure INC_PTC_Custom(TextLine: String; connection: Integer);
begin
  AddCriptoOp(4, TextLine, '');
end;

// Procesa una solicitud de customizacion
function PTC_Custom(TextLine: String): Integer;
var
  OrderInfo: TOrderData;
  Address: String = '';
  OpData: String = '';
  ErrorCode: Integer = 0;
begin
  Result := 0;
  if not GetOrderFromString(TextLine, OrderInfo) then exit;
  Address := GetAddressFromPublicKey(OrderInfo.Sender);
  if address <> OrderInfo.Address then ErrorCode := 1;
  // La direccion no dispone de fondos
  if GetAddressBalanceIndexed(Address) - GetAddressPendingPays(Address) <
    GetCustFee(MyLastBlock) then ErrorCode := 2;
  if TranxAlreadyPending(OrderInfo.TrfrID) then ErrorCode := 3;
  if OrderInfo.TimeStamp < LastBlockData.TimeStart then ErrorCode := 4;
  if TrxExistsInLastBlock(OrderInfo.TrfrID) then ErrorCode := 5;
  if AddressAlreadyCustomized(Address) then ErrorCode := 6;
  if AliasAlreadyExists(OrderInfo.Receiver) then ErrorCode := 7;
  if not VerifySignedString('Customize this ' + Address + ' ' + OrderInfo.Receiver,
    OrderInfo.Signature, OrderInfo.Sender) then ErrorCode := 8;
  if ErrorCode = 0 then
  begin
    OpData := GetOpData(TextLine); // Eliminar el encabezado
    AddArrayPoolTXs(OrderInfo);
    if form1.Server.Active then OutgoingMsjsAdd(GetPTCEcn + opdata);
  end;
  Result := ErrorCode;
end;

function IsAddressLocked(LAddress: String): Boolean;
begin
  Result := False;
  if AnsiContainsSTR(GetCFGDataStr(5), LAddress) then Result := True;
end;

// Verify a transfer
function ValidateTrfr(order: Torderdata; Origen: String): Integer;
begin
  Result := 0;
  if GetAddressBalanceIndexed(Origen) - GetAddressPendingPays(Origen) <
    Order.AmmountFee + order.AmmountTrf then
    Result := 1
  else if TranxAlreadyPending(order.TrfrID) then
    Result := 2
  else if Order.TimeStamp < LastBlockData.TimeStart then
    Result := 3
  else if Order.TimeStamp > LastBlockData.TimeEnd + 600 then
    Result := 4
  else if TrxExistsInLastBlock(Order.TrfrID) then
    Result := 5
  else if not VerifySignedString(IntToStr(order.TimeStamp) + origen +
    order.Receiver + IntToStr(order.AmmountTrf) + IntToStr(order.AmmountFee) +
    IntToStr(order.TrxLine), Order.Signature, Order.Sender) then
    Result := 6
  else if Order.AmmountTrf < 0 then
    Result := 7
  else if Order.AmmountFee < 0 then
    Result := 8
  else if not IsValidHashAddress(Origen) then
    Result := 9
  else if ((order.OrderType = 'TRFR') and (not IsValidHashAddress(Order.Receiver))) then
    Result := 10
  else if IsAddressLocked(Order.Address) then
    Result := 11
  else if ((AnsiContainsStr(GetCFGDataStr(0), 'EMPTY')) or
    (AnsiContainsStr(GetCFGDataStr(0), 'STOP'))) then
    Result := 12
  else if origen <> Order.Address then
    Result := 13
  else
    Result := 0;
end;

function IsOrderIDAlreadyProcessed(OrderText: String): Boolean;
var
  OrderID: String;
  counter: Integer;
begin
  Result := False;
  OrderId := parameter(OrderText, 7);
  EnterCriticalSection(CSIdsProcessed);
  if length(ArrayOrderIDsProcessed) > 0 then
  begin
    for counter := 0 to length(ArrayOrderIDsProcessed) - 1 do
    begin
      if ArrayOrderIDsProcessed[counter] = OrderID then
      begin
        Result := True;
        break;
      end;
    end;
  end;
  if Result = False then Insert(OrderID, ArrayOrderIDsProcessed, length(
      ArrayOrderIDsProcessed));
  LeaveCriticalSection(CSIdsProcessed);
end;

procedure INC_PTC_Order(TextLine: String; connection: Integer);
begin
  if not IsOrderIDAlreadyProcessed(TextLine) then
    AddCriptoOp(5, TextLine, '');
end;

function PTC_Order(TextLine: String): String;
var
  NumTransfers: Integer;
  TrxArray: array of Torderdata;
  senderTrx: array of String;
  cont: Integer;
  Textbak: String;
  sendersString: String = '';
  TodoValido: Boolean = True;
  Proceder: Boolean = True;
  ErrorCode: Integer = -1;
  TotalSent: Int64 = 0;
  TotalFee: Int64 = 0;
  RecOrderID: String = '';
  GenOrderID: String = '';
begin
  Result := '';
  try
    NumTransfers := StrToInt(Parameter(TextLine, 5));
    ToLog('events', format('Order with %d transfers', [Numtransfers]));
    ToLog('events', format('Complete line: %s', [TextLine]));
    RecOrderId := Parameter(TextLine, 7);
    GenOrderID := Parameter(TextLine, 5) + Parameter(TextLine, 10);
    Textbak := GetOpData(TextLine);
    if NumTransfers > 30 then
    begin
      Proceder := False;
      ErrorCode := 89;
    end;
    SetLength(TrxArray, 0);
    SetLength(senderTrx, 0);
    for cont := 0 to NumTransfers - 1 do
    begin
      if not Proceder then Break;
      SetLength(TrxArray, length(TrxArray) + 1);
      SetLength(senderTrx, length(senderTrx) + 1);
      if not GetOrderFromString(Textbak, TrxArray[cont]) then
      begin
        Proceder := False;
        ErrorCode := 96;
      end;
      Inc(TotalSent, TrxArray[cont].AmmountTrf);
      Inc(TotalFee, TrxArray[cont].AmmountFee);
      GenOrderID := GenOrderID + TrxArray[cont].TrfrID;
      if TranxAlreadyPending(TrxArray[cont].TrfrID) then
      begin
        Proceder := False;
        ErrorCode := 98;
      end;
      senderTrx[cont] := GetAddressFromPublicKey(TrxArray[cont].Sender);
      if senderTrx[cont] <> TrxArray[cont].Address then
      begin
        proceder := False;
        ErrorCode := 97;
        //ToLog('console',format('error: %s <> %s',[senderTrx[cont],TrxArray[cont].Address ]))
      end;
      if AnsiContainsstr(sendersString, senderTrx[cont]) then
      begin
        Proceder := False; // sender duplicated
        ErrorCode := 99;
      end;
      sendersString := sendersString + senderTrx[cont];
      Textbak := copy(textBak, 2, length(textbak));
      Textbak := GetOpData(Textbak);
    end;
    GenOrderID := GetOrderHash(GenOrderID);
    if TotalFee < GetMinimumFee(TotalSent) then
    begin
      TodoValido := False;
      ErrorCode := 100;
    end;
    if RecOrderId <> GenOrderID then
    begin
      TodoValido := False;
      ErrorCode := 101;
    end;
    if TodoValido then
    begin
      for cont := 0 to NumTransfers - 1 do
      begin
        ErrorCode := ValidateTrfr(TrxArray[cont], senderTrx[cont]);
        if ErrorCode > 0 then
        begin
          TodoValido := False;
          break;
        end;
      end;
    end;
    if not todovalido then Proceder := False;
    if proceder then
    begin
      Textbak := GetOpData(TextLine);
      Textbak := GetPTCEcn + 'ORDER ' + IntToStr(NumTransfers) + ' ' + Textbak;
      for cont := 0 to NumTransfers - 1 do
        AddArrayPoolTXs(TrxArray[cont]);
      if form1.Server.Active then OutgoingMsjsAdd(Textbak);
      U_DirPanel := True;
      Result := Parameter(Textbak, 7); // send order ID as result
    end
    else
    begin
      if ErrorCode > 0 then Result := 'ERROR ' + ErrorCode.ToString;
    end;
  except
    ON E: Exception do
    begin
      ToLog('Console', '****************************************' + slinebreak +
        'PTC_Order:' + E.Message);
    end;
  end; {TRY}
end;

procedure INC_PTC_SendGVT(TextLine: String; connection: Integer);
begin
  if not IsOrderIDAlreadyProcessed(TextLine) then
    AddCriptoOp(7, TextLine, '');
end;

function PTC_SendGVT(TextLine: String): Integer;
var
  OrderInfo: TOrderData;
  Address: String = '';
  OpData: String = '';
  ErrorCode: Integer = 0;
  StrTosign: String = '';
begin
  if not GetOrderFromString(TextLine, OrderInfo) then exit;
  Address := GetAddressFromPublicKey(OrderInfo.Sender);
  if address <> OrderInfo.Address then ErrorCode := 1;
  // La direccion no dispone de fondos
  if GetAddressBalanceIndexed(Address) - GetAddressPendingPays(Address) <
    GetCustFee(MyLastBlock) then ErrorCode := 2;
  if TranxAlreadyPending(OrderInfo.TrfrID) then ErrorCode := 3;
  if OrderInfo.TimeStamp < LastBlockData.TimeStart then ErrorCode := 4;
  if TrxExistsInLastBlock(OrderInfo.TrfrID) then ErrorCode := 5;
  if GVTAlreadyTransfered(OrderInfo.Reference) then ErrorCode := 6;
  StrTosign := 'Transfer GVT ' + OrderInfo.Reference + ' ' + OrderInfo.Receiver +
    OrderInfo.TimeStamp.ToString;
  if not VerifySignedString(StrToSign, OrderInfo.Signature, OrderInfo.Sender) then
    ErrorCode := 7;
  if OrderInfo.Sender <> AdminPubKey then ErrorCode := 8;
  if ErrorCode = 0 then
  begin
    OpData := GetOpData(TextLine); // remove trx header
    AddArrayPoolTXs(OrderInfo);
    if form1.Server.Active then OutgoingMsjsAdd(GetPTCEcn + opdata);
  end;
  Result := ErrorCode;
  if ErrorCode > 0 then
    ToLog('events', TimeToStr(now) + 'SendGVT error: ' + ErrorCode.ToString);
end;

procedure PTC_AdminMSG(TextLine: String);
const
  MsgsReceived: String = '';
var
  msgtime, mensaje, firma, hashmsg: String;
  msgtoshow: String = '';
  contador: Integer = 1;
  errored: Boolean = False;
  TCommand: String;
  TParam: String;

  procedure LaunchDirectiveThread(LParameter: String);
  var
    ThDirect: TThreadDirective;
  begin
    if not WO_AutoUpdate then exit;
    ThDirect := TThreadDirective.Create(True, LParameter);
    ThDirect.FreeOnTerminate := True;
    ThDirect.Start;
    ToLog('events', TimeToStr(now) + Format('Directive: %s', [LParameter]));
  end;

begin
  msgtime := parameter(TextLine, 5);
  mensaje := parameter(TextLine, 6);
  firma := parameter(TextLine, 7);
  hashmsg := parameter(TextLine, 8);
  if AnsiContainsStr(MsgsReceived, hashmsg) then errored := True;
  mensaje := StringReplace(mensaje, '_', ' ', [rfReplaceAll, rfIgnoreCase]);
  if not VerifySignedString(msgtime + mensaje, firma, AdminPubKey) then
  begin
    ToLog('events', TimeToStr(now) + 'Directive wrong sign');
    errored := True;
  end;
  if HashMD5String(msgtime + mensaje + firma) <> Hashmsg then
  begin
    ToLog('events', TimeToStr(now) + 'Directive wrong hash');
    errored := True;
  end;
  if not errored then
  begin
    MsgsReceived := MsgsReceived + hashmsg;
    TCommand := Parameter(mensaje, 0);
    TParam := Parameter(mensaje, 1);
    if UpperCase(TCommand) = 'UPDATE' then LaunchDirectiveThread('update ' + TParam);
    if UpperCase(TCommand) = 'RESTART' then LaunchDirectiveThread('restart');
    if UpperCase(TCommand) = 'SETMODE' then SetCFGData(TParam, 0);
    if UpperCase(TCommand) = 'ADDNODE' then
    begin
      AddCFGData(TParam, 1);
      FillNodeList;
      SetNodesArray(GetCFGDataStr(1));
    end;
    if UpperCase(TCommand) = 'DELNODE' then
    begin
      RemoveCFGData(TParam, 1);
      FillNodeList;
      SetNodesArray(GetCFGDataStr(1));
    end;
    if UpperCase(TCommand) = 'ADDNTP' then AddCFGData(TParam, 2);
    if UpperCase(TCommand) = 'DELNTP' then RemoveCFGData(TParam, 2);
   {
   if UpperCase(TCommand) = 'ADDPOOLADDRESS' then AddCFGData(TParam,3);
   if UpperCase(TCommand) = 'DELPOOLADDRESS' then RemoveCFGData(TParam,3);
   if UpperCase(TCommand) = 'ADDPOOLHOST' then AddCFGData(TParam,4);
   if UpperCase(TCommand) = 'DELPOOLHOST' then RemoveCFGData(TParam,4);
   }
    if UpperCase(TCommand) = 'ADDLOCKED' then AddCFGData(TParam, 5);
    if UpperCase(TCommand) = 'DELLOCKED' then RemoveCFGData(TParam, 5);
    if UpperCase(TCommand) = 'ADDNOSOPAY' then AddCFGData(TParam, 6);
    if UpperCase(TCommand) = 'DELNOSOPAY' then RemoveCFGData(TParam, 6);
    if UpperCase(TCommand) = 'CLEARCFG' then ClearCFGData(TParam);
    if UpperCase(TCommand) = 'RESTORECFG' then RestoreCFGData;
    OutgoingMsjsAdd(TextLine);
  end;
end;

procedure PTC_CFGData(Linea: String);
var
  startpos: Integer;
  content: String;
begin
  startpos := Pos('$', Linea);
  Content := Copy(Linea, Startpos + 1, Length(linea));
  if Copy(HAshMD5String(Content), 0, 5) = GetCOnsensus(19) then
  begin
    SaveCFGToFile(content);
    SetCFGDataStr(content);
    FillNodeList;
    ToLog('events', 'Noso CFG updated!');
  end
  else
    ToLog('events', Format('Failed CFG: %s <> %s',
      [Copy(HAshMD5String(Content), 0, 5), GetCOnsensus(19)]));
end;

{
Procedure PTC_SendMNsList(slot:integer);
var
  DataArray : array of string;
  counter   : integer;
Begin
  if FillMnsListArray(DataArray) then
    begin
    for counter := 0 to length(DataArray)-1 do
      PTC_SendLine(slot,GetPTCEcn+'$MNREPO '+DataArray[counter]);
    end;
End;
}

procedure PTC_SendUpdateHeaders(Slot: Integer; Linea: String);
const
  LastRequest: Int64 = 0;
var
  Block: Integer;
begin
  if UTCTime < LastRequest + 10 then exit;
  LastRequest := UTCTime;
  Block := StrToIntDef(Parameter(Linea, 5), 0);
  PTC_SendLine(slot, ProtocolLine(headupdate) + ' $' + LastHeadersString(Block));
end;

procedure PTC_ProcessMNFileIncoming(LText: String);
var
  MNText: String;
begin
  MNText := ExtractMNsText(LText);
  if copy(HashMD5String(MNText + #13#10), 1, 5) = GetConsensus(8) then
  begin
    //ToLog('console','Received MNs hash match!');
    SaveMNsFile(MNText);
    FillNodeList;
  end
  else
  begin
    //ToLog('console',' Wrong MN hash received');
    //ToLog('console',MNText);
  end;
end;

// This function must go to NosoHeaders
procedure PTC_HeadUpdate(linea: String);
var
  startpos: Integer;
  content: String;
  ThisHeader, blockhash, sumhash: String;
  Counter: Integer = 0;
  Numero: Integer;
  LastBlockOnSummary: Integer;
  TotalErrors: Integer = 0;
  TotalReceived: Integer = 0;
begin
  if GetResumenHash = GetConsensus(5) then exit;
  startpos := Pos('$', Linea);
  Content := Copy(Linea, Startpos + 1, Length(linea));
  //ToLog('console','Content: '+Linea);
  repeat
    ThisHeader := Parameter(Content, counter);
    if thisheader <> '' then
    begin
      Inc(TotalReceived);
      ThisHeader := StringReplace(ThisHeader, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
      Numero := StrToIntDef(Parameter(ThisHeader, 0), 0);
      blockhash := Parameter(ThisHeader, 1);
      sumhash := Parameter(ThisHeader, 2);
      LastBlockOnSummary := GetHeadersLastBlock();
      if numero = LastBlockOnSummary + 1 then
        AddRecordToHeaders(Numero, blockhash, sumhash)
      else
      begin
        Inc(TotalErrors);
      end;
    end;
    Inc(counter);
  until ThisHeader = '';
  SetResumenHash;
  if copy(GetResumenHash, 0, 5) <> GetConsensus(5) then
  begin
    ForceCompleteHeadersDownload := True;
    ToLog('Console', Format('Update headers failed (%d) : %s <> %s',
      [TotalErrors, Copy(GetResumenHash, 0, 5), GetConsensus(5)]));
  end
  else
  begin
    ToLog('Console', 'Headers Updated!');
    ForceCompleteHeadersDownload := False;
  end;
end;

// Añade una operacion a la espera de cripto
procedure AddCriptoOp(tipo: Integer; proceso, resultado: String);
var
  NewOp: TArrayCriptoOp;
begin
  NewOp.tipo := tipo;
  NewOp.Data := proceso;
  NewOp.Result := resultado;
  EnterCriticalSection(CSCriptoThread);
  try
    Insert(NewOp, ArrayCriptoOp, length(ArrayCriptoOp));

  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error adding Operation to crypto thread:' + proceso);
  end{Try};
  LeaveCriticalSection(CSCriptoThread);
end;

// Elimina la operacion cripto
procedure DeleteCriptoOp();
begin
  EnterCriticalSection(CSCriptoThread);
  if Length(ArrayCriptoOp) > 0 then
  begin
    try
      Delete(ArrayCriptoOp, 0, 1);
    except
      ON E: Exception do
      begin
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
          ' -> ' + 'Error removing Operation from crypto thread:' + E.Message);
      end;
    end{Try};
  end;
  LeaveCriticalSection(CSCriptoThread);
end;


end. // END UNIT
