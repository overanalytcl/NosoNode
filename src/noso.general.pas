unit Noso.General;

{
nosogeneral 1.2
March 7th 2024
Noso Unit for general functions
Requires: Not dependencyes
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils, IdTCPClient, IdGlobal, fphttpclient,
  opensslsockets, fileutil, Noso.Debug, Zipper;

type
  TStreamHelper = class helper for TStream
    procedure SetString(const S: String);
    function GetString: String;
  end;

  TStringArray = array of String;

  TOrderData = packed record
    Block: Integer;
    OrderID: String[64];
    OrderLines: Integer;
    OrderType: String[6];
    TimeStamp: Int64;
    Reference: String[64];
    TrxLine: Integer;
    Sender: String[120];
    Address: String[40];
    Receiver: String[40];
    AmmountFee: Int64;
    AmmountTrf: Int64;
    Signature: String[120];
    TrfrID: String[64];
  end;


  TMultiOrder = record
    Block: Integer;
    TimeStamp: Int64;
    OrderID: String[64];
    OrderType: String[6];
    Reference: String[64];
    Sender: String[40];
    PubKey: String[255];
    Receiver: String[40];
    Signer: String[120];
    Signature: String[120];
    AmmountFee: Int64;
    AmmountTrf: Int64;
  end;

  TBlockOrdersArray = array of TOrderData;

{Generic}
function Parameter(LineText: String; ParamNumber: Int64; de_limit: String = ' '): String;
function GetCommand(LineText: String): String;
function ProCommand(LineText: String): String;
function IsValidIP(IpString: String): Boolean;
function GetSupply(block: Integer): Int64;
function Restar(number: Int64): Int64;
function HashrateToShow(speed: Int64): String;
function Int2Curr(LValue: Int64): String;
procedure RunExternalProgram(ProgramToRun: String);
function GetCustFee(Block: Integer): Int64;
function GetStackRequired(block: Integer): Int64;
function GetMNsPercentage(block: Integer; MainnetMode: String = 'NORMAL'): Integer;
function GetPoSPercentage(block: Integer): Integer;
function GetDevPercentage(block: Integer): Integer;
function GetMinimumFee(amount: Int64): Int64;
function GetMaximunToSend(amount: Int64): Int64;
function OSVersion: String;

{Network}
function RequestLineToPeer(host: String; port: Integer; command: String): String;
function RequestToPeer(hostandPort, command: String): String;
function SendApiRequest(urltocheck: String): String;

{File handling}
function SaveTextToDisk(const aFileName: TFileName; const aText: String): Boolean;
function LoadTextFromDisk(const aFileName: TFileName): String;
function TryCopyFile(Source, destination: String): Boolean;
function TryDeleteFile(filename: String): Boolean;
function AppFileName(): String;
function MixTxtFiles(ListFiles: array of String; Destination: String;
  DeleteSources: Boolean = True): Boolean;
function SendFileViaTCP(filename, message, host: String; Port: Integer): Boolean;
function UnzipFile(filename: String; delFile: Boolean): Boolean;
function CreateEmptyFile(lFilename: String): Boolean;

{Protocol specific}
function GetStringFromOrder(order: Torderdata): String;
function ExtractMNsText(lText: String): String;

implementation

{$REGION Stream helper}

procedure TStreamHelper.SetString(const S: String);
var
  LSize: Word;
begin
  LSize := Length(S);
  WriteBuffer(LSize, SizeOf(LSize));
  WriteBuffer(Pointer(S)^, LSize);
end;

function TStreamHelper.GetString: String;
var
  LSize: Word = 0;
  P: Pbyte;
begin
  ReadBuffer(LSize, SizeOf(LSize));
  SetLength(Result, LSize);
  if LSize > 0 then
  begin
    ReadBuffer(Pointer(Result)^, LSize);
    P := Pointer(Result) + LSize;
    P^ := 0;
  end;
end;

{$ENDREGION}

{$REGION Generic}

{Returns a specific parameter number of text}
function Parameter(LineText: String; ParamNumber: Int64; de_limit: String = ' '): String;
var
  Temp: String = '';
  ThisChar: Char;
  Contador: Int64 = 1;
  WhiteSpaces: Int64 = 0;
  parentesis: Boolean = False;
begin
  while contador <= Length(LineText) do
  begin
    ThisChar := Linetext[contador];
    if ((thischar = '(') and (not parentesis)) then parentesis := True
    else if ((thischar = '(') and (parentesis)) then
    begin
      Result := '';
      exit;
    end
    else if ((ThisChar = ')') and (parentesis)) then
    begin
      if WhiteSpaces = ParamNumber then
      begin
        Result := temp;
        exit;
      end
      else
      begin
        parentesis := False;
        temp := '';
      end;
    end
    else if ((ThisChar = de_limit) and (not parentesis)) then
    begin
      WhiteSpaces := WhiteSpaces + 1;
      if WhiteSpaces > Paramnumber then
      begin
        Result := temp;
        exit;
      end;
    end
    else if ((ThisChar = de_limit) and (parentesis) and (WhiteSpaces = ParamNumber)) then
    begin
      temp := temp + ThisChar;
    end
    else if WhiteSpaces = ParamNumber then temp := temp + ThisChar;
    contador := contador + 1;
  end;
  if temp = de_limit then temp := '';
  Result := Temp;
end;

function GetCommand(LineText: String): String;
begin
  Result := uppercase(parameter(linetext, 0));
end;

function ProCommand(LineText: String): String;
begin
  Result := uppercase(parameter(linetext, 4));
end;

{Verify if a string is valid IPv4 address}
function IsValidIP(IpString: String): Boolean;
var
  valor1, valor2, valor3, valor4: Integer;
begin
  Result := True;
  //IPString := StringReplace(IPString,'.',' ',[rfReplaceAll, rfIgnoreCase]);
  valor1 := StrToIntDef(Parameter(IPString, 0, '.'), -1);
  valor2 := StrToIntDef(Parameter(IPString, 1, '.'), -1);
  valor3 := StrToIntDef(Parameter(IPString, 2, '.'), -1);
  valor4 := StrToIntDef(Parameter(IPString, 3, '.'), -1);
  if ((valor1 < 0) or (valor1 > 255)) then Result := False;
  if ((valor2 < 0) or (valor2 > 255)) then Result := False;
  if ((valor3 < 0) or (valor3 > 255)) then Result := False;
  if ((valor4 < 0) or (valor4 > 255)) then Result := False;
  if ((valor1 = 192) and (valor2 = 168)) then Result := False;
  if ((valor1 = 127) and (valor2 = 0)) then Result := False;
end;

{Returns the circulating supply on the specified block}
function GetSupply(block: Integer): Int64;
begin
  Result := 0;
  if block < 210000 then
    Result := (block * 5000000000) + 1030390730000
  else if ((block >= 210000) and (block < 420000)) then
  begin
    Inc(Result, (209999 * 5000000000) + 1030390730000);
    Inc(Result, (block - 209999) * 5000000000);
  end;
end;

{Convert any positive integer in negative}
function Restar(number: Int64): Int64;
begin
  if number > 0 then Result := number - (Number * 2)
  else
    Result := number;
end;

{Converts a integer in a human readeaeble format for hashrate}
function HashrateToShow(speed: Int64): String;
begin
  if speed > 1000000000 then Result := FormatFloat('0.00', speed / 1000000000) + ' Gh/s'
  else if speed > 1000000 then Result := FormatFloat('0.00', speed / 1000000) + ' Mh/s'
  else if speed > 1000 then Result := FormatFloat('0.00', speed / 1000) + ' Kh/s'
  else
    Result := speed.ToString + ' h/s';
end;

{Converts a integer in a human readeaeble format for currency}
function Int2Curr(LValue: Int64): String;
begin
  Result := IntToStr(Abs(LValue));
  Result := AddChar('0', Result, 9);
  Insert('.', Result, Length(Result) - 7);
  if LValue < 0 then Result := '-' + Result;
end;

{Runs an external program}
procedure RunExternalProgram(ProgramToRun: String);
var
  Process: TProcess;
  I: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Options := [];
    Process.ShowWindow := swoShow;
    for I := 1 to GetEnvironmentVariableCount do
      Process.Environment.Add(GetEnvironmentString(I));
    {$IFDEF UNIX}
    process.Executable := 'bash';
    process.Parameters.Add(ProgramToRun);
    {$ENDIF}
    {$IFDEF WINDOWS}
    Process.Executable := ProgramToRun;
    {$ENDIF}
    Process.Execute;
  except
    ON E: Exception do

  end; {TRY}
  Process.Free;
end;

{Returns the custom fee amount}
function GetCustFee(Block: Integer): Int64;
begin
  Result := 1000000;
  if block < 162000 then Result := 25000;
end;

{Returns the required noso stack size}
function GetStackRequired(block: Integer): Int64;
begin
  Result := (GetSupply(block) * 20) div 10000;
  if Result > 1100000000000 then Result := 1100000000000;
  if block > 110000 then Result := 1050000000000;
end;

{Returns the MNs percentage for the specified block (0 to 10000)}
function GetMNsPercentage(block: Integer; MainnetMode: String = 'NORMAL'): Integer;
begin
  Result := 0;
  if block >= 48010{MNBlockStart} then
  begin
    Result := 2000{MNsPercentage} + (((block - 48010{MNBlockStart}) div 4000) * 100);
    if block >= 88400{PoSBlockEnd} then Inc(Result, 1000);
    if Result > 6000 then Result := 6000;
    if AnsiContainsStr(MainnetMode, 'MNSONLY') then Result := 9000;
  end;
end;

{Returns the PoS percentage for the specified block (0 to 10000)}
function GetPoSPercentage(block: Integer): Integer;
begin
  Result := 0;
  if ((block > 8424) and (block < 40000)) then Result := 1000{PoSPercentage};
  if block >= 40000 then
  begin
    Result := 1000{PoSPercentage} + (((block - 39000) div 1000) * 100);
    if Result > 2000 then Result := 2000;
  end;
  if block >= 88400{PoSBlockEnd} then Result := 0;
end;

{Returns the Project percentage for the specified block}
function GetDevPercentage(block: Integer): Integer;
begin
  Result := 0;
  if block >= 88400{PoSBlockEnd} then Result := 1000;
end;

{Returns the minimum fee to be paid for the specified amount}
function GetMinimumFee(amount: Int64): Int64;
begin
  Result := amount div 10000{Comisiontrfr};
  if Result < 1000000{MinimunFee} then Result := 1000000{MinimunFee};
end;

{Returns the maximum that can be sent from the specified amount}
function GetMaximunToSend(amount: Int64): Int64;
var
  maximo: Int64;
  comision: Int64;
  Envio: Int64;
  Diferencia: Int64;
begin
  if amount < 1000000{MinimunFee} then
    exit(0);
  maximo := (amount * 10000{Comisiontrfr}) div (10000{Comisiontrfr} + 1);
  comision := maximo div 10000{Comisiontrfr};
  if Comision < 1000000{MinimunFee} then Comision := 1000000{MinimunFee};
  Envio := maximo + comision;
  Diferencia := amount - envio;
  Result := maximo + diferencia;
end;

// Gets OS version
function OSVersion: String;
begin
  {$IFDEF LCLcarbon}
  OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Linux Kernel ';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ELSE}
  {$IFDEF WINDOWS}
  OSVersion:= 'Windows';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

{$ENDREGION}

{$REGION Network}

function RequestLineToPeer(host: String; port: Integer; command: String): String;
var
  Client: TidTCPClient;
begin
  Result := '';
  Client := TidTCPClient.Create(nil);
  Client.Host := host;
  Client.Port := Port;
  Client.ConnectTimeout := 1000;
  Client.ReadTimeout := 1000;
  try
    Client.Connect;
    Client.IOHandler.WriteLn(Command);
    client.IOHandler.MaxLineLength := Maxint;
    Result := Client.IOHandler.ReadLn();
  except
    on E: Exception do

  end;{Try}
  if client.Connected then Client.Disconnect();
  client.Free;
end;

function RequestToPeer(hostandPort, command: String): String;
var
  Client: TidTCPClient;
begin
  Result := '';
  Client := TidTCPClient.Create(nil);
  Client.Host := Parameter(hostandPort, 0);
  Client.Port := StrToIntDef(Parameter(hostandPort, 1), 8080);
  Client.ConnectTimeout := 1000;
  Client.ReadTimeout := 1000;
  try
    Client.Connect;
    Client.IOHandler.WriteLn(Command);
    client.IOHandler.MaxLineLength := Maxint;
    Result := Client.IOHandler.ReadLn();
  except
    on E: Exception do

  end;{Try}
  if client.Connected then Client.Disconnect();
  client.Free;
end;

function SendApiRequest(urltocheck: String): String;
var
  Conector: TFPHttpClient;
begin
  Result := '';
  Conector := TFPHttpClient.Create(nil);
  conector.ConnectTimeout := 3000;
  conector.IOTimeout := 3000;
  try
    Result := Trim(Conector.SimpleGet(urltocheck));
  except
    on E: Exception do

  end;//TRY
  Conector.Free;
end;

{$ENDREGION}

{$REGION File handling}

function SaveTextToDisk(const aFileName: TFileName; const aText: String): Boolean;
var
  LStream: TStringStream;
begin
  Result := True;
  LStream := TStringStream.Create(aText);
  try
    LStream.SaveToFile(aFileName);
  except
    On E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,SaveTextToDisk,' + E.Message);
    end;
  end;{Try}
  LStream.Free;
end;

function LoadTextFromDisk(const aFileName: TFileName): String;
var
  LStream: TStringStream;
begin
  Result := '';
  LStream := TStringStream.Create;
  try
    LStream.LoadFromFile(aFileName);
    Result := LStream.DataString;
  except
    On E: Exception do
    begin
      Result := '';
      ToDeepDeb('NosoGeneral,LoadTextFromDisk,' + E.Message);
    end;
  end;{Try}
  LStream.Free;
end;

function TryCopyFile(Source, destination: String): Boolean;
begin
  Result := True;
  try
    copyfile(Source, destination, [cffOverwriteFile], True);
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,TryCopyFile,' + E.Message);
    end;
  end; {TRY}
end;

{Try to delete a file safely}
function TryDeleteFile(filename: String): Boolean;
begin
  Result := deletefile(filename);
end;

// Returns the name of the app file without path
function AppFileName(): String;
begin
  Result := ExtractFileName(ParamStr(0));
  // For working path: ExtractFilePAth
end;

function MixTxtFiles(ListFiles: array of String; Destination: String;
  DeleteSources: Boolean = True): Boolean;
var
  Count: Integer = 0;
  FinalFile: TStringList;
  ThisFile: TStringList;
  Added: Integer = 0;
  Index: Integer;
begin
  Result := True;
  FinalFile := TStringList.Create;
  ThisFile := TStringList.Create;
  while Count < Length(Listfiles) do
  begin
    if FileExists(ListFiles[Count]) then
    begin
      ThisFile.Clear;
      ThisFile.LoadFromFile(ListFiles[Count]);
      FinalFile.Add('-----> ' + ListFiles[Count]);
      Index := 0;
      while Index < ThisFile.Count do
      begin
        FinalFile.Add(ThisFile[index]);
        Inc(index);
      end;
      Inc(added);
    end;
    if DeleteSources then TryDeletefile(ListFiles[Count]);
    Inc(Count);
  end;
  if Added > 0 then
    FinalFile.SaveToFile(Destination);
  FinalFile.Free;
  ThisFile.Free;
  Result := True;
end;

function SendFileViaTCP(filename, message, host: String; Port: Integer): Boolean;
var
  Client: TidTCPClient;
  MyStream: TMemoryStream;
begin
  Result := True;
  if not fileExists(filename) then exit(False);
  MyStream := TMemoryStream.Create;
  MyStream.LoadFromFile(filename);
  Client := TidTCPClient.Create(nil);
  Client.Host := host;
  Client.Port := Port;
  Client.ConnectTimeout := 1000;
  Client.ReadTimeout := 1000;
  try
    Client.Connect;
    Client.IOHandler.WriteLn(message);
    Client.IOHandler.Write(MyStream, 0, True);
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,SendFile,' + filename + ' Error: ' + E.Message);
    end;
  end;{Try}
  if client.Connected then Client.Disconnect();
  client.Free;
  MyStream.Free;
end;

// Unzip a zip file and (optional) delete it
function UnzipFile(filename: String; delFile: Boolean): Boolean;
var
  UnZipper: TUnZipper;
begin
  Result := True;
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := filename;
    UnZipper.OutputPath := '';
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,UnzipFile,' + E.Message);
    end;
  end; {TRY}
  if delfile then Trydeletefile(filename);
  UnZipper.Free;
end;

// Creates an empty file
function CreateEmptyFile(lFilename: String): Boolean;
var
  lFile: textfile;
begin
  Result := True;
  try
    Assignfile(lFile, lFilename);
    rewrite(lFile);
    Closefile(lFile);
  except
    on E: Exception do
    begin
      ToDeepDeb('Nosogeneral,CreateEmptyFile,' + E.Message);
      Result := False;
    end;
  end;
end;

{$ENDREGION}

{$REGION Protocol specific}

// Convierte una orden en una cadena para compartir
function GetStringFromOrder(order: Torderdata): String;
begin
  Result := Order.OrderType + ' ' + Order.OrderID + ' ' +
    IntToStr(order.OrderLines) + ' ' + order.OrderType + ' ' +
    IntToStr(Order.TimeStamp) + ' ' + Order.reference + ' ' +
    IntToStr(order.TrxLine) + ' ' + order.Sender + ' ' + Order.Address +
    ' ' + Order.Receiver + ' ' + IntToStr(Order.AmmountFee) + ' ' +
    IntToStr(Order.AmmountTrf) + ' ' + Order.Signature + ' ' + Order.TrfrID;
end;

function ExtractMNsText(lText: String): String;
var
  startpos: Integer;
  content: String;
begin
  Result := '';
  startpos := Pos('$', lText);
  Result := Copy(lText, Startpos + 1, Length(lText));
end;

{$ENDREGION Protocol specific}


end.{UNIT}
