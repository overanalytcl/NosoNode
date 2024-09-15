// Unit for general functions.

unit Noso.General;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils, IdTCPClient, IdGlobal, fphttpclient,
  opensslsockets, fileutil, Noso.Debug, Zipper;

type
  // TStream helper class for string manipulation in streams
  TStreamHelper = class helper for TStream
    procedure SetString(const S: String);
    function GetString: String;
  end;

  // Dynamic array of strings
  TStringArray = specialize TArray<String>;

  // Record structure for order data
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
    AmountFee: Int64;
    AmountTransferred: Int64;
    Signature: String[120];
    TransferID: String[64];
  end;

  // Record structure for multi-order data
  TMultiOrderData = record
    Block: Integer;
    TimeStamp: Int64;
    OrderID: String[64];
    OrderType: String[6];
    Reference: String[64];
    Sender: String[40];
    PublicKey: String[255];
    Receiver: String[40];
    Signer: String[120];
    Signature: String[120];
    AmountFee: Int64;
    AmountTransferred: Int64;
  end;

  // Dynamic array of TOrderData records
  TBlockOrdersArray = specialize TArray<TOrderData>;

{
  @abstract(Extracts a specific parameter from a delimited string)
  @param(Line The input string containing delimited parameters)
  @param(ParamNumber The number of the parameter to extract (starting from 0))
  @param(Delimiter The character used to separate parameters (default is space))
  @returns(The extracted parameter, or an empty string if not found)
  @note(This function handles nested parentheses, where text inside parentheses is treated as a single parameter)
}
function GetParameter(const Line: String; const ParamNumber: Int64;
  const Delimiter: String = ' '): String;

{
  @abstract(Extracts the command from the first parameter of the input string.)

  This function extracts the first parameter from a delimited string and converts it to uppercase.

  @param(Line The input string containing delimited parameters.)
  @returns(The first parameter of the string, converted to uppercase.)
}
function GetCommand(const Line: String): String;

{
  @abstract(Extracts the protocol command from the input string.)

  This function extracts the protocol command from a delimited string and converts it to uppercase.

  @param(Line The input string containing delimited parameters.)
  @returns(The protocol command, converted to uppercase.)
}
function GetProtocolCommand(const Line: String): String;

{
  @abstract(Validates an IP address.)
  @param(IpAddress The IP address to be validated.)
  @returns(@true if the IP address is valid, @false otherwise.)
  @note(An IP address is considered valid if it is an IPv4 address (4 values
    from 0 to 255 separated by dots, e.g. 34.64.210.3) and it isn't in an
    invalid range like loopback (127.x.x.x) and private networks (192.168.x.x))
  @longcode(#
  if not IsValidIP('127.0.0.1') then
    Writeln('localhost is not valid!');

  if not IsValidIP('192.168.100.1') then
    Writeln('Private IP is not valid!');

  if IsValidIP('142.250.186.35') then
    Writeln('Google IP is valid!');
  #)
}
function IsValidIP(const IpAddress: String): Boolean;

{
  @abstract(Calculates the circulating supply of Noso based on the blockchain block number.)
  @param(Block The current block.)
  @returns(The circulating supply as an Int64 value.)
  @note(This function computes the circulating supply of Noso based on the given
        block number. The supply follows a specific issuance schedule. The
        calculation takes into account two phases:

        @unorderedList(
          @item(From block 0 to 209999 with a supply increase of 5,000,000,000 units per block;)
          @item(From block 210000 onwards with the same supply increase rate.)
        ))
}
function GetCirculatingSupply(const Block: Integer): Int64;

{
  @abstract(Calculates the negative absolute value of a number.)
  @param(Number The number.)
  @returns(The negated number if Number is positive, or Number otherwise.)
  @note(In essence, this is equivalent to -Abs(Number).)

  @longcode(#
  Writeln(ToNegative(-10)); // -10
  Writeln(ToNegative(0));   // 0
  Writeln(ToNegative(25));  // -25
  #)
}
function ToNegative(const Number: Int64): Int64;

{ @abstract(Converts an integer hashrate into a human-readable format.)

  The function formats the hashrate into Gh/s, Mh/s, kh/s, or h/s based on its magnitude.

  @param(Speed The hashrate value to be converted.)
  @return(A string representing the human-readable hashrate.)
  @longcode(#
  var
    HashRate: Int64;
  begin
    HashRate := 5000000000;
    WriteLn('Hash rate: ', HashRateToReadable(HashRate)); // 5,00 Gh/s

    HashRate := 234000000;
    WriteLn('Hash rate: ', HashRateToReadable(HashRate)); // 234,00 Mh/s

    HashRate := 740000;
    WriteLn('Hash rate: ', HashRateToReadable(HashRate)); // 740,00 Kh/s

    HashRate := 21240;
    WriteLn('Hash rate: ', HashRateToReadable(HashRate)); // 21,24 Kh/s

    HashRate := 240;
    WriteLn('Hash rate: ', HashRateToReadable(HashRate)); // 240 h/s

    HashRate := 56;
    WriteLn('Hash rate: ', HashRateToReadable(HashRate)); // 56 h/s
  end.
  #)
}
function HashRateToReadable(const Speed: Int64): String;

{ @abstract(Converts an integer value to a currency format with two decimal places.)

  The function formats the value as a string with thousands separators and a decimal point.

  @param(Value The integer value to be converted to currency format.)
  @return(A string representing the formatted currency value.)

  @longcode(#
  var
    Value: Int64;
    FormattedCurrency: String;
  begin
    Value := -10230456789;
    FormattedCurrency := IntToCurrency(Value);
    WriteLn('Currency: ', FormattedCurrency); // -102.30456789
  end.
  #)
}
function IntToCurrency(const Value: Int64): String;

{ Runs an external program or script based on the operating system.
  The procedure handles environment variables and executes the given program or script.

  @param(ProgramToRun The path to the executable or script to be run.)
  @note(On Unix systems, the program to run should be a script with a proper
        interpreter (e.g., bash script).)

  @longcode(#
  begin
    RunExternalProgram('notepad.exe');
  end.
  #)
}
procedure RunExternalProgram(const ProgramToRun: String);


function GetCustFee(Block: Integer): Int64;


function GetStackRequired(Block: Integer): Int64;


function GetMasterNodesPercentage(Block: Integer;
  MainnetMode: String = 'NORMAL'): Integer;


function GetPoSPercentage(Block: Integer): Integer;


function GetDevPercentage(Block: Integer): Integer;


function GetMinimumFee(Amount: Int64): Int64;


function GetMaximumToSend(Amount: Int64): Int64;


function OSVersion: String;

{Network}
function RequestLineToPeer(Host: String; Port: Integer; Command: String): String;
function RequestToPeer(HostAndPort, Command: String): String;
function SendApiRequest(Url: String): String;

{File handling}
function SaveTextToDisk(const FileName: TFileName; const Text: String): Boolean;
function LoadTextFromDisk(const FileName: TFileName): String;
function TryCopyFile(Source, Destination: String): Boolean;
function TryDeleteFile(FileName: String): Boolean;
function AppFileName(): String;
function CombineTextFiles(FileList: TStringArray; Destination: String;
  DeleteSources: Boolean = True): Boolean;
function SendFileViaTCP(FileName, Message, Host: String; Port: Integer): Boolean;
function UnzipFile(FileName: String; DeleteFile: Boolean): Boolean;
function CreateEmptyFile(FileName: String): Boolean;

{Protocol specific}
function GetStringFromOrder(Order: TOrderData): String;
function ExtractMasternodesText(Text: String): String;

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

function GetParameter(const Line: String; const ParamNumber: Int64;
  const Delimiter: String = ' '): String;
const
  OpenParen = '(';
  CloseParen = ')';
var
  Token: String = '';
  CurrentChar: Char;
  DelimiterCount: Int64 = 0;
  InParentheses: Boolean = False;
begin
  for CurrentChar in Line do
  begin

    if CurrentChar = OpenParen then
    begin
      // Either it's not InParentheses and thus it's a new opening paren,
      // or InParentheses is set already, thus the parens are incorrectly nested.
      if not InParentheses then
        InParentheses := True
      else
      begin
        Result := EmptyStr;
        Exit;
      end;
    end

    // We now know we are supposed to end a chain of parantheses, i.e. we're
    // in a situation like this:
    //    '((((something here)) test))
    //                   -----^
    else if (CurrentChar = CloseParen) and InParentheses then
    begin
      if DelimiterCount = ParamNumber then
      begin
        Result := Token;
        Exit;
      end
      else
      begin
        InParentheses := False;
        Token := EmptyStr;
      end;
    end

    // Handle delimiters outside parantheses
    else if (CurrentChar = Delimiter) and not InParentheses then
    begin
      Inc(DelimiterCount);

      if DelimiterCount > ParamNumber then
      begin
        Result := Token;
        Exit;
      end;

      Token := EmptyStr; // Reset token for next parameter
    end

    // Accumulate characters inside parentheses or for the desired parameter
    else if (DelimiterCount = ParamNumber) or (InParentheses) then
    begin
      Token := Token + CurrentChar;
    end;
  end;

  Result := Token;
end;

function GetCommand(const Line: String): String;
begin
  Result := UpperCase(GetParameter(Line, 0));
end;

function GetProtocolCommand(const Line: String): String;
begin
  Result := UpperCase(GetParameter(Line, 4));
end;

function IsValidIP(const IpAddress: String): Boolean;
var
  Part, DotCount, Digit, FirstPart, SecondPart: Integer;
  c: Char;
  IsLoopback, IsPrivate, IsReserved: Boolean;
begin
  // This is an optimized implementation.

  Part := -1;
  DotCount := 0;

  for C in IpAddress do
  begin
    case C of
      '0'..'9':
      begin
        Digit := Ord(C) - Ord('0');
        if Part = -1 then
          Part := Digit
        else
          Part := Part * 10 + Digit;

        if Part > 255 then Exit(False); // Fast reject if part exceeds 255
      end;

      '.':
      begin
        if Part = -1 then
          Exit(False); // No number before dot

        Inc(DotCount);

        if DotCount > 3 then
          Exit(False); // Too many dots

        // Store the first and second parts for special range checks
        if DotCount = 1 then
          FirstPart := Part
        else if DotCount = 2 then
          SecondPart := Part;

        Part := -1; // Reset part for next group
      end;

      else
        Exit(False); // Invalid character
    end;
  end;

  // Final checks: part must be valid and exactly 3 dots
  if (DotCount <> 3) or (Part = -1) then Exit(False);

  IsLoopback := (FirstPart = 127);
  IsReserved := (FirstPart = 0);
  IsPrivate := (FirstPart = 192) and (SecondPart = 168);

  if IsLoopback or IsReserved or IsPrivate then
    Exit(False);

  Result := True;
end;

function GetCirculatingSupply(const Block: Integer): Int64;
const
  MaxBlockBeforeHalving = 210000;
  BaseReward = 5000000000;
  InitialSupply = 1030390730000;
begin
  Result := 0;

  if Block < MaxBlockBeforeHalving then
    Result := Block * BaseReward + InitialSupply
  else
  begin
    Result := MaxBlockBeforeHalving * BaseReward + InitialSupply;
    Inc(Result, (Block - MaxBlockBeforeHalving + 1) * BaseReward);
  end;
end;

function ToNegative(const Number: Int64): Int64;
begin
  Result := Number;
  if Number > 0 then Result := -Result;
end;

function HashRateToReadable(const Speed: Int64): String;
const
  OneGigaHash = 1000000000;
  OneMegaHash = 1000000;
  OneKiloHash = 1000;
var
  Value: Double;
begin
  Value := Abs(Speed);

  if Value >= OneGigaHash then
    Result := FormatFloat('0.00', Value / OneGigaHash) + ' Gh/s'
  else if Value >= OneMegaHash then
    Result := FormatFloat('0.00', Value / OneMegaHash) + ' Mh/s'
  else if Value >= OneKiloHash then
    Result := FormatFloat('0.00', Value / OneKiloHash) + ' Kh/s'
  else
    Result := FormatFloat('0', Value) + ' h/s';
end;

function IntToCurrency(const Value: Int64): String;
var
  FormattedValue: String;
begin
  FormattedValue := FormatFloat('0.00', Abs(Value) / 100);

  Result := IntToStr(Abs(Value));
  Result := AddChar('0', Result, 9);
  Insert('.', Result, Length(Result) - 7);

  if Value < 0 then
    Result := '-' + Result;
end;

procedure RunExternalProgram(const ProgramToRun: String);
var
  Process: TProcess;
  i: Integer;
  ExitCode: Integer;
begin
  Process := TProcess.Create(nil);
  try
    Process.InheritHandles := False;
    Process.Options := [poWaitOnExit, poUsePipes];
    Process.ShowWindow := swoShow;

    for i := 1 to GetEnvironmentVariableCount do
    begin
      Process.Environment.Add(GetEnvironmentString(i));
    end;

    {$IFDEF UNIX}
    Process.Executable := 'bash';
    Process.Parameters.Add('-c');
    Process.Parameters.Add(ProgramToRun);
    {$ENDIF}

    {$IFDEF WINDOWS}
    Process.Executable := ProgramToRun;
    {$ENDIF}

    try
      Process.Execute;
      ExitCode := Process.ExitCode;
      if ExitCode <> 0 then
      begin
        WriteLn(Format('Process failed with exit code %d', [ExitCode]));
        Halt(ExitCode);
      end;
    except
      on E: Exception do
      begin
        WriteLn('An error occurred: ', E.Message);
        Halt(-1);
      end;
    end;
  finally
    Process.Free;
  end;
end;

{Returns the custom fee amount}
function GetCustFee(Block: Integer): Int64;
begin
  Result := 1000000;
  if block < 162000 then Result := 25000;
end;

{Returns the required noso stack size}
function GetStackRequired(Block: Integer): Int64;
begin
  Result := (GetCirculatingSupply(Block) * 20) div 10000;
  if Result > 1100000000000 then Result := 1100000000000;
  if Block > 110000 then Result := 1050000000000;
end;

{Returns the MNs percentage for the specified block (0 to 10000)}
function GetMasterNodesPercentage(Block: Integer;
  MainnetMode: String = 'NORMAL'): Integer;
begin
  Result := 0;
  if Block >= 48010{MNBlockStart} then
  begin
    Result := 2000{MNsPercentage} + (((Block - 48010{MNBlockStart}) div 4000) * 100);
    if Block >= 88400{PoSBlockEnd} then Inc(Result, 1000);
    if Result > 6000 then Result := 6000;
    if AnsiContainsStr(MainnetMode, 'MNSONLY') then Result := 9000;
  end;
end;

{Returns the PoS percentage for the specified block (0 to 10000)}
function GetPoSPercentage(Block: Integer): Integer;
begin
  Result := 0;
  if ((Block > 8424) and (Block < 40000)) then Result := 1000{PoSPercentage};
  if Block >= 40000 then
  begin
    Result := 1000{PoSPercentage} + (((Block - 39000) div 1000) * 100);
    if Result > 2000 then Result := 2000;
  end;
  if Block >= 88400{PoSBlockEnd} then Result := 0;
end;

{Returns the Project percentage for the specified block}
function GetDevPercentage(Block: Integer): Integer;
begin
  Result := 0;
  if Block >= 88400{PoSBlockEnd} then Result := 1000;
end;

{Returns the minimum fee to be paid for the specified amount}
function GetMinimumFee(Amount: Int64): Int64;
begin
  Result := Amount div 10000{Comisiontrfr};
  if Result < 1000000{MinimunFee} then Result := 1000000{MinimunFee};
end;

{Returns the maximum that can be sent from the specified amount}
function GetMaximumToSend(Amount: Int64): Int64;
var
  maximo: Int64;
  comision: Int64;
  Envio: Int64;
  Diferencia: Int64;
begin
  if Amount < 1000000{MinimunFee} then
    exit(0);
  maximo := (Amount * 10000{Comisiontrfr}) div (10000{Comisiontrfr} + 1);
  comision := maximo div 10000{Comisiontrfr};
  if Comision < 1000000{MinimunFee} then Comision := 1000000{MinimunFee};
  Envio := maximo + comision;
  Diferencia := Amount - envio;
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

{$REGION Network}

function RequestLineToPeer(Host: String; Port: Integer; Command: String): String;
var
  Client: TidTCPClient;
begin
  Result := '';
  Client := TidTCPClient.Create(nil);
  Client.Host := Host;
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

function RequestToPeer(HostAndPort, Command: String): String;
var
  Client: TidTCPClient;
begin
  Result := '';
  Client := TidTCPClient.Create(nil);
  Client.Host := GetParameter(HostAndPort, 0);
  Client.Port := StrToIntDef(GetParameter(HostAndPort, 1), 8080);
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

function SendApiRequest(Url: String): String;
var
  Conector: TFPHttpClient;
begin
  Result := '';
  Conector := TFPHttpClient.Create(nil);
  conector.ConnectTimeout := 3000;
  conector.IOTimeout := 3000;
  try
    Result := Trim(Conector.SimpleGet(Url));
  except
    on E: Exception do

  end;//TRY
  Conector.Free;
end;

{$ENDREGION}

{$REGION File handling}

function SaveTextToDisk(const FileName: TFileName; const Text: String): Boolean;
var
  LStream: TStringStream;
begin
  Result := True;
  LStream := TStringStream.Create(Text);
  try
    LStream.SaveToFile(FileName);
  except
    On E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,SaveTextToDisk,' + E.Message);
    end;
  end;{Try}
  LStream.Free;
end;

function LoadTextFromDisk(const FileName: TFileName): String;
var
  LStream: TStringStream;
begin
  Result := '';
  LStream := TStringStream.Create;
  try
    LStream.LoadFromFile(FileName);
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

function TryCopyFile(Source, Destination: String): Boolean;
begin
  Result := True;
  try
    copyfile(Source, Destination, [cffOverwriteFile], True);
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,TryCopyFile,' + E.Message);
    end;
  end; {TRY}
end;

{Try to delete a file safely}
function TryDeleteFile(FileName: String): Boolean;
begin
  Result := deletefile(FileName);
end;

// Returns the name of the app file without path
function AppFileName(): String;
begin
  Result := ExtractFileName(ParamStr(0));
  // For working path: ExtractFilePAth
end;

function CombineTextFiles(FileList: TStringArray; Destination: String;
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
  while Count < Length(FileList) do
  begin
    if FileExists(FileList[Count]) then
    begin
      ThisFile.Clear;
      ThisFile.LoadFromFile(FileList[Count]);
      FinalFile.Add('-----> ' + FileList[Count]);
      Index := 0;
      while Index < ThisFile.Count do
      begin
        FinalFile.Add(ThisFile[index]);
        Inc(index);
      end;
      Inc(added);
    end;
    if DeleteSources then TryDeletefile(FileList[Count]);
    Inc(Count);
  end;
  if Added > 0 then
    FinalFile.SaveToFile(Destination);
  FinalFile.Free;
  ThisFile.Free;
  Result := True;
end;

function SendFileViaTCP(FileName, Message, Host: String; Port: Integer): Boolean;
var
  Client: TidTCPClient;
  MyStream: TMemoryStream;
begin
  Result := True;
  if not fileExists(FileName) then exit(False);
  MyStream := TMemoryStream.Create;
  MyStream.LoadFromFile(FileName);
  Client := TidTCPClient.Create(nil);
  Client.Host := Host;
  Client.Port := Port;
  Client.ConnectTimeout := 1000;
  Client.ReadTimeout := 1000;
  try
    Client.Connect;
    Client.IOHandler.WriteLn(Message);
    Client.IOHandler.Write(MyStream, 0, True);
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDeb('NosoGeneral,SendFile,' + FileName + ' Error: ' + E.Message);
    end;
  end;{Try}
  if client.Connected then Client.Disconnect();
  client.Free;
  MyStream.Free;
end;

// Unzip a zip file and (optional) delete it
function UnzipFile(FileName: String; DeleteFile: Boolean): Boolean;
var
  UnZipper: TUnZipper;
begin
  Result := True;
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := FileName;
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
  if DeleteFile then Trydeletefile(FileName);
  UnZipper.Free;
end;

// Creates an empty file
function CreateEmptyFile(FileName: String): Boolean;
var
  lFile: textfile;
begin
  Result := True;
  try
    Assignfile(lFile, FileName);
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
function GetStringFromOrder(Order: TOrderData): String;
begin
  Result := Order.OrderType + ' ' + Order.OrderID + ' ' +
    IntToStr(Order.OrderLines) + ' ' + Order.OrderType + ' ' +
    IntToStr(Order.TimeStamp) + ' ' + Order.reference + ' ' +
    IntToStr(Order.TrxLine) + ' ' + Order.Sender + ' ' + Order.Address +
    ' ' + Order.Receiver + ' ' + IntToStr(Order.AmountFee) + ' ' +
    IntToStr(Order.AmountTransferred) + ' ' + Order.Signature + ' ' + Order.TransferID;
end;

function ExtractMasternodesText(Text: String): String;
var
  startpos: Integer;
  content: String;
begin
  Result := '';
  startpos := Pos('$', Text);
  Result := Copy(Text, Startpos + 1, Length(Text));
end;

{$ENDREGION Protocol specific}


end.{UNIT}
