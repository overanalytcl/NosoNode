// Unit for general functions.

unit Noso.General;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils, IdTCPClient, IdGlobal, fphttpclient,
  opensslsockets, fileutil, Noso.Debug, Zipper, Math;

type
  // TStream helper class for string manipulation in streams
  TStreamHelper = class helper for TStream
    {
      @abstract(Writes a string to the stream, preceded by its length.)

      @param(S The string to write to the stream.)

      Example:
      @longcode(#
      var
        MyStream: TMemoryStream;
      begin
        MyStream := TMemoryStream.Create;
        try
          MyStream.SetString('Hello, World!');
        finally
          MyStream.Free;
        end;
      end;
      #)
    }
    procedure SetString(const AString: String);

    {
      @abstract(Reads a string from the stream, which was previously written with @link(SetString).)

      @returns(The string read from the stream.)

      Example:
      @longcode(#
      var
        MyStream: TMemoryStream;
        S: String;
      begin
        MyStream := TMemoryStream.Create;
        try
          MyStream.SetString('Hello, World!');
          MyStream.Position := 0;
          S := MyStream.GetString;
          Writeln('Read string: ', S);
        finally
          MyStream.Free;
        end;
      end;
      #)
    }
    function GetString: String;
  end;

  // Dynamic array of strings
  TStringArray = specialize TArray<String>;

  // Record structure for order data
  TOrderData = packed record
    Block: Integer;
    OrderID: String[64];
    OrderLineCount: Integer;
    OrderType: String[6];
    TimeStamp: Int64;
    Reference: String[64];
    TransferLine: Integer;
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
  TBlockOrders = specialize TArray<TOrderData>;

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

{
  @abstract(Returns the custom fee.)
  @param(Block The current block.)
  @returns(The custom fee as an Int64 value.)
}
function GetCustomFee(const Block: Integer): Int64;

{
  @abstract(Returns the stack required for Noso.)
  @param(Block The current block.)
  @returns(The stack size as an Int64 value.)
}
function GetStackRequired(const Block: Integer): Int64;

{
  Calculates the percentage of master nodes based on the current block number and mode.

  @param(Block The current block number.)
  @param(MainnetMode The mode of the mainnet. If it contains 'MNSONLY',
                     the percentage is adjusted for master nodes only.
                     Defaults to 'NORMAL'.)
  @returns(The percentage of master nodes.)
}
function GetMasterNodesPercentage(const Block: Integer;
  const MainnetMode: String = 'NORMAL'): Integer;

{
  @abstract(Returns the PoS percentage.)
  @param(Block The current block.)
  @returns(The PoS percentage for the specified block, as a number from 0 to 10000.)
}
function GetPoSPercentage(const Block: Integer): Integer;

{
  @abstract(Returns the developer percentage.)
  @param(Block The current block.)
  @returns(The developer percentage for the specified block, as a number from 0 to 10000.)
}
function GetDevPercentage(Block: Integer): Integer;

{
  @abstract(Returns the minimum fee that can be paid for a specific amount.)
  @param(Amount The amount to which the fee applies.)
  @returns(The minimum fee.)
}
function GetMinimumFeeForAmount(const Amount: Int64): Int64;

{
  @abstract(Returns the maximum amount that can be sent for a specific amount.)
  @param(Amount The amount.)
  @returns(The maximum amount.)
}
function GetMaximumToSend(const Amount: Int64): Int64;

{
  @abstract(Returns the OS name as a string.)
}
function OSVersion: String;

{
  @abstract(Send a command to peer via TCP and return the response.)

  @param(Host The hostname or IP address of the peer.)
  @param(Port The port number of the peer.)
  @param(Command The command to be sent to the peer.)
  @returns(The response from the peer as a string.)

  Example:
  @longcode(#
  var
    Response: String;
  begin
    Response := RequestLineToPeer('192.168.1.100', 8080, 'GET_INFO');
    if Response <> '' then
      WriteLn('Response from peer: ', Response)
    else
      WriteLn('Failed to get a response from peer.');
  end;
  #)
}
function RequestLineToPeer(const Host: String; const Port: Integer;
  const Command: String): String;

{
  @abstract(Send a command to a peer using a combined Host:Port string.)

  @param(HostAndPort The string in the format 'host:port'.)
  @param(Command The command to send to the peer.)
  @returns(The response from the peer as a string.)

  Example:
  @longcode(#
  var
    Response: String;
  begin
    Response := RequestToPeer('example.com:9000', 'PING');
    if Response <> '' then
      WriteLn('Response from peer: ', Response)
    else
      WriteLn('Failed to get a response from peer.');
  end;
  #)
}
function RequestToPeer(const HostAndPort, Command: String): String;

{
  @abstract(Send HTTP GET request to the specified URL.)
  @param(Url The URL to send the request to.)
  @returns(The response from the API as a string.)

  Example:
  @longcode(#
  var
    ApiResponse: String;
  begin
    ApiResponse := SendApiRequest('https://api.example.com/data');
    if ApiResponse <> '' then
      WriteLn('API Response: ', ApiResponse)
    else
      WriteLn('Failed to get a response from the API.');
  end;
  #)
}
function SendApiRequest(const Url: String): String;

{
  @abstract(Saves a given text to a file on disk.)

  @param(FileName The name and path of the file to save.)
  @param(Text The content to save to the file.)
  @returns(@true if the file was saved successfully, @false otherwise.)

  Example:
  @longcode(#
  var
    Success: Boolean;
  begin
    Success := SaveTextToDisk('example.txt', 'This is some example text.');
    if Success then
      Writeln('File saved successfully!')
    else
      Writeln('Failed to save the file.');
  end;
  #)
}
function SaveTextToDisk(const FileName: TFileName; const Text: String): Boolean;

{
  @abstract(Loads text from a file on disk.)

  @param(FileName The name and path of the file to load.)
  @returns(The content of the file as a string, or an empty string in case of failure.)

  Example:
  @longcode(#
  var
    FileText: String;
  begin
    FileText := LoadTextFromDisk('example.txt');
    if FileText <> '' then
      Writeln('File loaded successfully: ', FileText)
    else
      Writeln('Failed to load the file.');
  end;
  #)
}
function LoadTextFromDisk(const FileName: TFileName): String;

{
  @abstract(Copies a file from source to destination.)

  @param(Source The full path of the source file.)
  @param(Destination The full path of the destination file.)
  @returns(@true if the file was copied successfully, @false otherwise.)
  @warning(This function will overwrite the destination, if it exists.)

  Example:
  @longcode(#
  var
    Success: Boolean;
  begin
    Success := TryCopyFile('source.txt', 'destination.txt');
    if Success then
      Writeln('File copied successfully!')
    else
      Writeln('Failed to copy the file.');
  end;
  #)
}
function TryCopyFile(const Source, Destination: String): Boolean;

{
  @abstract(Attempts to delete a file safely.)

  @param(FileName The name of the file to delete.)
  @returns(@true if the file was deleted successfully, @false otherwise.)

  Example:
  @longcode(#
  var
    Success: Boolean;
  begin
    Success := TryDeleteFile('example.txt');
    if Success then
      Writeln('File deleted successfully!')
    else
      Writeln('Failed to delete the file.');
  end;
  #)
}
function TryDeleteFile(const FileName: String): Boolean;

{
  @abstract(Returns the name of the application executable without the path.)
  @returns(The file name of the application executable.)

  Example:
  @longcode(#
  var
    AppName: String;
  begin
    AppName := AppFileName();
    Writeln('The application file name is: ', AppName);
  end;
  #)
}
function AppFileName(): String;

{
  @abstract(Combines multiple text files into one destination file.)

  @param(FileList An array of file names to combine.)
  @param(Destination The destination file where the combined text will be saved.)
  @param(DeleteSources If @true, the source files will be deleted after they are
         combined. Defaults to @true.)
  @returns(@true if all files were successfully combined and saved, @false otherwise.)

  Example:
  @longcode(#
  var
    Success: Boolean;
    Files: TStringArray;
  begin
    Files := ['file1.txt', 'file2.txt', 'file3.txt'];
    Success := CombineTextFiles(Files, 'combined.txt', True);
    if Success then
      Writeln('Files combined successfully!')
    else
      Writeln('Failed to combine the files.');
  end;
  #)
}
function CombineTextFiles(FileList: TStringArray; const Destination: String;
  DeleteSources: Boolean = True): Boolean;

{
  @abstract(Sends a file to a peer via TCP, preceded by a message.)

  @param(FileName The name of the file to be sent.)
  @param(Message The message to be sent before the file.)
  @param(Host The hostname or IP address of the peer.)
  @param(Port The port number to connect to.)
  @returns(@true if the file was sent successfully, @false otherwise.)

  Example:
  @longcode(#
  var
    Success: Boolean;
  begin
    Success := SendFileViaTCP('example.txt', 'Sending file', '192.168.1.10', 8080);
    if Success then
      Writeln('File sent successfully!')
    else
      Writeln('Failed to send the file.');
  end;
  #)
}
function SendFileViaTCP(const FileName, Message, Host: String;
  const Port: Integer): Boolean;

{
  @abstract(Unzips a file and optionally deletes the source file.)

  @param(FileName The name of the zip file to be extracted.)
  @param(DeleteFile If @true, deletes the source file after extraction.)
  @returns(@true if the file was unzipped successfully, @false otherwise.)

  Example:
  @longcode(#
  var
    Success: Boolean;
  begin
    Success := UnzipFile('archive.zip', True);
    if Success then
      Writeln('File unzipped and deleted successfully!')
    else
      Writeln('Failed to unzip the file.');
  end;
  #)
}
function UnzipFile(const FileName: String; DeleteFile: Boolean): Boolean;

{
  @abstract(Creates an empty file on disk.)

  @param(FileName The name of the empty file to create.)
  @returns(@true if the file was created successfully, @false otherwise.)

  Example:
  @longcode(#
  var
    Success: Boolean;
  begin
    Success := CreateEmptyFile('emptyfile.txt');
    if Success then
      Writeln('Empty file created successfully!')
    else
      Writeln('Failed to create the file.');
  end;
  #)
}
function CreateEmptyFile(const FileName: String): Boolean;

{
  @abstract(Converts an order record to a formatted string.)

  @param(Order The order record to convert.)
  @returns(A string representing the order.)

  Example:
  @longcode(#
  var
    OrderString: String;
    MyOrder: TOrderData;
  begin
    MyOrder := GetOrder(); // Could be any order.
    Writeln('Order as string: ', OrderToString(MyOrder));
  end;
  #)
}
function OrderToString(const Order: TOrderData): String;

{
  @abstract(Extracts the masternode's text in a string.)
  @param(Text The master node text.)
  @returns(The substring after the '$' character.)
}
function ExtractMasternodesText(const Text: String): String;

implementation

procedure TStreamHelper.SetString(const AString: String);
var
  StringSize: Word;
begin
  StringSize := Length(AString);

  WriteBuffer(StringSize, SizeOf(StringSize));
  if StringSize > 0 then
    WriteBuffer(Pointer(AString)^, StringSize);
end;

function TStreamHelper.GetString: String;
var
  StringSize: Word = 0;
begin
  ReadBuffer(StringSize, SizeOf(StringSize));
  SetLength(Result, StringSize);

  // If the string has any data, read it
  if StringSize > 0 then
    ReadBuffer(Pointer(Result)^, StringSize);
end;

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
begin
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

function GetCustomFee(const Block: Integer): Int64;
begin
  if Block < 162000 then
    Result := 2500
  else
    Result := 1000000;
end;

function GetStackRequired(const Block: Integer): Int64;
const
  MaxStackCap = 1100000000000;
  ReducedStackCap = 1050000000000;
begin
  // NOTE: it was 20/10000 = 1/500. Yeah, advanced math for sure. ;)
  Result := GetCirculatingSupply(Block) div 500;

  if Result > MaxStackCap then
    Result := MaxStackCap;

  if Block > 110000 then
    Result := ReducedStackCap;
end;

function GetMasterNodesPercentage(const Block: Integer;
  const MainnetMode: String = 'NORMAL'): Integer;
const
  MasterNodeBlockStart = 48010;
  PoSBlockEnd = 88400;
  MasterNodePercentageStart = 2000;
  MasterNodePercentageMax = 6000;
  MasterNodePercentageMNsOnly = 9000;
begin
  Result := 0;

  if Block >= MasterNodeBlockStart then
  begin
    Result := MasterNodePercentageStart + ((Block - MasterNodeBlockStart) div
      4000) * 100;

    if Block >= PoSBlockEnd then
      Inc(Result, 1000);

    if Result > MasterNodePercentageMax then
      Result := MasterNodePercentageMax;

    if AnsiContainsStr(MainnetMode, 'MNSONLY') then
      Result := MasterNodePercentageMNsOnly;
  end;
end;

function GetPoSPercentage(const Block: Integer): Integer;
const
  PoSPercentage = 1000;
  PoSBlockEnd = 88400;
  PoSBlockStart = 8424;
  PoSPercentageMax = 2000;
begin
  Result := 0;
  if (Block > PoSBlockStart) and (Block < 40000) then
    Result := PoSPercentage;

  if Block >= 40000 then
  begin
    Result := Min(PoSPercentage + ((Block - 39000) div 1000) * 100, PoSPercentageMax);
  end;

  // No more PoS after block 88400... wait, what? Why are you doing it like this,
  // Pedro? Whatever.
  if Block >= PoSBlockEnd then
    Result := 0;
end;

function GetDevPercentage(Block: Integer): Integer;
const
  PoSBlockEnd = 88400;
  DevPercentage = 1000;
begin
  if Block >= PoSBlockEnd then
    Result := DevPercentage
  else
    Result := 0;
end;

function GetMinimumFeeForAmount(const Amount: Int64): Int64;
const
  CommissionTransfer = 10000;
  MinimumFee = 1000000;
begin
  Result := Max(Amount div CommissionTransfer, MinimumFee);
end;

function GetMaximumToSend(const Amount: Int64): Int64;
const
  ComissionTransfer = 10000;
  MinimumFee = 1000000;
var
  MaxSend, Fee, Difference: Int64;
begin
  if Amount < MinimumFee then
    exit(0);

  MaxSend := (Amount * ComissionTransfer) div (ComissionTransfer + 1);
  Fee := MaxSend div ComissionTransfer;

  if Fee < MinimumFee then
    Fee := MinimumFee;

  Difference := Amount - (MaxSend + Fee);
  Result := MaxSend + Difference;
end;

function OSVersion: String;
begin
  {$IFDEF LCLcarbon}
  OSVersion := 'Mac OS X 10.';
  {$ENDIF}

  {$IFDEF UNIX}
  OSVersion := 'Linux ';
  {$ENDIF}

  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ENDIF}

  {$IFDEF WINDOWS}
  OSVersion := 'Windows ';
  {$ENDIF}
end;

function RequestLineToPeer(const Host: String; const Port: Integer;
  const Command: String): String;
var
  Client: TidTCPClient;
begin
  Result := '';
  Client := TIdTCPClient.Create(nil);
  try
    Client.Host := Host;
    Client.Port := Port;
    Client.ConnectTimeout := 1000;
    Client.ReadTimeout := 1000;
    Client.IOHandler.MaxLineLength := MaxInt;
    try
      Client.Connect;
      try
        Client.IOHandler.WriteLn(Command);
        Result := Client.IOHandler.ReadLn();
      finally
        if Client.Connected then
          Client.Disconnect;
      end;
    except
      on E: Exception do
        ToLog('exceps', 'Error during request to peer: ' + E.Message);
    end;
  finally
    Client.Free;
  end;
end;

function RequestToPeer(const HostAndPort, Command: String): String;
var
  Client: TidTCPClient;
begin
  Result := '';
  Client := TidTCPClient.Create(nil);
  try
    Client.Host := GetParameter(HostAndPort, 0);
    Client.Port := StrToIntDef(GetParameter(HostAndPort, 1), 8080);
    Client.ConnectTimeout := 1000;
    Client.ReadTimeout := 1000;
    Client.IOHandler.MaxLineLength := MaxInt;
    try
      Client.Connect;
      try
        Client.IOHandler.WriteLn(Command);
        Result := Client.IOHandler.ReadLn();
      finally
        if Client.Connected then
          Client.Disconnect;
      end;
    except
      on E: Exception do
        ToLog('exceps', 'Error during request to peer: ' + E.Message);
    end;
  finally
    Client.Free;
  end;
end;

function SendApiRequest(const Url: String): String;
var
  Client: TFPHTTPClient;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  try
    Client.ConnectTimeout := 3000;
    Client.IOTimeout := 3000;
    try
      Result := Trim(Client.SimpleGet(Url));
    except
      on E: Exception do
        ToLog('exceps', 'Error during API request: ' + E.Message);
    end;
  finally
    Client.Free;
  end;
end;

function SaveTextToDisk(const FileName: TFileName; const Text: String): Boolean;
var
  Stream: TStringStream;
begin
  Result := True;
  Stream := TStringStream.Create(Text, TEncoding.UTF8);
  try
    try
      Stream.SaveToFile(FileName);
    except
      on E: Exception do
      begin
        Result := False;
        ToDeepDebug('NosoGeneral,SaveTextToDisk,' + E.Message);
      end;
    end
  finally
    Stream.Free;
  end;
end;

function LoadTextFromDisk(const FileName: TFileName): String;
var
  Stream: TStringStream;
begin
  Result := '';
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    try
      Stream.LoadFromFile(FileName);
      Result := Stream.DataString;
    except
      on E: Exception do
      begin
        Result := '';
        ToDeepDebug('NosoGeneral,LoadTextFromDisk,' + E.Message);
      end;
    end
  finally
    Stream.Free;
  end;
end;

function TryCopyFile(const Source, Destination: String): Boolean;
begin
  Result := True;
  try
    CopyFile(Source, Destination, [cffOverwriteFile], True);
  except
    on E: Exception do
    begin
      Result := False;
      ToDeepDebug('NosoGeneral,TryCopyFile,' + E.Message);
    end;
  end;
end;

function TryDeleteFile(const FileName: String): Boolean;
begin
  Result := DeleteFile(FileName);
end;

function AppFileName(): String;
begin
  Result := ExtractFileName(ParamStr(0));
end;

function CombineTextFiles(FileList: TStringArray; const Destination: String;
  DeleteSources: Boolean = True): Boolean;
var
  FinalFile: TStringList;
  CurrentFile: TStringList;
  Index: Integer;
begin
  Result := True;
  FinalFile := TStringList.Create;
  CurrentFile := TStringList.Create;

  try
    for Index := 0 to High(FileList) do
    begin
      if FileExists(FileList[Index]) then
      begin
        try
          CurrentFile.LoadFromFile(FileList[Index]);
          FinalFile.Add('-----> ' + FileList[Index]);
          FinalFile.AddStrings(CurrentFile);
        except
          on E: Exception do
          begin
            ToDeepDebug(Format('NosoGeneral,CombineTextFiles,Error loading file: %s, %s',
              [FileList[Index], E.Message]));
            Result := False;
          end;
        end;

        if DeleteSources then
          TryDeleteFile(FileList[Index]);
      end
      else
      begin
        ToDeepDebug('NosoGeneral,CombineTextFiles, File not found: ' + FileList[Index]);
        Result := False;
      end;
    end;

    if FinalFile.Count > 0 then
    begin
      try
        FinalFile.SaveToFile(Destination);
      except
        on E: Exception do
        begin
          ToDeepDebug('NosoGeneral,CombineTextFiles,Error saving destination file: ' +
            Destination + ', ' + E.Message);
          Result := False;
        end;
      end;
    end;
  finally
    FinalFile.Free;
    CurrentFile.Free;
  end;
end;

function SendFileViaTCP(const FileName, Message, Host: String;
  const Port: Integer): Boolean;
var
  Client: TIdTCPClient;
  Stream: TMemoryStream;
begin
  Result := False;
  if not FileExists(FileName) then Exit;

  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);

    Client := TIdTCPClient.Create(nil);
    try
      Client.Host := Host;
      Client.Port := Port;
      Client.ConnectTimeout := 1000;
      Client.ReadTimeout := 1000;
      Client.Connect;

      Client.IOHandler.WriteLn(Message);
      Client.IOHandler.Write(Stream, 0, True);

      Result := True;
    except
      on E: Exception do
        ToDeepDebug('NosoGeneral,SendFile,' + FileName + ' Error: ' + E.Message);
    end;

    if Client.Connected then
      Client.Disconnect;
  finally
    Client.Free;
    Stream.Free;
  end;
end;

function UnzipFile(const FileName: String; DeleteFile: Boolean): Boolean;
var
  UnZipper: TUnZipper;
begin
  Result := False;

  if not FileExists(FileName) then Exit;

  UnZipper := TUnZipper.Create;
  try
    try
      UnZipper.FileName := FileName;
      UnZipper.OutputPath := '';
      UnZipper.Examine;
      UnZipper.UnZipAllFiles;

      Result := True;

      if DeleteFile then
        TryDeleteFile(FileName);
    except
      on E: Exception do
        ToDeepDebug('NosoGeneral,UnzipFile,' + E.Message);
    end;
  finally
    UnZipper.Free;
  end;
end;

function CreateEmptyFile(const FileName: String): Boolean;
var
  EmptyFile: TextFile;
begin
  Result := True;
  try
    AssignFile(EmptyFile, FileName);
    Rewrite(EmptyFile);
    CloseFile(EmptyFile);
  except
    on E: Exception do
    begin
      ToDeepDebug('NosoGeneral,CreateEmptyFile,' + E.Message);
      Result := False;
    end;
  end;
end;

function OrderToString(const Order: TOrderData): String;
begin
  Result := Format('%s %s %d %s %d %s %d %s %s %s %d %d %s %s',
    [Order.OrderType, Order.OrderID, Order.OrderLineCount, Order.OrderType,
    Order.TimeStamp, Order.Reference, Order.TransferLine, Order.Sender,
    Order.Address, Order.Receiver, Order.AmountFee, Order.AmountTransferred,
    Order.Signature, Order.TransferID]);
end;

function ExtractMasternodesText(const Text: String): String;
var
  StartPos: Integer;
begin
  Result := EmptyStr;
  StartPos := Pos('$', Text);

  if StartPos > 0 then
    Result := Copy(Text, StartPos + 1, Length(Text));
end;

end.{UNIT}
