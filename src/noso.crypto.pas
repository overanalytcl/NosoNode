unit Noso.Crypto;

{
Unit nosocrypto 1.3
December 4th, 2023
Noso Unit for crypto functions
Requires: cryptohashlib , mpsignerutils
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  HlpHashFactory, md5,
  ClpConverters, ClpBigInteger, SbpBase58,
  MP.Utils.Signing, base64, Noso.Debug, Noso.General;

type

  DivResult = packed record
    cociente: String[255];
    residuo: String[255];
  end;

{Hashing functions}
function HashSha256String(StringToHash: String): String;
function HashMD160String(StringToHash: String): String;
function HashMD5String(StringToHash: String): String;
function HashMD5File(FileToHash: String): String;

{General functions}
function IsValid58(base58text: String): Boolean;
function GetStringSigned(StringtoSign, PrivateKey: String): String;
function VerifySignedString(StringToVerify, B64String, PublicKey: String): Boolean;
function GetAddressFromPublicKey(PubKey: String; AddType: Integer = 0): String;
function NewGetAddressFromPublicKey(PubKey: String): String;
function FutureGetAddressFromPublicKey(const PubKey: String): String;
function GenerateNewAddress(out pubkey: String; out privkey: String): String;
function IsValidHashAddress(Address: String): Boolean;
function GetTransferHash(TextLine: String): String;
function GetOrderHash(TextLine: String): String;
function GetCertificateChecksum(certificate: String): String;
function GetCertificate(Pubkey, privkey, currtime: String): String;
function CheckCertificate(certificate: String; out TimeStamp: String): String;
function CheckHashDiff(Target, ThisHash: String): String;
function GetMultiSource(Source: String; AddsTotal: Integer;
  Out OrderedSource: String): Boolean;

{New base conversion functions}
function B10ToB16(const sVal: String): String;
function B10ToB58(const sVal: String): String;
function B16ToB10(const sHex: String): String;
function B16ToB36(const sHex: String): String;
function B16ToB58(const sHex: String): String;

function B16ToB58Lite1_60(const sHex: String): String;

function B58ToB10(const sVal: String): String;
function B58ToB16(const sVal: String): String;
function ChecksumBase58(const S: String): Integer;
function BMB58resumenNew(numero58: String): String;
function BMB58resumen(numero58: String): String;
function BMB58resumenInt(numero58: String): Integer;

// Big Maths
function ClearLeadingCeros(numero: String): String;
function BMAdicion(numero1, numero2: String): String;
function PonerCeros(numero: String; cuantos: Integer): String;
function BMMultiplicar(Numero1, Numero2: String): String;
function BMDividir(Numero1, Numero2: String): DivResult;
function BMExponente(Numero1, Numero2: String): String;
function BMHexToDec(numerohex: String): String;
function BM58ToDec(number58: String): String;
function BMHexTo58(numerohex: String; alphabetnumber: Integer): String;
function BMDecTo58(numero: String): String;
function BMDecToHex(numero: String): String;

const
  HexAlphabet: String = '0123456789ABCDEF';
  B36Alphabet: String = '0123456789abcdefghijklmnopqrstuvwxyz';
  B58Alphabet: String = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
  AddtypeReg = 0;
  AddTypeMulti = 1;

implementation

{$REGION Internal utility methods}
function GetAlphabet(const Base: Byte): String;
  {$ifopt D-}
inline;
  {$endif}
begin
  case Base of
    36: Result := B36Alphabet;
    58: Result := B58Alphabet;
    else
      Result := '';
  end;
end;

function IsValidInput(const S: String): Boolean;
  {$ifopt D-}
inline;
  {$endif}
begin
  Result := (Length(S) <> 0) and (S <> '0');
end;

function EncodeBaseN(const bytes: TBytes; const sAlphabet: String;
  iBase: Int32 = 0): String;
const
  growthPercentage = Int32(154);
var
  bytesLen, numZeroes, outputLen, Length, carry, i, resultLen: Int32;
  inputPtr, pInput, pEnd, outputPtr, pOutputEnd, pDigit, pOutput: Pbyte;
  alphabetPtr, resultPtr, pResult: PChar;
  ZeroChar: Char;
  output: TBytes;
  Value: String;
begin
  Result := '';
  bytesLen := System.Length(bytes);
  if (bytesLen = 0) then
  begin
    Exit;
  end;
  inputPtr := Pbyte(bytes);
  Value := sAlphabet;
  alphabetPtr := PChar(Value);
  pInput := inputPtr;
  pEnd := inputPtr + bytesLen;
  while ((pInput <> pEnd) and (pInput^ = 0)) do
  begin
    System.Inc(pInput);
  end;
  numZeroes := Int32(pInput - inputPtr);
  ZeroChar := alphabetPtr^;
  if (pInput = pEnd) then
  begin
    Result := StringOfChar(ZeroChar, numZeroes);
    Exit;
  end;
  outputLen := bytesLen * growthPercentage div 100 + 1;
  Length := 0;
  System.SetLength(output, outputLen);
  outputPtr := Pbyte(output);
  pOutputEnd := outputPtr + outputLen - 1;
  while (pInput <> pEnd) do
  begin
    carry := pInput^;
    i := 0;
    pDigit := pOutputEnd;
    while (((carry <> 0) or (i < Length)) and (pDigit >= outputPtr)) do
    begin
      carry := carry + (256 * pDigit^);
      pDigit^ := Byte(carry mod iBase);
      carry := carry div iBase;
      System.Dec(pDigit);
      System.Inc(i);
    end;
    Length := i;
    System.Inc(pInput);
  end;
  System.Inc(pOutputEnd);
  pOutput := outputPtr;
  while ((pOutput <> pOutputEnd) and (pOutput^ = 0)) do
  begin
    System.Inc(pOutput);
  end;
  resultLen := {numZeroes +} Int32(pOutputEnd - pOutput);
  Result := StringOfChar(ZeroChar, resultLen);
  resultPtr := PChar(Result);
  pResult := resultPtr {+ numZeroes};
  while (pOutput <> pOutputEnd) do
  begin
    pResult^ := alphabetPtr[pOutput^];
    System.Inc(pOutput);
    System.Inc(pResult);
  end;
end;
{$ENDREGION}

{Returns the hash Sha2-256 of a string}
function HashSha256String(StringToHash: String): String;
begin
  Result := THashFactory.TCrypto.CreateSHA2_256().ComputeString(StringToHash,
    TEncoding.UTF8).ToString();
end;

{Returns the hash RIPMED-160 of a string}
function HashMD160String(StringToHash: String): String;
begin
  Result := THashFactory.TCrypto.CreateRIPEMD160().ComputeString(StringToHash,
    TEncoding.UTF8).ToString();
end;

{Returns the hash MD5 of a string}
function HashMD5String(StringToHash: String): String;
begin
  Result := Uppercase(MD5Print(MD5String(StringToHash)));
end;

{Returns the hash MD5 of a file on disk}
function HashMD5File(FileToHash: String): String;
begin
  Result := Uppercase('d41d8cd98f00b204e9800998ecf8427e');
  try
    Result := UpperCase(MD5Print(MD5File(FileToHash)));
  except
    ON E: Exception do
  end;
end;

{Verify if a string is a valid Base58 one}
function IsValid58(base58text: String): Boolean;
var
  counter: Integer;
begin
  Result := True;
  if Length(base58text) = 0 then Exit(False);
  for counter := 1 to length(base58text) do
    if pos(base58text[counter], B58Alphabet) = 0 then
      Exit(False);
end;

{Signs a message with the given privatekey}
function GetStringSigned(StringtoSign, PrivateKey: String): String;
var
  Signature, MessageAsBytes: TBytes;
begin
  Result := '';
  try
    MessageAsBytes := StrToByte(DecodeStringBase64(StringtoSign));
    Signature := TSignerUtils.SignMessage(MessageAsBytes,
      StrToByte(DecodeStringBase64(PrivateKey)), TKeyType.SECP256K1);
    Result := EncodeStringBase64(ByteToString(Signature));
  except
    Exit;
  end{Try};
end;

{Verify if a signed message is valid}
function VerifySignedString(StringToVerify, B64String, PublicKey: String): Boolean;
var
  Signature, MessageAsBytes: TBytes;
begin
  Result := False;
  try
    MessageAsBytes := StrToByte(DecodeStringBase64(StringToVerify));
    Signature := StrToByte(DecodeStringBase64(B64String));
    Result := TSignerUtils.VerifySignature(Signature, MessageAsBytes,
      StrToByte(DecodeStringBase64(PublicKey)), TKeyType.SECP256K1);
  except
    Exit;
  end{Try};
end;

// Generates the public hash from the public key
function GetAddressFromPublicKey(PubKey: String; AddType: Integer = 0): String;
var
  PubSHAHashed, Hash1, Hash2, clave: String;
  sumatoria: String;
begin
  PubSHAHashed := HashSha256String(PubKey);
  Hash1 := HashMD160String(PubSHAHashed);
  hash1 := BMHexTo58(Hash1, 58);
  sumatoria := BMB58resumen(Hash1);
  clave := BMDecTo58(sumatoria);
  hash2 := hash1 + clave;
  if AddType = 0 then Result := 'N' + hash2
  else if Addtype = 1 then Result := 'M' + Hash2;
end;

function NewGetAddressFromPublicKey(PubKey: String): String;
var
  PubSHAHashed, Hash1, Hash2, clave: String;
  sumatoria: String;
begin
  PubSHAHashed := HashSha256String(PubKey);
  Hash1 := HashMD160String(PubSHAHashed);
  hash1 := B16toB58(Hash1);
  sumatoria := BMB58resumen(Hash1);
  clave := B10toB58(sumatoria);
  hash2 := hash1 + clave;
  Result := 'N' + hash2;
end;

function FutureGetAddressFromPublicKey(const PubKey: String): String;
var
  s_data, s_cksum, s_cksum_hex: Ansistring;
  hashSHA256: String;
  hashRMD160: TBytes;
begin
  Result := EmptyStr;
  if PubKey.IsEmpty then
    Exit;
  { SHA256 PubKey string hash }
  hashSHA256 := THashFactory.TCrypto.CreateSHA2_256.ComputeString(PubKey,
    TEncoding.ANSI).ToString;
  { RIPEMD160 hash of SHA256 PubKey hash }
  hashRMD160 := THashFactory.TCrypto.CreateRIPEMD160.ComputeString(
    hashSHA256, TEncoding.ANSI).GetBytes;
  // Quitar ceros al string aqui
  { Encode RIPEMD160 hash as Base58 string }
  s_data := TBase58.BitCoin.Encode(hashRMD160);
  if s_data[1] = '1' then Delete(s_Data, 1, 1);
  { Get s_data checksum in HEX }
  s_cksum_hex := HexStr(ChecksumBase58(s_data), 4);
  { Encode checksum as Base58 string }
  s_cksum := TBase58.BitCoin.Encode(
    TConverters.ConvertHexStringToBytes(s_cksum_hex));
  { Concat all }
  Result := Concat('N', s_data, s_cksum);
end;

{Generates a new keys pair and returns the hash}
function GenerateNewAddress(out pubkey: String; out privkey: String): String;
var
  KeysPair: TKeyPair;
  IsDone: Boolean = False;
  HashAdd: String;
begin
  Result := '';
  repeat
    KeysPair := TSignerUtils.GenerateECKeyPair(TKeyType.SECP256K1);
    HashAdd := GetAddressFromPublicKey(Keyspair.PublicKey);
    if length(HashAdd) >= 20 then
    begin
      pubkey := Keyspair.PublicKey;
      PrivKey := KeysPair.PrivateKey;
      Result := HashAdd;
      IsDone := True;
    end;
  until IsDone;
end;

{Checks if a string is a valid address hash}
function IsValidHashAddress(Address: String): Boolean;
var
  OrigHash: String;
  FirstChar: String;
begin
  Result := False;
  if length(Address) < 1 then exit;
  FirstChar := address[1];
  if ((length(address) > 20) and ((address[1] = 'N') or (address[1] = 'M'))) then
  begin
    OrigHash := Copy(Address, 2, length(address) - 3);
    if IsValid58(OrigHash) then
      if FirstChar + OrigHash + (B10toB58(BMB58resumen(OrigHash))) = Address then
        Result := True;
  end;
end;

{Returns a transfer hash, base58}
function GetTransferHash(TextLine: String): String;
var
  Resultado: String = '';
begin
  Resultado := HashSHA256String(TextLine);
  Resultado := B16toB58(Resultado);
  Result := 'tR' + Resultado + B10toB58(BMB58resumen(Resultado));
end;

{Returns the Order hash, base36}
function GetOrderHash(TextLine: String): String;
begin
  Result := HashSHA256String(TextLine);
  Result := 'OR' + B16ToB36(Result);
end;

{Returns the Address certificate for the submitted data}
function GetCertificate(Pubkey, privkey, currtime: String): String;
var
  Certificate: String;
  Address: String;
  Signature: String;
  Checksum: String;
begin
  Result := '';
  try
    Address := GetAddressFromPublicKey(Pubkey);
    Signature := GetStringSigned('OWN' + Address + currtime, PrivKey);
    Certificate := 'OWN:' + Pubkey + ':' + Currtime + ':' + signature;
    //ToLog('console','To encode: '+certificate);
    Certificate := UPPERCASE(XorEncode(HashSha256String('noso'), certificate));
    //ToLog('console','Encoded: '+certificate);
    Result := B16ToB58('1' + Certificate);
    Checksum := GetCertificateChecksum(Result);
    //ToLog('console','Checksum: '+Checksum);
    Result := Result + Checksum;
  except
    Exit;
  end; {TRY}
end;

{Verify if a given certificate is valid and returns the address and timestamp}
function CheckCertificate(certificate: String; out TimeStamp: String): String;
var
  DataArray: array of String;
  Address: String;
  CertTime: String;
  Signature: String;
  CheckSum: String;
begin
  Result := '';
  try
    Checksum := copy(Certificate, length(certificate) - 2, 3);
    Certificate := copy(Certificate, 1, length(certificate) - 3);
    if CheckSum <> GetCertificateChecksum(Certificate) then exit;
    Certificate := B58toB16(certificate);
    Certificate := copy(Certificate, 2, length(certificate));
    //ToLog('console','To decode: '+certificate);
    Certificate := XorDecode(HashSha256String('noso'), Certificate);
    //ToLog('console','Decoded: '+certificate);
    DataArray := SplitString(Certificate, ':');
    Address := GetAddressFromPublicKey(DataArray[1]);
    CertTime := DataArray[2];
    Signature := DataArray[3];
    if VerifySignedString('OWN' + Address + CertTime, Signature, DataArray[1]) then
      {Verified certificate}
    begin
      TimeStamp := CertTime;
      Result := Address;
    end;
  except
    Exit;
  end; {TRY}
end;

{Verify the difference between MD5 hashes}
function CheckHashDiff(Target, ThisHash: String): String;
var
  counter: Integer;
  ValA, ValB, Diference: Integer;
  ResChar: String;
  Resultado: String = '';
begin
  Result := 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF';
  if Length(Target) < 32 then SetLength(Target, 32);
  if Length(ThisHash) < 32 then SetLength(ThisHash, 32);
  for counter := 1 to 32 do
  begin
    ValA := Hex2Dec(ThisHash[counter]);
    ValB := Hex2Dec(Target[counter]);
    Diference := Abs(ValA - ValB);
    ResChar := UPPERCASE(IntToHex(Diference, 1));
    Resultado := Resultado + ResChar;
  end;
  Result := Resultado;
end;


//getmulti 1,2 N31ThXmSR2gua5tfCFNLEkYK9yNQsDi,N31ThXmSR2gua5tfCFNLEkYK9yNQsDi
function GetMultiSource(Source: String; AddsTotal: Integer;
  Out OrderedSource: String): Boolean;
var
  Addresses: array of String;
  Counter: Integer = 0;
  Counter2: Integer = 0;
  ThisSour: String;
  Inserted: Boolean = False;
  LocSource: String;
  Existing: String = '';
begin
  Result := False;
  OrderedSource := '';
  LocSource := StringReplace(Source, ',', ' ', [rfReplaceAll, rfIgnoreCase]);
  SetLength(Addresses, 0);
  repeat
    Inserted := False;
    ThisSour := Parameter(LocSource, counter);
    if thisSour <> '' then
    begin
      if AnsiContainsStr(Existing, ThisSour) then
      begin
        OrderedSource := 'Duplicated address -> ' + ThisSour;
        Exit;
      end;
      Existing := Existing + ',' + thisSour;
      if not IsValidHashAddress(ThisSour) then
      begin
        OrderedSource := 'Invalid hash address -> ' + ThisSour;
        Exit;
      end;
      if length(Addresses) = 0 then Insert(ThisSour, Addresses, 0)
      else
      begin
        for counter2 := 0 to length(addresses) - 1 do
        begin
          if ThisSour < Addresses[counter2] then
          begin
            Insert(ThisSour, Addresses, counter2);
            Inserted := True;
          end;
        end;
        if not inserted then Insert(ThisSour, Addresses, length(Addresses));
      end;
    end;
    Inc(Counter);
  until ((ThisSour = '') or (Counter = AddsTotal));
  if length(Addresses) <> AddsTotal then
    OrderedSource := format('Wrong addresses count %d/%d',
      [length(addresses), AddsTotal]);
  if OrderedSource = '' then
  begin
    for counter := 0 to length(Addresses) - 1 do
    begin
      OrderedSource := OrderedSource + Addresses[counter];
      if counter < LEngth(addresses) - 1 then OrderedSource := OrderedSource + ',';
    end;
    Result := True;
  end;
end;

{** New base conversion functions **}
function TrimLeadingCeros(const S: String): String;
begin
  Result := S.Trim;
  if Result[1] = '0' then
    Result := Result.TrimLeft('0');
end;

function B10ToB16(const sVal: String): String;
var
  S: String;
begin
  Result := '0';
  S := sVal.Trim;
  if (Length(S) = 0) then
    Exit;
  try
    Result := TConverters.ConvertBytesToHexString(
      TBigInteger.Create(S).ToByteArrayUnsigned, False);
    Result := TrimLeadingCeros(Result);
  except
    Exit; { convert or Parser Errors }
  end;
end;

function B10ToB58(const sVal: String): String;
var
  S: String;
begin
  Result := '1';
  S := sVal.Trim;
  if (Length(S) = 0) then
    Exit;
  Result := TBase58.BitCoin.Encode(TBigInteger.Create(S).ToByteArrayUnsigned);
end;

function B16ToB10(const sHex: String): String;
var
  bytes: TBytes;
  S: String;
begin
  Result := '0';
  S := sHex.Trim;
  if (Length(S) = 0) then
    Exit;
  if (Length(S) mod 2) <> 0 then
    S := Concat('0', S);
  try
    bytes := TConverters.ConvertHexStringToBytes(S);
  except
    Exit { invalid HEX input }
  end;
  Result := TBigInteger.Create(1, bytes).ToString;
end;

function B16ToB36(const sHex: String): String;
const
  baseLength = 36;
var
  bytes: TBytes;
  S: String;
begin
  Result := '';
  S := sHex.Trim;
  if not IsValidInput(S) then
    Exit('0');
  if (Length(S) mod 2) <> 0 then
    S := Concat('0', S);
  try
    bytes := TConverters.ConvertHexStringToBytes(S);
  except
    Exit('0') { invalid HEX input }
  end;
  Result := EncodeBaseN(bytes, GetAlphabet(baseLength), baseLength);
end;

function B16ToB58(const sHex: String): String;
var
  bytes: TBytes;
  S: String;
begin
  Result := '1';
  S := sHex.Trim;
  if (Length(S) = 0) then
    Exit;
  if (Length(S) mod 2) <> 0 then
    S := Concat('0', S);
  try
    bytes := TConverters.ConvertHexStringToBytes(S);
  except
    Exit { invalid HEX input }
  end;
  Result := TBase58.BitCoin.Encode(TBigInteger.Create(bytes).ToByteArrayUnsigned);
end;

function B16ToB58Lite1_60(const sHex: String): String;
var
  bytes: TBytes;
  S: String;
begin
  Result := '1';
  S := sHex.Trim;
  if (Length(S) = 0) then
    Exit;
  if (Length(S) mod 2) <> 0 then
    S := Concat('0', S);
  try
    bytes := TConverters.ConvertHexStringToBytes(S);
  except
    Exit { invalid HEX input }
  end;
  Result := TBase58.BitCoin.Encode(TBigInteger.Create(bytes).ToByteArrayUnsigned);
end;

function B58ToB10(const sVal: String): String;
var
  bytes: TBytes;
  S: String;
begin
  Result := '0';
  S := sVal.Trim;
  if (Length(S) = 0) then
    Exit;
  try
    bytes := TBase58.BitCoin.Decode(S);
  except
    Exit
  end;
  Result := TBigInteger.Create(1, bytes).ToString;
end;

function B58ToB16(const sVal: String): String;
var
  bytes: TBytes;
  S: String;
begin
  Result := '0';
  S := sVal.Trim;
  if (Length(S) = 0) then
    Exit;
  try
    bytes := TBase58.BitCoin.Decode(S);
  except
    Exit
  end;
  Result := TConverters.ConvertBytesToHexString(TBigInteger.Create(1,
    bytes).ToByteArrayUnsigned, False);
  Result := TrimLeadingCeros(Result);
end;

function ChecksumBase58(const S: String): Integer;
var
  C: Char;
  Total: Integer = 0;
begin
  for C in S do
    Inc(Total, Pos(C, B58Alphabet) - 1);
  Result := Total;
end;

function GetCertificateChecksum(certificate: String): String;
begin
  Result := BMB58resumenNew(Certificate);
  Result := B10ToB58(Result);
  if Length(Result) < 3 then AddChar('1', Result, 3);
end;

// RETURN THE SUMATORY OF A BASE58
function BMB58resumenNew(numero58: String): String;
var
  counter, total: Integer;
begin
  total := 0;
  for counter := 1 to length(numero58) do
  begin
    total := total + Pos(numero58[counter], B58Alphabet) - 1;
  end;
  Total := Total + length(numero58);
  Result := IntToStr(total);
end;


// RETURN THE SUMATORY OF A BASE58
function BMB58resumen(numero58: String): String;
var
  counter, total: Integer;
begin
  total := 0;
  for counter := 1 to length(numero58) do
  begin
    total := total + Pos(numero58[counter], B58Alphabet) - 1;
  end;
  Result := IntToStr(total);
end;

// RETURN THE SUMATORY OF A BASE58
function BMB58resumenInt(numero58: String): Integer;
var
  counter, total: Integer;
begin
  total := 0;
  for counter := 1 to length(numero58) do
  begin
    total := total + Pos(numero58[counter], B58Alphabet) - 1;
  end;
  Result := total;
end;

{$REGION Big maths}

 // *****************************************************************************
 // ***************************FUNCTIONS OF BIGMATHS*****************************
 // *****************************************************************************

// REMOVES LEFT CEROS
function ClearLeadingCeros(numero: String): String;
var
  Count: Integer = 0;
  movepos: Integer = 0;
begin
  Result := '';
  if numero[1] = '-' then movepos := 1;
  for Count := 1 + movepos to length(numero) do
  begin
    if numero[Count] <> '0' then Result := Result + numero[Count];
    if ((numero[Count] = '0') and (length(Result) > 0)) then
      Result := Result + numero[Count];
  end;
  if Result = '' then Result := '0';
  if ((movepos = 1) and (Result <> '0')) then Result := '-' + Result;
end;

// ADDS 2 NUMBERS
function BMAdicion(numero1, numero2: String): String;
var
  longitude: Integer = 0;
  Count: Integer = 0;
  carry: Integer = 0;
  resultado: String = '';
  thiscol: Integer;
  ceros: Integer;
begin
  longitude := length(numero1);
  if length(numero2) > longitude then
  begin
    longitude := length(numero2);
    ceros := length(numero2) - length(numero1);
    while Count < ceros do
    begin
      numero1 := '0' + numero1;
      Count := Count + 1;
    end;
  end
  else
  begin
    ceros := length(numero1) - length(numero2);
    while Count < ceros do
    begin
      numero2 := '0' + numero2;
      Count := Count + 1;
    end;
  end;
  for Count := longitude downto 1 do
  begin
    thiscol := StrToInt(numero1[Count]) + StrToInt(numero2[Count]) + carry;
    carry := 0;
    if thiscol > 9 then
    begin
      thiscol := thiscol - 10;
      carry := 1;
    end;
    resultado := IntToStr(thiscol) + resultado;
  end;
  if carry > 0 then resultado := '1' + resultado;
  Result := resultado;
end;

// DRAW CEROS FOR MULTIPLICATION
function PonerCeros(numero: String; cuantos: Integer): String;
var
  contador: Integer = 0;
  NewNumber: String;
begin
  NewNumber := numero;
  while contador < cuantos do
  begin
    NewNumber := NewNumber + '0';
    contador := contador + 1;
  end;
  Result := NewNumber;
end;

// MULTIPLIER
function BMMultiplicar(Numero1, Numero2: String): String;
var
  Count, count2: Integer;
  sumandos: array of String;
  thiscol: Integer;
  carry: Integer = 0;
  cantidaddeceros: Integer = 0;
  TotalSuma: String = '0';
begin
  setlength(sumandos, length(numero2));
  for Count := length(numero2) downto 1 do
  begin
    for count2 := length(numero1) downto 1 do
    begin
      thiscol := (StrToInt(numero2[Count]) * StrToInt(numero1[count2]) + carry);
      carry := thiscol div 10;
      ThisCol := ThisCol - (carry * 10);
      sumandos[cantidaddeceros] := IntToStr(thiscol) + sumandos[cantidaddeceros];
    end;
    if carry > 0 then sumandos[cantidaddeceros] :=
        IntToStr(carry) + sumandos[cantidaddeceros];
    carry := 0;
    sumandos[cantidaddeceros] := PonerCeros(sumandos[cantidaddeceros], cantidaddeceros);
    cantidaddeceros := cantidaddeceros + 1;
  end;
  for Count := 0 to length(sumandos) - 1 do
    TotalSuma := BMAdicion(Sumandos[Count], totalsuma);
  Result := TotalSuma;//ClearLeadingCeros(TotalSuma);
end;

// DIVIDES TWO NUMBERS
function BMDividir(Numero1, Numero2: String): DivResult;
var
  counter: Integer;
  cociente: String = '';
  long: Integer;
  Divisor: Int64;
  ThisStep: String = '';
begin
  long := length(numero1);
  Divisor := StrToInt64(numero2);
  for counter := 1 to long do
  begin
    ThisStep := ThisStep + Numero1[counter];
    if StrToInt(ThisStep) >= Divisor then
    begin
      cociente := cociente + IntToStr(StrToInt(ThisStep) div Divisor);
      ThisStep := (IntToStr(StrToInt(ThisStep) mod Divisor));
    end
    else
      cociente := cociente + '0';
  end;
  Result.cociente := ClearLeadingCeros(cociente);
  Result.residuo := ClearLeadingCeros(thisstep);
end;

// CALCULATES A EXPONENTIAL NUMBER
function BMExponente(Numero1, Numero2: String): String;
var
  Count: Integer = 0;
  resultado: String = '';
begin
  if numero2 = '1' then Result := numero1
  else if numero2 = '0' then Result := '1'
  else
  begin
    resultado := numero1;
    for Count := 2 to StrToInt(numero2) do
      resultado := BMMultiplicar(resultado, numero1);
    Result := resultado;
  end;
end;

// HEX TO DECIMAL
function BMHexToDec(numerohex: String): String;
var
  DecValues: array of Integer;
  ExpValues: array of String;
  MultipliValues: array of String;
  counter: Integer;
  Long: Integer;
  Resultado: String = '0';
begin
  Long := length(numerohex);
  numerohex := uppercase(numerohex);
  setlength(DecValues, 0);
  setlength(ExpValues, 0);
  setlength(MultipliValues, 0);
  setlength(DecValues, Long);
  setlength(ExpValues, Long);
  setlength(MultipliValues, Long);
  for counter := 1 to Long do
    DecValues[counter - 1] := Pos(NumeroHex[counter], HexAlphabet) - 1;
  for counter := 1 to long do
    ExpValues[counter - 1] := BMExponente('16', IntToStr(long - counter));
  for counter := 1 to Long do
    MultipliValues[counter - 1] :=
      BMMultiplicar(ExpValues[counter - 1], IntToStr(DecValues[counter - 1]));
  for counter := 1 to long do
    Resultado := BMAdicion(resultado, MultipliValues[counter - 1]);
  Result := resultado;
end;

function BM58ToDec(number58: String): String;
var
  long, counter: Integer;
  Resultado: String = '0';
  DecValues: array of Integer;
  ExpValues: array of String;
  MultipliValues: array of String;
begin
  Long := length(number58);
  setlength(DecValues, 0);
  setlength(ExpValues, 0);
  setlength(MultipliValues, 0);
  setlength(DecValues, Long);
  setlength(ExpValues, Long);
  setlength(MultipliValues, Long);
  for counter := 1 to Long do
    DecValues[counter - 1] := Pos(number58[counter], B58Alphabet) - 1;
  for counter := 1 to long do
    ExpValues[counter - 1] := BMExponente('58', IntToStr(long - counter));
  for counter := 1 to Long do
    MultipliValues[counter - 1] :=
      BMMultiplicar(ExpValues[counter - 1], IntToStr(DecValues[counter - 1]));
  for counter := 1 to long do
    Resultado := BMAdicion(resultado, MultipliValues[counter - 1]);
  Result := resultado;
end;

// Hex to base 58
function BMHexTo58(numerohex: String; alphabetnumber: Integer): String;
var
  decimalvalue: String;
  restante: Integer;
  ResultadoDiv: DivResult;
  Resultado: String = '';
  AlpahbetUsed: String;
begin
  AlpahbetUsed := B58Alphabet;
  if alphabetnumber = 36 then AlpahbetUsed := B36Alphabet;
  decimalvalue := BMHexToDec(numerohex);
  while length(decimalvalue) >= 2 do
  begin
    ResultadoDiv := BMDividir(decimalvalue, IntToStr(alphabetnumber));
    DecimalValue := Resultadodiv.cociente;
    restante := StrToInt(ResultadoDiv.residuo);
    resultado := AlpahbetUsed[restante + 1] + resultado;
  end;
  if StrToInt(decimalValue) >= alphabetnumber then
  begin
    ResultadoDiv := BMDividir(decimalvalue, IntToStr(alphabetnumber));
    DecimalValue := Resultadodiv.cociente;
    restante := StrToInt(ResultadoDiv.residuo);
    resultado := AlpahbetUsed[restante + 1] + resultado;
  end;
  if StrToInt(decimalvalue) > 0 then
    resultado := AlpahbetUsed[StrToInt(decimalvalue) + 1] + resultado;
  Result := resultado;
end;

// CONVERTS A DECIMAL VALUE TO A BASE58 STRING
function BMDecTo58(numero: String): String;
var
  decimalvalue: String;
  restante: Integer;
  ResultadoDiv: DivResult;
  Resultado: String = '';
begin
  decimalvalue := numero;
  while length(decimalvalue) >= 2 do
  begin
    ResultadoDiv := BMDividir(decimalvalue, '58');
    DecimalValue := Resultadodiv.cociente;
    restante := StrToInt(ResultadoDiv.residuo);
    resultado := B58Alphabet[restante + 1] + resultado;
  end;
  if StrToInt(decimalValue) >= 58 then
  begin
    ResultadoDiv := BMDividir(decimalvalue, '58');
    DecimalValue := Resultadodiv.cociente;
    restante := StrToInt(ResultadoDiv.residuo);
    resultado := B58Alphabet[restante + 1] + resultado;
  end;
  if StrToInt(decimalvalue) > 0 then
    resultado := B58Alphabet[StrToInt(decimalvalue) + 1] + resultado;
  Result := resultado;
end;

// CONVERTS A DECIMAL VALUE TO A HEX STRING
function BMDecToHex(numero: String): String;
var
  decimalvalue: String;
  restante: Integer;
  ResultadoDiv: DivResult;
  Resultado: String = '';
begin
  decimalvalue := numero;
  while length(decimalvalue) >= 2 do
  begin
    ResultadoDiv := BMDividir(decimalvalue, '16');
    DecimalValue := Resultadodiv.cociente;
    restante := StrToInt(ResultadoDiv.residuo);
    resultado := HexAlphabet[restante + 1] + resultado;
  end;
  if StrToInt(decimalValue) >= 16 then
  begin
    ResultadoDiv := BMDividir(decimalvalue, '16');
    DecimalValue := Resultadodiv.cociente;
    restante := StrToInt(ResultadoDiv.residuo);
    resultado := HexAlphabet[restante + 1] + resultado;
  end;
  if StrToInt(decimalvalue) > 0 then
    resultado := HexAlphabet[StrToInt(decimalvalue) + 1] + resultado;
  Result := resultado;
end;

{$ENDREGION BigMaths}

end.{Unit}
