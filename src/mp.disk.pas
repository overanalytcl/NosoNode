unit MP.Disk;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MPForm, Dialogs, Forms, Noso.Time, FileUtil, LCLType,
  lclintf, Controls, MP.Block, Zipper, MP.Coin, Noso.Debug,
  translation, strutils, Noso.General, Noso.Crypto, Noso.Summary, Noso.Consensus, Noso.Pso,
  Noso.WallCon, Noso.Headers, Noso.Config, Noso.Block, Noso.Network,
  Noso.Masternodes, Noso.Gvt;

function FileStructure(): Integer;
procedure VerifyFiles();

 // *** New files system
 // Nodes file
 //Procedure FillNodeList();
 //Function IsSeedNode(IP:String):boolean;

 // GVTs file handling
 //Procedure CreateGVTsFile();
 //Procedure GetGVTsFileData();
 //Procedure SaveGVTs();
 //Function ChangeGVTOwner(Lnumber:integer;OldOwner,NewOWner:String): integer;
 //Function CountAvailableGVTs():Integer;
 //Function GetGVTPrice(available:integer;ToSell:boolean = false):int64;


//Procedure CreateMasterNodesFile();
procedure CreateADV(saving: Boolean);
procedure LoadADV();
 //Function GetLanguage():string;
 //Procedure ExtractPoFiles();
 //Procedure CreateFileFromResource(resourcename,filename:string);


//Procedure UpdateBotData(IPUser:String);

// sumary
procedure UpdateWalletFromSumario();
procedure RebuildSummary();
procedure AddBlockToSumary(BlockNumber: Integer; SaveAndUpdate: Boolean = True);
procedure CompleteSumary();

procedure SaveUpdatedFiles();

 //function GetMyLastUpdatedBlock():int64;
 //Function CreateProperlyClosedAppFile(filename:String):Boolean;

//Function UnzipBlockFile(filename:String;delFile:boolean):boolean;
function UnZipUpdateFromRepo(Tver, TArch: String): Boolean;

procedure CreateLauncherFile(IncludeUpdate: Boolean = False);
procedure RestartNoso();
procedure RestoreBlockChain();
procedure RestoreSumary(fromBlock: Integer = 0);
//function AppFileName():string;

implementation

uses
  MP.Parser, MP.Gui, MP.Red, MP.Protocol;

// Builds the file structure
function FileStructure(): Integer;
begin
  Result := 0;
  if not directoryexists('NOSODATA') then
    if not CreateDir('NOSODATA') then Inc(Result);
  if not directoryexists(LogsDirectory) then
    if not CreateDir(LogsDirectory) then Inc(Result);
  if not directoryexists(BlockDirectory) then
    if not CreateDir(BlockDirectory) then Inc(Result);
  if not directoryexists(UpdatesDirectory) then
    if not CreateDir(UpdatesDirectory) then Inc(Result);
  if not directoryexists(MarksDirectory) then
    if not CreateDir(MarksDirectory) then Inc(Result);
  if not directoryexists(GVTMarksDirectory) then
    if not CreateDir(GVTMarksDirectory) then Inc(Result);
  if not directoryexists(RPCBakDirectory) then
    if not CreateDir(RPCBakDirectory) then Inc(Result);
  if not directoryexists(BlockDirectory + DBDirectory) then
    if not CreateDir(BlockDirectory + DBDirectory) then Inc(Result);
end;

// Complete file verification
procedure VerifyFiles();
var
  defseeds: String = '';
begin
  SetHeadersFileName('NOSODATA' + DirectorySeparator + 'blchhead.nos');
  //if not Fileexists(ResumenFilename) then CreateHeadersFile();
  OutText('✓ Headers file ok', False, 1);

  if not FileExists(AdvOptionsFilename) then CreateADV(False)
  else
    LoadADV();
  OutText('✓ Advanced options loaded', False, 1);

  SetMasternodesFilename('NOSODATA' + DirectorySeparator + 'masternodes.txt');
  //if not FileExists(MasterNodesFilename) then CreateMasterNodesFile;
  //LoadMNsFile;
  OutText('✓ Masternodes file ok', False, 1);

  if not FileExists(GVTsFilename) then CreateGVTsFile;
  GetGVTsFileData;
  OutText('✓ GVTs file ok', False, 1);

  SetCFGFilename('NOSODATA' + DirectorySeparator + CFGFilename);
  {
  if not FileExists(CFGFilename) then
    begin
    SaveCFGToFile(DefaultNosoCFG);
    GetCFGFromFile;
    Defseeds := GetRepoFile('https://raw.githubusercontent.com/nosocoin/NosoNode/main/defseeds.nos');
   // if DefSeeds = '' then Defseeds := GetRepoFile('https://api.nosocoin.com/nodes/seed');
    if defseeds <> '' then
      begin
      SetCFGData(Defseeds,1);
      Tolog('console','Defaults seeds downloaded from trustable source');
      end
    else ToLog('console','Unable to download default seeds. Please, use a fallback');
    end;
  GetCFGFromFile;
  }
  OutText('✓ NosoCFG file ok', False, 1);


  if not SetWalletFilename('NOSODATA' + DirectorySeparator + 'wallet.pkw') then
    S_AdvOpt := True;
  {
  if not FileExists (WalletFilename) then
    begin
    CreateNewWallet;
    S_AdvOpt := true;
    end
  else LoadWallet(WalletFilename);
  }
  OutText('✓ Wallet file ok', False, 1);

  FillNodeList;  // Fills the hardcoded seed nodes list

  if not Fileexists(SummaryFileName) then
    CreateNewSummaryFile(FileExists(BlockDirectory + '0.blk'));
  CreateSumaryIndex();
  OutText('✓ Sumary file ok', False, 1);


  if not FileExists(BlockDirectory + DBDirectory + DataBaseFilename) then CreateDBFile;
  OutText('✓ Database file ok.', False, 1);
  if WO_BlockDB then
  begin
    OutText('✓ Loading blocks Database.', False, 1);
    CreateOrderIDIndex;
  end;
  if not FileExists(BlockDirectory + '0.blk') then CrearBloqueCero();
  MyLastBlock := GetMyLastUpdatedBlock;
  OutText('✓ My last block verified: ' + MyLastBlock.ToString, False, 1);

  UpdateWalletFromSumario();
  OutText('✓ Wallet updated', False, 1);
  ImportAddressesFromBackup(RPCBakDirectory);

  LoadPSOFileFromDisk;
end;

 // ***********************
 // *** NEW FILE SYSTEM *** (0.2.0N and higher)
 // ***********************

// *** NODE FILE ***

{
// Fills hardcoded seed nodes list
Procedure FillNodeList(); // 0.2.1Lb2 revisited
var
  counter : integer;
  ThisNode : string = '';
  Thisport  : integer;
  continuar : boolean = true;
  NodeToAdd : TNodeData;
  SourceStr : String = '';
Begin
counter := 0;
SourceStr := GetParameter(GetCFGDataStr,1)+GetVerificatorsText;
//ToLog('console',sourcestr);
SourceStr := StringReplace(SourceStr,':',' ',[rfReplaceAll, rfIgnoreCase]);
SetLength(ListaNodos,0);
Repeat
   ThisNode := GetParameter(SourceStr,counter);
   ThisNode := StringReplace(ThisNode,';',' ',[rfReplaceAll, rfIgnoreCase]);
   ThisPort := StrToIntDef(GetParameter(ThisNode,1),8080);
   ThisNode := GetParameter(ThisNode,0);
   if thisnode = '' then continuar := false
   else
      begin
      NodeToAdd.ip:=ThisNode;
      NodeToAdd.port:=IntToStr(ThisPort);
      NodeToAdd.LastConexion:=UTCTimeStr;
      Insert(NodeToAdd,Listanodos,Length(ListaNodos));
      counter+=1;
      end;
until not continuar;
End;
}
{
// If the specified IP a seed node
Function IsSeedNode(IP:String):boolean;
Begin
  Result := false;
  if AnsiContainsStr(GetCFGDataStr(1),ip) then result := true;
End;
}
{
Procedure CreateMasterNodesFile();
var
  archivo : textfile;
Begin
TRY
Assignfile(archivo, MAsternodesfilename);
rewrite(archivo);
Closefile(archivo);
EXCEPT on E:Exception do
  ToLog('events',TimeToStr(now)+'Error creating the masternodes file');
END;
End;
}

 // *** OPTIONS FILE ***
 // *****************************************************************************
{$REGION OPTIONS}

// Creates/Saves Advopt file
procedure CreateADV(saving: Boolean);
begin
  BeginPerformance('CreateADV');
  try
    Assignfile(FileAdvOptions, AdvOptionsFilename);
    rewrite(FileAdvOptions);
    writeln(FileAdvOptions, '---NosoNode config file.---');
    writeln(FileAdvOptions, '');

    writeln(FileAdvOptions, '---Wallet related.---');
    writeln(FileAdvOptions, '//Hide empty addresses');
    writeln(FileAdvOptions, 'HideEmpty ' + BoolToStr(WO_HideEmpty, True));
    writeln(FileAdvOptions, '//Use all addresses to send funds');
    writeln(FileAdvOptions, 'MultiSend ' + BoolToStr(WO_MultiSend, True));
    writeln(FileAdvOptions, '//Po files language code');
    writeln(FileAdvOptions, 'Language ' + (WO_Language));
    writeln(FileAdvOptions, '//No GUI refresh');
    writeln(FileAdvOptions, 'NoGUI ' + BoolToStr(WO_StopGUI, True));
    writeln(FileAdvOptions, '//Po files last update');
    writeln(FileAdvOptions, 'PoUpdate ' + (WO_LastPoUpdate));
    writeln(FileAdvOptions, '//Close the launch form automatically');
    writeln(FileAdvOptions, 'Closestart ' + BoolToStr(WO_CloseStart, True));
    writeln(FileAdvOptions, '//Send anonymous report to developers');
    writeln(FileAdvOptions, 'SendReport ' + BoolToStr(WO_SendReport, True));
    writeln(FileAdvOptions, '//Keep a blocks database');
    writeln(FileAdvOptions, 'BlocksDB ' + BoolToStr(WO_BlockDB, True));
    writeln(FileAdvOptions, '//Restart periodically the node');
    writeln(FileAdvOptions, 'PRestart ' + IntToStr(WO_PRestart));
    writeln(FileAdvOptions, '//Skip new blocks creation');
    writeln(FileAdvOptions, 'SkipBlocks ' + BoolToStr(WO_skipBlocks, True));

    writeln(FileAdvOptions,
      '//Mainform coordinates. Do not manually change this values');
    writeln(FileAdvOptions, Format('FormState %d %d %d %d %d',
      [Form1.Top, form1.Left, form1.Width, form1.Height, form1.WindowState]));
    writeln(FileAdvOptions, '');

    writeln(FileAdvOptions, '---Masternode---');
    writeln(FileAdvOptions, '//Enable node server at start');
    writeln(FileAdvOptions, 'Autoserver ' + BoolToStr(WO_AutoServer, True));
    writeln(FileAdvOptions, '//Run autoupdate directives');
    writeln(FileAdvOptions, 'Autoupdate ' + BoolToStr(WO_AutoUpdate, True));
    writeln(FileAdvOptions, '//Download the complete blockchain');
    writeln(FileAdvOptions, 'WO_FullNode ' + BoolToStr(WO_FullNode, True));
    writeln(FileAdvOptions, '//Masternode static IP');
    writeln(FileAdvOptions, 'MNIP ' + (LocalMN_IP));
    writeln(FileAdvOptions, '//Masternode port');
    writeln(FileAdvOptions, 'MNPort ' + (LocalMN_Port));
    writeln(FileAdvOptions, '//Masternode funds address');
    writeln(FileAdvOptions, 'MNFunds ' + (LocalMN_Funds));
    if LocalMN_Sign = '' then LocalMN_Sign := GetWallArrIndex(0).Hash;
    writeln(FileAdvOptions, '//Masternode sign address');
    writeln(FileAdvOptions, 'MNSign ' + LocalMN_Sign);
    writeln(FileAdvOptions, '//Use automatic IP detection for masternode');
    writeln(FileAdvOptions, 'MNAutoIp ' + BoolToStr(MN_AutoIP, True));
    writeln(FileAdvOptions, '');

    writeln(FileAdvOptions, '---RPC server---');
    writeln(FileAdvOptions, '//RPC server port');
    writeln(FileAdvOptions, 'RPCPort ' + IntToStr(RPCPort));
    writeln(FileAdvOptions, '//RPC server password');
    writeln(FileAdvOptions, 'RPCPass ' + RPCPass);
    writeln(FileAdvOptions, '//RPC IP filter active/inactive');
    writeln(FileAdvOptions, 'RPCFilter ' + BoolToStr(RPCFilter, True));
    writeln(FileAdvOptions, '//RPC whitelisted IPs');
    writeln(FileAdvOptions, 'RPCWhiteList ' + RPCWhitelist);
    writeln(FileAdvOptions, '//Enable RPC server at start');
    writeln(FileAdvOptions, 'RPCAuto ' + BoolToStr(RPCAuto, True));
    writeln(FileAdvOptions, '//Save addresses keys created on a BAK folder');
    writeln(FileAdvOptions, 'RPCSaveNew ' + BoolToStr(RPCSaveNew, True));
    writeln(FileAdvOptions, '//Banned methods for RPC requests');
    writeln(FileAdvOptions, 'RPCBanned ' + RPCBanned);
    writeln(FileAdvOptions, '');

    writeln(FileAdvOptions, '---Deprecated. To be removed.---');
    writeln(FileAdvOptions, 'MaxPeers ' + IntToStr(MaxPeersAllow));
    writeln(FileAdvOptions, 'PosWarning ' + IntToStr(WO_PosWarning));

    Closefile(FileAdvOptions);
    if saving then ToLog('events', TimeToStr(now) + 'Options file saved');
    S_AdvOpt := False;
  except
    on E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error creating/saving AdvOpt file: ' + E.Message);
  end;
  EndPerformance('CreateADV');
end;

// Loads Advopt values
procedure LoadADV();
var
  linea: String;
begin
  try
    Assignfile(FileAdvOptions, AdvOptionsFilename);
    reset(FileAdvOptions);
    while not EOF(FileAdvOptions) do
    begin
      readln(FileAdvOptions, linea);
      if GetParameter(linea, 0) = 'RPCPort' then
        RPCPort := StrToIntDef(GetParameter(linea, 1), RPCPort);
      if GetParameter(linea, 0) = 'RPCPass' then RPCPass := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MaxPeers' then
        MaxPeersAllow := StrToIntDef(GetParameter(linea, 1), MaxPeersAllow);
      if GetParameter(linea, 0) = 'PosWarning' then
        WO_PosWarning := StrToIntDef(GetParameter(linea, 1), WO_PosWarning);
      if GetParameter(linea, 0) = 'SendReport' then
        WO_SendReport := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'BlocksDB' then
        WO_BlockDB := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'PRestart' then
        WO_PRestart := StrToIntDef(GetParameter(linea, 1), 0);
      if GetParameter(linea, 0) = 'SkipBlocks' then
        WO_skipBlocks := StrToBool(GetParameter(linea, 1));



      if GetParameter(linea, 0) = 'MultiSend' then
        WO_MultiSend := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'HideEmpty' then
        WO_HideEmpty := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'RPCFilter' then
        RPCFilter := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'RPCWhiteList' then RPCWhiteList := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'RPCBanned' then RPCBanned := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'RPCAuto' then RPCAuto := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'RPCSaveNew' then
        RPCSaveNew := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'Language' then WO_Language := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'Autoserver' then
        WO_AutoServer := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'PoUpdate' then WO_LastPoUpdate := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'Closestart' then
        WO_CloseStart := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'Autoupdate' then
        WO_AutoUpdate := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'NoGUI' then WO_StopGUI := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'FormState' then
      begin
        FormState_Top := StrToIntDef(GetParameter(linea, 1), 0);
        FormState_Left := StrToIntDef(GetParameter(linea, 2), 0);
        FormState_Width := StrToIntDef(GetParameter(linea, 3), 400);
        FormState_Heigth := StrToIntDef(GetParameter(linea, 4), 560);
        FormState_Status := StrToIntDef(GetParameter(linea, 5), 2);
        if FormState_Status = 2 then // Maximized
          form1.WindowState := wsMaximized;
        if FormState_Status = 0 then
        begin
          form1.Width := FormState_Width;
          form1.Height := FormState_Heigth;
        end;
        if FormState_Status = 1 then
        begin
          FormState_Status := 0;
          form1.Width := FormState_Width;
          form1.Height := FormState_Heigth;
        end;
      end;

      if GetParameter(linea, 0) = 'MNIP' then LocalMN_IP := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNPort' then LocalMN_Port := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNFunds' then LocalMN_Funds := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNSign' then LocalMN_Sign := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNAutoIp' then
        MN_AutoIP := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'WO_FullNode' then
        WO_FullNode := StrToBool(GetParameter(linea, 1));

    end;
    Closefile(FileAdvOptions);
  except
    on E: Exception do
      ToLog('events', TimeToStr(now) + 'Error loading AdvOpt file');
  end;
end;

{$ENDREGION Options File}


 // *** LANGUAGE HANDLING ***
 // *****************************************************************************
{$REGION LANGUAGE}
{
// returns the language to load the
Function GetLanguage():string;
var
  linea : string = '';
  archivo : textfile;
Begin
result := 'en';
WO_LastPoUpdate := '';
if not fileexists('NOSODATA'+DirectorySeparator+'advopt.txt') then
  begin
  result := 'en';
  WO_LastPoUpdate := '';
  end
else
   begin
   Assignfile(archivo, 'NOSODATA'+DirectorySeparator+'advopt.txt');
   reset(archivo);
   while not eof(archivo) do
      begin
      readln(archivo,linea);
      if GetParameter(linea,0) ='Language' then result:=GetParameter(linea,1);
      if GetParameter(linea,0) ='PoUpdate' then WO_LastPoUpdate:=GetParameter(linea,1);
      end;
   Closefile(archivo);
   end;
End;
}
{
Procedure ExtractPoFiles();
Begin
CreateFileFromResource('Noso.en','locale'+DirectorySeparator+'Noso.en.po');
CreateFileFromResource('Noso.es','locale'+DirectorySeparator+'Noso.es.po');
CreateFileFromResource('Noso.pt','locale'+DirectorySeparator+'Noso.pt.po');
CreateFileFromResource('Noso.zh','locale'+DirectorySeparator+'Noso.zh.po');
CreateFileFromResource('Noso.de','locale'+DirectorySeparator+'Noso.de.po');
CreateFileFromResource('Noso.ro','locale'+DirectorySeparator+'Noso.ro.po');
CreateFileFromResource('Noso.id','locale'+DirectorySeparator+'Noso.id.po');
CreateFileFromResource('Noso.ru','locale'+DirectorySeparator+'Noso.ru.po');
End;
}
{
Procedure CreateFileFromResource(resourcename,filename:string);
var
  Resource: TResourceStream;
begin
  Resource := TResourceStream.Create(HInstance, resourcename, RT_RCDATA);
  Resource.Position := 0;
  Resource.SaveToFile(filename);
  Resource.Free;
End;
}
{$ENDREGION LANGUAGE}

 // *** BOTS FILE ***
 // *****************************************************************************
{$REGION BOTS FILE}
{
// Modifica la hora del ultimo intento del bot, o lo añade si es la primera vez
Procedure UpdateBotData(IPUser:String);
var
  contador : integer = 0;
  updated : boolean = false;
Begin
  if IsSafeIP(IPUser) then exit;
  for contador := 0 to length(ListadoBots)-1 do
    begin
    if ListadoBots[Contador].ip = IPUser then
      begin
      ListadoBots[Contador].LastRefused:=UTCTimeStr;
      Updated := true;
      end;
    end;
  if not updated then
    begin
    SetLength(ListadoBots,Length(ListadoBots)+1);
    ListadoBots[Length(listadoBots)-1].ip:=IPUser;
    ListadoBots[Length(listadoBots)-1].LastRefused:=UTCTimeStr;
    end;
End;
}
{$ENDREGION}

// Saves updates files to disk
procedure SaveUpdatedFiles();
begin
  if S_Wallet then
  begin
    SaveWalletToFile();
    S_Wallet := False;
  end;
  if S_AdvOpt then CreateADV(True);
end;

// Updates wallet addresses balance from sumary
procedure UpdateWalletFromSumario();
var
  Contador, counter: Integer;
  ThisExists: Boolean = False;
  SumPos: Int64;
  ThisRecord: TSummaryData;
  ThisData: WalletData;
begin
  exit;
  for contador := 0 to LenWallArr - 1 do
  begin
    ThisData := GetWallArrIndex(contador);
    SumPos := GetIndexPosition(ThisData.Hash, thisRecord);
    ThisData.Balance := thisRecord.Balance;
    ThisData.LastOP := thisRecord.LastOP;
    ThisData.score := thisRecord.score;
    ThisData.Custom := thisRecord.Custom;
  end;
  S_Wallet := True;
  U_Dirpanel := True;
end;

procedure RebuildSummary();
var
  counter: Integer;
  TimeDuration: Int64;
begin
  CreateNewSummaryFile(FileExists(BlockDirectory + '0.blk'));
  for counter := 1 to MylastBlock do
  begin
    AddBlockToSumary(counter, False);
    if counter mod 100 = 0 then
    begin
      info('Rebuilding summary block: ' + IntToStr(counter));
      application.ProcessMessages;
      UpdateSummaryChanges;
      ResetBlockRecords;
    end;
  end;
  UpdateSummaryChanges;
  UpdateMyData();
  CreateSumaryIndex;
  TimeDuration := EndPerformance('RebuildSummary');
  ToLog('console', format('Sumary rebuild time: %d ms', [TimeDuration]));
end;

function deleteBlockFiles(fromnumber: Integer): Integer;
begin

end;

function UnZipUpdateFromRepo(Tver, TArch: String): Boolean;
var
  UnZipper: TUnZipper;
begin
  Result := True;
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := 'NOSODATA' + DirectorySeparator + 'UPDATES' +
      DirectorySeparator + TVer + '_' + TArch + '.zip';
    UnZipper.OutputPath := 'NOSODATA' + DirectorySeparator + 'UPDATES' +
      DirectorySeparator;
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
    OutText('File unzipped', False, 1)
  except
    on E: Exception do
    begin
      Result := False;
      OutText('Error unzipping update file', False, 1);
      OutText(E.Message, False, 1);
    end;
  end{Try};
  UnZipper.Free;
end;

// COmpletes the sumary from LAstUpdate to Lastblock
procedure CompleteSumary();
var
  StartBlock, finishblock: Integer;
  counter: Integer;
begin
  if copy(MySumarioHash, 0, 5) = GetConsensus(17) then exit;
  RebuildingSumary := True;
  StartBlock := SummaryLastop + 1;
  finishblock := Mylastblock;
  ToLog('console', 'Complete summary');
  for counter := StartBlock to finishblock do
  begin
    AddBlockToSumary(counter, True);
    if counter mod 1 = 0 then
    begin
      info('Rebuilding summary block: ' + IntToStr(counter));
      //'Rebuilding sumary block: '
      application.ProcessMessages;
      EngineLastUpdate := UTCTime;
    end;
  end;
  SummaryLastop := finishblock;
  RebuildingSumary := False;
  UpdateMyData();
  ZipSumary;
  ToLog('console', format('Summary completed from %d to %d (%s)',
    [StartBlock - 1, finishblock, Copy(MySumarioHash, 0, 5)]));
  info('Sumary completed');
end;

// Add 1 block transactions to sumary
procedure AddBlockToSumary(BlockNumber: Integer; SaveAndUpdate: Boolean = True);
var
  cont: Integer;
  BlockHeader: BlockHeaderData;
  ArrayOrders: TBlockOrdersArray;
  ArrayPos: BlockArraysPos;
  ArrayMNs: BlockArraysPos;
  PosReward: Int64 = 0;
  PosCount: Integer = 0;
  CounterPos: Integer;
  MNsReward: Int64;
  MNsCount: Integer;
  CounterMNs: Integer;
  GVTsTrfer: Integer = 0;
begin
  BlockHeader := Default(BlockHeaderData);
  BlockHeader := LoadBlockDataHeader(BlockNumber);
  if SaveAndUpdate then ResetBlockRecords;
  CreditTo(BlockHeader.AccountMiner, BlockHeader.Reward + BlockHeader.MinerFee,
    BlockNumber);
  ArrayOrders := Default(TBlockOrdersArray);
  ArrayOrders := GetBlockTrxs(BlockNumber);
  for cont := 0 to length(ArrayOrders) - 1 do
  begin
    if ArrayOrders[cont].OrderType = 'CUSTOM' then
    begin
      IsCustomizacionValid(ArrayOrders[cont].Sender, ArrayOrders[cont].Receiver,
        BlockNumber, True);
    end;
    if ArrayOrders[cont].OrderType = 'SNDGVT' then
    begin
      Inc(GVTsTrfer);
      SummaryPay(ArrayOrders[cont].Sender, GetCustomFee(BlockNumber), BlockNumber);
      ChangeGVTOwner(StrToIntDef(ArrayOrders[cont].Reference, 100),
        ArrayOrders[cont].Sender, ArrayOrders[cont].Receiver);
    end;
    if ArrayOrders[cont].OrderType = 'TRFR' then
    begin
      if SummaryValidPay(ArrayOrders[cont].Sender, ArrayOrders[cont].AmountFee +
        ArrayOrders[cont].AmountTransferred, blocknumber) then
        CreditTo(ArrayOrders[cont].Receiver, ArrayOrders[cont].AmountTransferred, BlockNumber)
      else
        SummaryPay(BlockHeader.AccountMiner, ArrayOrders[cont].AmountFee, blocknumber);
    end;
    if ArrayOrders[cont].OrderType = 'PROJCT' then
    begin
      CreditTo('NpryectdevepmentfundsGE', ArrayOrders[cont].AmountTransferred, BlockNumber);
      SummaryPay(BlockHeader.AccountMiner, ArrayOrders[cont].AmountTransferred, blocknumber);
    end;
  end;
  setlength(ArrayOrders, 0);
  if ((blocknumber >= PoSBlockStart) and (blocknumber <= PoSBlockEnd)) then
  begin
    ArrayPos := GetBlockPoSes(BlockNumber);
    PosReward := StrToIntDef(Arraypos[length(Arraypos) - 1].address, 0);
    SetLength(ArrayPos, length(ArrayPos) - 1);
    PosCount := length(ArrayPos);
    for counterpos := 0 to PosCount - 1 do
      CreditTo(ArrayPos[counterPos].address, Posreward, BlockNumber);
    SummaryPay(BlockHeader.AccountMiner, PosCount * Posreward, blocknumber);
    SetLength(ArrayPos, 0);
  end;

  if blocknumber >= MNBlockStart then
  begin
    ArrayMNs := GetBlockMNs(BlockNumber);
    MNsReward := StrToIntDef(ArrayMNs[length(ArrayMNs) - 1].address, 0);
    SetLength(ArrayMNs, length(ArrayMNs) - 1);
    MNsCount := length(ArrayMNs);
    for counterMNs := 0 to MNsCount - 1 do
      CreditTo(ArrayMNs[counterMNs].address, MNsreward, BlockNumber);
    SummaryPay(BlockHeader.AccountMiner, MNsCount * MNsreward, BlockNumber);
    SetLength(ArrayMNs, 0);
  end;
  CreditTo(AdminHash, 0, BlockNumber);
  if SaveAndUpdate then UpdateSummaryChanges;
  if BlockNumber mod 1000 = 0 then
    TryCopyFile(SummaryFileName, MarksDirectory + BlockNumber.tostring + '.bak');
  if GVTsTrfer > 0 then
  begin
    SaveGVTs;
    UpdateMyGVTsList;
  end;
  U_DirPanel := True;
end;

// Creates a bat file for restart
procedure CreateLauncherFile(IncludeUpdate: Boolean = False);
var
  archivo: textfile;
begin
  Assignfile(archivo, RestartFilename);
  rewrite(archivo);
  try
    {$IFDEF WINDOWS}
writeln(archivo,'echo Restarting Noso...');
writeln(archivo,'TIMEOUT 5');
writeln(archivo,'tasklist /FI "IMAGENAME eq '+AppFileName+'" 2>NUL | find /I /N "'+AppFileName+'">NUL');
writeln(archivo,'if "%ERRORLEVEL%"=="0" taskkill /F /im '+AppFileName);
if IncludeUpdate then
   begin
   writeln(archivo,'del '+AppFileName);
   writeln(archivo,'ren nosonew noso.exe');
   writeln(archivo,'start noso.exe');
   end
else writeln(archivo,'start '+Appfilename);
    {$ENDIF}
    {$IFDEF UNIX}
writeln(archivo,'for x in 5 4 3 2 1; do');
writeln(archivo,'echo -ne "Restarting in ${x}\r"');
writeln(archivo,'sleep 1');
writeln(archivo,'done');
writeln(archivo,'PID=$(ps ux | grep -v grep | grep -i '+AppFileName+' | cut -d" " -f 2)');
writeln(archivo,'if [ "${PID}" != "" ]; then');
writeln(archivo,'echo Killing '+AppFileName);
writeln(archivo,'kill ${PID}');
writeln(archivo,'fi');
if IncludeUpdate then
   begin
   writeln(archivo,'rm '+AppFileName);
   writeln(archivo,'mv Nosonew Noso');
   writeln(archivo,'chmod +x Noso');
   writeln(archivo,'./Noso');
   end
else
   writeln(archivo,'./'+AppFileName);
    {$ENDIF}
  except
    on E: Exception do
      if not G_ClosingAPP then
        ToLog('events', TimeToStr(now) + 'Error creating restart file: ' + E.Message);
  end{Try};
  Closefile(archivo);
end;

// Prepares for restart
procedure RestartNoso();
begin
  CreateLauncherFile();
  RunExternalProgram(RestartFilename);
end;

// Executes the required steps to restore the blockchain
procedure RestoreBlockChain();
begin
  CloseAllforms();
  CerrarClientes();
  StopServer();
  //setlength(CriptoOpsTIPO,0);
  deletefile(SummaryFileName);
  deletefile(SummaryFileName + '.bak');
  deletefile(ResumenFilename);
  if DeleteDirectory(BlockDirectory, True) then
    RemoveDir(BlockDirectory);
  ProcessLinesAdd('restart');
end;

procedure RestoreSumary(fromBlock: Integer = 0);
var
  startmark: Integer = 0;
begin
  if fromblock = 0 then StartMark :=
      ((GetMyLastUpdatedBlock div SumMarkInterval) - 1) * SumMarkInterval
  else
    StartMark := Fromblock;
  //LoadSummaryFromDisk(MarksDirectory+StartMark.ToString+'.bak');
  ToLog('console', 'Restoring sumary from ' + StartMark.ToString);
  CompleteSumary;
end;

{
// Returns the name of the app file without path
function AppFileName():string;
Begin
  result := ExtractFileName(ParamStr(0));
  // For working path: ExtractFilePAth
End;
}

end. // END UNIT
