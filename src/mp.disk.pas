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
  if not directoryexists(RPCBackupDirectory) then
    if not CreateDir(RPCBackupDirectory) then Inc(Result);
  if not directoryexists(BlockDirectory + DBDirectory) then
    if not CreateDir(BlockDirectory + DBDirectory) then Inc(Result);
end;

// Complete file verification
procedure VerifyFiles();
var
  defseeds: String = '';
begin
  SetHeadersFileName('NOSODATA' + DirectorySeparator + 'blchhead.nos');
  //if not Fileexists(SummaryFilename) then CreateHeadersFile();
  OutText('✓ Headers file ok', False, 1);

  if not FileExists(AdvOptionsFilename) then CreateADV(False)
  else
    LoadADV();
  OutText('✓ Advanced options loaded', False, 1);

  SetMasternodesFilename('NOSODATA' + DirectorySeparator + 'masternodes.txt');
  //if not FileExists(MasterNodesFilename) then CreateMasterNodesFile;
  //LoadMNsFile;
  OutText('✓ Masternodes file ok', False, 1);

  if not FileExists(GVTFilename) then CreateGVTFile;
  LoadGVTsFileData;
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
    ShowAdvOptions := True;
  {
  if not FileExists (WalletFilename) then
    begin
    CreateNewWallet;
    ShowAdvOptions := true;
    end
  else LoadWallet(WalletFilename);
  }
  OutText('✓ Wallet file ok', False, 1);

  PopulateNodeList;  // Fills the hardcoded seed nodes list

  if not Fileexists(SummaryFileName) then
    CreateNewSummaryFile(FileExists(BlockDirectory + '0.blk'));
  CreateSummaryIndex();
  OutText('✓ Sumary file ok', False, 1);


  if not FileExists(BlockDirectory + DBDirectory + DataBaseFilename) then CreateDBFile;
  OutText('✓ Database file ok.', False, 1);
  if BlockDatabaseEnabled then
  begin
    OutText('✓ Loading blocks Database.', False, 1);
    CreateOrderIDIndex;
  end;
  if not FileExists(BlockDirectory + '0.blk') then CrearBloqueCero();
  LastBlockIndex := GetMyLastUpdatedBlock;
  OutText('✓ My last block verified: ' + LastBlockIndex.ToString, False, 1);

  UpdateWalletFromSumario();
  OutText('✓ Wallet updated', False, 1);
  ImportAddressesFromBackup(RPCBackupDirectory);

  LoadPSOFileFromDisk;
end;

 // ***********************
 // *** NEW FILE SYSTEM *** (0.2.0N and higher)
 // ***********************

// *** NODE FILE ***

{
// Fills hardcoded seed nodes list
Procedure PopulateNodeList(); // 0.2.1Lb2 revisited
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
  StartPerformanceMeasurement('CreateADV');
  try
    Assignfile(AdvOptionsFile, AdvOptionsFilename);
    rewrite(AdvOptionsFile);
    writeln(AdvOptionsFile, '---NosoNode config file.---');
    writeln(AdvOptionsFile, '');

    writeln(AdvOptionsFile, '---Wallet related.---');
    writeln(AdvOptionsFile, '//Hide empty addresses');
    writeln(AdvOptionsFile, 'HideEmpty ' + BoolToStr(HideEmptyBalances, True));
    writeln(AdvOptionsFile, '//Use all addresses to send funds');
    writeln(AdvOptionsFile, 'MultiSend ' + BoolToStr(EnableMultiSend, True));
    writeln(AdvOptionsFile, '//Po files language code');
    writeln(AdvOptionsFile, 'Language ' + (LanguageSetting));
    writeln(AdvOptionsFile, '//No GUI refresh');
    writeln(AdvOptionsFile, 'NoGUI ' + BoolToStr(StopGUI, True));
    writeln(AdvOptionsFile, '//Po files last update');
    writeln(AdvOptionsFile, 'PoUpdate ' + (LastPoUpdate));
    writeln(AdvOptionsFile, '//Close the launch form automatically');
    writeln(AdvOptionsFile, 'Closestart ' + BoolToStr(CloseOnStart, True));
    writeln(AdvOptionsFile, '//Send anonymous report to developers');
    writeln(AdvOptionsFile, 'SendReport ' + BoolToStr(SendErrorReports, True));
    writeln(AdvOptionsFile, '//Keep a blocks database');
    writeln(AdvOptionsFile, 'BlocksDB ' + BoolToStr(BlockDatabaseEnabled, True));
    writeln(AdvOptionsFile, '//Restart periodically the node');
    writeln(AdvOptionsFile, 'PRestart ' + IntToStr(PendingRestart));
    writeln(AdvOptionsFile, '//Skip new blocks creation');
    writeln(AdvOptionsFile, 'SkipBlocks ' + BoolToStr(SkipBlockSync, True));

    writeln(AdvOptionsFile,
      '//Mainform coordinates. Do not manually change this values');
    writeln(AdvOptionsFile, Format('FormState %d %d %d %d %d',
      [Form1.Top, form1.Left, form1.Width, form1.Height, form1.WindowState]));
    writeln(AdvOptionsFile, '');

    writeln(AdvOptionsFile, '---Masternode---');
    writeln(AdvOptionsFile, '//Enable node server at start');
    writeln(AdvOptionsFile, 'Autoserver ' + BoolToStr(AutoServerMode, True));
    writeln(AdvOptionsFile, '//Run autoupdate directives');
    writeln(AdvOptionsFile, 'Autoupdate ' + BoolToStr(EnableAutoUpdate, True));
    writeln(AdvOptionsFile, '//Download the complete blockchain');
    writeln(AdvOptionsFile, 'WO_FullNode ' + BoolToStr(FullNodeMode, True));
    writeln(AdvOptionsFile, '//Masternode static IP');
    writeln(AdvOptionsFile, 'MNIP ' + (LocalMasternodeIP));
    writeln(AdvOptionsFile, '//Masternode port');
    writeln(AdvOptionsFile, 'MNPort ' + (LocalMasternodePort));
    writeln(AdvOptionsFile, '//Masternode funds address');
    writeln(AdvOptionsFile, 'MNFunds ' + (LocalMasternodeFunds));
    if LocalMasternodeSignature = '' then LocalMasternodeSignature := GetWallArrIndex(0).Hash;
    writeln(AdvOptionsFile, '//Masternode sign address');
    writeln(AdvOptionsFile, 'MNSign ' + LocalMasternodeSignature);
    writeln(AdvOptionsFile, '//Use automatic IP detection for masternode');
    writeln(AdvOptionsFile, 'MNAutoIp ' + BoolToStr(AutoDetectMasterNodeIP, True));
    writeln(AdvOptionsFile, '');

    writeln(AdvOptionsFile, '---RPC server---');
    writeln(AdvOptionsFile, '//RPC server port');
    writeln(AdvOptionsFile, 'RPCPort ' + IntToStr(RPCPort));
    writeln(AdvOptionsFile, '//RPC server password');
    writeln(AdvOptionsFile, 'RPCPass ' + RPCPassword);
    writeln(AdvOptionsFile, '//RPC IP filter active/inactive');
    writeln(AdvOptionsFile, 'RPCFilter ' + BoolToStr(UseRPCFilter, True));
    writeln(AdvOptionsFile, '//RPC whitelisted IPs');
    writeln(AdvOptionsFile, 'RPCWhiteList ' + RPCAllowedIPs);
    writeln(AdvOptionsFile, '//Enable RPC server at start');
    writeln(AdvOptionsFile, 'RPCAuto ' + BoolToStr(EnableRPCAutoStart, True));
    writeln(AdvOptionsFile, '//Save addresses keys created on a BAK folder');
    writeln(AdvOptionsFile, 'RPCSaveNew ' + BoolToStr(SaveNewRPCConnections, True));
    writeln(AdvOptionsFile, '//Banned methods for RPC requests');
    writeln(AdvOptionsFile, 'RPCBanned ' + RPCBannedIPs);
    writeln(AdvOptionsFile, '');

    writeln(AdvOptionsFile, '---Deprecated. To be removed.---');
    writeln(AdvOptionsFile, 'MaxPeers ' + IntToStr(MaxAllowedPeers));
    writeln(AdvOptionsFile, 'PosWarning ' + IntToStr(PosWarningThreshold));

    Closefile(AdvOptionsFile);
    if saving then ToLog('events', TimeToStr(now) + 'Options file saved');
    ShowAdvOptions := False;
  except
    on E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error creating/saving AdvOpt file: ' + E.Message);
  end;
  StopPerformanceMeasurement('CreateADV');
end;

// Loads Advopt values
procedure LoadADV();
var
  linea: String;
begin
  try
    Assignfile(AdvOptionsFile, AdvOptionsFilename);
    reset(AdvOptionsFile);
    while not EOF(AdvOptionsFile) do
    begin
      readln(AdvOptionsFile, linea);
      if GetParameter(linea, 0) = 'RPCPort' then
        RPCPort := StrToIntDef(GetParameter(linea, 1), RPCPort);
      if GetParameter(linea, 0) = 'RPCPass' then RPCPassword := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MaxPeers' then
        MaxAllowedPeers := StrToIntDef(GetParameter(linea, 1), MaxAllowedPeers);
      if GetParameter(linea, 0) = 'PosWarning' then
        PosWarningThreshold := StrToIntDef(GetParameter(linea, 1), PosWarningThreshold);
      if GetParameter(linea, 0) = 'SendReport' then
        SendErrorReports := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'BlocksDB' then
        BlockDatabaseEnabled := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'PRestart' then
        PendingRestart := StrToIntDef(GetParameter(linea, 1), 0);
      if GetParameter(linea, 0) = 'SkipBlocks' then
        SkipBlockSync := StrToBool(GetParameter(linea, 1));



      if GetParameter(linea, 0) = 'MultiSend' then
        EnableMultiSend := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'HideEmpty' then
        HideEmptyBalances := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'RPCFilter' then
        UseRPCFilter := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'RPCWhiteList' then RPCAllowedIPs := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'RPCBanned' then RPCBannedIPs := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'RPCAuto' then EnableRPCAutoStart := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'RPCSaveNew' then
        SaveNewRPCConnections := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'Language' then LanguageSetting := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'Autoserver' then
        AutoServerMode := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'PoUpdate' then LastPoUpdate := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'Closestart' then
        CloseOnStart := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'Autoupdate' then
        EnableAutoUpdate := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'NoGUI' then StopGUI := StrToBool(GetParameter(linea, 1));
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

      if GetParameter(linea, 0) = 'MNIP' then LocalMasternodeIP := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNPort' then LocalMasternodePort := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNFunds' then LocalMasternodeFunds := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNSign' then LocalMasternodeSignature := GetParameter(linea, 1);
      if GetParameter(linea, 0) = 'MNAutoIp' then
        AutoDetectMasterNodeIP := StrToBool(GetParameter(linea, 1));
      if GetParameter(linea, 0) = 'WO_FullNode' then
        FullNodeMode := StrToBool(GetParameter(linea, 1));

    end;
    Closefile(AdvOptionsFile);
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
LastPoUpdate := '';
if not fileexists('NOSODATA'+DirectorySeparator+'advopt.txt') then
  begin
  result := 'en';
  LastPoUpdate := '';
  end
else
   begin
   Assignfile(archivo, 'NOSODATA'+DirectorySeparator+'advopt.txt');
   reset(archivo);
   while not eof(archivo) do
      begin
      readln(archivo,linea);
      if GetParameter(linea,0) ='Language' then result:=GetParameter(linea,1);
      if GetParameter(linea,0) ='PoUpdate' then LastPoUpdate:=GetParameter(linea,1);
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
  if ShowWalletForm then
  begin
    SaveWalletToFile();
    ShowWalletForm := False;
  end;
  if ShowAdvOptions then CreateADV(True);
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
    SumPos := FindSummaryIndexPosition(ThisData.Hash, thisRecord);
    ThisData.Balance := thisRecord.Balance;
    ThisData.LastOP := thisRecord.LastOperation;
    ThisData.score := thisRecord.score;
    ThisData.Custom := thisRecord.CustomAlias;
  end;
  ShowWalletForm := True;
  UpdateDirPanel := True;
end;

procedure RebuildSummary();
var
  counter: Integer;
  TimeDuration: Int64;
begin
  CreateNewSummaryFile(FileExists(BlockDirectory + '0.blk'));
  for counter := 1 to LastBlockIndex do
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
  UpdateNodeData();
  CreateSummaryIndex;
  TimeDuration := StopPerformanceMeasurement('RebuildSummary');
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

// COmpletes the sumary from LAstUpdate to cLastBlock
procedure CompleteSumary();
var
  StartBlock, finishblock: Integer;
  counter: Integer;
begin
  if copy(ComputeSummaryHash, 0, 5) = GetConsensusData(17) then exit;
  IsRebuildingSummary := True;
  StartBlock := SummaryLastOperation + 1;
  finishblock := LastBlockIndex;
  ToLog('console', 'Complete summary');
  for counter := StartBlock to finishblock do
  begin
    AddBlockToSumary(counter, True);
    if counter mod 1 = 0 then
    begin
      info('Rebuilding summary block: ' + IntToStr(counter));
      //'Rebuilding sumary block: '
      application.ProcessMessages;
      LastEngineUpdate := UTCTime;
    end;
  end;
  SummaryLastOperation := finishblock;
  IsRebuildingSummary := False;
  UpdateNodeData();
  ZipSummary;
  ToLog('console', format('Summary completed from %d to %d (%s)',
    [StartBlock - 1, finishblock, Copy(ComputeSummaryHash, 0, 5)]));
  info('Sumary completed');
end;

// Add 1 block transactions to sumary
procedure AddBlockToSumary(BlockNumber: Integer; SaveAndUpdate: Boolean = True);
var
  cont: Integer;
  BlockHeader: BlockHeaderData;
  ArrayOrders: TBlockOrders;
  ArrayPos: TBlockAddresses;
  ArrayMNs: TBlockAddresses;
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
  ArrayOrders := Default(TBlockOrders);
  ArrayOrders := GetBlockTransfers(BlockNumber);
  for cont := 0 to length(ArrayOrders) - 1 do
  begin
    if ArrayOrders[cont].OrderType = 'CUSTOM' then
    begin
      IsCustomizationValid(ArrayOrders[cont].Sender, ArrayOrders[cont].Receiver,
        BlockNumber, True);
    end;
    if ArrayOrders[cont].OrderType = 'SNDGVT' then
    begin
      Inc(GVTsTrfer);
      ProcessSummaryPayment(ArrayOrders[cont].Sender, GetCustomFee(BlockNumber), BlockNumber);
      ChangeGVTOwner(StrToIntDef(ArrayOrders[cont].Reference, 100),
        ArrayOrders[cont].Sender, ArrayOrders[cont].Receiver);
    end;
    if ArrayOrders[cont].OrderType = 'TRFR' then
    begin
      if IsSummaryValidPayment(ArrayOrders[cont].Sender, ArrayOrders[cont].AmountFee +
        ArrayOrders[cont].AmountTransferred, blocknumber) then
        CreditTo(ArrayOrders[cont].Receiver, ArrayOrders[cont].AmountTransferred, BlockNumber)
      else
        ProcessSummaryPayment(BlockHeader.AccountMiner, ArrayOrders[cont].AmountFee, blocknumber);
    end;
    if ArrayOrders[cont].OrderType = 'PROJCT' then
    begin
      CreditTo('NpryectdevepmentfundsGE', ArrayOrders[cont].AmountTransferred, BlockNumber);
      ProcessSummaryPayment(BlockHeader.AccountMiner, ArrayOrders[cont].AmountTransferred, blocknumber);
    end;
  end;
  setlength(ArrayOrders, 0);
  if ((blocknumber >= PoSStartBlock) and (blocknumber <= PoSEndBlock)) then
  begin
    ArrayPos := GetBlockPoSes(BlockNumber);
    PosReward := StrToIntDef(Arraypos[length(Arraypos) - 1].Address, 0);
    SetLength(ArrayPos, length(ArrayPos) - 1);
    PosCount := length(ArrayPos);
    for counterpos := 0 to PosCount - 1 do
      CreditTo(ArrayPos[counterPos].Address, Posreward, BlockNumber);
    ProcessSummaryPayment(BlockHeader.AccountMiner, PosCount * Posreward, blocknumber);
    SetLength(ArrayPos, 0);
  end;

  if blocknumber >= MasterNodeStartBlock then
  begin
    ArrayMNs := GetBlockMNs(BlockNumber);
    MNsReward := StrToIntDef(ArrayMNs[length(ArrayMNs) - 1].Address, 0);
    SetLength(ArrayMNs, length(ArrayMNs) - 1);
    MNsCount := length(ArrayMNs);
    for counterMNs := 0 to MNsCount - 1 do
      CreditTo(ArrayMNs[counterMNs].Address, MNsreward, BlockNumber);
    ProcessSummaryPayment(BlockHeader.AccountMiner, MNsCount * MNsreward, BlockNumber);
    SetLength(ArrayMNs, 0);
  end;
  CreditTo(AdminHashAddress, 0, BlockNumber);
  if SaveAndUpdate then UpdateSummaryChanges;
  if BlockNumber mod 1000 = 0 then
    TryCopyFile(SummaryFileName, MarksDirectory + BlockNumber.tostring + '.bak');
  if GVTsTrfer > 0 then
  begin
    SaveGVTsAsData;
    UpdateMyGVTsList;
  end;
  UpdateDirPanel := True;
end;

// Creates a bat file for restart
procedure CreateLauncherFile(IncludeUpdate: Boolean = False);
var
  archivo: textfile;
begin
  Assignfile(archivo, RestartScriptName);
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
      if not AppClosing then
        ToLog('events', TimeToStr(now) + 'Error creating restart file: ' + E.Message);
  end{Try};
  Closefile(archivo);
end;

// Prepares for restart
procedure RestartNoso();
begin
  CreateLauncherFile();
  RunExternalProgram(RestartScriptName);
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
  deletefile(SummaryFilename);
  if DeleteDirectory(BlockDirectory, True) then
    RemoveDir(BlockDirectory);
  ProcessLinesAdd('restart');
end;

procedure RestoreSumary(fromBlock: Integer = 0);
var
  startmark: Integer = 0;
begin
  if fromblock = 0 then StartMark :=
      ((GetMyLastUpdatedBlock div BlockSummaryInterval) - 1) * BlockSummaryInterval
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
