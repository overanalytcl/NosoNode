unit mpGUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MasterPaskalForm, nosotime, Graphics, strutils,
  Forms, Controls, Grids, StdCtrls,
  ExtCtrls, Buttons, editbtn, Menus, Clipbrd, IdContext, LCLTranslator,
  nosodebug, nosogeneral,
  nosocrypto, nosoconsensus, nosounit, nosopsos,
  nosowallcon, nosoblock, nosonetwork, nosonosocfg, nosogvts,
  nosomasternodes, nosoheaders;

type
  TFormInicio = class(TForm)
    procedure closeFormInicio(Sender: TObject; var CanClose: Boolean);
  private
  public
  end;

  TFormSlots = class(TForm)
    procedure GridMSlotsPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
  public
  end;

function ThisPercent(percent, thiswidth: Integer; RestarBarra: Boolean = False): Integer;
procedure CreateFormInicio();
procedure CreateFormSlots();
procedure UpdateSlotsGrid();
function GetConnectedPeers(): String;
procedure InitGUI();
procedure OutText(Texto: String; inctime: Boolean = False; canal: Integer = 0);
procedure ActualizarGUI();
procedure Info(Text: String);
procedure Processhint(Sender: TObject);
procedure CloseAllForms();
procedure UpdateMyGVTsList();

var
  FormInicio: TFormInicio;
  GridInicio: TStringgrid;
  LastUpdateMonitor: Int64 = 0;

  FormSlots: TFormSlots;
  GridMSlots: TStringgrid;
  SlotsLastUpdate: Int64 = 0;

implementation

uses
  mpParser, mpDisk, mpRed, mpProtocol, mpcoin, mpblock, formexplore, translation;

// Returns the X percentage of a specified number
function ThisPercent(percent, thiswidth: Integer; RestarBarra: Boolean = False): Integer;
begin
  Result := (percent * thiswidth) div 100;
  if RestarBarra then Result := Result - 20;
end;

// Crea el formulario para el inicio
procedure CreateFormInicio();
begin
  FormInicio := TFormInicio.Createnew(form1);
  FormInicio.Caption := 'Noso ' + MainnetVersion + NodeRelease;
  FormInicio.SetBounds(0, 0, 350, 200);
  FormInicio.BorderStyle := bssingle;
  FormInicio.Position := poOwnerFormCenter;
  FormInicio.BorderIcons := FormInicio.BorderIcons - [biminimize] - [bisystemmenu];
  forminicio.ShowInTaskBar := sTAlways;
  forminicio.OnCloseQuery := @FormInicio.closeFormInicio;

  GridInicio := TStringGrid.Create(forminicio);
  GridInicio.Parent := forminicio;
  GridInicio.Font.Name := 'consolas';
  GridInicio.Font.Size := 10;
  GridInicio.Left := 1;
  GridInicio.Top := 1;
  GridInicio.Height := 198;
  GridInicio.Width := 348;
  GridInicio.FixedCols := 0;
  GridInicio.FixedRows := 0;
  GridInicio.rowcount := 0;
  GridInicio.ColCount := 1;
  GridInicio.ScrollBars := ssAutoVertical;
  GridInicio.Options := GridInicio.Options - [goRangeSelect];
  GridInicio.ColWidths[0] := 298;
  GridInicio.FocusRectVisible := False;
  GridInicio.Enabled := True;
  GridInicio.GridLineWidth := 0;
end;

// Al cerrar el formulario de inicio
procedure TFormInicio.closeFormInicio(Sender: TObject; var CanClose: Boolean);
begin
  if G_launching then
  begin
    CompleteInicio;
  end
  //else if RunningDoctor then canclose := false
  else
  begin
    forminicio.Visible := False;
    form1.Visible := True;
  end;
end;

// Color the conections form
procedure TFormSlots.GridMSlotsPrepareCanvas(Sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
begin
  if ((Arow > 0) and (GetConexIndex(Arow).IsBusy)) then
  begin
    (Sender as TStringGrid).Canvas.Brush.Color := clmoneygreen;
  end;
end;

// Crea el formulario de monitorizacion de los slots
procedure CreateFormSlots();
begin
  FormSlots := TFormSlots.Createnew(form1);
  FormSlots.Caption := coinname + ' Slots Monitor';
  FormSlots.SetBounds(0, 0, 900, 410);
  FormSlots.BorderStyle := bssingle;
  //FormSlots.Position:=poOwnerFormCenter;
  FormSlots.Top := 1;
  FormSlots.Left := 1;
  FormSlots.BorderIcons := FormSlots.BorderIcons - [biminimize];
  FormSlots.ShowInTaskBar := sTAlways;

  GridMSlots := TStringGrid.Create(FormSlots);
  GridMSlots.Parent := FormSlots;
  GridMSlots.Font.Name := 'consolas';
  GridMSlots.Font.Size := 8;
  GridMSlots.Left := 1;
  GridMSlots.Top := 1;
  GridMSlots.Height := 408;
  GridMSlots.Width := 894;
  GridMSlots.FixedCols := 0;
  GridMSlots.FixedRows := 1;
  GridMSlots.rowcount := MaxConecciones + 1;
  GridMSlots.ColCount := 23;
  GridMSlots.ScrollBars := ssVertical;
  GridMSlots.FocusRectVisible := False;
  GridMSlots.Options := GridMSlots.Options - [goRangeSelect];
  GridMSlots.ColWidths[0] := 20;
  GridMSlots.ColWidths[1] := 80;
  GridMSlots.ColWidths[2] := 25;
  GridMSlots.ColWidths[3] := 20;
  GridMSlots.ColWidths[4] := 48;
  GridMSlots.ColWidths[5] := 40;
  GridMSlots.ColWidths[6] := 40;
  GridMSlots.ColWidths[7] := 25;
  GridMSlots.ColWidths[8] := 25;
  GridMSlots.ColWidths[9] := 70;
  GridMSlots.ColWidths[10] := 30;
  GridMSlots.ColWidths[11] := 25;
  GridMSlots.ColWidths[12] := 40;
  GridMSlots.ColWidths[13] := 25;
  GridMSlots.ColWidths[14] := 29;
  GridMSlots.ColWidths[15] := 40;
  GridMSlots.ColWidths[16] := 25;
  GridMSlots.ColWidths[17] := 80;
  GridMSlots.ColWidths[18] := 25;
  GridMSlots.ColWidths[19] := 40;
  GridMSlots.ColWidths[20] := 40;
  GridMSlots.ColWidths[21] := 40;
  GridMSlots.ColWidths[22] := 40;
  GridMSlots.Enabled := True;
  GridMSlots.Cells[0, 0] := 'N';
  GridMSlots.Cells[1, 0] := 'IP';
  GridMSlots.Cells[2, 0] := 'T';
  GridMSlots.Cells[3, 0] := 'Cx';
  GridMSlots.Cells[4, 0] := 'LBl';
  GridMSlots.Cells[5, 0] := 'LBlH';
  GridMSlots.Cells[6, 0] := 'SumH';
  GridMSlots.Cells[7, 0] := 'Pen';
  GridMSlots.Cells[8, 0] := 'Pro';
  GridMSlots.Cells[9, 0] := 'Ver';
  GridMSlots.Cells[10, 0] := 'LiP';
  GridMSlots.Cells[11, 0] := 'Off';
  GridMSlots.Cells[12, 0] := 'HeaH';
  GridMSlots.Cells[13, 0] := 'Sta';
  GridMSlots.Cells[14, 0] := 'Ping';
  GridMSlots.Cells[15, 0] := 'MNs';
  GridMSlots.Cells[16, 0] := '#';
  GridMSlots.Cells[17, 0] := 'Besthash';
  GridMSlots.Cells[18, 0] := 'MNC';
  GridMSlots.Cells[19, 0] := 'GVTs';
  GridMSlots.Cells[20, 0] := 'CFG';
  GridMSlots.Cells[21, 0] := 'Mkl';
  GridMSlots.Cells[22, 0] := 'PSO';
  GridMSlots.GridLineWidth := 1;
  GridMSlots.OnPrepareCanvas := @FormSlots.GridMSlotsPrepareCanvas;
end;

procedure UpdateSlotsGrid();
var
  counter: Integer;
  CurrentUTC: Int64;
  LConex: Tconectiondata;
begin
  if WO_StopGUI then exit;
  BeginPerformance('UpdateSlotsGrid');
  CurrentUTC := UTCTime;
  if CurrentUTC > SlotsLastUpdate then
  begin
    for counter := 1 to MaxConecciones do
    begin
      LConex := GetConexIndex(counter);
      GridMSlots.Cells[0, counter] := IntToStr(counter);
      GridMSlots.Cells[1, counter] := LConex.ip;
      GridMSlots.Cells[2, counter] := LConex.tipo;
      GridMSlots.Cells[3, counter] := IntToStr(LConex.Connections);
      GridMSlots.Cells[4, counter] := LConex.Lastblock;
      GridMSlots.Cells[5, counter] := copy(LConex.LastblockHash, 0, 5);
      GridMSlots.Cells[6, counter] := copy(LConex.SumarioHash, 0, 5);
      GridMSlots.Cells[7, counter] := IntToStr(LConex.Pending);
      GridMSlots.Cells[8, counter] := IntToStr(LConex.Protocol);
      GridMSlots.Cells[9, counter] := LConex.Version;
      GridMSlots.Cells[10, counter] := IntToStr(LConex.ListeningPort);
      GridMSlots.Cells[11, counter] := IntToStr(LConex.offset);
      GridMSlots.Cells[12, counter] := copy(LConex.ResumenHash, 0, 5);
      GridMSlots.Cells[13, counter] := IntToStr(LConex.ConexStatus);
      GridMSlots.Cells[14, counter] :=
        IntToStr(UTCTime - StrToInt64Def(LConex.lastping, UTCTime));
      GridMSlots.Cells[15, counter] := LConex.MNsHash;
      GridMSlots.Cells[16, counter] := IntToStr(LConex.MNsCount);
      GridMSlots.Cells[17, counter] := LConex.BestHashDiff;
      GridMSlots.Cells[18, counter] := LConex.MNChecksCount.ToString;
      GridMSlots.Cells[19, counter] := copy(LConex.GVTsHash, 0, 5);
      GridMSlots.Cells[20, counter] := LConex.CFGHash;
      GridMSlots.Cells[21, counter] := copy(LConex.MerkleHash, 0, 5);
      GridMSlots.Cells[22, counter] := copy(LConex.PSOHash, 0, 5);
    end;
    SlotsLastUpdate := CurrentUTC;
  end;
  EndPerformance('UpdateSlotsGrid');
end;

function GetConnectedPeers(): String;
var
  counter: Integer;
begin
  Result := '';
  for counter := 1 to MaxConecciones do
  begin
    if ((GetConexIndex(counter).ip <> '') and
      (GetConexIndex(counter).ConexStatus >= 3)) then
    begin
      Result := Result + GetConexIndex(counter).ip + ' ';
    end;
  end;
  Trim(Result);
end;

// Inicializa el grid donde se muestran los datos
procedure InitGUI();
var
  contador: Integer = 0;
begin
  // datapanel
  form1.DataPanel.Cells[0, 0] := 'Merkle';
  form1.DataPanel.Cells[0, 1] := rs0505;  //'Server'
  form1.DataPanel.Cells[0, 2] := rs0506;  //'Connections'
  form1.DataPanel.Cells[0, 3] := rs0507;  //'Headers'
  form1.DataPanel.Cells[0, 4] := rs0508;  //'Summary'
  form1.DataPanel.Cells[0, 5] := rs0509;  //Lastblock
  form1.DataPanel.Cells[0, 6] := rs0510;  //'Blocks'
  form1.DataPanel.Cells[0, 7] := rs0511;  //'Pending'

  form1.DataPanel.Cells[2, 0] := 'PSOs';
  form1.DataPanel.Cells[2, 1] := 'Next';
  form1.DataPanel.Cells[2, 2] := 'Clients';
  form1.DataPanel.Cells[2, 3] := 'OrdIndex';
  form1.DataPanel.Cells[2, 4] := 'NosoCFG';
  form1.DataPanel.Cells[2, 5] := 'GVTs';
  form1.DataPanel.Cells[2, 6] := 'Masternodes';
  form1.DataPanel.Cells[2, 7] := 'MNsCount';

  Form1.SGridSC.Cells[0, 0] := rs0501;  //'Destination'
  Form1.SGridSC.Cells[0, 1] := rs0502;  //'Amount'
  Form1.SGridSC.Cells[0, 2] := rs0503;  //'reference'

  //Direccionespanel
  form1.Direccionespanel.RowCount := LenWallArr + 1;
  form1.Direccionespanel.Cells[0, 0] := format(rs0514, [LEnWallArr]);  //'Address'
  form1.Direccionespanel.Cells[1, 0] := rs0515;  //'Balance'

  for contador := 0 to LenWallArr - 1 do
  begin
    form1.Direccionespanel.Cells[0, contador + 1] := GetWallArrIndex(contador).Hash;
    form1.Direccionespanel.Cells[1, contador + 1] :=
      Int2Curr(GetWallArrIndex(contador).Balance);
  end;

  // Nodes Grid
  form1.GridNodes.Cells[0, 0] := 'Node';
  form1.GridNodes.Cells[1, 0] := 'Funds';
  form1.GridNodes.Cells[2, 0] := 'Last';
  form1.GridNodes.Cells[3, 0] := 'Total';
  form1.GridNodes.Cells[4, 0] := 'Conf';
  form1.GridNodes.FocusRectVisible := False;

  form1.GVTsGrid.Cells[0, 0] := '#';
  form1.GVTsGrid.Cells[1, 0] := 'Address';
  form1.GVTsGrid.FocusRectVisible := False;

end;

// Ordena las salidas de informacion
procedure OutText(Texto: String; inctime: Boolean = False; canal: Integer = 0);
begin
  if inctime then texto := timetostr(now) + ' ' + texto;
  if canal = 0 then ToLog('console', texto);
  if canal = 1 then  // Salida al grid de inicio
  begin
    gridinicio.RowCount := gridinicio.RowCount + 1;
    gridinicio.Cells[0, gridinicio.RowCount - 1] := Texto;
    gridinicio.TopRow := gridinicio.RowCount;
    Application.ProcessMessages;
    sleep(1);
  end;
  if canal = 2 then // A consola y label info
  begin
    ToLog('console', texto);
    info(texto);
  end;
end;

// Actualiza los datos en el grid
procedure ActualizarGUI();
const
  LocalLastUpdate: Int64 = 0;
  LastUpdateProcesses: Int64 = 0;
  LastUpdateConsensus: Int64 = 0;
  LastUpdateDataPanel: Int64 = 0;
var
  contador: Integer = 0;
  LocalProcesses: TProcessCopy;
  FileProcs: TFileMCopy;
  LConsensus: TNodeConsensus;
  LPSOs: TPSOsArray;
begin
  if WO_StopGUI then exit;
  BeginPerformance('UpdateGUITime');
  //Update Monitor Grid
  if ((form1.PCMonitor.ActivePage = Form1.TabMonitorMonitor) and
    (LastUpdateMonitor <> UTCTime)) then
  begin
    BeginPerformance('UpdateGUIMonitor');
    if length(ArrPerformance) > 0 then
    begin
      Form1.SG_Performance.RowCount := Length(ArrPerformance) + 1;
      for contador := 0 to high(ArrPerformance) do
      begin
        try
          Form1.SG_Performance.Cells[0, contador + 1] := ArrPerformance[contador].tag;
          Form1.SG_Performance.Cells[1, contador + 1] :=
            IntToStr(ArrPerformance[contador].Count);
          Form1.SG_Performance.Cells[2, contador + 1] :=
            IntToStr(ArrPerformance[contador].max);
          Form1.SG_Performance.Cells[3, contador + 1] :=
            IntToStr(ArrPerformance[contador].Average);
        except
          on E: Exception do
          begin
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + format('Error showing ArrPerformance data(%s): %s',
              [ArrPerformance[contador].tag, E.Message]));
          end;
        end;
      end;
    end;
    LastUpdateMonitor := UTCTime;
    EndPerformance('UpdateGUIMonitor');
  end;

  if LastUpdateProcesses <> UTCTime then
  begin
    if form1.PC_Processes.ActivePage = Form1.TabFiles then
    begin
      FileProcs := GetFileProcessCopy;
      Form1.SG_FileProcs.RowCount := Length(FileProcs) + 1;
      //Form1.SG_FileProcs.Cells[0,0]:=Format('Thread [%d]',[length(LocalProcesses)]);
      for contador := 0 to High(FileProcs) do
      begin
        Form1.SG_FileProcs.Cells[0, contador + 1] := FileProcs[contador].FiType;
        Form1.SG_FileProcs.Cells[1, contador + 1] := FileProcs[contador].FiFile;
        Form1.SG_FileProcs.Cells[2, contador + 1] := FileProcs[contador].FiPeer;
      end;
    end;
  end;

  if LastUpdateProcesses <> UTCTime then
  begin
    if form1.PC_Processes.ActivePage = Form1.TabThreads then
    begin
      LocalProcesses := GetProcessCopy;
      Form1.SG_OpenThreads.RowCount := Length(LocalProcesses) + 1;
      Form1.SG_OpenThreads.Cells[0, 0] :=
        Format('Thread [%d]', [length(LocalProcesses)]);
      for contador := 0 to High(LocalProcesses) do
      begin
        Form1.SG_OpenThreads.Cells[0, contador + 1] := LocalPRocesses[contador].ThName;
        Form1.SG_OpenThreads.Cells[1, contador + 1] :=
          TimeSinceStamp(LocalPRocesses[contador].ThStart);
        Form1.SG_OpenThreads.Cells[2, contador + 1] :=
          TimeSinceStamp(LocalPRocesses[contador].ThLast);
      end;
    end;
  end;
  if LastUpdateConsensus <> UTCTime then
  begin
    if form1.PCMonitor.ActivePage = Form1.TabConsensus then
    begin
      form1.Label1.Caption :=
        Format('Last update : %d seconds (OT= %d)',
        [UTCTime - LastConsensusTime, OpenThreadsValue]);
      form1.Label16.Caption := Format('Block       : %s', [GetConsensus(2)]);
      form1.Label17.Caption := Format('Merkle      : %s', [GetConsensus]);
      form1.Label18.Caption :=
        Format('Consensus   : %d %% (%d/%d)', [Css_Percentage,
        Css_ReachedNodes, Css_TotalNodes]);
      form1.SGConSeeds.RowCount := 1 + GetNodesArrayCount;
      for contador := 0 to GetNodesArrayCount - 1 do
      begin
        LConsensus := GetNodesArrayIndex(contador);
        Form1.SGConSeeds.Cells[0, contador + 1] := LConsensus.host;
        Form1.SGConSeeds.Cells[1, contador + 1] := LConsensus.peers.ToString;
        Form1.SGConSeeds.Cells[2, contador + 1] := LConsensus.ConStr;
        Form1.SGConSeeds.Cells[3, contador + 1] := LConsensus.Block.ToString;
      end;
    end;
  end;
  LastUpdateProcesses := UTCTime;
  //if LocalLastUpdate = UTCTime then exit;
  LocalLastUpdate := UTCTime;

  if LastUpdateDataPanel <> UTCTime then
  begin
    form1.DataPanel.Cells[1, 0] :=
      copy(GetConHash('NODESTATUS ' + GetNodeStatusString), 0, 5) +
      '/' + copy(GetConsensus(0), 0, 5);
    form1.DataPanel.Cells[1, 1] := NodeServerInfo;
    form1.DataPanel.Cells[1, 2] :=
      IntToStr(GetTotalConexiones) + ' (' + IntToStr(MyConStatus) +
      ') [' + IntToStr(G_TotalPings) + ']';
    form1.DataPanel.Cells[1, 3] :=
      Format('%s / %s', [copy(GetResumenHash, 0, 5), GetConsensus(5)]);
    form1.DataPanel.Cells[1, 4] :=
      format('%s / %s', [Copy(MySumarioHash, 0, 5), GetConsensus(17)]);
    form1.DataPanel.Cells[1, 5] :=
      format('%s / %s', [Copy(MyLastBlockHash, 0, 5), copy(GetConsensus(10), 0, 5)]);
    form1.DataPanel.Cells[1, 6] := format('%d / %s', [MyLastBlock, GetConsensus(2)]);
    form1.DataPanel.Cells[1, 7] :=
      format('(%d)  %d/%s', [length(ArrayCriptoOp), GetPendingCount, GetConsensus(3)]);
    form1.DataPanel.Cells[3, 0] :=
      format('[%d - %d] %s / %s', [GEtPSOHeaders.MNsLock, GetPSOHeaders.Count,
      Copy(PSOFileHash, 0, 5), GetConsensus(20)]);
    form1.DataPanel.Cells[3, 1] :=
      Format('[%s] %s Noso', [BlockAge.ToString, Copy(
      Int2curr(GetBlockReward(Mylastblock + 1)), 0, 5)]);
    form1.DataPanel.Cells[3, 2] :=
      GEtOutgoingconnections.ToString + '/' + GetClientReadThreads.ToString;
    form1.DataPanel.Cells[3, 3] := Format('%d (%d)', [GetDBLastBlock, GetDBRecords]);
    form1.DataPanel.Cells[3, 4] :=
      format('%s / %s', [Copy(GetCFGHash, 0, 5), GetConsensus(19)]);
    form1.DataPanel.Cells[3, 5] :=
      format('%s / %s', [Copy(MyGVTsHash, 0, 5), GetConsensus(18)]);
    form1.DataPanel.Cells[3, 6] :=
      format('%s / %s', [Copy(GetMNsHash, 0, 5), GetConsensus(8)]);
    form1.DataPanel.Cells[3, 7] :=
      format('(%d)  %d/%s (%d)', [GetMNsChecksCount, GetMNsListLength,
      GetConsensus(9), LengthWaitingMNs]);
    LastUpdateDataPanel := UTCTime;
  end;
  // update nodes grid
  if ((U_MNsGrid) or (UTCTime > U_MNsGrid_Last + 59)) then
  begin
    //{
    form1.GridNodes.RowCount := 1;
    if GetMNsListLength > 0 then
    begin
      for contador := 0 to length(MNsList) - 1 do
      begin
        form1.GridNodes.RowCount := form1.GridNodes.RowCount + 1;
        form1.GridNodes.Cells[0, 1 + contador] :=
          MNsList[contador].Ip + ':' + IntToStr(MNsList[contador].Port);
        form1.GridNodes.Cells[1, 1 + contador] := MNsList[contador].Fund;
        //form1.GridNodes.Cells[1,1+contador] := MNsList[contador].First.ToString;
        form1.GridNodes.Cells[2, 1 + contador] := MNsList[contador].Last.ToString;
        form1.GridNodes.Cells[3, 1 + contador] := MNsList[contador].Total.ToString;
        form1.GridNodes.Cells[4, 1 + contador] := MNsList[contador].Validations.ToString;
        ;
      end;
    end;
    //}
    U_MNsGrid_Last := UTCTime;
    form1.LabelNodesHash.Caption := 'Count: ' + GetMNsListLength.ToString;
    U_MNsGrid := False;
  end;
  if U_DirPanel then
  begin
    BeginPerformance('UpdateDirPanel');
    form1.Direccionespanel.RowCount := 1;
    for contador := 0 to LenWallArr - 1 do
    begin
      if ((GetAddressBalanceIndexed(GetWallArrIndex(contador).hash) = 0) and
        (WO_HideEmpty)) then continue;
      form1.Direccionespanel.RowCount := form1.Direccionespanel.RowCount + 1;
      if GetWallArrIndex(contador).Custom <> '' then
        form1.Direccionespanel.Cells[0, form1.Direccionespanel.RowCount - 1] :=
          GetWallArrIndex(contador).Custom
      else
        form1.Direccionespanel.Cells[0, form1.Direccionespanel.RowCount - 1] :=
          GetWallArrIndex(contador).Hash;
      form1.Direccionespanel.Cells[1, form1.Direccionespanel.RowCount - 1] :=
        Int2Curr(GetAddressBalanceIndexed(GetWallArrIndex(contador).hash) -
        GetWallArrIndex(contador).pending);
    end;
    form1.LabelBigBalance.Caption := Int2Curr(GetWalletBalance) + ' ' + CoinSimbol;
    form1.Direccionespanel.Cells[0, 0] := format(rs0514, [LEnWallArr]);  //'Address'
    U_DirPanel := False;
    EndPerformance('UpdateDirPanel');
  end;
  EndPerformance('UpdateGUITime');
end;

// Actualiza la informacion de la label info
procedure Info(Text: String);
begin
  if WO_StopGUI then exit;
  Form1.InfoPanel.Caption := copy(Text, 1, 40);
  InfoPanelTime := Length(Text) * 50;
  if InfoPanelTime < 1000 then InfoPanelTime := 1000;
  Form1.InfoPanel.Visible := True;
  Form1.InfoPanel.BringToFront;
  Form1.InfoPanel.Refresh;
  if form1.InfoTimer.Enabled = False then form1.InfoTimer.Enabled := True;
end;

// Fija el texto de hint
procedure Processhint(Sender: TObject);
var
  texto: String = '';
begin
  if Sender = form1.ImageInc then
  begin
    form1.ImageInc.Hint := 'Incoming: ' + Int2curr(MontoIncoming);
  end;
  if Sender = form1.ImageOut then
  begin
    form1.ImageOut.Hint := 'Outgoing: ' + Int2curr(MontoOutgoing);
  end;
end;

procedure CloseAllForms();
begin
  try
    formslots.Visible := False;
    CloseExplorer;
  except
    on E: Exception do
    begin

    end;
  end; {TRY}
end;

// Update my GVTsList
procedure UpdateMyGVTsList();
var
  counter: Integer;
  Owned: Integer = 0;
begin
  form1.GVTsGrid.RowCount := 1;
  EnterCriticalSection(CSGVTsArray);
  for counter := 0 to length(ArrGVTs) - 1 do
  begin
    if WallAddIndex(ArrGVTs[counter].owner) >= 0 then
    begin
      form1.GVTsGrid.RowCount := form1.GVTsGrid.RowCount + 1;
      form1.GVTsGrid.Cells[0, form1.GVTsGrid.RowCount - 1] := ArrGVTs[counter].number;
      form1.GVTsGrid.Cells[1, form1.GVTsGrid.RowCount - 1] := ArrGVTs[counter].owner;
      Inc(Owned);
    end;
  end;
  LeaveCriticalSection(CSGVTsArray);
  Form1.TabGVTs.TabVisible := Owned > 0;
end;

end. // END UNIT
