unit MPForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  Grids, ExtCtrls, Buttons, IdTCPServer, IdContext, IdGlobal, IdTCPClient,
  fileutil, Clipbrd, Menus, formexplore, lclintf, ComCtrls,
  strutils, Math, IdHTTPServer, IdCustomHTTPServer,
  IdHTTP, fpJSON, Types, DefaultTranslator, LCLTranslator, translation, nosodebug,
  IdComponent, nosogeneral, nosocrypto, nosounit, nosoconsensus, nosopsos, NosoWallCon,
  nosoheaders, nosoblock, nosonetwork, nosogvts, nosomasternodes,
  nosonosocfg, nosoIPControl;

type

  { TThreadClientRead }

  TServerTipo = class(TObject)
  private
    VSlot: Integer;
  public
    constructor Create;
    property Slot: Integer read VSlot write VSlot;
  end;

  TThreadDirective = class(TThread)
  private
    command: String;
  protected
    procedure Execute; override;
  public
    constructor Create(const CreatePaused: Boolean; const TCommand: String);
  end;

  TThreadSendOutMsjs = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  TThreadKeepConnect = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  TThreadIndexer = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  TUpdateMNs = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  TCryptoThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  TUpdateLogs = class(TThread)
  private
    procedure UpdateConsole;
    procedure UpdateEvents;
    procedure UpdateExceps;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean);
  end;

  {
  conectiondata = Packed Record
     Autentic: boolean;                 // si la conexion esta autenticada por un ping
     Connections : Integer;             // A cuantos pares esta conectado
     tipo: string[8];                   // Tipo: SER o CLI
     ip: string[20];                    // La IP del par
     lastping: string[15];              // UTCTime del ultimo ping
     context: TIdContext;               // Informacion para los canales cliente
     Lastblock: string[15];             // Numero del ultimo bloque
     LastblockHash: string[64];         // Hash del ultimo bloque
     SumarioHash : string[64];          // Hash del sumario de cuenta
     Pending: Integer;                  // Cantidad de operaciones pendientes
     Protocol : integer;                // Numero de protocolo usado
     Version : string[8];
     ListeningPort : integer;
     offset : integer;                  // Segundos de diferencia a su tiempo
     ResumenHash : String[64];           //
     ConexStatus : integer;
     IsBusy : Boolean;
     Thread : TThreadClientRead;
     MNsHash : string[5];
     MNsCount : Integer;
     BestHashDiff : string[32];
     MNChecksCount : integer;
     GVTsHash      : string[32];
     CFGHash       : string[32];
     MerkleHash    : string[32];
     PSOHash       : string[32];
     end;
  }
  {
  BlockHeaderData = Packed Record
     Number         : Int64;
     TimeStart      : Int64;
     TimeEnd        : Int64;
     TimeTotal      : integer;
     TimeLast20     : integer;
     TrxTotales     : integer;
     Difficult      : integer;
     TargetHash     : String[32];
     Solution       : String[200]; // 180 necessary
     LastBlockHash  : String[32];
     NxtBlkDiff     : integer;
     AccountMiner   : String[40];
     MinerFee       : Int64;
     Reward         : Int64;
     end;
   }

  NetworkData = packed record
    Value: String[64];   // el valor almacenado
    Porcentaje: Integer; // porcentaje de peers que tienen el valor
    Count: Integer;      // cuantos peers comparten ese valor
    Slot: Integer;       // en que slots estan esos peers
  end;

  {
  ResumenData = Packed Record
     block : integer;
     blockhash : string[32];
     SumHash : String[32];
     end;
  }
  {
  BlockOrdersArray = Array of OrderData;
  }

  TArrayPos = packed record
    address: String[32];
  end;

  BlockArraysPos = array of TArrayPos;

  TMasterNode = packed record
    SignAddress: String[40];
    PublicKey: String[120];
    FundAddress: String[40];
    Ip: String[40];
    Port: Integer;
    Block: Integer;
    BlockHash: String[32];
    Signature: String[120];
    Time: String[15];
    ReportHash: String[32];
  end;

  {
  TMNode = Packed Record
       Ip           : string[15];
       Port         : integer;
       Sign         : string[40];
       Fund         : string[40];
       First        : integer;
       Last         : integer;
       Total        : integer;
       Validations  : integer;
       Hash         : String[32];
       end;
   }

   {
  TMNCheck = Record
       ValidatorIP  : string;      // Validator IP
       Block        : integer;
       SignAddress  : string;
       PubKey       : string;
       ValidNodes   : string;
       Signature    : string;
       end;
   }

  TArrayCriptoOp = packed record
    tipo: Integer;
    Data: String;
    Result: String;
  end;

  {
  TNMSData = Packed Record
       Diff   : string;
       Hash   : String;
       Miner  : String;
       TStamp : string;
       Pkey   : string;
       Signat : string;
       end;
  }

  {
  TMNsData  = Packed Record
       ipandport  : string;
       address    : string;
       age        : integer;
       end;
  }
  {
  TGVT = packed record
       number   : string[2];
       owner    : string[32];
       Hash     : string[64];
       control  : integer;
       end;
  }
  {
  TNosoCFG = packed record
       NetStatus : string;
       SeedNode  : string;
       NTPNodes  : string;
       Pools     : string;
       end;
  }
  {TOrdIndex = record
       block  : integer;
       orders : string;
       end;}

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnDonate: TBitBtn;
    BitBtnWeb: TBitBtn;
    BSaveNodeOptions: TBitBtn;
    BitBtnPending: TBitBtn;
    BitBtnBlocks: TBitBtn;
    BTestNode: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    CBSendReports: TCheckBox;
    CBKeepBlocksDB: TCheckBox;
    CB_BACKRPCaddresses: TCheckBox;
    CB_WO_Autoupdate: TCheckBox;
    CBAutoIP: TCheckBox;
    CBRunNodeAlone: TCheckBox;
    CB_WO_HideEmpty: TCheckBox;
    Edit2: TEdit;
    Label19: TLabel;
    Memobannedmethods: TMemo;
    Label1: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    LabelNodesHash: TLabel;
    LE_Rpc_Pass: TEdit;
    Label13: TLabel;
    LE_Rpc_Port: TEdit;
    Label12: TLabel;
    LabeledEdit9: TEdit;
    Label11: TLabel;
    LabeledEdit8: TEdit;
    Label10: TLabel;
    LabeledEdit6: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    LabeledEdit5: TEdit;
    PageControl2: TPageControl;
    PCNodes: TPageControl;
    PC_Processes: TPageControl;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel23: TPanel;
    PanelTransferGVT: TPanel;
    PanelNodesHeaders: TPanel;
    Panel7: TPanel;
    Panel9: TPanel;
    SG_OpenThreads: TStringGrid;
    SG_FileProcs: TStringGrid;
    StaRPCimg: TImage;
    StaSerImg: TImage;
    StaConLab: TLabel;
    Imgs32: TImageList;
    ImgRotor: TImage;
    GridNodes: TStringGrid;
    GVTsGrid: TStringGrid;
    SGConSeeds: TStringGrid;
    TabGVTs: TTabSheet;
    TabConsensus: TTabSheet;
    TabSheet1: TTabSheet;
    TabNodesReported: TTabSheet;
    TabNodesVerified: TTabSheet;
    TabThreads: TTabSheet;
    TabFiles: TTabSheet;
    StaTimeLab: TLabel;
    SCBitSend: TBitBtn;
    SCBitClea: TBitBtn;
    CB_AUTORPC: TCheckBox;
    CB_WO_Multisend: TCheckBox;
    CheckBox4: TCheckBox;
    CB_RPCFilter: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Edit1: TEdit;
    ConsoleLine: TEdit;
    EditSCMont: TEdit;
    EditSCDest: TEdit;
    EditCustom: TEdit;
    Image1: TImage;
    ImageOptionsAbout: TImage;
    ImgSCMont: TImage;
    ImgSCDest: TImage;
    ImageOut: TImage;
    ImageInc: TImage;
    Imagenes: TImageList;
    LSCTop: TLabel;
    LabAbout: TLabel;
    LabelBigBalance: TLabel;
    Latido: TTimer;
    InfoTimer: TTimer;
    InicioTimer: TTimer;
    MainMenu: TMainMenu;
    MemoSCCon: TMemo;
    MemoConsola: TMemo;
    DataPanel: TStringGrid;
    MenuItem1: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    DireccionesPanel: TStringGrid;
    InfoPanel: TPanel;
    PanelCustom: TPanel;
    PanelSend: TPanel;
    ConsolePopUp2: TPopupMenu;
    ConsoLinePopUp2: TPopupMenu;
    SCBitCancel: TBitBtn;
    SCBitConf: TBitBtn;
    BDefAddr: TSpeedButton;
    BCustomAddr: TSpeedButton;
    BCopyAddr: TSpeedButton;
    BNewAddr: TSpeedButton;
    BOkCustom: TSpeedButton;
    SGridSC: TStringGrid;
    SBSCPaste: TSpeedButton;
    SBSCMax: TSpeedButton;
    TabAddresses: TTabSheet;
    TabNodes: TTabSheet;
    TabWalletMain: TPageControl;
    TopPanel: TPanel;
    StatusPanel: TPanel;
    RestartTimer: Ttimer;
    MemoRPCWhitelist: TMemo;
    Memo2: TMemo;
    MemoLog: TMemo;
    MemoExceptLog: TMemo;
    PageControl1: TPageControl;
    PCMonitor: TPageControl;
    PageMain: TPageControl;
    Server: TIdTCPServer;
    RPCServer: TIdHTTPServer;
    SG_Performance: TStringGrid;
    tabOptions: TTabSheet;
    TabOpt_Wallet: TTabSheet;
    TabProcesses: TTabSheet;
    TabNodeOptions: TTabSheet;
    Tab_Options_RPC: TTabSheet;
    Tab_Options_Trade: TTabSheet;
    TabMonitor: TTabSheet;
    TabDebug_Log: TTabSheet;
    TabSheet8: TTabSheet;
    TabMonitorMonitor: TTabSheet;
    Tab_Options_About: TTabSheet;
    TabWallet: TTabSheet;
    TabConsole: TTabSheet;

    procedure BitBtnDonateClick(Sender: TObject);
    procedure BitBtnWebClick(Sender: TObject);
    procedure BSaveNodeOptionsClick(Sender: TObject);
    procedure BTestNodeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CBKeepBlocksDBChange(Sender: TObject);
    procedure CBRunNodeAloneChange(Sender: TObject);
    procedure CBSendReportsChange(Sender: TObject);
    procedure CB_BACKRPCaddressesChange(Sender: TObject);
    procedure CB_RPCFilterChange(Sender: TObject);
    procedure CB_WO_AutoupdateChange(Sender: TObject);
    procedure CBAutoIPClick(Sender: TObject);
    procedure CB_WO_HideEmptyChange(Sender: TObject);
    procedure DataPanelResize(Sender: TObject);
    procedure DireccionesPanelDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DireccionesPanelResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GridNodesResize(Sender: TObject);
    procedure GVTsGridResize(Sender: TObject);
    procedure LE_Rpc_PassEditingDone(Sender: TObject);
    procedure LoadOptionsToPanel();
    procedure FormShow(Sender: TObject);
    procedure InicoTimerEjecutar(Sender: TObject);
    procedure MemobannedmethodsEditingDone(Sender: TObject);

    procedure MemoRPCWhitelistEditingDone(Sender: TObject);
    procedure PC_ProcessesResize(Sender: TObject);
    procedure RestartTimerEjecutar(Sender: TObject);
    procedure StartProgram();
    procedure ConsoleLineKeyup(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Grid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure Grid2PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure heartbeat(Sender: TObject);
    procedure InfoTimerEnd(Sender: TObject);
    function ClientsCount: Integer;
    procedure SG_PerformanceResize(Sender: TObject);
    procedure SG_OpenThreadsResize(Sender: TObject);
    procedure StaConLabDblClick(Sender: TObject);
    procedure SGConSeedsResize(Sender: TObject);
    procedure TabNodeOptionsShow(Sender: TObject);
    procedure Tab_Options_AboutResize(Sender: TObject);
    procedure TryCloseServerConnection(AContext: TIdContext; closemsg: String = '');
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPServer1Exception(AContext: TIdContext; AException: Exception);
    procedure BDefAddrOnClick(Sender: TObject);
    procedure BCustomAddrOnClick(Sender: TObject);
    procedure EditCustomKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BOkCustomClick(Sender: TObject);
    procedure PanelCustomMouseLeave(Sender: TObject);
    procedure BNewAddrOnClick(Sender: TObject);
    procedure BCopyAddrClick(Sender: TObject);
    procedure CheckForHint(Sender: TObject);
    procedure SBSCPasteOnClick(Sender: TObject);
    procedure SBSCMaxOnClick(Sender: TObject);
    procedure EditSCDestChange(Sender: TObject);
    procedure EditSCMontChange(Sender: TObject);
    procedure DisablePopUpMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure EditMontoOnKeyUp(Sender: TObject; var Key: Char);
    procedure SCBitSendOnClick(Sender: TObject);
    procedure SCBitCancelOnClick(Sender: TObject);
    procedure SCBitConfOnClick(Sender: TObject);
    procedure ResetSendFundsPanel(Sender: TObject);

    // NODE SERVER
    function TryMessageToNode(AContext: TIdContext; message: String): Boolean;
    function GetStreamFromContext(AContext: TIdContext;
      out LStream: TMemoryStream): Boolean;

    // RPC
    procedure RPCServerExecute(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);

    // MAIN MENU
    procedure MMImpWallet(Sender: TObject);
    procedure MMExpWallet(Sender: TObject);
    procedure MMQuit(Sender: TObject);
    procedure MMRestart(Sender: TObject);

    // CONSOLE POPUP
    procedure CheckConsolePopUp(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ConsolePopUpClear(Sender: TObject);
    procedure ConsolePopUpCopy(Sender: TObject);

    // CONSOLE LINE POPUP
    procedure CheckConsoLinePopUp(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ConsoLinePopUpClear(Sender: TObject);
    procedure ConsoLinePopUpCopy(Sender: TObject);
    procedure ConsoLinePopUpPaste(Sender: TObject);

    // OPTIONS
    // WALLET
    procedure CB_WO_MultisendChange(Sender: TObject);
    // RPC
    procedure CB_AUTORPCChange(Sender: TObject);
    procedure LE_Rpc_PortEditingDone(Sender: TObject);

  private

  public

  end;

procedure InitMainForm();
procedure CloseeAppSafely();
procedure UpdateStatusBar();
procedure CompleteInicio();



const
  HexAlphabet: String = '0123456789ABCDEF';
  ReservedWords: String = 'NULL,DELADDR';
  FundsAddress: String = 'NpryectdevepmentfundsGE';
  JackPotAddress: String = 'NPrjectPrtcRandmJacptE5';
  ValidProtocolCommands: String =
    '$PING$PONG$GETPENDING$NEWBL$GETRESUMEN$LASTBLOCK$GETCHECKS' +
    '$CUSTOMORDERADMINMSGNETREQ$REPORTNODE$GETMNS$BESTHASH$MNREPO$MNCHECK' +
    'GETMNSFILEMNFILEGETHEADUPDATE$GETSUMARY$GETGVTSGVTSFILE$SNDGVTGETCFGDATA' +
    'SETCFGDATA$GETPSOSPSOSFILE';
  HideCommands: String = 'CLEAR SENDPOOLSOLUTION SENDPOOLSTEPS DELBOTS';
  CustomValid: String =
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890@*+-_:';

  MainnetVersion = '0.4.4';
  {$IFDEF WINDOWS}
  RestartFileName = 'launcher.bat';
  updateextension = 'zip';
  {$ENDIF}
  {$IFDEF UNIX}
  RestartFileName = 'launcher.sh';
  updateextension = 'tgz';
  {$ENDIF}
  NodeRelease = 'Ab7';
  OficialRelease = True;
  BetaRelease = False;
  VersionRequired = '0.4.4';
  BuildDate = 'September 2024';
  {Developer addresses}
  ADMINHash = 'N3DthVsfEUtqrgWFHTEB6F88xLkT3Df';
  AdminPubKey =
    'BACo0LC7mWDn53orLANA2zKVSC1HfSe/jc/Ih7+cBQ1oh53Bu2Zo655WFmWnoKHEeaNflb1CR9WcfXuhmkKaY8Y=';
  Authorizedaddresses =
    'N3DthVsfEUtqrgWFHTEB6F88xLkT3Df N4ZR3fKhTUod34evnEcDQX3i6XufBDU';

  DefaultServerPort = 8080;
  MaxConecciones = 99;
  //Protocolo = 2;
  DefaultDonation = 10;
  // Custom values for coin
  SecondsPerBlock = 600;            // 10 minutes
  PremineAmount = 1030390730000;    // 1030390730000;
  InitialReward = 5000000000;       // Initial reward
  BlockHalvingInterval = 210000;    // 210000;
  HalvingSteps = 10;                // total number of halvings
  Comisiontrfr = 10000;             // ammount/Comisiontrfr = 0.01 % of the ammount
  ComisionCustom = 200000;          // 0.05 % of the Initial reward
  CoinSimbol = 'NOSO';              // Coin symbol
  CoinName = 'Noso';                // Coin name
  CoinChar = 'N';                   // Char for addresses
  MinimunFee = 10;
  NewMinFee = 1000000;              // Minimun fee for transfer
  PoSPercentage = 1000;             // PoS part: reward * PoS / 10000
  MNsPercentage = 2000;
  PosStackCoins = 20;               // PoS stack ammoount: supply*20 / PoSStack
  PoSBlockStart: Integer = 8425;    // first block with PoSPayment
  PoSBlockEnd: Integer = 88500;     // To verify
  MNBlockStart: Integer = 48010;    // First block with MNpayments
  InitialBlockDiff = 60;            // First 20 blocks diff
  GenesysTimeStamp = 1615132800;    // 1615132800;
  AvailableMarkets = '/LTC';
  SumMarkInterval = 100;
  SecurityBlocks = 4000;
  //GVTBaseValue     = 70000000000;
  Update050Block = 120000;

var
  Form1: TForm1;
  //Customizationfee : int64 = InitialReward div ComisionCustom;
  {Options}
  FileAdvOptions: textfile;
  S_AdvOpt: Boolean = False;
  RPCPort: Integer = 8078;
  RPCPass: String = 'default';
  MaxPeersAllow: Integer = 50;
  WO_AutoServer: Boolean = False;
  WO_PosWarning: Int64 = 7;
  WO_MultiSend: Boolean = False;
  WO_HideEmpty: Boolean = False;
  WO_Language: String = 'en';
  WO_LastPoUpdate: String = MainnetVersion + NodeRelease;
  WO_CloseStart: Boolean = True;
  WO_AutoUpdate: Boolean = True;
  WO_SendReport: Boolean = False;
  WO_StopGUI: Boolean = False;
  WO_BlockDB: Boolean = False;
  WO_PRestart: Int64 = 0;
  WO_skipBlocks: Boolean = False;
  RPCFilter: Boolean = True;
  RPCWhitelist: String = '127.0.0.1,localhost';
  RPCBanned: String = '';
  RPCAuto: Boolean = False;
  RPCSaveNew: Boolean = False;
  //MN_IP            : string = 'localhost';
  //MN_Port          : string = '8080';
  //MN_Funds         : string = '';
  //MN_Sign          : string = '';
  MN_AutoIP: Boolean = False;
  //MN_FileText      : String = '';
  WO_FullNode: Boolean = True;

  {Network}
  MaxOutgoingConnections: Integer = 3;
  {
  SlotLines        : array [1..MaxConecciones] of TStringList;
  CanalCliente     : array [1..MaxConecciones] of TIdTCPClient;
  }
  //ListadoBots      : array of BotData;
  //ListaNodos       : array of NodeData;
  //ArrayPoolTXs     : Array of TOrderData;
  ArrayOrderIDsProcessed: array of String;
  OutgoingMsjs: TStringList;
  KeepServerOn: Boolean = False;
  LastTryServerOn: Int64 = 0;
  ServerStartTime: Int64 = 0;
  {
  DownloadHeaders  : boolean = false;
  DownloadSumary   : Boolean = false;
  DownLoadBlocks   : boolean = false;
  DownLoadGVTs     : boolean = false;
  DownloadPSOs     : boolean = false;
  }
  RebuildingSumary: Boolean = False;
  //OpenReadClientThreads : integer = 0;

  // Threads
  SendOutMsgsThread: TThreadSendOutMsjs;
  KeepConnectThread: TThreadKeepConnect;
  IndexerThread: TThreadIndexer;
  ThreadMNs: TUpdateMNs;
  CryptoThread: TCryptoThread;
  UpdateLogsThread: TUpdateLogs;

  // GUI/APP related
  ConnectedRotor: Integer = 0;
  EngineLastUpdate: Int64 = 0;
  LastLogLine: String = '';
  RestartNosoAfterQuit: Boolean = False;
  U_DirPanel: Boolean = False;
  U_DataPanel: Boolean = True;
  G_ClosingAPP: Boolean = False;
  MyCurrentBalance: Int64 = 0;
  G_Launching: Boolean = True;
  G_CloseRequested: Boolean = False;
  G_LastPing: Int64;
  G_TotalPings: Int64 = 0;
  LastCommand: String = '';
  ProcessLines: TStringList;
  //LastBotClear         : string = '';
  S_Wallet: Boolean = False;
  MontoIncoming: Int64 = 0;
  MontoOutgoing: Int64 = 0;
  InfoPanelTime: Integer = 0;

  // FormState
  FormState_Top: Integer;
  FormState_Left: Integer;
  FormState_Heigth: Integer;
  FormState_Width: Integer;
  FormState_Status: Integer;

  // Masternodes
  //G_MNVerifications  : integer = 0;
  //ArrayMNsData       : array of TMNsData;
  LastTimeReportMyMN: Int64 = 0;
  MNsArray: array of TMasterNode;
  //WaitingMNs         : array of String;
  U_MNsGrid: Boolean = False;
  U_MNsGrid_Last: Int64 = 0;

  //MNsList       : array of TMnode;
  //ArrMNChecks   : array of TMNCheck;
  MNsRandomWait: Integer = 0;


  {
  //MySumarioHash : String = '';
  MyLastBlock     : integer = 0;
  MyLastBlockHash : String = '';
  MyResumenHash   : String = '';
  MyGVTsHash      : string = '';
  MyCFGHash       : string = '';
  MyPublicIP      : String = '';
  MyMNsHash       : String = '';
  }




  {LastBlockData : BlockHeaderData;}
  BuildingBlock: Integer = 0;


  Last_SyncWithMainnet: Int64 = 0;
  {
  LastTimeRequestSumary        : int64 = 0;
  LastTimeRequestBlock         : int64 = 0;
  LastTimeRequestResumen       : int64 = 0;
  LastTimePendingRequested     : int64 = 0;
  }
  //ForceCompleteHeadersDownload : boolean = false;
  {
  LastTimeMNHashRequestes      : int64 = 0;
  LastTimeBestHashRequested    : int64 = 0;
  LastTimeMNsRequested         : int64 = 0;
  LastTimeChecksRequested      : int64 = 0;
  LastRunMNVerification        : int64 = 0;
  LasTimeGVTsRequest           : int64 = 0;
  }
  //LasTimeCFGRequest            : int64 = 0;
  //LasTimePSOsRequest           : int64 = 0;

  // Variables asociadas a mi conexion
  MyConStatus: Integer = 0;
  STATUS_Connected: Boolean = False;




  BuildNMSBlock: Int64 = 0;

  ArrayCriptoOp: array of TArrayCriptoOp;

  // Critical Sections
  CSProcessLines: TRTLCriticalSection;
  CSOutgoingMsjs: TRTLCriticalSection;
  CSBlocksAccess: TRTLCriticalSection;
  //CSPending     : TRTLCriticalSection;
  CSCriptoThread: TRTLCriticalSection;
  CSClosingApp: TRTLCriticalSection;
  //CSClientReads : TRTLCriticalSection;
  //CSGVTsArray   : TRTLCriticalSection;
  CSNosoCFGStr: TRTLCriticalSection;

  //MNs system
  //CSMNsArray    : TRTLCriticalSection;
  //CSWaitingMNs  : TRTLCriticalSection;
  //CSMNsChecks   : TRTLCriticalSection;

  CSIdsProcessed: TRTLCriticalSection;


  // Outgoing lines, needs to be initialized
  //CSOutGoingArr : array[1..MaxConecciones] of TRTLCriticalSection;
  //ArrayOutgoing : array[1..MaxConecciones] of array of string;
  //CSIncomingArr : array[1..MaxConecciones] of TRTLCriticalSection;



  // Filename variables
  MarksDirectory: String = 'NOSODATA' + DirectorySeparator + 'SUMMARKS' +
    DirectorySeparator;
  GVTMarksDirectory: String = 'NOSODATA' + DirectorySeparator +
  'SUMMARKS' + DirectorySeparator + 'GVTS' + DirectorySeparator;
  UpdatesDirectory: String = 'NOSODATA' + DirectorySeparator + 'UPDATES' +
    DirectorySeparator;
  LogsDirectory: String = 'NOSODATA' + DirectorySeparator + 'LOGS' + DirectorySeparator;
  ExceptLogFilename: String = 'NOSODATA' + DirectorySeparator + 'LOGS' +
    DirectorySeparator + 'exceptlog.txt';
  ConsoleLogFilename: String = 'NOSODATA' + DirectorySeparator +
  'LOGS' + DirectorySeparator + 'console.txt';
  NodeFTPLogFilename: String = 'NOSODATA' + DirectorySeparator +
  'LOGS' + DirectorySeparator + 'nodeftp.txt';
  DeepDebLogFilename: String = 'NOSODATA' + DirectorySeparator +
  'LOGS' + DirectorySeparator + 'deepdeb.txt';
  EventLogFilename: String = 'NOSODATA' + DirectorySeparator + 'LOGS' +
    DirectorySeparator + 'eventlog.txt';
  ResumeLogFilename: String = 'NOSODATA' + DirectorySeparator + 'LOGS' +
    DirectorySeparator + 'report.txt';
  PerformanceFIlename: String = 'NOSODATA' + DirectorySeparator +
  'LOGS' + DirectorySeparator + 'performance.txt';
  AdvOptionsFilename: String = 'NOSODATA' + DirectorySeparator + 'advopt.txt';
  {MasterNodesFilename : string= 'NOSODATA'+DirectorySeparator+'masternodes.txt';}
  ZipHeadersFileName: String = 'NOSODATA' + DirectorySeparator + 'blchhead.zip';
  {GVTsFilename        : string= 'NOSODATA'+DirectorySeparator+'gvts.psk';}
  ClosedAppFilename: String = 'NOSODATA' + DirectorySeparator + 'LOGS' +
    DirectorySeparator + 'proclo.dat';
  RPCBakDirectory: String = 'NOSODATA' + DirectorySeparator + 'SUMMARKS' +
    DirectorySeparator + 'RPC' + DirectorySeparator;


implementation

uses
  mpgui, mpdisk, mpParser, mpRed, nosotime, mpProtocol, mpcoin,
  mpRPC, mpblock;

  {$R *.lfm}

{
// Identify the pool miners connections
constructor TNodeConnectionInfo.Create;
Begin
FTimeLast:= 0;
End;
}

constructor TServerTipo.Create;
begin
  VSlot := -1;
end;

 // ***************
 // *** THREADS ***
 // ***************

{$REGION Thread update logs}

constructor TUpdateLogs.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TUpdateLogs.UpdateConsole();
begin
  if not WO_StopGUI then
    form1.MemoConsola.Lines.Add(LastLogLine);
end;

procedure TUpdateLogs.UpdateEvents();
begin
  if not WO_StopGUI then
    form1.MemoLog.Lines.Add(LastLogLine);
end;

procedure TUpdateLogs.UpdateExceps();
begin
  if not WO_StopGUI then
    form1.MemoExceptLog.Lines.Add(LastLogLine);
end;

procedure TUpdateLogs.Execute;
begin
  AddNewOpenThread('UpdateLogs', UTCTime);
  while not terminated do
  begin
    sleep(10);
    UpdateOpenThread('UpdateLogs', UTCTime);
    while GetLogLine('console', lastlogline) do Synchronize(@UpdateConsole);
    while GetLogLine('events', lastlogline) do Synchronize(@UpdateEvents);
    while GetLogLine('exceps', lastlogline) do Synchronize(@UpdateExceps);
    GetLogLine('nodeftp', lastlogline);
    // Deep debug
    repeat
    until not GetDeepDebLine(lastlogline);
  end;
end;

{$ENDREGION Thread update logs}

{$REGION Thread Directive}

constructor TThreadDirective.Create(const CreatePaused: Boolean; const TCommand: String);
begin
  inherited Create(CreatePaused);
  Command := TCommand;
end;

procedure TThreadDirective.Execute;
var
  TimeToRun: Int64;
  TFinished: Boolean = False;
begin
  AddNewOpenThread('Directives', UTCTime);
  if command = 'rpcrestart' then
  begin
    timetorun := BlockAge + 3;
    command := 'restart';
  end
  else
    TimeToRun := 50 + (MNsRandomWait * 20);
  while not Tfinished do
  begin
    sleep(10);
    if BlockAge = TimeToRun then
    begin
      ProcesslinesAdd(Command);
      TFinished := True;
    end;
  end;
end;

{$ENDREGION Thread Directive}

{$REGION Thread Update MNs}

constructor TUpdateMNs.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

// Process the Masternodes reports
procedure TUpdateMNs.Execute;
const
  LastIPVerify: Int64 = 0;
var
  TextLine: String;
  ReportInfo: String = '';
  MyIP: String;
begin
  AddNewOpenThread('Masternodes', UTCTime);
  Randomize;
  MNsRandomWait := Random(21);
  while not terminated do
  begin
    UpdateOpenThread('Masternodes', UTCTime);
    if UTCTime mod 10 = 0 then
    begin
      if ((IsValidator(LocalMN_IP)) and (BlockAge > 500 + (MNsRandomWait div 4)) and
        (not IsMyMNCheckDone) and (BlockAge < 575) and
        (LastRunMNVerification <> UTCTime) and (MyConStatus = 3) and
        (VerifyThreadsCount <= 0)) then
      begin
        LastRunMNVerification := UTCTime;
        TextLine := RunMNVerification(MyLastBlock, GetSynctus,
          LocalMN_IP, GetWallArrIndex(WallAddIndex(LocalMN_Sign)).PublicKey,
          GetWallArrIndex(WallAddIndex(LocalMN_Sign)).PrivateKey);
        OutGoingMsjsAdd(ProtocolLine(MNCheck) + TextLine);
        //ToLog('console','Masternodes Verification completed: '+TextLine)
      end;
    end;
    if ((BlockAge > 10) and (LastIPVerify < UTCtime)) then
    begin
      LastIPVerify := NextBlockTimeStamp;
      MyIP := GetMiIP();
      //ToLog('console','Auto IP executed');
      if ((MyIP <> '') and (MyIP <> LocalMN_IP) and (MyIP <> 'Closing NODE') and
        (MyIP <> 'BANNED') and (IsValidIP(MyIP))) then
      begin
        ToLog('console', 'Auto IP: updated to ' + MyIp);
        LocalMN_IP := MyIP;
        S_AdvOpt := True;
      end;
    end;
    while LengthWaitingMNs > 0 do
    begin
      TextLine := GetWaitingMNs;
      if not IsIPMNAlreadyProcessed(TextLine) then
      begin
        ReportInfo := CheckMNReport(TextLine, MyLastBlock);
        if ReportInfo <> '' then
          outGOingMsjsAdd(GetPTCEcn + ReportInfo);
        sleep(1);
      end;
    end;
    Sleep(10);
  end;
end;

{$ENDREGION Thread Update MNs}

{$REGION Thread Crypto}

constructor TCryptoThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TCryptoThread.Execute;
var
  NewAddrss: Integer = 0;
  PosRef: Integer;
  cadena, claveprivada, firma, resultado: String;
  NewAddress: WalletData;
  PubKey, PriKey: String;
begin
  AddNewOpenThread('Crypto', UTCTime);
  while not terminated do
  begin
    UpdateOpenThread('Crypto', UTCTime);
    NewAddrss := 0;
    if length(ArrayCriptoOp) > 0 then
    begin
      if ArrayCriptoOp[0].tipo = 0 then
      begin

      end
      else if ArrayCriptoOp[0].tipo = 1 then // Crear direccion
      begin
        NewAddress := Default(WalletData);
        NewAddress.Hash := GenerateNewAddress(PubKey, PriKey);
        NewAddress.PublicKey := pubkey;
        NewAddress.PrivateKey := PriKey;
        InsertToWallArr(NewAddress);
        S_Wallet := True;
        U_DirPanel := True;
        Inc(NewAddrss);
      end
      else if ArrayCriptoOp[0].tipo = 2 then // customizar
      begin
        posRef := pos('$', ArrayCriptoOp[0].Data);
        cadena := copy(ArrayCriptoOp[0].Data, 1, posref - 1);
        claveprivada := copy(ArrayCriptoOp[0].Data, posref + 1, length(
          ArrayCriptoOp[0].Data));
        firma := GetStringSigned(cadena, claveprivada);
        resultado := StringReplace(ArrayCriptoOp[0].Result, '[[RESULT]]',
          firma, [rfReplaceAll, rfIgnoreCase]);
        OutgoingMsjsAdd(resultado);
        OutText('Customization sent', False, 2);
      end
      else if ArrayCriptoOp[0].tipo = 3 then // enviar fondos
      begin
        try
          Sendfunds(ArrayCriptoOp[0].Data);
        except
          ON E: Exception do
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + format(rs2501, [E.Message]));
        end{Try};
      end
      else if ArrayCriptoOp[0].tipo = 4 then // recibir customizacion
      begin
        try
          PTC_Custom(ArrayCriptoOp[0].Data);
        except
          ON E: Exception do
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + format(rs2502, [E.Message]));
        end{Try};
      end
      else if ArrayCriptoOp[0].tipo = 5 then // recibir transferencia
      begin
        try
          PTC_Order(ArrayCriptoOp[0].Data);
        except
          ON E: Exception do
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + format(rs2503, [E.Message]));
        end{Try};
      end
      else if ArrayCriptoOp[0].tipo = 6 then // Send GVT
      begin
        try
          SendGVT(ArrayCriptoOp[0].Data);
        except
          ON E: Exception do
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + format(rs2504, [E.Message]));
        end{Try};
      end
      else if ArrayCriptoOp[0].tipo = 7 then // Send GVT
      begin
        try
          PTC_SendGVT(ArrayCriptoOp[0].Data);
        except
          ON E: Exception do
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + format(rs2505, [E.Message]));
        end{Try};
      end
      else
      begin
        ToLog('exceps', 'Invalid cryptoop: ' + ArrayCriptoOp[0].tipo.ToString);
      end;
      DeleteCriptoOp();
      sleep(10);
    end;
    if NewAddrss > 0 then OutText(IntToStr(NewAddrss) + ' new addresses', False, 2);
    Sleep(10);
  end;
end;

{$ENDREGION Thread Crypto}

{$REGION Thread Send outgoing msgs}

constructor TThreadSendOutMsjs.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

// Send the outgoing messages
procedure TThreadSendOutMsjs.Execute;
var
  Slot: Integer = 1;
  Linea: String;
  Counter: Int64 = 0;
begin
  AddNewOpenThread('SendMSGS', UTCTime);
  while not terminated do
  begin
    UpdateOpenThread('SendMSGS', UTCTime);
    if OutgoingMsjs.Count > 0 then
    begin
      Linea := OutgoingMsjsGet();
      if Linea <> '' then
      begin
        for Slot := 1 to MaxConecciones do
        begin
          try
            if IsSlotConnected(slot) then PTC_SendLine(Slot, linea);
          except
            on E: Exception do
              ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz',
                Now) + ' -> ' + format(rs0008, [E.Message]));
            //ToLog('exceps',FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now)+' -> '+'Error sending outgoing message: '+E.Message);
          end{Try};
        end;
      end;
      Sleep(10);
    end;
    Sleep(10);
  end;
end;

{$ENDREGION Thread Send outgoing msgs}

{$REGION Thread keepConnected}

constructor TThreadKeepConnect.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TThreadKeepConnect.Execute;
const
  LastTrySlot: Integer = 0;
  LAstTryTime: Int64 = 0;
  Unables: Integer = 0;
  PRestartTime: Int64 = 0;
var
  TryThis: Boolean = True;
  Loops: Integer = 0;
  OutGoing: Integer;
begin
  AddNewOpenThread('KeepConnect', UTCTime);
  if WO_PRestart > 0 then
  begin
    PRestartTime := UTCTime + (WO_PRestart * 600);
    ToLog('Console', 'PRestart set at ' + TimestampToDate(PRestartTime));
  end;
  while not terminated do
  begin
    UpdateOpenThread('KeepConnect', UTCTime);
    TryThis := True;
    if getTotalConexiones >= 99 then TryThis := False;
    if GetTotalSyncedConnections >= 3 then TryThis := False;
    if ((BlockAge < 10) or (blockAge > 595)) then TryThis := False;
    if trythis then
    begin
      Inc(LastTrySlot);
      if LastTrySlot >= NodesListLen then LastTrySlot := 0;
      if ((GetSlotFromIP(NodesIndex(LastTrySlot).ip) = 0) and
        (GetFreeSlot() > 0) and (NodesIndex(LastTrySlot).ip <> LocalMN_IP)) then
        ConnectClient(NodesIndex(LastTrySlot).ip, NodesIndex(LastTrySlot).port);
    end;
    sleep(3000);
    if PRestartTime > 0 then
    begin
      if ((blockAge > 120) and (blockAge < 450) and (UTCTime > PRestartTime)) then
        ProcesslinesAdd('restart');
    end;
  end;
  if EngineLastUpdate + 10 < UTCTime then Parse_RestartNoso;
  CloseOpenThread('KeepConnect');
end;

{$ENDREGION Thread keepConnected}

{$REGION Thread Indexer}

constructor TThreadIndexer.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
end;

procedure TThreadIndexer.Execute;
begin
  AddNewOpenThread('Indexer', UTCTime);
  tolog('console', 'Starting indexer');
  while not terminated do
  begin
    if not WO_BlockDB then
    begin
      sleep(1000);
      continue;
    end;
    if MyLastBlock > GetDBLastBlock then
    begin
      if ((blockAge > 60) and (copy(MyLastBlockHash, 1, 5) =
        copy(getconsensus(10), 1, 5))) then
      begin
        UpdateBlockDatabase;
        ;
      end;
    end;
    sleep(1000);
  end;
  CloseOpenThread('Indexer');
end;

{$ENDREGION Thread Indexer}

 //***********************
 // *** FORM RELATIVES ***
 //***********************

{$REGION Form1}

procedure TForm1.FormCreate(Sender: TObject);
var
  counter: Integer;
begin
  ProcessLines := TStringList.Create;
  OutgoingMsjs := TStringList.Create;
  Randomize;
  InitCriticalSection(CSProcessLines);
  InitCriticalSection(CSOutgoingMsjs);
  InitCriticalSection(CSBlocksAccess);
  //InitCriticalSection(CSPending);
  InitCriticalSection(CSCriptoThread);
  //InitCriticalSection(CSMNsArray);
  //InitCriticalSection(CSWaitingMNs);
  InitCriticalSection(CSMNsChecks);
  InitCriticalSection(CSClosingApp);
  //InitCriticalSection(CSNosoCFGStr);
  InitCriticalSection(CSIdsProcessed);
  for counter := 1 to MaxConecciones do
  begin
    //InitCriticalSection(CSOutGoingArr[counter]);
    //InitCriticalSection(CSIncomingArr[counter]);
    //SetLength(ArrayOutgoing[counter],0);
    //SlotLines[counter] := TStringlist.Create;
    //CanalCliente[counter] := TIdTCPClient.Create(form1);
  end;
  CreateFormInicio();
  CreateFormSlots();
  SetLength(ArrayOrderIDsProcessed, 0);
  //SetLength(ArrayMNsData,0);
  //Setlength(ArrayPoolTXs,0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  contador: Integer;
begin
  DoneCriticalSection(CSProcessLines);
  DoneCriticalSection(CSOutgoingMsjs);
  DoneCriticalSection(CSBlocksAccess);
  //DoneCriticalSection(CSPending);
  DoneCriticalSection(CSCriptoThread);
  //DoneCriticalSection(CSMNsArray);
  //DoneCriticalSection(CSWaitingMNs);
  DoneCriticalSection(CSMNsChecks);
  DoneCriticalSection(CSClosingApp);
  //DoneCriticalSection(CSNosoCFGStr);
  DoneCriticalSection(CSIdsProcessed);
  for contador := 1 to MaxConecciones do
  begin
    //DoneCriticalSection(CSOutGoingArr[contador]);
    //DoneCriticalSection(CSIncomingArr[contador]);
  end;
  //for contador := 1 to maxconecciones do
  //If Assigned(SlotLines[contador]) then SlotLines[contador].Free;
  form1.Server.Free;
  form1.RPCServer.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  InfoPanel.Left := (Form1.ClientWidth div 2) - (InfoPanel.Width div 2);
  InfoPanel.Top := (Form1.ClientHeight div 2) - (InfoPanel.Height div 2);
end;

procedure TForm1.FormShow(Sender: TObject);
const
  GoAhead: Boolean = True;
begin
  if GoAhead then
  begin
    GoAhead := False;
    form1.Visible := False;
    forminicio.Visible := True;
    Form1.InicioTimer := TTimer.Create(Form1);
    Form1.InicioTimer.Enabled := True;
    Form1.InicioTimer.Interval := 1;
    Form1.InicioTimer.OnTimer := @form1.InicoTimerEjecutar;

    Form1.RestartTimer := TTimer.Create(Form1);
    Form1.RestartTimer.Enabled := False;
    Form1.RestartTimer.Interval := 1000;
    Form1.RestartTimer.OnTimer := @form1.RestartTimerEjecutar;
  end;
end;

// Manual request to close the app
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  G_CloseRequested := True;
  CanClose := G_ClosingAPP;
end;

{$ENDREGION}

{$REGION Start app}

procedure TForm1.InicoTimerEjecutar(Sender: TObject);
begin
  InicioTimer.Enabled := False;
  StartProgram;
end;

// Init the mainform
procedure InitMainForm();
begin
  // Make sure ALL tabs are set correct at startup
  Form1.PageMain.ActivePage := Form1.TabWallet;
  Form1.TabWalletMain.ActivePage := Form1.TabAddresses;
  Form1.PageControl1.ActivePage := Form1.TabOpt_Wallet;
  Form1.PCMonitor.ActivePage := form1.TabDebug_Log;
  // Resize all grids at launch
  Form1.SG_PerformanceResize(nil);
  {Add all resize methods here}
  form1.DataPanel.FocusRectVisible := False;
  form1.DataPanel.ColWidths[0] := 79;
  form1.DataPanel.ColWidths[1] := 115;
  form1.DataPanel.ColWidths[2] := 79;
  form1.DataPanel.ColWidths[3] := 115;
  Form1.imagenes.GetBitMap(9, Form1.ImageInc.Picture.Bitmap);
  Form1.imagenes.GetBitmap(10, Form1.Imageout.Picture.Bitmap);
  form1.DireccionesPanel.Options :=
    form1.DireccionesPanel.Options + [goRowSelect] - [goRangeSelect];
  form1.DireccionesPanel.ColWidths[0] := 260;
  form1.DireccionesPanel.ColWidths[1] := 107;
  form1.DireccionesPanel.FocusRectVisible := False;
  form1.SGConSeeds.FocusRectVisible := False;
  Form1.BDefAddr.Parent := form1.DireccionesPanel;
  form1.BCustomAddr.Parent := form1.DireccionesPanel;
  form1.BCopyAddr.Parent := form1.DireccionesPanel;
  Form1.BNewAddr.Parent := form1.DireccionesPanel;
  Form1.SGridSC.FocusRectVisible := False;
  Form1.imagenes.GetBitMap(54, form1.ImgRotor.picture.BitMap);
  form1.LabAbout.Caption := CoinName + ' project' + SLINEBREAK +
    'Brought to you by the Noso Team' + SLINEBREAK + 'Crypto routines by Xor-el' +
    SLINEBREAK + 'Version ' + MainnetVersion + NodeRelease + SLINEBREAK +
    'Protocol ' + IntToStr(Protocolo) + SLINEBREAK + BuildDate;
  form1.SG_Performance.FocusRectVisible := False;
  form1.SG_Performance.ColWidths[0] := 142;
  form1.SG_Performance.ColWidths[1] := 73;
  form1.SG_Performance.ColWidths[2] := 73;
  form1.SG_Performance.ColWidths[3] := 73;

  Form1.Latido := TTimer.Create(Form1);
  Form1.Latido.Enabled := False;
  Form1.Latido.Interval := 200;
  Form1.Latido.OnTimer := @form1.heartbeat;

  Form1.InfoTimer := TTimer.Create(Form1);
  Form1.InfoTimer.Enabled := False;
  Form1.InfoTimer.Interval := 50;
  Form1.InfoTimer.OnTimer := @form1.InfoTimerEnd;

  Form1.Server := TIdTCPServer.Create(Form1);
  Form1.Server.DefaultPort := DefaultServerPort;
  Form1.Server.Active := False;
  Form1.Server.UseNagle := True;
  Form1.Server.TerminateWaitTime := 10000;
  Form1.Server.OnExecute := @form1.IdTCPServer1Execute;
  Form1.Server.OnConnect := @form1.IdTCPServer1Connect;
  Form1.Server.OnDisconnect := @form1.IdTCPServer1Disconnect;
  Form1.Server.OnException := @Form1.IdTCPServer1Exception;

  Form1.RPCServer := TIdHTTPServer.Create(Form1);
  Form1.RPCServer.DefaultPort := RPCPort;
  Form1.RPCServer.Active := False;
  Form1.RPCServer.UseNagle := True;
  Form1.RPCServer.TerminateWaitTime := 5000;
  Form1.RPCServer.OnCommandGet := @form1.RPCServerExecute;
end;

// Start the application
procedure TForm1.StartProgram();
begin
  Form1.InfoPanel.Visible := False;
  AddNewOpenThread('Main', UTCTime);
  if FileStructure > 0 then
  begin
    Application.MessageBox(
      'There was an error creating the files structure and the program will close.',
      'NosoNode Error', MB_ICONINFORMATION);
    Halt();
  end;
  MixTxtFiles([DeepDebLogFilename, ConsoleLogFilename, EventLogFilename,
    ExceptLogFilename, NodeFTPLogFilename, PerformanceFIlename],
    ResumeLogFilename, True);
  InitDeepDeb(DeepDebLogFilename, format('( %s - %s )',
    [MainnetVersion + NodeRelease, OSVersion]));
  NosoDebug_UsePerformance := True;
  UpdateLogsThread := TUpdateLogs.Create(True);
  UpdateLogsThread.FreeOnTerminate := True;
  UpdateLogsThread.Start;
  CreateNewLog('console', ConsoleLogFilename);
  CreateNewLog('events', EventLogFilename);
  CreateNewLog('exceps', ExceptLogFilename);
  CreateNewLog('nodeftp', NodeFTPLogFilename);
  OutText(rs0022, False, 1); //'✓ Files tree ok'
  InitMainForm();
  OutText(rs0023, False, 1); //✓ GUI initialized
  VerifyFiles();
  if ((not fileExists(ClosedAppFilename)) and (WO_Sendreport)) then
  begin
    if SEndFileViaTCP(ResumeLogFilename, 'REPORT', 'debuglogs.nosocoin.com', 18081) then
    begin
      OutText('✓ Bug report sent to developers', False, 1);
    end
    else
      OutText('x Error sending report to developers', False, 1);
  end;
  TryDeleteFile(ClosedAppFilename);
  InitGUI();
  GetTimeOffset(PArameter(GetCFGDataStr, 2));
  OutText('✓ Mainnet time synced', False, 1);
  UpdateMyData();
  OutText(rs0024, False, 1); //'✓ My data updated'
  LoadOptionsToPanel();
  form1.Caption := coinname + format(rs0027, [MainnetVersion, NodeRelease]);
  Application.Title := coinname + format(rs0027, [MainnetVersion, NodeRelease]);
  // Wallet
  ToLog('console', coinname + format(rs0027, [MainnetVersion, NodeRelease]));
  if BetaRelease then ToLog('console', '*** WARNING ***' + slinebreak +
      'This is a beta version (' + MainnetVersion + NodeRelease +
      ') Use it carefully, do not store funds on its wallet and report any issue to development team.');
  UpdateMyGVTsList;
  OutText(rs0088, False, 1); // '✓ My GVTs grid updated';
  if fileexists(RestartFileName) then
  begin
    Deletefile(RestartFileName);
    OutText(rs0069, False, 1); // '✓ Launcher file deleted';
  end;
  Form1.Latido.Enabled := True;
  OutText('Noso is ready', False, 1);
  SetNodesArray(GetCFGDataStr(1));
  StartAutoConsensus;
  if WO_CloseStart then
  begin
    G_Launching := False;
    if WO_autoserver then KeepServerOn := True;
    FormInicio.BorderIcons := FormInicio.BorderIcons + [bisystemmenu];
    SetLength(ArrayCriptoOp, 0);
    Setlength(MNsArray, 0);
    Setlength(MNsList, 0);
    Setlength(ArrMNChecks, 0);
    //Setlength(WaitingMNs,0);
    ThreadMNs := TUpdateMNs.Create(True);
    ThreadMNs.FreeOnTerminate := True;
    ThreadMNs.Start;
    CryptoThread := TCryptoThread.Create(True);
    CryptoThread.FreeOnTerminate := True;
    CryptoThread.Start;
    SendOutMsgsThread := TThreadSendOutMsjs.Create(True);
    SendOutMsgsThread.FreeOnTerminate := True;
    SendOutMsgsThread.Start;
    KeepConnectThread := TThreadKeepConnect.Create(True);
    KeepConnectThread.FreeOnTerminate := True;
    KeepConnectThread.Start;
    if WO_BlockDB then
    begin
      IndexerThread := TThreadIndexer.Create(True);
      IndexerThread.FreeOnTerminate := True;
      IndexerThread.Start;
    end;
    ToLog('events', TimeToStr(now) + rs0029);
    //NewLogLines := NewLogLines-1; //'Noso session started'
    info(rs0029);  //'Noso session started'
    infopanel.BringToFront;
    forminicio.Visible := False;
    form1.Visible := True;
    if FormState_Status = 0 then
    begin
      form1.Top := FormState_Top;
      form1.Left := FormState_Left;
    end;
    Form1.RestartTimer.Enabled := True;
  end // CLOSE start form
  else
    FormInicio.BorderIcons := FormInicio.BorderIcons - [biminimize] + [bisystemmenu];
end;

procedure CompleteInicio();
begin
  G_Launching := False;
  if WO_autoserver then KeepServerOn := True;
  FormInicio.BorderIcons := FormInicio.BorderIcons + [bisystemmenu];
  SetLength(ArrayCriptoOp, 0);
  Setlength(MNsArray, 0);
  Setlength(MNsList, 0);
  Setlength(ArrMNChecks, 0);
  //Setlength(WaitingMNs,0);
  ThreadMNs := TUpdateMNs.Create(True);
  ThreadMNs.FreeOnTerminate := True;
  ThreadMNs.Start;
  CryptoThread := TCryptoThread.Create(True);
  CryptoThread.FreeOnTerminate := True;
  CryptoThread.Start;
  SendOutMsgsThread := TThreadSendOutMsjs.Create(True);
  SendOutMsgsThread.FreeOnTerminate := True;
  SendOutMsgsThread.Start;
  ToLog('events', TimeToStr(now) + rs0029);
  //NewLogLines := NewLogLines-1; //'Noso session started'
  info(rs0029);  //'Noso session started'
  form1.infopanel.BringToFront;
  forminicio.Visible := False;
  form1.Visible := True;
  if FormState_Status = 0 then
  begin
    form1.Top := FormState_Top;
    form1.Left := FormState_Left;
  end;
  Form1.RestartTimer.Enabled := True;
end;

procedure TForm1.LoadOptionsToPanel();
begin
  // WALLET
  CB_WO_HideEmpty.Checked := WO_HideEmpty;
  CB_WO_Multisend.Checked := WO_Multisend;
  CB_WO_Autoupdate.Checked := WO_AutoUpdate;
  CBSendReports.Checked := WO_SendReport;
  // RPC
  LE_Rpc_Port.Text := IntToStr(RPCPort);
  LE_Rpc_Pass.Text := RPCPass;
  CB_BACKRPCaddresses.Checked := RPCSaveNew;
  CBRunNodeAlone.Checked := WO_StopGUI;
  CBKeepBlocksDB.Checked := WO_BlockDB;
  CB_RPCFilter.Checked := RPCFilter;
  MemoRPCWhitelist.Text := RPCWhitelist;
  Memobannedmethods.Text := RPCBanned;
  if not RPCFilter then MemoRPCWhitelist.Enabled := False;
  CB_AUTORPC.Checked := RPCAuto;
end;

{$ENDREGION}

 //*********************
 // *** GUI CONTROLS ***
 //*********************

{$REGION GUI controls}

// Double click open conexions slots form
procedure TForm1.StaConLabDblClick(Sender: TObject);
begin
  formslots.Visible := True;
end;

// Check keypress on commandline
procedure TForm1.ConsoleLineKeyup(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LineText: String;
begin
  LineText := ConsoleLine.Text;
  if Key = VK_RETURN then
  begin
    ConsoleLine.Text := '';
    LastCommand := LineText;
    if LineText <> '' then ProcessLinesAdd(LineText);
  end;
  if Key = VK_F3 then
  begin
    ConsoleLine.Text := LastCommand;
    ConsoleLine.SelStart := Length(ConsoleLine.Text);
  end;
  if Key = VK_ESCAPE then
  begin
    ConsoleLine.Text := '';
    ConsoleLine.SelStart := Length(ConsoleLine.Text);
  end;
  if ((Shift = [ssCtrl]) and (Key = VK_I)) then
  begin
    {CTRL+I}
  end;
  if ((Shift = [ssCtrl]) and (Key = VK_K)) then
  begin
    {CTRL+K}
  end;
  if ((Shift = [ssCtrl]) and (Key = VK_O)) then
  begin
    {CTRL+O}
  end;
  if ((Shift = [ssCtrl]) and (Key = VK_L)) then
  begin
    {CTRL+L}
  end;
  if ((Shift = [ssCtrl, ssAlt]) and (Key = VK_D)) then
  begin
    {ctrl+alt+d}
  end;
end;

// Adjust data panel background colors
procedure TForm1.Grid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if ((ACol = 0) or (ACol = 2)) then
  begin
    (Sender as TStringGrid).Canvas.Brush.Color := cl3dlight;
    ts := (Sender as TStringGrid).Canvas.TextStyle;
    ts.Alignment := taCenter;
    (Sender as TStringGrid).Canvas.TextStyle := ts;
  end
  else
  begin
    ts := (Sender as TStringGrid).Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    (Sender as TStringGrid).Canvas.TextStyle := ts;
  end;
end;

// Color for addresses panel
procedure TForm1.Grid2PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
  posrequired: Int64;
begin
  posrequired := (GetSupply(MyLastBlock + 1) * PosStackCoins) div 10000;
  if (ACol = 1) then
  begin
    ts := (Sender as TStringGrid).Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    (Sender as TStringGrid).Canvas.TextStyle := ts;
    {
    if ((aRow>0) and (GetWallArrIndex(aRow-1).Balance>posrequired) and (GetWallArrIndex(aRow-1).Balance>(posrequired+(WO_PosWarning*140*10000000))) ) then
      begin
      (sender as TStringGrid).Canvas.Brush.Color :=  clmoneygreen;
      (sender as TStringGrid).Canvas.font.Color :=  clblack;
      end;
    if ((aRow>0) and (GetWallArrIndex(aRow-1).Balance>posrequired) and (GetWallArrIndex(aRow-1).Balance< (posrequired+(WO_PosWarning*140*10000000))) ) then
      begin
      (sender as TStringGrid).Canvas.Brush.Color :=  clYellow;
      (sender as TStringGrid).Canvas.font.Color :=  clblack;
      end
    }
  end;
  if ((ACol = 0) and (ARow > 0) and
    (AnsiContainsStr(GetCFGDataStr(5), GetWallArrIndex(aRow - 1).Hash))) then
  begin
    (Sender as TStringGrid).Canvas.Brush.Color := clRed;
    (Sender as TStringGrid).Canvas.font.Color := clblack;
  end;
end;

// Clear debug memo: Events
procedure TForm1.Button1Click(Sender: TObject);
begin
  MemoLog.Lines.Clear;
end;

// Clear debug memo: Exceptions
procedure TForm1.Button2Click(Sender: TObject);
begin
  MemoExceptLog.Lines.Clear;
end;

// Resize: data panel
procedure TForm1.DataPanelResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.DataPanel.Width;
  form1.DataPanel.ColWidths[0] := thispercent(20, GridWidth);
  form1.DataPanel.ColWidths[1] := thispercent(30, GridWidth);
  form1.DataPanel.ColWidths[2] := thispercent(20, GridWidth);
  form1.DataPanel.ColWidths[3] := thispercent(30, GridWidth);
end;

// Resize: GridAddresses
procedure TForm1.DireccionesPanelResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.DireccionesPanel.Width;
  form1.DireccionesPanel.ColWidths[0] := thispercent(68, GridWidth);
  form1.DireccionesPanel.ColWidths[1] := thispercent(32, GridWidth, True);
end;

// Resize: grid nodes
procedure TForm1.GridNodesResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.GridNodes.Width;
  form1.GridNodes.ColWidths[0] := thispercent(36, GridWidth);
  form1.GridNodes.ColWidths[1] := thispercent(64, GridWidth, True);
  form1.GridNodes.ColWidths[2] := thispercent(0, GridWidth);
  form1.GridNodes.ColWidths[3] := thispercent(0, GridWidth);
  form1.GridNodes.ColWidths[4] := thispercent(0, GridWidth, True);
end;

// Resize: consensus
procedure TForm1.SGConSeedsResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.SGConSeeds.Width;
  form1.SGConSeeds.ColWidths[0] := thispercent(20, GridWidth);
  form1.SGConSeeds.ColWidths[1] := thispercent(20, GridWidth);
  form1.SGConSeeds.ColWidths[2] := thispercent(40, GridWidth);
  form1.SGConSeeds.ColWidths[3] := thispercent(20, GridWidth, True);
end;

// Resize: Performance
procedure TForm1.SG_PerformanceResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.SG_Performance.Width;
  form1.SG_Performance.ColWidths[0] := thispercent(40, GridWidth);
  form1.SG_Performance.ColWidths[1] := thispercent(20, GridWidth);
  form1.SG_Performance.ColWidths[2] := thispercent(20, GridWidth);
  form1.SG_Performance.ColWidths[3] := thispercent(20, GridWidth, True);
end;

// Resize: Processes Threads
procedure TForm1.SG_OpenThreadsResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.SG_Performance.Width;
  form1.SG_OpenThreads.ColWidths[0] := thispercent(50, GridWidth);
  form1.SG_OpenThreads.ColWidths[1] := thispercent(30, GridWidth);
  form1.SG_OpenThreads.ColWidths[2] := thispercent(20, GridWidth, True);
end;

// Resize: Processes Files
procedure TForm1.PC_ProcessesResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.SG_Performance.Width;
  form1.SG_FilePRocs.ColWidths[0] := thispercent(25, GridWidth);
  form1.SG_FilePRocs.ColWidths[1] := thispercent(25, GridWidth);
  form1.SG_FilePRocs.ColWidths[2] := thispercent(25, GridWidth);
  form1.SG_FilePRocs.ColWidths[3] := thispercent(25, GridWidth, True);
end;

//Resize: About
procedure TForm1.Tab_Options_AboutResize(Sender: TObject);
begin
  ImageOptionsAbout.BorderSpacing.Left :=
    (Tab_Options_About.ClientWidth div 2) - (ImageOptionsAbout.Width div 2);
  BitBtnWeb.BorderSpacing.Left :=
    (Tab_Options_About.ClientWidth div 2) - (BitBtnWeb.Width div 2);
  BitBtnDonate.BorderSpacing.Left :=
    (Tab_Options_About.ClientWidth div 2) - (BitBtnDonate.Width div 2);
end;

// Resize: GVTs grid
procedure TForm1.GVTsGridResize(Sender: TObject);
var
  GridWidth: Integer;
begin
  GridWidth := form1.GVTsGrid.Width;
  form1.GVTsGrid.ColWidths[0] := thispercent(20, GridWidth);
  form1.GVTsGrid.ColWidths[1] := thispercent(80, GridWidth, True);
end;

{$ENDREGION}

{$REGION To Re-evaluate}

// App heartbeat
procedure TForm1.heartbeat(Sender: TObject);
begin
  UpdateOpenThread('Main', UTCTime);
  if EngineLastUpdate <> UTCtime then EngineLastUpdate := UTCtime;
  Form1.Latido.Enabled := False;
  if ((UTCTime >= BuildNMSBlock) and (BuildNMSBlock > 0) and
    (MyConStatus = 3) and (MyLastBlock = StrToIntDef(GetCOnsensus(2), -1))) then
  begin
    ToLog('events', 'Starting construction of block ' + (MyLastBlock + 1).ToString);
    BuildNewBlock(MyLastBlock + 1, BuildNMSBlock, MyLastBlockHash,
      {GetNMSData.Miner}'NpryectdevepmentfundsGE',{GetNMSData.Hash}'!!!!!!!!!100000000');
    G_MNVerifications := 0;
  end;
  BeginPerformance('ActualizarGUI');
  ActualizarGUI();
  EndPerformance('ActualizarGUI');
  BeginPerformance('SaveUpdatedFiles');
  SaveUpdatedFiles();
  EndPerformance('SaveUpdatedFiles');
  BeginPerformance('ProcesarLineas');
  ProcesarLineas();
  EndPerformance('ProcesarLineas');
  BeginPerformance('LeerLineasDeClientes');
  LeerLineasDeClientes();
  EndPerformance('LeerLineasDeClientes');
  BeginPerformance('ParseProtocolLines');
  ParseProtocolLines();
  EndPerformance('ParseProtocolLines');
  BeginPerformance('VerifyConnectionStatus');
  VerifyConnectionStatus();
  EndPerformance('VerifyConnectionStatus');
  if G_CloseRequested then CloseeAppSafely();
  if FormSlots.Visible then UpdateSlotsGrid();
  Inc(ConnectedRotor);
  if ConnectedRotor > 6 then ConnectedRotor := 0;
  UpdateStatusBar;
  if ((UTCTime mod 60 = 0) and (LastIPsClear <> UTCTime)) then ClearIPControls;
  if ((UTCTime mod 3600 = 3590) and (LastBotClear <> UTCTime) and (Form1.Server.Active))
  then DeleteBots;
  if ((UTCTime mod 600 >= 570) and (UTCTime > NosoT_LastUpdate + 599)) then
    UpdateOffset(PArameter(GetCFGDataStr, 2));
  Form1.Latido.Enabled := True;
end;

// Info label timer
procedure TForm1.InfoTimerEnd(Sender: TObject);
begin
  InfoPanelTime := InfoPanelTime - 50;
  if InfoPanelTime <= 0 then
  begin
    InfoPanelTime := 0;
    InfoPanel.Caption := '';
    InfoPanel.sendtoback;
  end;
end;

// Displays incoming/outgoing amounts
procedure TForm1.CheckForHint(Sender: TObject);
begin
  Processhint(Sender);
end;

// Disable default popup menu for a control
procedure TForm1.DisablePopUpMenu(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := True;
end;

// Updates status bar
procedure UpdateStatusBar();
begin
  if WO_StopGUI then exit;
  if Form1.Server.Active then Form1.StaSerImg.Visible := True
  else
    Form1.StaSerImg.Visible := False;
  Form1.StaConLab.Caption := IntToStr(GetTotalSyncedConnections);
  if MyConStatus = 0 then Form1.StaConLab.Color := clred;
  if MyConStatus = 1 then Form1.StaConLab.Color := clyellow;
  if MyConStatus = 2 then Form1.StaConLab.Color := claqua;
  if MyConStatus = 3 then Form1.StaConLab.Color := clgreen;
  Form1.BitBtnBlocks.Caption := IntToStr(MyLastBlock);
  form1.BitBtnPending.Caption := GetPendingCount.ToString;
  if form1.RPCServer.active then Form1.StaRPCimg.Visible := True
  else
    Form1.StaRPCimg.Visible := False;
  Form1.Imgs32.GetBitMap(ConnectedRotor, form1.ImgRotor.picture.BitMap);
end;

procedure TForm1.RestartTimerEjecutar(Sender: TObject);
begin
  if BlockAge < 590 then
  begin
    if BuildNMSBlock < UTCTime then
    begin
      BuildNMSBlock := NextBlockTimeStamp;
      ToLog('events', 'Next block time set to: ' + TimeStampToDate(BuildNMSBlock));
    end;
  end;
  RestartTimer.Enabled := False;
  if not WO_StopGUI then
    StaTimeLab.Caption := TimestampToDate(UTCTime);
  if G_CloseRequested then
  begin
    if not G_CloseRequested then
    begin
      RestartNosoAfterQuit := True;
    end;
    CloseeAppSafely;
  end
  else
    RestartTimer.Enabled := True;
end;

{$ENDREGION}

{$REGION CloseApp}

procedure CloseeAppSafely();
var
  counter: Integer;
  GoAhead: Boolean = False;
  EarlyRestart: Boolean;

  procedure CloseLine(texto: String);
  begin
    gridinicio.RowCount := gridinicio.RowCount + 1;
    gridinicio.Cells[0, gridinicio.RowCount - 1] := Texto;
    gridinicio.TopRow := gridinicio.RowCount;
    Application.ProcessMessages;
  end;

begin
  EnterCriticalSection(CSClosingApp);
  if not G_ClosingAPP then
  begin
    G_ClosingAPP := True;
    GoAhead := True;
  end;
  LeaveCriticalSection(CSClosingApp);
  if GoAhead then
  begin
    PerformanceToFile(PerformanceFilename);
    EarlyRestart := form1.Server.Active;
    Form1.Latido.Enabled := False; // Stopped the latido
    form1.RestartTimer.Enabled := False;
    forminicio.Caption := 'Closing';
    gridinicio.RowCount := 0;
    form1.Visible := False;
    forminicio.Visible := True;
    FormInicio.BorderIcons := FormInicio.BorderIcons - [bisystemmenu];
    CloseLine(rs0030);  //   Closing wallet
    CreateADV(False);   // save advopt
    sleep(100);
    CloseAllforms();
    CloseLine('Forms closed');
    sleep(100);
    CloseLine(CerrarClientes(False));
    sleep(100);
    if ((EarlyRestart) and (RestartNosoAfterQuit)) then RestartNoso;
    if form1.Server.Active then
    begin
      if StopServer then CloseLine('Node server stopped')
      else
        CloseLine('Error closing node server');
    end;
    sleep(100);
    if Assigned(ProcessLines) then ProcessLines.Free;
    CloseLine('Componnents freed');
    sleep(100);
    EnterCriticalSection(CSOutgoingMsjs);
    OutgoingMsjs.Clear;
    LeaveCriticalSection(CSOutgoingMsjs);
    try
      if Assigned(SendOutMsgsThread) then
      begin
        SendOutMsgsThread.Terminate;
        for counter := 1 to 10 do
        begin
          if ((Assigned(SendOutMsgsThread)) and
            (not SendOutMsgsThread.Terminated)) then
            sleep(1000)
          else
            break;
        end;
      end;
      if ((Assigned(SendOutMsgsThread)) and (not SendOutMsgsThread.Terminated)) then
        CloseLine('Out thread NOT CLOSED')
      else
        CloseLine('Out thread closed properly');
    except
      ON E: Exception do
        CloseLine('Error closing Out thread');
    end{Try};
    sleep(100);
    if Assigned(OutgoingMsjs) then OutgoingMsjs.Free;
    EnterCriticalSection(CSCriptoThread);
    SetLength(ArrayCriptoOp, 0);
    LeaveCriticalSection(CSCriptoThread);
    try
      if Assigned(CryptoThread) then
      begin
        CryptoThread.Terminate;
        for counter := 1 to 10 do
        begin
          if ((Assigned(CryptoThread)) and (not CryptoThread.Terminated)) then
            sleep(1000)
          else
            break;
        end;
      end;
      if ((Assigned(CryptoThread)) and (not CryptoThread.Terminated)) then
        CloseLine('Crypto thread NOT CLOSED')
      else
        CloseLine('Crypto thread closed properly');
    except
      ON E: Exception do
        CloseLine('Error closing crypto thread');
    end{Try};
    sleep(100);
    try
      if Assigned(UpdateLogsThread) then
      begin
        UpdateLogsThread.Terminate;
        for counter := 1 to 10 do
        begin
          if ((Assigned(UpdateLogsThread)) and (not UpdateLogsThread.Terminated)) then
            sleep(1000)
          else
            break;
        end;
      end;
      if ((Assigned(UpdateLogsThread)) and (not UpdateLogsThread.Terminated)) then
        CloseLine('Updatelogs thread NOT CLOSED')
      else
        CloseLine('Updatelogs thread closed properly');
    except
      ON E: Exception do
        CloseLine('Error closing Updatelogs thread');
    end{Try};
    sleep(100);
    try
      if Assigned(ThreadMNs) then
      begin
        ThreadMNs.Terminate;
        for counter := 1 to 10 do
        begin
          if ((Assigned(ThreadMNs)) and (not ThreadMNs.Terminated)) then sleep(1000)
          else
            break;
        end;
      end;
      if ((Assigned(ThreadMNs)) and (not ThreadMNs.Terminated)) then
        CloseLine('Nodes thread NOT CLOSED')
      else
        CloseLine('Nodes thread closed properly');
    except
      ON E: Exception do
        CloseLine('Error closing Nodes thread');
    end{Try};
    sleep(100);
    if ((not EarlyRestart) and (RestartNosoAfterQuit)) then RestartNoso;
    CreateEmptyFile(ClosedAppFilename);
    form1.Close;
  end;
end;

{$ENDREGION}

{$REGION RPC Server}

// A RPC REQUEST ENTERS
procedure TForm1.RPCServerExecute(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  PostString: String = '';
  //  StreamString: TStream ;
  StreamString: TStringStream;
begin
  if ((RPCFilter) and (not ValidRPCHost(ARequestInfo.RemoteIP))) then
  begin
    AResponseInfo.ContentText := GetJSONErrorCode(498, -1);
  end
  else if ARequestInfo.Command <> 'POST' then
  begin
    AResponseInfo.ContentText := GetJSONErrorCode(400, -1);
  end
  else if ARequestInfo.Command = 'POST' then
  begin  // Is a post request
    StreamString := TStringStream.Create('', TEncoding.UTF8);
    try
      StreamString.LoadFromStream(ARequestInfo.PostStream);
      if assigned(StreamString) then
      begin
        StreamString.Position := 0;
        PostString := ReadStringFromStream(StreamString, -1, IndyTextEncoding_UTF8);
      end;
    except
      ON E: Exception do
        ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
          ' -> ' + 'Error on Http server: ' + E.Message);
    end; {TRY}
    AResponseInfo.ContentText := ParseRPCJSON(PostString);
    StreamString.Free;
  end;
end;

{$ENDREGION}

 // *****************************
 // *** NODE SERVER FUNCTIONS ***
 // *****************************

{$REGION Node Server}

// returns the number of active connections
function TForm1.ClientsCount: Integer;
var
  Clients: TList;
begin
  Clients := server.Contexts.LockList;
  try
    Result := Clients.Count;
  except
    ON E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + 'Error getting server list count: ' + E.Message);
  end; {TRY}
  server.Contexts.UnlockList;
end;

// Try message to Node safely
function TForm1.TryMessageToNode(AContext: TIdContext; message: String): Boolean;
begin
  Result := True;
  try
    Acontext.Connection.IOHandler.WriteLn(message);
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;{Try}
end;

// Get stream from client
function TForm1.GetStreamFromContext(AContext: TIdContext;
  out LStream: TMemoryStream): Boolean;
begin
  Result := False;
  LStream.Clear;
  try
    AContext.Connection.IOHandler.ReadStream(LStream);
    Result := True;
  except
    on E: Exception do
      ToDeepDeb('NosoServer,GetStreamFromContext,' + E.Message);
  end;
end;

// Trys to close a server connection safely
procedure TForm1.TryCloseServerConnection(AContext: TIdContext; closemsg: String = '');
begin
  try
    if closemsg <> '' then
      Acontext.Connection.IOHandler.WriteLn(closemsg);
    AContext.Connection.Disconnect();
    Acontext.Connection.IOHandler.InputBuffer.Clear;
  except
    on E: Exception do
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + format(rs0042, [E.Message]));
  end; {TRY}
end;

// Node server gets a line
procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var
  LLine: String = '';
  IPUser: String = '';
  slot: Integer = 0;
  UpdateZipName: String = '';
  UpdateVersion: String = '';
  UpdateHash: String = '';
  UpdateClavePublica: String = '';
  UpdateFirma: String = '';
  MemStream: TMemoryStream;
  BlockZipName: String = '';
  GetFileOk: Boolean = False;
  GoAhead: Boolean;
  NextLines: array of String;
  LineToSend: String;
  LinesSent: Integer = 0;
  FTPTime, FTPSize, FTPSpeed: Int64;
begin
  GoAhead := True;
  IPUser := AContext.Connection.Socket.Binding.PeerIP;
  slot := GetSlotFromIP(IPUser);
  repeat
    LineToSend := GetTextToSlot(slot);
    if LineToSend <> '' then
    begin
      TryMessageToNode(AContext, LineToSend);
      Inc(LinesSent);
    end;
  until LineToSend = '';
  if LinesSent > 0 then exit;
  if slot = 0 then
  begin
    TryCloseServerConnection(AContext);
    exit;
  end;
  if ((MyConStatus < 3) and (not IsSeedNode(IPUser))) then
  begin
    TryCloseServerConnection(AContext, 'Closing NODE');
    exit;
  end;
  try
    LLine := AContext.Connection.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  except
    on E: Exception do
    begin
      TryCloseServerConnection(AContext);
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + format(rs0045, [IPUser, E.Message]));
      GoAhead := False;
    end;
  end{Try};
  if GoAhead then
  begin
    SetConexIndexBusy(Slot, True);
    if Parameter(LLine, 0) = 'RESUMENFILE' then
    begin
      MemStream := TMemoryStream.Create;
      DownloadHeaders := True;
      try
        AContext.Connection.IOHandler.ReadStream(MemStream);
        GetFileOk := True;
      except
        ON E: Exception do
        begin
          TryCloseServerConnection(AContext);
          GetFileOk := False;
        end;
      end; {TRY}
      if GetfileOk then
      begin
        if SaveStreamAsHeaders(MemStream) then
          ToLog('console', Format(rs0047, [copy(HashMD5File(ResumenFilename), 1, 5)]));
        //'Headers file received'
      end;
      UpdateMyData();
      LastTimeRequestResumen := 0;
      DownloadHeaders := False;
      MemStream.Free;
    end // END GET RESUMEN FILE
    else if LLine = 'BLOCKZIP' then
    begin
      BlockZipName := BlockDirectory + 'blocks.zip';
      TryDeleteFile(BlockZipName);
      MemStream := TMemoryStream.Create;
      DownLoadBlocks := True;
      try
        AContext.Connection.IOHandler.ReadStream(MemStream);
        MemStream.SaveToFile(BlockZipName);
        GetFileOk := True;
      except
        ON E: Exception do
        begin
          GetFileOk := False;
          TryCloseServerConnection(AContext);
        end;
      end; {TRY}
      if GetFileOk then
      begin
        if UnzipFile(BlockDirectory + 'blocks.zip', True) then
        begin
          MyLastBlock := GetMyLastUpdatedBlock();
          LastTimeRequestBlock := 0;
          ToLog('events', TimeToStr(now) + format(rs0021, [IntToStr(MyLastBlock)]));
          //'Blocks received up to '+IntToStr(MyLastBlock));
        end;
      end;
      MemStream.Free;
      DownLoadBlocks := False;
    end
    else if parameter(LLine, 4) = '$GETRESUMEN' then
    begin
      AddFileProcess('Send', 'Headers', IPUser, GetTickCount64);
      MemStream := TMemoryStream.Create;
      FTPSize := GetHeadersAsMemStream(MemStream);
      if FTPSize > 0 then
      begin
        try
          Acontext.Connection.IOHandler.WriteLn('RESUMENFILE');
          Acontext.connection.IOHandler.Write(MemStream, 0, True);
        except
          on E: Exception do
          begin
            Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
            ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
              ' -> ' + Format(rs0051, [E.Message]));
          end;
        end; {TRY}
      end;
      MemStream.Free;
      FTPTime := CloseFileProcess('Send', 'Headers', IPUser, GetTickCount64);
      FTPSpeed := (FTPSize div FTPTime);
      ToLog('nodeftp', 'Uploaded headers to ' + IPUser + ' at ' +
        FTPSpeed.ToString + ' kb/s');
    end
    else if parameter(LLine, 4) = '$GETSUMARY' then
    begin
      AddFileProcess('Send', 'Summary', IPUser, GetTickCount64);
      MemStream := TMemoryStream.Create;
      FTPSize := GetSummaryAsMemStream(MemStream);
      if FTPSize > 0 then
      begin
        try
          Acontext.Connection.IOHandler.WriteLn('SUMARYFILE');
          Acontext.connection.IOHandler.Write(MemStream, 0, True);
        except
          on E: Exception do
        end; {TRY}
      end;
      MemStream.Free;
      FTPTime := CloseFileProcess('Send', 'Summary', IPUser, GetTickCount64);
      FTPSpeed := (FTPSize div FTPTime);
      ToLog('nodeftp', 'Uploaded Summary to ' + IPUser + ' at ' +
        FTPSpeed.ToString + ' kb/s');
    end
    else if parameter(LLine, 4) = '$GETPSOS' then
    begin
      AddFileProcess('Send', 'PSOs', IPUser, GetTickCount64);
      MemStream := TMemoryStream.Create;
      FTPSize := GetPSOsAsMemStream(MemStream);
      if FTPSize > 0 then
      begin
        try
          Acontext.Connection.IOHandler.WriteLn('PSOSFILE');
          Acontext.connection.IOHandler.Write(MemStream, 0, True);
        except
          on E: Exception do
        end; {TRY}
      end;
      MemStream.Free;
      FTPTime := CloseFileProcess('Send', 'PSOs', IPUser, GetTickCount64);
      FTPSpeed := (FTPSize div FTPTime);
      ToLog('nodeftp', 'Uploaded PSOs to ' + IPUser + ' at ' +
        FTPSpeed.ToString + ' kb/s');
    end
    else if parameter(LLine, 4) = '$LASTBLOCK' then
    begin // START SENDING BLOCKS
      AddFileProcess('Send', 'Blocks', IPUser, GetTickCount64);
      BlockZipName := CreateZipBlockfile(StrToIntDef(parameter(LLine, 5), 0));
      if BlockZipName <> '' then
      begin
        MemStream := TMemoryStream.Create;
        try
          MemStream.LoadFromFile(BlockZipName);
          GetFileOk := True;
        except
          ON E: Exception do
          begin
            GetFileOk := False;
          end;
        end; {TRY}
        FTPSize := MemStream.Size;
        if GetFileOk then
        begin
          try
            Acontext.Connection.IOHandler.WriteLn('BLOCKZIP');
            Acontext.connection.IOHandler.Write(MemStream, 0, True);
            ToLog('events', TimeToStr(now) + Format(rs0052, [IPUser, BlockZipName]));
            //SERVER: BlockZip send to '+IPUser+':'+BlockZipName);
          except
            ON E: Exception do
            begin
              Form1.TryCloseServerConnection(GetConexIndex(Slot).context);
              //ToLog('exceps',FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now)+' -> '+Format(rs0053,[E.Message])); //'SERVER: Error sending ZIP blocks file ('+E.Message+')');
            end
          end; {TRY}
        end;
        MemStream.Free;
        FTPTime := CloseFileProcess('Send', 'Blocks', IPUser, GetTickCount64);
        FTPSpeed := (FTPSize div FTPTime);
        ToLog('nodeftp', 'Uploaded Blocks to ' + IPUser + ' at ' +
          FTPSpeed.ToString + ' kb/s');
        Trydeletefile(BlockZipName); // safe function to delete files
      end;
    end // END SENDING BLOCKS

    else if parameter(LLine, 4) = '$GETGVTS' then
    begin
      AddFileProcess('Send', 'GVTs', IPUser, GetTickCount64);
      MemStream := TMemoryStream.Create;
      FTPSize := GetGVTsAsStream(MemStream);
      if FTPSize > 0 then
      begin
        try
          Acontext.Connection.IOHandler.WriteLn('GVTSFILE');
          Acontext.connection.IOHandler.Write(MemStream, 0, True);
        except
          on E: Exception do
        end; {TRY}
      end;
      MemStream.Free;
      FTPTime := CloseFileProcess('Send', 'GVTs', IPUser, GetTickCount64);
      FTPSpeed := (FTPSize div FTPTime);
      ToLog('nodeftp', 'Uploaded GVTs to ' + IPUser + ' at ' +
        FTPSpeed.ToString + ' kb/s');
    end // SENDING GVTS FILE

    else if parameter(LLine, 0) = 'PSOSFILE' then
    begin
      DownloadPSOs := True;
      MemStream := TMemoryStream.Create;
      if GetStreamFromContext(Acontext, MemStream) then
      begin
        if SavePSOsToFile(MemStream) then
        begin
          LoadPSOFileFromDisk;
          UpdateMyData();
          ToLog('console', 'PSOs file received on server');
        end;
      end;
      MemStream.Free;
      DownloadPSOs := False;
      LasTimePSOsRequest := 0;
    end

    else if AnsiContainsStr(ValidProtocolCommands, Uppercase(parameter(LLine, 4))) then
    begin
      try
        AddToIncoming(slot, LLine);
      except
        On E: Exception do
          ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
            ' -> ' + Format(rs0054, [E.Message]));
        //ToLog('exceps',FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now)+' -> '+'SERVER: Server error adding received line ('+E.Message+')');
      end; {TRY}
    end
    else
    begin
      TryCloseServerConnection(AContext);
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + Format(rs0055, [LLine]));
      //ToLog('exceps',FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now)+' -> '+'SERVER: Got unexpected line: '+LLine);
    end;
    SetConexIndexBusy(Slot, False);
  end;
end;

// Un usuario intenta conectarse
procedure TForm1.IdTCPServer1Connect(AContext: TIdContext);
const
  LastBlocksRequest: Int64 = 0;
var
  IPUser: String;
  LLine: String;
  MiIp: String = '';
  Peerversion: String = '';
  GoAhead: Boolean;
  GetFileOk: Boolean = False;
  MemStream: TMemoryStream;
  ContextData: TServerTipo;
  ThisSlot: Integer;
  PeerUTC: Int64;
  BlockZipName: String = '';
  BlockZipsize: Int64;
begin
  GoAhead := True;
  ContextData := TServerTipo.Create;
  ContextData.Slot := 0;
  AContext.Data := ContextData;
  IPUser := AContext.Connection.Socket.Binding.PeerIP;
  if BotExists(IPUser) then
  begin
    TryCloseServerConnection(AContext, 'BANNED');
    exit;
  end;
  if AddIPControl(IPUser) > 99 then
  begin
    TryCloseServerConnection(AContext, '');
    UpdateBotData(IPUser);
    ToLog('console', 'IP spammer: ' + IPUser);
    exit;
  end;
  if ((MyConStatus < 3) and (not IsSeedNode(IPUser))) then
  begin
    TryCloseServerConnection(AContext, 'Closing NODE');
    exit;
  end;
  if KeepServerOn = False then // Reject any new connection if we are closing the server
  begin
    TryCloseServerConnection(AContext, 'Closing NODE');
    exit;
  end;
  LLine := '';
  try
    LLine := AContext.Connection.IOHandler.ReadLn('', 1000, -1, IndyTextEncoding_UTF8);
  except
    on E: Exception do
    begin
      TryCloseServerConnection(AContext);
      ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
        ' -> ' + format(rs0057, [E.Message]));
      GoAhead := False;
    end;
  end{Try};
  MiIp := Parameter(LLine, 1);
  Peerversion := Parameter(LLine, 2);
  PeerUTC := StrToInt64Def(Parameter(LLine, 3), 0);
  if GoAhead then
  begin
    if parameter(LLine, 0) = 'NODESTATUS' then
      TryCloseServerConnection(AContext, 'NODESTATUS ' + GetNodeStatusString)
    else if parameter(LLine, 0) = 'NSLORDER' then
      TryCloseServerConnection(AContext, PTC_Order(LLine))
    else if parameter(LLine, 0) = 'NSLCUSTOM' then
      TryCloseServerConnection(AContext, PTC_Custom(GetOpData(LLine)).ToString)
    else if parameter(LLine, 0) = 'NSLSENDGVT' then
      TryCloseServerConnection(AContext, PTC_SendGVT(LLine).ToString)
    else if parameter(LLine, 0) = 'GETMIIP' then
      TryCloseServerConnection(AContext, IPUser)
    else if parameter(LLine, 0) = 'MNVER' then
      TryCloseServerConnection(AContext, GetVerificationMNLine(IPUser))
    else if parameter(LLine, 0) = 'NSLBALANCE' then
      TryCloseServerConnection(AContext, IntToStr(
        GetAddressAvailable(parameter(LLine, 1))))
    else if parameter(LLine, 0) = 'NSLPEND' then
      TryCloseServerConnection(AContext, PendingRawInfo(False))
    else if parameter(LLine, 0) = 'NSLPENDFULL' then
      TryCloseServerConnection(AContext, PendingRawInfo)
    else if parameter(LLine, 0) = 'NSLBLKORD' then
      TryCloseServerConnection(AContext, GEtNSLBlkOrdInfo(LLine))
    else if parameter(LLine, 0) = 'NSLTIME' then
      TryCloseServerConnection(AContext, UTCTimeStr)
    else if parameter(LLine, 0) = 'NSLMNS' then
      TryCloseServerConnection(AContext, GetMN_FileText)
    else if parameter(LLine, 0) = 'NSLCFG' then
      TryCloseServerConnection(AContext, GetCFGDataStr)
    else if parameter(LLine, 0) = 'NSLGVT' then
    begin
      MemStream := TMemoryStream.Create;
      if GetGVTsAsStream(MemStream) > 0 then GetFileOk := True
      else
        GetFileOk := False;
      if GetFileOk then
      begin
        try
          Acontext.Connection.IOHandler.WriteLn('GVTFILE ' + Copy(MyGVTsHash, 0, 5));
          Acontext.connection.IOHandler.Write(MemStream, 0, True);
        except
          on E: Exception do
          begin
          end;
        end; {TRY}
      end;
      MemStream.Free;
      TryCloseServerConnection(AContext);
    end
    else if parameter(LLine, 0) = 'GETZIPSUMARY' then
    begin
      MemStream := TMemoryStream.Create;
      if GetZIPSummaryAsMemStream(MemStream) > 0 then GetFileOk := True
      else
        GetFileOk := False;
      if GetFileOk then
      begin
        try
          Acontext.Connection.IOHandler.WriteLn('ZIPSUMARY ' +
            Copy(MySumarioHash, 0, 5));
          Acontext.connection.IOHandler.Write(MemStream, 0, True);
        except
          on E: Exception do
          begin
          end;
        end; {TRY}
      end;
      MemStream.Free;
      TryCloseServerConnection(AContext);
    end
    else if Copy(LLine, 1, 4) <> 'PSK ' then  // invalid protocol
    begin
      ToLog('events', TimeToStr(now) + format(rs0058, [IPUser]));
      //ToLog('events',TimeToStr(now)+'SERVER: Invalid client->'+IPUser);
      TryCloseServerConnection(AContext, 'WRONG_PROTOCOL');
      UpdateBotData(IPUser);
    end

    else if IPUser = MyPublicIP then
    begin
      ToLog('events', TimeToStr(now) + rs0059);
      //ToLog('events',TimeToStr(now)+'SERVER: Own connected');
      TryCloseServerConnection(AContext);
    end

    else if ((Abs(UTCTime - PeerUTC) > 5) and (Mylastblock >= 70000)) then
    begin
      TryCloseServerConnection(AContext, 'WRONG_TIME');
    end
    else if GetSlotFromIP(IPUser) > 0 then
    begin
      ToLog('events', TimeToStr(now) + Format(rs0060, [IPUser]));
      //ToLog('events',TimeToStr(now)+'SERVER: Duplicated connection->'+IPUser);
      TryCloseServerConnection(AContext, GetPTCEcn + 'DUPLICATED');
      UpdateBotData(IPUser);
    end
    else if Copy(Peerversion, 1, 3) < Copy(VersionRequired, 1, 3) then
    begin
      TryCloseServerConnection(AContext, GetPTCEcn + 'OLDVERSION->REQUIRED_' +
        VersionRequired);
    end
    else if Copy(LLine, 1, 4) = 'PSK ' then
    begin    // Check for available slot
      ThisSlot := SaveConection('CLI', IPUser, Acontext);
      if ThisSlot = 0 then  // Server full
        TryCloseServerConnection(AContext)
      else
      begin
        ToLog('events', TimeToStr(now) + format(rs0061, [IPUser]));
        //New Connection from:
        ContextData.Slot := ThisSlot;
        AContext.Data := ContextData;
        if IsValidIP(MiIp) then MyPublicIP := MiIp;
        U_DataPanel := True;
        ClearOutTextToSlot(ThisSlot);
      end;
    end
    else
    begin
      ToLog('events', TimeToStr(now) + Format(rs0062, [IPUser]));
      //ToLog('events',TimeToStr(now)+'SERVER: Closed unhandled incoming connection->'+IPUser);
      TryCloseServerConnection(AContext);
    end;
  end;
end;

// Un cliente se desconecta del servidor
procedure TForm1.IdTCPServer1Disconnect(AContext: TIdContext);
var
  ContextData: TServerTipo;
begin
  ContextData := TServerTipo(AContext.Data);
  if ContextData.Slot > 0 then
    CloseSlot(ContextData.Slot);
end;

// Excepcion en el servidor
procedure TForm1.IdTCPServer1Exception(AContext: TIdContext; AException: Exception);
begin
  CloseSlot(GetSlotFromContext(AContext));
  ToLog('exceps', FormatDateTime('dd mm YYYY HH:MM:SS.zzz', Now) +
    ' -> ' + 'Server Excepcion: ' + AException.Message);    //Server Excepcion:
end;

{$ENDREGION Node Server}

{$REGION Addresses Stringgrid}

// Set selected address as default
procedure TForm1.BDefAddrOnClick(Sender: TObject);
begin
  if DireccionesPanel.Row > 0 then
    ProcessLinesAdd('SETDEFAULT ' + DireccionesPanel.Cells[0, DireccionesPanel.Row]);
end;

// Shows customization panel
procedure TForm1.BCustomAddrOnClick(Sender: TObject);
var
  Address: String;
begin
  Address := DireccionesPanel.Cells[0, DireccionesPanel.Row];
  if not IsValidHashAddress(address) then info('Address already customized')
  else if AddressAlreadyCustomized(address) then info('Address already customized')
  else if GetAddressBalanceIndexed(Address) - GetAddressPendingPays(address) <
    GetCustFee(MyLastBlock) then info('Insufficient funds')
  else
  begin
    DireccionesPanel.Enabled := False;
    PanelCustom.Visible := True;
    PanelCustom.BringToFront;
    EditCustom.SetFocus;
  end;
end;

// Get return press on customization panel
procedure Tform1.EditCustomKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ProcessLinesAdd('Customize ' + DireccionesPanel.Cells[0, DireccionesPanel.Row] +
      ' ' + EditCustom.Text);
    PanelCustom.Visible := False;
    EditCustom.Text := '';
  end;
end;

// Process customization
procedure TForm1.BOkCustomClick(Sender: TObject);
begin
  ProcessLinesAdd('Customize ' + DireccionesPanel.Cells[0, DireccionesPanel.Row] +
    ' ' + EditCustom.Text);
  PanelCustom.Visible := False;
  EditCustom.Text := '';
end;

// Close customization panel on mouse leave
procedure TForm1.PanelCustomMouseLeave(Sender: TObject);
begin
  PanelCustom.Visible := False;
  DireccionesPanel.Enabled := True;
end;

// New address button
procedure TForm1.BNewAddrOnClick(Sender: TObject);
begin
  ProcessLinesAdd('newaddress');
end;

// Copy address button
procedure TForm1.BCopyAddrClick(Sender: TObject);
begin
  Clipboard.AsText := DireccionesPanel.Cells[0, DireccionesPanel.Row];
  {
  if GetWallArrIndex(DireccionesPanel.Row-1).custom <> '' then
    Clipboard.AsText:= GetWallArrIndex(DireccionesPanel.Row-1).custom
  else Clipboard.AsText:= GetWallArrIndex(DireccionesPanel.Row-1).Hash;
  }
  info('Copied to clipboard');//'Copied to clipboard'
end;

// Grid Addresses DrawCell
procedure TForm1.DireccionesPanelDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Bitmap: TBitmap;
  myRect: TRect;
  ColWidth: Integer;
begin
  if ((aRow > 0) and (aCol = 0) and
    (AnsiContainsstr(GetMN_FileText, GetWallArrIndex(aRow - 1).Hash))) then
  begin
    ColWidth := (Sender as TStringGrid).ColWidths[0];
    Bitmap := TBitmap.Create;
    Imagenes.GetBitmap(68, Bitmap);
    myRect := Arect;
    myrect.Left := ColWidth - 20;
    myRect.Right := ColWidth - 4;
    myrect.top := myrect.Top + 2;
    myrect.Bottom := myrect.Top + 18;
    (Sender as TStringGrid).Canvas.StretchDraw(myRect, bitmap);
    Bitmap.Free;
  end;
end;

{$ENDREGION Addresses Stringgrid}

{$REGION sendfunds panel}

// Paste on target send funds address
procedure TForm1.SBSCPasteOnClick(Sender: TObject);
begin
  EditSCDest.SetFocus;
  EditSCDest.Text := Clipboard.AsText;
  EditSCDest.SelStart := length(EditSCDest.Text);
end;

// Paste maximum amount on edit
procedure TForm1.SBSCMaxOnClick(Sender: TObject);
begin
  if not WO_MultiSend then
  begin
    EditSCMont.Text := Int2curr(GetMaximunToSend(GetWalletBalance));
  end
  else
  begin
    EditSCMont.Text := Int2Curr(GetMaximunToSend(
      GetAddressBalanceIndexed(GetWallArrIndex(0).hash)));
  end;
end;

// Validate send funds target
procedure Tform1.EditSCDestChange(Sender: TObject);
begin
  if EditSCDest.Text = '' then ImgSCDest.Picture.Clear
  else
  begin
    EditSCDest.Text := StringReplace(EditSCDest.Text, ' ', '',
      [rfReplaceAll, rfIgnoreCase]);
    if ((IsValidHashAddress(EditSCDest.Text)) or
      (AliasAlreadyExists(EditSCDest.Text))) then
      Form1.imagenes.GetBitmap(17, ImgSCDest.Picture.Bitmap)
    else
      Form1.imagenes.GetBitmap(14, ImgSCDest.Picture.Bitmap);
  end;
end;

// On send funds amount edit
procedure TForm1.EditMontoOnKeyUp(Sender: TObject; var Key: Char);
var
  Permitido: String = '1234567890';
  Ultimo: Char;
  Actualmente: String;
  currpos: Integer;
  ParteEntera: String;
  ParteDecimal: String;
  PosicionEnElPunto: Integer;
begin
  if key = chr(27) then
  begin
    EditSCMont.Text := '0.00000000';
    EditSCMont.SelStart := 1;
    exit;
  end;
  ultimo := Char(key);
  if pos(ultimo, permitido) = 0 then exit;
  Actualmente := EditSCMont.Text;
  PosicionEnElPunto := Length(Actualmente) - 9;
  currpos := EditSCMont.SelStart;
  if EditSCMont.SelStart > length(EditSCMont.Text) - 9 then // Decimal
  begin
    Actualmente[currpos + 1] := ultimo;
    EditSCMont.Text := Actualmente;
    EditSCMont.SelStart := currpos + 1;
  end;
  if EditSCMont.SelStart <= length(EditSCMont.Text) - 9 then // Decimal
  begin
    ParteEntera := copy(actualmente, 1, length(Actualmente) - 9);
    ParteDecimal := copy(actualmente, length(Actualmente) - 7, 8);
    if currpos = PosicionEnElPunto then // Just before point
    begin
      if length(parteentera) > 7 then exit;
      ParteEntera := ParteEntera + Ultimo;
      ParteEntera := IntToStr(StrToIntDef(ParteEntera, 0));
      actualmente := parteentera + '.' + partedecimal;
      EditSCMont.Text := Actualmente;
      EditSCMont.SelStart := Length(Actualmente) - 9;
    end
    else
    begin
      Actualmente[currpos + 1] := ultimo;
      ParteEntera := copy(actualmente, 1, length(Actualmente) - 9);
      ParteEntera := IntToStr(StrToIntDef(ParteEntera, 0));
      actualmente := parteentera + '.' + partedecimal;
      EditSCMont.Text := Actualmente;
      EditSCMont.SelStart := currpos + 1;
      if ((currpos = 0) and (ultimo = '0')) then EditSCMont.SelStart := 0;
    end;
  end;
end;

// Validate send funds amount
procedure Tform1.EditSCMontChange(Sender: TObject);
begin
  if ((StrToInt64Def(StringReplace(EditSCMont.Text, '.', '',
    [rfReplaceAll, rfIgnoreCase]), -1) > 0) and
    (StrToInt64Def(StringReplace(EditSCMont.Text, '.', '', [rfReplaceAll, rfIgnoreCase]),
    -1) <= GetMaximunToSend(GetWalletBalance))) then
  begin
    Form1.imagenes.GetBitmap(17, ImgSCMont.Picture.Bitmap);
  end
  else
    Form1.imagenes.GetBitmap(14, ImgSCMont.Picture.Bitmap);
  if EditSCMont.Text = '0.00000000' then ImgSCMont.Picture.Clear;
end;

// Cancel sendfunds
procedure Tform1.SCBitCancelOnClick(Sender: TObject);
begin
  EditSCDest.Enabled := True;
  EditSCMont.Enabled := True;
  MemoSCCon.Enabled := True;
  SCBitSend.Visible := True;
  SCBitConf.Visible := False;
  SCBitCancel.Visible := False;
end;

// Accept send funds
procedure Tform1.SCBitSendOnClick(Sender: TObject);
begin
  if ((((AliasAlreadyExists(EditSCDest.Text)) or
    (IsValidHashAddress(EditSCDest.Text)))) and
    (StrToInt64Def(StringReplace(EditSCMont.Text, '.', '', [rfReplaceAll, rfIgnoreCase]),
    -1) > 0) and (StrToInt64Def(StringReplace(EditSCMont.Text, '.',
    '', [rfReplaceAll, rfIgnoreCase]), -1) <= GetMaximunToSend(GetWalletBalance))) then
  begin
    MemoSCCon.Text := Parameter(MemoSCCon.Text, 0);
    EditSCDest.Enabled := False;
    EditSCMont.Enabled := False;
    MemoSCCon.Enabled := False;
    SCBitSend.Visible := False;
    SCBitConf.Visible := True;
    SCBitCancel.Visible := True;
  end
  else
    info('Invalid parameters');
end;

// Process send funds
procedure Tform1.SCBitConfOnClick(Sender: TObject);
begin
  ProcessLinesAdd('SENDTO ' + EditSCDest.Text + ' ' +
    StringReplace(EditSCMont.Text, '.', '', [rfReplaceAll, rfIgnoreCase]) +
    ' ' + MemoSCCon.Text);
  ResetSendFundsPanel(Sender);
end;

// Clear send funds panel
procedure TForm1.ResetSendFundsPanel(Sender: TObject);
begin
  EditSCDest.Enabled := True;
  EditSCDest.Text := '';
  EditSCMont.Enabled := True;
  EditSCMont.Text := '0.00000000';
  MemoSCCon.Enabled := True;
  MemoSCCon.Text := '';
  SCBitSend.Visible := True;
  SCBitConf.Visible := False;
  SCBitCancel.Visible := False;
end;

{$ENDREGION sendfunds panel }

 //******************************************************************************
 // MAINMENU
 //******************************************************************************

{$REGION mainmenu}

// Main Menu: Import Wallet
procedure Tform1.MMImpWallet(Sender: TObject);
begin
  ShowExplorer(GetCurrentDir, 'Import Wallet', '*.pkw', 'impwallet (-resultado-)', True);
end;

// Main Menu: Export wallet
procedure Tform1.MMExpWallet(Sender: TObject);
begin
  ShowExplorer(GetCurrentDir, 'Export Wallet to', '*.pkw',
    'expwallet (-resultado-)', False);
end;

// menuprincipal restart
procedure Tform1.MMRestart(Sender: TObject);
begin
  ProcessLinesAdd('restart');
end;

// menuprincipal salir
procedure Tform1.MMQuit(Sender: TObject);
begin
  G_CloseRequested := True;
end;

{$ENDREGION mainmenu}

 //******************************************************************************
 // ConsolePopUp
 //******************************************************************************

{$REGION Console popup}

// Validate popup status
procedure TForm1.CheckConsolePopUp(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if MemoConsola.Text <> '' then ConsolePopUp2.Items[0].Enabled := True
  else
    ConsolePopUp2.Items[0].Enabled := False;
  if length(Memoconsola.SelText) > 0 then ConsolePopUp2.Items[1].Enabled := True
  else
    ConsolePopUp2.Items[1].Enabled := False;
end;

// Clear
procedure TForm1.ConsolePopUpClear(Sender: TObject);
begin
  ProcessLinesAdd('clear');
end;

// Copy
procedure TForm1.ConsolePopUpCopy(Sender: TObject);
begin
  Clipboard.AsText := Memoconsola.SelText;
  info('Copied to clipboard');
end;

{$ENDREGION Console popup}

 //******************************************************************************
 // CommandLine PopUp
 //******************************************************************************

{$REGION command line popup}

// Validate command line popup
procedure TForm1.CheckConsoLinePopUp(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if ConsoleLine.Text <> '' then ConsoLinePopUp2.Items[0].Enabled := True
  else
    ConsoLinePopUp2.Items[0].Enabled := False;
  if length(ConsoleLine.SelText) > 0 then ConsoLinePopUp2.Items[1].Enabled := True
  else
    ConsoLinePopUp2.Items[1].Enabled := False;
  if length(Clipboard.AsText) > 0 then ConsoLinePopUp2.Items[2].Enabled := True
  else
    ConsoLinePopUp2.Items[2].Enabled := False;
end;

// Clear
procedure TForm1.ConsoLinePopUpClear(Sender: TObject);
begin
  ConsoleLine.Text := '';
  ConsoleLine.SetFocus;
end;

// Copy
procedure TForm1.ConsoLinePopUpCopy(Sender: TObject);
begin
  Clipboard.AsText := ConsoleLine.SelText;
  info('Copied to clipboard');
end;

// Paste
procedure TForm1.ConsoLinePopUpPaste(Sender: TObject);
var
  CurrText: String;
  Currpos: Integer;
begin
  CurrText := ConsoleLine.Text;
  Currpos := ConsoleLine.SelStart;
  Insert(Clipboard.AsText, CurrText, ConsoleLine.SelStart + 1);
  ConsoleLine.Text := CurrText;
  ConsoleLine.SelStart := currpos + length(Clipboard.AsText);
  ConsoleLine.SetFocus;
end;

{$ENDREGION command line popup}

 //******************************************************************************
 // OPTIONS CONTROLS
 //******************************************************************************

{$REGION Options: wallet}

// Autoupdate option
procedure TForm1.CB_WO_AutoupdateChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CB_WO_Autoupdate.Checked then
    begin
      WO_AutoUpdate := True;
    end
    else
    begin
      WO_AutoUpdate := False;
    end;
    S_AdvOpt := True;
  end;
  if WO_AutoUpdate then
  begin
    {$IFDEF WINDOWS}
    if ( (not fileexists('libeay32.dll')) or (not fileexists('ssleay32.dll')) ) then
      ToLog('console','Warning: SSL files missed. Auto directive update will not work properly');
    {$ENDIF}
  end
  else
  begin
    ToLog('console',
      'Auto-update option is disabled. This could cause your node to become inactive on mandatory updates.');
  end;
end;

// Send from multiple addresses
procedure TForm1.CB_WO_MultisendChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CB_WO_Multisend.Checked then WO_Multisend := True
    else
      WO_Multisend := False;
    S_AdvOpt := True;
  end;
end;

// hide empty addresses
procedure TForm1.CB_WO_HideEmptyChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CB_WO_HideEmpty.Checked then WO_HideEmpty := True
    else
      WO_HideEmpty := False;
    S_AdvOpt := True;
    U_DirPanel := True;
  end;
end;

// Options Wallet: Keep blocks Database
procedure TForm1.CBKeepBlocksDBChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CBKeepBlocksDB.Checked then WO_BlockDB := True
    else
      WO_BlockDB := False;
  end;
end;

// Options Wallet: Stop GUI
procedure TForm1.CBRunNodeAloneChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CBRunNodeAlone.Checked then WO_StopGUI := True
    else
      WO_StopGUI := False;
  end;
end;

// Options Wallet: Send reports
procedure TForm1.CBSendReportsChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CBSendReports.Checked then WO_SendReport := True
    else
      WO_SendReport := False;
  end;
end;

{$ENDREGION Options: wallet}

{$REGION Options: Node}

// Load Masternode options when TAB is selected
procedure TForm1.TabNodeOptionsShow(Sender: TObject);
begin
  CBAutoIP.Checked := MN_AutoIP;
  CheckBox4.Checked := WO_AutoServer;
  LabeledEdit5.Text := LocalMN_IP;
  //LabeledEdit5.visible:=not MN_AutoIP;
  LabeledEdit6.Text := LocalMN_Port;
  LabeledEdit8.Text := LocalMN_Funds;
  LabeledEdit9.Text := LocalMN_Sign;
end;

// Save Node options
procedure TForm1.BSaveNodeOptionsClick(Sender: TObject);
begin
  WO_AutoServer := CheckBox4.Checked;
  LocalMN_IP := Trim(LabeledEdit5.Text);
  LocalMN_Port := Trim(LabeledEdit6.Text);
  LocalMN_Funds := Trim(LabeledEdit8.Text);
  LocalMN_Sign := Trim(LabeledEdit9.Text);
  MN_AutoIP := CBAutoIP.Checked;
  LastTimeReportMyMN := 0;
  S_AdvOpt := True;
  if not WO_AutoServer and form1.Server.Active then processlinesadd('serveroff');
  if WO_AutoServer and not form1.Server.Active then processlinesadd('serveron');
  info('Masternode options saved');
end;

// Test master node configuration
procedure TForm1.BTestNodeClick(Sender: TObject);
var
  Client: TidTCPClient;
  LineResult: String = '';
  ServerActivated: Boolean = False;
  IPToUse: String;
begin
  if WallAddIndex(LabeledEdit9.Text) < 0 then
  begin
    info(rs0081); // Invalid sign address
    exit;
  end;
  if GetAddressBalanceIndexed(LabeledEdit8.Text) < GetStackRequired(MylastBlock) then
  begin
    info(rs0082); // Funds address do not owns enough coins
    exit;
  end;
  if form1.Server.Active then
  begin
    info(rs0080);   //You can not test while server is active
    exit;
  end;
  if MyConStatus < 3 then
  begin
    info(rs0083);   //You need update the wallet
    exit;
  end;
  try
    form1.Server.Active := True;
    ServerActivated := True;
  except
    on E: Exception do
    begin
      info('Error activating server: ' + E.Message);
      exit;
    end;
  end;{Try}
  LineResult := '';
  Client := TidTCPClient.Create(nil);
  if CBAutoIP.Checked then IPToUse := GetMiIp()
  else
    IPToUse := trim(LabeledEdit5.Text);
  Client.Host := IPToUse;
  Client.Port := StrToIntDef(Trim(LabeledEdit6.Text), 8080);
  Client.ConnectTimeout := 1000;
  Client.ReadTimeout := 1000;
  try
    Client.Connect;
    Client.IOHandler.WriteLn('NODESTATUS');
    LineResult := Client.IOHandler.ReadLn(IndyTextEncoding_UTF8);
  except
    on E: Exception do
    begin
      info('Cant connect to ' + IPToUse);
      client.Free;
      if ServerActivated then form1.Server.Active := False;
      exit;
    end;
  end;{Try}
  if LineResult <> '' then info(IPToUse + ': OK')
  else
    info('Test Failed: ' + IPToUse);
  if client.Connected then Client.Disconnect();
  if ServerActivated then form1.Server.Active := False;
  client.Free;
end;

{$ENDREGION Options: Node}

{$REGION Options: RPC}

// Enable/Disable RPC
procedure TForm1.CB_AUTORPCChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    RPCAuto := CB_AUTORPC.Checked;
    S_AdvOpt := True;
  end;
end;

// Edit RPC port
procedure TForm1.LE_Rpc_PortEditingDone(Sender: TObject);
begin
  if StrToIntDef(LE_Rpc_Port.Text, -1) <> RPCPort then
  begin
    SetRPCPort('SETRPCPORT ' + LE_Rpc_Port.Text);
    LE_Rpc_Port.Text := IntToStr(RPCPort);
    S_AdvOpt := True;
    info('New RPC port set');
  end;
end;

// Edit RPC password
procedure TForm1.LE_Rpc_PassEditingDone(Sender: TObject);
begin
  if ((not G_Launching) and (LE_Rpc_Pass.Text <> RPCPass)) then
  begin
    setRPCpassword(LE_Rpc_Pass.Text);
    LE_Rpc_Pass.Text := RPCPass;
    S_AdvOpt := True;
    info('New RPC password set');
  end;
end;

// Enable RPC filter
procedure TForm1.CB_RPCFilterChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CB_RPCFilter.Checked then
    begin
      RPCFilter := True;
      MemoRPCWhitelist.Enabled := True;
    end
    else
    begin
      RPCFilter := False;
      MemoRPCWhitelist.Enabled := False;
    end;
    S_AdvOpt := True;
  end;
end;

// Backup RPC created addresses (to be deprecated, always enable)
procedure TForm1.CB_BACKRPCaddressesChange(Sender: TObject);
begin
  if not G_Launching then
  begin
    if CB_BACKRPCaddresses.Checked then RPCSaveNew := True
    else
      RPCSaveNew := False;
  end;
end;

// Set MN IP to Auto
procedure TForm1.CBAutoIPClick(Sender: TObject);
var
  MyIP: String;
begin
  if CBAutoIP.Checked then
  begin
    MyIP := GetMiIP();
    begin
      LabeledEdit5.Caption := MyIP;
      if MyIP <> LocalMN_IP then
      begin
        LocalMN_IP := MyIP;
        S_AdvOpt := True;
      end;
    end;
  end;
  //LabeledEdit5.Visible:=false
  //else LabeledEdit5.Visible:=true;
end;

// Editing RPC filter memo
procedure TForm1.MemoRPCWhitelistEditingDone(Sender: TObject);
var
  newlist: String;
begin
  if ((not G_Launching) and (MemoRPCWhitelist.Text <> RPCWhitelist)) then
  begin
    newlist := trim(MemoRPCWhitelist.Text);
    newlist := parameter(newlist, 0);
    MemoRPCWhitelist.Text := newlist;
    RPCWhitelist := newlist;
    S_AdvOpt := True;
  end;
end;

// Editing RPC banned memo
procedure TForm1.MemobannedmethodsEditingDone(Sender: TObject);
var
  newlist: String;
begin
  if ((not G_Launching) and (Memobannedmethods.Text <> RPCBanned)) then
  begin
    newlist := trim(Memobannedmethods.Text);
    newlist := parameter(newlist, 0);
    Memobannedmethods.Text := newlist;
    RPCBanned := newlist;
    S_AdvOpt := True;
  end;
end;

{$ENDREGION Options: RPC}

{$REGION Options: About}

// Button: Options -> About -> donate
procedure TForm1.BitBtnDonateClick(Sender: TObject);
begin
  form1.PageMain.ActivePage := form1.TabWallet;
  form1.TabWalletMain.ActivePage := form1.TabAddresses;
  PanelSend.Visible := True;
  Form1.EditSCDest.Text := 'NpryectdevepmentfundsGE';
  Form1.EditSCMont.Text := IntToStr(DefaultDonation) + '.00000000';
  Form1.MemoSCCon.Text := 'Donation';
end;

// Button: Options -> About -> web
procedure TForm1.BitBtnWebClick(Sender: TObject);
begin
  OpenDocument('https://nosocoin.com');
end;

{$ENDREGION Options: About}

end. // END PROGRAM
