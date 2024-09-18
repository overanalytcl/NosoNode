program Noso;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MPForm,
  MP.Utils.Signing,
  indylaz,
  SysUtils,
  LCLTranslator,
  Noso.Time,
  Noso.Debug,
  Noso.Crypto,
  Noso.Summary,
  Noso.Consensus,
  Noso.Pso,
  Noso.WallCon,
  Noso.Headers,
  Noso.Config,
  Noso.Block,
  Noso.Network,
  Noso.Gvt,
  Noso.Masternodes,
  Noso.IP.Control, Noso.Types;

  {$R *.res}

begin
  Application.Scaled := True;
  Application.Initialize;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
