unit Noso.IP.Control;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Noso.Time;

type
  IPControl = record
    IP: String;
    Count: Integer;
  end;

function AddIPControl(ThisIP: String): Integer;
procedure ClearIPControls();

var
  ArrCont: array of IPControl;
  CS_ArrCont: TRTLCriticalSection;
  LastIPsClear: Int64 = 0;

implementation

function AddIPControl(ThisIP: String): Integer;
var
  counter: Integer;
  Added: Boolean = False;
begin
  EnterCriticalSection(CS_ArrCont);
  for counter := 0 to length(ArrCont) - 1 do
  begin
    if ArrCont[Counter].IP = ThisIP then
    begin
      Inc(ArrCont[Counter].Count);
      Result := ArrCont[Counter].Count;
      Added := True;
      Break;
    end;
  end;
  if not added then
  begin
    Setlength(ArrCont, length(ArrCont) + 1);
    ArrCont[length(ArrCont) - 1].IP := thisIP;
    ArrCont[length(ArrCont) - 1].Count := 1;
    Result := 1;
  end;
  LeaveCriticalSection(CS_ArrCont);
end;

procedure ClearIPControls();
begin
  EnterCriticalSection(CS_ArrCont);
  Setlength(ArrCont, 0);
  LeaveCriticalSection(CS_ArrCont);
  LAstIPsClear := UTCTime;
end;

initialization
  SetLength(ArrCont, 0);
  InitCriticalSection(CS_ArrCont);

finalization
  DoneCriticalSection(CS_ArrCont);

end.
