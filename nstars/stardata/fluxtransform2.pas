unit fluxtransform2;

{$mode delphi}

interface

uses
  Classes, SysUtils, DAMath, fluxtransform, Utilities;
//******************************************************************
(* revised fits for UCAC4, URAT1, and CMC 15 *)

// I only find myself using UCAC4 if G is unreliable, so a simpler function...
function UCAC_to_VRI(UCAC4,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
// CMC 15 to VRI
function CMC15_to_VRI(CMC,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;

//******************************************************************
implementation
//==================================================================
// I only find myself using UCAC4 if G is unreliable, so a simpler function...
function UCAC_to_VRI(UCAC4,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
var ucacmj,vmj,rmj,imj,rj:Real;
const rcoff:array[0..2] of Real = ( -0.12126, 0.90737, -0.026637 );
      icoff:array[0..2] of Real = ( 0.53983, 0.14898, 0.022933 );
begin
  Result := False;
  if not MakeColorCheck(UCAC4,J,2.228,5.558,ucacmj) then Exit;
  vmj := 0.52007 + 0.94656*ucacmj;
  rmj := PolEval(ucacmj,rcoff,3);
  imj := PolEval(ucacmj,icoff,3);
  Result := True;
  rj := CurrToReal(J);
  Vest := vmj + rj;
  RcEst := RealToCurr(rmj + rj);
  RcEst := RoundCurrency(RcEst,False);
  IcEst := RealToCurr(imj + rj);
  IcEst := RoundCurrency(IcEst,False);
end;
//----------------------------------------------------
function CMC15_to_VRI(CMC,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
var cmcmj,vmj,rmj,imj,rj:Real;
const rcoff:array[0..3] of Real = ( 0.73806, 0.2203, 0.18383, -0.019145 );
      icoff:array[0..2] of Real = ( 0.52485, 0.16874, 0.026694 );
begin
  Result := False;
  if not MakeColorCheck(CMC,J,2.088,5.294,cmcmj) then Exit;
  vmj := 0.46352 + 1.0449*cmcmj;
  rmj := PolEval(cmcmj,rcoff,4);
  imj := PolEval(cmcmj,icoff,3);
  Result := True;
  rj := CurrToReal(J);
  Vest := vmj + rj;
  RcEst := RealToCurr(rmj + rj);
  RcEst := RoundCurrency(RcEst,False);
  IcEst := RealToCurr(imj + rj);
  IcEst := RoundCurrency(IcEst,False);
end;
//******************************************************************
end.

