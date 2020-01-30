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
// revised USNO B catalog I (and 2MASS J) to Ic
function ClassifyUSNO_I(UBI,J:Currency; out ij:Real):Word;
function USNO_IJ_Ic(UBI,J:Currency; out IcEst:Currency):Boolean;

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
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// revised USNO B catalog I (and 2MASS J) to Ic
function ClassifyUSNO_I(UBI,J:Currency; out ij:Real):Word;
begin
  Result := 0;
  if not MakeColorCheck(UBI,J,-1,5,ij) then Exit;
  if (ij > (0.388*UBI - 2.208)) then Result := 1
  else if (UBI > 11.6) and (ij < (0.294*UBI - 3.41)) then Result := 3
  else if (UBI > 15) and (ij <(1.364*UBI - 18.46)) then Result := 4
  else Result := 2;
end;
(* USNO B-Catalog I to Ic. We use Magnitude-Color to sort the stars
into 4 groups, which then have different equations. *)
function USNO_IJ_Ic(UBI,J:Currency; out IcEst:Currency):Boolean;
var bimj,interm:Real; classn:Word;
const poly1:array[0..2] of Real = ( 0.078344, 0.36977, -0.010863 );
      poly4:array[0..2] of Real = ( -0.096615, 1.5505, -0.1991 );
begin
  Result := False;
  IcEst := UBI;
  // classifying
  classn := ClassifyUSNO_I(UBI,J,bimj);
  if classn = 0 then Exit;
  // Class 1: sensible looking line (~1100 stars, std err ~0.0573 at 98%)
  if (classn = 1) then begin
    if (bimj < 0.667) or (bimj > 4.731) then Exit;
    interm := PolEval(bimj,poly1,3);
  end
  // Class 2: the ugly blob (~5000 stars, std err ~0.153 at 98%)
  else if (classn = 2) then begin
    if (bimj < -0.243) or (bimj > 3.249) then Exit;
    interm := 1.1673 + 0.214*bimj;
  end
  // Class 3: mostly white dwarfs, I think (~60 stars, std err ~0.161 at 95%)
  else if (classn = 3) then begin
    if (bimj < -0.912) or (bimj >0.957) then Exit;
    interm := 0.18482 + 0.47801*bimj;
  end
  // Class 4: the blob's thin red tail (~60 stars, std err ~0.172 at 95%)
  else begin
    if (bimj < 1.535) or (bimj > 4.363) then Exit;
    interm := PolEval(bimj,poly4,3);
  end;
  // if we get here, we deliver a result
  Result := True;
  IcEst := RealToCurr(interm) + J;
  IcEst := RoundCurrency(IcEst,False)
end;

//******************************************************************
end.

