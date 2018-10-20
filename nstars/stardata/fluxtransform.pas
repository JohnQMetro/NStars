unit fluxtransform;

{$mode delphi}

interface

uses
  Classes, SysUtils, DAMath, df_strings, Utilities;
//******************************************************************************
type RealArray = array of Real;


(* Helper methods for flux conversions *)
procedure LoadMulti(color1,color2:Real; fcubic:Boolean; var target:RealArray);
function MakeColorCheck(mag1,mag2:Currency; min,max:Real; out color:Real):Boolean;
(* Non-SDSS magnitude conversions *)
function TychoToJohnson(const Bt,Vt:Real; out Bj,Vj:Real):Boolean;
function TychoToFluxes(indata:string; out Vj:Real; out Bj:Currency):Boolean;
procedure Johnson_to_Cousins(inV:Real; inR,inI:Currency; out Rc,Ic:Currency);
function DENIStoCousinsI(const inDenisI:Currency):Currency;

(* SDSS related magnitude conversions *)
procedure SDSS_gri2BVRI(g,r,i:Real; out Bj,Vj,Rc,Ic:Real);
function SDSS_ugriz2BVRI(u,g,r,i,z:Real; out Bj,Vj,Rc,Ic:Real):Boolean;
procedure SDSS_gri2BVRI_c(g,r,i:Real; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency);
function SDSS_ugriz2BVRI_c(u,g,r,i,z:Real; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency):Boolean;
procedure SDSSpAB_to_SDSS(up,gp,rp,ip,zp:Real; out u,g,r,i,z:Real);
function APASS_to_Fluxes(indata:string; out VBinc:Boolean; out Vj,Vje,Vest:Real;
                                        out Bj,Bje,Best,Rc,Ic:Currency):Boolean;
function SDSS_to_Fluxes(indata:string; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency):Boolean;
procedure Pgri_to_BVRI(gp,rp,ip:Real; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency);
function PanSTARRSgri_to_BVRI(gp,rp,ip:Real; out Bj,Vj,Rc,Ic:Real):Boolean;
(* UCAC4 fits *)
function UCAC4_To_VRI(UCACin,J,G1,G2:Currency; out Vest:Real; out RcEst:Currency; out IcEst:Currency):Boolean;
function UCAC4_To_V(UCACin,J,G1,G2:Currency; out Vest:Real):Boolean;
function UCAC4_To_RcS(UCACin,J,G1,G2:Currency; out RcEst:Currency):Boolean;
function UCAC4_To_Ic(UCACin,G1,G2,J:Currency; out Icest:Currency):Boolean;
(* Magnitude Splits and conversions *)
procedure SplitMagnitude(const sum_mag,mag_diff:Real; out mag_one,mag_two:Real);
procedure TrisectMagnitude(const sum_mag, diff1,diff2:Real; out mag_one,mag_two,mag_three:Real);
procedure FirstMagFromSumSecond(const sum_mag,mag_two:Real; out mag_one:Real);
function MakeEPD(const val1,val2:Real):Real;
(* Other methods *)
function JohnsonQIndex(Uj,Bj,Vj:Real):Real;
function USNO_B2_Adjust(B2in,J,H,Ks:Currency; out Best:Currency):Boolean;
function USNO_R2_Adjust(R2in,J,H,Ks:Currency; out Rcest:Currency):Boolean;
function USNO_I_Adjust(Iin,J,H,Ks:Currency; out Icest:Currency):Boolean;
(* BV estimation methods *)
function URATG_ToBV(URATin:Real; Gin,J,H,Ks:Currency; out Best:Currency; out Vest:Real):Boolean;
function CMC15_ToBV(CMCr:Real; Gin,J,Ks:Currency; out Best:Currency; out Vest:Real):Boolean;
function URATJ_To_Ic(URATin,Jin:Currency; out Icest:Currency):Boolean;
(* New Pan-STARRS DR1 conversions *)
function PS1_StrToVals(const indata:string; var values:RealArray):Boolean;
function PS1_iyToJHK(const inValues:RealArray;const G:Currency; out Je:Currency; out He:Currency; out Ke:Currency):Boolean;


//******************************************************************************
implementation
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Helper procedure for multilinear fits *)
procedure LoadMulti(color1,color2:Real; fcubic:Boolean; var target:RealArray);
begin
  if fcubic then SetLength(target,7)
  else SetLength(target,6);
  target[0] := 1;
  target[1] := color1;
  target[2] := Sqr(color1);
  if not fcubic then begin
    target[3] := color1 * color2;
    target[4] := color2;
    target[5] := Sqr(color2);
  end else begin
    target[3] := intpower(color1,3);
    target[4] := color1 * color2;
    target[5] := color2;
    target[6] := Sqr(color2);
  end;
end;
//-----------------------------------------------------------
(* handles computing a colour and checking if it is within bounds. *)
function MakeColorCheck(mag1,mag2:Currency; min,max:Real; out color:Real):Boolean;
begin
  Result := False;
  if (mag1 > 90) or (mag2 > 90) then Exit;
  color := CurrToReal(mag1) - CurrToReal(mag2);
  if (color < min) or (color > max) then Exit;
  Result := True;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Non-SDSS magnitude conversions *)
//--------------------------------------------------------
(* Tries to convert Tycho Bt and Vt magnitudes to Johnson B and V,
using the method found on page 23 and page 24 of
http://iopscience.iop.org/article/10.1086/341952/pdf.
This is for the Bt-Vt range of -0.25 to 2 *)
function TychoToJohnson(const Bt,Vt:Real; out Bj,Vj:Real):Boolean;
var BtmVt,Vdiff:Real;
    BmVdiff,BmV:Real;
const vjcoff:array[0..3] of Real = (9.7E-4,-1.334E-1,5.486E-2,-1.998E-2);
      bcoff1:array[0..3] of Real = (0,-7.813E-3,-1.489E-1,3.384E-2);
      bcoff2:array[0..2] of Real = (-0.006,-1.069E-1,1.459E-1);
begin
  // computing Bt-Vt
  BtmVt := Bt - Vt;
  // out of range
  Result := False;
  if BtmVt < -0.25 then Exit;
  if BtmVt > 2 then Exit;
  // next, calculating Vj using the polynomial
  Vdiff := PolEval(BtmVt,vjcoff,4);
  Vj := Vt + Vdiff;
  // next, B-V as a polynomial, depends of Bt-Vt in more than one way
  if BtmVt <= 0.4 then BmVdiff := PolEval(BtmVt,bcoff2,3)
  else BmVdiff := PolEval(BtmVt,bcoff1,4);
  BmV := BtmVt + BmVdiff;
  // finally, B
  Bj := Vj + BmV;
  // done
  Result := True;
end;
//------------------------------------------------------------
function TychoToFluxes(indata:string; out Vj:Real; out Bj:Currency):Boolean;
var splitlist:TStringList;
    btin,vtin:Real;
    bj_real:Real;
    rok:Boolean;
begin
  Result := False;
  // getting the list of values
  splitlist := SplitWithSpaces(indata,1);
  if splitlist = nil then Exit;
  if splitlist.Count <> 2 then begin
    FreeAndNil(splitlist);   Exit;
  end;
  // converting to numbers
  if not StrToRealBoth(splitlist[0],splitlist[1],btin,vtin) then begin
    FreeAndNil(splitlist);   Exit;
  end;
  FreeAndNil(splitlist);
  // converting to B V
  rok := TychoToJohnson(btin,vtin,bj_real,Vj);
  if not rok then Exit;
  Result := True;
  Bj := CurrToReal(bj_real);
end;
//-------------------------------------------------------------
(* Some Simbad fluxes are from the 'Catalogue of Stellar Photometry in Johnson's
11-color system', and Johnson R and I is *not* the same as the more standard
Cousins Rc and Ic. To convert, I've decided that Taylor's (1986) equations are
too much of a pain. I get the equations from the ADPS. *)
procedure Johnson_to_Cousins(inV:Real; inR,inI:Currency; out Rc,Ic:Currency);
var rj_real,ij_real:Real;
    vmij,vmic:Real;
    vmrj,vmrc:Real;
    outrc_real,outic_real:Real;
begin
  // preparing values used in the equations
  rj_real := CurrToReal(inR);
  ij_real := CurrToReal(inI);
  vmij := inV - ij_real;
  vmrj := inV - rj_real;
  // calculating result V-Ic and V-Rc
  vmic := 0.779*vmij + 0.014;
  if vmrj < 1 then vmrc := 0.73*vmrj - 0.03
  else vmrc := 0.62*vmrj - 0.08;
  // finishing off
  outrc_real := inV - vmrc;
  outic_real := inV - vmic;
  Rc := RealToCurr(outrc_real);
  Ic := RealToCurr(outic_real);
end;
//----------------------------------------------------------
(* The magnitudes for I from DENIS are almost but not quite Ic.
   The relation is from Bilir+ 2012.  *)
function DENIStoCousinsI(const inDenisI:Currency):Currency;
var real_denis,real_cousins:Real;
begin
  real_denis := CurrToReal(inDenisI);
  real_cousins := 1.04*real_denis-0.501;
  Result := RealToCurr(real_cousins);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* SDSS related magnitude conversions *)
//-------------------------------------------------------------
// based on Lupton (2005) BVRI from gri only. Averaging two equations for Rc
procedure SDSS_gri2BVRI(g,r,i:Real; out Bj,Vj,Rc,Ic:Real);
var R1,R2:Real;
begin
  Bj := g + 0.3130*(g - r) + 0.2271;
  Vj := g - 0.5784*(g - r) - 0.0038;
  R1 := r - 0.1837*(g - r) - 0.0971;
  R2 := r - 0.2936*(r - i) - 0.1439;
  Rc := (R1+R2)/2;
  Ic := r - 1.2444*(r - i) - 0.3820;
end;
//---------------------------------------------------------
// based on Lupton (2005) BVRI from ugriz, averaging two different equations
function SDSS_ugriz2BVRI(u,g,r,i,z:Real; out Bj,Vj,Rc,Ic:Real):Boolean;
var B1,B2,V1,V2,I1,I2:Real;
begin
  Result := False;
  SDSS_gri2BVRI(g,r,i,B1,V1,Rc,I1);
  // making use of u (if available)
  if u < 90 then begin
    B2 := u - 0.8116*(u - g) + 0.1313;
    Bj := (B1+B2)/2;
    V2 := g - 0.2906*(u - g) + 0.0885;
    Vj := (V1+V2)/2;
    Result := True;
  end
  else begin
    Bj := B1;
    Vj := V1;
  end;
  // making use of z (if available)
  if z < 90 then begin
    I2 := i - 0.3780*(i - z)  -0.3974;
    Ic := (I1 + I2)/2;
  end
  else Ic := I1;
end;
//---------------------------------------------------------
procedure SDSS_gri2BVRI_c(g,r,i:Real; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency);
var breal,rreal,ireal:Real;
begin
  SDSS_gri2BVRI(g,r,i,breal,Vj,rreal,ireal);
  Bj := RealToCurr(breal);
  Rc := RealToCurr(rreal);
  Ic := RealToCurr(ireal);
end;
//---------------------------------------------------------
function SDSS_ugriz2BVRI_c(u,g,r,i,z:Real; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency):Boolean;
var breal,rreal,ireal:Real;
begin
  Result := SDSS_ugriz2BVRI(u,g,r,i,z,breal,Vj,rreal,ireal);
  Bj := RealToCurr(breal);
  Rc := RealToCurr(rreal);
  Ic := RealToCurr(ireal);
end;
//------------------------------------------------------------------
procedure SDSSpAB_to_SDSS(up,gp,rp,ip,zp:Real; out u,g,r,i,z:Real);
begin
  u := up + 0.04;
  g := gp + 0.060*( (gp-rp) - 0.53 );
  r := rp + 0.035*( (rp-ip) - 0.21 );
  i := ip + 0.041*( (rp-ip) - 0.21 );
  z := zp - 0.030*( (ip-zp) - 0.09) - 0.02;
end;
//----------------------------------------------------------
(* Takes [BV]gri magnitudes from the APASS catalog (cut and pasted from VizieR
as a single string), and converts them to numeric values *)
function APASS_to_Fluxes(indata:string; out VBinc:Boolean; out Vj,Vje,Vest:Real;
                                        out Bj,Bje,Best,Rc,Ic:Currency):Boolean;
var maglist:RealArray;
    mllen:Integer;
    vc,verr,bc,berr:Real;
    gc,rcs,ics:Real;
    listx:Integer;
begin
  Result := False;
  // getting the list of values
  if not SplitWithSpacesToReal(indata,5,maglist) then Exit;
  // checking the count
  mllen := Length(maglist);
  if (mllen = 7) or (mllen = 8) or (mllen > 10) then Exit;
  VBinc := (mllen >= 9);
  // getting gri
  if VBInc then listx := 4
  else listx := 0;
  gc := maglist[listx];
  rcs := maglist[listx+2];
  ics := maglist[listx+4];
  // getting V and B
  if VBInc then begin
    vc := maglist[0];    verr := maglist[1];
    bc := maglist[2];    berr := maglist[3];
  end;
  // now, we convert SDSS gri to estimates for V,B,Rc, and Ic
  Pgri_to_BVRI(gc,rcs,ics,Best,Vest,Rc,Ic);
  // also, adding non-converted V,B (if available)
  if VBInc then begin
    Vj := vc;
    Vje := verr;
    Bj := RealToCurr(bc);
    Bje := RealToCurr(berr);
  end;
  // done
  Result := True;
end;
//---------------------------------------------
function SDSS_to_Fluxes(indata:string; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency):Boolean;
var splitlist:TStringList;
    hasu,haserr:Boolean;
    slc,adexdex:Integer;
    adexes:array[1..4] of Integer;
    xuin,xgin,xrin,xiin,xzin:Real;
begin
  Result := False;
  // getting the list of values
  splitlist := SplitWithSpaces(indata,4);
  if splitlist = nil then Exit;
  slc := splitlist.Count;
  // 4 options from the number of items
  if (slc = 4) or (slc = 8) or (slc = 7) then begin
    hasu := False;
    haserr := (slc <> 4);
  end
  else if (slc = 5) or (slc = 10) or (slc = 9) then begin
    hasu := True;
    haserr := (slc <> 5)
  end
  else begin
    FreeAndNil(splitlist);   Exit;
  end;
  // calculating the list indexes for the values
  for adexdex := 1 to 4 do begin
    adexes[adexdex] := adexdex;
    if not hasu then Dec(adexes[adexdex]);
    if haserr then adexes[adexdex] := (adexes[adexdex])*2
  end;
  // getting the first value, u, if it is there
  if hasu then begin
    if not StrToReal(splitlist[0],xuin) then begin
      FreeAndNil(splitlist);   Exit;
    end;
  end
  else xuin := 99.999;
  // g and r
  if not StrToRealBoth(splitlist[adexes[1]],splitlist[adexes[2]],xgin,xrin) then begin
    FreeAndNil(splitlist);   Exit;
  end;
  // i and z
  if not StrToRealBoth(splitlist[adexes[3]],splitlist[adexes[4]],xiin,xzin) then begin
    FreeAndNil(splitlist);   Exit;
  end;
  FreeAndNil(splitlist);
  // now, we call the conversion function to get the B V Rc Ic results
  SDSS_ugriz2BVRI_c(xuin,xgin,xrin,xiin,xzin,Bj,Vj,Rc,Ic);
  Result := True;
end;
//-------------------------------------------------------------------------
// takes g' r' i' insput (Real) and produces B V Rc Ic output
procedure Pgri_to_BVRI(gp,rp,ip:Real; out Bj:Currency; out Vj:Real; out Rc,Ic:Currency);
var g,r,i:Real;
    gmik:Boolean;
    gmr,gmi,interm:Real;
    dottarg:RealArray;
const rcoff:array[0..2] of Real = ( 0.69278, 0.0011081, 0.07592 );
begin
  g := gp + 0.060*( (gp-rp) - 0.53 );
  r := rp + 0.035*( (rp-ip) - 0.21 );
  i := ip + 0.041*( (rp-ip) - 0.21 );
  SDSS_gri2BVRI_c(g,r,i,Bj,Vj,Rc,Ic);
  (* Estimated Rc is rather dim for M dwarfs, so I have derived an alternate fit... *)
  gmik := MakeColorCheck(gp,ip,1.741,4.418,gmi);
  if gmik and MakeColorCheck(gp,rp,1.082,1.631,gmr) then begin
    interm := PolEval(gmi,rcoff,3) + 0.47729*gmr;
    Rc := gp - interm;
  end;
end;
//-----------------------------------------------------------
(* From 'Transformation of Pan-STARRS1 gri to Stetson BVRI magnitudes. Photometry
of small bodies observations.' , Kostov and Bonev, 2017 *)
function PanSTARRSgri_to_BVRI(gp,rp,ip:Real; out Bj,Vj,Rc,Ic:Real):Boolean;
var gmr,rmi:Real;
    Vj1,Vj2,Rc1,Rc2,Ic1,Ic2:Real;
const bcoff:array[0..2] of Real  = ( 0.199, 0.540, 0.016);
      vcoff1:array[0..2] of Real = (-0.020,-0.498,-0.008);
      vcoff2:array[0..2] of Real = (-0.020, 0.502,-0.008);
      rcoff1:array[0..2] of Real = (-0.163,-0.086,-0.061);
      rcoff2:array[0..2] of Real = (-0.172,-0.221,-0.081);
      icoff1:array[0..2] of Real = (-0.387,-0.123,-0.034);
      icoff2:array[0..2] of Real = (-0.433,-0.040,-0.263);
begin
  Result := False;
  // initial checks
  gmr := gp - rp;
  rmi := rp - ip;
  if gmr < -0.5 then Exit;
  if gmr > 2.3 then Exit;
  if rmi < -0.4 then Exit;
  if rmi > 1.5 then Exit;
  // computing the results
  Bj := gp + PolEval(gmr,bcoff,3);
  Vj1 := gp + PolEval(gmr,vcoff1,3);
  Vj2 := rp + PolEval(gmr,vcoff2,3);
  Vj := (Vj1+Vj2)/2;
  Rc1 := rp + PolEval(gmr,rcoff1,3);
  Rc2 := rp + PolEval(rmi,rcoff2,3);
  Rc := (Rc1+Rc2)/2;
  Ic1 := ip + PolEval(gmr,icoff1,3);
  Ic2 := ip + PolEval(rmi,icoff2,3);
  Ic := (Ic1+Ic2)/2;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* UCAC4 fits *)

// UCAC4 to VRI combo, with optional G
function UCAC4_To_VRI(UCACin,J,G1,G2:Currency; out Vest:Real; out RcEst:Currency; out IcEst:Currency):Boolean;
var vres,rres,ires:Boolean;
begin
  Result := False;
  if (UCACin > 90) or (J > 90) or (UCACin < 10) then Exit;
  vres := UCAC4_To_V(UCACin,J,G1,G2,Vest);
  rres := UCAC4_To_RcS(UCACin,J,G1,G2,RcEst);
  ires := UCAC4_To_Ic(UCACin,G1,G2,J,IcEst);
  Result := vres or rres or ires;
end;

// UCAC4 to V, with optional G
function UCAC4_To_V(UCACin,J,G1,G2:Currency; out Vest:Real):Boolean;
var ucmj,gmj:Real;
    resval:Real;
begin
  Result := False;
  // reject...
  if (UCACin > 90) or (J > 90) or (UCACin < 10) then Exit;
  // computing source color
  ucmj := CurrToReal(UCACin) - CurrToReal(J);
  if (ucmj < 2.23) or (ucmj > 6.745) then Exit;
  // uses accurate Rc for 705 stars (SN 35,38)
  if MakeColorCheck(G2,J,1.937,3.687,gmj)  and (ucmj < 5.616) then begin
    resval := -0.54203+ 0.3778*ucmj + 1.1322*gmj;
  end else if MakeColorCheck(G1,J,1.833,3.22,gmj)  and (ucmj < 5.178) then begin
    resval := -0.56531 + 0.391*ucmj + 1.2063*gmj;
  end else resval := 0.52215 + 0.94567*ucmj;
  // finishing off
  Vest := J + resval;
  Result := True;
end;

(* Simbad sometimes treats UCAC fit model magnitude as equivalent to R, which it
is not. A rough conversion *)
function UCAC4_To_RcS(UCACin,J,G1,G2:Currency; out RcEst:Currency):Boolean;
var ucmj,gmj:Real;
    interm:Real;
const offr:array[0..2] of Real =  ( -0.12841, 0.91098, -0.027073 );
begin
  Result := False;
  // reject...
  if UCACin < 10 then Exit;
  if not MakeColorCheck(UCACin,J,2.3,5.561,ucmj) then Exit;
  // we now check whether we will be using the G1 fit
  if MakeColorCheck(G2,J,1.94,3.68,gmj) then begin
    interm := -0.74142 + 0.19988*ucmj + 1.0237*gmj;
  end else if MakeColorCheck(G1,J,1.83,3.22,gmj) and (ucmj < 5.178) then begin
    interm := -0.73692 + 0.23892*ucmj + 1.043*gmj;
  end else interm := PolEval(ucmj,offr,3);
  // finishing
  RcEst := J + interm;
  RcEst := RoundCurrency(RcEst,False);
  Result := True;
end;
//----------------------------------------------------------------
// UCAC4 to Ic, with optional G
function UCAC4_To_Ic(UCACin,G1,G2,J:Currency; out Icest:Currency):Boolean;
var ucmj,gmj:Real;
    interm:Real;
const coff1:array[0..2] of Real = ( 0.55654, 0.13847, 0.024528 );
begin
  Result := False;
  // reject...
  if UCACin < 10 then Exit;
  if not MakeColorCheck(UCACin,J,2.3,5.55,ucmj) then Exit;
  // we now check whether we will be using the G1 fit
  if MakeColorCheck(G2,J,1.94,3.68,gmj) then begin
    interm := 0.61674 - 0.10211*ucmj + 0.077549*ucmj*gmj + 0.12772*gmj;
  end else if MakeColorCheck(G1,J,1.83,3.22,gmj) and (ucmj < 5.178) then begin
    interm := 0.54509 - 0.11113*ucmj + 0.081946*ucmj*gmj + 0.18368*gmj
  end else interm := PolEval(ucmj,coff1,3);
  // finishing
  Icest := J + interm;
  Icest := RoundCurrency(Icest,False);
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Magnitude Splits and conversions *)
//-------------------------------------------------------------
procedure SplitMagnitude(const sum_mag,mag_diff:Real; out mag_one,mag_two:Real);
var temp,inside_logp:Real;
begin
  temp := -mag_diff/2.5;
  inside_logp := exp10(temp);
  temp := log10p1(inside_logp);
  mag_one := sum_mag + 2.5*temp;
  mag_two := mag_one + mag_diff;
end;
//-------------------------------------------------------------
procedure TrisectMagnitude(const sum_mag, diff1,diff2:Real; out mag_one,mag_two,mag_three:Real);
var temp1,temp2,inside_logp:Real;
begin
  temp1 := -diff1/2.5;
  temp2 := -diff2/2.5;
  inside_logp := exp10(temp1) + exp10(temp2);
  temp1 := log10p1(inside_logp);
  mag_one := sum_mag + 2.5*temp1;
  mag_two := mag_one + diff1;
  mag_three := mag_one + diff2;
end;
//--------------------------------------------------------------
procedure FirstMagFromSumSecond(const sum_mag,mag_two:Real; out mag_one:Real);
var temp1,temp2,inside_log:Real;
begin
  temp1 := -sum_mag/2.5;
  temp2 := -mag_two/2.5;
  inside_log := exp10(temp1) - exp10(temp2);
  temp1 := log10(inside_log);
  mag_one := (temp1 / -0.4);
end;
//-----------------------------------------------------------
// calculates 'Energy percent difference'
function MakeEPD(const val1,val2:Real):Real;
var interm1,interm2:Real;
begin
  Assert(val2>0);
  Assert(val1>0);
  interm1 := intpower(val1/val2,4);
  interm2 := Abs(1-interm1);
  Result := 100*interm2;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Other functions *)
//--------------------------------------------------------
function JohnsonQIndex(Uj,Bj,Vj:Real):Real;
begin
  Result := (Uj-Bj) - 0.72*(Bj-Vj);
end;

//--------------------------------------------------------
(* Trying to estimate a better value of B from USNO B Blue2 and JHKs. Still not
very accurate, but probably better than using Blue2 directly. Rvar 0.080992.
Please note that the redder the star, the worse the estimate gets. *)
function USNO_B2_Adjust(B2in,J,H,Ks:Currency; out Best:Currency):Boolean;
var bb2mks,hmks:Real;
    resval:Real;
    cbases:RealArray;
const coff:array[0..6] of Real = ( 2.4068,-0.69291,0.27012,-0.022159,1.5602,1.8474,-9.1413 );
begin
  Result := False;
  // reject...
  if (B2in > 90) or (J > 90) or (H > 90) or (Ks > 90) then Exit;
  // computing source colors
  bb2mks := CurrToReal(B2in) - CurrToReal(Ks);
  hmks := CurrToReal(H) - CurrToReal(Ks);
  if (bb2mks < -0.079) or (bb2mks > 10.062) then Exit;
  if (hmks < -0.024) or (hmks > 0.418) then Exit;
  // computing the result
  LoadMulti(bb2mks,hmks,True,cbases);
  resval := dot2(cbases,coff,7);
  // checking to see if the result is within expected bounds
  if (resval < 1.902) or (resval > 7.677) then Exit;
  // finishing off
  Best := J + resval;
  Best := RoundCurrency(Best,False);
  Result := True;
end;
//---------------------------------------------------------------------------------
(* Trying to estimate a better value of R from USNO B Red2 and JHKs. Still not
very accurate, but probably better than using Red2 directly. Rvar 0.0397
Please note that the redder the star, the worse the estimate gets. *)
function USNO_R2_Adjust(R2in,J,H,Ks:Currency; out Rcest:Currency):Boolean;
var br2mj,hmks:Real;
    resval:Real;
    cbases:RealArray;
const coff:array[0..6] of Real = ( 1.1125, -0.3534, 0.22185, -0.031435, 1.5609, 3.2721, -7.3793 );
begin
  Result := False;
  // reject...
  if (R2in > 90) or (J > 90) or (H > 90) or (Ks > 90) then Exit;
  // computing source colors
  br2mj := CurrToReal(R2in) - CurrToReal(J);
  hmks := CurrToReal(H) - CurrToReal(Ks);
  if (br2mj < -1.23) or (br2mj > 4.639) then Exit;
  if (hmks < -0.024) or (hmks > 0.572) then Exit;
  // computing the result
  LoadMulti(br2mj,hmks,True,cbases);
  resval := dot2(cbases,coff,7);
  // checking to see if the result is within expected bounds
  if (resval < 0.787) or (resval > 3.924) then Exit;
  // finishing off
  Rcest := J + resval;
  Rcest := RoundCurrency(Rcest,False);
  Result := True;
end;
//-------------------------------------------------------
(* Trying to estimate a better value of Ic from USNO B Infrared and JHKs. More
accurate than the blue equation (but still imprecise). Rvar 0.015962 *)
function USNO_I_Adjust(Iin,J,H,Ks:Currency; out Icest:Currency):Boolean;
var bimj,hmks:Real;
    resval:Real;
    colorray:array of Real;
const coffs:array[0..5] of Real = ( 0.44999, 0.072116, -0.022102, 0.28465, 2.5375, 1.1511 );
begin
  Result := False;
  // reject...
  if (Iin > 90) or (J > 90) or (H > 90) or (Ks > 90) then Exit;
  // computing source colors
  bimj := CurrToReal(Iin) - CurrToReal(J);
  hmks := CurrToReal(H) - CurrToReal(Ks);
  if (bimj < -2.875) or (bimj > 6.297) then Exit;
  if (hmks < -0.024) or (hmks > 0.418) then Exit;
  // computing the result
  LoadMulti(bimj,hmks,True,colorray);
  resval := dot2(coffs,colorray,7);
  // checking to see if the result is within expected bounds
  if (resval < 0.3) or (resval > 2.098) then Exit;
  // finishing off
  Icest := J + resval;
  Icest := RoundCurrency(Icest,False);
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* BV estimation methods *)
//--------------------------------------------------------
(* Estimates B and V for KM dwarfs using URAT fit model magnitude, as well as
H and Ks. If a valid GAIA G and J is provided, better results can be obtained. *)
function URATG_ToBV(URATin:Real; Gin,J,H,Ks:Currency; out Best:Currency; out Vest:Real):Boolean;
var urmh,urmks,urmj,gmks:Real; // colors
    interm:Real; // tempval
    useGV,useGB:Boolean;
    colorhash:RealArray;
const coffV:array[0..3] of Real = ( 1.6941, -2.5794, 1.4319, -0.19351 );
      coffVG:array[0..5] of Real = ( -0.57493, 0.80089, -0.44462, 1.2176, -0.48984, -0.72724 );
      coffB:array[0..3] of Real = ( 0.93501, -1.2878, 1.1813, -0.17572 );
      coffBG:array[0..5] of Real = ( -1.785, 2.1492, -0.64765, 1.3077, -0.33171, -0.82774 );
begin
  Result := False;
  // V check
  useGV := (Gin < 90);
  if useGV then begin
    useGV := MakeColorCheck(URATin,J,0.941,2.98,urmj);
    useGV := useGV and MakeColorCheck(Gin,Ks,1.57,4.25,gmks);
  end;
  if not useGV then begin
    if not MakeColorCheck(URATin,H,1.320,4.194,urmh) then Exit;
  end;
  // B check
  if Gin < 90 then begin
    useGB := MakeColorCheck(URATin,J,0.890,2.953,urmj);
    useGB := useGB and MakeColorCheck(Gin,Ks,1.57,4.38,gmks);
  end else useGB := False;
  if not useGB then begin
    if not MakeColorCheck(URATin,Ks,1.338,4.35,urmks) then Exit;
  end;
  // finally, computing...
  // B
  if useGB then begin
    LoadMulti(gmks,urmj,False,colorhash);
    interm := dot2(colorhash,coffBG,6);
  end else begin
    interm := PolEval(urmks,coffB,4);
  end;
  Best := URATin + interm;
  Best := RoundCurrency(Best,False);
  // V
  if useGV then begin
    LoadMulti(gmks,urmj,False,colorhash);
    interm := dot2(colorhash,coffVG,6);
  end else begin
    interm := PolEval(urmh,coffV,4);
  end;
  Vest := URATin + interm;
  // done
  Result := True;
end;
//------------------------------------------------------------------------------
(* converts the r' found in the Carlsberg Meridian Catalogue 15 (which may be the
same as SDSS or Sloan Standard r') to BV, also using J. For better accuracy, you
can also use GAIA G and Ks as well. *)
function CMC15_ToBV(CMCr:Real; Gin,J,Ks:Currency; out Best:Currency; out Vest:Real):Boolean;
var rmj,rmks,gmks,gmj:Real; // colors
    interm:Real; // tempval
    useGV,useGB:Boolean;
    colorhash:RealArray;
const coffV:array[0..2] of Real = ( -0.23459, 0.48687, -0.069716 );
      coffVG:array[0..6] of Real = ( -1.1852, 2.0771, -0.73998, 0.054223, 0.31114, -0.36091, -0.093404 );
      coffB:array[0..2] of Real = ( -0.5763, 1.6096, -0.23162 );
      coffBG:array[0..6] of Real = ( -2.5055, 2.713, -0.78864, 0.046977, 0.3849, 0.20047, -0.20487 );
begin
  Result := False;
  // required mangitudes
  if (CMCr > 90) or (J > 90) then Exit;
  // initial check to see if we have G and Ks
  useGV := (Gin < 90);
  useGB := useGV and (Ks < 90);
  // calculating initial color
  rmj := CMCr - CurrToReal(J);
  // oob rejects
  if (rmj < 0.915) or (rmj > 6.374) then Exit;
  // testing if we use G-J for V as well
  if useGV then begin
    gmj := CurrToReal(Gin) - CurrToReal(J);
    useGV := (gmj >= 1.092) and (gmj<=8.279);
    useGV := useGV and (rmj >=1.088) and (rmj<=4.412);
  end;
  // testing if we use G-Ks for B instead
  if useGB then begin
    rmks := CMCr - CurrToReal(Ks);
    gmks := CurrToReal(Gin) - CurrToReal(Ks);
    useGB := (gmks >= 1.451) and (gmks<=9.109);
    useGV := useGV and (rmks >=1.479) and (rmks<=5.741);
  end;
  // finally, computing...
  // B
  if useGB then begin
    LoadMulti(gmks,rmks,True,colorhash);
    interm := dot2(colorhash,coffBG,7);
  end else begin
    interm := PolEval(rmj,coffB,3);
  end;
  Best := CMCr + interm;
  Best := RoundCurrency(Best,False);
  // V
  if useGV then begin
    LoadMulti(gmj,rmj,True,colorhash);
    interm := dot2(colorhash,coffVG,7);
  end else begin
    interm := PolEval(rmj,coffV,3);
  end;
  Vest := CMCr + interm;
  // done
  Result := True;
end;
//------------------------------------------------------------------------------
(* 'Ic' in this case is the IC obtained by running APASS g' r' i' thru transforms.
It seems to be okay enough. *)
function URATJ_To_Ic(URATin,Jin:Currency; out Icest:Currency):Boolean;
var umj,interm:Real;
const coff:array[0..2] of Real = ( -0.24767, 0.83674, -0.06675 );
begin
  Result := False;
  if (URATin > 90) or (Jin > 90) then Exit;
  umj := CurrToReal(URATin) - CurrToReal(Jin);
  if (umj < 0.798) or (umj > 3.743) then Exit;
  // we compute
  interm := PolEval(umj,coff,3);
  Icest := (CurrToReal(Jin) + interm);
  Icest := RoundCurrency(Icest,False);
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* New Pan-STARRS DR1 conversions *)
//--------------------------------------------
(* parse by space and convert. rp ip zp yp, no errors *)
function PS1_StrToVals(const indata:string; var values:RealArray):Boolean;
var tout:RealArray;
begin
  Result := False;
  if not SplitWithSpacesToReal(indata,4,tout) then Exit;
  if Length(tout) > 4 then Exit;
  SetLength(values,4);
  if tout[0] < 13.5 then values[0] := 99.999
  else values[0] := tout[0];
  if tout[1] < 13.5 then values[1] := 99.999
  else values[1] := tout[1];
  if tout[2] < 13 then values[2] := 99.999
  else values[2] := tout[2];
  if tout[3] < 12 then values[3] := 99.999
  else values[3] := tout[3];
  Result := (values[1] < 90) and (values[3] < 90);
end;

//------------------------------------------------
function PS1_iyToJHK(const inValues:RealArray;const G:Currency; out Je:Currency; out He:Currency; out Ke:Currency):Boolean;
var ipmyp,ipmzp,rpmyp,gmyp:Real;
    interm:Real;
begin
  Result := False;
  if (Length(inValues) <> 4) then Exit;
  // ip - yp
  if not MakeColorCheck(inValues[1],inValues[3],0.69,2.565,ipmyp) then Exit;

  // calculating for J, two sections...
  if ipmyp <= 2.25 then begin
    // also ip-zp
    if MakeColorCheck(inValues[1],inValues[2],-0.893,1.471,ipmzp) and (ipmyp >= 0.701) then begin
      interm := 0.94401 + 1.3092*ipmyp + 0.0047025*ipmzp;
    end
    // also rp-yp
    else if MakeColorCheck(inValues[0],inValues[3],0.993,5.817,rpmyp) then begin
      interm := 1.1095 + 0.9685*ipmyp + 0.036432*ipmyp*rpmyp + 0.039262*rpmyp;
    end
    // just ip-yp
    else interm := 0.97026 + 1.2898*ipmyp;
  end else begin
    // also ip-zp
    if MakeColorCheck(inValues[1],inValues[2],-1.964,1.635,ipmzp) and (ipmyp < 2.561) then begin
      interm := -31.286 + 15.739*ipmyp - 9.0167*ipmyp*ipmzp + 20.125*ipmzp;
    end
    // also G-yp
    else if MakeColorCheck(G,inValues[3],1.512,3.008,gmyp) then begin
      interm := -1.403 + 1.752*ipmyp + 0.5048*gmyp;
    end
    // also rp-yp
    else if MakeColorCheck(inValues[0],inValues[3],2.075,5.272,rpmyp) then begin
      interm := 5.9527 - 1.0602*ipmyp + 0.66212*ipmyp*rpmyp - 1.4203*rpmyp;
    end
    // just ip-yp
    else interm := -0.70379 + 2.058*ipmyp;
  end;
  Je := RealToCurr(inValues[1] - interm);
  Je := RoundCurrency(Je,False);

  // calculating for H, two sections...
  if ipmyp <= 2.25 then begin
    // also ip-zp
    if MakeColorCheck(inValues[1],inValues[2],-1.964,1.471,ipmzp) and (ipmyp >= 0.701) then begin
      interm := 1.5385 + 1.3717*ipmyp + 0.094597*ipmyp*ipmzp - 0.24823*ipmzp;
    end
    // also rp-yp
    else if MakeColorCheck(inValues[0],inValues[3],1.512,5.817,rpmyp) then begin
      interm := 1.6608 + 0.97937*ipmyp + 0.05518*ipmyp*rpmyp + 0.014919*rpmyp;
    end
    // just ip-yp
    else interm := 1.4278 + 1.3755*ipmyp;
  end else begin
    // also ip-zp
    if MakeColorCheck(inValues[1],inValues[2],1.313,1.575,ipmzp) and (ipmyp < 2.561) then begin
      interm := 1.0505 + 4.3641*ipmyp - 4.461*ipmzp;
    end
    // also G-yp
    else if MakeColorCheck(G,inValues[3],1.512,3.008,gmyp) then begin
      interm := -0.84658 + 1.591*ipmyp + 0.12603*ipmyp*gmyp + 0.40745*gmyp;
    end
    // also rp-yp
    else if MakeColorCheck(inValues[0],inValues[3],2.075,5.272,rpmyp) then begin
      interm := 14.834 - 4.4221*ipmyp + 1.4555*ipmyp*rpmyp - 3.3379*rpmyp;
    end
    // just ip-yp
    else interm := -83.179 + 70.851*ipmyp - 14.204*ipmyp*ipmyp;
  end;
  He := RealToCurr(inValues[1] - interm);
  He := RoundCurrency(He,False);

  // calculating for K, two sections...
  if ipmyp < 2.27 then begin
    // also ip-zp
    if MakeColorCheck(inValues[1],inValues[2],-0.893,1.484,ipmzp) and (ipmyp >= 0.701) then begin
      interm := 1.5458 + 1.5619*ipmyp - 0.056613*ipmzp;
    end
    // also rp-yp
    else if MakeColorCheck(inValues[0],inValues[3],1.512,5.817,rpmyp) then begin
      interm := 11.843 + 0.97154*ipmyp + 0.06361*ipmyp*rpmyp + 0.046494*rpmyp;
    end
    // just ip-yp
    else interm := 1.5914 + 1.4855*ipmyp;
  end else begin
    // also ip-zp
    if MakeColorCheck(inValues[1],inValues[2],1.315,1.635,ipmzp) and (ipmyp < 2.561) then begin
      interm := 26.685 + 5.0587*ipmyp - 39.525*ipmyp*ipmzp + 11.436*ipmzp;
    end
    // also G-yp
    else if MakeColorCheck(G,inValues[3],1.512,3.008,gmyp) then begin
      interm := -0.84658 + 1.591*ipmyp + 0.12603*ipmyp*gmyp + 0.40745*gmyp;
    end
    // also rp-yp
    else if MakeColorCheck(inValues[0],inValues[3],2.075,5.272,rpmyp) then begin
      interm := 10.259 - 2.5448*ipmyp + 1.1479*ipmyp*rpmyp - 2.4865*rpmyp;
    end
    // just ip-yp
    else interm := -41.376 + 36.85*ipmyp - 7.2135*ipmyp*ipmyp;
  end;
  Ke := RealToCurr(inValues[1] - interm);
  Ke := RoundCurrency(Ke,False);

  Result := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

end.

