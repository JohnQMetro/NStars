unit fluxtransform;

{$mode delphi}

interface

uses
  Classes, SysUtils, DAMath, df_strings, Utilities;
//******************************************************************************
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
(* Magnitude Splits and conversions *)
procedure SplitMagnitude(const sum_mag,mag_diff:Real; out mag_one,mag_two:Real);
procedure TrisectMagnitude(const sum_mag, diff1,diff2:Real; out mag_one,mag_two,mag_three:Real);
procedure FirstMagFromSumSecond(const sum_mag,mag_two:Real; out mag_one:Real);
function MakeEPD(const val1,val2:Real):Real;
(* Other methods *)
function JohnsonQIndex(Uj,Bj,Vj:Real):Real;
function UCAC_2MASS_ToBV(ucac4:Double; Ks:Currency; out Vest:Real; out Best:Currency):Boolean;
//******************************************************************************
implementation
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
var splitlist:TStringList;
    vc,verr,bc,berr:Real;
    gc,rcs,ics:Real;
    temp1,temp2,temp3,temp4,xgc,xrc,xic:Real;
    listx:Integer;
begin
  Result := False;
  // getting the list of values
  splitlist := SplitWithSpaces(indata,5);
  if splitlist = nil then Exit;
  if (splitlist.Count = 6) or (splitlist.Count = 5) then VBinc := False
  else if (splitlist.Count = 10) or (splitlist.Count = 9) then VBinc := True
  else begin
    FreeAndNil(splitlist);    Exit;
  end;
  // getting gri
  if VBInc then listx := 4
  else listx := 0;
  if not StrToRealBoth(splitlist[listx],splitlist[listx+2],gc,rcs) then begin
    FreeAndNil(splitlist);    Exit;
  end;
  if not StrToReal(splitlist[listx+4],ics) then begin
    FreeAndNil(splitlist);    Exit;
  end;
  // getting V and B
  if VBInc then begin
    // V
    if not StrToRealBoth(splitlist[0],splitlist[1],vc,verr) then begin
      FreeAndNil(splitlist);    Exit;
    end;
    // B
    if not StrToRealBoth(splitlist[2],splitlist[3],bc,berr) then begin
      FreeAndNil(splitlist);    Exit;
    end;
  end;
  FreeAndNil(splitlist);
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
begin
  g := gp + 0.060*( (gp-rp) - 0.53 );
  r := rp + 0.035*( (rp-ip) - 0.21 );
  i := ip + 0.041*( (rp-ip) - 0.21 );
  SDSS_gri2BVRI_c(g,r,i,Bj,Vj,Rc,Ic)
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
(* Estimates B and V from UCAC4 Model Fit Magnitude and 2 MASS Ks. For KM dwarfs
only, polynomials derived by myself. Kinda crude. *)
function UCAC_2MASS_ToBV(ucac4:Double; Ks:Currency; out Vest:Real; out Best:Currency):Boolean;
var umks:Double;
const bcoff1:array[0..3] of Real  = ( 5.02, -6.1338, 2.8479, -0.38889 );
      bcoff2:array[0..3] of Real  = ( -3.9581, 4.1783, -0.98665, 0.076675 );
      vcoff1:array[0..3] of Real = ( 3.5781, -4.6244, 2.0114, -0.27394 );
      vcoff2:array[0..2] of Real = ( -0.71231, 0.63993, -0.091608 );
begin
  Result := False;
  if (ucac4 > 90) or (Ks > 90) then Exit;
  umks := ucac4 - CurrToReal(Ks);
  if (umks < 1.56) or (umks > 5.882) then Exit;
  // using the polynomials
  if umks < 3.15 then begin
     Best := ucac4 + PolEval(umks,bcoff1,4);
     Vest := ucac4 + PolEval(umks,vcoff1,4);
  end else begin
    Best := ucac4 + PolEval(umks,bcoff2,4);
    Vest := ucac4 + PolEval(umks,vcoff2,3);
  end;
  Result := True;
end;

//******************************************************************************
end.

