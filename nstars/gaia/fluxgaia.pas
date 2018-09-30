unit fluxgaia;

{$mode delphi}

(* The number of flux transforms involving Gaia DR2 is increasing, so I think
I will split them off into a separate unit. *)
//*******************************************************************************
interface

uses
  Classes, SysUtils, DAMATH, Utilities, fluxtransform, gaiadr2base;

(* Misc Utilities *)
function CheckErr(const mval:Currency; const verr:Currency):byte;
function GaiaTransHelper(const gmags:GaiaDR2Mags; const Jmag:Currency):Integer;
(* Published transforms *)
function Gaia2ToVRI(Gmag:Currency; BPmRP:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function Gaia2To2MASS(Gmag:Currency; BPmRP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2ToVI_Jao(BPmag,RPmag:Currency; out Vest:Real; out Icest:Currency):Boolean;
(* My own transforms to VRI *)
function Gaia2ToVRI_MyWay(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_Red(Gmag,RPmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_Blue(Gmag,BPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_2M(Gmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToV(dr2mags:GaiaDR2Mags; Jmag:Currency; out Vest:Real):Boolean;
(* My own transform to B *)
function Gaia2ToB(Gmag,BPmag,RPmag:Currency; out Best:Currency):Boolean;
(* My own tranforms to J H K *)
function GaiaTo_JHK_Wr(dr2mags:GaiaDR2Mags; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_MyWay(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_BadG(BP,RP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_BadB(Gin,RP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_NoRP(Gin,BP:Currency; out Jest,Hest,Ksest:Currency):Boolean;

implementation
//*******************************************************************************
(* Misc Utilities *)
// accuracy est: 0 precise, 1 off, 2 awful, 3 unavailable
function CheckErr(const mval:Currency; const verr:Currency):byte;
begin
  if (mval > 90) then Result := 3
  else if (verr < 0.015) then Result := 0
  else if (verr < 0.030) then Result := 1
  else Result := 2;
end;
//-------------------------------------------------------------------
function GaiaTransHelper(const gmags:GaiaDR2Mags; const Jmag:Currency):Integer;
var bedre:Real;
begin
  Result := 0;
  // red and J
  if (Jmag < 90) then begin
    if (gmags.BP >= 90) then begin
      if (gmags.RP >= 90) then Result := 3
      else Result := 1
    end
    else begin
      bedre := gmags.BPerr / gmags.RPerr;
      if bedre > 5 then Result := 1
    end;
    if (Result <> 0) then Exit;
  end;
  // blue
  if (gmags.RP >= 90) then Result := 2
  else begin
    bedre := gmags.BPerr / gmags.RPerr;
    if (bedre < 0.5) and (gmags.RPerr > 0.01) then Result := 2
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* From '﻿Gaia Data Release 2: Photometric content and validation' (Evans+ 2018)
Does not work for M3.5 or redder. *)
function Gaia2ToVRI(Gmag:Currency; BPmRP:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var bmr,interm:Real;
const coffV:array[0..2] of Real = (-0.0176  ,-0.00686,-0.1732 );
      coffR:array[0..2] of Real = (-0.003226, 0.3833 ,-0.1345 );
      coffI:array[0..2] of Real = ( 0.02085 , 0.7419 ,-0.09631);
begin
  Result := False;
  if (BPmRP >= 2.75) or (BPmRP <= -0.5) then Exit;
  if Gmag > 90 then Exit;
  // converting
  bmr := CurrToReal(BPmRP);
  interm := PolEval(bmr,coffV,3);
  Vest := CurrToReal(Gmag) - interm;
  interm := PolEval(bmr,coffR,3);
  Rcest := CurrToReal(Gmag) - interm;
  Rcest := RoundCurrency(Rcest,False);
  interm := PolEval(bmr,coffI,3);
  Icest := CurrToReal(Gmag) - interm;
  Icest := RoundCurrency(Icest,False);
  Result := True;
end;
//-------------------------------------------------------------------------
(* This could be useful for binary components. For some reason, it also goes
much more into the red than the VRI transformations:
NOTE: however, it seems to give bad results for red dwarfs. avoid. *)
function Gaia2To2MASS(Gmag:Currency; BPmRP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var bmr,gr,interm:Real;
const coffKs:array[0..2] of Real = ( -0.1885 , 2.092 ,-0.1345 );
      coffH:array[0..2] of Real  = ( -0.1621 , 1.628 ,-0.1328 );
      coffJ:array[0..2] of Real  = ( -0.01883, 1.394 ,-0.07893);
begin
  Result := False;
  if (BPmRP >= 5.5) or (BPmRP <= -0.5) then Exit;
  if Gmag > 90 then Exit;
  // converting
  bmr := CurrToReal(BPmRP);
  gr := CurrToReal(Gmag);
  // J
  interm := PolEval(bmr,coffJ,3);
  Jest := gr - interm;
  Jest := RoundCurrency(Jest,False);
  // H
  if (BPmRP < 5) and (BpmRP > 0.25) then begin
    interm := PolEval(bmr,coffH,3);
    Hest := gr - interm;
    Hest := RoundCurrency(Hest,False);
  end
  else Hest := 99.999;
  // Ks
  if BpmRP > 0.25 then begin
    interm := PolEval(bmr,coffKs,3);
    Ksest := gr - interm;
    Ksest := RoundCurrency(Ksest,False);
  end
  else Ksest := 99.999;
  // done
  Result := True;
end;
//------------------------------------------------------------------------------
(* BP and RP to V and I from 'A Gap in the Lower Main Sequence Revealed by Gaia
Data Realease 2' (Jao+ 2018). *)
function Gaia2ToVI_Jao(BPmag,RPmag:Currency; out Vest:Real; out Icest:Currency):Boolean;
var bpmrp,intm1:Real;
const coffv:array[0..4] of Real = (0.75319,-1.41105,1.00316,-0.27106,0.02489);
begin
  Result := False;
  if (BPmag > 90) or (RPmag > 90) then Exit;
  bpmrp := CurrToReal(BPmag) - CurrToReal(RPmag);
  if (bpmrp < 1) or (bpmrp > 4) then Exit;
  // color in range, proceeding...
  Vest := CurrToReal(BPmag) - (0.2022 + 0.02489*bpmrp);
  intm1 := PolEval(bpmrp,coffv,5);
  Icest := RPmag - CurrToReal(intm1);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* DR2 to V Rc Ic, with my own fits. Uses photometry from SN 35 as the target,
so covers Red Dwarfs, and goes to much redder BP-RP.  *)
function Gaia2ToVRI_MyWay(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var bpmrp,gmrp:Real;
    interm:Real;
    colorx:RealArray;
const coffv:array[0..5] of Real = ( -0.12021, 0.88909, 0.0062748, 0.059802, -0.99844, -0.060393 );
      coffr:array[0..5] of Real = ( -1.0864, 0.62724, -0.020821, 0.033311, 0.096642, -0.42465 );
      coffi:array[0..5] of Real = ( -0.81587, 0.49131, 0.020283, -0.47688, 1.6593, 0.22504 );
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(BPmag,RPmag,1.771,5.197,bpmrp) then Exit;
  if not MakeColorCheck(Gmag,RPmag,0.89,1.68,gmrp) then Exit;
  // computing the vector
  LoadMulti(bpmrp,gmrp,False,colorx);
  // V
  interm := dot2(coffv,colorx,6);
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  interm := dot2(coffr,colorx,6);
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  interm := dot2(coffi,colorx,6);
  Icest := Gmag - RealToCurr(interm);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;

//-----------------------------------------------------------------
(* It is stated that BP is inaccurate for very red stars. SN35 only goes to L2,
but here is a function that avoids BP, and uses the 274 stars from SN35
with the highest G-RP (> 1.25) and has V Rc Ic. Requires J. *)
function GaiaToVRI_Red(Gmag,RPmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var gmrp,gmj:Real;
    interm:Real;
    colorx:RealArray;
const coffv:array[0..5] of Real = ( 9.9991, -12.668, -9.8856, 11.96, -1.2334, -2.0537 );
      coffr:array[0..5] of Real = ( 3.1156, -4.8787, -5.1848, 5.5582, -0.24206, -0.98861 );
      coffi:array[0..5] of Real = ( -1.5454, 2.7611, -0.25001, -0.28776, 0.42253, -0.025289 );
begin
  Result := False;
  if (Gmag > 90) or (Jmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(Gmag,RPmag,1.251,1.68,gmrp) then Exit;
  if not MakeColorCheck(Gmag,Jmag,2.874,4.777,gmj) then Exit;
  // computing the vector
  LoadMulti(gmrp,gmj,False,colorx);
  // V
  interm := dot2(coffv,colorx,6);
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  interm := dot2(coffr,colorx,6);
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  interm := dot2(coffi,colorx,6);
  Icest := Gmag - RealToCurr(interm);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;
//--------------------------------------------------------------------------
(* Using G and BP only. This gives the best results for V, but unlike RP, other
mags are not needed to get reasonable results. *)
function GaiaToVRI_Blue(Gmag,BPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var bpmg:Real;
    interm:Real;
const coffv:array[0..2] of Real = ( -0.2084, 0.92954, 0.022026 );
      coffr:array[0..2] of Real = ( -0.69696, 0.55157, -0.026898 );
      coffi:array[0..3] of Real = ( 0.005271, 1.6144, -0.53945, 0.05862 );
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(BPmag,Gmag,0.867,3.61,bpmg) then Exit;
  // V
  interm := PolEval(bpmg,coffv,3);
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  interm := PolEval(bpmg,coffr,3);
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  interm := PolEval(bpmg,coffi,4);
  Icest := Gmag - RealToCurr(interm);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;
//-----------------------------------------------------------------
(* Sometimes there are single stars with G, but no BP or RP. Estimating using
G-J, which works better than using 2MASS alone. *)
function GaiaToVRI_2M(Gmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var gmj:Real;
    interm:Real;
const coffv1:array[0..2] of Real = ( 0.084193, -0.084446, 0.17371 );
      coffv2:array[0..3] of Real = ( 35.34, -30.245, 8.6621, -0.78355 );
      coffr1:array[0..2] of Real = ( -1.1743, 0.49163, -0.018049 );
      coffr2:array[0..3] of Real =  ( 11.947, -10.706, 3.1073, -0.28321 );
      coffi1:array[0..2] of Real = ( -0.89387, 1.2845, -0.16599 );
      coffi2:array[0..2] of Real = ( -0.97998, 1.3248, -0.17 );
begin
  Result := False;
  if (Gmag > 90) or (Jmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(Gmag,Jmag,2.016,4.777,gmj) then Exit;
  // V
  if gmj < 3.1 then interm := PolEval(gmj,coffv1,3)
  else interm := PolEval(gmj,coffv2,4);
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  if gmj < 3.1 then interm := PolEval(gmj,coffr1,3)
  else interm := PolEval(gmj,coffr2,4);
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  if gmj < 3.1 then interm := PolEval(gmj,coffi1,3)
  else interm := PolEval(gmj,coffi2,3);
  Icest := Gmag - RealToCurr(interm);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;
//-------------------------------------------------------------
(* Uses all of the different methods, but only for computing V *)
function GaiaToV(dr2mags:GaiaDR2Mags; Jmag:Currency; out Vest:Real):Boolean;
var okay:Boolean;
    cpick:Integer;
    Gin,BPmRP, BPin,RPin:Currency;
    colorx:RealArray;
    gmrp,gmj,bpmg,bmr,interm:Real;
const coffv_r:array[0..5] of Real = ( 9.9991, -12.668, -9.8856, 11.96, -1.2334, -2.0537 );
      coffv_b:array[0..2] of Real = ( -0.2084, 0.92954, 0.022026 );
      coffvj1:array[0..2] of Real = ( 0.084193, -0.084446, 0.17371 );
      coffvj2:array[0..3] of Real = ( 35.34, -30.245, 8.6621, -0.78355 );
      coffvx:array[0..5] of Real = ( -0.12021, 0.88909, 0.0062748, 0.059802, -0.99844, -0.060393 );
      coffVy:array[0..2] of Real = (-0.0176 ,-0.00686,-0.1732 );
begin
  Result := False;
  // basic bad exit cases...
  if dr2mags = nil then Exit;
  if dr2mags.G >= 90 then Exit;
  // we have different methods depending on values...
  cpick := GaiaTransHelper(dr2mags,Jmag);
  BPin := dr2mags.BP;
  RPin := dr2mags.RP;
  BPmRP := dr2mags.BPminRP;
  Gin := dr2mags.G;
  okay := False;

  // red and j
  if cpick = 1 then begin
    if MakeColorCheck(Gin,RPin,1.251,1.68,gmrp) then begin
      if MakeColorCheck(Gin,Jmag,2.874,4.777,gmj) then begin
        LoadMulti(gmrp,gmj,False,colorx);
        interm := dot2(coffv_r,colorx,6);
        Vest := CurrToReal(Gin) + interm;
        okay := True;
      end;
    end;
  // blue
  end else if cpick = 2 then begin
    if MakeColorCheck(BPin,Gin,0.867,3.61,bpmg) then begin
      interm := PolEval(bpmg,coffv_b,3);
      Vest := CurrToReal(Gin) + interm;
      okay := True;
    end;
  // g and j
  end else if cpick = 3 then begin
    if MakeColorCheck(Gin,Jmag,2.016,4.777,gmj) then begin
      if gmj < 3.1 then interm := PolEval(gmj,coffvj1,3)
      else interm := PolEval(gmj,coffvj2,4);
      Vest := CurrToReal(Gin) + interm;
      okay := True;
    end;
  end;
  // using G and BP-RP
  if not okay then begin
    if (Gin > 90) or (BPin > 90) or (RPin > 90) then Exit;
    // the 'my way' BP-RP and G-RP
    if (BPmRP > 2) then begin
      if MakeColorCheck(BPin,RPin,1.771,5.197,bmr) then begin
        if MakeColorCheck(Gin,RPin,0.89,1.68,gmrp) then begin
          LoadMulti(bmr,gmrp,False,colorx);
          interm := dot2(coffvx,colorx,6);
          Vest := CurrToReal(Gin) + interm;
          okay := True;
        end;
      end;
    // the version provided my Gaia: use for less red stars
    end else begin
      if (BPmRP >= 2.75) or (BPmRP <= -0.5) then Exit;
      // converting
      bmr := CurrToReal(BPmRP);
      interm := PolEval(bmr,coffVy,3);
      Vest := CurrToReal(Gin) - interm;
      okay := True;
    end;
  end;
  Result := okay;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Rough transform to B for BP-RP > 1 using APASS B. Not very good. *)
function Gaia2ToB(Gmag,BPmag,RPmag:Currency; out Best:Currency):Boolean;
var bpmrp:Real;
    interm:Real;
const coffb:array[0..2] of Real = ( -0.42609, 1.6263, -0.16102 );
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) or (RPmag > 90) then Exit;
  if not MakeColorCheck(BPmag,RPmag,0.99,3.83,bpmrp) then Exit;
  // computing the results
  interm := PolEval(bpmrp,coffb,3);
  Best := Gmag + RealToCurr(interm);
  Best := RoundCurrency(Best,False);
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* General get JHK from Gaia mags. Picks one of the JHKs functions below based
on the G,BP,RP availability and error size *)
function GaiaTo_JHK_Wr(dr2mags:GaiaDR2Mags; out Jest,Hest,Ksest:Currency):Boolean;
var gc,bc,rc:byte;
    gtest:Boolean;
begin
  Result := False;
  if (dr2mags = nil) then Exit;
  gc := CheckErr(dr2mags.G,dr2mags.Gerr);
  bc := CheckErr(dr2mags.BP,dr2mags.BPerr);
  rc := CheckErr(dr2mags.RP,dr2mags.RPerr);
  // we chose functions for J H Ks using gc, bc, and rc
  // in some cases, do not event try
  if (bc > 1) and (rc > 1) then Exit;
  // G bad, but BP and RP are good
  gtest := (gc > 0) and (bc = 0) and (rc = 0);
  gtest := gtest or ((gc = 3) and (bc < 2) and (rc < 2));
  if gtest then begin
    Result := Gaia2To2MASS_BadG(dr2mags.BP,dr2mags.RP,Jest,Hest,Ksest);
    Exit;
  end;
  // G and RP is good, but BP is not good
  gtest := (bc > 0) and (rc < 2) and (gc < 2);
  gtest := gtest and (((dr2mags.BPerr / dr2mags.RPerr) > 4) and (gc < 2));
  if gtest then begin
    Result := Gaia2To2MASS_BadB(dr2mags.G,dr2mags.RP,Jest,Hest,Ksest);
    if (Result) then Exit;
  end;
  // default 'myway'
  Result := Gaia2To2MASS_MyWay(dr2mags.G,dr2mags.BP, dr2mags.RP,Jest,Hest,Ksest);
  if Result then Exit;
  Result := Gaia2To2MASS_NoRP(dr2mags.G,dr2mags.BP,Jest,Hest,Ksest);
  if Result then Exit;
  // dr2 provided
  Result := Gaia2To2MASS(dr2mags.G,dr2mags.BPminRP,Jest,Hest,Ksest);
end;

//-------------------------------------------------------------------------
(* Since the Gaia provided transforms for JHKs seem to work badly, I've done my
own, for BP-RP > 0.8 only. *)
function Gaia2To2MASS_MyWay(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var bpmrp,gmrp:Real;
    interm:Real;
    colorx:RealArray;
(*
const coffj:array[0..5] of Real = ( -0.025999, 0.17069, -0.062699, 0.49271, 2.0394, -0.80351 );
      coffh:array[0..4] of Real = ( -1.3427, 4.4554, -1.8405, 0.39909, -0.031809 );
      coffk:array[0..4] of Real = ( -1.3698, 4.5444, -1.801, 0.38128, -0.029769 ); *)
const coffj1:array[0..2] of Real = ( -0.33535, 0.56945, -0.049115);
      coffj2:array[0..3] of Real = ( 0, 2.5949, -1.6955, 0.77989 );
      coffhx:array[0..4] of Real = ( -1.1191, 4.1699, -2.1679, 0.43246, -0.033342);
      coffk:array[0..4] of Real = ( -1.0001, 3.8979, -1.4273, 0.29579, -0.02353 );
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are almost the same for all results
  if not MakeColorCheck(BPmag,RPmag,0.8,4.5,bpmrp) then Exit;
  if not MakeColorCheck(Gmag,RPmag,0.401,1.529,gmrp) then Exit;
  // computing the vector
  // J
  interm := PolEval(bpmrp,coffj1,3) + PolEval(gmrp,coffj2,4);
  Jest := Gmag - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  // H
  interm := PolEval(bpmrp,coffhx,5) + 0.77736*bpmrp*gmrp - 0.12826*gmrp;
  Hest := Gmag - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  interm := PolEval(bpmrp,coffk,5);
  Ksest := Gmag - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
  // done
  Result := True;
end;
//----------------------------------------------------------------
(* Additional transforms to use if G is less precise (and likely more inaccurate)
than BP and RP (this is a very rare case). BP-RP > 0.8 *)
function Gaia2To2MASS_BadG(BP,RP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var bpmrp,interm:Real;
const coffj1:array[0..2] of Real = ( 0.027553, 1.6806, -0.028169 );
      coffh1:array[0..3] of Real = ( -0.85523, 3.7441, -0.93998, 0.12841 );
      coffk1:array[0..3] of Real =  ( -0.83457, 3.7909, -0.89966, 0.11939 );
begin
  Result := False;
  if (BP > 90) or (RP > 90) then Exit;
  // conveniently, the color bounds are almost the same for all results
  if not MakeColorCheck(BP,RP,0.8,4.9,bpmrp) then Exit;
  // J
  if bpmrp < 3.1 then interm := PolEval(bpmrp,coffj1,3)
  else interm := 0.45311 + 1.4562*bpmrp;
  Jest := BP - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  // H
  if bpmrp < 3.1 then interm := PolEval(bpmrp,coffh1,4)
  else interm := 0.94965 + 1.4791*bpmrp;
  Hest := BP - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  if bpmrp < 3.1 then interm := PolEval(bpmrp,coffk1,4)
  else interm := 1.0475 + 1.5409*bpmrp;
  Ksest := BP - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
  // done
  Result := True;
end;
//------------------------------------------------------------------
(* J H Ks using G and RP only, if it is known that BP is missing or bad *)
(* using matches to Gaia DR2 nearby stars *)
function Gaia2To2MASS_BadB(Gin,RP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var gmrp,interm:Real;
const coffj1:array[0..3] of Real = ( -0.35976, 2.6179, -1.8467, 0.89697 );
      coffj2:array[0..3] of Real = ( -8.104, 20.101, -14.947, 4.1539 );
      coffh1:array[0..3] of Real = ( -1.5611, 7.5494, -6.2037, 2.1333 );
      coffh2:array[0..3] of Real = ( -9.6993, 25.891, -20.028, 5.6209 );
      coffk1:array[0..3] of Real = ( -1.4771, 7.359, -5.7231, 1.9695 );
      // coffk1:array[0..4] of Real = ( 0.95569, -5.0521, 16.748, -15.275, 4.7687 );  oversensetive
      coffk2:array[0..3] of Real = ( -12.595, 32.675, -25.05, 6.9207 );
begin
  Result := False;
  if (Gin > 90) or (RP > 90) then Exit;
  // the color bounds are almost the same for all results
  if not MakeColorCheck(Gin,RP,0.42,1.72,gmrp) then Exit;
  // J
  if gmrp < 1.25 then interm := PolEval(gmrp,coffj1,4)
  else interm := PolEval(gmrp,coffj2,4);
  Jest := RP - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  // H
  if gmrp < 1.25 then interm := PolEval(gmrp,coffh1,4)
  else interm := PolEval(gmrp,coffh2,4);
  Hest := RP - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  if gmrp < 1.25 then interm := PolEval(gmrp,coffk1,5)
  else interm := PolEval(gmrp,coffk2,4);
  Ksest := RP - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
  // done
  Result := True;
end;
//-------------------------------------------------------------------
// J H Ks using G and BP only (because several times G - RP has been unusually large).
function Gaia2To2MASS_NoRP(Gin,BP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var bpmg,interm:Real;
const coffj:array[0..3] of Real =  ( 0.16349, 2.7538, -0.79604, 0.10266 );
      coffh:array[0..4] of Real = ( -0.37956, 6.1599, -4.2681, 1.5534, -0.21332 );
      coffk:array[0..4] of Real =  ( -0.37769, 6.3911, -4.2797, 1.5244, -0.20597 );
begin
  Result := False;
  if (Gin > 90) or (BP > 90) then Exit;
  if not MakeColorCheck(BP,Gin,0.25,3.1,bpmg) then Exit;
  // J
  interm := PolEval(bpmg,coffj,4);
  Jest := Gin - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  // H
  interm := PolEval(bpmg,coffh,5);
  Hest := Gin - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  interm := PolEval(bpmg,coffk,5);
  Ksest := Gin - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
  // done
  Result := True;
end;




//******************************************************************************

end.

