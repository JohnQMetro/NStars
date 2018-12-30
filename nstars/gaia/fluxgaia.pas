unit fluxgaia;

{$mode delphi}

(* The number of flux transforms involving Gaia DR2 is increasing, so I think
I will split them off into a separate unit. *)
//*******************************************************************************
interface

uses
  Classes, SysUtils, DAMATH, Utilities, fluxtransform, gaiadr2base, EstBasics;

const
  vmgspt:array[0..23] of Real = (0.6,0.648,0.676,0.713,0.766,0.833,0.875,0.930,
      1.054,1.077,1.283,1.405,1.599,1.746,2.044,2.484,2.636,2.899,3.294,3.220,
      3.266,3.344,3.422,3.526);
  absgspt:array[0..23] of Real = (7.55,7.822,8.014,8.197,8.434,8.857,9.095,9.37,
      9.646,10.063,10.907,11.395,11.971,12.554,13.466,14.136,14.434,14.911,
      15.126,15.62,15.874,16.016,16.328,16.474);

(* Misc Utilities *)
function CheckErr(const mval:Currency; const verr:Currency):byte;
function GaiaTransHelper(const gmags:GaiaDR2Mags; const Jmag:Currency):Integer;
function EstVmG(const Gmag:Currency; const pllx:Real; out Vmgest:Real):Boolean;
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
function GaiaTo_JHK_Wr(dr2mags:GaiaDR2Mags; useWD:Boolean; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_MyWay(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_MyWay2(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_BadG(BP,RP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_BadB(Gin,RP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2To2MASS_NoRP(Gin,BP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
(* G - G1 *)
function Gaia12_To_VRI(G1in,Gin:Currency; wd:Boolean; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
function Gaia12RP_To_VRI(G1in,Gin,RPin:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
(* White Dwarf specific transforms *)
function WD_GaiaToVRI(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function WD_GaiaToJHK(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;

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
//-------------------------------------------------------
(* Estimates V-G using absolute G alone, using Mamajek's tables. accuracy??
 K7 to M9.5 kinda. *)
function EstVmG(const Gmag:Currency; const pllx:Real; out Vmgest:Real):Boolean;
var absg:Real;
    fdex,ldex:Integer;
    delta_absg,delta_vmg:Real;
begin
  Result := False;
  if (Gmag >= 90) or (pllx <= 0) then Exit;
  absg := MakeAbsoluteMagnitude(CurrToReal(Gmag),pllx);
  if (absg < absgspt[0]) and (absg >= High(absgspt)) then Exit;
  // the absolute g is within range, so we find the index...
  for fdex := High(absgspt) downto 0 do begin
    if absg >= absgspt[fdex] then begin
      ldex := fdex;  Break;
    end;
  end;
  // next, we calcuate the deltas...
  delta_absg := (absg - absgspt[ldex])/(absgspt[ldex+1] - absgspt[ldex]);
  delta_vmg := vmgspt[ldex+1] - vmgspt[ldex];
  // computing the final V-G values
  Vmgest := vmgspt[ldex] + delta_absg*delta_vmg;
  Result := True;
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
(* DR2 to V Rc Ic, with my own fits. Uses photometry from SN 35 and NOFS as the target,
so covers Red Dwarfs, and goes to much redder BP-RP.  *)
function Gaia2ToVRI_MyWay(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var bpmrp,gmrp:Real;
    interm:Real;
(* const coffv:array[0..5] of Real = ( -0.12021, 0.88909, 0.0062748, 0.059802, -0.99844, -0.060393 );
      coffr:array[0..5] of Real = ( -1.0864, 0.62724, -0.020821, 0.033311, 0.096642, -0.42465 );
      coffi:array[0..5] of Real = ( -0.81587, 0.49131, 0.020283, -0.47688, 1.6593, 0.22504 ); *)
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(BPmag,RPmag,1.771,5.197,bpmrp) then Exit;
  if not MakeColorCheck(Gmag,RPmag,0.89,1.59,gmrp) then Exit;
  // V
  interm := -0.22933 + 1.0173*bpmrp - 1.067*gmrp;
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  interm := -1.0628 + 0.86623*bpmrp - 0.22124*bpmrp*gmrp - 0.41058*gmrp;
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  interm := -0.7614 + 0.32682*bpmrp - 0.28461*bpmrp*gmrp + 1.8593*gmrp;
  Icest := Gmag - RealToCurr(interm);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;

//-----------------------------------------------------------------
(* For the many stars that have inaccurate BP, here is a G-RP based set, sometimes
using G-J as well. Uses stars from SN 35, 38, and NOFS. *)
function GaiaToVRI_Red(Gmag,RPmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var gmrp,gmj:Real;
    interm:Real;
    mcc:Boolean;
const coffv:array[0..3] of Real = ( -0.42039, 2.9996, -4.0081, 2.2436 );
      coffr1:array[0..2] of Real = ( 0.64862, -4.1054, 2.5421);
      coffr2:array[0..2] of Real = ( 0.43012, -2.0044, 1.4374 );
      coffi2:array[0..3] of Real = ( 1.1209, -2.8342, 4.4989, -1.6017 );
begin
  Result := False;
  if (Gmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(Gmag,RPmag,0.89,1.65,gmrp) then Exit;
  // V
  if gmrp <= 1.45 then interm := PolEval(gmrp,coffv,4)
  else interm := -6.8916 + 6.4052*gmrp;
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  mcc := MakeColorCheck(Gmag,Jmag,1.45,4.84,gmj);
  if mcc then begin
    interm := PolEval(gmrp,coffr1,3) - 0.4461*gmrp*gmj + 0.78221*gmj;
  end else interm := PolEval(gmrp,coffr2,3);
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  if mcc then begin
    interm := -1.1455 + 2.3309*gmrp - 0.38216*gmrp*gmj +0.38185*gmj;
  end else interm := PolEval(gmrp,coffi2,4);
  Icest := Gmag - RealToCurr(interm);
  Icest := RoundCurrency(Icest,False);
  // done
  Result := True;
end;
//--------------------------------------------------------------------------
(* Using G and BP only. This gives the best results for V *)
function GaiaToVRI_Blue(Gmag,BPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var bpmg:Real;
    interm:Real;
const coffr:array[0..2] of Real =  ( -0.70652, 0.56137, -0.028931 );
      coffi:array[0..3] of Real =  ( -0.034846, 1.762, -0.62547, 0.072043 );
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  if not MakeColorCheck(BPmag,Gmag,0.814,3.61,bpmg) then Exit;
  // V
  interm := -0.26726 + 1.0042*bpmg;
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
const coffv1:array[0..3] of Real = ( 2.2906, -2.2367, 0.8365, -0.063055 );
      coffr1:array[0..2] of Real = ( -0.86072, 0.22667, 0.036967 );
      coffi1:array[0..2] of Real = ( -0.8565, 1.2568, -0.16092 );
begin
  Result := False;
  if (Gmag > 90) or (Jmag > 90) then Exit;
  if not MakeColorCheck(Gmag,Jmag,1.438,5,gmj) then Exit;

  // conveniently, the color bounds are the same for all results
  //if not MakeColorCheck(Gmag,Jmag,2.016,4.777,gmj) then Exit;
  // V
  if gmj < 4.6 then interm := PolEval(gmj,coffv1,4)
  else interm := -3.3107 + 1.3601*gmj;
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  interm := PolEval(gmj,coffr1,3);
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  interm := PolEval(gmj,coffi1,3);
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
(* Rough transform to B for BP-RP 0.42 using APASS and Tycho B. Not very good,
 esp for Red Dwarfs. *)
function Gaia2ToB(Gmag,BPmag,RPmag:Currency; out Best:Currency):Boolean;
var bpmrp:Real;
    interm:Real;
const coffb:array[0..2] of Real = ( -0.39606, 1.6096, -0.15548 );
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) or (RPmag > 90) then Exit;
  if not MakeColorCheck(BPmag,RPmag,0.49,3.6,bpmrp) then Exit;
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
function GaiaTo_JHK_Wr(dr2mags:GaiaDR2Mags; useWD:Boolean; out Jest,Hest,Ksest:Currency):Boolean;
var gc,bc,rc:byte;
    gtest:Boolean;
begin
  Result := False;
  if (dr2mags = nil) then Exit;
  gc := CheckErr(dr2mags.G,dr2mags.Gerr);
  bc := CheckErr(dr2mags.BP,dr2mags.BPerr);
  rc := CheckErr(dr2mags.RP,dr2mags.RPerr);
  // wd
  if (useWD) then begin
    if (gc > 1) or (bc > 1) or (rc > 1) then Exit;
    Result := WD_GaiaToJHK(dr2mags.G,dr2mags.BP,dr2mags.RP,Jest,Hest,Ksest);
    Exit;
  end;

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
  Result := Gaia2To2MASS_MyWay2(dr2mags.G,dr2mags.BP, dr2mags.RP,Jest,Hest,Ksest);
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
//-----------------------------------------------------------------
(* October 2018 revision of the above: BP-RP 0.49 to 4.9 *)
function Gaia2To2MASS_MyWay2(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var bpmrp,gmrp:Real;
    interm:Real;
const coffh:array[0..4] of Real = ( -0.87075, 3.5802, -1.3091, 0.26988, -0.02135 );
      coffk:array[0..4] of Real = ( -0.82357, 3.5561, -1.2019, 0.23446, -0.017645 );
      jdef:Currency = 99.999;
begin
  Result := False;
  if (Gmag > 90) or (BPmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are almost the same for all results
  if not MakeColorCheck(BPmag,RPmag,0.49,4.9,bpmrp) then Exit;
  // computing the vector
  // J
  if MakeColorCheck(Gmag,RPmag,0.285,1.567,gmrp) then begin
    interm :=  -0.0070518 + 0.37295*bpmrp + 1.5529*gmrp;
    Jest := Gmag - RealToCurr(interm);
    Jest := RoundCurrency(Jest,False);
  end else Jest := jdef;
  // H
  interm := PolEval(bpmrp,coffh,5);
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
const coffj:array[0..3] of Real = ( -0.32076, 3.4897, -1.7097, 0.84939 );
      coffh:array[0..3] of Real = ( -1.4199, 8.1901, -5.9478, 2.0881 );
      coffk:array[0..3] of Real = ( -1.4042, 8.2834, -5.8321, 2.071 );
begin
  Result := False;
  if (Gin > 90) or (RP > 90) then Exit;
  // the color bounds are almost the same for all results
  if not MakeColorCheck(Gin,RP,0.3,1.79,gmrp) then Exit;
  // J
  if gmrp <= 1.5 then interm := PolEval(gmrp,coffj,4)
  else interm := -3.7121 + 5.0946*gmrp;
  Jest := Gin - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  // H
  if gmrp <= 1.5 then interm := PolEval(gmrp,coffh,4)
  else interm := -4.4121 + 5.9605*gmrp;
  Hest := Gin - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  if gmrp <= 1.5 then interm := PolEval(gmrp,coffk,5)
  else interm := -5.1153 + 6.6719*gmrp;
  Ksest := Gin - RealToCurr(interm);
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
//============================================================================
(* G - G1 *)
function Gaia12_To_VRI(G1in,Gin:Currency; wd:Boolean; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
var interm,gmg1:Real;
const coffv1:array[0..2] of Real =  ( 1.3896, -10.968, 50.277 );
      coffv2:array[0..2] of Real =  ( 0.14014, -1.175, 46.894 );
      coffr1:array[0..2] of Real =  ( -0.17201, -1.5349, 13.429 );
      coffr2:array[0..3] of Real =  ( -0.10507, -1.0079, -43.004, 382.31 );
      coffi2:array[0..3] of Real =  ( -0.54679, 5.8682, -231.26, 1253.3 );
      coffi3:array[0..2] of Real =  ( -0.032324, 11.14, -21.293 );
begin
  Result := False;
  if not MakeColorCheck(Gin,G1in,-0.041,0.25,gmg1) then Exit;
  // V
  if wd then interm := -0.042052 + 2.2887*gmg1
  else begin
    if gmg1 >= 0.1 then interm := PolEval(gmg1,coffv1,3)
    else interm := PolEval(gmg1,coffv2,3);
  end;
  Vest := CurrToReal(Gin) + interm;
  // Rc
  if wd then interm := -0.01754 + 4.4352*gmg1
  else begin
    if gmg1 >= 0.1 then interm := PolEval(gmg1,coffr1,3)
    else interm := PolEval(gmg1,coffr2,4);
  end;
  RcEst := CurrToReal(Gin) + interm;
  RcEst := RoundCurrency(RcEst,False);
  // Ic
  if wd then interm := PolEval(gmg1,coffi3,3)
  else begin
    if gmg1 >= 0.1 then interm := -0.72648 - 3.3531*gmg1
    else interm := PolEval(gmg1,coffi2,4);
  end;
  IcEst := CurrToReal(Gin) + interm;
  IcEst := RoundCurrency(IcEst,False);
  // done
  Result := True;
end;
(* Using RP as well. Because G-G1 might not be accurate enough, and perhaps RP
is only somewhat messed up, enough to improve things? *)
function Gaia12RP_To_VRI(G1in,Gin,RPin:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
var interm,gmg1,gmrp:Real;
begin
  Result := False;
  if not MakeColorCheck(Gin,G1in,0.096,0.25,gmg1) then Exit;
  if not MakeColorCheck(Gin,RPin,0.883,1.653,gmrp) then Exit;
  // V
  interm := -2.2852 - 3.0947*gmg1 + 3.5268*gmrp;
  Vest := CurrToReal(Gin) + interm;
  // Rc
  if (gmrp < 0.89) then RcEst := 99.999
  else begin
    interm := -1.5365 - 1.1512*gmg1 + 1.5628*gmrp;
    RcEst := CurrToReal(Gin) + interm;
    RcEst := RoundCurrency(RcEst,False);
  end;
  // Ic
  if (gmrp > 1.62) then IcEst := 99.999
  else begin
    interm := 0.17452 + 0.60946*gmg1 + 0.93149*gmrp;
    IcEst := CurrToReal(Gin) - interm;
    IcEst := RoundCurrency(IcEst,False);
  end;
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* White Dwarf specific transforms *)
//---------------------------------------
function WD_GaiaToVRI(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var interm,bpmg,bpmrp:Real;
    ok1,ok2:Boolean;
const coff_vp:array[0..2] of Real = (-0.072114,0.82726,0.26221);
      coff_ip:array[0..2] of Real = (-0.051756,-0.8131,0.17353);
begin
  Result := False;
  ok1 := MakeColorCheck(BPmag,Gmag,-0.254,0.732,bpmg);
  ok2 := MakeColorCheck(BPmag,RPmag,-0.555,1.552,bpmrp);
  if not (ok1 and ok2) then Exit;
  // V
  interm := PolEval(bpmrp,coff_vp,3) - 1.9711*bpmg;
  Vest := CurrToReal(Gmag) + interm;
  Result := True;
  // Rc and Ic are based on less data and are less accurate
  RcEst := 99.999;
  IcEst := 99.999;
  if (bpmrp >= -0.376) then begin
    if (bpmg >= -0.179) then begin
       interm := -0.04924 - 1.8723*bpmg + 0.47084*bpmg*bpmrp +0.38608*bpmrp;
       RcEst := CurrToReal(Gmag) + interm;
       RcEst := RoundCurrency(RcEst,False);
    end;
    // Ic is especially inaccurate (std err 0.087, max err 0.6!)
    interm := PolEval(bpmrp,coff_ip,3);
    IcEst := CurrToReal(Gmag) + interm;
    IcEst := RoundCurrency(IcEst,False);
  end;
end;
//--------------------------------------
// J H K needs a custom transform more, but accuracies are relativly poor...
function WD_GaiaToJHK(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var interm,bpmrp,gmrp:Real;
const coff_jp:array[0..3] of Real = (-0.063243,1.3532 , 0.054762,-0.16766);
      coff_hp:array[0..3] of Real = (-0.12813 ,0.22817, 0.29664 ,-0.28919);
      coff_kp:array[0..2] of Real = (-0.12973 ,2.9173 ,-0.32451);
begin
  Result := False;
  // J mag
  if not MakeColorCheck(BPmag,RPmag,-0.555,1.594,bpmrp) then Exit;
  interm := PolEval(bpmrp,coff_jp,4);
  Jest := Gmag - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  Result := True;
  // H mag and Ks mag
  Hest := 99.999;
  Ksest := 99.999;
  if not MakeColorCheck(Gmag,RPmag,-0.301,1.128,gmrp) then Exit;
  // H mag
  if bpmrp <= 1.552 then begin
    interm := PolEval(bpmrp,coff_hp,4) + 2.2561*gmrp;
    Hest := Gmag - RealToCurr(interm);
    Hest := RoundCurrency(Hest,False);
  end;
  // Ks mag
  if (gmrp < -0.235) or (gmrp > 0.889) then Exit;
  interm := PolEval(gmrp,coff_kp,3);
  Ksest := Gmag - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
end;


//******************************************************************************

end.

