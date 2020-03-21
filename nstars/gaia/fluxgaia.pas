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
(*
function CheckErr(const mval:Currency; const verr:Currency):byte;
function GaiaTransHelper(const gmags:GaiaDR2Mags; const Jmag:Currency):Integer;
*)
function EstVmG(const Gmag:Currency; const pllx:Real; out Vmgest:Real):Boolean;
function UseGmJ(dr2mags:GaiaDR2Mags; Jin:Currency):Boolean;
(* Published transforms *)
function Gaia2ToVRI(Gmag:Currency; BPmRP:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function Gaia2To2MASS(Gmag:Currency; BPmRP:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function Gaia2ToVI_Jao(BPmag,RPmag:Currency; out Vest:Real; out Icest:Currency):Boolean;
(* My own transforms to VRI *)
function Gaia2ToVRI_MyWay(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_Red(Gmag,RPmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_Blue(Gmag,BPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_2M(Gmag,Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function GaiaToVRI_Gen(dr2mags:GaiaDR2Mags; Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
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
function ClassifyG1(G1:Currency; gmg1:Real):Word;
function Gaia12_BVRI(G1in,Gin:Currency; out Vest:Real; out Best,RcEst,IcEst:Currency):Boolean;
function Gaia12_BVRIwd(G1in,Gin:Currency; out Vest:Real; out Best,RcEst,IcEst:Currency):Boolean;
function Gaia12RP_To_VRI(G1in,Gin,RPin:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
(* White Dwarf specific transforms *)
function WD_GaiaToVRI(Gmag,BPmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function WD_GaiaToVRI_bB(Gmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function WD_GaiaToVRI_bR(Gmag,BPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function WD_GaiaToVRI_Gen(dr2mags:GaiaDR2Mags; out Vest:Real; out Rcest,Icest:Currency):Boolean;
function WD_GaiaToJHK(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function WD_GaiaToJHK_badB(Gmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
function WD_GaiaToJHK_badR(Gmag,BPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
(* Bt, Vt, and G *)
function TychoG_toRI(Gmag,G1in:Currency; Vt:Real; out RcEst,IcEst:Currency):Boolean;
function TychoG_toBV(Gmag,BPmag:Currency; Vt:Real; out Vest:Real; out Best:Currency):Boolean;
function TychoG2_toBV(dr2mags:GaiaDR2Mags; Vt:Real; out Vest:Real; out Best:Currency):Boolean;
function VtG_to_BV(Gmag,G1:Currency; Vt:Real; out Vest:Real; out Best:Currency):Boolean;

implementation
//*******************************************************************************

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
//----------------------------------------------------------------
// checks if using G-J is okay
function UseGmJ(dr2mags:GaiaDR2Mags; Jin:Currency):Boolean;
begin
  Result := False;
  if Jin > 90 then Exit;
  if dr2mags.qual = GQ_OK then Exit;
  Result := True;
  if dr2mags.qual = GQ_G then Exit;
  if (dr2mags.qual = GQ_BP) and (dr2mags.BPerr >= 0.01) then Exit;
  if (dr2mags.qual = GQ_RP) and (dr2mags.RPerr >= 0.01) then Exit;
  Result := False;
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
    mccx,mcc:Boolean;
const coffv:array[0..3] of Real = ( -0.42039, 2.9996, -4.0081, 2.2436 );
      coffvq:array[0..2] of Real = ( 0.1897, -0.67226, 1.304 );
      coffr1:array[0..2] of Real = ( 0.64862, -4.1054, 2.5421);
      coffr2:array[0..2] of Real = ( 0.43012, -2.0044, 1.4374 );
      coffrq:array[0..2] of Real = ( 0.49807, -2.1211, 1.4469 );
      coffi2:array[0..3] of Real = ( 1.1209, -2.8342, 4.4989, -1.6017 );
      coffiq:array[0..2] of Real = ( 0.2981, 0.1931, 0.74255 );
begin
  Result := False;
  if (Gmag > 90) or (RPmag > 90) then Exit;
  // conveniently, the color bounds are the same for all results
  gmrp := 99.999;
  mccx := MakeColorCheck(Gmag,RPmag,0.89,1.65,gmrp);
  if (not mccx) and ((gmrp > 90) or (gmrp < 0.299)) then Exit;
  // V
  if (not mccx) then interm := PolEval(gmrp,coffvq,3)
  else if gmrp <= 1.45 then interm := PolEval(gmrp,coffv,4)
  else interm := -6.8916 + 6.4052*gmrp;
  Vest := CurrToReal(Gmag) + interm;
  // Rc
  if (not mccx) then interm := PolEval(gmrp,coffrq,3)
  else begin
    mcc := MakeColorCheck(Gmag,Jmag,1.45,4.84,gmj);
    if mcc then begin
      interm := PolEval(gmrp,coffr1,3) - 0.4461*gmrp*gmj + 0.78221*gmj;
    end else interm := PolEval(gmrp,coffr2,3);
  end;
  Rcest := Gmag + RealToCurr(interm);
  Rcest := RoundCurrency(Rcest,False);
  // Ic
  if (not mccx) then interm := PolEval(gmrp,coffiq,3)
  else if mcc then interm := -1.1455 + 2.3309*gmrp - 0.38216*gmrp*gmj +0.38185*gmj
  else interm := PolEval(gmrp,coffi2,4);
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
//---------------------------------------------------------------------------
(* General Gaia mags to VRI for stars (not white dwarfs) *)
function GaiaToVRI_Gen(dr2mags:GaiaDR2Mags; Jmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var BPmRP:Currency;
    interm:Real;
const coffr:array[0..2] of Real = (-0.79679,0.9198,-0.057604 );
begin
  Result := False;
  if dr2mags = nil then Exit;
  BPmRP := dr2mags.BPminRP;
  // all 3 mags
  if (dr2mags.qual = GQ_OK) then begin
    if BPmRP >= 1.9 then begin
      Result := Gaia2ToVRI_MyWay(dr2mags.G,dr2mags.BP,dr2mags.RP,Vest,Rcest,Icest);
    end
    else begin
      Result := Gaia2ToVRI(dr2mags.G,BPmRP,Vest,Rcest,Icest);
    end;
  end
  // G-J based
  else if UseGmJ(dr2mags,Jmag) then begin
    Result := GaiaToVRI_2M(dr2mags.G,Jmag,Vest,Rcest,Icest);
  end
  // BP - G based
  else if (dr2mags.qual = GQ_BP) then begin
    Result := GaiaToVRI_Blue(dr2mags.G,dr2mags.BP,Vest,Rcest,Icest);
  end
  // G-RP
  else if (dr2mags.qual = GQ_RP) then begin
    Result := GaiaToVRI_Red(dr2mags.G,dr2mags.RP,Jmag,Vest,Rcest,Icest);
  end
  // no G, BP-RP only
  else if (dr2mags.qual = GQ_BR) then begin
    Result := Gaia2ToVI_Jao(dr2mags.BP,dr2mags.RP,Vest,Icest);
    if Result then begin
      if (BPmRP > 1.8) then begin
        interm := PolEval(CurrToReal(BPmRP),coffr,3);
        Rcest := dr2mags.RP + RealToCurr(interm);
        Rcest := RoundCurrency(Rcest,False);
      end;
    end;
  end;
end;

//-------------------------------------------------------------
(* Uses all of the different methods, but only for computing V *)
function GaiaToV(dr2mags:GaiaDR2Mags; Jmag:Currency; out Vest:Real):Boolean;
var okay:Boolean;
    BPmRP:Currency;
    gmrp,gmj,bpmg,bmr,interm:Real;
const coffV1:array[0..2] of Real = (-0.0176  ,-0.00686,-0.1732 );
      coffvj:array[0..3] of Real = ( 2.2906, -2.2367, 0.8365, -0.063055 );
      coffvr:array[0..3] of Real = ( -0.42039, 2.9996, -4.0081, 2.2436 );
begin
  Result := False;
  // basic bad exit cases...
  if dr2mags = nil then Exit;
  // we use the newly implemented 'Quality' value to pick the equation to use
  BPmRP := dr2mags.BPminRP;
  okay := False;
  // all 3 mags
  if dr2mags.qual = GQ_OK then begin
    if (BPmRP > 2) then begin
      if MakeColorCheck(dr2mags.BP,dr2mags.RP,1.771,5.197,bmr) then begin
        if MakeColorCheck(dr2mags.G,dr2mags.RP,0.89,1.59,gmrp) then begin
          interm := -0.22933 + 1.0173*bmr - 1.067*gmrp;
          Vest := CurrToReal(dr2mags.G) + interm;
          okay := True;
        end;
      end;
    end;
    if not okay and (BPmRP >= -0.5 )then begin
      interm := PolEval(CurrToReal(BPmRP),coffV1,3);
      Vest := CurrToReal(dr2mags.G) - interm;
      okay := True;
    end;
  end
  // G-J
  else if UseGmJ(dr2mags,Jmag) then begin
    if MakeColorCheck(dr2mags.G,Jmag,1.438,4.6,gmj) then begin
      // the G-J to V-G color-color plot shows a break around G-J ~ 4.6
      if gmj < 4.6 then interm := PolEval(gmj,coffvj,4)
      else interm := -3.3107 + 1.3601*gmj;
      Vest := CurrToReal(dr2mags.G) + interm;
      okay := True;
    end;
  end
  // BP-G only
  else if dr2mags.qual =  GQ_BP then begin
    if MakeColorCheck(dr2mags.BP,dr2mags.G,0.814,3.61,bpmg) then begin
      interm := -0.26726 + 1.0042*bpmg;
      Vest := CurrToReal(dr2mags.G) + interm;
      okay := True;
    end;
  end
  // G-RP only
  else if dr2mags.qual = GQ_RP then begin
    if MakeColorCheck(dr2mags.G,dr2mags.RP,0.89,1.65,gmrp) then begin
      if gmrp <= 1.45 then interm := PolEval(gmrp,coffvr,4)
      else interm := -6.8916 + 6.4052*gmrp;
      Vest := CurrToReal(dr2mags.G) + interm;
      okay := True;
    end;
  end
  // BP and RP, no G
  else if dr2mags.qual = GQ_BR then begin
    if (BPmRP >= 1) and (BPmRP <= 4) then begin
      // color in range, proceeding...
      Vest := CurrToReal(dr2mags.BP) - (0.2022 + 0.02489*CurrToReal(BPmRP));
      okay := True;
    end;
  end;
  // finishing off
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
on the G,BP,RP availability and quality
GQ_OK, GQ_BP, GQ_RP, GQ_G, GQ_BR, GQ_X *)
function GaiaTo_JHK_Wr(dr2mags:GaiaDR2Mags; useWD:Boolean; out Jest,Hest,Ksest:Currency):Boolean;
begin
  Result := False;
  if (dr2mags = nil) then Exit;
  // White Dwarf specific
  if (useWD) then begin
    if (dr2mags.qual = GQ_OK) then begin
      Result := WD_GaiaToJHK(dr2mags.G,dr2mags.BP,dr2mags.RP,Jest,Hest,Ksest);
    end else if (dr2mags.qual = GQ_RP) then begin
      Result := WD_GaiaToJHK_badB(dr2mags.G,dr2mags.RP,Jest,Hest,Ksest);
    end else if (dr2mags.qual = GQ_BP) then begin
      Result := WD_GaiaToJHK_badR(dr2mags.G,dr2mags.BP,Jest,Hest,Ksest);
    end;
    Exit;
  end;

  // we chose functions for J H Ks using quality
  // using G-RP
  if dr2mags.qual = GQ_RP then begin
    Result := Gaia2To2MASS_BadB(dr2mags.G,dr2mags.RP,Jest,Hest,Ksest);
    if (Result) then Exit;
  end
  else if dr2mags.qual = GQ_BP then begin
    Result := Gaia2To2MASS_NoRP(dr2mags.G,dr2mags.BP,Jest,Hest,Ksest);
    if Result then Exit;
  end
  else if dr2mags.qual = GQ_BR then begin
    Result := Gaia2To2MASS_BadG(dr2mags.BP,dr2mags.RP,Jest,Hest,Ksest);
    if Result then Exit;
  end
  else if dr2mags.qual = GQ_OK then begin
    Result := Gaia2To2MASS_MyWay2(dr2mags.G,dr2mags.BP, dr2mags.RP,Jest,Hest,Ksest);
    if not Result then begin
      Result := Gaia2To2MASS(dr2mags.G,dr2mags.BPminRP,Jest,Hest,Ksest);
    end;
  end;
end;

//-------------------------------------------------------------------------
(* Since the Gaia provided transforms for JHKs seem to work badly, I've done my
own, for BP-RP > 0.8 only. *)
function Gaia2To2MASS_MyWay(Gmag,BPmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var bpmrp,gmrp:Real;
    interm:Real;
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
(* non white dwarf, used to make G-G1 to VRI more accurate *)
function ClassifyG1(G1:Currency; gmg1:Real):Word;
var g1x:Real;
begin
  Result := 0;
  if (G1 > 90) then Exit;
  g1x := G1;
  // from bright to dim
  if gmg1 > (0.2 * g1x - 2) then Result := 1
  else if g1x < 11.7 then Result := 2
  else if gmg1 < ((0.1/1.5)*g1x - (2.45/3)) then Result := 4
  else Result := 3;
end;
//------------------------------------------------------------------------
(* G - G1. restricted to < 0.16 to avoid problems with red stars. *)
function Gaia12_BVRI(G1in,Gin:Currency; out Vest:Real; out Best,RcEst,IcEst:Currency):Boolean;
var interm,gmg1:Real;
    gclass:Word;
const coff_b:array[0..3] of Real = ( 0.5776, 0.96881, 165.87, -640.89 );
      coff1_v:array[0..2] of Real = ( 0.097589, 0.71169, 29.38 );
      coff2_v:array[0..2] of Real = ( 1.2888, -11.431, 53.917 );
      coff1_r:array[0..3] of Real = ( 0.16536, -12.519, 105.13, -222.76 );
      coff2_r:array[0..3] of Real = ( -0.12758, -0.14506, -12.904, 88.585 );
      coff2_i:array[0..2] of Real = ( -1.2818, 4.3073, -24.04 );
begin
  Result := False;
  if not MakeColorCheck(Gin,G1in,0.069,0.25,gmg1) then Exit;
  gclass := ClassifyG1(G1in,gmg1);
  if (gclass < 1) or (gclass > 3) then Exit;
  // B
  Best := 99.999;
  if (gmg1 < 0.16) and (gmg1 > 0.0) then begin
    interm := PolEval(gmg1,coff_b,4);
    Best := Gin + RealToCurr(interm);
    Best := RoundCurrency(Best,False);
    Result := True;
  end;
  // V
  Vest := 99.999; interm := 99.999;
  if (gclass = 1) and (gmg1 <= 0.222) then interm := PolEval(gmg1,coff1_v,3)
  else if (gclass = 2) and (gmg1 >=0.046) then begin
    if gmg1 <= 0.24 then interm  := PolEval(gmg1,coff2_v,3);
  end
  else if (gmg1 >= 0.148) then interm := -1.2233 + 11.682*gmg1;
  if (interm < 90) then begin
    Result := True;
    Vest := CurrToReal(Gin) + interm;
  end;

  // Rc
  RcEst := 99.999; interm := 99.999;
  if (gclass = 1) and (gmg1 >= -0.014) then begin
    if (gmg1 <= 0.256) then interm := PolEval(gmg1,coff1_r,4);
  end
  else if (gclass = 2) and (gmg1 >= -0.041) then begin
    if (gmg1 <= 0.24) then interm := PolEval(gmg1,coff2_r,4);
  end
  else if (gmg1 > 0.148) then interm := -1.1174 + 5.6025*gmg1;
  if (interm < 90) then begin
    Result := True;
    RcEst := Gin + RealToCurr(interm);
    RcEst := RoundCurrency(RcEst,False);
  end;

  // Ic
  IcEst := 99.999; interm := 99.999;
  if (gclass = 1) and (gmg1 >= -0.014) then begin
    if (gmg1 <= 0.256) then interm := -0.36793 - 5.6455*gmg1;
  end
  else if (gclass = 2) and (gmg1 >= 0.033) then begin
    if (gmg1 <= 0.24) then interm := PolEval(gmg1,coff2_i,3);
  end
  else if (gmg1 > 0.148) then interm := -0.40721 - 4.8031*gmg1;
  if (interm < 90) then begin
    Result := True;
    IcEst := Gin + RealToCurr(interm);
    IcEst := RoundCurrency(IcEst,False);
  end;

end;

(* G-G1 for white dwarfs. rather rough. *)
function Gaia12_BVRIwd(G1in,Gin:Currency; out Vest:Real; out Best,RcEst,IcEst:Currency):Boolean;
var interm,gmg1:Real;
const coff_b:array[0..2] of Real = ( 0.07732, 6.262, 23.348 );
      coff_r:array[0..3] of Real =  (-0.074741, 10.846, -117.81, 565.95 );
      coff_i:array[0..3] of Real =  ( -0.072968, 14.788, -107.65, 482.32 );
begin
  Result := False;
  if not MakeColorCheck(Gin,G1in,-0.031,0.174,gmg1) then Exit;
  // B (extra rough)
  Best := 99.999;
  if (gmg1 <= 0.132) then begin
     interm := PolEval(gmg1,coff_b,3);
     Best := Gin + RealToCurr(interm);
     Best := RoundCurrency(Best,False);
     Result := True;
  end;
  // V
  Vest := 99.999;
  if (gmg1 <= 0.154) then begin
     interm := -0.043711 + 2.3046*gmg1;
     Vest := CurrToReal(Gin) + interm;
     Result := True;
  end;
  // Rc
  RcEst := 99.999;
  if (gmg1 >= -0.007) then begin
     interm := PolEval(gmg1,coff_r,4);
     RcEst := Gin + RealToCurr(interm);
     RcEst := RoundCurrency(RcEst,False);
     Result := True;
  end;
  // Ic (okay for white dwarfs )
  IcEst := 99.999;
  if (gmg1 >= -0.007) then begin
     interm := PolEval(gmg1,coff_i,4);
     IcEst := Gin + RealToCurr(interm);
     IcEst := RoundCurrency(RcEst,False);
  end

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
const coff_vp1:array[0..2] of Real = ( -0.072035, 0.82915, 0.26298);
      coff_vp2:array[0..2] of Real = ( -0.019933, 0.11665, 0.10219 );
      coff_rp:array[0..2] of Real = ( -0.010177, -0.31358, 0.080597 );
      coff_ip1:array[0..2] of Real = ( 0.033511, 0.72949, -0.095172 );
      coff_ip2:array[0..2] of Real = ( 0.011394, 1.0915, -0.010347);
begin
  Result := False;
  ok1 := MakeColorCheck(BPmag,Gmag,-0.254,0.732,bpmg);
  ok2 := MakeColorCheck(BPmag,RPmag,-0.555,1.552,bpmrp);
  if not ok2 then Exit;
  // V
  if ok1 then begin
    interm := PolEval(bpmrp,coff_vp1,3) - 1.9785*bpmg; // std err ~ 0.0375
  end else interm := PolEval(bpmrp,coff_vp2,3); // std er ~ 0.0451
  Vest := CurrToReal(Gmag) + interm;
  Result := True;
  // Rc and Ic are based on less data
  RcEst := 99.999;
  IcEst := 99.999;
  if (bpmrp >= -0.376) then begin
    interm := PolEval(bpmg,coff_rp,3);  // std err ~ 0.014, but only 68 stars
    RcEst := CurrToReal(Gmag) + interm;
    RcEst := RoundCurrency(RcEst,False);
    if ok1 and (bpmg >= -0.179) then begin
      interm := PolEval(bpmrp,coff_ip2,3) - 1.0157*bpmg; // std err ~ 0.017 (78 stars)
    end else interm := PolEval(bpmrp,coff_ip1,3); // std err ~ 0.021 (78 stars)
    IcEst := CurrToReal(Gmag) - interm;
    IcEst := RoundCurrency(IcEst,False);
  end;
end;
//--------------------------------------
function WD_GaiaToVRI_bB(Gmag,RPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var gmrp,interm:Real;
const coff_v:array[0..2] of Real = ( -0.028268, 0.15886, 0.41231 );
      coff_r:array[0..2] of Real = ( -0.0014536, -0.50506, 0.16002 );
begin
  Result := False;
  if not MakeColorCheck(Gmag,RPmag,-0.301,0.83,gmrp) then Exit;
  interm := PolEval(gmrp,coff_v,3); // std err ~ 0.045
  Result := True;
  Vest := interm + CurrToReal(Gmag);
  // Rc and Ic are based on less data
  RcEst := 99.999;
  IcEst := 99.999;
  if (gmrp >= -0.235) then begin
    interm := PolEval(gmrp,coff_r,3); // std err ~ 0.015, but only 68 stars
    Rcest := RealToCurr(interm) + Gmag;
    RcEst := RoundCurrency(RcEst,False);
    interm := 0.0089493 + 1.1322*gmrp; // std err ~ 0.017, but only 78 stars
    Icest := Gmag - RealToCurr(interm);
    IcEst := RoundCurrency(IcEst,False);
  end;
end;
//---------------------------------------
function WD_GaiaToVRI_bR(Gmag,BPmag:Currency; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var gmbp,interm:Real;
const coff_v:array[0..2] of Real = ( -0.0081346, 0.33511, 0.35441 );
      coff_r:array[0..2] of Real = ( -0.026347, -0.74993, 0.52873 );
      coff_i:array[0..2] of Real = (  0.070114,  1.8454, -0.95777 );
begin
  Result := False;
  if not MakeColorCheck(BPmag,Gmag,-0.254,0.732,gmbp) then Exit;
  interm := PolEval(gmbp,coff_v,3); // std err ~ 0.048
  Result := True;
  Vest := interm + CurrToReal(Gmag);
  // Rc and Ic are based on less data
  RcEst := 99.999;
  IcEst := 99.999;
  if (gmbp >= -0.179) then begin
    interm := PolEval(gmbp,coff_r,3); // std err ~ 0.016, but only 68 stars
    Rcest := RealToCurr(interm) + Gmag;
    RcEst := RoundCurrency(RcEst,False);
    interm := PolEval(gmbp,coff_i,3); // std err ~ 0.036, but only 78 stars
    Icest := Gmag - RealToCurr(interm);
    IcEst := RoundCurrency(IcEst,False);
  end;
end;
//--------------------------------------
function WD_GaiaToVRI_Gen(dr2mags:GaiaDR2Mags; out Vest:Real; out Rcest,Icest:Currency):Boolean;
var G:Currency;
begin
  Result := False;
  if dr2mags = nil then Exit;
  G := dr2mags.G;
  // all 3 mags
  if (dr2mags.qual = GQ_OK) then begin
      Result := WD_GaiaToVRI(G,dr2mags.BP,dr2mags.RP,Vest,Rcest,Icest);
  end
  // BP - G based
  else if (dr2mags.qual = GQ_BP) then begin
    Result := WD_GaiaToVRI_bR(G,dr2mags.BP,Vest,Rcest,Icest);
  end
  // G-RP
  else if (dr2mags.qual = GQ_RP) then begin
    Result := WD_GaiaToVRI_bB(G,dr2mags.RP,Vest,Rcest,Icest);
  end
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
//-------------------------------------------------------------------
// G-RP specific for J H K
function WD_GaiaToJHK_badB(Gmag,RPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var interm,gmrp:Real;
const coff_hp:array[0..3] of Real = ( -0.14084, 2.5682, 1.15, -1.6628 );
      coff_kp:array[0..2] of Real = ( -0.13398, 2.8898, -0.18748 );
begin
  Result := False;
  // J mag
  if not MakeColorCheck(Gmag,RPmag,-0.301,0.848,gmrp) then Exit;
  interm := -0.10634 + 2.1701*gmrp;
  Jest := Gmag - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  Result := True;
  // H mag and Ks mag
  Hest := 99.999;
  Ksest := 99.999;
  // H
  if gmrp > 0.82 then Exit;
  interm := PolEval(gmrp,coff_hp,4);
  Hest := Gmag - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  if (gmrp < -0.235) or (gmrp > 0.797) then Exit;
  interm := PolEval(gmrp,coff_kp,3);
  Ksest := Gmag - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
end;
//-------------------------------------------------------------
// BP-G specific for J H K
function WD_GaiaToJHK_badR(Gmag,BPmag:Currency; out Jest,Hest,Ksest:Currency):Boolean;
var interm,bpmg:Real;
const coff_jp:array[0..2] of Real = ( 0.040726, 3.5978, -2.0329 );
      coff_hp:array[0..2] of Real =  ( 0.056297, 4.472, -2.8638 );
      coff_kp:array[0..4] of Real = ( -0.012161, 5.0443, 0.96868, -17.026, 15.804 );
begin
  Result := False;
  // J mag
  if not MakeColorCheck(BPmag,Gmag,-0.254,0.776,bpmg) then Exit;
  interm := PolEval(bpmg,coff_jp,3);
  Jest := Gmag - RealToCurr(interm);
  Jest := RoundCurrency(Jest,False);
  Result := True;
  // H mag and Ks mag
  Hest := 99.999;
  Ksest := 99.999;
  // H
  if bpmg > 0.732 then Exit;
  interm := PolEval(bpmg,coff_hp,3);
  Hest := Gmag - RealToCurr(interm);
  Hest := RoundCurrency(Hest,False);
  // Ks
  interm := PolEval(bpmg,coff_kp,3);
  Ksest := Gmag - RealToCurr(interm);
  Ksest := RoundCurrency(Ksest,False);
end;
//--------------------------------------------------------------
(* Bt, Vt, and G *)
function TychoG_toRI(Gmag,G1in:Currency; Vt:Real; out RcEst,IcEst:Currency):Boolean;
var interm,vtmg,gmg1:Real;
    useg:Boolean;
const coff_r:array[0..2] of Real = ( -0.14589, -0.36123, 0.36372 );
      coff_i:array[0..2] of Real = ( 0.24121, 1.3251, -0.38169 );
begin
  Result := False;
  if not MakeColorCheck(Vt,Gmag,0.213,2.014,vtmg) then Exit;
  useg := MakeColorCheck(Gmag,G1in,0.006,0.18,gmg1);
  // Rc (standard error ~0.063)
  interm := PolEval(vtmg,coff_r,3);
  RcEst := Gmag + RealToCurr(interm);
  RcEst := RoundCurrency(RcEst,False);
  // Ic
  if useg and (vtmg <= 1.442) then begin
    interm := 0.30797 + 0.23138*vtmg + 4.6568*gmg1;
  end else interm := PolEval(vtmg,coff_i,3);
  IcEst := Gmag - RealToCurr(interm);
  IcEst := RoundCurrency(IcEst,False);
  Result := True;
end;
//------------------------------------------------------------------------
function TychoG_toBV(Gmag,BPmag:Currency; Vt:Real; out Vest:Real; out Best:Currency):Boolean;
var vtmg,bpmg,interm:Real;
    use_bp:Boolean;
const bmg_a:array[0..2] of Real = ( -0.012696, 3.9787, -1.9468 );
      bmg_b:array[0..2] of Real = ( -0.15935, -0.97609, 2.3817 );
      vmg_b:array[0..2] of Real = ( -0.044567, 0.30056, 0.19177 );
begin
  Result := False;
  if not MakeColorCheck(Vt,Gmag,0.15,1.378,vtmg) then Exit;
  use_bp :=  MakeColorCheck(BPmag,Gmag,0.221,1.662,bpmg);
  // V
  if (use_bp) then interm := 0.32543*vtmg + PolEval(bpmg,vmg_b,3)
  else interm := -0.03645 + 0.83631*vtmg;
  Vest := CurrToReal(Gmag) + interm;
  // B
  if (use_bp) then interm := 0.92139*vtmg + PolEval(bpmg,bmg_b,3)
  else if (vtmg < 0.825) then interm := PolEval(vtmg,bmg_a,3)
  else interm := 0.98177 + 1.2329*vtmg;
  Best := Gmag + RealToCurr(interm);
  Best := RoundCurrency(Best,False);
  Result := True;
end;
//-------------------------------------------------------------------------
function TychoG2_toBV(dr2mags:GaiaDR2Mags; Vt:Real; out Vest:Real; out Best:Currency):Boolean;
var ubp:Boolean;
const xcur:Currency = 99.999;
begin
  Result := False;
  if (dr2mags = nil) or (Vt > 20) then Exit;
  if (dr2mags.G > 90) or (dr2mags.Gerr > 0.01) then Exit;
  ubp := (dr2mags.BP < 90) and (dr2mags.BPerr < 0.006);
  if ubp then ubp := not dr2mags.BadRatio;
  if ubp then Result := TychoG_toBV(dr2mags.G,dr2mags.BP,Vt,Vest,Best)
  else Result := TychoG_toBV(dr2mags.G,xcur,Vt,Vest,Best)
end;
//---------------------------------------------------------------
function VtG_to_BV(Gmag,G1:Currency; Vt:Real; out Vest:Real; out Best:Currency):Boolean;
var vtmg,gmg1,vmg,bmg:Real;
    ggok,br:Boolean;
const bp1:array[0..2] of Real = ( 0.067935, 3.5441, -1.4466 );
      bp2:array[0..2] of Real = ( 0.19102, 1.9921, -0.67172 );
begin
  Result := False;
  // initial checks
  if not MakeColorCheck(Vt,Gmag,-0.053,1.662,vtmg) then Exit;
  ggok := MakeColorCheck(Gmag,G1,-0.036,0.182,gmg1);
  br := vtmg <= 1.0;
  // Vt
  if br and ggok and (gmg1 <= 0.174) then begin
    vmg := 0.0027502 + 0.59371*vtmg + 1.4615*vtmg*gmg1 + 0.17212*gmg1;
  end
  else if br then vmg := -0.036248 + 0.83527*vtmg
  else if (not br) and ggok and (gmg1 >= 0.101) then begin
    vmg := -0.39333 + 0.52027*vtmg + 4.9954*gmg1;
  end
  else vmg := -0.17598 + 0.96325*vtmg;
  Vest := vmg + CurrToReal(Gmag);
  Result := True;
  // Bt
  Best := 99.999;
  if vtmg < 0.048 then Exit;
  if br and ggok and (gmg1 <= 0.174) then begin
    bmg := PolEval(vtmg,bp2,3) + 5.262*gmg1;
  end
  else if br then bmg :=PolEval(vtmg,bp1,3)
  else if (not br) and ggok and (gmg1 >= 0.101) then begin
    bmg := 0.67196 + 0.82876*vtmg +5.3366*gmg1;
  end
  else bmg := 0.96573 + 1.246*vtmg;
  Best := RealToCurr(bmg) + Gmag;
  Best := RoundCurrency(Best,False);
end;

//******************************************************************************

end.

