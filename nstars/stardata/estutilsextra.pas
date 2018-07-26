unit EstUtilsExtra;

{$mode delphi}

interface

uses
  Classes, SysUtils, DAMath, fluxtransform, Utilities, EstBasics;
// specials
function CarbonStarJmK_BCk(const jmk:Real; out BCk:Real):Boolean;
function vanBelleSGiantVmK_TEff(const vmk:Real; sg:Boolean; out TEffout:Real):Boolean;
function BoyajianBmV_TEff(const bmv:Real;out TEffout:Real):Boolean;
function NievaOB_UBV_Teff(Uj,Bj,Vj:Real; out TEffout:Real):Boolean;
function NievaOB_TEff_BCv(const teffin:Real):Real;
// ultracool star methods
function UltracoolJmK_BCk(const jmk:Real; out BCk:Real):Boolean;
function Ultracool_AbsHMagToTEff(young:Boolean; const AbsHMag:Real; out TEffout:Real):Boolean;
function Ultracool_SpTToBCj(young:Boolean; scolor:Char; subrange:Real; out BCjout:Real):Boolean;
// red dwarf star methods
function BoyajianMKLuminosityVmK(const vmk:Real; const infeh:Currency; out Lum:Real):Boolean;
function BoyajianMKTEffVmK(const vmk:Real; const infeh:Currency; out TEffout:Real):Boolean;
function RedDwarfMassFit(const Mv_in:Real; out mass_est:Real):Boolean;
function RedDwarfMassFitK(const Mk_in:Real; out mass_est:Real):Boolean;
function RedDwarfMassFitMann(const Mk_in:Real; out mass_est:Real):Boolean;
// Subdwarf estimates
function LateESubdwarfSpT2TEff(const sptin:Real; out Teff_est:Real):Boolean;
// Alonso TEff
function AlonsoBVTEff(const BmV:Real; const fehin:Currency; out Teffout:Real):Boolean;
function AlonsoVITEff(const VmIc:Real; const fehin:Currency; out Teffout:Real):Boolean;
function AlonsoVKTEff(const VmKs:Real; const fehin:Currency; out Teffout:Real):Boolean;
// Alonso mid-FGK BCK estimator (later replaced by Casagrande fits)
function AlonsoPhi(const VmKin:Real; const fehin:Currency; out resval:Real):Boolean;
function AlonsoBCCalc(const VmKin:Real; sunx:Boolean; const fehin:Currency; out bck:Real):Boolean;
// Pecault-Mamajek Pre-Main-Sequence Relations
function PecMamjPMS_VmKsToTEff(const vmks_in:Real; out TEffout:Real):Boolean;
function PecMamjPMS_VmJToTEff(const vmj_in:Real; out TEffout:Real):Boolean;
function PecMamjPMS_VmKsToBCv(const vmks_in:Real; out BCvout:Real):Boolean;
function PecMamjPMS_VmJToBCj(const vmj_in:Real; out BCjout:Real):Boolean;
// Hernández and Bonifacio methods
function HerBon_DwarfTEffEstVK(const VmKsin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HerBon_DwarfTEffEstVJ(const VmJin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HerBon_DwarfTEffEstBV(const BmVin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HerBon_GiantTEffEstVK(const VmKsin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HerBon_GiantTEffEstVJ(const VmJin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HerBon_DwarfBLumEstVK(const aVm,aKm:Real; const feh_in:Currency; adj:Boolean; out BLumout:REal):Boolean;
function HerBon_DwarfBLumEstVJ(const aVm,aJm:Real; const feh_in:Currency; adj:Boolean; out BLumout:REal):Boolean;
// Huang+ methods
function HuangEtAl_DwarfTEffEstVK(const VmKsin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HuangEtAl_DwarfTEffEstBV(const BmVin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HuangEtAl_GiantTEffEstVH(const VmHin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function HuangEtAl_GiantTEffEstBV(const BmVin:Real; const fehin:Currency; out TEffout:Real):Boolean;
// additional mass estimation methods
function TorresEtAlMass(const TEff:Real; const feh,logg:Currency; out mass:Real):Boolean;
function GafeiraEtAlMass(const Lum,Age:Real; const feh:Currency; out mass:Real):Boolean;

const
(*For late-cool subdwarfs classified using the Reid-Gizis-Lépine-Shara scheme,
I can find no Bolometric corrections or complete temperature scale.
 The BCk figures below are based on ultracool luminosities from 'The Benchmark
Ultracool Subdwarf HD 114762B: A Test of Low-Metallicity Atmospheric and
Evolutionary Models' and MKs relations from 'A spectroscopic and proper motion
search of Sloan digital Sky Survey: red subdwarfs in binary systems'.*)
BCk_for_sdM:array[0..19] of Real = (2.426,2.422,2.417,2.413,2.409,2.405,2.400,
                           2.396,2.392,2.388,2.383,2.379,2.375,2.371,2.366,
                           2.362,2.358,2.354,2.349,2.345);
(* the Temeratures are based on a rounded linear fit to sdM0 to sdM3.5 in
'Nearby M subdwarfs from LAMOST DR2 (Bai+, 2016)' for Fe/H -1 only. Those
TEffs are based on fitting to a 100K model grid. The temperatures seem a bit
cooler than older estimations. extrapolating to ultracool temperatures might be
risky, but there is little available for ultracool, and nothing in-between. *)
TEff_for_sdM:array[0..19] of Real = (3630,3580,3540,3490,3450,3400,3360,3310,
                          3270,3220,3180,3130,3090,3050,3000,2960,2910,2870,
                          2820,2780);

(* possible adjustments for the Hernández-Bonifacio Bolometric Luminosity func *)
HerBon_Ks_adj = 1.0256;  // to make sure the sun luminosity is 1 using V-Ks
HerBon_J_adj  = 1.0302;  // to make sure the sun luminosity is 1 using V-J

var
  sunAlonsoPhi_1:Real;
  sunAlonsoPhi_2:Real;


implementation
//==============================================================================
(* From 'Near-Infrared Photometry of Carbon Stars' by Whitelock et al, 2006. *)
function CarbonStarJmK_BCk(const jmk:Real; out BCk:Real):Boolean;
const coff:array[0..4] of Real = (0.972,2.9292,-1.1144,0.1595,-9.5689e-3);
begin
  Result := False;
  if jmk > 6.5 then Exit;
  if jmk < 1.5 then Exit;
  Result := True;
  BCk := PolEval(jmk,coff,5);
end;
//---------------------------------------------------
(* Supergiant and Bright Giant V-K to TEff from 'Supergiant Temperatures and
Linear Radii from Near-Infrared Interferometry' (van Belle+ 2008). *)
function vanBelleSGiantVmK_TEff(const vmk:Real; sg:Boolean; out TEffout:Real):Boolean;
begin
  Result := False;
  // Flux rejection
  if sg and (vmk < 0.7) then Exit
  else if sg and (vmk > 5.1) then Exit
  else if (not sg) and (vmk < 0.4) then Exit
  else if (not sg) and (vmk > 4.3) then Exit;
  // the rest is simple
  TEffout := 3037 + 5264*exp10(-0.2158*vmk);
  Result := True;
end;

//---------------------------------------------------
(* From STELLAR DIAMETERS AND TEMPERATURES. III by Boyaijan et al, 2013.
This is the B-V formula that includes A stars *)
function BoyajianBmV_TEff(const bmv:Real;out TEffout:Real):Boolean;
const coff:array[0..6] of Real = (9552,-17443,44350,-68940,57338,-24072,4009);
begin
  Result := False;
  if bmv > 1.73 then Exit;
  if bmv < -0.02 then Exit;
  Result := True;
  TEffout := PolEval(bmv,coff,7);
end;
//------------------------------------------------------------
(* From 'Temperature, gravity and bolometric correction scales for
non-supergiant OB stars' (Nieva 2012)
Really covers O7 to B3 or so...   *)
function NievaOB_UBV_Teff(Uj,Bj,Vj:Real; out TEffout:Real):Boolean;
var Qj:Real;
const coff:array[0..2] of Real = (22.59,48.57,70.26);
begin
  // checking the validity of the inputs
  Result := False;
  if (Uj > 90) or (Bj > 90) or (Vj > 90) then Exit;
  Qj := JohnsonQIndex(Uj,Bj,Vj);
  if Qj < -0.88 then Exit;
  if Qj > -0.6 then Exit;
  // computing the Result
  TEffout := 1000*PolEval(Qj,coff,3);
end;
//----------------
function NievaOB_TEff_BCv(const teffin:Real):Real;
begin
  Assert(teffin>0);
  Result := 21 - 5.34*Log10(teffin);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* from 'Calibrating Ultracool Dwarfs: Optical Template Spectra, Bolometric
Corrections, and χ Values' by Schmidt et al, 2014. Not very accurate (±0.536). *)
function UltracoolJmK_BCk(const jmk:Real; out BCk:Real):Boolean;
const coff:array[0..2] of Real = (1.42,2.13,-0.576);
begin
  Result := False;
  if jmk < 0.9 then Exit;
  if jmk > 2 then Exit;
  BCk := PolEval(jmk,coff,3);
  Result := True;
end;
//----------------------------------------------------------------
(* BC_KS = 5.05 − 0.461 × SpT + 0.0184 × SpT^2, where SpT = 0 for sdM0,
for ultracool (sdM7 or later) sd only . From 'The Benchmark Ultracool Subdwarf
HD 114762B: A Test of Low-Metallicity Atmospheric and Evolutionary Models' by
Bowler et al, 2009. *)
//----------------------------------------------------------------
(* Relations from 'Fundamental Parameters and Spectral Energy Distributions of
Young and Field Age Objects with Masses Spanning the Stellar to Planetary Regime'
by Filippazzo et al, 2015. *)
function Ultracool_AbsHMagToTEff(young:Boolean; const AbsHMag:Real; out TEffout:Real):Boolean;
const cofff:array[0..5] of Real = (-6.108e+04,2.491e+04,-3.690e+03,2.625e+02,-9.131,1.252e-01);
      coffy:array[0..4] of Real = (-5.977e+04,2.348e+04,-3.215e+03,1.904e+02,-4.167);
begin
  Result := False;
  // reject cases...
  if young then begin
    if AbsHMag < 8.6 then Exit;
    if AbsHMag > 14.5 then Exit;
  end else begin
    if AbsHMag < 9.4 then Exit;
    if AbsHMag > 18.1 then Exit;
  end;
  // the result
  Result := True;
  if young then TEffout := PolEval(absHMag,coffy,5)
  else TEffout := PolEval(absHMag,cofff,6);
end;
//-----------------------------------
function Ultracool_SpTToBCj(young:Boolean; scolor:Char; subrange:Real; out BCjout:Real):Boolean;
var inputx:Real;
const cofff:array[0..5] of Real = (8.842,-2.862,4.566e-01,-3.437e-02,1.200e-03,-1.555e-05);
      coffy:array[0..5] of Real = (-7.352,2.899,-3.020e-01,1.165e-02,-1.118e-04,-1.283e-06);
begin
  Result := False;
  // reject cases
  if (subrange < 0) or (subrange >=10) then Exit;
  if (scolor <> 'M') and (scolor <> 'L') and (scolor<>'T') then Exit;
  if (scolor = 'M') and (subrange < 6) then Exit;
  if (scolor = 'T') and (subrange > 9) Then Exit;
  // calculating the input
  if scolor = 'M' then inputx := subrange
  else if scolor = 'L' then inputx := 10 + subrange
  else inputx := 20 + subrange;
  // the result
  Result := True;
  if young then BCjout:= PolEval(inputx,coffy,6)
  else BCjout:= PolEval(inputx,cofff,6);
end;
//-----------------------------------------

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Stellar Diameters and Temperatures II. Main Sequence K & M Stars (Boyajian+ 2012)    *)
function BoyajianMKLuminosityVmK(const vmk:Real; const infeh:Currency; out Lum:Real):Boolean;
const
  coff:array[0..5] of Real = (0.0613,-0.1329,-0.0535,0.3048,-0.6038,-0.0614);
  fehmax:Currency = 0.1;
  fehmin:Currency = -0.5;
begin
  Result := false;
  if vmk < 1.7 then Exit;
  if vmk > 5 then Exit;
  if infeh < fehmin then Exit;
  if infeh > fehmax then Exit;
  // making the result
  Lum := AltBoloMaker(vmk,coff,CurrToReal(infeh));
  Result := True;
end;

function BoyajianMKTEffVmK(const vmk:Real; const infeh:Currency; out TEffout:Real):Boolean;
const
  coff:array[0..5] of Real = (7643,-1523,134,137,202,157);
  fehmax:Currency = 0.1;
  fehmin:Currency = -0.5;
begin
  Result := false;
  if vmk < 1.7 then Exit;
  if vmk > 5 then Exit;
  if infeh < fehmin then Exit;
  if infeh > fehmax then Exit;
  // making the result
  TEffout := AltBoloMaker(vmk,coff,CurrToReal(infeh));
  Result := True;
end;
//-----------------------------
(* using https://arxiv.org/pdf/1608.04775v4.pdf for red dwarf mass estimates.*)
function RedDwarfMassFit(const Mv_in:Real; out mass_est:Real):Boolean;
var modmag:Real;
const coff:array[0..4] of Real = (0.19226,-0.050737,0.010137,-0.00075399,0.000019858);
      X0 =  13;
begin
  // non–Red-Dwarfs
  Result := False;
  // the fit is supposed to be for brighter than 19
  if Mv_in > 19 then Exit;
  Result:= True;
  // polynomial fit
  modmag := Mv_in - X0;
  mass_est := PolEval(modmag,coff,5);
end;
//-----------------------------
function RedDwarfMassFitK(const Mk_in:Real; out mass_est:Real):Boolean;
var modmag:Real;
const coff:array[0..4] of Real = (0.2311,-0.1352,0.04,0.0038,-0.0032);
      X0 = 7.5;
begin
Result := False;
  // the fit is supposed to be for brighter than 19
  if Mk_in > 10 then Exit;
  Result:= True;
  // polynomial fit
  modmag := Mk_in - X0;
  mass_est := PolEval(modmag,coff,5);
end;
//----------------------------------------------------------------
(* From 'How to Constrain your M-Dwarf II (Mann+ 2018). Mk_in is absolute Ks magnitude *)
function RedDwarfMassFitMann(const Mk_in:Real; out mass_est:Real):Boolean;
var modmag,logmass:Real;
const coff:array[0..5] of Real = (-0.64661,-0.21246,-2.6534e-3,7.9485e-3,3.6899e-4,-1.9226e-4);
begin
  // boundaries (The paper has a 'safe' range of 4.5 to 10.5)
  Result := False;
  if (Mk_in > 11) or (Mk_in < 4) then Exit;
  // polynomial fir for the log mass
  modmag := Mk_in - 7.5;
  logmass := PolEval(modmag,coff,6);
  mass_est := exp10(logmass);
  Result := True;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(**)
function LateESubdwarfSpT2TEff(const sptin:Real; out Teff_est:Real):Boolean;
const poly:array[0..3] of Real = (3706,-107.8,1.686,-0.1606);
begin
  Result := False;
  if sptin < 5.5 then Exit;
  if sptin > 17 then Exit;
  Result := True;
  Teff_est := PolEval(sptin,poly,4);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Alonso TEff: from 'The Empirical scale of temperatures of the low main
sequence (F0V-K5V)' (Alonso, Arribas, and Martínez-Roger, 1995). *)
function AlonsoBVTEff(const BmV:Real; const fehin:Currency; out Teffout:Real):Boolean;
var fehr:Real;
const coff:array[0..5] of Real = (0.541,0.533,0.007,-0.019,-0.047,-0.011);
begin
  // reject ranges
  Result := False;
  if (fehin < -3.5) or (fehin > 0.5) then Exit;
  if (BmV < 0.2) or (BmV > 1.5) then exit;
  if (BmV < 0.3) and (fehin < -0.5) then Exit;
  if (BmV > 1) and (fehin < -0.5) then Exit;
  if (BmV > 0.9) and (fehin < -1.5) then Exit;
  if (BmV > 0.8) and (fehin < -2.5) then Exit;
  if (BmV < 0.35) and (fehin < -1.5) and (fehin > -2.5) then Exit;
  // computing
  fehr := fehin;
  TEffout := TEffMaker(BmV,coff,fehr);
  Result := True;
end;
//----------------------------------
function AlonsoVITEff(const VmIc:Real; const fehin:Currency; out Teffout:Real):Boolean;
var vmij:Real;
const coff:array[0..2] of Real = (0.424,0.61,-0.096);
begin
  vmij := (VmIc - 0.014)/0.779;
  // reject ranges
  Result := False;
  if (fehin < -3.5) or (fehin > 0.5) then Exit;
  if (vmij < 0.5) or (vmij > 2.5) then exit;
  if (vmij < 0.6) and (fehin < -0.5) then Exit;
  if (vmij < 0.65) and (fehin < -1.5) then Exit;
  if (vmij < 0.7) and (fehin < -1.5) and (fehin >= -2.5) then Exit;
  if (vmij > 1.3) and (fehin < -0.5) then Exit;
  if (vmij > 1.2) and (fehin < -2.5) then Exit;
  // computing
  TEffout := 5050/PolEval(vmij,coff,3);
  Result := True;
end;
//----------------------------------
function AlonsoVKTEff(const VmKs:Real; const fehin:Currency; out Teffout:Real):Boolean;
var fehr,vmktcs:Real;
const coffA:array[0..5] of Real = (0.555,0.195, 0.013,-0.008,0.009,-0.002);
      coffB:array[0..5] of Real = (0.566,0.217,-0.003,-0.024,0.037,-0.002);
begin
  vmktcs := VmKs - 0.069;
  // reject ranges
  Result := False;
  if (fehin < -3.5) or (fehin > 0.5) then Exit;
  if (vmktcs < 0.4) or (vmktcs > 4.1) then exit;
  if (vmktcs < 0.8) and (fehin < -0.5) then Exit;
  if (vmktcs < 1.1) and (fehin < -1.5) then Exit;
  if (vmktcs > 3) and (fehin < -0.5) then Exit;
  if (vmktcs > 2.4) and (fehin < -1.5) then Exit;
  if (vmktcs > 2.2) and (fehin < -2.5) then Exit;
  // computing
  fehr := fehin;
  if vmktcs <= 1.6 then TEffout := TEffMaker(vmktcs,coffA,fehr)
  else TEffout := TEffMaker(vmktcs,coffB,fehr);
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* The following 2 functions are used to estimate BCK from V-K and
Fe/H, from 'Determination of Bolometric Fluxes for F,G,K subdwarfs',
Alonso, Arribas, and Martínez-Roger, 1994.  *)
// Also, the K Alonso uses is K_TCS, where I will use K_TCS = Ks + 0.069
//--------------
function AlonsoPhi(const VmKin:Real; const fehin:Currency; out resval:Real):Boolean;
var vmk2,vmk3:Real;
    interm1,interm2,interm3:Real;
    xresult1,xresult2:Real;
const coff1:array[0..3] of Real = (2.38619E-4,-1.93659E-4,6.52621E-5,-7.95862E-6);
      coff2:array[0..3] of Real = (2.23403E-4,-1.71897E-4,5.51085E-5,-6.41071E-6);
begin
  Result := False;
  if fehin > 2 then Exit;
  if VmKin <= 0.9 then Exit;
  if VmKin > 2.9 then Exit;
  if (fehin < -0.5) and (VmKin > 2.6) then Exit;
  if (fehin < -1.5) and (VmKin < 1.1) then Exit;
  if (fehin < -1.5) and (VmKin > 2.3) then Exit;
  // calculating some intermedate values
  vmk2 := VmKin*VmKin;
  vmk3 := intpower(VmKin,3);
  interm1 := VmKin*CurrToReal(fehin);
  interm2 := vmk2*CurrToReal(fehin);
  interm3 := vmk3*CurrToReal(fehin);
  // there are two versions, one for smaller V-K
  if VmKin <= 1.7 then begin
    xresult1 := -1.01449E-5*CurrToReal(fehin) + 8.17345E-6*interm1;
    xresult1 += -2.87876E-6*interm2 + 5.40944E-7*interm3;
    resval := PolEval(VmKin,coff1,4) + xresult1;
  end
  else begin
    xresult2 := -3.71945E-5*CurrToReal(fehin) + 4.99847E-5*interm1;
    xresult2 += -2.41517E-5*interm2 + 4.10655E-6*interm3;
    resval := PolEval(VmKin,coff2,4) + xresult2;
  end;
  //resval += 0.069;
  // done
  Result := True;
end;
//--------------------------------------------------
function AlonsoBCCalc(const VmKin:Real; sunx:Boolean; const fehin:Currency; out bck:Real):Boolean;
var phi:Real;
begin
  Result := AlonsoPhi(VmKin-0.069,fehin,phi);
  if not Result then Exit;
  // if we get Φ, then ...
  if sunx then begin
    bck := -2.5*log10(phi/sunAlonsoPhi_2) + (1.438-0.069);
  end else begin
    bck := -2.5*log10(phi/sunAlonsoPhi_1) + 1.366;
  end;
  bck += 0.069;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Pecault-Mamajek Pre-Main-Sequence Relations
//-----------------------------------------------
function PecMamjPMS_VmKsToTEff(const vmks_in:Real; out TEffout:Real):Boolean;
const coff:array[0..5] of Real = (9323.430,-3516.011,1046.787,-186.3349,16.41182,-0.5188853);
begin
  Result := False;
  if vmks_in < 1 then Exit;
  if vmks_in > 6.7 then Exit;
  TEffout := PolEval(vmks_in,coff,6);
  Result := True;
end;
//-----------------------------------------------
function PecMamjPMS_VmJToTEff(const vmj_in:Real; out TEffout:Real):Boolean;
const coff:array[0..5] of Real = (9593.475,-5095.204,2053.259,-481.3940,58.16754,-2.779565);
begin
  Result := False;
  if vmj_in < 0.8 then Exit;
  if vmj_in > 5.8 then Exit;
  TEffout := PolEval(vmj_in,coff,6);
  Result := True;
end;
//-----------------------------------------------
function PecMamjPMS_VmKsToBCv(const vmks_in:Real; out BCvout:Real):Boolean;
const coff:array[0..4] of Real = (-7.443324e-2,0.247178,-0.1923234,1.318867e-2,-3.630511e-4);
begin
  Result := False;
  if vmks_in < 1 then Exit;
  if vmks_in > 6.7 then Exit;
  BCvout := PolEval(vmks_in,coff,5);
  Result := True;
end;
//-----------------------------------------------
function PecMamjPMS_VmJToBCj(const vmj_in:Real; out BCjout:Real):Boolean;
const coff:array[0..5] of Real = (-0.4557821,2.299875,-1.191653,0.3442879,-4.932544e-2,2.7244e-3);
begin
  Result := False;
  if vmj_in < 0.8 then Exit;
  if vmj_in > 5.8 then Exit;
  BCjout := PolEval(vmj_in,coff,6);
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Hernández and Bonifacio methods from 'A new implementation of the
infrared flux method using the 2MASS catalogue' (2009).  *)
//-------------------------------------
function HerBon_DwarfTEffEstVK(const VmKsin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var usefeh:Real;
const coff:array[0..5] of Real = (0.5201,0.2511,-0.0118,-0.0186,0.0408,0.0033);
begin
  Result := False;
  // flux rejection
  if VmKsin < 0.7 then Exit;
  if VmKsin > 3 then Exit;
  // feh adjusting
  usefeh := AdjFeh(fehin,-3.5,0.5);
  // calculation
  TEffout := TEffMaker(VmKsin,coff,usefeh);
  Result := True;
end;
//-------------------------------------
function HerBon_DwarfTEffEstVJ(const VmJin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var usefeh:Real;
const coff:array[0..5] of Real = (0.4997,0.3504,-0.0230,-0.0295,0.0468,0.0037);
begin
  Result := False;
  // flux rejection
  if VmJin < 0.5 then Exit;
  if VmJin > 2.3 then Exit;
  // calculation
  usefeh := AdjFeh(fehin,-3.5,0.5);
  TEffout := TEffMaker(VmJin,coff,usefeh);
  Result := True;
end;
//-------------------------------------
function HerBon_DwarfTEffEstBV(const BmVin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var usefeh:Real;
const coff:array[0..5] of Real = (0.5725,0.4722,0.0086,-0.0628,-0.0038,-0.0051);
begin
  Result := False;
  // flux rejection
  if BmVin < 0.2 then Exit;
  if BmVin > 1.3 then Exit;
  // calculation
  usefeh := AdjFeh(fehin,-3.5,0.5);
  TEffout := TEffMaker(BmVin,coff,usefeh);
  Result := True;
end;
//-------------------------------------
function HerBon_GiantTEffEstVK(const VmKsin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var usefeh:Real;
const coff:array[0..5] of Real = (0.5293,0.2489,-0.0119,-0.0042,0.0135,0.0010);
begin
  Result := False;
  // flux rejection
  if VmKsin < 1.1 then Exit;
  if VmKsin > 3.4 then Exit;
  // calculation
  usefeh := AdjFeh(fehin,-3.5,0.5);
  TEffout := TEffMaker(VmKsin,coff,usefeh);
  Result := True;
end;
//-------------------------------------
function HerBon_GiantTEffEstVJ(const VmJin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var usefeh:Real;
const coff:array[0..5] of Real = (0.4629,0.4124,-0.0417,-0.0012,0.0094,0.0013);
begin
  Result := False;
  // flux rejection
  if VmJin < 1 then Exit;
  if VmJin > 2.4 then Exit;
  // calculation
  usefeh := AdjFeh(fehin,-3.5,0.5);
  TEffout := TEffMaker(VmJin,coff,usefeh);
  Result := True;
end;
//-------------------------------------------------------------
function HerBon_DwarfBLumEstVK(const aVm,aKm:Real; const feh_in:Currency; adj:Boolean; out BLumout:Real):Boolean;
var vmks,phi,interm1,interm2,usefeh:Real;
const coff:array[0..6] of Real = (2.3522,-1.8817,0.6229,-0.0745,0.0371,-0.0990,-0.0052);
begin
  Result := False;
  // flux rejection
  if (aVm > 90) or (aKm > 90) then Exit;
  vmks := aVm-aKm;
  if vmks < 1 then Exit;
  if vmks > 3 then Exit;
  // feh adjusting
  usefeh := AdjFeh(feh_in,-3.5,0.3);
  // calculation of the phi value
  phi := BoloMaker(vmks,coff,usefeh);
  // calculating bolometric luminosity
  interm1 := exp10(-0.4*aKm)*bolexp;
  interm2 := phi/Flux_ZP;
  if adj then interm2 := HerBon_Ks_adj*interm2;
  BLumout := interm1*interm2;
  // finally
  Result := True;
end;
//-------------------------------------------------------
function HerBon_DwarfBLumEstVJ(const aVm,aJm:Real; const feh_in:Currency; adj:Boolean; out BLumout:REal):Boolean;
var vmj,phi,interm1,interm2,usefeh:Real;
const coff:array[0..6] of Real = (2.4945,-2.2635,0.9615,-0.1509,0.0657,-0.1365,-0.0074);
begin
  Result := False;
  // flux rejection
  if (aVm > 90) or (aJm > 90) then Exit;
  vmj := aVm-aJm;
  if vmj < 0.8 then Exit;
  if vmj > 2.4 then Exit;
  // feh adjusting
  usefeh := AdjFeh(feh_in,-3.5,0.3);
  // calculation of the phi value
  phi := BoloMaker(vmj,coff,usefeh);
  // calculating bolometric luminosity
  interm1 := exp10(-0.4*aJm)*bolexp;
  interm2 := phi/Flux_ZP;
  if adj then interm2 := HerBon_J_adj*interm2;
  BLumout := interm1*interm2;
  // finally
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Color+[Fe/H] to TEff relations from 'Empirical metallicity-dependent
calibrations of effective temperature against colours for dwarfs and giants
based on interferometric data' (Huang et al, 2015). These use angular measurements
and are around ~130K cooler than the Casagrande 2009 estimations. *)
//------------------------------------------------
function HuangEtAl_DwarfTEffEstVK(const VmKsin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var fehr:Real;
const coffA:array[0..5] of Real = (0.54042,0.23676,-0.00796,-0.03798,0.05413,-0.00448);
      coffB:array[0..5] of Real = (0.55470,0.22536,-0.00647,-0.06318,0.11273, 0.00697);
      fehmax:Currency = 0.4;
      fehmin:Currency = -0.9;
begin
  // reject tests
  Result := False;
  if fehin < fehmin then Exit;
  if fehin > fehmax then EXit;
  if VmKsin < -0.1 then Exit;
  if VmKsin > 5.5 then Exit;
  // going ahead. for Redder Stars, we use B
  fehr := fehin;
  if VmKsin >= 0.86 then TEffout := TEffMaker(VmKsin,coffB,fehr)
  else TEffout := TEffMaker(VmKsin,coffA,fehr);
  Result := True;
end;
//------------------------------------------------
function HuangEtAl_DwarfTEffEstBV(const BmVin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var fehr:Real;
const coffA:array[0..5] of Real = (0.59225,0.39926,0.07848,-0.04374,-0.04289,-0.01406);
      coffB:array[0..5] of Real = (0.63421,0.30538,0.12308,-0.06216,-0.01987,-0.00951);
      fehmax:Currency = 0.4;
      fehmin:Currency = -0.9;
begin
  // reject tests
  Result := False;
  if fehin < fehmin then Exit;
  if fehin > fehmax then EXit;
  if BmVin < -0.05 then Exit;
  if BmVin > 1.73 then Exit;
  // going ahead. for Redder Stars, we use B
  fehr := fehin;
  if BmVin >= 0.32 then TEffout := TEffMaker(BmVin,coffB,fehr)
  else TEffout := TEffMaker(BmVin,coffA,fehr);
  Result := True;
end;
//------------------------------------------------
// interestingly, V-H seems to have a smaller sd than V-Ks for Giant Stars
function HuangEtAl_GiantTEffEstVH(const VmHin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var fehr:Real;
const coff:array[0..5] of Real = (0.42367,0.33746,-0.02298,-0.04768,0.11713,-0.05342);
      fehmax:Currency = 0.3;
      fehmin:Currency = -0.6;
begin
  // reject tests
  Result := False;
  if fehin < fehmin then Exit;
  if fehin > fehmax then EXit;
  if VmHin < 1.94 then Exit;
  if VmHin > 5.79 then Exit;
  // going ahead.
  fehr := fehin;
  TEffout := TEffMaker(VmHin,coff,fehr);
  Result := True;
end;
//------------------------------------------------
// probably not the best option because of B-V turnback
function HuangEtAl_GiantTEffEstBV(const BmVin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var fehr:Real;
const coff:array[0..5] of Real = (0.81784,0.03096,0.19350,-0.03292,0.00762,0.11140);
      fehmax:Currency = 0.3;
      fehmin:Currency = -0.6;
begin
  // reject tests
  Result := False;
  if fehin < fehmin then Exit;
  if fehin > fehmax then EXit;
  if BmVin < 0.88 then Exit;
  if BmVin > 1.59 then Exit;
  // going ahead.
  fehr := fehin;
  TEffout := TEffMaker(BmVin,coff,fehr);
  Result := True;
end;
//---------------------------------------------------------
// additional mass estimation methods
//-----------------------------------------
(* From 'Accurate masses and radii of normal stars: Modern results and applications'
G. Torres, J. Andersen and A. Giménez, 2010. Somewhat doubtful accuaracy due to
lack of Fe/H estimations and use of Flower BCv. *)
function TorresEtAlMass(const TEff:Real; const feh,logg:Currency; out mass:Real):Boolean;
var xval,interm1,interm2,fehr,loggr:Real;
const coff_teff:array[0..3] of Real = (1.5689,1.3787,0.4243,1.139);
      coff_logg:array[0..2] of Real = (0,-0.1425,0.01969);
      feh_coff = 0.1010;
begin
  // reject conditions...
  Result := False;
  if TEff <  4000 then Exit;
  if TEff > 33000 then Exit;
  if logg > 4.8 then Exit;
  if (logg < 3.6) and (logg > -8) then Exit;
  // setting up values...
  if logg < 0 then loggr := 4.5
  else loggr := logg;
  fehr := AdjFeh(feh,-0.6,0.4);
  xval := log10(TEff) - 4.1;
  // computing...
  interm1 := PolEval(xval,coff_teff,4) + feh_coff*fehr;
  interm2 := PolEval(loggr,coff_logg,3);
  mass := interm1 + interm2;
  // done
  Result := True;
end;
//---------------------------------------------------------------------------
(* Mass estimation from 'Mass-luminosity relation for FGK main sequence stars:
metallicity and age contributions' (Gafeira, Patacas, and Fernandes : 2012).
Uses a subset of the Torres data, FGK only *)
function GafeiraEtAlMass(const Lum,Age:Real; const feh:Currency; out mass:Real):Boolean;
var agec,fehr,logl:Real;
    interm1,interm2,interm3,lmass:Real;
const coff_lumA:array[0..3] of Real = (0,0.208, 0.063,-0.13);
      coff_lumB:array[0..3] of Real = (0,0.238, 0.130,-0.28);
      coff_fehB:array[0..3] of Real = (0,0.044,-0.167,-0.116);
      coff_lumC:array[0..3] of Real = (0,0.219, 0.063,-0.119);
      coff_fehC:array[0..3] of Real = (0,0.079,-0.122,-0.145);
      coff_ageC:array[0..3] of Real = (0,0.144,-0.224,-0.076);
begin
  // reject conditions
  Result := False;
  if (Age < 0) or (Age > 13.82) then Exit;
  if (Lum<=0) then Exit;
  logl := log10(Lum);
  if (logl < -0.5) or (logl > 0.7) then Exit;
  // setting up some values
  fehr := AdjFeh(feh,-0.6,0.4);
  agec := Age/4.6;
  // calculating case A (luminosity only)
  if feh > 9 then lmass := PolEval(logl,coff_lumA,4)
  // calculating case B (luminosity and [Fe/H])
  else if Age = 0 then begin
    interm1 := PolEval(logl,coff_lumB,4);
    interm2 := PolEval(fehr,coff_fehB,4);
    lmass := interm1 + interm2;
  end
  // calculating case C  (luminosity, [Fe/H] and Age)
  else begin
    interm1 := PolEval(logl,coff_lumC,4);
    interm2 := PolEval(fehr,coff_fehC,4);
    interm3 := PolEval(agec,coff_ageC,4);
    lmass := interm1 + interm2 + interm3;
  end;
  // finishing...
  mass := exp10(lmass);
  Result := True;
end;
//==============================================================================
begin
  AlonsoPhi(1.486,0,sunAlonsoPhi_1);
  AlonsoPhi(1.552-0.069,0,sunAlonsoPhi_2);
end.

