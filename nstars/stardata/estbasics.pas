unit EstBasics;

{$mode delphi}
(* Basic Magnitude to Luminosity (and vice versa), as well as blackbody methods
and related bolometric corrections. *)

interface

uses
  Classes, SysUtils, DAMath;
//***********************************************************************
const
  // physical and derived constants
  (* absolute physical constants *)
  c = 299792458.0;      // speed of light (in m/s)
  h = 6.6260709544E-34; // Planck's constant
  k = 1.380648528E-23;  // Boltzmann constant
  (* derived constants *)
  sigma = 5.670367E-8;   // stefan bolzmann constant
  Z1 = 2*h*c*c;
  Z2 = h*c/k;

  (* using Mamajek's Values *)
  SUN_TEff = 5772;
  SUN_VMag = 4.862;
  SUN_BoMag = 4.74;
  SUN_BCv = SUN_BoMag - SUN_VMag;
  // Bolometric flux, apparent magnitude 0
  Flux_ZP = 2.518021002; // in e-5 erg/s/cm^2
  (* Extra values *)
  nm = 1/1000000000;
  vislow = 380;
  vishigh = 750;

  // johnson V transmission curve slopes
  // [465 to to 590) nm
  jv_tcsA:array[0..24] of Real = (0.0047613554,0.0196936734,0.1500556886,
     1.0093599796,3.541099929,5.422516252,4.421020508,2.46495819,1.146624756,
     0.471817016,0.157060242,-0.011073302,-0.093215944,-0.174934386,-0.248100282,
     -0.323776244,-0.391165162,-0.482321166,-0.563375854,-0.651313782,
     -0.748812866,-0.82855835,-0.90890503,-0.97605667,-1.028894806);
  // [590 to 730) nm
  jv_tcsB:array[0..13] of Real = (-1.073589325,-1.063876725,-0.978486061,
     -0.825518608,-0.639563465,-0.4539581775,-0.2960114956,-0.1766756058,
     -0.0960973978,-0.0479863435,-0.0220390856,-0.0094572008,-0.0036999006,
     -0.0022724818);
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
var
   planckSunInt:Real;
   VplanckSunInt:Real;
   boltz_sun,sunbol_mul:Real;
   bolexp:Real;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// magnitude to luminosity and vice versa
function AbsVMagToVLum(const avm:Real):Real;
function VLumToAbsVMag(const vlum:Real):Real;
function AbsBolMagToBolLum(const abolm:Real):Real;
function BolLumToAbsBolMag(const bollum:Real):Real;
// bolometric estimation
function StefanBoltzmann(T:Real):Real;
function PlanckBlackbody(const lambada,T:Real):Real;
function PlanckVisIntegral(const T:Real):Real;
function PlanckVBandIntegral(const T:Real):Real;
function BlackBodyExtraBCv(const inTEff:Real):Real;
// msic extras
function MakeAbsoluteMagnitude(const inmag,pllx:Real):Real;
function TEffMaker(const color:Real; const coffs:array of Real; const fehin:Real):Real;
function BoloMaker(const color:Real; const coffs:array of Real; const fehin:Real):Real;
function AltBoloMaker(const xparam:Real; const coffs:array of Real; const fehin:Real):Real;
function AdjFeh(const fehin:Currency; const fmin,fmax:Real):Real;
//***********************************************************************
implementation
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// magnitude to luminosity and vice versa
//---------------------------------------
function AbsVMagToVLum(const avm:Real):Real;
begin
  Result := exp10(0.4*(SUN_VMag - avm));
end;
//---------------------------------------
function VLumToAbsVMag(const vlum:Real):Real;
begin
  Result := SUN_VMag - 2.5*Log10(vlum);
end;
//---------------------------------------
function AbsBolMagToBolLum(const abolm:Real):Real;
begin
  Result := exp10(0.4*(SUN_BoMag - abolm));
end;
//---------------------------------------
function BolLumToAbsBolMag(const bollum:Real):Real;
begin
  Result := SUN_BoMag - 2.5*Log10(bollum);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// bolometric estimation
//---------------------------------------------
function StefanBoltzmann(T:Real):Real;
var ival:Real;
begin
  ival := IntPower(T,4);
  Result := sigma*ival;
end;
//------------------------------------------------------
function PlanckBlackbody(const lambada,T:Real):Real;
var inv1,inv2:Real;
begin
  // we calculate in steps
  inv1 := IntPower(lambada,5);
  if T < 100 then begin
    Assert(False);
  end;
  inv2 := Z2/(lambada*T);
  inv2 := Exp(inv2)-1;
  inv1 := inv1*inv2;
  Result := Z1/inv1;
end;
//------------------------------------------------------------------------
function PlanckVisIntegral(const T:Real):Real;
var lambada:Real;
    cresult,stotal:Real;
    I,nloops:Integer;
begin
    // we set up the initial values
    nloops := vishigh - vislow;
    stotal := 0;
    lambada := vislow + 0.5;
    lambada := lambada*nm;
    // the loop
    for I := 0 to nloops - 1 do begin
      cresult := PlanckBlackbody(lambada,T);
      cresult := cresult*nm;
      stotal := stotal + cresult;
      lambada := lambada + nm;
    end;
    // done!
    Result := stotal;
end;
//-------------------------------------------------------------------
function PlanckVBandIntegral(const T:Real):Real;
var wavelength,startwave,lambada:Real;
    cslope,coffset,transm:Real;
    oloop1,oloop2,iloop1,iloop2:Integer;
    cresult,stotal:Real;
begin
  // initial values
  wavelength := 465;
  startwave := wavelength;
  coffset := 0;
  stotal := 0;
  for oloop1 := 0 to High(jv_tcsA) do begin
    cslope := jv_tcsA[oloop1];
    for iloop1 := 0 to 4 do begin
      transm := iloop1*cslope + coffset;
      // calculating the wavelength
      wavelength := startwave + iloop1;
      lambada := wavelength*nm;
      // the planck value
      cresult := PlanckBlackbody(lambada,T);
      cresult := (transm/100)*cresult*nm;
      stotal := stotal + cresult;
    end;
    // preparing for the next loop
    startwave += 5;
    coffset := 5*cslope + coffset;
  end;
  for oloop2 := 0 to High(jv_tcsB) do begin
    cslope := jv_tcsB[oloop2];
    for iloop2 := 0 to 9 do begin
      transm := iloop2*cslope + coffset;
      // calculating the wavelength
      wavelength := startwave + iloop2;
      lambada := wavelength*nm;
      // the planck value
      cresult := PlanckBlackbody(lambada,T);
      cresult := (transm/100)*cresult*nm;
      stotal := stotal + cresult;
    end;
    // preparing for the next loop
    startwave += 10;
    coffset := 10*cslope + coffset;
  end;
  Result := stotal;
end;
//-----------------------------------------------------------
function BlackBodyExtraBCv(const inTEff:Real):Real;
var alpha,boltzt,core:Real;
begin
  alpha := PlanckVBandIntegral(inTEff);
  boltzt := StefanBoltzmann(inTeff);
  core := boltzt / (alpha*sunbol_mul);
  Result := -2.5*log10(core);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// msic extras
function MakeAbsoluteMagnitude(const inmag,pllx:Real):Real;
var log_parsec:Real;
begin
  Assert(pllx>0);
  log_parsec := Log10(1000.0/pllx);
  Result := inmag - 5*(log_parsec-1);
end;
//-------------------------------------------------------------------
// Many Color to TEff estimation methods use the exact same formula...
function TEffMaker(const color:Real; const coffs:array of Real; const fehin:Real):Real;
var interm:Real;
begin
  interm := AltBoloMaker(color,coffs,fehin);
  Result := 5040 / interm;
end;
//--------------------------------------------------------------
// Casagrande and Hernández-Bonifacio use the same method of flux estimation
function BoloMaker(const color:Real; const coffs:array of Real; const fehin:Real):Real;
var interm:Real;
begin
  Assert(Length(coffs) = 7);
  interm := coffs[0] + color*coffs[1] + Sqr(color)*coffs[2];
  interm += coffs[3]*IntPower(color,3);
  if fehin <> 0 then begin
    interm += fehin*color*coffs[4];
    interm += coffs[5]*fehin + Sqr(fehin)*coffs[6];
  end;
  Result := interm;
end;
//---------------------------------------------------------------
function AltBoloMaker(const xparam:Real; const coffs:array of Real; const fehin:Real):Real;
var interm:Real;
begin
  Assert(Length(coffs) = 6);
  interm := coffs[0] + xparam*coffs[1] + Sqr(xparam)*coffs[2];
  if fehin <> 0 then begin
    interm += fehin*xparam*coffs[3];
    interm += coffs[4]*fehin + Sqr(fehin)*coffs[5];
  end;
  Result := interm;
end;
//---------------------------------------------------------------
function AdjFeh(const fehin:Currency; const fmin,fmax:Real):Real;
const fehrej:Currency = 0.7;
begin
  Assert(fmin<fmax);
  Result := fehin;
  if fehin >= fehrej then Result := 0
  else if fehin < fmin then Result := fmin
  else if fehin > fmax then Result := fmax;
end;
//***********************************************************************
begin
  planckSunInt  :=   PlanckVisIntegral(SUN_TEff);
  VplanckSunInt := PlanckVBandIntegral(SUN_TEff);
  boltz_sun := StefanBoltzmann(SUN_TEff);
  sunbol_mul := boltz_sun/VplanckSunInt;
  bolexp := exp10(0.4*SUN_BoMag);
end.

