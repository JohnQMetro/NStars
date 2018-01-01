unit TEffBCv;

{$mode delphi}

interface

uses
  Classes, SysUtils, fluxtransform, DAMath, starconstants, EstBasics, Utilities;
//******************************************************************************
const
  // offsets for converting Casagrande Bolo values to bolometric magnitudes (or corrections)
  CasaBO_VmK = 0.974511;
  CasaBO_VmI = 0.9661;
  CasaBO_BmV = 0.98472324;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
var
  CasaBoloM_VmK:Real;
  CasaBoloM_VmI:Real;
  CasaBoloM_BmV:Real;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Casagrande 2009 functions
function CasagrandeTEffEstK(const VmKin:Real; const fehin:Currency; out Teffout:Real):Boolean;
function CasagrandeTEffEstI(const VmIin:Real; const fehin:Currency; out Teffout:Real):Boolean;
function CasagrandeTEffEstB(const BmVin:Real; const fehin:Currency; out Teffout:Real):Boolean;
function CasagrandeBoloEstK(const VmKin:Real; const fehin:Currency; out BoloOut:Real):Boolean;
function CasagrandeBoloEstI(const VmIin:Real; const fehin:Currency; out BoloOut:Real):Boolean;
function CasagrandeBoloEstB(const BmVin:Real; const fehin:Currency; out BoloOut:Real):Boolean;
function CasagrandeBoloToBcV(const BoloIn,CasaBO:Real):Real;
// Casagrande M Dwarf functions
function CasagradeM_TEffEstK(const VmKin:Real; out TEffout:Real):Boolean;
function CasagradeM_TEffEstJ(const VmJin:Real; out TEffout:Real):Boolean;
// Alonso+ Giant Temp and BCv
function AlonsoGiantTeff(const VmKin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function AlonsoGiantTEffVmI(const VmIin:Real; const fehin:Currency; out TEffout:Real):Boolean;
function AlonsoGiantBCv(const Tempin:Real; const fehin:Currency; out BCvout:Real):Boolean;
// 'How to Constrain Your M Dwarf' (Mann+ 2015) TEff and BCv for K7 to M7 [Fe/H]~0
function ConstrainMDwarfTeff(const vmj,jmh:Real; const fehin:Currency; out Teffout:Real):Boolean;
function ConstrainMDwarfBCv(const VmJ:Real; fehin:Currency; out BCv:Real):Boolean;
function ConstrainMDwarfMass(const absKsmag:Real; out massest:Real):Boolean;
// Ultracool Bolometric Luminosity as a function of absolute H or K magnitudes
function DupuyLiuMagToLum(const AbsMag:Real; const isHmag:Boolean; out resLum:Real):Boolean;
// test output functions
procedure WriteKMTest;
procedure WriteBlackbodyBCv;
//******************************************************************************
implementation
//==========================================================================
function CasagrandeTEffEstK(const VmKin:Real; const fehin:Currency; out Teffout:Real):Boolean;
var fehreal:Real;
const coff1:array[0..5] of Real = (0.5057,0.2600,-0.0146,-0.0131,0.0288,0.0016);
begin
  // reject tests
  Result := False;
  if VmKin < 0.78 then Exit;
  if VmKin > 3.15 then Exit;
  fehreal := AdjFeh(fehin,-3.5,0.4);
  // The result
  Teffout := TEffMaker(VmKin,coff1,fehreal);
  Result := True;
end;
//----------------------------------------------------
function CasagrandeTEffEstI(const VmIin:Real; const fehin:Currency; out Teffout:Real):Boolean;
var fehreal:Real;
const coff1:array[0..5] of Real = (0.4033,0.8171,-0.1987,-0.0409,0.0319,0.0012);
begin
  // reject tests
  Result := False;
  if VmIin < 0.46 then Exit;
  if VmIin > 1.47 then Exit;
  fehreal := AdjFeh(fehin,-3.5,0.3);
  // The result
  Teffout := TEffMaker(VmIin,coff1,fehreal);
  Result := True;
end;
//-----------------------------------------------------------
function CasagrandeTEffEstB(const BmVin:Real; const fehin:Currency; out Teffout:Real):Boolean;
var fehreal:Real;
const coff1:array[0..5] of Real = (0.5665,0.4809,-0.006,-0.0613,-0.0042,-0.0055);
begin
  // reject tests
  Result := False;
  if BmVin < 0.275 then Exit;
  if BmVin > 1.29 then Exit;
  fehreal := AdjFeh(fehin,-3.5,0.4);
  // The result
  Teffout := TEffMaker(BmVin,coff1,fehreal);
  Result := True;
end;
//-------------------------------------------------
function CasagrandeBoloEstK(const VmKin:Real; const fehin:Currency; out BoloOut:Real):Boolean;
var fehreal:Real;
const coff1:array[0..6] of Real = (1.7662,1.4154,-0.9302,0.2726,0.0692,-0.2506,-0.0160);
begin
  // reject tests
  Result := False;
  if VmKin < 0.93 then Exit;
  if VmKin > 3.15 then Exit;
  fehreal := AdjFeh(fehin,-3.5,0.4);
  // The result
  BoloOut := BoloMaker(VmKin,coff1,fehreal);
  Result := True;
end;
//-------------------------------------------------
function CasagrandeBoloEstI(const VmIin:Real; const fehin:Currency; out BoloOut:Real):Boolean;
var fehreal:Real;
const coff1:array[0..6] of Real = (3.4468,-3.8760,4.5692,-0.7285,0.1832,-0.2991,-0.0231);
begin
  // reject tests
  Result := False;
  if VmIin < 0.48 then Exit;
  if VmIin > 1.47 then Exit;
  fehreal := AdjFeh(fehin,-3.5,0.3);
  // The result
  BoloOut := BoloMaker(VmIin,coff1,fehreal);
  Result := True;
end;
//----------------------------------------------------------------
function CasagrandeBoloEstB(const BmVin:Real; const fehin:Currency; out BoloOut:Real):Boolean;
var fehreal:Real;
const coff1:array[0..6] of Real = (1.2581,5.8828,-9.9287,6.8432,0.2290,-0.3935,-0.042);
begin
  // reject tests
  Result := False;
  if BmVin < 0.3 then Exit;
  if BmVin > 1.03 then Exit;
  fehreal := AdjFeh(fehin,-3.5,0.4);
  // The result
  BoloOut := BoloMaker(BmVin,coff1,fehreal);
  Result := True;
end;
//--------------------------------------------------------------------
function CasagrandeBoloToBcV(const BoloIn,CasaBO:Real):Real;
begin
  Result := CasaBO - 2.5*Log10(BoloIn);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Casagrande M Dwarf functions
//---------------------------------
function CasagradeM_TEffEstK(const VmKin:Real; out TEffout:Real):Boolean;
var inteff:Real;
const coff1:array[0..3] of Real = (-0.4809,0.8009,-0.1039,0.0056);
begin
  Result := False;
  if VmKin < 3.219 then Exit;
  if VmKin > 8.468 then Exit;
  // going ahead
  inteff := PolEval(VmKin,coff1,4);
  Result := True;
  TEffout := 5040/inteff;
end;
//---------------------------------------------------
function CasagradeM_TEffEstJ(const VmJin:Real; out TEffout:Real):Boolean;
var inteff:Real;
const coff1:array[0..3] of Real = (0.1926,0.5738,-0.0726,0.0042);
begin
  Result := False;
  if VmJin < 2.26 then Exit;
  if VmJin > 7.231 then Exit;
  // going ahead
  inteff := PolEval(VmJin,coff1,4);
  Result := True;
  TEffout := 5040/inteff;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ~Late G/Early-Mid K Giant TEffs and Bolometric Corrections from Alonso+ 1999.
//-------------------------------------------------
function AlonsoGiantTeff(const VmKin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var VmKt,result1,result2,fehr:Real;
const coffA:array[0..5] of Real = (0.5558,0.2105,1.981E-3,-9.965e-3,1.325E-2,-2.726E-3);
      coffB:array[0..5] of Real = (0.3770,0.3660,-3.170E-2,-3.074e-3,-2.765E-3,-2.973E-3);
      feha:Currency = 0.2;
      fehb:Currency = -0.5;
      fehc:Currency = -1.5;
      fehd:Currency = -2.5;
      fehe:Currency = -3.0;
begin
  // reject conditions
  Result := False;
  // metallicity limits
  if fehin > feha then Exit;
  if fehin < fehe then Exit;
  // input is assumed to be V-Ks, converting to V-K_TCS
  VmKt := VmKin + 0.069;
  // V-K limits depend on metallicity
  if fehin < fehd then begin
    if VmKt < 1.7 then Exit;
    if VmKt > 2.8 then Exit;
  end else if fehin < fehc then begin
    if VmKt < 1.2 then Exit;
    if VmKt > 3.4 then Exit;
  end else if fehin< fehb then begin
    if VmKt < 1 then Exit;
    if VmKt > 3.6 then Exit
  end else begin
    if VmKt < 0.2 then Exit;
    if VmKt > 4.9 then Exit;
  end;
  // going ahead
  Result := True;
  fehr := fehin;
  result2 := 0;
  // Alonso provides 2 equations with overlapping ranges!
  if VmKin < 2 then begin
    result1 := TEffMaker(VmKt,coffA,fehr);
  end else if VmKin < 2.5 then begin
    result1 := TEffMaker(VmKt,coffA,fehr);
    result2 := TEffMaker(VmKt,coffB,fehr);
  end else begin
    result1 := TEffMaker(VmKt,coffB,fehr);
  end;
  // finishing
  if result2 > 0 then TEffout := (result1+result2)/2
  else TEffout := result1;
end;
//-------------------------------------------------
function AlonsoGiantTEffVmI(const VmIin:Real; const fehin:Currency; out TEffout:Real):Boolean;
var vmij,result1:Real;
const coff:array[0..3] of Real = (0.5379,0.3981,4.432E-2,-2.693E-2);
      feha:Currency = 0.2;
      fehb:Currency = -0.5;
      fehc:Currency = -1.5;
      fehd:Currency = -2.5;
      fehe:Currency = -3.0;
begin
  // reject conditions
  Result := False;
  // metallicity limits
  if fehin > feha then Exit;
  if fehin < fehe then Exit;
  // converting V-Ic to V-Ij
  vmij := (VmIin - 0.014) / 0.779;
  // V-Ij limits depend on metallicity
  if fehin < fehd then begin
    if vmij < 1.0 then Exit;
    if vmij > 1.7 then Exit;
  end else if fehin < fehc then begin
    if vmij < 0.85 then Exit;
    if vmij > 2.2 then Exit;
  end else if fehin< fehb then begin
    if vmij < 0.8 then Exit;
    if vmij > 2 then Exit
  end else begin
    if vmij < 0.2 then Exit;
    if vmij > 2.9 then Exit;
  end;
  // going ahead
  Result := True;
  result1 := PolEval(vmij,coff,4);
  // finishing
  TEffout := 5040/result1;
end;
//-------------------------------------------------
function AlonsoGiantBCv(const Tempin:Real; const fehin:Currency; out BCvout:Real):Boolean;
var ltemp,xltemp:Real;
    fehr,int1_A,int1_B:Real;
    result1,result2:Real;
const coffA:array[0..5] of Real = (-0.6177,4.42,-2.669,0.6943,-0.1071,-8.612E-3);
      coffB:array[0..5] of Real = (2.887E-2,2.275,-4.425,0.3505,-5.558E-2,-5.375E-3);
      feha:Currency = 0.2;
      fehb:Currency = -0.5;
      fehc:Currency = -1.5;
      fehd:Currency = -2.5;
      fehe:Currency = -3.0;
begin
  // reject conditions
  Result := False;
  if Tempin <=0 then Exit;
  ltemp := log10(Tempin);
  // metallicity limits
  if fehin > feha then Exit;
  if fehin < fehe then Exit;
  // Log10(Teff) limits depend on metallicity
  if fehin < fehd then begin
    if ltemp < 3.61 then Exit;
    if ltemp > 3.74 then Exit;
  end else if fehin < fehc then begin
    if ltemp < 3.58 then Exit;
    if ltemp > 3.8 then Exit;
  end else if fehin< fehb then begin
    if ltemp < 3.56 then Exit;
    if ltemp > 3.83 then Exit
  end else begin
    if ltemp < 3.5 then Exit;
    if ltemp > 3.96 then Exit;
  end;
  // going ahead
  Result := True;
  result2 := 999;
  xltemp := ltemp - 3.52;
  fehr := CurrToReal(fehin);
  // Alonso provides 2 equations with overlapping ranges!
  if ltemp < 3.65 then begin
    int1_A := -5.531e-2/xltemp;
    result1 := int1_A + AltBoloMaker(xltemp,coffA,fehr);
  end else if ltemp < 3.67 then begin
    int1_A := -5.531e-2/xltemp;
    result1 := int1_A + AltBoloMaker(xltemp,coffA,fehr);
    int1_B := -9.930e-2/xltemp;
    result2 := int1_B + AltBoloMaker(xltemp,coffB,fehr);
  end else begin
    int1_B := -9.930e-2/xltemp;
    result1 := int1_B + AltBoloMaker(xltemp,coffB,fehr);
  end;
  // finishing
  if result2 < 900 then BCvout := (result1+result2)/2
  else BCvout := result1;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// 'How to Constrain Your M Dwarf' (Mann+ 2015) TEff and BCv for K7 to M7 [Fe/H]~0
//----------------------------------------------------------
// temperature estimates from 'How to contain your M Dwarf' (Mann+ 2015)
function ConstrainMDwarfTeff(const vmj,jmh:Real; const fehin:Currency; out Teffout:Real):Boolean;
var intres:Real;
const coff1:array[0..4] of Real = (2.840,-1.3453,0.3906,-0.0546,0.002913);
      coff2:array[0..4] of Real = (2.515,-1.054,0.2965,-0.04150,0.002245);
      coff3:array[0..4] of Real = (2.769,-1.421,0.4284,-0.06133,0.003310);
      coffj:array[0..2] of Real = (0,0.1333,0.05416);
begin
  Result := False;
  // reject conditions
  if vmj < 2.6 then Exit;
  if vmj > 6.48 then Exit;    // supposed to be 7, but ...
  if fehin < -1 then Exit;
  // 3 cases...
  // Fe/H is good
  if (fehin < 0.5) then begin
    intres := PolEval(vmj,coff2,5) + 0.05262*CurrToReal(fehin);
  end
  // jmh is good
  else if jmh < 2 then begin
    intres := PolEval(vmj,coff3,5) + PolEval(jmh,coffj,3);
  end
  // we have neither
  else intres := PolEval(vmj,coff1,5);
  // finally
  Teffout := intres*3500;
  Result := True;
end;
//---------------------------------------------------------------
function ConstrainMDwarfBCv(const VmJ:Real; fehin:Currency; out BCv:Real):Boolean;
var intres:Real;
const coff1:array[0..3] of Real = (0.5817,-0.4168,-0.08165,4.084E-3);
      coff2:array[0..3] of Real = (0.6570,-0.4710,-0.06943,3.206E-3);
begin
  // reject conditions
  Result := False;
  if VmJ < 2.6 then Exit;
  if VmJ > 7 then Exit;
  if fehin < -1 then Exit;
  // 2 cases
  // Fe/H is good
  if fehin < 0.5 then begin
    intres := PolEval(VmJ,coff2,4) +  -0.04885*CurrToReal(fehin);
  end
  // we don't have fe/h
  else intres := PolEval(VmJ,coff1,4);
  // finally
  BCv := intres;
  Result := True;
end;
//----------------------------------------------------------
function ConstrainMDwarfMass(const absKsmag:Real; out massest:Real):Boolean;
const coffs:array[0..4] of Real = (0.5858,0.3872,-0.1217,0.0106,-2.7262e-4);
begin
  Result := False;
  if absKsmag <= 4.6 then Exit;
  if absKsMag >= 9.8 then Exit;
  Result := True;
  massest := PolEval(absKsMag,coffs,5);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function DupuyLiuMagToLum(const AbsMag:Real; const isHmag:Boolean; out resLum:Real):Boolean;
var loglum:Real;
const coff_k:array[0..5] of Real = (-59.877,22.58776,-3.364101,0.2357643,-0.00785837,0.000098821);
      coff_h:array[0..3] of Real = (-10.426,2.74259,-0.290797,0.0089222);
begin
  Result := False;
  // reject conditions
  if isHmag then begin
   if AbsMag < 9.2 then Exit;
   if AbsMag > 13.3 then Exit;
  end else begin
   if AbsMag < 8.8 then Exit;
   if AbsMag > 16.6 then Exit;
  end;
  // computing
  if isHmag then loglum := PolEval(AbsMag,coff_h,4)
  else loglum := PolEval(AbsMag,coff_k,6);
  // final...
  Result := True;
  resLum := exp10(loglum);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// test functions
//-------------------------------------------------
// making a test file for Teff and BCv (KM dwarf range)
procedure WriteKMTest;
var  mdtestfile:TextFile;
     sptdex1,sptdex2,sptdex3,sptdex4:Integer;
     idex1:Integer;
     teff0,teff1,teff2,teff3:Real;
     creal,cvmj:Real;
     outline:string;
     isok:Boolean;
     vmksplit,xdiff:Real;
     intvmk,tempc,bco,bcox:Real;
const splitr = 3;
begin
    // starting the file...
    AssignFile(mdtestfile,'mtestdata.csv');
    Rewrite(mdtestfile);
    Writeln(mdtestfile,'SpT,BcV,V-K,T Teff,V-K Teff,V-J,C Teff 1,EPD 1,CTeff 2,EPD 2');
    // k range
    for sptdex1 := 0 to 9 do begin
      // building the comma delimited list of values
      outline := 'K' + IntToStr(sptdex1) + ',';
      outline += Trim(FloatToStrF(BCorTable[6][sptdex1],ffFixed,6,3)) + ',';
      outline += Trim(FloatToStrF(StarVmKs[6][sptdex1],ffFixed,6,3)) + ',';
      teff0 := TempTable[6][sptdex1];
      outline += Trim(FloatToStrF(teff0,ffFixed,4,0)) + ',';
      isok := VmK_TeffEst(StarVmKs[6][sptdex1],teff1);
      outline += Trim(FloatToStrF(teff1,ffFixed,4,0)) + ',';
      cvmj :=  KVmJ[sptdex1];
      outline += Trim(FloatToStrF(cvmj,ffFixed,6,3)) + ',';
      // constrained M Dwarf estimates
      isok := ConstrainMDwarfTeff(cvmj,99,0,teff2);
      if not isok then outline += ',,'
      else begin
        outline += Trim(FloatToStrF(teff2,ffFixed,4,0)) + ',';
        creal := MakeEPD(teff2,teff0);
        outline += Trim(FloatToStrF(creal,ffFixed,5,1)) + ',';
      end;
      isok := ConstrainMDwarfTeff(cvmj,99,99,teff3);
      if not isok then outline += ',,'
      else begin
        outline += Trim(FloatToStrF(teff3,ffFixed,4,0)) + ',';
        creal := MakeEPD(teff3,teff0);
        outline += Trim(FloatToStrF(creal,ffFixed,5,1)) + ',';
      end;
      isok := ConstrainMDwarfBCv(cvmj,0,bco);
      if isok then begin
        outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
        xdiff := Abs(1-exp10(0.4*(BCorTable[6][sptdex1]-bco)));
        outline += Trim(FloatToStrF(100*xdiff,ffFixed,7,2)) + ',';
      end else outline += ',';
      // writing
      WriteLn(mdtestfile,outline);
    end;
    // M Range
    for sptdex2 := 0 to 19 do begin
      // building the comma delimited list of values
      outline := 'M' + Trim(FloatToStrF(sptdex2/2,ffFixed,3,1)) + ',';
      if altMSeq then bco := MBCorAlt[sptdex2]
      else bco := MBCorM[sptdex2];
      outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
      outline += Trim(FloatToStrF(RedDwarfVmKs[sptdex2],ffFixed,6,3)) + ',';
      teff0 := GetMTemp(sptdex2);
      outline += Trim(FloatToStrF(teff0,ffFixed,4,0)) + ',';
      isok := VmK_TeffEst(RedDwarfVmKs[sptdex2],teff1);
      outline += Trim(FloatToStrF(teff1,ffFixed,4,0)) + ',';
      cvmj :=  MVmJ[sptdex2];
      outline += Trim(FloatToStrF(cvmj,ffFixed,6,3)) + ',';
      // constrained M Dwarf estimates
      isok := ConstrainMDwarfTeff(cvmj,99,0,teff2);
      if not isok then outline += ',,'
      else begin
        outline += Trim(FloatToStrF(teff2,ffFixed,4,0)) + ',';
        creal := MakeEPD(teff2,teff0);
        outline += Trim(FloatToStrF(creal,ffFixed,5,1)) + ',';
      end;
      isok := ConstrainMDwarfTeff(cvmj,99,99,teff3);
      if not isok then outline += ',,'
      else begin
        outline += Trim(FloatToStrF(teff3,ffFixed,4,0)) + ',';
        creal := MakeEPD(teff3,teff0);
        outline += Trim(FloatToStrF(creal,ffFixed,5,1)) + ',';
      end;
      isok := ConstrainMDwarfBCv(cvmj,0,bco);
      if isok then begin
        if altMSeq then bcox := MBCorAlt[sptdex2]
        else bcox := MBCorM[sptdex2];
        outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
        xdiff := Abs(1-exp10(0.4*(bcox-bco)));
        outline += Trim(FloatToStrF(100*xdiff,ffFixed,7,2)) + ',';
      end else outline += ',';
      // writing
      WriteLn(mdtestfile,outline);
    end;
    // finally, a separate K test involving differences
    for sptdex3 := 0 to 9 do begin
      if (sptdex3<>9) then vmksplit := (StarVmKs[6][sptdex3]-StarVmKs[6][sptdex3+1])/splitr
      else vmksplit := (StarVmKs[6][9]-RedDwarfVmKs[0])/splitr;
      for idex1 := 0 to (splitr-1) do begin
        // building the comma delimited list of values
        if (idex1 = 0) then outline := 'K' + IntToStr(sptdex3) + ','
        else outline := ',';
        outline += Trim(FloatToStrF(BCorTable[6][sptdex3],ffFixed,6,3)) + ',';
        intVmk := CurrToReal(StarVmKs[6][sptdex3]) - idex1*vmksplit;
        outline += Trim(FloatToStrF(intVmk,ffFixed,6,3)) + ',';
        isok := Inter_GKML_TeffEst(intVmk,teff3);
        if isok then begin
          outline += Trim(FloatToStrF(teff3,ffFixed,4,0))
        end;
        outline += ',';
        isok := VmK_TeffEst(intVmk,tempc);
        outline += Trim(FloatToStrF(tempc,ffFixed,4,0)) + ',';
        isok := TeffToBCv(teff3,useBCvLog,bco);
        if isok then begin
          outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
        end else outline += ',';
        // constrained M Dwarf estimates
        // bcolometric correction...
        isok := TeffToBCv(tempc,useBCvLog,bco);
        if isok then begin
          outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
        end else outline += ',';
        // extra teff
        outline += ',,';
        // writing
        WriteLn(mdtestfile,outline);
      end;
    end;

    // finally, a separate M test involving differences
    for sptdex4 := 0 to 19 do begin
      if (sptdex4<>19) then vmksplit := (RedDwarfVmKs[sptdex4]-RedDwarfVmKs[sptdex4+1])/splitr
      else vmksplit := (RedDwarfVmKs[19]-LDwarfVmKs[0])/splitr;
      for idex1 := 0 to (splitr-1) do begin
        // building the comma delimited list of values
        if (idex1 = 0) then outline := 'M' + Trim(FloatToStrF(sptdex4/2,ffFixed,3,1)) + ','
        else outline := ',';
        if altMSeq then bco := MBCorAlt[sptdex4]
        else bco := MBCorM[sptdex4];
        outline += Trim(FloatToStrF(bcox,ffFixed,6,3)) + ',';
        intVmk := CurrToReal(RedDwarfVmKs[sptdex4]) - idex1*vmksplit;
        outline += Trim(FloatToStrF(intVmk,ffFixed,6,3)) + ',';
        isok := Inter_GKML_TeffEst(intVmk,teff3);
        if isok then begin
          outline += Trim(FloatToStrF(teff3,ffFixed,4,0))
        end;
        outline += ',';
        isok := VmK_TeffEst(intVmk,tempc);
        outline += Trim(FloatToStrF(tempc,ffFixed,4,0)) + ',';
        isok := TeffToBCv(teff3,useBCvLog,bco);
        if isok then begin
          outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
        end else outline += ',';
        // constrained M Dwarf estimates
        // bcolometric correction...
        isok := TeffToBCv(tempc,useBCvLog,bco);
        if isok then begin
          outline += Trim(FloatToStrF(bco,ffFixed,6,3)) + ',';
        end else outline += ',';
        // extra teff
        outline += ',,';
        // writing
        WriteLn(mdtestfile,outline);
      end;
    end;

    // finishing off the file
    Flush(mdtestfile);
    CloseFile(mdtestfile);
end;
//-----------------------------------------------------
procedure WriteBlackbodyBCv;
var  mdtestfile:TextFile;
     sptdex1,sptdex2:Integer;
     outline:string;
     teffx,calcBcv:Real;
begin
    // starting the file...
    AssignFile(mdtestfile,'calcbcv.csv');
    Rewrite(mdtestfile);
    Writeln(mdtestfile,'SpT,TEff,Calc BCv');
    for sptdex1 := 1 to 4 do begin
      for sptdex2 := 0 to 9 do begin
        outline := HR_Let[sptdex1] + IntToStr(sptdex2) + ',';
        teffx := TempTable[sptdex1][sptdex2];
        outline += FloatToStrF(teffx,ffFixed,5,0) + ',';
        calcBcv := SUN_BCv + BlackBodyExtraBCv(teffx);
        outline += FloatToStrF(calcBcv,ffFixed,6,3) + ',';
        // writing
        WriteLn(mdtestfile,outline);
      end;
    end;

    // finishing off the file
    Flush(mdtestfile);
    CloseFile(mdtestfile);
end;
//******************************************************************************
begin
  CasaBoloM_VmK := exp10(0.4*(SUN_BCv-CasaBO_VmK));
  CasaBoloM_VmI := exp10(0.4*(SUN_BCv-CasaBO_VmI));
  CasaBoloM_BmV := exp10(0.4*(SUN_BCv-CasaBO_BmV));

  // WriteKMTest();
  // WriteBlackbodyBCv();
end.

