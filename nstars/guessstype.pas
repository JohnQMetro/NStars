unit guessstype;

{$mode delphi}

interface

uses
  Classes, SysUtils, Utilities, sptfluxest, DAMath,StarExt2, StrUtils;

type
(* type to bundle and streamline spectral type guessing stuff *)
SpecTypeGuesser = class
  protected
    // fluxes
    vmag,avmag,ksmag:Real;
    akmag_min,akmag_max:Currency;
    bmv,bmv_min,bmv_max:Currency;
    vmr:Currency;
    vmi,vmk,vmk_min,vmk_max:Currency;
    vmj:Real;
    // typedata
    is_pms:Boolean;
    // results
    r_magv,r_magk,r_bmv,r_vmr,r_vmi,r_vmj,r_vmk:string;
    ok_magv,ok_magk,ok_bmv,ok_vmr,ok_vmi,ok_vmj,ok_vmk:Boolean;
    has_rec,has_dist,has_dist2:Boolean;
    rec_stype,vmk_dist,vmk_dist2,jmk_note:string;
    // helper methods
    function MagnitudeSpectra:Boolean;
    function ColorSpectras:Integer;
    function ColorSpectrasPMS:Integer;
    function RecommendSType:Boolean;
    function VmKEstimateDistance:Boolean;
    function UCAC4_VmKDistance:Boolean;
    function MakeMagResult:string;
    function MakeColourResult:string;
    // property methods
    function GHR:Boolean;
    function GRecStr:string;
    function GHVmKDist:Boolean;
    function GVmKDist:string;
  public
    // properties
    property HasRecommendation:Boolean read GHR;
    property Recommendation:string read GRecStr;
    property HasVmKDistance:Boolean read GHVmKDist;
    property VmKDistance:string read GVmKDist;
    // public methods
    constructor Create;
    function SetFluxes(pllx:Real; invmag:Real; fluxes:StarFluxPlus):Boolean;
    function MakeGuesses(const curspec:string; is_pms_in:Boolean):Boolean;
    function ProduceResult(const stype_pllx:string; const vmagdiff:Real):string;
end;


implementation
//==============================================================================
// helper methods
//-------------------------------------------
function SpecTypeGuesser.MagnitudeSpectra:Boolean;
var ok1,ok2:Boolean;
    res1,res2:string;
begin
  if avmag < 90 then begin
    ok_magv := MagnitudeToType(avmag,r_magv);
  end else ok_magv := False;
  if akmag_min < 90 then begin
    ok1 := MKsLookupDual(akmag_min,akmag_max,False,res1);
    ok2 := MKsLookupDual(akmag_min,akmag_max,True,res2);
    // checking and setting the the results
    ok_magk := (ok1 or ok2);
    if ok_magk then begin
      if (ok1 and ok2) and (res1<>res2) then r_magk := res1 + '/' + res2
      else if ok1 then r_magk := res1
      else if ok2 then r_magk := res2;
    end;
  end
  else ok_magk := False;
  // done
  Result := ok_magv or ok_magk;
end;
//---------------------------------------------
function SpecTypeGuesser.ColorSpectras:Integer;
var ok1,ok2:Boolean;
    vmkg1,vmkg2:string;
begin
  Result := 0;

  // B-V
  if bmv < 90 then begin
    r_bmv := BminVDual(bmv_min,bmv_max);
    ok_bmv := (Length(r_bmv)>0);
    if ok_bmv then Inc(Result);
  end;
  // V-R
  if vmr < 90 then begin
    ok_vmr := VminRLookup(vmr,r_vmr);
    if ok_vmr then Inc(result);
  end;
  // V-I
  if vmi < 90 then begin
    ok_vmi := VminILookup(vmi,r_vmi);
    if ok_vmi then Inc(Result);
  end;
  // V-J
  if vmj < 90 then begin

  end;
  // V - K
  if vmk < 90 then begin
    // looking up, assuming K is Ks, or otherwise
    ok1 := VminKsDual(vmk_min,vmk_max,False,vmkg1);
    ok2 := VminKsDual(vmk_min,vmk_max,False,vmkg2);
    // checking and setting the the results
    if (ok1 and ok2) and (vmkg1<>vmkg2) then r_vmk := vmkg1 + '/' + vmkg2
    else if ok1 then r_vmk := vmkg1
    else if ok2 then r_vmk := vmkg2;
    ok_vmk := (ok1 or ok2);
    if ok_vmk then Inc(Result);
  end;

end;
//---------------------------------------------
function SpecTypeGuesser.ColorSpectrasPMS:Integer;
begin
  Result := 0;
  // B-V
  if bmv < 90 then begin
    r_bmv := PMS_BminVDual(bmv_min,bmv_max);
    ok_bmv := (Length(r_bmv)>0);
    if ok_bmv then Inc(Result);
  end;
  // V-I
  if vmi < 90 then begin
    ok_vmi := PMS_VminILookup(vmi,r_vmi);
    if ok_vmi then Inc(Result);
  end;
  // V-J
  if vmj < 90 then begin

  end;
    // V-K
  if vmk < 90 then begin
    ok_vmk := PMS_VminKsDual(vmk_min,vmk_max,r_vmk);
    if ok_vmk then Inc(Result);
  end;

end;
//-------------------------------------------------------
function SpecTypeGuesser.RecommendSType:Boolean;
const midred_vmk:Currency = 3.3;
begin
  Result := False;
  has_rec := False;
  if (vmk < 90) and (vmk >= midred_vmk) then begin
    // V-K based spectral type (late K, M, L )
    if (Length(r_vmk)<=5) and ok_vmk then begin
      has_rec := True;
      rec_stype := r_vmk;
    end else begin
      has_rec := VminKsLookup(vmk,False,rec_stype);
    end;
  end
  else if (bmv < 90) then begin
    // B-V based spectral type (early stars)
    if (Length(r_bmv)<=5) and ok_bmv then begin
      has_rec := True;
      rec_stype := r_bmv;
    end else begin
      rec_stype := BminVLookup(bmv);
      has_rec := (Length(rec_stype)>0);
    end;
  end;
  // done
  Result := has_rec;
end;
//---------------------------------------------
function SpecTypeGuesser.VmKEstimateDistance:Boolean;
var mvest:Real;
    mvok:Boolean;
begin
  has_dist := False;
  vmk_dist := '';
  if vmk < 90 then begin
    mvok := VmK_MagnitudeEst(CurrToReal(vmk),mvest);
    if mvok then has_dist := MagsToParallax(vmag,mvest,vmk_dist);
    Result := has_dist;
  end else Result := False;
end;
//-----------------------------------------------
function SpecTypeGuesser.UCAC4_VmKDistance:Boolean;
var mkest:Real;
const coff:array[0..2] of Real = (-2.472,2.523,-0.1239);
begin
  // startup and checking
  has_dist2 := False;
  Result := False;
  vmk_dist2 := '';
  if vmk >= 8 then Exit;
  if vmk < 4 then Exit;
  // caclulating
  mkest := PolEval(CurrToReal(vmk),coff,3);
  has_dist2 := MagsToParallax(ksmag,mkest,vmk_dist2);
  Result := has_dist2;
end;
//-----------------------------------------------
function SpecTypeGuesser.MakeMagResult:string;
begin
  Result := '';
  if (ok_magv) then Result += 'V Magnitude  : ' + r_magv + sLineBreak;
  if (ok_magk) then Result += 'Ks Magnitude : ' + r_magk + sLineBreak;
end;
//-----------------------------------------------
function SpecTypeGuesser.MakeColourResult:string;
begin
  Result := '';
  if (ok_vmk) then Result += 'V−K  : ' + r_vmk + sLineBreak;
  if (ok_bmv) then Result += 'B−V  : ' + r_bmv + sLineBreak;
  if (ok_vmr) then Result += 'V-Rc : ' + r_vmr + sLineBreak;
  if (ok_vmi) then Result += 'V−Ic : ' + r_vmi + sLineBreak;
  if (ok_vmj) then Result += 'V−J  : ' + r_vmj + sLineBreak;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// property methods
function SpecTypeGuesser.GHR:Boolean;
begin  Result := has_rec;    end;
function SpecTypeGuesser.GRecStr:string;
begin  Result := rec_stype;  end;
function SpecTypeGuesser.GHVmKDist:Boolean;
begin  Result := has_dist;    end;
function SpecTypeGuesser.GVmKDist:string;
begin  Result := vmk_dist;    end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor SpecTypeGuesser.Create;
begin
  vmag := 99;   avmag := 99;
  ksmag := 99;
  akmag_min := 99;  akmag_max := 99;
  bmv := 99;    bmv_min := 99;  bmv_max := 99;
  vmr := 99;    vmi := 99;      vmj := 99;
  vmk := 99;    vmk_min := 99;  vmk_max := 99;
  ok_magv := False;     ok_magk := False;
  ok_bmv  := False;     ok_vmi  := False;
  ok_vmj  := False;     ok_vmk  := False;
  has_rec := False;     is_pms := False;
  has_dist := False;    has_dist2 := False;
end;
//---------------------------------
function SpecTypeGuesser.SetFluxes(pllx:Real;  invmag:Real; fluxes:StarFluxPlus):Boolean;
var absoffset:Real;
    jmkval:Currency;
const jmklim1:Currency = 0.85;
      jmklim2:Currency = 1.02;
begin
  Result := False;
  if pllx <= 0 then Exit;
  // absolute magnitudes
  absoffset := 5*(log10(1000/pllx)-1);
  if invmag < 90 then begin
    avmag := invmag - absoffset;
    ok_magv := True;
    vmag := invmag;
  end
  else ok_magv := False;
  if fluxes <> nil then begin
    fluxes.AbsoluteKMag(pllx,akmag_min,akmag_max);
  end;
  // colors
  if (invmag < 90) and (fluxes<>nil) then begin
    if fluxes.Valid_BlueMagnitude then begin
      bmv := fluxes.blue_mag - RealToCurr(invmag);
      fluxes.BminusVval(invmag,bmv_min,bmv_max);
    end;
    fluxes.VminusRval(invmag,vmr);
    fluxes.VminusIval(invmag,vmi);
    if fluxes.Valid_JMagnitude then vmj := invmag - CurrToReal(fluxes.J_mag);
    if fluxes.Valid_KMagnitude then begin
      ksmag := CurrToReal(fluxes.K_mag);
      vmk := RealToCurr(invmag) - fluxes.K_mag;
      fluxes.VminusKval(invmag,vmk_min,vmk_max);
    end;

  end;
  // special J-K checking
  if (fluxes<>nil) then begin
    if fluxes.JminusKval(jmkval) then begin
      if jmkval > jmklim1 then begin
        jmk_note := fluxes.JminusK;
        if jmkval > jmklim2 then jmk_note += ' (Very High)'
        else jmk_note += ' (High)'
      end;
    end;
  end;
  // result (True if anything is okay)
  Result := ((avmag < 90) or (akmag_min<90));
  Result := Result or ((bmv<90) or (vmi<90) or (vmj<90) or (vmk<90));
end;
//---------------------------------
function SpecTypeGuesser.MakeGuesses(const curspec:string; is_pms_in:Boolean):Boolean;
var res1,res3,res4,res5:Boolean;
    spectr_res:Integer;
begin
  is_pms := is_pms_in;
  res1 := MagnitudeSpectra;
  if not is_pms then spectr_res := ColorSpectras
  else spectr_res := ColorSpectrasPMS;
  res3 := RecommendSType;
  if has_rec and (Length(curspec)>0) then begin
    if AnsiEndsStr('e',curspec) or AnsiEndsStr('eV',curspec) then begin
      rec_stype += 'e'; end
    else if AnsiEndsStr('n',curspec) or AnsiEndsStr('(n)',curspec) then begin
      rec_stype += 'n'; end
    else if AnsiEndsStr('m',curspec) or AnsiEndsStr('(m)',curspec) then begin
      rec_stype += 'm'; end
  end;

  if (not is_pms) then res4 := VmKEstimateDistance
  else res4 := False;
  if (not is_pms) then res5 := UCAC4_VmKDistance()
  else res5 := False;
  Result := res1 or (spectr_res>0) or res3 or res4 or res5;
end;
//---------------------------------
function SpecTypeGuesser.ProduceResult(const stype_pllx:string; const vmagdiff:Real):string;
var hasother:Boolean;
    diffstr:string;
begin
  // spectral type guesses for display
  if is_pms then begin
    Result := 'The Pre-Main-Sequence spectral' + sLineBreak;
    Result += 'type guesses are: ' + sLineBreak;
  end
  else Result := 'The spectral type guesses are: ' + sLineBreak;
  Result += MakeMagResult + MakeColourResult + sLineBreak;
  // other estimates (if any)
  hasother := (vmagdiff<90) or (Length(stype_pllx)>0);
  if not hasother then begin
    hasother := hasother or (Length(jmk_note)>0) or (HasVmkDistance);
  end;
  if hasother then begin
    Result += 'Other estimates : ' + sLineBreak;
    diffstr := Trim(FloatToStrF(vmagdiff,ffFixed,5,2));
    if (vmagdiff<90) then Result += 'ΔVexp : ' + diffstr + sLineBreak;
    if (Length(stype_pllx)>0) then Result += 'Crude Phot Pllx : ~' + stype_pllx + sLineBreak;
    if (HasVmkDistance) then Result += 'V−K Phot Pllx : ~' + VmKDistance + sLineBreak;
    if (Length(vmk_dist2)>0) then Result += 'UCAC4 NSS V-K Pllx : ~' + vmk_dist2 + sLineBreak;
    if (Length(jmk_note)>0) then Result += jmk_note;
  end;
  // recommended Spectral Type
  if HasRecommendation then begin
    Result += sLineBreak;
    Result += sLineBreak + 'RECOMMENDED : ' + Recommendation;
  end;

end;

//==============================================================================

end.

