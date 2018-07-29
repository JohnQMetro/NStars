unit StarEstimator;

{$mode delphi}

interface

uses
  Classes, SysUtils,StrUtils, DAMath, df_strings, fluxtransform,Utilities,
  StarExt2,EstUtilsExtra,starconstants,EstBasics,TEffBCv,WhiteDwarf;

const
  late_let = 'KMLTY';
  (* used for red dwarf mass estimation *)
  rdmin:Currency = 0.0595;
  rdmax:Currency = 0.6515;
  rdstep:Currency = 0.001;
  rdhstep:Currency = 0.0005;

  LuminosityClassName:array[0..16] of string = ('??','0','Ia','Iab','Ib','II',
             'III','III-IV','IV','IV-V','V','V-VI','VI','WD','esd','usd','PMS');


type
  // an enumerated error type
  ParseResult = (PS_NOERROR,PS_EMPTY,PS_PECULIAR,PS_MISSING,PS_UNRECOGNIZED,
                  PS_SUBRANGE,PS_NOLCLASS);

  // luminosity class enum
  LuminosityClassType = (XUnknown,lHyperGiant,lSuperGianta,lSuperGiantab,
             lSuperGiantb,lBrightGiant,lGiant,lLowGiant,lSubGiant,lDwarfGiant,
             lDwarf,lLowDwarf,lSubDwarf,lWhiteDwarf,lExtremeSubDwarf,
             lUltraSubDwarf,lPreMainSequence);

  (* the primary class: input spectral string and visual magnitude, out comes
  estimates... *)
  EstimationParser = class
    protected
      // inputs
      absVisMag:Real;
      absKMag:Real;
      satk:Boolean;
      absJMag:Real;
      absHMag:Real;
      absIMag:Real;
      absBMag:Real;
      stype:string;
      feh:Currency;
      logg:Currency;
      // backups
      bak_avmag,bak_akmag,bak_bmag,bak_jmag:Real;
      bak_stype:string;
      bak_tempA:Real;
      // extracted amounts
      parsed:Boolean;
      errtype:ParseResult;
      scolor:Char;
      xpart:string;
      subrange:Real;
      lclass:LuminosityClassType;
      // calculated amounts
      luminosity:Real;
      efftempA,efftempB,efftempC:Real;
      boltzA,boltzB,planckI:Real;
      bollum:Real;
      radiusest:Real;
      wdbloat:Real;
      massest:Real;
    // value property methods
      function GMag:Real;
      function GLum:Real;
      function GBLum:Real;
      function GREst:Real;
      function GMEst:Real;
      function GLClass:LuminosityClassType;
      function GCChar:Char;
      function GRDwarf:Boolean;
      function GSubrange:Real;
      function GIsParsed:Boolean;
      function GTF1:Real;
      function GTF2:Real;
    // string property methods
      function GLumStr:string;
      function GBLumStr:string;
      function GRadEstStr:string;
      function GMassEstStr:string;
      function GLumExport:string;
      function GBLumExport:string;
      function GRadiusExport:string;
      function GMassExport:string;
      function GSType:string;
      function GUTEff:string;
    // private helper parse methods
      function WhiteDwarfParse(wdspecin:string):Boolean;
      function HandleStartPart(var specstr:string):Boolean;
      function SubRangeHandle(var specstr:string; out subrout:Real):Boolean;
      function LumClassHandle(var specstr:string):Boolean;
      function MainParse(sstring:string; is_pms:Boolean):Boolean;
    // temperature methods
      function LookupDwarfTempA():Boolean;
      function LookupGiantTempA():Boolean;
      function LookupPMSTempA():Boolean;
      function SetTempA:Boolean;
      function SetTempB:Boolean;
      function ConstrainMDwarfTemp(out Teffout:Real):Boolean;
    // bolometric correction / luminosity methods
      function CalcDwarfBollumKML():Boolean;
      function CalcDwarfSGBollumBAFGK():Boolean;
      function CalcWhiteDwarfBollum():Boolean;
      function CalcBoloGiant():Boolean;
      function CalcBoloPMS():Boolean;
      function HasBolomCorr():Boolean;
      function IsLate():Boolean;
      function CalcBolometricLuminosity:Boolean;
    // radius estimation methods
      function BolometricTempRadius(out radius_est:Real):Boolean;
      function BlackbodyRadius(vfilter:Boolean; out radius_est:Real):Boolean;
    // mass estimation methods
      function DwarfBolLumToMass(out mass_est:Real):Boolean;
      function RedDwarfMass(out mass_est:Real):Boolean;
      function WhiteDwarfMass(out mass_est:Real):Boolean;
      function SubGiantMassGuesstimate(out mass_est:Real):Boolean;
      function SubGiantMassEstimateB(out mass_est:Real):Boolean;
    // other private methods
      procedure ClearMag;
      procedure ClearSpec;
      function VmOK(isj:Boolean):Boolean;
      // estimator methods
      function DualMagToStr(mag1,mag2:Real):string;
      function TEffToStr(const teff1:Real):String;
      function DualTEffToStr(teff1,teff2:Real):string;
      function MakeEPDStr(teff1,teff2:Real):string;
      function MakeInfoLineStart(wdi:Boolean; out bmag1,vmkteff:Real):string;
      function MakeInfoLineMDwarf(const bcor1:Real):string;
      function MakeInfoLineBB(const satkout:Boolean):string;
      function MakeInfoLineWhiteDwarf(const bcor1:Real):string;
      function MakeInfoLineOther(const calctemp,bmag1:Real):string;
      function MakeInfoLineGiant(bmag:Real):string;
    public
      //properties
      property AbsoluteVisMagnitude:Real read GMag;
      property VisualLuminosity:Real read GLum;
      property BoloLuminosityEst:Real read GBLum;
      property RadiusEstimate:Real read GREst;
      property MassEstimate:Real read GMEst;

      property LuminosityClass:LuminosityClassType read GLClass;
      property ColorLetter:Char read GCChar;
      property isRedDwarf:Boolean read GRDwarf;
      property ColorSubrange:Real read GSubrange;
      property SpectraOK:Boolean read GIsParsed;
      property StoredSpectra:string read GSType;

      property TEff1:Real read GTF1;
      property TEff2:Real read GTF2;

      property LuminosityString:string read GLumStr;
      property BoloLuminosityEstString:string read GBLumStr;
      property RadiusEstimateString:string read GRadEstStr;
      property MassEstimateString:string read GMassEstStr;

      property LuminosityExport:string read GLumExport;
      property BoloLuminosityEstExport:string read GBLumExport;
      property RadiusEstimateExport:string read GRadiusExport;
      property MassEstimateExport:string read GMassExport;

      property TEffUsed:string read GUTEff;

      // constructors
      constructor Create();
      // functions to set the data
      function SetBasic(const avm:Real; in_spectra:string; is_pms:Boolean):Boolean;
      function SetSimple(const avm,pllx:Real; in_spectra:string; is_pms:Boolean):Boolean;
      function SetSimpleOther(const avm,pllx:Real):Boolean;
      function SetAll(const avm,pllx:Real; in_spectra:string; is_pms:Boolean; other:StarFluxPlus):Boolean;
      function SetOther(const avm,pllx:Real; other:StarFluxPlus):Boolean;

      // calculation methods
      protected
      function CalcRadiusEst:Boolean;
      function GetRadiusEst(const typex:Boolean; out radest:Real):Boolean;
      function MakeAllRadiusEstimates:string;
      function CalcMassEst:Boolean;
      function GetMassEst(dwarfm:Boolean; out xmassest:Real):Boolean;
      function MakeEstimates:Boolean;
      public
      // extra output
      function GetMassRadiusBLuminosity:string;
      function IsLateDwarf:Boolean;
      function WriteTest(const inname:string):Boolean;
      // used when splitting binaries
      function DivideMagnitude(const divisor:Real; reest:Boolean):Real;
      procedure BackupMS;
      procedure RestoreMS(reestimate:Boolean);
      procedure DivideSplit(const divisor:Real; reestimate:Boolean);
      function SecondarySplit(secmag,pllx:Real; reestimate:Boolean):Boolean;
      function ChopJ():Boolean;
  end;

  (* --- Functions --- *)
  // additional functions
  function RedDwarfMassToMag(const inmass:Real; kmag:Boolean; out outmag:Real):Boolean;
  procedure FillInRedDwarfArrays;
  function RedDwarfMagnitudeToMassLookup(const inmag:Real; kmag:Boolean; out massest:Real):Boolean;
  function AlonsoMultiStr(const vmkin:Real):string;
  // estimator i/o
  function StartEstimateFiles():Boolean;
  function EndEstimateFiles():Boolean;

var

  rd_vmag:array of Real;
  rd_kmag:array of Real;


  cbolIsun:Real;
  cbolKsun:Real;

  examinfile,examinfile_m,examinfile_d,examinfile_g:TextFile;
  examinopen:Boolean;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

implementation
//=========================================================
// value property methods
//-------------------------------
function EstimationParser.GMag:Real;
begin   Result := absVisMag;    end;
//-------------------------------
function EstimationParser.GLum:Real;
begin   Result := luminosity;    end;
//--------------------------------
function EstimationParser.GBLum:Real;
begin   Result := bollum;    end;
//--------------------------------
function EstimationParser.GREst:Real;
begin   Result := radiusest;    end;
//--------------------------------
function EstimationParser.GMEst:Real;
begin   Result := massest;    end;
//--------------------------------
function EstimationParser.GLClass:LuminosityClassType;
begin   Result := lClass;    end;
//--------------------------------
function EstimationParser.GCChar:Char;
begin   Result := scolor;    end;
//--------------------------------
function EstimationParser.GRDwarf:Boolean;
begin
  Result := False;
  if lclass<>lDwarf then Exit;
  if scolor<>'M' then Exit;
  Result := True;
end;
//-------------------------------
function EstimationParser.GSubrange:Real;
begin   Result := subrange;    end;
//------------------------------
function EstimationParser.GIsParsed:Boolean;
begin   Result := parsed;    end;
//------------------------------
function EstimationParser.GTF1:Real;
begin   Result := efftempA;    end;
//------------------------------
function EstimationParser.GTF2:Real;
begin   Result := efftempB;    end;
//++++++++++++++++++++++++++++++++++++++++
// string property methods
//--------------------------------
function EstimationParser.GLumStr:string;
begin
  if luminosity < 0 then Result := 'Unknown'
  else Result := FloatToStrF(luminosity,ffGeneral,3,0) + ' × Sun';
end;
 //------------------------------
function EstimationParser.GBLumStr:string;
begin
  if bollum < 0 then Result := 'Unknown'
  else Result := FloatToStrF(bollum,ffGeneral,3,0) + ' × Sun';
end;
 //------------------------------
function EstimationParser.GRadEstStr:string;
begin
  if radiusest < 0 then Result := 'Unknown'
  else Result := FloatToStrF(radiusest,ffGeneral,3,0) + ' × Sun';
end;
//------------------------------
function EstimationParser.GMassEstStr:string;
begin
  if massest < 0 then Result := 'Unknown'
  else Result := FloatToStrF(massest,ffGeneral,3,0) + ' × Sun';
end;
//------------------------------
function EstimationParser.GLumExport:string;
begin
  if luminosity < 0 then Result := '1'
  else Result := FloatToStrF(luminosity,ffGeneral,3,0);
end;
//------------------------------
function EstimationParser.GBLumExport:string;
begin
  if bollum < 0 then Result := '1'
  else Result := FloatToStrF(bollum,ffGeneral,3,0);
end;
//------------------------------
function EstimationParser.GRadiusExport:string;
begin
  if radiusest < 0 then Result := '1'
  else Result := FloatToStrF(radiusest,ffGeneral,3,0);
end;
//------------------------------
function EstimationParser.GMassExport:string;
begin
  if massest < 0 then Result := '1'
  else Result := FloatToStrF(massest,ffGeneral,3,0);
end;
//------------------------------
function EstimationParser.GSType:string;
begin    Result := stype;   end;
//------------------------------
function EstimationParser.GUTEff:string;
var ktuq:Integer;
begin
  ktuq := 0;
  if efftempB > 0 then ktuq := Round(efftempB)
  else if efftempA > 0 then ktuq := Round(efftempA);
  if ktuq > 0 then Result := IntToStr(ktuq) + 'K'
  else Result := '??';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function EstimationParser.WhiteDwarfParse(wdspecin:string):Boolean;
var startn,endn:Integer;
    subrstr:string;
    sc:Integer;
begin
  Result := False;
  if Length(wdspecin)<2 then Exit;
  // looking for the subrange
  startn := PosSetEx('0123456789.',wdspecin,2);
  if startn<>2 then scolor := wdspecin[2];
  if startn = 0 then begin
    errtype := PS_MISSING;
    Exit;
  end;
  if (startn > 3) then begin
    xpart := Trim(Copy(wdspecin,3,startn-3));
  end else xpart := '';
  // looking for the end of the subrange
  endn := FindFirstNotOf(wdspecin,'0123456789.',startn);
  // endn cannot be between 0 and startn+1
  Assert((endn<=0) or (endn>startn));
  if endn <= 0 then subrstr := AnsiRightStr(wdspecin,Length(wdspecin)-startn+1)
  else subrstr := Copy(wdspecin,startn,endn-startn);
  subrstr := Trim(subrstr);
  // converting the string to a number...
  Val(subrstr,subrange,sc);
  if (sc<>0) then begin
    errtype := PS_SUBRANGE;
    Exit;
  end;
  if (subrange<0) then begin
    errtype:= PS_SUBRANGE;
    Exit;
  end;
  Result := True;
  parsed := True;
  errtype := PS_NOERROR;
end;
//-----------------------------------------
function EstimationParser.HandleStartPart(var specstr:string):Boolean;
var firstchar:Char;
    posx,subx:Integer;
begin
  Result := False;
  firstchar := specstr[1];
  // no nearby stars are of these 'extended' types.
  if AnsiContainsStr('WCSNR',firstchar) then begin
    errtype:= PS_PECULIAR;
    Exit;
  end;
  // dealing with prefixes...
  if AnsiContainsStr('cdgseu',firstchar) then begin
    // subdwarf
    if AnsiStartsStr('sd',specstr) then lclass := lSubDwarf
    // extreme/ultra subdwarf
    else if AnsiStartsStr('esd',specstr) then lclass := lExtremeSubDwarf
    else if AnsiStartsStr('usd',specstr) then lclass := lUltraSubDwarf
    // looks like a dwarf
    else if firstchar = 'd' then lclass := lDwarf
    // looks like a giant
    else if firstchar = 'g' then lclass := lGiant
    // looks like a supergiant
    else if firstchar = 'c' then lclass := lSuperGiantb
    else begin
      errtype := PS_UNRECOGNIZED;
      Exit;
    end;
    // premoving the prefix
    if lclass = lSubDwarf then specstr := AnsiRightStr(specstr,Length(specstr)-2)
    else if (lclass = lExtremeSubDwarf) or (lclass = lUltraSubDwarf) then begin
      specstr := AnsiRightStr(specstr,Length(specstr)-3);
    end
    else specstr := AnsiRightStr(specstr,Length(specstr)-1);
    if Length(specstr)=0 then begin
      errtype := PS_MISSING;
      Exit;
    end;
    // back to checking the first character (after the prefix)
    firstchar := specstr[1];
  end;
  // is the first char part of the recognized temperature range (uppercase)
  Result := AnsiContainsStr(HR_Let,firstchar);
  if (not Result) then errtype := PS_UNRECOGNIZED
  else begin
    // things are okay here...
    scolor := firstchar;
    // there might be some stuff after the color...
    posx := PosSetEx('0123456789',specstr,2);
    if (posx<>0) and (posx>2) then begin
      xpart := Copy(specstr,2,posx-2);
      subx := posx - 1;
    end else subx := 1;
    specstr := AnsiRightStr(specstr,Length(specstr)-subx);
  end;
end;
//-----------------------------------------
(* Here, we try to get the numeric subrange, which is more complicated than
  it should be. Formats include <num>, <num>+ , <num>/<num>, and <num>-<num> *)
function EstimationParser.SubRangeHandle(var specstr:string; out subrout:Real):Boolean;
var chardex,splitdex,sc:Integer;
    subr_string:string;
    subr1,subr2:string;
    sub1,sub2:Real;
    addplus:Boolean;
begin
  Result := False;
  // finding the subrange and extracting it
  if (specstr[1] <> '-') then chardex := FindFirstNotOf(specstr,'0123456789./-+',1)
  else chardex := FindFirstNotOf(specstr,'0123456789./-+',2);
  if chardex <= 0 then  subr_string := Trim(specstr)
  else if chardex = 1 then subr_string:= ''
  else subr_string := Trim(AnsiLeftStr(specstr,chardex-1));
  // removing the subrange from the spectral string
  if chardex <= 0 then specstr := ''
  else if chardex <> 1 then specstr := Trim(AnsiRightStr(specstr,Length(specstr)-(chardex-1)));
  // processing the extracted subrange
  if Length(subr_string)=0 then Exit;
  splitdex := PosSet('/-',subr_string);
  // here, we have a single part number, <num> or <num>+
  if splitdex = 0 then begin
    if AnsiLastChar(subr_string)='+' then begin
      subr_string := AnsiLeftStr(subr_string,Length(subr_string)-1);
      addplus := True;
    end
    else addplus := False;
    Val(subr_string,sub1,sc);
    if (sc<>0) then Exit;
    if addplus then subrout := sub1 + 0.5
    else subrout := sub1;

  end
  // we have 2 numbers here, we try to split
  else begin
    subr1 := Trim(AnsiLeftStr(subr_string,splitdex-1));
    if (length(subr1)) = 0 then Exit;
    subr2 := Trim(AnsiRightStr(subr_string,Length(subr_string)-splitdex));
    if (length(subr2)) = 0 then Exit;
    Val(subr1,sub1,sc);
    if (sc<>0) then Exit;
    Val(subr2,sub2,sc);
    if (sc<>0) then Exit;
    // averaging the two values
    subrout := (sub1+sub2)/2.0;
  end;
  Result := True;
end;
//-----------------------------------------
function EstimationParser.LumClassHandle(var specstr:string):Boolean;
var lclassstr:string;   chardex:Integer;
const hg_cstr:array[0..6] of string = ('Ia0','Ia+','Ia-0','Ia/0','0-Ia','0','0/Ia');
      sga_cstr:array[0..2] of string = ('Ia','Ia-Iab','Ia/ab');
      sgab_cstr:array[0..2] of string = ('Iab','I','Ia-Ib');
      sgb_cstr:array[0..2] of string = ('Ib','Iab-Ib','Iab-b');
      bg_cstr:array[0..6] of string = ('Ib-II','Ib-IIa','IIa','II','IIab','IIb','I-III');
      dg_cstr:array[0..7] of string = ('IV/V','IV-V','IVa-V','IVa/V','V/IV','V-IV','V-IVa','V/IVa');
      ld_cstr:array[0..5] of string = ('V/VI','V-VI','VI-V','VI/V','Vb/VI','Vb-VI');
begin
  chardex := FindFirstNotOf(specstr,'IV/-+ab0',1);
  if chardex <= 0 then lclassstr := Trim(specstr)
  else if chardex = 1 then lclassstr := ''
  else lclassstr := Trim(LeftStr(specstr,chardex-1));
  // A check
  if AnsiContainsStr(specstr,'J') then begin
    Assert(Length(specstr)>0);
  end;
  // removing the luminosity class from the spectral string
  if chardex <= 0 then specstr := ''
  else if chardex <> 1 then specstr := AnsiRightStr(specstr,Length(specstr)-(chardex-1));
  // classifying (empty is assumed to be dwarf, unless already set by a prefix)
  Result := True;
  if lclassstr = '' then begin
    if lclass = XUnknown then lclass := lDwarf;
    Exit;
  end;
  (* XUnknown,lHyperGiant,lSuperGiant,lBrightGiant, lGiant,lSubGiant,lLowGiant,
             lDwarfGiant,lDwarf,lLowDwarf,lSubDwarf,lWhiteDwarf  *)
  if AnsiMatchStr(lclassstr,hg_cstr) then begin
    lclass := lHyperGiant;
  end
  else if (lclassstr = 'Ia') and AnsiStartsStr('e',specstr) then begin
    lclass := lHyperGiant;
  end
  else if AnsiMatchStr(lclassstr,sga_cstr) then lclass := lSuperGianta
  else if AnsiMatchStr(lclassstr,sgab_cstr) then lclass := lSuperGiantab
  else if AnsiMatchStr(lclassstr,sgb_cstr) then lclass := lSuperGiantb
  else if AnsiMatchStr(lclassstr,bg_cstr) then lclass := lBrightGiant
  else if AnsiContainsStr(lclassstr,'IV') and AnsiContainsStr(lclassstr,'III') then lclass := lLowGiant
  else if AnsiContainsStr(lclassstr,'III') then lclass := lGiant
  else if AnsiMatchStr(lclassstr,dg_cstr) then lclass := lDwarfGiant
  else if AnsiContainsStr(lclassstr,'IV') then lclass := lSubGiant
  else if AnsiContainsStr(lclassstr,'VII') then lclass := lWhiteDwarf
  else if AnsiMatchStr(lclassstr,ld_cstr) then lclass := lLowDwarf
  else if AnsiContainsStr(lclassstr,'VI') then lclass := lSubDwarf
  else if AnsiContainsStr(lclassstr,'V') then lclass := lDwarf
  else Result := False;
end;

//-----------------------------------------
function EstimationParser.MainParse(sstring:string; is_pms:Boolean):Boolean;
var qsubr:Real;
    ostring,tstr,thstr:string;
begin
  // startup...
  ClearSpec;
  ostring := sstring;
  if Length(sstring) < 2 then begin
    if Length(sstring) = 0 then errtype := PS_EMPTY
    else errtype := PS_MISSING;
    Exit;
  end;
  // looking at the first character
  // white dwarfs have a separate parsing method...
  if AnsiStartsStr('D',sstring) then begin
    lclass := lWhiteDwarf;
    Result := WhiteDwarfParse(sstring);
    Exit;
  end;
  // we use a separate method to get color char (or fail)
  if (not HandleStartPart(sstring)) then Exit;
  tstr := sstring;
  if (Length(sstring)<1) then Exit;
  // parsing the numeric subrange...
  if (not SubRangeHandle(sstring,qsubr)) then begin
    errtype := PS_SUBRANGE;
    Exit;
  end;
  if (qsubr<0) or (qsubr>=10) then begin
    errtype := PS_SUBRANGE;
    Exit;
  end
  else subrange := qsubr;
  thstr := sstring;
  // trying the get the luminosity class
  if (not LumClassHandle(sstring)) then begin
    errtype := PS_NOLCLASS;
    Exit;
  end;
  if is_pms then lclass := lPreMainSequence;
  parsed := True;
  // that is all.
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// temperature methods
//-----------------------------------------------
(* Using the values scolor and subrange, looks up or calculates an effective
temperature using a table meant for Main-Sequence (Dwarf) stars. Sets efftempA
from this value, Works on O B A F G K M L.  *)
function EstimationParser.LookupDwarfTempA():Boolean;
var colordex:Integer;
    lowtemp:Real;
    m_subrange:Real;
begin
  // reject for bad input
  Result := False;
  if (subrange < 0) or (subrange >= 10) then Exit;
  colordex := AnsiPos(scolor,HR_Let);
  if (colordex = 0) or (colordex > 8) then Exit;
  // initial checks are done
  Result := True;
  // red dwarf lookup
  if scolor = 'M' then begin
    m_subrange := 2*subrange;
    if altMSeq then efftempA := InterLookupReal(m_subrange,MTempTableAlt,LTempTable[0])
    else efftempA := InterLookupReal(m_subrange,MTempTableM,LTempTable[0]);
    Exit;
  end;
  // L dwarf lookup
  if scolor = 'L' then begin
    efftempA := InterLookupReal(subrange,LTempTable,1260);
    Exit;
  end;
  // O thru K?
  // table fixed lookup
  if colordex = 6 then lowtemp := GetMTemp(0)
  else lowtemp := TempTable[colordex+1][0];
  efftempA := InterLookupReal(subrange,TempTable[colordex],lowtemp);
end;
//------------------------------------------------
(* Using the values scolor and subrange, looks up or calculates an effective
temperature using a table meant for Giant (III) stars. Sets efftempA using this
result. Accepts F to M. *)
function EstimationParser.LookupGiantTempA():Boolean;
var colordex:Integer;
    lowtemp:Real;
begin
  // reject for bad input
  Result := False;
  if (subrange < 0) or (subrange >= 10) then Exit;
  colordex := AnsiPos(scolor,Giant_Let);
  if (colordex = 0) then Exit;
  // initial checks are done
  Result := True;
  if colordex = 4 then lowtemp := 2400
  else lowtemp := GiantTempTable[colordex+1][0];
  efftempA := InterLookupReal(subrange,GiantTempTable[colordex],lowtemp);
end;
//----------------------------------------------
(* Using the values scolor and subrange, looks up or calculates an effective
temperature using a table meant for Pre-Main-Sequence stars. Sets efftempA using
this result. Accepts F to M. *)
function EstimationParser.LookupPMSTempA():Boolean;
var colordex:Integer;
    lowtemp:Real;
begin
  // reject for bad input
  Result := False;
  if (subrange < 0) or (subrange >= 10) then Exit;
  colordex := AnsiPos(scolor,Giant_Let);
  if (colordex = 0) then Exit;
  // initial checks are done
  Result := True;
  if colordex = 4 then lowtemp := 2250
  else lowtemp := PMSTempTable[colordex+1][0];
  efftempA := InterLookupReal(subrange,PMSTempTable[colordex],lowtemp);
end;
//-----------------------------------------------
// called after setting a spectra to set the crude TEff
function EstimationParser.SetTempA:Boolean;
begin
  Result := False;
  if lclass = XUnknown then Exit
  // White Dwarfs
  else if lclass = lWhiteDwarf then begin
    // the effective temperate is computed directy from the subrange
    if subrange = 0 then efftempA := 110000   // a guess. Rounding is bad, but happens
    else efftempA := 50400/subrange;
    Result := True;
  // Dwarfs, SubDwarfs, and SubGiants
  end else if Ord(lclass) >= Ord(lSubGiant) then begin
    Result := LookupDwarfTempA;
  // Giants (Supergiants have a separate scale, but I'm ignoring that for now)
  end else begin
    Result := LookupGiantTempA;
  end;
  // boltzmann value
  if Result then boltzA := StefanBoltzmann(efftempA)
  else boltzA := -1;
end;
//------------------------------------------------
// temp B uses calculations from fluxes to get the TEff
function EstimationParser.SetTempB:Boolean;
var usefeh:Currency;
    colordex:Integer;
    gotit:Boolean;
const fehrej:Currency = 0.7;
      fehbig1:Currency = 0.4;
      fehbig2:Currency = 0.2;
begin
  // getting some cases out of the way...
  Result := False;
  efftempB := -1;
  if (lclass = lWhiteDwarf) or (Ord(lclass)<Ord(lGiant)) then Exit;
  colordex := AnsiPos(scolor,HR_Let);
  if colordex <= 0 then Exit;
  if colordex >= 9 then Exit; // no TY for now
  // Pre-Main-Sequence TEff
  if (lclass = lPreMainSequence) and (colordex >= 4) and (colordex <= 7) then begin
    gotit := False;
    if VmOK(False) then begin
      gotit := PecMamjPMS_VmKsToTEff(absVisMag-absKMag,efftempB);
    end;
    if (not gotit) and VmOK(True) then begin
      gotit := PecMamjPMS_VmJToTEff(absVisMag - absJMag,efftempB);
    end;
  end
  // Subgiants, Dwarfs, and SubDwarfs
  else if Ord(lclass) >= Ord(lSubGiant) then begin
    (* Red Dwarfs (including late K): either V-J based from 'How to constrain
    your M Dwarf', or interpolated V-K based, from Mamajek's Table. *)
    if IsLateDwarf() then begin
      gotit := False;
      if VmOK(True) then gotit := ConstrainMDwarfTemp(efftempB);
      if VmOK(False) and (not gotit) then begin
        gotit := Inter_GKML_TeffEst(absVisMag-absKMag,efftempB);
      end;
    end
    // covers A to K stars (no special method for A stars)
    else if colordex > 2 then begin
      // feh, assume 0 if we have none, if more than 0.4, treat as 0.4
      usefeh := feh;
      if usefeh > fehrej then usefeh := 0
      else if usefeh > fehbig1 then usefeh := fehbig1;
      // V-K temperature determination
      if VmOk(False) then begin
        // try casagrande teff
        if not CasagrandeTEffEstK(absVisMag-absKMag,usefeh,efftempB) then begin
          // try interpolated v-k method
          Inter_GKML_TeffEst(absVisMag-absKMag,efftempB);
        end;
      end
      // casagrande B-V teff
      else if absBMag < 90 then begin
        CasagrandeTEffEstB(absBMag-absVisMag,usefeh,efftempB);
      end;
    end;
    // nothing for OB stars
  // giants (including III-IV)
  end else begin
    // Using the method in Alonso+ 1999
    if VmOK(False) then begin
      // setting Fe/H
      usefeh := feh;
      if usefeh > fehrej then usefeh := 0
      else if usefeh > fehbig2 then usefeh := fehbig2;
      // using the equation
      AlonsoGiantTeff(absVisMag-absKMag,usefeh,efftempB);
    end;
  end;
  // done
  Result := (efftempB > 0);
  // boltzmann value
  if Result then boltzB := StefanBoltzmann(efftempB)
  else boltzB := -1;
end;
//---------------------------------------------------------
function EstimationParser.ConstrainMDwarfTemp(out Teffout:Real):Boolean;
var vmj_in,jmh_in,jmk_test:Real;
begin
  Result := False;
  Teffout := -1;
  if (absVisMag > 90) or (absJMag > 90) then Exit;
  // anomalous j-k sanity test...
  if AbsKMag < 90 then begin
    jmk_test := absJMag - absKMag;
    if jmk_test < 0.45 then Exit;
    if jmk_test > 1.02 then Exit;
  end;
  // calculating inputs
  vmj_in := AbsVisMag - AbsJMag;
  if AbsHMag < 90 then jmh_in := AbsJMag - AbsHMag
  else jmh_in := 999;
  // finally
  Result := ConstrainMDwarfTeff(vmj_in,jmh_in,feh,Teffout);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// bolometric correction / luminosity methods
//-----------------------------------------------------
(* Gets or calculates a bolometric luminosity for M Dwarfs (and subdwarfs), early
 L dwarfs, and late K dwarfs  *)
function EstimationParser.CalcDwarfBollumKML():Boolean;
var jmk_test,vmj_in,subdiv:Real;
    cor_out:Real;
begin
  Result := False;
  // Trying 'How to constrain your M Dwarf' first
  if VmOK(True) then begin
    // J - K should be in a reasonable range...
    jmk_test := absJMag - absKMag;
    if not ((jmk_test < 0.45) or (jmk_test > 1.02)) then begin
      // calling the method...
      vmj_in := AbsVisMag - AbsJMag;
      Result := ConstrainMDwarfBCv(vmj_in,feh,cor_out);
      bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
      // checking the results...
      if Result then Exit;
    end;
  end;
  (* Trying the Dupuy-Liu AbsHMag or AbsKMag to Bolometrtic Luminosity (This
  obsoletes the BCK table lookup method). *)
  if AbsHMag < 90 then Result := DupuyLiuMagToLum(AbsHMag,True,bollum);
  if (not Result) and (AbsKMag < 90) then begin
    Result := DupuyLiuMagToLum(AbsKMag,False,bollum);
  end;
  if Result then Exit;
  // Interpolated using EfftempB
  if efftempB > 0 then begin
    Result := TeffToBCv(efftempB,useBCvLog,cor_out);
    bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
    if Result then Exit;
  end;
  // fallback lookup tables...
  // K Dwarf
  if scolor = 'K' then begin
    if altMSeq then cor_out := InterLookupReal(subrange,BCorTable[6],MBCorAlt[0])
    else  cor_out := InterLookupReal(subrange,BCorTable[6],MBCorM[0]);
    bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
  end
  // M Dwarf
  else if scolor = 'M' then begin
    // using the K-band corrections if we have no V (and the star is ultracool)
    if (subrange >= 7) and (absVisMag >= 90) and (absKMag < 90) then Begin
      subdiv := subrange - 7;
      cor_out := InterLookupReal(subdiv,MK_BCor,LK_BCor[0]);
      bollum := AbsBolMagToBolLum(AbsKMag + cor_out);
    end else begin
      subdiv := subrange*2;
      if altMSeq then cor_out := InterLookupReal(subdiv,MBCorAlt,5.9)
      else cor_out := InterLookupReal(subdiv,MBCorM,5.9);
      bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
    end;
  end
  // L Dwarf
  else begin
    if (subrange >= 8) or (absKMag >= 90) then Exit;
    cor_out := InterLookupReal(subrange,LK_BCor,3.34);
    bollum := AbsBolMagToBolLum(AbsKMag + cor_out);
  end;
  // done...
  Result := True;
end;
//-----------------------------------------------------
(* Covers Subgiants, Dwarfs, and Subdwarfs B A F G K. mid F to mid K uses
Casagrande's polynomial fits, B A F uses interpolated BCv using TEff. *)
function EstimationParser.CalcDwarfSGBollumBAFGK():Boolean;
var usefeh:Currency;
    cor_out,blumx:Real;
const fehrej:Currency = 0.6;
      fehbig:Currency = 0.4;
begin
   Result := False;
   // feh, assume 0 if we have none, if more than 0.4, treat as 0.4
   usefeh := feh;
   if usefeh > fehrej then usefeh := 0
   else if usefeh > fehbig then usefeh := fehbig;

   // trying for Casagrande's fits
   if (scolor = 'F') or (scolor = 'G') or (scolor = 'K') then begin
     if VmOk(False) then begin
       // Casagrande's Vmk, covers F4 to K5, more or less
       Result := CasagrandeBoloEstK(absVisMag-absKMag,usefeh,blumx);
       if Result then bollum := luminosity*CasaBoloM_VmK*blumx;
     end
     else if AbsBMag < 90 then begin
       // Casagrande's B-V fit, covers F4 to K5, more or less
       Result := CasagrandeBoloEstB(absBMag-absVisMag,usefeh,blumx);
       if Result then bollum := luminosity*CasaBoloM_BmV*blumx;
     end;
     if Result then Exit;
   end;

   // for hotter stars, we use TEff based interpolation...
   if (scolor = 'B') or (scolor = 'A') or (scolor = 'F') then begin
     if efftempB > 0 then Result := TeffToBCv(efftempB,useBCvLog,cor_out)
     else if efftempA > 0 then Result := TeffToBCv(efftempA,useBCvLog,cor_out);
     if Result then bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
   end
   // K fallback: efftempB interpolation or stype lookup
   else if (scolor = 'K') then begin
     if efftempB > 0 then begin
       Result := TeffToBCv(efftempB,useBCvLog,cor_out);
       bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
       if Result then Exit;
     end;
     if altMSeq then cor_out := InterLookupReal(subrange,BCorTable[6],MBCorAlt[0])
     else  cor_out := InterLookupReal(subrange,BCorTable[6],MBCorM[0]);
     bollum := AbsBolMagToBolLum(AbsVisMag + cor_out);
     Result := True;
  end;
  // done
end;
//-----------------------------------------------------
function EstimationParser.CalcWhiteDwarfBollum():Boolean;
var ishydr:Boolean;
    uselogg:Currency;
    bvcor:Real;
const deflogg:Currency = 8;
begin
  Result := False;
  // preparing for the call
  if logg < 0 then uselogg := deflogg
  else uselogg := logg;
  ishydr := AnsiStartsStr('DA',stype);
  // the call
  Result := wdbc.GetBCvBloat(ishydr,uselogg,efftempA,bvcor,wdbloat);
  if Result then bollum := AbsBolMagToBolLum(AbsVisMag + bvcor);
end;
//---------------------------------------------------------
function EstimationParser.CalcBoloGiant:Boolean;
var calcok:Boolean;
    qtemp,bcor,bmag:Real;
    usefeh:Currency;
const fehrej:Currency = 0.7;
      fehbig2:Currency = 0.2;
begin
  Result := False;
  // getting the temperature we will use
  if efftempB > 0 then qtemp := efftempB
  else qtemp := efftempA;
  // setting Fe/H
  usefeh := feh;
  if usefeh > fehrej then usefeh := 0
  else if usefeh > fehbig2 then usefeh := fehbig2;
  (* Alonso+ 1999 based method *)
  calcok := AlonsoGiantBCv(qtemp,usefeh,bcor);
  (* Rough colometric correction equation from Buzzoni+ 2006.
   with offset because of different magnitudes for the Sun. *)
  if not calcok then begin
    if qtemp < 5200 then begin
      bcor := -exp(27500/qtemp)/1000 - 0.052;
      calcok := True;
    end;
  end;
  if not calcok then Exit;
  // calculating bolometric luminosity
  bmag := absVisMag + bcor;
  bollum := AbsBolMagToBolLum(bmag);
  Result := True;
end;
//-----------------------------------------------------
function EstimationParser.CalcBoloPMS():Boolean;
var bcv,bcj,bmag:Real;
    colordex:Integer;
begin
  Result := False;
  bcj := 99;
  colordex := AnsiPos(scolor,HR_Let);
  // first, try the V-K fit
  if VmOK(False) then Result := PecMamjPMS_VmKsToBCv(AbsVisMag-AbsKMag,bcv);
  // next, try the V-J BCj fit
  if (not Result) and VmOK(True) then begin
      Result := PecMamjPMS_VmJToBCj(AbsVisMag-AbsJMag,bcj);
  end;
  // if those fail, use table lookup
  if (not Result) and (colordex >= 4) and (colordex <=7) then begin
    if colordex <> 7 then begin
      bcv := InterLookupReal(subrange,PMSBCvTable[colordex],PMSBCvTable[colordex+1][0]);
    end else begin
      bcv := InterLookupReal(subrange,PMSBCvTable[7],-5.9);
    end;
  end;
  // here, at the end...
  if Result then begin
    // calculating bolometric luminosity
    if bcj < 90 then bmag := AbsJMag + bcj
    else bmag := AbsVisMag + bcv;
    bollum := AbsBolMagToBolLum(bmag);
  end;

end;
//-----------------------------------------------------
function EstimationParser.HasBolomCorr:Boolean;
begin
  Result := False;
  // white dwarfs have BCv (except for ultracool helium-atmos wd)
  if (lclass = lWhiteDwarf) then begin
    Result := True;
    if (efftempA < 3500) and (not AnsiStartsStr('DA',stype)) then Result := False;
    Exit;
  end;
  if (scolor = 'O') or (scolor = 'W') then Exit;
  if (scolor = 'T') or (scolor = 'Y')  then Exit;
  if Ord(lclass) <= Ord(lBrightGiant) then Exit;
  Result := True;
end;
//--------------------------------------
// dwarfs and subdwarfs only, returns true if we use late BCv and TEff methods
// (K7 or later)
function EstimationParser.IsLate():Boolean;
var vmj:Real;
begin
  Result := False;
  if not AnsiContainsStr(late_let,scolor) then Exit;
  Result := True;
  if (scolor <> 'K') then Exit;
  if VmOK(True) then begin
    vmj := AbsVisMag - AbsJMag;
    if vmj >= 2.6 then Exit;
  end
  else begin
    if subrange > 6.5 then Exit;
  end;
  Result := False;
end;
//--------------------------------------
// estimates bolometric luminosity, setting the property
function EstimationParser.CalcBolometricLuminosity:Boolean;
var blumx:Real;
    xtest:Boolean;
begin
  // bad cases
  Result := False;
  if (efftempA <= 0) and (efftempB <= 0) then Exit;
  if lclass = XUnknown then Exit;

  // if we have no other way, calculate using blackbody assumtions
  if (not HasBolomCorr()) then begin
    if efftempB > 0 then begin
      planckI := PlanckVBandIntegral(efftempB);;
      blumx := luminosity*boltzB / planckI;
    end else begin
      planckI := PlanckVBandIntegral(efftempA);
      blumx := luminosity*boltzA / planckI;
    end;
    bollum := blumx / sunbol_mul;
    Result := True;
    Exit;
  end;

  xtest := (scolor='M') and (subrange < 6);
  if not xtest then xtest := (scolor='F') or (scolor='G') or (scolor='K');
  // category 1 : white dwarfs
  if lclass = lWhiteDwarf then begin
    Result := CalcWhiteDwarfBollum();
  end
  // category 2 : Pre-Main-Sequence stars
  else if (lclass = lPreMainSequence) and xtest then begin
    Result := CalcBoloPMS();
  end
  // category 3 : Subdwarfs, Dwarfs, Subgaints
  else if Ord(lclass) >= Ord(lSubGiant) then begin
    // split into cool (K7 or later) and hot
    if IsLate() then Result := CalcDwarfBollumKML()
    else Result := CalcDwarfSGBollumBAFGK();
  end
  // left to do: Giants (III) and Giant/Subgiants (IV-III)
  else begin
    Assert((lclass = lGiant) or (lclass = lLowGiant));
    Result := CalcBoloGiant;
  end;
  // done
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// radius estimation methods
//-----------------------------
function EstimationParser.BolometricTempRadius(out radius_est:Real):Boolean;
var boltzmul:Real;
    useboltz:Real;
begin
  Result := False;
  if (efftempA<=0) and (efftempB<=0) then Exit;
  if bollum<0 then Exit;
  if boltzB > 0 then useboltz := boltzB
  else if boltzA > 0 then useboltz := boltzA
  else Exit;
  // we estimate the radius, first by assuming the boltz value is good
  // we then compute the ratio of boltz vs the sun (area to get a certain energy output)
  boltzmul := boltz_sun/useboltz;
  // using the bolometric luminosity, we can then calculate the result
  radius_est := Sqrt(boltzmul*bollum);
  Result := True;
end;
//-----------------------------
function EstimationParser.BlackbodyRadius(vfilter:Boolean; out radius_est:Real):Boolean;
var lummul,altplanxk:Real;
begin
  Result := False;
  if efftempB < 1 then efftempB := 0;
  if (efftempA<0) and (efftempB<0) then Exit;
  if luminosity<0 then Exit;
  if vfilter then begin
    // amount of visible light that passes through V Filter, per area...
    if efftempB > 20 then altplanxk := PlanckVBandIntegral(efftempB)
    else altplanxk := PlanckVBandIntegral(efftempA);
    lummul :=  VplanckSunInt / altplanxk;
  end else begin
    // amount of visible light per area...
    if planckI < 0 then begin
      if efftempB > 20 then planckI := PlanckVisIntegral(efftempB)
      else planckI := PlanckVisIntegral(efftempA);
    end;
    lummul := planckSunInt / planckI;
  end;
  // calculating the result
  radius_est := Sqrt(luminosity*lummul);
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++
// mass estimation methods

//-----------------------------
(* using new equations  *)
function EstimationParser.DwarfBolLumToMass(out mass_est:Real):Boolean;
var loglum:Real;
    okval:Boolean;
begin
  Result := False;
  // if lclass <>lDwarf then Exit;
  if bollum <= 0 then Exit;
  Result := True;
  loglum := log10(bollum);
  // using some non-purely-luminosity based methods...
  okval := False;
  if (efftempB > 50) then okval := MoyaEtAlMass(efftempB,bollum,feh,mass_est)
  else if (efftempA > 50) then okval := MoyaEtAlMass(efftempA,bollum,feh,mass_est);
  if okval then Exit;
  (* from Wikipedia, might end up being used by ultracool stars not covered by the
  Red Dwarf mass estimate methods *)
  if bollum < 0.0087 then mass_est := power(bollum/0.23,1/2.3)
  // from Eker+ 2015
  else if bollum <= 1.21 then mass_est := exp10((loglum+0.02625)/4.841132)
  else if bollum <= 43.17 then mass_est := exp10((loglum+0.0022)/4.3289)
  else if bollum <= 3209 then mass_est := exp10((loglum-0.120112)/3.962203)
  else if bollum <= 219060 then mass_est := exp10((loglum-1.237228)/3.726203)
  // from Wikipedia
  else mass_est := bollum /32000;
end;
//---------------------------------------------------
(* Using reverse lookup to estimate cool and ultracool stars *)
function EstimationParser.RedDwarfMass(out mass_est:Real):Boolean;
begin
  Result := False;
  // I assume most checks are done before calling this method...
  if (AbsVisMag >= 90) and (AbsKMag >= 90) then Exit;
  // K band estimations are usually better, V band is done if no K mag exists
  if (AbsKMag >= 90) then begin
    Result := RedDwarfMagnitudeToMassLookup(AbsVisMag,False,mass_est);
  end else begin
    // K mag is brighter than 10 : use K mag (if possible)
    if (AbsKMag <= 10) or ((AbsKMag>10) and (AbsVisMag > 90)) then begin
      Result := RedDwarfMagnitudeToMassLookup(AbsKMag,True,mass_est);
    end else begin   // k mag is dimmer than 10, we have a valid V
      Result := RedDwarfMagnitudeToMassLookup(AbsVisMag,False,mass_est);
    end;
  end;
  // done
end;
//---------------------------------------------------
(* For White Dwarfs, mass is estimated from the radius: smaller White Dwarves
are more massive! *)
function EstimationParser.WhiteDwarfMass(out mass_est:Real):Boolean;
begin
  Result := False;
  if lclass <> lWhiteDwarf then Exit;
  if radiusest <= 0 then Exit;
  Result := WhiteDwarfRadiusToMassLookup(radiusest/wdbloat,mass_est);
  Result := True;
end;
//-----------------------------------------------------
(* I could not find any simple way to guesstimate SubGiant mass in the
literature, so I came up with my own, ignoring color or metallicity.
Assumes Log(M) ≈ A × Log(L) + B  .  Treat as wild guess. *)
function EstimationParser.SubGiantMassGuesstimate(out mass_est:Real):Boolean;
var logl,logm:Real;
const A = 0.20112;  B = 0.007933;
begin
  Result := False;
  // low luminosity subgiants are probably better handled as main sequence
  if bollum < 1.5 then Exit;
  // my sample did not include any L > 30 SubGiants, I will ignore that
  Result := True;
  logl := Log10(bollum);
  logm := A*logl + B;
  mass_est := exp10(logm);
end;
//-------------------------------------------------------------------
(* Another subgiant mass estimate based on a fit to table information in
'Oxygen abundances in nearby FGK stars (Ramirez+, 2013)  *)
function EstimationParser.SubGiantMassEstimateB(out mass_est:Real):Boolean;
var xteff,logteff,sqlteff:Real;
    usefeh:Currency;
const feht   =   0.108878;
      teff1t = -37.930294;
      teff2t =   5.105139;
      absvt  =  -0.081176;
      offst  =  70.791868;
      minfeh = -1.6;
      maxfeh = 0.36;
begin
  // limits
  Result := False;
  if (lclass <> lSubGiant) and (lclass <> lDwarfGiant) then Exit;
  if (AbsVisMag < 1.9) or (AbsVisMag > 4.9) then Exit;
  if feh < minfeh then Exit;
  if efftempB > 0 then xteff := efftempB
  else xteff := efftempA;
  if (xteff > 7100) or (xteff<5000) then Exit;
  // temperature
  logteff := log10(xteff);
  sqlteff := logteff*logteff;
  // fe/h
  if feh > 9 then usefeh := 0
  else if feh > maxfeh then usefeh := maxfeh
  else usefeh := feh;
  // computing the result
  mass_est := absvt*AbsVisMag + teff1t*logteff + teff2t*sqlteff;
  mass_est += feht*CurrToReal(usefeh) + offst;
  mass_est := exp10(mass_est);
  Result := True;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// other private methods
//-----------------------
procedure EstimationParser.ClearMag;
begin
  absVisMag := 999;
  luminosity := -1;   bollum := -1;
  radiusest := -1;    massest := -1;
  absKMag := 999;
  satk := False;
  absJMag := 999;     absHMag := 999;
  absIMag := 999;     absBMag := 999;
  efftempB := -1;
  efftempC := -1;
  boltzB := -1;
  planckI := -1;
end;
//-----------------------
procedure EstimationParser.ClearSpec;
begin
  parsed := False;
  errtype := PS_NOERROR;
  scolor := 'X';
  subrange := -1;
  lclass := XUnknown;
  efftempA := -1;
  boltzA := -1;
end;
//-----------------------
function EstimationParser.VmOK(isj:Boolean):Boolean;
begin
  Result := False;
  if AbsVisMag > 90 then Exit;
  if isj then begin
    if (AbsJMag > 90) then Exit
  end else begin
      if (AbsKMag > 90) then Exit;
      if satk then Exit;
  end;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// estimator methods
//-----------------------
function EstimationParser.DualMagToStr(mag1,mag2:Real):string;
begin
  if mag1 >= 60 then Result := ','
  else Result := Trim(FloatToStrF(mag1,ffFixed,7,3)) + ',';
  if mag2 >= 60 then Result += ','
  else Result += Trim(FloatToStrF(mag2,ffFixed,7,3)) + ',';
end;
//-----------------------
function EstimationParser.TEffToStr(const teff1:Real):String;
begin
  if teff1 <= 50 then Result := ','
  else Result := IntToStr(Round(teff1)) + ',';
end;
//----------------------
function EstimationParser.DualTEffToStr(teff1,teff2:Real):string;
begin
  Result := TeffToStr(teff1) + TEffToStr(teff2);
end;
//-----------------------
function EstimationParser.MakeEPDStr(teff1,teff2:Real):string;
var tepd:Real;
begin
  Result := ',';
  if (teff2 <= 0) or (teff1 <= 0) then Exit;
  tepd := MakeEPD(teff1,teff2);
  Result := Trim(FloatToStrF(tepd,ffFixed,7,3)) + ',';
end;
//-----------------------
function EstimationParser.MakeInfoLineStart(wdi:Boolean; out bmag1,vmkteff:Real):string;
var vmk:Real;
    radprec:Integer;
begin
  // spectral type and magnitudes (V and K)
  Result := stype + ',' + LuminosityClassName[Ord(lclass)] + ',';
  Result += DualMagToStr(absVisMag,absKMag);
  // temperature, calculated b mag and 'bolometric correction' (calculated)
  Result += DualTEffToStr(efftempA,efftempB);
  bmag1 := BolLumToAbsBolMag(bollum);
  Result += DualMagToStr(bmag1,bmag1-absVisMag);
  // Fe/H
  Result += CurrToStrF(feh,ffFixed,2) +',';
  // radius and mass estimates
  radprec := 3;
  if wdi then Inc(radprec);
  Result += Trim(FloatToStrF(radiusest,ffFixed,7,radprec)) + ',';
  Result += Trim(FloatToStrF(massest,ffFixed,7,3)) + ',';
  // V-K and alternate temperature derived from that.
  if (AbsKMag < 90) and (AbsVisMag < 90) then begin
    vmk := absVisMag-absKMag;
    Result += Trim(FloatToStrF(vmk,ffFixed,7,3)) + ',';
    vmkteff := 0;
    VmK_TeffEst(vmk,vmkteff);
    Result += Trim(FloatToStrF(vmkteff,ffFixed,5,0));
  end else Result += ','
end;
//---------------------------------------------------
function EstimationParser.MakeInfoLineMDwarf(const bcor1:Real):string;
var redtemp,quadval:Real;
    qtemp:Real;
    bcvo,bcedif:Real;
    xbmag,massx:Real;
    bollum,bxmag:Real;
begin
  redtemp := 0;
  if efftempB > 0 then qtemp := efftempB
  else qtemp := efftempA;
  if ConstrainMDwarfTemp(redtemp) then begin
    Result := Trim(FloatToStrF(redtemp,ffFixed,5,0)) + ',';
    Result += Trim(FloatToStrF(redtemp-qtemp,ffFixed,5,0)) + ',';
    quadval := MakeEPD(redtemp,qtemp);
    Result += Trim(FloatToStrF(quadval,ffFixed,6,1)) + ',';
  end
  else Result += ',,,';
  if ConstrainMDwarfBCv(absVisMag-absJMag,feh,bcvo) then begin
    Result += Trim(FloatToStrF(bcvo,ffFixed,7,3)) + ',';
    Result += Trim(FloatToStrF(Abs(bcvo-bcor1),ffFixed,7,3)) + ',';
    bcedif := 100*Abs(1-exp10(-0.4*(bcvo-bcor1)));
    Result += Trim(FloatToStrF(bcedif,ffFixed,7,2)) + ',';
  end
  else Result += ',,,';
  // casagrande simple relations
  if AbsKMag < 90 then begin
    xbmag := 2.07+1.08*AbsKMag;
    Result += Trim(FloatToStrF(xbmag-AbsVisMag,ffFixed,7,3)) + ',';
    bcedif := 100*Abs(1-exp10(-0.4*((xbmag-AbsVisMag)-bcor1)));
    Result += Trim(FloatToStrF(bcedif,ffFixed,7,2)) + ',';
  end else Result += ',,';
  if AbsJMag < 90 then begin
    xbmag := 1.22+1.07*AbsJMag;
    Result += Trim(FloatToStrF(xbmag-AbsVisMag,ffFixed,7,3)) + ',';
  end else Result += ',';
  // Casagrande V-K M Dwarf Teff
  if CasagradeM_TEffEstK(absVisMag-absKMag,redtemp) then begin
    Result += Trim(FloatToStrF(redtemp,ffFixed,5,0)) + ',';
    quadval := MakeEPD(redtemp,qtemp);
    Result += Trim(FloatToStrF(quadval,ffFixed,6,1)) + ',';
  end
  else Result += ',,';
  // Boyajian
  if BoyajianMKTEffVmK(absVisMag-absKMag,feh,redtemp) then begin
    Result += Trim(FloatToStrF(redtemp,ffFixed,5,0)) + ',';
    quadval := MakeEPD(redtemp,qtemp);
    Result += Trim(FloatToStrF(quadval,ffFixed,6,1)) + ',';
  end
  else Result += ',,';
  // new luminosity est
  if AbsKMag < 90 then begin
    if DupuyLiuMagToLum(AbsKMag,False,bollum) then begin
      bxmag := BolLumToAbsBolMag(bollum);
      Result += Trim(FloatToStrF(bxmag,ffFixed,6,3)) + ',';
      if AbsVisMag < 90 then Result += Trim(FloatToStrF(bxmag-AbsVisMag,ffFixed,6,3)) + ','
      else Result += ',';
    end else Result += ',,';
  end else Result += ',,';
  if AbsHMag < 90 then begin
    if DupuyLiuMagToLum(AbsHMag,True,bollum) then begin
      bxmag := BolLumToAbsBolMag(bollum);
      Result += Trim(FloatToStrF(bxmag,ffFixed,6,3)) + ',';
      if AbsVisMag < 90 then Result += Trim(FloatToStrF(bxmag-AbsVisMag,ffFixed,6,3));
      Result += ',';
    end else Result += ',,';
  end else Result += ',,';
  // extra mass estimate from Mann+ 2018
  if AbsKMag < 90 then begin
    if RedDwarfMassFitMann(AbsKMag,massx) then begin
      Result += Trim(FloatToStrF(massx,ffFixed,5,3)) + ',';
    end;
  end;

end;
//---------------------------------------------------
function EstimationParser.MakeInfoLineBB(const satkout:Boolean):string;
var bb_bcv:Real;
begin
  // Blackbody Magnitude and correction
  if efftempB > 0 then bb_bcv := SUN_BCv + BlackBodyExtraBCv(efftempB)
  else if efftempA > 0 then bb_bcv := SUN_BCv + BlackBodyExtraBCv(efftempA)
  else bb_bcv := 10000;
  if bb_bcv > 1000 then Result := ',,'
  else Result := DualMagToStr(AbsVisMag + bb_bcv,bb_bcv);
  // saturated Ks
  if satkout then begin
    if satk then Result += 'T,'
    else Result += 'F,';
  end;
end;

//---------------------------------------------------
function EstimationParser.MakeInfoLineWhiteDwarf(const bcor1:Real):string;
var zmass1,zmass2:Real;
    newbcv,bbRadius:Real;
    lookok,ishydr:Boolean;
    bloatest:Real;
    uselogg:Currency;
const deflogg:Currency = 8;
begin

  Result := MakeInfoLineBB(False);
  // alternate Zhami Mass
  zmass1 := WhiteDwarfRadiusToMassP(radiusest,False);
  Result += Trim(FloatToStrF(zmass1,ffFixed,6,2)) + ',';
  // preparing for white dwarf call
  if logg < 0 then uselogg := deflogg
  else uselogg := logg;
  ishydr := AnsiStartsStr('DA',stype);
  // the call
  lookok := wdbc.GetBCvBloat(ishydr,uselogg,efftempA,newbcv,bloatest);
  if lookok then begin
    Result += Trim(FloatToStrF(newbcv,ffFixed,6,3)) + ',';
  end else Result += ',';
  bbRadius := radiusest/bloatest;
  Result += Trim(FloatToStrF(bbRadius,ffFixed,6,4)) + ',';
  if WhiteDwarfRadiusToMassLookup(bbRadius,zmass2) then begin
    Result += Trim(FloatToStrF(zmass2,ffFixed,6,2)) + ',';
  end else Result += ',';
end;
//---------------------------------------------------
// info line for other (non ultracool/white dwarf/red dwarf/red subdwarf/giant)
function EstimationParser.MakeInfoLineOther(const calctemp,bmag1:Real):string;
var cteffb,cteffi,cteffk:Real;
    vmk,bmv,cgbb,cgbi,cgbk,abck,pmbcj:Real;
    blum1,blum2,bcv1,bcv2,masse1,masse2:Real;
begin
  Result := MakeInfoLineBB(True);
  // Casagrande Teff
  cteffb := 0;  cteffi := 0;  cteffk := 0;
  bmv := absBMag-absVisMag;
  CasagrandeTEffEstB(bmv,feh,cteffb);
  CasagrandeTEffEstI(absVisMag-absIMag,feh,cteffi);
  vmk := absVisMag-AbsKMag;
  CasagrandeTEffEstK(vmk,feh,cteffk);
  Result += DualTEffToStr(cteffb,cteffi);
  Result += TEffToStr(cteffk);
  // Casagrande BCv
  cgbb := 999;  cgbi := 999;  cgbk := 999;
  if CasagrandeBoloEstB(bmv,feh,cgbb) then begin
    cgbb := CasaBO_BmV-2.5*Log10(exp10(-0.4*AbsVisMag)*cgbb);
  end;
  if CasagrandeBoloEstI(absVismag-absIMag,feh,cgbi) then begin
    cgbi := CasaBO_VmI-2.5*Log10(exp10(-0.4*AbsVisMag)*cgbi);
  end;
  if CasagrandeBoloEstK(vmk,feh,cgbk) then begin
    cgbk := CasaBO_VmK-2.5*Log10(exp10(-0.4*AbsVisMag)*cgbk);
  end;
  Result += DualMagToStr(cgbb-absVisMag,cgbi-AbsVisMag);
  if cgbk < 900 then Result += Trim(FloatToStrF(cgbk-absVisMag,ffFixed,7,3)) + ','
  else Result += ',';
  // Alonso TEff
  cteffb := 0;  cteffk := 0;
  AlonsoBVTEff(bmv,feh,cteffb);
  AlonsoVKTEff(vmk,feh,cteffk);
  Result += DualTEffToStr(cteffb,cteffk);
  // BCv using Alonso method
  if AlonsoBCCalc(vmk,False,feh,abck) then begin
    bcv1 := absKMag + abck - AbsVisMag;
    Result += Trim(FloatToStrF(bcv1,ffFixed,7,3)) + ',';
  end
  else Result += ',';
  // some additional TEffs from Boyajian (interferonomy based)
  cteffb := 0;  cteffk := 0;
  BoyajianBmV_TEff(bmv,cteffb);
  BoyajianMKTEffVmK(vmk,feh,cteffk);
  Result += DualTEffToStr(cteffb,cteffk);
  // Pecault-Mamajek Pre-Main-Sequence TEffs
  cteffb := 0;  cteffk := 0;
  PecMamjPMS_VmKsToTEff(vmk,cteffk);
  PecMamjPMS_VmJToTEff(vmk,cteffb);
  Result += DualTEffToStr(cteffk,cteffb);
  // Pecault-Mamajek Pre-Main-Sequence BCv's
  bcv1 := 1000;  bcv2 := 1000;  pmbcj := 1000;
  PecMamjPMS_VmKsToBCv(vmk,bcv1);
  PecMamjPMS_VmJToBCj(AbsVisMag-AbsJMag,pmbcj);
  if pmbcj < 90 then bcv2 := AbsJMag + pmbcj - AbsVisMag;
  Result += DualMagToStr(bcv2,pmbcj);
  // Hernandez-Bonifacio Methods
  cteffb := 0;  cteffk := 0;
  HerBon_DwarfTEffEstBV(bmv,feh,cteffb);
  HerBon_DwarfTEffEstVK(vmk,feh,cteffk);
  Result += DualTEffToStr(cteffb,cteffk);
  bcv1 := 1000;  bcv2 := 1000;
  if HerBon_DwarfBLumEstVK(absVisMag,absKMag,feh,False,blum1) then begin
    bcv1:= BolLumToAbsBolMag(blum1) - AbsVisMag;
  end;
  if HerBon_DwarfBLumEstVJ(absVisMag,absJMag,feh,False,blum2) then begin
    bcv2:= BolLumToAbsBolMag(blum2) - AbsVisMag;
  end;
  Result += DualMagToStr(bcv1,bcv2);
  // Huang 2015 Interferonomy-Based TEff
  cteffb := 0;  cteffk := 0;
  HuangEtAl_DwarfTEffEstBV(bmv,feh,cteffb);
  HuangEtAl_DwarfTEffEstVK(vmk,feh,cteffk);
  Result += DualTEffToStr(cteffb,cteffk);
  // mass estimates
  masse1 := 1000; masse2 := 1000;
  if efftempB > 0 then TorresEtAlMass(efftempB,feh,logg,masse1)
  else if efftempA > 0 then TorresEtAlMass(efftempA,feh,logg,masse1);
  if cteffk > 50 then TorresEtAlMass(cteffk,feh,logg,masse2);
  Result += DualMagToStr(masse1,masse2);
  if efftempB > 0 then MoyaEtAlMass(efftempB,bollum,feh,masse1)
  else if efftempA > 0 then MoyaEtAlMass(efftempA,bollum,feh,masse1);
  if cteffk > 50 then MoyaEtAlMass(cteffk,bollum,feh,masse2);
  Result += DualMagToStr(masse1,masse2);


end;
//--------------------------------------------------------
function EstimationParser.MakeInfoLineGiant(bmag:Real):string;
var gbcv,gtemp1,gtemp2,vmk:Real;
begin
  Result := MakeInfoLineBB(True);
  // Alonso Giant TEff and BCv
  vmk := absVismag-absKMag;
  gtemp1 := 0;
  AlonsoGiantTeff(vmk,feh,gtemp1);
  Result += TEffToStr(gtemp1);
  if gtemp1 > 0 then begin
    if AlonsoGiantBCv(gtemp1,feh,gbcv) then begin
      Result += Trim(FloatToStrF(gbcv,ffFixed,7,3)) + ','
    end else Result += ',';
    Result += MakeEPDStr(gtemp1,efftempA);
  end else Result += ',,';
  // Hernandez-Bonifacio Methods
  gtemp1 := 0;  gtemp2 := 0;
  HerBon_GiantTEffEstVK(vmk,feh,gtemp1);
  HerBon_GiantTEffEstVJ(absVisMag-absJMag,feh,gtemp2);
  Result += DualTEffToStr(gtemp1,gtemp2);
  // Huang 2015 Interferonomy-Based TEff
  gtemp1 := 0;  gtemp2 := 0;
  HuangEtAl_GiantTEffEstBV(absBMag-absVisMag,feh,gtemp1);
  HuangEtAl_GiantTEffEstVH(absVisMag-absHMag,feh,gtemp2);
  Result += DualTEffToStr(gtemp1,gtemp2);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// default constructor
constructor EstimationParser.Create();
const invfeh:Currency  =  9.99;
      invlogg:Currency = -9.99;
begin
  ClearMag;
  ClearSpec;
  feh := invfeh;
  logg := invlogg;
  wdbloat := 1;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
// spectra and absolute magnitude only
function EstimationParser.SetBasic(const avm:Real; in_spectra:string; is_pms:Boolean):Boolean;
var parsok:Boolean;
begin
  // clearing the old
  ClearSpec;
  ClearMag;
  Result := False;
  // setting and parsing the spectra
  stype := in_spectra;
  parsok := MainParse(in_spectra,is_pms);
  if not parsok then Exit;
  // efftempA is based on the spectra
  if not SetTempA then Exit;
  // visual magnitude and luminosity
  if avm < 90 then begin
    absVisMag := avm;
    luminosity := AbsVMagToVLum(absVisMag);
  end;
  // done
  Result := MakeEstimates;
end;
//--------------------------------------------------------
(* a wrapper for SetBasic that converts avm (apparent visual magnitude) to
absolute visual magnitude first (using pllx).  *)
function EstimationParser.SetSimple(const avm,pllx:Real; in_spectra:string; is_pms:Boolean):Boolean;
var absmag:Real;
begin
  // checking for bad input
  Result := False;
  if pllx <= 0 then Exit;
  // bad avm
  if avm >= 90 then Result := SetBasic(avm,in_spectra,is_pms)
  else begin
    // magnitude correction values
    absmag := MakeAbsoluteMagnitude(avm,pllx);
    Result := SetBasic(absmag,in_spectra,is_pms);
  end;
end;
//--------------------------------------------------------
(* works like set simple, but we assume spectra has already been set *)
function EstimationParser.SetSimpleOther(const avm,pllx:Real):Boolean;
begin
  ClearMag;
  Result := False;
  if pllx <= 0 then Exit;
  // visual magnitude and luminosity
  if avm < 90 then begin
    // magnitude correction values
    absVisMag := MakeAbsoluteMagnitude(avm,pllx);
    luminosity := AbsVMagToVLum(absVisMag);
  end;
  // done
  Result := MakeEstimates;
end;
//--------------------------------------------------------
// sets all inputs, calculates estimates
function EstimationParser.SetAll(const avm,pllx:Real; in_spectra:string; is_pms:Boolean; other:StarFluxPlus):Boolean;
var parsok:Boolean;
begin
  // clearing the old
  ClearSpec;
  ClearMag;
  Result := False;
  if other = nil then Exit;
  if pllx <= 0 then Exit;
  // setting and parsing the spectra
  stype := in_spectra;
  parsok := MainParse(in_spectra,is_pms);
  if not parsok then Exit;
  // efftempA is based on the spectra, unless it is manually entered...
  SetTempA;
  Result := SetOther(avm,pllx,other);
end;
//------------------------------------------
// when using extra data, sets everything except the stuff set from spectra
function EstimationParser.SetOther(const avm,pllx:Real; other:StarFluxPlus):Boolean;
var log_parsec,corr:Real;
    ctempBres:Boolean;
begin
  // checking for bad input
  Result := False;
  if other = nil then Exit;
  if pllx <= 0 then Exit;
  ClearMag;
  // magnitude correction values
  log_parsec := Log10(1000.0/pllx);
  corr := -5*(log_parsec-1);
  // visual magnitude...
  if avm < 90 then begin
    absVisMag := avm + corr;
    luminosity := AbsVMagToVLum(absVisMag);
  end;
  // other magnitudes
  if other.Valid_BlueMagnitude then begin
    absBMag := CurrToReal(other.blue_mag) + corr;
  end;
  if other.Valid_IMagnitude then begin
    absIMag := CurrToReal(other.I_mag) + corr;
  end;
  if other.Valid_JMagnitude then begin
    absJMag := CurrToReal(other.J_mag) + corr;
  end;
  if other.Valid_HMagnitude then begin
    absHMag := CurrToReal(other.H_mag) + corr;
  end;
  if other.Valid_KMagnitude then begin
    absKMag := CurrToReal(other.K_mag) + corr;
    satk := other.K_err > 0.1;
  end;
  // possible temperature override, efftempA should already be set
  if other.HasEffectveTemp then begin
    efftempA := other.EffectiveTemp;
    boltzA := StefanBoltzmann(efftempA)
  end;
  // more data entry
  feh := other.FeHMedian;
  logg := other.logg;
  // calculate efftempB (if possible). This is the non-table lookup temp
  ctempBres := SetTempB;
  Result := MakeEstimates;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// calculation methods
//--------------------------------------
// sets the internal radius estimate based on what I think is best
function EstimationParser.CalcRadiusEst:Boolean;
var rad_est_value:Real;
begin
  Result := False;
  radiusest := -1;
  if errtype <> PS_NOERROR then Exit;
  // L dwarfs...
  if (scolor='L') then begin
    Result := BolometricTempRadius(rad_est_value);
  end
  // bolometric for FGKM subgiants,dwarfs, and subdwarfs
  // bolometric for cool giants as well
  else if HasBolomCorr() then begin
    Result := BolometricTempRadius(rad_est_value);
  end
  // planck visual luminosity for others
  else begin
    Result := BlackbodyRadius(True,rad_est_value);
  end;
  // done
  if Result then radiusest := rad_est_value;
end;

//--------------------------------------
(* since there is more than one way to estimate the radius, this external method
can be used to get estimates of different types *)
function EstimationParser.GetRadiusEst(const typex:Boolean; out radest:Real):Boolean;
begin
  // typex: True is bolometric, False is visual blackbody
  Result := False;
  if typex then Result := BolometricTempRadius(radest)
  else Result := BlackbodyRadius(False,radest);
end;
//--------------------------------------
(* to compare radius extimates, this method makes a string containing all of them *)
function EstimationParser.MakeAllRadiusEstimates:string;
var radest0,radest1:Real;
    estok0,estok1:Boolean;
begin
  estok0 := GetRadiusEst(True,radest0);
  estok1 := GetRadiusEst(True,radest1);
  if estok0 then Result := FloatToStrF(radest0,ffGeneral,3,0) + ' | '
  else Result := '??? | ';
  if estok1 then Result += FloatToStrF(radest1,ffGeneral,3,0) + ' | '
  else Result += '???';
end;
//--------------------------------------
// sets the internal mass estimate, using one method for red dwarves
function EstimationParser.CalcMassEst:Boolean;
var zmassest:Real;
    dwarfmass:Boolean;
begin
  dwarfmass := (scolor='M') or (scolor='L');
  if (scolor = 'K') then dwarfmass := subrange <= 7;
  Result := GetMassEst(dwarfmass,zmassest);
  if Result then massest := zmassest
  else massest := -1;
end;
//--------------------------------------
// gets mass estimate
function EstimationParser.GetMassEst(dwarfm:Boolean; out xmassest:Real):Boolean;
var est1,est2:Real;
    res1,res2:Boolean;
begin
  Result := False;
  // both of the current mass extimation methods are for dwarves
  if lclass = lWhiteDwarf then Result := WhiteDwarfMass(xmassest)
  // test subgiant
  else if lclass = lSubGiant then begin
    Result := SubGiantMassEstimateB(xmassest);
    if (not Result) then Result := SubGiantMassGuesstimate(xmassest);
    if (not Result) then Result := DwarfBolLumToMass(xmassest);
  end
  // intermediate (V-IV or IV-V)
  else if lclass = lDwarfGiant then begin
    res1 := SubGiantMassEstimateB(est1);
    if not res1 then res1 := SubGiantMassGuesstimate(est1);
    res2 := DwarfBolLumToMass(est2);
    if res1 and res2 then xmassest := (est1+est2)/2
    else if res1 then xmassest := est1
    else if res2 then xmassest := est2;
    Result := (res1 or res2);
  end
  // dwarves and subdwarves (low metallicity)
  else if Ord(lclass) >= Ord(lDwarf) then begin
    if dwarfm then begin
      Result := RedDwarfMassFitMann(absKMag,xmassest);
      if (not Result) then Result := RedDwarfMass(xmassest);
      if (not Result) then Result := DwarfBolLumToMass(xmassest);
    end
    else Result := DwarfBolLumToMass(xmassest);
  end;
end;
//--------------------------------------
// calls the estimation methods...
function EstimationParser.MakeEstimates:Boolean;
var radok:Boolean;
begin
  Result := CalcBolometricLuminosity;
  if (not Result) then Exit;
  radok := CalcRadiusEst;
  Result := CalcMassEst;
  Result := (Result and radok);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
// extra output
function EstimationParser.GetMassRadiusBLuminosity:string;
begin
  Result := MassEstimateExport + ',' + RadiusEstimateExport;
  Result += ',' + BoloLuminosityEstExport;
end;
//--------------------------------------------------
function EstimationParser.IsLateDwarf:Boolean;
begin
  Result := False;
  if Ord(lclass) < Ord(lDwarf) then Exit;
  if lclass = lWhiteDwarf then Exit;
  Result := IsLate();
end;
//--------------------------------------------------
function EstimationParser.WriteTest(const inname:string):Boolean;
var part1,part2,outline:string;
    vmkteff,bmago:Real;
    mdwarf:Boolean;
begin
  // special not used cases
  Result := False;
  if not examinopen then Exit;
  if AnsiStartsStr('?',stype) then Exit;
  if bollum < 0 then Exit;
  // starting
  mdwarf := IsLateDwarf();
  if mdwarf and (AbsKMag >= 90) and (AbsVisMag >= 90) then Exit;
  // white dwarf output
  if AnsiStartsStr('D',stype) then begin
    part1 := MakeInfoLineStart(True,bmago,vmkteff);
    part2 := MakeInfoLineWhiteDwarf(bmago-AbsVisMag);
    outline := inname + ',' + part1 + ',' + part2;
  end
  else if ((lclass<>XUnknown) and (Ord(lclass) <= Ord(lLowGiant))) then begin
    part1 := MakeInfoLineStart(True,bmago,vmkteff);
    part2 := MakeInfoLineGiant(bmago);
    outline := inname + ',' + part1 + ',' + part2;
  end else begin
    part1 := MakeInfoLineStart(False,bmago,vmkteff);
    if mdwarf then part2 := MakeInfoLineMDwarf(bmago-AbsVisMag)
    else part2 := MakeInfoLineOther(vmkteff,bmago);
    outline := inname + ',' + part1 + ',' + part2;
  end;
  // writing the data
  if mdwarf then begin
    Writeln(examinfile_m,outline);
    Flush(examinfile_m);
  end
  else if AnsiStartsStr('D',stype) then begin
    Writeln(examinfile_d,outline);
    Flush(examinfile_d);
  end
  else if ((lclass<>XUnknown) and (Ord(lclass) <= Ord(lLowGiant))) then begin
    Writeln(examinfile_g,outline);
    Flush(examinfile_g);
  end
  else begin
    Writeln(examinfile,outline);
    Flush(examinfile);
  end;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
function EstimationParser.DivideMagnitude(const divisor:Real; reest:Boolean):Real;
var offset,vbackup:Real;
begin
  Assert(divisor > 0);
  vbackup := AbsVisMag;
  offset := 2.5*Log10(divisor);
  if AbsVisMag < 90 then begin
    absVisMag += offset;
    luminosity := AbsVMagToVLum(absVisMag);
  end;
  if AbsKMag < 90 then absKMag += offset;
  if AbsBMag < 90 then absBMag += offset;
  if AbsJMag < 90 then absJMag += offset;
  if reest then MakeEstimates;
  Result := vbackup;
end;
//--------------------------------------------
procedure EstimationParser.BackupMS;
begin
  bak_avmag := absVisMag;
  bak_akmag := absKMag;
  bak_bmag := absBMag;
  bak_jmag := absJMag;
  bak_stype := stype;
  bak_tempA := efftempA;
end;
//--------------------------------------------
procedure EstimationParser.RestoreMS(reestimate:Boolean);
begin
  AbsVisMag := bak_avmag;
  if AbsVisMag < 90 then luminosity := AbsVMagToVLum(absVisMag);
  absKMag := bak_akmag;
  absBMag := bak_bmag;
  absJMag := bak_jmag;
  efftempA := bak_tempA;
  stype := bak_stype;
  if reestimate then MakeEstimates;
end;
//--------------------------------------------
procedure EstimationParser.DivideSplit(const divisor:Real; reestimate:Boolean);
begin
  // checks and backups
  Assert(divisor > 0);
  BackupMS;
  // reducing the magnitudes
  DivideMagnitude(divisor,reestimate);
  // removing the 'J' (we assume it is there)
  if (divisor >= 40) or (divisor < 1.3) then ChopJ()
  else stype := AnsiReplaceStr(stype,'J','?');
end;
//--------------------------------------------
function EstimationParser.SecondarySplit(secmag,pllx:Real; reestimate:Boolean):Boolean;
var secamag,temp,newmag,magdiff:Real;
begin
  // checks and backups
  Assert(pllx > 0);
  BackupMS;
  Result := False;
  if AbsVisMag >= 90 then Exit;
  //reducing the magnitude of the primary
  temp := Log10(1000.0 / pllx);
  secamag := secmag - 5*(temp-1);
  FirstMagFromSumSecond(AbsVisMag,secamag,newmag);
  if newmag > secamag then Exit;
  AbsVisMag := newmag;
  luminosity := AbsVMagToVLum(absVisMag);
  // removing the 'J' (we assume it is there)
  magdiff := secamag - newmag;
  if (magdiff>4) or (magdiff<0.3) then ChopJ()
  else stype := AnsiReplaceStr(stype,'J','?');
  // altering K and B
  magdiff := newmag - bak_avmag;
  if AbsKMag < 90 then AbsKMag += magdiff;
  if AbsBMag < 90 then AbsBMag += magdiff;
  if AbsJMag < 90 then AbsJMag += magdiff;
  // redoing the estimates
  if reestimate then MakeEstimates;
  Result := True;
end;
//----------------------------------------------------------------
// when splitting stars for output, the trailing J (if there) should be dropped
function EstimationParser.ChopJ():Boolean;
begin
  Result := False;
  if stype.Length < 2 then Exit;
  if AnsiEndsStr('J?',stype) then begin
    stype := LeftStr(stype,stype.Length-2) + '?';
  end
  else begin
      if AnsiLastChar(stype) <> 'J' then Exit;
      // trimming the last
      stype := LeftStr(stype,stype.Length-1);
  end;
  Result := True;
end;

//================================================================

//----------------------------------------------------------
(* from 'The Solar Neighborhood XXXVII: The Mass-Luminosity Relation for Main
Sequence M Dwarfs' (Benedict+ 2016). Upon closer examination, the Polynomial
Luminosity to Mass relation is flawed, but the reverse double exponential seems
to be better.*)
function RedDwarfMassToMag(const inmass:Real; kmag:Boolean; out outmag:Real):Boolean;
var interm1,interm2:Real;
    const x0 = 0.076;
begin
  Result := False;
  if inmass < 0.055 then Exit;
  if inmass > 0.66 then Exit;
  // computing the base exponent parts
  if kmag then begin
    interm1 := -(inmass-x0)/0.05;
    interm2 := -(inmass-x0)/3.10;
  end else begin
    interm1 := -(inmass-x0)/0.03;
    interm2 := -(inmass-x0)/1.38;
  end;
  // the result
  if kmag then outmag := -11.41 + 1.64*exp(interm1) + 19.81*exp(interm2)
  else outmag := -2.59 + 4.77*exp(interm1) + 16.98*exp(interm2);
  Result := True;
end;
//-------------------------------------------------------------
procedure FillInRedDwarfArrays;
var curmass,xsteps:Currency;
    xlength,curstep:Integer;
    okv,okk:Boolean;
    magv,magk:Real;
begin
  // initial preps
  xsteps := CurrencyDivide(rdmax-rdmin,rdstep);
  xlength := Ceil(CurrToReal(xsteps));
  SetLength(rd_vmag,xlength);
  SetLength(rd_kmag,xlength);
  // pre-loop setup
  curmass := rdmin;
  curstep := 0;
  // the magnitude calculation loop
  while curmass <= rdmax do begin
    // might not be needed...
    if curstep > High(rd_vmag) then begin
      SetLength(rd_vmag,curstep+1);
      SetLength(rd_kmag,curstep+1);
    end;
    // claculating and setting the Mv and Mk
    okv := RedDwarfMassToMag(CurrToReal(curmass),False,magv);
    Assert(okv);
    okk := RedDwarfMassToMag(CurrToReal(curmass),True,magk);
    Assert(okk);
    rd_vmag[curstep] := magv;
    rd_kmag[curstep] := magk;
    // preparing for the next step
    Inc(curstep);
    curmass += rdstep;
  end;
  // done
end;
//--------------------------------------------------------------------------
function RedDwarfMagnitudeToMassLookup(const inmag:Real; kmag:Boolean; out massest:Real):Boolean;
var maxdex:Integer;
    mindex,middex:Integer;
    found:Boolean;
    brighter,dimmer:Boolean;
    massx:Currency;
begin
  Result := False;
  // checking for out of bounds
  if kmag then begin
    if inmag > rd_kmag[0] then Exit;
    if inmag <= rd_kmag[High(rd_kmag)] then Exit;
  end else begin
    if inmag > rd_vmag[0] then Exit;
    if inmag <= rd_vmag[High(rd_kmag)] then Exit;
  end;
  // otherwise, we use a binary search
  mindex := 0;
  maxdex := High(rd_vmag);
  found := False;
  // the binary search loop
  while true do begin
    middex := (maxdex + mindex) div 2;
    // checking
    if kmag then begin
      dimmer := (inmag > rd_kmag[middex]);
      brighter := (inmag <= rd_kmag[middex+1]);
    end else begin
      dimmer := (inmag > rd_vmag[middex]);
      brighter := (inmag <= rd_vmag[middex+1]);
    end;
    // handling the results of our check
    found := not (brighter or dimmer);
    if found then break;
    if dimmer then maxdex := middex-1
    else mindex := middex + 1;
  end;
  // done
  if found then begin
    massx := rdmin + middex*rdstep;
    massest := CurrToReal(massx+rdhstep);
  end;
  Result := found;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function AlonsoMultiStr(const vmkin:Real):string;
var cfeh:Currency;
    ok1,ok2:Boolean;
    temp1,ar1,ar2,teffo:Real;
const startfeh:Currency = 0.4;
      stepfeh:Currency = 0.2;
      endfeh:Currency = -1.2;
begin
  cfeh := startfeh;
  Result := '';
  // alonso
  while (cfeh >= endfeh) do begin
    ok1 := AlonsoBCCalc(vmkin,False,cfeh,ar1);
    ok2 := AlonsoBCCalc(vmkin,True,cfeh,ar2);
    if (not ok1) then Result += ','
    else Result += Trim(FloatToStrF(ar1-vmkin,ffFixed,5,3)) + ',';
    if (not ok2) then Result += ','
    else Result += Trim(FloatToStrF(ar2-vmkin,ffFixed,5,3)) + ',';
    cfeh -= stepfeh;
  end;
  // casagrande
  cfeh := startfeh;
  while (cfeh >= endfeh) do begin
    ok1 := CasagrandeBoloEstK(vmkin,cfeh,temp1);
    if ok1 then ar1 := CasagrandeBoloToBcV(temp1,CasaBO_VmK);
    if (not ok1) then Result += ','
    else Result += Trim(FloatToStrF(ar1,ffFixed,5,3)) + ',';
    cfeh -= stepfeh;
  end;
  // casagrande Teff
  ok1 := CasagrandeTEffEstK(vmkin,0,teffo);
  if ok1 then begin
    Result += Trim(FloatToStrF(teffo,ffFixed,5,0)) + ',';
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++
function StartEstimateFiles():Boolean;
const head1 = 'ID,SpT,LClass,V Mag,K Mag,TEff A,Teff B,Bol Mag 1,BcV 1,Fe/H,Radius,Mass,V-K,V-K TEff';
      headA = 'Cas B-V TEff,Cas V-I TEff, Cas V-K TEff,Cas B-V BCv,Cas V-I BCv,';
      headB = 'Cas V-K BCv,Al B-V TEff,Al V-K TEff,Al V-K BCv,Boy B-V Teff,';
      headC = 'Boy MK TEff,PM V-K TEff,PM V-J Teff,PM V-K BCv,PM V-J BCv,';
      headD = 'HB B-V TEff,HB V-K TEff,HB V-K BCv,HB V-J BCv,Huang B-V TEff,';
      headE = 'Huang V-K TEff,Torres Mass 1,Torres Mass 2,Moya Mass,';
      head_gen = headA + headB + headC + headD + headE;
      head_m = 'C Teff,Teff Diff 2,EPD 2,C BCv,BCv Diff,% BCv Diff,CKB,% BCv Diff 2,CJB';
      head_m2 = 'Casa TEff, Casa EPD,Byj TEff,Byj EPD, DupK Bmag, Dup BCv, DupH Bmag, Dup BC,Mann Mass';
      head_wd = 'Z Mass 1,BCv,Blackbody Radius,ZMass 2';
      head_g = 'Alonso TEff,Alonso BCv,TEff EPD,HB V-K TEff,HB V-J TEff,Huang B-V TEff,Huang V-H TEff,';
      headx = 'BB BoMag,BB BCv,';
var headstr:string;
begin
  Result := False;
  if examinopen = True then Exit;
  // regular stars
  headstr := head1 + ',' + headx + 'SatK,' + head_gen;
  AssignFile(examinfile,'estdata.csv');
  Rewrite(examinfile);
  Writeln(examinfile,headstr);
  // m dwarf (and late k as well)
  AssignFile(examinfile_m,'estdata_m.csv');
  Rewrite(examinfile_m);
  Writeln(examinfile_m,head1 + ',' + head_m + ',' + head_m2);
  // white dwarfs
  AssignFile(examinfile_d,'estdata_d.csv');
  Rewrite(examinfile_d);
  Writeln(examinfile_d,head1 + ',' + headx + head_wd);
  // giant stars
  headstr := head1 + ',' + headx + 'SatK,' + head_g;
  AssignFile(examinfile_g,'estdata_g.csv');
  Rewrite(examinfile_g);
  Writeln(examinfile_g,headstr);
  // finisher
  examinopen := True;
  Result := True;
end;
//--------------------------------------------------------
function EndEstimateFiles():Boolean;
begin
  Flush(examinfile);
  CloseFile(examinfile);
  Flush(examinfile_m);
  CloseFile(examinfile_m);
  Flush(examinfile_d);
  CloseFile(examinfile_d);
  Flush(examinfile_g);
  CloseFile(examinfile_g);
  examinopen := False;
  Result := True;
end;
//================================================================
begin
  FillInRedDwarfArrays;
  examinopen := False;

end.

