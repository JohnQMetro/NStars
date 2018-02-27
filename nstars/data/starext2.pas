unit StarExt2;

{$mode delphi}

interface

uses
  Classes, SysUtils, simbad, df_strings, newlocation, Utilities;

type

StarFluxPlus = class
  protected
    // internal temp
    startemp:Integer;
    simbadtemp:string;
    // internal [Fe/H]
    fehm,fehpm:Currency;
    simbadfeh:string;
    // helper methods
    function XMagToString(const imag:Currency):string;
    function InStrToMag(const instr:string):Currency;
    // property methods (minimal checking, don't abuse)
    function UMGet:string;
    procedure UMSet(value:string);
    function UMValid:Boolean;

    function BMGet:string;
    procedure BMSet(value:string);
    function BMValid:Boolean;
    function BEGet:string;
    procedure BESet(value:string);
    function BMGCombo:string;

    function GAMGet:string;
    procedure GAMSet(value:string);
    function GAValid:Boolean;

    function RMGet:string;
    procedure RMSet(value:string);
    function RMValid:Boolean;

    function IMGet:string;
    procedure IMSet(value:string);
    function IMValid:Boolean;

    function JMGet:string;
    procedure JMSet(value:string);
    function JMValid:Boolean;

    function HMGet:string;
    procedure HMSet(value:string);
    function HMValid:Boolean;

    function KMGet:string;
    procedure KMSet(value:string);
    function KMValid:Boolean;
    function KEGet:string;
    procedure KESet(value:string);
    function KMGCombo:string;

    function GEffTemp:Integer;
    procedure SEffTemp(value:Integer);
    function GHasTemp:Boolean;
    function GEffTempStr:string;
    function GSimbTempI:string;

    function GHasFeH:Boolean;
    function GFeH:Currency;
    function GFeHStr:string;
    function GFeHErrStr:string;
    function GSimbadFeH:string;

    function GLogGStr:string;

    // other helpers
    function StringToMag(const source:string; out dest:Currency):Boolean;
    function SetMagH(const source:Currency; var dest:Currency):Integer;
  public
    // ultraviolet + visual magnitudes
    ultraviolet_mag:Currency;
    blue_mag,blue_err:Currency;
    red_mag:Currency;
    gaia_mag:Currency;
    // infrared magnitudes
    I_mag,J_mag,H_mag,K_mag,K_err:Currency;
    // log g
    logg:Currency;
    // ultraviolet/visual magnitude properties
    property UltravioletMagStr:string read UMGet write UMSet;
    property Valid_UltravioletMag:Boolean read UMValid;
    property BlueMagnitudeStr:string read BMGet write BMSet;
    property Valid_BlueMagnitude:Boolean read BMValid;
    property BlueMag_ErrorStr:string read BEGet write BESet;
    property BlueMag_DisplayStr:string read BMGCombo;
    property GaiaMagnitudeStr:string read GAMGet write GAMSet;
    property Valid_GaiaMagnitude:Boolean read GAValid;
    property RedMagnitudeStr:string read RMGet write RMSet;
    property Valid_RedMagnitude:Boolean read RMValid;

    // infrared magnitude properties
    property I_MagnitudeStr:string read IMGet write IMSet;
    property Valid_IMagnitude:Boolean read IMValid;
    property J_MagnitudeStr:string read JMGet write JMSet;
    property Valid_JMagnitude:Boolean read JMValid;
    property H_MagnitudeStr:string read HMGet write HMSet;
    property Valid_HMagnitude:Boolean read HMValid;
    property K_MagnitudeStr:string read KMGet write KMSet;
    property KMag_ErrorStr:string read KEGet write KESet;
    property Valid_KMagnitude:Boolean read KMValid;

    // temperature properties
    property HasEffectveTemp:Boolean read GHasTemp;
    property EffectiveTemp:Integer read GEffTemp write SEffTemp;
    property EffectiveTempStr:string read GEffTempStr;
    property SimbadTempInfo:string read GSimbTempI;
    // [Fe/H] properties
    property HasFeH:Boolean read GHasFeH;
    property FeHMedianStr:string read GFeHStr;
    property FeHErrStr:string read GFeHErrStr;
    property FeHMedian:Currency read GFeH;
    property SimbadFeHStr:string read GSimbadFeH;
    property LogGStr:string read GLogGStr;

  // constructor
  constructor Create;
  // methods
  function SetFromSimbad(insim:SimbadData; browndwarf:Boolean):Boolean;
  // setting magnitudes with boolean ok or not results
  function SetUMag(source:string):Boolean;
  function SetBMag(source,errsource:string):Boolean;
  function SetGaiaMag(source:string):Boolean;
  function SetRMag(source:string):Boolean;
  function SetIMag(source:string):Boolean;
  function SetJMag(source:string):Boolean;
  function SetHMag(source:string):Boolean;
  function SetKMag(source,errsource:string):Boolean;
  // setting the temperature and feh and logg
  function SetTemp(source:string):Boolean;
  function SetFeHDex(median, errb:string):Boolean;
  function AddFeH(inval:Currency):Boolean;
  function AddFeHRange(inmean,inpm:Currency):Boolean; overload;
  function AddFeHRange(inmean,inpm:string):Boolean; overload;
  function SetLogG(tinval:string):Boolean;
  // to and from strings
  function ToIOString:string;
  function FromIOString(inval:string):Boolean;
  // photometry ?
  function AbsoluteKMag(const pllx:Real; out absk_min,absk_max:Currency):Boolean;
  function UminusB:string;
  function BminusV(vismag:Real):string;
  function BminusVval(const vismag:Real; out mind,maxd:Currency):Boolean;
  function VminusR(vismag:Real):string;
  function VminusRval(vismag:Real; out vmrval:Currency):Boolean;
  function VminusI(vismag:Real):string;
  function VminusIval(vismag:Real; out vmibal:Currency):Boolean;
  function VminusK(vismag:Real):string;
  function VminusKval(const vismag:Real; out mind,maxd:Currency):Boolean;
  function JminusKval(out jmkval:Currency):Boolean;
  function RminusI:string;
  function IminusK:string;
  function JminusH:string;
  function HminusK:string;
  function JminusK:string;
end;

const temp99:Currency = 99.999;
      deffeh:Currency = 9.99;
      lowlogg:Currency = -9.99;

implementation

//=======================================================================
function StarFluxPlus.XMagToString(const imag:Currency):string;
begin
  if imag > 99 then Result := '+99.999'
  else Result := MagToString(imag);
end;
//---------------------------------
function StarFluxPlus.InStrToMag(const instr:string):Currency;
var sc:Integer;
begin
  Val(instr,Result,sc);
  Assert(sc = 0);
  Assert(sc < 100);
  Assert(sc > -100);
end;
//+++++++++++++++++++++++++++++++++++++++++++
// private photometric flux magnitude methods
//---------------------------------------
function StarFluxPlus.UMGet:string;
begin  Result := XMagToString(ultraviolet_mag);   end;
//---------------------------------------
procedure StarFluxPlus.UMSet(value:string);
begin  ultraviolet_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.UMValid:Boolean;
begin  Result := (ultraviolet_mag <= 99);  end;
//-----------------------------------------------------------
function StarFluxPlus.BMGet:string;
begin  Result := XMagToString(blue_mag);   end;
//---------------------------------------
procedure StarFluxPlus.BMSet(value:string);
begin  blue_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.BMValid:Boolean;
begin  Result := (blue_mag <= 99);  end;
//---------------------------------------
function StarFluxPlus.BEGet:string;
begin  Result := CurrToStrF(blue_Err,ffFixed,3);   end;
//---------------------------------------
procedure StarFluxPlus.BESet(value:string);
var prv_err:Currency;  sc:Integer;
begin
  Val(value,prv_err,sc);
  Assert(sc = 0);
  Assert(prv_err >= 0);
  Assert(prv_err<10);
  blue_err := prv_err;
end;
//---------------------------------------
function StarFluxPlus.BMGCombo:string;
begin
  Result := CurrToStrF(blue_mag,ffFixed,3) + ' ± ';
  Result += CurrToStrF(blue_err,ffFixed,3);
end;
//-----------------------------------------------------------
function StarFluxPlus.GAMGet:string;
begin  Result := XMagToString(gaia_mag);   end;
//-----------------------------------------------------------
procedure StarFluxPlus.GAMSet(value:string);
begin  gaia_mag := InStrToMag(value);  end;
//-----------------------------------------------------------
function StarFluxPlus.GAValid:Boolean;
begin  Result := (gaia_mag <= 99);  end;
//-----------------------------------------------------------
function StarFluxPlus.RMGet:string;
begin  Result := XMagToString(red_mag);   end;
//------------------------------------------------------
procedure StarFluxPlus.RMSet(value:string);
begin  red_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.RMValid:Boolean;
begin  Result := (red_mag <= 99);  end;
//-----------------------------------------------------------
function StarFluxPlus.IMGet:string;
begin  Result := XMagToString(i_mag);   end;
//---------------------------------------
procedure StarFluxPlus.IMSet(value:string);
begin  i_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.IMValid:Boolean;
begin  Result := (i_mag <= 99);  end;
//-----------------------------------------------------------
function StarFluxPlus.JMGet:string;
begin  Result := XMagToString(j_mag);   end;
//---------------------------------------
procedure StarFluxPlus.JMSet(value:string);
begin  j_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.JMValid:Boolean;
begin  Result := (j_mag <= 99);  end;
//-----------------------------------------------------------
function StarFluxPlus.HMGet:string;
begin  Result := XMagToString(h_mag);   end;
//---------------------------------------
procedure StarFluxPlus.HMSet(value:string);
begin  h_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.HMValid:Boolean;
begin  Result := (h_mag <= 99);  end;
//-----------------------------------------------------------
function StarFluxPlus.KMGet:string;
begin  Result := XMagToString(k_mag);   end;
//---------------------------------------
procedure StarFluxPlus.KMSet(value:string);
begin  k_mag := InStrToMag(value);  end;
//---------------------------------------
function StarFluxPlus.KMValid:Boolean;
begin  Result := (k_mag <= 99);  end;
//---------------------------------------
function StarFluxPlus.KEGet:string;
begin   Result := CurrToStrF(K_Err,ffFixed,3);  end;
//---------------------------------------
procedure StarFluxPlus.KESet(value:string);
var prv_err:Currency;  sc:Integer;
begin
  Val(value,prv_err,sc);
  Assert(sc = 0);
  Assert(prv_err >= 0);
  Assert(prv_err<10);
  K_Err := prv_err;
end;
//---------------------------------------
function StarFluxPlus.KMGCombo:string;
begin
  Result := CurrToStrF(K_mag,ffFixed,3) + ' ± ';
  Result += CurrToStrF(K_err,ffFixed,3);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function StarFluxPlus.GEffTemp:Integer;
begin  Result := startemp;  end;
//---------------------------------------
procedure StarFluxPlus.SEffTemp(value:Integer);
begin startemp := value;  end;
//---------------------------------------
function StarFluxPlus.GHasTemp:Boolean;
begin  Result := (startemp>0);   end;
//---------------------------------------
function StarFluxPlus.GEffTempStr:string;
begin  Result := IntToStr(startemp);   end;
//---------------------------------------
function StarFluxPlus.GSimbTempI:string;
begin  Result := simbadtemp;  end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function StarFluxPlus.GHasFeH:Boolean;
begin  Result := (fehm < deffeh);  end;
//-----------------------------------
function StarFluxPlus.GFeH:Currency;
begin  Result := fehm;  end;
//-----------------------------------
function StarFluxPlus.GFeHStr:string;
var notneg:Boolean;
begin
  notneg := (fehm >= 0);
  Result := CurrToStrF(fehm,ffFixed,2);
  if notneg then Result := '+' + Result;
end;
//-----------------------------------
function StarFluxPlus.GFeHErrStr:string;
begin
  Result := CurrToStrF(fehpm,ffFixed,2);
end;
//-----------------------------------
function StarFluxPlus.GSimbadFeH:string;
begin   Result := simbadfeh;   end;
//--------------------------------
function StarFluxPlus.GLogGStr:string;
var notneg:Boolean;
begin
  notneg := (logg >= 0);
  Result := CurrToStrF(logg,ffFixed,2);
  if notneg then Result := '+' + Result;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// other helpers
//---------------------------------------
function StarFluxPlus.StringToMag(const source:string; out dest:Currency):Boolean;
var temp:Currency;  sc:Integer;
begin
  Result := False;
  Val(Trim(source),temp,sc);
  if sc<>0 then Exit;
  if temp = temp99 then begin
    Result := True;
    dest := temp;
    Exit;
  end;
  if temp > 40 then Exit;
  if temp < -45 then Exit;
  dest := temp;
  Result := True;
end;
//---------------------------------------
function StarFluxPlus.SetMagH(const source:Currency; var dest:Currency):Integer;
begin
  if source < 99 then begin
    Result := 1;
    dest := source;
  end
  else begin
    Result := 0;
    dest := temp99;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor StarFluxPlus.Create;
begin
  ultraviolet_mag := temp99;
  blue_mag := temp99;
  blue_err := 0;
  red_mag := temp99;
  gaia_mag := temp99;
  i_mag := temp99;
  j_mag := temp99;
  h_mag := temp99;
  k_mag := temp99;
  k_err := 0;
  startemp := 0;
  fehm := deffeh;
  fehpm := 0.00;
  logg := lowlogg;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// methods
function StarFluxPlus.SetFromSimbad(insim:SimbadData; browndwarf:Boolean):Boolean;
var movcount:Integer;
begin
  Result := False;
  movcount := 0;
  if (not browndwarf) then begin
    // copying ultraviolet
    movcount += SetMagH(insim.UltravioletMagnitude,ultraviolet_mag);
    // copying blue
    if SetMagH(insim.BlueMagnitude,blue_mag)=1 then begin
      Inc(movcount);
      blue_err := insim.BlueMagnitudeError;
    end
    else blue_err := 0;
  end;
  // gaia magnitude
  movcount += SetMagH(insim.GaiaGMagnitude,gaia_mag);
  // copying red
  movcount += SetMagH(insim.RedMagnitude,red_mag);
  // copying near infrared
  movcount += SetMagH(insim.InfraredMagnitude,i_mag);
  movcount += SetMagH(insim.JMagnitude,j_mag);
  movcount += SetMagH(insim.HMagnitude,h_mag);
  if SetMagH(insim.KMagnitude,k_mag)=1 then begin
    Inc(movcount);
    k_err := insim.KMagnitudeError;
  end
  else k_err := 0;

  movcount += SetMagH(insim.KMagnitude,k_mag);
  // copying effective temperature
  if (not browndwarf) then begin
    startemp := Round(insim.EffectiveTempAvg);
    // setting the special simbad temperature string
    if (insim.EffTempCount = 0)  then simbadtemp := ''
    else begin
      simbadtemp := insim.EffTempRangeStr;
      if insim.EffTempCount > 2 then begin
        simbadtemp += ' (' + IntToStr(Round(insim.EffectiveTempAvg)) + ') [';
        simbadtemp += IntToStr(insim.EffTempCount) + ']';
      end;
      Inc(movcount);
    end;
  end;
  // not done yet ...
  if insim.HasFeH then begin
    fehm := insim.MedianFeH;
    fehpm := insim.FeHError;
    simbadfeh := insim.MakeFeHString;
  end
  else begin
    fehm := deffeh;
    fehpm := 0.00;
    simbadfeh := '';
  end;
  // Log G
  logg := insim.logg;

  // finally...
  Result := (movcount <> 0);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting magnitudes with boolean ok or not results
//--------------------------------
function StarFluxPlus.SetUMag(source:string):Boolean;
begin  Result := StringToMag(source,ultraviolet_mag);  end;
//--------------------------------
function StarFluxPlus.SetBMag(source,errsource:string):Boolean;
var xbmag,xerr:Currency;
  sc:Integer;
begin
  // converting the magnitude
  Result := StringToMag(source,xbmag);
  if (not Result) then Exit;
  // converting the error
  Result := False;
  Val(errsource,xerr,sc);
  if sc<>0 then Exit;
  if xerr<0 then Exit;
  if xerr>=10 then Exit;
  // final test
  if xbmag = temp99 then begin
    if xerr <> 0 then Exit;
  end;
  // all tests passed
  Result := True;
  blue_mag := xbmag;
  blue_err := xerr;
end;
//--------------------------------
function StarFluxPlus.SetGaiaMag(source:string):Boolean;
begin  Result := StringToMag(source,gaia_mag);  end;
//--------------------------------
function StarFluxPlus.SetRMag(source:string):Boolean;
begin  Result := StringToMag(source,red_mag);  end;
//--------------------------------
function StarFluxPlus.SetIMag(source:string):Boolean;
begin  Result := StringToMag(source,i_mag);  end;
//--------------------------------
function StarFluxPlus.SetJMag(source:string):Boolean;
begin  Result := StringToMag(source,j_mag);  end;
//--------------------------------
function StarFluxPlus.SetHMag(source:string):Boolean;
begin  Result := StringToMag(source,h_mag);  end;
//--------------------------------
function StarFluxPlus.SetKMag(source,errsource:string):Boolean;
var xkmag,xerr:Currency;
  sc:Integer;
begin
  // converting the magnitude
  Result := StringToMag(source,xkmag);
  if (not Result) then Exit;
  // converting the error
  Result := False;
  Val(errsource,xerr,sc);
  if sc<>0 then Exit;
  if xerr<0 then Exit;
  if xerr>=10 then Exit;
  // final test
  if xkmag = temp99 then begin
    if xerr <> 0 then Exit;
  end;
  // all tests passed
  Result := True;
  k_mag := xkmag;
  k_err := xerr;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting the temperature
function StarFluxPlus.SetTemp(source:string):Boolean;
var xtemp,sc:Integer;
begin
  Result := False;
  Val(source,xtemp,sc);
  if sc<>0 then Exit;
  if xtemp < 0 then Exit;
  if xtemp > 400000 then Exit;
  Result := True;
  startemp := xtemp;
end;
//-----------------------------------------------------
function StarFluxPlus.SetFeHDex(median, errb:string):Boolean;
var sc:Integer;
    xfeh,xfeherr:Currency;
begin
  Result := False;
  // converting and checking the median [Fe/H] value
  Val(median,xfeh,sc);
  if sc<>0 then Exit;
  if xfeh >= 10 then Exit;
  if xfeh <= -10 then Exit;
  // converting and checking the [Fe/H] plus or minus
  Val(errb,xfeherr,sc);
  if sc<>0 then Exit;
  if xfeherr<0 then Exit;
  if xfeherr>=10 then Exit;
  (* I've decided not to further reject Fe/H values outside
  the range of sensible values ~ -8 < [Fe/H] < 1  *)
  fehm := xfeh;
  fehpm := xfeherr;
  Result := True;
end;
//------------------------------------
function StarFluxPlus.AddFeH(inval:Currency):Boolean;
var curfehmin,curfehmax:Currency;
    newmin,newmax:Currency;
begin
  // special cases
  // no current feh
  if (not HasFeH) then begin
    fehm := inval;
    Result := True;
  end
  // no feh error
  else if fehpm = 0.0 then begin
    // same value, no change
    if fehm = inval then Result := False
    // we must set up a range
    else begin
      SetMinMaxCurr(fehm,inval,newmin,newmax);
      CurrMinMaxToMedRange(newmin,newmax,fehm,fehpm);
      Result := True;
    end;
  end
  // there is an feh error
  else begin
    curfehmin := fehm - fehpm;
    curfehmax := fehm + fehpm;
    // new value within current range, no change
    if (inval>=curfehmin) and (inval<=curfehmax) then Result := False
    // we must set up a new range
    else begin
      if inval < curfehmin then curfehmin := inval
      else curfehmax := inval;
      CurrMinMaxToMedRange(curfehmin,curfehmax,fehm,fehpm);
      Result := True;
    end;
  end;
end;
//------------------------------------
function StarFluxPlus.AddFeHRange(inmean,inpm:Currency):Boolean; overload;
var curfehmin,curfehmax:Currency;
    newmin,newmax,discard:Currency;
begin
  // special cases
  // no current feh
  if (not HasFeH) then begin
    fehm := inmean;
    fehpm := inpm;
    Result := True;
  end
  // no feh error
  else if fehpm = 0.0 then begin
    newmin := inmean - inpm;
    newmax := inmean + inpm;
    Result := True;
    if (fehm>=newmin) and (fehm<=newmax) then begin
      fehm := inmean;
      fehpm := inpm;
    end
    else begin
      if fehpm < newmin then newmin := fehm
      else newmax := fehm;
      CurrMinMaxToMedRange(newmin,newmax,fehm,fehpm);
    end;
  end
  // there is an feh error
  else begin
    // calculating the various min-maxes
    curfehmin := fehm - fehpm;
    curfehmax := fehm + fehpm;
    newmin := inmean - inpm;
    newmax := inmean + inpm;
    SetMinMaxCurr(curfehmin,newmin,newmin,discard);
    SetMinMaxCurr(curfehmax,newmax,discard,newmax);
    // finishing off with the special range function
    CurrMinMaxToMedRange(newmin,newmax,fehm,fehpm);
    Result := True;
  end;
end;
//------------------------------------
function StarFluxPlus.AddFeHRange(inmean,inpm:string):Boolean; overload;
var curmean,curpm:Currency;
    sc:Integer;
begin
  Result := False;
  // converting to currency
  inmean := Trim(inmean);
  if Length(inmean) = 0 then Exit;
  Val(inmean,curmean,sc);
  if sc<>0 then Exit;
  inmean := Trim(inpm);
  if Length(inpm) = 0 then Exit;
  Val(inpm,curpm,sc);
  if sc<>0 then Exit;
  // calling the currency version of the method to complete
  Result:= AddFeHRange(curmean,curpm);
end;
//-----------------------------------------------------
function StarFluxPlus.SetLogG(tinval:string):Boolean;
var xstr:string;
    sc:Integer;
begin
  Result := False;
  xstr := Trim(tinval);
  if Length(xstr) = 0 then Exit;
  Val(xstr,logg,sc);
  if sc<>0 then Exit;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
// to and from strings
function StarFluxPlus.ToIOString:string;
begin
  Result := UMGet + ';' + BMGet + ';' + BEGet + ';' + RMGet;
  Result += ';' + IMGet + ';' + JMGet + ';' + HMGet + ';' + KMGet;
  Result += ';' + EffectiveTempStr + ';' + SimbadTempInfo + ';';
  Result += GFeHStr + ';' + GFehErrStr + ';' + GSimbadFeH;
  Result += ';' + KEGet + ';' + GLogGStr + ';' + GAMGet;
end;
//--------------------------------------------------
function StarFluxPlus.FromIOString(inval:string):Boolean;
var splitlist:TStringList;
    kerr_val:string;
    loggval:string;
begin
  Result := False;
  splitlist := SplitWithDelim(inval,';',13);
  if splitlist = nil then Exit;
  // GAIA Magnitude
  if splitlist.Count >= 16 then begin
    if (not SetGaiaMag(splitlist[15])) then begin
      FreeAndNil(splitlist);
      Exit;
    end;
  end;
  // logg
  if splitlist.Count >= 15 then loggval := splitlist[14]
  else loggval := '-9.99';
  // k_error value
  if splitlist.Count >= 14 then kerr_val := splitlist[13]
  else kerr_val := '0';
  // loading the magnitudes
  if (not SetUMag(splitlist[0])) then FreeAndNil(splitlist)
  else if (not SetBMag(splitlist[1],splitlist[2])) then FreeAndNil(splitlist)
  else if (not SetRMag(splitlist[3])) then FreeAndNil(splitlist)
  else if (not SetIMag(splitlist[4])) then FreeAndNil(splitlist)
  else if (not SetJMag(splitlist[5])) then FreeAndNil(splitlist)
  else if (not SetHMag(splitlist[6])) then FreeAndNil(splitlist)
  else if (not SetKMag(splitlist[7],kerr_val)) then FreeAndNil(splitlist);
  // past there
  if splitlist = nil then Exit;
  // load temperature stuff
  if (not SetTemp(splitlist[8])) then begin
    splitlist.Free;
    Exit;
  end;
  simbadtemp := splitlist[9];
  // reading Fe/H
  if (not SetFeHDex(splitlist[10],splitlist[11])) then begin
    splitlist.Free;
    Exit;
  end;
  simbadfeh := splitlist[12];
  // done
  splitlist.Free;
  SetLogG(loggval);
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
// photometry ?
//-------------------------
function StarFluxPlus.AbsoluteKMag(const pllx:Real; out absk_min,absk_max:Currency):Boolean;
var realk_min,realk_max:Real;
    absrk_min,absrk_max:Real;
begin
  Result := False;
  if pllx <= 0 then Exit;
  if (not KMValid) then Exit;
  // converting K to a real so we can use it
  realk_min := CurrToReal(K_mag-K_err);
  absrk_min := CalculateAbsMagnitude(realk_min,pllx);
  absk_min := RealToCurr(absrk_min);
  if K_err <> 0 then begin
    realk_max := CurrToReal(K_mag+K_err);
    absrk_max := CalculateAbsMagnitude(realk_max,pllx);
    absk_max := RealToCurr(absrk_max);
  end
  else absk_max := 99;
  Result := True;
end;
//-------------------------
function StarFluxPlus.UminusB:string;
var difval1,difval2,rval:Currency;
    diffstr:string;
begin
  Result := '';
  if (not Valid_UltravioletMag) then Exit;
  if (not Valid_BlueMagnitude) then Exit;
  // we start the calculation
  Result := 'U−B: ';
  if blue_err = 0.0 then begin
    rval := ultraviolet_mag - blue_mag;
    diffstr := CurrToStrF(rval,ffFixed,3);
  end
  else begin
   difval1 := ultraviolet_mag - (blue_mag + blue_err);
   difval2 := ultraviolet_mag - (blue_mag - blue_err);
   diffstr := CurrToStrF(difval1,ffFixed,3) + ' to ';
   diffstr += CurrToStrF(difval2,ffFixed,3);
  end;
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.BminusV(vismag:Real):string;
var difval1,difval2:Currency;
    diffstr1,diffstr2:string;
begin
  Result := '';
  if (not BminusVval(vismag,difval1,difval2)) then Exit;
  Result := 'B−V: ';
  diffstr1 := CurrToStrF(difval1,ffFixed,3);
  if difval2 > 90 then Result += diffstr1
  else begin
    diffstr2 := CurrToStrF(difval2,ffFixed,3);
    if diffstr1 = diffstr2 then Result += diffstr1
    else Result += diffstr1 + ' to ' + diffstr2;
  end;
end;
//-------------------------
function StarFluxPlus.BminusVval(const vismag:Real; out mind,maxd:Currency):Boolean;
var viscurr:Currency;
begin
  Result := False;
  if vismag > 99 then Exit;
  if (not Valid_BlueMagnitude) then Exit;
  viscurr := RealToCurr(vismag);
  // we start the calculation
  Result := True;
  if blue_err = 0.0 then begin
    mind := blue_mag - viscurr;
    maxd := 99.9;
  end
  else begin
    mind := (blue_mag - blue_err) - viscurr;
    maxd := (blue_mag + blue_err) - viscurr;
  end;
end;
//-------------------------
function StarFluxPlus.VminusR(vismag:Real):string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not Valid_RedMagnitude) then Exit;
  if vismag > 99 then Exit;
  // we start...
  Result := 'V−R: ';
  difval := RealToCurr(vismag) - red_mag;
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.VminusRval(vismag:Real; out vmrval:Currency):Boolean;
var viscurr:Currency;
begin
  Result := False;
  if (not Valid_RedMagnitude) then Exit;
  if vismag > 99 then Exit;
  // we start
  viscurr := RealToCurr(vismag);
  Result := True;
  vmrval := viscurr - red_mag;
end;
//-------------------------
function StarFluxPlus.VminusI(vismag:Real):string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not VminusIval(vismag,difval)) then Exit;
  // we start...
  Result := 'V−I: ';
  difval := RealToCurr(vismag) - i_mag;
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.VminusIval(vismag:Real; out vmibal:Currency):Boolean;
var viscurr:Currency;
begin
  Result := False;
  if (not Valid_IMagnitude) then Exit;
  if vismag > 99 then Exit;
  // we start
  viscurr := RealToCurr(vismag);
  Result := True;
  vmibal := viscurr - i_mag;
end;
//-------------------------
function StarFluxPlus.VminusK(vismag:Real):string;
var difval1,difval2:Currency;
    diffstr1,diffstr2:string;
begin
  Result := '';
  if (not VminusKval(vismag,difval1,difval2)) then Exit;
  Result := 'V−K: ';
  diffstr1 := CurrToStrF(difval1,ffFixed,3);
  if difval2 > 90 then Result += diffstr1
  else begin
    diffstr2 := CurrToStrF(difval2,ffFixed,3);
    if diffstr1 = diffstr2 then Result += diffstr1
    else Result += diffstr1 + ' to ' + diffstr2;
  end;
end;
//-------------------------
function StarFluxPlus.VminusKval(const vismag:Real; out mind,maxd:Currency):Boolean;
var viscurr:Currency;
begin
  Result := False;
  if vismag > 99 then Exit;
  if (not Valid_KMagnitude) then Exit;
  viscurr := RealToCurr(vismag);
  // we start the calculation
  Result := True;
  if k_err = 0.0 then begin
    mind := viscurr - k_mag;
    maxd := 99.9;
  end
  else begin
    mind := viscurr - (k_mag + k_err);
    maxd := viscurr - (k_mag - k_err);
  end;
end;
//----------------------------------------------
// high J−K is useful for red stars because it oftern indicates the star is a red giant
function StarFluxPlus.JminusKval(out jmkval:Currency):Boolean;
const high:Currency = 90;
begin
  Result := False;
  if k_mag > high then Exit;
  if j_mag > high then Exit;
  jmkval := j_mag - k_mag;
  Result := True;
end;
//----------------------------------------------
function StarFluxPlus.RminusI:string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not Valid_RedMagnitude) then Exit;
  if (not Valid_IMagnitude) then Exit;
  // we start...
  Result := 'R−I: ';
  difval := red_mag - i_mag;
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.IminusK:string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not Valid_IMagnitude) then Exit;
  if (not Valid_KMagnitude) then Exit;
  // we start...
  Result := 'I−K: ';
  difval := i_mag - k_mag;
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.JminusH:string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not Valid_JMagnitude) then Exit;
  if (not Valid_HMagnitude) then Exit;
  // we start...
  Result := 'J−H: ';
  difval := j_mag - h_mag;
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.HminusK:string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not Valid_HMagnitude) then Exit;
  if (not Valid_KMagnitude) then Exit;
  // we start...
  Result := 'H−K: ';
  difval := h_mag - k_mag;
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//-------------------------
function StarFluxPlus.JminusK:string;
var difval:Currency;  diffstr:string;
begin
  Result :='';
  if (not JminusKval(difval)) then Exit;
  // we start...
  Result := 'J−K: ';
  diffstr := CurrToStrF(difval,ffFixed,3);
  Result += diffstr;
end;
//===========================================================================
end.

