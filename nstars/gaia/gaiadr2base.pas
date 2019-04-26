unit gaiadr2base;

{$mode delphi}

interface

uses
  Classes, SysUtils, Math, DAMath,df_strings, strutils, utilities;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

(* Enum to label Gaia Mag quality *)
GMagQ = (
      GQ_OK, // all mags good, can use G, BP-RP
      GQ_BP, // RP is bad, use BP-G if available
      GQ_RP, // BP is bad, use G-RP if available
      GQ_G,  // BP and RP bad, but G is okay
      GQ_BR, // G is bad, but BP and RP are okay
      GQ_X // other (including all mags bad)
);

(* Holds G, BP, and RP magnitudes, and objects of this class will be attached to
stars directly as well as being used in the GaiaDR2Star class. *)
GaiaDR2Mags = class
  protected
    ratio_exp,ratio_act:Real;
    const defbad:Currency = 99.999;

    function StrToMag(inval:string; var target:Currency):Boolean;
    function StrToMagE(inval:string; var target:Currency):Boolean;
    procedure SetQuality();
    procedure SetRatio();
    function GtoStr:string;
    function BPtoStr:string;
    function RPtoStr:string;
    function vbpmrp:Boolean;
    function mbpmrp:Currency;
    function badrat:Boolean;
  public
    G,BP,RP:Currency; // the core data. note that DR2 G is not the same as DR1 G
    Gerr,BPerr,RPerr:Currency;
    qual:GMagQ;
    // constants
    const Header = 'G;G err;BP;BP err;RP;RPerr';
    const fieldcount = 6;
    // string versions of the magnitudes
    property GString:string read GtoStr;
    property BPString:string read BPtoStr;
    property RPString:string read RPtoStr;
    // other properties
    property ValidBPmRP:Boolean read vbpmrp;
    property BPminRP:Currency read mbpmrp;
    property BadRatio:Boolean read badrat;
    // basic constructors
    constructor Create(); overload;
    constructor Create(Gstr:string; BPstr:string; RPstr:string); overload;
    constructor Create(source:TStringList; startdex:Integer); overload;
    // setting data
    function SetG(source:string):Boolean;
    function SetBP(source:string):Boolean;
    function SetRP(source:string):Boolean;
    function SetFromList(source:TStringList; startdex:Integer):Boolean;
    function SetFromLine(source:string):Boolean;
    function SetFromSource(source:TStringList; startdex:Integer; out bpfe:Double; out rpfe:Double):Boolean;
    procedure ClearG();
    procedure ClearBP();
    procedure ClearRP();
    procedure SetG_C(Gin,GinE:Currency);
    procedure SetBP_C(BPin,BPinE:Currency);
    procedure SetRP_C(RPin,RPinE:Currency);
    // outputting data
    function ToSemiString():string;
    function MakeCopy():GaiaDR2Mags;
    function DisplayData():string;
    // photometry stuff
    function MakeVest(out Vest:Real):Boolean;
end;

(* Hold the various IDS for a star that can come with Gaia Data. *)
GaiaDR2IDs = class
  protected
    procedure ResultHelper(var target:string; const idlabel:string; const id2add:string); overload;
    procedure ResultHelper(var target:string; const idlabel:string; const id2add:QWord); overload;
    function ParseURAT(var uratin:string):Boolean;
  public
    TwoMASS:string;
    Hip:Integer;
    Tycho2:string;
    SDSS9:string;
    GSC23:string;
    PPMXL:QWord;
    ALLWISE:string;
    URAT1:string;
    RAVEDR5:string;
    APASS:Cardinal;
    PS1:QWord;
    // constants
    const Header = '2MASS id;Hipparcos id;Tycho-2 id;SDSS objID;GSC 2.3 id;PPM-XL id;AllWISE id;URAT-1 id;RAVE DR5 id;APASS id;Pan-STARRS 1 objID';
    const fieldcount = 11;
    // to and from one string
    function ToSemiString():string;
    function SetFromList(source:TStringList;startdex:Integer):Boolean;
    function IDStrings(reduced:Boolean):string;
    function SetFromSourceList(source:TStringList; startdex:Integer; maxdist:Single):Boolean;
end;

(* The all important position, parallax, proper motion, and maybe radial velocity *)
GaiaDR2Astrometry = class
  protected
    function DtoStr(const valin:Double; prec:Word):String;
  public
    // positions
    rapos,rapos_err:Double;
    decpos,decpos_err:Double;
    parallax,parallax_err:Double;
    glong,glat:Double;
    // motions
    rapm,rapm_err:Double;
    decpm,decpm_err:Double;
    hasRV:Boolean;
    RV,RVerr:Double;
    // constants
    const Header = 'RA (deg);RA err (mas);Dec (deg);Dec err (mas);Parallax;Pllx Err;PM RA;PM RA err;PM Dec;PM Dec Err;RV;RV err;Gal Long;Gal Lat';
    const fieldcount = 14;

    // methods
    constructor Create;
    // to and from one string
    function ToSemiString():string;
    function SetFromList(source:TStringList;startdex:Integer):Boolean;
    // positioning
    function GetPositionIndexes(out decdex:Integer; out radex:Integer):Integer;
    function GetDistanceTo(const rain:Double; const decin:Double):Double;
    // string summaries for display
    function GetStringPos():string;
    function GetStringParallax():String;
    function GetStringPM():String;
    function GetStringGalPos():string;
    // loading from source
    function SetFromSource(source:TStringList; startdex:Integer; out unit_weight_error:Double):Boolean;
    function SetRVFromSource(source:TStringList; startdex:Integer):Boolean;

end;

(* Extra data that Gaia calculates for a small amount of stars. *)
GaiaDR2Extra = class
  protected
    function DtoStr(const valin:Double):String;
  public
    Teff:Integer;
    extinction,reddening:Double;
    luminosity,radius:Double;
    vartype:string;
    // constants
    const Header = 'TEff;Ext AG;E(BP-RP);Luminosity;Radius;Var Type';
    const fieldcount = 6;

    // methods
    constructor Create;
    function HasData():Boolean;
    function ToSemiString():string;
    function SetFromList(source:TStringList;startdex:Integer):Boolean;
    function SetFromSource(source:TStringList; startdex:Integer):Boolean;
end;

(* Pulling it all together *)
GaiaDR2Star = class
  protected
    is_valid:Boolean;
    procedure ClearContents();
    function gvalid():Boolean;
  public
    gaia_id:QWord;
    ids:GaiaDR2IDs;
    astrometry:GaiaDR2Astrometry;
    mags:GaiaDR2Mags;
    extra:GaiaDR2Extra;
    matched:Boolean;
    permaReject:Boolean;
    (* flags for filtering bad results, as per 'Gaia Data Release 2 - Astrometry'
    Appendix C: 'Selecting Astrometrically clean subsets' *)
    selectionAfail:Boolean;
    selectionBfail:Boolean;
    selectionCfail:Boolean;

    distance:Single; // used when matching by position...
    // constants
    const Header = 'Gaia DR2 id;Matched;Perma-Reject;SelA Fail;SelB Fail;SelC Fail';
    const fieldcount = 6;

    property isValid:Boolean read gvalid;

    constructor Create;
    destructor Destroy; override;
    // basic string i/o
    function GaiaID():string;
    function ToSemiString():string;
    function FromSemiString(const semi_source:string):Boolean;
    function MakeSummaryString():string;
    // setting from the source
    function SetFromSource(source:string; maxdist:Single):Boolean;
end;

function MakePositionIndexes(ra,dec:Double; out decdex:Integer; out radex:Integer):Integer;
function GetLevelForIndex(indexin:Integer):Integer;
function StarHeader():string;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//=========================================================================
function GaiaDR2Mags.StrToMag(inval:string; var target:Currency):Boolean;
begin
  Result := True;
  if inval = '' then target := defbad
  else Result := Str2Curr(inval,target);
end;
//----------------------------------------------------------
function GaiaDR2Mags.StrToMagE(inval:string; var target:Currency):Boolean;
begin
  Result := True;
  if inval = '' then target := 0
  else Result := Str2Curr(inval,target);
end;
//--------------------------------------------------------
procedure GaiaDR2Mags.SetQuality();
var gok1,gok2,bok1,bok2,rok1,rok2:Boolean;
begin
    (* GQ_OK, GQ_BP, GQ_RP, GQ_G, GQ_BR, GQ_X *)
  // reducings things to booleans
  gok1 := (G < 90) and (Gerr < 0.01);
  gok2 := (G < 90) and (Gerr < 0.02);
  bok1 := (BP < 90) and (BPerr < 0.01);
  bok2 := (BP < 90) and (BPerr < 0.02);
  rok1 := (RP < 90) and (RPerr < 0.01);
  rok2 := (RP < 90) and (RPerr < 0.02);
  // setting the 'good quality pick'
  if gok1 and bok1 and rok1 then qual := GQ_OK
  else if gok1 and rok1 then qual := GQ_RP
  else if gok1 and bok1 then qual := GQ_BP
  else if rok1 and bok1 then qual := GQ_BR
  // fallback, only G is good
  else if gok1 and rok2 and ((RPerr < BPerr) or (BP >= 90)) then qual := GQ_RP
  else if gok1 and bok2 and ((BPerr < RPerr) or (RP >= 90))  then qual := GQ_BP
  else if gok1 then qual := GQ_G
  else qual := GQ_X;
end;
//--------------------------------------------------------------
procedure GaiaDR2Mags.SetRatio();
var gmrp,bpmrp:Real;
const exp_coff:array[0..2] of Real = (1.616,0.1341,0.03902);
begin
  if (G > 90) or (BP > 90) or (RP > 90) then begin
    ratio_exp := 0;  ratio_act := 0;
  end else begin
      bpmrp := CurrToReal(BP) - CurrToReal(RP);
      gmrp := CurrToReal(G) - CurrToReal(RP);
      if (gmrp = 0) then ratio_act := 0
      else ratio_act := bpmrp / gmrp;
      if (bpmrp > 4.5) or (bpmrp < 0.6) then ratio_exp := 0
      else ratio_exp := PolEval(bpmrp,exp_coff,3);
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Mags.GtoStr:string;
begin
  if G > 90 then Result := ''
  else Result := MagToString(G,True);
end;
//--------------------------------------
function GaiaDR2Mags.BPtoStr:string;
begin
  if BP > 90 then Result := ''
  else Result := MagToString(BP,True);
end;
//--------------------------------------
function GaiaDR2Mags.RPtoStr:string;
begin
  if RP > 90 then Result := ''
  else Result := MagToString(RP,True);
end;
//--------------------------------------
function GaiaDR2Mags.vbpmrp:Boolean;
begin  Result := (BP < 90) and (RP < 90);  end;
//------------------------------------------
function GaiaDR2Mags.mbpmrp:Currency;
begin  Result := (BP - RP);  end;
//------------------------------------------
function GaiaDR2Mags.badrat:Boolean;
begin
  Result := False;
  if (ratio_exp = 0) then Exit;
  Result := (Abs(ratio_exp-ratio_act) > 0.07);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// basic constructors
constructor GaiaDR2Mags.Create(); overload;
begin
  G := defbad;  BP := defbad;   RP := defbad;
  Gerr := 0;   BPerr := 0;   RPerr := 0;
  qual := GQ_X;
end;
//--------------------------------------------------
constructor GaiaDR2Mags.Create(Gstr:string; BPstr:string; RPstr:string); overload;
begin
  if (not StrToMag(Gstr,G)) then Fail;
  if (not StrToMag(BPstr,BP)) then Fail;
  if (not StrToMag(RPstr,RP)) then Fail;
  Gerr := 0;   BPerr := 0;   RPerr := 0;
  SetQuality();
  SetRatio();
end;
//--------------------------------------------------
constructor GaiaDR2Mags.Create(source:TStringList; startdex:Integer); overload;
begin
  if (not SetFromList(source,startdex)) then Fail
  else begin
    SetQuality(); SetRatio();
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting data
//-------------------------
function GaiaDR2Mags.SetG(source:string):Boolean;
var temp_g:Currency;
begin
  Result := False;
  if (not StrToMag(source,temp_g)) then Exit;
  G := temp_g;  Result := True;
  SetQuality();
  SetRatio();
end;
//-------------------------
function GaiaDR2Mags.SetBP(source:string):Boolean;
var temp_bp:Currency;
begin
  Result := False;
  if (not StrToMag(source,temp_bp)) then Exit;
  BP := temp_bp;  Result := True;
  SetQuality();
  SetRatio();
end;
//-------------------------
function GaiaDR2Mags.SetRP(source:string):Boolean;
var temp_rp:Currency;
begin
  Result := False;
  if (not StrToMag(source,temp_rp)) then Exit;
  RP := temp_rp;  Result := True;
  SetQuality();
  SetRatio();
end;
//-------------------------
function GaiaDR2Mags.SetFromList(source:TStringList; startdex:Integer):Boolean;
var temp_g,temp_ge,temp_bp,temp_bpe,temp_rp,temp_rpe:Currency;
begin
  Result := False;
  // bounds checking
  if (source = nil) or (startdex < 0) then Exit;
  if (startdex+6) > source.Count then Exit;
  // trying to convert to the magnitudes
  if (not StrToMag(source[startdex],temp_g)) then Exit;
  if (not StrToMagE(source[startdex+1],temp_ge)) then Exit;
  if (not StrToMag(source[startdex+2],temp_bp)) then Exit;
  if (not StrToMagE(source[startdex+3],temp_bpe)) then Exit;
  if (not StrToMag(source[startdex+4],temp_rp)) then Exit;
  if (not StrToMagE(source[startdex+5],temp_rpe)) then Exit;
  // if we get here, things are converted
  G := temp_g;
  Gerr := temp_ge;
  BP := temp_bp;
  BPerr := temp_bpe;
  RP := temp_rp;
  RPerr := temp_rpe;
  SetQuality();
  SetRatio();
  Result := True;
end;
//-----------------------------------------------------------------
function GaiaDR2Mags.SetFromLine(source:string):Boolean;
var slist:TStringList;
begin
  Result := False;
  slist := SplitWithDelim(source,';',6);
  if slist = nil then Exit;
  Result := SetFromList(slist,0);
  slist.Free;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Mags.SetFromSource(source:TStringList; startdex:Integer; out bpfe:Double; out rpfe:Double):Boolean;
var tempreal_1,tempreal_2,magerr:Double;
begin
  Result := False;
  if (source = nil) then Exit;
  if (startdex < 0) or ((startdex+6) >= source.Count) then Exit;
  // G magnitude and error are required...
  if not StrToRealBoth(source[startdex],source[startdex+1],tempreal_2,tempreal_1) then Exit;
  if (tempreal_2 <= 0) then Exit;
  G := RealToCurr(tempreal_1);
  // Calculating G magnitude error, which might not be symmetric, but that's good enough
  magerr := Abs(2.5*log10(1.0 - 1/tempreal_2));
  // errors under 0.001 mag are actualy fairly common...
  Gerr := RealToCurr(magerr);
  // Blue magnitude and error
  if source[startdex+3] <> '' then begin
    if not StrToRealBoth(source[startdex+2],source[startdex+3],tempreal_2,tempreal_1) then Exit;
    if (tempreal_2 <= 0) then Exit;
    bpfe := tempreal_2;
    BP := RealToCurr(tempreal_1);
    magerr := Abs(2.5*log10(1.0 - 1/tempreal_2));
    BPerr := RealToCurr(magerr);
  end else begin
    BP := 99.999;  BPerr := 0;
    bpfe := -1;
  end;
  // Red magnitude and error
  if source[startdex+5] <> '' then begin
    if not StrToRealBoth(source[startdex+4],source[startdex+5],tempreal_2,tempreal_1) then Exit;
    if (tempreal_2 <= 0) then Exit;
    rpfe := tempreal_2;
    RP := RealToCurr(tempreal_1);
    magerr := Abs(2.5*log10(1.0 - 1/tempreal_2));
    RPerr := RealToCurr(magerr);
  end else begin
    RP := 99.999;  RPerr := 0;
    rpfe := -1;
  end;
  // done
  SetQuality();
  SetRatio();
  Result := True;
end;
//-----------------------------
procedure GaiaDR2Mags.ClearG();
begin
  G := 99.999;  Gerr := 0;
  SetQuality();
  SetRatio();
end;
//-----------------------------
procedure GaiaDR2Mags.ClearBP();
begin
  BP := 99.999;  BPerr := 0;
  SetQuality();
  SetRatio();
end;
//-----------------------------
procedure GaiaDR2Mags.ClearRP();
begin
  RP := 99.999;  RPerr := 0;
  SetQuality();
  SetRatio();
end;
//-----------------------------
procedure GaiaDR2Mags.SetG_C(Gin,GinE:Currency);
begin
  G := Gin;
  Gerr := GinE;
  SetQuality();
  SetRatio();
end;
//-----------------------------
procedure GaiaDR2Mags.SetBP_C(BPin,BPinE:Currency);
begin
  BP := BPin;
  BPerr := BPinE;
  SetQuality();
  SetRatio();
end;
//-----------------------------
procedure GaiaDR2Mags.SetRP_C(RPin,RPinE:Currency);
begin
  RP := RPin;
  RPerr := RPinE;
  SetQuality();
  SetRatio();
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// outputting data
function GaiaDR2Mags.ToSemiString():string;
begin
  Result := GtoStr() + ';' + Trim(CurrToStrF(Gerr,ffFixed,4)) + ';';
  Result += BPtoStr() + ';' + Trim(CurrToStrF(BPerr,ffFixed,4)) + ';';
  Result += RPtoStr() +';' + Trim(CurrToStrF(RPerr,ffFixed,4));
end;
//----------------------------------------
function GaiaDR2Mags.MakeCopy():GaiaDR2Mags;
begin
  Result := GaiaDR2Mags.Create;
  Result.G := G;  Result.Gerr := Gerr;
  Result.BP := BP;  Result.BPerr := BPerr;
  Result.RP := RP;  Result.RPerr := RPerr;
  Result.qual := qual;
end;
//----------------------------------------
function GaiaDR2Mags.DisplayData():string;
var bpmrp:Currency;
begin
  Result := '';
  if G < 90 then Result += 'G: ' + MagToString(G,False);
  if (BP < 90) and (RP < 90) then begin
    bpmrp := BP - RP;
    if Result <> '' then Result += '  ';
    Result += 'BP-RP: ' + MagToString(bpmrp,False);
  end;
end;
//---------------------------------------------------------------
function GaiaDR2Mags.MakeVest(out Vest:Real):Boolean;
var bpmrp,bpmg,interm:Real;
const coffs:array[0..2] of Real = (-0.0176,-0.00686,-0.1732);
      coffsB:array[0..2] of Real = (-0.2084,0.92954,0.022026);
begin
  Result := False;
  if (G >= 90) then Exit;
  if (BP >= 90) then Exit;
  bpmg := CurrToReal(BP)-CurrToReal(G);
  if (bpmg >= 0.867) and (bpmg <= 3.61) then begin
    interm := PolEval(bpmg,coffsB,3);
    Vest := CurrToReal(G) + interm;
    Result := True;
    Exit;
  end;
  if not ValidBPmRp then Exit;
  bpmrp := CurrToReal(BPminRP);
  if (bpmrp <= -0.5) or (bpmrp >= 2.75) then Exit;
  interm := PolEval(bpmrp,coffs,3);
  Vest := CurrToReal(G) - interm;
  Result := True;
end;
//=========================================================================
procedure GaiaDR2IDs.ResultHelper(var target:string; const idlabel:string; const id2add:string);
begin
  if id2add = '' then Exit;
  if target <> '' then target += ', ';
  target += idlabel + ' ' + id2add;
end;
//------------------------------------
procedure GaiaDR2IDs.ResultHelper(var target:string; const idlabel:string; const id2add:QWord);
begin
  if id2add = 0 then Exit;
  if target <> '' then target += ', ';
  target += idlabel + ' ' + IntToStr(id2add);
end;
//-----------------------------------
// the URAT 1 id in the downloaded DR2 data does not match the usual format
function GaiaDR2IDs.ParseURAT(var uratin:string):Boolean;
var workstr1,workstr2:string;
begin
  Result := False;
  if not AnsiStartsStr('URAT1-',uratin) then Exit;
  workstr1 := RightStr(uratin,Length(uratin)-6);
  if (Length(workstr1) < 7) then Exit;
  workstr2 := RightStr(workstr1,6);
  workstr1 := LeftStr(workstr1,Length(workstr1)-6);
  if (Length(workstr1) < 3) then workstr1 := '0' + workstr1;
  if (Length(workstr1) < 3) then workstr1 := '0' + workstr1;
  uratin := workstr1 + '-' + workstr2;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// to and from one string
function GaiaDR2IDs.ToSemiString():string;
begin
  Result := TwoMass + ';';
  if (Hip = 0) then Result += ';'
  else Result += IntToStr(Hip) + ';';
  Result += Tycho2 + ';';
  Result += SDSS9 + ';' + GSC23 + ';';
  if (PPMXL = 0) then Result += ';'
  else Result += IntToStr(PPMXL) + ';';
  Result += ALLWISE + ';' + URAT1 + ';';
  Result += RAVEDR5 + ';';
  if (APASS = 0) then Result += ';'
  else Result += IntToStr(APASS) + ';';
  if (PS1 <> 0) then Result += IntToStr(PS1);
end;
//----------------------------------------------
function GaiaDR2IDs.SetFromList(source:TStringList;startdex:Integer):Boolean;
var tempPPM,tempPS1:QWord;
    tempHip:Integer;
    tempApass:LongWord;
begin
  Result := False;
  if (source = nil) or (startdex < 0) then Exit;
  if (startdex + fieldcount) >= source.Count then Exit;
  // trying to convert Hipparcos
  if source[startdex+1] = '' then tempHip := 0
  else if (not TryStrToInt(source[startdex+1],tempHip)) then Exit;
  // trying to convert PPMXL
  if source[startdex+5] = '' then tempPPM := 0
  else if (not TryStrToQWord(source[startdex+5],tempPPM)) then Exit;
  // trying to convert APASS
  if source[startdex+9] = '' then tempApass := 0
  else if (not TryStrToDWord(source[startdex+9],tempApass)) then Exit;
  // trying to convert Pan-STARRS 1
  if source[startdex+10] = '' then tempPS1 := 0
  else if (not TryStrToQWord(source[startdex+10],tempPS1)) then Exit;
  // if we get here, everyting is okay
  TwoMass := source[startdex];
  Hip := tempHip;
  Tycho2 := source[startdex+2];
  SDSS9 := source[startdex+3];
  GSC23 := source[startdex+4];
  PPMXL := tempPPM;
  ALLWISE := source[startdex+6];
  URAT1 := source[startdex+7];
  RAVEDR5 := source[startdex+8];
  APASS := tempApass;
  PS1 := tempPS1;
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2IDs.IDStrings(reduced:Boolean):string;
begin
  Result := '';
  // reduced options first
  ResultHelper(Result,'Hip',Hip);
  ResultHelper(Result,'Tyc',Tycho2);
  ResultHelper(Result,'2MASS',TwoMass);
  // now non-reduced
  if not reduced then begin
    ResultHelper(Result,'SDSSobjID',SDSS9);
    ResultHelper(Result,'GSC2.3',GSC23);
    ResultHelper(Result,'PPMXL',PPMXL);
    ResultHelper(Result,'WISEA',ALLWISE);
    ResultHelper(Result,'URAT1',URAT1);
    ResultHelper(Result,'RAVE',RAVEDR5);
    ResultHelper(Result,'APASS',APASS);
    ResultHelper(Result,'PS1',PS1);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2IDs.SetFromSourceList(source:TStringList; startdex:Integer; maxdist:Single):Boolean;
var dist:Single;
    index,hiptemp:Integer;
    bigntemp:QWord;
    uratalt:string;
begin
  Result := False;
  // basic reject conditions
  if (source = nil) or (maxdist < 0) then Exit;
  if (startdex < 0) or ((startdex + 2*fieldcount) > source.Count) then Exit;
  // tycho-2
  index := startdex;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then Tycho2 := source[index];
  end else Tycho2 := '';
  // 2MASS
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then TwoMass := 'J' + source[index];
  end else TwoMass := '';
  // Hipparcos
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToInt(source[index],hiptemp) then Exit;
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then Hip := hiptemp;
  end else Hip := 0;
  // AllWISE
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then ALLWISE := source[index];
  end else ALLWISE := '';
  // APASS
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToQWord(source[index],bigntemp) then Exit;
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then APASS := bigntemp;
  end else APASS := 0;
  // Guide Star Catalog 2.3
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then GSC23 := source[index];
  end else GSC23 := '';
  // Pan-STARRS DR 1
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToQWord(source[index],bigntemp) then Exit;
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then PS1 := bigntemp;
  end else PS1 := 0;
  // PPMXL
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToQWord(source[index],bigntemp) then Exit;
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then PPMXL := bigntemp;
  end else PPMXL := 0;
  // RAVE DR5
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then RAVEDR5 := source[index];
  end else RAVEDR5 := '';
  // SDSS DR9 object id
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    if dist <= maxdist then SDSS9 := source[index];
  end else SDSS9 := '';
  // URAT 1
  index += 2;
  if source[index] <> '' then begin
    if not TryStrToFloat(source[index+1],dist) then Exit;
    uratalt := source[index];
    if not ParseURAT(uratalt) then Exit;
    if dist <= maxdist then URAT1 := uratalt;
  end else URAT1 := '';
  // done
  Result := True;
end;
//============================================================================
// methods
function GaiaDR2Astrometry.DtoStr(const valin:Double; prec:Word):String;
begin
  Result := Trim(FloatToStrF(valin,ffFixed,32,prec));
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor GaiaDR2Astrometry.Create;
begin
  hasRV := False;
  RV := 0;
  RVerr := 0;
  // all other variables must be set by the parsing, or the object is invalid
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// to and from one string
//--------------------------------------------------
function GaiaDR2Astrometry.ToSemiString():string;
begin
  // positions
  Result := Real2StrZ(rapos,7,3) + ';' + DtoStr(rapos_err,3) + ';';
  Result += Real2Str(decpos,7,2,True,True) + ';' + DtoStr(decpos_err,3) + ';';
  Result += DtoStr(parallax,4) + ';' + DtoStr(parallax_err,4) + ';';
  // proper motions
  Result += DtoStr(rapm,3) + ';' + DtoStr(rapm_err,3) + ';';
  Result += DtoStr(decpm,3) + ';' + DtoStr(decpm_err,3) + ';';
  // radial velocity
  if (not hasRV) then Result += ';;'
  else begin
    Result += DtoStr(RV,2) + ';' + DtoStr(RVerr,2) + ';';
  end;
  // galactic longtitude and latitude
  Result += Real2StrZ(glong,7,3) + ';' + Real2Str(glat,7,2,True,True);
end;
//--------------------------------------------------
function GaiaDR2Astrometry.SetFromList(source:TStringList;startdex:Integer):Boolean;
var temps:Array of Double;
begin
  Result := False;
  // quick discard conditions
  if (source = nil) or (startdex < 0) then Exit;
  if (startdex + fieldcount) >= source.Count then Exit;
  (* Temp values avoid overwriting the object fields until everything is okay *)
  SetLength(temps,fieldcount-2);
  // positions
  if not StrToRealBoth(source[startdex],source[startdex+1],temps[0],temps[1]) then Exit;
  if not StrToRealBoth(source[startdex+2],source[startdex+3],temps[2],temps[3]) then Exit;
  if not StrToRealBoth(source[startdex+4],source[startdex+5],temps[4],temps[5]) then Exit;
  // proper motions
  if not StrToRealBoth(source[startdex+6],source[startdex+7],temps[6],temps[7]) then Exit;
  if not StrToRealBoth(source[startdex+8],source[startdex+9],temps[8],temps[9]) then Exit;
  // radial velocities
  hasRV := (source[startdex+10] <> '');
  if hasRV then begin
    if not StrToRealBoth(source[startdex+10],source[startdex+11],RV,RVerr) then Exit;
  end else begin
    RV := 0;  RVerr := 0;
  end;
  // galactic longtitude and latitude
  if not StrToRealBoth(source[startdex+12],source[startdex+13],glong,glat) then Exit;
  // copying over the remaining values
  rapos := temps[0];
  rapos_err := temps[1];
  decpos := temps[2];
  decpos_err := temps[3];
  parallax := temps[4];
  parallax_err := temps[5];
  // motions
  rapm := temps[6];
  rapm_err := temps[7];
  decpm := temps[8];
  decpm_err := temps[9];
  // done
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Astrometry.GetPositionIndexes(out decdex:Integer; out radex:Integer):Integer;
begin
  Result := MakePositionIndexes(rapos,decpos,decdex,radex);
end;
//------------------------------------------------------------------
function GaiaDR2Astrometry.GetDistanceTo(const rain:Double; const decin:Double):Double;
var interm1,interm2,interm3:Real;
begin
  // special case
  if (rain = rapos) and (decin = decpos) then begin
    Result := 0;
    Exit;
  end;
  // intermediate calaculations (equation is from Wikipedia)
  interm1 := sind(decin)*sind(decpos);
  interm2 := cosd(decin)*cosd(decpos);
  interm3 := cosd(rain-rapos);
  interm1 := interm1 + interm2*interm3;
  // finishing off
  Result := arccosd(interm1);
  Result := 60*result;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// string summaries for display
//----------------------------------------
function GaiaDR2Astrometry.GetStringPos():string;
begin
  Result := 'RA: ' + DtoStr(rapos,4) + '°, Dec: ';
  Result += DtoStr(decpos,4) + '°';
end;
//----------------------------------------
function GaiaDR2Astrometry.GetStringParallax():String;
begin
  Result := 'Pllx: ' + DtoStr(parallax,2) + '±' + DtoStr(parallax_err,2) + ' mas';
end;
//----------------------------------------
function GaiaDR2Astrometry.GetStringPM():String;
begin
  Result := 'PM RA: ' + DtoStr(rapm,1) + ', PM Dec: ' + DtoStr(decpm,1);
end;
//----------------------------------------
function GaiaDR2Astrometry.GetStringGalPos():string;
begin
  Result := 'Gal Long: ' + DtoStr(glong,2) + '°, Lat: ';
  Result += DtoStr(glat,2) + '°';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// loading from source
function GaiaDR2Astrometry.SetFromSource(source:TStringList; startdex:Integer; out unit_weight_error:Double):Boolean;
var index,nobs:Integer;
    chi_square:Double;
begin
  Result := False;
  if (source = nil) then Exit;
  if (startdex < 0) or ((startdex + 14) >= source.Count) then Exit;
  // we start converting: Right Ascension
  index := startdex;
  if not StrToRealBoth(source[index],source[index+1],rapos,rapos_err) then Exit;
  // Declination
  index += 2;
  if not StrToRealBoth(source[index],source[index+1],decpos,decpos_err) then Exit;
  // Parallax
  index += 2;
  if not StrToRealBoth(source[index],source[index+1],parallax,parallax_err) then Exit;
  // RA Proper Motion
  index += 2;
  if not StrToRealBoth(source[index],source[index+1],rapm,rapm_err) then Exit;
  // Dec Proper Motion
  index += 2;
  if not StrToRealBoth(source[index],source[index+1],decpm,decpm_err) then Exit;
  // galactic longtitude and latitude
   index += 2;
  if not StrToRealBoth(source[index],source[index+1],glong,glat) then Exit;
  // number of observations and chi square...
  index += 2;
  if not TryStrToInt(source[index],nobs) then Exit;
  if not TryStrToFloat(source[index+1],chi_square) then Exit;
  if (nobs < 6) or (chi_square < 0) then Exit;
  // calculating the unit weight error
  unit_weight_error := Sqrt(chi_square/(nobs-5));
  Result := True;
end;
//--------------------------------------------------
function GaiaDR2Astrometry.SetRVFromSource(source:TStringList; startdex:Integer):Boolean;
begin
  Result := False;
  if (source = nil) then Exit;
  if (startdex < 0) or ((startdex + 2) >= source.Count) then Exit;
  // RV might be contained in two fields
  if source[startdex] <> '' then begin
     if not StrToRealBoth(source[startdex],source[startdex+1],RV,Rverr) then Exit;
     hasRV := True;
  end else begin
    hasRV := False;
    RV := 0; RVerr := 0;
  end;
  Result := True;
end;
//================================================================================
function GaiaDR2Extra.DtoStr(const valin:Double):String;
begin
  Result := Trim(FloatToStrF(valin,ffFixed,22,4));
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor GaiaDR2Extra.Create;
begin
  Teff := 0;
  extinction := -1;   reddening := -1;
  luminosity := 0;    radius := 0;
  vartype := '';
end;
//------------------------------------------
function GaiaDR2Extra.HasData():Boolean;
begin
  Result := True;
  if (teff > 0) then Exit;
  if (extinction > 0) or (reddening > 0) then Exit;
  if (luminosity > 0) or (radius > 0) then Exit;
  Result := (vartype <> '');
end;
//------------------------------------------
function GaiaDR2Extra.ToSemiString():string;
begin
  if (teff <= 0) then Result := ';'
  else Result := IntToStr(teff) + ';';
  if (extinction <= -1) then Result += ';'
  else Result += DtoStr(extinction) + ';';
  if (reddening <= -1) then Result += ';'
  else Result += DtoStr(reddening) + ';';
  if (luminosity <= 0) then Result += ';'
  else Result += DtoStr(luminosity) + ';';
  if (radius <= 0) then Result += ';'
  else Result += DtoStr(radius) + ';';
  Result += vartype;
end;
//------------------------------------------
function GaiaDR2Extra.SetFromList(source:TStringList;startdex:Integer):Boolean;
var temp_teff:Integer;
    temp_ext,temp_redd:Double;
    temp_lum,temp_rad:Double;
begin
  Result := False;
  if (source = nil) or (startdex < 0) then Exit;
  if (startdex + fieldcount) < source.Count then Exit;
  // we have enough items, so starting to parse...
  if (source[startdex] = '') then temp_teff := 0
  else if not TryStrToInt(source[startdex],temp_teff) then Exit;
  if (source[startdex+1] = '') then temp_ext := -1
  else if not StrToReal(source[startdex+1],temp_ext) then Exit;
  if (source[startdex+2] = '') then temp_redd := -1
  else if not StrToReal(source[startdex+2],temp_redd) then Exit;
  if (source[startdex+3] = '') then temp_lum := 0
  else if not StrToReal(source[startdex+3],temp_lum) then Exit;
  if (source[startdex+4] = '') then temp_rad := 0
  else if not StrToReal(source[startdex+4],temp_rad) then Exit;
  // if we get here, then things are okay
  Teff := temp_teff;
  extinction := temp_ext;
  reddening := temp_redd;
  luminosity := temp_lum;
  radius := temp_rad;
  vartype := source[startdex+5];
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Extra.SetFromSource(source:TStringList; startdex:Integer):Boolean;
var tempreal:Single;
begin
  Result := False;
  if (source = nil) then Exit;
  if (startdex < 0) or ((startdex+8) >= source.Count) then Exit;
  (* Checking variable type first. Gaia makes checking a bit complicated because
  the varibale data is speread. Specific types are BY Draconis and 'Delta Scuti
  + SX Phoenicis' type (there are others, but those are generally distant luminous
  types like RR Lyrae and Cepheids).*)
  if source[startdex] = 'VARIABLE' then begin
    if source[startdex+1] <> '' then vartype := source[startdex+1]
    else if (source[startdex+2] <> '') then vartype := 'BYDra'
    else vartype := 'Variable';
  end else vartype := '';
  // TEff
  if source[startdex+3] = '' then Teff := 0
  else begin
    if not TryStrToFloat(source[startdex+3],tempreal) then Exit;
    Teff := round(tempreal);
  end;
  // G-band extinction
  if source[startdex+4] = '' then extinction := -1
  else if not TryStrToFloat(source[startdex+4],extinction) then Exit;
  // BP-RP reddening
  if source[startdex+5] = '' then reddening := -1
  else if not TryStrToFloat(source[startdex+5],reddening) then Exit;
  // radius
  if source[startdex+6] = '' then radius := 0
  else if not TryStrToFloat(source[startdex+6],radius) then Exit;
  // luminosity
  if source[startdex+7] = '' then luminosity := 0
  else if not TryStrToFloat(source[startdex+6],luminosity) then Exit;
  // done
  Result := True;
end;
//========================================================================
procedure GaiaDR2Star.ClearContents();
begin
  FreeAndNil(ids);
  FreeAndNil(astrometry);
  FreeAndNil(mags);
  FreeAndNil(extra);
  is_valid := False;
  permaReject := False;
  selectionAfail := False;
  selectionBfail := False;
  selectionCfail := False;
  gaia_id := 0;
  distance := 0.0;
end;
//-----------------------------------------
function GaiaDR2Star.gvalid():Boolean;
begin   Result := is_valid;   end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor GaiaDR2Star.Create;
begin
  ClearContents();
end;
//------------------------------
destructor GaiaDR2Star.Destroy;
begin
  ClearContents();
  inherited;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Star.GaiaID():string;
begin
  Result := 'Gaia DR2 ' + IntToStr(gaia_id);
end;
//----------------------------------------------------
function GaiaDR2Star.ToSemiString():string;
begin
  Result := '';
  if (not is_valid) then Exit;
  Result += IntToStr(gaia_id) + ';';
  Result += IfThen(matched,'T','F') + ';';
  Result += IfThen(permaReject,'T','F') + ';';
  Result += IfThen(selectionAfail,'T','F') + ';';
  Result += IfThen(selectionBfail,'T','F') + ';';
  Result += IfThen(selectionCfail,'T','F') + ';';
  Result += ids.ToSemiString() + ';';
  Result += astrometry.ToSemiString() + ';';
  Result += mags.ToSemiString();
  if (extra = nil) then Result += StringOfChar(';',GaiaDR2Extra.fieldcount)
  else Result += ';' + extra.ToSemiString();
end;

//------------------------------
function GaiaDR2Star.FromSemiString(const semi_source:string):Boolean;
var xlist:TStringList;
    fcount,fmax:Integer;
    tempids:GaiaDR2IDs;
    tempastro:GaiaDR2Astrometry;
    tempmags:GaiaDR2Mags;
    tempextra:GaiaDR2Extra;
begin
  // preliminary splitting
  Result := False;
  fmax := fieldcount + GaiaDR2IDs.fieldcount + GaiaDR2Astrometry.fieldcount;
  fmax += GaiaDR2Mags.fieldcount + GaiaDR2Extra.fieldcount;
  xlist := SplitWithDelim(semi_source,';',fmax);
  if (xlist = nil) then Exit;
  ClearContents();
  // individual parsing : gaia id
  if not TryStrToQWord(xlist[0],gaia_id) then begin
    xlist.Free;    Exit;
  end;
  // matched and permaReject flags
  matched := (xlist[1] = 'T');
  permaReject := (xlist[2] = 'T');
  // the selection failures
  selectionAfail := (xlist[3] = 'T');
  selectionBfail := (xlist[4] = 'T');
  selectionCfail := (xlist[5] = 'T');

  // IDS
  tempids := GaiaDR2IDS.Create;
  if (not tempids.SetFromList(xlist,fieldcount)) then begin
    xlist.Free;  tempids.Free;
    Exit;
  end;
  fcount := fieldcount + GaiaDR2IDS.fieldcount;
  // Astrometry
  tempastro := GaiaDR2Astrometry.Create;
  if (not tempastro.SetFromList(xlist,fcount)) then begin
    xlist.Free;
    tempids.Free;  tempastro.Free;
    Exit;
  end;
  fcount += GaiaDR2Astrometry.fieldcount;
  // Magnitudes
  tempmags := GaiaDR2Mags.Create(xlist,fcount);
  if (tempmags = nil) then begin
    xlist.Free;
    tempids.Free;  tempastro.Free;
    Exit;
  end;
  fcount += GaiaDR2Mags.fieldcount;
  // Extra Stuff
  tempextra := GaiaDR2Extra.Create;
  if (not tempextra.SetFromList(xlist,fcount)) then begin
    xlist.Free;
    tempids.Free;  tempastro.Free;
    tempmags.Free;   tempextra.Free;
    Exit;
  end;
  xlist.Free;
  // copying the created data over
  ids := tempids;
  astrometry := tempastro;
  mags := tempmags;
  if tempextra.HasData() then extra := tempextra
  else tempextra.Free;
  // done
  is_valid := True;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Star.MakeSummaryString():string;
var vest:Real;
begin
  Result := '';
  if not is_valid then Exit;
  if distance > 0 then Result += Trim(FloatToStrF(distance,ffFixed,7,3)) + ' | ';
  Result += GaiaID() + ' | ' + mags.DisplayData();
  if mags.MakeVest(vest) then begin
    Result += ', Vest: ' + Trim(FloatToStrF(vest,ffFixed,6,2));
  end;
  Result += sLineBreak;
  Result += IfThen(matched,'Matched','Unmatched') + ' | ';
  if selectionAfail then Result += 'Fails Selection A | ';
  if selectionBfail then Result += 'Fails Selection B | ';
  if selectionCfail then Result += 'Fails Selection C';
  Result += sLineBreak;
  Result += ids.IDStrings(true) + sLineBreak;
  Result += astrometry.GetStringPos() + ' | ' + astrometry.GetStringParallax();
  Result += sLineBreak + astrometry.GetStringPM() + ' | ';
  Result += astrometry.GetStringGalPos();
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting from the source
function GaiaDR2Star.SetFromSource(source:string; maxdist:Single):Boolean;
var mainlist:TStringList;
    unit_weight_error,bpfe,rpfe,bprp_excess:Double;
    interm,interm2:Double;
begin
  Result := False;
  // basic splitting
  mainlist := SplitWithDelim(source,',',54);
  if mainlist = nil then Exit;
  // some setup before detailed parsing
  ClearContents();
  // parsing...
  try
    // source id
    if not TryStrToQWord(mainlist[0],gaia_id) then Exit;
    // astrometry
    astrometry := GaiaDR2Astrometry.Create;
    if not astrometry.SetFromSource(mainlist,1,unit_weight_error) then Exit;
    // photometry
    mags := GaiaDR2Mags.Create;
    if not mags.SetFromSource(mainlist,15,bpfe,rpfe) then Exit;
    // radial velocity
    if not astrometry.SetRVFromSource(mainlist,22) then Exit;
    // extra data
    extra := GaiaDR2Extra.Create;
    if not extra.SetFromSource(mainlist,24) then Exit;
    // extra identifiers
    ids := GaiaDR2ids.Create;
    if not ids.SetFromSourceList(mainlist,32,maxdist) then Exit;
    // 'phot_bp_rp_excess_factor' is the final field to read
    if mainlist[21] = '' then bprp_excess := -1
    else if not TryStrToFloat(mainlist[21],bprp_excess) then Exit;
    // if we get here, things are okay
    Result := True;
    is_valid := True;
    if not extra.HasData() then FreeAndNil(extra);
    // time to do the selection tests...
    // selection A: close, good accuracy in parallax and bp,rp flux
    selectionAfail := astrometry.parallax <= 10;
    if not selectionAfail then begin
      selectionAfail := (astrometry.parallax / astrometry.parallax_err) <= 10;
      if not selectionAfail then selectionAfail := (bpfe < 10) or (rpfe < 10);
    end;
    // selection B: based on magnitude and unit weight error
    interm := -0.2 * (CurrToReal(mags.G) - 19.5);
    interm := 1.2 * Max(1,exp(interm));
    selectionBfail := unit_weight_error >= interm;
    // selection C : 'phot_bp_rp_excess_factor' within a certain range
    // if there is no BP or RP, selection C auto fails
    if (bpfe < 0) or (rpfe < 0) or (bprp_excess < 0) then selectionCfail := True
    else begin
      interm := 1 + 0.015*Sqr(CurrToReal(mags.BP - mags.RP));
      interm2 := 1.3 + 0.006*Sqr(CurrToReal(mags.BP - mags.RP));
      selectionCfail := (bprp_excess <= interm) or (bprp_excess >= interm2);
    end;

  finally
    mainlist.Free;
  end;
end;

//======================================================================
(* we start off with 4 by 4 indexes per square degree. For declination, we add 90
to make sure the decdexes are always positive. For high latitudes, we reduce the
indexes, because when matching stars by position, it seems simpler than trying to
search entire arrays of positions. This means the max match distance is 3.75 arcmin.
level 1: [0 to 60) <- the numbers are the absolute value of the declination
level 2: [60 to 75)
level 3: [75 to 82)
level 4: [82 to 86)
level 5: [86 to 88)
level 6 : 88
level 7 : 89 ( the 'polar caps' get stored as single units)

*)
function MakePositionIndexes(ra,dec:Double; out decdex:Integer; out radex:Integer):Integer;
var level,absdec:Integer;
    radex_base:Double;
begin
  // determining the level
  absdec := Trunc(Abs(dec));
  if (absdec < 60) then level := 1
  else if (absdec < 75) then level := 2
  else if (absdec < 82) then level := 3
  else if (absdec < 86) then level := 4
  else if (absdec < 88) then level := 5
  else if (absdec = 88) then level := 6
  else level := 7;
  // calculating decdex
  decdex := Trunc((dec+90.0)*4.0);
  // right ascension requires special stuff
  if ((level = 7) or (level = 6)) then  radex_base := (ra*4)/32.0
  else radex_base := (ra*4.0)/(intpower(2,level-1));
  radex := Trunc(radex_base);
  // done
  Result := level;
end;
//-----------------------------------------------------------------
// mostly used to determine the level from strip array index
function GetLevelForIndex(indexin:Integer):Integer;
var absdec,level:Integer;
    isneg:Boolean;
begin
  // converting the indexin to equivalent of Abs(Dec)
  Assert((indexin > 0) and (indexin < 720));
  absdec := Abs(indexin - 360);
  if (indexin < 360) then absdec -= 1;
  absdec := absdec div 4;
  // assigning the level based on that
  if (absdec < 60) then level := 1
  else if (absdec < 75) then level := 2
  else if (absdec < 82) then level := 3
  else if (absdec < 86) then level := 4
  else if (absdec < 88) then level := 5
  else if (absdec = 88) then level := 6
  else level := 7;
  Result := Level;
end;
//---------------------------------------------------------------------
function StarHeader():string;
begin
  Result := GaiaDR2Star.Header + ';' + GaiaDR2IDs.Header + ';' + GaiaDR2Astrometry.Header;
  Result += ';' + GaiaDR2Mags.Header + ';' + GaiaDR2Extra.Header;
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.

