unit tgas;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, DAMath, StrUtils,
  df_strings, namedata,simbad,Utilities2;

type

TGASAlterParams = class
  public
    maxdist:Real;
    maxerr:Real;
    offset:Real;
    transform:Integer;
    // methods
    constructor Create;
    function SetNumbersFromText(maxdist_text,maxerr_text,offset_text:string):Boolean;
end;

(* class holding data for a star in the Tycho-Gaia Astrometric Solution *)
TGASData = class
  protected
    dataset:Boolean;
    hip_desig, tycho2_desig:string;  // catalog id/names for hipparchos and Tycho 2 catalogs
    // position (from source)
    ra,raerr:Double;      // right-ascension (deg, milli-arcsecond standard dev)
    dec,decerr:Double;    // Declination     (as above)
    pllx,pllxerr:Double;  // Parallax and parallax error (mas, mas dev)
    // proper motion (radial velocity not included)
    pmra, pmra_error:Double; // right-ascension motion  (mas/year)
    pmdec, pmdec_error:Double; // declination motion (mas/year)

    imported1,imported2:string;

    // boundaries for J2000 (in arcseconds)
    raj, raj_error:Double;
    decj, decj_error: Double;

    function ArcSecToDeg(input:Double):string;
    function J2000RAToHMS:string;
  public
    matched:Boolean;

    constructor Create; overload;
    // input
    function SetFromSource(input:string):Boolean;
    function SetFromReducedSource(input:string):Boolean;
    function DataIsSet:Boolean;
    procedure AddParallaxOffset(addoff:Real);
    // getting finished data
    function GetRA_Arcsec:Double;
    procedure GetRA_HMS(out hours:Integer; out minutes:Integer; out secs:Real);
    function GetDec_Arcsec:Double;
    function GetDecDegrees:Real;
    function GetLY_Distance:Double;
    function GetParallax:Double;
    function GetParallaxError:Double;
    function GetHipparcos:string;
    function GetTycho2:string;
    function GetReduced:string;
    // getting proper motion
    function GetPM_RAmas:Double;
    function GetPM_Decmas:Double;
    procedure GetPMOut(out magnitude:Double; out position_angle:Double);
    // calculating J2000 positions
    function CalcJ2000Pos:Boolean;
    function GetJ2000RAPos:Integer;
    function J2000PosMatch(ra_arcsec,dec_arcsec,tolerance_arcsec:Double):Boolean;
    // final stuff
    function SummaryString:string;
    function SummaryString2:string;
    function NameMatch(namegroup:StarName):Boolean;
end;

TGASList = TFPGObjectList<TGASData>;
TGAS_StringMap = TFPGMap<string,TGASData>;

TGASCollection = class
  protected
    // main list of all of the stars
    list_of_stars:TGASList;
    (* array of lists of stars grouped by right ascension. measurement is by
    deci-degrees, each list includes index -1, index, and index+1 ra positions
    (and thus the lists overlap). This is to accomodate errors in position. *)
    grouped_lists: array[0..3599] of TGASList;
    hip_ptr, tyc_ptr:TGAS_StringMap;

    // unmatched stuff
    unmatched_index:Integer;

    // helper methods
    function InputLineToStar(input_line:string;aparams:TGASAlterParams):TGASData;
    function InputReducedLineToStar(input_line:string; max_distance:Integer):TGASData;
    function InsertStarData(to_insert:TGASData):Boolean;
    function CreateUnMatched(minpllx:Real):Integer;
  public
    unmatched:TGASList;
    // basic constructor
    constructor Create; overload;
    // external methods
    function FindJ2000Matches(ra_arcsec, dec_arcsec:Double):TGASList;
    function FindNameMatches(const inlistn:TStringList):TGASList;
    function SetMatchByID(xid:string; tomatch:Boolean):Boolean;
    procedure ImportFromCSVFile(fnin:TFileName; aparams:TGASAlterParams);
    procedure ImportFromReducedCSVFile(fnin:TFileName);
    function WriteReducedToFile(fnout:TFileName):Boolean;
    function WriteUnmatchedToFile(minpllx:Real; fnout:TFileName):Integer;
    function StarCount:Integer;
    function MakeUnmatched(minpllx:Real):Boolean;
    function NextUnmatched:TGASData;
    // destructor
    destructor Destroy; override;

end;

// TGAS parallax alteration functions...
function EclipticNorth(rapos,decpos:Real):Boolean;
function StassunTorresCorrection(rapos,decpos:Real):Real;

var
    tgas_main:TGASCollection;

const
  ARCSEC_PDEG = 3600.0;
  ARCSEC_CIRCLE = 360*ARCSEC_PDEG;
  ARCSEC_QUARTER = 90*ARCSEC_PDEG;
  INV_THOUSAND = 1.0 / 1000.0;
  MAS_DIST_NUM = 3261.56378;  // divide this by mas-parallax to get LY distance
  tgas_reduced_fields = 'hip,tycho2_id,ra,ra_error,dec,dec_error,parallax,parallax_error,pmra,pmra_error,pmdec,pmdec_error,matched';
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
implementation
//====================================================================
// methods
//-----------------------
constructor TGASAlterParams.Create;
begin
  maxdist := 300.0;
  maxerr := 5;
  offset := 0.24;
  transform := 0;
end;
//-----------------------
function TGASAlterParams.SetNumbersFromText(maxdist_text,maxerr_text,offset_text:string):Boolean;
begin
  Result := False;
  if not StrToRealBoth(maxdist_text,maxerr_text,maxdist,maxerr) then Exit;
  if not StrToReal(offset_text,offset) then Exit;
  Result := True;
end;

//====================================================================
// tries to convert arcsec double to +/- deg arcmin arcsec.sss
function TGASData.ArcSecToDeg(input:Double):string;
var deg,min,mintotal:Integer;
    sectotal,secpart:Integer;
    remainder,seconds:Extended;
    isneg:Boolean;
    tempstr:string;
begin
  isneg := input < 0;
  input := Abs(input);
  // getting the leftover seconds value
  remainder := Frac(input);
  sectotal := Trunc(input);
  secpart := sectotal mod 60;
  seconds := secpart + remainder;
  // getting the minutes
  mintotal := sectotal div 60;
  min := mintotal mod 60;
  // degrees
  deg := mintotal div 60;
  // now that the input is broken up, we produce a string result
  if (isneg) then Result := '-'
  else Result := '+';
  Str(deg,tempstr);
  Result := Result + tempstr + ' ';
  Str(min,tempstr);
  Result := Result + tempstr + ' ';
  Str(seconds:7:4,tempstr);
  Result := Result + tempstr;
end;
//-----------------------------------------------
(* raj is in arcseconds, but the LocationInfo class stores RA in hours, minutes,
and seconds. This function returns a string version of raj using HMS for comparison. *)
function TGASData.J2000RAToHMS:string;
var secraj:Double;
begin
  // divide by 15 to convert arcsec to HMS seconds
  secraj := raj / 15.0;
  // the rest can be handled by TGASData.ArcSecToDeg
  Result := ArcSecToDeg(secraj);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor TGASData.Create;
begin
  inherited;
  dataset := False;
  matched := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// set initial data from a line of TGAS source data
function TGASData.SetFromSource(input:string):Boolean;
var parsed:TStringList;
    temstr1:string;
    sc:Integer;
begin
  Result := False;
  sc := Length(input);
  parsed := ParseByChar(input,',',True,True);
  sc := parsed.Count;
  if parsed.Count < 59 then begin
    parsed.Free;
    Exit;
  end;
  dataset := False;
  // names
  hip_desig := parsed[0];
  imported1 := hip_desig + ',';
  tycho2_desig := parsed[1];
  imported1 += tycho2_desig + ',';
  // right ascension
  temstr1 := parsed[6];
  Val(temstr1,ra,sc);
  imported1 += temstr1 + ',';
  if (sc<>0) or (ra >= 360.0) then begin
    parsed.Free;
    Exit;
  end;
  temstr1 := parsed[7];
  Val(temstr1,raerr,sc);
  imported1 += temstr1 + ',';
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  // declination
  temstr1 := parsed[8];
  Val(temstr1,dec,sc);
  imported1 += temstr1 + ',';
  if (sc<>0) or (dec > 90) or (dec < -90) then begin
    parsed.Destroy;
    Exit;
  end;
  temstr1 := parsed[9];
  Val(temstr1,decerr,sc);
  imported1 += temstr1 + ',';
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  // parallax
  temstr1 := parsed[10];
  Val(temstr1,pllx,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  temstr1 := parsed[11];
  Val(temstr1,pllxerr,sc);
  imported2 := ',' + temstr1 + ',';
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  // proper motion right ascension
  temstr1 := parsed[12];
  Val(temstr1,pmra,sc);
  imported2 += temstr1 + ',';
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  temstr1 := parsed[13];
  Val(temstr1,pmra_error,sc);
  imported2 += temstr1 + ',';
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  // proper motion declination
  temstr1 := parsed[14];
  Val(temstr1,pmdec,sc);
  imported2 += temstr1 + ',';
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  temstr1 := parsed[15];
  Val(temstr1,pmdec_error,sc);
  imported2 += temstr1;
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  parsed.Free;
  dataset := True;
  Result:= True;
end;
//-----------------------------------------------
// set data from a line of 'reduced' TGAS source data
function TGASData.SetFromReducedSource(input:string):Boolean;
var parsed:TStringList;
    temstr1:string;
    sc:Integer;
begin
  Result := False;
  sc := Length(input);
  parsed := ParseByChar(input,',',True,True);
  sc := parsed.Count;
  if parsed.Count < 13 then begin
    parsed.Free;
    Exit;
  end;
  dataset := False;
  // names
  hip_desig := parsed[0];
  imported1 := hip_desig + ',';
  tycho2_desig := parsed[1];
  imported1 += tycho2_desig + ',';
  // right ascension
  temstr1 := parsed[2];
  Val(temstr1,ra,sc);
  if (sc<>0) or (ra >= 360.0) then begin
    parsed.Free;
    Exit;
  end;
  imported1 += temstr1 + ',';
  temstr1 := parsed[3];
  Val(temstr1,raerr,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported1 += temstr1 + ',';
  // declination
  temstr1 := parsed[4];
  Val(temstr1,dec,sc);
  if (sc<>0) or (dec > 90) or (dec < -90) then begin
    parsed.Destroy;
    Exit;
  end;
  imported1 += temstr1 + ',';
  temstr1 := parsed[5];
  Val(temstr1,decerr,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported1 += temstr1 + ',';
  // parallax
  temstr1 := parsed[6];
  Val(temstr1,pllx,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  temstr1 := parsed[7];
  Val(temstr1,pllxerr,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported2 := ',' + temstr1 + ',';
  // proper motion right ascension
  temstr1 := parsed[8];
  Val(temstr1,pmra,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported2 += temstr1 + ',';
  temstr1 := parsed[9];
  Val(temstr1,pmra_error,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported2 += temstr1 + ',';
  // proper motion declination
  temstr1 := parsed[10];
  Val(temstr1,pmdec,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported2 += temstr1 + ',';
  temstr1 := parsed[11];
  Val(temstr1,pmdec_error,sc);
  if (sc<>0) then begin
    parsed.Destroy;
    Exit;
  end;
  imported2 += temstr1 + ',';
  temstr1 := parsed[12];
  if temstr1 = 'True' then matched := True
  else matched := False;
  // done with parsing
  parsed.Free;
  dataset := True;
  Result:= True;
end;
//-----------------------------------------------
function TGASData.DataIsSet:Boolean;
begin  Result := dataset;  end;
//-----------------------------------------------
procedure TGASData.AddParallaxOffset(addoff:Real);
begin
  pllx += addoff;
  pllx += 0;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// returning internal data for external consumption

function TGASData.GetRA_Arcsec:Double;
begin
  Result := ra*ARCSEC_PDEG;
end;
//-----------------------------------------------
procedure TGASData.GetRA_HMS(out hours:Integer; out minutes:Integer; out secs:Real);
var totsecs,fracsec:Double;
    truncsecs,secleft:Integer;
    mintotal:Integer;
begin
  // degrees to seconds
  totsecs := 240*ra;
  fracsec := Frac(totsecs);
  truncsecs := Trunc(totsecs);
  secleft := truncsecs mod 60;
  secs := secleft;
  secs := secs + fracsec;
  // mintotal
  mintotal := truncsecs div 60;
  minutes := mintotal mod 60;
  hours := mintotal div 60;

end;
//-----------------------------------------------
function TGASData.GetDec_Arcsec:Double;
begin
  Result := dec*ARCSEC_PDEG;
end;
//-----------------------------------------------
function TGASData.GetDecDegrees:Real;
begin   Result := dec;    end;
//-----------------------------------------------
function TGASData.GetLY_Distance:Double;
begin
  Result := MAS_DIST_NUM / pllx;
end;
//-----------------------------------------------
function TGASData.GetParallax:Double;
begin
  Result:= pllx;
end;
//-----------------------------------------------
function TGASData.GetParallaxError:Double;
begin
  Result := pllxerr;
end;
//-----------------------------------------------
function TGASData.GetHipparcos:string;
begin
  Result := hip_desig;
end;
//-----------------------------------------------
function TGASData.GetTycho2:string;
begin
  Result := tycho2_desig;
end;
//-----------------------------------------------
function TGASData.GetReduced:string;
begin
  Result := imported1 + FloatToStrF(pllx,ffFixed,11,6);
  Result += imported2 + ',' + Bool2Str(matched);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// getting proper motion
function TGASData.GetPM_RAmas:Double;
begin
  Result := pmra;
end;
//-----------------------------------------------
function TGASData.GetPM_Decmas:Double;
begin
  Result := pmdec;
end;
//-----------------------------------------------
(* Nstars uses mas/year total  and degree direction, whereas TGAS uses mas/year
ra and mas/year dec. We convert here. *)
procedure TGASData.GetPMOut(out magnitude:Double; out position_angle:Double);
var invatan:Double;
begin
  // total amount is simple to calculate.
  magnitude := hypot(pmra,pmdec);
  // direction is more complicated
  invatan := arctand(pmdec/pmra);
  if (pmra < 0) then position_angle := 270 -invatan
  else position_angle := 90 - invatan;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* using pm info to get a J2000 position, with big error bars. lack of radial
velocity info means the position will be off, but not by much (only 15 years) *)
function TGASData.CalcJ2000Pos:Boolean;
var
    arc_ra, arc_raerr, arc_pmra, arc_pmraerror:Double;
    arc_dec, arc_decerr, arc_pmdec, arc_pmdec_error:Double;
begin
  // startoff
  Result := False;
  if not dataset then Exit;
  Result := True;
  // converting RA values to arcseconds
  arc_ra := ra*ARCSEC_PDEG;
  arc_raerr := raerr*INV_THOUSAND;
  arc_pmra := pmra*INV_THOUSAND;
  arc_pmraerror := pmra_error*INV_THOUSAND;
  // calculating the new ra J2000 values
  raj := arc_ra - 15*arc_pmra;
  if raj < 0 then raj := ARCSEC_CIRCLE + raj
  else if raj >= ARCSEC_CIRCLE then raj := raj - ARCSEC_CIRCLE;
  raj_error := 2*arc_raerr + 15*arc_pmraerror*2;
  // converting dec values to arcseconds
  arc_dec := dec*ARCSEC_PDEG;
  arc_decerr := decerr*INV_THOUSAND;
  arc_pmdec := pmdec*INV_THOUSAND;
  arc_pmdec_error := pmdec_error*INV_THOUSAND;
  // calculating the new dec J2000 values
  decj := arc_dec - 15*arc_pmdec;
  // the following 2 lines cover polar problems
  if (decj > ARCSEC_QUARTER) then Result := False
  else if (decj < -ARCSEC_QUARTER) then Result := False;
  decj_error := 2*arc_decerr + 15*arc_pmdec_error*2;
  // done
  Exit;
end;
//-----------------------------------------------
(* Returns the calculated J2000 Right Ascension in deci-degrees *)
function TGASData.GetJ2000RAPos:Integer;
begin
  Result := Trunc(raj / 360);
end;
//-----------------------------------------------
(* Checks if the inputted position is 'Close Enough' to the J2000 position.
Remember to call CalcJ2000Pos before using this function.
NOTE: Inputs are NOT checked. should be : 0 <= ra_arcsec < ARCSEC_CIRCLE,
-ARCSEC_QUARTER <= dec_arcsec <= ARCSEC_QUARTER, and tolerance should be > 0 but
still low enough ( less than 3600, probably). *)

function TGASData.J2000PosMatch(ra_arcsec,dec_arcsec,tolerance_arcsec:Double):Boolean;
var ra_min, ra_max, dec_min, dec_max:Double;
    ra_inbounds, dec_inbounds:Boolean;
begin
  // calculating the bounds...
  ra_min := raj - raj_error - tolerance_arcsec;
  ra_max := raj + raj_error + tolerance_arcsec;
  dec_min := decj - decj_error - tolerance_arcsec;
  dec_max := decj + decj_error + tolerance_arcsec;
  // simple ra check
  if (ra_min >= 0) and (ra_max < ARCSEC_CIRCLE) then
    ra_inbounds := (ra_min <= ra_arcsec) and (ra_max > ra_arcsec)
  // not so simple ra checks
  else if (ra_min < 0) then
    ra_inbounds := (ra_arcsec < ra_max) or (ra_arcsec >= (ARCSEC_CIRCLE+ra_min))
  else if (ra_max >= ARCSEC_CIRCLE) then
    ra_inbounds := (ra_arcsec >= ra_min) or (ra_arcsec < (ra_max-ARCSEC_CIRCLE))
  else Raise Exception.Create('Bad J2000PosMatch inputs!');
  // attempting problem check...
  if hip_desig = '37279' then begin

  end;
  // check, we go on if the previous test passed
  if not ra_inbounds then begin
    Result := False;
    Exit;
  end;
  // now for the declination
  // simple check
  if (dec_min >= -ARCSEC_QUARTER) and (dec_max <= ARCSEC_QUARTER) then
    dec_inbounds := (dec_min <= dec_arcsec) and (dec_max >= dec_arcsec)
  // non-simple should hopefully never happen... so I'll treat it as false
  else dec_inbounds := False;

  // done
  Result := dec_inbounds;
end;
//-----------------------------------------------
(* data string for J2000, positions given in arcseconds *)
function TGASData.SummaryString:string;
var buffer:string;
begin
  if Length(hip_desig) <> 0 then Result := 'HIP ' + hip_desig + ' ('
  else Result := 'Tyc ' + tycho2_desig + ' (';
  Str(GetLY_Distance:6:2,buffer);
  Result := Result + buffer + 'LY) ; ';
  Str(raj:14:4,buffer);
  Result := Result + 'RA: ' + buffer + ' , ';
  Str(decj:14:4,buffer);
  Result := Result + 'Dec: ' + buffer;
end;
//--------------------------------------------------------------------
(* J2000 to string, using HMS for RA and degrees for Decl *)
function TGASData.SummaryString2:string;
var buffer:string;
begin
  if Length(hip_desig) <> 0 then Result := 'HIP ' + hip_desig + ' ('
  else Result := 'Tyc ' + tycho2_desig + ' (';
  Str(GetLY_Distance:6:2,buffer);
  Result := Result + buffer + 'LY) ; ';
  Result := Result + 'RA: ' + J2000RAToHMS + ' , ';
  Result := Result + 'Dec: ' + ArcSecToDeg(decj) + ' Pllx: ';
  Str(GetParallax:8:6,buffer);
  Result := Result + buffer +  ' ';
  Str(GetParallaxError:8:6,buffer);
  Result := Result + buffer;
end;
//---------------------------------------------------------------------
(* checks if the names contain a matching Hipparcos or Tycho catalog ID *)
function TGASData.NameMatch(namegroup:StarName):Boolean;
var rok:Boolean;  catstr:string;
begin
  Result := False;
  if namegroup = nil then Exit;
  // hipparcos
  if Length(hip_desig) <> 0 then begin
   rok := namegroup.GetCatValue('Hip',catstr);
   if (not rok) then Exit;
   Result := (catstr = hip_desig);
  end
  // tycho
  else if Length(tycho2_desig) <> 0 then begin
   rok := namegroup.GetCatValue('Tyc',catstr);
   if (not rok) then Exit;
   Result := (catstr = tycho2_desig);
  end;
end;

//=========================================================================
// helper methods
(* creates a star data object from a line of text from the TGAS csv data file *)
function TGASCollection.InputLineToStar(input_line:string; aparams:TGASAlterParams):TGASData;
var CovSource:TGASData;  FlagResult:Boolean;
    lydist:Double;
    eclnorth:Boolean;
    stc:Real;
begin
  Result := nil;
  // creating from the source
  CovSource := TGASData.Create;
  FlagResult := CovSource.SetFromSource(input_line);
  if not FlagResult then begin
    CovSource.Destroy;
    Exit;
  end;
  // we discard stars if the error is too big
  if (CovSource.pllxerr) > aparams.maxerr then begin
     CovSource.Destroy;
     Exit;
  end;
  stc := aparams.offset;
  // fiddiling around with the parallax
  if aparams.transform > 0 then begin
   // offset
   if aparams.transform = 1 then CovSource.AddParallaxOffset(aparams.offset)
   // ecliptic hemisphere offsets from Lindegren
   else if aparams.transform = 2 then begin
    eclnorth := EclipticNorth(CovSource.ra,CovSource.dec);
    if eclnorth then CovSource.AddParallaxOffset(0.13)
    else CovSource.AddParallaxOffset(0.053);
   end
   // Stassun and Torres offset as a function ofecliptic latitude
   else if aparams.transform = 3 then begin
     stc := StassunTorresCorrection(CovSource.ra,CovSource.dec);
     CovSource.AddParallaxOffset(stc);
   end
   // ecliptic hemisphere offsets from Jao
   else if aparams.transform = 4 then begin
     eclnorth := EclipticNorth(CovSource.ra,CovSource.dec);
     if eclnorth then CovSource.AddParallaxOffset(0.17)
     else CovSource.AddParallaxOffset(0.32);
   end
   else Assert(False);
  end;
  // we discard any stars with a distance greater than max distance
  lydist := CovSource.GetLY_Distance;
  if (lydist > aparams.maxdist) or (lydist < 0) then begin
     CovSource.Destroy;
     Exit;
  end;
  // next, calculate the J2000 position we will use
  FlagResult := CovSource.CalcJ2000Pos;
  // the calc method is unlikely to fail, but it is possible, discard
  if not FlagResult then CovSource.Destroy
  else Result := CovSource;
end;
//-----------------------------------------------
function TGASCollection.InputReducedLineToStar(input_line:string; max_distance:Integer):TGASData;
var CovSource:TGASData;  FlagResult:Boolean;
    lydist:Double;
begin
  Result := nil;
  // creating from the source
  CovSource := TGASData.Create;
  FlagResult := CovSource.SetFromReducedSource(input_line);
  if not FlagResult then begin
    CovSource.Destroy;
    Exit;
  end;
  // we discard any stars with a distance greater than max distance
  lydist := CovSource.GetLY_Distance;
  if (lydist > max_distance) or (lydist < 0) then begin
     CovSource.Destroy;
     Exit;
  end;
  // next, calculate the J2000 position we will use
  FlagResult := CovSource.CalcJ2000Pos;
  // the calc method is unlikely to fail, but it is possible, discard
  if not FlagResult then CovSource.Destroy
  else Result := CovSource;
end;
//-----------------------------------------------
(* takes a star and sticks it in the lists properly *)
function TGASCollection.InsertStarData(to_insert:TGASData):Boolean;
var dd_pos, dd_posm, dd_posp:Integer;
    hipid,tycid:string;
begin
  Result := False;
  // bad inputs
  if to_insert = nil then Exit;
  if not to_insert.DataIsSet then Exit;
  // things are okay... maybe
  // getting calculated (and truncated) J2000 RA in dec-degrees
  dd_pos := to_insert.GetJ2000RAPos;
  if (dd_pos < 0) or (dd_pos >= 3600) then Exit;
  // things should be okay from here on
  Result := True;
  // calculating the grouped lists to add to
  if dd_pos = 0 then dd_posm := 3599
  else dd_posm := dd_pos -1;
  if dd_pos = 3599 then dd_posp := 0
  else dd_posp := dd_pos +1;
  // adding to all the necessary lists
  list_of_stars.Add(to_insert);
  grouped_lists[dd_posm].Add(to_insert);
  grouped_lists[dd_pos].Add(to_insert);
  grouped_lists[dd_posp].Add(to_insert);
  // inserting in the name lists
  hipid := to_insert.hip_desig;
  tycid := to_insert.tycho2_desig;
  if Length(hipid) <> 0 then hip_ptr.Add(hipid,to_insert);
  if Length(tycid) <> 0 then tyc_ptr.Add(tycid,to_insert);
  // done
end;
//-----------------------------------------------
function TGASCollection.CreateUnMatched(minpllx:Real):Integer;
var listmax,index1,index2,mvalue:Integer;
    ksv:Boolean;
    datapointer1,datapointer2:TGASData;
    parallax1,parallax2:Real;
begin
  // setting up initial values
  Result := 0;
  if unmatched <> nil then unmatched.Free;
  listmax := list_of_stars.Count - 1;
  unmatched := TGASList.Create();
  unmatched.FreeObjects := False;
  // copying pointers to data with minimum parallax or greater
  for index1 := 0 to listmax do begin
    datapointer1 := list_of_stars.Items[index1];
    (* parallax must be bigger (or equal) to the minimum, and the star must be
    unmatched previously to a star in the main list. *)
    ksv := (datapointer1.GetParallax >= (minpllx*1000)) and not (datapointer1.matched);
    // finally, if all conditions aremet...
    if ksv then unmatched.Add(datapointer1);
  end;
  // post loop stuff
  Result := unmatched.Count;
  if (Result = 0) then Exit;

  // sorting the resulting list, ordered by parallax (decreasing)
  // only plan to do this once...
  listmax := Result-1;
  for index1 := 0 to listmax-1 do begin
    datapointer1 := unmatched[index1];
    parallax1 := datapointer1.GetParallax;
    mvalue := index1;
    for index2 := index1+1 to listmax do begin
      datapointer2 := unmatched[index2];
      parallax2 := datapointer2.GetParallax;
      if parallax2 > parallax1 then begin
       mvalue := index2;
       parallax1 := parallax2;
      end;
    end;
    if mvalue <> index1 then begin
      datapointer2 := unmatched[mvalue];
      unmatched[index1] := datapointer2;
      unmatched[mvalue] := datapointer1;
    end;
  end;
  // done...
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
// basic constructor
constructor TGASCollection.Create; overload;
var cindex:Integer;
begin
  inherited;
  list_of_stars := TGASList.Create;
  for cindex := 0 to 3599 do begin
    grouped_lists[cindex] := TGASList.Create;
    grouped_lists[cindex].FreeObjects := False;
  end;
  hip_ptr := TGAS_StringMap.Create;
  hip_ptr.Sorted := True;
  tyc_ptr := TGAS_StringMap.Create;
  tyc_ptr.Sorted := True;
  unmatched := nil;
end;
//-----------------------------------------------
// external methods
(* returns a list of stars with a J2000 position close enough to the inputs *)
function TGASCollection.FindJ2000Matches(ra_arcsec, dec_arcsec:Double):TGASList;
var Match:Boolean;
    list_index, list_max:Integer;
    dd_position:Integer;
    curr_star:TGASData;
const tolerance = 120;
begin
  // setting up some initial values
  dd_position := Trunc(ra_arcsec / 360.0);
  list_max := grouped_lists[dd_position].Count-1;
  Result := TGASList.Create;
  Result.FreeObjects := False;
  if list_max < 0 then Exit;
  // iterating through the list
  for list_index := 0 to list_max do begin
    curr_star := grouped_lists[dd_position].Items[list_index];
    Match := curr_star.J2000PosMatch(ra_arcsec,dec_arcsec,tolerance);
    if Match then Result.Add(curr_star);
  end;
  // done (Result is already set, it might be empty)
end;
//-----------------------------------------------
function TGASCollection.FindNameMatches(const inlistn:TStringList):TGASList;
var TXResult:TGASList;
    listc,listdex:Integer;
    currid,whichx:string;
    testo:Boolean;
    foundex:Integer;
begin
  // the usual checks
  Result := nil;
  if inlistn = nil then Exit;
  listc := inlistn.Count;
  if listc = 0 then Exit;
  // name checking loop
  for listdex := 0 to (listc-1) do begin
    currid := inlistn[listdex];
    if not (AnsiStartsStr('Hip',currid) or AnsiStartsStr('Tyc',currid)) then Exit;
  end;
  // name lookup!
  TXResult := TGASList.Create(False);
  for listdex := 0 to (listc-1) do begin
    currid := inlistn[listdex];
    testo := ExtractFirstWord(currid,whichx);
    Assert(testo);
    if whichx = 'Hip' then begin
      foundex := hip_ptr.IndexOf(currid);
      if foundex >=0 then TXResult.Add(hip_ptr.Data[foundex]);
    end
    else begin
      foundex := tyc_ptr.IndexOf(currid);
      if foundex >=0 then TXResult.Add(tyc_ptr.Data[foundex]);
    end;
  end;
  // afterwards, we count...
  if TXResult.Count = 0 then FreeAndNil(TXResult);
  Result := TXResult;
end;
//-----------------------------------------------
function TGASCollection.SetMatchByID(xid:string; tomatch:Boolean):Boolean;
var ident:string;
    foundex:Integer;
    target:TGASData;
begin
  Result := False;
  if (not ExtractFirstWord(xid,ident)) then Exit;
  // looking for the input id...
  if ident = 'Hip' then begin
     foundex := hip_ptr.IndexOf(xid);
     if foundex < 0 then Exit;
     target := hip_ptr.Data[foundex];
  end
  else if ident = 'Tyc' then begin
     foundex := tyc_ptr.IndexOf(xid);
     if foundex < 0 then Exit;
     target := tyc_ptr.Data[foundex];
  end else Exit;
  // finishing...
  target.matched := tomatch;
  Result := True;
end;
//-----------------------------------------------
procedure TGASCollection.ImportFromCSVFile(fnin:TFileName; aparams:TGASAlterParams);
var infile:text;
    readline:string;
    curr_star:TGASData;
begin
  // starting
  AssignFile(infile,fnin);
  FileMode := fmOpenRead;
  Reset(infile);
  Readln(infile,readline);
  // the first line is for headings, discard
  while not Eof(infile) do begin
    Readln(infile,readline);
    readline := Trim(readline);
    curr_star := InputLineToStar(readline,aparams);
    if curr_star <> nil then InsertStarData(curr_star);
  end;
  // we've gotten to the end!
  CloseFile(infile);
  FileMode := fmOpenReadWrite;
end;
//-----------------------------------------------
procedure TGASCollection.ImportFromReducedCSVFile(fnin:TFileName);
var infile:TextFile;
    readline:string;
    curr_star:TGASData;
begin
  // starting
  AssignFile(infile,fnin);
  FileMode := fmOpenRead;
  Reset(infile);
  Readln(infile,readline);
  // the first line is for headings, discard
  while not Eof(infile) do begin
    Readln(infile,readline);
    readline := Trim(readline);
    curr_star := InputReducedLineToStar(readline,1000);
    if curr_star <> nil then InsertStarData(curr_star);
  end;
  // we've gotten to the end!
  CloseFile(infile);
  FileMode := fmOpenReadWrite;
end;
//-----------------------------------------------
(* writes list_of_stars to a CSV, this is Reduced compared to the original TGAS
data, which has 59 fields per star (Reduced has only 12, plus a flag ) . *)
function TGASCollection.WriteReducedToFile(fnout:TFileName):Boolean;
var curr_star:TGASData;
    outline:string;
    outfile:TextFile;
    write_index, max_index:Integer;
begin
  // quick bad cases
  Result := False;
  if Length(fnout) = 0 then Exit;
  if list_of_stars.Count = 0 then Exit;
  // prepping for file output
  AssignFile(outfile,fnout);
  ReWrite(outfile);
  // starting the write
  Writeln(outfile,tgas_reduced_fields);
  max_index := list_of_stars.Count - 1;
  // loop writing each star
  for write_index := 0 to max_index do begin
    curr_star := list_of_stars[write_index];
    outline := curr_star.GetReduced;
    Writeln(outfile,outline);
  end;
  // done
  CloseFile(outfile);
  Result := True;
end;
//-----------------------------------------------
function TGASCollection.WriteUnmatchedToFile(minpllx:Real; fnout:TFileName):Integer;
var datapointer1:TGASData;
    index1,listmax:Integer;
    outfile:TextFile;
    outline:string;
begin
  // setting up initial unmatched list
  Result := CreateUnMatched(minpllx);
  if Result = 0 then begin
    unmatched.Free;
    unmatched := nil;
    Exit;
  end;
  // once sorted, we output to file
  // prepping for file output
  AssignFile(outfile,fnout);
  ReWrite(outfile);
  // starting the write
  Writeln(outfile,tgas_reduced_fields);
  listmax := unmatched.Count-1;
  // loop writing each star
  for index1 := 0 to listmax do begin
    datapointer1 := unmatched[index1];
    outline := datapointer1.GetReduced;
    Writeln(outfile,outline);
  end;
  // done
  CloseFile(outfile);
end;

//-----------------------------------------------
function TGASCollection.StarCount:Integer;
begin
  Result := list_of_stars.Count;
end;
//-----------------------------------------------
function TGASCollection.MakeUnmatched(minpllx:Real):Boolean;
var umcount:Integer;
begin
  Result := False;
  if (minpllx<=0) then Exit;
  umcount := CreateUnMatched(minpllx);
  unmatched_index := -1;
  if umcount = 0 then begin
    unmatched.Free;
    unmatched := nil;
    Exit;
  end;
  Result := True;
end;
//-----------------------------------------------
function TGASCollection.NextUnmatched:TGASData;
var ismat:Boolean;
    tgasx:TGASData;
begin
  // basic nil (no result, false) cases
  Result := nil;
  if unmatched = nil then Exit;
  if unmatched.Count = 0 then Exit;
  if unmatched.Count = unmatched_index then Exit;
  // moving the index
  ismat := True;
  while ismat do begin
    Inc(unmatched_index);
    if unmatched.Count = unmatched_index then Exit;
    tgasx := unmatched[unmatched_index];
    ismat := tgasx.matched;
  end;
  Result := tgasx;
end;

//-----------------------------------------------
// destructor
destructor TGASCollection.Destroy;
var arcloop:Integer;
begin
  for arcloop := 0 to 3599 do begin
    grouped_lists[arcloop].Free;
    grouped_lists[arcloop] := nil;
  end;
  list_of_stars.Free;

  inherited;
end;
//========================================================================
// TGAS parallax alteration functions...
//-------------------------------------------
function EclipticNorth(rapos,decpos:Real):Boolean;
var ecl_lat,ecl_long:Real;
    rok:Boolean;
begin
  rok := MatrixTransform(rapos,decpos,Equatorial_to_Ecliptic,ecl_long,ecl_lat);
  Assert(rok);
  Result := (ecl_lat >= 0);
end;
//-------------------------------------------
function StassunTorresCorrection(rapos,decpos:Real):Real;
var ecl_lat,ecl_long:Real;
    rok:Boolean;
begin
  rok := MatrixTransform(rapos,decpos,Equatorial_to_Ecliptic,ecl_long,ecl_lat);
  Assert(rok);
  Result := 0.22 + (0.003*ecl_lat);
end;
//========================================================================
begin
  tgas_main := nil;
end.

