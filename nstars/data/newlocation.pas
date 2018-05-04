unit newlocation;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, Math, DAMath, df_strings, tgas, newImports,
  Utilities2, gaiadr2base;

type

NsHours = 0..23;
Ns60int = 0..59;
NsDeg = 0..90;

Location = class
  protected
    xepoch:EpochType;
    // right ascension
    ra_hms:string;
    ra_hours:NsHours;   ra_minutes:Ns60int;   ra_seconds:Real;
    // declination
    dec_dms:string;
    southern:Boolean;
    dec_degrees:NsDeg;  dec_arcmins:Ns60int;  dec_arcsecs:Real;

    // parallax and proper motion
    parallax,parallax_err:Real;  // in milli-arcseconds
    pm_magnitude,pm_posang:Real; // proper motion in mas/yr , angle in dec degrees
    binarycopy:Boolean;

    // radial velocity (in km/s)
    radialv:Real;

    // property helper methods
    function GEpoch:EpochType;
    function GEpochStr:string;
    function GRA:string;
    function GDec:string;
    function GParallax:Real;
    function GParallaxE:Real;
    function IsCopy:Boolean;
    // more property helper methods
    function GPMMag:Real;
    function GPMDir:Real;
    function GRadialV:Real;
    // string conversion helper methods
    function StrTo60(instr:string; out xval:Ns60int):Boolean; overload;
    function StrTo60(instr:string; out xval:Real):Boolean; overload;
    procedure ReduceSexag(in_sec:Real; out sec_left:Real; out min:Ns60int; out big:Integer);
    function SetRAInternal(in_ra:string; out hrs:NsHours;out mins:Ns60int; out secs:Real):Boolean;
    function SetDecDegDec(in_decdegdec:Real):Boolean;
    // other helper methods
    procedure MakeRAString;
    procedure MakeDecString;
    function RA_toDegrees:Real;
    function Dec_toDegrees:Real;
    function AddPToOld(zsource:string; in_pllx,in_pllxe:Real):Boolean;
    function OldParallaxParser():TStringList;
    function OldParallaxCleaner():Integer;
    // transforms and other matrix-using stuff
    procedure MakeVelocity(usematrix:RMatrix; out uv,vv,wv:Real);
    procedure MakeDeltaPos(opt:Integer; out delX,delY,delZ:Real);
    function YearDiff(target_year:Integer):Real;

  public
    uncertain:Boolean;   // uncertain parallax
    source:string;       // source for parallax and proper motion
    oldparallax:string;  // parallax backups

    // properties
    property Epoch:EpochType read GEpoch;
    property EpochLabel:string read GEpochStr;

    property RightAscensionHMS:string read GRA;
    property DeclinationDMS:string read GDec;

    property ParallaxMAS:Real read GParallax;
    property ParallaxErrorMAS:Real read GParallaxE;
    property IsACopy:Boolean read IsCopy;
    property ProperMotionMagnitude:Real read GPMMag;
    property ProperMotionAngle:Real read GPMDir;
    property RadialVelocity:Real read GRadialV;

    // startup methods
    constructor Create; overload;
    constructor Create(xsource:Location; copy:Boolean); overload;
    constructor Create(xsource:Location; arcsec_sep,pos_angle:Double); overload;
    procedure MakeNotCopy;

    // setting the position
    function SetPositionHMS(in_epoch:EpochType; in_ra,in_dec:string):Boolean;
    function SetPositionDDeg(in_epoch:EpochType; in_ra,in_dec:string):Boolean; overload;
    function SetPositionDDeg(in_epoch:EpochType; in_ra,in_dec:Real):Boolean; overload;

    // setting the parallax
    function SetParallax(in_median,in_error:Real):Boolean; overload;
    function SetParallax(in_median,in_error:string):Boolean; overload;
    function SetParallaxArcSeconds(in_median,in_error:string):Boolean;
    function UpdateParallax(in_median,in_errorx:Real):Boolean; overload;
    function UpdateParallax(in_median,in_errorx:string):Boolean; overload;
    function CheckEqParallax(in_median,in_errorx:string):Boolean;
    function SetParallaxPasted(indata:string; out xsrc:string):Boolean;
    function SwapParallax(last:Boolean):Boolean;

    // setting proper motion
    function SetProperMotion(in_magnitude,in_angle:Real):Boolean; overload;
    function SetProperMotion(in_magnitude,in_angle:string):Boolean; overload;
    function SetProperMotionArcSeconds(in_magnitude,in_angle:string):Boolean; overload;
    function SetRadialVelocity(in_radialv:Real):Boolean;

    // getting data (requiring calculation)
    function GetDecimalRightAscension:Real;
    function GetDecimalDeclination:Real;
    procedure GetArcsecPosition(secra:Boolean; out rapos,decpos:Real);
    function GetDistance(parsecs:Boolean):Real; // LY by default
    function GetDistanceAtYear(parsecs:Boolean; targyear:Integer):Real;
    function GetDistanceStr(decimp,targyear:Integer; parsecs:Boolean):string; // LY by default
    function GetDistanceExtremaStr(min,double:Boolean; prec:Integer):string;
    function GetLYRange(double:Boolean):Real;

    // string getting data
    function GetParallaxString(prec:Integer; padding:Boolean):string;
    function GetParallaxErrString(prec:Integer; padding:Boolean):string;
    function GetProperMotionMagStr(prec:Integer):string;
    function GetProperMotionAngleStr(prec:Integer; padding:Boolean):string;
    function GetRadialVStr(prec:Integer):string;

    // Getting XYZ coordinates
    procedure GetXYZ(out xo,yo,zo:Real);
    function GetXYZ_CSV(prec:Integer):string;
    procedure GetXYZatYear(const inyear:Integer; out xo,yo,zo:Real);
    function GetXYZatYear_CSV(const inyear,prec:Integer):string;
    procedure GetGalacticCoords(out lat,long:Real);
    function GetGalacticCoordString():string;
    procedure GetGalacticXYZ(out galx,galy,galz:Real);
    procedure GetAstrosynthesisXYZ(out astr_x,astr_y,astr_z:Real);
    procedure GetAstrosynthesisXYZatYear(const inyear:Integer; out astr_x,astr_y,astr_z:Real);
    function MakeAstrosynXYZString(precision:Integer):string;
    function GetAstrosynthesisXYZatYear_String(const inyear,precison:Integer):string;
    function MakeAstrosynXYZ_AUOffsetString(const base:Location; out offs:string):string;

    // specialty functions
    function BinaryUpdate(parent:Location):Boolean;
    function GetDistanceFrom(other:Location; targyear:Integer):Real;
    function GetRADecDifference(const newloc:Location; out radfs,decdfas:Real):Boolean;
    function AddRADecDifference(const radfs,decfas:Real):Boolean;
    function GetArsecSeparationFrom(other:Location):Real;
    function PositionProperMotionMatch(other:Location; max_dist,max_pmangdiff,max_pmmagpdiff,max_pmmagdiff:Real):Boolean;
    function GetUVWVelocities(lefthanded:Boolean; out uv,vv,wv:Real):Boolean;
    function GetUVWString(lefthanded:Boolean):string;
    procedure MakeGetJ2015p5Pos(out radeg:Real; out decdeg:Real);

    // importing data from special sources
    function SetFromTGAS(instar:TGASData):Boolean;
    function SetFromImported(instar:ImportedData; epch:EpochType; srcin:string; setlocat:Boolean):Boolean;
    function SetFromGaiaDR2(inastro:GaiaDR2Astrometry; ckeepold:Boolean):Boolean;
    // to and from strings as a whole
    function ConvertAllToString:string;
    function ConvertAllFromString(inval:string):Boolean;
    function ConvertAllFromStringOld(inval:string):Boolean;
    function ConvertAllFromStringOldC(inval:string; parent_pllx,parent_pllxe:Real):Boolean;

end;
//-----------------------------------------------------------------------
// functions
(* takes in magnitude and parallax (in mas), outputs absolute magnitude *)
function CalculateAbsMagnitude(zmagnitude:Real; parallax:Real):Real;
function CalculateVisMagnitude(absmag,parallax:Real):Real;

// used for cutting and pasting parallax entry
function ParsePastedParallax(pentry:string; out osource:string; out xpllx,xpllxe:Real):Boolean;

(* Used for checking and converting input for making a new location from an
old location, and a string input that is supposed to contain sepration and
position angle. *)
function SeparationCheck(source_loc:Location; in_string:string; out xerr:string; out rho:Real; out theta:Real):Boolean;

//---------------------------------------------------------------
// constants
const

EpochNames:array[0..6] of string = ('B1950','B1975','J2000','J2014','J2015','J2017','J2015.5');

(* Pre-stored constants and variables for co-ordinate conversion. RA and Dec from
"Allen's Astrophysical Quantities" by  Arthur N. Cox (p 575), the more accurate
longtitude of the ascending node figures are from :
http://bado-shanai.net/Astrogation/astrogGalacticCoord.htm and also "Using
SI Units in Astronomy" by Richard Dodd (page 42).
Refining using Liu+ 2010 *)

  galnp_ra_b1950 =  192.25;
  galnp_ra_j2000 =  192.85948123;
  galnp_dec_b1950 = 27.4;
  galnp_dec_j2000 = 27.1282511944;
  galp_an_b1950 = 33;
  galp_an_j2000 = 32.93192;

  ly_per_parsec = 3.26156378;

(* constants for getting galactic space velocities *)
(* Rather than doing Johnson and Soderblom Matrix multiplication, I'll
get the method (and constants) from the astrolib Python library *)
uvwTconsts:array[0..8] of Real = (-0.0548755604,-0.8734370902,-0.4838350155,
                                  +0.4941094279,-0.4448296300,+0.7469822445,
                                  -0.8676661490,-0.1980763734,+0.4559837762);
kmps_auy = 4.74057183;

GAIA2_TAG = 'Gaia DR2';


var
 (* 'constants' calculated when the unit loads *)
 sin_galnp_dec_b1950:Extended;
 sin_galnp_dec_j2000:Extended;
 cos_galnp_dec_b1950:Extended;
 cos_galnp_dec_j2000:Extended;

 // lfsOut: TFileStream;


implementation
//==========================================================================
// property helper methods
//-------------------------------------
function Location.GEpoch:EpochType;
begin   Result := xepoch;   end;
//--------------------------------------
function Location.GEpochStr:string;
begin   Result := EpochNames[Ord(xepoch)];   end;
//--------------------------------------
function Location.GRA:string;
begin   Result := ra_hms;   end;
//--------------------------------------
function Location.GDec:string;
begin   Result := dec_dms;   end;
//--------------------------------------
function Location.GParallax:Real;
begin   Result := parallax;   end;
//--------------------------------------
function Location.GParallaxE:Real;
begin   Result := parallax_err;   end;
//--------------------------------------
function Location.IsCopy:Boolean;
begin   Result := binarycopy;   end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// more property helper methods
function Location.GPMMag:Real;
begin   Result := pm_magnitude;   end;
//--------------------------------------
function Location.GPMDir:Real;
begin   Result := pm_posang;   end;
//-------------------------------------
function Location.GRadialV:Real;
begin   Result := radialv;   end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// string conversion helper methods
//---------------------------------
function Location.StrTo60(instr:string; out xval:Ns60int):Boolean; overload;
var sc,oval:Integer;
begin
  Result := False;
  instr := Trim(instr);
  Val(instr,oval,sc);
  if sc <> 0 then Exit;
  if (oval<0) or (oval>=60) then Exit;
  xval := oval;
  Result := True;
end;
//---------------------------------
function Location.StrTo60(instr:string; out xval:Real):Boolean; overload;
var sc:Integer;
begin
  Result := False;
  instr := Trim(instr);
  Val(instr,xval,sc);
  if sc <> 0 then Exit;
  if (xval<0) or (xval>=60) then Exit;
  Result := True;
end;
//--------------------------------
procedure Location.ReduceSexag(in_sec:Real; out sec_left:Real; out min:Ns60int; out big:Integer);
var min_sum:Integer;
begin
  in_sec := Abs(in_sec);
  min_sum := Trunc(in_sec/60.0);
  sec_left := in_sec - min_sum*60;
  min := min_sum mod 60;
  big := min_sum div 60;
end;
//--------------------------------
function Location.SetRAInternal(in_ra:string; out hrs:NsHours;out mins:Ns60int; out secs:Real):Boolean;
var list:TStringList;
    str1,str2,str3:string;
    xhrs,sc:Integer;
begin
  Result := False;
  // initial RA splitting
  in_ra := AnsiReplaceStr(in_ra,'  ',' ');
  list := SplitWithDelim(in_ra,' ',3);
  if list = nil then Exit;
  if list.Count > 3 then begin
    FreeAndNil(list);
    Exit;
  end;
  // converting RA
  str1 := list[0];     str2 := list[1];     str3 := list[2];
  FreeAndNil(list);
  Val(str1,xhrs,sc);
  if (sc<>0) then Exit;
  if (xhrs<0) or (xhrs>=24) then Exit;
  if not StrTo60(str2,mins) then Exit;
  if not StrTo60(str3,secs) then Exit;
  // done
  Result := True;
  hrs := xhrs;
end;
//------------------------------------------------------
function Location.SetDecDegDec(in_decdegdec:Real):Boolean;
var in_dec:Real;
begin
  southern := (in_decdegdec < 0);
  in_dec := Abs(in_decdegdec);
  dec_degrees := Trunc(in_dec);
  in_dec := 60*Frac(in_dec);
  dec_arcmins := Trunc(in_dec);
  dec_arcsecs := 60*Frac(in_dec);
  Result := True;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// other helper methods
//---------------------------------
procedure Location.MakeRAString;
var buildr:string;
begin
  if ra_hours < 10 then buildr := '0';
  buildr += IntToStr(ra_hours);
  buildr += ' ';
  if ra_minutes < 10 then buildr += '0';
  buildr += IntToStr(ra_minutes);
  buildr += ' ';
  if ra_seconds < 10 then buildr += '0';
  ra_hms := buildr + FloatToStrF(ra_seconds,ffFixed,2,3);
end;
//---------------------------------
procedure Location.MakeDecString;
var buildr:string;
begin
  if southern then buildr := '-'
  else buildr := '+';
  if dec_degrees < 10 then buildr += '0';
  buildr += IntToStr(dec_degrees);
  buildr += ' ';
  if dec_arcmins < 10 then buildr += '0';
  buildr += IntToStr(dec_arcmins);
  buildr += ' ';
  if dec_arcsecs < 10 then buildr += '0';
  dec_dms := buildr + FloatToStrF(dec_arcsecs,ffFixed,2,2);
end;
//---------------------------------
function Location.RA_toDegrees:Real;
var intr:Real;
begin
  intr := (ra_minutes/4) + (ra_seconds/240);
  Result := (15*ra_hours) + intr;
end;
//---------------------------------
function Location.Dec_toDegrees:Real;
var intr:Real;
begin
  intr := (dec_arcmins/60) + (dec_arcsecs/3600);
  Result := dec_degrees + intr;
  if southern then Result := -Result;
end;
//-----------------------------------
function Location.AddPToOld(zsource:string; in_pllx,in_pllxe:Real):Boolean;
var str1,str2:string;
begin
  // a source can only be addedonce
  Result := False;
  if AnsiContainsStr(oldparallax,zsource) then Exit;
  // going ahead
  if Length(oldparallax) <> 0 then oldparallax += ', ';
  str1 := Trim(FloatToStrF(in_pllx,ffFixed,3,2));
  oldparallax += str1;
  if in_pllxe <> 0 then begin
    str2 := Trim(FloatToStrF(in_pllxe,ffFixed,3,2));
    oldparallax += '±' + str2;
  end;
  if (Length(zsource)<>0) then begin
    oldparallax += ' (' + zsource + ')';
  end;
  Result := True;
end;
//------------------------------------------------------
function Location.OldParallaxParser():TStringList;
var notend:Boolean;
    parsed_list:TStringList;
    cpos,oppos,lenop:Integer;
    curpllx,currsrc:string;
begin
  // initial startup...
  Result := nil;
  lenop := Length(oldparallax);
  if lenop = 0 then Exit;
  notend := True;
  cpos := 1;
  parsed_list := TStringList.Create;
  // parsing loop
  while notend do begin
    // initial looking
    oppos := PosEx('(',oldparallax,cpos);
    if oppos < 1 then begin
      notend := False;  Break;
    end;
    // getting the parallax
    curpllx := Trim(Copy(oldparallax,cpos,oppos-cpos));
    cpos := oppos+1;
    // getting the source
    oppos := PosEx(')',oldparallax,cpos);
    if oppos < 1 then begin
      notend := False;  Break;
    end;
    currsrc := Trim(Copy(oldparallax,cpos,oppos-cpos));
    // after extract position checking...
    cpos := oppos+1;
    notend := not (cpos >= lenop);
    // checking the extracted parallax and source
    if (Length(curpllx)=0) or (Length(currsrc)=0) then Continue;
    if curpllx[1]=',' then curpllx := Trim(Copy(curpllx,2,Length(curpllx)-1));
    if Length(curpllx) = 0 then Continue;
    // adding to the list
    parsed_list.Add(curpllx);
    parsed_list.Add(currsrc);
  end;
  // post parse checking
  if parsed_list.Count < 2 then FreeAndNil(parsed_list);
  Result := parsed_list;
end;
//------------------------------------------------------
function Location.OldParallaxCleaner():Integer;
var parsed_list:TStringList;
    cleandex,checkdex:Integer;
    backup_old,currsrc:string;
    newcount,newdex:Integer;
    // logstr:string;
begin
  // initial startup...
  Result := 0;
  // parsing
  backup_old := oldparallax;
  parsed_list := OldParallaxParser();
  // some non-clean cases
  if parsed_list = nil then begin
    oldparallax := '';  Exit;
  end;
  if parsed_list.Count = 2 then begin
    oldparallax := parsed_list[0] + ' (' + parsed_list[1] +')';
    FreeAndNil(parsed_list);
    Exit;
  end;
  // the cleaning loop
  cleandex := 1;
  while cleandex < (parsed_list.Count-1) do begin
    currsrc := parsed_list[cleandex];
    checkdex := cleandex + 2;
    // inner check loop
    while checkdex < parsed_list.Count do begin
      if (parsed_list[checkdex] = currsrc) then begin
        parsed_list.Delete(checkdex-1);
        parsed_list.Delete(checkdex-1);
        Inc(Result);
      end else checkdex += 2;
    end;
    cleandex += 2;
  end;
  // after cleaning, we rebuild oldparallax
  newcount := (parsed_list.Count div 2);
  oldparallax := '';
  for newdex := 0 to (newcount-1) do begin
    if (newdex<>0) then oldparallax += ', ';
    oldparallax += parsed_list[2*newdex] + ' (';
    oldparallax += parsed_list[2*newdex+1] + ')';
  end;
  // done
  FreeAndNil(parsed_list);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// transforms and other matrix-using stuff
// for calculating space velocities, with or without a transform matrix
procedure Location.MakeVelocity(usematrix:RMatrix; out uv,vv,wv:Real);
var deg_matrix,mul_matrix,vel_vector,result_vector:RMatrix;
    ra_deg,dec_deg:Real;
    domatrix:Boolean;
begin
  // preparatory calculations
  ra_deg := RA_toDegrees;
  dec_deg := Dec_toDegrees;
  deg_matrix := MakeGalCoordMatrix(ra_deg,dec_deg);
  // possible transform matrix
  domatrix := (Length(usematrix) = 3);
  if domatrix then mul_matrix := UnchkMatrixMultiply(usematrix,deg_matrix);
  // making the velocity vector
  vel_vector := MakeMatrix(3,1);
  vel_vector[0][0] := radialv;
  vel_vector[1][0] := (kmps_auy * pm_magnitude * sind(pm_posang))/parallax;
  vel_vector[2][0] := (kmps_auy * pm_magnitude * cosd(pm_posang))/parallax;
  // applying the matrix to the velocities
  if domatrix then result_vector := UnchkMatrixMultiply(mul_matrix,vel_vector)
  else result_vector := UnchkMatrixMultiply(deg_matrix,vel_vector);
  // finishing off
  uv := result_vector[0][0];
  vv := result_vector[1][0];
  wv := result_vector[2][0];
end;
//---------------------------------------------------------
procedure Location.MakeDeltaPos(opt:Integer; out delX,delY,delZ:Real);
var xv,yv,zv:Real;
    uv,vv,wv:Real;
    qmatrix:RMatrix;
begin
  // Getting the space velocities
  SetLength(qmatrix,0);
  if opt = 0 then MakeVelocity(qmatrix,xv,yv,zv)
  else if (opt = 1) or (opt = 2) then begin
    qmatrix := GetTransformToGalactic(xepoch);
    MakeVelocity(qmatrix,uv,vv,wv);
    if opt = 2 then begin
      // astrosynthesis space velocity
      xv := -vv;  yv := wv;   zv := -uv;
    end else begin
      // space velocity is righthanded U V W
      xv := uv;   yv := vv;   zv := wv;
    end;
  end else Assert(False);
  // multiplying to get the annual ly change output
  // (aka, converting the velocity from km/s to ly/yr)
  delX := xv * KPS2LYY;
  delY := yv * KPS2LYY;
  delZ := zv * KPS2LYY;
end;
//------------------------------------------------
function Location.YearDiff(target_year:Integer):Real;
var startyear:Real;
const JultoGreg = 1.000020534302552;
      BestoGreg = 1.000000999621512;
begin
  // epoch start year
  if xepoch = eJ2000 then startyear := 2000
  else if xepoch = zJ2015 then startyear := 2015
  else if xepoch = zJ2015h then startyear := 2015.5
  else if xepoch = zJ2014 then startyear := 2014
  else if xepoch = zJ2017 then startyear := 2017
  else if xepoch = eB1950 then startyear := 1950
  else if xepoch = eB1975 then startyear := 1975
  else Assert(False);
  // subtracting light year
  startyear -= GetDistance(False);
  // converting to gregorian
  if (xepoch = eB1950) or (xepoch = eB1975) then startyear := startyear / BestoGreg
  else startyear := startyear / JultoGreg;
  // finally
  Result := target_year - startyear;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// startup methods
constructor Location.Create; overload;
begin
  inherited Create;
  uncertain := True;
  xepoch := eJ2000;
  ra_hours := 0;  ra_minutes := 0;   ra_seconds := 0.0;
  ra_hms := '00 00 00.000';
  dec_degrees := 0;  dec_arcmins := 0;   dec_arcsecs := 0.0;
  southern := False;
  dec_dms := '+00 00 00.00';
  parallax := 0.1;
  parallax_err := 0;
  binarycopy := False;
  pm_magnitude := 0;
  pm_posang := 0;
  source := '';
  radialv := 0.0;
end;
//---------------------------------------
constructor Location.Create(xsource:Location; copy:Boolean); overload;
begin
  Assert(xsource<>nil);
  inherited Create;
  // copy position
  xepoch := xsource.xepoch;
  ra_hours := xsource.ra_hours;
  ra_minutes := xsource.ra_minutes;
  ra_seconds := xsource.ra_seconds;
  ra_hms := xsource.ra_hms;

  southern := xsource.southern;
  dec_degrees := xsource.dec_degrees;
  dec_arcmins := xsource.dec_arcmins;
  dec_arcsecs := xsource.dec_arcsecs;
  dec_dms := xsource.dec_dms;

  parallax := xsource.parallax;
  parallax_err := xsource.parallax_err;
  source := xsource.source;
  uncertain := xsource.uncertain;
  oldparallax := xsource.oldparallax;
  pm_magnitude := xsource.pm_magnitude;
  pm_posang := xsource.pm_posang;
  binarycopy := copy;

  radialv := xsource.radialv;
end;
//------------------------------------------
// rough new location from old with separation and position angle
// for small separations only, cannot handle declinations too extreme
constructor Location.Create(xsource:Location; arcsec_sep,pos_angle:Double);
var dec_ra,dec_dec,modangle:Real;
    sepdeg,temp1:Real;
    twres:Boolean;
begin
    Assert(xsource<>nil);
    inherited Create;
    // copy position
    xepoch := xsource.xepoch;

    // the position is modified by the inputted separation info
    dec_ra := xsource.RA_toDegrees();
    dec_dec := xsource.Dec_toDegrees;
    // intermediate values
    pos_angle := 450.0 - pos_angle;
    modangle := pos_angle - Floor(pos_angle/360.0)*360;
    sepdeg := (arcsec_sep / 3600.0);
    temp1 := 1 / cosd(dec_dec);
    // calculating new right ascencion
    dec_ra := dec_ra + cosd(modangle)*sepdeg*temp1;
    if dec_ra < 0 then dec_ra += 360.0
    else if dec_ra >= 360 then dec_ra := dec_ra - 360.0;
    // calculating new declination
    dec_dec := dec_dec + sind(modangle)*sepdeg;

    twres := SetPositionDDeg(xepoch,dec_ra,dec_dec);
    Assert(twres);

    // copying parallax and proper motion
    parallax := xsource.parallax;
    parallax_err := xsource.parallax_err;
    source := xsource.source;
    uncertain := xsource.uncertain;
    oldparallax := xsource.oldparallax;
    pm_magnitude := xsource.pm_magnitude;
    pm_posang := xsource.pm_posang;
    binarycopy := True;
    // copying radial velocity
    radialv := xsource.radialv;
end;
//------------------------------------------
procedure Location.MakeNotCopy;
begin    binarycopy := False;    end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting the position
//---------------------------
function Location.SetPositionHMS(in_epoch:EpochType; in_ra,in_dec:string):Boolean;
var deg:Integer;
    hrs:NsHours;
    mins,arcmin:Ns60int;
    secs,arcsec:Real;
    list:TStringList;
    sc:Integer;
    str1,str2,str3:string;
begin
  Result := False;
  // initial RA splitting
  if not SetRAInternal(in_ra,hrs,mins,secs) then Exit;

  // initial Dec splitting
  in_dec := AnsiReplaceStr(in_dec,'  ',' ');
  list := SplitWithDelim(in_dec,' ',3);
  if list = nil then Exit;
  if list.Count > 3 then begin
    FreeAndNil(list);
    Exit;
  end;
  // converting RA
  str1 := list[0];     str2 := list[1];     str3 := list[2];
  FreeAndNil(list);
  Val(str1,deg,sc);
  if (sc<>0) then Exit;
  if (deg<-90) or (deg>=90) then Exit;
  if not StrTo60(str2,arcmin) then Exit;
  if not StrTo60(str3,arcsec) then Exit;
  if (Abs(deg)=90) and not (((arcmin = 0) and (arcsec = 0.0))) then Exit;

  // everything passed here!
  // setting the internal values
  // right ascension
  ra_hours := hrs;
  ra_minutes := mins;
  ra_seconds := secs;
  MakeRAString;
  // declination
  southern := (deg < 0) or (str1[1] = '-');
  dec_degrees := Abs(deg);
  dec_arcmins := arcmin;
  dec_arcsecs := arcsec;
  MakeDecString;
  // final values
  xepoch := in_epoch;
  Result := True;
end;
//---------------------------
function Location.SetPositionDDeg(in_epoch:EpochType; in_ra,in_dec:string):Boolean; overload;
var rareal,decreal:Real;
begin
  Result := False;
  if not StrToRealBoth(in_ra,in_dec,rareal,decreal) then Exit;
  Result := SetPositionDDeg(in_epoch,rareal,decreal);
end;
//---------------------------
function Location.SetPositionDDeg(in_epoch:EpochType; in_ra,in_dec:Real):Boolean; overload;
begin
  // initial checking...
  Result := False;
  if (in_ra < 0) or (in_ra>=360) then Exit;
  if (in_dec < -90) or (in_dec > 90) then Exit;
  // right ascension
  in_ra := in_ra/15;
  ra_hours := Trunc(in_ra);
  in_ra := Frac(in_ra)*60;
  ra_minutes := Trunc(in_ra);
  ra_seconds := 60*Frac(in_ra);
  // declination
  SetDecDegDec(in_dec);
  southern := (in_dec < 0);
  in_dec := Abs(in_dec);
  dec_degrees := Trunc(in_dec);
  in_dec := 60*Frac(in_dec);
  dec_arcmins := Trunc(in_dec);
  dec_arcsecs := 60*Frac(in_dec);
  // finalizing
  xepoch := in_epoch;
  MakeRAString;
  MakeDecString;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting the parallax
//----------------------------
function Location.SetParallax(in_median,in_error:Real):Boolean; overload;
begin
  Result := False;
  if (in_error < 0) then Exit;
  if (in_median <= 0) then Exit;
  Result := True;
  parallax := in_median;
  parallax_err := in_error;
  binarycopy := False;
end;
//----------------------------
function Location.SetParallax(in_median,in_error:string):Boolean; overload;
var preal,pereal:Real;
begin
  Result := False;
  if not StrToRealBoth(in_median,in_error,preal,pereal) then Exit;
  Result := SetParallax(preal,pereal);
end;
//-----------------------------
function Location.SetParallaxArcSeconds(in_median,in_error:string):Boolean;
var preal,pereal:Real;
begin
  Result := False;
  if not StrToRealBoth(in_median,in_error,preal,pereal) then Exit;
  preal := 1000*preal;
  pereal := 1000*pereal;
  Result := SetParallax(preal,pereal);
end;
//-----------------------------
function Location.UpdateParallax(in_median,in_errorx:Real):Boolean; overload;
var old_pllx,old_pllxe:Real;
    actdiff,maxdiff:Real;
begin
  old_pllx := parallax;
  old_pllxe := parallax_err;
  Result := SetParallax(in_median,in_errorx);
  if not Result then Exit;
  // the difference between this method and SetParallax is that we keep the old parallax
  oldparallax := Trim(oldparallax);
  if Length(oldparallax)<>0 then oldparallax += ', ';
  oldparallax += Trim(FloatToStrF(old_pllx,ffFixed,3,2)) + '±';
  oldparallax += Trim(FloatToStrF(old_pllxe,ffFixed,3,2)) + ' (';
  oldparallax += source + ')';
  // also, I set uncertain
  uncertain := (in_errorx >= 4);
  if (not uncertain) then begin
    actdiff := Abs(in_median-old_pllx);
    maxdiff := 2*Math.Max(old_pllxe,in_errorx);
    uncertain := (actdiff >maxdiff) and (actdiff>2);
  end;
  // the Result is True here
end;
//------------------------------------------
function Location.UpdateParallax(in_median,in_errorx:string):Boolean; overload;
var preal,pereal:Real;
begin
  Result := False;
  if not StrToRealBoth(in_median,in_errorx,preal,pereal) then Exit;
  Result := UpdateParallax(preal,pereal);
end;
//------------------------------------------
function Location.CheckEqParallax(in_median,in_errorx:string):Boolean;
var preal,pereal,diffp,diffpe:Real;
begin
  Result := False;
  if not StrToRealBoth(in_median,in_errorx,preal,pereal) then Exit;
  diffp := Abs(parallax-preal);
  diffpe := Abs(parallax_err-pereal);
  Result := (diffp < 0.0001) and (diffpe < 0.0001);
end;
//---------------------------------------------------------
function Location.SetParallaxPasted(indata:string; out xsrc:string):Boolean;
var xpllx,xpllxe:Real;
    outxsrc:string;
begin
  Result := ParsePastedParallax(indata,outxsrc,xpllx,xpllxe);
  if not Result then Exit;
  Result := UpdateParallax(xpllx,xpllxe);
  if Result then source := outxsrc;
  xsrc := outxsrc + ' ';
end;
//----------------------------------------------------------
// used to provide a non-manual way of switching back to an old parallax...
function Location.SwapParallax(last:Boolean):Boolean;
var olist:TStringList;
    bufx,buf1,buf2,osrc:string;
    opllx,opllxe:Real;
    spos,epos,pmlen:Integer;
begin
  Result := False;
  bufx := Trim(oldparallax);
  if Length(bufx) = 0 then Exit;
  // getting the old parallax in string form pllx±err (source)
  olist := SplitWithDelim(bufx,',',1);
  if (olist.Count = 1) or (not last) then bufx := olist[0]
  else bufx := olist[olist.Count-1];
  // parsing the old string parallax (finding the split locations)
  spos := AnsiPos('±',bufx);
  if spos < 1 then begin
    FreeAndNil(olist);
    Exit;
  end;
  epos := PosEx('(', bufx,spos);
  if epos < 1 then begin
    FreeAndNil(olist);
    Exit;
  end;
  // splitting...
  buf1 := Trim(Copy(bufx,1,spos-1));
  pmlen := Length('±');
  buf2 := Trim(Copy(bufx,spos+pmlen,epos-spos-pmlen));
  if not StrToRealBoth(buf1,buf2,opllx,opllxe) then begin
    FreeAndNil(olist);
    Exit;
  end;
  // getting the old parallax source
  spos := PosEx(')', bufx,epos+1);
  if spos < 1 then begin
    FreeAndNil(olist);
    Exit;
  end;
  osrc := Trim(Copy(bufx,epos+1,spos-epos-1));
  if Length(osrc) = 0 then begin
    FreeAndNil(olist);
    Exit;
  end;
  // done with parsing, remove the old parallax...
  if (olist.Count = 1) then oldparallax := ''
  else begin
    if last then olist.Delete(olist.Count-1)
    else olist.Delete(0);
    oldparallax := olist.DelimitedText;
  end;
  FreeAndNil(olist);
  // replacing the current parallax with the extracted old one...
  Result := UpdateParallax(opllx,opllxe);
  source := osrc;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting proper motion
//------------------------------------------
function Location.SetProperMotion(in_magnitude,in_angle:Real):Boolean; overload;
begin
  Result := False;
  if (in_magnitude < 0) or (in_magnitude > 99000) then Exit;
  if (in_angle < 0) or (in_angle >= 360) then Exit;
  pm_magnitude := in_magnitude;
  pm_posang := in_angle;
  Result := True;
end;

//------------------------------------------
function Location.SetProperMotion(in_magnitude,in_angle:string):Boolean; overload;
var pmmag,pmang:Real;
begin
  Result := False;
  if not StrToRealBoth(in_magnitude,in_angle,pmmag,pmang) then Exit;
  Result := SetProperMotion(pmmag,pmang);
end;
//------------------------------------------
function Location.SetProperMotionArcSeconds(in_magnitude,in_angle:string):Boolean; overload;
var pmmag,pmang:Real;
begin
  Result := False;
  if not StrToRealBoth(in_magnitude,in_angle,pmmag,pmang) then Exit;
  pmmag := 1000*pmmag;
  Result := SetProperMotion(pmmag,pmang);
end;
//------------------------------------------
function Location.SetRadialVelocity(in_radialv:Real):Boolean;
begin
  Result := False;
  if in_radialv < -9999.9 then Exit;
  if in_radialv > 9999.9 then Exit;
  Result := True;
  radialv := in_radialv;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// getting data (requiring calculation)
//-----------------------------------
function Location.GetDecimalRightAscension:Real;
begin  Result:= RA_toDegrees;   end;
//-----------------------------------
function Location.GetDecimalDeclination:Real;
begin  Result:= Dec_toDegrees;   end;
//-----------------------------------
procedure Location.GetArcsecPosition(secra:Boolean; out rapos,decpos:Real);
var temp1,temp2:Real;
begin
  temp1 := 3600.0*ra_hours + 60.0*ra_minutes + ra_seconds;
  if (not secra) then temp1 := temp1*15.0;
  rapos := temp1;
  temp2 := 3600.0*dec_degrees + 60.0*dec_arcmins;
  temp2 += dec_arcsecs;
  if southern then decpos := -temp2
  else decpos := temp2;
end;
//-----------------------------------
function Location.GetDistance(parsecs:Boolean):Real;
var inversv:Real;
begin
  inversv := 1 / parallax;
  if parsecs then Result := 1000*inversv
  else Result := inversv*3261.56378;
end;
//-----------------------------------
function Location.GetDistanceAtYear(parsecs:Boolean; targyear:Integer):Real;
var xpos,ypos,zpos,distt:Real;
begin
  Assert(targyear>=0);
  GetXYZatYear(targyear,xpos,ypos,zpos);
  distt := hypot3(xpos,ypos,zpos);
  if parsecs then Result := distt / 3.261563777
  else Result := distt;
end;
//------------------------------------
function Location.GetDistanceStr(decimp,targyear:Integer; parsecs:Boolean):string;
var distc:Real;
begin
  Assert(decimp>=0);
  if targyear < 0 then distc := GetDistance(parsecs)
  else distc := GetDistanceAtYear(parsecs,targyear);
  Result := Trim(FloatToStrF(distc,ffFixed,6,decimp));
end;
//------------------------------------
function Location.GetDistanceExtremaStr(min,double:Boolean; prec:Integer):string;
var diff,mpar,inver:Real;
begin
  Assert(prec>=0);
  // getting the extreme parallax
  diff := parallax_err;
  if double then diff += parallax_err;
  if min then mpar := parallax + diff
  else mpar := parallax - diff;
  // a special case
  if mpar <= 0 then Result := '?Infinity?'
  else begin
    inver := 3261.56378 / mpar;
    Result := Trim(FloatToStrF(inver,ffFixed,10,prec));
  end;
end;
//------------------------------------
function Location.GetLYRange(double:Boolean):Real;
var invers1,invers2,diff:Real;
begin
  if double then invers1 := 1/(parallax+2*parallax_err)
  else invers1 := 1/(parallax+parallax_err);
  if double then invers2 := 1/(parallax-2*parallax_err)
  else invers2 := 1/(parallax-parallax_err);
  diff := Abs(invers2-invers1);
  Result := diff*3261.56378;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function Location.GetParallaxString(prec:Integer; padding:Boolean):string;
var pbase:string;
begin
  Assert(prec>=0);
  pbase := Trim(FloatToStrF(parallax,ffFixed,3,prec));
  if padding then begin
    if parallax < 10 then pbase := '0' + pbase;
    if parallax < 100 then pbase := ' ' + pbase;
  end;
  Result := pbase;
end;
//------------------------------------
function Location.GetParallaxErrString(prec:Integer; padding:Boolean):string;
var pbase:string;
begin
  Assert(prec>=0);
  pbase := Trim(FloatToStrF(parallax_err,ffFixed,3,prec));
  if padding then begin
    if parallax_err < 10 then pbase := '0' + pbase;
    if parallax_err < 100 then pbase := ' ' + pbase;
  end;
  Result := pbase;
end;
//------------------------------------
function Location.GetProperMotionMagStr(prec:Integer):string;
begin
  Assert(prec>=0);
  Result := Trim(FloatToStrF(pm_magnitude,ffFixed,5,prec));
end;
//------------------------------------
function Location.GetProperMotionAngleStr(prec:Integer; padding:Boolean):string;
var tempr:string;
begin
  Assert(prec>=0);
  tempr := Trim(FloatToStrF(pm_posang,ffFixed,3,prec));
  if padding then begin
    if pm_posang < 10 then tempr := '0' + tempr;
    if pm_posang < 100 then tempr := '0' + tempr;
  end;
  Result := tempr;
end;
//------------------------------------
function Location.GetRadialVStr(prec:Integer):string;
begin
  Assert(prec>=0);
  Result := Trim(FloatToStrF(radialv,ffFixed,5,prec));
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Getting XYZ coordinates
//------------------------------------
procedure Location.GetXYZ(out xo,yo,zo:Real);
var dist,ra_deg,dec_deg:Real;
    tra,tdec:Real;
    interm1,interm2:Real;
    tran_matrix:RMatrix;
begin
  // basic values
  dist := GetDistance(False);
  ra_deg := RA_toDegrees;
  dec_deg := Dec_toDegrees;
  // transformations?
  if (xepoch = eB1950) or (xepoch = eB1975) then begin
    tran_matrix := GetTransformToJ2000(xepoch);
    UnchkMatrixTransform(ra_deg,dec_deg,tran_matrix,tra,tdec);
  end else begin
    tra := ra_deg; tdec := dec_deg;
  end;
  // intemediate calculations
  interm1 := dist*cosd(tra);
  interm2 := cosd(tdec);
  // producing the results
  xo := interm1*interm2;
  interm1 := dist*sind(tra);
  yo := interm1*interm2;
  zo:= dist*sind(tdec);

end;
//------------------------------------
function Location.GetXYZ_CSV(prec:Integer):string;
var xres,yres,zres:Real;
begin
  Assert(prec>=0);
  GetXYZ(xres,yres,zres);
  Result := Trim(FloatToStrF(xres,ffFixed,5,prec)) + ',';
  Result += Trim(FloatToStrF(yres,ffFixed,5,prec)) + ',';
  Result += Trim(FloatToStrF(zres,ffFixed,5,prec));
end;
//---------------------------------------
procedure Location.GetXYZatYear(const inyear:Integer; out xo,yo,zo:Real);
var xres,yres,zres:Real;
    delx,dely,delz:Real;
    yeardiffz:Real;
begin
  // getting initial position and change per year
  GetXYZ(xres,yres,zres);
  MakeDeltaPos(0,delx,dely,delz);
  // calculating the year diff, we assume superluminal travel
  yeardiffz := YearDiff(inyear);
  // final value
  xo := xres + yeardiffz*delx;
  yo := yres + yeardiffz*dely;
  zo := zres + yeardiffz*delz;
end;
//-----------------------------------------
function Location.GetXYZatYear_CSV(const inyear,prec:Integer):string;
var xres,yres,zres:Real;
begin
  Assert(prec>=0);
  GetXYZatYear(inyear,xres,yres,zres);
  Result := Trim(FloatToStrF(xres,ffFixed,5+prec,prec)) + ',';
  Result += Trim(FloatToStrF(yres,ffFixed,5+prec,prec)) + ',';
  Result += Trim(FloatToStrF(zres,ffFixed,5+prec,prec));
end;
//------------------------------------------
(* conversion uisng matrix multiplication (my pervious version did not work? *)
procedure Location.GetGalacticCoords(out lat,long:Real);
var deg_ra, deg_dec:Real;
    transmatrix:RMatrix;
begin
  // making versions of RA and Dec in Degrees
  deg_ra := RA_toDegrees;
  deg_dec := Dec_toDegrees;
  // matrix transform
  transmatrix := GetTransformToGalactic(xepoch);
  MatrixTransform(deg_ra,deg_dec,transmatrix,long,lat);
end;
//----------------------------------------------
function Location.GetGalacticCoordString():string;
var olat,olong:Real;
begin
  GetGalacticCoords(olat,olong);
  Result := 'Lat : ';
  Result += Trim(FloatToStrF(olat,ffFixed,9,6)) + '° , Long : ';
  Result += Trim(FloatToStrF(olong,ffFixed,9,6));
end;
//----------------------------------------------
(* XYZ that uses galactic coordinates instead of RA and Dec. one function,
since that way I only call GetGalacticCoords once. *)
procedure Location.GetGalacticXYZ(out galx,galy,galz:Real);
var gallat,gallong:Real;
    lydist:Real;
    dcosglat:Real;
begin
  // source values for computing x, y, z
  GetGalacticCoords(gallat,gallong);
  lydist := GetDistance(False);
  // the only intermediate value
  dcosglat := lydist *Cosd(gallat);
  // producing x, y, z
  galx := dcosglat * Cosd(gallong);
  galy := dcosglat * Sind(gallong);
  galz := lydist * Sind(gallat);
end;
//----------------------------------------------
(* The 3d star viewing and system generation program Astrosynthesis uses a
'modified' Galactic XYZ. This method is to generate Astrosynthesis compatible values. *)
procedure Location.GetAstrosynthesisXYZ(out astr_x,astr_y,astr_z:Real);
var gal_x, gal_y, gal_z:Real;
begin
  GetGalacticXYZ(gal_x, gal_y, gal_z);
  astr_x := -gal_y;
  astr_y := gal_z;
  astr_z := -gal_x;
end;
//----------------------------------------------
procedure Location.GetAstrosynthesisXYZatYear(const inyear:Integer; out astr_x,astr_y,astr_z:Real);
var axres,ayres,azres:Real;
    delx,dely,delz:Real;
    yeardiffz:Real;
begin
  // getting initial position and change per year
  GetAstrosynthesisXYZ(axres,ayres,azres);
  MakeDeltaPos(2,delx,dely,delz);
  // calculating the year diff, we assume superluminal travel
  yeardiffz := YearDiff(inyear);
  // final value
  astr_x := axres + yeardiffz*delx;
  astr_y := ayres + yeardiffz*dely;
  astr_z := azres + yeardiffz*delz;
end;
//----------------------------------------------
function Location.MakeAstrosynXYZString(precision:Integer):string;
var astrx, astry, astrz:Real;
begin
  Assert(precision>=0);
  GetAstrosynthesisXYZ(astrx,astry,astrz);
  Result := Trim(FloatToStrF(astrx,ffFixed,5+precision,precision));
  Result += ',' + Trim(FloatToStrF(astry,ffFixed,5+precision,precision));
  Result += ',' + Trim(FloatToStrF(astrz,ffFixed,5+precision,precision));
end;
//----------------------------------------------
function Location.GetAstrosynthesisXYZatYear_String(const inyear,precison:Integer):string;
var astrx, astry, astrz:Real;
begin
  Assert(precison>=0);
  GetAstrosynthesisXYZatYear(inyear,astrx,astry,astrz);
  Result := Trim(FloatToStrF(astrx,ffFixed,5+precison,precison));
  Result += ',' + Trim(FloatToStrF(astry,ffFixed,5+precison,precison));
  Result += ',' + Trim(FloatToStrF(astrz,ffFixed,5+precison,precison));
end;
//----------------------------------------------
function Location.MakeAstrosynXYZ_AUOffsetString(const base:Location; out offs:string):string;
var this_x, this_y, this_z:Real;
    base_x, base_y, base_z:Real;
    au_x, au_y, au_z:Real;
    buffer,result2:string;
const au_per_ly = 63239.7263;
begin
  Assert(base<>nil);
  // getting X Y Z for the two locations
  GetAstrosynthesisXYZ(this_x,this_y,this_z);
  base.GetAstrosynthesisXYZ(base_x,base_y,base_z);
  // getting the au offsets
  au_x := (base_x - this_x) * au_per_ly;
  au_y := (base_y - this_y) * au_per_ly;
  au_z := (base_z - this_z) * au_per_ly;
  // making the string outputs
  Result := Trim(FloatToStrF(au_x,ffFixed,7,1));
  result2 := Trim(FloatToStrF(au_x+2,ffFixed,7,1));
  buffer := Trim(FloatToStrF(au_y,ffFixed,7,1));
  Result += ',' + buffer;
  result2 += ',' + buffer;
  buffer := Trim(FloatToStrF(au_z,ffFixed,7,1));
  Result += ',' + buffer;
  offs := result2 + ',' + buffer;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// specialty functions
function Location.BinaryUpdate(parent:Location):Boolean;
begin
  Result := False;
  if parent = nil then Exit;
  if not binarycopy then Exit;

  // copying over the stuff
  parallax := parent.parallax;
  parallax_err := parent.parallax_err;
  radialv := parent.radialv;
  uncertain := parent.uncertain;
  source := parent.source;
  // done
  Result := True;
end;
//----------------------------------------------------
function Location.GetDistanceFrom(other:Location; targyear:Integer):Real;
var x1,y1,z1:Real;
    x2,y2,z2:Real;
begin
  // getting the values
  if targyear < 0 then GetXYZ(x1,y1,z1)
  else GetXYZatYear(targyear,x1,y1,z1);
  if other <> nil then begin
    if targyear < 0 then other.GetXYZ(x2,y2,z2)
    else other.GetXYZatYear(targyear,x2,y2,z2);
  end
  else begin
    x2 := 0;  y2 := 0;  z2 := 0;
  end;
  // subtracting
  x1 := x1-x2;
  y1 := y1-y2;
  z1 := z1-z2;
  // finishing
  Result := hypot3(x1,y1,z1);
end;
//--------------------------------------------------
function Location.GetRADecDifference(const newloc:Location; out radfs,decdfas:Real):Boolean;
var new_rasec, new_decarcsec:Real;
    cur_rasec,cur_decarcsec:Real;
begin
  // trivial bad cases
  Result := False;
  if newloc = nil then Exit;
  // on to the calculation...
  Result := True;
  newloc.GetArcsecPosition(True,new_rasec,new_decarcsec);
  GetArcsecPosition(True,cur_rasec,cur_decarcsec);
  radfs := new_rasec - cur_rasec;
  decdfas := new_decarcsec - cur_decarcsec;
end;
//--------------------------------------------------
function Location.AddRADecDifference(const radfs,decfas:Real):Boolean;
var ra_sec,dec_asec:Real;
    new_ras,new_decas:Real;
    temp_deg,temp_hours:Integer;
const asecndeg = 90*3600;
      seccircle = 24*3600;
begin
  Result := False;
  // getting changed values in seconds (for RA) and arcsecons (for Dec)
  GetArcsecPosition(True,ra_sec,dec_asec);
  new_ras := ra_sec + radfs;
  new_decas := dec_asec + decfas;
  // converting the changed RA to internal Hours Minutes Seconds
  // adjusting for wrapping
  if new_ras < 0 then new_ras := seccircle + new_ras
  else if new_ras >= seccircle then new_ras := new_ras - seccircle;
  ReduceSexag(new_ras,ra_seconds,ra_minutes,temp_hours);
  ra_hours := NsHours(temp_hours);
  MakeRAString;
  // converting the changed Dec to internal Degrees ArcMinutes ArcSeconds
  // this is not really a good way of handling wrapping declination
  if new_decas > asecndeg then new_decas := 2*asecndeg - new_decas
  else if new_decas < -asecndeg then new_decas := -2*asecndeg - new_decas;
  // convering to degrees, arcminutes, arcseconds
  ReduceSexag(new_decas, dec_arcsecs,dec_arcmins,temp_deg);
  southern := (new_decas < 0);
  dec_degrees := temp_deg;
  MakeDecString;
  // done
  Result := True;
end;
//----------------------------------------------------------
function Location.GetArsecSeparationFrom(other:Location):Real;
var ra1,ra2,dec1,dec2:Real;
    interm1,interm2,interm3:Real;
begin
  Assert(other<>nil);
  // getting degrees of both locations
  ra1 := RA_toDegrees();
  dec1 := Dec_toDegrees();
  ra2 := other.RA_toDegrees;
  dec2 := other.Dec_toDegrees;
  // special case
  if (ra1 = ra2) and (dec1 = dec2) then begin
    Result := 0;
    Exit;
  end;
  // intermediate calaculations (equation is from Wikipedia)
  interm1 := sind(dec1)*sind(dec2);
  interm2 := cosd(dec1)*cosd(dec2);
  interm3 := cosd(ra1-ra2);
  interm1 := interm1 + interm2*interm3;
  // finishing off
  Result := arccosd(interm1);
  Result := 3600*result;
end;
//-------------------------------------------------------
function Location.PositionProperMotionMatch(other:Location; max_dist,max_pmangdiff,max_pmmagpdiff,max_pmmagdiff:Real):Boolean;
var pmang_diff,pmmag_diff,pmmag_percent:Real;
    arcsec_sep:Real;
    absdiff,pdiff:Boolean;
begin
  Result := False;
  // asserts for bad input
  Assert(other<>nil);
  Assert(max_pmangdiff<180);

  // calculating difference in proper motion position angles
  pmang_diff := Abs(pm_posang - other.pm_posang);
  if pmang_diff > 180 then pmang_diff := 360 - pmang_diff;
  // reject test
  if pmang_diff >= max_pmangdiff then Exit;

  // difference between the magnitude of the proper motion
  pmmag_diff := Abs(pm_magnitude-other.pm_magnitude);
  absdiff := (pmmag_diff < max_pmmagdiff);
  if pm_magnitude > 0 then begin
    pmmag_percent :=  (pmmag_diff/pm_magnitude)*100;
    pdiff := (pmmag_percent < max_pmmagpdiff);
  end else pdiff := False;
  if not (pdiff or absdiff) then Exit;

  // position difference
  arcsec_sep := GetArsecSeparationFrom(other);
  Result := arcsec_sep < max_dist;
end;
//-------------------------------------------------
function Location.GetUVWVelocities(lefthanded:Boolean; out uv,vv,wv:Real):Boolean;
var transform_matrix:RMatrix;
begin
  Result := True;
  // the transform matrix depends on Epoch
  transform_matrix := GetTransformToGalactic(xepoch);
  // computing...
  MakeVelocity(transform_matrix,uv,vv,wv);
  if lefthanded then uv := -uv;
end;
//-------------------------------------------------
function Location.GetUVWString(lefthanded:Boolean):string;
var rok:Boolean;
    uout,vout,wout:Real;
begin
  rok := GetUVWVelocities(lefthanded,uout,vout,wout);
  Assert(rok);
  Result := 'U: ' + Trim(FloatToStrF(uout,ffFixed,7,2)) + ' km/s , ';
  Result += 'V: ' + Trim(FloatToStrF(vout,ffFixed,7,2)) + ' km/s , ';
  Result += 'W: ' + Trim(FloatToStrF(wout,ffFixed,7,2)) + ' km/s';
end;
//-------------------------------------------------
(* Estimates a J2015.5 position for the location. This is used to help
match the star up with a star from GAIA DR2. *)
procedure Location.MakeGetJ2015p5Pos(out radeg:Real; out decdeg:Real);
var startra,startdec:Real;
    timeoffset, cosdec:Real;
    okay:Boolean;
    pmra,pmdec:Real;
begin
  startra := GetDecimalRightAscension();
  startdec := GetDecimalDeclination();
  // in this case, it is already done...
  if (xepoch = zJ2015h) then begin
    radeg := startra;
    decdeg := startdec;
  end
  else begin
    (* since the time period will usualy be 15.5 years or less, and great
    precision is unneeded, radial velocity and matrix transforms will not be
    into account (except for B to J) *)
    if xepoch = eB1950 then begin
      okay := MatrixTransform(startra,startdec,B1950toJ2000,startra,startdec);
      Assert(okay);
      timeoffset := 65.5002175;
    end else if xepoch = eB1975 then begin
      okay := MatrixTransform(startra,startdec,B1975toJ2000,startra,startdec);
      Assert(okay);
      timeoffset := 40.500752;
    end else if xepoch = eJ2000 then timeoffset := 15.5
    else if xepoch = zJ2014 then timeoffset := 1.5
    else if xepoch = zJ2015 then timeoffset := 0.5
    else if xepoch = zJ2017 then timeoffset := -1.5
    else Assert(false);
  end;
  // converting proper motion...
  pmdec := pm_magnitude * cosd(pm_posang);
  pmra := pm_magnitude * sind(pm_posang);
  pmdec := pmdec / 3600000;
  pmra := pmra / 3600000;
  // new position
  decdeg := startdec + pmdec * timeoffset;
  if decdeg >= 90 then decdeg := 89.999999
  else if decdeg <= -90 then decdeg := -89.999999;
  cosdec := cosd((decdeg + startdec)/2);
  radeg := startra + ((timeoffset * pmra)/cosdec);
  if radeg < 0 then radeg := 360.0 + radeg
  else if radeg >= 360.0 then radeg -= 360.0
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// importing data from special sources
function Location.SetFromTGAS(instar:TGASData):Boolean;
var uhrs:Integer;   umin:Integer;   hsec:Double;
    pmmag, pmang:Double;
    ddeg:Real;
begin
  // bas cases
  Result := False;
  if instar = nil then Exit;
  if not instar.DataIsSet then Exit;
  // otherwise
  // right ascension
  instar.GetRA_HMS(uhrs,umin,hsec);
  ra_hours := NsHours(uhrs);
  ra_minutes := Ns60int(umin);
  RA_seconds := hsec;
  MakeRAString;
  // declination
  ddeg := instar.GetDecDegrees;
  SetDecDegDec(ddeg);
  MakeDecString;
  // parallax
  if (Length(source) = 0) or (source='TGAS') then begin
    parallax := instar.GetParallax;
    parallax_err := instar.GetParallaxError;
    uncertain := (parallax_err >= 4);
    binarycopy := False;
  end
  else begin
    UpdateParallax(instar.GetParallax,instar.GetParallaxError);
  end;
  xepoch := zJ2015;
  source := 'TGAS';

  // proper motion
  instar.GetPMOut(pmmag,pmang);
  pm_magnitude := pmmag;
  pm_posang := pmang;
  // done, radial velocity is not included in TGAS, so we do not touch it
  Result := True;
end;
//---------------------------------------------
function Location.SetFromImported(instar:ImportedData; epch:EpochType; srcin:string; setlocat:Boolean):Boolean;
var pllx_diff,pllxe_diff:Real;
    usepllx,setok:Boolean;
begin
  // bad cases
  Result := False;
  if instar = nil then Exit;
  // checking the differences
  pllx_diff := Abs(parallax-instar.pllx);
  pllxe_diff := Abs(parallax_err-instar.pllx_err);
  // parallaxes are pretty much the same (any diff likely due to rounding)
  if (pllx_diff < 0.002) and (pllxe_diff < 0.002) then Exit;
  // use parallax
  usepllx := (instar.pllx_err < parallax_err) and (source<>'TGAS');

  // setting the parallax ...
  if (parallax_err <> 0) and (not usepllx) then begin
    AddPToOld(srcin,instar.pllx,instar.pllx_err);
    Exit;
  end;
  // parallax
  if (Length(source) = 0) or (source=srcin) then begin
    SetParallax(instar.pllx,instar.pllx_err);
    uncertain := (parallax_err >= 4);
  end
  else begin
    UpdateParallax(instar.pllx,instar.pllx_err);
  end;
  source := srcin;

  // setting proper motion
  if instar.pm_ang <> 0 then begin
    SetProperMotion(instar.pm_mag,instar.pm_ang);
  end;
  // next up, setting location ...
  if setlocat then begin
    setok := SetPositionHMS(epch,instar.rapos,instar.decpos);
    if (not setok) then Exit;
  end;
  // finally
  Result := True;
end;
//--------------------------------------------------
function Location.SetFromGaiaDR2(inastro:GaiaDR2Astrometry; ckeepold:Boolean):Boolean;
var uhrs:Integer;   umin:Integer;   hsec:Double;
    pmmag, pmang:Double;
    ddeg:Real;
begin
  // bas cases
  Result := False;
  if inastro = nil then Exit;
  // otherwise
  if not SetPositionDDeg(zJ2015h,inastro.rapos,inastro.decpos) then Exit;
  // parallax
  if (Length(source) = 0) or (source = GAIA2_TAG) then begin
    parallax := inastro.parallax;
    parallax_err := inastro.parallax_err;
    uncertain := (parallax_err >= 4);
    binarycopy := False;
  end
  else begin
    // if ckeepold = True, we keep the old parallax if the old error is smaller
    if (inastro.parallax_err > parallax_err) and ckeepold then begin
      AddPToOld(GAIA2_TAG,inastro.parallax,inastro.parallax_err);
      Exit;
    end;
    // otherwise, we use the Gaia DR2 Parallax
    binarycopy := False;
    UpdateParallax(inastro.parallax,inastro.parallax_err);
  end;
  source := GAIA2_TAG;
  // proper motion
  ProperMotionConvert(inastro.decpm,inastro.rapm,pm_magnitude,pm_posang);
  // radial velocity
  if inastro.hasRV then radialV := inastro.RV;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// to and from strings as a whole
//------------------------------------------
function Location.ConvertAllToString:string;
var buffer:string;
begin
  Result := EpochNames[Ord(xepoch)] + ';';
  Result += ra_hms + ';' + dec_dms + ';';
  buffer := Trim(FloatToStrF(parallax,ffFixed,3,3));
  Result += buffer +';';
  buffer := Trim(FloatToStrF(parallax_err,ffFixed,3,3));
  Result += buffer +';' + Bool2Str(binarycopy) + ';';
  buffer := AnsiReplaceStr(oldparallax,';',':');
  Result += buffer +';';
  buffer := Trim(FloatToStrF(pm_magnitude,ffFixed,5,2));
  Result += buffer +';';
  buffer := Trim(FloatToStrF(pm_posang,ffFixed,3,2));
  Result += buffer +';';
  buffer := AnsiReplaceStr(source,';',':');
  Result += source + ';' + Bool2Str(uncertain) + ';';
  Result += Trim(FloatToStrF(radialv,ffFixed,5,2));
end;
//------------------------------------------
function Location.ConvertAllFromString(inval:string):Boolean;
var inlist:TStringList;
    epochdex:Integer;
    buffer:string;
begin
  Result := False;
  inlist := SplitWithDelim(inval,';',12);
  if inlist = nil then Exit;
  // epoch
  epochdex := AnsiIndexStr(inlist[0],EpochNames);
  if epochdex < 0 then begin
    FreeAndNil(inlist);    Exit;
  end;
  // right ascension and declination
  if not SetPositionHMS(EpochType(epochdex),inlist[1],inlist[2]) then begin
    FreeAndNil(inlist);    Exit;
  end;
  // parallax
  if not SetParallax(inlist[3],inlist[4]) then begin
    FreeAndNil(inlist);    Exit;
  end;
  // parallax is a copy flag
  if not Str2Bool(inlist[5],binarycopy) then begin
    FreeAndNil(inlist);    Exit;
  end;
  // old parallax string
  oldparallax := inlist[6];
  // OldParallaxCleaner();
  if AnsiStartsStr(',',oldparallax) then begin
    oldparallax := Trim(AnsiRightStr(oldparallax,Length(oldparallax)-1));
  end;
  // proper motion
  if not SetProperMotion(inlist[7],inlist[8]) then begin
    FreeAndNil(inlist);    Exit;
  end;
  // source
  source := inlist[9];
  (* fixing wrong TGAS data...
  if (source = 'TGAS') then begin
    pm_posang += 180;
    if pm_posang >= 360 then pm_posang -= 360;
  end;        *)
  // uncertain parallax plag
  if not Str2Bool(inlist[10],uncertain) then begin
    FreeAndNil(inlist);    Exit;
  end;
  // radial velocity
  buffer := inlist[11];
  FreeAndNil(inlist);
  if (not StrToReal(buffer,radialv)) then Exit;
  // done
  Result := True;
end;

//------------------------------------------
(* converts from old LocationInfo string, example:
C;2000.00;04 38 12.559;101580.110;0.825000 0.0031000; 0.3811 100.3400;35.700
certainty;epoch;ra;dec (arcseconds );parallax pllx error(arcsec);pm ang(arcsec deg);radial v
*)
function Location.ConvertAllFromStringOld(inval:string):Boolean;
var inlist:TStringList;
    xhrs:NsHours;
    temp_min,temp_arcmin:Ns60int;
    temp_sec,temp_arcsec,tempreal:Real;
    temp_southern:Boolean;
    temp_xmin,temp_deg,sc:Integer;
    buffer1,buffer2:string;
    temp_parallax,temp_pllxerr:Real;
begin
  Result := False;
  inlist := SplitWithDelim(inval,';',7);
  if inlist = nil then Exit;
  // uncertainty
  uncertain := ((inlist[0])<>'C');
  // epoch
  if inlist[1] = '2000.00' then xepoch := eJ2000
  else if inlist[1] = '2015.00' then xepoch := zJ2015;
  // right ascension
  if not SetRAInternal(inlist[2],xhrs,temp_min,temp_sec) then begin
    FreeAndNil(inlist);    Exit;
  end;
  // declination is in arcseconds
  Val(inlist[3],tempreal,sc);
  if sc <> 0 then begin
    FreeAndNil(inlist);    Exit;
  end;
  temp_southern := (tempreal < 0);
  tempreal := Abs(tempreal);
  temp_xmin := Trunc(tempreal/60);
  temp_arcsec := tempreal - temp_xmin*60;
  temp_arcmin := temp_xmin mod 60;
  temp_deg := temp_xmin div 60;
  // parallax and parallax error, separated by a space
  buffer2 := Trim(inlist[4]);
  if not ExtractFirstWord(buffer2,buffer1) then begin
    FreeAndNil(inlist);    Exit;
  end;
  if not StrToRealBoth(buffer1,buffer2,temp_parallax,temp_pllxerr) then begin
    FreeAndNil(inlist);    Exit;
  end;
  if not SetParallax(1000*temp_parallax,1000*temp_pllxerr) then begin
    FreeAndNil(inlist);    Exit;
  end;

  // here, I go ahead a commit some of the changes (tired of temps)
  // of course, of data after this point is bad, the old values are gone
  ra_hours := xhrs;
  ra_minutes := temp_min;
  ra_seconds := temp_sec;
  MakeRAString;

  southern := temp_southern;
  dec_degrees := temp_deg;
  dec_arcmins := temp_arcmin;
  dec_arcsecs := temp_arcsec;
  MakeDecString;

  // proper motion: magnitude and angle, separated by a space
  buffer2 := Trim(inlist[5]);
  if not ExtractFirstWord(buffer2,buffer1) then begin
    FreeAndNil(inlist);    Exit;
  end;
  if not StrToRealBoth(buffer1,buffer2,temp_parallax,temp_pllxerr) then begin
    FreeAndNil(inlist);    Exit;
  end;
  if not SetProperMotion(1000*temp_parallax ,temp_pllxerr) then begin
    FreeAndNil(inlist);    Exit;
  end;

  // radial velocity
  buffer1 := inlist[6];
  FreeAndNil(inlist);
  if not StrToReal(buffer1,radialv) then Exit;

  // done
  if not uncertain then uncertain := parallax_err >= 4;
  Result := True;
end;
//---------------------------------------------------------------------
function Location.ConvertAllFromStringOldC(inval:string; parent_pllx,parent_pllxe:Real):Boolean;
var diffpl,diffpe:Real;
begin
  Result := ConvertAllFromStringOld(inval);
  if not Result then Exit;
  diffpl := Abs(parallax-parent_pllx);
  diffpe := Abs(parallax_err-parent_pllxe);
  binarycopy :=  (diffpl<0.001) and (diffpe<0.001);
end;
//==========================================================================
(* takes in magnitude and parallax, outputs absolute magnitude *)
function CalculateAbsMagnitude(zmagnitude:Real; parallax:Real):Real;
var log_parsec:Real;
begin
  Assert(parallax > 0);
  log_parsec := Log10(1000.0 / parallax);
  Result := zmagnitude - 5*(log_parsec-1);
end;
//------------------------------------------------------
function CalculateVisMagnitude(absmag,parallax:Real):Real;
var log_parsec:Real;
begin
  Assert(parallax > 0);
  log_parsec := Log10(1000.0 / parallax);
  Result := absmag + 5*(log_parsec-1);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// used for cutting and pasting parallax entry
function ParsePastedParallax(pentry:string; out osource:string; out xpllx,xpllxe:Real):Boolean;
var enlist:TStringList;
    lastx,bindex,bmax:Integer;
begin
  // basic splitting
  Result := False;
  enlist := SplitWithSpaces(pentry,3);
  if enlist = nil then Exit;
  // getting the parallax
  lastx := enlist.Count-1;
  if not StrToRealBoth(enlist[lastx-1],enlist[lastx],xpllx,xpllxe) then begin
    FreeAndNil(enlist);   Exit;
  end;
  osource := enlist[0];
  if lastx > 2 then begin
    bmax := lastx-2;
    for bindex := 1 to bmax do osource += ' ' + enlist[bindex];
  end;
  FreeAndNil(enlist);
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Used for checking and converting input for making a new location from an
old location, and a string input that is supposed to contain sepration and
position angle. *)
function SeparationCheck(source_loc:Location; in_string:string; out xerr:string; out rho:Real; out theta:Real):Boolean;
var ddeg : Real;
    splitlist:TStringList;
    crho,ctheta:Real;
begin
  // setting initial faliure values...
  rho := 0.0;
  theta := 0.0;
  Result := False;
  // basic location checks
  if source_loc = nil then begin
    xerr := 'Source location is not given! (nil value)';
    Exit;
  end;
  ddeg := source_loc.GetDecimalDeclination();
  if (ddeg < -89.5) or (ddeg > 89.5) then begin
    xerr := 'Declination is too extreme!';
    Exit;
  end;
  // we now try to convert the string input...
  splitlist := SplitWithSpaces(in_string,2);
  if splitlist = nil then begin
     xerr := 'Input must have 2 values!';
     Exit;
  end
  else if splitlist.Count <> 2 then begin
     xerr := 'Input must have 2 values!';
     FreeAndNil(splitlist);
     Exit;
  end;
  if not StrToRealBoth(splitlist[0],splitlist[1],crho,ctheta) then begin
    xerr := 'Could not convert input to numbers!';
    FreeAndNil(splitlist);
    Exit;
  end;
  FreeAndNil(splitlist);
  // checking the numeric input
  if crho < 0 then begin
    xerr := 'Separation must be positive!';
    Exit;
  end;
  if crho >= 120 then begin
    xerr := 'Separation must be less than 120 arcsec.';
    Exit;
  end;
  // finishing okay
  xerr := '';
  rho := crho;
  theta := ctheta;
  Result := True;
end;
//==========================================================================
begin
  sin_galnp_dec_b1950 := Sind(galnp_dec_b1950);
  sin_galnp_dec_j2000 := Sind(galnp_dec_j2000);
  cos_galnp_dec_b1950 := Cosd(galnp_dec_b1950);
  cos_galnp_dec_j2000 := Cosd(galnp_dec_j2000);

  // lfsOut := TFileStream.Create('newloc_debug.txt', fmCreate);

end.

