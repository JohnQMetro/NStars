unit newImports;
(* Additions (started mid Nov 2016):
   - Import function for MEarth Parallaxes


*)
{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, Math, DAMath, DAMTools,
  df_strings, simbad, Utilities2, fluxtransform, Utilities;

type

ImportedData = class;

ParseImportFunction = function(X:string):ImportedData;
NameListToString = function(Y:TStringList):string;

// general parameter type for the imports
ImportParameters = class
  public
    // flags for data to be used
    useRadius:Boolean;
    useMass:Boolean;
    useU_Flux:Boolean;
    useB_Flux:Boolean;
    useVRI_Flux:Boolean;
    useSpT:Boolean;
    useTeff:Boolean;
    useBLum:Boolean;
    useRadv:Boolean;
    // extra write info
    idheader:string;
    idmaker:NameListToString;
    // special
    parser:ParseImportFunction;
    lineskip:Integer;
    useSimbadLocation:Boolean;
    useSimbadProperMotion:Boolean;
    epochdata:Real;
    // source info
    pllx_sourceid:string;
    fullname:string;
    paperurl:string;
    fullout:TFilename;
    leftout:TFilename;

    // startup
    constructor Create;
    function UseFluxesTemp:Boolean;

end;

// Generic imported data class
ImportedData = class
  protected
    function SetRAFromDegrees(const inra:Real):Boolean;
    function SetDecFromDegrees(const indec:Real):Boolean;
    function namec:Integer;
    function fid:string;
  public
    ismatched:Boolean;
    used:Boolean;
    check:Boolean;
    // position for RA is H,M,S, and for Dec deg, arcmin, arcsec (as a string!)
    rapos,decpos:string;
    b1950pos:Boolean;
    // names/ids
    nameids:TStringList;
    // proper motion in magnitude (mas/yr) and position angle
    pm_mag,pm_ang:Real;
    radialv:Real;
    // parallax (in milli-arcseconds)
    pllx,pllx_err:Real;
    // mass and radius (only sometimes used);
    mass,radius,raderr:string;
    // fluxes (only sometimes used)
    Umag:Currency;
    Bmag,BMagE:Currency;
    Vmag,VmagE:Real;
    Rmag,Imag:Currency;
    stype:string;
    teff:Integer;
    blum,blume:string;
    // properties
    property NameCount:Integer read namec;
    property FirstID:string read fid;
    // methods
    constructor Create;
    destructor Destroy; override;
    // setting methods
    function SetDecimalRA(decm_in:string):Boolean;   // set RA from decimal degrees
    function SetDecimalDec(decm_in:string):Boolean;  // set Dec from decimal degrees
    function SetRADec_FromRadians(rad_ras,rad_decs:string):Boolean;
    function SetProperMotionPartsM(racosdpm_in,decpm_in:string):Boolean;
    function SetProperMotionPartsS(racosdpm_in,decpm_in:string):Boolean;
    function SetProperMotionMagSec(inpmmag:string):Boolean;
    function AddNameID(catlabel,catvalue:string):Boolean;
    function SetParallaxFromDistance(dist,err:Real):Boolean;
    // output methods
    function CVSRest(whichdata:ImportParameters; inclused:Boolean):string;

end;


ImportedDataList = class
  protected
    // data
    starlist:array of ImportedData;
    starcount,starindex:Integer;
    // parameter object
    paramdata:ImportParameters;

    // property methods
    function GPStarCount:Integer;
    function CXItem:ImportedData;
    // helper methods
    procedure DeleteData;
  public
    // properties
    property ImportedStarCount:Integer read GPStarCount;
    property CurrentStar:ImportedData read CXItem;
    // methods
    constructor Create(in_paramdata:ImportParameters);
    function LoadFromFile(inname:TFileName):Boolean;
    function WriteToFile(outname:TFileName; unuseddo:Boolean):Integer;
    procedure ResetIndex;
    function NextStar:Boolean;
    destructor Destroy; override;
end;

//=====================================================

(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/ftp-index?/ftp/pub/ftp/ftp/ftp/cats/J/AJ/152/24
table2.dat, converted into a semicolon delimited text file, of:
~ Trigonometric Parallaxes and Proper Motions of 134 Southern Late M, L, and T
Dwarfs from the Carnegie Astrometric Planet Search Program
~ 2016, Weinberger, Boss, Keiser  *)
function ParseCarnegieSouthernLine(linein:string):ImportedData;
function MakeCarnegieConst():ImportParameters;

(* Importing from : https://arxiv.org/abs/1610.07552
~The Solar Neighborhood XXXVIII. Results from the CTIO/SMARTS 0.9m:
Trigonometric Parallaxes for 151 Nearby M Dwarf Systems
~2016, RECONS : Winters, Sevrinsky, Jao, Henry, Riedel, Subasavage, Lurie, Ianna
~ cust and paste from PDF file (custom parsing)  *)
function SplitConvPM(const insource:string; out res1,res2:Real):Boolean;
function ParseSmartsDataLine(linein:string):ImportedData;
function MakeSN38CTIOConst():ImportParameters;

(* Importing from : https://www.cfa.harvard.edu/MEarth/MEarth_Parallaxes.txt
~ MEarth Parallaxes of Nearby M Dwarfs:
Trigonometric Parallaxes for 1,507 Nearby Mid-to-late M-dwarfs
~2016, Dittmann, Irwin, Charbonneau, Berta-Thompson *)
function ParseMEarthLine(linein:string):ImportedData;
function MEarthsCSVNames(inputl:TStringList):string;
function MakeMEarthConst():ImportParameters;

(*importing from : http://www.recons.org/2014.01.aaswin.dieterich.pdf
~ The Solar Neighborhood XXXII. The Hydrogen Burning Limit
~ 2014, RECONS : Dieterich, Henry, Jao, Winters, Hosey, Riedel, Subasavage
~ from VizieR CDS tabel 1 *)
function IbToIc(invmag:Real; inIb:Currency):Currency;
function HydrLimitLine(linein:string):ImportedData;
function MakeSN32HydrLimit():ImportParameters;

function SplitNames(const inname:string; out cid,cnum:string):Boolean;

(* importing from : http://www.as.utexas.edu/~tdupuy/plx/Database_of_Ultracool_Parallaxes.html
~ Database of Ultracool Parallaxes
~ ( Itself compiled from various sources)
~ from vlm-plx-all.txt  *)
function UltracoolDatabaseLine(linein:string):ImportedData;
function MakeUltracoolDatabaseConst():ImportParameters;


//------------------------------------
function ImportedDataToSimbad(incsd:ImportedData; xepoch:Real):SimbadData;

var carnegie_params:ImportParameters;
    sn38_params:ImportParameters;
    mearth_params:ImportParameters;
    sn32_params:ImportParameters;
    ultracool_params:ImportParameters;


//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

implementation
//================================================================
// startup
constructor ImportParameters.Create;
begin
  // flags for data to be used
  useRadius := False;     useMass := False;
  useB_Flux := False;     useVRI_Flux := False;
  useSpT := False;        useTeff := False;
  useBLum := False;       useU_Flux := False;
  useRadv := False;

  // extra info
  idmaker := nil;
  parser := nil;
  lineskip := 0;
  useSimbadLocation := False;
  useSimbadProperMotion := False;
  epochdata := 2000;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
function ImportParameters.UseFluxesTemp:Boolean;
begin
  Result := useB_Flux or useVRI_Flux or useTeff;
end;

//=================================================================
function ImportedData.SetRAFromDegrees(const inra:Real):Boolean;
var rareal,secleft:Real;
    hr,min:Integer;
    zpad:Boolean;
begin
  Result := False;
  // bounds checking
  if inra<0 then Exit;
  if inra >= 360 then Exit;
  // we start conversion...
  rareal := inra/15;
  // extracting hours
  hr := Trunc(rareal);
  zpad := hr<10;
  rapos := IfThen(zpad,'0','') + IntToStr(hr) + ' ';
  // extracting minutes
  rareal := Frac(rareal)*60;
  min := Trunc(rareal);
  zpad := min<10;
  rapos += IfThen(zpad,'0','') + IntToStr(min) + ' ';
  // extracting seconds
  secleft := Frac(rareal)*60;
  zpad := secleft<10;
  rapos += IfThen(zpad,'0','') + Trim(FloatToStrF(secleft,ffFixed,7,4));
  // done
  Result := True;
end;
//-------------------------------
function ImportedData.SetDecFromDegrees(const indec:Real):Boolean;
var decreal,asecleft:Real;
    isneg,zpad:Boolean;
    deg,amin:Integer;
begin
  Result := False;
  // bounds checking
  if indec < -90 then Exit;
  if indec > 90 then Exit;
  // doing the conversion
  isneg := (indec<0);
  decreal := Abs(indec);
  // extracting degrees
  deg := Trunc(decreal);
  zpad := deg<10;
  decpos := IfThen(zpad,'0','') + IntToStr(deg) + ' ';
  // extracting arcminutes
  decreal := Frac(decreal)*60;
  amin := Trunc(decreal);
  zpad := amin<10;
  decpos += IfThen(zpad,'0','') + IntToStr(amin) + ' ';
  // extracting seconds
  asecleft := Frac(decreal)*60;
  zpad := asecleft<10;
  decpos += IfThen(zpad,'0','') + Trim(FloatToStrF(asecleft,ffFixed,7,3));
  // done
  if isneg then decpos := '-' + decpos
  else decpos := '+' + decpos;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function ImportedData.namec:Integer;
begin
  if nameids = nil then Result := 0
  else Result := nameids.Count;
end;
//---------------------------------
function ImportedData.fid:string;
begin
  if nameids = nil then Result := ''
  else if nameids.Count = 0 then Result := ''
  else Result := nameids[0];
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor ImportedData.Create;
const invcurr:Currency = 99.999;
begin
  used := False;        nameids := nil;
  ismatched := False;
  check := False;
  Bmag := invcurr;      BmagE := 0;
  Vmag := 99.999;       VmagE :=0;
  Rmag := invcurr;      Umag := invcurr;
  Imag := invcurr;
  teff := 0;
  pm_mag := 0;
  pm_ang := 0;
  radialv := 9999999;
  b1950pos := False;
end;
//-----------------------------------
destructor ImportedData.Destroy;
begin
  FreeAndNil(nameids);
  inherited;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting methods
//-------------------------------
function ImportedData.SetDecimalRA(decm_in:string):Boolean;
var sc:Integer;
    decreal:Real;
begin
  Result := False;
  // conversion to a number
  decm_in := Trim(decm_in);
  if Length(decm_in) = 0 then Exit;
  Val(decm_in,decreal,sc);
  if (sc<>0) then Exit;
  // using a private method to finish this
  Result := SetRAFromDegrees(decreal);
end;
//-------------------------------
function ImportedData.SetDecimalDec(decm_in:string):Boolean;
var sc:Integer;
    decreal:Real;
begin
  Result := False;
  // conversion to a number
  decm_in := Trim(decm_in);
  if Length(decm_in) = 0 then Exit;
  Val(decm_in,decreal,sc);
  if (sc<>0) then Exit;
  // finishing...
  Result := SetDecFromDegrees(decreal);
end;
//------------------------------
// The 2007 re-reduction of Hipparcos uses Radians instead of degrees!
function ImportedData.SetRADec_FromRadians(rad_ras,rad_decs:string):Boolean;
var ra_rad,dec_rad:Real;
    ra_deg,dec_deg:Real;
begin
  Result := False;
  // getting numeric degrees
  if (not StrToRealBoth(rad_ras,rad_decs,ra_rad,dec_rad)) then Exit;
  ra_deg := RadToDeg(ra_rad);
  dec_deg := RadToDeg(dec_rad);
  if (ra_deg > 360) then Exit;
  if (dec_deg < -90) or (dec_deg > 90) then Exit;
  // hgt
  Result := SetRAFromDegrees(ra_deg);
  if (not Result) then Exit;
  Result := SetDecFromDegrees(dec_deg);
end;
//-------------------------------
function ImportedData.SetProperMotionPartsM(racosdpm_in,decpm_in:string):Boolean;
var sc:Integer;
    racval,decval:Real;
begin
  // converting the inputs to numbers
  Result := False;
  racosdpm_in := Trim(racosdpm_in);
  if Length(racosdpm_in) = 0 then Exit;
  decpm_in := Trim(decpm_in);
  if Length(decpm_in) = 0 then Exit;
  Val(racosdpm_in,racval,sc);
  if (sc<0) then Exit;
  Val(decpm_in,decval,sc);
  if (sc<0) then Exit;
  // using the conversion Function
  ProperMotionConvert(decval,racval,pm_mag,pm_ang);
  Result := True;
end;
//------------------------------
function ImportedData.SetProperMotionPartsS(racosdpm_in,decpm_in:string):Boolean;
var sc:Integer;
    racval,decval:Real;
begin
  // converting the inputs to numbers
  Result := False;
  if not StrToRealBoth(racosdpm_in,decpm_in,racval,decval) then Exit;
  // input is in arcseconds, but our conversion takes mas
  racval := racval*1000;
  decval := decval*1000;
  // using the conversion Function
  ProperMotionConvert(decval,racval,pm_mag,pm_ang);
  Result := True;
end;
//-------------------------------
function ImportedData.SetProperMotionMagSec(inpmmag:string):Boolean;
var tempcurval:Currency;
    sc:Integer;
begin
  Result := False;
  // converting to a number (in arcseconds)
  inpmmag := Trim(inpmmag);
  if Length(inpmmag) = 0 then Exit;
  Val(inpmmag,tempcurval,sc);
  if sc<>0 then Exit;
  // checking the number
  if tempcurval <= 0 then Exit;
  // converting to mas and setting the value
  tempcurval := tempcurval*1000;
  pm_mag := CurrToReal(tempcurval);
  Result := True;
end;
//-------------------------------
function ImportedData.AddNameID(catlabel,catvalue:string):Boolean;
var strin:string;
begin
  // checking the inputs
  Result := False;
  catvalue := Trim(catvalue);
  if Length(catvalue)=0 then Exit;
  strin := Trim(catlabel + ' ' + catvalue);
  // adding the name+id
  if nameids = nil then begin
    nameids := TStringList.Create;
    nameids.StrictDelimiter := True;
  end;
  nameids.Add(strin);
  Result := True;
end;
//-------------------------------
function ImportedData.SetParallaxFromDistance(dist,err:Real):Boolean;
var medpllx,maxpllx,minpllx:Real;
begin
  Result := False;
  if dist <= 0 then Exit;
  if err < 0 then Exit;
  // calculating the parallaxes
  medpllx := 1000/dist;
  minpllx := 1000/(dist+err);
  maxpllx := 1000/(dist-err);
  // the final values
  Result := True;
  pllx := medpllx;
  pllx_err := Max(maxpllx-medpllx,medpllx-minpllx);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++
// output methods
function ImportedData.CVSRest(whichdata:ImportParameters; inclused:Boolean):string;
begin
  Assert(whichdata<>nil);
  // position, parallax, and proper motion
  Result := rapos + ',' + decpos + ',';
  Result += Trim(FloatToStrF(pllx,ffFixed,7,3)) + ',';
  Result += Trim(FloatToStrF(pllx_err,ffFixed,7,3)) + ',';
  Result += Trim(FloatToStrF(pm_mag,ffFixed,7,3)) + ',';
  Result += Trim(FloatToStrF(pm_ang,ffFixed,7,2));
  // radial velocity
  if whichdata.useRadv then Result += ',' + Trim(FloatToStrF(radialv,ffFixed,7,2));
  // spectral type
  if whichdata.useSpT then Result += ',' + stype;
  // U magnitude
  if whichdata.useU_Flux then begin
    if Umag < 90 then Result += ',' + CurrToStrF(Umag,ffFixed,3)
    else Result += ',';
  end;
  // B magnitude
  if whichdata.useB_Flux then begin
    if Bmag < 90 then begin
      Result += ',' + CurrToStrF(Bmag,ffFixed,3);
      Result += ',' + CurrToStrF(BMagE,ffFixed,3);
    end
    else Result += ',,';
  end;
  // VRI magnitudes
  if whichdata.useVRI_Flux then begin
    if Vmag < 90 then begin
      Result += ',' + FloatToStrF(Vmag,ffFixed,7,3);
      Result += ',' + FloatToStrF(VMagE,ffFixed,7,3);
    end
    else Result += ',,';
    Result += ',';
    if Rmag < 90 then Result += CurrToStrF(Rmag,ffFixed,3);
    Result += ',';
    if Imag < 90 then Result += CurrToStrF(Imag,ffFixed,3);
  end;
  // TEff
  if whichdata.useTeff then Result += ',' + IntToStr(teff);
  // Bolometric Luminosity
  if whichdata.useBLum then begin
    Result += ',' + blum + ',' + blume;
  end;
  // mass
  if whichdata.useMass then Result += ',' + mass;
  if whichdata.useRadius then begin
    Result += ',' + radius + ',' + raderr;
  end;
  // matched (if used)
  if not inclused then begin
    Result += ',' + Bool2Str(used) + ',' + Bool2Str(ismatched);
    Result += ',' + Bool2Str(check);
  end;
end;
//==================================================================
// property methods
//--------------------
function ImportedDataList.GPStarCount:Integer;
begin    Result:= starcount;    end;
//----------------------------------
function ImportedDataList.CXItem:ImportedData;
begin
  if (starindex = -1) or (starindex = starcount) then Result := nil
  else Result := starlist[starindex];
end;
//++++++++++++++++++++++++++++++++++++++++++++++++
// helper methods
procedure ImportedDataList.DeleteData;
var stardex:Integer;
begin
  if starcount = 0 then Exit;
  for stardex := 0 to starcount -1 do begin
    starlist[stardex].Free;
    starlist[stardex] := nil;
  end;
  SetLength(starlist,0);
  starcount := 0;
  starindex := -1;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// public methods
//----------------------------
constructor ImportedDataList.Create(in_paramdata:ImportParameters);
begin
  Assert(in_paramdata<>nil);
  Assert(Assigned(in_paramdata.parser));
  paramdata := in_paramdata;
  starindex := -1;
  starcount := 0;
end;
//----------------------------
function ImportedDataList.LoadFromFile(inname:TFileName):Boolean;
var infile:TextFile;
    curline:string;
    curdata:ImportedData;
    skipcounter:Integer;
begin
  Result := False;
  // if it exists, clearing old data
  if (starcount > 0) then DeleteData;
  // opening the file
  AssignFile(infile,inname);
  FileMode := fmOpenRead;
  Reset(infile);
  // skipping lines
  if paramdata.lineskip > 0 then begin
    for skipcounter := 1 to paramdata.lineskip do ReadLn(infile,curline);
  end;
  // loop reading and processing
  while (not Eof(infile)) do begin
    ReadLn(infile,curline);
    curdata := paramdata.parser(curline);
    if curdata<>nil then begin
      Inc(starcount);
      SetLength(starlist,starcount);
      starlist[starcount-1] := curdata;
    end;
  end;
  // in the end, a success if we've got at least one star
  Result := (starcount > 0);
end;
//---------------------------
function ImportedDataList.WriteToFile(outname:TFileName; unuseddo:Boolean):Integer;
var outfile:TextFile;
    curline:string;
    recmax,recdex:Integer;
    written:Integer;
const coreheader = 'Right Ascension (HMS),Declination (dms),Parallax (mas),Pllx.Err.(mas),PM Magn.(mas/yr),PM Angle (deg)';
begin
  Result := 0;
  // preparing the new file
  AssignFile(outfile,outname);
  Rewrite(outfile);
  // writing the header
  curline := paramdata.idheader +',' + coreheader;
  if paramdata.useRadv then curline += ',Radial V';
  if paramdata.useSpT then curline += ',SpT';
  if paramdata.useU_Flux then curline += ',U Mag';
  if paramdata.useB_Flux then curline += ',B Mag,Err B';
  if paramdata.useVRI_Flux then curline += ',VMag,Err V,R Mag,I Mag';
  if paramdata.useTeff then curline += ',TEff';
  if paramdata.useBLum then curline += ',Bo Lum,Bo Lum E';
  if paramdata.useMass then curline += ',Mass';
  if paramdata.useRadius then curline += ',Radius,Radius Err';
  if unuseddo then curline += ',Used,Matched,Check';
  WriteLn(outfile,curline);
  // initial value setup
  recmax := starcount -1;
  written := 0;
  // the detail writing loop
  for recdex := 0 to recmax do begin
    if (starlist[recdex].used) and unuseddo then Continue;
    // the name/ids
    if Assigned(paramdata.idmaker) then curline := paramdata.idmaker(starlist[recdex].nameids)
    else curline := starlist[recdex].FirstID;
    curline += ',';
    // the rest
    curline += starlist[recdex].CVSRest(paramdata,not unuseddo);
    WriteLn(outfile,curline);
    Inc(written);
  end;
  // done
  Flush(outfile);
  Close(outfile);
  Result := written;
end;
//----------------------------
procedure ImportedDataList.ResetIndex;
begin  starindex:= -1;    end;
//----------------------------
function ImportedDataList.NextStar:Boolean;
begin
  Result := False;
  if starindex = starcount then Exit
  else if starindex = (starcount-1) then begin
    starindex := starcount;
    Exit;
  end
  else begin
    Inc(starindex);
    Result := True;
  end;
end;
//----------------------------------
destructor ImportedDataList.Destroy;
begin
  DeleteData;
  inherited;
end;
//==============================================================
function ParseCarnegieSouthernLine(linein:string):ImportedData;
var parsedl:TStringList;
  sc:Integer;
  impres:ImportedData;
begin
  Result := nil;
  parsedl := SplitWithDelim(linein,';',16);
  if parsedl = nil then Exit;
  // we have the data, we need to put it into the right member variables
  impres := ImportedData.Create;
  impres.AddNameID('',parsedl[0]);
  impres.rapos := Trim(parsedl[1]);
  if Length(impres.rapos) < 8 then begin
    impres.Free;
    Exit;
  end;
  impres.decpos := Trim(parsedl[2]);
  if Length(impres.decpos) < 7 then begin
    impres.Free;
    Exit;
  end;
  // proper motion
  if not impres.SetProperMotionPartsM(parsedl[10],parsedl[12]) then begin
    impres.Free;
    Exit;
  end;
  // parallax
  Val(Trim(parsedl[14]),impres.pllx,sc);
  if sc <> 0 then begin
    impres.Free;
    Exit;
  end;
  Val(Trim(parsedl[15]),impres.pllx_err,sc);
  if sc <> 0 then FreeAndNil(impres);
  // okay!
  // impres.source := 'Carnegie Astrom. 2016';
  Result := impres;
end;
//-----------------------------------------------------------
function MakeCarnegieConst():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'ID/Name';
  Result.parser := ParseCarnegieSouthernLine;
  Result.lineskip := 1;
  // source info
  Result.pllx_sourceid := 'Carnegie 2016';
  Result.fullname := 'Trigonometric Parallaxes and Proper Motions of 134 Southern Late M, L, and T Dwarfs from the Carnegie Astrometric Planet Search Program';
  Result.paperurl := 'https://arxiv.org/abs/1604.05611';
  Result.fullout := 'carnegie_southern.csv';
  Result.leftout := 'carnegie_leftover.csv';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function SplitConvPM(const insource:string; out res1,res2:Real):Boolean;
var str1,str2:string;
    slen,spos,sc:Integer;
begin
  // initial
  Result:= False;
  slen := Length(insource);
  if slen<3 then Exit;
  // splitting
  spos := AnsiPos('±',insource);
  if spos < 2 then Exit;
  str1 := Trim(AnsiLeftStr(insource,spos-1));
  // position is measured in UTF-8 bytes, not characters
  str2 := Trim(AnsiRightStr(insource,slen-spos-1));
  if (Length(str2)=0) then Exit;
  // converting the split sections
  Val(str1,res1,sc);
  if (sc<>0) then Exit;
  Val(str2,res2,sc);
  Result := (sc=0);
end;
//---------------------------------------------------------------
function ParseSmartsDataLine(linein:string):ImportedData;
var linesplit:TStringList;
    cbuf:string;
    cindex,sc,xindex:Integer;
    tempreal1,tempreal2:Real;
    builtstr:string;
    impres:ImportedData;
begin
  // intial prep...
  Result := nil;
  linesplit := SplitWithDelim(Trim(linein),' ',20);
  if linesplit = nil then Exit;
  // moving backwards...
  cindex := linesplit.Count-1;
  cbuf := Trim(linesplit[cindex]);
  if cbuf = '!' then begin
    Dec(cindex);
    cbuf := Trim(linesplit[cindex]);
  end;
  // the last item is the tangetial velocity (do not use, but convert to check)
  Val(cbuf,tempreal1,sc);
  if sc<>0 then begin
    linesplit.Free;
    Exit;
  end;
  // previous, proper motion position angle
  Dec(cindex);
  cbuf := Trim(linesplit[cindex]);
  if not SplitConvPM(cbuf,tempreal1,tempreal2) then begin
    linesplit.Free;
    Exit;
  end;
  impres := ImportedData.Create;
  impres.pm_ang:= tempreal1;
  // previous, proper motion magnitude
  Dec(cindex);
  cbuf := Trim(linesplit[cindex]);
  if not SplitConvPM(cbuf,tempreal1,tempreal2) then begin
    linesplit.Free;
    impres.Free;
    Exit;
  end;
  impres.pm_mag := tempreal1;
  // previous, we skip over any 'b' note
  Dec(cindex);
  cbuf := Trim(linesplit[cindex]);
  if cbuf = 'b' then begin
    Dec(cindex);
    cbuf := Trim(linesplit[cindex]);
  end;
  // we should be at the 'absolute parallax' now
  if not SplitConvPM(cbuf,tempreal1,tempreal2) then begin
    linesplit.Free;
    impres.Free;
    Exit;
  end;
  impres.pllx := tempreal1;
  impres.pllx_err := tempreal2;
  // previous, we skip over any 'a' note
  Dec(cindex);
  cbuf := Trim(linesplit[cindex]);
  if cbuf = 'a' then Dec(cindex);
  // skipping over parallax correction and relative parallax
  Dec(cindex);
  Dec(cindex);
  (* the previous field is mmag variation, which is sometimes just a bunch
  of dots (separated by spaces) instead. In this case, one dot will be attached
  to the end of the previous field, and 2 will be separate. Since we are skippin
  the mmag variation, that just means one extra field to skip*)
  cbuf := Trim(linesplit[cindex]);
  if cbuf = '·' then Dec(cindex);
  Dec(cindex);
  // skipping over Nref (possibly with extra dot)
  Dec(cindex);
  // skipping over T, coverage, Nfr, Nsea, and fil
  cindex -= 5;
  // declination is in dd:mm:ss format, so we build it up
  builtstr := Trim(linesplit[cindex]);
  Dec(cindex);
  builtstr := Trim(linesplit[cindex]) + ' ' + builtstr;
  Dec(cindex);
  builtstr := Trim(linesplit[cindex]) + ' ' + builtstr;
  (* for negative (any only negative) declinations, the - is attached
  to the end of the previous field. *)
  Dec(cindex);
  cbuf := Trim(linesplit[cindex]);
  if AnsiEndsStr('-',cbuf) then begin
    builtstr := '-' + builtstr;
    cbuf := AnsiLeftStr(cbuf,Length(cbuf)-1);
  end;
  impres.decpos := builtstr;
  // right ascension
  Dec(cindex);
  builtstr := Trim(linesplit[cindex]) + ' ' + cbuf;
  Dec(cindex);
  impres.rapos := Trim(linesplit[cindex]) + ' ' + builtstr;
  // creating the id
  builtstr := '';
  for xindex := 0 to (cindex-1) do begin
    builtstr += Trim(linesplit[xindex]) + ' ';
  end;
  builtstr := AnsiReplaceStr(builtstr,'SCR ','SCR J');
  if AnsiEndsStr('AB',builtstr) then builtstr := Copy(builtstr,1,Length(builtstr)-2);
  impres.AddNameID('',builtstr);
  // done
  // impres.source := 'SN38: CTIO 2016';
  Result := impres;
end;
//------------------------------------------------
function MakeSN38CTIOConst():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'ID/Name';
  Result.parser := ParseSmartsDataLine;
  Result.lineskip := 0;
  // source info
  Result.pllx_sourceid := 'SN38: CTIO 2016';
  Result.fullname := 'The Solar Neighborhood XXXVIII. Results from the CTIO/SMARTS 0.9m: Trigonometric Parallaxes for 151 Nearby M Dwarf Systems';
  Result.paperurl := 'https://arxiv.org/abs/1610.07552';
  Result.fullout := 'ctio_smarts.csv';
  Result.leftout := 'ctioleftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// parsing a parallax line fromMEarth
function ParseMEarthLine(linein:string):ImportedData;
var impres:ImportedData;
    cbuf1,cbuf2:string;
    sc:Integer;
begin
  Result := nil;
  if Length(linein) < 164 then Exit;
  // linelength is fixed

  // unlike previous data formats, MEarths has 4 id fields
  impres := ImportedData.Create;
  // LSPM
  cbuf1 := Copy(linein,1,11);
  impres.AddNameID('LSPM',cbuf1);
  // Gliese
  cbuf1 := Copy(linein,13,2); // might be Gl, GJ, or empty
  cbuf2 := Copy(linein,15,6);
  impres.AddNameID(cbuf1,cbuf2);
  // LHS
  cbuf1 := Copy(linein,22,5);
  impres.AddNameID('LHS',cbuf1);
  // NLTT
  cbuf1 := Copy(linein,28,5);
  impres.AddNameID('NLTT',cbuf1);

  // right ascension
  cbuf1 := Copy(linein,34,11);
  // making sure right ascension is properly zero padded
  if cbuf1[1] = ' ' then cbuf1[1] := '0';
  if cbuf1[4] = ' ' then cbuf1[4] := '0';
  if cbuf1[7] = ' ' then cbuf1[7] := '0';
  impres.rapos := cbuf1;

  // declination
  cbuf1 := Copy(linein,46,11);
  // making sure declination is properly zero padded
  if cbuf1[1] = ' ' then cbuf1[1] := '0';
  if cbuf1[4] = ' ' then cbuf1[4] := '0';
  if cbuf1[7] = ' ' then cbuf1[7] := '0';
  // MEarth stars are, it seems, 100% northern hemisphere
  impres.decpos:= '+' + cbuf1;

  // MEarth proper motion is in arsec components, not mas
  cbuf1 := Copy(linein,58,7);
  cbuf2 := Copy(linein,66,7);
  if (not impres.SetProperMotionPartsS(cbuf1,cbuf2)) then begin
    impres.Free;
    Exit;
  end;

  // unlike proper motion, parallax and parallax error is given in mas
  cbuf1 := Trim(Copy(linein,106,6));
  Val(cbuf1,impres.pllx,sc);
  if (sc<>0) then begin
    impres.Free;
    Exit;
  end;
  if (impres.pllx <= 0) then begin   // this can actually happen
    impres.Free;
    Exit;
  end;
  cbuf2 := Trim(Copy(linein,113,6));
  Val(cbuf2,impres.pllx_err,sc);
  if (sc<>0) then begin
    impres.Free;
    Exit;
  end;
  if (impres.pllx_err < 0) then begin   // this should never happen
    impres.Free;
    Exit;
  end;

  // MEarths includes mass and radius estimates
  cbuf1 := Copy(linein,133,5);
  impres.mass := ' ' + cbuf1; // to match the StarExtraData mass format
  cbuf1 := Copy(linein,139,5);
  impres.radius := ' 0' + cbuf1; // to match the StarExtraData radius format
  cbuf2 := Copy(linein,145,5);
  impres.raderr := ' 0' + cbuf2; // to match the StarExtraData radius format

  //done!
  // impres.source:= 'MEarth';
  Result := impres;
end;
//-------------------------------------------------
function MEarthsCSVNames(inputl:TStringList):string;
var curdex,maxcount:Integer;
    cxid:string;
begin
  // no results here
  Result := ',,,';    // default empty result
  if inputl = nil then Exit;
  maxcount := inputl.Count;
  if maxcount = 0 then Exit;
  // first result field (mandatory)
  curdex := 0;
  cxid := inputl[curdex];
  Assert(AnsiStartsStr('LSPM',cxid));
  Result := cxid + ',';
  // after first result
  Inc(curdex);
  if curdex = maxcount then begin
    Result += ',,';
    Exit;
  end;
  // second field is Gl/GJ
  cxid := inputl[curdex];
  if AnsiStartsStr('Gl',cxid) or AnsiStartsStr('GJ',cxid) then begin
    Result += cxid + ',';
    Inc(curdex);
  end
  else Result += ',';
  // after second result
  if curdex = maxcount then begin
    Result += ',';
    Exit;
  end;
  // third field is LHS
  cxid := inputl[curdex];
  if AnsiStartsStr('LHS',cxid) then begin
    Result += cxid + ',';
    Inc(curdex);
  end
  else Result += ',';
  // fourth field is NLTT
  if curdex<>maxcount then begin
    cxid := inputl[curdex];
    if AnsiStartsStr('NLTT',cxid) then Result += cxid;
  end;
  // done
end;
//--------------------------------------------
function MakeMEarthConst():ImportParameters;
begin
  Result := ImportParameters.Create;
  // flags for data to be used
  Result.useRadius := True;
  Result.useMass := True;
  // extra write info
  Result.idheader := 'ID??';
  Result.idmaker := MEarthsCSVNames;
  // special
  Result.parser := ParseMEarthLine;
  Result.lineskip := 0;
  // source info
  Result.pllx_sourceid := 'MEarth';
  Result.fullname := 'MEarth Trigonometric Parallaxes for 1507 Nearby M Dwarfs';
  Result.paperurl := 'https://www.cfa.harvard.edu/MEarth/MEarth_Parallaxes.txt';
  Result.fullout := 'mearth_parallax.csv';
  Result.leftout := 'mearth_leftover.csv'
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* the paper converts Ic to Is using the equation :
(V − Ib) = −0.0364(V − Ic)2 + 1.4722(V − Ic) − 1.3563.
I want to reverse that and convert Ib to Ic   *)
function IbToIc(invmag:Real; inIb:Currency):Currency;
var c:Real;       rescount:Integer;
    resx1,resy1,resx2,resy2:Real;
    xres1:Real;
const a = -0.0364;
      b = 1.4722;
begin
  c := -1.3563 - (invmag - CurrToReal(inIb));
  rescount := squad(a,b,c,resx1,resy1,resx2,resy2);
  Assert(rescount>0);
  xres1 := invmag - resx1;
  Result := RealToCurr(xres1);
end;
//----------------------------------------------------
function HydrLimitLine(linein:string):ImportedData;
var impres:ImportedData;
    cbuf1,cbuf2:string;
    tempv,tempve,tempmin,tempmax:Real;
    tempflux:Currency;
begin
  Result := nil;
  if Length(linein) < 270 then Exit;
  impres := ImportedData.Create;
  // position
  // right ascension
  cbuf1 := Copy(linein,5,10);
  impres.rapos := Trim(cbuf1);
  // declination
  cbuf1 := Copy(linein,16,9);
  impres.decpos:= Trim(cbuf1);
  // the single name comes from a variety of sources, and is too abbreviated to be useful
  impres.AddNameID('',Trim(Copy(linein,26,14)));
  // spectral type
  impres.stype := Trim(Copy(linein,41,5));
  // proper motion
  cbuf2 := Copy(linein,50,5);
  if (not impres.SetProperMotionMagSec(cbuf2)) then begin
    FreeAndNil(impres);    Exit;
  end;
  cbuf2 := Trim(Copy(linein,56,5));
  if (not StrToReal(cbuf2,impres.pm_ang)) then begin
    FreeAndNil(impres);    Exit;
  end;
  // parallax
  cbuf1 := Trim(Copy(linein,62,6));
  cbuf2 := Trim(Copy(linein,69,4));
  if (not StrToRealBoth(cbuf1,cbuf2,impres.pllx,impres.pllx_err)) then begin
    FreeAndNil(impres);    Exit;
  end;

  // fluxes. 6 sets are provided
  // V from CTIO
  StrToReal(Copy(linein,89,5),impres.Vmag);
  StrToReal(Copy(linein,95,4),impres.VmagE);
  // Rc from CTIO
  SubstrCurr(linein,100,5,impres.Rmag);
  // Ic from CTIO
  SubstrCurr(linein,111,5,impres.Imag);
  // V from SOAR
  tempv := 99.9;   tempve := 0;
  StrToReal(Copy(linein,122,5),tempv);
  StrToReal(Copy(linein,128,4),tempve);
  //if we have both CTIO and SOAR, we average
  if (impres.Vmag < 99) and (tempv < 99) then begin
    tempmin := Min(impres.Vmag - impres.VmagE,tempv-tempve);
    tempmax := Max(impres.Vmag + impres.VmagE,tempv+tempve);
    impres.Vmag:= (tempmin+tempmax)/2;
    impres.VmagE :=Max(Abs(tempmax-impres.Vmag),Abs(impres.Vmag-tempmin));
  end
  else if (tempv < 99) then begin
    impres.Vmag := tempv;
    impres.VmagE := tempve;
  end;
  // R from SOAR
  if SubstrCurr(linein,133,5,tempflux) then begin
    // if we have both, we average
    if impres.Rmag < 99 then begin
      tempflux += impres.Rmag;
      impres.Rmag := CurrHalfVal(tempflux,False);
    end
    else impres.Rmag := tempflux;
  end;
  // Is from SOAR is not the same as Ic...
  if SubstrCurr(linein,144,5,tempflux) then begin
    if impres.Vmag < 99 then tempv := impres.Vmag
    else tempv := 20.95;  // there is only one such case... so I guess V
    tempflux := IbToIc(tempv,tempflux);
    // if we have both, we average
    if impres.Imag < 99 then begin
      tempflux += impres.Imag;
      impres.Imag := CurrHalfVal(tempflux,False);
    end
    else impres.Imag := tempflux;
  end;

  // effective temperature
  cbuf1 := Trim(Copy(linein,237,4));
  if cbuf1 <> '' then impres.teff := StrToInt(cbuf1);

  // bolometric luminosity
  cbuf1 := Copy(linein,246,6);
  cbuf2 := Copy(linein,253,5);
  if StrToRealBoth(cbuf1,cbuf2,tempv,tempve) then begin
    tempmin := exp10(tempv-tempve);
    tempmax := exp10(tempv+tempve);
    MinMaxToMedRangeR(tempmin,tempmax,3,tempv,tempve);
    impres.blum := FloatToStrF(tempv,ffGeneral,3,0);
    impres.blume := FloatToStrF(tempve,ffGeneral,3,0);
  end;
  // estimated radius
  cbuf1 := Trim(Copy(linein,259,5));
  if cbuf1<>'' then begin
    if SubstrCurr(linein,265,6,tempflux) then begin
      impres.radius := ' 0' + cbuf1; // to match the StarExtraData radius format
      // the error is 4 decimal digits in the source, so we round up...
      tempflux := Ceil3rdCurrency(tempflux);
      impres.raderr:= '0' + CurrToStrF(tempflux,ffFixed,3);
    end;
  end;

  //done
  // impres.source := 'SN 32: Hydr.Bur.Limit';
  Result := impres;
end;
//-------------------------------------------
function MakeSN32HydrLimit():ImportParameters;
begin
  Result := ImportParameters.Create;
  // flags for data to be used
  Result.useRadius := True;
  Result.useVRI_Flux := True;
  Result.useSpT := True;
  Result.useTeff := True;
  Result.useBLum := True;
  // extra write info
  Result.idheader := 'Abbrev ID.';
  Result.idmaker := MEarthsCSVNames;
  // special
  Result.parser := ParseMEarthLine;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  // source info
  Result.pllx_sourceid := 'SN32: HB Lim';
  Result.fullname := 'The Solar Neighborhood XXXII. The Hydrogen Burning Limit';
  Result.paperurl := 'http://iopscience.iop.org/article/10.1088/0004-6256/147/5/94/pdf';
  Result.fullout := 'hydrogen_burning.csv';
  Result.leftout := 'hblimit_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function SplitNames(const inname:string; out cid,cnum:string):Boolean;
var rpos,innl,sc:Integer;
    glnum:Real;
begin
  Result := True;
  innl := Length(inname);
  if AnsiStartsStr('GJ',inname) then begin
    cnum := Trim(Copy(inname,3,innl-2));
    cid := 'GJ';
    Val(cnum,glnum,sc);
    if sc = 0 then begin
      if glnum < 1000 then cid := 'Gl'
      else if glnum > 9000 then cid := 'Wo';
    end;
  end
  // non GJ
  else begin
    rpos := PosSetEx('0123456789J ',inname,2);
    if rpos >= 2 then begin
      cid := Trim(Copy(inname,1,rpos-1));
      cnum := Trim(Copy(inname,rpos,innl-rpos+1));
    end
    else Result := False;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* importing from : http://www.as.utexas.edu/~tdupuy/plx/Database_of_Ultracool_Parallaxes.html
~ Database of Ultracool Parallaxes
~ ( Itself compiled from various sources)
~ from vlm-plx-all.txt  *)
function UltracoolDatabaseLine(linein:string):ImportedData;
var buffer,part1,part2,part3,part4:string;
    impres:ImportedData;
    gf,rf,ifx,zf:Real;
    bdiscard:Currency;
begin
  Result := nil;
  if Length(linein) < 601 then Exit;

  // the name...
  buffer := Trim(Copy(linein,1,36));
  if Length(buffer)<2 then Exit;
  // the name is *not* simbad-able directly, and must be fiddled with
  buffer := AnsiReplaceStr(buffer,'NAME_','');
  buffer := Trim(AnsiReplaceStr(buffer,'_',' '));
  // trying to split the catalog prefix from the id
  if SplitNames(buffer,part1,part2) then buffer := part1 + ' ' + part2;
  // note that the letter on the end does not need to be separated
  // Simbad will recognize it, but for certain catalogs only

  // spectral types (2 of them)
  part1 := Trim(Copy(linein,37,7));
  part2 := Trim(Copy(linein,45,10));
  part2 := AnsiReplaceStr(part2,'$\geq$','');

  // starting to build the result
  impres := ImportedData.Create;
  impres.AddNameID('',buffer);

  // picking the spectral type
  if (part1 = 'null') and (part2 = 'null') then buffer := ''
  else if part1 = 'null' then buffer := part2
  else buffer := part1;
  impres.stype := buffer;

  // position
  part1 := Trim(Copy(linein,107,10)); // RA in degrees
  part2 := Trim(Copy(linein,118,10)); // Dec in degrees
  // these positions are apparently *not* epoch standardized!
  if not impres.SetDecimalRA(part1) then begin
    FreeAndNil(impres);    Exit;
  end;
  if not impres.SetDecimalDec(part2) then begin
    FreeAndNil(impres);    Exit;
  end;

  // parallax
  part1 := Copy(linein,141,8);
  part2 := Copy(linein,149,7);
  if not StrToRealBoth(part1,part2,impres.pllx,impres.pllx_err) then begin
    FreeAndNil(impres);    Exit;
  end;

  // proper motion
  part1 := Copy(linein,156,9);
  part2 := Copy(linein,173,7);
  StrToRealBoth(part1,part2,impres.pm_mag,impres.pm_ang);  // not essential

  (* lots of fluxes, however, none of them are UVBRI. Theoretically, the griz
  fluxes could be used to get V Rc Ic, but I could not confirm that any of
  the many transformations available are valid for Brown Dwarfs.*)
  // I will, however, try
  part1 := Copy(linein,208,6);
  part2 := Copy(linein,223,6);
  part3 := Copy(linein,238,6);
  part4 := Copy(linein,253,6);
  if StrToRealBoth(part1,part2,gf,rf) and StrToRealBoth(part3,part4,ifx,zf) then begin
    SDSS_ugriz2BVRI_c(99,gf,rf,ifx,zf,bdiscard,impres.Vmag,impres.Rmag,impres.Imag);
  end;
  // impres.source:= 'Ultracool DB';
  Result := impres;
end;
//---------------------------------------------------
function MakeUltracoolDatabaseConst():ImportParameters;
begin
  Result := ImportParameters.Create;
  // flags for data to be used
  Result.useSpT := True;
  Result.useVRI_Flux := True;
  // extra write info
  Result.idheader := 'Name/ID';
  // special
  Result.parser := UltracoolDatabaseLine;
  Result.lineskip := 2;
  Result.useSimbadLocation := True;
  // source info
  Result.pllx_sourceid := 'Ultracool DB';
  Result.fullname := 'Database of Ultracool Parallaxes';
  Result.paperurl := 'http://www.as.utexas.edu/~tdupuy/plx/Database_of_Ultracool_Parallaxes.html';
  Result.fullout := 'ultracool_database.csv';
  Result.leftout := 'ultracool_leftover.csv';
end;
//==============================================================
// useful functions
// takes imported data and tries to get a SimbadData
function ImportedDataToSimbad(incsd:ImportedData; xepoch:Real):SimbadData;
var curid,lookup_url:string;
    idcount,iddex:Integer;
    discardfail:Boolean;
    qEpoch:EpochType;
begin
  Result := nil;
  Assert(incsd<>nil);
  // starting the lookup...
  idcount := incsd.NameCount;
  if idcount<>0 then begin
    for iddex := 0 to (idcount-1) do begin
      curid := incsd.nameids[iddex];
      lookup_url := MakeSimbadIdLookupURL(curid);
      Result := GetSimbadDataURL(lookup_url,discardfail);
      if Result <> nil then Exit;
    end;
  end;
  // here, we try a location based lookup
  if (xepoch = 2000) then qEpoch := eJ2000
  else if (incsd.b1950pos) then qEpoch := eB1950
  else if (xepoch = 1975) then qEpoch := eB1975
  else if (xepoch = 1991.25) then qEpoch := zJ1991q
  else if (xepoch = 2014) then qEpoch := zJ2014
  else if (xepoch = 2015) then qEpoch := zJ2015
  else if (xepoch = 2015.5) then qEpoch := zJ2015h
  else if (xepoch = 2017) then qEpoch := zJ2017
  else qEpoch := eJ2000;
  lookup_url := MakeSimbadCoordLookupURL(incsd.rapos,incsd.decpos,1,qEpoch);
  Result := GetSimbadDataURL(lookup_url,discardfail);
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

begin

  carnegie_params := MakeCarnegieConst();
  sn38_params := MakeSN38CTIOConst();
  mearth_params := MakeMEarthConst();
  sn32_params := MakeSN32HydrLimit();
  ultracool_params := MakeUltracoolDatabaseConst();
end.

