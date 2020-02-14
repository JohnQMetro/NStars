unit WhiteDwarf;

{$mode delphi}
(* consolidates the White Dwarf specific classes and functions for estimation *)

interface

uses
  Classes, SysUtils, DAMath, EstBasics, df_strings,utilities, fluxtransform;
//********************************************************************
const
(* used for white dwarf mass estimation *)
  wdmin = 0.005;
  wdmax = 1.44;
  wdstep = 0.01;
  zhami_min:Currency = 0.150;
  zhami_step:Currency = 0.001;
  zhami_max:Currency = 1.428;

type
(* 2 classes used to hold data taken or calculated from the DA and DB
tables from 'Synthetic Colors and Evolutionary Sequences of Hydrogen-and-
Helium-Atmosphere White Dwarfs' at http://www.astro.umontreal.ca/~bergeron/CoolingModels/
Holberg & Bergeron (2006, AJ, 132, 1221), Kowalski & Saumon (2006, ApJ, 651,
L137), Tremblay et al. (2011, ApJ, 730, 128), Bergeron et al. (2011, ApJ,
737, 28), and Blouin et al. (2018, ApJ, 863, 184).
- This data is used to estimate BCv and Thermal Bloat from TEff and log g. *)
WDBoloEntry = class
  protected
    radius,zradius:Real;
    procedure CalculateRadius();

  public
    TEff:Integer;
    logTEff:Real;
    logg:Currency;
    mass,avm,abm,agm:Real;
    bcor:Real;
    bcdif,bcslope:Real;
    gcor:Real;
    gcdif,gcslope:Real;

    bloat,bloatslope:Real;

    constructor Create;
    function SetFromCSV(instr:string):Boolean;
    function MakeMassRadisCSV():string;
end;

WDBoloData = class
  private
    // list of temperatures, and calculated blackbody BCv's (for each temp)
    DA_temparray:array of Integer;
    DA_bb_bcvarray:array of Real;
    DB_temparray:array of Integer;
    DB_bb_bcvarray:array of Real;
    // loaded BCv data for Hydrogen Atmosphere (DA) White Dwarfs
    DA_Data:array of array of WDBoloEntry;
    // loaded BCv data for Helium Atmosphere (non DA) White Dwarfs
    DB_Data:array of array of WDBoloEntry;
    // loading and setting data methods
    function LoadAllData(sourcef:TFileName):Boolean;
    function SetupBlackbody:Boolean;
    function SetupDifferencesAndSlopes:Boolean;
    function SetupBloats:Boolean;
    function VerifyChecks:Boolean;
    // locating where to find data to calculate the result
    function FindTEffIndex(const hydr:Boolean; const findTEff:Real):Integer;
    function FindLogG(const hydr:Boolean; const findLogG:Currency; out blend:Boolean):Integer;
    // calculating BCv and bloat with the locations...
    function CalculateBCv(TEff:Real; hydr,blend:Boolean; loggdex,teffdex:Integer):Real;
    function CalculateBloat(TEff:Real; hydr,blend:Boolean; loggdex,teffdex:Integer):Real;
    // special test output
    function MakeRadiusLine(const isdb:Boolean; tempdex:Integer):string;
    function MakeRadiusHeader():string;
  public
    constructor Create;
    Destructor Destroy; override;
    // The external use methods
    function InitializeFromFile(datafn:TFileName):Boolean;
    function GetBCvBloat(const hydr:Boolean; const logg_in:Currency; const TEff_in:Real; out BCv_out,Bloat_out:Real):Boolean;
    // test output
    procedure MakeRadiusInfoFile();
end;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
var
  wdbc:WDBoloData;
  wdbcloaded:Boolean;
  wd_radii:array of Real;
  zhami_mass_radius:array of Real;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// the current core 'Zero-Temp' WD mass-radius method
function WhiteDwarfRadiusFromMass(const inmass:Real):Real;
// alternative/additional WD mass-radius methods
function WhiteDwarfRadiusToMassP(const inRad:Real; const inpkm:Boolean):Real;
function NauenbergWhiteDwarfRadius(const mass,mmwpe:Real; out resx:Real):Boolean;
procedure SoaresMuZeta(const mass:Real; isdb:Boolean; out mu,zeta:Real);
function SoaresWDMassRadius(const mass,teff,mmwpe:Real; isdb:Boolean; out out_radius:Real):Boolean;
function CarvalhoWDMassFromRadius(const inrad:Real; out massr:Real):Boolean;
function CarvalhoWDMassFromRadius2(const inrad:Real; code:Integer; out massr:Real):Boolean;
// used to fill-in, and use, mass-radius lookup tables
procedure FillInWhiteDwarfArray;
function WhiteDwarfRadiusToMassLookup(const inradius:Real; out massest:Real):Boolean;
function FillZhamiMassToRadiusArray():Integer;
function ZhamiLookup(inmass:Real; out foundrad:Real):Boolean;
// TEff estimates for White Dwarfs
function EstDATEff_DR2(const BPmRP:Currency; out TEff_est:Integer):Boolean;
function EstDATEff_DR2r(G:Currency; RP:Currency; out TEff_est:Integer):Boolean;
function GDA_TEff(G,BP,RP:Currency; out TEff_est:Integer):Boolean;
function GDA_TEffr(G,RP:Currency; out TEff_est:Integer):Boolean;
function GDA_TEffx(G,G1:Currency; out TEff_est:Integer):Boolean;
// extra tests
procedure MakeWDMassRadiusCSV();
//********************************************************************
implementation
//==================================================================
procedure WDBoloEntry.CalculateRadius();
var wdblum,edpa:Real;
begin
  // bolometric luminosity (in suns)
  wdblum := AbsBolMagToBolLum(abm);
  // energy diff (ratio) per area (vs the sun)
  edpa := intpower(TEff/SUN_TEff,4);
  // calculating the radius (in suns)
  radius := Sqrt(wdblum/edpa);
  if mass < 1.44 then begin
     zradius := WhiteDwarfRadiusFromMass(mass);
     bloat := radius/zradius;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor WDBoloEntry.Create;
begin
  TEff := 0;       logTEff := -1;
  logg := -9;
  mass := 0;       avm := -999;
  bcor := 0;       abm := -999;
  bcdif := 0;      bcslope := 0;
  radius := 0;     zradius := 0;
  bloat := 1;      bloatslope := 0;
  gcor := 0;
  gcdif := 0;      gcslope := 0;
end;
//--------------------------------------
function WDBoloEntry.SetFromCSV(instr:string):Boolean;
var splitlist:TStringList;
  reok:Boolean;
  sc:Integer;
begin
  Result := False;
  // setting up the source list...
  splitlist := TStringList.Create;
  splitlist.StrictDelimiter := True;
  splitlist.Delimiter := ',';
  splitlist.DelimitedText := instr;
  // we have 7 items
  if splitlist.Count = 7 then begin
    // TEff
    Val(splitlist[0],TEff,sc);
    reok := (sc=0);
    reok := (TEff > 0);
    // Log g
    if reok then begin
      Val(splitlist[1],logg,sc);
      reok := (sc=0);
    end;
    // default mass
    if reok then reok := StrToReal(splitlist[2],mass);
    // absolute visual magnitude
    if reok then reok := StrToReal(splitlist[3],avm);
    // bolometric correction
    if reok then reok := StrToReal(splitlist[4],bcor);
    // absolute bolometric magnitude
    if reok then reok := StrToReal(splitlist[5],abm);
    // absolute G magnitude (Gaia DR2)
    if reok then reok := StrToReal(splitlist[6],agm);
    if reok then CalculateRadius();
  end;
  // cleanup
  FreeAndNil(splitlist);
  if not reok then Exit;
  // the remaining values get calculated later
  Result := True;
end;
//-------------------------------------------------
function WDBoloEntry.MakeMassRadisCSV():string;
var raddiv,crraddiv:Real;
begin
  Result := FloatToStrF(mass,ffFixed,5,3) + ',';
  Result += FloatToStrF(radius,ffFixed,5,4) + ',';
  Result += FloatToStrF(zradius,ffFixed,5,4) + ',';
  if zradius > 0 then begin
    raddiv := radius/zradius;
    Result += FloatToStrF(raddiv,ffFixed,5,4) + ',';
  end;
  Result += FloatToStrF(bloat,ffFixed,5,3) + ',';
end;
//=====================================================================
// loading and setting data methods
function WDBoloData.LoadAllData(sourcef:TFileName):Boolean;
var DA_loggdex,DA_teffdex,DB_loggdex,DB_teffdex:Integer;
    curritem:WDBoloEntry;
    infile:TextFile;
    currline:string;
const DA_TEffCount = 51;
      DA_LogCount =  5;
      DB_TEffCount = 62;
      DB_LogCount =  5;
begin
  Result := False;
  // open the file
  AssignFile(infile,sourcef);
  Reset(infile);
  ReadLn(infile,currline);
  ReadLn(infile,currline);
  if EOF(infile) then begin
    CloseFile(infile);
    Exit;
  end;
  // starting DA
  SetLength(DA_Data,DA_LogCount);
  // looping over DA
  for DA_loggdex := 0 to (DA_LogCount-1) do begin
    SetLength(DA_Data[DA_loggdex],DA_TEffCount);
    // Different TEff's, same Log g
    for DA_teffdex := 0 to (DA_TEffCount-1) do begin
      Readln(infile,currline);
      if EOF(infile) then begin
        CloseFile(infile);
        Exit;
      end;
      curritem := WDBoloEntry.Create;
      if curritem.SetFromCSV(currline) then begin
        DA_Data[DA_loggdex][DA_teffdex] := curritem;
      end else begin
        FreeAndNil(curritem);
        CloseFile(infile);
        Exit;
      end;
    end;
    (* please note I do not check to see if the termperatures and log g vaues
    are consistent *)
  end;
  // DA and DB, separated by an extra line
  ReadLn(infile,currline);
  ReadLn(infile,currline);
  if EOF(infile) then begin
    CloseFile(infile);
    Exit;
  end;
  // starting DB
  SetLength(DB_Data,DB_LogCount);
  // looping over DB
  for DB_loggdex := 0 to (DB_LogCount-1) do begin
    SetLength(DB_Data[DB_loggdex],DB_TEffCount);
    // Different TEff's, same Log g
    for DB_teffdex := 0 to (DB_TEffCount-1) do begin
      Readln(infile,currline);
      if EOF(infile) and (DB_teffdex <> (DB_TEffCount-1)) then begin
        CloseFile(infile);
        Exit;
      end;
      curritem := WDBoloEntry.Create;
      if curritem.SetFromCSV(currline) then begin
        DB_Data[DB_loggdex][DB_teffdex] := curritem;
      end else begin
        FreeAndNil(curritem);
        CloseFile(infile);
        Exit;
      end;
    end;
    (* please note I do not check to see if the termperatures and log g vaues
    are consistent *)
  end;
  // done
  CloseFile(infile);
  Result := True;
  Exit;
end;
//--------------------------------------------
// sets up temperatures and blackbody bolometric corrections
function WDBoloData.SetupBlackbody:Boolean;
var DA_nteff,DB_nteff:Integer;
    DA_nlogg,DB_nlogg:Integer;
    DA_loggdex,DA_teffdex1,DA_teffdex2:Integer;
    DB_loggdex,DB_teffdex1,DB_teffdex2:Integer;
    clogg:Currency;
    DA_bbdex,DB_bbdex:Integer;
    cbcv:Real;
    tempteff1,tempteff2:Integer;
begin
  Result := False;
  // Getting DA TEffs and checking the first Log g array
  DA_nlogg := Length(DA_Data);
  if DA_nlogg = 0 then Exit;
  DA_nteff := Length(DA_Data[0]);
  if DA_nteff = 0 then Exit;
  SetLength(DA_temparray,DA_nteff);
  clogg := DA_Data[0][0].logg;
  for DA_teffdex1 := 0 to (DA_nteff-1) do begin
    DA_temparray[DA_teffdex1] := DA_Data[0][DA_teffdex1].TEff;
    if (DA_Data[0][DA_teffdex1].logg) <> clogg then Exit;
  end;
  // now, we check for log g and teff consistency for the other log g's
  for DA_loggdex := 1 to (DA_nlogg-1) do begin
    clogg := DA_Data[DA_loggdex][0].logg;
    for DA_teffdex2 := 0 to (DA_nteff-1) do begin
      tempteff1 := DA_temparray[DA_teffdex2];
      tempteff2 := DA_Data[DA_loggdex][DA_teffdex2].TEff;
      if tempteff1 <> tempteff2 then Exit;
      if (DA_Data[DA_loggdex][DA_teffdex2].logg) <> clogg then Exit;
    end;
  end;
  // Getting DB TEffs and checking the first Log g array
  DB_nlogg := Length(DB_Data);
  if DB_nlogg = 0 then Exit;
  DB_nteff := Length(DB_Data[0]);
  if DB_nteff = 0 then Exit;
  SetLength(DB_temparray,DB_nteff);
  clogg := DB_Data[0][0].logg;
  for DB_teffdex1 := 0 to (DB_nteff-1) do begin
    DB_temparray[DB_teffdex1] := DB_Data[0][DB_teffdex1].TEff;
    if (DB_Data[0][DB_teffdex1].logg) <> clogg then Exit;
  end;
  // now, we check for log g and teff consistency for the other log g's
  for DB_loggdex := 1 to (DB_nlogg-1) do begin
    clogg := DB_Data[DB_loggdex][0].logg;
    for DB_teffdex2 := 0 to (DB_nteff-1) do begin
      tempteff1 := DB_temparray[DB_teffdex2];
      tempteff2 := DB_Data[DB_loggdex][DB_teffdex2].TEff;
      if tempteff1 <> tempteff2 then Exit;
      if (DB_Data[DB_loggdex][DB_teffdex2].logg) <> clogg then Exit;
    end;
  end;
  (* With the checks out of the way, we now calculate TEff and Blackbody based
  Bolometric corrections *)
  SetLength(DA_bb_bcvarray,DA_nteff);
  for DA_bbdex := 0 to (DA_nteff-1) do begin
    cbcv := SUN_BCv + BlackBodyExtraBCv(DA_temparray[DA_bbdex]);
    DA_bb_bcvarray[DA_bbdex] := cbcv;
  end;
  SetLength(DB_bb_bcvarray,DB_nteff);
  for DB_bbdex := 0 to (DB_nteff-1) do begin
    cbcv := SUN_BCv + BlackBodyExtraBCv(DB_temparray[DB_bbdex]);
    DB_bb_bcvarray[DB_bbdex] := cbcv;
  end;
  // done
  Result := True;
end;
//----------------------------------------
// calculates blackbody BCv diffrences from the loaded ones, as well as
// associated slopes
function WDBoloData.SetupDifferencesAndSlopes:Boolean;
var DA_loggdex1,DA_loggdex2,DA_tempdex1,DA_tempdex2:Integer;
    DB_loggdex1,DB_loggdex2,DB_tempdex1,DB_tempdex2:Integer;
    curritem,nextitem:WDBoloEntry;
    teffdiff,bcvdiff:Real;
begin
  // bolometric correction differences
  for DA_loggdex1 := 0 to (Length(DA_Data)-1) do begin
    for DA_tempdex1 := 0 to (Length(DA_Data[0])-1) do begin
      curritem := DA_Data[DA_loggdex1][DA_tempdex1];
      curritem.bcdif := curritem.bcor - DA_bb_bcvarray[DA_tempdex1];
    end;
  end;
  for DB_loggdex1 := 0 to (Length(DB_Data)-1) do begin
    for DB_tempdex1 := 0 to (Length(DB_Data[0])-1) do begin
      curritem := DB_Data[DB_loggdex1][DB_tempdex1];
      curritem.bcdif := curritem.bcor - DB_bb_bcvarray[DB_tempdex1];
    end;
  end;
  // slopes for DA
  for DA_loggdex2 := 0 to (Length(DA_Data)-1) do begin
    for DA_tempdex2 := 0 to (Length(DA_Data[0])-2) do begin
      curritem := DA_Data[DA_loggdex2][DA_tempdex2];
      nextitem := DA_Data[DA_loggdex2][DA_tempdex2+1];
      teffdiff := nextitem.TEff - curritem.TEff;
      bcvdiff := nextitem.bcdif - curritem.bcdif;
      curritem.bcslope:= bcvdiff/teffdiff;
    end;
    // for the last temp, we just use the next to last
    nextitem.bcslope := curritem.bcslope;
  end;
  // slopes for DB
  for DB_loggdex2 := 0 to (Length(DB_Data)-1) do begin
    for DB_tempdex2 := 0 to (Length(DB_Data[0])-2) do begin
      curritem := DB_Data[DB_loggdex2][DB_tempdex2];
      nextitem := DB_Data[DB_loggdex2][DB_tempdex2+1];
      teffdiff := nextitem.TEff - curritem.TEff;
      bcvdiff := nextitem.bcdif - curritem.bcdif;
      curritem.bcslope:= bcvdiff/teffdiff;
    end;
    // for the last temp, we just use the next to last
    nextitem.bcslope := curritem.bcslope;
  end;
  // done
  Result := True;
end;
//-----------------------------------------------------
(* The items contains radii and masses from the Bergeron files, I compare these
with the analytical approximate radius (from the mass) to get a rough 'thermal bloat'.
In this method, the bloats are 'normalized' (always >=1, always the same or increasing),
and then a slope based on Log10(TEff) increase and bloat is calculated for
interpolation using TEff. Please note that accuracy is uncertain, but for hot
low-mass white dwarfs, taking bloat into account certainly is much more accurate
than using the analytical radius. *)
function WDBoloData.SetupBloats:Boolean;
var DA_loggdex1,DA_loggdex2,DA_tempdex1,DA_tempdex2:Integer;
    DB_loggdex1,DB_loggdex2,DB_tempdex1,DB_tempdex2:Integer;
    curritem,nextitem:WDBoloEntry;
    logteffdiff,bloatdiff:Real;
begin
  // calculating log10 TEff and making sure bloat is always 1 or more
  for DA_loggdex1 := 0 to (Length(DA_Data)-1) do begin
    for DA_tempdex1 := 0 to (Length(DA_Data[0])-1) do begin
      curritem := DA_Data[DA_loggdex1][DA_tempdex1];
      curritem.logTEff := log10(curritem.TEff);
      if curritem.logg = 9 then curritem.bloat:= curritem.bloat / 1.0155;  // special
      if curritem.bloat < 1 then curritem.bloat := 1;
    end;
  end;
  for DB_loggdex1 := 0 to (Length(DB_Data)-1) do begin
    for DB_tempdex1 := 0 to (Length(DB_Data[0])-1) do begin
      curritem := DB_Data[DB_loggdex1][DB_tempdex1];
      curritem.logTEff := log10(curritem.TEff);
      if curritem.bloat < 1 then curritem.bloat := 1;
    end;
  end;
  // making sure bloat never decreases, and calculating bloat slopes
  for DB_loggdex2 := 0 to (Length(DB_Data)-1) do begin
    for DB_tempdex2 := 0 to (Length(DB_Data[0])-2) do begin
      curritem := DB_Data[DB_loggdex2][DB_tempdex2];
      nextitem := DB_Data[DB_loggdex2][DB_tempdex2+1];
      // never decreasing bloat
      if nextitem.bloat < curritem.bloat then nextitem.bloat := curritem.bloat;
      // the slope
      bloatdiff := nextitem.bloat - curritem.bloat;
      logteffdiff := nextitem.logTEff - curritem.logTEff;
      curritem.bloatslope := bloatdiff/logteffdiff;
    end;
    // for the last temp, we just use the next to last
    nextitem.bloatslope := curritem.bloatslope;
  end;
  Result := True;
end;

//------------------------------------------------------
function WDBoloData.VerifyChecks:Boolean;
var DA_logdex,DA_teffdex,DB_logdex,DB_teffdex:Integer;
    curritem:WDBoloEntry;
    teffdex,loggdex:Integer;
    doblend:Boolean;
    calcbc,diffbc:Real;
begin
  Result := False;
  // checking DA
  for DA_logdex := 0 to High(DA_Data) do begin
    for DA_teffdex := 0 to High(DA_Data[0]) do begin
      curritem := DA_Data[DA_logdex][DA_teffdex];
      // checking Log g finding
      loggdex := FindLogG(True,curritem.logg,doblend);
      Assert(loggdex = DA_logdex);
      Assert(not doblend);
      // checking Teff finding
      teffdex := FindTEffIndex(True,curritem.TEff);
      Assert(teffdex = DA_teffdex);
      // checking the calculated BCv
      calcbc := CalculateBCv(curritem.TEff,True,False,loggdex,teffdex);
      diffbc := 1000*Abs(calcbc - curritem.bcor);
      Assert(diffbc < 1);
    end;
  end;
  // checking DB
  for DB_logdex := 0 to High(DB_Data) do begin
    for DB_teffdex := 0 to High(DB_Data[0]) do begin
      curritem := DB_Data[DB_logdex][DB_teffdex];
      // checking Log g finding
      loggdex := FindLogG(False,curritem.logg,doblend);
      Assert(loggdex = DB_logdex);
      Assert(not doblend);
      // checking Teff finding
      teffdex := FindTEffIndex(False,curritem.TEff);
      Assert(teffdex = DB_teffdex);
      // checking the calculated BCv
      calcbc := CalculateBCv(curritem.TEff,False,False,loggdex,teffdex);
      diffbc := 1000*Abs(calcbc - curritem.bcor);
      Assert(diffbc < 1);
    end;
  end;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// locating where to find data to calculate the result
//------------------------------------------
function WDBoloData.FindTEffIndex(const hydr:Boolean; const findTEff:Real):Integer;
var maxi,mini,median:Integer;
begin
  mini := 0;
  Result := 0;
  if hydr then begin
    // end cases
    maxi := High(DA_temparray);
    if findTEff >= DA_temparray[maxi] then begin
      Result := maxi;
      Exit;
    end
    else if findTEff < DA_temparray[0] then Exit;
    // binary search
    Dec(maxi);
    while(True) do begin
      median := (mini+maxi) div 2;
      if (findTEff >= DA_temparray[median]) and (findTEff < DA_temparray[median+1]) then begin
        Result := median;
        Exit;
      end;
      if findTEff < DA_temparray[median] then maxi := median-1
      else mini := median+1;
    end;
  end else begin
    // end cases
    maxi := High(DB_temparray);
    if findTEff >= DB_temparray[maxi] then begin
      Result := maxi;
      Exit;
    end
    else if findTEff < DB_temparray[0] then Exit;
    // binary search
    Dec(maxi);
    while(True) do begin
      median := (mini+maxi) div 2;
      if (findTEff >= DB_temparray[median]) and (findTEff < DB_temparray[median+1]) then begin
        Result := median;
        Exit;
      end;
      if findTEff < DB_temparray[median] then maxi := median-1
      else mini := median+1;
    end;
  end;
  // we should never get here
  Assert(False);
end;
//------------------------------------------
function WDBoloData.FindLogG(const hydr:Boolean; const findLogG:Currency; out blend:Boolean):Integer;
var logdexA,logdexB:Integer;
    cclog:Currency;
const badmax:Currency = 10.5;
      badmin:Currency = 6;
      goffs1:Currency = 0.1667;
      goffs2:Currency = 0.3333;
begin
  Result := -1;
  if findLogG < badmin then Exit;
  if findLogG > badmax then Exit;
  // hydroden atmosphere
  if hydr then begin
    // looping over all except the last...
    for logdexA := 0 to (Length(DA_Data)-2) do begin
      cclog := DA_Data[logdexA][0].logg;
      if findLogG < (cclog + goffs2) then begin
        Result := logdexA;
        blend := (findLogG > (cclog+goffs1));
        Exit;
      end;
    end;
    // if we get here, the last one is the result
    Result := Length(DA_Data)-1;
    blend := False;
  end else begin
    // looping over all except the last...
    for logdexB := 0 to (Length(DB_Data)-2) do begin
      cclog := DB_Data[logdexB][0].logg;
      if findLogG < (cclog + goffs2) then begin
        Result := logdexB;
        blend := (findLogG > (cclog+goffs1));
        Exit;
      end;
    end;
    // if we get here, the last one is the result
    Result := Length(DB_Data)-1;
    blend := False;
  end;
  // done!
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// calculating BCv with the locations...
function WDBoloData.CalculateBCv(TEff:Real; hydr,blend:Boolean; loggdex,teffdex:Integer):Real;
var bbbcv,teffdif,mult1,mult2,offset1,offset2:Real;
    tempshow:Real;
begin
  // calculating Black-Body BCv
  bbbcv := SUN_BCv + BlackBodyExtraBCv(TEff);
  // temperature difference
  if hydr then teffdif := TEff - DA_temparray[teffdex]
  else teffdif := TEff - DB_temparray[teffdex];
  // slope multiples
  if hydr then mult1 := (DA_Data[loggdex][teffdex].bcslope)*teffdif
  else mult1 := (DB_Data[loggdex][teffdex].bcslope)*teffdif;
  if blend then begin
    if hydr then mult2 := (DA_Data[loggdex+1][teffdex].bcslope)*teffdif
    else mult2 := (DB_Data[loggdex+1][teffdex].bcslope)*teffdif;
  end;
  // offsets
  if hydr then offset1 := (DA_Data[loggdex][teffdex].bcdif) + mult1
  else offset1 := (DB_Data[loggdex][teffdex].bcdif) + mult1;
  if blend then begin
    if hydr then offset2 := (DA_Data[loggdex+1][teffdex].bcdif) + mult2
    else offset2 := (DB_Data[loggdex+1][teffdex].bcdif) + mult2;
    offset1 := (offset1 + offset2)/2;
  end;
  // finally
  tempshow := DA_bb_bcvarray[teffdex];
  Result := bbbcv + offset1;
end;
//------------------------------------------------------------
function WDBoloData.CalculateBloat(TEff:Real; hydr,blend:Boolean; loggdex,teffdex:Integer):Real;
var logteffdiff,mult1,mult2:Real;
    bloat1,bloat2:Real;
begin
  // log temperature difference
  if hydr then logteffdiff := log10(TEff) - DA_Data[0][teffdex].logTEff
  else logteffdiff := log10(TEff) - DB_Data[0][teffdex].logTEff;
  // slope multiples
  if hydr then mult1 := (DA_Data[loggdex][teffdex].bloatslope)*logteffdiff
  else mult1 := (DB_Data[loggdex][teffdex].bloatslope)*logteffdiff;
  if blend then begin
    if hydr then mult2 := (DA_Data[loggdex+1][teffdex].bloatslope)*logteffdiff
    else mult2 := (DB_Data[loggdex+1][teffdex].bloatslope)*logteffdiff;
  end;
  // bloats
  if hydr then bloat1 := (DA_Data[loggdex][teffdex].bloat) + mult1
  else bloat1 := (DB_Data[loggdex][teffdex].bloat) + mult1;
  if blend then begin
    if hydr then bloat2 := (DA_Data[loggdex+1][teffdex].bloat) + mult2
    else bloat2 := (DB_Data[loggdex+1][teffdex].bloat) + mult2;
    bloat1 := (bloat1 + bloat2)/2;
  end;
  // finally
  Result := bloat1;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// special test output
function WDBoloData.MakeRadiusLine(const isdb:Boolean; tempdex:Integer):string;
var logg_dex:Integer;
    lteff,lteffs:Real;
    item_ptr:WDBoloEntry;
    entryout:string;
begin
  // no checks here!
  if (not isdb) then begin
    Result := IntToStr(DA_temparray[tempdex]) + ',';
    lteff := log10(DA_temparray[tempdex]);
    Result += FloatToStrF(lteff,ffFixed,7,5) + ',';
    lteffs := Sqr(lteff);
    Result += FloatToStrF(lteffs,ffFixed,7,5) + ',';
    // log g looping
    for logg_dex := 0 to 5 do begin
      item_ptr := DA_Data[logg_dex][tempdex];
      entryout := item_ptr.MakeMassRadisCSV();
      Result += entryout + ',,,'; // 2 empty fields for spreadsheet use
    end;
  end else begin
    Result := IntToStr(DB_temparray[tempdex]) + ',';
    lteff := log10(DB_temparray[tempdex]);
    Result += FloatToStrF(lteff,ffFixed,7,5) + ',';
    lteffs := Sqr(lteff);
    Result += FloatToStrF(lteffs,ffFixed,7,5) + ',';
    // log g looping
    for logg_dex := 0 to 4 do begin
      item_ptr := DB_Data[logg_dex][tempdex];
      entryout := item_ptr.MakeMassRadisCSV();
      Result += entryout + ',,,'; // 2 empty fields for spreadsheet use
    end;
  end;
end;
//---------------------------------------------------
function WDBoloData.MakeRadiusHeader():string;
var loggdex:Integer;
const iloghead = 'Mass,Radius,0K Radius,Ratio,Bloat,Ratio,(User 1),(User 2)';
begin
  Result := 'TEff,Log(TEff),Log(TEff)^2,';
  for loggdex := 0 to 5 do  Result += iloghead + ',';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor WDBoloData.Create;
begin

end;
//--------------------------------------------------
Destructor WDBoloData.Destroy;
var oindexA,iindexA,oindexB,iindexB:Integer;
begin
  // getting rid of DA Data
  for oindexA := 0 to High(DA_Data) do begin
    for iindexA := 0 to High(DA_Data[oindexA]) do begin
      FreeAndNil(DA_Data[oindexA][iindexA]);
    end;
    SetLength(DA_Data[oindexA],0);
  end;
  SetLength(DA_Data,0);
  SetLength(DA_temparray,0);
  SetLength(DA_bb_bcvarray,0);
  // getting rid of DB Data
  for oindexB := 0 to High(DB_Data) do begin
    for iindexB := 0 to High(DB_Data[oindexB]) do begin
      FreeAndNil(DB_Data[oindexB][iindexB]);
    end;
    SetLength(DB_Data[oindexB],0);
  end;
  SetLength(DB_Data,0);
  SetLength(DB_temparray,0);
  SetLength(DB_bb_bcvarray,0);
  inherited;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// The external use methods
//--------------------------------
function WDBoloData.InitializeFromFile(datafn:TFileName):Boolean;
begin
  Result := False;
  if not LoadAllData(datafn) then Exit;
  if not SetupBlackbody() then Exit;
  Result := SetupDifferencesAndSlopes();
  Result := SetupBloats();
  // VerifyChecks();
end;
//--------------------------------
function WDBoloData.GetBCvBloat(const hydr:Boolean; const logg_in:Currency; const TEff_in:Real; out BCv_out,Bloat_out:Real):Boolean;
var teffdex,loggdex:Integer;
    doblend:Boolean;
begin
  Result := False;
  // finding where to look for the data
  teffdex := FindTEffIndex(hydr,TEff_in);
  if (teffdex < 0) then Exit;
  loggdex := FindLogG(hydr,logg_in,doblend);
  if loggdex < 0 then Exit;
  // calculating the result...
  BCv_out := CalculateBCv(TEff_in,hydr,doblend,loggdex,teffdex);
  // currenty, logg = 9.5 is 'bloated' even at 0 temp, so skip that
  if (loggdex = 6 ) or (doblend and (loggdex = 5)) then Bloat_out := 1
  else Bloat_out := CalculateBloat(TEff_in,hydr,doblend,loggdex,teffdex);
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// test output
procedure WDBoloData.MakeRadiusInfoFile();
var wdradfile:TextFile;
    outline,inhead:string;
    tempdex_da,tempdex_db:Integer;
    tempmax_da,tempmax_db:Integer;
const loghead = ',,,Log g,7:,,,,,,,Log g,7.5:,,,,,,,Log g,8:,,,,,,,Log g:,8.5,,,,,,,Log g:,9,,,,,,,Log g:,9.5';
begin
    // starting the file...
    inhead := MakeRadiusHeader();
    AssignFile(wdradfile,'model_wdradius.csv');
    Rewrite(wdradfile);
    // hydrogen header
    Writeln(wdradfile,'DA Radii');
    Writeln(wdradfile,loghead);
    Writeln(wdradfile,inhead);
    // temperature line output...
    tempmax_da := Length(DA_temparray) - 1;
    for tempdex_da := 0 to tempmax_da do begin
      outline := MakeRadiusLine(False,tempdex_da);
      Writeln(wdradfile,outline);
    end;
    // helium header
    Writeln(wdradfile,'DB Radii');
    Writeln(wdradfile,loghead);
    Writeln(wdradfile,inhead);
    // temperature line output...
    tempmax_db := Length(DB_temparray) - 1;
    for tempdex_db := 0 to tempmax_db do begin
      outline := MakeRadiusLine(True,tempdex_db);
      Writeln(wdradfile,outline);
    end;
    // finishing off the file
    Flush(wdradfile);
    CloseFile(wdradfile);
end;
//===================================================================
// the current core 'Zero-Temp' WD mass-radius method
function WhiteDwarfRadiusFromMass(const inmass:Real):Real;
var mass1,mass2,interm:Real;
const basdiam = 0.01121;
      climit = 1.44;
begin
  Assert(inmass<climit);
  Assert(inmass>=wdmin);
  // intermediates calculation
  mass1 := climit / inmass;
  mass2 := inmass / climit;
  mass1 := power(mass1,2/3);
  mass2 := power(mass2,2/3);
  interm := Sqrt(mass1-mass2);
  Result := basdiam*interm;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// alternative/additional WD mass-radius methods
//--------------------------------------------------------
(* second version: R/ polynomial in R, where R is in km:
https://www.icranet.org/am/talks/talk_zhami.pdf
*)
function WhiteDwarfRadiusToMassP(const inRad:Real; const inpkm:Boolean):Real;
var kmrad,interm:Real;
const coff:array[0..4] of Real = (14.65,0.665,2.171e-5,-1.381e-9,1.341e-12);
begin
  Assert(inRad>0);
  if (not inpkm) then kmrad := inrad*695700
  else kmrad := inrad;
  interm := PolEval(kmrad,coff,5);
  Result := kmrad/interm;
end;
//-------------------------------------------------------------
(* from 'Constraining Effective Temperature, Mass and Radius of Hot White Dwarfs'
Elvis do A. Soares (2017)  . Radii is in Earth Radii *)
// according to the paper, the following comes from Nauenberg 1972
function NauenbergWhiteDwarfRadius(const mass,mmwpe:Real; out resx:Real):Boolean;
var mch,interm1,interm2,interm3:Real;
begin
  // basic setup
  Result := False;
  if (mmwpe <= 0) or (mass <= 0) then Exit;
  mch := 5.816/Sqr(mmwpe);
  if mass >= mch then Exit;
  // calculating...
  interm1 := 2.45354/mmwpe;
  interm2 := cbrt(mch/mass);
  interm3 := intpower(cbrt(mass/mch),4);
  resx :=  interm1*interm2*sqrt(1-interm3);
  Result := True;
end;
//----------------------
procedure SoaresMuZeta(const mass:Real; isdb:Boolean; out mu,zeta:Real);
begin
  if (not isdb) then begin
     zeta := 0.984 - 0.021*mass;
     if mass < 0.448 then mu := 0.48
     else if mass > 0.503 then mu := 0.78*mass + 0.32
     else mu := 4.2*mass - 1.4;
  end else begin
     zeta := 0.92 + 0.02*mass;
     mu := 1.25 - 0.59*mass;
  end;
end;
//--------------------------
function SoaresWDMassRadius(const mass,teff,mmwpe:Real; isdb:Boolean; out out_radius:Real):Boolean;
var mu,zeta,radch:Real;
    interm1,interm2:Real;
begin
    Result := False;
    if teff <= 0 then Exit;
    if (not NauenbergWhiteDwarfRadius(mass,mmwpe,radch)) then Exit;
    SoaresMuZeta(mass,isdb,mu,zeta);
    // calculating...
    interm1 := (1/mu)*(teff/588862)*(1/mass);
    interm2 := zeta*radch;
    interm2 := (1 - interm1*interm2);
    interm1 := zeta*radch;
    out_radius := interm1/interm2;
    // done
    Result := True;
end;
//---------------------------------------------------------------------------
(* From 'Mass-Radius diagram for compact stars' , Carvalho, Marinho and
Malheiro (2015). Uses Tolman-Openheimer-Volkof, radius input in Km *)
function CarvalhoWDMassFromRadius(const inrad:Real; out massr:Real):Boolean;
const xa = 2.325e-5;
      xc = 7.277e-9;
var interm1,interm2:Real;
begin
    Result := False;
    if (inrad < 600) or (inrad > 17000) then Exit;
    interm1 := xa*inrad + 0.4617;
    interm2 := xc*Sqr(inrad);
    interm2 := exp(interm2) - 0.644;
    massr := interm1 / interm2;
    Result := True;
end;
//--------------------------------------------------------------------
(* From 'General Relativity effects in the structure of massive white dwarfs',
Carvaljo, Mrinho, and Malheiro (2017).Uses Tolman-Oppenheimer-Volkof, radius
inut in Km. code 0: μe=2, 1: μe=2.154 (Iron), 2:He, 3:C, 4:O  *)
function CarvalhoWDMassFromRadius2(const inrad:Real; code:Integer; out massr:Real):Boolean;
var interm:Real;
const coff0:array[0..4] of Real = ( 20.86,0.66, 2.48e-5, 2.43e-9 ,0.69105826);
      coff1:array[0..4] of Real = ( 15.05,0.79, 3.56e-6, 4.90e-9 ,0.69105826);
      coff2:array[0..4] of Real = ( 18.95,0.68, 1.84e-5,-9.85e-10,0.69105826);
      coff3:array[0..4] of Real = (  0.79,0.69, 1.22e-5, 6.70e-12,0.69105826);
      coff4:array[0..4] of Real = (-27.06,0.76,-1.21e-5, 3.10e-9 ,0.69105826);
begin
    Result := False;
    if (code < 0) or (code > 4) then Exit;
    if (inrad < 1145) then Exit;
    // computing the intermediate
    case code of
        0: interm := PolEval(inrad,coff0,5);
        1: interm := PolEval(inrad,coff1,5);
        2: interm := PolEval(inrad,coff2,5);
        3: interm := PolEval(inrad,coff3,5);
        4: interm := PolEval(inrad,coff4,5);
        else Exit;
    end;
    // final result
    Result := True;
    massr := inrad / interm;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// used to fill-in, and use, mass-radius lookup tables
//-----------------------------------
procedure FillInWhiteDwarfArray;
var steps,aindex:Integer;
    massx:Real;
begin
  steps := Floor((wdmax-wdmin)/wdstep);
  SetLength(wd_radii,steps);
  for aindex := 0 to (steps-1) do begin
    massx := wdmin + aindex*wdstep;
    wd_radii[aindex] := WhiteDwarfRadiusFromMass(massx);
  end;
end;
//-----------------------------------
function WhiteDwarfRadiusToMassLookup(const inradius:Real; out massest:Real):Boolean;
var maxdex:Integer;
    mindex,middex:Integer;
    found:Boolean;
begin
  Result := False;
  // special out-of-bounds cases
  if inradius > wd_radii[0] then Exit;
  maxdex:= High(wd_radii);
  if inradius <= wd_radii[maxdex] then begin
    Result := True;
    massest := wdmin + wdstep*maxdex + 0.005;
    Exit;
  end;
  // otherwise, we use a binary search
  mindex := 0;
  found := False;
  while true do begin
    middex := (maxdex + mindex) div 2;
    found := (inradius <= wd_radii[middex]) and (inradius > wd_radii[middex+1]);
    if found then break;
    if inradius > wd_radii[middex] then maxdex := middex-1
    else mindex := middex + 1;
  end;
  // done
  if found then massest := wdmin + middex*wdstep + 0.005;
  Result := found;
end;
//----------------------------------------------------
(* since the zhami function is radius -> mass, and I need mass -> radius for
checking thermal bloat (from the Bergeron files), I fill in a reverse lookup
array, using intermixed loops for mass (increasing) and radius (decreasing)
This relies on the mass always increasing as radius decreases. *)
function FillZhamiMassToRadiusArray:Integer;
var arrlen,workrad,therad:Integer;
    currmass:Currency;
    cmass,cmassdif,omass,omassdif:Real;
begin
  // initial setup
  currmass := zhami_min;
  workrad := 17001;
  cmass := WhiteDwarfRadiusToMassP(workrad,True);
  cmassdif := cmass-Real(currmass);
  arrlen := 0;
  // the mass loop
  while currmass < zhami_max do begin
    // the radius loop
    while workrad > 0 do begin
      Dec(workrad);
      omass := cmass;
      omassdif := cmassdif;
      cmass := WhiteDwarfRadiusToMassP(workrad,True);
      cmassdif := cmass-Real(currmass);
      if cmassdif >= 0 then break;
    end;
    // once here, we find the best radius for the mass (workrad or workrad+1)
    if Abs(omassdif) < cmassdif then therad := workrad + 1
    else therad := workrad;
    Inc(arrlen);
    SetLength(zhami_mass_radius,arrlen);
    zhami_mass_radius[arrlen-1] := workrad / 695700;
    // next mass...
    currmass += zhami_step;
  end;
  // done
  Result := arrlen;
end;
//-------------------------------------------
function ZhamiLookup(inmass:Real; out foundrad:Real):Boolean;
var lindex:Integer;
begin
  Result := False;
  if inmass < zhami_min then Exit;
  if inmass >= zhami_max then Exit;
  inmass := (inmass - zhami_min)*1000;
  lindex := Round(inmass);
  foundrad := zhami_mass_radius[lindex];
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// TEff estimates for White Dwarfs
(* Fitted in 2 parts using poly trend line in Libre Office, data from
Gentile Fusillo+ 2018 (https://arxiv.org/abs/1807.03315) *)
function EstDATEff_DR2(const BPmRP:Currency; out TEff_est:Integer):Boolean;
var bprpr,interm:Real;
    tempint:Int64;
const hot_coff:array[0..5] of Real = (1.179e4,-1.631e4,2.583e4,-3.77e4,3.428e4,-1.265e4);
      cool_coff:array[0..3] of Real = (1.031e4,-8774,5139,-1401);
begin
  Result := False;
  // bounds
  if (BPmRP < -0.455) or (BPmRP > 1.64) then Exit;
  // the ranges of the 2 equation overlap. I'll set the division at 0.87
  bprpr := CurrToReal(BPmRP);
  if (bprpr < 0.87) then interm := PolEval(bprpr,hot_coff,6)
  else interm := PolEval(bprpr,cool_coff,4);
  // rounding
  Result := True;
  tempint := round(interm/50);
  TEff_est := tempint*50;
end;
//---------------------------------------------------------
function EstDATEff_DR2r(G:Currency; RP:Currency; out TEff_est:Integer):Boolean;
var gmrp,interm:Real;
    tempint:Int64;
const gmrp_coff:array[0..3] of Real = (1.248e4,-2.393e4,2.859e4,-1.461e4);
begin
  Result := False;
  if (RP >= 90) or (G > 90) then Exit;
  gmrp := G - RP;
  // bounds
  if (gmrp < 0.1) or (gmrp > 0.85) then Exit;
  interm := PolEval(gmrp,gmrp_coff,4);
  // rounding
  Result := True;
  tempint := round(interm/50);
  TEff_est := tempint*50;
end;
//------------------------------------------------------------------
(* Fitted in 2 parts using ptrans (my utility) at 99%, data from
Gentile Fusillo+ 2018 (https://arxiv.org/abs/1807.03315) *)
function GDA_TEff(G,BP,RP:Currency; out TEff_est:Integer):Boolean;
var bpmrp,bpmg,interm:Real;
    tempint:Int64;
    bmgok:Boolean;
const hot_a:array[0..3] of Real = ( 11644, -15866, 30349, -41679 );
      hot_b:array[0..3] of Real = ( 12009, -23195, 57803, -58067 );
      cool:array[0..4] of Real = ( 11883, -15152, 14294, -6982.9, 1235.5 );
begin
  Result := False;
  if not MakeColorCheck(BP,RP,-0.415,1.624,bpmrp) then Exit;
  bmgok := MakeColorCheck(BP,G,-0.181,0.256,bpmg);
  // Hotter WDs
  if (bpmrp <= 0.2) and bmgok and (bpmrp >= -0.405) then begin
    interm := PolEval(bpmrp,hot_b,3) - 75731*bpmrp*bpmg + 18423*bpmg; // std err ~ 287K
  end
  else if (bpmrp <= 0.2) then interm := PolEval(bpmrp,hot_a,3)  // std err ~ 435K
  // Cooler WDs
  else interm := PolEval(bpmrp,cool,5); // std err ~51 K

  // rounding...
  Result := True;
  if bpmrp <= 0.2 then begin
    tempint := round(interm/100);
    TEff_est := tempint*100;
  end else begin
    tempint := round(interm/50);
    TEff_est := tempint*50;
  end;
end;
//-----------------------------------------
// using no BP, I will assume for cooler White Dwarfs only
function GDA_TEffr(G,RP:Currency; out TEff_est:Integer):Boolean;
var gmrp,interm:Real;
    tempint:Int64;
const coff:array[0..3] of Real = ( 12260, -23093, 27207, -13933 );
begin
  Result := False;
  if not MakeColorCheck(G,Rp,0.3,0.848,gmrp) then Exit;
  interm := PolEval(gmrp,coff,4); // std err ~ 61K
  // rounding
  Result := True;
  tempint := round(interm/50);
  TEff_est := tempint*50;
end;
//------------------------------------------------------------
(* Uses G and G from Gaia DR1 to estimate DA TEff *)
function GDA_TEffx(G,G1:Currency; out TEff_est:Integer):Boolean;
var gmg,interm:Real;
    tempint:Int64;
const  hot:array[0..3] of Real = ( 12816, -2.0795E5, 2.8447E6, -1.8435E7 );
      cold:array[0..3] of Real = ( 10484, -84218, 4.2504E5, -9.0008E5 );
begin
  Result := False;
  if not MakeColorCheck(G,G1,-0.031,0.192,gmg) then Exit;
  if gmg < 0.05 then interm := PolEval(gmg,hot,4) // std err ~ 545 K
  else interm := PolEval(gmg,cold,4); // std err ~ 282K
  // rounding
  Result := True;
  if gmg < 0.05 then begin
      tempint := round(interm/100);
      TEff_est := tempint*100;
  end else begin
      tempint := round(interm/250);
      TEff_est := tempint*250;
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// testing White Dwarf radius to mass stuff
procedure MakeWDMassRadiusCSV();
var outline:string;
    currad:Integer;
    curmass,zmass,workrad:Real;
    wdtestfile:TextFile;
begin
  // starting the file...
  AssignFile(wdtestfile,'calcwdm.csv');
  Rewrite(wdtestfile);
  Writeln(wdtestfile,'Radius (KM),Mass (Current), Mass (Zhami), Mass (Carvalho 1), Soares 15k DA,Soares 15k DB');
  // we test masses for particular radii
  currad := 500;
  while (currad < 40000 ) do begin
    outline := IntToStr(currad) + ',';
    // current estimate
    workrad := currad/695700.0;
    if WhiteDwarfRadiusToMassLookup(workrad,curmass) then begin
      zmass := curmass;
      outline += FloatToStrF(curmass,ffFixed,5,3) + ',';
    end else outline += ',';
    // zhami estimate
    curmass := WhiteDwarfRadiusToMassP(workrad,False);
    outline += FloatToStrF(curmass,ffFixed,5,3) + ',';
    workrad := currad;
    // carvalho estimate
    if CarvalhoWDMassFromRadius(currad,curmass) then begin
      outline += FloatToStrF(curmass,ffFixed,5,3) + ',';
    end else outline += ',';
    // Soares 15k DA
    if SoaresWDMassRadius(zmass,15000,2,False,workrad) then begin
      workrad := 6371*workrad;
      outline += FloatToStrF(workrad,ffFixed,5,0) + ',';
    end else outline += ',';
    // Soares 15k DB
    if SoaresWDMassRadius(zmass,15000,2,True,workrad) then begin
      workrad := 6371*workrad;
      outline += FloatToStrF(workrad,ffFixed,5,0);
    end;
    // writing...
    Writeln(wdtestfile,outline);
    // incrementing, the stepsize depends of the radius...
    if currad < 2500 then currad += 100
    else if currad < 10000 then currad += 200
    else currad += 500;
  end;
  // finishing off the file
  Flush(wdtestfile);
  CloseFile(wdtestfile);
end;
//**************************************************************************
begin
  wdbcloaded := False;
  wdbc := WDBoloData.Create;
  wdbcloaded := wdbc.InitializeFromFile('wdbc.dat');
  if not wdbcloaded then begin
    FreeAndNil(wdbc);
    Assert(False);
  end;

  FillInWhiteDwarfArray();
  // FillZhamiMassToRadiusArray();
  // wdbc.MakeRadiusInfoFile();
  // MakeWDMassRadiusCSV();

end.

