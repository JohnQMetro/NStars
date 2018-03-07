unit sptfluxest;

{$mode delphi}

interface

uses
  Classes, SysUtils, DAMath, EstBasics, StarEstimator, Utilities, df_strings,
  starconstants, NewStar, unitdata;

const
  scolors = 'OBAFGK';
  KtoKs:Currency = 0.032;

  // V Magnitudes
  StarMagnitudes:array[1..7] of array[0..9] of Real = (
    (* O *) (-6   ,-5.9	,-5.8 ,-5.7 ,-5.5 ,-5.4	,-5.1 ,-4.8 ,-4.5 ,-4.2 ),
    (* B *) (-4   ,-3.1	,-1.7 ,-1.1 ,-1	  ,-0.9	,-0.5 ,-0.4 ,-0.2 , 0.7 ),
    (* A *) ( 1.11, 1.34, 1.48, 1.55, 1.76, 1.84, 1.89, 2.07, 2.29, 2.3 ),
    (* F *) ( 2.51, 2.79, 2.99, 3.08, 3.23, 3.4	, 3.7 , 3.87, 4.01, 4.15),
    (* G *) ( 4.45, 4.5 , 4.80, 4.88, 4.96, 5.01, 5.13, 5.18, 5.32, 5.55),
    (* K *) ( 5.76, 5.89, 6.19, 6.57, 7.04, 7.36, 7.80, 8.25, 8.47, 8.69),
    (* M *) ( 8.91, 9.69,10.23,11.10,12.8,14.3 ,16.62,17.81,18.84,19.36)
  );
  MMagnitudes:array[0..19] of Real = (8.91,9.20,9.69,9.97,10.30,10.7,11.14,12.19,
              12.8,13.57,14.3,15.51,16.62,17.07,17.81,18.42,18.84,19.14,19.36,19.75);
  LMagnitudes:array[0..5] of Real = (20,20.5,20.9,21.7,22.3,23.1);

  // B-V
  O9BmV:Currency = -0.318;
  StarBmV:array[2..6] of array[0..9] of Currency = (
    (* B *) (-0.307,-0.278,-0.210,-0.178,-0.165,-0.156,-0.140,-0.128,-0.109,-0.070),
    (* A *) (     0, 0.043, 0.074, 0.090, 0.140, 0.160, 0.170, 0.210, 0.250, 0.255),
    (* F *) ( 0.290, 0.334, 0.374, 0.389, 0.412, 0.438, 0.484 ,0.510, 0.530, 0.552),
    (* G *) ( 0.596, 0.604, 0.650, 0.662, 0.672, 0.680, 0.700, 0.720, 0.730, 0.775),
    (* K *) ( 0.816, 0.847, 0.893, 0.990, 1.100, 1.150, 1.240, 1.330, 1.356, 1.382)
  );
  MBmV: array[0..18] of Currency = (1.408,1.441,1.475,1.486,1.500,1.522,1.544,1.602,
              1.661,1.72,1.874,1.91,2.00,2.05,2.06,2.17,2.20,2.27,2.33);

  // Ks Magnitudes
  KsO95Mag:Currency = -3.12;
  KsStarMagn:array[2..6] of array[0..9] of Currency = (
    (* B *) (-3.04,-2.23,-1.10,-0.61,-0.55,-0.48,-0.14,-0.08, 0.05, 0.82),
    (* A *) ( 1.07, 1.24, 1.29, 1.32, 1.41, 1.44, 1.46, 1.54, 1.65, 1.66),
    (* F *) ( 1.76, 1.96, 2.07, 2.12, 2.21, 2.32, 2.52, 2.63, 2.72, 2.81),
    (* G *) ( 3.01, 3.04, 3.20, 3.27, 3.32, 3.35, 3.44, 3.47, 3.55, 3.69),
    (* K *) ( 3.81, 3.97, 4.04, 4.16, 4.31, 4.48, 4.66, 4.86, 4.98, 5.11)
  );
  KsMMagn:array[0..19] of Currency = (5.22,5.36,5.67,5.85,6.06,6.27,6.54,7.19,
              7.55,7.93,8.36,9.01,9.32,9.47,9.76,9.97,10.11,10.22,10.30,10.45);
  KsLMagn:array[0..5] of Currency = (10.55,10.8,10.9,11.3,11.4,11.82);

  // J Magnitudes
  JO95Mag:Currency = -3.35;
  JStarMagn:array[2..6] of array[0..9] of Currency = (
    (* B *) (-3.27,-2.43,-1.24,-0.73,-0.66,-0.59,-0.23,-0.16,-0.01, 0.79),
    (* A *) ( 1.07, 1.25, 1.32, 1.35, 1.47, 1.51, 1.54, 1.64, 1.77, 1.78),
    (* F *) ( 1.90, 2.13, 2.26, 2.32, 2.42, 2.55, 2.78, 2.90, 3.01, 3.11),
    (* G *) ( 3.34, 3.38, 3.57, 3.64, 3.70, 3.74, 3.84, 3.88, 3.97, 4.14),
    (* K *) ( 4.29, 4.47, 4.57, 4.76, 4.98, 5.18, 5.41, 5.65, 5.78, 5.92)
  );
  JMMagn:array[0..19] of Currency = (6.04,6.19,6.51,6.69,6.89,7.01,7.40,8.02,
              8.39,8.79,9.25,9.93,10.28,10.47,10.76,10.68,11.23,11.45,11.53,11.78);
  JLMagn:array[0..5] of Currency = (11.84,12.14,12.34,12.93,13.17,13.60);

  (* V-I. The latest (Sept 2017) values from Mamajek now always increase, without
  the earlier dip around M9 which made lookup ambiguous for ultracool dwarfs. *)
  LDwarfVmIc:array[0..5] of Currency = (4.82,4.91,5.05,5.29,5.57,6.28);
  RedDwarfVmIc:array[0..19] of Currency = (1.766,1.886,2.019,2.089,2.173,2.306,
                        2.420,2.680,2.831,3.073,3.277,3.664,4.13,4.31,4.45,4.56,
                        4.64,4.71,4.75,4.79);
  StarVmIc:array[2..6] of array[0..9] of Currency = (
    (* B *) (-0.355,-0.325,-0.230,-0.192,-0.176,-0.165,-0.145,-0.133,-0.108,-0.061),
    (* A *) ( 0.004, 0.044, 0.091, 0.108, 0.164, 0.186, 0.197, 0.242, 0.288, 0.294),
    (* F *) ( 0.334, 0.385, 0.432, 0.449, 0.476, 0.506, 0.553, 0.579, 0.599, 0.599),
    (* G *) ( 0.664, 0.672, 0.713, 0.722, 0.733, 0.738, 0.758, 0.766, 0.786, 0.818),
    (* K *) ( 0.853, 0.879, 0.929, 1.025, 1.190, 1.272, 1.420, 1.565, 1.632, 1.699)
  );
  VmIcO95V:Currency = -0.361;

  (* V-Rc from Mamajek. B is incomplete (so I drop it). Late M has a dip, so
  the array goes down to M6 only. *)
  StarVmRc:array[3..6] of array[0..9] of Currency = (
    (* A *) (0.001,0.019,0.042,0.05 ,0.078,0.089,0.094,0.117,0.14 ,0.143),
    (* F *) (0.166,0.19 ,0.213,0.222,0.236,0.252,0.276,0.29 ,0.3  ,0.312),
    (* G *) (0.336,0.34 ,0.363,0.368,0.374,0.377,0.388,0.393,0.404,0.423),
    (* K *) (0.443,0.457,0.487,0.544,0.64 ,0.685,0.759,0.82 ,0.843,0.866)
  );
  RedDwarfVmRc:array[0..12] of Currency = (0.889,0.924,0.959,0.978,1.001,1.041,
                          1.079,1.178,1.241,1.345,1.446,1.656,1.95);
  VmRcB95V:Currency = -0.017;

(* B-V for Pre-Main-Sequence Stars, from Peault and Mamajek 2013. *)
PMSStarBmV:array[4..6] of array[0..9] of Currency = (
    (* F *) ( 0.28, 0.34, 0.38, 0.41, 0.43, 0.47, 0.50, 0.53, 0.55, 0.56),
    (* G *) ( 0.57, 0.59, 0.60, 0.63, 0.66, 0.70, 0.74, 0.77, 0.79, 0.80),
    (* K *) ( 0.82, 0.86, 0.93, 1.02, 1.11, 1.18, 1.24, 1.28, 1.32, 1.37)
);
PMSRedDwarfBmV:array[0..5] of Currency = (1.41,1.45,1.46,1.47,1.53,1.65);

(* V-Ic for Pre-Main-Sequence Stars, from Peault and Mamajek 2013. *)
PMSStarVmIc:array[4..6] of array[0..9] of Currency = (
    (* F *) ( 0.34, 0.39, 0.43, 0.45, 0.48, 0.51, 0.55, 0.58, 0.60, 0.62),
    (* G *) ( 0.66, 0.67, 0.71, 0.72, 0.73, 0.76, 0.79, 0.83, 0.87, 0.91),
    (* K *) ( 0.93, 0.96, 1.01, 1.12, 1.27, 1.44, 1.57, 1.66, 1.74, 1.83)
);
PMSRedDwarfVmIc:array[0..5] of Currency = (1.95,2.11,2.28,2.48,2.78,3.31);

(* V-Ks for Pre-Main-Sequence Stars, from Peault and Mamajek 2013. *)
PMSStarVmKs:array[4..7] of array[0..9] of Currency = (
  (* F *) ( 0.73, 0.89, 0.99, 1.01, 1.05, 1.14, 1.25, 1.31, 1.34, 1.35),
  (* G *) ( 1.37, 1.42, 1.49, 1.58, 1.68, 1.77, 1.86, 1.95, 2.02, 2.10),
  (* K *) ( 2.19, 2.32, 2.49, 2.75, 3.06, 3.35, 3.54, 3.62, 3.67, 3.77),
  (* M *) ( 3.96, 4.33, 4.50, 4.78, 5.23, 6.08, 7.38, 8.47, 9.28, 9.80)
);



// adjust an absolute magnitde for a change in parallax
function NewAbsVisMag(oldp,newp:Real; oldav:Real):Real;
// luminosity / magnitude methods
procedure MagnitudeSetup;
function LookupExpectedLuminosity(scolor:Char; subrange:Real; out expLum:Real):Boolean;
function LookupExpectedAbsVMag(scolor:Char; subrange:Real; out expMag:Real):Boolean;
function MagnitudeToType(avm:Real; out spectra:string):Boolean;
function LuminosityToType(luminosity:Real; out spectra:string):Boolean;
// crude photometric parallax tool
function PhotoParallax(instype:string; inMag:Real; out distpllx:string):Boolean;
// absolute visual magnitude to parallax
function MagsToParallax(inMag,AbsMag:Real; out distpllx:string):Boolean;
// V minus Ks
procedure VminKsSetup;
function VminKsLookup(inval:Currency; KtKs:Boolean; out spectra:string):Boolean;
function VminKsDual(const inv_min,inv_max:Currency; KtKs:Boolean; out spectra:string):Boolean;
function VmK_MagnitudeEst(const vmkin:Real; out Mv_est:Real):Boolean;
procedure PMS_VmKsSetup;
function PMS_VminKsLookup(inval:Currency; out spectra:string):Boolean;
function PMS_VminKsDual(const inv_min,inv_max:Currency; out spectra:string):Boolean;
// BminusV
procedure BminVSetup;
function BminVLookup(const inval:Currency):string;
function BminVDual(const inv_min,inv_max:Currency):string;
procedure PMS_BminVSetup;
function PMS_BminVLookup(const inval:Currency):string;
function PMS_BminVDual(const inv_min,inv_max:Currency):string;
// Ks Magnitude
procedure MKsSetup;
function MKsLookup(inval:Currency; KtKs:Boolean; out resspec:string):Boolean;
function MksLookupDual(inval_min,inval_max:Currency; KtKs:Boolean; out resspec:string):Boolean;
function MagFromKs(scolor:Char; subrange:Real; out expMk:Currency):Boolean;
// V minus Ic
procedure VminISetup;
function VminILookup(inval:Currency; out spectra:string):Boolean;
procedure PMS_VminISetup;
function PMS_VminILookup(inval:Currency; out spectra:string):Boolean;
// V minus Rc
procedure VminRSetup;
function VminRLookup(inval:Currency; out spectra:string):Boolean;

// helper functions for StarSplit
function MakeGenericDwarf(ultradim:Boolean):EstimationParser;
function MakeGenericWD():EstimationParser;
function OkStar(var instar:EstimationParser; out newstar:EstimationParser):Boolean;
// treats the input as a binary, using expected luminosity, attempts to split the star
function StarSplit(var instar:EstimationParser; out newstar:EstimationParser):Boolean;
// similar to the above, but used a separate second visual magnitude
function StarSplitX(var primary:EstimationParser; secmag,secpllx:Real;
    out newstar:EstimationParser):Boolean;

function StarSplitGeneral(var instar:StarInfo; pllx_mas:Double; out newstar:EstimationParser):Boolean;
// producing test output for the Alonso method of generating BC
procedure AlonsoTest;

var
  // Mv (Luminosity) arrays
  MvBright:array[1..6] of array[0..9] of Real;
  MvRedDwarf:array[0..20] of Real;
  MvL:array[0..5] of Real;

  // V-K arrays
  VminKBright:array[2..6] of array[0..9] of Currency;
  VminKs_RD:array[0..20] of Currency;
  VminKs_L:array[0..5] of Currency;
  PMS_VmKsStar:array[4..7] of array[0..9] of Currency;

  // B-V arrays
  BmVBright:array[2..6] of array[0..9] of Currency;
  BmVRedDwarf:array[0..18] of Currency;
  PMS_BmVBright:array[4..6] of array[0..9] of Currency;
  PMS_BmVRedDwarf:array[0..5] of Currency;

  // Ks Magnitude arrays
  MKsBright:array[2..6] of array[0..9] of Currency;
  MKsRedDwarf:array[0..20] of Currency;
  MKsL:array[0..5] of Currency;

  // V-I arrays
  VminIBright:array[2..6] of array[0..9] of Currency;
  VminI_RD:array[0..19] of Currency;
  VminI_L:array[0..5] of Currency;
  PMS_VmIcBright:array[4..6] of array[0..9] of Currency;
  PMS_VmIcRedDwarf:array[0..5] of Currency;

  // V-Rc arrays
  VminRBright:array[3..6] of array[0..9] of Currency;
  VminRc_RD:array[0..12] of Currency;

  // temperature arrays
  TempBoundBright:array[2..6] of array[0..9] of Real;
  TempBoundRedDwarf:array[0..20] of Real;
  TempBoundL:array[0..5] of Real;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

implementation

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// adjust an absolute magnitde for a change in parallax
function NewAbsVisMag(oldp,newp:Real; oldav:Real):Real;
var intval1,intval2:Real;
begin
  Assert(oldp>=0);
  Assert(newp>=0);
  // calculating the luminosity difference
  intval1  := oldp/newp;
  intval1 := Sqr(intval1);
  // calculating the magnitude difference
  intval2 := 2.5*Log10(intval1);
  // the final result
  Result := oldav - intval2;
end;
//--------------------------------------------------
procedure MagnitudeSetup;
var colordex1,colordex2:Integer;
    prev,mid:Currency;
begin
  // calculating values for O to K
  for colordex1 := 1 to 6 do begin
    for colordex2 := 0 to 9 do begin
      // each value is a *start* (between the table value and the one before it)
      if (colordex1 = 1) and (colordex2 = 0) then prev := -6.1
      else if (colordex2 = 0) then prev := StarMagnitudes[colordex1-1][9]
      else prev := StarMagnitudes[colordex1][colordex2-1];
      mid := StarMagnitudes[colordex1][colordex2];
      // producing that value
      MvBright[colordex1][colordex2] := (prev+mid)/2;
    end;
  end;
  // calculating values for M
  MvRedDwarf[0] := (StarMagnitudes[6][9]+ MMagnitudes[0])/2;
  for colordex2 := 1 to 19 do begin
    MvRedDwarf[colordex2] := (MMagnitudes[colordex2-1]+MMagnitudes[colordex2])/2;
  end;
  MvRedDwarf[20] := (MMagnitudes[19]+LMagnitudes[0])/2;
  // calculating values for L
  MvL[0] := (MMagnitudes[19]+ LMagnitudes[0])/2;
  for colordex2 := 1 to 5 do begin
    MvL[colordex2] := (LMagnitudes[colordex2-1]+LMagnitudes[colordex2])/2;
  end;
end;
//--------------------------------------------------
// related
function LookupExpectedLuminosity(scolor:Char; subrange:Real; out expLum:Real):Boolean;
var magout:Real;
begin
  Result := LookupExpectedAbsVMag(scolor,subrange,magout);
  if (not Result) then Exit;
  expLum := AbsVMagToVLum(magout);
end;
//---------------------------------------------------------------
function LookupExpectedAbsVMag(scolor:Char; subrange:Real; out expMag:Real):Boolean;
var cpos,sbrl,sbrh:Integer;
    mlow,mhigh,magval:Real;
    ism:Boolean;
begin
  Result := False;
  cpos := AnsiPos(scolor,scolors);
  if (0 = cpos) and (scolor<>'M') and (scolor<>'L') then Exit;
  if (subrange < 0) or(subrange >=10) then Exit;
  if (scolor='L') and (subrange >= 5) then Exit;
  // l or m
  ism := (scolor = 'M');
  // we go ahead...
  if ism then subrange := subrange*2;
  sbrl := Floor(subrange);
  Result := True;
  // direct lookup
  if Frac(subrange) = 0 then begin
    if cpos <> 0 then magval := StarMagnitudes[cpos][sbrl]
    else if ism then magval := MMagnitudes[sbrl]
    else magval := LMagnitudes[sbrl];
  end
  else begin
    sbrh := Ceil(subrange);
    (* interpolating magnitudes is a a bit more accurate than interpolating luminosities. *)
    // low
    if cpos <> 0 then mlow := StarMagnitudes[cpos][sbrl]
    else if ism then mlow := MMagnitudes[sbrl]
    else mlow := LMagnitudes[sbrl];
    // high
    if (cpos <> 0) and (sbrh <> 10) then mhigh := StarMagnitudes[cpos][sbrh]
    else if (cpos <> 0) and (sbrh = 10) then mhigh := StarMagnitudes[cpos+1][0]
    else if ism and (sbrh = 20) then mhigh := LMagnitudes[0]
    else if ism then mhigh := MMagnitudes[sbrh]
    else mhigh := LMagnitudes[sbrh];
    // interpolating
    magval := mlow + Frac(subrange)*(mhigh-mlow);
  end;
  expMag := magval;
end;
//---------------------------------------------------------------
function MagnitudeToType(avm:Real; out spectra:string):Boolean;
var oindex,mindex,mainx:Integer;
    foundx,isodd:Boolean;
begin
  Result := True;
  foundx := False;
  spectra := '';
  // searching the L Array
  if avm >= MvL[0] then begin
    // searching the loop
    for mindex := 0 to 4 do begin // we only go up to L4
      if (MvL[mindex]<=avm) and (avm<MvL[mindex+1]) then begin
        foundx := True;
        Break;
      end;
    end;
    // after the loop
    if (not foundx) then Result := False
    else begin
      // setting the L spectra
      spectra := 'L' + IntToStr(mindex) + 'V';
    end;
  end
  // searching the Red Dwarf Array
  else if avm >= MvRedDwarf[0] then begin
    // searching the loop
    for mindex := 0 to 19 do begin
      if (MvRedDwarf[mindex]<=avm) and (avm<MvRedDwarf[mindex+1]) then begin
        foundx := True;
        Break;
      end;
    end;
    // after the loop
    if (not foundx) then Result := False
    else begin
      // setting the M spectra
      isodd := Odd(mindex);
      mainx := Trunc(mindex/2);
      spectra := 'M' + IntToStr(mainx);
      if isodd then spectra += '.5';
      spectra += 'V';
    end;
  end
  // searching the O to K arrays (in reverse)
  else begin
    for oindex := 6 downto 1 do begin
      if avm >= MvBright[oindex][0] then begin
        for mindex := 0 to 8 do begin
          if (MvBright[oindex][mindex]<=avm) and (avm<MvBright[oindex][mindex+1]) then begin
            foundx := True;
            Break;
          end;
        end;
        // setting the spectral string afterwards
        if foundx = False then mindex := 9;
        foundx := True;
        spectra := scolors[oindex] + IntToStr(mindex) + 'V';
        Break;
      end;
    end;
    // if we get here, without finding a value, then the star is too bright
    if (not foundx) then Result := False;
  end;
end;
//----------------------------------------------------------
function LuminosityToType(luminosity:Real; out spectra:string):Boolean;
var avm:Real;
begin
  avm := VLumToAbsVMag(luminosity);
  Result := MagnitudeToType(avm,spectra);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// crude photometric parallax tool
function PhotoParallax(instype:string; inMag:Real; out distpllx:string):Boolean;
var scolor:Char;   subrange:Real;
    extrbf:string;
    ppos,sc:Integer;
    expabsmag:Real;
begin
  Result := False;
  // parsing the input spectral type
  if Length(instype)<2 then Exit;
  scolor := instype[1];
  ppos := FindFirstNotOf(instype,'0987654321.',2);
  if ppos = 2 then Exit;
  if ppos < 0 then extrbf := Copy(instype,2,Length(instype)-1)
  else extrbf := Copy(instype,2,ppos-2);
  Val(extrbf,subrange,sc);
  if sc<>0 then Exit;
  if subrange >= 10 then Exit;
  // getting expected absolute magnitude
  if scolor = 'M' then begin
    subrange := subrange*2;
    expabsmag := InterLookupReal(subrange,MMagnitudes,LMagnitudes[0]);
  end else if scolor = 'L' then begin
    if subrange >=5 then Exit;
    expabsmag := InterLookupReal(subrange,LMagnitudes,LMagnitudes[5]);
  end else begin
    ppos := AnsiPos(scolor,scolors);
    if ppos<1 then Exit;
    expabsmag := InterLookupReal(subrange,StarMagnitudes[ppos],StarMagnitudes[ppos+1][0]);
  end;
  // calculating the difference
  Result := MagsToParallax(inMag,expabsmag,distpllx);
end;
//-----------------------------------------------------------
// absolute visual magnitude to parallax
function MagsToParallax(inMag,AbsMag:Real; out distpllx:string):Boolean;
var diffmag,interm1,parallax:Real;
begin
  // calculating the difference
  diffmag := inMag - AbsMag;
  // calculating the parallax
  interm1 := (diffmag/5)+1;
  interm1 := exp10(interm1);
  parallax := 1000.0 / interm1;
  // producing the results...
  Result := True;
  distpllx := Trim(FloatToStrF(parallax,ffFixed,5,1)) + ' mas';
end;
//-----------------------------------------------------------
// V minus K
//-----------------------
procedure VminKsSetup;
var coldex:Integer;
    precval:Currency;
begin
  // setting up the Bright star settings (B to K)
  for coldex := 2 to 6 do begin
    if coldex = 2 then precval := VmKO95V
    else precval := StarVmKs[coldex-1][9];
    InitCurrArrayA(precval,StarVmKs[coldex],VminKBright[coldex]);
  end;
  // M (red dwarf) array setup
  InitCurrArrayB(StarVmKs[6][9],LDwarfVmKs[0],RedDwarfVmKs,VminKs_RD);
  // L Dwarf (very red dwarf/brown dwarf) array setup
  InitCurrArrayA(RedDwarfVmKs[19],LDwarfVmKs,VminKs_L);
end;
//----------------------------
function VminKsLookup(inval:Currency; KtKs:Boolean; out spectra:string):Boolean;
var colordex,lookdex,mainx:Integer;
    isodd:Boolean;
begin
  if KtKs then inval -= KtoKs;
  // quick rejection tests
  Result := False;
  if inval < VminKBright[2][0] then Exit;
  if inval >= VminKs_L[5] then Exit;
  Result := True;
  // start with L dwarfs
  if inval >= VminKs_L[0] then begin
    lookdex := CurrArrLookupLT(inval,VminKs_L);
    Assert(lookdex>=0);
    Assert(lookdex<5);
    spectra := 'L' + IntToStr(lookdex);
  end
  // then M dwarfs
  else if inval >= VminKs_RD[0] then begin
    lookdex := CurrArrLookupLT(inval,VminKs_RD);
    Assert(lookdex>=0);
    // setting the M spectra
    isodd := Odd(lookdex);
    mainx := Trunc(lookdex/2);
    spectra := 'M' + IntToStr(mainx);
    if isodd then spectra += '.5';
  end
  // then bright (B to K) stars
  else begin
    for colordex := 6 downto 2 do begin
      if inval < VminKBright[colordex][0] then Continue;
      lookdex := CurrArrLookupLT(inval,VminKBright[colordex]);
      Break;
    end;
    spectra := scolors[colordex] + IntToStr(lookdex);
  end;
  // finishing off
  spectra += 'V';
end;
//-------------------------------------------------------------
function VminKsDual(const inv_min,inv_max:Currency; KtKs:Boolean; out spectra:string):Boolean;
var res1,res2:Boolean;
    spec1,spec2:string;
begin
  Result := False;
  // making the min and max spectrae stimates
  res1 := VminKsLookup(inv_min,KtKs,spec1);
  if inv_max > 90 then res2 := False
  else res2 := VminKsLookup(inv_max,KtKs,spec2);
  // handling the results
  if not (res1 or res2) then Exit;
  Result := True;
  // two results
  if res1 and res2 then begin
    spectra := spec1;
    if spec1 <> spec2 then spectra += ' to ' + spec2;
  end
  else if res1 then spectra := spec1
  else spectra := spec2;
end;
//-------------------------------------------------------------
(* Estimates absolute visual magnitude, based on V-Ks, for late K3 to L5
using Eric Mamjek's table as the Mv and V-Ks source. Uses 2 separate polynomial
fits, one for K3 to M5, the other for M5.5 to L5. The fit is worst at M5.5,
M4.5, and L3. *)
function VmK_MagnitudeEst(const vmkin:Real; out Mv_est:Real):Boolean;
const
  coffe:array[0..3] of Real = (7.33558,-2.376939, 1.029929,-0.07257);
  coffl:array[0..3] of Real = (3.87924, 1.960727,-0.03999 , 0.001421);
begin
  Result := False;
  if vmkin < 2.3 then Exit;
  if vmkin > 11.4 then Exit;
  // calculating using the smaller V-K (K3 to M5)
  if vmkin < 6.22 then Mv_est := PolEval(vmkin,coffe,4)
  // bigger V-K (M5.5 to L5)
  else Mv_est := PolEval(vmkin,coffl,4);
  Result := True;
end;
//----------------------------------------------------------------
procedure PMS_VmKsSetup;
var coldex:Integer;
    precval:Currency;
begin
  // setting up the Bright star settings (B to K)
  for coldex := 4 to 7 do begin
    if coldex = 4 then precval := 0.638
    else precval := PMSStarVmKs[coldex-1][9];
    InitCurrArrayA(precval,PMSStarVmKs[coldex],PMS_VmKsStar[coldex]);
  end;
end;
//------------------------------------------------
function PMS_VminKsLookup(inval:Currency; out spectra:string):Boolean;
var colordex,lookdex:Integer;
begin
  // quick rejection tests
  Result := False;
  if inval < PMS_VmKsStar[4][0] then Exit;
  if inval >= PMS_VmKsStar[7][9] then Exit;
  // there is a single array for F to M
  for colordex := 7 downto 4 do begin
      if inval < PMS_VmKsStar[colordex][0] then Continue;
      lookdex := CurrArrLookupLT(inval,PMS_VmKsStar[colordex]);
      Break;
  end;
  if colordex = 7 then spectra := 'M'
  else spectra := scolors[colordex];
  spectra += IntToStr(lookdex);
  Result := True;
end;
//--------------------------------------------
function PMS_VminKsDual(const inv_min,inv_max:Currency; out spectra:string):Boolean;
var res1,res2:Boolean;
    spec1,spec2:string;
begin
  Result := False;
  // making the min and max spectra estimates
  res1 := PMS_VminKsLookup(inv_min,spec1);
  if inv_max > 90 then res2 := False
  else res2 := PMS_VminKsLookup(inv_max,spec2);
  // handling the results
  if not (res1 or res2) then Exit;
  Result := True;
  // two results
  if res1 and res2 then begin
    spectra := spec1;
    if spec1 <> spec2 then spectra += ' to ' + spec2;
  end
  else if res1 then spectra := spec1
  else spectra := spec2;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// BminusV
//---------------------------
procedure BminVSetup;
var colordex1,colordex2:Integer;
    prev,mid:Currency;
begin
  // calculating values for B to K
  for colordex1 := 2 to 6 do begin
    for colordex2 := 0 to 9 do begin
      // each value is a *start* (between the table value and the one before it)
      if (colordex1 = 2) and (colordex2 = 0) then prev := O9BmV
      else if (colordex2 = 0) then prev := StarBmV[colordex1-1][9]
      else prev := StarBmV[colordex1][colordex2-1];
      mid := StarBmV[colordex1][colordex2];
      // producing that value
      BmVBright[colordex1][colordex2] := (prev+mid)/2;
    end;
  end;
  // calculating values for M
  BmVRedDwarf[0] := (StarBmV[6][9]+ MBmV[0])/2;
  for colordex2 := 1 to 18 do begin
    BmVRedDwarf[colordex2] := (MBmV[colordex2-1]+MBmV[colordex2])/2;
  end;
  // done
end;
//---------------------------
function BminVLookup(const inval:Currency):string;
var foundx,isodd:Boolean;
    mindex,mainx,oindex:Integer;
begin
  foundx := False;
  // searching the Red Dwarf Array
  if inval >= BmVRedDwarf[0] then begin
    if inval > MBmV[High(MBmV)] then Result := 'Very red (>M9)'
    else begin
      mindex := CurrArrLookupLT(inval,BmVRedDwarf);
      isodd := Odd(mindex);
      mainx := Trunc(mindex/2);
      Result := 'M' + IntToStr(mainx);
      if isodd then Result += '.5';
      Result += 'V';
    end;
  end
  // searching the B to K arrays (in reverse)
  else begin
    for oindex := 6 downto 2 do begin
      if inval < BmVBright[oindex][0] then Continue;
      mindex := CurrArrLookupLT(inval,BmVBright[oindex]);
      foundx := True;
      Break;
    end;
    if foundx then Result := scolors[oindex] + IntToStr(mindex) + 'V'
    else Result := 'Very blue (<B0)';
  end;
end;
//---------------------------
function BminVDual(const inv_min,inv_max:Currency):string;
var res1,res0:string;
begin
  res1 := BminVLookup(inv_min);
  if (inv_min<>inv_max) and (inv_max<99) then begin
    res0 := BminVLookup(inv_max);
    if res0 <> res1 then Result := res1 + ' to ' + res0
    else Result := res1;
  end
  else Result := res1;
end;
//------------------------------------------------------------
procedure PMS_BminVSetup;
var coldex:Integer;
    prev:Currency;
begin
  // calculating values for F to K
  for coldex := 4 to 6 do begin
    if coldex = 4 then prev := 0.26
    else prev := PMSStarBmV[coldex-1][9];
    InitCurrArrayA(prev,PMSStarBmV[coldex],PMS_BmVBright[coldex]);
  end;
  // calculating values for M
  InitCurrArrayA(PMSStarBmV[6][9],PMSRedDwarfBmV,PMS_BmVRedDwarf);
  // done
end;
//------------------------------------------------------------
function PMS_BminVLookup(const inval:Currency):string;
var foundx:Boolean;
    mindex,oindex:Integer;
begin
  foundx := False;
  // searching the Red Dwarf Array
  if inval >= PMS_BmVRedDwarf[0] then begin
    for mindex := 0 to 4 do begin
      if (PMS_BmVRedDwarf[mindex]<=inval) and (inval<PMS_BmVRedDwarf[mindex+1]) then begin
        foundx := True;
        Break;
      end;
    end;
    if (not foundx) then Result := 'Red (M5+)'
    else Result := 'M' + IntToStr(mindex);
  end
  // searching the F to K arrays (in reverse)
  else begin
    for oindex := 6 downto 4 do begin
      if inval < PMS_BmVBright[oindex][0] then Continue;
      mindex := CurrArrLookupLT(inval,PMS_BmVBright[oindex]);
      foundx := True;
      Break;
    end;
    if foundx then Result := scolors[oindex] + IntToStr(mindex)
    else Result := 'Blue (< F0)';
  end;
end;
//------------------------------------------------------------
function PMS_BminVDual(const inv_min,inv_max:Currency):string;
var res1,res0:string;
begin
  res1 := PMS_BminVLookup(inv_min);
  if (inv_min<>inv_max) and (inv_max<99) then begin
    res0 := PMS_BminVLookup(inv_max);
    if res0 <> res1 then Result := res1 + ' to ' + res0
    else Result := res1;
  end
  else Result := res1;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Ks Magnitude
//------------------------------------
procedure MKsSetup;
var colordex1:Integer;
    prev:Currency;
begin
  // calculating values for B to K
  for colordex1 := 2 to 6 do begin
    if colordex1 = 2 then prev := KsO95Mag
    else prev := KsStarMagn[colordex1-1][9];
    InitCurrArrayA(prev,KsStarMagn[colordex1],MKsBright[colordex1]);
  end;
  // calculating values for M
  prev := KsStarMagn[6][9];
  InitCurrArrayB(prev,KsLMagn[0],KsMMagn,MKsRedDwarf);
  // calculating values for L
  prev := KsMMagn[19];
  InitCurrArrayA(prev,KsLMagn,MKsL);
end;
//----------------------------------
function MKsLookup(inval:Currency; KtKs:Boolean; out resspec:string):Boolean;
var oindex,mindex,mainx:Integer;
    foundx,isodd:Boolean;
begin
  if KtKs then inval -= KtoKs;
  // initial stuff
  Result := False;
  if inval >= MksL[5] then Exit;
  if inval < MKsBright[2][0] then Exit;
  foundx := False;
  resspec := '';

  // searching the L Array
  if inval >= MKsL[0] then begin
    // searching the loop
    for mindex := 0 to 4 do begin // we only go up to L4
      if (MKsL[mindex]<=inval) and (inval<MKsL[mindex+1]) then begin
        foundx := True;
        Break;
      end;
    end;
    // after the loop
    if (not foundx) then Exit
    // setting the L spectra
    else resspec := 'L' + IntToStr(mindex);
  end
  // searching the Red Dwarf Array
  else if inval >= MKsRedDwarf[0] then begin
    // searching the loop
    for mindex := 0 to 19 do begin
      if (MKsRedDwarf[mindex]<=inval) and (inval<MKsRedDwarf[mindex+1]) then begin
        foundx := True;
        Break;
      end;
    end;
    // after the loop
    if (not foundx) then Exit
    else begin
      // setting the M spectra
      isodd := Odd(mindex);
      mainx := Trunc(mindex/2);
      resspec := 'M' + IntToStr(mainx);
      if isodd then resspec += '.5';
    end;
  end
  // searching the O to K arrays (in reverse)
  else begin
    for oindex := 6 downto 2 do begin
      if inval >= MKsBright[oindex][0] then begin
        for mindex := 0 to 8 do begin
          if (MKsBright[oindex][mindex]<=inval) and (inval<MKsBright[oindex][mindex+1]) then begin
            foundx := True;
            Break;
          end;
        end;
        // setting the spectral string afterwards
        if foundx = False then mindex := 9;
        foundx := True;
        resspec:= scolors[oindex] + IntToStr(mindex);
        Break;
      end;
    end;
    // if we get here, without finding a value, then the star is too bright
    if (not foundx) then Result := False;
  end;
  // done
  resspec += 'V';
  Result := True;
end;
//---------------------------------------------------------------
function MksLookupDual(inval_min,inval_max:Currency; KtKs:Boolean; out resspec:string):Boolean;
var res1,res2:Boolean;
    spec1,spec2:string;
begin
  Result := False;
  // making the min and max spectrae stimates
  res1 := MKsLookup(inval_min,KtKs,spec1);
  if inval_max > 90 then res2 := False
  else res2 := MKsLookup(inval_max,KtKs,spec2);
  // handling the results
  if not (res1 or res2) then Exit;
  Result := True;
  // two results
  if res1 and res2 then begin
    resspec := spec1;
    if spec1 <> spec2 then resspec += ' to ' + spec2;
  end
  else if res1 then resspec := spec1
  else resspec := spec2;
end;

//---------------------------------------------------------------
function MagFromKs(scolor:Char; subrange:Real; out expMk:Currency):Boolean;
var coldex:Integer;
    limitval:Currency;
    dsubr:Real;
const L6Mk:Currency = 12.27;
begin
  Result := False;
  if subrange >= 10 then Exit;
  // L dwarfs
  if scolor = 'L' then begin
    if subrange >= 5 then Exit;
    Result := True;
    expMk := InterLookupCurr(subrange,KsLMagn,L6Mk);
  end
  // M dwarfs
  else if scolor = 'M' then begin
    dsubr := 2*subrange;
    Result := True;
    expMk := InterLookupCurr(dsubr,KsMMagn,KsLMagn[0]);
  end
  // B thru K
  else begin
    coldex := AnsiPos(scolor,scolors);
    if coldex < 2 then Exit;
    Result := True;
    if coldex = 6 then limitval := KsMMagn[0]
    else limitval := KsStarMagn[coldex+1][0];
    expMk := InterLookupCurr(subrange,KsStarMagn[coldex],limitval);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//-----------------------
procedure VminISetup;
var coldex:Integer;
    precval:Currency;
begin
  // setting up the Bright star settings (B to K)
  for coldex := 2 to 6 do begin
    if coldex = 2 then precval := VmIcO95V
    else precval := StarVmIc[coldex-1][9];
    InitCurrArrayA(precval,StarVmIc[coldex],VminIBright[coldex]);
  end;
  // M (red dwarf) array setup
  InitCurrArrayA(StarVmIc[6][9],RedDwarfVmIc,VminI_RD);
  // L Dwarf (very red dwarf/brown dwarf) array setup
  InitCurrArrayA(RedDwarfVmIc[19],LDwarfVmIc,VminI_L);
end;
//----------------------------
function VminILookup(inval:Currency; out spectra:string):Boolean;
var colordex,lookdex,mainx:Integer;
    isodd:Boolean;
begin
  // quick rejection tests
  Result := False;
  if inval < VminIBright[2][0] then Exit;
  if inval >= VminI_L[5] then Exit;
  Result := True;
  // start with L dwarfs
  if inval >= VminI_L[0] then begin
    lookdex := CurrArrLookupLT(inval,VminI_L);
    Assert(lookdex>=0);
    Assert(lookdex<5);
    spectra := 'L' + IntToStr(lookdex);
  end
  // then M dwarfs
  else if inval >= VminI_RD[0] then begin
    lookdex := CurrArrLookupLT(inval,VminI_RD);
    Assert(lookdex>=0);
    // setting the M spectra
    isodd := Odd(lookdex);
    mainx := Trunc(lookdex/2);
    spectra := 'M' + IntToStr(mainx);
    if isodd then spectra += '.5';
  end
  // then bright (B to K) stars
  else begin
    for colordex := 6 downto 2 do begin
      if inval < VminIBright[colordex][0] then Continue;
      lookdex := CurrArrLookupLT(inval,VminIBright[colordex]);
      Break;
    end;
    spectra := scolors[colordex] + IntToStr(lookdex);
  end;
  // finishing off
  spectra += 'V';
end;
//-----------------------------------------------------------
procedure PMS_VminISetup;
var coldex:Integer;
    precval:Currency;
begin
  // setting up the Bright star settings (B to K)
  for coldex := 4 to 6 do begin
    if coldex = 4 then precval := 0.294
    else precval := PMSStarVmIc[coldex-1][9];
    InitCurrArrayA(precval,PMSStarVmIc[coldex],PMS_VmIcBright[coldex]);
  end;
  // M (red dwarf) array setup
  InitCurrArrayA(PMSStarVmIc[6][9],PMSRedDwarfVmIc,PMS_VmIcRedDwarf);
end;
//---------------------------------------------
function PMS_VminILookup(inval:Currency; out spectra:string):Boolean;
var mindex,oindex:Integer;
begin
  Result := False;
  // searching the Red Dwarf Array
  if inval >= PMS_VmIcRedDwarf[0] then begin
    for mindex := 0 to 4 do begin
      if (PMS_VmIcRedDwarf[mindex]<=inval) and (inval<PMS_VmIcRedDwarf[mindex+1]) then begin
        Result := True;
        Break;
      end;
    end;
    if Result then spectra := 'M' + IntToStr(mindex);
  end
  // searching the F to K arrays (in reverse)
  else begin
    for oindex := 6 downto 4 do begin
      if inval < PMS_VmIcBright[oindex][0] then Continue;
      mindex := CurrArrLookupLT(inval,PMS_VmIcBright[oindex]);
      Result := True;
      Break;
    end;
    if Result then spectra:= scolors[oindex] + IntToStr(mindex);
  end;
end;
//-----------------------
procedure VminRSetup;
var coldex:Integer;
    precval:Currency;
begin
  // setting up the Bright star settings (A to K)
  for coldex := 3 to 6 do begin
    if coldex = 3 then precval := VmRcB95V
    else precval := StarVmRc[coldex-1][9];
    InitCurrArrayA(precval,StarVmRc[coldex],VminRBright[coldex]);
  end;
  // M (red dwarf) array setup
  InitCurrArrayA(StarVmRc[6][9],RedDwarfVmRc,VminRc_RD);
end;
//----------------------------
function VminRLookup(inval:Currency; out spectra:string):Boolean;
var colordex,lookdex,mainx:Integer;
    isodd:Boolean;
begin
  // quick rejection tests
  Result := False;
  if inval < VminRBright[3][0] then Exit;
  if inval >= VminRc_RD[High(VminRc_RD)] then Exit;
  Result := True;
  // then M dwarfs
  if inval >= VminRc_RD[0] then begin
    lookdex := CurrArrLookupLT(inval,VminRc_RD);
    Assert(lookdex>=0);
    // setting the M spectra
    isodd := Odd(lookdex);
    mainx := Trunc(lookdex/2);
    spectra := 'M' + IntToStr(mainx);
    if isodd then spectra += '.5';
  end
  // then bright (B to K) stars
  else begin
    for colordex := 6 downto 3 do begin
      if inval < VminRBright[colordex][0] then Continue;
      lookdex := CurrArrLookupLT(inval,VminRBright[colordex]);
      Break;
    end;
    spectra := scolors[colordex] + IntToStr(lookdex);
  end;
  // finishing off
  spectra += 'V';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function MakeGenericDwarf(ultradim:Boolean):EstimationParser;
const dspec1 = 'M5V??';
      dspec2 = 'M9V??';
begin
  Result := EstimationParser.Create();
  if ultradim then Result.SetBasic(MMagnitudes[18],dspec2,False)
  else Result.SetBasic(MMagnitudes[10],dspec1,False);
end;
//----------------------------------------
function MakeGenericWD():EstimationParser;
begin
  Result := EstimationParser.Create();
  Result.SetBasic(13.44,'DA7??',False);
end;
//----------------------------------------
function OkStar(var instar:EstimationParser; out newstar:EstimationParser):Boolean;
begin
  Assert(instar<>nil);
  Result := False;
  // if the spectral classification is not parsed, generic red dwarf
  if (not instar.SpectraOK) then begin
    newstar := MakeGenericDwarf(false);
    Exit;
  end;
  // the split functionality only works on V, if not V, we just make a generic red dwarf
  if instar.LuminosityClass <> lDwarf then begin
    newstar := MakeGenericDwarf(false);
    Exit;
  end;
  // otheriwse
  Result := True;
end;
//----------------------------------------
// treats the input as a binary, using expected luminosity, attempts to split the star
function StarSplit(var instar:EstimationParser; out newstar:EstimationParser):Boolean;
var lumcore,lumexp,lumdiff,lumdiv:Real;
    scolor:Char; subrx,newamag:Real;
    lookupok:Boolean;
    outspec,nsspec:string;
begin
  // startup and special checks
  Assert(instar <>nil);
  Result := False;
  if (not OkStar(instar,newstar)) then Exit;
  // if we get here, we try to get the excess luminosity
  scolor := instar.ColorLetter;
  subrx := instar.ColorSubrange;
  lumcore := instar.VisualLuminosity;
  lookupok := LookupExpectedLuminosity(scolor,subrx,lumexp);
  // if the lookup fails
  if not lookupok then begin
    newstar := MakeGenericDwarf(scolor='M');
    Exit;
  end;
  // if we get here, we start calculating the differences...
  lumdiff := lumcore - lumexp;
  lumdiv := lumcore / lumexp;
  // if the difference is too small... generic dwarf
  if lumdiff < 0.0000017 then begin
    newstar := MakeGenericDwarf(scolor='M');
    Exit;
  end;

  // the 'primary' must be the most luminous
  if lumdiv > 1.9 then lumdiv := 1.9;
  // here, we reduce the input star by lumdiv
  instar.DivideSplit(lumdiv,True);
  lookupok := LuminosityToType(lumdiff,outspec);
  // if trying to get a spectral type from leftover luminosity fails ... generic dwarf
  if not lookupok then begin
    newstar := MakeGenericDwarf(scolor='M');
    Exit;
  end;
  // finally... NOT a generic dwarf
  newamag := VLumToAbsVMag(lumdiff);
  newstar := EstimationParser.Create();
  nsspec := outspec + '?';
  newstar.SetBasic(newamag,nsspec,False);
  Result := True;
end;
//-----------------------------------------------------------------
// similar to the above, but uses a separate second visual magnitude
function StarSplitX(var primary:EstimationParser; secmag,secpllx:Real;
    out newstar:EstimationParser):Boolean;
var seclum,sec_absmag:Real;
    opok:Boolean;
    nsspec:string;
begin
  // startup and special checks
  Assert(primary <>nil);
  Result := False;
  if (not OkStar(primary,newstar)) then Exit;
  // the second star is defined by its absolute visual magnitude
  newstar := EstimationParser.Create();
  sec_absmag := MakeAbsoluteMagnitude(secmag,secpllx);
  opok := MagnitudeToType(sec_absmag,nsspec);
  // if we cannot look up as spectral type, we discard and make a generic red dwarf
  if (not opok) then begin
    FreeAndNil(newstar);
    newstar := MakeGenericDwarf(primary.ColorLetter='M');
  end
  // otherwise, we finish things off
  else begin
    nsspec += '?';
    newstar.SetBasic(sec_absmag,nsspec,False);
  end;
  //reducing the magnitude of the primary
  primary.SecondarySplit(secmag,secpllx,True);
  // final touches
  Result := True;
end;
//-------------------------------------------------------
// wraps the various star split functions into a more general method
function StarSplitGeneral(var instar:StarInfo; pllx_mas:Double; out newstar:EstimationParser):Boolean;
var sdata1:EstimationParser;
begin
  Result := False;
  newstar := nil;
  // inapplicable cases
  if instar = nil then Exit;
  if instar.MinPartCount < 2 then Exit;
  // starting
  sdata1 := instar.estimator;
  // case 1 : generic white dwarf binary
  if instar.Arity = WHITE_DWARF_BINARY then begin
    Result := True;
    newstar := MakeGenericWD();
    Exit;
  end;
  // case 2 : we have a secondary magnitude
  if instar.ValidSecondaryMagnitude then begin
     Result := StarSplitX(sdata1,instar.SecondaryMagnitude,pllx_mas,newstar);
  end
  // case 3: we do not have a secondary magnitude
  else Result := StarSplit(sdata1,newstar);
  sdata1.ChopJ();
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// producing test output for the Alonso method of generating BC
procedure AlonsoTest;
var alfile:TextFile;
    oindex,iindex:Integer;
    qspec,qline:string;
    cvmk:Real;
begin
  // starting
  AssignFile(alfile,'alonsotest.csv');
  Rewrite(alfile);
  // header
  qline := 'SpT,V-K,,,,,A 0.4,M 0.4,A 0.2,M 0.2,A 0.0,M 0.0,A-0.2,M-0.2,';
  qline += 'A-0.4,M-0.4,A-0.6,M-0.6,A-0.8,M-0.8,A-1.0,M-1.0,A-1.2,M-1.2,';
  qline += 'C 0.4,C 0.2,C 0.0,C-0.2,C-0.4,C-0.6,C-0.8,C-1.0,C-1.2,C Teff,';
  WriteLn(alfile,qline);

  // main data
  for oindex := 4 to 6 do begin
    for iindex := 0 to 9 do begin
      qspec := scolors[oindex] + IntToStr(iindex);
      cvmk := CurrToReal(StarVmKs[oindex][iindex]);
      qline := qspec + ',' + Trim(FloatToStrF(cvmk,ffFixed,6,3)) + ',,,,,';
      qline += AlonsoMultiStr(cvmk);
      WriteLn(alfile,qline);
    end;
  end;
  Flush(alfile);
  CloseFile(alfile);
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  MagnitudeSetup;
  VminKsSetup;
  BminVSetup;
  MKsSetup;
  VminISetup;
  VminRSetup;
  PMS_BminVSetup;
  PMS_VmKsSetup;
  PMS_VminISetup;
end.
