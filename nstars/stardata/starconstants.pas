unit starconstants;
(* used to consolidate things like star temperatures and bolometric corrections
into a single file *)

{$mode delphi}

interface

uses
  Classes, SysUtils, DAMath, Utilities;
//**************************************************************
const

(* Star Temperature Constants *)
HR_Let = 'OBAFGKMLTY';
(* http://www.pas.rochester.edu/~emamajek/EEM_dwarf_UBVIJHK_colors_Teff.txt *)
TempTable:array[1..6] of array[0..9] of Real = (
  (* O *) (60000,57000,54000,46000,43000,41500,39000,36500,34500,32500),
  (* B *) (31500,26000,20600,17000,16700,15700,14500,14000,12500,10700),
  (* A *) ( 9700, 9200, 8840, 8550, 8270, 8080, 8000, 7800, 7500, 7440),
  (* F *) ( 7220, 7030, 6810, 6720, 6640, 6510, 6340, 6240, 6170, 6040),
  (* G *) ( 5920, 5880, 5770, 5720, 5680, 5640, 5590, 5540, 5470, 5340),
  (* K *) ( 5280, 5170, 5040, 4840, 4600, 4410, 4200, 4070, 4000, 3940)
);
MTempTableM:array[0..19] of Real = (3870,3800,3700,3650,3550,3500,3410,3250,
                3200,3100,3030,3000,2850,2710,2650,2600,2500,2440,2400,2320);
//alternate m table with 'how to constrain your M dwarf values (to M6).
MTempTableAlt:array[0..19] of Real = (3810,3740,3675,3650,3595,3515,3450,3300,
                3230,3120,3040,2915,2810,2710,2650,2600,2500,2475,2450,2320);
LTempTable:array[0..9] of Real = (2250,2100,1960,1830,1700,1590,1490,1410,1350,1300);

// Giant star temperatures, pulled/interpolated from a variety of sources
  // (and rather vague, published tables vary by hundred of degrees)
  Giant_Let = 'FGKM';
  GiantTempTable:array[1..4] of array [0..9] of Real = (
      (* F *)  (7046,6925,6804,6621,6438,6255,6190,6070,5950,5875),
      (* G *)  (5805,5730,5550,5370,5190,5120,5050,5005,4960,4840),
      (* K *)  (4720,4630,4540,4285,4095,3980,3950,3915,3885,3850),
      (* M *)  (3820,3780,3710,3630,3560,3420,3250,3000,2900,2670)
     );

(* Pre-main-sequence TEff's from 'Intrinsic Colors, Temperatures and Bolometric
Corrections of Pre-Main Sequence Stars' (Pecault and Mamajek, 2013). Post M5
values are made up. *)
PMSTempTable:array[4..7] of array[0..9] of Real = (
  (* F *) ( 7280, 6990, 6710, 6660, 6590, 6420, 6250, 6140, 6100, 6090),
  (* G *) ( 6050, 5970, 5870, 5740, 5620, 5500, 5390, 5290, 5210, 5120),
  (* K *) ( 5030, 4920, 4760, 4550, 4330, 4140, 4020, 3970, 3940, 3880),
  (* M *) ( 3770, 3630, 3490, 3360, 3160, 2880, 2760, 2650, 2550, 2450)
);


// Bolometric corrections from Mamajek (except for made-up O0, O1, and O2).
BCorTable:array[1..6] of array[0..9] of Real = (
    (* O *) (-4.55,-4.40,-4.25,-4.05,-3.92,-3.77,-3.57,-3.380,-3.220,-3.092),
    (* B *) (-3.02,-2.61,-2.06,-1.58,-1.53,-1.35,-1.16,-1.070,-0.810,-0.42),
    (* A *) (-0.24,-0.15,-0.10,-0.06,-0.04,-0.03,-0.02, 0.000, 0.000, 0.00),
    (* F *) (-0.01,-0.01,-0.02,-0.03,-0.04,-0.04,-0.05,-0.060,-0.070,-0.08),
    (* G *) (-0.09,-0.10,-0.11,-0.12,-0.13,-0.13,-0.15,-0.160,-0.170,-0.21),
    (* K *) (-0.22,-0.26,-0.29,-0.41,-0.55,-0.68,-0.81,-0.97 ,-1.03 ,-1.10)
);
MBCorM:array[0..19] of Real = (-1.16,-1.38,-1.44,-1.57,-1.65,-1.76,-1.97,-2.27,
     -2.59, -3.05,-3.28,-3.80,-4.36,-4.6,-5.06,-5.46,-5.70,-5.80,-5.90,-6.13);
MBCorAlt:array[0..19] of Real = (-1.266,-1.373,-1.481,-1.526,-1.626,-1.786,-1.933,
     -2.285,-2.496,-2.826,-3.078,-3.579,-4.306,-4.58,-5.06,-5.46,-5.70,-5.80,-5.73,-5.80);

// k flux bolometric corrections for late M and L
MK_BCor:array[7..9] of Real= (2.915,3,3.125);
LK_BCor:array[0..8] of Real= (3.225,3.225,3.35,3.275,3.325,3.375,3.285,3.31,3.35);

(* Pre-main-sequence BCv's from 'Intrinsic Colors, Temperatures and Bolometric
Corrections of Pre-Main Sequence Stars' (Pecault and Mamajek, 2013). Post M5
values are made up. *)
PMSBCvTable:array[4..7] of array[0..9] of Real = (
  (* F *) ( 0.01, 0.00,-0.01,-0.01,-0.01,-0.02,-0.04,-0.05,-0.06,-0.06),
  (* G *) (-0.06,-0.07,-0.09,-0.11,-0.14,-0.17,-0.20,-0.23,-0.26,-0.29),
  (* K *) (-0.33,-0.38,-0.46,-0.60,-0.77,-0.95,-1.08,-1.14,-1.17,-1.24),
  (* M *) (-1.38,-1.58,-1.80,-2.03,-2.43,-3.21,-4.36,-5.06,-5.70,-5.90)
);


// V-K
LDwarfVmKs:array[0..5] of Currency = (9.45,9.7,10,10.4,10.9,11.4);
RedDwarfVmKs:array[0..19] of Currency = (3.68,3.84,4.02,4.12,4.24,4.43,4.6,
               5,5.25,5.64,5.94,6.5,7.3,7.6,8.05,8.45,8.73,8.92,9.0,9.3);
StarVmKs:array[2..6] of array[0..9] of Currency = (
  (* B *) (-0.958,-0.874,-0.602,-0.492,-0.447,-0.417,-0.358,-0.325,-0.254,-0.121),
  (* A *) ( 0.041, 0.101, 0.188, 0.228, 0.353, 0.403, 0.428, 0.528, 0.626, 0.638),
  (* F *) ( 0.722, 0.828, 0.925, 0.961, 1.017, 1.079, 1.185, 1.244, 1.290, 1.340),
  (* G *) ( 1.440, 1.458, 1.564, 1.590, 1.621, 1.635, 1.691, 1.712, 1.768, 1.857),
  (* K *) ( 1.953, 2.021, 2.155, 2.410, 2.733, 2.883, 3.190, 3.391, 3.487, 3.584)
);
VmKO95V:Currency = -0.977;

// V-J (used to produce test output)
KVmJ:array[0..9] of Real = (1.47,1.535,1.624,1.81,2.064,2.183,2.434,2.602,2.733,2.915);
MVmJ:array[0..19] of Real = (2.965,3.098,3.23,3.285,3.396,3.597,3.769,4.173,4.411,
                  4.775,5.049,5.583,6.343,6.627,7.04,7.378,7.606,7.761,7.702,7.815);
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
var
    altMSeq:Boolean;
    G_TempRate:array[0..9] of Real;
    K_TempRate:array[0..9] of Real;
    M_TempRate:array[0..19] of Real;
    L_TempRate:array[0..4] of Real;
    useBCvLog:Boolean;
    BCv_Slopes:array[2..6] of array[0..9] of Real;
    MBCv_Slopes:array[0..18] of Real;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// setting up tables used for interpolation for TEff and BCv
procedure TempTablesSetup(doalt:Boolean);
procedure BCvTablesSetup(doalt,uselog:Boolean);
// looking up values in the tables
function FindTeff(const Teffin:Real; out pos1,pos2:Integer):Boolean;
function TeffToBCv(const Teffin:Real; uselog:Boolean; out estBcV:Real):Boolean;
// temperature estimate functions that use the tables
function FindPosFromVmk_FGKML(const vmkin:Real; out pos1,pos2:Integer):Boolean;
function GetMTemp(const dex:Integer):Real;
function Inter_GKML_TeffEst(const vmkin:Real; out TEff_est:Real):Boolean;
// V-K 4 polynomial TEff estimate
function VmK_TeffEst(const vmkin:Real; out TEff_est:Real):Boolean;
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//***************************************************************************
implementation
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting up tables used for interpolation for TEff and BCv
//----------------------------------------------
procedure TempTablesSetup(doalt:Boolean);
var startt,endtt:Real;
    startvmk,endvmk:Real;
    slopeval:Real;
    oindex,iindex,mindex,lindex:Integer;
begin
  // setting up the slopes for G and K stars
  for oindex := 5 to 6 do begin
    for iindex := 0 to 9 do begin
      // finding the startpoints and endpoints
      startt := TempTable[oindex][iindex];
      startvmk := StarVmKs[oindex][iindex];
      if iindex<>9 then begin
        endtt := TempTable[oindex][iindex+1];
        endvmk := StarVmKs[oindex][iindex+1];
      end
      else if oindex < 6 then begin
        endtt := TempTable[oindex+1][0];
        endvmk := StarVmKs[oindex+1][0];
      end else begin
        endtt := GetMTemp(0);
        endvmk := RedDwarfVmKs[0];
      end;
      // calculating the slope
      slopeval := (endtt-startt)/(endvmk-startvmk);
      if oindex = 5 then G_TempRate[iindex] := slopeval
      else K_TempRate[iindex] := slopeval;
    end;
  end;
  // setting up the slopes for M stars
  for mindex := 0 to 19 do begin
    // finding the startpoints and endpoints
    startt := GetMTemp(mindex);
    startvmk := RedDwarfVmKs[mindex];
    if mindex<>19 then begin
      endtt := GetMTemp(mindex+1);
      endvmk := RedDwarfVmKs[mindex+1];
    end else begin
      endtt := LTempTable[0];
      endvmk := LDwarfVmKs[0];
    end;
    // calculating the slope
    slopeval := (endtt-startt)/(endvmk-startvmk);
    M_TempRate[mindex] := slopeval;
  end;
  // setting up the slopes for L stars
  for lindex := 0 to 4 do begin
    // finding the startpoints and endpoints
    startt := LTempTable[lindex];
    startvmk := LDwarfVmKs[lindex];
    endtt := LTempTable[lindex+1];
    endvmk := LDwarfVmKs[lindex+1];
    // calculating the slope
    slopeval := (endtt-startt)/(endvmk-startvmk);
    L_TempRate[lindex] := slopeval;
  end;
end;
//----------------------------------------------
procedure BCvTablesSetup(doalt,uselog:Boolean);
var oindex,iindex,findex:Integer;
    lowtemp,tempdiff,lowbcv,bcvdiff:Real;
begin
  // filling in the ΔBCv/ΔTEff arrays for B to K
  for oindex := 2 to 6 do begin
    for iindex := 0 to 9 do begin
      // finding the lower temperature
      if iindex = 9 then begin
          if oindex <> 6 then lowtemp := TempTable[oindex+1][0]
          else if doalt then lowtemp := MTempTableAlt[0]
          else lowtemp := MTempTableM[0];
      end else lowtemp := TempTable[oindex][iindex+1];
      // calculating the temp difference
      if uselog then tempdiff := log10(TempTable[oindex][iindex]) - log10(lowtemp)
      else tempdiff := TempTable[oindex][iindex] - lowtemp;
      // finding the lower BCv
      if iindex = 9 then begin
          if oindex <> 6 then lowbcv := BCorTable[oindex+1][0]
          else if doalt then lowbcv := MBCorAlt[0]
          else lowbcv := MBCorM[0];
      end else lowbcv := BCorTable[oindex][iindex+1];
      // calculating the BCv difference
      bcvdiff := BCorTable[oindex][iindex] - lowbcv;
      // filling in the calculated slope value...
      BCv_Slopes[oindex][iindex] := bcvdiff/tempdiff;;
    end;
  end;
  // filling in the ΔBCv/ΔTEff arrays for M
  for findex := 0 to 18 do begin
    if doalt then begin
      if uselog then tempdiff := log10(MTempTableAlt[findex])-(MTempTableAlt[findex+1])
      else tempdiff := MTempTableAlt[findex]-MTempTableAlt[findex+1];
      bcvdiff := MBCorAlt[findex]-MBCorAlt[findex+1];
    end else begin
      if uselog then tempdiff := log10(MTempTableM[findex])-log10(MTempTableM[findex+1])
      else tempdiff := MTempTableM[findex]-MTempTableM[findex+1];
      bcvdiff := MBCorM[findex]-MBCorM[findex+1];
    end;
    MBCv_Slopes[findex] := bcvdiff/tempdiff;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// looking up values in the tables
//----------------------------------------------------
function FindTeff(const Teffin:Real; out pos1,pos2:Integer):Boolean;
var rowdex,tempdex:Integer;
begin
  // out of bounds test
  Result := False;
  if Teffin > TempTable[1][0] then Exit;
  if Teffin <= LTempTable[0] then Exit;
  // finding where to look, M Dwarfs
  if Teffin <= GetMTemp(0) then begin
    pos1 := 7;
    for tempdex := 19 downto 0 do begin
      if Teffin <= GetMTemp(tempdex) then Break;
      pos2 := tempdex;
    end;
  end
  // finding where to look, O thru K dwarfs
  else begin
    // locating thr row ( O to K )
    for rowdex := 6 downto 1 do begin
       if TEffin <= TempTable[rowdex][0] then Break;
    end;
    pos1 := rowdex;
    // finding the subrange
    for tempdex := 9 downto 0 do begin
      if Teffin <= TempTable[pos1][tempdex] then Break;
    end;
    pos2 := tempdex;
  end;
  // don
  Result := True;
end;
//-----------------------------------------
function TeffToBCv(const Teffin:Real; uselog:Boolean; out estBcV:Real):Boolean;
var fpos1,fpos2:Integer;
   teffdiff,bcdiff:Real;
begin
    // getting the position of the BCv range
    Result := FindTeff(Teffin,fpos1,fpos2);
    if not Result then Exit;
    // we do not cover O, or very late M
    Result := False;
    if fpos1 = 1 then Exit;
    if (fpos1 = 7) and (fpos2 > 18) then Exit;
    // M Dwarf Case
    if fpos1 = 7 then begin
        // getting the temp (or log(temp)) difference
        if uselog then teffdiff := log10(Teffin) - log10(GetMTemp(fpos2))
        else teffdiff := Teffin - GetMTemp(fpos2);
        // calculating the new BCv
        bcdiff := teffdiff*MBCv_Slopes[fpos2];
        if altMSeq then estBcV := MBCorAlt[fpos2] + bcdiff
        else estBcV := MBCorM[fpos2] + bcdiff;
    end
    // B to K dwarf case
    else begin
      // getting the temp (or log(temp)) difference
      if uselog then teffdiff := log10(Teffin) - log10(TempTable[fpos1][fpos2])
      else teffdiff := Teffin - TempTable[fpos1][fpos2];
      // calculating the new BCv
      bcdiff := teffdiff*BCv_Slopes[fpos1][fpos2];
      estBcV := BCorTable[fpos1][fpos2] + bcdiff
    end;
    // done
    Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* uses typical VmK to find a position for the input. Typical values cover
mid F0 to mid L4 only *)
function FindPosFromVmk_FGKML(const vmkin:Real; out pos1,pos2:Integer):Boolean;
var opos,ipos,mpos:Integer;
begin
  // reject tests
  pos1 := -1;
  pos2 := -1;
  Result := False;
  if vmkin < CurrToReal(StarVmKs[4][0]) then Exit;
  if vmkin >= CurrToReal(LDwarfVmKs[4]) then Exit;
  // starting
  Result := True;
  // L Dwarf Range
  if vmkin >= CurrToReal(LDwarfVmKs[0]) then begin
    pos1 := 4;
    for mpos := 4 downto 0 do begin
      if vmkin >= CurrToReal(LDwarfVmKs[mpos]) then Break;
    end;
    pos2 := mpos;
    Exit;
  end;
  // M Dwarf range
  if vmkin >= CurrToReal(RedDwarfVmKs[0]) then begin
    pos1 := 3;
    for mpos := 19 downto 0 do begin
      if vmkin >= CurrToReal(RedDwarfVmKs[mpos]) then Break;
    end;
    pos2 := mpos;
    Exit;
  end;
  // F to K
  for opos := 6 downto 4 do begin
    if vmkin >= CurrToReal(StarVmKs[opos][0]) then begin
      pos1 := opos - 4;
      for ipos := 9 downto 0 do begin
        if vmkin >= CurrToReal(StarVmKs[opos][ipos]) then Break;
      end;
      pos2 := ipos;
      Exit;
    end;
  end;
  Assert(False);
end;
//---------------------------------------------------------
function GetMTemp(const dex:Integer):Real;
begin
  Assert(dex>=0);
  Assert(dex<20);
  if altMSeq then Result := MTempTableAlt[dex]
  else Result := MTempTableM[dex];
end;
//----------------------------------------------------------
(* TEff estimate from V-K based of linear interpolation from the
Tables of Eric Mamajek: GKML main sequence only. *)
function Inter_GKML_TeffEst(const vmkin:Real; out TEff_est:Real):Boolean;
var tpos1,tpos2:Integer;
    found_vmk,vmk_diff,foundtemp,found_slope:Real;
begin
  // reject tests
  Result := FindPosFromVmk_FGKML(vmkin,tpos1,tpos2);
  if not Result then Exit;
  // not doing F right now...
  if tpos1 = 0 then begin
    Result := False;   Exit;
  end;
  // getting the start values
  // M Dwarf
  if tpos1 = 3 then begin
    foundtemp := GetMTemp(tpos2);
    found_vmk := RedDwarfVmKs[tpos2];
    found_slope := M_TempRate[tpos2];
  // L Dwarf
  end else if tpos1 = 4 then begin
    foundtemp := LTempTable[tpos2];
    found_vmk := LDwarfVmKs[tpos2];
    found_slope := L_TempRate[tpos2];
  // G and K
  end else begin
    foundtemp := TempTable[tpos1+4][tpos2];
    found_vmk := StarVmKs[tpos1+4][tpos2];
    if tpos1 = 1 then found_slope := G_TempRate[tpos2]
    else found_slope := K_TempRate[tpos2];
  end;
  // computing 1
  vmk_diff := vmkin - found_vmk;
  TEff_est := foundtemp + found_slope*vmk_diff;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Estimates absolute visual magnitude, based on V-Ks, for late B to M stars
using Eric Mamjek's table as the TEff and V-Ks source. Uses several polynomial
fits *)
function VmK_TeffEst(const vmkin:Real; out TEff_est:Real):Boolean;
const
  coff1:array[0..4] of Real = (223119.8,-309597.343150,168811.499993,-41172.841255,3761.4720766);
  coff2:array[0..4] of Real = (-57999.7,72196.130249,-29165.767234,5061.017668,-323.556489);
  coff3:array[0..4] of Real = (18234.56,-7804.022036,1825.522529,-211.525322,9.471532);
  coff4:array[0..4] of Real = (20290.3904775,-6569.8626906,931.9620975,-57.241452,1.2143439);
begin
  Result := False;
  if vmkin < -0.08 then Exit;
  if vmkin > 10.5 then Exit;
  // B9.5 to F6
  if vmkin < 1.215 then TEff_est := PolEval(vmkin+2,coff1,5)
  // F7 to K5
  else if vmkin < 3.013 then TEff_est := PolEval(vmkin+2,coff2,5)
  // K6 to M3.5
  else if vmkin < 5.075 then TEff_est := PolEval(vmkin+2,coff3,5)
  // M4 to L3, less accurate
  else TEff_est := PolEval(vmkin+2,coff4,5);
  Result := True;
end;

//***************************************************************************
begin
  altMSeq := False;
  useBCvLog := False;
  TempTablesSetup(altMSeq);
  BCvTablesSetup(altMSeq,useBCvLog);
end.

