unit newImports3;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, newImports, df_strings, fluxtransform,
  collecdata, newlocation, stardata, NewStar;

(* Importing from :
https://arxiv.org/abs/1802.08272
table_good.tex and table_north.tex
URAT South Parallax Results: Discovery of New Nearby Stars
~ 2018, Finch, Zacharias, Jao *)
function ParseURATS_Line(linein:string):ImportedData;
function MakeURATS_Const():ImportParameters;

(* Importing from :
https://arxiv.org/abs/2010.15850
http://bit.ly/UltracoolSheet, stripped down somewhat
The Hawaii Infrared Parallax Program.
IV. A Comprehensive Parallax Survey of L0–T8 Dwarfs with UKIRT
William M. J. Best, 1,2 Michael C. Liu, 3 Eugene A. Magnier, 3 and Trent J. Dupuy
*)
function GetHIPP4Names(splitraw:TStringList):TStringList;
function ParseHIPP4_Line(linein:string):ImportedData;
function MakeHIPP4_Const():ImportParameters;

var urats_params:ImportParameters;
var hipp4_params:ImportParameters;
var kirk20_params:ImportParameters;
//======================================================================
implementation
//---------------------------------------------------------------------
(* 'URAT South Parallax Results: Discovery of New Nearby Stars'
C. Finch, N. Zacharias, W-C. Jao (2018)
https://arxiv.org/abs/1802.08272 (Other formats)
parsing the tex files for tables 5 and 6, which are almost identical.
Tex files are almost but not quite identical *)

function ParseURATS_Line(linein:string):ImportedData;
var nameid,buffer1,buffer2:string;
    nlength,soff:Integer;
    isgood:Boolean;
    impres:ImportedData;
    gpv,rpv,ipv,vtemp:Real;
    btemp:Currency;
begin
  Result := nil;
  if Length(linein) < 243 then Exit;
  // checking whether we parsing a line from 'good' or 'north' tables
  if linein[32] = '&' then begin
      isgood := True;
      soff := 5;
      nlength := 31;
  end else begin
    soff := 0;
    nlength := 26;
  end;
  // extracting and cleaning extra spaces from the id.
  nameid := Trim(Copy(linein,1,nlength));
  nameid := DelSpace1(nameid);
  // position (in degrees)
  buffer1 :=  Trim(Copy(linein,29+soff,12));
  buffer2 :=  Trim(Copy(linein,44+soff,12));
  // setting up the output object
  impres := ImportedData.Create;
  impres.AddNameID('',nameid);

  if (not impres.SetDecimalRA(buffer1)) then begin
     impres.Free;   Exit;
  end;
  if (not impres.SetDecimalDec(buffer2)) then begin
     impres.Free;   Exit;
  end;
  // for some seeminly un-necessary reason, there is an extra space in the 'good' table
  if isgood then soff += 1;
  // parallax...
  buffer1 :=  Trim(Copy(linein,95+soff,7));
  buffer2 :=  Trim(Copy(linein,106+soff,5));
  if not StrToRealBoth(buffer1,buffer2,impres.pllx,impres.pllx_err) then begin
     impres.Free;   Exit;
  end;
  // proper motion
  buffer1 := Trim(Copy(linein,123+soff,9));
  buffer2 := Trim(Copy(linein,143+soff,9));
  if (not impres.SetProperMotionPartsM(buffer1,buffer2)) then begin
      impres.Free;   Exit;
  end;

  // B and V fluxes
  SubstrCurr(linein,194+soff,7,impres.Bmag); // can fail, okay
  buffer1 := Trim(Copy(linein,204+soff,7));
  StrToReal(buffer1,impres.Vmag);             // can fail, okay
  // g' r' i' fluxes
  buffer1 := Trim(Copy(linein,214+soff,7));
  buffer2 := Trim(Copy(linein,224+soff,7));
  if StrToRealBoth(buffer1,buffer2,gpv,rpv) then begin
     buffer1 := Trim(Copy(linein,234+soff,7));
     if StrToReal(buffer1,ipv) then begin
        // only here can we convert g' r' i' to BVRcIc
        Pgri_to_BVRI(gpv,rpv,ipv,99.999,btemp,vtemp,impres.Rmag,impres.Imag);
        if impres.Vmag > 99 then impres.Vmag := vtemp;
        if impres.Bmag > 99 then impres.Bmag := btemp;
     end;
  end;

  // done
  Result := impres;
end;


function MakeURATS_Const():ImportParameters;
begin
    Result := ImportParameters.Create;
    // extra write info
    Result.idheader := 'ID?Name';
    Result.parser := ParseURATS_Line;
    Result.lineskip := 66;

    Result.useB_Flux := True;
    Result.useVRI_Flux := True;
    Result.epochdata := 2015.5;

    // source info
    Result.pllx_sourceid := 'URATS';
    Result.fullname := 'URAT South Parallax Results: Discovery of New Nearby Stars (Finch+ 2018)';
    Result.paperurl := 'https://arxiv.org/abs/1802.08272';
    Result.fullout := 'urats.csv';
    Result.leftout := 'urats_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* 'The Hawaii Infrared Parallax Program. IV'
William M. J. Best, 1,2 Michael C. Liu, 3 Eugene A. Magnier, 3 and Trent J. Dupuy
https://arxiv.org/abs/2010.15850
parsing a CSV export of the string down Google Sheets spreadsheet they link to.
*)

function GetHIPP4Names(splitraw:TStringList):TStringList;
var simbadlist:TStringList;
    part,cname:string;
begin
  Result := TStringList.Create;
  if (splitraw[34] <> 'null') then Result.Add('Gaia DR2 ' + splitraw[34]); // Gaia DR2
  if (splitraw[11] <> 'null') then Result.Add(splitraw[11]); // 2MASS
  if (splitraw[4] <> 'null') then Result.Add(splitraw[4]); // PSO
  if (splitraw[22] <> 'null') then Result.Add(splitraw[22]); // WISEA
  if (splitraw[21] <> 'null') then Result.Add(splitraw[21]); // ULAS/UGCS/VIKING etc
  // 'Simbad' Names
  part := splitraw[44];
  if (part <> 'null') then begin
     simbadlist := SplitWithDelim(part,'|',1);
     for cname in simbadlist do begin
       if cname.StartsWith('*') then Continue;
       if (cname.StartsWith('Cl*')) then Continue;
       if (cname.StartsWith('EQ ')) then Continue;
       if (Result.IndexOf(cname) >= 0) then Continue;
     end;
  end;
  // name 0
  if (Result.IndexOf(splitraw[0]) < 0) then Result.Add(splitraw[0]);
end;

function ParseHIPP4_Line(linein:string):ImportedData;
var splitraw:TStringList;
    has_dr2:Boolean;
    pdat:ImportedData;
    plx_src:string;
begin
  Result := nil;
  splitraw := SplitWithDelim(linein,';',48);
  if (splitraw = nil) then Exit;

  try
    pdat := ImportedData.Create;
    // position
    has_dr2 := (splitraw[34] <> 'null');
    if has_dr2 then begin
       // DR2 positions (precise), J2015.5
       if not pdat.SetDecimalRA(splitraw[32]) then Exit;
       if not pdat.SetDecimalDec(splitraw[33]) then Exit;
    end else begin
       // not very precise (only 4 digits) J2000
       if not pdat.SetDecimalRA(splitraw[1]) then Exit;
       if not pdat.SetDecimalDec(splitraw[2]) then Exit;
    end;
    // names
    pdat.nameids := GetHIPP4Names(splitraw);
    // proper motion 26, 28
    if not pdat.SetProperMotionPartsM(splitraw[26],splitraw[28]) then Exit;
    // parallax 23,24, src is 25 (DR2 or other)
    if not StrToRealBoth(splitraw[23],splitraw[24],pdat.pllx,pdat.pllx_err) then Exit;
    plx_src := splitraw[25];
    if (plx_src = 'DR2') then begin
       pdat.pllx += 0.029;
       pdat.pllx_sourceid := 'Gaia DR2';
    end
    else pdat.pllx_sourceid := plx_src;
    // spectral type
    pdat.stype := '??';
    if (splitraw[40] <> 'null') then pdat.stype += '/' + splitraw[40] ;
    if (splitraw[42] <> 'null') then pdat.stype += '/' + splitraw[42] ;

    Result := pdat;
  finally
    if (Result = nil) then begin
       pdat.nameids.Free;
       pdat.Free;
    end;
    splitraw.free;
  end;
end;

function MakeHIPP4_Const():ImportParameters;
begin
    Result := ImportParameters.Create;
    // extra write info
    Result.idheader := 'ID?Name';
    Result.parser := ParseHIPP4_Line;
    Result.lineskip := 1;

    Result.useB_Flux := False;
    Result.useVRI_Flux := False;
    Result.use_altid := True;
    Result.useSpT_emp := True;

    // source info
    Result.pllx_sourceid := 'HIPP4';
    Result.fullname := 'The Hawaii Infrared Parallax Program. IV. (Best+ 2020)';
    Result.paperurl := 'https://arxiv.org/abs/2010.15850 ';
    Result.fullout := 'hipp4.csv';
    Result.leftout := 'hipp4_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* 'The Field Substellar Mass Function Based on the Full-sky 20-pc Census
of 525 L, T, and Y Dwarfs'  Kirkpatrick+ 2020
https://arxiv.org/abs/2011.11616
parsing Table A1
*)

// make spt from inputs
function MakeKirk20_SpT(adopted,flag:string):string;
var top:Real;
begin
  Result := '';
  if not TryStrToFloat(Trim(adopted),top) then Exit;
  if (top <= -9) then Exit;
  // number to character...
  if top < 0 then begin
     Result := 'M'; top += 10;
  end
  else if top < 10 then Result := 'L'
  else if top < 20 then begin
     Result := 'T';
     top -= 10;
  end
  else begin
      Result := 'Y';
      top -= 20;
  end;
  // adding the number part (and trimming trailing zero if there)
  Result += FloatToStrF(top,ffFixed,3,1);
  if AnsiEndsStr('.0',Result) then Result := Copy(Result,0,Length(Result)-2);
  // flag checking
  flag := Trim(flag);
  if flag = 'S' then Result := 'sd' + Result
  else if flag = 'M' then Result += 'J';
end;

// translate astrometric source
function Kirk20_AstSrcTrans(inchar:string):string;
begin
  Result := 'Kirk20';
  if Length(inchar) <> 1 then Exit;
  case inchar[1] of
      'A': Result := 'Dahn02';
      'b': Result := 'Burg08a';      'B': Result := 'Bart17';
      'c': Result := 'CatWISE2020';  'C': Result := 'Tinn2014';
      'd': Result := 'Dahn17';       'D': Result := 'DupLiu12';
      'E': Result := 'Dup19';
      'F': Result := 'Fah12';
      'G': Result := 'Gaia DR2';     'g': Result := 'Gaia DR2';
      'J': Result := 'Kirk11';
      'H': Result := 'New Hipparcos';
      'K': Result := 'Kirk19a';      'k': Result := 'Kirk19b';
      'l': Result := 'Legg12';       'L': Result := 'Liu16';
      'm': Result := 'Manj13';       'M': Result := 'Maro10';
      'r': Result := 'Smart-k';      'R': Result := 'Smart18';
      'S': Result := 'Casw08';       's': Result := 'Smart13';
      't': Result := 'Tinn03';
      'V': Result := 'Vrba04';
      'W': Result := 'Best20';
      'z': Result := 'Dup20';        'Z': Result := 'LazS18';
  end;
end;

//------------------------------------------
function ParseKirk20_Line(linein:string):ImportedData;
var splitraw:TStringList;
    pdat:ImportedData;
    buf1,buf2:string;
    pmra,pmd:Real;
begin
  Result := nil;
  splitraw := SplitWithDelim(linein,',',53);
  if (splitraw = nil) then Exit;
  try
    pdat := ImportedData.Create;
    // spectral type
    pdat.stype := MakeKirk20_SpT(splitraw[4],splitraw[51]);
    // parallax and error
    if not StrToRealBoth(splitraw[6],splitraw[7],pdat.pllx,pdat.pllx_err) then Exit;
    if pdat.pllx <= 0 then Exit;
    // proper motion
    buf1 := Trim(splitraw[8]);
    if buf1 = '-22222' then Exit;
    buf2 := Trim(splitraw[10]) + Trim(splitraw[12]);
    if buf2 = '-11111' then Exit;
    if not pdat.SetProperMotionPartsM(buf1,buf2) then Exit;
    // astrometry source
    pdat.pllx_sourceid := Kirk20_AstSrcTrans(Trim(splitraw[13]));
    if (pdat.pllx_sourceid = 'Gaia DR2') then pdat.pllx += 0.029;
    // magnitudes
    Str2Curr(splitraw[16],pdat.Jmag);
    Str2Curr(splitraw[18],pdat.Hmag);
    StrToCurrBoth(splitraw[22],splitraw[23],pdat.Ksmag,pdat.KsmagE);
    // position
    if not pdat.SetDecimalRA(splitraw[25]) then Exit;
    if not pdat.SetDecimalDec(splitraw[26]) then Exit;
    // identifier (only 1)
    pdat.nameids := TStringlist.Create;
    pdat.nameids.Add(Trim(splitraw[52]));
    Result := pdat;
  finally
    if (Result = nil) then begin
       pdat.nameids.Free;
       pdat.Free;
    end;
    splitraw.free;
  end;
end;

function MakeKirk20_Const():ImportParameters;
begin
    Result := ImportParameters.Create;
    // extra write info
    Result.idheader := 'ID?Name';
    Result.parser := ParseKirk20_Line;
    Result.lineskip := 7;

    Result.useB_Flux := False;
    Result.useVRI_Flux := False;
    Result.useJHK_Flux := True;
    Result.use_altid := True;
    Result.useSpT_emp := True;

    // source info
    Result.pllx_sourceid := 'Kirk20';
    Result.fullname := 'The Field Substellar Mass Function Based on the Full-sky 20-pc Census of 525 L, T, and Y Dwarfs (Kirkpatrick+ 2020)';
    Result.paperurl := 'https://arxiv.org/abs/2011.11616';
    Result.fullout := 'kirk20.csv';
    Result.leftout := 'kirk20_leftover.csv';
end;


//=====================================================================
begin
  urats_params := MakeURATS_Const();
  hipp4_params := MakeHIPP4_Const();
  kirk20_params := MakeKirk20_Const();
end.

