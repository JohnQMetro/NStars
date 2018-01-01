unit newImports2;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, newImports, df_strings, fluxtransform, DAMath,
  Utilities;

(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/Cat?I/333
upc.dat ~ The URAT Parallax Catalog (UPC)
~ 2016, Finch, Zacharias *)
function ParseURAT_Line(linein:string):ImportedData;
function URAT_CSVNames(inputl:TStringList):string;
function MakeURAT_Const():ImportParameters;

(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/AJ/149/5
table1.dat ~ Solar neighborhood. XXXV. Distances to M dwarfs
~ 2015, Winters, Hambly, Jao, Dieterich, Lurie, Henry, Hosey, Slatten, Boyd,
  Ianna, Riedel, Subasavage, Finch, Bartlett  *)
function ImproveNames(const indat:string):string;
function ParseSN35_Line(linein:string):ImportedData;
function MakeSN35_Const():ImportParameters;

(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/Cat?I/311
hip2.dat ~ Hipparcos, the new Reduction of the Raw data
~ 2007, van Leeuwen F *)
function ParseNewHipparcos_Line(linein:string):ImportedData;
function MakeNewHipparcos_Const():ImportParameters;

(* Importing From
http://cdsarc.u-strasbg.fr/viz-bin/Cat?cat=B%2Fwd&target=brief&
catalog.dat ~ A Catalogue of Spectroscopically Identified White Dwarfs
~ 2014, McCook and Sion *)
function FarseSIWDLine(linein:string):ImportedData;
function SIWD_CSVNames(inputl:TStringList):string;
function MakeSIWD_Const():ImportParameters;

(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/AJ/149/5
table2.dat ~ Solar neighborhood. XXXV. Distances to M dwarfs
~ 2015, Winters, Hambly, Jao, Dieterich, Lurie, Henry, Hosey, Slatten, Boyd,
  Ianna, Riedel, Subasavage, Finch, Bartlett  *)
function ParseSN35T2_Line(linein:string):ImportedData;
function MakeSN35T2_Const():ImportParameters;

(* Importing from :
All-sky catalog of bright M dwarfs (Lepine+, 2011)
http://vizier.u-strasbg.fr/viz-bin/VizieR?-source=J/AJ/142/138
Using an ASCII text/plain table produced by VizieR with:
PM,HIP,CNS3,RAJ,DECJ,pmra,pmdec,plx,e_plx,J,H,K,V,
phot pllx, phot e_pllx, SimbadName
*)
function ParseLepBMDC_Line(linein:string):ImportedData;
function LepBMDC_CSVNames(inputl:TStringList):string;
function MakeLepBMDC_Const():ImportParameters;

(* Importing from UCAC4 nearby star search.
http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/AJ/148/119
table5 or table6
Finch C.T., Zacharias N., Subasavage J.P., Henry T.J., Riedel A.R. (2014) *)
function ParseUCAC4SearchLine(linein:string):ImportedData;
function MakeUCAC4_Const():ImportParameters;

(* Importing from The Solar Neighborhood. XXXIX. Parallax Results from the
CTIOPI and NOFS Programs: 50 New Members of the 25 Parsec White Dwarf Sample
https://arxiv.org/abs/1706.00709
John P. Subasavage, Wei-Chun Jao, Todd J. Henry, Hugh C. Harris, Conard C. Dahn,
Pierre Bergeron, Patrick Dufour, Bart H. Dunlap, Brad N. Barlow,
Philip A. Ianna, Sebastien Lepine, Steven J. Margheim
Cut and Pasted from the PDF file     *)
function ParseSN39Line(linein:string):ImportedData;
function MakeSN39_Const():ImportParameters;

(* A catalog of GALEX ultraviolet emission from spectroscopically confirmed
M dwarfs.  Jones D.O., West A.A. (2016)
table3.dat at CDS includes Photometric distance estimates for Palomar/MSU
dwarfs, which are nearer than the SDSS dwarfs in table2.dat  *)
function ParseGALEX_3Line(linein:string):ImportedData;
function MakeGALEX3_Const():ImportParameters;

(* CONCH-SHELL catalog of nearby M dwarfs   (Gaidos+, 2014)
table3.dat from http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/MNRAS/443/2561
*)
function ParseConchShell_Line(linein:string):ImportedData;
function ConchShell_CSVNames(inputl:TStringList):string;
function MakeConchShell_Const():ImportParameters;

(* RAVE DR5 extract from Vizier *)
function ParseRAVEDR5Extract_Line(linein:string):ImportedData;
function RAVEDR5Extract_CSVNames(inputl:TStringList):string;
function MakeRAVEDR5Extract_Const():ImportParameters;

(* 'CCD Parallaxes for 309 Late-type Dwarfs and Subdwarfs' (Dahn+ 2017)
tab1.txt from the extra data. Uses code NOFS-17, since the parallaxes
are from observations made by the Naval Observatory Flagstaff Station *)
function ParseNOFS17_Line(linein:string):ImportedData;
function NOFS17_CSVNames(inputl:TStringList):string;
function MakeNOFS17_Const():ImportParameters;


//----------------------------------------------------
var urat_params:ImportParameters;
    sn35_params:ImportParameters;
    newhip_params:ImportParameters;
    siwd_params:ImportParameters;
    sn35_2_params:ImportParameters;
    lepgad_params:ImportParameters;
    ucac4nss_params:ImportParameters;
    sn39_params:ImportParameters;
    galex3_params:ImportParameters;
    conchshell_params:ImportParameters;
    rave5_params:ImportParameters;
    nofs17_params:ImportParameters;
//===============================================================
implementation

function ParseURAT_Line(linein:string):ImportedData;
var name1,name2,buffer1,buffer2:string;
    impres:ImportedData;
    urat_mag:Currency;
begin
  Result := nil;
  if Length(linein) < 150 then Exit;
  // UPC number
  name1 := Trim(Copy(linein,1,6));
  // right ascension and declination
  buffer1 := Copy(linein,8,11);
  buffer2 := Copy(linein,20,11);
  // identifier
  name2 := Trim(Copy(linein,32,30));

  // starting to build the result
  impres := ImportedData.Create;
  impres.AddNameID('UPC',name1);
  impres.AddNameID('',name2);
  if (not impres.SetDecimalRA(buffer1)) then begin
    FreeAndNil(impres);   Exit;
  end;
  if (not impres.SetDecimalDec(buffer2)) then begin
    FreeAndNil(impres);   Exit;
  end;

  // getting the magnitude (custom URAT bandpass, not actually using it)
  if (not SubstrCurr(linein,63,5,urat_mag)) then begin
    FreeAndNil(impres);   Exit;
  end;

  // parallax
  buffer1 := Copy(linein,82,6);
  buffer2 := Copy(linein,89,5);
  if (not StrToRealBoth(buffer1,buffer2,impres.pllx,impres.pllx_err)) then begin
    FreeAndNil(impres);   Exit;
  end;
  (* The URAT parallax catalog is generally poor quality, so I'll weed out the
  worst results now. *)
  if ((impres.pllx <= 0) or (impres.pllx_err > 40)) then begin
    FreeAndNil(impres);    Exit;
  end;

  // proper motion
  buffer1 := Copy(linein,95,7);
  buffer2 := Copy(linein,109,7);
  if (not impres.SetProperMotionPartsM(buffer1,buffer2)) then begin
    FreeAndNil(impres);   Exit;
  end;
  //done
  Result := impres;
end;
//-----------------------------------------------------------------
function URAT_CSVNames(inputl:TStringList):string;
var cxid:string;
begin
  // no results here
  Result := ',';    // default empty result
  if inputl = nil then Exit;
  if inputl.Count = 0 then Exit;
  // first result field (mandatory)
  cxid := inputl[0];
  Assert(AnsiStartsStr('UPC',cxid));
  Result := cxid + ',';
  // after first result
  if inputl.Count > 1 then Result += inputl[1];
  // done
end;
//-----------------------------------------------------------------
function MakeURAT_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idmaker := URAT_CSVNames;
  Result.idheader := 'URAT ID, Other ID';
  // special
  Result.parser := ParseURAT_Line;
  Result.lineskip := 0;
  Result.epochdata := 2014;
  // source info
  Result.pllx_sourceid := 'URAT';
  Result.fullname := 'URAT Parallax Catalog';
  Result.paperurl := 'https://arxiv.org/pdf/1604.06739v1.pdf';
  Result.fullout := 'urat_parallax.csv';
  Result.leftout := 'urat_leftover.csv'
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* RECONS has a bad habit of abbreviating, zero-padding, and space-removing
in catalog ids  *)
function ImproveNames(const indat:string):string;
var cid,cnum:string;
begin
  if SplitNames(indat,cid,cnum) then begin
    // LEHPM to simbad format
    if (cid = 'LEHPM') and AnsiStartsStr('1-',cnum) then begin
      cnum := RightStr(cnum,Length(cnum)-2);
    end;
    // removing zero padding
    if StrEqAny(cid,['GJ','L','LHS','LTT','WT','Gl','Wo','EG','G','LEHPM']) then begin
      while AnsiStartsStr('0',cnum) do begin
        cnum := RightStr(cnum,Length(cnum)-1);
      end;
    end;
    // altering some catalog names to make them Simbad-able
    if cid = '2MA' then cid := '2MASS'   // id is still too abbreviated
    else if cid = 'APM' then cid := 'APMPM'
    else if cid = 'DEN' then cid := 'DENIS'
    else if cid = 'ESO' then cid := 'Ruiz'
    else if cid = 'HB' then cid := '[HB88]'
    else if cid = 'WOLF' then cid := 'Wolf'
    else if cid = 'ROSS' then cid := 'Ross'
    else if cid = 'SIP' then cid := 'SIPS';
    // inserting the proper 'entry-prefix'
    if StrEqAny(cid,['2MASS','APMPM','DENIS','SCR','UPM','SIPS']) then cnum := 'J' + cnum
    else if cid = 'BRI' then cnum := 'B' + cnum;
    // finally, building the result
    Result := cid + ' ' + cnum;
  end
  else Result := indat;
end;
//------------------------------------------------
function ParseSN35_Line(linein:string):ImportedData;
var name1,buffer1,buffer2:string;
    impres:ImportedData;
begin
  Result := nil;
  if Length(linein) < 130 then Exit;
  // identifier
  name1 := Trim(Copy(linein,1,14));
  // right ascension and declination
  buffer1 := Copy(linein,20,10);
  buffer2 := Copy(linein,31,9);

  // starting to build the result
  impres := ImportedData.Create;
  name1 := ImproveNames(name1);
  impres.AddNameID('',name1);
  impres.rapos := buffer1;
  impres.decpos := buffer2;

  // proper motion magnitude (in arcseconds) and position
  buffer1 := Copy(linein,41,5);
  buffer2 := Copy(linein,47,5);
  if (not StrToReal(buffer2,impres.pm_ang)) then begin
    FreeAndNil(impres);    Exit;
  end;
  if (not impres.SetProperMotionMagSec(buffer1)) then begin
    FreeAndNil(impres);    Exit;
  end;

  // parallax and parallax error (the 'parallax' is sometimes photometric).
  buffer1 := Copy(linein,56,7);
  buffer2 := Copy(linein,69,4);
  if (not StrToRealBoth(buffer1,buffer2,impres.pllx,impres.pllx_err)) then begin
    FreeAndNil(impres);    Exit;
  end;
  // plate fluxes will not be used
  // recons fluxes (will be used, if present)
  buffer1 := Copy(linein,111,5);
  StrToReal(buffer1,impres.Vmag);
  SubstrCurr(linein,118,5,impres.Rmag);
  SubstrCurr(linein,125,5,impres.Imag);

  // done
  Result := impres;
end;
//-----------------------------------------------------------------
function MakeSN35_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // flags for data to be used
  Result.useVRI_Flux := True;
  // extra write info
  Result.idheader := 'Abbrev ID.';
  Result.idmaker := nil;
  // special
  Result.parser := ParseSN35_Line;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  // source info
  Result.pllx_sourceid := 'SN 35';
  Result.fullname := 'The Solar Neighborhood XXXV: Distances to 1404 M Dwarf Systems Within 25 pc in the Southern Sky';
  Result.paperurl := 'https://www.cfa.harvard.edu/~jwinters/published35.pdf';
  Result.fullout := 'sn35_mdwarf.csv';
  Result.leftout := 'sn35_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/Cat?I/311
hip2.dat ~ Hipparcos, the new Reduction of the Raw data
~ 2007, van Leeuwen F *)
//-------------------------------------------------
function ParseNewHipparcos_Line(linein:string):ImportedData;
var name,buffer1,buffer2:string;
    impres:ImportedData;
    hp_mag:Currency;
begin
  Result := nil;
  if Length(linein) < 140 then Exit;
  // Hip number
  name := Trim(Copy(linein,1,6));
  // right ascension and declination
  buffer1 := Copy(linein,16,13);
  buffer2 := Copy(linein,30,13);

  // starting to build the result
  impres := ImportedData.Create;
  impres.AddNameID('Hip',name);
  if (not impres.SetRADec_FromRadians(buffer1,buffer2)) then begin
    FreeAndNil(impres);   Exit;
  end;

  // parallax
  buffer1 := Copy(linein,44,7);
  if (not StrToReal(buffer1,impres.pllx)) then begin
    FreeAndNil(impres);   Exit;
  end;
  // because both new and old Hipparcos have bad errors...
  if impres.pllx <= 0 then begin;
    FreeAndNil(impres);   Exit;
  end;
  // proper motion
  buffer1 := Copy(linein,52,8);
  buffer2 := Copy(linein,61,8);
  if (not impres.SetProperMotionPartsM(buffer1,buffer2)) then begin
    FreeAndNil(impres);   Exit;
  end;
  // parallax error
  buffer2 := Copy(linein,84,6);
  if (not StrToReal(buffer2,impres.pllx_err)) then begin
    FreeAndNil(impres);   Exit;
  end;
  // getting the magnitude (custom Hp bandpass, not actually using it)
  if (not SubstrCurr(linein,130,7,hp_mag)) then begin
    FreeAndNil(impres);   Exit;
  end;

  // done
  Result := impres;
end;
//--------------------------------------------------
function MakeNewHipparcos_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idmaker := nil;
  Result.idheader := 'Hip ID';
  // special
  Result.parser := ParseNewHipparcos_Line;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  // source info
  Result.pllx_sourceid := 'New Hipparcos';
  Result.fullname := 'Hipparcos, the new Reduction of the Raw data';
  Result.paperurl := 'https://arxiv.org/abs/0708.1752';
  Result.fullout := 'newhipparcos_parallax.csv';
  Result.leftout := 'newhipparcos_leftover.csv'
end;
//============================================================================
(* Importing From

catalog.dat ~ A Catalogue of Spectroscopically Identified White Dwarfs
~ 2014, McCook and Sion *)
//----------------------------------------
function FarseSIWDLine(linein:string):ImportedData;
var name,buffer1,buffer2:string;
    impres:ImportedData;
    realmag:Real;
    vimp,bimp:Boolean;
begin
  Result := nil;
  if Length(linein) < 211 then Exit;
  // getting WD number
  name := Trim(Copy(linein,2,10));
  // position
  buffer1 := Trim(Copy(linein,13,8)); // RA
  buffer2 := Trim(Copy(linein,22,8));  // Dec (no seconds, but that is good enough for simbad
  if (Length(buffer1)=0) and (Length(buffer2)=0) then Exit;

  // if we get here, we start making the result
  impres := ImportedData.Create;
  impres.AddNameID('WD',name);
  impres.rapos := buffer1;
  impres.decpos := buffer2;
  impres.b1950pos := True;
  // spectral type
  impres.stype:= Trim(Copy(linein,31,10));
  // additional names
  // the first is a 2MASS id, but is 'rounded' by 1 digit each, making it useless
  buffer1 := Trim(Copy(linein,47,14));  // I'll get it anyway...
  if Length(buffer1)=14 then begin
    buffer1 := 'J' + buffer1;
    impres.AddNameID('2MASS',buffer1);
  end;
  // the second extra name also needs processing, if there
  buffer2 := Trim(Copy(linein,61,11));
  if (Length(buffer2)<>0) and (not StrEqAny(buffer2,['PG','MCT','KUV','HS','RE','SCR','SDSS','EC'])) then begin
    buffer1 := ImproveNames(buffer2);
    impres.AddNameID('',buffer1);
  end;

  // v magnitude
  vimp := StrToReal(Copy(linein,73,6),realmag);
  if vimp then begin
    buffer1 := Trim(Copy(linein,80,2));
    bimp := (buffer1 = 'B');
    if bimp then begin
      vimp := False;
      impres.Bmag:= RealToCurr(realmag);
    end
    else impres.Vmag:= realmag;
  end;
  // B-V
  if StrToReal(Copy(linein,82,6),realmag) then begin
    if vimp then impres.Bmag:= RealToCurr(impres.Vmag+realmag);
  end;
  // U-B
  if StrToReal(Copy(linein,92,6),realmag) then begin
    if impres.Bmag < 90 then impres.Umag := impres.Bmag + RealToCurr(realmag);
  end;

  // proper motion
  buffer1 := Copy(linein,162,6);
  if (not impres.SetProperMotionMagSec(buffer1)) then begin
    FreeAndNil(impres);    Exit;
  end;
  buffer2 := Copy(linein,170,5);
  if not StrToReal(buffer2,impres.pm_ang) then begin
    FreeAndNil(impres);    Exit;
  end;

  // parallax
  buffer1 := Copy(linein,200,7);
  buffer2 := Copy(linein,207,4);
  if not StrToRealBoth(buffer1,buffer2,realmag,impres.pllx_err) then begin
    FreeAndNil(impres);    Exit;
  end;
  impres.pllx:= realmag*1000;
  if impres.pllx <= 0 then begin
    FreeAndNil(impres);    Exit;
  end;

  // done (some WD have griz magnitudes, which can be used to get Rc and Ic, I'll not bother)
  Result := impres;
end;
//----------------------------------------
function SIWD_CSVNames(inputl:TStringList):string;
begin
  // bad input...
  Result := ',,';
  if inputl = nil then Exit;
  if inputl.Count = 0 then Exit;
  // starting (WD ident)
  Result := inputl[0] + ',';
  if inputl.Count = 1 then Result += ','
  // possible second or third ident.
  else begin
    if AnsiStartsStr('2MASS',inputl[1]) then begin
      Result += inputl[1] + ',';
      if inputl.Count = 3 then Result+= inputl[2];
    end
    else Result += ',' + inputl[1];
  end;
end;

//----------------------------------------
function MakeSIWD_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'WD ID,2MASS ID (s),Other ID';
  // special
  Result.parser := FarseSIWDLine;
  Result.idmaker := SIWD_CSVNames;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  // usage
  Result.useU_Flux := True;
  Result.useB_Flux := True;
  Result.useVRI_Flux := True;
  Result.useSpT := True;
  // source info
  Result.pllx_sourceid := 'Spec.WD';
  Result.fullname := 'A Catalogue of Spectroscopically Identified White Dwarfs (Version Apr. 2014)';
  Result.paperurl := 'http://cdsarc.u-strasbg.fr/viz-bin/Cat?cat=B%2Fwd&target=brief&';
  Result.fullout := 'siwd_parallax.csv';
  Result.leftout := 'siwd_leftover.csv'
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Importing from :
http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/AJ/149/5
table2.dat ~ Solar neighborhood. XXXV. Distances to M dwarfs
~ 2015, Winters, Hambly, Jao, Dieterich, Lurie, Henry, Hosey, Slatten, Boyd,
  Ianna, Riedel, Subasavage, Finch, Bartlett  *)
function ParseSN35T2_Line(linein:string):ImportedData;
var name,buffer1,buffer2:string;
    pltdist,pltdiste,ccddist,ccddiste,trgdist,trgdiste:Real;
    okplt,okccd,oktrg:Boolean;
    impres:ImportedData;
    thedist,disterr:Real;
    medpllx,maxpllx,minpllx:Real;
    xpllxerr:Real;
begin
  Result := nil;
  if Length(linein) < 98 then Exit;
  // getting Name number
  name := Trim(Copy(linein,1,14));
  // right ascension and declination
  buffer1 := Copy(linein,16,10);
  buffer2 := Copy(linein,27,9);

  // starting to build the result
  impres := ImportedData.Create;
  name := ImproveNames(name);
  impres.AddNameID('',name);
  impres.rapos := buffer1;
  impres.decpos := buffer2;

  // this table contains 3 distances in parsecs
  // plate photometric distance
  buffer1 := Copy(linein,37,5);
  buffer2 := Copy(linein,47,4);
  okplt := StrToRealBoth(buffer1,buffer2,pltdist,pltdiste);
  // ccd photometric distance
  buffer1 := Copy(linein,55,5);
  buffer2 := Copy(linein,65,4);
  okccd := StrToRealBoth(buffer1,buffer2,ccddist,ccddiste);
  // weighted average of trig distances
  buffer1 := Copy(linein,73,5);
  buffer2 := Copy(linein,81,5);
  oktrg := StrToRealBoth(buffer1,buffer2,trgdist,trgdiste);

  // we must have at least one distance!
  if not (okplt or okccd or oktrg) then begin
    FreeAndNil(impres);   Exit;
  end;

  // the most reliable distance ...
  buffer1 := Copy(linein,96,3);
  if buffer1 = 'plt' then begin
    thedist := pltdist;   disterr := pltdiste;
  end else if buffer1 = 'ccd' then begin
    thedist := ccddist;   disterr := ccddiste;
  end else if buffer1 = 'trg' then begin
    thedist := trgdist;   disterr := trgdiste;
  end else begin
    FreeAndNil(impres);  Exit;
  end;

  // computing parallax from distance
  if not impres.SetParallaxFromDistance(thedist,disterr) then begin
    FreeAndNil(impres);   Exit;
  end;
  if (impres.pllx_err < 0.001) or (impres.pllx_err>100) then begin
    FreeAndNil(impres);  Exit;
  end;

  // done
  Result := impres;
end;
//-----------------------------------------------------------------
function MakeSN35T2_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idmaker := nil;
  Result.idheader := 'ID';
  // special
  Result.parser := ParseSN35T2_Line;
  Result.idmaker := nil;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  Result.useSimbadProperMotion := True;;
  // source info
  Result.pllx_sourceid := 'SN 35-2';
  Result.fullname := 'The Solar Neighborhood XXXV: Distances to 1404 M Dwarf Systems Within 25 pc in the Southern Sky (Table 2)';
  Result.paperurl := 'https://www.cfa.harvard.edu/~jwinters/published35.pdf';
  Result.fullout := 'sn35_2mdwarf.csv';
  Result.leftout := 'sn35_2leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Importing from :
All-sky catalog of bright M dwarfs (Lepine+, 2011)
http://vizier.u-strasbg.fr/viz-bin/VizieR?-source=J/AJ/142/138
Using an ASCII text/plain table produced by VizieR with:
PM,HIP,CNS3,RAJ,DECJ,pmra,pmdec,plx,e_plx,J,H,K,V,
phot pllx, phot e_pllx, SimbadName
*)
//--------------------------------------------
function ParseLepBMDC_Line(linein:string):ImportedData;
var curname,buffer1,buffer2:string;
    impres:ImportedData;
    hastrig:Boolean;
    real1,real2:Real;
begin
  Result := nil;
  if Length(linein) < 147 then Exit;
  // initial startup and first name
  curname := Trim(Copy(linein,2,13));
  if Length(curname) < 10 then Exit;
  curname := 'J' + curname;
  impres := ImportedData.Create;
  impres.AddNameID('PM',curname);
  // second name (Hipparcos)
  curname := Trim(Copy(linein,15,8));
  if Length(curname)<>0 then impres.AddNameID('Hip',curname);
  // third name (GJ or Gl)
  curname := Trim(Copy(linein,24,15));
  if Length(curname)<>0 then impres.AddNameID('',curname);

  // right ascension and declination in degrees
  buffer1 := Trim(Copy(linein,42,10));
  buffer2 := Trim(Copy(linein,53,10));
  if not impres.SetDecimalRA(buffer1) then begin
    FreeAndNil(impres);    Exit;
  end;
  if not impres.SetDecimalDec(buffer2) then begin
    FreeAndNil(impres);    Exit;
  end;

  // proper motion
  buffer1 := Copy(linein,64,6);
  buffer2 := Copy(linein,70,7);
  if not impres.SetProperMotionPartsS(buffer1,buffer2) then begin
    FreeAndNil(impres);    Exit;
  end;

  // next is trig parallax
  buffer1 := Trim(Copy(linein,79,7));
  buffer2 := Trim(Copy(linein,86,7));
  hastrig := StrToRealBoth(buffer1,buffer2,real1,real2);
  if hastrig then begin
    impres.pllx:= 1000*real1;
    impres.pllx_err:= 1000*real2;
  end;

  // I will skip the JHK, but I will record V.
  buffer1 := Trim(Copy(linein,112,5));
  if StrToReal(buffer1,real1) then impres.Vmag := real1;

  // photometric parallax
  buffer1 := Trim(Copy(linein,119,7));
  buffer2 := Trim(Copy(linein,127,7));
  if not StrToRealBoth(buffer1,buffer2,real1,real2) then begin
    FreeAndNil(impres);    Exit;
  end;
  real1 := 1000*real1;
  real2 := 1000*real2;

  // which parallax do we use
  if (not hastrig) or (impres.pllx_err > real2) then begin
    impres.pllx := real1;
    impres.pllx_err := real2;
  end;
  // done
  Result := impres;
end;
//--------------------------------------------
function LepBMDC_CSVNames(inputl:TStringList):string;
var inputlc:Integer;
const bado = ',,';
begin
  // bad cases
  Result := bado;
  if (inputl=nil) then  Exit;
  inputlc := inputl.Count;
  if inputlc = 0 then Exit;
  if inputlc > 3 then Exit;
  // regular
  Result := inputl[0];
  // one item: PM
  if inputlc = 1 then Result += bado
  else begin
    Result += ',';
    // three items means PM, Hip, and GJ
    if inputlc = 3 then begin
      Result += inputl[1] +',' + inputl[2];
    // two items: Hip or not
    end else begin
      if AnsiStartsStr('Hip',inputl[1]) then Result += inputl[1] + ','
      else Result += ',' + inputl[1];
    end;
  end;
  // done
end;
//--------------------------------------------
function MakeLepBMDC_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idmaker := LepBMDC_CSVNames;
  Result.idheader := 'PM ID,Hip ID,Gl/GJ ID';
  // special
  Result.parser := ParseLepBMDC_Line;
  Result.lineskip := 66;
  // stuff i'm not sure we need (only V is available)
  Result.useVRI_Flux := True;
  // source info
  Result.pllx_sourceid := 'LG CBMD';
  Result.fullname := 'All-sky catalog of bright M dwarfs (Lepine and Gaidos, 2011)';
  Result.paperurl := 'http://iopscience.iop.org/article/10.1088/0004-6256/142/4/138/pdf';
  Result.fullout := 'lepine_bmdc.csv';
  Result.leftout := 'lepine_bmdc_leftover.csv';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Importing from UCAC4 nearby star search.
http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/AJ/148/119
table5 or table6
Finch C.T., Zacharias N., Subasavage J.P., Henry T.J., Riedel A.R. (2014)
*)
//---------------------------------------------
function ParseUCAC4SearchLine(linein:string):ImportedData;
var curname,buffer1,buffer2:string;
    impres:ImportedData;
    curval,Best:Currency;
    xg,xr,xi,Vest:Real;
    temp1,temp2:Real;
begin
  Result := nil;
  if Length(linein) < 185 then Exit;
  // name
  curname := Trim(Copy(linein,1,25));
  curname := AnsiReplaceStr(curname,'UPM ','UPM J');
  curname := AnsiReplaceStr(curname,'SCR ','SCR J');
  curname := AnsiReplaceStr(curname,'PM I','PM J');
  curname := AnsiReplaceStr(curname,'LHS 0','LHS ');
  curname := AnsiReplaceStr(curname,'GJ 0','Gl ');
  if length(curname) = 0 then Exit;
  // ra and dec in decimal degrees
  buffer1 := Trim(Copy(linein,27,11));
  buffer2 := Trim(Copy(linein,39,11));
  if (Length(buffer1) = 0) or (Length(buffer2) = 0) then Exit;
  // setting up the object
  impres := ImportedData.Create;
  impres.AddNameID('',curname);
  if not impres.SetDecimalRA(buffer1) then begin
    FreeAndNil(impres);    Exit;
  end;
  if not impres.SetDecimalDec(buffer2) then begin
    FreeAndNil(impres);    Exit;
  end;

  // proper motion
  buffer1 := Copy(linein,51,7);
  buffer2 := Copy(linein,66,7);
  if not impres.SetProperMotionPartsM(buffer1,buffer2) then begin
    FreeAndNil(impres);    Exit;
  end;

  // magnitudes
  // Bmag
  if SubstrCurr(linein,79,6,curval) then impres.Bmag := curval;
  // Vmag
  buffer1 := Trim(Copy(linein,86,6));
  StrToReal(buffer1,impres.Vmag);
  // g r i
  buffer1 := Trim(Copy(linein,93,6));
  buffer2 := Trim(Copy(linein,100,6));
  if StrToRealBoth(buffer1,buffer2,xg,xr) then begin
    buffer1 := Trim(Copy(linein,107,6));
    if StrToReal(buffer1,xi) then begin
      // now, we convert APASS gri to estimates for V,B,Rc, and Ic
      Pgri_to_BVRI(xg,xr,xi ,Best,Vest,impres.Rmag,impres.Imag);
      if (impres.Bmag > 90) and (Best < 90) then impres.Bmag := Best;
      if (impres.Vmag > 90) and (Vest < 90) then impres.Vmag := Vest;
    end;
  end;

  // photometric distance estimate
  buffer1 := Trim(Copy(linein,163,5));
  buffer2 := Trim(Copy(linein,169,4));
  if not StrToRealBoth(buffer1,buffer2,temp1,temp2) then begin
    FreeAndNil(impres);  Exit;
  end;
  if not impres.SetParallaxFromDistance(temp1,temp2) then begin
    FreeAndNil(impres);  Exit;
  end;

  // done
  Result := impres;
end;
//---------------------------------------------
function MakeUCAC4_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idmaker := nil;
  Result.idheader := 'ID';
  // special
  Result.parser := ParseUCAC4SearchLine;
  Result.lineskip := 0;
  // BVRI
  Result.useB_Flux := True;
  Result.useVRI_Flux := True;
  // source info
  Result.pllx_sourceid := 'UCAC4 NSS';
  Result.fullname := 'UCAC4 nearby star survey (Finch+ 2014)';
  Result.paperurl := 'http://iopscience.iop.org/article/10.1088/0004-6256/148/6/119/pdf';
  Result.fullout := 'ucac4_nss.csv';
  Result.leftout := 'ucac4_nss_leftover.csv';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//-------------------------
function ParseSN39Line(linein:string):ImportedData;
var itemlist:TStringList;
    curname,buffer1,buffer2:string;
    impres:ImportedData;
    spos:Integer;
    tempr:Real;
begin
  Result := nil;
  if Length(linein) < 120 then Exit;
  itemlist := SplitWithSpaces(linein,19);
  if itemlist = nil then Exit;
  // idname
  curname := itemlist[0];
  curname := ImproveNames(curname);
  // ra and dec in decimal degrees
  buffer1 := itemlist[1] + ' ' + itemlist[2] + ' ' + itemlist[3];
  buffer2 := itemlist[4] + ' ' + itemlist[5] + ' ' + itemlist[6];

  // setting up the object
  impres := ImportedData.Create;
  impres.AddNameID('',curname);
  impres.rapos := buffer1;
  impres.decpos := buffer2;

  // Field 15 contains the parallax
  buffer1 := itemlist[15];
  if not SplitConvPM(buffer1,impres.pllx,impres.pllx_err) then begin
     itemlist.Free;  impres.Free;
     Exit;
  end;
  // Fields 16 and 17 contains the proper motion magnitude
  buffer1 := itemlist[16];  // ends with ±
  spos := AnsiPos('±',buffer1);
  if spos < 2 then begin
     itemlist.Free;  impres.Free;
     Exit;
  end;
  buffer2 := Trim(AnsiLeftStr(buffer1,spos-1));
  if not StrToReal(buffer2,impres.pm_mag) then begin
     itemlist.Free;  impres.Free;
     Exit;
  end;
  // Field 18 contains proper motion angle
  buffer1 := itemlist[18];
  itemlist.Free;      // this is the last field we use
  if not SplitConvPM(buffer1,impres.pm_ang,tempr) then begin
     impres.Free;
     Exit;
  end;
  // done
  Result := impres;
end;
//-------------------------
function MakeSN39_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'ID/Name';
  Result.parser := ParseSN39Line;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  // source info
  Result.pllx_sourceid := 'SN 39';
  Result.fullname := 'The Solar Neighborhood. XXXIX. Parallax Results from the CTIOPI and NOFS Programs: 50 New Members of the 25 Parsec White Dwarf Sample';
  Result.paperurl := 'https://arxiv.org/abs/1706.00709';
  Result.fullout := 'sn39.csv';
  Result.leftout := 'sn39_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* A catalog of GALEX ultraviolet emission from spectroscopically confirmed
M dwarfs.  Jones D.O., West A.A. (2016)
table3.dat at CDS includes Photometric distance estimates for Palomar/MSU
dwarfs, which are nearer than the SDSS dwarfs in table2.dat  *)
function ParseGALEX_3Line(linein:string):ImportedData;
var name1,name2,sptx:string;
    buffer1,buffer2:string;
    impres:ImportedData;
    rtemp1,rtemp2:Real;
begin
  Result := nil;
  if Length(linein) < 241 then Exit;
  // names
  name1 := Trim(Copy(linein,1,19));
  name2 := Trim(Copy(linein,21,9));
  name2 := ImproveNames(name2);
  // spectral type
  sptx := Copy(linein,31,3);
  // position (I will use the Palomar/MSU positions)
  buffer1 :=  Trim(Copy(linein,59,11));
  buffer2 :=  Trim(Copy(linein,71,11));
  // setting up the output object
  impres := ImportedData.Create;
  impres.AddNameID('',name2);
  impres.stype := 'M' + sptx + 'V';
  if (not impres.SetDecimalRA(buffer1)) then begin
     impres.Free;   Exit;
  end;
  if (not impres.SetDecimalDec(buffer2)) then begin
     impres.Free;   Exit;
  end;
  // geting distance info
  buffer1 :=  Trim(Copy(linein,215,4));
  buffer2 :=  Trim(Copy(linein,220,4));
  if (not StrToRealBoth(buffer1,buffer2,rtemp1,rtemp2)) then begin
    impres.Free;   Exit;
  end;
  // converting distance to parallax...
  impres.pllx := 1000*(1/rtemp1);
  rtemp2 := rtemp2/100;
  if rtemp2 = 0 then begin
    impres.Free;   Exit;
  end;
  rtemp2 := rtemp2/(1-rtemp2);
  impres.pllx_err := impres.pllx*rtemp2;
  // proper motion
  buffer1 := Trim(Copy(linein,225,8));
  buffer2 := Trim(Copy(linein,234,8));
  if (not impres.SetProperMotionPartsM(buffer1,buffer2)) then begin
      impres.Free;   Exit;
  end;
  // done
  Result := impres;
end;
//---------------------------------------------------
function MakeGALEX3_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'ID/Name';
  Result.parser := ParseGALEX_3Line;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  Result.useSpT := True;
  // source info
  Result.pllx_sourceid := 'GALEX M';
  Result.fullname := 'A catalog of GALEX ultraviolet emission from spectroscopically confirmed M dwarfs.';
  Result.paperurl := 'https://arxiv.org/abs/1509.03645';
  Result.fullout := 'galexm3.csv';
  Result.leftout := 'galexm3_leftover.csv';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* CONCH-SHELL catalog of nearby M dwarfs   (Gaidos+, 2014)
table3.dat from http://cdsarc.u-strasbg.fr/viz-bin/Cat?J/MNRAS/443/2561
*)
function ParseConchShell_Line(linein:string):ImportedData;
var name1x,name1,name2,name3:string;
    buffer1,buffer2:string;
    impres:ImportedData;
    rtemp1,rtemp2,rtemp3:Real;
    tempC1,tempC2,Best:Currency;
    Vest:Real;
begin
  Result := nil;
  if Length(linein) < 374 then Exit;
  // names LSPM, Hip, Tyc
  name1x := Trim(Copy(linein,4,13));   // SUPERBLINK NAME
  name1 := 'J' + Copy(name1x,2,4) + Copy(name1x,7,Length(name1x)-6+1);
  name2 := Trim(Copy(linein,18,6));
  name3 := Trim(Copy(linein,25,4));
  name3 += '-' + Trim(Copy(linein,30,4)) + '-' + Copy(linein,35,1);
  // right ascension and declination
  buffer1 := Trim(Copy(linein,37,10));
  buffer2 := Trim(Copy(linein,48,9));
  if (Length(buffer1)<10) or (Length(buffer2)<9) then Exit;

  // setting up the output object
  impres := ImportedData.Create;
  impres.AddNameID('LSPM',name1);
  // to simplify, I'll leave off handling Hip and Tyc until later
  // (because I want to insert 2MASS before, Hip and Tyc might be empty...)
  impres.rapos := buffer1;
  impres.decpos := buffer2;

  // parallax in arcsec
  buffer1 := Trim(Copy(linein,58,6));
  buffer2 := Trim(Copy(linein,65,6));
  if (not StrToRealBoth(buffer1,buffer2,rtemp1,rtemp2)) then begin
     impres.Free;  Exit;
  end;
  if (rtemp1<=0) or (rtemp2<=0) then begin
     impres.Free;  Exit;
  end;
  impres.pllx := 1000*rtemp1;
  impres.pllx_err := 1000*rtemp2;

  // proper motion in arcsec
  buffer1 := Trim(Copy(linein,77,6));
  buffer2 := Trim(Copy(linein,84,6));
  if (not impres.SetProperMotionPartsS(buffer1,buffer2)) then begin
     impres.Free;  Exit;
  end;

  // APASS magnitudes...
  // B
  if (not SubstrCurr(linein,103,5,tempC1)) then begin
     impres.Free;  Exit;
  end;
  if (not SubstrCurr(linein,109,5,tempC2)) then begin
     impres.Free;  Exit;
  end;
  if (tempC1>0) then begin
    impres.Bmag := tempC1;  impres.BMagE := tempC2;
  end;
  // V
  buffer1 := Trim(Copy(linein,115,5));
  buffer2 := Trim(Copy(linein,121,5));
  if (not StrToRealBoth(buffer1,buffer2,rtemp1,rtemp2)) then begin
     impres.Free;  Exit;
  end;
  if rtemp1 > 0 then begin
    impres.Vmag := rtemp1;  impres.VmagE := rtemp2;
  end;
  // g r i
  buffer1 := Trim(Copy(linein,127,5));
  buffer2 := Trim(Copy(linein,139,5));
  if (not StrToRealBoth(buffer1,buffer1,rtemp1,rtemp2)) then begin
     impres.Free;  Exit;
  end;
  buffer1 := Trim(Copy(linein,151,5));
  if (not StrToReal(buffer1,rtemp3)) then begin
     impres.Free;  Exit;
  end;
  // converting g r i to Rc and Ic
  if (rtemp1>0) and (rtemp2>0) and (rtemp3>0) then begin
    // now, we convert SDSS gri to estimates for V,B,Rc, and Ic
    Pgri_to_BVRI(rtemp1,rtemp2,rtemp3,Best,Vest,impres.Rmag,impres.Imag);
    if impres.Bmag > 90 then impres.Bmag := Best;
  end;
  // spectral type...     kinda crude
  buffer1 := Trim(Copy(linein,201,2));
  if linein[204] <> 'S' then buffer1 += 'J';
  impres.stype := buffer1;
  // Fe/H
  buffer1 := Trim(Copy(linein,298,5));
  buffer2 := Trim(Copy(linein,304,5));
  // Teff
  buffer1 := Trim(Copy(linein,310,4));
  if (not StrToReal(buffer1,rtemp1)) then begin
     impres.Free;  Exit;
  end;
  impres.teff := Round(rtemp1);
  // 2MASS id
  name1 := Trim(Copy(linein,359,16));
  name1 := 'J' + name1;
  // finishing off the names
  impres.AddNameID('2MASS',name1);
  if (name3 <> '0-0000-0') then impres.AddNameID('Tyc',name3);
  if (name2 <> '0') then impres.AddNameID('Hip',name2);
  // done
  Result := impres;
end;
//-------------------------------------------------------
function ConchShell_CSVNames(inputl:TStringList):string;
var inputlc:Integer;
begin
  // bad cases
  Result := ',,,';
  if (inputl = nil) then  Exit;
  inputlc := inputl.Count;
  if inputlc = 0 then Exit;
  if inputlc > 4 then Exit;
  // LSPM and 2MASS
  Result := inputl[0] + ',' + inputl[1] + ',';
  // three items or more
  if inputlc = 2 then Result += ','
  else begin
    if AnsiStartsStr('Tyc',inputl[2]) then Result += inputl[2] + ','
    else Result += ',';
    if inputlc = 4 then Result += inputl[3];
  end;
  // done

end;
//-------------------------------------------------------
function MakeConchShell_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'LSPM,2MASS,Tycho,Hipparcos';
  Result.idmaker := ConchShell_CSVNames;
  Result.parser := ParseConchShell_Line;
  Result.lineskip := 0;
  Result.useSimbadLocation := True;
  Result.useB_Flux := True;
  Result.useVRI_Flux := True;
  Result.useTeff := True;

  // source info
  Result.pllx_sourceid := 'CONCH-SHELL';
  Result.fullname := 'Trumpeting M dwarfs with CONCH-SHELL: a catalogue of nearby cool host-stars for habitable exoplanets and life.';
  Result.paperurl := 'https://arxiv.org/abs/1406.7353';
  Result.fullout := 'conchshell.csv';
  Result.leftout := 'conchshell_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* TODO :  RAVE  *)
(* RAVE DR5 extract from Vizier *)
function ParseRAVEDR5Extract_Line(linein:string):ImportedData;
var slist:TStringList;
    impres:ImportedData;
    rtemp1,rtemp2,rtemp3:Real;
    ctemp1,ctemp2,Best:Currency;
    buffer:string;
    Vest:Real;
begin
  Result := nil;
  if Length(linein) < 308 then Exit;
  // the file is semicolom delimited, and has 40 fields
  slist := SplitWithDelim(linein,';',40);
  if slist = nil then Exit;
  // position is the first 2 fields (and the last 2)
  impres := ImportedData.Create;
  if (not impres.SetDecimalRA(slist[0])) then begin
    slist.Free;  impres.Free;  Exit;
  end;
  if (not impres.SetDecimalDec(slist[1])) then begin
    slist.Free;  impres.Free;  Exit;
  end;
  // radial velocity
  if (not StrToReal(slist[3],impres.radialv)) then begin
     slist.Free;  impres.Free;  Exit;
  end;
  // 3 different TEffs are included... which often contradict. I'll add an average
  if (not StrToRealBoth(slist[5],slist[6],rtemp1,rtemp2)) then begin
     slist.Free;  impres.Free;  Exit;
  end;
  // skipping over log g and M/H to get the 3rd temp
  rtemp3 := -9;
  StrToReal(slist[11],rtemp3);
  if (rtemp3 > 100) then impres.teff := Round((rtemp1+rtemp2+rtemp3)/3)
  else impres.teff := Round((rtemp1+rtemp2)/2);
  // RAVE spectromorphic parallax
  if (not StrToRealBoth(slist[17],slist[18],impres.pllx,impres.pllx_err)) then begin
     slist.Free;  impres.Free;  Exit;
  end;
  // Compiling names...
  impres.AddNameID('RAVE',slist[2]);
  buffer := Trim(slist[29]);
  impres.AddNameID('UCAC4',buffer);
  buffer := 'J' + Trim(slist[30]);
  impres.AddNameID('2MASS',buffer);
  buffer := Trim(slist[28]);
  if Length(buffer)>0 then impres.AddNameID('Tyc',buffer);
  buffer := Trim(slist[38]);
  if Length(buffer)>0 then impres.AddNameID('DENIS',buffer);
  buffer := Trim(slist[27]);
  if Length(buffer)>0 then impres.AddNameID('Hip',buffer);
  // Fluxes from APASS (if present)
  // B
  if StrToCurrBoth(slist[31],slist[32],ctemp1,ctemp2) then begin
     impres.Bmag := ctemp1;
     impres.BMagE := ctemp2;
  end;
  // V
  if StrToRealBoth(slist[33],slist[34],rtemp1,rtemp2) then begin
    impres.Vmag := rtemp1;
    impres.VmagE := rtemp2;
  end;
  // g r i
  if StrToRealBoth(slist[35],slist[36],rtemp1,rtemp2) then begin
    if StrToReal(slist[37],rtemp3) then begin
       // now, we convert SDSS gri to estimates for V,B,Rc, and Ic
       Pgri_to_BVRI(rtemp1,rtemp2,rtemp3,Best,Vest,impres.Rmag,impres.Imag);
       if impres.Bmag > 90 then impres.Bmag := Best;
       if impres.Vmag > 90 then impres.Vmag := Vest;
    end;
  end;
  // done
  slist.Free;
  Result := impres;
end;
//------------------------------------------
function RAVEDR5Extract_CSVNames(inputl:TStringList):string;
var inputlc:Integer;
    exdex:Integer;
begin
  // bad cases
  Result := ',,,';
  if (inputl = nil) then  Exit;
  inputlc := inputl.Count;
  if inputlc < 3 then Exit;
  if inputlc > 6 then Exit;
  // RAVE, UCAC4, and 2MASS
  Result := inputl[0] + ',' + inputl[1] + ',' + inputl[2] + ',';
  // three items or more
  if inputlc = 3 then Result += ',,'
  else begin
    exdex := 3;
    if AnsiStartsStr('Tyc',inputl[3]) then begin
       Result += inputl[3] + ',';
       Inc(exdex);
    end else Result += ',';
    if inputlc = 4 then Result += ','
    else begin
      if AnsiStartsStr('DENIS',inputl[exdex]) then begin
        Result += inputl[exdex] + ',';
        Inc(exdex);
      end else Result += ',';
      if exdex = (inputlc + 1) then Result += inputl[exdex];
    end;
  end;
  // done
end;
//------------------------------------------
function MakeRAVEDR5Extract_Const():ImportParameters;
begin
  Result := ImportParameters.Create;
  // extra write info
  Result.idheader := 'RAVE,UCAC4,2MASS,Tycho,DENIS,Hipparcos';
  Result.idmaker := RAVEDR5Extract_CSVNames;
  Result.parser := ParseRAVEDR5Extract_Line;
  Result.lineskip := 117;
  Result.useSimbadLocation := True;
  Result.useB_Flux := True;
  Result.useVRI_Flux := True;
  Result.useTeff := True;
  Result.useRadv := True;
  Result.useSimbadProperMotion := True;

  // source info
  Result.pllx_sourceid := 'RAVE DR5';
  Result.fullname := 'The Radial Velocity Experiment (RAVE): fifth data release.';
  Result.paperurl := 'https://arxiv.org/abs/1609.03210';
  Result.fullout := 'rave5.csv';
  Result.leftout := 'rave5_leftover.csv';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* 'CCD Parallaxes for 309 Late-type Dwarfs and Subdwarfs' (Dahn+ 2017)
tab1.txt from the extra data. Uses code NOFS-17, since the parallaxes
are from observations made by the Naval Observatory Flagstaff Station *)
//----------------------------------------------------------
function ParseNOFS17_Line(linein:string):ImportedData;
var slist:TStringList;
    impres:ImportedData;
    buffer1,buffer2:string;
begin
  Result := nil;
  if Length(linein) < 162 then Exit;
  // the file is in fixed colums with 22 to 23 fields
  // names
  buffer1 := Trim(Copy(linein,1,16));
  buffer2 := Trim(Copy(linein,18,20));
  buffer2 := AnsiReplaceStr(buffer2,'TYC','Tyc');
  // setting the names
  impres := ImportedData.Create;
  if Length(buffer1)>0 then impres.AddNameID('2MASS','J'+buffer1);
  if Length(buffer2)>0 then impres.AddNameID('',buffer2);
  // right ascension
  impres.rapos := Copy(linein,39,11);
  // declination
  buffer2 := linein[51] + Copy(linein,53,10);
  impres.decpos := buffer2;
  // relative proper motion (source does not give absolute pm)
  buffer1 := Trim(Copy(linein,118,6));
  if not StrToReal(buffer1,impres.pm_mag) then begin
    impres.Free;   Exit;
  end;
  // relative proper motion angle
  buffer2 := Trim(Copy(linein,129,5));
  if not StrToReal(buffer2,impres.pm_ang) then begin
    impres.Free;   Exit;
  end;
  // finally, the parallax
  buffer1 := Trim(Copy(linein,139,6));
  buffer2 := Trim(Copy(linein,146,4));
  if not StrToRealBoth(buffer1,buffer2,impres.pllx,impres.pllx_err) then begin
    impres.Free;   Exit;
  end;
  // done
  Result := impres;
end;
//----------------------------------------------------------
// 2 names max
function NOFS17_CSVNames(inputl:TStringList):string;
begin
  Result := ',';
  if inputl.Count > 0 then begin
    if AnsiStartsStr('2MASS',inputl[0]) then Result := inputl[0] + ','
    else Result := ',' + inputl[0];
    if inputl.Count > 1 then Result += inputl[1];
  end;
end;
//----------------------------------------------------------
function MakeNOFS17_Const():ImportParameters;
begin
    Result := ImportParameters.Create;
    // extra write info
    Result.idheader := '2MASS,Other ID';
    Result.idmaker := NOFS17_CSVNames;
    Result.parser := ParseNOFS17_Line;
    Result.lineskip := 70;
    Result.useSimbadLocation := True;
    Result.useSimbadProperMotion := True;   // NOFS 2017 has 'relative' proper motion

    // source info
    Result.pllx_sourceid := 'NOFS-17';
    Result.fullname := 'CCD Parallaxes for 309 Late-type Dwarfs and Subdwarfs (Dahn+ 2017)';
    Result.paperurl := 'https://arxiv.org/abs/1709.02729v1';
    Result.fullout := 'nofs17.csv';
    Result.leftout := 'nofs17_leftover.csv';
end;
//============================================================================
begin
  urat_params := MakeURAT_Const();
  sn35_params := MakeSN35_Const();
  newhip_params := MakeNewHipparcos_Const();
  siwd_params := MakeSIWD_Const();
  sn35_2_params := MakeSN35T2_Const();
  lepgad_params := MakeLepBMDC_Const();
  ucac4nss_params := MakeUCAC4_Const();
  sn39_params := MakeSN39_Const();
  galex3_params := MakeGALEX3_Const();
  conchshell_params := MakeConchShell_Const();
  rave5_params := MakeRAVEDR5Extract_Const();
  nofs17_params := MakeNOFS17_Const();
end.

