unit ExtraImports;

{$mode delphi}

(* import data not related to parallax *)

interface

uses
  Classes, SysUtils, df_strings, Utilities, newImports2, simbad;

type

// MEarth photometry info from:
// MEarth photometry: nearby M-dwarf magnitudes   (Dittmann+, 2016)

MEarthPhotoData = class
  protected
    function specfehs:Boolean;
    function relafehs:Boolean;
  public
    matched:Boolean;
    // the data
    id_2mass,id_lspm:string;
    mearth_mag,mearth_magerr:Currency;
    spectral:string;
    FeH,FeHErr:Currency;
    FeHEst:Currency;
    // properties
    property SpecFeHSet:Boolean read specfehs;
    property PhotoFeHSet:Boolean read relafehs;
    // methods
    constructor Create;
    function SetFromLine(const in_line:string):Boolean;
    function ToCVS(writematch:Boolean):string;
end;

MEarthPhotoList = class
  protected
    // data
    starlist:array of MEarthPhotoData;
    starcount,starindex:Integer;
    // property methods
    function GPStarCount:Integer;
    function CXItem:MEarthPhotoData;
    // helper methods
    procedure DeleteData;
  public
    // properties
    property ImportedStarCount:Integer read GPStarCount;
    property CurrentStar:MEarthPhotoData read CXItem;
    // methods
    constructor Create();
    function LoadFromFile(inname:TFileName; in_skiplines:Integer):Boolean;
    function WriteToFile(outname:TFileName; unmatchedo:Boolean):Integer;
    procedure ResetIndex;
    function NextStar:Boolean;
    destructor Destroy; override;
end;
//-------------------------------------------------------------------
(* combined datatypes for : Meeting the cool neighbors. X. Ultracool dwarfs
from the 2MASS all-sky data release. (Reid N.I., Cruz K.L., Kirkpatrick J.D.,
Allen P.R., Mungall F.,Liebert J., Lowrance P., Sweet A. | 2008)
Tables 2,3,4 *)
CoolNeighboursData = class
  protected
    // internal table specific parsing
    function Table2LineParse(const in_line:string):Boolean;
    function Table3LineParse(const in_line:string):Boolean;
    function Table4LineParse(const in_line:string):Boolean;
  public
    matched:Boolean;
    // ids
    id_2mass,id_2mucd,id_other:string;
    Jmag_2mass:Currency;
    spectral,new_spectral:string;
    distance,distance_err:Currency;
    approxdist:Boolean;
    // methods
    constructor Create;
    function SetFromLine(const in_line:string; tablenum:Integer):Boolean;
    procedure GetParallax(out pllx,range:Real);
    function ToCVS(writematch:Boolean; parsecdist:Boolean):string;
end;
//---------------------------------------------------------------
Sn35Photometry = class
  protected
  public
    name:string;
    rapos,decpos:string;
    Vmag:Real;
    Rc,Ic:Currency;

    constructor Create;
    function SetFromLine(linein:string):Boolean;
    function FindSimbadData():SimbadData;
    function HasRI:Boolean;
    function TestReplace(const current:Currency; const infr:Boolean):Boolean;
end;
//----------------------------
Sn35PhotoList = class
  protected
    data:array of Sn35Photometry;
    data_index,data_count:Integer;
    // property methods
    function GCount:Integer;
    function CItem:Sn35Photometry;
    function VIndex:Boolean;
  public
    updated,skipped,notfound:Integer;
    // properties
    property StarCount:Integer read GCount;
    property CurrentStar:Sn35Photometry read CItem;
    property ValidIndex:Boolean read VIndex;
    // public
    constructor Create;
    destructor Destroy;
    // public methods
    procedure EmptyData;
    function LoadFromFile(fnname:TFilename):Boolean;
    procedure ResetIndex;
    function NextIndex:Boolean;
    procedure UpdateCounts(const wused,not_found:Boolean);
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//===================================================================
function MEarthPhotoData.specfehs:Boolean;
begin   Result := (FeH < 9);   end;
//-------------------------------
function MEarthPhotoData.relafehs:Boolean;
begin   Result := (FeHEst < 9);   end;
//+++++++++++++++++++++++++++++++
// methods
//------------------------
constructor MEarthPhotoData.Create;
begin
  FeH := 9.99;
  FeHErr := 0;
  FeHEst := 9.99;
  matched := False;
end;
//------------------------
function MEarthPhotoData.SetFromLine(const in_line:string):Boolean;
var inlen:Integer;
begin
  // basic checking
  inlen := Length(in_line);
  Result := False;
  if inlen < 227 then Exit;
  // getting the ids
  id_2mass := Copy(in_line,1,16);
  id_lspm := Trim(Copy(in_line,18,11));
  // MEarth Band (similar to I) magnitude
  if (not SubstrCurr(in_line,30,6,mearth_mag)) then Exit;
  if (not SubstrCurr(in_line,37,5,mearth_magerr)) then Exit;
  // spectral type (from literature)
  spectral := Trim(Copy(in_line,118,6));
  // the 'Spectroscopic metallicity' (usually empty)
  if SubstrCurr(in_line,145,6,FeH) then begin
    if (not SubstrCurr(in_line,152,5,FeHErr)) then Exit;
  end;
  // the 'Metallicity from our photometric relation'
  SubstrCurr(in_line,158,6,FeH);
  Result := True;
end;
//------------------------
function MEarthPhotoData.ToCVS(writematch:Boolean):string;
begin
  Result := '2MASS J' + id_2mass + ',' + 'LSPM ' + id_lspm + ',';
  Result += CurrToStrF(mearth_mag,ffFixed,3) + ',';
  Result += CurrToStrF(mearth_magerr,ffFixed,3) + ',';
  Result += spectral + ',';
  if specfehs then begin
    Result += CurrToStrF(FeH,ffFixed,3) + ',';
    Result += CurrToStrF(FeHErr,ffFixed,2) + ',';
  end
  else Result += ',,';
  if relafehs then Result += CurrToStrF(FeHEst,ffFixed,3);
  if writematch then Result += ',' + Bool2Str(matched);
end;
//==================================================================
// property methods
//--------------------
function MEarthPhotoList.GPStarCount:Integer;
begin    Result:= starcount;    end;
//----------------------------------
function MEarthPhotoList.CXItem:MEarthPhotoData;
begin
  if (starindex = -1) or (starindex = starcount) then Result := nil
  else Result := starlist[starindex];
end;
//++++++++++++++++++++++++++++++++++++++++++++++++
// helper methods
procedure MEarthPhotoList.DeleteData;
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
constructor MEarthPhotoList.Create();
begin
  starindex := -1;
  starcount := 0;
end;
//----------------------------
function MEarthPhotoList.LoadFromFile(inname:TFileName; in_skiplines:Integer):Boolean;
var infile:TextFile;
    curline:string;
    curdata:MEarthPhotoData;
    skipcounter:Integer;
    readok:Boolean;
begin
  Result := False;
  // if it exists, clearing old data
  if (starcount > 0) then DeleteData;
  // opening the file
  AssignFile(infile,inname);
  FileMode := fmOpenRead;
  Reset(infile);
  // skipping lines
  if in_skiplines > 0 then begin
    for skipcounter := 1 to in_skiplines do ReadLn(infile,curline);
  end;
  // loop reading and processing
  while (not Eof(infile)) do begin
    ReadLn(infile,curline);
    curdata := MEarthPhotoData.Create;
    readok := curdata.SetFromLine(curline);
    if readok then begin
      Inc(starcount);
      SetLength(starlist,starcount);
      starlist[starcount-1] := curdata;
    end
    else FreeAndNil(curdata);
  end;
  // in the end, a success if we've got at least one star
  Result := (starcount > 0);
end;
//--------------------------------
function MEarthPhotoList.WriteToFile(outname:TFileName; unmatchedo:Boolean):Integer;
var outfile:TextFile;
    curline:string;
    recmax,recdex:Integer;
    written:Integer;
const header1 = '2MASS ID, LSPM ID,MEarth Mag,MEarth Mag Err,Spectra';
      header2 = ',Spec FeH,Spec FeH Err,Est FeH';
begin
  Result := 0;
  // preparing the new file
  AssignFile(outfile,outname);
  Rewrite(outfile);
  // writing the header
  curline := header1 + header2;
  if (not unmatchedo) then curline += ',' + 'Matched';
  WriteLn(outfile,curline);
  // initial value setup
  recmax := starcount -1;
  written := 0;
  // the detail writing loop
  for recdex := 0 to recmax do begin
    if (starlist[recdex].matched) and unmatchedo then Continue;
    curline := starlist[recdex].ToCVS(not unmatchedo);
    WriteLn(outfile,curline);
    Inc(written);
  end;
  // done
  Flush(outfile);
  Close(outfile);
  Result := written;
end;
//----------------------------
procedure MEarthPhotoList.ResetIndex;
begin  starindex:= -1;    end;
//----------------------------
function MEarthPhotoList.NextStar:Boolean;
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
destructor MEarthPhotoList.Destroy;
begin
  DeleteData;
  inherited;
end;
//======================================================================
// internal table specific parsing
//--------------------------------
function CoolNeighboursData.Table2LineParse(const in_line:string):Boolean;
begin
  Result := False;
  // 2MUCD
  id_2mucd := Trim(Copy(in_line,18,5));
  // othernames
  id_other := Trim(Copy(in_line,24,31));
  // 2MASS J magnitude
  if (not SubstrCurr(in_line,56,6,Jmag_2mass)) then Exit;
  // spectral type
  spectral := Trim(Copy(in_line,75,7));
  new_spectral := Trim(Copy(in_line,82,1));
  // distance in parsecs
  if (not SubstrCurr(in_line,96,5,distance)) then Exit;
  if (not SubstrCurr(in_line,102,4,distance_err)) then Exit;
  // done
  Result := True;
end;
//--------------------------------
function CoolNeighboursData.Table3LineParse(const in_line:string):Boolean;
var dpstr:string;
begin
  Result := False;
  // 2MUCD
  id_2mucd := Trim(Copy(in_line,18,5));
  // othernames
  id_other := Trim(Copy(in_line,24,24));
  // 2MASS J magnitude
  if (not SubstrCurr(in_line,49,6,Jmag_2mass)) then Exit;
  // spectral type
  spectral := Trim(Copy(in_line,113,9));
  // distance in parsecs
  dpstr := Trim(in_line[137]);
  approxdist := (dpstr = '~');
  if (not SubstrCurr(in_line,138,5,distance)) then Exit;
  if (not SubstrCurr(in_line,144,4,distance_err)) then Exit;
  // done
  Result := True;
end;
//--------------------------------
function CoolNeighboursData.Table4LineParse(const in_line:string):Boolean;
var dpstr:string;
begin
  Result := False;
  // 2MUCD
  id_2mucd := Trim(Copy(in_line,20,5));
  // othernames
  id_other := Trim(Copy(in_line,26,29));
  // 2MASS J magnitude
  if (not SubstrCurr(in_line,56,6,Jmag_2mass)) then Exit;
  // spectral type
  spectral := Trim(Copy(in_line,98,8));
  // distance in parsecs
  dpstr := Trim(in_line[120]);
  approxdist := (dpstr = '~');
  if (not SubstrCurr(in_line,121,5,distance)) then Exit;
  SubstrCurr(in_line,127,5,distance_err);
  // done
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// methods
//--------------------------------
constructor CoolNeighboursData.Create;
const maxmag:Currency = 99.999;
begin
  matched := false;
  Jmag_2mass := maxmag;
  distance := 0;
  distance_err := 0;
  approxdist := False;
end;
//--------------------------------
function CoolNeighboursData.SetFromLine(const in_line:string; tablenum:Integer):Boolean;
begin
  // initial rejection tests...
  Result := False;
  if tablenum < 2 then Exit;
  if tablenum > 4 then Exit;
  if Length(in_line) < 124 then Exit;
  // 2mass is at the start, and is the same in all tables
  id_2mass := Trim(Copy(in_line,1,16));
  // table specific methods
  if tablenum = 2 then Result := Table2LineParse(in_line)
  else if tablenum = 3 then Result := Table3LineParse(in_line)
  else Table4LineParse(in_line);
end;
//--------------------------------
procedure CoolNeighboursData.GetParallax(out pllx,range:Real);
var rdist,rdisterr:Real;
    rnearpllx,rfarpllx:Real;
    rnearrange,rfarrange:Real;
begin
  // converting to parallax
  rdist := CurrToReal(distance);
  rdisterr := CurrToReal(distance_err);
  rnearpllx := 1/(rdist-rdisterr);
  rfarpllx :=  1/(rdist+rdisterr);
  (* keep the distance, pllx will be 1/rdist, even though the ranges will be
  lopsided *)
  pllx := 1/rdist;
  // getting the ranges
  rnearrange := Abs(rnearpllx-pllx);
  rfarrange := Abs(pllx-rfarpllx);
  // the fnal range will be the biggest...
  if rnearrange > rfarrange then range := rnearrange
  else range := rfarrange;
end;
//--------------------------------
function CoolNeighboursData.ToCVS(writematch:Boolean; parsecdist:Boolean):string;
var pllx,pllx_err:Real;
begin
  // ids, 2MASS J-band Magnitude, and spectral types
  Result := id_2mass + ',' + id_2mucd + ',' + id_other + ',';
  Result += CurrToStrF(Jmag_2mass,ffFixed,3) + ',';
  Result += spectral + ',' + new_spectral + ',';
  // distance related stuff
  if approxdist then Result += '~';
  Result += ',';
  if parsecdist then begin
    Result += CurrToStrF(distance,ffFixed,2) + ',' + CurrToStrF(distance_err,ffFixed,2);
  end
  else begin
    GetParallax(pllx,pllx_err);
    Result += FloatToStrF(pllx,ffFixed,1,6) + ',' + FloatToStrF(pllx_err,ffFixed,1,6);
  end;
  // optional: matched or not?
  if writematch then Result += ',' + Bool2Str(matched);
end;
//======================================================================
(*  name:string;     rapos,decpos:string;
    Vmag:Real;       Rc,Ic:Currency;     *)
constructor Sn35Photometry.Create;
const maxc:Currency = +99.999;
begin
  rapos := '00 00 00.000';
  decpos := '+00 00 00.00';
  Vmag := 99.999;
  Rc := maxc;
  Ic := maxc;
end;
//---------------------------------------------
function Sn35Photometry.SetFromLine(linein:string):Boolean;
var name1,buffer:string;
const maxc:Currency = +99.999;
begin
  Result := False;

  if Length(linein) < 130 then Exit;
  // identifier
  name1 := Trim(Copy(linein,1,14));
  name1 := ImproveNames(name1);
  name := name1;
  // right ascension and declination
  rapos := Copy(linein,20,10);
  decpos := Copy(linein,31,9);

  // plate fluxes will not be used
  // recons fluxes (will be used, if present)
  buffer := Copy(linein,111,5);
  StrToReal(buffer,Vmag);
  SubstrCurr(linein,118,5,Rc);
  SubstrCurr(linein,125,5,Ic);

  // done
  Result := (Vmag < 99) or (Rc < maxc) or (Ic < maxc);
end;
//---------------------------------------------------------
// takes the data and tries to get a SimbadData
function Sn35Photometry.FindSimbadData():SimbadData;
var lookup_url:string;
    discardfail:Boolean;
begin
  Result := nil;
  // starting the lookup...
  lookup_url := MakeSimbadIdLookupURL(name);
  Result := GetSimbadDataURL(lookup_url,discardfail);
  if Result <> nil then Exit;
  // here, we try a location based lookup
  lookup_url := MakeSimbadCoordLookupURL(rapos,decpos,1,False);
  Result := GetSimbadDataURL(lookup_url,discardfail);
end;
//---------------------------------------------------------
function Sn35Photometry.HasRI:Boolean;
begin
  Result := (Rc < 99) or (Ic < 99);
end;
//---------------------------------------------------------
function Sn35Photometry.TestReplace(const current:Currency; const infr:Boolean):Boolean;
var xdiff:Currency;
const maxdiff:Currency = 0.1;
begin
  Result := False;
  // we have no value
  if infr and (Ic > 90) then Exit
  else if (Rc > 90) then Exit;
  // values are the same
  if infr and (Ic = current) then Exit
  else if (Rc = current) then Exit;
  Result := True;
  // quick 'replace' conditions
  if current > 90 then Exit;
  if CurrHasOneDec(current) then Exit;
  // replace if too different
  if infr then xdiff:= CurrAbs(Ic-current)
  else xdiff := CurrAbs(Rc-current);
  if xdiff > maxdiff then Exit;
  // no, don't replace
  Result := False;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(*  data:array of Sn35Photometry;
    data_index,data_count:Integer;  *)
// property methods
//-----------------------------------------
function Sn35PhotoList.GCount:Integer;
begin   Result := data_count;   end;
//-----------------------------------------
function Sn35PhotoList.CItem:Sn35Photometry;
begin
  Result := nil;
  if VIndex then Result := data[data_index];
end;
//-----------------------------------------
function Sn35PhotoList.VIndex:Boolean;
begin  Result := (data_index >= 0) and (data_index < data_count);  end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
(*    updated,skipped,notfound:Integer;   *)
// public
constructor Sn35PhotoList.Create;
begin
  data_count := 0;
  EmptyData;
  inherited;
end;
//-----------------------------------------
destructor Sn35PhotoList.Destroy;
begin  EmptyData;   end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
// public methods
procedure Sn35PhotoList.EmptyData;
var qdex:Integer;
begin
  for qdex := 0 to (data_count-1) do FreeAndNil(data[qdex]);
  SetLength(data,0);
  data_count := 0;   data_index := 0;
  updated := 0;      skipped := 0;
  notfound := 0;
end;
//-----------------------------------------
function Sn35PhotoList.LoadFromFile(fnname:TFilename):Boolean;
var infile:TextFile;
    currline:string;
    curritem:Sn35Photometry;
    rok:Boolean;
begin
  EmptyData;
  Result := False;
  // opening the file
  AssignFile(infile,fnname);
  Reset(infile);
  // reading loop
  while not EoF(infile) do begin
    ReadLn(infile,currline);
    curritem := Sn35Photometry.Create;
    rok := curritem.SetFromLine(currline);
    if rok then begin
      SetLength(data,data_count+1);
      data[data_count] := curritem;
      Inc(data_count);
    end
    else curritem.Free;
  end;
  // finishing...
  CloseFile(infile);
  Result := (data_count>0);
end;
//-----------------------------------------
procedure Sn35PhotoList.ResetIndex;
begin  data_index := -1;   end;
//-----------------------------------------
function Sn35PhotoList.NextIndex:Boolean;
begin
  Result := False;
  if data_index = data_count then Exit;
  Inc(data_index);
  Result := VIndex;
end;
//-----------------------------------------
procedure Sn35PhotoList.UpdateCounts(const wused,not_found:Boolean);
begin
  if not_found then Inc(notfound)
  else begin
    if wused then Inc(updated)
    else Inc(skipped);
  end;
end;
//======================================================================
end.

