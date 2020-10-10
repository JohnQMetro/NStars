unit simbad;

{$mode delphi}
(* I find myself having to use the CDS Simbad service (more up to date than
ARICNS) to lookup extra catalog names, check the radial velocity and magnitudes,
and sometimes lookup stars based on positions. To save time, I want to semi-
automate some of this stuff *)

interface

uses
  Classes, SysUtils, StrUtils, Math, fgl,
  df_strings, StringParser, fluxtransform, unitdata, Utilities, Utilities2,
  fluxtransform2;

type

SimbadData = class
  protected
    // data that is gotten
    namestr:string;     // header name (sometime includes useful info)
    group:Boolean;   // true if siblings/parents
    radial:Double; // radial velocity in km/s
    spectral:string;       // spectral type
    vmag:Double;           // visual magnitude
    raw_catalog_list:TStringList;      // list of names and catalogs
    processed_catalogs:TFPGMap<string,string>;  // the above, processed
    extranames:string;     // extracted NAME, bayer, flamsteed
    ootypes:string; // other object types code list
    xarity:ArityType;

    // effective temperature
    teff_count:Integer;
    teffavg,teffmed,teffpm:Integer;

    // [Fe/H] dex
    feh_count:Integer;
    fehavg,fehmed,fehpm:Currency;

    // extra fluxes (currency is to avoid conversion errors)
    umag,bmag,berr,rmag:Currency;   // visible fluxes , B gets an error
    gamag:Currency; // GAIA G-mag
    imag,jmag,hmag,kmag,kerr:Currency;   // infared fluxes

    // property functions
    function GetNamestr:string;
    function GetSpectral:string;
    function GetGroup:Boolean;
    function GetRadial:Double;
    function GetVMag:Double;
    function GetNames:string;
    // more properties
    function GTempC:Integer;
    function HsTemp:Boolean;
    function GTRangeStr:string;
    function GTempAvg:Real;
    function GFeHC:Integer;
    function HsFeH:Boolean;
    function GtFeHAvg:Currency;
    function GetFeHMed:Currency;
    function GetFeHPM:Currency;
    function GetArr:ArityType;
    // extra flux properties
    function UMagG:Currency;
    function BMagG:Currency;
    function BMagErrG:Currency;
    function GAMagG:Currency;
    function RMagG:Currency;
    function IMagG:Currency;
    function JMagG:Currency;
    function HMagG:Currency;
    function KMagG:Currency;
    function KMagErrG:Currency;

    // helper for process identifiers
    function DoAddCatalog(var ctag:string; var iddata:string):Boolean;
    function RawListToString:string;
    // more helpers
    function GetFluxSrc(inrawsrc:string):string;
    function ExtractFlux(var xparser:StringParsing; out flc:Char; out fvalue,ferror:Currency; out src:string):Boolean;
    function AssignFlux(const fchar:Char; value,error:Currency):Boolean;
    function ParseExtractCatalogs(var sourcedata:StringParsing):Boolean;
    function ParseFeHLine(cinline:string; out teff:Real; out feh,logg:Currency):Boolean;
    function ParseFeHBlock(inblock:string):Boolean;
    function MakeFluxString:string;
  public
    hasplanet:Boolean;
    ra_coord,dec_coord:string;
    pmra,pmdec:string;
    logg:Currency;
    isprms:Boolean;
    // constructor and destructor
    constructor Create;
    destructor Destroy; override;
    // informational properties
    property StarTitle:string read GetNamestr;
    property SpectralType:string read GetSpectral;
    property inGroup:Boolean read GetGroup;
    property RadialVelocity:Double read GetRadial;
    property VisualMag:Double read GetVMag;
    property Names:string read GetNames;
    property Arity:ArityType read GetArr;
    // effective temperature and Fe/H properties
    property EffTempCount:Integer read GTempC;
    property HasEffTemp:Boolean read HsTemp;
    property EffTempRangeStr:string read GTRangeStr;
    property EffectiveTempAvg:Real read GTempAvg;
    property FeHCount:Integer read GFeHC;
    property HasFeH:Boolean read HsFeH;
    property AverageFeH:Currency read GtFeHAvg;
    property MedianFeH:Currency read GetFeHMed;
    property FeHError:Currency read GetFeHPM;
    // extra flux properties
    property UltravioletMagnitude:Currency read UMagG;
    property BlueMagnitude:Currency read BMagG;
    property BlueMagnitudeError:Currency read BMagErrG;
    property GaiaGMagnitude:Currency read GAMagG;
    property RedMagnitude:Currency read RMagG;
    property InfraredMagnitude:Currency read IMagG;
    property JMagnitude:Currency read JMagG;
    property HMagnitude:Currency read HMagG;
    property KMagnitude:Currency read KMagG;
    property KMagnitudeError:Currency read KMagErrG;
    // output methods
    function OutputCatalogCVS:string;
    function MakeSummaryString(fluxonly:Boolean):string;
    function MakeSummaryLoggString():string;
    function VBRIString():string;
    // parsing input
    function ParseFromHTMLPage(input:string):Boolean;
    function ProcessIdentifiers:Integer;
    // extra
    function NameContains(const findstr:string):Boolean;
    function MakeFeHString:string;
end;

function MakeSimbadCoordLookupURL(const rastring,decstring:string; arcminp:Integer; inEpoch:EpochType):string;
function MakeSimbadIdLookupURL(ident:string):string;
function GetSimbadDataURL(inurl:string; out fetchfail:Boolean):SimbadData;

const ok_catalogs:array[0..72] of string = ('BD','CD','CPD','Wolf','Ross','FK5',
      'HD','HR','SAO','LHS','LFT','LTT','Vys','2MASS','DENIS','Luhman','Lal',
      'ADS','LDS','AC','Lac','LP','Gmb','L','SO','SCR','LPM','Kr','WISE',
      'PPM','Zkh','Sm','WDS','G','Sa','NLTT','VVO','Ruiz','Wor','Heintz',
      'Struve','Bu','Tou','SIPS','WISEP','SDSS','VB','WD','LEHPM','WT','SIP',
      'BPM','LSPM','San','2MUCD','APMPM','BRI','AG','UCAC4','GSC','GD',
      'KIC','Ton','ULAS','PM','WISEA','RAVE','RX','GSC2.3','1RXS','URAT1',
      'UPM','SSSPM');

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//===================================================================
// helper for process identifiers
function SimbadData.DoAddCatalog(var ctag:string; var iddata:string):Boolean;
var gjint,egint,sc:Integer;   gjreal:Real;
    endid,reddata:String;
begin
  // regular cases, catalog ids in our pre-approved list
  Result := True;
  if AnsiMatchStr(ctag,ok_catalogs) then begin
    iddata := AnsiReplaceStr(iddata,',','$');
    Exit;
  end;
  // cases where we need to alter the tag
  if ctag = 'HIP' then ctag := 'Hip'
  else if ctag = 'PLX' then ctag := 'YPC'
  else if ctag = 'MCC' then ctag := 'Vys'
  else if ctag = 'Smethells' then ctag := 'Sm'
  else if ctag = 'MR' then ctag := 'Rob'
  else if ctag = 'KUI' then ctag := 'Kpr'
  else if ctag = 'TYC' then ctag := 'Tyc'
  else if ctag = 'StKM' then begin
    if AnsiStartsStr('1-',iddata) then begin
      ctag := 'Steph';
      iddata := Trim(RightStr(iddata,Length(iddata)-2));
    end
    else Result := False;
  end
  else if ctag = 'GJ' then begin
    // converting GJ back into Gl, Wo, and GJ
    endid := RightStr(iddata,1);
    if AnsiContainsStr('ABCDEF',endid) then reddata := Trim(LeftStr(iddata,Length(iddata)-1))
    else reddata := iddata;
    Val(reddata,gjint,sc);
    if (sc<>0) then begin
      Val(reddata,gjreal,sc);
      Assert(sc=0);
      if gjreal > 9000 then ctag := 'Wo'
      else if gjreal < 1000 then ctag := 'Gl'; // Gliese Catalogue 2nd Ed (1969)
    end
    else if (gjint < 1000) then ctag := 'Gl'   // Gliese Catalogue 1st Ed (1957)
    else if (gjint > 9000) then ctag :=  'Wo';  // Woolley's extension (1970)
    // anything else is still GJ (Gliese–Jahreiß)
  end
  else if ctag = 'EGGR' then begin
    // converting EGGR to EG (less than 203) and Gr (Greenstein)
    endid := RightStr(iddata,1);
    if AnsiContainsStr('ABCabc',endid) then reddata := Trim(LeftStr(iddata,Length(iddata)-1))
    else reddata := iddata;
    Val(reddata,egint,sc);
    if sc<>0 then ctag := 'EG' // should not happen
    else if egint < 203 then ctag := 'EG'
    else ctag := 'Gr';
  end
  else if ctag = 'Gaia' then begin
    // gaia idents have two words
    if AnsiStartsStr('DR1',iddata) then ctag := 'Gaia DR1'
    else if AnsiStartsStr('DR2',iddata) then ctag := 'Gaia DR2'
    else if AnsiStartsStr('DR3',iddata) then ctag := 'Gaia DR3'  // forthcoming
    else Result := False;
    if Result then iddata := Trim(RightStr(iddata,Length(iddata)-3));
  end
  // double star catalogs should have the right info
  else if ctag = '**' then begin
    ExtractFirstWord(iddata,ctag);
    if ctag ='BU' then ctag := 'Bu'
    else if ctag = 'CIN' then ctag := 'Cin'
    else if ctag = 'COU' then ctag := 'Cou'
    else if ctag = 'EGG' then ctag := 'Eggen'
    else if ctag = 'GRB' then ctag := 'Gmb'
    else if ctag = 'HEI' then ctag := 'Heintz'
    else if ctag = 'KRU' then ctag := 'Kr'
    else if ctag = 'KUI' then ctag := 'Kpr'
    else if ctag = 'LCL' then ctag := 'Lac'
    else if ctag = 'LUH' then ctag := 'Luhman'
    else if ctag = 'LUY' then ctag := 'L'
    else if ctag = 'MLT' then ctag := 'Mel'
    else if ctag = 'MR'  then ctag := 'Rob'
    else if ctag = 'RBR' then ctag := 'Rob'
    else if ctag = 'SDK' then ctag := 'San'
    else if ctag = 'STF' then ctag := 'Struve'
    else if ctag = 'STT' then ctag := 'O.Struve'
    else if ctag = 'VBS' then ctag := 'VB'
    else if ctag = 'WOR' then ctag := 'Wor'
    else if ctag = 'SOZ' then ctag := 'Scholz'
    else if ctag = 'STN' then ctag := 'Stone'
    else if ctag = 'STI' then ctag := 'Stein'
    else if ctag = 'BAG' then ctag := 'Balega'
    else if ctag = 'TOK' then ctag := 'Tok'
    else if ctag = 'WG' then ctag := 'Wg'
    // else if ctag = 'SHJ' then ctag := 'HJ'
    else if (ctag<>'H') and (ctag<>'HJ') and (ctag<>'I') and
    (ctag <> 'LDS') then Result := False;
  end
  // otherwise...
  else begin
    Result := False; // some catalogs, I just do not use
    // however, there might be extra names
    if (ctag = 'NAME') or (ctag = '*') or (ctag = 'V*') then begin
      if Length(extranames)<>0 then extranames += ',';
      extranames += iddata;
    end;
  end;
  // finally, removing any commas
  iddata := AnsiReplaceStr(iddata,',','$');
end;
//----------------------------------
function SimbadData.RawListToString:string;
var catdex, catmax:Integer;
begin
  Result := '';
  if raw_catalog_list = nil then Exit;
  catmax := (raw_catalog_list.Count div 2)-1;
  // the output loop
  for catdex := 0 to catmax do begin
      Result := Result + raw_catalog_list[catdex*2] + ' ';
      Result := Result + raw_catalog_list[catdex*2+1];
      if catdex <> catmax then Result := Result + ',';
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
// more helpers
//-----------------------
function SimbadData.GetFluxSrc(inrawsrc:string):string;
var rpos,endpos:Integer;
begin
  Result := '';
  // finding the start
  rpos := AnsiPos('">',inrawsrc);
  if rpos <=0 then Exit;
  rpos := PosEx('">',inrawsrc,rpos+2);
  if rpos <=0 then Exit;
  rpos += 2;
  // finding the end
  endpos := PosEx('</A>',inrawsrc,rpos);
  if endpos <= 0 then Exit;
  // extracting
  Result := Trim(Copy(inrawsrc,rpos,endpos-rpos));
end;
//----------------------------
function SimbadData.ExtractFlux(var xparser:StringParsing; out flc:Char; out fvalue,ferror:Currency; out src:string):Boolean;
var buffer:string;  sc:Integer;
begin
  Result := False;
  // getting to a flux
  if (not xparser.MovePastTwo('<TR>','<TT>')) then Exit;
  if (not xparser.GetMovePast(buffer,'[')) then Exit;
  buffer := Trim(buffer);
  if Length(buffer)<2 then Exit;
  // we've got a flux! (I think)
  flc := buffer[1];
  buffer := Trim(AnsiRightStr(buffer,Length(buffer)-1));
  if (Length(buffer) = 0) then Exit;
  // buffer should contain a numeric value now...
  Val(buffer,fvalue,sc);
  if sc<>0 then Exit;
  // the error might be useful
  if (not xparser.GetMovePast(ferror,']')) then ferror := 0;
  // discarding too large (and probably mistaken errors)
  if ferror > 2 then ferror := 0;
  // source string
  src := '';
  if xparser.ExtractField('<TT>','</TT>',buffer,True) then begin
    if buffer = '~' then src := buffer
    else src := GetFluxSrc(buffer);
  end;
  Result := True;
end;
//----------------------
function SimbadData.AssignFlux(const fchar:Char; value,error:Currency):Boolean;
begin
  Result := True;
  if fchar = 'U' then umag := value
  else if fchar = 'B' then begin
    bmag := value;
    berr := error;
  end
  else if fchar = 'V' then vmag := CurrToReal(value)
  else if fchar = 'G' then gamag := value
  else if fchar = 'R' then rmag := value
  else if fchar = 'I' then imag := value
  else if fchar = 'J' then jmag := value
  else if fchar = 'H' then hmag := value
  else if fchar = 'K' then begin
    kmag := value;
    kerr := error;
  end
  else Result := False;
end;

//-----------------------
function SimbadData.ParseExtractCatalogs(var sourcedata:StringParsing):Boolean;
var buffer,buffer2:string;
    clc:Integer;
begin
  Result := False;
  // skipping to the identifiers
  if not sourcedata.MovePastTwo('Identifiers (','</TABLE>') then Exit;
  if not sourcedata.SetLimitString('</TABLE>') then Exit;
  // loop get the identifiers
  raw_catalog_list := TStringList.Create;
  while sourcedata.MovePast('<A HREF="https://cds.u-strasbg.fr/cgi-bin/Dic-Simbad?') do begin
    if not sourcedata.ExtractFieldNE('>','</A>',buffer) then Break;
    if not sourcedata.GetMovePast(buffer2,'</TT>') then Break;
    buffer2 := Trim(buffer2);
    if Length(buffer2) = 0 then Break;
    raw_catalog_list.Add(buffer);
    raw_catalog_list.Add(buffer2);
  end;
  // post check
  sourcedata.ClearLimitString;
  clc := raw_catalog_list.Count;
  if  (clc = 0) or Odd(clc) then Exit;
  Result := True;
end;
//-----------------------------------------------------
function SimbadData.ParseFeHLine(cinline:string; out teff:Real; out feh,logg:Currency):Boolean;
var teffs,fehs,loggs,buffer,years:string;
    endyear,yearint,sc:Integer;
    yok:Boolean;
    badteff:Boolean;
const badlogg:Currency = -9.99;
begin
  Result := False;
  // extracting Teff, Fe/H, and publication year string
  teffs := Trim(AnsiMidStr(cinline,2,6));
  loggs := Trim(AnsiMidStr(cinline,7,5));
  fehs := Trim(AnsiMidStr(cinline,15,6));
  yok := ExtrField(cinline,'">','</A>',buffer,False);
  if (not yok) then Exit;
  endyear := FindFirstNotOf(buffer,'0123456789',1);
  if endyear<=0 then years := buffer
  else years := AnsiLeftStr(buffer,endyear-1);
  // converting the year
  Val(years,yearint,sc);
  if sc<>0 then Exit;
  if yearint < 2000 then Exit;
  // Teff (Effective Temperature)
  if Length(teffs)=0 then teff := 0
  else begin
    Val(teffs,teff,sc);
    if sc <> 0 then teff := 0
    // quick sanity check, the temperature of Delta Eridani is not 27K!
    else badteff := teff < 400;
    if badteff then teff := 0;
  end;
  // Log g
  if Length(loggs)=0 then logg := badlogg
  else begin
    Val(loggs,logg,sc);
    if sc <> 0 then logg := badlogg;
    if badteff then logg := badlogg;
  end;
  // Fe/H (Metallicity in dex)
  Val(fehs,feh,sc);
  if sc <> 0 then feh := 999
  else if feh >= 1.00 then feh := 999;
  // checking our results
  Result := (teff <> 0) or (feh <> 999) or (logg > -9);
end;
//--------------------------------------------------------------
function SimbadData.ParseFeHBlock(inblock:string):Boolean;
var lines:TStringList;
    curline:string;
    lmax,lidx:Integer;
    teffnum:Real;
    fehnum:Currency;
    xteff_count,xfeh_count:Integer;
    teff_sum, teff_min, teff_max:Real;
    feh_sum, feh_min, feh_max:Currency;
    loggcount:Integer;
    clogg,clogsum:Currency;
    teff_temp:Real;
    qpos,ilen:Integer;
begin
  // moving past the header
  Result := False;
  qpos := AnsiPos('Reference',inblock);
  if qpos <= 0 then Exit;
  qpos := PosEx('|',inblock,qpos+3);
  if qpos <= 0 then Exit;
  ilen := Length(inblock);
  inblock := Trim(Copy(inblock,qpos+1,ilen-qpos));
  if Length(inblock) = 0 then Exit;
  // split into lines
  lines := TStringList.Create;
  lines.Text := inblock;
  lmax := lines.Count-1;
  //setting up numbers
  xteff_count := 0;
  xfeh_count := 0;
  teff_sum := 0;
  feh_sum := 0;
  teff_min := 2000000000;
  teff_max := -1;
  feh_min := 999;
  feh_max := -999;
  clogsum := 0;
  loggcount := 0;
  // we iterate over each line...
  for lidx := 0 to lmax do begin
    curline := Trim(lines[lidx]);
    // getting Teff and Fe/H from each line
    if (not ParseFeHLine(curline,teffnum,fehnum,clogg)) then Continue;
    // min, max, sum teff
    if teffnum <> 0 then begin
      Inc(xteff_count);
      teff_sum += teffnum;
      if teffnum < teff_min then teff_min := teffnum;
      if teffnum > teff_max then teff_max := teffnum;
    end;
    if fehnum < 999 then begin
      Inc(xfeh_count);
      feh_sum += fehnum;
      if fehnum < feh_min then feh_min := fehnum;
      if fehnum > feh_max then feh_max := fehnum;
    end;
    if clogg < 990 then begin
      Inc(loggcount);
      clogsum += clogg;
    end;
  end;
  // finishing calaculations after the loop
  // teff
  if xteff_count > 0 then begin
    teff_count := xteff_count;
    teff_temp := teff_sum / teff_count;
    teffavg := Trunc(teff_temp);
    if Frac(teff_temp) >= 0.5 then Inc(teffavg);
    teff_temp := (teff_min + teff_max ) / 2;
    teffpm := Ceil(teff_max-teff_temp);
    teffmed := Trunc(teff_temp);
    if Frac(teff_temp) >= 0.5 then Inc(teffmed);
  end;
  // Fe/H
  if xfeh_count > 0 then begin
    // computing average feh (no need to round...)
    feh_count := xfeh_count;
    fehavg := CurrencyDivideI(feh_sum,feh_count);
    // using a special custom function to make median and range
    CurrMinMaxToMedRange(feh_min,feh_max,fehmed,fehpm);
  end;
  // average Log g
  if loggcount > 0 then begin
    if loggcount = 1 then logg := clogsum
    else logg := CurrencyDivideI(clogsum,loggcount);
  end;
  // done
  Result := True;
end;
//--------------------------------------------------
function SimbadData.MakeFluxString:string;
begin
  Result := '';
  if umag < 99 then Result += 'U: ' + CurrToStrF(umag,ffFixed,3) + '  ';
  if bmag < 99 then begin
    Result += 'B: ' + CurrToStrF(bmag,ffFixed,3);
    if berr <> 0 then Result += '±' + CurrToStrF(berr,ffFixed,3);
    Result += '  '
  end;
  if gamag < 99 then Result += 'G: ' + CurrToStrF(gamag,ffFixed,3) + '  ';
  if rmag < 99 then Result += 'Rc: ' + CurrToStrF(rmag,ffFixed,3) + sLineBreak;
  if imag < 99 then Result += 'Ic: ' + CurrToStrF(imag,ffFixed,3) + '  ';
  if jmag < 99 then Result += 'J: ' + CurrToStrF(jmag,ffFixed,3) + '  ';
  if hmag < 99 then Result += 'H: ' + CurrToStrF(hmag,ffFixed,3) + '  ';
  if kmag < 99 then begin
    Result += 'K: ' + CurrToStrF(kmag,ffFixed,3);
    if kerr <> 0 then Result += '±' + CurrToStrF(kerr,ffFixed,3);
    Result += '  '
  end;
  Result := Trim(Result);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
constructor SimbadData.Create;
const badlogg:Currency = -9.99;
begin
  group := False;
  raw_catalog_list := nil;
  processed_catalogs := nil;
  fehavg := 999;
  fehmed := 999;
  logg := badlogg;
  xarity := SINGLE;
  hasplanet := False;
  isprms := False;
end;
//--------------------------
destructor SimbadData.Destroy;
begin
  processed_catalogs.Free;
  raw_catalog_list.Free;
  inherited;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++
// property getters
function SimbadData.GetNamestr:string;
begin  Result := namestr;   end;
//--------------------------------------------
function SimbadData.GetSpectral:string;
begin
  Result := spectral;
  if Ord(xarity)>=Ord(SPECTROCOPIC_BINARY) then Result += 'J';
end;
//--------------------------------------------
function SimbadData.GetGroup:Boolean;
begin      Result := group;      end;
//--------------------------------------------
function SimbadData.GetRadial:Double;
begin         Result := radial;    end;
//--------------------------------------------
function SimbadData.GetVMag:Double;
begin     Result := vmag;     end;
//--------------------------------------------
function SimbadData.GetNames:string;
begin        Result := extranames;     end;
//++++++++++++++++++++++++++++++++++++++++++++
// more properties
//---------------------------------------
function SimbadData.GTempC:Integer;
begin    Result := teff_count;    end;
//----------------------------------------
function SimbadData.HsTemp:Boolean;
begin    Result := (teff_count > 0);    end;
//----------------------------------------
function SimbadData.GTRangeStr:string;
begin
  Result := IntToStr(teffmed);
  if (teffpm>0) then Result += '±' + IntToStr(teffpm);
end;
//----------------------------------------
function SimbadData.GTempAvg:Real;
begin    Result := teffavg;    end;
//----------------------------------------
function SimbadData.GFeHC:Integer;
begin    Result := feh_count;    end;
//----------------------------------------
function SimbadData.HsFeH:Boolean;
begin    Result := (feh_count> 0);    end;
//----------------------------------------
function SimbadData.GtFeHAvg:Currency;
begin    Result := teffavg;   end;
//----------------------------------------
function SimbadData.GetFeHMed:Currency;
begin    Result := fehmed;    end;
//----------------------------------------
function SimbadData.GetFeHPM:Currency;
begin    Result := fehpm;     end;
//----------------------------------------
function SimbadData.GetArr:ArityType;
begin    Result := xarity;     end;
//++++++++++++++++++++++++++++++++++++++++++++++
// extra flux properties
//------------------------------------
function SimbadData.UMagG:Currency;
begin    Result := umag;    end;
//----------------------------------------
function SimbadData.BMagG:Currency;
begin    Result := bmag;    end;
//----------------------------------------
function SimbadData.BMagErrG:Currency;
begin    Result := berr;    end;
//----------------------------------------
function SimbadData.GAMagG:Currency;
begin    Result := gamag;   end;
//----------------------------------------
function SimbadData.RMagG:Currency;
begin    Result := rmag;    end;
//----------------------------------------
function SimbadData.IMagG:Currency;
begin    Result := imag;    end;
//----------------------------------------
function SimbadData.JMagG:Currency;
begin    Result := jmag;    end;
//----------------------------------------
function SimbadData.HMagG:Currency;
begin    Result := hmag;    end;
//----------------------------------------
function SimbadData.KMagG:Currency;
begin    Result := kmag;    end;
//----------------------------------------
function SimbadData.KMagErrG:Currency;
begin    Result := kerr;    end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++
// output methods
function SimbadData.OutputCatalogCVS:string;
var kindex, kmax, lenr:Integer;
    systemz,hipstr:string;
    key:string;
    lchar:PChar;
begin
  // basic null output
  Result := '';
  if processed_catalogs = nil then Exit;
  if processed_catalogs.Count = 0 then Exit;
  // building the output using a loop
  kmax := processed_catalogs.Count-1;
  for kindex := 0 to kmax do begin
    // we split lists into system catalog ids and star catalog ids
    key := processed_catalogs.Keys[kindex];
    lchar := AnsiLastChar(processed_catalogs.Data[kindex]);
    if AnsiMatchStr(key,sys_cats) or AnsiContainsStr('ABCDEabc',lchar) then begin
      systemz += key + ' ' + processed_catalogs.Data[kindex] + ',';
    end
    else if key ='Hip' then begin
      hipstr := key + ' ' + processed_catalogs.Data[kindex] + ',';
    end
    else begin
      Result += key + ' ' + processed_catalogs.Data[kindex] + ',';
    end;
  end;
  Result := systemz + hipstr + Result;
  lenr := Length(Result);
  if lenr > 0 then Result := LeftStr(Result,lenr-1);
end;
//--------------------------------------------------
function SimbadData.MakeSummaryString(fluxonly:Boolean):string;
var namex,radialx:string;
begin
  // names
   Result := namestr + sLineBreak;
   if (not fluxonly) then begin
     if group then Result := Result + 'PART OF A GROUP' + sLineBreak;
     if hasplanet then Result += 'HAS ONE OR MORE PLANETS' + sLineBreak;
   end;
   if processed_catalogs = nil then begin
     namex := RawListToString;
     Result := Result + 'RAW IDENTIFIERS:'+ sLineBreak + namex + sLineBreak;
   end
   else begin
     if Length(extranames)<>0 then
             Result := Result + 'NAMES : ' + extranames + sLineBreak;
     Result := Result + 'CATALOG IDS : ' + sLineBreak;
     Result := Result + OutputCatalogCVS + sLineBreak;
   end;
   // spectral type
   Result := Result + 'Spectral Type : ' + spectral + sLineBreak;
   // shown when requesting full results...
   if (not fluxonly) then begin
     // radial velocity
     if radial <> 0.0 then begin
       Str(radial:9:3,radialx);
       Result := Result + 'Radial Velocity : ' + radialx + ' km/s' + sLineBreak;
     end;
     // visual magnitude
     if vmag <> 9999.9 then begin
       Str(vmag:7:3,radialx);
       Result := Result + 'Visual Magnitude : ' + radialx + sLineBreak;
     end;
   end;
   // special stuff for flux data
   Result += MakeFluxString + sLineBreak;

   if HasEffTemp then begin
     Result += 'Effective Temperature : ' + EffTempRangeStr + ' (';
     Result += IntToStr(teffavg) + ') from ' + IntToStr(teff_count);
     Result += sLineBreak;
   end;

   if HasFeH then begin
     Result += 'Metallicity [Fe/H] : ' + Real2Str(fehmed,2,1,True,True);
     if fehpm > 0 then Result += '±' + Real2Str(fehpm,2,1,True,False);
     Result += ' (' + Real2Str(fehavg,2,1,True,True) + ') from ';
     Result += IntToStr(feh_count) + sLineBreak;
   end;
   // Log g
   Result += 'Log g : ' + Trim(CurrToStrF(logg,ffFixed,2));

end;
//------------------------------------------
function SimbadData.MakeSummaryLoggString():string;
var namex:string;
begin
  // names
  Result := namestr + sLineBreak;
  if processed_catalogs = nil then begin
    namex := RawListToString;
    Result := Result + 'RAW IDENTIFIERS:'+ sLineBreak + namex + sLineBreak;
  end
  else begin
    if Length(extranames)<>0 then
             Result := Result + 'NAMES : ' + extranames + sLineBreak;
    Result := Result + 'CATALOG IDS : ' + sLineBreak;
    Result := Result + OutputCatalogCVS + sLineBreak;
  end;
  // spectral type
  if Length(spectral)<>0 then
           Result := Result + 'Spectral Type : ' + spectral + sLineBreak;
  Result += sLineBreak;
  // Log g
  Result += 'Log g : ' + Trim(CurrToStrF(logg,ffFixed,2));
end;
//-------------------------------------------------
function SimbadData.VBRIString():string;
var xstr:String;
begin
  Result := 'V: ';
  if vmag < 90 then Str(vmag:7:3,xstr)
  else xstr := '----';
  Result += Trim(xstr) + '  B: ';
  if bmag < 90 then Result += CurrToStrF(bmag,ffFixed,3)
  else Result += '----';
  Result += '  Rc: ';
  if rmag < 90 then Result += CurrToStrF(rmag,ffFixed,3)
  else Result += '----';
  Result += '  Ic: ';
  if imag < 90 then Result += CurrToStrF(imag,ffFixed,3)
  else Result += '----';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++
// parsing input
function SimbadData.ParseFromHTMLPage(input:string):Boolean;
var xparser:StringParsing;
    buffer,buffer2:string;
    lchar:PChar;
    fluxchar:Char;
    fvalue,ferror,dumflux,g2mag:Currency;
    parentc,siblingc,childrenc:Integer;
    clindex,clhigh:Integer;
    fluxsrc:string;
    johnson_R,johnson_I,denisi,dr2g:Boolean;
    BcatB,BCatR,BcatI,RUCAC:Boolean;
    checkx:Boolean;
const extrps = '<INPUT TYPE="hidden" NAME="parents" ID="parents" VALUE="';
      extrcs = '<INPUT TYPE="hidden" NAME="children" ID="children" VALUE="';
      extrss = '<INPUT TYPE="hidden" NAME="siblings" ID="siblings" VALUE="';
begin
  Result := False;
  dr2g := False;
  g2mag := 99.999;
  xparser := StringParsing.Create(input,False);
  if (not xparser.MovePast('<TD VALIGN="TOP" id="basic_data"')) then begin
    xparser.Free;    Exit;
  end;
  // getting the 'name'
  if (not xparser.ExtractFieldNE('<B>','</B>',namestr)) then begin
    xparser.Free;    Exit;
  end;
  if xparser.ExtractField('--','</FONT>',buffer2,True) then begin
    if (Length(buffer2) <> 0) then namestr += ' -- ' +buffer2;
  end;
  // building the 'other object types' string
  if xparser.MovePastTwo('Other object types','<TD>') then begin
    xparser.SetLimitString('</TD>');
    while xparser.ExtractFieldNE('<TT>','</TT>',buffer) do begin
      // simbad object id
      if Length(ootypes) <> 0 then ootypes += ',';
      ootypes += buffer;
      // extracting (or skipping) special source info
      if (not xparser.ExtractFieldNE('<TT title="','">',buffer)) then Continue;
      if (buffer='Ref') then Continue;
      if (not xparser.GetMovePast(buffer,'</TT>')) then Continue;
      // incorporating special source info
      ootypes += ' [' + buffer + ']';
    end;
    xparser.ClearLimitString;
  end;
  // setting the arity
  if AnsiContainsStr('SB*',ootypes) or NameContains('Spectroscopic binary') then begin
    xarity := SPECTROCOPIC_BINARY;
  end
  else if StrContAny(ootypes,['EB*','Al*','bL*','WU*']) or NameContains('Eclipsing binary') then begin
    xarity := ECLIPSING_BINARY;
  end
  else if AnsiContainsStr('**',ootypes) or NameContains('Double or multiple star') then begin
       xarity := DOUBLE_OR_MULTIPE;
  end;
  // is this a pre-main-sequence star?
  checkx := StrContAny(ootypes,['TT*','pr*']);
  checkx := checkx or NameContains('T Tau-type Star') or NameContains('Pre-main sequence Star');
  if checkx then isprms := True;
  // getting ICRS J2000 coordinates
  if not xparser.MovePast('ICRS') then begin
    xparser.Free;    Exit;
  end;
  if not xparser.ExtractField('<TT>','<SPAN',buffer,True) then begin
    xparser.Free;    Exit;
  end;
  // splitting at + or -
  parentc := PosSetEx('+-',buffer,2);
  if parentc < 8 then begin
    xparser.Free;    Exit;
  end;
  ra_coord := Trim(AnsiLeftStr(buffer,parentc-1));
  dec_coord := AnsiRightStr(buffer,Length(buffer)-parentc+1);
  // Trying to skip past the rest the co-ordinates...
  if (not xparser.MovePast('Gal')) then begin
    xparser.Free;    Exit;
  end;
  // proper motions
  if xparser.MovePast('Proper motions <i>mas/yr</i>') then begin
    if xparser.ExtractField('<TT>','[',buffer,True) then begin
      ExtractFirstWord(buffer,pmra);
      pmdec := buffer;
    end;
  end;
  // looking for the radial velocity...
  if xparser.MovePast('Radial velocity / Redshift') then begin
    if (not xparser.ExtractNumberField('V(km/s)','[',radial)) then begin
      xparser.Free;    Exit;
    end;
  end
  else radial := 0.0;
  // next, spectral type (if there)
  if xparser.MovePast('Spectral type:') then begin
    if not xparser.ExtractField('<TT>','</TT>',spectral,True) then begin
      xparser.Free;    Exit;
    end;
    (* These strings often end with a quality character (which is not part of
    the spectra, and should be removed *)
    if Length(spectral)>3 then begin
      lchar := AnsiLastChar(spectral);
      if (lchar='C') or (lchar='D') or (lchar='E') then begin
        spectral := Trim(AnsiLeftStr(spectral,Length(spectral)-1));
      end;
    end;
    // if no luminosity class is given, assume V
    if (PosSet('VIDdg',spectral)=0) then spectral += 'V';
  end;

  // next up, fluxes...
  if (not xparser.MovePast('Fluxes (')) then begin
    xparser.Free;    Exit;
  end;
  // assiging initial values
  umag := 9999;  bmag := 9999;  berr := 0;
  vmag := 9999;  rmag := 9999;
  imag := 9999;  jmag := 9999;
  hmag := 9999;  kmag := 9999;
  gamag := 9999;
  //flux extraction loop
  if (not xparser.MovePast('<TABLE>')) then begin
    xparser.Free;    Exit;
  end;
  if (not xparser.SetLimitString('</TABLE>')) then begin
    xparser.Free;    Exit;
  end;
  johnson_R := False;  johnson_I := False;  denisi := False;
  while ExtractFlux(xparser,fluxchar,fvalue,ferror,fluxsrc) do begin
    (* 'G' is *not* from DR2, but DR1. since Simbad now includes DR2 and ignores
    the distinction, we have to take into account. *)
    if (fluxchar <> 'G') then AssignFlux(fluxchar,fvalue,ferror)
    else begin
      dr2g := (fluxsrc = '2018yCat.1345....0G');
      if not dr2g then AssignFlux('G',fvalue,ferror)
      else g2mag := fvalue;
    end;
    // checking for sources that likely need to be adjusted...
    if (fluxchar = 'B') then BcatB := (fluxsrc='2003AJ....125..984M');
    if (fluxchar = 'R') then begin
      johnson_R := (fluxsrc='2002yCat.2237....0D');
      RUCAC := (fluxsrc='2012yCat.1322....0Z'); // UCAC bandpass is not Rc
      BCatR := (fluxsrc='2003AJ....125..984M');
    end;
    if (fluxchar = 'I') then begin
      johnson_I := (fluxsrc = '2002yCat.2237....0D');
      if (not johnson_I) then begin
        denisi := (fluxsrc = '2005yCat.2263....0T');
        BcatI := (fluxsrc='2003AJ....125..984M');
      end;
    end;
  end;
  xparser.ClearLimitString;
  // possible flux conversion
  if (vmag < 99) and (johnson_R or johnson_I) then begin
    if (johnson_R and johnson_I) then Johnson_to_Cousins(vmag,rmag,imag,rmag,imag)
    else if johnson_R then Johnson_to_Cousins(vmag,rmag,imag,rmag,dumflux)
    else Johnson_to_Cousins(vmag,rmag,imag,dumflux,imag);
  end;
  if (imag < 90) and denisi then imag := DENIStoCousinsI(imag);
  // B Catalog conversions for B2 (assuming B2 instead of B1)
  if BcatB then USNO_B2_Adjust(bmag,jmag,hmag,kmag,bmag);
  // B Catalog conversions for R1/R2 (inferred)
  if BcatR then USNO_RJ_Rc(rmag,jmag,rmag);
  // B Catalog conversion for Infrared
  if BcatI then USNO_IJ_Ic(imag,jmag,imag);
  // UCAC fit Model Magnitude is not Rc
  if RUCAC and (rmag >= 10) then UCAC4_To_RcS(rmag,jmag,99.999,g2mag,rmag);

  // planets?
  hasplanet := xparser.MovePast('Substellar companion');

  // group info
  parentc := 0;
  childrenc := 0;
  siblingc:= 0;
  xparser.ExtractNumberField(extrps,'" />',parentc);
  xparser.ExtractNumberField(extrcs,'" />',childrenc);
  xparser.ExtractNumberField(extrss,'" />',siblingc);

  (* in a group if there is any children, multiple parents, or one parent with a
  small number of siblings (a large number indicates a moving group or cluster). *)
  group := (childrenc > 0) or (parentc > 1);
  if (not group) and (parentc = 1) then group := (siblingc < 8);
  // one child, has planets ⇒ not part of a groupp
  if group and (parentc = 0) and (siblingc = 0) then begin
    if (hasplanet) and (childrenc = 1) then group := False;
  end;

  // getting catalog ids
  if (not ParseExtractCatalogs(xparser)) then begin
    xparser.Free;    Exit;
  end;
  // checking for doubles
  if xarity = SINGLE then begin
    clhigh := (raw_catalog_list.Count div 2) - 1;
    for clindex := 0 to clhigh do begin
      buffer := raw_catalog_list[2*clindex+1];
      if StrEndwAny(buffer,['AB','BC','CD','AC','AD','EW','NS']) then begin
        xarity := DOUBLE_OR_MULTIPE;
        Break;
      end;
    end;
  end;
  // I've decided to try and get Fe/H as well
  // jumping forwards to measurements
  if xparser.MovePast('?data=meas#fe_h">fe_h</A>') then begin
    if xparser.ExtractField('<PRE>','</PRE>',buffer,False) then begin
      ParseFeHBlock(buffer);
    end;
  end;
  // That's all
  xparser.Free;
  Result := True;
end;
//--------------------------------------------------------
// called after parsing to make a sensible identifier list
function SimbadData.ProcessIdentifiers:Integer;
var counter,countmax:Integer;
    tagstr, idstr:string;
    addcatalog,nogiclas:Boolean;
    wdlength:Integer;
begin
  Result := 0;
  if raw_catalog_list = nil then Exit;
  // setting up values for the loop processing
  countmax := (raw_catalog_list.Count div 2)-1;
  if processed_catalogs<>nil then processed_catalogs.Free;
  processed_catalogs := TFPGMap<string,string>.Create;
  extranames := '';
  nogiclas := False;
  wdlength := 0;
  // looping through the pairs of the raw catalog list
  for counter := 0 to countmax do begin
    tagstr := raw_catalog_list[2*counter];
    idstr := raw_catalog_list[2*counter + 1];
    // checking (and possibly changing) the catalog
    addcatalog := DoAddCatalog(tagstr,idstr);
    if not addcatalog then Continue;
    // checking if the catalog is not already included
    if processed_catalogs.IndexOf(tagstr) = -1 then begin
      (* adding the catalog id. 'G' is an annoying case because
      it sometimes has multiple different ids for the same star,
      so I've decided to ban it if a star has more than one *)
      if (tagstr <> 'G') and (tagstr <> 'WD') then processed_catalogs.Add(tagstr,idstr)
      else if (tagstr = 'G') and (not nogiclas) then processed_catalogs.Add(tagstr,idstr)
      else if (tagstr = 'WD') then begin
        wdlength := Length(idstr);
        processed_catalogs.Add(tagstr,idstr);
      end;
    end
    // already included, nothing to do but check for Giclas and WD
    else if tagstr = 'G' then begin
      processed_catalogs.Remove('G');
      nogiclas := True;
    end
    else if tagstr = 'WD' then begin
      if Length(idstr) > wdlength then begin
        wdlength := Length(idstr);
        processed_catalogs.Remove('WD');
        processed_catalogs.Add(tagstr,idstr);
      end;
    end;
  end;
  // the end of the loop, we return the catalogs in the list
  Result := processed_catalogs.Count;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++
// extra
function SimbadData.NameContains(const findstr:string):Boolean;
begin
  Result := AnsiContainsStr(namestr,findstr);
end;
//----------------------------------------
//makes [Fe/h] Display string
function SimbadData.MakeFeHString:string;
begin
  if feh_count = 0 then Result := ''
  else begin
    Result := CurrToStrF(fehmed,ffFixed,2);
    if fehmed >=0 then Result := '+' + Result;
    if (feh_count > 1) and (fehpm <> 0) then begin
      Result += ' ± ' + CurrToStrF(fehpm,ffFixed,2);
    end;
    if (feh_count > 2) and (fehpm<>0) then begin
      Result += ' (' + CurrToStrF(fehavg,ffFixed,2) + ')';
    end;
    if (feh_count > 1) then Result += ' [' + IntToStr(feh_count) + ']';
  end;
end;

//======================================================================
function MakeSimbadCoordLookupURL(const rastring,decstring:string; arcminp:Integer; inEpoch:EpochType):string;
var partstr1,partstr2:string;
const simbad_loc_lookup1 = 'http://simbad.u-strasbg.fr/simbad/sim-coo?Coord=';
      simbad_disp = '&submit=display+selected+measurements';
      coofep = '&CooFrame=ICRS&CooEpoch=';
      cooNone = '&CooEqui=2000&CooDefinedFrames=none';
begin
  Assert(arcminp>0);
  Assert(Length(rastring)<>0);
  Assert(Length(decstring)<>0);
  // going ahead, with the start and the encoded position
  Result := simbad_loc_lookup1;
  partstr1 := rastring + ' ' + decstring;
  partstr2 := StringToUrlQ(partstr1);
  Result += partstr2;
  // epoch options
  case inEpoch of
    eB1950: Result += '&CooDefinedFrames=FK4-B1950';
    eB1975: Result += '&CooFrame=FK4&CooEpoch=1975&CooEqui=1975&CooDefinedFrames=none';
    eJ2000: Result += '&CooDefinedFrames=ICRS-J2000';
    zJ2014: Result += coofep + '2014' + cooNone;
    zJ2015: Result += coofep + '2015' + cooNone;
    zJ2017: Result += coofep + '2017' + cooNone;
    zJ2015h: Result += coofep + '2015.5' + cooNone;
    zJ1991q: Result += coofep + '1991.25' + cooNone;
  end;
  // adding upto radius
  Result := Result + '&Radius=';
  // the radius
  Str(arcminp,partstr1);
  Result := Result + partstr1 + '&Radius.unit=arcmin';
end;
//-------------------------------------------------
function MakeSimbadIdLookupURL(ident:string):string;
var trans:string;
const simlook1 = 'http://simbad.u-strasbg.fr/simbad/sim-id?';
      simlook2 = 'mescat.fe_h=on&mescat.mk=on&Ident=';
      simbad_disp = '&submit=display+selected+measurements';
begin
  Assert(Length(ident)<>0);
  trans := StringToUrlQ(ident);
  Result := simlook1 + simlook2 + trans + simbad_disp;
end;
//--------------------------------------------------
function GetSimbadDataURL(inurl:string; out fetchfail:Boolean):SimbadData;
var pagedata:string;
    simbadout:SimbadData;
begin
  Result := nil;
  fetchfail := True;
  // download
  pagedata := GetMainPage(inurl);
  if Length(pagedata) = 0 then Exit;
  fetchfail := False;
  // parsing
  simbadout := SimbadData.Create;
  if not simbadout.ParseFromHTMLPage(pagedata) then begin
    simbadout.Free;
    Exit;
  end;
  // post parse processing
  simbadout.ProcessIdentifiers;
  Result := simbadout;
end;

end.

