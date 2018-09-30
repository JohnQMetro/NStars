unit ImportVizier;

{$mode delphi}

interface

uses
  Classes, SysUtils,Synacode,
  fluxtransform, StringParser, df_strings, Utilities, gaiadr2base, strutils;

type

VizieRDataBase = class
  protected
    xparser:StringParsing;
    parsed:Boolean;
    zsnippet:string;

    // property methods
    function Qparsed:Boolean;
    function Qgetsnippet:string;
    // private methods
    function NextTableCell(out tccont:string):Boolean;
    function StartParsing(const instr:string):Boolean;

  public
    // properties
    property IsParsed:Boolean read Qparsed;
    property Snippet:string read Qgetsnippet;
    // external methods
    constructor Create;
    destructor Destroy; override;
end;
//---------------------------------------------------------------
APASSVizieRData = class(VizieRDataBase)
  protected
    apass:Boolean;
    // apass results
    apass_v:array of Real;
    apass_ve:array of Real;
    apass_b:array of Currency;
    apass_be:array of Currency;
    apass_rc:array of Currency;
    apass_ic:array of Currency;

    // property methods
    function Qhapass:Boolean;
    function Qapasscount:Integer;
    // private method
    procedure APASSReset;
    procedure NewAPASSRow;
    function SetFromAPASS():Boolean;

  public
    // properties
    property APASS_Count:Integer read Qapasscount;
    property APASSDone:Boolean read Qhapass;
    // external methods
    constructor Create;
    function SetFromString(inraw:string):Boolean;
    function GetAPASS_String(aindex:Integer; out resval:string):Boolean;
    // APASS access methods
    function GetAPASS_V(const index:Integer; out targ:Real):Boolean;
    function GetAPASS_B(const index:Integer; out targ,targe:Currency):Boolean;
    function GetAPASS_Rc(const index:Integer; out targ:Currency):Boolean;
    function GetAPASS_Ic(const index:Integer; out targ:Currency):Boolean;
end;
//-------------------------------------------------------------
VizieR2MASSData = class(VizieRDataBase)
  protected
    function Next2Curr(out mag:Currency; out mage:Currency):Boolean;
  public
    J,Jerr:Currency;
    H,Herr:Currency;
    Ks,Kserr:Currency;
    jbad,hbad,kbad:Boolean;

    constructor Create;
    function SetFromString(inraw:string):Boolean;
    function AllBad():Boolean;
    function ToString():string;
end;
//-------------------------------------------------------------
VizieRGaiaData = class(VizieRDataBase)
  protected
    function Next2String(out one:string; out two:string):Boolean;
    function ghpllx():Boolean;
  public
    mags:GaiaDR2Mags;
    ra_pos,dec_pos:string;
    pllx,pllx_err:Real;
    pmra,pmdec:string;

    property HasParallax:Boolean read ghpllx;

    constructor Create;
    destructor Destroy;
    function SetFromString(inraw:string):Boolean;
    function ToString():string;
end;
//---------------------------------------------------------------
const invmag:Currency = 99.999;
//-------------------------------------------------------------
function MakeVizAPASS_Params(targetname:string; radius:Real):string;
function MakeViz2MASS_Post(targetstr:string):string;
function MakeVizGaiaDR2_Post(targetstr:string):string;
function GetFromVizier(targetname:string):APASSVizieRData;
function Get2MASSFromVizier(targ2mass:string):VizieR2MASSData;
function GetGaiaFromVizier(targ_gaia:string):VizieRGaiaData;

implementation
//================================================================
(*    xparser:StringParsing;      parsed:Boolean;         apass:Boolean;
    zsnippet:string;          *)
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// property methods
function VizieRDataBase.Qparsed:Boolean;
begin  Result := parsed;    end;
//---------------------------------
function VizieRDataBase.Qgetsnippet:string;
begin  Result := zsnippet;    end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
function VizieRDataBase.NextTableCell(out tccont:string):Boolean;
var bufstr,bufstr2:string;
    pos1,pos2:Integer;
begin
  Assert(xparser<>nil);
  Result := False;
  if not xparser.MovePast('<TD') then Exit;
  if not xparser.ExtractField('>','</TD>',bufstr,True) then Exit;
  Result := True;
  if bufstr = '&nbsp;' then bufstr := ''
  else bufstr := bufstr;
  // sometimes the cell contents are in a FONT tag...
  if AnsiStartsStr('<FONT',bufstr) then begin
    pos1 := AnsiPos('>',bufstr);
    if (pos1 > 0) then begin
      pos2 := PosEx('<',bufstr,pos1);
      if (pos2 > 0) then bufstr := Copy(bufstr,pos1+1,pos2-(pos1+1));
    end;
  end;
  tccont := Trim(bufstr);
end;
//--------------------------------------------
function VizieRDataBase.StartParsing(const instr:string):Boolean;
begin
  Result := False;
  if xparser<>nil then FreeAndNil(xparser);
  parsed := False;
  xparser := StringParsing.Create(instr,True);
  if not xparser.MovePast('<h1>VizieR</h1>') then Exit;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// external methods
//------------------------
constructor VizieRDataBase.Create;
begin
  xparser := nil;   parsed := False;
end;
//--------------------------
destructor VizieRDataBase.Destroy;
begin
  inherited;
  if xparser<>nil then FreeAndNil(xparser);
end;

//===============================================================
function APASSVizieRData.Qhapass:Boolean;
begin  Result := apass;    end;
//----------------------------------
function APASSVizieRData.Qapasscount:Integer;
begin  Result := Length(apass_v);    end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
procedure APASSVizieRData.APASSReset;
begin
  apass := False;
  SetLength(apass_v,0);
  SetLength(apass_ve,0);
  SetLength(apass_b,0);
  SetLength(apass_be,0);
  SetLength(apass_rc,0);
  SetLength(apass_ic,0);
end;
//---------------------------------------------
procedure APASSVizieRData.NewAPASSRow;
var xlen,xdex:Integer;
const inv:Currency = 99.999;
begin
  xlen := Length(apass_v)+1;
  xdex := xlen - 1;
  SetLength(apass_v,xlen);
  apass_v[xdex] := 99.999;
  SetLength(apass_ve,xlen);
  apass_ve[xdex] := 0;
  SetLength(apass_b,xlen);
  apass_b[xdex] := inv;
  SetLength(apass_be,xlen);
  apass_be[xdex] := 0;
  SetLength(apass_rc,xlen);
  apass_rc[xdex] := inv;
  SetLength(apass_ic,xlen);
  apass_ic[xdex] := inv;
end;
//---------------------------------------------
// private method
function APASSVizieRData.SetFromAPASS():Boolean;
var vmag,vemag,bmag,bemag,gmag,rmag,imag:string;
    havegri,haveall,convok,vbinc:Boolean;
    convertstr:string;
    mdex:Integer;
    vest:Real;
    best:Currency;
begin
  Result := False;
  Assert(xparser<>nil);
  // moving past the header
  if not xparser.MovePast('AAVSO Photometric All Sky Survey') then Exit;
  // moving to the data row
  while xparser.MovePast('<TR class=''tuple-2''>') do begin
    // skipping the 11 fields we do not use
    if not xparser.MovePastNTimes('<TD',12) then Continue;
    // getting V Magnitude
    if not NextTableCell(vmag) then Continue;
    if not NextTableCell(vemag) then Continue;
    // getting B Magnitude
    if not NextTableCell(bmag) then Continue;
    if not NextTableCell(bemag) then Continue;
    // getting g'
    if not NextTableCell(gmag) then Continue;
    if not xparser.MovePast('<TD') then Continue;
    // getting r'
    if not NextTableCell(rmag) then Continue;
    if not xparser.MovePast('<TD') then Continue;
    // getting i'
    if not NextTableCell(imag) then Continue;

    // if we get here, we have values which may be blank...
    havegri := (Length(gmag)<>0) and (Length(rmag)<>0) and (Length(imag)<>0);
    haveall := havegri and (Length(vmag)<>0) and (Length(bmag)<>0);
    if ( havegri or (Length(vmag)<>0) or (Length(vmag)<>0)) then begin
      // results are V,B,Rc,Ic
      NewAPASSRow();
      mdex := High(apass_v);
      // case where we can get Rc and Ic conversions
      if havegri then begin
        if haveall then begin
          convertstr := vmag + ' ' + vemag + ' ';
          convertstr += bmag + ' ' + bemag + ' ';
        end;
        convertstr += gmag + ' 0 ' + rmag + ' 0 ' + imag + ' 0';
        convok := APASS_to_Fluxes(convertstr,vbinc,apass_v[mdex],apass_ve[mdex],
              vest,apass_b[mdex],apass_be[mdex],best,apass_rc[mdex],apass_ic[mdex]);
        // post conversion finishing
        if not vbinc then begin
          apass_v[mdex] := vest;
          apass_b[mdex] := best;
        end;
      end else begin
        StrToReal(vmag,apass_v[mdex]);
        StrToReal(vemag,apass_ve[mdex]);
        Str2Curr(bmag,apass_b[mdex]);
        Str2Curr(bemag,apass_be[mdex]);
      end;
      // end of conversions...
    end;
    // if we get here, we have a row
  end;
  // post
  apass := (Length(apass_v) > 0);
  Result := apass;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor APASSVizieRData.Create;
begin
  inherited;
  apass := False;
end;
//--------------------------
function APASSVizieRData.SetFromString(inraw:string):Boolean;
begin
  // initial setup
  Result := False;
  if not StartParsing(inraw) then Exit;
  APASSReset;
  Result := SetFromAPASS();
end;
//-----------------------------
function APASSVizieRData.GetAPASS_String(aindex:Integer; out resval:string):Boolean;
begin
  Result := False;
  if (not apass) then Exit;
  if aindex < 0 then Exit;
  if aindex >= Length(apass_v) then Exit;
  // making the result
  resval := 'V: ' + Trim(FloatToStrF(apass_v[aindex],ffFixed,7,3)) + '±';
  resval += Trim(FloatToStrF(apass_ve[aindex],ffFixed,7,3));
  resval += '  B: ' + Trim(CurrToStrF(apass_b[aindex],ffFixed,3)) + '±';
  resval += Trim(CurrToStrF(apass_be[aindex],ffFixed,3));
  resval += '  Rc: ' + Trim(CurrToStrF(apass_rc[aindex],ffFixed,3));
  resval += '  Ic: ' + Trim(CurrToStrF(apass_ic[aindex],ffFixed,3));
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// APASS access methods
//------------------------
function APASSVizieRData.GetAPASS_V(const index:Integer; out targ:Real):Boolean;
begin
  Result := False;  if index < 0 then Exit;
  if index >= Length(apass_v) then Exit;
  if apass_v[index] >= 99 then Exit;
  Result := True;  targ := apass_v[index];
end;
//-----------------------
function APASSVizieRData.GetAPASS_B(const index:Integer; out targ,targe:Currency):Boolean;
begin
  Result := False;  if index < 0 then Exit;
  if index >= Length(apass_v) then Exit;
  if apass_b[index] >= 99 then Exit;
  Result := True;
  targ := apass_b[index];
  targe := apass_be[index];
end;
//-----------------------
function APASSVizieRData.GetAPASS_Rc(const index:Integer; out targ:Currency):Boolean;
begin
  Result := False;  if index < 0 then Exit;
  if index >= Length(apass_v) then Exit;
  if apass_rc[index] >= 99 then Exit;
  Result := True;  targ := apass_rc[index];
end;
//-----------------------
function APASSVizieRData.GetAPASS_Ic(const index:Integer; out targ:Currency):Boolean;
begin
  Result := False;  if index < 0 then Exit;
  if index >= Length(apass_v) then Exit;
  if apass_ic[index] >= 99 then Exit;
  Result := True;  targ := apass_ic[index];
end;
//===============================================================================
function VizieR2MASSData.Next2Curr(out mag:Currency; out mage:Currency):Boolean;
var str1,str2:string;
    tmag,tmage:Currency;
begin
  Result := False;
  // extracting
  if not NextTableCell(str1) then Exit;
  if not NextTableCell(str2) then Exit;
  // converting
  if not TryStrToCurr(str1,tmag) then Exit;
  if not TryStrToCurr(str2,tmage) then Exit;
  // // setting the results
  mag := tmag;
  mage := tmage;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(*    J,Jerr:Currency;
    H,Herr:Currency;
    Ks,Kserr:Currency;  *)
constructor VizieR2MASSData.Create;
begin
  inherited;
  J := invmag;  Jerr := 0;
  H := invmag;  Herr := 0;
  Ks := invmag; Kserr := 0;
  jbad := True;  hbad := True;
  kbad := True;
end;
//-------------------------------------------------------
function VizieR2MASSData.SetFromString(inraw:string):Boolean;
var rj,rh,rk:Boolean;
    contf:string;
begin
  Result := False;
  if not StartParsing(inraw) then Exit;
  if not xparser.MovePast('2MASS All-Sky Catalog of Point Sources') then Exit;
  // moving to the data row
  if not xparser.MovePast('<TR class=''tuple-2''>') then Exit;
  // ignoring 4 fields
  if not xparser.MovePastNTimes('<TD',4) then Exit;
  // J, H, K
  rj := Next2Curr(J,Jerr);
  rh := Next2Curr(H,Herr);
  rk := Next2Curr(Ks,Kserr);
  if not (rj or rh or rk) then Exit;
  // skipping the quality...
  if not xparser.MovePast('<TD') then Exit;
  // we check confusion and contamination
  if not NextTableCell(contf) then Exit;
  if Length(contf) <> 3 then Exit;
  jbad := (not rj) or (contf[1] <> '0');
  hbad := (not rh) or (contf[2] <> '0');
  kbad := (not rk) or (contf[3] <> '0');
  // done
  parsed := True;
  FreeAndNil(xparser);
  Result := True;
end;
//---------------------------------------------------
function VizieR2MASSData.AllBad():Boolean;
begin
  Result := jbad and hbad and kbad;
end;
//--------------------------------------------------
function VizieR2MASSData.ToString():string;
begin
  Result := '';
  if not jbad then begin
    Result += 'J : ' + CurrToStrF(J,ffFixed,3) + ' ( ± ' + CurrToStrF(Jerr,ffFixed,3);
    Result += ' )' + sLineBreak;
  end;
  if not hbad then begin
    Result += 'H : ' + CurrToStrF(H,ffFixed,3) + ' ( ± ' + CurrToStrF(Herr,ffFixed,3);
    Result += ' )' + sLineBreak;
  end;
  if not kbad then begin
    Result += 'Ks: ' + CurrToStrF(Ks,ffFixed,3) + ' ± ' + CurrToStrF(Kserr,ffFixed,3);
    Result += sLineBreak;
  end;
end;
//===========================================================================
function VizieRGaiaData.Next2String(out one:string; out two:string):Boolean;
var str1,str2:string;
begin
  Result := False;
  // extracting
  if not NextTableCell(str1) then Exit;
  if not NextTableCell(str2) then Exit;
  // // setting the results
  one := str1;
  two := str2;
  Result := True;
end;
//--------------------------------
function VizieRGaiaData.ghpllx():Boolean;
begin
  Result := (pllx > 0);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(*    J,Jerr:Currency;
    H,Herr:Currency;
    Ks,Kserr:Currency;  *)
constructor VizieRGaiaData.Create;
begin
  inherited;
  mags := nil;
  pllx_err := 0;
  pllx := -1;
end;
destructor VizieRGaiaData.Destroy;
begin
  FreeAndNil(mags);
  inherited;
end;

//-------------------------------------------------------
function VizieRGaiaData.SetFromString(inraw:string):Boolean;
var str1,str2:string;
    maglist:TStringList;
begin
  Result := False;
  maglist := nil;
  try
    if not StartParsing(inraw) then Exit;
    if not xparser.MovePast('Gaia DR2 (Gaia Collaboration, 2018)') then Exit;
    // moving to the data row
    if not xparser.MovePast('<TR class=''tuple-2''>') then Exit;
    // ignoring the first field
    if not xparser.MovePast('<TD') then Exit;
    // RA and Dec
    if not Next2String(ra_pos,dec_pos) then Exit;
    // skipping the id
    if not xparser.MovePast('<TD') then Exit;
    // parallax
    if not Next2String(str1,str2) then Exit;
    if (str1 <> '') then begin
      if (not StrToRealBoth(str1,str2,pllx,pllx_err)) then Exit;
      pllx += 0.029;
    end;
    // proper motion
    if (not next2String(pmra,pmdec)) then Exit;
    if (pmra = '') and (pllx >= 0) then begin
      pllx := -1;
      pllx_err := 0;
    end;
    // magnitudes
    maglist := TStringList.Create;
    // G
    if (not next2String(str1,str2)) then Exit;
    maglist.Add(str1);
    maglist.Add(str2);
    // BP
    if (not next2String(str1,str2)) then Exit;
    maglist.Add(str1);
    maglist.Add(str2);
    // RP
    if (not next2String(str1,str2)) then Exit;
    maglist.Add(str1);
    maglist.Add(str2);
    // creating the mag object and converting the list
    mags := GaiaDR2Mags.Create;
    if (not mags.SetFromList(maglist,0)) then begin
      mags.Free;   Exit;
    end;
  finally
    maglist.Free;
    FreeAndNil(xparser);
  end;
  // done
  parsed := True;
  Result := True;
end;
//--------------------------------------------------
function VizieRGaiaData.ToString():string;
begin
  Result := mags.DisplayData() + sLineBreak;
  Result += 'RA: ' + ra_pos + '  Dec: ' + dec_pos + sLineBreak;
  if HasParallax then begin
    Result += 'Pllx : ' + Trim(FloatToStrF(pllx,ffFixed,7,3)) + '±';
    Result += Trim(FloatToStrF(pllx_err,ffFixed,5,3));
  end;
end;

//===============================================================================
function MakeVizAPASS_Params(targetname:string; radius:Real):string;
begin
  Result := '-to=4&-from=-3&-this=-3&%2F%2F';
  Result += 'source=II%2F336%2Fapass9&%2F%2Ftables=II%2F336%2F';
  Result += 'apass9&-out.max=50&%2F%2FCDSportal=http%3A%2F%2F';
  Result += 'cdsportal.u-strasbg.fr%2FStoreVizierData.html&-out.form=';
  Result += 'HTML+Table&-out.add=_r&-out.add=_RAJ%2C_DEJ&%2F%2F';
  Result += 'outaddvalue=default&-sort=_r&-oc.form=sexa&-nav=';
  Result += 'cat%3AII%2F336%26tab%3A%7BII%2F336%2Fapass9%7D%26key%3A';
  Result += 'source%3DII%2F336%2Fapass9%26HTTPPRM%3A%26-out.max%3D50%26';
  Result += '-out.form%3DHTML+Table%26-out.add%3D_r%26-out.add%3D_RAJ%2C_DEJ';
  Result += '%26-sort%3D_r%26-oc.form%3Dsexa%26&-c=';
  Result += EncodeURLElement(targetname) + '&-c.eq=J2000&-c.r=';
  Result += Trim(FloatToStrF(radius,ffFixed,4,2)) + '&-c.u=arcmin&-c.geom=';
  Result += 'r&-source=II%2F336%2Fapass9&-order=I&-out.orig=standard&-out=';
  Result += 'recno&recno=&-out=RAJ2000&RAJ2000=&-out=DEJ2000&DEJ2000=';
  Result += '&e_RAJ2000=&e_DEJ2000=&-out=Field&Field=&-out=nobs&nobs=&-out=';
  Result += 'mobs&mobs=&-out=B-V&B-V=&-out=e_B-V&e_B-V=&-out=Vmag&Vmag=';
  Result += '&-out=e_Vmag&e_Vmag=&u_e_Vmag=&-out=Bmag&Bmag=&-out=e_Bmag';
  Result += '&e_Bmag=&u_e_Bmag=&-out=g%27mag&g%27mag=&-out=e_g%27mag&e_g%27';
  Result += 'mag=&u_e_g%27mag=&-out=r%27mag&r%27mag=&-out=e_r%27mag&e_r%27';
  Result += 'mag=&u_e_r%27mag=&-out=i%27mag&i%27mag=&-out=e_i%27mag&e_i%27';
  Result += 'mag=&u_e_i%27mag=&%2F%2Fnoneucd1p=on&-file=.&-meta.ucd=2&-meta=';
  Result += '1&-meta.foot=1&-usenav=1&-bmark=POST';
end;

function MakeViz2MASS_Post(targetstr:string):string;
begin
  Result := '-to=4&-from=-2&-this=-2&%2F%2Fsource=II%2F246&%2F%2Ftables';
  Result += '=II%2F246%2Fout&-out.max=50&%2F%2FCDSportal=http%3A%2F%2F';
  Result += 'cdsportal.u-strasbg.fr%2FStoreVizierData.html&-out.form';
  Result += '=HTML+Table&%2F%2Foutaddvalue=default&-oc.form=sexa&-nav=';
  Result += 'cat%3AII%2F246%26tab%3A%7BII%2F246%2Fout%7D%26key%3Asource';
  Result += '%3DII%2F246%26HTTPPRM%3A%26&-c=&-c.eq=J2000&-c.r=++2&-c.u=arcmin';
  Result += '&-c.geom=r&-source=II%2F246%2Fout&-order=I&-out.orig=standard&-out';
  Result += '=RAJ2000&RAJ2000=&-out=DEJ2000&DEJ2000=&errMaj=&errMin=&errPA=&';
  Result += '-out=2MASS&2MASS=' + EncodeURLElement(targetstr) + '&-out=Jmag&';
  Result += 'Jmag=&Jcmsig=&-out=e_Jmag&e_Jmag=&Jsnr=&-out=Hmag&Hmag=&Hcmsig=&';
  Result += '-out=e_Hmag&e_Hmag=&Hsnr=&-out=Kmag&Kmag=&Kcmsig=&-out=e_Kmag&';
  Result += 'e_Kmag=&Ksnr=&-out=Qflg&Qflg=&Rflg=&Bflg=&-out=Cflg&Cflg=&Ndet=';
  Result += '&prox=&pxPA=&pxCntr=&-out=Xflg&Xflg=&-out=Aflg&Aflg=&Cntr=&Hemis=';
  Result += '&Date=&Scan=&GLON=&GLAT=&Xscan=&JD=&Jpsfchi=&Hpsfchi=&Kpsfchi=';
  Result += '&Jstdap=&e_Jstdap=&Hstdap=&e_Hstdap=&Kstdap=&e_Kstdap=&edgeNS=';
  Result += '&edgeEW=&edge=&dup=&use=&opt=&Dopt=&PAopt=&Bmag=&Rmag=&Nopt=';
  Result += '&extKey=&scanKey=&coaddKey=&coadd=&-ignore=Opt%3D*&Opt=Opt';
  Result += '&%2F%2Fnoneucd1p=on&-file=.&-meta.ucd=2&-meta=1&-meta.foot=1&';
  Result += '-usenav=1&-bmark=POST';
end;

function MakeVizGaiaDR2_Post(targetstr:string):string;
begin
  Result := '-to=4&-from=-3&-this=-3&%2F%2Fsource=I%2F345%2Fgaia2&%2F%2Ftables';
  Result += '=I%2F345%2Fgaia2&-out.max=50&%2F%2FCDSportal=http%3A%2F%2F';
  Result += 'cdsportal.u-strasbg.fr%2FStoreVizierData.html&-out.form=HTML+Table';
  Result += '&%2F%2Foutaddvalue=default&-oc.form=sexa&-nav=cat%3AI%2F345%26tab';
  Result += '%3A%7BI%2F345%2Fgaia2%7D%26key%3Asource%3DI%2F345%2Fgaia2%26';
  Result += 'HTTPPRM%3A%26&-c=&-c.eq=J2000&-c.r=++2&-c.u=arcmin&-c.geom=r&';
  Result += '-source=I%2F345%2Fgaia2&-order=I&-out.orig=standard&DR2Name=&-out';
  Result += '=RA_ICRS&RA_ICRS=&e_RA_ICRS=&-out=DE_ICRS&DE_ICRS=&e_DE_ICRS=';
  Result += '&SolID=&-out=Source&Source=' + EncodeURLElement(targetstr);
  Result += '&RandomI=&Epoch=&-out=Plx&Plx=&-out=e_Plx&e_Plx=&RPlx=&-out=pmRA';
  Result += '&pmRA=&e_pmRA=&-out=pmDE&pmDE=&e_pmDE=&RADEcor=&RAPlxcor=';
  Result += '&RApmRAcor=&RApmDEcor=&DEPlxcor=&DEpmRAcor=&DEpmDEcor=&PlxpmRAcor';
  Result += '=&PlxpmDEcor=&pmRApmDEcor=&NAL=&NAC=&NgAL=&NbAL=&gofAL=&chi2AL';
  Result += '=&epsi=&sepsi=&Solved=&APF=&WAL=&pscol=&e_pscol=&fvarpi=';
  Result += '&MatchObsA=&Nper=&amax=&type=&MatchObs=&Dup=&o_Gmag=&FG=&e_FG=';
  Result += '&RFG=&-out=Gmag&Gmag=&-out=e_Gmag&e_Gmag=&o_BPmag=&FBP=&e_FBP=';
  Result += '&RFBP=&-out=BPmag&BPmag=&-out=e_BPmag&e_BPmag=&o_RPmag=&FRP=';
  Result += '&e_FRP=&RFRP=&-out=RPmag&RPmag=&-out=e_RPmag&e_RPmag=';
  Result += '&E%28BR%2FRP%29=&Mode=&BP-RP=&BP-G=&G-RP=&-out=RV&RV=&-out=e_RV';
  Result += '&e_RV=&o_RV=&Tefftemp=&loggtemp=&%5BFe%2FH%5Dtemp=&Var=&GLON=';
  Result += '&GLAT=&ELON=&ELAT=&fPriam=&Teff=&b_Teff=&B_Teff=&AG=&b_AG=&B_AG';
  Result += '=&E%28BP-RP%29=&b_E%28BP-RP%29=&B_E%28BP-RP%29=&fFLAME=&Rad=&b_Rad';
  Result += '=&B_Rad=&Lum=&b_Lum=&B_Lum=&RAJ2000=&e_RAJ2000=&DEJ2000';
  Result += '=&e_DEJ2000=&%2F%2Fnoneucd1p=on&-file=.&-meta.ucd=2&-meta=1';
  Result += '&-meta.foot=1&-usenav=1&-bmark=POST';
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++
function GetFromVizier(targetname:string):APASSVizieRData;
var params:string;
    dok:Boolean;
    downstring:string;
    fsOut: TFileStream;
const vizurl1 = 'http://vizier.u-strasbg.fr/viz-bin/VizieR-4';
      vizurl2 = 'http://vizier.cfa.harvard.edu/viz-bin/VizieR-4';
begin
  Result := nil;
  params := MakeVizAPASS_Params(targetname,0.2);
  dok := GetByPOSTS(vizurl1,params,ctLatin1,downstring);
  if not dok then Exit;
  (*
  // testing the download
  fsOut := TFileStream.Create('testo.html', fmCreate);
  fsOut.WriteAnsiString(downstring);
  fsOut.Free;
  *)
  // parsing
  Result := APASSVizieRData.Create;
  dok := Result.SetFromString(downstring);
  if not dok then FreeAndNil(Result);
end;
//-------------------------------------------------------------
function Get2MASSFromVizier(targ2mass:string):VizieR2MASSData;
var params:string;
    dok:Boolean;
    downstring:string;
    fsOut: TFileStream;
const vizurl1 = 'http://vizier.u-strasbg.fr/viz-bin/VizieR-4';
      vizurl2 = 'http://vizier.cfa.harvard.edu/viz-bin/VizieR-4';
begin
  Result := nil;
  if targ2mass[1] = 'J' then targ2mass := RightStr(targ2mass,Length(targ2mass)-1);
  params := MakeViz2MASS_Post(targ2mass);
  dok := GetByPOSTS(vizurl1,params,ctLatin1,downstring);
  if not dok then Exit;
  Result := VizieR2MASSData.Create;
  dok := Result.SetFromString(downstring);
  if not dok then FreeAndNil(Result);
end;
//-------------------------------------------------------------
function GetGaiaFromVizier(targ_gaia:string):VizieRGaiaData;
var params:string;
    dok:Boolean;
    downstring:string;
    fsOut: TFileStream;
const vizurl1 = 'http://vizier.u-strasbg.fr/viz-bin/VizieR-4';
      vizurl2 = 'http://vizier.cfa.harvard.edu/viz-bin/VizieR-4';
begin
  Result := nil;
  params := MakeVizGaiaDR2_Post(targ_gaia);
  dok := GetByPOSTS(vizurl1,params,ctLatin1,downstring);
  if not dok then Exit;
  Result := VizieRGaiaData.Create;
  dok := Result.SetFromString(downstring);
  if not dok then FreeAndNil(Result);
end;

//===================================================================
end.

