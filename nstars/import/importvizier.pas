unit ImportVizier;

{$mode delphi}

interface

uses
  Classes, SysUtils,Synacode,
  fluxtransform, StringParser, df_strings, Utilities;

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
function MakeVizieRParams(targetname:string; radius:Real):TStringList;
function MakeVizAPASS_Params(targetname:string; radius:Real):string;
function GetFromVizier(targetname:string):APASSVizieRData;

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
var bufstr:string;
begin
  Assert(xparser<>nil);
  Result := False;
  if not xparser.MovePast('<TD') then Exit;
  if not xparser.ExtractField('>','</TD>',bufstr,True) then Exit;
  Result := True;
  if bufstr = '&nbsp;' then tccont := ''
  else tccont := bufstr;
end;
//--------------------------------------------
function VizieRDataBase.StartParsing(const instr:string):Boolean;
begin
  Result := False;
  if xparser<>nil then FreeAndNil(xparser);
  parsed := False;
  xparser := StringParsing.Create(instr,True);
  if not xparser.MovePast('<h1>VizieR Result Page</h1>') then Exit;
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
    if not xparser.MovePastNTimes('<TD',11) then Continue;
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
//================================================================
function MakeVizieRParams(targetname:string; radius:Real):TStringList;
begin
  Result := TStringList.Create;




  // Result.Add('-ref');  Result.Add('VIZ58975199504f');
  Result.Add('-to');  Result.Add('2');
  Result.Add('-from');  Result.Add('-1');
  Result.Add('-this');  Result.Add('-1');
  // Result.Add('-out.add');  Result.Add('_r');
  Result.Add('-out.add');  Result.Add('_RAJ,_DEJ');
  Result.Add('-sort');  Result.Add('_r');
  Result.Add('-order');  Result.Add('I');
  Result.Add('-oc.form');  Result.Add('sexa');
  Result.Add('-meta.foot');  Result.Add('1');
  Result.Add('-meta');  Result.Add('1');
  Result.Add('-meta.ucd');  Result.Add('2');

  Result.Add('-c');  Result.Add(targetname);
  Result.Add('-c.r');  Result.Add(Trim(FloatToStrF(radius,ffFixed,3,1)));

  Result.Add('-c.geom');  Result.Add('r');
  Result.Add('-meta.ucd');  Result.Add('2');
  Result.Add('-usenav');  Result.Add('1');
  Result.Add('-bmark');  Result.Add('POST');
  Result.Add('-out.max');  Result.Add('100');
  Result.Add('-out.form');  Result.Add('HTML+Table');
  Result.Add('-c.eq');  Result.Add('J2000');
  Result.Add('-c.u');  Result.Add('arcmin');
  Result.Add('-4c');  Result.Add('Go!');
end;
//------------------------------------------------
function MakeVizAPASS_Params(targetname:string; radius:Real):string;
begin
  Result :='-to=4&-from=-2&-this=-2&';
  Result += '%2F%2Fsource=II%2F336&%2F%2Ftables=II%2F336%2Fapass9';
  Result += '&-out.max=50&%2F%2FCDSportal=http%3A%2F%2Fcdsportal.u-strasbg.fr';
  Result += '%2FStoreVizierData.html&-out.form=HTML+Table&%2F%2Foutaddvalue=default';
  Result += '&-oc.form=sexa&-nav=cat%3AII%2F336%26tab%3A%7BII%2F336%2Fapass9';
  Result += '%7D%26key%3Asource%3DII%2F336%26HTTPPRM%3A%26&-c=';
  Result += EncodeURLElement(targetname) + '&-c.eq=J2000&-c.r=';
  Result += Trim(FloatToStrF(radius,ffFixed,3,1)) + '&-c.u=arcmin';
  Result += '&-c.geom=r&-source=II%2F336%2Fapass9&-order=I&-out=recno&recno=';
  Result += '&-out=RAJ2000&RAJ2000=&-out=DEJ2000&DEJ2000=&-out=e_RAJ2000&';
  Result += 'e_RAJ2000=&-out=e_DEJ2000&e_DEJ2000=&-out=Field&Field=&-out=nobs';
  Result += '&nobs=&-out=mobs&mobs=&-out=B-V&B-V=&-out=e_B-V&e_B-V=&-out=';
  Result += 'Vmag&Vmag=&-out=e_Vmag&e_Vmag=&u_e_Vmag=&-out=Bmag&Bmag=&-out=';
  Result += 'e_Bmag&e_Bmag=&u_e_Bmag=&-out=g%27mag&g%27mag=&-out=e_g%27mag';
  Result += '&e_g%27mag=&u_e_g%27mag=&-out=r%27mag&r%27mag=&-out=e_r%27mag';
  Result += '&e_r%27mag=&u_e_r%27mag=&-out=i%27mag&i%27mag=&-out=e_i%27mag';
  Result += '&e_i%27mag=&u_e_i%27mag=&%2F%2Fnoneucd1p=on&-file=.&-meta.ucd=2';
  Result += '&-meta=1&-meta.foot=1&-usenav=1&-bmark=POST';
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

//===================================================================
end.

