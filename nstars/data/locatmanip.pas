unit LocatManip;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, Math, DAMath;

(* Small utilities for converting things like position and proper motion go
in this module, to de-clutter Location.  *)

type

  NsHours = 0..23;
  Ns60int = 0..59;
  NsDeg = 0..90;

// converting right ascension and Declination
function DecRA_To_HMS(const dra:Real; out hours:NsHours; out mins:Ns60int; out secs:Real):Boolean;
function DecDec_To_DMS(const ddeg:Real; out south:Boolean; out deg:NsDeg; out min:Ns60int; out decs:Real):Boolean;
function HMS_To_DecRA(hours:NsHours; mins:Ns60int; secs:Real; out decRA:Real):Boolean;
function DMS_To_DecDec(southern:Boolean; deg:NsDeg; min:Ns60int; secs:Real; out decDeg:Real):Boolean;

// position to string...
function DecRA_ToString(const dra:Real; prec:Word; out stringRA:string):Boolean;
function DecDec_ToString(const ddec:Real; prec:Word; out stringDec:string):Boolean;
function HMS_ToString(hours:NsHours; mins:Ns60int; secs:Real; prec:Word; colon:Boolean; out stringRA:string):Boolean;
function DMS_ToString(southern:Boolean; deg:NsDeg; min:Ns60int; secs:Real; prec:Word; colon:Boolean; out stringDec:string):Boolean;


//*************************************************************************

implementation
//*************************************************************************
function DecRA_To_HMS(const dra:Real; out hours:NsHours; out mins:Ns60int; out secs:Real):Boolean;
var wdra:Real;
begin
  Result := False;
  if (dra >= 360) or (dra < 0) then Exit;
  // splitting
  wdra := dra / 15.0;
  hours := Trunc(wdra);
  wdra := Frac(wdra) * 60;
  mins := Trunc(wdra);
  secs := Frac(wdra) * 60.0;
  Result := True;
end;
//--------------------------------------------
function DecDec_To_DMS(const ddeg:Real; out south:Boolean; out deg:NsDeg; out min:Ns60int; out decs:Real):Boolean;
var wddec:Real;
begin
  Result := False;
  if (ddeg > 90) or (ddeg < -90) then Exit;
  // splitting
  south := (ddeg < 0);
  wddec := Abs(ddeg);
  deg := Trunc(wddec);
  wddec := Frac(wddec) * 60;
  min := Trunc(wddec);
  decs := Frac(wddec)*60;
  Result := True;
end;
//--------------------------------------------
function HMS_To_DecRA(hours:NsHours; mins:Ns60int; secs:Real; out decRA:Real):Boolean;
var interm:Real;
begin
  Result := False;
  if (secs < 0) or (secs >= 60) then Exit;
  interm := (mins + (secs / 60.0))/4.0;
  decRa := (hours * 15.0) + interm;
  Result := True;
end;
//--------------------------------------------
function DMS_To_DecDec(southern:Boolean; deg:NsDeg; min:Ns60int; secs:Real; out decDeg:Real):Boolean;
var interm:Real;
begin
  Result := False;
  if (secs < 0) or (secs >= 60) then Exit;
  if (deg = 90) and ((min <> 0) or (secs <> 0.0)) then Exit;
  // building the result...
  interm := (min / 60.0) + (secs / 3600.0);
  if southern then decDeg := - (deg + interm)
  else decDeg := deg + interm;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// position to string...
function DecRA_ToString(const dra:Real; prec:Word; out stringRA:string):Boolean;
begin
  Result := False;
  if (dra < 0) or (dra >= 360) then Exit;
  // zero-padding
  if (dra < 10) then stringRA := '00'
  else if (dra < 100) then stringRA := '0'
  else stringRA := '';
  // converting and adding
  stringRA += Trim(FloatToStrF(dra,ffFixed,4+prec,prec));
  Result := True;
end;
//----------------------------------------
function DecDec_ToString(const ddec:Real; prec:Word; out stringDec:string):Boolean;
var south:Boolean;
    xdec:Real;
begin
  Result := False;
  if (ddec < -90) or (ddec > 90) then Exit;
  // zero and sign padding
  south := (ddec < 0);
  xdec := Abs(ddec);
  if (xdec < 10) and south then stringDec := '-0'
  else if (xdec < 10) and (not south) then stringDec := '+0'
  else if south then stringDec := '-'
  else stringDec := '+';
  // converting and adding
  stringDec += Trim(FloatToStrF(xdec,ffFixed,3+prec,prec));
  Result := True;
end;
//----------------------------------------
function HMS_ToString(hours:NsHours; mins:Ns60int; secs:Real; prec:Word; colon:Boolean; out stringRA:string):Boolean;
begin
  Result := False;
  if (secs < 0) or (secs >= 60) then Exit;
  if (hours < 10) then stringRA := '0'
  else stringRA := '';
  stringRA += IntToStr(hours) + IfThen(colon,':',' ');
  if (mins < 10) then stringRA += '0';
  stringRA += IntToStr(mins) + IfThen(colon,':',' ');
  if (secs < 10) then stringRA += '0';
  stringRA += Trim(FloatToStrF(secs,ffFixed,3+prec,prec));
  Result := True;
end;
//----------------------------------------
function DMS_ToString(southern:Boolean; deg:NsDeg; min:Ns60int; secs:Real; prec:Word; colon:Boolean; out stringDec:string):Boolean;
begin
  Result := False;
  if (secs < 0) or (secs >= 60) then Exit;
  if (deg = 90) and ((min <> 0) or (secs <> 0.0)) then Exit;
  // starting...
  if (deg < 10) and southern then stringDec := '-0'
  else if (deg < 10) and (not southern) then stringDec := '+0'
  else if southern then stringDec := '-'
  else stringDec := '+';
  stringDec += IntToStr(deg) + IfThen(colon,':',' ');
  if (min < 10) then stringDec += '0';
  stringDec += IntToStr(min) + IfThen(colon,':',' ');
  if (secs < 10) then stringDec += '0';
  stringDec += Trim(FloatToStrF(secs,ffFixed,3+prec,prec));
  Result := True;
end;
//*************************************************************************
end.

