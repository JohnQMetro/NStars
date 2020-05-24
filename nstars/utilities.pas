unit Utilities;

{$mode delphi}

interface

uses
  Classes, SysUtils,Math,DAMath, HTTPSend, Synacode, LConvEncoding, df_strings;

type
  CharsetType = (ctUTF8,ctLatin1,ctWin1251,ctWin1252,ctShiftJIS,ctGB2312,ctEUCKR);


(* Basic Currency methods, assuming currency is Int64 backed. *)

(* If there are any accurate real/currency conversion functions already,
I can't seem to find them. *)
function RealToCurr(const inval:Real):Currency;
function CurrToReal(const inval:Currency):Real;

(* Division that might reduce errors, because / converts to real by dividing by
10000 before doing a floating point division. *)
function CurrHalfVal(const inval:Currency; roundup:Boolean):Currency;   // divide by 2
function CurrencyDivide(dividend,divisor:Currency):Currency; overload;
function CurrencyDivideI(const dividend:Currency; divisor:Integer):Currency;
function CurrencyDivide(const dividend:Currency; divisor:string):Currency; overload;

(* To avoid defining currency constants for every currency literal to add, here
is inline addition with a string! *)
function CurrAdd(const A:Currency; const B:string):Currency;

(* currcncy absolute value (because Abs would implicity convert to Real) *)
function CurrAbs(const inval:Currency):Currency;

(* Rounds to 2 decimal places (RoundTo converts to real first, bad for exactness),
halfup is True is you want 0.005 to always be rounded up, bankers round otherwise. *)
function RoundCurrency(const thevalue:Currency; halfup:Boolean):Currency;
function CeilMagCurrency(const thevalue:Currency):Currency;
function Ceil3rdCurrency(const thevalue:Currency):Currency;
// combo RealToCurr and RoundCurrency
function RoundConv(const invalue:Real):Currency;

(* Converts a Currency max and min to a median and range, rounded to 2 decimals *)
function CurrMinMaxToMedRange(minc,maxc:Currency; out med,range:Currency):Boolean;

(* converts a min max to a median and a range, with precision digits in the median *)
procedure MinMaxToMedRangeR(minv,maxv:Real; prec:Integer; out med,range:Real);

(* Returns true if there is only 1 decimal place *)
function CurrHasOneDec(const cval:Currency):Boolean;

(* minima *)
procedure SetMinMaxCurr(inval1,inval2:Currency; out outmin,outmax:Currency);

(* HTTP Downloads *)
function GetMainPage(url:string):string;
function GetByPOST(url:string; params:TStringList; ctx:CharsetType; out downpage:string):Boolean;
function GetByPOSTS(url,params:string; ctx:CharsetType; out downpage:string):Boolean;

(* special array creation/lookup functions *)
procedure InitCurrArrayA(prec:Currency; const source:array of Currency; var target:array of Currency);
procedure InitCurrArrayB(prec,succ:Currency; const source:array of Currency; var target:array of Currency);
function CurrArrLookupLT(const vals:Currency; const lookin:array of Currency):Integer;
function InterLookupCurr(index:Real; const source:array of Currency; max:Currency):Currency;
function InterLookupReal(index:Real; const source:array of Real; max:Real):Real;



const max_intcurr:Int64 = 9007199254740991;
      min_intcurr:Int64 = -9007199254740991;


implementation

(* --------------------------------------
Real and Currency types, it seems, covert correctly when using assignment, but
there is no inline conversion cast/function that I can find. *)
//------------------------------------
function RealToCurr(const inval:Real):Currency;
begin   Result := inval;    end;
//-------------------------------------
function CurrToReal(const inval:Currency):Real;
begin   Result := inval;    end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function CurrHalfVal(const inval:Currency; roundup:Boolean):Currency;
var IntVal:Int64 absolute Result;
    isOdd,isNeg:Boolean;
begin
  Result := inval;
  isOdd := Odd(IntVal);
  isNeg := (inval < 0);
  IntVal := IntVal div 2;
  if isOdd then begin
    // always round up
    if roundup and (not isNeg) then Inc(IntVal)
    // round towards even
    else if (not roundup) then begin
      if isneg and Odd(IntVal) then Dec(IntVal)
      else if (not isneg) and Odd(IntVal) then Inc(IntVal);
    end;
  end;
end;
//----------------------------------------------------
function CurrencyDivide(dividend,divisor:Currency):Currency; overload;
var int_dividend:Int64 absolute dividend;
    int_divisor:Int64 absolute divisor;
    divide_result:Real;
    int_result:Int64 absolute result;
begin
  (* int_dividend and int_divisor contain integer versions of the currency.
  They should be between min_intcurr and max_intcurr, because I will divide by
  converting to float (53 bit mantissa). *)
  divide_result := int_dividend / int_divisor;
  divide_result := 10000*divide_result;
  int_result := Round(divide_result);
end;

//----------------------------------------------------
function CurrencyDivideI(const dividend:Currency; divisor:Integer):Currency;
var int_dividend:Int64 absolute dividend;
    divide_result:Real;
    int_result:Int64 absolute Result;
begin
  divide_result := int_dividend / divisor;
  int_result := Round(divide_result);
end;
//----------------------------------------------------
function CurrencyDivide(const dividend:Currency; divisor:string):Currency; overload;
var curr_divisor:Currency;
begin
  curr_divisor := StrToCurr(divisor);
  Result := CurrencyDivide(dividend,curr_divisor);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function CurrAdd(const A:Currency; const B:string):Currency;
var curr_value:Currency;
begin
  curr_value := StrToCurr(B);
  Result := A + curr_value;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function CurrAbs(const inval:Currency):Currency;
begin
  if inval < 0 then Result := -inval
  else Result := inval;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function RoundCurrency(const thevalue:Currency; halfup:Boolean):Currency;
var IntVal:Int64 absolute Result;
    past2:Integer;
    bankr:Int64;
begin
  // if we are using banker's round, the builtin Round can be used
  if (not halfup) then begin
    Result := 100*thevalue;
    bankr := Round(Result);
    Result := bankr;
    Result := CurrencyDivideI(Result,100);
  end
  // otherwise, more complcated
  else begin
    // truncating the currency and getting the leftover
    Result := thevalue;
    past2 := IntVal mod 100;
    IntVal -= past2;   // positive down, negative up
    // completing the round (always round up if ~50)
    if past2 < -50 then IntVal -= 100
    else if past2 >= 50 then IntVal += 100;
  end;
end;
//--------------------------------------------------------------------
function CeilMagCurrency(const thevalue:Currency):Currency;
var IntVal:Int64 absolute Result;
    past2:Integer;
begin
    // truncating the currency and getting the leftover
    Result := thevalue;
    past2 := IntVal mod 100;
    if past2 = 0 then Exit; // value is good as is
    IntVal -= past2;   // positive down, negative up
    // positive numbers up, negative down
    if past2 < 0 then IntVal -= 100
    else if past2 > 0 then IntVal += 100;
end;
//--------------------------------------------------------
// use for rounding positive currency values after the 3rd decimal place
function Ceil3rdCurrency(const thevalue:Currency):Currency;
var IntVal:Int64 absolute Result;
    leftover,addup:Integer;
begin
  Result := thevalue;
  leftover := IntVal mod 10;
  if leftover = 0 then Exit; // value is good as is
  addup := 10 - leftover;
  IntVal += addup;
end;
//----------------------------------------------
// combo RealToCurr and RoundCurrency
function RoundConv(const invalue:Real):Currency;
begin
  Result := RoundCurrency(RealToCurr(invalue),False);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Converts a Currency max and min to a median and range, rounded to 2 decimals *)
function CurrMinMaxToMedRange(minc,maxc:Currency; out med,range:Currency):Boolean;
var max_diff,min_diff:Currency;
    work_median:Currency;
const minrange:Currency = 0.01;
begin
  // only one bad case
  Result := False;
  if minc > maxc then Exit;
  Result := True;
  // quasi-trivial case
  if minc = maxc then begin
    med := RoundCurrency(minc,True);
    if med <> minc then range := minrange
    else range := 0;
    Exit;
  end;
  // there are two values, we calculate the median...
  work_median := minc+maxc;
  work_median := CurrHalfVal(work_median,True);
  med := RoundCurrency(work_median,True);
  // find minimum and maximum differences...
  max_diff := CurrAbs(maxc - med);
  min_diff := CurrAbs(med - minc);
  // make unrounded range
  if min_diff > max_diff then max_diff := min_diff;
  // finishing off
  range := CeilMagCurrency(max_diff);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* converts a min max to a median and a range, with precision digits in the median *)
procedure MinMaxToMedRangeR(minv,maxv:Real; prec:Integer; out med,range:Real);
var medb,range1,range2:Real;
    medstr:string;
    rangceilat,ceiled:Integer;
    multipl:Real;
begin
  // calculating a rounded median
  medb := (minv+maxv)/2;
  medstr := FloatToStrF(medb,ffGeneral,prec,0);
  med := StrToFloat(medstr);
  // base range
  range1 := Abs(med-minv);
  range2 := Abs(maxv-med);
  range1 := Max(range1,range2);
  // calculating the decimal position for the ceil
  rangceilat := prec + Floor(Abs(log10(range1)));
  multipl := Exp10(rangceilat);
  // doing the range ceil (some loss of accuracy...)
  ceiled := Ceil(multipl*range1);
  range := ceiled / multipl;
end;
//------------------------------------------------------------
(* Returns true if there is only 1 decimal place *)
function CurrHasOneDec(const cval:Currency):Boolean;
var IntVal:Int64 absolute cval;
    remaindex:Int64;
begin
  remaindex := IntVal mod 1000;
  Result := (remaindex = 0);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* minima *)
procedure SetMinMaxCurr(inval1,inval2:Currency; out outmin,outmax:Currency);
begin
  if inval1 < inval2 then begin
    outmin := inval1;
    outmax := inval2;
  end
  else begin
    outmin := inval2;
    outmax := inval1;
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GetMainPage(url:string):string;
var MainResult:TStringList;
begin
    MainResult := TStringList.Create;
    if HttpGetText(url, MainResult) then
    begin
        Result := MainResult.Text;
    end
    else Result := '';
    MainResult.Free;
end;
//---------------------------------------------------------------
function GetByPOST(url:string; params:TStringList; ctx:CharsetType; out downpage:string):Boolean;
var postdata:string;
    param_pairc,param_pardex:Integer;
    // downdata:TStringStream;
begin
  // reject cases
  Result := False;
  if params = nil then Exit;
  if (params.Count=0) then Exit;
  if ((params.Count) mod 2)<>0 then Exit;
  if Length(url)=0 then Exit;
  // creating the post data from the params
  param_pairc := (params.Count div 2);
  for param_pardex := 0 to (param_pairc-1) do begin
    if param_pardex<>0 then postdata += '&';
    postdata += params[param_pardex*2] + '=';
    postdata += EncodeURLElement(params[1+param_pardex*2]);
  end;
  // trying to get the data
  Result :=GetByPOSTS(url,postdata,ctx,downpage);
end;
//--------------------------------------------------------
function GetByPOSTS(url,params:string; ctx:CharsetType; out downpage:string):Boolean;
var downdata:TStringStream;
begin
  // reject cases
  Result := False;
  if Length(url)=0 then Exit;
  // trying to get the data
  downdata := TStringStream.Create('');
  if HttpPostURL(URL, params, downdata) then begin
    if ctx = ctUTF8 then downpage := downdata.DataString
    else if ctx = ctLatin1 then downpage := ISO_8859_1ToUTF8(downdata.DataString)
    else if ctx = ctWin1251 then downpage := CP1251ToUTF8(downdata.DataString)
    else if ctx = ctWin1252 then downpage := CP1252ToUTF8(downdata.DataString)
    else if ctx = ctShiftJIS then downpage := CP932ToUTF8(downdata.DataString)
    else if ctx = ctGB2312 then downpage := CP936ToUTF8(downdata.DataString)
    else if ctx = ctEUCKR then downpage := CP949ToUTF8(downdata.DataString)
    else Assert(False);
    Result := True;
  end;
  downdata.Free;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// helper methods for the estimation lookups
//-------------------------------------------------------------
(* Sets up a destination array from a source array. The values for target[x]
are to be halfway between source[x-1] and source[x] (this is meant to be used
in a lookup for x, where it is assumed source[x] is the median value for x. *)
procedure InitCurrArrayA(prec:Currency; const source:array of Currency; var target:array of Currency);
var workval:Currency;
    setupdex:Integer;
begin
  Assert(High(source) = High(target));
  workval := prec + source[0];
  target[0] := CurrHalfVal(workval,True);
  for setupdex := 1 to High(source) do begin
    workval := source[setupdex-1] + source[setupdex];
    target[setupdex] := CurrHalfVal(workval,True);
  end;
end;
//-------------------------------------------------------------
(* same as InitCurrArrayA, but for Real values instead of Currency *)
procedure InitCurrArrayB(prec,succ:Currency; const source:array of Currency; var target:array of Currency);
var workval:Currency;
    setupdex:Integer;
    sourcemax:Integer;
begin
  sourcemax := High(source);
  Assert(sourcemax = (High(target)-1));
  workval := prec + source[0];
  target[0] := CurrHalfVal(workval,True);
  for setupdex := 1 to sourcemax do begin
    workval := source[setupdex-1] + source[setupdex];
    target[setupdex] := CurrHalfVal(workval,True);
  end;
  workval := source[sourcemax] + succ;
  target[sourcemax+1] := CurrHalfVal(workval,True);
end;
//----------------------------------------------------
(* locates vals in the array. assumes array values get larger for higher indexes,
and lookin[Result] <= vals < lookin[Result+1]  *)
function CurrArrLookupLT(const vals:Currency; const lookin:array of Currency):Integer;
var highl:Integer;
begin
  Result := -1;
  Assert(Length(lookin)>0);
  if vals < lookin[0] then Exit;
  highl := High(lookin);
  for Result := 0 to (highl-1) do begin
    if vals < lookin[Result+1] then Exit;
  end;
  Result := highl;
end;
//----------------------------------------------------
(* returns a value using the index. if the index is not an integer, the result
is an interpolated value. Assertion faliure if the index is out of range. *)
function InterLookupCurr(index:Real; const source:array of Currency; max:Currency):Currency;
var basedex,plusdex:Integer;
    baseval,plusval,diffval:Currency;
    fracdif:Real;
begin
  Assert(index>=Low(source));
  Assert(index<(High(source)+1));
  basedex := Floor(index);
  if Frac(index) = 0 then Result := source[basedex]
  // here, the index is not an Integer, we interpolate
  else begin
    // getting low and high values
    plusdex := basedex+1;
    baseval := source[basedex];
    if plusdex = (High(source)+1) then plusval := max
    else plusval := source[plusdex];
    // calculating the differences and sums
    diffval := plusval - baseval;
    fracdif := Frac(index)*CurrToReal(diffval);
    // the final result
    Result := baseval + RealToCurr(fracdif);
  end;
end;

//----------------------------------------------------
function InterLookupReal(index:Real; const source:array of Real; max:Real):Real;
var basedex,plusdex:Integer;
    baseval,plusval,diffval:Real;
    fracdif:Real;
begin
  Assert(index>=Low(source));
  Assert(index<(High(source)+1));
  basedex := Floor(index);
  if Frac(index) = 0 then Result := source[basedex]
  // here, the index is not an Integer, we interpolate
  else begin
    // getting low and high values
    plusdex := basedex+1;
    baseval := source[basedex];
    if plusdex = (High(source)+1) then plusval := max
    else plusval := source[plusdex];
    // calculating the differences and sums
    diffval := plusval - baseval;
    fracdif := Frac(index)*diffval;
    // the final result
    Result := baseval + fracdif;
  end;
end;
//=====================================================================
end.

