unit StringParser;
(* Intended to integrate methods for extracting unformatted data from source
text (like an HTML document) into a single class with all the right methods. *)

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, lazutf8;

type

StringParsing = class
  protected
    // data and indexes
    data,lower_data:string;
    casesen:Boolean;
    cur_index,old_index,limit_index:Integer;
    // property methods
    function gcont:string;
    function thedex:Integer;
    function pxend:Boolean;
    function hlim:Boolean;
    function ics:Boolean;
    // helper method
    function SearchFor(const this_str:string; const fromdex:Integer; out foundpos:Integer):Boolean;
    function MovePastInternal(const this_str:string; out old_index:Integer):Boolean;
    function ExtractFieldCore(const starts,ends:string; out field:string; out xold_index:Integer):Boolean;
  public
    // properties
    property StringData:string read gcont;
    property InternalIndex:Integer read thedex;
    property PastEnd:Boolean read pxend;
    property HasLimit:Boolean read hlim;
    property IsCaseSen:Boolean read ics;
    // constructors
    constructor Create; overload;
    constructor Create(const source_str:string; is_casesen:Boolean); overload;
    function SetData(const source_str:string; is_casesen:Boolean):Boolean;
    function SetLimitString(const limit_string:string):Boolean;
    function ClearLimitString:Boolean;
    // moving on
    function MovePast(const this_str:string):Boolean;
    function MovePastTwice(const this_str:string):Boolean;
    function MovePastNTimes(const this_str:string; ntimes:Integer):Boolean;
    function MovePastTwo(const firsts,seconds:string):Boolean;
    // extract strings
    function GetMovePast(out field:string; const endmark:string):Boolean; overload;
    function ExtractField(starts,ends:string; out field:string; dotrim:Boolean):Boolean;
    function ExtractFieldNE(starts,ends:string; out field:string):Boolean;
    // extract number
    function GetMovePast(out field:Real; const endmark:string):Boolean; overload;
    function GetMovePast(out field:Currency; const endmark:string):Boolean; overload;
    function ExtractNumberField(starts,ends:string; out field:Integer):Boolean; overload;
    function ExtractNumberField(starts,ends:string; out field:Real):Boolean; overload;
    function ExtractNumberField(starts,ends:string; out field:Currency):Boolean; overload;
end;

implementation
//=======================================================================
// property methods
//---------------------------
function StringParsing.gcont:string;
begin   Result := data;   end;
//---------------------------
function StringParsing.thedex:Integer;
begin   Result := cur_index;   end;
//---------------------------
function StringParsing.pxend:Boolean;
begin   Result := (cur_index > Length(data));   end;
//---------------------------
function StringParsing.hlim:Boolean;
begin   Result := (limit_index <= Length(data));   end;
//---------------------------
function StringParsing.ics:Boolean;
begin   Result := casesen;   end;
//++++++++++++++++++++++++++++++++++++
// helper method
//---------------------------
function StringParsing.SearchFor(const this_str:string; const fromdex:Integer; out foundpos:Integer):Boolean;
var lower_this:string;
begin
  // no checking is done on inputs
  Result := False;
  foundpos := 0;
  // case-insensetive looking
  if (not casesen) then begin
    lower_this := UTF8LowerCase(this_str);
    foundpos := PosEx(lower_this,lower_data,fromdex);
  end
  // case sensetive looking
  else foundpos := PosEx(this_str,data,fromdex);
  // checking the results
  if foundpos = 0 then Exit; // not found
  if foundpos >= limit_index then Exit; // past the limit
  // otherwise, okay
  Result := True;
end;
//---------------------------
function StringParsing.MovePastInternal(const this_str:string; out old_index:Integer):Boolean;
var foundp,thislen:Integer;
begin
  Result := False;
  if (not SearchFor(this_str,cur_index,foundp)) then Exit;
  thislen := Length(this_str);
  old_index := cur_index;
  cur_index := foundp + thislen;
  Result := True;
end;
//-------------------------------
function StringParsing.ExtractFieldCore(const starts,ends:string; out field:string; out xold_index:Integer):Boolean;
var original_pos,startpos,endpos:Integer;
begin
  Result := False;
  // checking the inputs
  if Length(starts) = 0 then Exit;
  if Length(ends) = 0 then Exit;
  // looking for the start
  if (not MovePastInternal(starts,original_pos)) then Exit;
  startpos := cur_index;
  cur_index := original_pos;
  // looking for the end
  if (not SearchFor(ends,startpos,endpos)) then Exit;
  // handling success
  field := Copy(data,startpos,endpos-startpos);
  xold_index := cur_index;
  cur_index := endpos + Length(ends);
  Result := True;
end;

//++++++++++++++++++++++++++++++++++++
// constructors
//---------------------------
constructor StringParsing.Create;
begin
  inherited;
  casesen := True;
  cur_index := 0;
  old_index := 0;
  limit_index := 0;
end;
//---------------------------
constructor StringParsing.Create(const source_str:string; is_casesen:Boolean);
var sdata:Boolean;
begin
  inherited Create;
  sdata := SetData(source_str,is_casesen);
  Assert(sdata);
end;
//---------------------------
function StringParsing.SetData(const source_str:string; is_casesen:Boolean):Boolean;
begin
  Result := False;
  if Length(source_str)=0 then Exit;
  data := source_str;
  casesen := is_casesen;
  cur_index := 1;
  old_index := 1;
  limit_index := Length(data)+1;
  if (not casesen) then lower_data := UTF8LowerCase(data);
  Result := True;
end;
//---------------------------
function StringParsing.SetLimitString(const limit_string:string):Boolean;
var limfound:Integer;
begin
  Result := False;
  if Length(limit_string)=0 then Exit;
  if (not SearchFor(limit_string,cur_index,limfound)) then Exit;
  limit_index := limfound;
  Result := True;
end;
//---------------------------
function StringParsing.ClearLimitString:Boolean;
begin
  Result := (limit_index <= Length(data));
  limit_index := Length(data) + 1;
end;
//++++++++++++++++++++++++++++++++++++
// moving on
//---------------------------
function StringParsing.MovePast(const this_str:string):Boolean;
begin
  Result := False;
  if Length(this_str)=0 then Exit;
  if pxend then Exit;
  Result := MovePastInternal(this_str,old_index);
end;
//---------------------------
function StringParsing.MovePastTwice(const this_str:string):Boolean;
var backup_index, discard_index:Integer;
begin
  Result := False;
  if Length(this_str)=0 then Exit;
  if pxend then Exit;
  if (not MovePastInternal(this_str,backup_index)) then Exit;
  Result := MovePastInternal(this_str,discard_index);
  if Result then old_index := backup_index
  else cur_index := backup_index;
end;
//----------------------------------------------------
function StringParsing.MovePastNTimes(const this_str:string; ntimes:Integer):Boolean;
var backup_index,temp_oldex,moved_count:Integer;
begin
  Result := False;
  // basic reject cases...
  if ntimes < 3 then Exit;
  if Length(this_str)=0 then Exit;
  if pxend then Exit;
  // moving past loop
  moved_count := 0;
  while MovePastInternal(this_str,temp_oldex) do begin
    Inc(moved_count);
    if moved_count = ntimes then Break;
    if moved_count = 1 then backup_index := temp_oldex;
  end;
  //  after the loop
  Result := (moved_count = ntimes);
  if Result then old_index := backup_index
  else cur_index := backup_index;
end;

//-----------------------------------------------------
function StringParsing.MovePastTwo(const firsts,seconds:string):Boolean;
var backup_index, discard_index:Integer;
begin
  // bad cases
  Result := False;
  if Length(firsts)=0 then Exit;
  if Length(seconds)=0 then Exit;
  if pxend then Exit;
  // looking for the first item
  if (not MovePastInternal(firsts,backup_index)) then Exit;
  Result := MovePastInternal(seconds,discard_index);
  if Result then old_index := backup_index
  else cur_index := backup_index;
end;
//++++++++++++++++++++++++++++++++++++
// extract strings
//---------------------------
function StringParsing.GetMovePast(out field:string; const endmark:string):Boolean; overload;
var endpos:Integer;
begin
  Result := False;
  if Length(endmark)=0 then Exit;
  if pxend then Exit;
  // doing...
  if (not SearchFor(endmark,cur_index,endpos)) then Exit;
  field := Copy(data,cur_index,endpos-cur_index);
  old_index := cur_index;
  cur_index := Length(endmark) + endpos;
  Result := True;
end;
//---------------------------
function StringParsing.ExtractField(starts,ends:string; out field:string; dotrim:Boolean):Boolean;
begin
  Result := ExtractFieldCore(starts,ends,field,old_index);
  if Result and dotrim then field := Trim(field);
end;
//---------------------------
function StringParsing.ExtractFieldNE(starts,ends:string; out field:string):Boolean;
var backup_index:Integer;
begin
  Result := ExtractFieldCore(starts,ends,field,backup_index);
  if (not Result) then Exit;
  field := Trim(field);
  if Length(field) = 0 then begin
    Result := False;
    cur_index := backup_index;
  end
  else old_index := backup_index;
end;
//++++++++++++++++++++++++++++++++++++
// extract number
//---------------------------
function StringParsing.GetMovePast(out field:Real; const endmark:string):Boolean; overload;
var backup_old_index,backup_cur_index:Integer;
    sc:Integer;
    strfield:string;
begin
  backup_old_index := old_index;
  backup_cur_index := cur_index;
  Result := False;
  // get move past...
  if (not GetMovePast(strfield,endmark)) then Exit;
  // trimming and checking
  strfield := Trim(strfield);
  if Length(strfield) = 0 then begin
    old_index := backup_old_index;
    cur_index := backup_cur_index;
    Exit;
  end;
  // converting the string
  Val(strfield,field,sc);
  if sc<>0 then begin
    old_index := backup_old_index;
    cur_index := backup_cur_index;
  end
  else Result := True;
end;
//---------------------------
function StringParsing.GetMovePast(out field:Currency; const endmark:string):Boolean; overload;
var backup_old_index,backup_cur_index:Integer;
    sc:Integer;
    strfield:string;
begin
  backup_old_index := old_index;
  backup_cur_index := cur_index;
  Result := False;
  // get move past...
  if (not GetMovePast(strfield,endmark)) then Exit;
  // trimming and checking
  strfield := Trim(strfield);
  if Length(strfield) = 0 then begin
    old_index := backup_old_index;
    cur_index := backup_cur_index;
    Exit;
  end;
  // converting the string
  Val(strfield,field,sc);
  if sc<>0 then begin
    old_index := backup_old_index;
    cur_index := backup_cur_index;
  end
  else Result := True;
end;
//---------------------------
function StringParsing.ExtractNumberField(starts,ends:string; out field:Integer):Boolean; overload;
var backup_index,sc:Integer;
    stringfld:string;
begin
  Result := False;
  if (not ExtractFieldCore(starts,ends,stringfld,backup_index)) then Exit;
  stringfld := Trim(stringfld);
  if Length(stringfld) = 0 then cur_index := backup_index
  else begin
    Val(stringfld,field,sc);
    if sc<>0 then cur_index := backup_index
    else begin
      Result := True;
      old_index := backup_index;
    end;
  end;
end;
//---------------------------
function StringParsing.ExtractNumberField(starts,ends:string; out field:Real):Boolean; overload;
var backup_index,sc:Integer;
    stringfld:string;
begin
  Result := False;
  if (not ExtractFieldCore(starts,ends,stringfld,backup_index)) then Exit;
  stringfld := Trim(stringfld);
  if Length(stringfld) = 0 then cur_index := backup_index
  else begin
    Val(stringfld,field,sc);
    if sc<>0 then cur_index := backup_index
    else begin
      Result := True;
      old_index := backup_index;
    end;
  end;
end;
//---------------------------
function StringParsing.ExtractNumberField(starts,ends:string; out field:Currency):Boolean; overload;
var backup_index,sc:Integer;
    stringfld:string;
begin
  Result := False;
  if (not ExtractFieldCore(starts,ends,stringfld,backup_index)) then Exit;
  stringfld := Trim(stringfld);
  if Length(stringfld) = 0 then cur_index := backup_index
  else begin
    Val(stringfld,field,sc);
    if sc<>0 then cur_index := backup_index
    else begin
      Result := True;
      old_index := backup_index;
    end;
  end;
end;

//=======================================================================

end.

