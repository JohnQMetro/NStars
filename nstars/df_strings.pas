unit df_strings;

{$MODE Delphi}

interface

uses SysUtils,StrUtils, Math, Types, Classes, synacode, Windows;

const nonalpha = ' !"#$%&''()*+,-./:;<=>?@[\]^_`{|}~';
const nonalpha2 = '¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿×÷';
(* windows-1252 delimiters below *)
const nonalpha3 = #130#132#133#134#135#137#139#145#146#147#148#149#150#151#155;
(* delimiter combinations *)
const na_delims = nonalpha + nonalpha2;
const ext_delims = na_delims + nonalpha3 + #13#10;
const whitespace = ' ';

type
// a class for making exclusive searches of substrings
ExclSearch = class
    public
      constructor Create(incasesen:Boolean);
      procedure SetString(inval:string);
      procedure AddExclude(inval:string);
      procedure AddExcludes(indata:array of string); overload;
      procedure AddExcludes(indata:TStrings); overload;
      procedure ClearExcludes();
      function Find(instr:string):Boolean;
    protected
      // private methods
      function IndvMatch(var xpos:Integer; index:Integer):Boolean;
      function MakeALook(var xpos:Integer):Boolean;
    private
      // internal data
      search:string;
      lookin:string;
      casesen:Boolean;
      lilength:Integer;
      numexcludes:Integer;
      excludes:TStringDynArray;
      eoffsets:TIntegerDynArray;
  end;

RealArray = array of Real;

// a case-insenstive version of AnsiPos
function CaseIPos(const base:string; const pattern:string):Integer;

// lets you find the first character of a given set in a string at or after
// a certain position
function FindFirstOf(const base:string; const charset:string; pos:Integer):Integer;

// lets you find the first character not of a given set in a string at or after
// a certain position
function FindFirstNotOf(const base:string; const charset:string; pos:Integer):Integer;

// lets you find the first character in a string at or after a given position
function FindChar(const base:string; const inchar:char; pos:Integer):Integer;

// looks for the first string at or after a given position, the slow way
function FindString(const base:string; const pattern:string; pos:Integer; casi:Boolean):Integer;

// checks to see if a string is at position n in the base string
function FoundAt(const base:string; const pattern:string; pos:Integer; casei:Boolean):Boolean;

// escapes curly brackets in he provided string
function EscapeBrackets(const input:string):string;

// unescapes curly brackets in he provided string
function UnEscapeBrackets(const input:string):string;

// finds the first positon of the given character that is *not* preceeded by
// a back slash
function FindUnescaped(const input:string;const inchar:char;start:Integer):Integer;

// extracts a field (up to the next character), and raises an exception if
// it is empty
function ExtractField(const name:string; base:string; const endchar:char;
 var pos:Integer):string;

// getting rid of html tags
function DeTag(const name:string):string;

(* another extract field function, this time using start and end strings,
which snips the base parameter. use idelim to specify if the start and end
are included *)
function ExtrField(var basestr:string; const starts, ends:string; out xresult:string; idelim:Boolean):Boolean;

(* chops a string at the next ocurrence of a substring *)
function ChopAfter(var basestr:string; const atthis:string):Boolean;

(* This function looks for a tag, returning an integer position *)
function FindTag(const basestr:string; const tagid:string):Integer;

(* another extract field function, this time using html start and end strings,
which snips the base parameter. use idelim to specify if the start and end tags
are included *)
function ExtrTagCont(var basestr:string; const tagid:string; out xresult:string; idelim:Boolean):Boolean;

(* extracts everything after the first colon, returns false if empty *)
function AfterColon(const indat:string; out xresult:string):Boolean;

(* returns true if the first character in the string is inchar, ignores
whitespace chars, returning the position in outpos *)
function StartsNonWS(const indat:string; const inchar:char; out outpos:Integer):Boolean;

(* searches through a list of strings until one is found, and then returns the
position and length *)
function ListPos(const base:string; slist:TStringList; var fleng:Integer):Integer;

// lets you find the last character of a given set in a string
function FindlastOf(const base:string; const charset:string):Integer;

(* Searches for an isolated substring (namely, one bracketed by delimiters or
the start or end of the search string) *)
function FindIsolated(const base:string; const word:string; casi:Boolean; out rpos:Integer):Boolean;

(* takes the base string, looks for the pattern, and if found, trims everything
before the end of the pattern off the base. Case insensetive *)
function RemUnlAfter(var basestr:string; const pattern:string):Boolean;

(* same as RemUnlAfter, but uses two alternate patternss, the first is the one used *)
function RemUnlAfterAlt(var basestr:string; const pattern1,pattern2:string):Boolean;

(* Get next word *)
function ExtractFirstWord(var base:string; var oword:string):Boolean;

// a function to parse by a specific character
function ParseByChar(indata:string; delim:Char; empt,trail:Boolean):TStringList;

// another parse by char, but with no escapes, returns nil if the count is below min
function SplitWithDelim(const indata:string; delim:Char; min:Integer):TStringList;
function SplitWithSpaces(indata:string; min:Integer):TStringList;
function SplitWithSpacesToReal(indata:string; min:Integer; var results:RealArray):Boolean;

(* function TrimZeros: removes zeros from start and end of a
formatted number. *)
function TrimZeros(indata:string):string;

(* function FindAlterate: like AnsiPos except it tries to return info on the
first found of two alternates *)
function FindAlternate(indata:string; look1,look2:string; out ffirst:Boolean; out fpos,flen:Integer):Boolean;

(* replaces some unicode specific characters... *)
procedure DeUnicode(var instr:string);

(* an attempt to shorten multiple uses of AnsiContainsStr *)
function StrContAny(const lookin:string; values:array of string):Boolean;
function StrEndwAny(const lookin:string; values:array of string):Boolean;
function StrEqAny(const matcher:string; values:array of string):Boolean;

(* shorten things *)
function TLZ(test:string):Boolean;

(* Append with comma, if non-blank*)
procedure AppendCommaNB(var base_str:string; toadd:string);

(* boolean to and from string *)
function Bool2Str(const inbool:Boolean):string;
function Str2BoolR(const source:string):Boolean;
function Str2Bool(const source:string; out xresult:Boolean):Boolean;

(* more exact fixed point Real to string methods *)
function Real2Str(const inp:Extended; dprec,mwbd:Integer; padzero,addplus:Boolean):string;
function Real2StrZ(const inp:Extended; dprec,mwbd:Integer):string;
function Real2StrZP(const inp:Extended; dprec,mwbd:Integer):string;

(* magnitude to string, with + sign for positive and 0 prefix *)
function MagToString(const inmag:Currency):string;

(* get substring and convert to currency *)
function SubstrCurr(const sourcestr:string; index,len:Integer; out res:Currency):Boolean;

(* for making url query strings *)
function StringToUrlQ(const insrc:string):string;

(* oneline string real with boolean errors *)
function StrToReal(insrc:string; out xresult:Real):Boolean;
function StrToRealBoth(insrc1,insrc2:string; out result1,result2:Real):Boolean;
function StrToRealBothNN(insrc1,insrc2:string; out result1,result2:Real):Boolean;
function Str2Curr(insrc:string; out xresult:Currency):Boolean;
function StrToCurrBoth(insrc1,insrc2:string; out xresult1,xresult2:Currency):Boolean;

(* this should not be the pain that it is! *)
function StringToLatin1(inval:string):RawByteString;



implementation
//==============================================================================
constructor ExclSearch.Create(incasesen:Boolean);
begin
  search := '';
  lookin := '';;
  lilength := 0;
  numexcludes := 0;
  casesen := incasesen;
  SetLength(excludes,0);
  SetLength(eoffsets,0);
end;
//------------------------------------------------------------------------------
// sets the string to look for. cannot be zero length
procedure ExclSearch.SetString(inval:string);
var i:Integer;
begin
  // setting the string
  search := Trim(inval);
  Assert(length(search)<>0);
  // recalculating all of the offsets
  for i:=0 to (numexcludes-1) do begin
    if casesen then eoffsets[i] := AnsiPos(search, excludes[i])-1
    else eoffsets[i] := CaseIPos(excludes[i],search)-1;
  end;
  // done
end;
//------------------------------------------------------------------------------
// Adds a new exclude string. Each new exclude string should have the search
// string as a substring
procedure ExclSearch.AddExclude(inval:string);
var ebuffer:string;
begin
  // trimming and checking the string
  ebuffer := Trim(inval);
  Assert(length(search)<>0);
  // recalculating the dimensions
  numexcludes:= numexcludes+1;
  SetLength(excludes,numexcludes);
  SetLength(eoffsets,numexcludes);
  // setting the new values
  excludes[numexcludes-1] := ebuffer;
  if casesen then eoffsets[numexcludes-1] := AnsiPos(search,ebuffer)-1
  else eoffsets[numexcludes-1] := CaseIPos(ebuffer,search)-1;
end;
//------------------------------------------------------------------------------
// adds an array of strings at a time
procedure ExclSearch.AddExcludes(indata:array of string);
var alength,i:Integer;
begin
  alength := Length(indata);
  for i:= 0 to (alength-1) do AddExclude(indata[i]);
end;
//------------------------------------------------------------------------------
// adds a set of string from a string list
procedure ExclSearch.AddExcludes(indata:TStrings);
var icount,i:Integer;
begin
  if (indata<>nil) then begin
    icount := indata.Count;
    if (0<>icount) then begin
      // adding the items one by one
      for i:=0 to (icount-1) do AddExclude(indata[i]);
    end;
  end;
end;
//------------------------------------------------------------------------------
// removes all of the excludes
procedure ExclSearch.ClearExcludes;
begin
  SetLength(excludes,0);
  numexcludes := 0;
  SetLength(eoffsets,0);
end;
//------------------------------------------------------------------------------
// we look for the search string, non within the excludes, within the instr
// passed to this method
function ExclSearch.Find(instr:string):Boolean;
var index:Integer;
begin
  // setting the global variables
  lookin := Trim(instr);
  lilength := length(lookin);
  if (lilength=0) then begin
    Result := False;
    Exit;
  end;
  index := 1;
  // the looking loop
  while not MakeALook(index) do begin
    if (index>lilength) then begin
      Result := False;
      Exit;
    end;
  end;
  // we are at the end
  Result := True;
end;
//------------------------------------------------------------------------------
// The function tries to determine if a substring is at a certain position
// note that we assume the search string starts at xpos, and the substring
// contains the search string.
function ExclSearch.IndvMatch(var xpos:Integer; index:Integer):Boolean;
var offset, newsch:Integer;
    posit:Boolean;
begin
  // getting the offset, and handling the trivial case
  offset := eoffsets[index];
  if (offset<0) then begin
    Result := False;
    Exit;
  end;
  // calculating the new start position
  newsch := xpos - offset;
  if (newsch<1) then begin
    Result := False;
    Exit;
  end;
  // checking
  if casesen then  posit := FoundAt(lookin,excludes[index],newsch,False)
  else posit := FoundAt(lookin,excludes[index],newsch,True);
  // finishing
  if posit then begin
    xpos := newsch + length(excludes[index]);
    Result := True;
  end
  else Result := False;
end;
//------------------------------------------------------------------------------
// This function is designed to look for the desired substring. If it returns
// false, either the substring was not found, or the first example was within
// an exclude substring. The value of xpos should be checked to determine
// which case has occurred.
function ExclSearch.MakeALook(var xpos:Integer):Boolean;
var lookres,i:Integer;
    match:Boolean;
begin
  if casesen then lookres := FindString(lookin,search,xpos,False)
  else lookres := FindString(lookin,search,xpos,True);
  if (lookres>0) then begin
    // we've found an example of the substring, so we now look to
    // see if it is embedded in any of the exclude substrings
    match := False;
    for i:=0 to (numexcludes-1) do begin
      match := IndvMatch(lookres,i);
      if (match) then Break;
    end;
    // we now check to see if we've found a match. If so, MakeALook fails
    if match then begin
      // we set the new position from where to start looking
      xpos := lookres;
      Result := False;
    end
    else Result := True;
    
  end
  // we find no instances of the substring after this
  else begin
    xpos := lilength+1;
    Result := False;
  end;
end;
//==============================================================================
// a case-insenstive version of AnsiPos
function CaseIPos(const base:string; const pattern:string):Integer;
var lbase,lpattern:string;
begin
  lbase:= AnsiLowerCase(base);
  lpattern := AnsiLowerCase(pattern);
  Result := AnsiPos(lpattern,lbase);
end;
//==============================================================================
function FindFirstOf(const base:string; const charset:string; pos:Integer):Integer;
var csetlen, baselen:Integer;
    I:Integer;
    found:Boolean;
begin
  // calculating some lengths
  csetlen := Length(charset);
  baselen := Length(base);
  // starting
  found := false;
  // bounds checking
  if ((pos < 1) or (pos > baselen) or (csetlen = 0)) then
  begin
    Result := 0;
    Exit;
  end;
  // the search loop
  for I := pos to baselen do
  begin
    // getting the char and looking for it in the set
    found := IsDelimiter(charset,base,I);
    if found then Break;
  end;
  // returning...
  if found then Result := I
  else result := -1;
end;
//==============================================================================
function FindFirstNotOf(const base:string; const charset:string; pos:Integer):Integer;
var csetlen, baselen:Integer;
    I:Integer;
    found:Boolean;
begin
  // calculating some lengths
  csetlen := Length(charset);
  baselen := Length(base);
  // starting
  found := false;
  // bounds checking
  if ((pos < 1) or (pos > baselen) or (csetlen = 0)) then
  begin
    Result := 0;
    Exit;
  end;
  // the search loop
  for I := pos to baselen do
  begin
    // getting the char and looking for it in the set
    found := IsDelimiter(charset,base,I);
    if not found then Break;
  end;
  // returning...
  if not found then Result := I
  else result := -1;
end;
//=========================================================================
// lets you find the first character in a string after a given position
function FindChar(const base:string; const inchar:char; pos:Integer):Integer;
var baselen:Integer;
    I:Integer;
    T:char;
    found:Boolean;
begin
  // calculating some lengths
  baselen := Length(base);
  // starting
  found := false;
  // bounds checking
  if ((pos < 1) or (pos > baselen) ) then
  begin
    Result := 0;
    Exit;
  end;
  // the search loop
  for I := pos to baselen do
  begin
    // getting the char and looking for it in the set
    T := base[I];
    found := (T = inchar);
    if found then Break;
  end;
  // returning...
  if found then Result := I
  else Result := 0;
end;
//=========================================================================
// looks for the first string at or after a given position, the slow way
function FindString(const base:string; const pattern:string; pos:Integer; casi:Boolean):Integer;
var plen, baselen:Integer;
    startpos:Integer;
    firstpos, index:Integer;
    found:Boolean;
    lpattern,lbase:string;
begin
  // calculating some lengths
  plen := Length(pattern);
  baselen := Length(base);
  // bounds checking
  if ((pos < 1) or (pos > baselen) or (plen = 0)) then
  begin
    Result := 0;
    Exit;
  end;
  // possibly getting lowercase versions
  if casi then begin
    lpattern := AnsiLowerCase(pattern);
    lbase := AnsiLowerCase(base);
  end
  else begin
    lpattern := pattern;
    lbase := base;
  end;
  // setting up the loop
  firstpos := FindChar(lbase,lpattern[1],pos);
  found := false;
  // the loop
  while firstpos<>0 do begin
    found := True;
    if plen > 1 then begin
      // the inner matching loop
      for index := 1 to (plen-1) do begin
        // checking about going past the end of the base
        if (firstpos+index) > baselen then begin
          found:= False;
          Break;
        end;
        // extracting the characters and comparing
        if lpattern[index+1] <> lbase[firstpos+index] then begin
          found := False;
          Break;
        end;
      end;
    end;
    // we check if a match has been found
    if found then Break;
    // otherwise...
    startpos := firstpos +1;
    firstpos := FindChar(lbase,lpattern[1],startpos);
  end;
  // we're done
  if not found then Result := 0
  else Result := firstpos;
end;
//==============================================================================
// checks to see if a string is at position n in the base string
function FoundAt(const base:string; const pattern:string; pos:Integer;
      casei:Boolean):Boolean;
var bufstring:string;
begin
  // we extract a substring of the base starting at pos, and extending to the end
  bufstring := Copy(base,pos,length(base)-pos+1);
  // case insensetive, or case sensetive start testing
  if casei then Result := AnsiStartsText(pattern,bufstring)
  else Result := AnsiStartsStr(pattern,bufstring);
end;
//==============================================================================
// escapes curly brackets in he provided string
function EscapeBrackets(const input:string):string;
var temp:string;
begin
  temp := AnsiReplaceStr(input,'{', '\{');
  Result := AnsiReplaceStr(temp,'}','\}');
end;
//=============================================================================
// unescapes curly brackets in he provided string
function UnEscapeBrackets(const input:string):string;
var temp:string;
begin
  temp := AnsiReplaceStr(input,'\{', '{');
  Result := AnsiReplaceStr(temp,'\}','}');
end;
///=============================================================================
// finds the first positon of the given character that is *not* preceeded by
// a back slash
function FindUnescaped(const input:string;const inchar:char;start:Integer):Integer;
var findpos:Integer;
    stringlen:Integer;
    found:Boolean;
    tchar:char;
begin
  // getting the length
  stringlen := Length(input);
  // special case checking
  if (start < 1) or (start > stringlen) then
  begin
    Result := 0;
    Exit;
  end;
  // we start looking
  found := False;
  findpos := FindChar(input,inchar,start);
  // the checking loop
  while findpos <> 0 do begin
    // special case: we are at the start
    if findpos = 1 then begin
      found := True;
      Break;
    end;
    // otherwise, we check the previous character
    tchar := input[findpos-1];
    if tchar <> '\' then begin
      found := True;
      Break;
    end;
    // we search for the next
    start := findpos+1;
    findpos := FindChar(input,inchar,start);
  end;
  // the final checking
  if found then Result := findpos
  else Result := 0;
end;
//==============================================================================
// extracts a field (up to the next character), and raises an exception if
// it is empty
function ExtractField(const name:string; base:string; const endchar:char; var pos:Integer):string;
var endpos:Integer;
    buffer:string;
begin
  endpos := FindChar(base,endchar,pos);
  if (endpos = 0) then begin
    raise Exception.Create('End of ' + name + ' field not found!');
  end;
  if endpos = pos then begin
    raise Exception.Create(name + ' field is empty');
  end;
  // extracting the name
  buffer := Copy(base,pos, endpos-pos);
  buffer := Trim(buffer);
  if buffer = '' then begin
    raise Exception.Create(name + ' field is empty');
  end;
  pos := endpos+1;
  Result :=buffer;
end;
//=============================================================================
// getting rid of html tags
function DeTag(const name:string):string;
var pos1,pos2:Integer;
    startpos:Integer;
    xresult:string;
    xlen:Integer;
begin
  // values
  startpos :=1;
  pos1:=AnsiPos('<',name);
  xlen := length(name);
  // the loop
  while pos1>0 do begin
    // extracting all before the start
    xresult := xresult + Copy(name,startpos,(pos1-startpos));
   // finding the end
    pos2:=FindChar(name,'>',pos1);
    if pos2<=0 then begin
      pos2 := Length(name);
    end;
    startpos := pos2+1;
    // searching for the next start
    pos1:=FindChar(name,'<',startpos);
  end;
  // once here, we try to add anything on the end
  if startpos<=xlen then begin
    xresult := xresult + Copy(name,startpos,xlen-startpos+1);
  end;
  // we handle possible casses where the start is in a tag
  pos1:=FindChar(xresult,'>',1);
  if (pos1>0) then begin
    xresult := RightStr(xresult,Length(xresult)-pos1);
  end;
  Result := xresult;
end;

//=============================================================================
(* another extract field function, this time using start and end strings,
which snips the base parameter. use idelim to specify if the start and end
are included *)
function ExtrField(var basestr:string; const starts, ends:string;
       out xresult:string; idelim:Boolean):Boolean;
var blen:Integer;
    spos, epos:Integer;
    startp, endp:Integer;
    startbuffer:string;
    buffer:string;
begin
  // looking for the star of the finder
  spos := AnsiPos(starts,basestr);
  if (spos=0) then begin
    Result := false;
    Exit;
  end;
  // trimming off the start (we save it for later)
  startp := spos + length(starts);
  startbuffer := Copy(basestr,1,startp);
  blen := length(basestr);
  basestr := Copy(basestr,startp,blen-startp+1);
  // we now look for the end of the finder
  epos := AnsiPos(ends,basestr);
  if (epos = 0) then begin
    basestr := startbuffer +  basestr;
    Result := false;
    Exit;
  end;
  // if found, we extract
  buffer := Copy(basestr,1,epos-1);
  // trimming off the end
  endp := epos+length(ends);
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  // we now prepare the result
  if (idelim) then xresult := starts + buffer + ends
  else xresult := Trim(buffer);
  Result := true;
end;
//===============================================================
(* another Extract Field, but starts from the start of the string *)
function ExtractUpTo(var basestr:string; const ends:string; out xresult:string):Boolean;
var blen:Integer;
    epos,endp:Integer;
    buffer:string;
begin
  Result := False;
  // we now look for the end of the finder
  epos := AnsiPos(ends,basestr);
  if (epos = 0) then Exit;
  // if found, we extract
  buffer := Copy(basestr,1,epos-1);
  // trimming ends and everything before it
  endp := epos+length(ends);
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  // we now prepare the result
  xresult := Trim(buffer);
  Result := true;
end;
//-------------------------------------------------------------
(* Extract Field : Floating-Point version *)
function ExtractFieldFloat(var basestr:string; const starts, ends:string; out xresult:Extended):Boolean;
var blen,sc:Integer;
    spos, epos:Integer;
    startp, endp:Integer;
    buffer:string;
    xbackup:Extended;
begin
  Result := False;
  // looking for the start of the finder
  spos := AnsiPos(starts,basestr);
  if (spos=0) then Exit;
  // looking for the end
  startp := spos + length(starts);
  epos := PosEx(ends,basestr,startp);
  if epos = 0 then Exit;
  // extracting the middle
  buffer := Copy(basestr,startp,epos-startp);
  buffer := Trim(buffer);
  if Length(buffer) = 0 then Exit;
  // converting to a number
  xbackup := xresult;
  Val(buffer,xresult,sc);
  if (sc<>0) then begin
    xresult := xbackup;
    Exit;
  end;
  // finding the end of the end delimiter
  endp := epos + Length(ends);
  // chopping off everything before endp
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  Result := true;
end;
//-----------------------------------------------------------
function ExtractFieldDecimal(var basestr:string; const starts, ends:string; out xresult:Currency):Boolean;
var blen,sc:Integer;
    spos, epos:Integer;
    startp, endp:Integer;
    buffer:string;
begin
  Result := False;
  // looking for the start of the finder
  spos := AnsiPos(starts,basestr);
  if (spos=0) then Exit;
  // looking for the end
  startp := spos + length(starts);
  epos := PosEx(ends,basestr,startp);
  if epos = 0 then Exit;
  // extracting the middle
  buffer := Copy(basestr,startp,epos-startp);
  // converting to a number
  if not Str2Curr(buffer,xresult) then Exit;
  // finding the end of the end delimiter
  endp := epos + Length(ends);
  // chopping off everything before endp
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  Result := true;
end;
//-----------------------------------------------------------
function ExtractFieldInteger(var basestr:string; const starts, ends:string; out xresult:Integer):Boolean;
var blen,sc,ibackup:Integer;
    spos, epos:Integer;
    startp, endp:Integer;
    buffer:string;
begin
  Result := False;
  // looking for the start of the finder
  spos := AnsiPos(starts,basestr);
  if (spos=0) then Exit;
  // looking for the end
  startp := spos + length(starts);
  epos := PosEx(ends,basestr,startp);
  if epos = 0 then Exit;
  // extracting the middle
  buffer := Copy(basestr,startp,epos-startp);
  buffer := Trim(buffer);
  if Length(buffer) = 0 then Exit;
  // converting to a number
  ibackup := xresult;
  Val(buffer,xresult,sc);
  if (sc<>0) then begin
    xresult := ibackup;
    Exit;
  end;
  // finding the end of the end delimiter
  endp := epos + Length(ends);
  // chopping off everything before endp
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  Result := true;
end;
//-------------------------------------------------------------
(* companion to the above, but just moves past a string *)
function MovePast(var basestr:string; const pastthis:string):Boolean;
var blen:Integer;
    epos,endp:Integer;
begin
  Result := False;
  // we now look for the end of the finder
  epos := AnsiPos(pastthis,basestr);
  if (epos = 0) then Exit;
  // trimming pastthis and everything before it
  endp := epos+length(pastthis);
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  // done
  Result := true;
end;
//-------------------------------------------------------------
function MovePastTwice(var basestr:string; const pastthis:string):Boolean;
var blen:Integer;
    epos1,epos2,endp:Integer;
begin
  Result := False;
  // we now look for the end of the finder
  epos1 := AnsiPos(pastthis,basestr);
  if (epos1 = 0) then Exit;
  epos2 := PosEx(basestr,pastthis,epos1+1);
  // trimming pastthis and everything before it
  endp := epos2+length(pastthis);
  blen := length(basestr);
  basestr := Copy(basestr,endp,blen-endp+1);
  // done
  Result := true;
end;
//-------------------------------------------------------------
(* chops a string at the next ocurrence of a substring *)
function ChopAfter(var basestr:string; const atthis:string):Boolean;
var epos:Integer;
begin
  Result := False;
  // we now look for the end of the finder
  epos := AnsiPos(atthis,basestr);
  if (epos = 0) then Exit;
  // trimming atthis and everything after it
  basestr := AnsiLeftStr(basestr,epos-1);
  // done
  Result := true;
end;
//============================================================================
(* This function looks for a tag, returning an integer position *)
function FindTag(const basestr:string; const tagid:string):Integer;
var tag1, tag2 :string;
    spos1, spos2:Integer;
begin
  // starting up...
  tag1 := '<' + tagid + '>';
  tag2 := '<' + tagid + ' ';
  // looking for these two variations
  spos1 := AnsiPos(tag1,basestr);
  spos2 := AnsiPos(tag2,basestr);
  // the result
  if (spos1=0) and (spos2=0) then Result := 0
  else if (spos1=0) or (spos2=0) then Result := Max(spos1,spos2)
  else Result := Min(spos1, spos2);
end;
//============================================================================
(* another extract field function, this time using html start and end strings,
which snips the base parameter. use idelim to specify if the start and end tags
are included *)
function ExtrTagCont(var basestr:string; const tagid:string; out xresult:string;
       idelim:Boolean):Boolean;
var endtag:string;
    spos, epos:Integer;
    newstart:Integer;
    startbuf:string;
    blen:Integer;
begin
  // preparing the tags
  endtag := '</' + tagid + '>';
  // looking for the start of the finder
  spos := FindTag(basestr,tagid);
  if (spos=0) then begin
    Result := false;
    Exit;
  end;
  // we now slice everything off before
  startbuf := Copy(basestr,1,spos-1);
  blen := length(basestr);
  basestr := Copy(basestr,spos,blen-spos+1);
  // we look for the end of the tag
  spos := AnsiPos('>',basestr);
  if (spos=0) then begin
    basestr := startbuf + basestr;
    Result := false;
    Exit;
  end;
  // if found, we look for the end tag
  epos := AnsiPos(endtag,basestr);
  if (epos=0) then begin
    basestr := startbuf + basestr;
    Result := false;
    Exit;
  end;
  // we now extract, the way we do it depends on whether we keep the tags
  newstart := epos+length(endtag);
  // we keep the tags
  if idelim then begin
    xresult := Copy(basestr,1,newstart-1);
  end
  // we do not include tags
  else begin
    xresult := Copy(basestr,spos+1,(epos-1)-(spos+1)+1);
    xresult := Trim(xresult);
  end;
  // trimming off stuff
  blen := length(basestr);
  basestr := Copy(basestr,newstart,blen-newstart+1);
  // done
  Result := true;
end;
//============================================================================
(* extracts everything after the first colon, returns false if empty or if
the colon could not be found *)
function AfterColon(const indat:string; out xresult:string):Boolean;
var spos,mlen:Integer;
    buffer:string;
begin
  // getting the values
  spos := AnsiPos(':',indat);
  mlen := length(indat);
  // first check
  if (spos=0) then begin
    Result := false;
    Exit;
  end;
  // next, another check
  if (spos=mlen) then begin
    Result := false;
    Exit;
  end;
  // getting the string
  buffer := Copy(indat,spos+1,mlen-(spos+1)+1);
  buffer := Trim(buffer);
  // preparing the result
  xresult := buffer;
  if (length(buffer)=0) then Result := false
  else Result := true;
end;
//=============================================================================
(* returns true if the first character in the string is inchar, ignores
whitespace chars, returning the position in outpos *)
function StartsNonWS(const indat:string; const inchar:char; out outpos:Integer):Boolean;
var tpos:Integer;
    tchar:Char;
begin
  // looking for the first non whitespace
  tpos := FindFirstNotOf(indat,' '#09#10#11#13,0);
  if (tpos < 1) then begin
    Result := false;
    Exit;
  end;
  // testing what we get
  tchar := indat[tpos];
  if (tchar<>inchar) then Result := false
  else begin
    // we've got it, we prepare the result
    outpos := tpos;
    Result := true;
  end;
end;
//============================================================================
(* searches through a list of strings until one is found, and then returns the
position and length *)
function ListPos(const base:string; slist:TStringList; var fleng:Integer):Integer;
var I:Integer;
    tpos:Integer;
    found:Boolean;
begin
  Assert(slist<>nil);
  Assert(slist.Count>0);
  found := false;
  for I:=0 to (slist.Count-1) do begin
    tpos := AnsiPos(slist.Strings[I],base);
    if (tpos<>0) then begin
      found := True;
      Break;
    end;
  end;
  // post processing
  Result := tpos;
  if found then fleng := length(slist.Strings[I]);
end;
//=============================================================================
// lets you find the last character of a given set in a string
function FindLastOf(const base:string; const charset:string):Integer;
var I,clen:Integer;
    xchar:Char;
    clast,rlast:Integer;
begin
  // starting
  rlast := 0;
  clen := length(charset);
  // the main loop
  for I:=1 to clen do begin
   xchar := charset[I];
   clast := LastDelimiter(xchar,base);
   if clast>rlast then rlast := clast;
  end;
  // done
  Result := rlast;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Searches for an isolated substring (namely, one bracketed by delimiters or
the start or end of the base string) *)
function FindIsolated(const base:string; const word:string; casi:Boolean; out rpos:Integer):Boolean;
var blen, wlen:Integer;
    wpos,before,after:Integer;
    bef_delim,aft_delim:Boolean;
begin
  // initial calcs
  blen := length(base);
  wlen := length(word);
  // initial checks
  Assert(wlen>0);
  // trivial case
  if (wlen>blen) then begin
    Result := false;
    rpos:=0;
    Exit;
  end;
  // preparing for the loop
  if casi then wpos := CaseIPos(base,word)
  else wpos := AnsiPos(word,base);
  Result := False;
  // the loop
  while (wpos<>0) do begin
    // calculating the positions
    before := wpos-1;
    after := wpos+wlen;
    // testing before
    if before = 0 then bef_delim := True
    else bef_delim := IsDelimiter(ext_delims,base,before);
    // testing after
    if after = (blen+1) then aft_delim := true
    else aft_delim := IsDelimiter(ext_delims,base,after);
    // what do we conclude?
    if bef_delim and aft_delim then begin
      Result := True;
      Break;
    end;
    // we prepare for the next iteration by searching
    wpos := FindString(base,word,after,casi);
  end;
  // when we reach here, the result is in Result
  rpos := wpos;
  
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* takes the base string, looks for the pattern, and if found, trims everything
before the end of the pattern off the base. Case insensetive *)
function RemUnlAfter(var basestr:string; const pattern:string):Boolean;
var startpos,blen,plen:Integer;
begin
  // looking for the pattern
  startpos := CaseIPos(basestr,pattern);
  if (startpos=0) then begin
    Result := False;
    Exit;
  end;
  // trimming
  plen := length(pattern);
  blen := length(basestr);
  basestr := Copy(basestr,startpos+plen,blen-(startpos+plen)+1);
  // done
  Result := True
end;
//----------------------------------------------------------------------------------
(* same as RemUnlAfter, but uses two alternate patternss, the first is the one used *)
function RemUnlAfterAlt(var basestr:string; const pattern1,pattern2:string):Boolean;
var startpos,blen,plen:Integer;
    fwhich:Boolean;
begin
  // looking for the patterns
  if not FindAlternate(basestr,pattern1,pattern2,fwhich,startpos,plen) then Result := False
  else begin
    // trimming
    blen := length(basestr);
    basestr := Copy(basestr,startpos+plen,blen-(startpos+plen)+1);
    // done
    Result := True
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Get next word *)
function ExtractFirstWord(var base:string; var oword:string):Boolean;
var  endpos,pos,blen:Integer;
begin
  // looking for the character
  pos := 1;
  base := Trim(base);
  endpos := FindChar(base,' ',pos);
  if endpos=0 then begin
    Result := False;
    Exit;
  end;
  // extracting
  oword := Trim(copy(base,1,endpos-1));
  // doing things
  blen := length(base);
  base := copy(base,endpos,blen-endpos+1);
  base := Trim(base);
  // almost done!
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// a function to parse by a specific character, allows escaped
function ParseByChar(indata:string; delim:Char; empt,trail:Boolean):TStringList;
var pcount:Integer;
    buffer:string;
    cpos,datlen:Integer;
begin
  // starting
  indata := Trim(indata);
  datlen := length(indata);
  pcount := 0;
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  // he, we start...
  cpos := FindUnescaped(indata,delim,1);
  while (cpos<>0) do begin
    // extracting
    buffer := Copy(indata,1,cpos-1);
    buffer := Trim(buffer);
    // adding
    if empt or (buffer<>'') then begin
      Result.Add(buffer);
      Inc(pcount);
    end;
    // preparing for the next loop
    indata := Copy(indata,cpos+1,datlen-(cpos+1)+1);
    datlen := length(indata);
    cpos := FindUnescaped(indata,delim,1);
  end;
  // we add the final part
  indata := Trim(indata);
  if trail or (indata<>'') then begin
    Result.Add(indata);
    Inc(pcount);
  end;
end;
//----------------------------------------------------------------------
// another parse by char, but with no escapes, returns nil if the count is below min
function SplitWithDelim(const indata:string; delim:Char; min:Integer):TStringList;
begin
  // initial setup
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := delim;
  Result.DelimitedText := indata;
  // quick check post faction
  if Result.Count < min then begin
    Result.Free;
    Result := nil;
  end;
end;
//----------------------------------------------------------------------
function SplitWithSpaces(indata:string; min:Integer):TStringList;
begin
  Result := nil;
  Assert(min>=0);
  // reducing spaces
  indata := Trim(Tab2Space(indata,1));
  indata := AnsiReplaceStr(indata,' ',' '); // nbsp
  indata := AnsiReplaceStr(indata,'  ',' ');
  indata := Trim(AnsiReplaceStr(indata,'  ',' ')); // enough for the purposes of this program
  // special case...
  if (min = 0) and (Length(indata)=0) then Result := TStringList.Create
  else Result := SplitWithDelim(indata,' ',min);
end;
//--------------------------------------------------------------------
function SplitWithSpacesToReal(indata:string; min:Integer; var results:RealArray):Boolean;
var tstr:TStringList;
    xarr:RealArray;
    dex:Integer;
begin
  Result := False;
  tstr := SplitWithSpaces(indata,min);
  if (tstr = nil) then Exit;
  if (tstr.Count < min) then Exit;
  // converting the real
  SetLength(xarr,tstr.Count);
  for dex := 0 to tstr.Count-1 do begin
    if not StrToReal(tstr[dex],xarr[dex]) then Exit;
  end;
  // here, all have been converted fine, we copy to the results array
  FreeAndNil(tstr);
  SetLength(results,Length(xarr));
  for dex := 0 to Length(xarr)-1 do results[dex] := xarr[dex];
  Result := True;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* function TrimZeros: removes zeros from start and end of a
formatted number. *)
function TrimZeros(indata:string):string;
var len:Integer;
begin
  Result := AnsiReplaceStr(indata,'0',' ');
  Result := Trim(Result);
  Result := AnsiReplaceStr(Result,' ','0');
  len := Length(Result);
  if Result[len] = '.' then Result := copy(Result,1,len-1);
  if Result='' then Result := '0';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* function FindAlterate: like AnsiPos except it tries to return info on the
first found of two alternates *)
function FindAlternate(indata:string; look1,look2:string; out ffirst:Boolean; out fpos,flen:Integer):Boolean;
var fpos1,fpos2:Integer;
    flen1,flen2:Integer;
begin
  // gathering lengths
  flen1 := Length(look1);
  flen2 := Length(look2);
  // looking for the data
  fpos1 := AnsiPos(look1,indata);
  fpos2 := AnsiPos(look2,indata);
  // the cases: both cannot be found
  if (fpos1=0) and (fpos2=0) then Result := false
  // the second is not found, or fpos2 is later
  else if (fpos2=0) or ((fpos1<fpos2) and (fpos1<>0)) then begin
    ffirst := true;
    fpos := fpos1;
    flen := flen1;
    Result := True;
  end
  // the first is not found, or fpos1 is later
  else begin
    ffirst := false;
    fpos := fpos2;
    flen := flen2;
    Result := True;
  end
end;
//-----------------------------------------------------------------
(* replaces some unicode specific characters... *)
procedure DeUnicode(var instr:string);
begin
  instr := AnsiReplaceStr(instr,'−','-'); // minus to hyphen
end;
//=============================================================================
(* an attempt to shorten multiple uses of AnsiContainsStr *)
function StrContAny(const lookin:string; values:array of string):Boolean;
var ldex,lhigh:Integer;
begin
  Result := True;
  lhigh := High(values);
  for ldex := 0 to lhigh do begin
    if AnsiContainsStr(lookin,values[ldex]) then Exit;
  end;
  Result := False;
end;
//------------------------------------------
function StrEndwAny(const lookin:string; values:array of string):Boolean;
var ldex,lhigh:Integer;
begin
  Result := True;
  lhigh := High(values);
  for ldex := 0 to lhigh do begin
    if AnsiEndsStr(values[ldex],lookin) then Exit;
  end;
  Result := False;
end;
//----------------------------------------------
function StrEqAny(const matcher:string; values:array of string):Boolean;
var ldex,lhigh:Integer;
begin
  Result := True;
  lhigh := High(values);
  for ldex := 0 to lhigh do begin
    if matcher = values[ldex] then Exit;
  end;
  Result := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* shorten things *)
function TLZ(test:string):Boolean;
begin
  test := Trim(test);
  Result := (Length(test)=0);
end;
//----------------------------------------------------------
(* Append with comma, if non-blank*)
procedure AppendCommaNB(var base_str:string; toadd:string);
begin
  if not TLZ(toadd) then begin
    if not TLZ(base_str) then base_str += ', ';
    base_str += toadd;
    base_str := Trim(base_str);
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* boolean to and from string *)
function Bool2Str(const inbool:Boolean):string;
begin
  Result := IfThen(inbool,'True','False');
end;

function Str2BoolR(const source:string):Boolean;
begin
  if source = 'True' then Result := True
  else if source = 'False' then Result := False
  else raise Exception.Create('Invalid Boolean String!');
end;

function Str2Bool(const source:string; out xresult:Boolean):Boolean;
begin
    Result := True;
    if (source='T') or (AnsiCompareText('Y',source)=0) then xresult := True
    else if AnsiCompareText('True',source)=0 then xresult := True
    else if (source='F') or (AnsiCompareText('N',source)=0) then xresult := False
    else if AnsiCompareText('False',source)=0 then xresult := False
    else if AnsiCompareText('Yes',source)=0 then xresult := True
    else if AnsiCompareText('No',source)=0 then xresult := False
    else Result := False;
end;

//=====================================================================
(* more exact fixed point Real to string methods *)
function Real2Str(const inp:Extended; dprec,mwbd:Integer; padzero,addplus:Boolean):string;
var interm:string;   hassign:Boolean;
    fulllen:Integer; isneg:Boolean;
begin
  // quick conversion 1...
  interm := Trim(FloatToStrF(inp,ffFixed,22,dprec));
  // padding and add plus...
  hassign := addplus or (inp<0);
  // padding with spaces
  if not padzero then begin
    if addplus and (inp>=0) then interm := '+' + interm;
    fulllen := mwbd + dprec + 1;
    if hassign then Inc(fulllen);
    Result := AddChar(' ',interm,fulllen);
  end
  // padding with zeroes, more complicated due to sign
  else begin
    isneg := inp<0;
    if isneg then interm := AnsiRightStr(interm,Length(interm)-1);
    fulllen := mwbd + dprec + 1;
    Result := AddChar('0',interm,fulllen);
    if isneg then Result := '-' + Result
    else if addplus then Result := '+' + Result;
  end;
end;
//----------------------------------------------------------------
function Real2StrZ(const inp:Extended; dprec,mwbd:Integer):string;
begin
  Assert(inp>=0);
  Result := Real2Str(inp,dprec,mwbd,True,False);
end;
//-----------------------------------------------------------------
function Real2StrZP(const inp:Extended; dprec,mwbd:Integer):string;
var maxlen:Integer;
begin
  maxlen := dprec + mwbd +1 ;
  Result := Real2Str(inp,dprec,mwbd,True,False);
  if maxlen = Length(Result) then Result := ' ' + Result;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* magnitude to string, with + sign for positive and 0 prefix *)
function MagToString(const inmag:Currency):string;
var isneg:Boolean;
    absmag:Currency;
    magstr:string;
const speclen = 6;
begin
  // I will only support -100.000 < inmag < +100.000
  Assert(inmag<100);
  Assert(inmag>-100);
  // negative sign gets in the way of pre-padding with zeroes
  isneg := (inmag < 0);
  if isneg then absmag := -inmag
  else absmag := inmag;
  // initial converstion to string
  magstr := CurrToStrF(absmag,ffFixed,3);
  // zero pre-padding
  if Length(magstr) = (speclen-1) then magstr := '0' + magstr;
  Assert(Length(magstr) = speclen);
  // the sign
  if isneg then Result := '-' + magstr
  else Result := '+' + magstr;
end;
//-------------------------------------
(* get substring and convert to currency *)
function SubstrCurr(const sourcestr:string; index,len:Integer; out res:Currency):Boolean;
var sstl,sc:Integer;
    substr:string;
    resbak:Currency;
begin
  // bad input
  Result := False;
  sstl := Length(sourcestr);
  if index <= 0 then Exit;
  if len = 0 then Exit;
  if (index + len-1) > sstl then Exit;
  // extracting and converting
  substr := Trim(Copy(sourcestr,index,len));
  if Length(substr)=0 then Exit;
  resbak := res;
  Val(substr,res,sc);
  if sc<>0 then begin
    res := resbak;
    Exit;
  end;
  // okay
  Result := True;
end;
//-----------------------------------------------
(* for making url query strings *)
function StringToUrlQ(const insrc:string):string;
var trans:string;
begin
  trans := EncodeURL(insrc);
  trans := AnsiReplaceStr(trans,'+','%2B');
  Result := AnsiReplaceStr(trans,' ','+');
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* oneline string real with boolean errors *)
//-----------------------------------------------
function StrToReal(insrc:string; out xresult:Real):Boolean;
var sc:Integer;
    resbak:Real;
begin
  Result := False;
  insrc := Trim(insrc);
  if insrc ='NaN' then Exit;
  if Length(insrc) = 0 then Exit;
  resbak := xresult;
  Val(insrc,xresult,sc);
  if sc<>0 then begin
    xresult := resbak;
    Exit;
  end;
  Result := True;
end;
//-----------------------------------------------
function StrToRealBoth(insrc1,insrc2:string; out result1,result2:Real):Boolean;
var backup1:Real;
begin
  Result := False;
  backup1 := result1;
  if (not StrToReal(insrc1,result1)) then Exit;
  Result := StrToReal(insrc2,result2);
  if (not Result) then result1 := backup1;
end;
//----------------------------------------------
function StrToRealBothNN(insrc1,insrc2:string; out result1,result2:Real):Boolean;
var backup1,backup2:Real;
begin
  backup1 := result1;
  backup2 := result2;
  Result := StrToRealBoth(insrc1,insrc2,result1,result2);
  if Result then begin
    Result := (result1>=0) and (result2>=0);
    if (not Result) then begin
      result1 := backup1;
      result2 := backup2;
    end;
  end;
end;
//-----------------------------------------------
function Str2Curr(insrc:string; out xresult:Currency):Boolean;
var sc:Integer;
    backp:Currency;
begin
  Result := False;
  insrc := Trim(insrc);
  if insrc ='NaN' then Exit;
  if Length(insrc) = 0 then Exit;
  backp := xresult;
  Val(insrc,xresult,sc);
  if sc<>0 then begin
    xresult := backp;
    Exit;
  end;
  Result := True;
end;
//-----------------------------------------------
function StrToCurrBoth(insrc1,insrc2:string; out xresult1,xresult2:Currency):Boolean;
var backup1:Currency;
begin
     Result := False;
     backup1 := xresult1;
     if (not Str2Curr(insrc1,xresult1)) then Exit;
     Result := Str2Curr(insrc2,xresult2);
     if (not Result) then xresult1 := backup1;
end;
//-------------------------------------------------------
(* this should not be the pain that it is! *)
function StringToLatin1(inval:string):RawByteString;
var utf8: UTF8String;
    latin1: AnsiString;
    ws: WideString;
    len: Integer;
begin
  utf8 := inval;
  len := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(utf8), Length(utf8), nil, 0);
  SetLength(ws, len);
  MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(utf8), Length(utf8), PWideChar(ws), len);
  len := WideCharToMultiByte(28591, 0, PWideChar(ws), Length(ws), nil, 0, nil, nil);
  SetLength(latin1, len);
  WideCharToMultiByte(28591, 0, PWideChar(ws), Length(ws), PAnsiChar(latin1), len, nil, nil);
  Result := latin1;
end;

end.
 
