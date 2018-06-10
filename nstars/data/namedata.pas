unit namedata;

{$MODE Delphi}

interface

uses SysUtils, StrUtils, Classes, Math, df_strings, unitdata;

type

TBayerSuperscript = 0..9;
//-------------------------------------------------------------------------
(* Stars can be named at the system level, at the level of a star, or the
level of a grouping (As an example, Alpha Centaurus A & B share the catalog
name Gliese 559, but Alpha Centaurus C has Gliese 551 to itself) *)
StarName = class
  protected
    cat_desig:TStringList;
    function FindCat(inv:string):Integer;
    function ISDBProcess(inv:string):string;
  public
    proper_name:string;
    var_designation:string;
    bayer_sup:TBayerSuperscript;
    (* the constructor *)
    constructor Create; overload;
    constructor Create(source:StarName); overload;
    (* Utilities *)
    function GetCCode(inv:string):string;
    function IsEmpty:Boolean;
    function Search(infind:string; conste:Integer):Boolean;
    function AnyCatEquiv(check_against:StarName):Boolean;
    (* managing the catalog list *)
    function SetCat(inval:string):Boolean;
    function SetMultipleCat(inval:string):Boolean;
    function SetISDBCat(inval:string):Boolean;
    function DelCat(incat:string):Boolean;
    function DelByIndex(index:Integer):Boolean;
    function GetCat(inval:string):string;
    function GetCatValue(const cc:string; out value:string):Boolean;
    function RenameCats(incats:TStringList):Integer;
    (* returns the entire catalog list (for display) *)
    function GetList:TStringList;
    function ExtractNonSystemCats:TStringList;
    (* string I/O *)
    function ToIOString:string;
    procedure FromString(inval:string);
    function ListOfCats:string;
    function LimitedListofCats(maxlen:Integer):string;
    (* get catalog info *)
    function GetCatalogCount:Integer;
    function GetCatalog(index:Integer):string;
    function GetDefaultCatalog:string;
    function GetRandomCat:string;
    (* done *)
    destructor Destroy; override;
end;

implementation
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function StarName.FindCat(inv:string):Integer;
var I,CC:Integer;
    tempstr,gcat:string;
begin
  // a special case
  if cat_desig = nil then begin
    Result := -1;
    Exit;
  end;
  // we prepare for looking
  inv := inv;
  CC := cat_desig.Count;
  // looking
  for I := 0 to CC - 1 do begin
    tempstr := cat_desig[I];
    gcat := GetCCode(tempstr);
    if gcat = inv then begin
      Result := I;
      Exit;
    end;
  end;
  // no match
  Result := -1;
end;
//--------------------------------------------------------
function StarName.ISDBProcess(inv:string):string;
var buf2:string;
begin
  if ExtrField(inv,'(',')',buf2,false) then begin
    Result := Trim(buf2) + ' ' + Trim(inv);
  end
  else Result := inv;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor StarName.Create;
begin
  cat_desig := nil;
  proper_name := '';
  var_designation := '';
  bayer_sup:=0;
end;
//-----------------------------------------------
constructor StarName.Create(source:StarName);
begin
  if source <> nil then begin
    proper_name := source.proper_name;
    var_designation := source.var_designation;
    bayer_sup := source.bayer_sup;
    if (source.cat_desig<>nil) then begin
      cat_desig := TStringList.Create;
      cat_desig.AddStrings(source.cat_desig);
    end
    else cat_desig := nil;
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* gets the cargeory code *)
function StarName.GetCCode(inv:string):string;
var qpos:Integer;
begin
  qpos := FindFirstOf(inv,' +-',1);
  if qpos<1 then Result := ''
  else Result := copy(inv,1,qpos-1);
end;
//-----------------------------------------------------------
function StarName.IsEmpty:Boolean;
begin
  Result := False;
  if (bayer_sup<>0) or (proper_name<>'') then Exit;
  if (var_designation<>'') or (cat_desig<>nil) then Exit;
  if GetCatalogCount = 0 then Exit;
  Result := True;
end;
//-----------------------------------------------------------
function StarName.Search(infind:string; conste:Integer):Boolean;
var buf:string;
    I:Integer;
begin
  Result := False;
  if IsEmpty then Exit;
  Result := True;
  // looking in the proper name
  if AnsiContainsText(proper_name,infind) then Exit;
  // looking in the variable star info
  if var_designation<>'' then begin
    buf := var_designation + ' ' + constellations[(conste-1)*3];
    if AnsiContainsText(buf,infind) then Exit;
  end;
  // we look in the catalog names
  if cat_desig<>nil then begin
    for I := 0 to cat_desig.Count - 1 do begin
      buf := cat_desig[I];
      if buf=infind then Exit;
    end;
  end;
  // done
  Result := False;  
end;
//--------------------------------------------------------
function StarName.AnyCatEquiv(check_against:StarName):Boolean;
var catdex,catmax:Integer;
    currcat:string;
begin
  // various 'no catalogs' cases to get out of the way
  Result := False;
  if check_against = nil then Exit;
  if cat_desig = nil then Exit;
  if check_against.GetCatalogCount = 0 then Exit;
  if cat_desig.Count = 0 then Exit;
  // we now loop thru the items it cat desig, and look for them in check_against
  Result := True;
  catmax := cat_desig.Count-1;
  for catdex := 0 to catmax do begin
    currcat := cat_desig[0];
    if (0<=(check_against.cat_desig.IndexOf(currcat))) then Exit;
  end;
  // if we get here, nothing has been found...
  Result := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* managing the catalog list *)
//-----------------------------------------------------------
function StarName.SetCat(inval:string):Boolean;
var cstr:string;
    catdex:Integer;
begin
  // preliminary checks and preps
  cstr := GetCCode(inval);
  if cstr='' then begin
    Result := False;
    Exit;
  end;
  Result := True;
  // special case...
  if cat_desig = nil then begin
    cat_desig := TStringList.Create;
    cat_desig.Sorted := True;
    cat_desig.Duplicates := dupIgnore;
    cat_desig.Add(inval);
  end
  // general case
  else begin
    catdex := FindCat(cstr);
    if catdex = -1 then cat_desig.Add(inval)
    else begin
      cat_desig.Delete(catdex);
      cat_desig.Add(inval);
    end;
  end;
  // we are done,
  Result := True;
end;
//----------------------------------------------------------
function StarName.SetMultipleCat(inval:string):Boolean;
var liststr:TStringList;
    I:Integer;
begin
  // starting, incl parsing the list
  Result := False;
  liststr := ParseByChar(inval,',',false,false);
  if liststr.Count=0 then begin
    liststr.Free;
    Exit;
  end;
  // looping
  for I := 0 to liststr.Count - 1 do begin
    if not SetCat(liststr[I]) then begin
      liststr.Free;
      Exit;
    end;
  end;
  // done
  liststr.Free;
  Result := True;
end;
//----------------------------------------------------------
function StarName.SetISDBCat(inval:string):Boolean;
var liststr:TStringList;
    bufq:string;
    I:Integer;
begin
  // starting, incl parsing the list
  Result := False;
  // hic! hip-hip hooray!
  inval := AnsiReplaceStr(inval,'Hipparcos Input Catalog (HIC)','Hipparcos Input Catalog (Hip)');
  // we parse
  liststr := ParseByChar(inval,',',false,false);
  if liststr.Count=0 then begin
    liststr.Free;
    Exit;
  end;
  // looping
  for I := 0 to liststr.Count - 1 do begin
    bufq := liststr[I];
    bufq := ISDBProcess(bufq);
    if not SetCat(bufq) then begin
      liststr.Free;
      Exit;
    end;
  end;
  // done
  liststr.Free;
  Result := True;
end;
//-----------------------------------------------------------
function StarName.DelCat(incat:string):Boolean;
var catdex:Integer;
begin
  catdex := FindCat(incat);
  if (catdex=-1) then Result := False
  else begin
    cat_desig.Delete(catdex);
    if cat_desig.Count=0 then begin
      cat_desig.Free;
      cat_desig := nil;
    end;
    Result := True;
  end;
end;
//----------------------------------------------------------
function StarName.DelByIndex(index:Integer):Boolean;
begin
  Result := False;
  if (index<0) or (index>=(cat_desig.Count)) then Exit;
  cat_desig.Delete(index);
  if cat_desig.Count=0 then begin
    cat_desig.Free;
    cat_desig := nil;
  end;
  Result := True;  
end;
//-----------------------------------------------------------
function StarName.GetCat(inval:string):string;
var catdex:Integer;
begin
  catdex := FindCat(inval);
  if catdex = -1 then Result := ''
  else Result := cat_desig[catdex];
end;
//-----------------------------------------------------------
function StarName.GetCatValue(const cc:string; out value:string):Boolean;
var cindex,len1,len2:Integer;
begin
  // first look for the index
  cindex := FindCat(cc);
  if cindex=-1 then begin
    Result := False;
    Exit;
  end;
  // now, we extract
  value := cat_desig[cindex];
  len1 := length(cc);
  len2 := length(value);
  value := Trim(copy(value,len1+1,len2-len1));
  // done
  Result := True;
end;
//-----------------------------------------------------------
function StarName.RenameCats(incats:TStringList):Integer;
var I,caindex,lcount:Integer;
    tag, value,rstring:string;
begin
  // checks
  Assert(incats<>nil);
  // starting
  Result := 0;
  lcount := incats.Count div 2;
  // the loop, working by category pairs
  for I := 0 to lcount - 1 do begin
    tag := incats[2*I];
    // looking for the category to rename
    caindex := FindCat(tag);
    if caindex = -1 then Continue;
    // if found, we rename by getting the value and making a new entry...
    Assert(GetCatValue(tag,value));
    rstring := incats[2*I+1] + ' ' + value;
    // deleting the old one
    DelByIndex(caindex);
    // and inserting the new entry
    Assert(SetCat(rstring));
    Inc(result);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 function StarName.GetList:TStringList;
 begin
   Result := cat_desig;
 end;
//---------------------------------------------
function StarName.ExtractNonSystemCats:TStringList;
var sysdex,catdex:Integer;
    sysfound:Boolean;
begin
  Result := nil;
  if (cat_desig = nil) or (cat_desig.Count = 0) then Exit;
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := ',';
  // looping over the catalogs to see if they are system cats...
  for catdex := (cat_desig.Count -1) downto 0 do begin
    sysfound := False;
    for sysdex := Low(sys_cats) to High(sys_cats) do begin
      if AnsiStartsStr(sys_cats[sysdex],cat_desig[catdex]) then begin
         if GetCCode(cat_desig[catdex]) = sys_cats[sysdex] then begin
            sysfound := True;  Break;
         end;
      end;
    end;
    // here, if sysfound is still False, we move the string to Result
    if (not sysfound) then begin
       Result.Add(cat_desig[catdex]);
       cat_desig.Delete(catdex);
    end;
  end;
  // some final checks
  if Result.Count = 0 then FreeAndNil(Result);
  if cat_desig.Count = 0 then FreeAndNil(cat_desig);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* string I/O *)
function StarName.ToIOString:string;
var buf1:string;
    I,CC:Integer;
begin
  // proper name and variable name
  Result := proper_name + ';' +  var_designation + ';';
  // bayer superscript
  Str(bayer_sup,buf1);
  Result := Result + Trim(buf1) + ';';
  // we now convert catalog names to csv
  buf1 := '';
  if cat_desig<>nil then begin
    CC := cat_desig.Count;
    buf1:= cat_desig[0];
    for I := 1 to CC - 1 do buf1 := buf1 + ',' + cat_desig[I];
  end;
  // done with that
  Result := Result + buf1;
end;
//-----------------------------------------------------------
(* conversion from string, here, we allow exceptions! *)
procedure StarName.FromString(inval:string);
var buf1:TStringList;
    icns:string;
    tval:Boolean;
    bval, sc:Integer;
begin
  // we parse into the big parts
  buf1 := ParseByChar(inval,';',true,true);
  if (buf1.Count)<>4 then
      raise Exception.Create('There must be 4 fields in the name input!');
  // assigning part 1 and 2
  proper_name := buf1[0];
  var_designation := buf1[1];
  // bayer superscript
  Val(buf1[2],bval,sc);
  if (sc<>0) or (bval>9) or (bval<0) then
    raise Exception.Create('The bayer superscript is malformed!');
  bayer_sup := bval;
  // we load individual catalog names
  // preparation
  icns:= buf1[3];
  buf1.Free;
  cat_desig.Free;
  cat_desig := nil;
  if icns<>'' then begin
    // using the SetMultipleCat method
    tval := SetMultipleCat(icns);
    if not tval then
      raise Exception.Create('There are problems with the catalog names');
  end;
  // done
end;
//-----------------------------------------------------------
function StarName.ListOfCats:string;
var buf1:string;
    I,CC:Integer;
begin
  buf1 := '';
  if cat_desig<>nil then begin
    CC := cat_desig.Count;
    buf1:= cat_desig[0];
    for I := 1 to CC - 1 do buf1 := buf1 + ';' + cat_desig[I];
  end;
  Result := buf1;
end;
//------------------------------------------------
function StarName.LimitedListofCats(maxlen:Integer):string;
var runlen,curlen:Integer;
    prefdex,prefmax:Integer;
    curcat:string;
begin
  (* since the result has a maximum list, we use an array of preferred catalog
  ids to pick the most important catalogs... *)
  runlen := 0;
  prefmax := High(prefcats);
  Result := '';
  // looping over the cats
  for prefdex := 0 to prefmax do begin
    curcat := GetCat(prefcats[prefdex]);
    curlen := Length(curcat);
    if curlen = 0 then Continue;
    if (curlen+runlen+1) > maxlen then Break;
    Result += curcat + ';';
    runlen += curlen + 1;
  end;
  // afterwards, get rid of any slashes
  Result := AnsiReplaceStr(Result,'/','-');
  // if there is a trailing semicolon, get rid of it.
  if AnsiEndsStr(';',Result) then Result := AnsiLeftStr(Result,runlen-1);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* get catalog info *)
//-----------------------------------------------------------
function StarName.GetCatalogCount:Integer;
begin
  if cat_desig=nil then Result := 0
  else Result := cat_desig.Count;
end;
//-----------------------------------------------------------
function StarName.GetCatalog(index:Integer):string;
begin
  Assert(index>=0);
  if (index>=GetCatalogCount) then Result:=''
  else Result := cat_desig[index];  
end;
//-----------------------------------------------------------
// returns certain catalogs
function StarName.GetDefaultCatalog:string;
begin
  Result := GetCat('LHS');
  if Length(Result)<>0 then Exit;
  Result := GetCat('2MASS');
  if Length(Result)<>0 then Exit;
  Result := GetCat('Tyc');
  if Length(Result)<>0 then Exit;
  Result := GetCat('LTT');
  if Length(Result)<>0 then Exit;
  Result := GetCat('NLTT');
  if Length(Result)<>0 then Exit;
  Result := GetCat('Gl');
  if Length(Result)<>0 then Exit;
  Result := GetCat('GJ');
  if Length(Result)<>0 then Exit;
  Result := GetCat('Hip');
  if Length(Result)<>0 then Exit;
  Result := GetCat('HR');
  if Length(Result)<>0 then Exit;
  Result := GetCat('HD');
  if Length(Result)<>0 then Exit;
  Result := GetCat('UCAC4');
  if Length(Result)<>0 then Exit;
  Result := GetCat('LP');
  if Length(Result)<>0 then Exit;
  Result := GetCat('LSPM');
  if Length(Result)<>0 then Exit;
  Result := GetCat('SCR');
  if Length(Result)<>0 then Exit;
  Result := GetCat('WISE');
  if Length(Result)<>0 then Exit;
  Result := GetCat('WISEP');
  if Length(Result)<>0 then Exit;
  Result := GetCat('ULAS');
  if Length(Result)<>0 then Exit;
  Result := GetCat('APMPM');
  if Length(Result)<>0 then Exit;
  // if result is still '' here, we try no more
end;
//------------------------------------------------
function StarName.GetRandomCat:string;
var listc,randdex:Integer;
begin
  // special cases where we cannot pick a catalog id at random
  Result := '';
  if cat_desig = nil then Exit;
  listc := cat_desig.Count;
  if listc = 0 then Exit;
  if listc = 1 then begin
    Result := cat_desig[0];
    Exit;
  end;
  // more than 1 catalog, we can pick random!
  randdex := RandomRange(0,listc);
  Result := cat_desig[randdex];
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* done *)
destructor StarName.Destroy;
begin
  cat_desig.Free;
  inherited;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  Randomize;
end.
