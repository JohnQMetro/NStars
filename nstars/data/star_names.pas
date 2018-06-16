unit star_names;
{$mode delphi}
interface
//***************************************************************************
(* Replacing the old StarName with a class that handles things faster *)
uses
  Classes, SysUtils, StrUtils, contnrs, character, Math, unitdata, df_strings;

type

// catalog
CatalogID = class
  protected
    function InitCore(source:string; full:Boolean; out cattag:string):Boolean;
  public
    raw:string; // the full id
    id:string; // the part without the catalog tag or component suffix
    comp:string; // component suffix

    function Init(source:string; out cattag:string):Boolean;
    function InitFull(source:string; out cattag:string):Boolean;
    function MakeCopy():CatalogID;
end;

TBayerSuperscript = 0..9;

(* Stores catalog ids and names for both systems and individual stars *)
StarNames = class
  protected
    catalogs:TFPHashObjectList;
    function ISDBProcess(inv:string):string;
    function GetCatalog(index:Integer):string;
    function gcatc():Integer;
    function ncatz():Boolean;
    function isempt():Boolean;
  public
    proper_name:string;
    var_designation:string;
    bayer_sup:TBayerSuperscript;

    property CatalogCount:Integer read gcatc;
    property NoCatalogs:Boolean read ncatz;
    property IsEmpty:Boolean read isempt;

    (* the constructor *)
    constructor Create; overload;
    constructor Create(source:StarNames); overload;
    (* Utilities *)
    function Search(infind:string; conste:Integer):Boolean;
    function AnyCatEquiv(check_against:StarNames):Boolean;
    procedure TransferCatalogs(target:StarNames; const comp:string);
    (* managing the catalog list *)
    function SetCat(inval:string):Boolean;
    function SetMultipleCat(inval:string):Boolean;
    function SetISDBCat(inval:string):Boolean;
    function DelCat(incat:string):Boolean;
    function DeleteThis(value:string):Boolean;
    function RenameCats(incats:TStringList):Integer;
    (* Getting individual catalog info *)
    function GetCat(inval:string):string;
    function HasCat(const tag:string):Boolean;
    function GetCatValue(const cc:string; out value:string):Boolean;
    function GetDefaultCatalog:string;
    function GetRandomCat:string;
    (* returns the entire catalog list (for display) *)
    function GetList:TStringList;
    function ExtractNonSystemCats:TStringList;
    (* string I/O *)
    function ToIOString:string;
    procedure FromString(inval:string);
    function ListOfCats:string;
    function LimitedListofCats(maxlen:Integer):string;
    (* done *)
    destructor Destroy; override;
end;

// helpers
function ComponentsKindaMatch(const first:CatalogID; const second:CatalogID):Boolean;
function NotDigit(const test:Char):Boolean;
function MSortList(delim:Char):TStringList;

const
  TwoWordIDS:array[0..0] of String = ('Gaia');
  JCats:array[0..10] of String = ('2MASS','WDS','APMPM','DENIS','SCR','UPM',
          'SIPS','SDSS','WISE','WISEA','PM');
//***************************************************************************
implementation
//=========================================================================
function CatalogID.InitCore(source:string; full:Boolean; out cattag:string):Boolean;
var qpos,qposb:Integer;
    xrest:string;
    xchar,ychar:Char;
    complen,compdex:Integer;
    xmix:Real;
begin
  Result := False;
  if full then source := AnsiReplaceStr(source,'−','-');
  // getting the catalog tag
  qpos := PosSetEx(' +-',source,1);
  if qpos < 1 then Exit;
  cattag := Trim(Copy(source,1,qpos-1));
  if cattag = '' then Exit;
  if AnsiIndexStr(cattag,TwoWordIDS) >= 0 then begin
    qposb := PosSetEx(' +-',source,qpos+1);
    if qposb < (qpos+2) then Exit;
    cattag += source[qpos] + Trim(Copy(source,qpos+1,qposb - (qpos+1)));
    qpos := qposb;
  end;
  // some catalog tag modifications
  if cattag = 'GaiaDR1' then cattag := 'Gaia DR1'
  else if cattag = 'GaiaDR2' then cattag := 'Gaia DR2'
  else if full then begin
       if cattag = 'TYC' then cattag := 'Tyc'
       else if cattag = 'MCC' then cattag := 'Vys'
       else if cattag = 'PLX' then cattag := 'YPC'
       else if cattag = 'STF' then cattag := 'Struve'
       else if cattag = 'BU' then cattag := 'Bu'
       else if (cattag = 'DENIS') and (source[qpos] = '-') then begin
         if (Length(source) > (qpos+1)) and (source[qpos+1] = 'P') then begin
           cattag := 'DENIS-P';
           qpos += 2;
         end;
       end;
  end;
  // the id part
  xrest := Trim(RightStr(source,Length(source)-qpos+1));
  if xrest = '' then Exit;
  if full then begin
     if (AnsiIndexStr(cattag,JCats) >= 0) and (xrest[1]<>'J') then xrest := 'J' + xrest;
  end;
  // checking if there is a component at the end...
  if Length(xrest) > 1 then begin
    xchar := xrest[High(xrest)-1];
    ychar := xrest[High(xrest)];
    if (not IsDigit(ychar)) or NotDigit(xchar) then begin
      // if there if a component, we scan backwards until we find a numeric digit
      complen := 1;
      for compdex := (High(xrest)-1) downto 1 do begin
        if IsDigit(xrest[compdex]) then Break;
        Inc(complen);
      end;
      if complen = Length(xrest) then Exit;
      // splitting
      comp := Trim(AnsiRightStr(xrest,complen));
      xrest := Trim(AnsiLeftStr(xrest,Length(xrest)-complen));
    end;
    // demergeing GJ and EGGR
    if full and ((cattag = 'GJ') or (cattag = 'EGGR')) then begin
      if TryStrToFloat(xrest,xmix) then begin
        if cattag = 'GJ' then begin
          if xmix < 1000 then cattag := 'Gl'
          else if xmix > 9000 then cattag := 'Wo';
        end else begin
          if xmix < 203 then cattag := 'EG'
          else cattag := 'Gr';
        end;
      end;
    end;
  end;
  id := xrest;
  // producing 'raw' (like source, but with a space between id and component)
  raw := cattag + source[qpos] + id;
  if comp <> '' then raw += ' ' + comp;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function CatalogID.Init(source:string; out cattag:string):Boolean;
begin
  Result := InitCore(source,False,cattag);
end;
//------------------------------------
function CatalogID.InitFull(source:string; out cattag:string):Boolean;
begin
  Result := InitCore(source,True,cattag);
end;
//-------------------------------------
function CatalogID.MakeCopy():CatalogID;
begin
  Result := CatalogID.Create;
  Result.comp := comp;
  Result.id := id;
  Result.raw := id;
end;
//=====================================================================

function StarNames.ISDBProcess(inv:string):string;
var buf2:string;
begin
  if ExtrField(inv,'(',')',buf2,false) then begin
    Result := Trim(buf2) + ' ' + Trim(inv);
  end
  else Result := inv;
end;
//-----------------------------------------------------------
function StarNames.GetCatalog(index:Integer):string;
var cid:CatalogID;
begin
  Assert(index>=0);
  if (index >= CatalogCount) then Result:=''
  else begin
    cid := catalogs.Items[index] as CatalogID;
    Result := cid.raw;
  end;
end;
//------------------------------------
function StarNames.gcatc():Integer;
begin
  if catalogs = nil then Result := 0
  else Result := catalogs.Count;
end;
//-------------------------------
function StarNames.ncatz():Boolean;
begin
  if catalogs = nil then Result := True
  else Result := (catalogs.Count = 0);
end;
//----------------------------------
function StarNames.isempt():Boolean;
begin
  Result := False;
  if (bayer_sup<>0) or (proper_name<>'') then Exit;
  if (var_designation<>'') then Exit;
  if NoCatalogs then Exit;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor StarNames.Create;
begin
  catalogs := TFPHashObjectList.Create();
  proper_name := '';
  var_designation := '';
  bayer_sup:=0;
end;
//-----------------------------------------------
constructor StarNames.Create(source:StarNames);
var catmax,catdex:Integer;
    cattag:string;
    tempcat:CatalogID;
begin
  if source <> nil then begin
    proper_name := source.proper_name;
    var_designation := source.var_designation;
    bayer_sup := source.bayer_sup;
    // copying catalog ids
    catalogs := TFPHashObjectList.Create();
    catmax := source.catalogs.Count-1;
    if (catmax >= 0) then begin
       for catdex := 0 to catmax do begin
         cattag := source.catalogs.NameOfIndex(catdex);
         tempcat := source.catalogs.Items[catdex] as CatalogID;
         catalogs.Add(cattag,tempcat.MakeCopy());
       end;
    end;
  end else Fail;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* gets the catgeory code *)
function StarNames.Search(infind:string; conste:Integer):Boolean;
var buf,cattag:string;
    tempcat,foundcat:CatalogID;
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
  Result := False;
  if not NoCatalogs then begin
    tempcat := CatalogID.Create;
    try
      if not tempcat.InitFull(infind,cattag) then Exit;
      foundcat := catalogs.Find(cattag) as CatalogID;
      if (foundcat = nil) then Exit;
      if (foundcat.id <> tempcat.id) then Exit;
      // handling the components ads extra complication
      Result := ComponentsKindaMatch(tempcat,foundcat);
    finally
      tempcat.Free;
    end;
  end;
end;
//--------------------------------------------------------
function StarNames.AnyCatEquiv(check_against:StarNames):Boolean;
var catdex,catmax:Integer;
    currtag:string;
    currid,targid:CatalogID;
begin
  // various 'no catalogs' cases to get out of the way
  Result := False;
  if check_against = nil then Exit;
  if check_against.NoCatalogs then Exit;
  if NoCatalogs then Exit;
  // we now loop thru the catalogs, and look for them in check_against
  Result := True;
  catmax := catalogs.Count - 1;
  for catdex := 0 to catmax do begin
    // getting a catalog id
    currtag := catalogs.NameOfIndex(catdex);
    currid := catalogs.Items[catdex] as CatalogID;
    // looking for a match in check_against
    targid := check_against.catalogs.Find(currtag) as CatalogID;
    if (targid = nil) then Continue;
    if (targid.id <> currid.id) then Continue;
    // if we get this far, there is still the component matching issue
    if ComponentsKindaMatch(currid,targid) then Exit;
  end;
  // if we get here, nothing has been found...
  Result := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure StarNames.TransferCatalogs(target:StarNames; const comp:string);
var cmax,cindex:Integer;
    ctag:string;
    currid,newid:CatalogID;
begin
  if NoCatalogs or (target = nil) then Exit;
  if target.catalogs = nil then target.catalogs := TFPHashObjectList.Create();
  cmax := CatalogCount - 1;
  for cindex := 0 to cmax do begin
    ctag := catalogs.NameOfIndex(cindex);
    currid := catalogs.Items[cindex] as CatalogID;
    if target.HasCat(ctag) then target.DelCat(ctag);
    newid := currid.MakeCopy();
    if comp <> '' then begin
      newid.comp := comp;
      newid.raw := ctag + ' ' + newid.id + ' ' + comp;
    end;
    target.catalogs.Add(ctag,newid);
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* managing the catalog list *)
//-----------------------------------------------------------
function StarNames.SetCat(inval:string):Boolean;
var cstr:string;
    tempid:CatalogID;
    fdex:Integer;
begin
  // preliminary checks and preps
  Result := False;
  CollapseWhiteSpace(inval);
  tempid := CatalogID.Create;
  if not tempid.InitFull(inval,cstr) then begin
    tempid.Free; Exit;
  end;
  Result := True;
  // adding or replacing
  fdex := catalogs.FindIndexOf(cstr);
  if (fdex >= 0) then catalogs.Delete(fdex);
  catalogs.Add(cstr,tempid);
end;
//----------------------------------------------------------
function StarNames.SetMultipleCat(inval:string):Boolean;
var liststr:TStringList;
    I:Integer;
begin
  // starting, incl parsing the list
  Result := False;
  liststr := ParseByChar(inval,',',false,false);
  try
    if liststr.Count=0 then Exit;
    // looping
    for I := 0 to liststr.Count - 1 do begin
        if not SetCat(liststr[I]) then  Exit;
    end;
    Result := True;
  finally
    liststr.Free;
  end;
end;
//----------------------------------------------------------
function StarNames.SetISDBCat(inval:string):Boolean;
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
function StarNames.DelCat(incat:string):Boolean;
var catdex:Integer;
begin
  catdex := catalogs.FindIndexOf(incat);
  Result := False;
  if (catdex < 0) then Exit;
  catalogs.Delete(catdex);
  Result := True;
end;
//----------------------------------------------------------
function StarNames.DeleteThis(value:string):Boolean;
var tempid:CatalogID;
    tag:string;
    index:Integer;
begin
  Result := False;
  if NoCatalogs then Exit;
  tempid := CatalogID.Create;
  if not tempid.InitFull(value,tag) then Exit;
  tempid.Free;
  index := catalogs.FindIndexOf(tag);
  if (index < 0) then Exit;
  catalogs.Delete(index);
  Result := True;
end;
//-----------------------------------------------
function StarNames.RenameCats(incats:TStringList):Integer;
var I,caindex,lcount:Integer;
    tempid:CatalogID;
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
    caindex := catalogs.FindIndexOf(tag);
    if caindex < 0 then Continue;
    // taking it out if we find it (more complex than it should be)
    tempid := catalogs.Items[caindex] as CatalogID;
    tempid := tempid.MakeCopy();
    catalogs.Delete(caindex);
    // modifying tempid
    value := incats[2*I+1];
    rstring := value + ' ' + tempid.id;
    if tempid.comp <> '' then rstring += ' ' + tempid.comp;
    tempid.raw:= rstring;
    // and inserting the new entry
    caindex := catalogs.FindIndexOf(value);
    if (caindex >= 0) then catalogs.Delete(caindex);
    catalogs.Add(value,tempid);
    Inc(result);
  end;
end;
//-----------------------------------------------------------
function StarNames.GetCat(inval:string):string;
var tempid:CatalogID;
begin
  tempid := catalogs.Find(inval) as CatalogID;
  if (tempid = nil) then Result := ''
  else Result := tempid.raw;
end;
//----------------------------------------
function StarNames.HasCat(const tag:string):Boolean;
begin
  if catalogs = nil then Result := False
  else if NoCatalogs then Result := False
  else Result := (catalogs.FindIndexOf(tag) > 0);
end;
//-----------------------------------------------------------
function StarNames.GetCatValue(const cc:string; out value:string):Boolean;
var tempid:CatalogID;
begin
  // first look for the index
  tempid := catalogs.Find(cc) as CatalogID;
  if (tempid = nil) then Result := False
  else begin
    if tempid.comp = '' then value := tempid.id
    else value := tempid.id + ' ' + tempid.comp;
    Result := True;
  end;
end;

//-----------------------------------------------------------
// returns certain catalogs
function StarNames.GetDefaultCatalog:string;
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
  Result := GetCat('WISEA');
  if Length(Result)<>0 then Exit;
  Result := GetCat('ULAS');
  if Length(Result)<>0 then Exit;
  Result := GetCat('Gaia DR2');
  if Length(Result)<>0 then Exit;
  Result := GetCat('APMPM');
  if Length(Result)<>0 then Exit;
  // if result is still '' here, we try no more
end;
//------------------------------------------------
function StarNames.GetRandomCat:string;
var listc,randdex:Integer;
begin
  // special cases where we cannot pick a catalog id at random
  Result := '';
  if NoCatalogs then Exit;
  listc := CatalogCount;
  if listc = 1 then Result := GetCatalog(0)
  else begin
    // more than 1 catalog, we can pick random!
    randdex := RandomRange(0,listc);
    Result := GetCatalog(randdex);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function StarNames.GetList:TStringList;
var outlist:TStringList;
    cmax,cdex:Integer;
    cid:CatalogID;
begin
  if NoCatalogs then Result := nil
  else begin
    outlist := TStringList.Create;
    outlist.Sorted := True;
    cmax := CatalogCount - 1;
    for cdex := 0 to cmax do begin
      cid := catalogs.Items[cdex] as CatalogID;
      outlist.Add(cid.raw);
    end;
    Result := outlist;
  end;
end;
//---------------------------------------------
function StarNames.ExtractNonSystemCats:TStringList;
var catdex:Integer;
    sysfound:Boolean;
    tag:string;
    cid:CatalogID;
begin
  Result := nil;
  if NoCatalogs then Exit;
  Result := MSortList(',');
  // looping over the catalogs to see if they are system cats...
  for catdex := (catalogs.Count -1) downto 0 do begin
    tag := catalogs.NameOfIndex(catdex);
    sysfound := (AnsiIndexStr(tag,sys_cats) >= 0);
    // here, if sysfound is still False, we move the string to Result
    if (not sysfound) then begin
      cid := catalogs.Items[catdex] as CatalogID;
      Result.Add(cid.raw);
      catalogs.Delete(catdex);
    end;
  end;
  // some final checks
  if Result.Count = 0 then FreeAndNil(Result);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* string I/O *)
function StarNames.ToIOString:string;
var buf1:string;
    cdex,cmax:Integer;
    cid:CatalogID;
    xlist:TStringList;
begin
  // proper name and variable name
  Result := proper_name + ';' +  var_designation + ';';
  // bayer superscript
  Str(bayer_sup,buf1);
  Result := Result + Trim(buf1) + ';';
  // we now convert catalog names to csv
  xlist := MSortList(',');
  if not NoCatalogs then begin
    cmax := CatalogCount - 1;
    for cdex := 0 to cmax do begin
      cid := catalogs.Items[cdex] as CatalogID;
      xlist.Add(cid.raw);
    end;
  end;
  Result := Result + xlist.DelimitedText;
  xlist.Free;;
end;
//-----------------------------------------------------------
(* conversion from string, here, we allow exceptions! *)
procedure StarNames.FromString(inval:string);
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
  catalogs.Clear;
  if icns<>'' then begin
    // using the SetMultipleCat method
    tval := SetMultipleCat(icns);
    if not tval then
      raise Exception.Create('There are problems with the catalog names');
  end;
  // done
end;
//-----------------------------------------------------------
function StarNames.ListOfCats:string;
var I,cmax:Integer;
    cid:CatalogID;
    xlist:TStringList;
begin
  if not NoCatalogs then begin
    xlist := MSortList(';');
    cmax := CatalogCount-1;
    for I := 0 to cmax do begin
      cid := catalogs.Items[I] as CatalogID;
      xlist.Add(cid.raw);
    end;
    Result := xlist.DelimitedText;
    xlist.Free;
  end else Result := '';
end;
//------------------------------------------------
function StarNames.LimitedListofCats(maxlen:Integer):string;
var runlen,curlen,usec:Integer;
    prefdex,prefmax:Integer;
    olist:TStringList;
    curcat:string;
    cid:CatalogID;
begin
  (* since the result has a maximum list, we use an array of preferred catalog
  ids to pick the most important catalogs... *)
  Result := '';
  if NoCatalogs then Exit;
  runlen := 0;
  usec := 0;
  prefmax := High(prefcats);
  olist := MSortList(';');
  // looping over the cats
  for prefdex := 0 to prefmax do begin
    cid := catalogs.Find(prefcats[prefdex]) as CatalogID;
    if cid = nil then Continue;
    if (Length(cid.raw)+runlen+1) > maxlen then Break;
    olist.Add(cid.raw);
    runlen += Length(cid.raw)+1;
    Inc(usec);
    if usec = CatalogCount then Break;
  end;
  // afterwards, get rid of any slashes
  Result := AnsiReplaceStr(olist.DelimitedText,'/','-');
  olist.Free;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* done *)
destructor StarNames.Destroy;
begin
  catalogs.Free;
  inherited;
end;
//==============================================================
// handling the components adds extra complication
function ComponentsKindaMatch(const first:CatalogID; const second:CatalogID):Boolean;
begin
  Result := True;
  if (first.comp = '') or (second.comp = '') then Exit;
  if first.comp = second.comp then Exit;
  if AnsiContainsStr(first.comp,second.comp) then Exit;
  if AnsiContainsStr(second.comp,first.comp) then Exit;
  Result := False;
end;
//-----------------------------------------------
function NotDigit(const test:Char):Boolean;
begin
  Result := False;
  if IsDigit(test) then Exit;
  if (test = '.') or (test = '+') or (test = '-') then Exit;
  Result := True;
end;
//-------------------------------------------------
function MSortList(delim:Char):TStringList;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := delim;
  Result.Sorted := True;
end;

//***************************************************************************
end.

