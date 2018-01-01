unit Arcins;

{$MODE Delphi}

interface

uses SysUtils,StrUtils,Classes,df_strings,stardata, namedata,
    unitdata,NewStar;

const
  cat_names:array[0..19] of string = ('Gliese','Gliese-Jahreiß','Hipparcos',
      'Luyten Half-Second','Woolley',
      'Fundamental Catalogue','Bright Star','Henry Draper',
      'Bonner Durchmusterung','Cordoba Durchmusterung',
      'Cape Photographic Durchmusterung','Luyten Two-Tenths',
      'Luyten Palomar','Giclas','Vyssotsky','Ross','Aitken Double Stars',
      'Eggen and Greenstein','Greenstein','Position-Based');

  cat_abbrev:array[0..19] of string = ('Gl','GJ','HIP','LHS','Wo','FK','BS','HD',
     'BD','CoD','CPD','LTT','LP','G','Vys','Ross','ADS','EG','Gr','NS');

  cat_urls:array[0..19] of string = (
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/gliese.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/gj.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/hip.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/lhs.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/woolley.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/fk/fkall.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/bs.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/hd.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/bd.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/cod.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/cpd.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/ltt.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/lp.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/giclas.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/vys.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/ross.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/ads.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/wd.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/wd.htm',
        'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/pos2000/pos2000all.htm');

  cat_sys:array[0..19] of Boolean = (True,True,False,False,True,False,False,False,
     False,False,False,False,False,False,True,True,True,False,False,True);

type

CatalogID = (CN_Gliese, CN_GJ,CN_HIP, CN_LHS,CN_Woolley,CN_FK, CN_HR, CN_HD,
     CN_BD,CN_CD, CN_CPD, CN_LTT, CN_LP, CN_Giclas, CN_Vys, CN_Ross, CN_ADS,
     CN_EG,CN_Gr, CN_Pos);

AricnsEntry = Class
  protected
    running_no:Integer;
    url:string;
    backlinks:array[0..19] of Integer;
    checked:Boolean;
  public
    (*the constructor*)
    constructor Create(nurl:string; inl:CatalogID; intin:Integer);
    (* check if the url matches, if so, set the catalog backlink,
    and return true *)
    function CheckAdd(nurl:string; inl:CatalogId; intin:Integer):Boolean;
    (* returns the url *)
    function GetUrl:string;
    (* checking and unchecking *)
    procedure SetChecked(inv:Boolean);
    function IsChecked:Boolean;
End;

AricnsListing = Class
  protected
    count:Integer;
    listing:array of AricnsEntry;
  public
    constructor Create;
    function GetCreate(nurl:string; inl:CatalogId; intin:Integer):AricnsEntry;
    // aricns cross ref
    function GetCount:Integer;
    procedure UncheckAll;
    function GetAtIndex(index:Integer):AricnsEntry;
    function IsChecked(index:Integer):Boolean;
    // done
    destructor Destroy; override;
End;

CatalogLinks = Class
  protected
    loaded:Boolean;
    // id data
    id:CatalogID;
    idname:string;
    idurl:string;
    id_abbrev:string;
    // link information
    listlen:Integer;
    catalog_ids:array of string;
    catalog_links:array of AricnsEntry;
    alist_link:AricnsListing;
    // custom internal methods
    function StripInput(var inpage:string):Boolean;
  public
    constructor Create(inid:CatalogID; alist:AricnsListing);
    // loading data
    function LoadData(inpage:string):Boolean;
    // getting information
    function GetAbbrev:string;
    function FindUrl(inp,component:string):string;
    function FindEntry(inp,component:string):AricnsEntry;
    function GetCatId(index:Integer):string;
End;

(* next up is a class that holds data from the aricns page, which is rather
loosley structured... we shall prevail **)
AricnsStar = Class
  protected
    data:TStringList;
    tempdata:TStringList;
    // private parsing function
    function Strip(var indata:string):Boolean;
    function TestLine(const linein:string):Boolean;
    function SplitLine(var line1:string; out line2:string):Boolean;
    // more private parsing functions
    function SecondParse(inval:string):Boolean;
    procedure Massage(var inval:string);
    procedure ReduceSpaces(var inval:string);
    // more private parsing functions
    function ProcessLine(const inval:string; out outval:string):Boolean;
  public
    constructor Create(indata:string);
    // external data access methods
    function GetAbsoluteMag(out magr:Real):Boolean;
    function GetApparentMag(out amag:Real):Boolean;
    function GetSpectra(out spectr:string):Boolean;
    function GetRadialV(out radv:Real):Boolean;
    function GetParallax(out para:Real):Boolean;
    function GetParaUnc(out punc:Real):Boolean;
    function GetCNSValue(out cns:string):Boolean;
    function GetPMMag(out pmm:Real):Boolean;
    function GetPMDir(out pmdir:Real):Boolean;
    function GetHip:string;
    function GetYPC:string;
    function GetAddDesig:string;
    // done
    destructor Destroy;override;
End;

(* a record to hold the general info, makes things neater *)
AricnsGlobal = record
  loaded:Boolean;
  catalogs:array[0..19] of CatalogLinks;
  urldata:AricnsListing;
end;


// an url getting function
function FindAricnsEntry(istar:StarSystem; comp:Integer):AricnsEntry;
function FindAricnsArity(istar:StarSystem):Integer;
function FindAricns(istar:StarSystem; comp:Integer):string;

// to get a list of designations is somewhat complicated
function GenerateDesignations(inval:AricnsEntry; modeval:Integer):string;

function GenerateDesignations1(inval:AricnsEntry):string;
function GenerateDesignations2(inval:AricnsEntry):string;

function FindDesignations(istar:StarSystem; comp:Integer; modeval:Integer):string;

procedure ClearAdat;

var
  I:Integer;
  adat:AricnsGlobal;

  ARICNS_CheckSystem:StarSystem;
  ARICNS_CheckStar:Integer;

implementation
//*************************************************************************
constructor AricnsEntry.Create(nurl:string; inl:CatalogID; intin:Integer);
var I,spos,epos,sc:Integer;
    buf:string;
begin
  // some basic stuff first
  Assert(intin>=0);
  for I := 0 to 19 do backlinks[I] := -1;
  checked := false;
  // the link
  spos := AnsiPos('cnspages/4c',nurl);
  Assert(spos<>0);
  url := nurl;
  // getting the running number
  buf := Copy(nurl,spos+11,length(nurl)-(spos+11)+1);
  epos := AnsiPos('.htm',buf);
  Assert(epos<>0);
  buf := Copy(buf,1,epos-1);
  Val(buf,I,sc);
  Assert(sc=0);
  running_no := I;
  // finally setting the first backlink
  backlinks[Ord(inl)] := intin;
end;
//---------------------------------------------------------
(* check if the url matches, if so, set the catalog backlink, and return true *)
function AricnsEntry.CheckAdd(nurl:string; inl:CatalogId; intin:Integer):Boolean;
begin
  if nurl=url then begin
    backlinks[Ord(inl)] := intin;
    Result := True;
  end
  else Result := False;
end;
//----------------------------------------------------------
(* returns the url *)
function AricnsEntry.GetUrl:string;
begin
  Result := url;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure AricnsEntry.SetChecked(inv:Boolean);
begin
  checked := inv;
end;
//----------------------------------------------------------
function AricnsEntry.IsChecked:Boolean;
begin
  Result := checked;
end;
//*************************************************************************
constructor AricnsListing.Create;
begin
  count :=0;
end;
//-------------------------------------------------
function AricnsListing.GetCreate(nurl:string; inl:CatalogId; intin:Integer):AricnsEntry;
var I:Integer;
    found:Boolean;
begin
  if count=0 then begin
    SetLength(listing,1);
    listing[0] := AricnsEntry.Create(nurl,inl,intin);
    count := 1;
    Result := listing[0];
  end
  else begin
    // looking for things
    for I := 0 to count - 1 do begin
      found := listing[I].CheckAdd(nurl,inl,intin);
      if found then Break;
    end;
    // if we have not found, we add new
    if not found then begin
      Inc(count);
      SetLength(listing,count);
      listing[count-1] := AricnsEntry.Create(nurl,inl,intin);
      Result := listing[count-1];
    end
    // otherwise, we return what we have found
    else begin
      Result := listing[I];
    end;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function AricnsListing.GetCount:Integer;
begin
  Result := Count;
end;
//-------------------------------------------------------------------------
procedure AricnsListing.UncheckAll;
var I:Integer;
begin
  for I := 0 to count - 1 do begin
    listing[I].SetChecked(False);
  end;
end;
//-----------------------------------------------------------------
function AricnsListing.GetAtIndex(index:Integer):AricnsEntry;
begin
  Assert(index>=0);
  Assert(index<count);
  Result := listing[index];
end;
//-----------------------------------------------------------------
function AricnsListing.IsChecked(index:Integer):Boolean;
begin
  Assert(index>=0);
  Assert(index<count);
  Result := listing[index].IsChecked;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// done
destructor AricnsListing.Destroy;
var I:Integer;
begin
  inherited;
  for I := 0 to count - 1 do listing[I].Free;
  SetLength(listing,0);
  count := 0;
end;
//*************************************************************************
// we trim header and footer data from an ARICNS catalog listing
function CatalogLinks.StripInput(var inpage:string):Boolean;
var len,spos,epos:Integer;
begin
  Result := False;
  // start
  spos := AnsiPos('<HR><BR>',inpage);
  if spos=0 then Exit;
  len := length(inpage);
  inpage := copy(inpage,spos+8,len-(spos+8)+1);
  // end
  epos := AnsiPos('<BR><BR>',inpage);
  if epos=0 then Exit;
  inpage := copy(inpage,1,epos-1);
  // done
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor CatalogLinks.Create(inid:CatalogID; alist:AricnsListing);
begin
  Assert(alist<>nil);
  loaded:=False;
  // id data
  id:=inid;
  idname:=cat_names[Ord(inid)];
  idurl:=cat_urls[Ord(inid)];
  id_abbrev:=cat_abbrev[Ord(inid)];
  // link information
  listlen := 0;
  SetLength(catalog_ids,0);
  SetLength(catalog_links,0);
  alist_link:=alist;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// takes a web page and sets the internal data from this
function CatalogLinks.LoadData(inpage:string):Boolean;
var urlstr,part1,part2:string;
    spos,len,qlen:Integer;
    tval1,tval2:string;
    qval1,qval2:string;
begin
  Result := False;
  // removing other parts of the page
  if not StripInput(inpage) then Exit;
  if (id=CN_FK) or (id=CN_Pos) then tval1 := '<a href="../'
  else tval1 := '<a href="./';
  // the loading loop
  while ExtrField(inpage,tval1,'</a>',urlstr,False) do begin
    // splitting the string into url and tag
    spos := AnsiPos('">',urlstr);
    if spos=0 then Exit;
    part1 := copy(urlstr,1,spos-1);
    len := length(urlstr);
    part2 := copy(urlstr,spos+2,len-(spos+2)+1);
    // processing part2
    part2 := Trim(part2);
    // since the position catalog does not use prefixes, we process differently
    if id<>CN_Pos then begin
      // if part2 does not start with right tage, we skip
      if not AnsiStartsStr(id_abbrev,part2) then Continue;
      // we remove the star abbreviation
      len := length(part2);
      qlen := length(id_abbrev);
      part2:= copy(part2,qlen+1,len-(qlen+1)+1);
      // fiddiling with the components
      part2 := AnsiReplaceStr(part2,'(A'  ,'A');
      part2 := AnsiReplaceStr(part2,'[B'  ,'B');
      part2 := AnsiReplaceStr(part2,'[C'  ,'C');
      part2 := AnsiReplaceStr(part2,'[D'  ,'D');
      part2 := AnsiReplaceStr(part2,'A'  ,' A');
      part2 := AnsiReplaceStr(part2,'B'  ,' B');
      part2 := AnsiReplaceStr(part2,'A B',' AB');
      part2 := AnsiReplaceStr(part2,'C'  ,' C');
      (* certain catalogs have the 'ARICNS comp.' thingy rather than just using A or B *)
      if (id=CN_ADS) or (id=CN_Ross) then begin
        if ExtrField(inpage,'<TD>','</TD>',tval2,False) then begin
          tval2 := trim(tval2);
          spos := AnsiPos('--> ARICNS comp.:',tval2);
          qlen := length(tval2);
          if spos<>0 then begin
            tval2 := Trim(Copy(tval2,spos+17,qlen-(spos+17)+1));
            part2 := part2 + ' ' + tval2;
          end;
        end;
      end;
    end
    // the position 'catalog' will be treated like an 'NS' desig
    else begin
      qval1 := Copy(part2,1,2)+ Copy(part2,4,2);
      qval1 := qval1 + Copy(part2,25,3) + Copy(part2,29,2);
      // processing the parts section
      if Length(part2)>32 then begin
        qval2 := Trim(Copy(part2,33,Length(part2)-33+1));
        if Length(qval2)>0 then begin
          if qval2='(A' then qval2:='A'
          else if qval2='[B' then qval2:='B'
          else if qval2='[C' then qval2:='C'
          else if qval2='[D' then qval2:='D';
          qval1 := qval1 + ' ' + qval2;
        end;
      end;
      // done
      part2 := qval1;
    end;
    // we build the url
    part1 := 'http://wwwadd.zah.uni-heidelberg.de/datenbanken/aricns/'+ part1;
    // processing the abbreviation
    part2 := Trim(part2);
    part2 := AnsiReplaceStr(part2,'   ',' ');
    part2 := AnsiReplaceStr(part2,'  ',' ');
    part2 := AnsiReplaceStr(part2,'  ',' ');
    // creating a new object
    Inc(listlen);
    SetLength(catalog_ids,listlen);
    SetLength(catalog_links,listlen);
    catalog_ids[listlen-1] := part2;
    catalog_links[listlen-1] := alist_link.GetCreate(part1,id,listlen-1); 
  end;
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function CatalogLinks.GetAbbrev:string;
begin
  Result := id_abbrev;
end;
//-----------------------------------------------------------------
function CatalogLinks.FindUrl(inp,component:string):string;
var I:Integer;
    qstr:string;
begin
  for I := 0 to listlen - 1 do begin
    if AnsiStartsStr(inp,catalog_ids[I]) then begin
      // we have a tentative match
      if inp=catalog_ids[I] then begin
        Result := catalog_links[I].GetUrl;
        Exit;
      end;
      // we try adding the component
      qstr := Trim(inp + ' ' + component);
      if qstr=catalog_ids[I] then begin
        Result := catalog_links[I].GetUrl;
        Exit;
      end;
      // no match
    end;
  end;
  // the end is nigh, we have failed to find an url!
  Result := '';
end;
//----------------------------------------------------------------
function CatalogLinks.FindEntry(inp,component:string):AricnsEntry;
var I:Integer;
    qstr:string;
begin
  for I := 0 to listlen - 1 do begin
    if AnsiStartsStr(inp,catalog_ids[I]) then begin
      // we have a tentative match
      if inp=catalog_ids[I] then begin
        Result := catalog_links[I];
        Exit;
      end;
      // we try adding the component
      qstr := Trim(inp + ' ' + component);
      if qstr=catalog_ids[I] then begin
        Result := catalog_links[I];
        Exit;
      end;
      // no match
    end;
  end;
  // the end is nigh, we have failed to find an url!
  Result := nil;
end;
//-------------------------------------------------------------------------
function CatalogLinks.GetCatId(index:Integer):string;
begin
  Assert(index>=0);
  Assert(index<listlen);
  Result := catalog_ids[index];
end;
//*************************************************************************
(* The general parsing method is this: once the unimportant stuff is stripped
away, we split the data into lines (separated not by <BR>, but by linefeeds ).
Line that start with < are tags only, discard. Names have : following them,
data in enclosed in <strong>...</strong>, anything after that is discarded
(unless there is another <strong>...</strong>). If a line does not have a : in it,
we discard that line as well. Before we extract that data, however,
we do a line split: check and see if there is a sequence of three &nbsp;, if
there is, and there is something after it, we treat that something as a separate
line. Processing includes replacing &nbsp; with spaces, &micro; with m, and
&plusminus; with +/- . <sub>...</sub> tags are also removed (but not the contents!),
any spaces are reduced to a single space
*)

// removing extra stuff that we do not examine for info
function AricnsStar.Strip(var indata:string):Boolean;
var spos,epos,len:Integer;
begin
  Result := False;
  // taking off the start
  spos := AnsiPos('<BR><BR><P>',indata);
  if (spos=0) then Exit;
  len := length(indata);
  indata := Copy(indata,spos+11,len-(spos+11)+1);
  // taking off the end
  epos := AnsiPos('<BR><BR><PRE>',indata);
  if (epos=0) then Exit;
  indata := Copy(indata,1,epos-1);
  // done
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.TestLine(const linein:string):Boolean;
var xchar:Char;
    spos:Integer;
begin
  Result := False;
  if linein='' then Exit;
  xchar := linein[1];
  if xchar='<' then Exit;
  spos := AnsiPos(':',linein);
  if spos=0 then Exit;
  Result := True;
end;
//-------------------------------------------------------------------------
// we see if a single line contains two datums
function AricnsStar.SplitLine(var line1:string; out line2:string):Boolean;
const tnbsp='&nbsp;&nbsp;&nbsp;';
var   spos,len,tlen:Integer;
begin
  spos := AnsiPos(tnbsp,line1);
  if spos<>0 then begin
    tlen := length(tnbsp);
    len := length(line1);
    line2 := copy(line1,spos+tlen,len-(spos+tlen)+1);
    line1 := copy(line1,1,spos-1);
    line2 := Trim(line2);
    line1 := Trim(line1);
    Result := (line2<>'') and (line2<>'&nbsp;');
  end
  else Result := False;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* after running strip, we use this function to get the data down to a list
of lines with data *)
function AricnsStar.SecondParse(inval:string):Boolean;
var first_list:TStringList;
    linebuf,buffer2:string;
    I:Integer;
begin
  // first step, linebreaking
  first_list := ParseByChar(inval,#10,false,false);
  Assert(first_list<>nil);
  Assert(first_list.Count>0);
  for I := 0 to first_list.Count - 1 do first_list[I] := Trim(first_list[I]);
  // now we go line by line for discarding and splitting
  tempdata := TStringList.Create;
  for I := 0 to first_list.Count - 1 do begin
    linebuf :=first_list[I];
    if not TestLine(linebuf) then Continue;
    // we test for line splitting
    if SplitLine(linebuf,buffer2) then begin
      tempdata.Add(linebuf);
      tempdata.Add(buffer2);
    end
    else tempdata.Add(linebuf);
  end;
  Result := True;
end;
//-------------------------------------------------------------------------
// the purpose of this is to replace HTML entities with characters
procedure AricnsStar.Massage(var inval:string);
begin
  inval := AnsiReplaceStr(inval,'&nbsp;',' ');
  inval := AnsiReplaceStr(inval,'&micro;','m');
  inval := AnsiReplaceStr(inval,'&plusmn;','±');
  inval := AnsiReplaceStr(inval,'&deg;','°');
  // sheesh
  inval := AnsiReplaceStr(inval,'&nbsp',' ');
end;
//-------------------------------------------------------------------------
procedure AricnsStar.ReduceSpaces(var inval:string);
var oval:string;
begin
  while True do begin
    oval := AnsiReplaceStr(inval,'  ',' ');
    if oval=inval then Break;
    inval := oval;
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// more private parsing functions
function AricnsStar.ProcessLine(const inval:string; out outval:string):Boolean;
var spos,len:Integer;
    bufn,buf1,buf2:string;
    nlc_inval:string;
begin
  Result := False;
  // extrating the name/label
  spos := AnsiPos(':',inval);
  if (spos=0) then Exit;
  bufn := Trim(copy(inval,1,spos-1));
  nlc_inval := inval;
  Massage(nlc_inval);
  // extracting the first..
  if not ExtrTagCont(nlc_inval,'strong',buf1,false) then begin
    spos := AnsiPos(':',inval);
    len :=length(nlc_inval);
    buf1 := copy(nlc_inval,spos+1,len-(spos+1)+1);
  end;
  // extracting the second
  if not ExtrTagCont(nlc_inval,'strong',buf2,false) then buf2 :='';
  // putting together and processing
  buf1 := Trim(buf1 + ' ' + buf2);
  ReduceSpaces(buf1);
  // detagging
  bufn := DeTag(bufn);
  buf1 := DeTag(buf1);
  // final result
  outval := bufn+'='+buf1;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor AricnsStar.Create(indata:string);
var I:Integer;
    tbuf:string;
    spacevel:Boolean;
begin
  // starting
  data := nil;
  tempdata := nil;
  spacevel := False;
  // first level parsing
  if not Strip(indata) then
    raise Exception.Create('ARICNS star page not formatted as expected! (1)');
  if not SecondParse(indata) then
    raise Exception.Create('ARICNS star page not formatted as expected! (2)');
  // next, we do the loop
  data := TStringList.Create;
  for I := 0 to tempdata.Count - 1 do begin
    // we try to take the raw data and get a datum
    if not ProcessLine(tempdata[I],tbuf) then begin
      raise Exception.Create('Something is wrong with the ARICNS star processing!');
    end;
    (* space velocity is stored in U, V, W... tag change to avoid conflict with
    apparent magnitude. *)
    if AnsiStartsStr('Space Velocity',tbuf) then spacevel := True;
    if spacevel then begin
      if AnsiStartsStr('U',tbuf) then tbuf := 'Vel ' + tbuf
      else if AnsiStartsStr('V',tbuf) then tbuf := 'Vel ' + tbuf
      else if AnsiStartsStr('W',tbuf) then tbuf := 'Vel ' + tbuf;
    end;
    // assigning
    data.Add(tbuf);
  end;
  // done
  tempdata.Free;
  tempdata := nil;  
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// external data acces methods
//-------------------------------------------------------------------------
// get absolute Magnitude
function AricnsStar.GetAbsoluteMag(out magr:Real):Boolean;
const numset='0123456789.+-';
var buf:string; outval:Real; sc:Integer;
    spos:Integer;
begin
  Result := False;
  // getting the value
  buf := data.Values['MV'];
  if buf='' then begin
    buf := data.Values['Mvis'];
    if buf='' then Exit;    
  end;
  // removing the last c
  spos := FindFirstNotOf(buf,numset,1);
  if spos>=1 then buf := Trim(Copy(buf,1,spos-1));
  // converting it
  Val(buf,outval,sc);
  if (sc<>0) then Exit;
  magr := outval;
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetApparentMag(out amag:Real):Boolean;
const numset='0123456789.+-';
var buf:string; outval:Real; sc:Integer;
    spos:Integer;
begin
  Result := False;
  // getting the value
  buf := data.Values['V'];
  if buf='' then Exit;
  // removing the last bit (if there)
  spos := FindFirstNotOf(buf,numset,1);
  if spos>=1 then buf := Trim(Copy(buf,1,spos-1));
  // converting it
  Val(buf,outval,sc);
  if (sc<>0) then Exit;
  amag := outval;
  Result := True;
end;
//-------------------------------------------------------------------------
// spectral data
function AricnsStar.GetSpectra(out spectr:string):Boolean;
var buf:string;
    qpos:Integer;
begin
  Result := False;
  // getting the value
  buf := data.Values['Spectral Type'];
  if buf='' then Exit;
  qpos := FindFirstOf(buf,'IVD',1);
  if qpos<=0 then spectr := buf + ' V'
  else spectr := buf;
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetRadialV(out radv:Real):Boolean;
var buf:string; outval:Real; sc:Integer;
begin
  Result := False;
  // getting the value
  buf := data.Values['RV'];
  if buf='' then Exit;
  buf := AnsiReplaceStr(buf,'+ ','+');
  buf := AnsiReplaceStr(buf,'- ','-');
  // converting it
  Val(buf,outval,sc);
  if (sc<>0) then Exit;
  radv := outval;
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetParallax(out para:Real):Boolean;
var buf:string; outval:Real;
    sc,spos:Integer;
begin
  Result := False;
  // getting the value
  buf := data.Values['Trig. Par.'];
  if buf='' then begin
    buf := data.Values['Res. Par. (SpT/UBVRI)'];
    if buf='' then begin
      buf := data.Values['Res. Par. (uvby-phot.)'];
      if buf='' then Exit;
    end;
  end;
  // looking for the divider
  spos := AnsiPos('±',buf);
  if spos=0 then Exit;
  // extracting
  buf := Trim(Copy(buf,1,spos-1));
  // converting it
  Val(buf,outval,sc);
  if (sc<>0) then Exit;
  para := outval;
  // done
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetParaUnc(out punc:Real):Boolean;
var buf,buf2:string; outval:Real;
    sc,spos,len,aftpos:Integer;
begin
  Result := False;
  // getting the value
  buf := data.Values['Trig. Par.'];
  if buf='' then begin
    buf := data.Values['Res. Par. (SpT/UBVRI)'];
    if buf='' then begin
      buf := data.Values['Res. Par. (uvby-phot.)'];
      if buf='' then Exit;
    end;
  end;
  // looking for the divider
  spos := AnsiPos('±',buf);
  if spos=0 then Exit;
  // extracting
  len := length(buf);
  aftpos := spos + Length('±');
  buf := Trim(Copy(buf,aftpos,len-aftpos+1));
  if not ExtractFirstWord(buf,buf2) then begin
    buf2 := buf;
  end;
  // converting it
  Val(buf2,outval,sc);
  if (sc<>0) then Exit;
  punc := outval;
  // done
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetCNSValue(out cns:string):Boolean;
var buf:string;
begin
  Result := False;
  // getting the value
  buf := data.Values['CNS designation'];
  if buf='' then Exit;
  // converting it
  cns := buf;
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetPMMag(out pmm:Real):Boolean;
var buf1,buf2:string; outval:Real;
    sc:Integer;
begin
  Result := False;
  // getting the value
  buf2 := data.Values['Total Proper Motion'];
  if buf2='' then Exit;
  // looking for the divider
  if not ExtractFirstWord(buf2,buf1) then Exit;
  // converting it
  Val(buf1,outval,sc);
  if (sc<>0) then Exit;
  pmm := outval;
  // done
  Result := True;
end;
//-------------------------------------------------------------------------
function AricnsStar.GetPMDir(out pmdir:Real):Boolean;
var buf1,buf2:string; outval:Real;
    sc:Integer;
begin
  Result := False;
  // getting the value
  buf2 := data.Values['Total Proper Motion'];
  if buf2='' then Exit;
  // looking for the divider
  if not ExtractFirstWord(buf2,buf1) then Exit;
  buf2 := Trim(AnsiReplaceStr(buf2,'°',' '));
  // converting it
  Val(buf2,outval,sc);
  if (sc<>0) then Exit;
  pmdir := outval;
  // done
  Result := True;
end;
//------------------------------------------------------------------------
function AricnsStar.GetHip:string;
begin
  Result := data.Values['HIP'];
end;
//------------------------------------------------------------------------
function AricnsStar.GetYPC:string;
begin
  Result := data.Values['YPC'];
end;
//------------------------------------------------------------------------
function AricnsStar.GetAddDesig:string;
begin
  // getting the value
  Result := data.Values['Additional Designations'];
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// done
destructor AricnsStar.Destroy;
begin
  inherited;
  data.Free;
  tempdata.Free;
end;
//******************************************************************
// a helper functions that checks if the star has a specified catalog *)
function CheckForCat(inname:StarName; tag,compstr:string; I:Integer):AricnsEntry;
var buf:string;
begin
  Result := nil;
  // we look for a value using the current catalog
  if inname.GetCatValue(tag,buf) then begin
    // we try to look up the name
    Result:= adat.catalogs[I].FindEntry(buf,compstr);
  end;
end;
//------------------------------------------------------------------
function LookForCat(pointer:StarName; I:Integer; compstr:string):AricnsEntry;
var rarcentry:AricnsEntry;
    tag,otag:string;
begin
  // we look for the category in the current name
  tag := cat_abbrev[I];
  rarcentry := CheckForCat(pointer,tag,compstr,I);
  if rarcentry<>nil then begin
    Result:= rarcentry;
    Exit;
  end;
  // for certain categories, we will also look for alternatives
  otag := tag;
  case I of
    2 : tag := 'Hip';
    5 : tag := 'FK5';
    6 : tag := 'HR';
    9 : tag := 'CD';
    10: tag := 'CP(D)';
  end;
  if (otag<>tag) then begin
    rarcentry := CheckForCat(pointer,tag,compstr,I);
    if rarcentry<>nil then begin
      Result:= rarcentry;
      Exit;
    end;
  end;
  // we are done looking up the catalog name
  Result := nil;
end;
//------------------------------------------------------------------
function FindAricnsEntry(istar:StarSystem; comp:Integer):AricnsEntry;
var I:Integer;
    pointer:StarName;
    cstar:NewStarBase;
    compstr:string;
    rarcentry:AricnsEntry;
begin
  // starting checks
  Assert(adat.loaded);
  Assert(istar<>nil);
  Result := nil;
  compstr :='';
  // if there is more than 1 star in the system, we check sub catalogs first
  if istar.GetCompC<>1 then begin
    compstr := completters[comp-1];
    cstar := istar.GetNewStar(comp);
    if cstar.HasNames then begin
      pointer := cstar.GetNames;
      for I := 0 to 19 do begin
        rarcentry := LookForCat(pointer,I,compstr);
        if rarcentry<>nil then begin
          Result := rarcentry;
          Exit;
        end;
      end;
    end;
  end;
  // we use the global names
  pointer := istar.GetNames;
  for I := 0 to 19 do begin
    rarcentry := LookForCat(pointer,I,compstr);
    if rarcentry<>nil then begin
      Result := rarcentry;
      Exit;
    end;
  end;
  // done
  // we have failed!
end;
//----------------------------------------------------------------
(* sometimes, we get an entry that has one star listed when there are
actually multiple (in ARICNS). This method can be used to count the
number of entries, using the component strings *)
function FindAricnsArity(istar:StarSystem):Integer;
const
  syscats:array[0..6] of Integer = (0,1,4,14,15,16,19);
var
  compstr,buf,tag:string;
  catidx, aloop:Integer;
  arity:array[0..6] of Integer;
  pointer:StarName;
  rarcentry:AricnsEntry;
begin
  // preliminary stuff
  pointer := istar.GetNames;
  // we loop thru the catalogs that categorize by system instead of star
  for catidx := 0 to 6 do begin
    arity[catidx] :=0;
    // to save some time, we check if the star even has this catalog
    tag := cat_abbrev[syscats[catidx]];
    if not pointer.GetCatValue(tag,buf) then Continue;
    // looping thru the component indexes
    for aloop := 0 to 11 do begin
      compstr := completters[aloop];
      // trying to look up the item
      rarcentry := LookForCat(pointer,syscats[catidx],compstr);
      // if found, we increment the result
      if rarcentry<>nil then Inc(arity[catidx]);
    end;

  end;
  // we now loop to find the highest arity
  Result := 0;
  for catidx := 0 to 6 do begin
    if arity[catidx]>Result then Result := arity[catidx];
  end;
  // if the result is higher than 1, as expected
  if (Result>=2) then Exit;
  (* if the result is 0, then there are no parts (this should not happen as
  this function should only be used if the regular designation search fails,
  and it normally works (if there is anything to match) for single stars), OR
  the part name is actually 'AB')        *)
  // we will use 0 as a normal exit, 1 for odd names, so we check for AB
  if Result=0 then begin
    // look for AB
    for catidx := 0 to 6 do begin
      arity[catidx] := 0;
      compstr := 'AB';
      rarcentry := LookForCat(pointer,syscats[catidx],compstr);
      // if found, we increment the result
      if rarcentry<>nil then arity[catidx] := 1;
    end;
    // seeing if asy AB was found
    for catidx := 0 to 6 do begin
      if arity[catidx]=1 then begin
        Result := 1;
        Break;
      end;
    end;
  end;
  // here, the result is 1 if we found AB or an isolated part (user check req)
  // the result is 0 if no parts ocurr
end;
//******************************************************************
// an url getting function
function FindAricns(istar:StarSystem; comp:Integer):string;
var qval:AricnsEntry;
begin
  qval := nil;
  qval := FindAricnsEntry(istar,comp);
  if qval<>nil then Result := qval.GetUrl
  else Result :='';
end;
//*****************************************************************
// modeval 0 (both), 1 stars, 2 system
function GenerateDesignations(inval:AricnsEntry; modeval:Integer):string;
var res1,res2:string;
begin
  // we start
  Assert(modeval<3);
  res1 := '';
  res2 := '';
  // next...
  if (modeval<>2) then res1 := GenerateDesignations1(inval);
  if (modeval<>1) then res2 := GenerateDesignations2(inval);
  Result := res1;
  if (res2<>'') then begin
    if Result<>'' then Result := Result + ',';
    Result := Result + res2;
  end;
end;
//------------------------------------------------------------------
// Generate designations for catalogs that number stars
function GenerateDesignations1(inval:AricnsEntry):string;
var I,indexv,len:Integer;
    buf:string;
begin
  // we do a loop to see all of the catgory names, we skip NS
  Result := '';
  for I := 0 to 18 do begin
    // we skip over the system catalogs
    if (cat_sys[I]) then Continue;
    // finding the catalog index
    indexv := inval.backlinks[I];
    if indexv=-1 then Continue;
    // preparing the catalog abbreviation at the start
    buf := cat_abbrev[I]+' ';
    if buf='BS ' then buf := 'HR ';
    if buf='CoD ' then buf := 'CD ';
    if buf='HIP ' then buf := 'Hip ';
    // building the result string
    buf := buf + adat.catalogs[I].GetCatId(indexv);
    Result := Result + buf + ',';
  end;
  // we remove any trailing commas (unles len=0 this is always the case)
  len := length(Result);
  if (len<>0) then begin
    if Result[len]=',' then Result := Copy(result,1,len-1);
  end;
end;
//------------------------------------------------------------------
// Generate designations for catalogs that number systems
function GenerateDesignations2(inval:AricnsEntry):string;
var I,cpos, indexv,len:Integer;
    buf1,buf2:string;
begin
  // we do a loop to see all of the catgory names
  Result := '';
  for I := 0 to 18 do begin
    // we skip over the non-system catalogs
    if not (cat_sys[I]) then Continue;
    // finding the catalog index
    indexv := inval.backlinks[I];
    if indexv=-1 then Continue;
    // getting the catalog abbreviation at the start
    buf1 := cat_abbrev[I]+' ';
    // getting and prepping the second part
    buf2 := Trim(adat.catalogs[I].GetCatId(indexv));
    // for these catalogs, there are no spaces before the component
    cpos := AnsiPos(' ',buf2);
    if cpos<>0 then buf2 := Copy(buf2,1,cpos-1);
    // building the result string
    buf1 := buf1 + buf2;
    Result := Result + buf1 + ',';
  end;
  // we remove any trailing commas (unles len=0 this is always the case)
  len := length(Result);
  if (len<>0) then begin
    if Result[len]=',' then Result := Copy(result,1,len-1);
  end;
end;
//*****************************************************************
// to get a list of designations is somewhat complicated
function FindDesignations(istar:StarSystem; comp:Integer; modeval:Integer):string;
var qval:AricnsEntry;
begin
  // getting the star
  qval := FindAricnsEntry(istar,comp);
  if qval=nil then begin
    Result := '';
    Exit;
  end;
  // doing
  Result := GenerateDesignations(qval, modeval);
end;
//*****************************************************************
// empties out adat
procedure ClearAdat;
var I:Integer;
begin
  // clearing the catalogs
  for I := 0 to 19 do begin
    if adat.catalogs[I]<>nil then begin
      adat.catalogs[I].Free;
      adat.catalogs[I] := nil;
    end;
  end;
  // removing url data
  if (adat.urldata)<>nil then adat.urldata.Free;
  adat.urldata := nil;
  adat.loaded := False;
  // finishing
  ARICNS_CheckSystem := nil;
  ARICNS_CheckStar := 0;
end;
//*****************************************************************

begin
  adat.loaded := False;
  adat.urldata := nil;
  for I := 0 to 19 do adat.catalogs[I]:= nil;
  ARICNS_CheckSystem := nil;
  ARICNS_CheckStar := 0;
end.
