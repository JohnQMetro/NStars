unit nltt_match;

{$mode delphi}

(* For use in cross-matching with NLTT, I have created a NLTT to Gaia DR2 cross
match file for NLTT idents not in Simbad. *)

interface
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
uses
  Classes, SysUtils, df_strings, fgl, star_names;

type

(* Holds cross-match data for an NLTT id *)
NLTTcm = class
  protected
    split_data:array of TStringList;
    nlttid,extra_id:string;
    // propert functions
    function pfcount():Integer;
    function pfgetnltt():string;
    function pfgaia1():string;
    function pfexid():string;
    function pfmatch():Boolean;
  public
    property Count:Integer read pfcount;
    property NLTT:string read pfgetnltt;
    property FirstGaiaID:string read pfgaia1;
    property FirstExtraID:string read pfexid;
    property Matched:Boolean read pfmatch;

    constructor Create(line_one:TStringList);
    destructor Destroy; override;
    function AddGaiaLine(extra_line:TStringList):Boolean;
    procedure SetMatched();
    procedure AppendToFile(var target:TextFile);
    function CloserThan(pllx:Real):Boolean;
end;

NLTT_List = TFPGObjectList<NLTTcm>;

(* Holds, reads, and writes cross-match data for NLTT *)
NLTT_CrossMatch = class
  protected
    main:NLTT_List;
    infile:TFileName;
    firstline:string;
  public
    // counts when doing matches
    skip_m,skip_t,count_match,count_unmatch:Integer;

    constructor Create;
    destructor Destroy;
    function LoadFromFile(fname:TFileName):Integer;
    function DoMatching():Boolean;
    function WriteToFile(unmat:Boolean; minpllx:Real):Boolean;
    function MakeCountsMessage():string;
end;

var
    nltt_matcher:NLTT_CrossMatch;


//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//================================================================
(*  split_data:array of TStringList;
    nltt,extra_id:string; *)

// property functions
//---------------------------
function NLTTcm.pfcount():Integer;
begin  Result := Length(split_data);  end;
//---------------------------
function NLTTcm.pfgetnltt():string;
begin  Result := nlttid;  end;
//---------------------------
function NLTTcm.pfgaia1():string;
begin  Result := split_data[0][15];  end;
//---------------------------
function NLTTcm.pfexid():string;
begin
  if StrStartswAny(extra_id,['L ','LP ','Ross ','Wolf ']) then Result := extra_id
  else Result := '';
end;
//-------------------------------
function NLTTcm.pfmatch():Boolean;
begin  Result := (split_data[0][1] = 'T');  end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor NLTTcm.Create(line_one:TStringList);
begin
  // reject
  if (line_one = nil) or (line_one.Count < 16) then Fail;
  if (line_one[2] = '') then Fail;
  if Length(line_one[15]) < 10 then Fail;
  // accept
  SetLength(split_data,1);
  split_data[0] := line_one;
  nlttid := line_one[0];
  extra_id := line_one[8];
end;
//---------------------------
destructor NLTTcm.Destroy;
var ddex,dmax:Integer;
    outline:string;
begin
  dmax := Length(split_data)-1;
  for ddex := 0 to dmax do split_data[ddex].Free;
  SetLength(split_data,0);
end;
//---------------------------
function NLTTcm.AddGaiaLine(extra_line:TStringList):Boolean;
var ndex:Integer;
begin
  Result := False;
  // bad input, or different NLTT id
  if (extra_line = nil) or (extra_line.Count < 16) then Exit;
  if (extra_line[0] <> nlttid) then Exit;
  if Length(extra_line[15]) < 10 then Exit;
  // adding
  ndex := Length(split_data);
  SetLength(split_data,ndex+1);
  split_data[ndex] := extra_line;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure NLTTcm.SetMatched();
begin  split_data[0][1] := 'T';  end;
//---------------------------
procedure NLTTcm.AppendToFile(var target:TextFile);
var ddex,dmax:Integer;
    outline:string;
begin
  dmax := Length(split_data)-1;
  for ddex := 0 to dmax do begin
    outline := split_data[ddex].DelimitedText;
    writeLn(target,outline);
  end;
end;
//------------------------------
function NLTTcm.CloserThan(pllx:Real):Boolean;
var ddex,dmax:Integer;
    tplx:Real;
begin
  Result := True;
  dmax := Length(split_data)-1;
  for ddex := 0 to dmax do begin
    if not TryStrToFloat(split_data[ddex][21],tplx) then Continue;
    if (tplx >= pllx) then Exit;
  end;
  Result := False;
end;
//========================================================================
(*   main:NLTT_List;
    infile:TFileName;
    skip_m,skip_t,count_match,count_unmatch:Integer;  *)
constructor NLTT_CrossMatch.Create;
begin
  main := NLTT_List.Create(True);
  main.Capacity := 8800;
end;
//-------------------------------------------------------------
destructor NLTT_CrossMatch.Destroy;
begin
  main.Clear;
  main.Free;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function NLTT_CrossMatch.LoadFromFile(fname:TFileName):Integer;
var cline,err_msg:string;
    temp_nltt:NLTTcm;
    nltt_source:TextFile;
    split_line:TStringList;
begin
    Result := 0;
    infile := fname;
    AssignFile(nltt_source,infile);
    temp_nltt := nil;
    try
      Reset(nltt_source);
      // the first line is column headers, not used
      readLn(nltt_source,firstline);
      while not Eof(nltt_source) do begin
        // read and pare line
        readLn(nltt_source,cline);
        split_line := SplitWithDelim(cline,';',16);
        if (split_line = nil) then Continue;
        // this option is for line 1 only
        if temp_nltt = nil then begin
          temp_nltt := NLTTcm.Create(split_line);
          if temp_nltt = nil then begin
             split_line.Free;  Continue;
          end else main.Add(temp_nltt);
        end
        // otherwise, we try to add it as a second gaia match for the previous
        else if not temp_nltt.AddGaiaLine(split_line) then begin
          (* If we fail to add it as a second line (probably due to a different
          NLTT id), we try to add it as a new NLTT object. *)
          temp_nltt := NLTTcm.Create(split_line);
          if temp_nltt = nil then begin
             split_line.Free;  Continue;
          end else main.Add(temp_nltt);
        end;
      end;
    except
      on Ferr:EInOutError do begin
        err_msg := 'File Error: ' + Ferr.Message;
        CloseFile(nltt_source);  Exit;
      end;
    end;
  // finishing
  CloseFile(nltt_source);
  Result := main.Count;
end;
//------------------------------------
function NLTT_CrossMatch.DoMatching():Boolean;
var mmax,mdex:Integer;
    dr2id,nltt_id,extra_id:string;
    matched:Boolean;
begin
  Result := False;
  if (main.Count = 0) then Exit;
  skip_m := 0; skip_t := 0;
  count_match := 0; count_unmatch := 0;
  // looping over each NLTT entry
  mmax := main.Count -1;
  for mdex := 0 to mmax do begin
    // skipping multiples (manual check)
    if main[mdex].Count > 1 then begin
      Inc(skip_m);  Continue;
    end
    // skipping already matched
    else if main[mdex].Matched then begin
      Inc(skip_t);  Continue;
    end
    // we try to match and add nltt
    else begin
      dr2id := main[mdex].FirstGaiaID;
      nltt_id := main[mdex].NLTT;
      extra_id := main[mdex].extra_id;
      matched := gaiamap.NLTTSet(dr2id,nltt_id,extra_id);
      if matched then begin
        Inc(count_match);
        main[mdex].SetMatched();
      end else Inc(count_unmatch);
    end;
  end;
  // Done
  Result := True;
end;

//------------------------------------
function NLTT_CrossMatch.WriteToFile(unmat:Boolean; minpllx:Real):Boolean;
var nltt_out:TextFile;
    err_msg:string;
    nlttdex:Integer;
    cout:NLTTcm;
begin
    Result := False;
    if main.Count = 0 then Exit;
    AssignFile(nltt_out,infile);
  try
    rewrite(nltt_out);
    // writing the saved header
    writeLn(nltt_out,firstline);
    // writing the NLTT entries in a loop
    for nlttdex := 0 to (main.Count-1) do begin
      cout := main[nlttdex];
      // skipping matches entires (if specified)
      if unmat and cout.Matched then Continue;
      // skipping entries more distant (if specified)
      if (minpllx > 0) and (not cout.CloserThan(minpllx)) then Continue;
      // writing
      cout.AppendToFile(nltt_out);
    end;
  except
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      CloseFile(nltt_out);  Exit;
    end;
  end;
  // finishing
  CloseFile(nltt_out);
  Result := True;
end;
//------------------------------------------------
// after a match, get a string count summary using this.
function NLTT_CrossMatch.MakeCountsMessage():string;
begin
  Result := IntToStr(count_match) + ' entries matched, ';
  Result += IntToStr(count_unmatch) + ' entries not matched, ';
  Result += IntToStr(skip_t) + ' entries already macthed, and ';
  Result += IntToStr(skip_m) + ' entries skipped due to multiple Gaia IDs.'
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  nltt_matcher := nil;
end.

