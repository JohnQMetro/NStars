unit gaiadr2holder;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, DAMath, FileUtil, gaiadr2base, newlocation, namedata;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

GaiaList = TFPGObjectList<GaiaDR2Star>;
GaiaDR2_StringMap = TFPGMap<string,GaiaList>;
GaiaDR2_IntMap = TFPGMap<Integer,GaiaList>;

GaiaDR2_MatchType = ( G2_NONE, G2_STARNAME, G2_SYSNAME, G2_POSITION);

DR2MatchConditions = record
  skip_matched:Boolean;  // ignore matched dr2 objects
  skip_reject:Boolean;   // ignore dr2 objects where permaReject = True
  max_search_dist:Single; // maximum dist in arcmins to look for location matches
end;

// used only for position based storage and matching in GaiaDR2Collection
GaiaStarStrip = class
  protected
    level_var:Integer;
    data:array of GaiaList;
    function AddNearby(slot:Integer; tra_in,tdecin:Double; mparams:DR2MatchConditions; var stickHere:GaiaList):Integer;
    function glevel():Integer;
  public
    property Level:Integer read glevel;
    constructor Create(in_level:Integer);
    destructor Destroy; override;
    function MaxSize():Integer;
    procedure AddStar(thestar:GaiaDR2Star; index:Integer); // does NO checking
    function GetStarsCloseTo(rap,decp:Double; mparams:DR2MatchConditions; var putHere:GaiaList):Integer;
end;

// input parameters
GaiaSourceParams = record
  infilename:TFileName;
  maxdist:Single;
  parallaxOffset:Double;
end;

// the main Gaia DR2 star storage class
GaiaDR2Collection = class
  protected
    mainlist:GaiaList;
    // name based lookup
    tycMap:GaiaDR2_StringMap;
    hipMap:GaiaDR2_IntMap;
    twomMap:GaiaDR2_StringMap;
    // position based lookup
    locstrips:array[4..715] of GaiaStarStrip;
    ncap,scap:GaiaStarStrip;
    // file i/o (formatted)
    ftarget:TFileName;
    formFile:TextFile;
    filedex,filecount:Integer;
    fmode:Integer; // -1: nothing, 0: storing, 1: load from store, 2: load from source
    // for input from source...
    amountread,amounttotal:Integer;
    sparams:GaiaSourceParams;
    // index for unmatched
    cunmatched:Integer;

    function AddToStringMap(thestar:GaiaDR2Star; target:GaiaDR2_StringMap; idstr:string):Boolean;
    function AddToHipparcos(thestar:GaiaDR2Star):Boolean;
    procedure AddToLocations(thestar:GaiaDR2Star);
    procedure AddStar(thestar:GaiaDR2Star);
    procedure ClearStringMap(target:GaiaDR2_StringMap);
    procedure ClearIntMap(target:GaiaDR2_IntMap);
    function FindNameMatches(namelist:StarName; mparams:DR2MatchConditions):GaiaList;
    function FindPositionMatches(matchThis:Location; mparams:DR2MatchConditions):GaiaList;
    procedure FileClose();
    function scount():Integer;
  public
    property StarCount:Integer read scount;
    constructor Create;
    destructor Destroy; override;
    function FindMatches(multiple:Boolean; starname,sysname:StarName; starloc:Location; mparams:DR2MatchConditions ;out mtype:GaiaDR2_MatchType):GaiaList;
    // file I/O
    function StartOutput(filename:TFileName; out err_msg:string):Boolean;
    function OutputStars(oamount:Integer; out finished:Boolean; out err_msg:string):Boolean;
    function OutputtedAmount():Integer;
    function StartInput(filename:TFileName; out err_msg:string):Boolean;
    function InputStars(iamount:Integer; out finished:Boolean; out err_msg:string):Boolean;
    function AmountToGet():Integer;
    // file input from a source csv
    function StartSourceInput(params:GaiaSourceParams; out err_msg:string):Boolean;
    function ReadSourceStars(iamount:Integer; out finished:Boolean; out err_msg:string):Boolean;
    function SourceReadAmount(total:Boolean):Int64;
    // unmatched and rejected manipulation
    function NextUnmatched(out star_dex:Integer):GaiaDR2Star;
    procedure ResetUnmatchedIndex();
    function ClearMarks(matched_mark,perma_reject_mark:Boolean):Boolean;
end;

// sorts a GaiaList by distance...
procedure SortLisByDistance(var thelist:GaiaList);
// clears a GaiaList (sets distance to zero)
procedure ClearGaiaList(var thelist:GaiaList);
procedure AddUniqueToList(var targetlist:GaiaList; sourcelist:GaiaList; mparams:DR2MatchConditions);

var
    DR2Data:GaiaDR2Collection;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//==========================================================
(* data:array of GaiaList; *)
function GaiaStarStrip.AddNearby(slot:Integer; tra_in,tdecin:Double; mparams:DR2MatchConditions; var stickHere:GaiaList):Integer;
var slotlist:GaiaList;
    starmax,stardex,qsize:Integer;
    cdist:Double;
begin
  Result := 0;
  qsize := Length(data);
  if level_var = 7 then slot := 0;
  if data[slot] = nil then Exit;
  slotlist := data[slot];
  starmax := slotlist.Count - 1;
  for stardex := 0 to starmax do begin
    if mparams.skip_matched and slotlist[stardex].matched then Continue;
    if mparams.skip_reject and slotlist[stardex].permaReject then Continue;
    cdist := slotlist[stardex].astrometry.GetDistanceTo(tra_in,tdecin);
    if (cdist <= mparams.max_search_dist) then begin
      slotlist[stardex].distance := cdist;
      stickHere.Add(slotlist[stardex]);
      Inc(Result);
    end;
  end;
end;

//-------------------------------------------------------------
function GaiaStarStrip.glevel():Integer;
begin  Result := level_var;  end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor GaiaStarStrip.Create(in_level:Integer);
var nlen,ndex:Integer;
begin
  if (in_level <= 0) or (in_level > 7) then Fail;
  if (in_level = 7) then begin
    SetLength(data,1);
    data[0] := nil;
  end else begin
    nlen := Trunc (1440.0 / intpower(2,in_level-1));
    SetLength(data,nlen);
    for ndex := 0 to (nlen-1) do data[ndex] := nil;
  end;
  level_var := in_level;
end;
//-----------------------------------------
destructor GaiaStarStrip.Destroy;
var ndex,nmax:Integer;
begin
  nmax := Length(data)-1;
  for ndex := 0 to nmax do data[ndex].Free;
  SetLength(data,0);
  inherited;
end;
//------------------------------------------
function GaiaStarStrip.MaxSize():Integer;
begin  Result := Length(data);  end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure GaiaStarStrip.AddStar(thestar:GaiaDR2Star; index:Integer);
var ra,dec:Double;
begin
  if (level_var = 7) then begin
     if data[0] = nil then data[0] := GaiaList.Create(False);
     data[0].Add(thestar);
  end else begin
    if index > High(data) then begin
       ra := thestar.astrometry.rapos;
       dec := thestar.astrometry.decpos;
       Assert(False);
    end;
    if data[index] = nil then data[index] := GaiaList.Create(False);
    data[index].Add(thestar);
  end;
end;
//-----------------------------------------
function GaiaStarStrip.GetStarsCloseTo(rap,decp:Double; mparams:DR2MatchConditions; var putHere:GaiaList):Integer;
var raindex,decdex,calc_level:Integer;
    tra,tdec,offset:Real;
    cosdec,decleft:Double;
    nextpos:Integer;
begin
  // asserts
  Assert((mparams.max_search_dist >= 0) and (mparams.max_search_dist < 3.75),'Maximum separation not in range!');
  Assert(putHere <> nil,'Target List is nil!');
  // we get the indexes and positions
  tra := rap;
  tdec := decp;
  calc_level := MakePositionIndexes(tra,tdec,decdex,raindex);
  cosdec := 60*cosd(decp);
  // checking to see if we look in more than one slot...
  // horizontal...
  offset := (tra - (raindex * 360.0/Length(data)))*cosdec;
  if (offset < mparams.max_search_dist) then begin
    if (raindex = 0) then nextpos := High(data)
    else nextpos := raindex - 1;
  end else begin
    offset := (((raindex+1) * 360.0/Length(data))-tra)*cosdec;
    if (offset < mparams.max_search_dist) then begin
      if (raindex = High(data)) then nextpos := 0
      else nextpos := raindex + 1;
    end else nextpos := -1;
  end;
  // vertical...
  decleft := 15*Frac(tdec + 90)*4;
  if (decleft < mparams.max_search_dist) then Result := -1
  else if (decleft + mparams.max_search_dist) > 15 then Result := 1
  else Result := 0;
  (* The task now is to go through the stars in the targeted slots and
  calculate the separations, keeping the ones below the limit *)
  AddNearby(raindex,tra,tdec,mparams,putHere);
  if nextpos >= 0 then AddNearby(nextpos,tra,tdec,mparams,putHere);

end;
//==========================================================
(* adds a star to a string map, returns true if there is an id *)
function GaiaDR2Collection.AddToStringMap(thestar:GaiaDR2Star; target:GaiaDR2_StringMap; idstr:string):Boolean;
var holdlist:GaiaList;
begin
  Assert(thestar <> nil,'thestar must not be nil.');
  Assert(target<>nil,'Target Stringmap must not be nil.');
  // moving on...
  Result := False;
  if (idstr = '') then Exit;
  // we look for a starlist associated with the string
  if not target.TryGetData(idstr,holdlist) then begin
    // if there is not, we create one and stick it in the map
    holdlist := GaiaList.Create(False);
    target.AddOrSetData(idstr,holdlist);
  end;
  // adding the star to the list
  holdlist.Add(thestar);
  Result := True;
end;
//-----------------------------------
(* adds a star to the Hipparcos map, returns true if there is a Hipparcos ID. *)
function GaiaDR2Collection.AddToHipparcos(thestar:GaiaDR2Star):Boolean;
var holdlist:GaiaList;
    hipid:Integer;
begin
  Assert(thestar <> nil,'thestar must not be nil.');
  Assert(thestar.ids <> nil,'thestar.ids must not be nil.');
  // moving on...
  Result := False;
  // we look for a starlist associated with the Hipparcos id
  hipid := thestar.ids.Hip;
  if (hipid <= 0) then Exit;
  if not hipMap.TryGetData(hipid,holdlist) then begin
    Result := False;
    // if there is not, we create one and stick it in the map
    holdlist := GaiaList.Create(False);
    hipMap.AddOrSetData(hipid,holdlist);
  end;
  // adding the star to the list
  holdlist.Add(thestar);
  Result := True;
end;
//-----------------------------------
procedure GaiaDR2Collection.AddToLocations(thestar:GaiaDR2Star);
var decdex,radex:Integer;
begin
  Assert(thestar <> nil,'thestar must not be nil.');
  Assert(thestar.isValid,'thestar must be valid');
  thestar.astrometry.GetPositionIndexes(decdex,radex);
  if decdex < 4 then scap.AddStar(thestar,radex)
  else if decdex >= 716 then ncap.AddStar(thestar,radex)
  else locstrips[decdex].AddStar(thestar,radex);
end;
//-----------------------------------
procedure GaiaDR2Collection.AddStar(thestar:GaiaDR2Star);
begin
  Assert(thestar <> nil,'thestar must not be nil.');
  Assert(thestar.isValid,'thestar must be valid');
  mainlist.Add(thestar);
  AddToStringMap(thestar,tycMap,thestar.ids.Tycho2);
  AddToHipparcos(thestar);
  AddToStringMap(thestar,twomMap,thestar.ids.TwoMASS);
  AddToLocations(thestar);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure GaiaDR2Collection.ClearStringMap(target:GaiaDR2_StringMap);
var index,idxMax:Integer;
    ckey:string;
    clist:GaiaList;
begin
  if target = nil then Exit;
  if target.Count = 0 then Exit;
  idxMax := target.Count - 1;
  // looping...
  for index := 0 to idxMax do begin
    ckey := target.Keys[index];
    target.TryGetData(ckey,clist);
    target.AddOrSetData(ckey,nil);
    clist.Free;
  end;
  // all vales have been set to nil, we collape the list
  target.Clear;
end;
//-----------------------------------
procedure GaiaDR2Collection.ClearIntMap(target:GaiaDR2_IntMap);
var index,idxMax:Integer;
    ckey:Integer;
    clist:GaiaList;
begin
  if target = nil then Exit;
  if target.Count = 0 then Exit;
  idxMax := target.Count - 1;
  // looping...
  for index := 0 to idxMax do begin
    ckey := target.Keys[index];
    target.TryGetData(ckey,clist);
    target.AddOrSetData(ckey,nil);
    clist.Free;
  end;
  // all vales have been set to nil, we collape the list
  target.Clear;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Collection.FindNameMatches(namelist:StarName; mparams:DR2MatchConditions):GaiaList;
var nvalue:string;
    namepos,hipidx:Integer;
    gltyc,glhip,gl2mass:GaiaList;
begin
  Result := nil;
  if namelist = nil then Exit;
  Result := GaiaList.Create(False);
  // looking for Tycho-2 catalog matches
  if namelist.GetCatValue('Tyc',nvalue) then begin
    namepos := tycMap.IndexOf(nvalue);
    if namepos >= 0 then begin
      gltyc := tycMap.Data[namepos];
      AddUniqueToList(Result,gltyc,mparams);
      Result.Assign(gltyc);
    end;
  end;
  // looking for 2MASS catalog matches
  if namelist.GetCatValue('2MASS',nvalue) then begin
    namepos := TwoMMap.IndexOf(nvalue);
    if namepos >= 0 then begin
      gl2mass := TwoMMap.Data[namepos];
      AddUniqueToList(Result,gl2mass,mparams);
    end;
  end;
  // looking for Hipparcos catalog matches
  if namelist.GetCatValue('Hip',nvalue) then begin
    if TryStrToInt(nvalue,hipidx) then begin
      namepos := HipMap.IndexOf(hipidx);
      if namepos >= 0 then begin
        glhip := HipMap.Data[namepos];
        AddUniqueToList(Result,glhip,mparams);
      end;
    end;
  end;
  // final checks
  if Result.Count = 0 then FreeAndNil(Result);
end;
//---------------------------------------------------------------------
function GaiaDR2Collection.FindPositionMatches(matchThis:Location; mparams:DR2MatchConditions):GaiaList;
var ra15,dec15:Double;
    raindex,decindex, looksee,looklevel:Integer;
begin
  Assert((mparams.max_search_dist < 3.75) and (mparams.max_search_dist > 0),'The maxdist is out of bounds');
  Assert(matchThis <> nil,'The provided location is nil');
  Result := GaiaList.Create(False);
  // getting where to look
  matchThis.MakeGetJ2015p5Pos(ra15,dec15);
  looklevel := MakePositionIndexes(ra15,dec15,decindex,raindex);
  // special pole checks
  if looklevel = 7 then begin
    if decindex < 4 then begin
      looksee := scap.GetStarsCloseTo(ra15,dec15,mparams,Result);
      if (looksee > 0) then locstrips[4].GetStarsCloseTo(ra15,dec15,mparams,Result);
    end else begin
      looksee := ncap.GetStarsCloseTo(ra15,dec15,mparams,Result);
      if (looksee < 0) then locstrips[715].GetStarsCloseTo(ra15,dec15,mparams,Result);
    end;
  end else begin
    looksee := locstrips[decindex].GetStarsCloseTo(ra15,dec15,mparams,Result);
    if looksee < 0 then begin
       if decindex = 4 then scap.GetStarsCloseTo(ra15,dec15,mparams,Result)
       else locstrips[decindex-1].GetStarsCloseTo(ra15,dec15,mparams,Result);
    end else if looksee > 0 then begin
       if decindex = 715 then ncap.GetStarsCloseTo(ra15,dec15,mparams,Result)
       else locstrips[decindex+1].GetStarsCloseTo(ra15,dec15,mparams,Result);
    end;
  end;
  // at the end, check how many stars we have found
  if Result.Count = 0 then FreeAndNil(Result)
  else if Result.Count > 1 then SortLisByDistance(Result);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure GaiaDR2Collection.FileClose();
begin
  if fmode <> -1 then begin
    CloseFile(formFile);
    fmode := -1;
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Collection.scount():Integer;
begin
  if mainlist = nil then Result := 0
  else Result := mainlist.Count;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor GaiaDR2Collection.Create;
var ssdex:Integer;
begin
  mainlist := GaiaList.Create(True);
  tycMap := GaiaDR2_StringMap.Create;
  hipMap := GaiaDR2_IntMap.Create;
  twomMap := GaiaDR2_StringMap.Create;
  // position based holders
  ncap := GaiaStarStrip.Create(7);
  scap := GaiaStarStrip.Create(7);
  for ssdex := 4 to 715 do begin
    locstrips[ssdex] := GaiaStarStrip.Create(GetLevelForIndex(ssdex));
  end;
  fmode := -1; // actually means 'no i/o going on'
  cunmatched := -1;
end;
//-----------------------------------------
destructor GaiaDR2Collection.Destroy;
var ssdex:Integer;
begin
  ClearStringMap(tycMap);
  tycMap.Free;
  ClearStringMap(twomMap);
  twomMap.Free;
  ClearIntMap(hipMap);
  hipMap.Free;
  ncap.Free;
  scap.Free;
  for ssdex := 4 to 714 do locstrips[ssdex].Free;
  mainlist.Free;
  inherited;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaDR2Collection.FindMatches(multiple:Boolean; starname,sysname:StarName; starloc:Location; mparams:DR2MatchConditions; out mtype:GaiaDR2_MatchType):GaiaList;
var resbak:GaiaList;
begin
  mtype := G2_NONE;
  Result := nil;
  resbak := nil;
  // looking for star names (best accuracy)
  if (starname <> nil) then begin
    Result := FindNameMatches(starname,mparams);
    if Result <> nil then begin
      mtype := G2_STARNAME;
      Exit;
    end;
  end;
  // fallback system names (probably not the best, unless there is one star only)
  if (sysname <> nil) then begin
    Result := FindNameMatches(sysname,mparams);
    if Result <> nil then begin
      mtype := G2_SYSNAME;
      if (not multiple) then Exit;
      resbak := Result;
    end;
  end;
  // position based (requires the most work)
  if (starloc <> nil) then begin
    Result := FindPositionMatches(starloc,mparams);
    if Result <> nil then begin
      if (resbak <> nil) then begin
        AddUniqueToList(resbak,Result,mparams);
        Result.Free;
        Result := resbak;
      end
      else mtype := G2_POSITION;
    end;
  end;
  if (Result = nil) then mtype := G2_NONE;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// file I/O
function GaiaDR2Collection.StartOutput(filename:TFileName; out err_msg:string):Boolean;
begin
  Result := False;
  if fmode >= 0 then begin
    err_msg := 'File I/O already ongoing!';   Exit;
  end;
  AssignFile(formFile,filename);
  try
    rewrite(formFile);
    writeLn(formFile,mainlist.Count);
    writeLn(formFile,StarHeader());
  except
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      FileClose();  Exit;
    end;
  end;
  // if we get here, things are okay
  ftarget := filename;
  filecount := mainlist.Count;
  filedex := 0;
  fmode := 0;
  // finishing
  Result := True;
end;
//----------------------------------------
function GaiaDR2Collection.OutputStars(oamount:Integer; out finished:Boolean; out err_msg:string):Boolean;
var oindex:Integer;
    sdata:string;
begin
  Result := False;
  // basic errors
  if fmode <> 0 then begin
    err_msg := 'Not in output mode!';  Exit;
  end;
  if (oamount < 1) then begin
    err_msg := 'Output amount is less than 1!';  Exit;
  end;
  // some initial checks
  finished := False;
  try
    // writing max oamount stars in a loop
    for oindex := 0 to (oamount-1) do begin
      if (filedex + oindex) >= filecount then Break;
      sdata := mainlist[filedex+oindex].ToSemiString();
      writeLn(formFile,sdata);
    end;
  except
    // error handling
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      FileClose();  Exit;
    end;
  end;
  // after loop checking...
  finished := ((filedex + oindex) >= (filecount - 1));
  if finished then FileClose();
  if (finished) then filedex := filecount
  else filedex += (oindex + 1);
  Result := True;
end;
//----------------------------------------
function GaiaDR2Collection.OutputtedAmount():Integer;
begin   Result := filedex;  end;
//----------------------------------------
function GaiaDR2Collection.StartInput(filename:TFileName; out err_msg:string):Boolean;
var readstr:string;
    lcount:Integer;
begin
  Result := False;
  if fmode <> -1 then begin
    err_msg := 'File I/O already ongoing!';   Exit;
  end;
  AssignFile(formFile,filename);
  try
    Reset(formFile);
    readLn(formFile,readstr);
    if (not TryStrToInt(readstr,lcount)) then begin
      err_msg := 'First line is not a number!';
      FileClose();  Exit;
    end;
    readLn(formFile,readstr); // header line, for spreadsheets. not used in the program...
  except
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      FileClose();  Exit;
    end;
  end;
  // checking the count...
  if (lcount < 0) then begin
    err_msg := 'First line is negative!';
    FileClose();  Exit;
  end;
  // here, things are okay, we set the variables and exit
  ftarget := filename;
  filecount := lcount;
  filedex := 0;
  fmode := 1;
  // finishing
  Result := True;
end;
//----------------------------------------
function GaiaDR2Collection.InputStars(iamount:Integer; out finished:Boolean; out err_msg:string):Boolean;
var iindex:Integer;
    sdata:string;
    tempStar:GaiaDR2Star;
begin
  Result := False;
  // basic errors
  if fmode <> 1 then begin
    err_msg := 'Not in input mode!';  Exit;
  end;
  if (iamount < 1) then begin
    err_msg := 'Input amount is less than 1!';  Exit;
  end;
  // some initial checks
  finished := False;
  try
    // reading max iamount stars in a loop
    for iindex := 0 to (iamount-1) do begin
      if (filedex + iindex) >= filecount then Break;
      readLn(formFile,sdata);
      tempStar := GaiaDR2Star.Create;
      // try to convert the line to a gaia star
      if (not tempStar.FromSemiString(sdata)) then begin
        err_msg := 'Line :' + sdata + sLineBreak + 'Could not be parsed properly.';
        filedex += iindex;
        FileClose();  Exit;
      end;
      // here, things are okay, so add the star
      AddStar(tempStar);
    end;
  except
    // error handling
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      FileClose();  Exit;
    end;
  end;
  // after loop checking...
  finished := ((filedex +iindex) >= (filecount - 1));
  if finished then FileClose();
  if (finished) then filedex := filecount
  else filedex += (iindex + 1);
  Result := True;
end;
//----------------------------------------
function GaiaDR2Collection.AmountToGet():Integer;
begin   Result := filecount;   end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* ftarget:TFileName;
formFile:TextFile;
filedex,filecount:Integer;
fmode:Integer;
// for input from source...
amountread,amounttotal:Integer;   *)
//---------------------------------------------
// file input from a source csv
//---------------------------------------------
function GaiaDR2Collection.StartSourceInput(params:GaiaSourceParams; out err_msg:string):Boolean;
var readstr:string;
    sizeraw:Double;
    capest:Integer;
begin
  Result := False;
  if fmode <> -1 then begin
    err_msg := 'File I/O already ongoing!';   Exit;
  end;
  sparams := params;
  AssignFile(formFile,sparams.infilename);
  try
    Reset(formFile);
    // the first line is column headers, not used
    readLn(formFile,readstr);
  except
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      FileClose();  Exit;
    end;
  end;
  // here, things are okay, we set the variables and exit
  (* Disgustingly, the FileSize and FilePos functions do not work for TextFile
  and NOBODY CARES (except me). *)
  amounttotal := FileUtil.FileSize(sparams.infilename);
  amountread := Length(readstr)+1; // the only works properly if the line ending is 1 char
  // setting up capacity
  sizeraw := (amounttotal - 661) / 474.0;
  capest := Trunc(sizeraw);
  mainlist.Capacity := capest;
  ftarget := sparams.infilename;
  filecount := -1; // this not known in advance
  filedex := 0;
  fmode := 2;
  // finishing
  Result := True;
end;
//---------------------------------------------
function GaiaDR2Collection.ReadSourceStars(iamount:Integer; out finished:Boolean; out err_msg:string):Boolean;
var iindex:Integer;
    sdata:string;
    tempStar:GaiaDR2Star;
begin
  Result := False;
  // basic errors
  if fmode <> 2 then begin
    err_msg := 'Not in source mode!';  Exit;
  end;
  if (iamount < 1) then begin
    err_msg := 'Input amount is less than 1!';  Exit;
  end;
  // some initial checks
  finished := False;
  try
    // reading max iamount lines in a loop
    for iindex := 0 to (iamount-1) do begin
      if EoF(formFile) then Break;
      readLn(formFile,sdata);
      amountread += Length(sdata)+1;
      tempStar := GaiaDR2Star.Create;
      // try to convert the line to a gaia star
      if (not tempStar.SetFromSource(sdata,sparams.maxdist)) then begin
        filedex += iindex;
        err_msg := 'Failed to convert line to Star/Brown Dwarf';
        FileClose();  Exit;
      end;
      // here, things are okay, so add the star
      tempStar.astrometry.parallax += sparams.parallaxOffset;
      AddStar(tempStar);
    end;
    filedex += iindex;
  except
    // error handling
    on Ferr:EInOutError do begin
      err_msg := 'File Error: ' + Ferr.Message;
      FileClose();  Exit;
    end;
  end;
  // after loop checking...
  finished := Eof(formFile);
  if finished then FileClose();
  Result := True;
end;
//---------------------------------------------
function GaiaDR2Collection.SourceReadAmount(total:Boolean):Int64;
begin
  if total then Result := amounttotal
  else Result := amountread;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// unmatched and rejected manipulation
//-------------------------------------
function GaiaDR2Collection.NextUnmatched(out star_dex:Integer):GaiaDR2Star;
begin
  // what we do if we are already past the end
  Result := nil;
  star_dex := cunmatched;
  if (cunmatched >= StarCount) then Exit;;
  // moving to the next unmatched star (unless we reach the end first)
  repeat
    Inc(cunmatched);
    if (cunmatched >= StarCount) then Break;
  until (not mainlist[cunmatched].matched);
  // after, we check if we are at the end..
  star_dex := cunmatched;
  if (cunmatched >= StarCount) then Exit
  else Result := mainlist[cunmatched];
end;
//-------------------------------------
procedure GaiaDR2Collection.ResetUnmatchedIndex();
begin  cunmatched := -1;   end;
//-------------------------------------
function GaiaDR2Collection.ClearMarks(matched_mark,perma_reject_mark:Boolean):Boolean;
var cstar:GaiaDR2Star;
begin
  Result := False;
  if not (matched_mark or perma_reject_mark) then Exit;
  // looping
  for cstar in mainlist do begin
    if matched_mark then cstar.matched := False;
    if perma_reject_mark then cstar.permaReject := False;
  end;
  Result := True;
end;
//============================================================================
// sorts a GaiaList by distance...
procedure SortLisByDistance(var thelist:GaiaList);
var lcount,iindex,oindex,sindex:Integer;
    mindist:Single;
    tempstar:GaiaDR2Star;
begin
  if (thelist = nil) then Exit;
  lcount := thelist.Count;
  if (lcount < 2) then Exit;
  // doing a proper selection sort
  for oindex := 0 to (lcount-2) do begin
    sindex := oindex;
    mindist := thelist[oindex].distance;
    for iindex := (oindex+1) to (lcount-1) do begin
      if thelist[iindex].distance < mindist then begin
        sindex := iindex;
        mindist := thelist[iindex].distance;
      end;
    end;
    // swapping if need be
    if sindex <> oindex then begin
      tempstar := thelist[oindex];
      thelist[oindex] := thelist[sindex];
      thelist[sindex] := tempstar;
    end;
  end;
  // done
end;
//-------------------------------------------
// clears a GaiaList (sets distance to zero)
procedure ClearGaiaList(var thelist:GaiaList);
var iindex,imax:Integer;
begin
  if thelist = nil then Exit;
  if thelist.Count = 0 then Exit;
  imax := thelist.Count - 1;
  for iindex := 0 to imax do thelist[iindex].distance := 0.0;
  thelist.Clear;
end;
//-----------------------------------------------
(* adds the items in the second list to the first if they are not already in
the first list. *)
procedure AddUniqueToList(var targetlist:GaiaList; sourcelist:GaiaList; mparams:DR2MatchConditions);
var sindex,tindex,smax:Integer;
    foundx:Boolean;
begin
  if sourcelist = nil then Exit;
  if sourcelist.Count = 0 then Exit;
  if targetlist = nil then Exit;
  // looping over the items in the second list
  smax := sourcelist.Count - 1;
  for sindex := 0 to smax do begin
    foundx := false;
    if mparams.skip_reject and sourcelist[sindex].permaReject then Continue;
    if mparams.skip_matched and sourcelist[sindex].matched then Continue;
    for tindex := 0 to (targetlist.Count -1 ) do begin
      foundx := (targetlist[tindex] = sourcelist[sindex]);
      if foundx then Break;
    end;
    if not foundx then targetlist.Add(sourcelist[sindex]);
  end;
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  DR2Data := nil;
end.

