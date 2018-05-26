unit gaiadr2types;

{$mode delphi}

interface

uses
  Classes, SysUtils, fgl, contnrs, gaiadr2base;

type

GaiaList = TFPGObjectList<GaiaDR2Star>;

GaiaStringMap = class(TFPHashObjectList)
  private
    data:TFPHashObjectList;
    function gcount():Integer;
  public
    property count:Integer read gcount;
    constructor Create();
    destructor Destroy(); override;
    function Add(const key:string; starlist:GaiaList):Boolean;
    function AddStarObj(const key:string; starobj:GaiaDR2Star):Boolean;
    function Contains(const key:string):Boolean;
    function Get(const key:string):GaiaList;
    function GetFirst(const key:string):GaiaDR2Star;
    procedure Clear();
end;

GaiaDR2_IntMap = TFPGMap<Integer,GaiaList>;

GaiaDR2_MatchType = ( G2_NONE, G2_STARNAME, G2_SYSNAME, G2_POSITION);

DR2MatchConditions = record
  skip_matched:Boolean;  // ignore matched dr2 objects
  skip_reject:Boolean;   // ignore dr2 objects where permaReject = True
  max_search_dist:Single; // maximum dist in arcmins to look for location matches
end;

// input parameters
GaiaSourceParams = record
  infilename:TFileName;
  maxdist:Single;
  parallaxOffset:Double;
end;

// list manipulation routines...
// sorts a GaiaList by distance...
procedure SortLisByDistance(var thelist:GaiaList);
// clears a GaiaList (sets distance to zero)
procedure ClearGaiaList(var thelist:GaiaList);
// takes unique items (if they pass the match parameters) from the second to first list
procedure AddUniqueToList(var targetlist:GaiaList; sourcelist:GaiaList; mparams:DR2MatchConditions);


//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//=============================================================
function GaiaStringMap.gcount():Integer;
begin  Result := data.Count;  end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor GaiaStringMap.Create();
begin
  data := TFPHashObjectList.Create(True);
  data.Capacity := 10000;
end;
//------------------------------------
destructor GaiaStringMap.Destroy();
begin
  data.Clear();
  data.Free;
  inherited;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function GaiaStringMap.Add(const key:string; starlist:GaiaList):Boolean;
var nkey:shortstring;
begin
  Result := False;
  if (starlist = nil) then Exit;
  if Length(key) > 255 then Exit;
  if Length(key) = 0 then Exit;
  // going ahead
  nkey := key;
  data.Add(nkey,starlist);
  Result := True;
end;
//----------------------------------
function GaiaStringMap.AddStarObj(const key:string; starobj:GaiaDR2Star):Boolean;
var tlist:GaiaList;
begin
  Result := False;
  if (starobj = nil) then Exit;
  if Length(key) > 255 then Exit;
  if Length(key) = 0 then Exit;
  // going ahead
  if data.FindIndexOf(key) < 0 then begin
    tlist := GaiaList.Create(False);
    data.Add(key,tlist);
  end else tlist := (data.Find(key) as GaiaList);
  tlist.Add(starobj);
  Result := True;
end;
//----------------------------------
function GaiaStringMap.Contains(const key:string):Boolean;
begin
  Result := False;
  if Length(key) > 255 then Exit;
  if Length(key) = 0 then Exit;
  Result := (data.FindIndexOf(key) >= 0);
end;
//------------------------------------
function GaiaStringMap.Get(const key:string):GaiaList;
begin
  Result := nil;
  if Length(key) > 255 then Exit;
  if Length(key) = 0 then Exit;
  Result := (data.Find(key) as GaiaList);
end;
//------------------------------------
function GaiaStringMap.GetFirst(const key:string):GaiaDR2Star;
var glist:GaiaList;
begin
  Result := nil;
  glist := Get(key);
  if (glist = nil) or (glist.Count = 0) then Exit;
  Result := glist[0];
end;
//------------------------------------
procedure GaiaStringMap.Clear();
begin  data.Clear;   end;
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

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.

