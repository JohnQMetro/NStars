unit dr2matchthread;

{$mode delphi}

interface

(* Thread and data types for matching multiple stars with Gaia DR2 data. Basic
matching is included with GaiaDR2Collection. *)

uses
  Classes, SysUtils, LCLIntf, LMessages,
  gaiadr2holder, stardata, gaiadr2base, collecdata, newlocation, namedata,
  NewStar;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

// parameters for matching routine...
DR2MatchParams = record
  skip_dr2:Boolean; // skip stars that have a DR2 parallax already
  skip_matched:Boolean;  // ignore matched dr2 objects
  skip_reject:Boolean;   // ignore dr2 objects where permaReject = True
  auto_match_good:Boolean; // good means one star system, name match, meets selection A,B
  auto_match_v:Boolean; // means estimated V and given V should be within 0.3 for auto match
  match_conditional:Boolean; // use the parallax based on the parallax error
  max_search_dist:Single; // maximum dist in arcmins to look for location matches
end;

(* When we want to confirm a match, we need to many bits of data
to the GUI, so here is a class for that. *)
DR2MatchData = class
  public
    system:StarSystem;  // system we are checking
    stardex:Integer; // 1..star count : star index we checking for the match
    mtype:GaiaDR2_MatchType; // match type
    matches:GaiaList; // list of potential dr2 matches
    // methods
    constructor Create;
    destructor Destroy; override;
    function ApplyMatch(using:Integer; conditional:Boolean):Boolean;
    function ApplySome(using:Integer):Boolean;
    function RejectObject(thisOne:Integer):Integer;
end;
//---------------------------------------------------------------------

// thread that handles bulk matching, possibly semi-automated
MatchToDR2Thread = class(TThread)
  protected
    params:DR2MatchParams;   // rules that govern matching
    qparams:DR2MatchConditions; // a subset of the above
    startdex,sysdex,stardex:Integer;  // first system index, current system index, index of star in system
    currentSystem:StarSystem;   // currently checked system

    msgTarget:THandle; // window handle for message target
    waitlock:PRTLEvent;  // used to pause and resume thread
    doquit:Boolean; // set to true to terminate early

    procedure PostCannotStart(why:string); // just in case things go badly wrong
    procedure PostCheckMatch(mtypein:GaiaDR2_MatchType;stars:GaiaList); // send stuff to confirm match
    procedure MatchForSystem();  // does a gaia match for the stars in the current system
    procedure Execute; override;  // top level execution
  public
    constructor Create(createSuspended:Boolean; mparams:DR2MatchParams; startat:Integer; mtarget:THandle);
    procedure WaitIsOver(quit:Boolean);  // the form should call this to resume
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
const
// message constants
  MSG_IOSTART = LM_USER + 3001;
  MSG_CANNOTSTART = LM_USER + 3010;
  MSG_STARMATCHED = LM_USER + 3011;
  MSG_CHECKMATCH = LM_USER + 3012;
  MSG_DONE = LM_USER + 3006;

//---------------------------------------------------------------------
procedure InitDR2MatchParam(var sparam:DR2MatchParams; test:Boolean);
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//==========================================================================
constructor DR2MatchData.Create;
begin
  system := nil;
  stardex := 1;
  mtype := G2_NONE;
  matches := nil;
end;
//------------------------------------
destructor DR2MatchData.Destroy;
begin
  if matches <> nil then begin
    ClearGaiaList(matches);
    matches.Free;
  end;
  system := nil;
  inherited;
end;
//------------------------------------
function DR2MatchData.ApplyMatch(using:Integer; conditional:Boolean):Boolean;
var starm:GaiaDR2Star;
begin
  Result := False;
  // bad cases which should not happen
  if (matches = nil) or (system = nil) then Exit;
  if (using < 0) or (using >= matches.Count) then Exit;
  // applying the match
  starm := matches[using];
  system.ApplyGaiaObject(stardex,starm,conditional);
  starm.matched := True;
  starm.permaReject := False;
  // done
  Result := True;
end;
//-----------------------------------------
function DR2MatchData.ApplySome(using:Integer):Boolean;
var starm:GaiaDR2Star;
begin
  Result := False;
  // bad cases which should not happen
  if (matches = nil) or (system = nil) then Exit;
  if (using < 0) or (using >= matches.Count) then Exit;
  // applying the match
  starm := matches[using];
  system.ApplyGaiaNameMags(stardex,starm);
  starm.permaReject:= True;
  // done
  Result := True;
end;
//-----------------------------------------
function DR2MatchData.RejectObject(thisOne:Integer):Integer;
begin
  Result := -1;
  // bad cases which should not happen
  if (matches = nil)then Exit;
  if (thisOne < 0) or (thisOne >= matches.Count) then Exit;
  // item marking and clearing it from the list
  matches[thisOne].permaReject := True;
  matches[thisOne].distance := 0.0;
  matches.Delete(thisOne);
  // done
  Result := matches.Count;
end;

//==========================================================================
 // just in case things go badly wrong
procedure MatchToDR2Thread.PostCannotStart(why:string);
var pcopy:PString;
begin
  New(pcopy);
  pcopy^ := why;
  PostMessage(msgTarget,MSG_CANNOTSTART,Int64(pcopy),0);
end;
//-----------------------------------------------
// send stuff to confirm match
procedure MatchToDR2Thread.PostCheckMatch(mtypein:GaiaDR2_MatchType; stars:GaiaList);
var tosend:DR2MatchData;
begin
  tosend := DR2MatchData.Create;
  tosend.matches := stars;
  tosend.mtype := mtypein;
  tosend.stardex := stardex;
  tosend.system := currentSystem;
  PostMessage(msgTarget,MSG_CHECKMATCH,Int64(tosend),0);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// does a gaia match for the stars in the current system
procedure MatchToDR2Thread.MatchForSystem();
var xstardex,starcount:Integer;
    starloc,oldloc:Location;
    stnames,sysnames:StarName;
    matchlist:GaiaList;
    mtype:GaiaDR2_MatchType;
    hasmatch,doautomatch:Boolean;
    Vest:Real;
    cstar:StarInfo;
begin
  // initial system values
  currentSystem := primaryl.SystemAtIndex(sysdex);
  hasmatch := False;
  xstardex := 1;
  oldloc := nil;
  // trying to match each component individually with DR2
  while xstardex <= currentSystem.GetCompC do begin;
    stardex := xstardex;
    starcount := currentSystem.GetCompC;
    // performing the match, we might skip if the location already is DR2
    currentSystem.GetStuffForMatching(xstardex,starloc,stnames,sysnames);
    if params.skip_dr2 and (oldloc <> starloc) then begin
      if (starloc.HasGaiaDR2()) and (not starloc.IsACopy) then begin
        Inc(xstardex);
        Continue;
      end;
    end;
    matchlist := DR2Data.FindMatches(starcount > 1,stnames,sysnames,starloc,qparams,mtype);
    // checking the results
    if (mtype = G2_NONE) then begin
      Inc(xstardex);
      Continue;
    end;
    hasmatch := True;
    // are conditions good for an auto match up?
    doautomatch := (params.auto_match_good and (starcount = 1));
    if doautomatch then begin
       doautomatch := (mtype = G2_SYSNAME) and (matchlist.Count = 1);
       if doautomatch then doautomatch := (not matchlist[0].selectionAfail) and (not matchlist[0].selectionBfail);
       if doautomatch then doautomatch := (matchlist[0].astrometry.parallax_err < 0.12);
       // for tighter auto fits, requre v mag close enough
       if doautomatch and params.auto_match_v then begin
          doautomatch := not currentSystem.IsBrownDwarf(xstardex);   // Brown Dwarfs have no V
          if doautomatch then doautomatch := matchlist[0].mags.MakeVest(Vest);
          if doautomatch then begin
            cstar := (currentSystem.GetNewStar(xstardex)) as StarInfo;
            doautomatch := Abs(cstar.VisualMagnitude - Vest) < 0.3;
          end;
       end;
    end;
    // that was ugly, but now we know...
    if doautomatch then begin
      currentSystem.ApplyGaiaObject(stardex,matchlist[0],params.match_conditional);
      matchlist[0].matched := True;
      matchlist[0].permaReject:=False;
      ClearGaiaList(matchlist);
      FreeAndNil(matchlist);
      PostMessage(msgTarget,MSG_STARMATCHED,0,0);
    end else begin
      // we now send our match results to the UI for matching, and wait
      PostCheckMatch(mtype,matchlist);   // the matching will be decided amd handled in the UI
      RTLEventWaitFor(waitlock);   // wait, unitl wait is over is called...
      RTLeventResetEvent(waitlock); // once resumed, we clear the wait
    end;
    if doquit then Break; // UI has signalled quitting
    Inc(xstardex);
  end;
  // the system has been fully handled...
  if hasmatch then currentSystem.UpdateEstimates; // not always needed, but often is.
end;
//------------------------------------------------
// top level execution
procedure MatchToDR2Thread.Execute;
var loopdex:Integer;
begin
  // we first do some initial checks
  if primaryl = nil then PostCannotStart('No Stars to match against!')
  else if DR2Data = nil then PostCannotStart('No Gaia DR2 Data!')
  else if DR2Data.StarCount = 0 then PostCannotStart('Gaia DR2 Data is empty!')
  else if (startdex >= primaryl.GetCount) then PostCannotStart('The index is higher than the stars!')
  else begin
    // here, we may go ahead!
    PostMessage(msgTarget,MSG_IOSTART,0,0);
    for loopdex := startdex to (primaryl.GetCount-1) do begin
      sysdex := loopdex;
      MatchForSystem();
      if doquit then Break;
    end;
    PostMessage(msgTarget,MSG_DONE,0,0);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor MatchToDR2Thread.Create(createSuspended:Boolean; mparams:DR2MatchParams; startat:Integer; mtarget:THandle);
begin
  Assert(startat > 0); // we never check the sun
  startdex := startat;
  // parameters
  params := mparams;
  qparams.max_search_dist:= params.max_search_dist;
  qparams.skip_matched := params.skip_matched;
  qparams.skip_reject := params.skip_reject;
  // additional
  msgTarget := mtarget;
  waitlock := RTLEventCreate;
  currentSystem := nil;
  doquit := False;
  // done
  inherited Create(createSuspended);
end;
//------------------------------------------------
// the form should call this to resume
procedure MatchToDR2Thread.WaitIsOver(quit:Boolean);
begin
  doquit := quit;
  RTLeventSetEvent(waitlock);
end;
//==========================================================================
procedure InitDR2MatchParam(var sparam:DR2MatchParams; test:Boolean);
begin
  if test then begin
    sparam.skip_dr2 := False;
    sparam.skip_matched := False;
    sparam.skip_reject := False;
    sparam.auto_match_good := False;
    sparam.auto_match_v := True;
  end else begin
    sparam.skip_dr2 := True;
    sparam.skip_matched := True;
    sparam.skip_reject := True;
    sparam.auto_match_good := True;
    sparam.auto_match_v := False;
  end;
  sparam.max_search_dist := 0.3;
  sparam.match_conditional :=True;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.

