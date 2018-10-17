unit dr2addnew;

{$mode delphi}

interface

(* Thread and data types for adding unmatched stars from DR2 Data to the main
star list. *)

uses
  Classes, SysUtils, LCLIntf, LMessages, Math, DAMath,
  gaiadr2base, gaiadr2holder, stardata, collecdata, newlocation, namedata,
  NewStar, ImportVizier, simbad, StarExt2, gaiaset;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

(* Basic settings *)
DR2AddSettings = record
  minpllx:Real;
  ignoreRejects:Boolean;
  downSimbad:Boolean;
  down2mass:Boolean;
end;

(* Settings / Options for automatically adding unmatched stars  *)
DR2AutoAddSettings = record
  minGMag:Currency;
  maxPllxError:Real;
  reqSelectionA:Boolean;
  reqSelectionC:Boolean;
end;

(* Settings / Options for auto rejecting unmatched stars, all must match *)
DR2AutoRejectSettings = record
  maxGMag:Currency;  // this brightness or dimmer required
  minPllxErr:Real;   // parallax error must be this or bigger
  maxPMMag:Real;     // large proper motions can be checked, require smaller
  maxLatitude:Real; // galactic latitude must be within this band
  nevRej2Mass:Boolean; // if true, a star is never rejected if it has a 2MASS match
end;

(* Class used when we pass data to the user for approval *)
DR2AddBundle = class
  public
    curobj:GaiaDR2Star;
    simdat:SimbadData;
    tmassd:VizieR2MASSData;

    constructor Create(curobj_in:GaiaDR2Star;simdat_in:SimbadData;tmassd_in:VizieR2MASSData); overload;
    constructor Create(curobj_in:GaiaDR2Star); overload;
    destructor Destroy();
    procedure AddObject();
    procedure RejectObject();
end;

// thread that handles bulk matching, possibly semi-automated
AddFromDR2Thread = class(TThread)
  protected
    baseparams:DR2AddSettings;
    autoparams:DR2AutoAddSettings;
    rejectparams:DR2AutoRejectSettings;
    curobj:GaiaDR2Star;

    msgTarget:THandle; // window handle for message target
    waitlock:PRTLEvent;  // used to pause and resume thread
    doquit:Boolean; // set to true to terminate early

    procedure PostCannotStart(why:string); // just in case things go badly wrong
    procedure PostConfirmAdd(curobj_in:GaiaDR2Star;simdat_in:SimbadData;tmassd_in:VizieR2MASSData); // send stuff to confirm add
    function CheckForAutoReject():Boolean;
    function CheckForAutoAdd():Boolean;
    procedure AddCheckForUnmatched();  // looks at the current gaia star/brown dwarf
    procedure Execute; override;  // top level execution
  public
    constructor Create(createSuspended:Boolean; sett:DR2AddSettings; aparams:DR2AutoAddSettings; rparams:DR2AutoRejectSettings; mtarget:THandle);
    procedure WaitIsOver(quit:Boolean);  // the form should call this to resume
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
const
// message constants
  MSG_IOSTART = LM_USER + 3001;
  MSG_CANNOTSTART = LM_USER + 3010;
  MSG_STARADDED = LM_USER + 3013;
  MSG_CHECKADD = LM_USER + 3014;
  MSG_DONE = LM_USER + 3006;



procedure AddGaiaStar(newobj:GaiaDR2Star; simdat:SimbadData; tmassd:VizieR2MASSData);

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//==========================================================================
constructor DR2AddBundle.Create(curobj_in:GaiaDR2Star;simdat_in:SimbadData;tmassd_in:VizieR2MASSData);
begin
  curobj := curobj_in;
  simdat := simdat_in;
  tmassd := tmassd_in;
end;
//-----------------------------------------------------------
constructor DR2AddBundle.Create(curobj_in:GaiaDR2Star);
begin  curobj := curobj_in;  end;
//-----------------------------------------------------------
destructor DR2AddBundle.Destroy();
begin
  simdat.Free;
  tmassd.Free;
end;
//-----------------------------------------------------------
procedure DR2AddBundle.AddObject();
begin
  AddGaiaStar(curobj,simdat,tmassd);
  curobj.matched := True;
  curobj.permaReject := False;
end;
//-----------------------------------------------------------
procedure DR2AddBundle.RejectObject();
begin
  curobj.permaReject := True;
end;
//==========================================================================
 // just in case things go badly wrong
procedure AddFromDR2Thread.PostCannotStart(why:string);
var pcopy:PString;
begin
  New(pcopy);
  pcopy^ := why;
  PostMessage(msgTarget,MSG_CANNOTSTART,Int64(pcopy),0);
end;
//-----------------------------------------------
// send stuff to confirm match
procedure AddFromDR2Thread.PostConfirmAdd(curobj_in:GaiaDR2Star;simdat_in:SimbadData;tmassd_in:VizieR2MASSData);
var bundle:DR2AddBundle;
begin
  bundle := DR2AddBundle.Create(curobj_in,simdat_in,tmassd_in);
  PostMessage(msgTarget,MSG_CHECKADD,Int64(bundle),0);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(*  autoparams:DR2AutoAddSettings;   rejectparams:DR2AutoRejectSettings;  *)
// checking for automatic rejection, 2 zones
function AddFromDR2Thread.CheckForAutoReject():Boolean;
var cpmmag,pxerr,accepterr:Real;
begin
  Result := False;
  if curobj.mags.G < rejectparams.maxGMag then Exit;   // too bright, include
  if (curobj.ids.TwoMASS <> '') and rejectparams.nevRej2Mass then Exit;
  pxerr := curobj.astrometry.parallax_err;
  // outisde 'zone of avoidance'
  if Abs(curobj.astrometry.glat) > rejectparams.maxLatitude then begin
     // if the parallax error is lower than the limit, okay
    if ( pxerr < rejectparams.minPllxErr) then Exit;
    // twice the limit is always reject, but between, the pmmag has to be met
    if (pxerr < (2 * rejectparams.minPllxErr)) then begin
      cpmmag := hypot(curobj.astrometry.rapm,curobj.astrometry.decpm);
      if cpmmag > rejectparams.maxPMMag then Exit;
    end;
  end
  // within the 'zone of avoidance'
  else begin
    (* We compute a parallax error that is always not rejected, between
       auto include and auto reject. *)
    accepterr := (autoparams.maxPllxError + rejectparams.minPllxErr) / 2.0;
    accepterr := Max(autoparams.maxPllxError,accepterr);
    if (pxerr < accepterr) then Exit;
    // if the parallax error is below the limit, we do a pm check...
    if (pxerr < rejectparams.minPllxErr) then begin
      cpmmag := hypot(curobj.astrometry.rapm,curobj.astrometry.decpm);
      if cpmmag > rejectparams.maxPMMag then Exit;
    end;
  end;

  Result := True; // the object is rejected!
end;
//-----------------------------------------------
// checking for automatic addition, by elimination
function AddFromDR2Thread.CheckForAutoAdd():Boolean;
begin
  Result := False;
  if curobj.selectionBfail then Exit;
  if curobj.astrometry.parallax_err > autoparams.maxPllxError then Exit;
  if curobj.mags.G > autoparams.minGMag then Exit;
  if autoparams.reqSelectionA and curobj.selectionAfail then Exit;
  if autoparams.reqSelectionC and curobj.selectionCfail then Exit;
  Result := True;
end;
//--------------------------------------------------------
// handles the current gaia object for addition/rejection, etc
procedure AddFromDR2Thread.AddCheckForUnmatched();
var autoadd:Boolean;
    simdat:SimbadData;
    tmassd:VizieR2MASSData;
    dotmass:Boolean;
    testid:string;
begin
  // quick ignore checks
  if baseparams.ignoreRejects and curobj.permaReject then Exit;
  if curobj.astrometry.parallax < baseparams.minpllx then Exit;
  (* Due to a previous error, many 'unmatched' objects are actually matched. We
  get around this by checking if the id is in a string set that has all DR2 ids
  that are currently in the list of stars *)
  testid := IntToStr(curobj.gaia_id);
  if DR2_IDSet.Contains(testid) then begin
    curobj.matched := True;
    Exit;
  end;
  // auto reject testing
  if baseparams.ignoreRejects then begin
    if CheckForAutoReject() then begin
      curobj.permaReject:= True;
      Exit;
    end;
  end;
  // simbad and two mass
  simdat := nil;
  tmassd := nil;
  dotmass := False;
  if baseparams.downSimbad then simdat := GaiaToSimbad(curobj.ids);
  if baseparams.down2mass then begin
    if simdat = nil then dotmass := True
    else if simdat.KMagnitudeError = 0 then dotmass := True;
  end;
  if dotmass and (Length(curobj.ids.TwoMASS) > 0) then begin
    tmassd := Get2MASSFromVizier(curobj.ids.TwoMASS);
  end;
  // the star or brown dwarf should not be ignored, but do we add it automatically?
  autoadd := CheckForAutoAdd();
  // now we know...
  if autoadd then begin
    AddGaiaStar(curobj,simdat,tmassd);
    curobj.matched := True;
    curobj.permaReject := False;
    simdat.Free;
    tmassd.Free;
    PostMessage(msgTarget,MSG_STARADDED,0,0);
  end
  // otherwise we pass the object to the user for confirmation
  else begin
    PostConfirmAdd(curobj,simdat,tmassd);
    RTLEventWaitFor(waitlock);   // wait, unitl wait is over is called...
    RTLeventResetEvent(waitlock); // once resumed, we clear the wait
  end;
end;
//------------------------------------------------
// top level execution
procedure AddFromDR2Thread.Execute;
var stardex:Integer;
begin
  // we first do some initial checks
  if primaryl = nil then PostCannotStart('No Star list to add to!')
  else if DR2Data.StarCount = 0 then PostCannotStart('Gaia DR2 Data is empty!')
  else begin
    // here, we may go ahead!
    PostMessage(msgTarget,MSG_IOSTART,0,0);
    DR2Data.ResetUnmatchedIndex();
    while True do begin
      curobj := Dr2Data.NextUnmatched(stardex);
      if (curobj = nil) then Break;
      AddCheckForUnmatched();
      if doquit then Break;
    end;
    PostMessage(msgTarget,MSG_DONE,0,0);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor AddFromDR2Thread.Create(createSuspended:Boolean; sett:DR2AddSettings; aparams:DR2AutoAddSettings; rparams:DR2AutoRejectSettings; mtarget:THandle);
begin
  // parameters
  baseparams := sett;
  autoparams := aparams;
  rejectparams := rparams;
  // additional
  msgTarget := mtarget;
  waitlock := RTLEventCreate;
  curobj := nil;
  doquit := False;
  // done
  inherited Create(createSuspended);
end;
//------------------------------------------------
// the form should call this to resume
procedure AddFromDR2Thread.WaitIsOver(quit:Boolean);
begin
  doquit := quit;
  RTLeventSetEvent(waitlock);
end;
//========================================================================
procedure AddGaiaStar(newobj:GaiaDR2Star; simdat:SimbadData; tmassd:VizieR2MASSData);
var newid:Integer;
    newsys:StarSystem;
    cstar:StarInfo;
begin
  Assert(newobj<>nil);
  Assert(primaryl<>nil);
  newid := primaryl.NextID;
  newsys := StarSystem.Create(newid);
  if simdat <> nil then newsys.AddSimbadData(1,simdat,False);
  newsys.ApplyGaiaObject(1,newobj,False);
  if tmassd <> nil then begin
    if not tmassd.AllBad() then begin
      cstar := (newsys.GetNewStar(1) as StarInfo);
      if cstar.fluxtemp = nil then cstar.fluxtemp := StarFluxPlus.Create;
      if not tmassd.jbad then cstar.fluxtemp.J_mag := tmassd.J;
      if not tmassd.hbad then cstar.fluxtemp.H_mag := tmassd.H;
      if not tmassd.kbad then begin
        cstar.fluxtemp.K_mag := tmassd.Ks;
        cstar.fluxtemp.K_err := tmassd.Kserr;
      end;
    end;
  end;
  primaryl.AppendSystem(newsys);
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end.

