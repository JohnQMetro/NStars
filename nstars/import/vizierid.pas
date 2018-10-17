unit VizierID;

{$mode delphi}

interface
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(* Getting IDs from Vizier *)

uses
  Classes, SysUtils, NewStar, gaiadr2base, utilities, star_names, character,
  newlocation, stardata, LMessages, LCLIntf, ImportVizier, starext2,
  VizierPOST;

type

(* The 3 IDs we could try to fetch are UCAC4, Gaia DR1, and DENIS *)
FetchIDWrapper = class
  protected
    function EstimateDR1_G(const dr2mags:GaiaDR2Mags):Boolean;
    function Load2MASS(ccomp:NewStarBase; names:StarNames):Boolean;
  public
    ucac4_do,gaia1_do,denis_do:Boolean; // do we try to fetch this id?
    tmass_id:string;  // 2MASS id (used for UCAC4 and DENIS)
    J,H,Ks:Currency;  // UCAC4 tables include cross-matched 2MASS mags
    Je,Ke:Currency;   // estimates for DENIS J and K
    Gmin,Gmax:Real;  // range of Gaia DR1 G mag
    raDR2,decDR2:Real;   // Gaia DR2 position
    names_ptr:StarNames;
    flux_ptr:StarFluxPlus;

    constructor Create;
    function InitFromObject(ccomp:NewStarBase; names:StarNames; inlocation:Location):Boolean;
end;

IDFetchThread = class(TThread)
protected
  // startdex,sysdex:Integer;   // first system index, current system index
  stardex:Integer;  // index of star in system
  currentSystem:StarSystem;   // currently checked system
  currentInfo:FetchIDWrapper; // id fetching info for current star

  msgTarget:THandle; // window handle for message target
  // waitlock:PRTLEvent;  // used to pause and resume thread
  // doquit:Boolean; // set to true to terminate early

  procedure PostCannotStart(why:string); // just in case things go wrong
  // procedure PostBlockDone(const stars_done:Integer; const ids_added:Integer);
  procedure PostDone(ids_added:string);

  function MakeWrapper(const sys:StarSystem; const stardex:Integer):Boolean;
  function ParseUCAC4(const down_page:string):string;
  function ParseDENIS(const down_page:string):string;
  function ParseGaia1(const down_page:string):string;
  function DoCurrentStar():string;

  procedure Execute; override;  // top level execution
public
  constructor Create(createSuspended:Boolean; sys_in:StarSystem; stardex_in:Integer; mtarget:THandle);
  // procedure WaitIsOver(quit:Boolean);  // the form should call this to resume
end;

const
// message constants
  MSG_IDSTART = LM_USER + 3501;
  MSG_IDFAIL  = LM_USER + 3502;
  MSG_IDDATA  = LM_USER + 3503;
  MSG_IDDONE  = LM_USER + 3504;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//=========================================================================
(* Estimate Gaia DR1 G mag and a safety range from DR2 magnitudes. Note: for best
 accuracy, white dwarfs should use a separate fit, I am ignoring that. *)
function FetchIDWrapper.EstimateDR1_G(const dr2mags:GaiaDR2Mags):Boolean;
var gr,bpmrp,gmid,gmrp:Real;
begin
  Result := False;
  if (dr2mags = nil) or (dr2mags.G >= 90) then Exit;
  Result := True;
  // default values
  gr := CurrToReal(dr2mags.G);
  Gmin := gr - 0.4;  Gmax := gr + 0.1;
  if (dr2mags.RP >= 90) or (dr2mags.RPerr > 0.025) then Exit;
  // check BP - RP
  if (dr2mags.BP < 90) and (dr2mags.BPerr < 0.025) then begin
   bpmrp := CurrToReal(dr2mags.BPminRP);
   if (bpmrp > -0.34) and (bpmrp < 3.2) then begin
     gmid := gr - ( -0.014702 + 0.082157*bpmrp);
     Gmin := gmid - 0.11;  Gmax := gmid + 0.11;
   end else if (bpmrp >= 3.2) then begin
     Gmin := gr - 0.3;  Gmax := gr - 0.1;
   end else begin
     Gmin := gr - 0.05; Gmax := gr + 0.15;
   end;
   Exit;
  end;
  // Try G - RP
  gmrp := gr - CurrToReal(dr2mags.RP);
  if (gmrp > -0.195) and (gmrp < 1.3) then begin
    gmid := gr - ( -0.055451 + 0.2193*gmrp );
    Gmin := gmid - 0.11;  Gmax := gmid + 0.11;
  end else if (gmrp >= 1.3) then begin
    Gmin := gr - 0.32;  Gmax := gr - 0.1;
  end;
end;
//--------------------------------------
function FetchIDWrapper.Load2MASS(ccomp:NewStarBase; names:StarNames):Boolean;
var tmassx:string;
begin
  Result := False;
  // we need a valid 2MASS is, and a minimal set of magnitudes
  if not names.GetCatValue('2MASS',tmassx) then Exit;
  if not IsDigit(tmassx[High(tmassx)]) then Exit;
  if ccomp.fluxtemp = nil then Exit;
  if (not ccomp.fluxtemp.Valid_JMagnitude) then Exit;
  if (not ccomp.fluxtemp.Valid_KMagnitude) then Exit;
  // if we get here, things are okay...
  tmass_id := '2MASS ' + tmassx;
  J := ccomp.fluxtemp.J_mag;
  H := ccomp.fluxtemp.H_mag;
  Ks := ccomp.fluxtemp.K_mag;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor FetchIDWrapper.Create;
const cmax:Currency = 99.999;
begin
  ucac4_do := False;
  gaia1_do := False;
  denis_do := False;
  J := cmax;  H := cmax;  Ks := cmax;
  Je := 99.999;  Ke := 99.999;
  Gmin := 99.999;  Gmax := 99.999;
end;

//------------------------------------
function FetchIDWrapper.InitFromObject(ccomp:NewStarBase; names:StarNames; inlocation:Location):Boolean;
var has2mass,hasgaia2:Boolean;
begin
  Result := False;
  if (ccomp = nil) or (names = nil) then Exit;
  // checking for exiting ids
  ucac4_do := not names.HasCat('UCAC4');
  gaia1_do := not names.HasCat('Gaia DR1');
  denis_do := not names.HasCat('DENIS');
  if (denis_do) then denis_do := (inlocation.GetDecimalDeclination < 2.2);
  if (gaia1_do) then gaia1_do := (inlocation <> nil);
  if (not ucac4_do) and (not gaia1_do) and (not denis_do) then Exit;
  // checking for what we need for UCAC4 and DENIS
  if ucac4_do or denis_do then begin
    if Load2MASS(ccomp,names) then begin
      if denis_do then begin
        Je := J;  Ke := Ks;
      end;
    end else begin
      ucac4_do := False;
      denis_do := False;
    end;
  end;
  // checking gaia...
  if gaia1_do then begin
    gaia1_do := EstimateDR1_G(ccomp.dr2mags);
    if gaia1_do then begin
      // get J2015.5 position... (very close to J2015 in DR1)
      inlocation.MakeGetJ2015p5Pos(raDR2,decDR2);
    end;
  end;
  // done
  if (not ucac4_do) and (not gaia1_do) and (not denis_do) then Exit;
  names_ptr := names;
  if gaia1_do then flux_ptr := ccomp.fluxtemp
  else flux_ptr := nil;
  Result := True;
end;
//===========================================================================
(*// startdex,sysdex:Integer;   // first system index, current system index
  stardex:Integer;  // index of star in system
  currentSystem:StarSystem;   // currently checked system
  currentInfo:FetchIDWrapper; // id fetching info for current star

  msgTarget:THandle; // window handle for message target
  // waitlock:PRTLEvent;  // used to pause and resume thread
  // doquit:Boolean; // set to true to terminate early *)

procedure IDFetchThread.PostCannotStart(why:string);
var pcopy:PString;
begin
  New(pcopy);
  pcopy^ := why;
  PostMessage(msgTarget,MSG_IDFAIL,Int64(pcopy),0);
end;
//-----------------------------------------------
// procedure PostBlockDone(const stars_done:Integer; const ids_added:Integer);
//-----------------------------------------------
procedure IDFetchThread.PostDone(ids_added:string);
var pcopy:PString;
begin
  New(pcopy);
  pcopy^ := ids_added;
  PostMessage(msgTarget,MSG_IDDONE,Int64(pcopy),0);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function IDFetchThread.MakeWrapper(const sys:StarSystem; const stardex:Integer):Boolean;
var temp:FetchIDWrapper;
    ccomp_in:NewStarBase;
    names_in:StarNames;
    location_in:Location;
begin
  Result := False;
  if (sys = nil) or (stardex < 1) then Exit;
  if (stardex > sys.GetCompC) then Exit;
  // getting the inputs
  ccomp_in := sys.GetNewStar(stardex);
  if ccomp_in = nil then Exit;
  if (stardex = 1) then location_in := sys.GetLocation()
  else location_in := ccomp_in.GetLocation();
  if (sys.GetCompC = 1) then names_in := sys.GetNames()
  else names_in := ccomp_in.GetNames();
  // finally, creating the output and checking for getting the ids
  temp := FetchIDWrapper.Create;
  if (not temp.InitFromObject(ccomp_in,names_in,location_in)) then begin
    temp.Free;  Exit;
  end;
  currentInfo := temp;
  Result := True;
end;
//----------------------------------------------------------------------
function IDFetchThread.ParseUCAC4(const down_page:string):string;
var zparse:VizieRDataBase;
    idf:string;
begin
  Result := '';
  zparse := VizieRDataBase.Create;
  try
    if not zparse.Start('UCAC4 Catalogue (Zacharias+, 2012)',down_page) then Exit;
    if not zparse.SkipCells(4) then Exit;
    // this one is the ID
    if not zparse.NextCellStr(idf) then Exit;
    // no duplicates
    if zparse.HasAnotherRow() then Exit;
    Result := idf;
  finally
    zparse.Free;
  end;
end;
//----------------------------------------------------------------------
function IDFetchThread.ParseDENIS(const down_page:string):string;
var zparse:VizieRDataBase;
    idf,idfold:string;
begin
  Result := '';
  idfold := '';
  zparse := VizieRDataBase.Create;
  try
    if not zparse.Start('The DENIS database (DENIS Consortium, 2005)',down_page) then Exit;
    repeat
      if not zparse.SkipCells(4) then Exit;
      // skipping over position
      if not zparse.SkipCells(2) then Exit;
      // skipping over IJK magnitudes
      if not zparse.SkipCells(6) then Exit;
      // the identifier
      if not zparse.NextCellStr(idf) then Exit;
      // DENIS sometime has more than 1 row for the same id, so if we have more, we check
      if idfold <> '' then begin
        if idfold <> idf then Exit;
      end;
      idfold := idf;
    until (not zparse.HasAnotherRow());
    Result := idf;
  finally
    zparse.Free;
  end;
end;
//-------------------------------------------------------------------
function IDFetchThread.ParseGaia1(const down_page:string):string;
var zparse:VizieRDataBase;
    idf:string;
    flux,fluxe,gmag:Real;
begin
  Result := '';
  zparse := VizieRDataBase.Create;
  try
    if not zparse.Start('Gaia DR1 (Gaia Collaboration, 2016)',down_page) then Exit;
    if not zparse.SkipCells(4) then Exit;
    // the first field should be the id
    if not zparse.NextCellStr(idf) then Exit;
    // next, we have flux and flux error
    if not zparse.Next2CellsFloatR(flux,fluxe) then Exit;
    // next, G magnitude
    if not zparse.NextCellFloat(99.9999,gmag) then Exit;
    // if there is more than 1 row, we exit: 2 possible matches
    if zparse.HasAnotherRow() then Exit;
    // done
    if (currentInfo.flux_ptr <> nil) then currentInfo.flux_ptr.gaia_mag := gmag;
    Result := idf;
  finally
    zparse.Free;
  end;
end;
//---------------------------------------------
function IDFetchThread.DoCurrentStar():string;
var postdata,downloaded:string;
    dok:Boolean;
    ostring,finstr:string;
    fiw:FetchIDWrapper;
    rcount:Integer;
    fsOut: TFileStream;
const vizurl2 = 'http://vizier.u-strasbg.fr/viz-bin/VizieR-4';
begin
  Result := '';
  rcount := 0;
  if currentInfo = nil then Exit;
  fiw := currentInfo;
  // UCAC4
  if fiw.ucac4_do then begin
    postdata := MakeUCAC4_Post(fiw.tmass_id,fiw.J,fiw.H,fiw.Ks);
    dok := GetByPOSTS(vizurl2,postdata,ctLatin1,downloaded);
    if dok then begin
      ostring := ParseUCAC4(downloaded);
      if (ostring <> '') then begin
        Inc(rcount);
        finstr := 'UCAC4 ' + ostring;
      end;
    end;
  end;
  // DENIS
  if fiw.denis_do then begin
    postdata := MakeDENIS_Post(fiw.tmass_id,20,fiw.Je,fiw.Ke);
    dok := GetByPOSTS(vizurl2,postdata,ctLatin1,downloaded);
    if dok then begin
      ostring := ParseDENIS(downloaded);
      if (ostring <> '') then begin
        Inc(rcount);
        if finstr <> '' then finstr += ',';
        finstr += 'DENIS ' + ostring;
      end;
    end;
  end;
  // Gaia DR1
  if fiw.gaia1_do then begin
    postdata := MakeGaiaDR1_Post(fiw.raDR2,fiw.decDR2,fiw.Gmin,fiw.Gmax);
    dok := GetByPOSTS(vizurl2,postdata,ctLatin1,downloaded);
    if dok then begin
      ostring := ParseGaia1(downloaded);
      if (ostring <> '') then begin
        Inc(rcount);
        if finstr <> '' then finstr += ',';
        finstr += 'Gaia DR1 ' + ostring;
      end;
    end;
  end;
  // if we have anything, we add it to the names
  if rcount > 0 then fiw.names_ptr.SetMultipleCat(finstr);
  Result := finstr;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure IDFetchThread.Execute;
var res:string;
begin
  PostMessage(msgTarget,MSG_IDSTART,0,0);
  if not MakeWrapper(currentSystem,stardex) then begin
    PostCannotStart('Cannot lookup ids: missing prerequisites, or we already have them.')
  end else begin
    res := DoCurrentStar();
    PostDone(res);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor IDFetchThread.Create(createSuspended:Boolean; sys_in:StarSystem; stardex_in:Integer; mtarget:THandle);
begin
  msgTarget := mtarget;
  currentSystem := sys_in;
  stardex := stardex_in;
  inherited Create(createSuspended)
end;
  // procedure WaitIsOver(quit:Boolean);  // the form should call this to resume

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.

