unit startproxy;

{$mode delphi}

interface

(* Moving StarProxy to a separate unit, to try and reduce the massive size of the
collecdata unit. *)

uses
  Classes, SysUtils, Dialogs, Controls, Forms, StrUtils,
  tgas, stardata, newlocation, star_names (* namedata*), NewStar, constellation, gaiadr2holder,
  simbad, guessstype, sptfluxest, fluxtransform, starext2, importvizier,
  df_strings, Utilities, gaiadr2types, utilities2, fluxgaia, WhiteDwarf,
  fluxtransform2;

type

(* this class encapuslates easy access to info for the currently edited system and star *)
StarProxy = class
  protected
    function FindTGASMatches(name_search:Boolean):TGASList;
    function TGASMatchToCurrentStar:TGASList;
    function FoundListToString(inlinst:TGASList):string;
    function GetCurrentParallax:Real;
    procedure ApplyTGASChange(newdata:TGASData);
    function ShowEst(Vest:Real; Best,Rcest,Icest:Currency; amsg:string):Boolean;
    function ShowEstJHK(Jest,Hest,Ksest:Currency; amsg:string):Boolean;
  public
    // system level information
    sys:StarSystem;
    sysl:Location;
    sysn:StarNames;
    // star level info
    ccomponent:NewStarBase;
    cstar:StarInfo;
    cstarl:Location;
    cstarn:StarNames;
    starindex:Integer;
    // public methods
    constructor Create;
    // setting the system
    procedure SetSystem(inval:StarSystem);
    // setting the star
    procedure SetStar(index:Integer);
    procedure AddStar;
    procedure AddBrownDwarf;
    procedure InsertStar;
    function SwapComponent():Boolean;
    procedure DeleteStar(index:Integer);
    // extra methods
    function StarNameTest:Boolean;
    function LocateConst:Integer;
    function ShowTGASMatches(byname:Boolean):Boolean;
    function TGASStarMatch:Boolean;
    function GaiaDR2ShowMatch(maxdist:Single):Boolean;
    function SimbadDownload(simbadurl:string; fluxonly:Boolean):Boolean;
    function SimbadDownloadLogg(simbadurl:string):Boolean;
    function SimbadCatalogs(simbadurl:string):Boolean;
    function CurrentToBrownDwarf:Boolean;
    // location via separation and position angle
    function PosAngLocationSet:Boolean;
    // more location stuff
    function OldParallaxCount():Integer;
    function OldParallaxSwap():Boolean;
    // even more methods
    function GuessSpectra(out usespec:Boolean):Boolean;
    function APASS_Helper(indata:string):Boolean;
    function SDSS_Helper(indata:string):Boolean;
    function Tycho2_Helper(indata:string):Boolean;
    function VizierAPASSGet(simbadalso:Boolean):Boolean;
    function Vizier2MASSGet():Boolean;
    function UCAC4_ToVRI_Helper(indata:string):Boolean;
    function CMC_ToVRI(indata:string):Boolean;
    function BPRP_To_VRI():Boolean;
    function GaiaDR2_To_JHK():Boolean;
    function PanStarrs_To_BVRI(indata:string):Boolean;
    function VizierGaiaGet():Boolean;
    function GuessVFromG():Boolean;
    function PanStarrsJHK(indata:string):Boolean;
    function GG1_VRI(useRP:Boolean):Boolean;
    function DA_GaiaTEff(which:Word):Boolean;
    function Tycho2G_Helper(indata:string):Boolean;
    function VtG_Helper(indata:string):Boolean;
    function SMSSHelper(indata:string):Boolean;
    function BinaryGSplit():Boolean;
end;

var
  current:StarProxy;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//========================================================================
function StarProxy.FindTGASMatches(name_search:Boolean):TGASList;
var inpra, inpdec:Double;
    namelist:TStringList;
begin
  Result := nil;
  if sysl = nil then Exit;
  if tgas_main.StarCount = 0 then Exit;
  // now, we actually look for matches
  if name_search then begin
    namelist := sys.HipTycNames();
    Result := tgas_main.FindNameMatches(namelist);
    FreeAndNil(namelist);
  end else begin
    sysl.GetArcsecPosition(False,inpra,inpdec);
    Result := tgas_main.FindJ2000Matches(inpra,inpdec);
  end;
end;
//--------------------------------------------
function StarProxy.TGASMatchToCurrentStar:TGASList;
var inpra, inpdec:Double;
begin
  // asserts (make sure they pass before calling this method!)
  Assert(starindex>1);
  Assert(cstarl <> nil);
  // going ahead
  cstarl.GetArcsecPosition(False,inpra,inpdec);
  Result := tgas_main.FindJ2000Matches(inpra,inpdec);
end;
//---------------------------------------------
function StarProxy.FoundListToString(inlinst:TGASList):string;
var sindex,smax:Integer;
    resline, outmsg:string;
begin
  // no stars case
  if (inlinst = nil) or (inlinst.Count = 0) then begin
    Result := 'No Stars Found!';
    Exit;
  end;
  // otherwise...
  smax := inlinst.Count - 1;
  outmsg := 'Stars Found :' + sLineBreak;
  for sindex := 0 to smax do begin
    resline := inlinst[sindex].SummaryString2;
    outmsg := outmsg + resline + sLineBreak;
  end;
  Result := outmsg;
end;
//--------------------------------------
function StarProxy.GetCurrentParallax:Real;
begin
  if cstarl<>nil then Result := cstarl.ParallaxMAS
  else Result := sysl.ParallaxMAS;
end;
//--------------------------------------
procedure StarProxy.ApplyTGASChange(newdata:TGASData);
var ra_offsets:array of Real;
    dec_offsets:array of Real;
    currstar:NewStarBase;
    stardex,starmax:Integer;
    sok:Boolean;
begin
  Assert(newdata<>nil);
  starmax := current.sys.GetCompC;
  // preparing for multi-location shift...
  if starmax > 1 then begin
    SetLength(ra_offsets,starmax-1);
    SetLength(dec_offsets,starmax-1);
    for stardex := 2 to starmax do begin
      currstar := current.sys.GetNewStar(stardex);
      if (not currstar.HasLocation) then begin
        ra_offsets[stardex-2] := 0;
        dec_offsets[stardex-2] := 0;
      end
      else begin
        current.sysl.GetRADecDifference(currstar.GetLocation,ra_offsets[stardex-2],dec_offsets[stardex-2]);
      end;
    end;
  end;
  // done with storing the sub-component offsets
  // updating the primary location to the tgas one...
  sok := current.sysl.SetFromTGAS(newdata);
  Assert(sok);
  // adding the offsets to the new location for components
  if starmax > 1 then begin
    SetLength(ra_offsets,starmax-1);
    SetLength(dec_offsets,starmax-1);
    for stardex := 2 to starmax do begin
      currstar := current.sys.GetNewStar(stardex);
      if (currstar.HasLocation) then begin
        currstar.CopyLocation(current.sysl,True);
        currstar.GetLocation.AddRADecDifference(ra_offsets[stardex-2],dec_offsets[stardex-2]);
      end;
    end;
  end;
  // done...
  newdata.matched := True;
end;
//----------------------------------------------------------
function StarProxy.ShowEst(Vest:Real; Best,Rcest,Icest:Currency; amsg:string):Boolean;
var msgdata:string;
    rval:Word;
    usecount:Integer;
    vstr,bstr,outm:string;
begin
  Result := False;
  usecount := 0;
  outm := '';
  // building the message to show
  msgdata := 'The estimates are :' + sLineBreak;
  // B and V
  if (Best < 90) then begin
    bstr := CurrToStrF(Best,ffFixed,3);
    msgdata += 'B : ' + bstr + sLineBreak;
    Inc(usecount);
    outm += 'B';
  end;
  if (Vest < 90) then begin
    vstr := FloatToStrF(Vest,ffFixed,2,3);
    msgdata += 'V : ' + vstr + sLineBreak;
    Inc(usecount);
    outm += 'V';
  end;
  // Rc and Ic
  if (Rcest < 90) then begin
    msgdata += 'Rc : ' + CurrToStrF(Rcest,ffFixed,3) + sLineBreak;
    Inc(usecount);
    outm += 'R';
  end;
  if (Icest < 90) then begin
    msgdata += 'Ic : ' + CurrToStrF(Icest,ffFixed,3) + sLineBreak;
    Inc(usecount);
    outm += 'I';
  end;
  if (usecount = 0) then Exit
  else if usecount = 1 then msgdata += 'Do you want to use this magnitude?'
  else msgdata += 'Do you want to use these magnitudes?';
  // asking if we want to use the estimated magnitudes
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    // V and B
    if (Best < 90) or (Vest < 90) then begin
      // for brown dwarfs (unlikley), we put the magnitudes in the notes
      if ccomponent.isBrownDwarf then begin
        if (Vest < 90) then ccomponent.AppndNote('V~' + vstr,False);
        if (Best < 90) then begin
          if (Vest < 90) then ccomponent.AppndNote(', ',False);
          ccomponent.AppndNote('B~' + bstr,False);
        end;
      // otherwise, setting the star magnitudes
      end else begin
        if (Vest < 90) then cstar.SetVisualMagnitude(Vest);
        if (Best < 90) then begin
          if cstar.fluxtemp = nil then cstar.fluxtemp := StarFluxPlus.Create;
          cstar.fluxtemp.blue_mag := Best;
        end;
      end;
    end;
    // Rc and Ic
    if (Rcest < 90) or (Icest < 90) then begin
      if ccomponent.fluxtemp = nil then ccomponent.fluxtemp := StarFluxPlus.Create;
      if (Rcest < 90) then ccomponent.fluxtemp.red_mag := Rcest;
      if (Icest < 90) then ccomponent.fluxtemp.I_mag := Icest;
    end;
    // adding some notes
    if not ccomponent.NotesConatins(amsg) then begin
      ccomponent.AppndNote(' ' + outm + ' ' + amsg,False);
    end;
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//----------------------------------------------------------
function StarProxy.ShowEstJHK(Jest,Hest,Ksest:Currency; amsg:string):Boolean;
var msgdata:string;
    rval:Word;
    usecount:Integer;
    outm:string;
begin
  Result := False;
  usecount := 0;
  outm := '';
  // building the message to show
  msgdata := 'The estimates are :' + sLineBreak;
  // J
  if (Jest < 90) then begin
    msgdata += 'J : ' + CurrToStrF(Jest,ffFixed,3) + sLineBreak;
    Inc(usecount);
    outm += 'J';
  end;
  // H
  if (Hest < 90) then begin
    msgdata += 'H : ' + CurrToStrF(Hest,ffFixed,3) + sLineBreak;
    Inc(usecount);
    outm += 'H';
  end;
  // Ks
  if (Ksest < 90) then begin
    msgdata += 'Ks: ' + CurrToStrF(Ksest,ffFixed,3) + sLineBreak;
    Inc(usecount);
    outm += 'K';
  end;
  if (usecount = 0) then Exit
  else if usecount = 1 then msgdata += 'Do you want to use this magnitude?'
  else msgdata += 'Do you want to use these magnitudes?';
  // asking if we want to use the estimated magnitudes
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    if ccomponent.fluxtemp = nil then ccomponent.fluxtemp := StarFluxPlus.Create;
    if (Jest < 90) then ccomponent.fluxtemp.J_mag := Jest;
    if (Hest < 90) then ccomponent.fluxtemp.H_mag := Hest;
    if (Ksest < 90) then ccomponent.fluxtemp.K_mag := Ksest;
    // adding some notes
    if not ccomponent.NotesConatins(amsg) then begin
      ccomponent.AppndNote(' ' + outm + ' ' + amsg,False);
    end;
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// public methods
constructor StarProxy.Create;
begin
  sys := nil;
  sysl := nil;
  sysn := nil;
  ccomponent := nil;
  cstar := nil;
  cstarl := nil;
  cstarn := nil;
  starindex := 0;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting the system
procedure StarProxy.SetSystem(inval:StarSystem);
begin
  Assert(inval<>nil);
  sys := inval;
  sysl := inval.GetLocation;
  sysn := inval.GetNames;
  if sys.GetId = 1 then begin
    ccomponent := nil;
    cstar := nil;
    cstarl := nil;
    cstarn := nil;
    starindex := 0;
  end
  else begin
    ccomponent := inval.GetNewStar(1);
    cstarl := ccomponent.GetLocation;
    cstarn := ccomponent.GetNames;
    if ccomponent.isBrownDwarf then cstar := nil
    else cstar := StarInfo(ccomponent);
    starindex := 1;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting the star
procedure StarProxy.SetStar(index:Integer);
begin
  // checks
  if index = 0 then Exit;
  Assert(index>0);
  Assert(index<=(sys.GetCompC));
  // changing the star
  if sys.GetId = 1 then begin
    ccomponent := nil;
    cstar := nil;
    cstarl := nil;
    cstarn := nil;
    starindex := 0;
  end
  else begin
    ccomponent := sys.GetNewStar(index);
    if ccomponent.isBrownDwarf then cstar := nil
    else cstar := StarInfo(ccomponent);
    cstarl := ccomponent.GetLocation;
    cstarn := ccomponent.GetNames;
    starindex := index;

  end;
end;
//------------------------------------------------------------
procedure StarProxy.AddStar;
begin
  ccomponent := sys.AddStar;
  cstar := StarInfo(ccomponent);
  starindex := sys.GetCompC;
  cstarl := nil;
  cstarn := nil;
end;
//------------------------------------------------------------
procedure StarProxy.AddBrownDwarf;
begin
  ccomponent := sys.AddBrownDwarf;
  starindex := sys.GetCompC;
  cstar := nil;
  cstarl := nil;
  cstarn := nil;
end;
//--------------------------------------------------------
procedure StarProxy.InsertStar;
begin
  if sys = nil then Exit;
  if starindex = 0 then Exit;
  if (not sys.InsertStarAtIndex(starindex,cstar)) then Exit;
  ccomponent := cstar;
  cstarl := nil;
  cstarn := nil;
end;
//--------------------------------------------------------
function StarProxy.SwapComponent():Boolean;
begin
  // reject conditions
  Result := False;
  if sys = nil then Exit;
  if starindex <= 0 then Exit;
  if starindex >= sys.GetCompC then Exit;
  // doing the swap
  sys.SwapPositions(starindex);
  // reloading the components...
  sysl := sys.GetLocation;
  ccomponent := sys.GetNewStar(starindex);
  if ccomponent.isBrownDwarf then cstar := nil
  else cstar := ccomponent as StarInfo;
  cstarl := ccomponent.GetLocation;
  cstarn := ccomponent.GetNames;
  Result := True;
end;
//--------------------------------------------------------
procedure StarProxy.DeleteStar(index:Integer);
begin
  // checks
  Assert(1<>(sys.GetCompC));
  Assert(index>0);
  Assert(index<=(sys.GetCompC));
  // deleting and setting the new index
  sys.DelStar(index);
  SetStar(1);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function StarProxy.StarNameTest:Boolean;
begin
  Result := False;
  if cstarn=nil then Exit;
  ccomponent.TestClearNames;
  cstarn := ccomponent.GetNames;
  Result := True;
end;
//------------------------------------------------------------
function StarProxy.LocateConst:Integer;
var rapos,decpos:Real;
begin
  rapos := sysl.GetDecimalRightAscension;
  decpos := sysl.GetDecimalDeclination;
  Result := Const_Data.LocatePoint(rapos,decpos,sysl.Epoch);
end;
//--------------------------------------------
(* Method for finding a TGAS match for the current position, and perhaps updating
the star location to match the TGAS data if the conditions are right *)
function StarProxy.ShowTGASMatches(byname:Boolean):Boolean;
var msg,rmsg:string;      twostarmr:Integer;
    starlist:TGASList;
    starcount:Integer;
    rval:Word;
    sok:Boolean;
begin
  Result := False;
  if sysl = nil then ShowMessage('No Star System!')
  else if tgas_main.StarCount = 0 then ShowMessage('TGAS Data is Empty!')
  else begin

    // basic matching, list getting, etc
    starlist := FindTGASMatches(byname);
    if starlist = nil then starcount := 0
    else starcount := starlist.Count;
    msg := FoundListToString(starlist);
    (* To simplify the the interface, I will currently support only one result,
    updates. *)
    if (starcount = 1) then begin
       rmsg := msg + sLineBreak;
       rmsg := rmsg + 'Do you want to use this value?';
       // asking if we want to use the TGAS location
       rval := mrNo;
       rval := MessageDlg(rmsg, mtConfirmation,[mbYes, mbNo],0);
       if (rval = mrYes) then begin
         ApplyTGASChange(starlist[0]);
         starlist.Free;
         if ccomponent.HasLocation then cstarl := ccomponent.GetLocation;
         Result := True;
       end;
    end
    (* special routines used for 2 star systems where we have an appropriate
    match for both stars *)
    else if (starcount = 2) then begin
      twostarmr := sys.TGAS2StarCheck(starlist);
      if twostarmr <> 0 then begin
        sok := (twostarmr = 1);
        rmsg := msg + sLineBreak;
        if sok then rmsg += 'Matching the first result to the first star.'
        else rmsg += 'Matching the second result to the first star.';
        rmsg += sLineBreak + 'Do you want to use these two values?';
        // asking if we want to use the TGAS locations
        rval := mrNo;
        rval := MessageDlg(rmsg, mtConfirmation,[mbYes, mbNo],0);
        if (rval = mrYes) then begin
          sok := sys.ApplyDualTGAS(starlist,sok);
          Assert(sok);
          starlist.Free;
          if ccomponent.HasLocation then cstarl := ccomponent.GetLocation;
          Result := True;
        end;
      end
      else begin
        starlist.Free;
        ShowMessage(msg);
      end;
    end
    else begin
      FreeAndNil(starlist);
      ShowMessage(msg);
    end;
  end;
end;
//----------------------------------------------------------
function StarProxy.TGASStarMatch:Boolean;
var starlist:TGASList;
    starcount:Integer;
    msg,rmsg:string;
    rval:Word;
begin
  // search for TGAS inapplicable...
  Result := False;
  if tgas_main.StarCount = 0 then begin
    ShowMessage('TGAS Data is Empty!');
    Exit;
  end;
  if (starindex <= 1) or (cstarl = nil) then begin
    ShowMessage('This procedure is only for non-primary' + sLineBreak +
         'stars in widely separated binaries!');
    Exit;
  end;
  // basic matching, list getting, etc
  starlist := TGASMatchToCurrentStar;
  starcount := starlist.Count;
  msg := FoundListToString(starlist);
  // applying for a single star match
  if (starcount = 1) then begin
    rmsg := msg + sLineBreak;
    rmsg := rmsg + 'Do you want to use this value?';
    // asking if we want to use the TGAS location
    rval := mrNo;
    rval := MessageDlg(rmsg, mtConfirmation,[mbYes, mbNo],0);
    if (rval = mrYes) then begin
      cstarl.SetFromTGAS(starlist[0]);
      starlist[0].matched := True;
      starlist.Free;
      Result := True;
    end;
  end
  // nothing found or multiple found... do nothing (other than show a message)
  else begin
    starlist.Free;
    ShowMessage(msg);
  end;
end;
//----------------------------------------------------------
function StarProxy.GaiaDR2ShowMatch(maxdist:Single):Boolean;
var cloc:Location;
    name_star,name_sys:StarNames;
    rok,ismulti:Boolean;
    outdata:GaiaList;
    mtype:GaiaDR2_MatchType;
    topstr:String;
    mparams:DR2MatchConditions;
begin
  Result := False;
  if DR2Data = nil then Exit;
  if (maxdist >= 3.75) or (maxdist <= 0) then Exit;
  if (sys = nil) then Exit;
  // getting current star data
  rok := sys.GetStuffForMatching(starindex,cloc,name_star,name_sys);
  if not rok then Exit;
  if (cloc = nil) and (name_star = nil) and (name_sys = nil) then Exit;
  // using the star data to find matches...
  mparams.max_search_dist := maxdist;
  mparams.skip_matched := False;
  mparams.skip_reject := False;
  ismulti := (sys.GetCompC > 1);
  outdata := DR2Data.FindMatches(ismulti,name_star,name_sys,cloc,mparams,mtype);
  Result := True;
  // checking the result...
  if (outdata = nil) then ShowMessage('No matches found!')
  else if (outdata.Count = 0) then ShowMessage('No Matches Found!')
  else begin
    // top line
    if mtype = G2_STARNAME then topstr := 'Name Match to Star'
    else if mtype = G2_SYSNAME then topstr := 'Name Match to System'
    else if mtype = G2_NONE then topstr := 'No Matches'
    else topstr := 'Location Matches';
    topstr += ' | ' + IntToStr(outdata.Count) + 'matches.' + sLineBreak;
    // the data for star 0
    topstr += sLineBreak;
    topstr += outdata[0].MakeSummaryString();
    // if there is a star 1, we display it as well
    if outdata.Count > 1 then begin
       topstr += sLineBreak + sLineBreak;
       topstr += outdata[1].MakeSummaryString();
    end;
    // finishing
    ClearGaiaList(outdata);
    outdata.Free;
    ShowMessage(topstr);
  end;
end;
//----------------------------------------------------------
function StarProxy.SimbadDownload(simbadurl:string; fluxonly:Boolean):Boolean;
var dataparsed:SimbadData;    simbadstring:string;
    multistar:Boolean;        rval:Word;
    simbadok:Boolean;         fetchfail:Boolean;
const mnmsg = 'Due to multiple stars, names and catalog IDs will be placed in the notes';
begin
  Result := False;
  Assert(sys <> nil);
  Assert(Length(simbadurl)<>0);
  Screen.Cursor := crHourglass;
  dataparsed := GetSimbadDataURL(simbadurl,fetchfail);
  if dataparsed = nil then begin
    Screen.Cursor := crDefault;
    if fetchfail then ShowMessage('Unable to download the page!')
    else ShowMessage('Unable to parse the resulting page!');
  end
  else begin
    simbadstring := dataparsed.MakeSummaryString(fluxonly);
    multistar :=  (sys.GetCompC > 1);
    simbadstring := simbadstring + sLineBreak;
    simbadstring += 'Do you want to use this data?';
    if multistar then simbadstring += sLineBreak + mnmsg;
    Screen.Cursor := crDefault;
    rval := MessageDlg(simbadstring, mtConfirmation,[mbYes, mbNo],0);
    if rval = mrYes then begin
      Screen.Cursor := crHourGlass;
      simbadok := sys.AddSimbadData(starindex,dataparsed,fluxonly);
      Screen.Cursor := crDefault;
      if not simbadok then ShowMessage('Unable to add Simbad Data.');
      Result := simbadok;
    end;
    FreeAndNil(dataparsed);
  end;
end;
//------------------------------------------------
function StarProxy.SimbadDownloadLogg(simbadurl:string):Boolean;
var dataparsed:SimbadData;    simbadstring:string;
    fetchfail,simbadok:Boolean;
    rval:Word;
const mc:Currency = -9;
begin
  Result := False;
  Assert(sys <> nil);
  Assert(Length(simbadurl)<>0);
  Screen.Cursor := crHourglass;
  dataparsed := GetSimbadDataURL(simbadurl,fetchfail);
  if dataparsed = nil then begin
    Screen.Cursor := crDefault;
    if fetchfail then ShowMessage('Unable to download the page!')
    else ShowMessage('Unable to parse the resulting page!');
  end
  else if dataparsed.logg < mc then begin
    FreeAndNil(dataparsed);
    Screen.Cursor := crDefault;
    ShowMessage('No Log g information found!');
  end
  else begin
    simbadstring := dataparsed.MakeSummaryLoggString + sLineBreak;
    simbadstring := simbadstring + sLineBreak;
    simbadstring += 'Do you want to use this data?';
    Screen.Cursor := crDefault;
    rval := MessageDlg(simbadstring, mtConfirmation,[mbYes, mbNo],0);
    if rval = mrYes then begin
      Screen.Cursor := crHourGlass;
      simbadok := sys.SetLoggFromSimbad(starindex,dataparsed);
      Screen.Cursor := crDefault;
      if not simbadok then ShowMessage('Unable to add Log g from Simbad Data.');
      Result := simbadok;
    end;
    FreeAndNil(dataparsed);
  end;
end;
//------------------------------------------------
function StarProxy.SimbadCatalogs(simbadurl:string):Boolean;
var dataparsed:SimbadData;    simbadstring:string;
    rval:Word;                fetchfail:Boolean;
begin
  Result := False;
  Assert(sys <> nil);
  Assert(Length(simbadurl)<>0);
  dataparsed := GetSimbadDataURL(simbadurl,fetchfail);
  if dataparsed = nil then begin
    Screen.Cursor := crDefault;
    if fetchfail then ShowMessage('Unable to download the page!')
    else ShowMessage('Unable to parse the resulting page!');
  end
  else begin
    simbadstring := dataparsed.MakeSummaryString(False) + sLineBreak;
    simbadstring += 'Do you want to copy Catalog IDs to the notes?';
    Screen.Cursor := crDefault;
    rval := MessageDlg(simbadstring, mtConfirmation,[mbYes, mbNo],0);
    if rval = mrYes then begin
      current.sys.AppndNote(sLineBreak + dataparsed.Names,True);
      current.sys.AppndNote(sLineBreak + dataparsed.OutputCatalogCVS,False);
      Result := True;
    end;
  end;
end;
//------------------------------------------------------
function StarProxy.CurrentToBrownDwarf:Boolean;
begin
  Result := False;
  if sys = nil then Exit;
  if (not sys.ConvertToBrownDwarf(starindex-1)) then Exit;
  // the star has been replaced, the new object has eqiv namees and location,
  // but is new.
  ccomponent := sys.GetNewStar(starindex);
  cstar := nil;
  cstarl := ccomponent.GetLocation;
  cstarn := ccomponent.GetNames;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
// location via separation and position angle
function StarProxy.PosAngLocationSet:Boolean;
var rok:Boolean;
    data,errmsg,resdata:string;
    crho,ctheta:Real;
    newlocat:Location;
    rval:Word;
const msg1 = 'Enter the separation ρ (in arcseconds) and' + sLineBreak + 'the position angle θ (in degrees) below :';
begin
  Result := False;
  // this should be un-necessary, but I will check anyways
  if sys = nil then Exit;
  if starindex < 2 then Exit;
  if ccomponent = nil then Exit;
  if sysl = nil then Exit;
  // showing the message...
  data := Trim(InputBox('Set Secondary Location',msg1,''));
  if Length(data)<>0 then begin
    rok := SeparationCheck(sysl,data,errmsg,crho,ctheta);
    if not rok then begin
       ShowMessage('Could not calculate location : ' + errmsg);
       Exit;
    end;
    // entry is okay, we go head and create the new location...
    newlocat := Location.Create(sysl,crho,ctheta);
    resdata := 'The computeted position is : ' + sLineBreak + 'RA: ';
    resdata += newlocat.RightAscensionHMS + '  Dec: ' + newlocat.DeclinationDMS;
    resdata += sLineBreak + 'Do you want to use this location?';
    // asking if we use this location
    rval := mrNo;
    rval := MessageDlg(resdata, mtConfirmation,[mbYes, mbNo],0);
    // if we do, go ahead and replace
    if rval = mrYes then begin
      cstarl := newlocat;
      ccomponent.InsertLocation(newlocat);
      Result := True;
    end;
  end;
end;
//----------------------------------------------------
// more location stuff
//-----------------------
// returns 2 if the parallax count is 2 or more
function StarProxy.OldParallaxCount():Integer;
begin
  if sysl = nil then Result := 0
  else if Length(sysl.oldparallax) = 0 then Result := 0
  else if AnsiContainsStr(sysl.oldparallax,',') then Result := 2
  else Result := 1;
end;
//-----------------------
// swapping parallax gui helper
function StarProxy.OldParallaxSwap():Boolean;
var ocount:Integer;
    rval:Word;
begin
  Result := False;
  ocount := OldParallaxCount();
  if ocount = 0 then Exit;
  // two different pop-ups depending on count
  if ocount = 1 then begin
    rval := mrYes;
    rval := MessageDlg('Do you want to swap the current parallax with the old one?', mtConfirmation,[mbYes, mbNo],0);
    if rval = mrYes then begin
      Result := sysl.SwapParallax(False);
      if (not Result) then ShowMessage('Error while trying to swap Parallax!');
    end;
  end
  else begin
    rval := mrCancel;
    rval := QuestionDlg('Pick the parallax','Which old parallax do you want to swap with the old one?',
                      mtCustom,[100,'First',200,'Last',mrCancel],'');
    if rval = mrCancel then Exit;
    Result := sysl.SwapParallax(rval = 200);
    if (not Result) then ShowMessage('Error while trying to swap Parallax!');
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++
// even more methods
function StarProxy.GuessSpectra(out usespec:Boolean):Boolean;
var styper:SpecTypeGuesser;
    resok,vmdiffok,hasrec:Boolean;
    cpllx,avpllx,expmag:Real;
    cppout,therec,dispmsg:string;
    rval:Word;
begin
  // basic
  Result := False;
  vmdiffok := False;
  if cstar = nil then Exit;
  // initialization of the guesser
  Screen.Cursor := crHourGlass;
  styper := SpecTypeGuesser.Create;
  cpllx := GetCurrentParallax;
  resok := styper.SetFluxes(cpllx,cstar.VisualMagnitude,cstar.fluxtemp,cstar.dr2mags);
  if not resok then begin
    FreeAndNil(styper);
    Screen.Cursor := crDefault;
    Exit;
  end;
  // Trying to make the results
  if not styper.MakeGuesses(cstar.SpectralClass,cstar.premain) then begin
    FreeAndNil(styper);
    Screen.Cursor := crDefault;
    Exit;
  end;
  (* if there is a valid spectral type, we might try to calculate magnitude
  difference and a very crude photometric parallax. *)
  if (cstar.estimator_i<>nil) and (cstar.VisualMagnitude<90) then begin
    // calculating expected vs actual magnitude
    vmdiffok := LookupExpectedAbsVMag(cstar.estimator_i.ColorLetter,cstar.estimator_i.ColorSubrange,expmag);
    if vmdiffok then expmag := cstar.estimator_i.AbsoluteVisMagnitude - expmag
    else expmag := 99;
    // crude photometric parallax
    resok := PhotoParallax(cstar.SpectralClass,cstar.VisualMagnitude,cppout);
  end else expmag := 99;
  // getting the output
  dispmsg := styper.ProduceResult(cppout,expmag);
  hasrec := styper.HasRecommendation;
  therec := styper.Recommendation;
  Result := True;
  // two options...
  if hasrec then begin
    dispmsg += sLineBreak + sLineBreak;
    dispmsg += 'Do you want to use this Spectral Type?';
    rval := mrNo;
    Screen.Cursor := crDefault;
    rval := MessageDlg(dispmsg, mtConfirmation,[mbYes, mbNo],0);
    if (rval = mrYes) then begin
      avpllx := sys.GetAvgPllx();
      cstar.SpectralClass := therec;
      cstar.InitializeEstimation(cpllx,avpllx);
      usespec := True;
    end else usespec := False;
  end else begin
    Screen.Cursor := crDefault;
    ShowMessage(Trim(dispmsg));
    usespec := False;
  end;
end;
//--------------------------------------------------------
function StarProxy.APASS_Helper(indata:string):Boolean;
var out_B,out_Be:Currency;
    out_V,out_Ve:Real;
    out_Rc,out_Ic:Currency;
    Jx:Currency;
    msgdata:string;
    rval:Word;
const amsg = 'BVRI converted/used from APASS.';
begin
  Result := False;
  Assert(cstar<>nil);
  if cstar.fluxtemp = nil then Jx := 99.999
  else Jx := cstar.fluxtemp.J_mag;
  if not sAPASS_to_BVRI(indata,Jx,out_V,out_Ve,out_B,out_Be,out_Rc,out_Ic) then begin
    ShowMessage('Unable to parse or convert the input!');
    Exit;
  end;
  msgdata := 'The values are :' + sLineBreak;
  // B and V
  msgdata += 'V : ' + FloatToStrF(out_V,ffFixed,2,3) + '±';
  msgdata += FloatToStrF(out_Ve,ffFixed,2,3) + sLineBreak;
  msgdata += 'B : ' + CurrToStrF(out_B,ffFixed,3) + '±';
  msgdata += CurrToStrF(out_Be,ffFixed,3) + sLineBreak;
  // Rc and Ic
  msgdata += 'Rc estimate : ' + CurrToStrF(out_Rc,ffFixed,3);
  msgdata += sLineBreak;
  msgdata += 'Ic estimate : ' + CurrToStrF(out_Ic,ffFixed,3);
  msgdata += sLineBreak;
  // final touches
  msgdata += 'Do you want to use these magnitudes?';
  // asking
  // asking if we want to use the APASS magnitudes
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    if cstar.fluxtemp = nil then cstar.fluxtemp := StarFluxPlus.Create;
    // setting B
    cstar.fluxtemp.blue_mag := out_B;
    cstar.fluxtemp.blue_err := out_Be;
    // setting V
    cstar.SetVisualMagnitude(out_V);
    sys.UpdateEstimates;
    // Rc and Ic
    cstar.fluxtemp.red_mag := out_Rc;
    cstar.fluxtemp.I_mag := out_Ic;
    // adding some notes
    if not cstar.NotesConatins(amsg) then begin
      cstar.AppndNote(' ' + amsg,False);
    end;
    Result := True;
  end;
end;
//--------------------------------------------------------
function StarProxy.SDSS_Helper(indata:string):Boolean;
var out_V:Real;
    out_B,out_Rc,out_Ic:Currency;
    msgdata:string;
    rval:Word;
const amsg = 'BVRI converted from SDSS magnitudes.';
begin
  Result := False;
  Assert(cstar<>nil);
  if not SDSS_to_Fluxes(indata,out_B,out_V,out_Rc,out_Ic) then begin
    ShowMessage('Unable to parse or convert the input!');
    Exit;
  end;
  // building the message to show
  msgdata := 'The values are :' + sLineBreak;
  // V and B
  msgdata += 'V estimate : ' + FloatToStrF(out_V,ffFixed,2,3);
  msgdata += sLineBreak;
  msgdata += 'B estimate : ' + CurrToStrF(out_B,ffFixed,3);
  msgdata += sLineBreak;
  // Rc and Ic
  msgdata += 'Rc estimate : ' + CurrToStrF(out_Rc,ffFixed,3);
  msgdata += sLineBreak;
  msgdata += 'Ic estimate : ' + CurrToStrF(out_Ic,ffFixed,3);
  msgdata += sLineBreak;
  // final touches
  msgdata += 'Do you want to use these magnitudes?';
  // asking
  // asking if we want to use the SDSS derived values
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    if cstar.fluxtemp = nil then cstar.fluxtemp := StarFluxPlus.Create;
    // setting B
    cstar.fluxtemp.blue_mag := out_B;
    cstar.fluxtemp.blue_err := 0.0;
    // setting V
    cstar.SetVisualMagnitude(out_V);
    // Rc and Ic
    cstar.fluxtemp.red_mag := out_Rc;
    cstar.fluxtemp.I_mag := out_Ic;
    // adding some notes
    if not cstar.NotesConatins(amsg) then begin
      cstar.AppndNote(' ' + amsg,False);
    end;
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//----------------------------------------------------------
function StarProxy.Tycho2_Helper(indata:string):Boolean;
var out_V:Real;
    out_B:Currency;
    msgdata:string;
    rval:Word;
const amsg = 'BV converted from Tycho-2.';
begin
  Result := False;
  Assert(cstar<>nil);
  if not TychoToFluxes(indata,out_V, out_B) then begin
    ShowMessage('Unable to parse or convert the input!');
    Exit;
  end;
  // building the message to show
  msgdata := 'The values are :' + sLineBreak;
  // V and B
  msgdata += 'V estimate : ' + FloatToStrF(out_V,ffFixed,2,3);
  msgdata += sLineBreak;
  msgdata += 'B estimate : ' + CurrToStrF(out_B,ffFixed,3);
  msgdata += sLineBreak;
  // final touches
  msgdata += 'Do you want to use these magnitudes?';
  // asking
  // asking if we want to use the Tycho-2 derived values
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    if cstar.fluxtemp = nil then cstar.fluxtemp := StarFluxPlus.Create;
    // setting B
    cstar.fluxtemp.blue_mag := out_B;
    cstar.fluxtemp.blue_err := 0.0;
    // setting V
    cstar.SetVisualMagnitude(out_V);
    // adding some notes
    if not cstar.NotesConatins(amsg) then begin
      cstar.AppndNote(' ' + amsg,False);
    end;
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//-------------------------------------------------------
function StarProxy.VizierAPASSGet(simbadalso:Boolean):Boolean;
var gname:string;
    fok,boxo,boxo1,boxo2:Boolean;
    downd:APASSVizieRData;
    downsim:SimbadData;
    outdex,outmax:Integer;
    outmsg,buffer,simburl:string;
    Jx:Currency;
    rval:Word;
    vmago:Real;
    fetchfail:Boolean;
begin
  Result := False;
  gname := '';
  fok := (sys<>nil);
  if fok then begin
    if sys.GetCompC = 1 then begin
      if sysn <> nil then gname := sysn.GetDefaultCatalog;
    end else begin
      if cstarn <> nil then gname := cstarn.GetDefaultCatalog;
    end;
    fok := (Length(gname)<>0);
    if fok then begin
      // finally, we can truly start here!
      Screen.Cursor := crHourGlass;
      if ccomponent.fluxtemp = nil then Jx := 99.999
      else Jx := ccomponent.fluxtemp.J_mag;
      if simbadalso then begin
        simburl := MakeSimbadIdLookupURL(gname);
        downsim := GetSimbadDataURL(simburl,fetchfail);
      end else downsim := nil;
      downd := GetFromVizier(gname,Jx);
      if downd = nil then begin
        Screen.Cursor := crDefault;
        ShowMessage('Unable to find valid APASS results!')
      end else begin
        // building the data message to display
        outmax := downd.APASS_Count-1;
        outmsg := 'Found ' + IntToStr(outmax+1) + ' valid APASS result';
        if outmax > 0 then outmsg += 's';
        outmsg += ':' + sLineBreak;
        for outdex := 0 to outmax do begin
          downd.GetAPASS_String(outdex,buffer);
          outmsg += buffer + sLineBreak;
        end;
        if downsim <> nil then begin
          outmsg += 'Simbad ' + downsim.VBRIString() +sLineBreak;
          FreeAndNil(downsim);
        end;
        outmsg += ' Do you want to use valid values from the first result?';
        // showing that message
        Screen.Cursor := crDefault;
        rval := mrNo;
        rval := MessageDlg(outmsg, mtConfirmation,[mbYes, mbNo],0);
        // if we are told to go ahead...
        if (rval = mrYes) then begin
          // B, Rc, and Ic
          if ccomponent.fluxtemp = nil then begin
            fok := True;
            ccomponent.fluxtemp := StarFluxPlus.Create;
          end;
          boxo := downd.GetAPASS_B(0,ccomponent.fluxtemp.blue_mag,ccomponent.fluxtemp.blue_err);
          boxo1 := downd.GetAPASS_Rc(0,ccomponent.fluxtemp.red_mag);
          boxo2 := downd.GetAPASS_Ic(0,ccomponent.fluxtemp.I_mag);
          boxo := (boxo or boxo1 or boxo2);
          // perhaps we have none of these values!
          if (not boxo) and fok then FreeAndNil(ccomponent.fluxtemp);
          // v magnitude (if there)
          if cstar <> nil then begin
            boxo := downd.GetAPASS_V(0,vmago);
            if boxo then cstar.SetVisualMagnitude(vmago);
          end;
          Result := True;
        end;
        FreeAndNil(downd);
      end;
    end;
  end;
  if not fok then ShowMessage('Unable to get lookup catalog id!');
end;
//----------------------------------------------------------
function StarProxy.Vizier2MASSGet():Boolean;
var gname:string;
    fok:Boolean;
    downd:VizieR2MASSData;
    outmsg:string;
    rval:Word;
begin
  Result := False;
  gname := '';
  fok := (sys<>nil);
  if fok then begin
    if sys.GetCompC = 1 then begin
      if sysn <> nil then sysn.GetCatValue('2MASS',gname);
    end else begin
      if cstarn <> nil then cstarn.GetCatValue('2MASS',gname);
    end;
    fok := (Length(gname)<>0);
    if fok then begin
      // finally, we can truly start here!
      Screen.Cursor := crHourGlass;
      downd := Get2MASSFromVizier(gname);
      if (downd = nil) or (downd.AllBad()) then begin
        Screen.Cursor := crDefault;
        ShowMessage('Unable to find valid 2MASS results!');
        if downd <> nil then downd.Free;
      end else begin
        // building the data message to display
        outmsg := 'Found 2MASS Results: ' + sLineBreak;
        outmsg += downd.ToString() + sLineBreak;
        outmsg += ' Do you want to use these values?';
        // showing that message
        Screen.Cursor := crDefault;
        rval := mrNo;
        rval := MessageDlg(outmsg, mtConfirmation,[mbYes, mbNo],0);
        // if we are told to go ahead...
        if (rval = mrYes) then begin
          // this is in the fluxtemp object
          if ccomponent.fluxtemp = nil then begin
            fok := True;
            ccomponent.fluxtemp := StarFluxPlus.Create;
          end;
          if not downd.jbad then ccomponent.fluxtemp.J_mag := downd.J;
          if not downd.hbad then ccomponent.fluxtemp.H_mag := downd.H;
          if not downd.kbad then begin
             ccomponent.fluxtemp.K_mag := downd.Ks;
             ccomponent.fluxtemp.K_err := downd.Kserr;
          end;
          Result := True;
        end;
        FreeAndNil(downd);
      end;
    end;
  end;
  if not fok then ShowMessage('Object does not have 2MASS ID!');
end;
//----------------------------------------------------------
function StarProxy.UCAC4_ToVRI_Helper(indata:string):Boolean;
var params:RealArray;
    RcEst,IcEst:Currency;
    Vest:Real;
    qmsg:string;
const amsg = 'estimated from UCAC4+J';
begin
  Result := False;
  if (cstar.fluxtemp = nil) then begin
    ShowMessage('J mag missing');
    Exit;
  end;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // preparing the info message
    qmsg := amsg;
    // calling the transform function
    if not UCAC_to_VRI(params[0],cstar.fluxtemp.J_mag,Vest,RcEst,IcEst) then begin
      ShowMessage('Cannot estimate, Color out of range!');
    end else Result := ShowEst(Vest,99.999,RcEst,IcEst,qmsg)
  end
  else ShowMessage('Unable to parse the input');
end;
//--------------------------------------------
function StarProxy.CMC_ToVRI(indata:string):Boolean;
var params:RealArray;
    Vest:Real;
    RcEst,IcEst:Currency;
const amsg = 'estimated from CMC r’ and 2MASS J.';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // calling the transform function
    if not CMC15_to_VRI(params[0],cstar.fluxtemp.J_mag,Vest,RcEst,IcEst) then begin
         ShowMessage('Cannot estimate, Color out of range!');
       end else Result := ShowEst(Vest,99,RcEst,IcEst,amsg);
  end
  else ShowMessage('Unable to parse the input');
end;
//-------------------------------------------
function StarProxy.BPRP_To_VRI():Boolean;
var okay,isWD:Boolean;
    Gin,BPin,RPin,Jin:Currency;
    Best,Rcest,Icest:Currency;
    Vest:Real;
const amsg = 'estimated from Gaia DR2 magnitudes.';
begin
  Result := False;
  Best := 99;
  // basic bad exit cases...
  if ccomponent = nil then begin
    ShowMessage('No Star to estimate!');
    Exit;
  end;
  okay := ccomponent.dr2mags <> nil;
  if okay then okay := (ccomponent.dr2mags.G < 90);
  if not okay then begin
     ShowMessage('Object does not have enough Gaia Magnitudes!');
     Exit;
  end;
  BPin := ccomponent.dr2mags.BP;
  RPin := ccomponent.dr2mags.RP;
  Gin := ccomponent.dr2mags.G;
  // special white dwarf specific method
  isWD := AnsiStartsStr('D',ccomponent.SpectralClass);
  if isWD then begin
    okay := WD_GaiaToVRI_Gen(ccomponent.dr2mags,Vest,Rcest,Icest);
  end
  else begin
    // we have different methods depending on values...
    if ccomponent.fluxtemp = nil then Jin := 99.999
    else Jin := ccomponent.fluxtemp.J_mag;
    okay := GaiaToVRI_Gen(ccomponent.dr2mags,Jin,Vest,Rcest,Icest);
  end;
  if not okay then begin
    ShowMessage('Cannot estimate: Colors/Errors not within range!');
    Exit;
  end;
  // also, B
  if not Gaia2ToB(Gin,BPin,RPin,Best) then Best := 99;
  Result := ShowEst(Vest,Best,Rcest,Icest,amsg);
end;
//--------------------------------------------------------
function StarProxy.GaiaDR2_To_JHK():Boolean;
var okay,isWD:Boolean;
    Jest,Hest,Ksest:Currency;
    msgdata:string;
const amsg = 'estimated from Gaia DR2 magnitudes.';
begin
  Result := False;
  // basic bad exit cases...
  if ccomponent = nil then begin
    ShowMessage('No Star to estimate!');
    Exit;
  end;
  okay := ccomponent.dr2mags <> nil;
  if okay then okay := ccomponent.dr2mags.ValidBPmRP;
  if okay then okay := (ccomponent.dr2mags.G < 90);
  if not okay then begin
     ShowMessage('Object does not have all Gaia Magnitudes!');
     Exit;
  end;
  // getting the magnitudes and the values...
  isWD := AnsiStartsStr('D',ccomponent.SpectralClass);
  if not GaiaTo_JHK_Wr(ccomponent.dr2mags,isWD,Jest,Hest,Ksest) then begin
    ShowMessage('Cannot estimate: Colors not within range!');
    Exit;
  end;
  Result := ShowEstJHK(Jest,Hest,Ksest,amsg);
end;
//----------------------------------------------------------------
function StarProxy.PanStarrs_To_BVRI(indata:string):Boolean;
var params:RealArray;
    Best,Icest,Rcest:Currency;
    Bx,Ix,Rx,Vest:Real;
const amsg = 'estimated from Pan-STARRS gri';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,3,params) then begin
    // calling the transform function
    if not PanSTARRSgri_to_BVRI(params[0],params[1],params[2],Bx,Vest,Rx,Ix) then begin
      ShowMessage('Cannot estimate, Colors out of range!');
    end
    else begin
      // converting to decimal
      Best := RoundCurrency(RealToCurr(Bx),False);
      Rcest := RoundCurrency(RealToCurr(Rx),False);
      Icest := RoundCurrency(RealToCurr(Ix),False);
      // showing the message...
      Result := ShowEst(Vest,Best,Rcest,Icest,amsg);
    end;
  end
  else ShowMessage('Input not in expected format!');
end;
//----------------------------------------------------------
function StarProxy.VizierGaiaGet():Boolean;
var gname:string;
    fok:Boolean;
    downd:VizieRGaiaData;
    outmsg:string;
    rval:Word;
    nloc:Location;
begin
  Result := False;
  gname := '';
  fok := (sys<>nil);
  if fok then begin
    if sys.GetCompC = 1 then begin
      if sysn <> nil then sysn.GetCatValue('Gaia DR2',gname);
    end else begin
      if cstarn <> nil then cstarn.GetCatValue('Gaia DR2',gname);
    end;
    fok := (Length(gname)<>0);
    if fok then begin
      // finally, we can truly start here!
      Screen.Cursor := crHourGlass;
      downd := GetGaiaFromVizier(gname);
      if (downd = nil) then begin
        Screen.Cursor := crDefault;
        ShowMessage('Unable to find valid Gaia DR2 results!');
        if downd <> nil then downd.Free;
      end else begin
        // building the data message to display
        outmsg := 'Found Gaia DR2 Results: ' + sLineBreak;
        outmsg += downd.ToString() + sLineBreak;
        outmsg += ' Do you want to use these values?';
        Screen.Cursor := crDefault;
        // showing that message
        rval := mrNo;
        if (downd.HasParallax) then begin
          rval := QuestionDlg('Choose the Data',outmsg, mtCustom,
                         [110,'Mags',120,'Mags and Pos',130,'Mags,Pos,Pllx',mrNo],'');
        end else begin
            rval := QuestionDlg('Choose the Data',outmsg, mtCustom,
                         [110,'Mags',120,'Mags and Pos',mrNo],'');
        end;

        // using data
        if rval <> mrNo then begin
          ccomponent.dr2mags := downd.mags;
          downd.mags := nil;
          // setting the position
          if (rval = 120) or (rval = 130) then begin
            if (cstarl <> nil) then cstarl.SetPositionDDeg(zJ2015h,downd.ra_pos,downd.dec_pos)
            else if (starindex = 1) then sysl.SetPositionDDeg(zJ2015h,downd.ra_pos,downd.dec_pos)
            else begin
              nloc := Location.Create(sysl,True);
              nloc.SetPositionDDeg(zJ2015h,downd.ra_pos,downd.dec_pos);
              ccomponent.InsertLocation(nloc);
              cstarl := nloc;
            end;
          end;
          // setting the parallax
          if (rval = 130) then begin
            if (cstarl <> nil) then nloc := cstarl
            else nloc := sysl;
            nloc.UpdateParallax(downd.pllx,downd.pllx_err);
            nloc.source := GAIA2_TAG;
            nloc.binarycopy := False;
            if (not nloc.uncertain) then nloc.uncertain := (downd.pllx_err > 0.2);
          end;
          Result := True;
        end;
        FreeAndNil(downd);
      end;
    end;
  end;
  if not fok then ShowMessage('Object does not have Gaia DR2!');
end;
//---------------------------------------------------------
function StarProxy.GuessVFromG():Boolean;
var Gin:Currency;
    pllx,vmg,vcalc:Real;
    msg:string;
    rval:Word;
    okay:Boolean;
begin
  Result := False;
  // basic bad exit cases...
  if cstar = nil then begin
    ShowMessage('No Star to estimate!');
    Exit;
  end;
  okay := ccomponent.dr2mags <> nil;
  if okay then okay := (ccomponent.dr2mags.G < 90);
  if not okay then begin
     ShowMessage('Star does not have Gaia G');
     Exit;
  end;
  // getting the magnitudes and the values...
  Gin := ccomponent.dr2mags.G;
  pllx := GetCurrentParallax();
  if not EstVmG(Gin,pllx,vmg) then begin
    ShowMessage('Cannot estimate: G not within range!');
    Exit;
  end;
  vcalc := vmg + CurrToReal(Gin);
  // asking to use them
  msg := 'V guess is: ' + Trim(FloatToStrF(vcalc,ffFixed,6,3)) + sLineBreak;
  msg += 'Do you want to use this value?';

  rval := mrNo;
  rval := MessageDlg(msg, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    cstar.SetVisualMagnitude(vcalc);
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//----------------------------------------------------------
function StarProxy.PanStarrsJHK(indata:string):Boolean;
var params:RealArray;
    Jest,Hest,Kest:Currency;
    Gx:Currency;
const amsg = 'estimated from Pan-STARRS.';
begin
  Result := False;
  // basic bad exit cases...
  if ccomponent = nil then begin
    ShowMessage('Nothing to estimate!');
    Exit;
  end;
  if not PS1_StrToVals(indata,params) then begin
    ShowMessage('Cannot estimate, magnitudes missing or too bright.');
    Exit;
  end;
  if ccomponent.dr2mags = nil then Gx := 99.999
    else Gx := ccomponent.dr2mags.G;
  if not PS1_iyToJHK(params,Gx,Jest,Hest,Kest) then begin
    ShowMessage('Cannot estimate, Colors out of range!');
    Exit;
  end;
  Result := ShowEstJHK(Jest,Hest,Kest,amsg);
end;
//--------------------------------------------------
function StarProxy.GG1_VRI(useRP:Boolean):Boolean;
var okay,iswd:Boolean;
    G,G1,RP:Currency;
    Vest:Real;
    RcEst,IcEst,BcEst:Currency;
    qmsg:string;
const amsg = 'Estimated used G+G1';
begin
  Result := False;
  // checking if we have the magnitudes
  okay := (ccomponent <> nil) and (ccomponent.dr2mags <> nil);
  okay := okay and (ccomponent.fluxtemp <> nil) and (ccomponent.fluxtemp.gaia_mag < 90);
  G := ccomponent.dr2mags.G;
  okay := okay and (G < 90);
  if useRP then begin
    RP := ccomponent.dr2mags.RP;
    okay := okay and (RP < 90);
  end;
  if not okay then begin
    ShowMessage('Cannot estimate, missing magnitudes.');
    Exit;
  end;
  // calling the methods
  G1 := ccomponent.fluxtemp.gaia_mag;
  iswd := AnsiStartsStr('D',ccomponent.SpectralClass);
  if (not useRP) then begin
    IcEst := 99.999;
    if iswd then okay := Gaia12_BVRIwd(G1,G,Vest,BcEst,RcEst,IcEst)
    else okay := Gaia12_BVRI(G1,G,Vest,BcEst,RcEst,IcEst);
  end
  else okay := Gaia12RP_To_VRI(G1,G,RP,Vest,RcEst,IcEst);
  if not okay then begin
    ShowMessage('Cannot estimate, Colours out of bounds.');
    Exit;
  end;
  // showing the results
  if useRP then qmsg := amsg + '+RP.'
  else qmsg := amsg + '.';
  Result := ShowEst(Vest,BcEst,Rcest,Icest,qmsg);
end;
//-----------------------------------------------------------
function StarProxy.DA_GaiaTEff(which:Word):Boolean;
var gm,g1m,bpm,rpm:Currency;
    okay:Boolean;
    teff_out:Integer;
    msg:string;
    rval:Word;
begin
  Result := False;
  if (which < 1) or (which > 4) then Exit;
  // checking if we have G
  okay := (ccomponent <> nil) and (ccomponent.dr2mags <> nil);
  if okay then begin
    gm := ccomponent.dr2mags.G;
    okay := okay and (gm < 90) and (ccomponent.dr2mags.Gerr < 0.025);
  end;
  if (not okay) then begin
    ShowMessage('Cannot estimate, missing G');
    Exit;
  end;
  // checking the other mags
  if (which = 1) then begin
    okay := ccomponent.dr2mags.ValidBPmRP;
    rpm := ccomponent.dr2mags.RP;
    bpm := ccomponent.dr2mags.BP;
  end else if (which = 2) then begin
    rpm := ccomponent.dr2mags.RP;
    okay := (rpm < 90) and (ccomponent.dr2mags.RPerr < 0.05);
  end else if (which = 3) then begin
    okay := (ccomponent.fluxtemp <> nil);
    if okay then begin
      g1m := ccomponent.fluxtemp.gaia_mag;
      okay := g1m < 90;
    end;
  end else if (which = 4) then begin
    bpm := ccomponent.dr2mags.BP;
    okay := (bpm < 90) and (ccomponent.dr2mags.BPerr < 0.05);
  end;
  if not okay then begin
    ShowMessage('Cannot estimate, missing magnitudes.');
    Exit;
  end;
  // computing TEff estimate

  case which of
    1 : okay := GDA_TEff(gm,bpm,rpm,teff_out);
    2 : okay := GDA_TEffr(gm,rpm,teff_out);
    3 : okay := GDA_TEffx(gm,g1m,teff_out);
    4 : okay := GDA_TEffb(gm,bpm,teff_out);
  end;

  if not okay then begin
    ShowMessage('Cannot estimate, Colours out of bounds.');
    Exit;
  end;
  // showing the TEff and asking if we use it...
  msg := 'TEff guess is: ' + IntToStr(teff_out) + 'K' + sLineBreak;
  msg += 'Do you want to use this value?';
  rval := mrNo;
  rval := MessageDlg(msg, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    if ccomponent.fluxtemp = nil then ccomponent.fluxtemp := StarFluxPlus.Create;
    ccomponent.fluxtemp.EffectiveTemp := teff_out;
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//-------------------------------------------------------------
function StarProxy.Tycho2G_Helper(indata:string):Boolean;
var vtin,g1in:Real;
    RcEst,IcEst:Currency;
    okay:Boolean;
    splitlist:TStringList;
const amsg = 'Estimated using Vt and G/G1.';
begin
  Result := False;

  Assert(cstar<>nil);
  if (cstar.dr2mags = nil) then Exit;
  if (cstar.dr2mags.G > 90) then Exit;
  // getting the values
  splitlist := SplitWithSpaces(indata,1);
  if splitlist = nil then Exit;
  if splitlist.Count > 2 then begin
    FreeAndNil(splitlist);   Exit;
  end;
  // converting to numbers
  if (splitlist.Count = 2) then begin
     if not StrToRealBoth(splitlist[0],splitlist[1],vtin,g1in) then begin
        ShowMessage('Unable to parse or convert the input!');
        FreeAndNil(splitlist);   Exit;
     end;
  end
  else begin
     if not StrToReal(splitlist[0],vtin) then begin
       ShowMessage('Unable to parse or convert the input!');
       FreeAndNil(splitlist);   Exit;
     end;
     if cstar.fluxtemp = nil then g1in := 99.999
     else g1in := cstar.fluxtemp.gaia_mag;
  end;
  FreeAndNil(splitlist);
  // calculating results
  okay := TychoG_toRI(cstar.dr2mags.G,g1in,vtin,RcEst,IcEst);
  if not okay then begin
    ShowMessage('Cannot estimate, Colours out of bounds.');
    Exit;
  end;
  // showing the results
  Result := ShowEst(99.9999,99.9999,RcEst,IcEst,amsg);
end;
//---------------------------------------------------------------------
(* For 'mid-bright' binarys, Vt and G is available, but sometimes Bt is
bad, compute B and V using Vt, G, and sometimes G1 *)
function StarProxy.VtG_Helper(indata:string):Boolean;
var Vest,vtin,g1inx:Real;
    Best,G1in:Currency;
    okay:Boolean;
    splitlist:TStringList;
const amsg = 'Estimated from Tycho Vt and GAIA G';
begin
  Result := False;
  Assert(cstar<>nil);
  if (cstar.dr2mags = nil) then Exit;
  if (cstar.dr2mags.G > 90) then Exit;
  // getting the values
  splitlist := SplitWithSpaces(indata,1);
  if splitlist = nil then Exit;
  if splitlist.Count > 2 then begin
    FreeAndNil(splitlist);   Exit;
  end;
  // converting to numbers
  if (splitlist.Count = 2) then begin
     if not StrToRealBoth(splitlist[0],splitlist[1],vtin,g1inx) then begin
        ShowMessage('Unable to parse or convert the input!');
        FreeAndNil(splitlist);   Exit;
     end;
  end
  else begin
     if not StrToReal(splitlist[0],vtin) then begin
       ShowMessage('Unable to parse or convert the input!');
       FreeAndNil(splitlist);   Exit;
     end;
     g1inx := 99.999;
  end;
  FreeAndNil(splitlist);
  // G1
  if (g1inx <= 90) then G1in := g1inx
  else if cstar.fluxtemp = nil then G1in := 99.999
  else G1in := cstar.fluxtemp.gaia_mag;
  // calculating results
  okay := VtG_to_BV(cstar.dr2mags.G,G1in,vtin,Vest,Best);
  if not okay then begin
    ShowMessage('Cannot estimate, Colours out of bounds.');
    Exit;
  end;
  // showing the results
  Result := ShowEst(Vest,Best,99.999,99.999,amsg);
end;
//---------------------------------------------------------------------
function StarProxy.SMSSHelper(indata:string):Boolean;
var Vest:Real;
    Jin,RcEst,IcEst:Currency;
    okay:Boolean;
const amsg = 'Estimated from Skymapper griz/2MASS J';
begin
  Result := False;
  Assert(cstar<>nil);
  // getting J
  if cstar.fluxtemp = nil then Jin := 99.999
  else Jin := cstar.fluxtemp.J_mag;
  if (not SMSStoFluxes(indata,Jin,Vest,RcEst,IcEst)) then begin
      ShowMessage('Unable to parse or convert the input!');
  end
  else begin
     Result := ShowEst(Vest,99.9999,RcEst,IcEst,amsg);
  end;
end;
//-----------------------------------------------------------------
function StarProxy.BinaryGSplit():Boolean;
begin
  Result := sys.ApplyGMagSplit(starindex);
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  current := nil;
end.

