unit startproxy;

{$mode delphi}

interface

(* Moving StarProxy to a separate unit, to try and reduce the massive size of the
collecdata unit. *)

uses
  Classes, SysUtils, Dialogs, Controls, Forms, StrUtils,
  tgas, stardata, newlocation, namedata, NewStar, constellation, gaiadr2holder,
  simbad, guessstype, sptfluxest, fluxtransform, starext2, importvizier,
  df_strings;

type

(* this class encapuslates easy access to info for the currently edited system and star *)
StarProxy = class
  protected
    function FindTGASMatches(name_search:Boolean):TGASList;
    function TGASMatchToCurrentStar:TGASList;
    function FoundListToString(inlinst:TGASList):string;
    function GetCurrentParallax:Real;
    procedure ApplyTGASChange(newdata:TGASData);
    function ShowBVEst(Vest:Real; Best:Currency; amsg:string):Boolean;
    function ShowRIEst(Rcest,Icest:Currency; amsg:string):Boolean;
  public
    // system level information
    sys:StarSystem;
    sysl:Location;
    sysn:StarName;
    // star level info
    ccomponent:NewStarBase;
    cstar:StarInfo;
    cstarl:Location;
    cstarn:StarName;
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
    function URAT_ToBV(indata:string):Boolean;
    function UCAC4_ToBV_Helper(indata:string):Boolean;
    function CMC_ToBV(indata:string):Boolean;
    function URAT_To_Ic(indata:string):Boolean;
    function UCAC4_To_Ic(indata:string):Boolean;
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
function StarProxy.ShowBVEst(Vest:Real; Best:Currency; amsg:string):Boolean;
var msgdata:string;
    rval:Word;
begin
  Result := False;
  // building the message to show
  msgdata := 'The estimates are :' + sLineBreak;
  // V and B
  msgdata += 'V estimate : ' + FloatToStrF(Vest,ffFixed,2,3);
  msgdata += sLineBreak;
  msgdata += 'B estimate : ' + CurrToStrF(Best,ffFixed,3);
  msgdata += sLineBreak;
  // final touches
  msgdata += 'Do you want to use these magnitudes?';
  // asking
  // asking if we want to use the estimated magnitudes
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    // setting B
    cstar.fluxtemp.blue_mag := Best;
    cstar.fluxtemp.blue_err := 0.16;
    // setting V
    cstar.SetVisualMagnitude(Vest);
    // adding some notes
    if not cstar.NotesConatins(amsg) then begin
      cstar.AppndNote(' ' + amsg,False);
    end;
    sys.UpdateEstimates;
    Result := True;
  end;
end;
//---------------------------------------------------------------------
function StarProxy.ShowRIEst(Rcest,Icest:Currency; amsg:string):Boolean;
var msgdata:string;
    rval:Word;
    urc,uic:Boolean;
begin
  Result := False;
  urc := Rcest < 90;
  uic := Icest < 90;
  // building the message to show
  if not (urc or uic) then Exit;
  if (urc and uic) then begin
     msgdata := 'The estimates are :' + sLineBreak;
     msgdata += 'Rc : ' + CurrToStrF(Rcest,ffFixed,2) + sLineBreak;
     msgdata += 'Ic : ' + CurrToStrF(Icest,ffFixed,2) + sLineBreak;
     msgdata += 'Do you want to use these magnitudes?';
  end else begin
    if urc then msgdata := 'The Rc estimate is  :' + CurrToStrF(Rcest,ffFixed,2)
    else msgdata := 'The Ic estimate is  :' + CurrToStrF(Icest,ffFixed,2);
    msgdata += sLineBreak;
  msgdata += 'Do you want to use this magnitude?';
  end;
  // asking if we want to use the estimated magnitudes
  rval := mrNo;
  rval := MessageDlg(msgdata, mtConfirmation,[mbYes, mbNo],0);
  if (rval = mrYes) then begin
    // setting Ic
    if uic then cstar.fluxtemp.I_mag := Icest;
    if urc then cstar.fluxtemp.red_mag := Rcest;
    // adding some notes
    if not cstar.NotesConatins(amsg) then begin
      if uic and urc then amsg := 'RI ' + amsg
      else if uic then amsg := 'Ic ' + amsg
      else amsg := 'Rc ' + amsg;
      cstar.AppndNote(' ' + amsg,False);
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
    name_star,name_sys:StarName;
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
    cpllx,expmag:Real;
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
  resok := styper.SetFluxes(cpllx,cstar.VisualMagnitude,cstar.fluxtemp);
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
  if (cstar.estimator<>nil) and (cstar.VisualMagnitude<90) then begin
    // calculating expected vs actual magnitude
    vmdiffok := LookupExpectedAbsVMag(cstar.estimator.ColorLetter,cstar.estimator.ColorSubrange,expmag);
    if vmdiffok then expmag := cstar.estimator.AbsoluteVisMagnitude - expmag
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
      cstar.SpectralClass := therec;
      cstar.InitializeEstimation(cpllx);
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
var usingVB:Boolean;
    out_B,out_Be,out_Best:Currency;
    out_V,out_Ve,out_Vest:Real;
    out_Rc,out_Ic:Currency;
    msgdata:string;
    rval:Word;
const amsg = 'BVRI converted/used from APASS.';
begin
  Result := False;
  Assert(cstar<>nil);
  if not APASS_to_Fluxes(indata,usingVB,out_V,out_Ve,out_Vest,out_B,out_Be,
                                    out_Best,out_Rc,out_Ic) then begin
    ShowMessage('Unable to parse or convert the input!');
    Exit;
  end;
  msgdata := 'The values are :' + sLineBreak;
  // B and V
  if usingVB then begin
    msgdata += 'V : ' + FloatToStrF(out_V,ffFixed,2,3) + '±';
    msgdata += FloatToStrF(out_Ve,ffFixed,2,3) + sLineBreak;
    msgdata += 'V est (not used) : ' + FloatToStrF(out_Vest,ffFixed,2,3);
    msgdata += sLineBreak;
    msgdata += 'B : ' + CurrToStrF(out_B,ffFixed,3) + '±';
    msgdata += CurrToStrF(out_Be,ffFixed,3) + sLineBreak;
    msgdata += 'B est (not used) : ' + CurrToStrF(out_Best,ffFixed,3);
    msgdata += sLineBreak;
  end
  else begin
    msgdata += 'V estimate : ' + FloatToStrF(out_Vest,ffFixed,2,3);
    msgdata += sLineBreak;
    msgdata += 'B estimate : ' + CurrToStrF(out_Best,ffFixed,3);
    msgdata += sLineBreak;
  end;
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
    if usingVB then begin
      cstar.fluxtemp.blue_mag := out_B;
      cstar.fluxtemp.blue_err := out_Be;
    end
    else begin
      cstar.fluxtemp.blue_mag := out_Best;
      cstar.fluxtemp.blue_err := 0.0;
    end;
    // setting V
    if usingVB then cstar.SetVisualMagnitude(out_V)
    else cstar.SetVisualMagnitude(out_Vest);
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
      if simbadalso then begin
        simburl := MakeSimbadIdLookupURL(gname);
        downsim := GetSimbadDataURL(simburl,fetchfail);
      end else downsim := nil;
      downd := GetFromVizier(gname);
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
function StarProxy.URAT_ToBV(indata:string):Boolean;
var params:RealArray;
    gval,Best:Currency;
    Vest:Real;
const amsg = 'BV estimated from URAT/G';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // setting gval
    if Length(params) >= 2 then gval := params[1]
    else gval := cstar.fluxtemp.gaia_mag;
    // calling the transform function
    if not URATG_ToBV(params[0],gval,cstar.fluxtemp.J_mag,cstar.fluxtemp.H_mag,
       cstar.fluxtemp.K_mag,Best,Vest) then begin
         ShowMessage('Cannot estimate, Color out of range!');
       end else Result := ShowBVEst(Vest,Best,amsg);
  end
  else ShowMessage('Unable to parse the input');
end;
//----------------------------------------------------------
function StarProxy.UCAC4_ToBV_Helper(indata:string):Boolean;
var params:RealArray;
    gval,Best:Currency;
    Vest:Real;
const amsg = 'BV estimated from UCAC4/G';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // setting gval
    if Length(params) >= 2 then gval := params[1]
    else gval := cstar.fluxtemp.gaia_mag;
    // calling the transform function
    if not UC2MG_To_BV(params[0],gval,cstar.fluxtemp.J_mag,cstar.fluxtemp.H_mag,
       cstar.fluxtemp.K_mag,Best,Vest) then begin
         ShowMessage('Cannot estimate, Color out of range!');
       end else Result := ShowBVEst(Vest,Best,amsg);
  end
  else ShowMessage('Unable to parse the input');
end;
//--------------------------------------------
function StarProxy.CMC_ToBV(indata:string):Boolean;
var params:RealArray;
    gval,Best:Currency;
    Vest:Real;
const amsg = 'BV estimated from CMC r’/G';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // setting gval
    if Length(params) >= 2 then gval := params[1]
    else gval := cstar.fluxtemp.gaia_mag;
    // calling the transform function
    if not CMC15_ToBV(params[0],gval,cstar.fluxtemp.J_mag,
       cstar.fluxtemp.K_mag,Best,Vest) then begin
         ShowMessage('Cannot estimate, Color out of range!');
       end else Result := ShowBVEst(Vest,Best,amsg);
  end
  else ShowMessage('Unable to parse the input');
end;
//-------------------------------------------
function StarProxy.URAT_To_Ic(indata:string):Boolean;
var params:RealArray;
    Icest:Currency;
const amsg = 'estimated from URAT+J.';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // calling the transform function
    if not URATJ_To_Ic(params[0],cstar.fluxtemp.J_mag,Icest) then begin
         ShowMessage('Cannot estimate, Color out of range!');
    end else Result := ShowRIEst(99,Icest,amsg);
  end
  else ShowMessage('Unable to parse the input');
end;
//--------------------------------------------
function StarProxy.UCAC4_To_Ic(indata:string):Boolean;
var params:RealArray;
    gval,Icest,Rcest:Currency;
    urc,uic:Boolean;
const amsg = 'estimated from UCAC/G+J.';
begin
  Result := False;
  if SplitWithSpacesToReal(indata,1,params) then begin
    // setting gval
    if Length(params) >= 2 then gval := params[1]
    else gval := cstar.fluxtemp.gaia_mag;
    // calling the transform functions
    Rcest := 99;
    Icest := 99;
    urc := UCAC_To_RcS(params[0],cstar.fluxtemp.J_mag,cstar.fluxtemp.H_mag,cstar.fluxtemp.K_mag, Rcest);
    uic := UC2MG_To_Ic(params[0],gval,cstar.fluxtemp.J_mag,Icest);
    if not (urc or uic)  then begin
         ShowMessage('Cannot estimate, Color out of range!');
    end else Result := ShowRIEst(Rcest,Icest,amsg);
  end
  else ShowMessage('Unable to parse the input');
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  current := nil;
end.

