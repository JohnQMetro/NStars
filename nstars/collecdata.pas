unit collecdata;

{$MODE Delphi}

interface

uses SysUtils, StdCtrls, Classes, StrUtils, Dialogs, Controls, Forms, FileUtil,
  stardata, newlocation, namedata, recons, constellation, sptfluxest, df_strings,
  Arcins, cluster, tgas, simbad, NewStar, newImports, StarDataBase, ExtraImports,
  StarExt2,fluxtransform,ImportVizier,Utilities2,StarEstimator, guessstype;

type

(* this class encapuslates easy access to info for the currently
edited system and star *)
StarProxy = class
  protected
    function FindTGASMatches(name_search:Boolean):TGASList;
    function TGASMatchToCurrentStar:TGASList;
    function FoundListToString(inlinst:TGASList):string;
    function GetCurrentParallax:Real;
    procedure ApplyTGASChange(newdata:TGASData);
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
    function SimbadDownload(simbadurl:string; fluxonly:Boolean):Boolean;
    function SimbadDownloadLogg(simbadurl:string):Boolean;
    function SimbadCatalogs(simbadurl:string):Boolean;
    function CurrentToBrownDwarf:Boolean;
    // location via separation and position angle
    function PosAngLocationSet:Boolean;
    // even more methods
    function GuessSpectra(out usespec:Boolean):Boolean;
    function APASS_Helper(indata:string):Boolean;
    function SDSS_Helper(indata:string):Boolean;
    function Tycho2_Helper(indata:string):Boolean;
    function VizierAPASSGet(simbadalso:Boolean):Boolean;
end;

(* I've decided to wrap up the star lists *)
StarList = class
  protected
    // internal list
    maincount:Integer;
    idcount:Integer;
    System_List:array of StarSystem;
    bookmarks:array[0..2] of Integer;
    // displayed items
    fcount:Integer;
    Filtered_List:array of StarSystem;
    list_proxy:TListBox;
    pointer:StarProxy;
    p_index:Integer;
    // file i/o information
    fnameset:Boolean;
    filename:TFileName;
    // helper methods
    procedure SetAfterFilter;
    function tscount:Integer;
  public
    // internal global items
    name, author:string;
    sources:string;
    global_notes:string;
    mergesource:Integer;
    // properties
    property TotalCount:Integer read tscount;
    // the constructor
    constructor Create(inlist:TListBox);
    // basic system manipulation
    procedure AddNewSystem;
    procedure DeleteCurrentSystem;
    function RemoveSystem(atIndex:Integer):StarSystem;
    function NextID:Integer;
    procedure AppendSystem(newsys:StarSystem);
    // extra stuff
    function GetProxy:StarProxy;
    procedure ReID;
    procedure ReSortByDistance;
    function FindByString(inval:string; out where:Integer):Integer;
    function FindByID(tofind:Integer):Integer;
    function RenameCatalogs(indata:string):Integer;
    function AricnsDiff(xoutfile:TFileName):Integer;
    function ReLinkCluster(inclus:ClusterData):Integer;
    function BoldAtIndex(theIndex:Integer):Boolean;
    function BoldAtFilteredIndex(theIndex:Integer):Boolean;
    // stuff implemented for mass nstars parallax setting
    function GetIndex:Integer;
    function GetCount:Integer;
    function GetNSDes(index:Integer):string;
    // list box changes
    procedure LoadListBox;
    procedure ChangeSystem(index:Integer);
    function LoadHereAndAfter(const startdex:Integer):Boolean;
    // list changes
    procedure ClearFiltered;
    procedure EmptyAll;
    procedure CopyToFiltered;
    // file i/o
    procedure SaveToFile(fnin:TFileName);
    procedure SaveToCHview(fnin:TFileName; oparams:SysOutParams);
    procedure SaveToAstrosynthesis(fnin:TFileName; oparams:SysOutParams);
    procedure Save;
    procedure OpenFromFile(fnin:TFileName);
    procedure GetFromRecons(fnin:TFileName);
    // filters
    procedure RadVIsZeroFilter;
    procedure DistanceTest(indist:Real; targyear:Integer);
    procedure CoolAsCats(maxcats:Integer);
    procedure ParallaxUncertain;
    procedure NoCluster;
    procedure SearchSpectra(const inval:string);
    procedure SpectralProblems;
    procedure HasMultiples;
    procedure LuminosityProblems;
    procedure HasProblems;
    procedure NoMassBrownDwarfs;
    procedure SearchNotes(tofind:string);
    procedure SearchParallaxSource(tofind:string);
    procedure MultiHip();
    // additional misc methods
    function FindViaSimbad(insim:SimbadData; out where:Integer):Integer;
    function FindFromCatList(inlist:TStringList; out where:Integer):Integer;
    function SystemAtIndex(in_index:Integer):StarSystem;
    function FindStarIDMatches(const startdex:Integer; out reportstr:string):Integer;
    function FindPosPMMatch(const startdex:Integer;  max_dist,max_pmangdiff,max_pmmagpdiff,max_pmmagdiff:Real; out reportstr:string):Integer;
    function MergeSystems(target,source:Integer):Boolean;
    function WriteEstimatorData():Integer;
    function SetTGASMatch(data_ptr:TGASCollection):Integer;
    // bookmark methods
    function GetBookmark(whichb:Integer):Integer;
    function SetBookmarkToCurrent(whichb:Integer):Boolean;
    procedure ClearBookmark(whichb:Integer);
    function GoToBookmark(whichb:Integer):Boolean;
    // the destructor
    destructor Destroy; override;
end;

HandleImportParams = class
  public
    xparams:ImportParameters;
    minpllx,maxpllxe:Real;
    percentpllxe:Boolean;
    picksysn:Boolean;
    function TestParallax(inpllx:ImportedData):Boolean;
end;

function MakeSol:StarSystem;

function TGASToStarSystem(inpllx:TGASData):StarSystem;

function AddDataToStar(inpllx:ImportedData; thestar:NewStarBase; xparams:ImportParameters):Boolean;
function UpdateExistingImD(xparams:ImportParameters; inpllx:ImportedData;
                      depoch:EpochType; targetIndex:Integer):Boolean;
function AddNewImportedSystem(xparams:ImportParameters; inpllx:ImportedData;
                      insimb:SimbadData; picksysn:Boolean; depoch:EpochType):Boolean;
function HandleNewImported(inpllx:ImportedData; const_params:HandleImportParams;
                      out new_system:Boolean):Boolean;

function HandleNewImportedAlt(inpllx:ImportedData; const_params:HandleImportParams;
                      out new_system:Boolean):Boolean;

function HandleSN35PhotometryImport(indata:Sn35Photometry; out notfound:Boolean):Boolean;


var
  // interface info
  current:StarProxy;
  primaryl:StarList;


implementation
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
procedure StarList.SetAfterFilter;
begin
  // setting the pointers
  if fcount = 0 then begin
    p_index := -1;
    pointer.Free;
    pointer := StarProxy.Create;
  end
  else begin
    p_index := 0;
    pointer.SetSystem(Filtered_List[0]);
  end;
  LoadListBox;
end;
//---------------------------------
function StarList.tscount:Integer;
begin  Result := maincount;   end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor StarList.Create(inlist:TListBox);
var bdex:Integer;
begin
  Assert(inlist<>nil);
  // we create the main list and the first star
  SetLength(System_List,1);
  idcount := 1;
  // System_List[0] := StarSystem.Create(idcount);
  System_List[0] := MakeSol();
  maincount := 1;
  // we set the list of displayed items
  SetLength(Filtered_List,1);
  fcount := 1;
  Filtered_List[0] := System_List[0];
  // setting the pointers
  pointer := StarProxy.Create;
  pointer.SetSystem(Filtered_List[0]);
  p_index := 0;
  // filenames...
  fnameset := False;
  filename := '';
  // global data
  name := '';
  author := '';
  sources := '';
  global_notes := '';
  // finishing
  list_proxy := inlist;
  for bdex := 0 to High(bookmarks) do bookmarks[bdex] := -1;
  mergesource := -1;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// basic system manipulation
procedure StarList.AddNewSystem;
var xstr:string;
begin
  // setting the length of the mainlist
  Inc(idcount);
  Inc(maincount);
  SetLength(System_List,maincount);
  // creating the new star system
  System_List[maincount-1] := StarSystem.Create(idcount);
  // adding to the filtered list
  Inc(fcount);
  SetLength(Filtered_List,fcount);
  Filtered_List[fcount-1] := System_List[maincount-1];
  // list box manipulation
  xstr := Filtered_List[fcount-1].System_name;
  list_proxy.AddItem(xstr,Filtered_List[fcount-1]);
  // pointing the pointers
  p_index := fcount-1;
  pointer.SetSystem(Filtered_List[fcount-1]);
  list_proxy.ItemIndex := fcount-1;
end;
//-----------------------------------------------------------
procedure StarList.DeleteCurrentSystem;
var delsystem:StarSystem;
    deldex:Integer;
begin
  deldex := p_index;
  delsystem := RemoveSystem(deldex);
  FreeAndNil(delsystem);
  // done!
end;
//------------------------------------------------------
function StarList.RemoveSystem(atIndex:Integer):StarSystem;
var remsystem:StarSystem;
    mainindex:Integer;
    I,J:Integer;
begin
  Result := nil;
  if atIndex < 1 then Exit;
  if atIndex >= fcount then Exit;
  // recording what we are deleting
  remsystem := Filtered_List[atIndex];
  // removing it from the filtered list
  Filtered_List[atIndex] := nil;
  for I := (atIndex+1) to fcount - 1 do begin
    Filtered_List[I-1] := Filtered_List[I];
  end;
  Dec(fcount);
  SetLength(Filtered_List,fcount);
  // resetting all of the pointers
  if atIndex = p_index then begin
    if p_index=fcount then p_index := fcount-1;
    pointer.SetSystem(Filtered_List[p_index]);
    list_proxy.ItemIndex := p_index;
  end
  else if atIndex < p_index then begin
    Dec(p_index);
    list_proxy.ItemIndex := p_index;
  end;
  //moving things from the mainlist
  mainindex := -1;
  // finding where we delete
  for J := 0 to maincount - 1 do begin
    if remsystem=(System_List[J]) then begin
      mainindex := J;
      Break;
    end;
  end;
  // deleting...
  Assert(mainindex<>-1);
  System_List[mainindex] := nil;
  // shrinking the list
  for I := (mainindex+1) to maincount - 1 do begin
    System_List[I-1] := System_List[I];
  end;
  Dec(maincount);
  SetLength(System_List,maincount);
  // done!
  Result := remsystem;
end;
//------------------------------------------------------
function StarList.NextID:Integer;
begin  Result := idcount + 1;  end;
//------------------------------------------------------
procedure StarList.AppendSystem(newsys:StarSystem);
var xstr:string;
begin
  // the usual assertions
  Assert(newsys<>nil);
  Assert((newsys.GetId)=(idcount+1));
  // after, this procedure is mostly just like the AddNewSystem
  // setting the length of the mainlist
  Inc(maincount);
  SetLength(System_List,maincount);
  // creating the new star system
  System_List[maincount-1] := newsys;
  // adding to the filtered list
  Inc(fcount);
  SetLength(Filtered_List,fcount);
  Filtered_List[fcount-1] := System_List[maincount-1];
  // list box manipulation
  Inc(idcount);
  xstr := Filtered_List[fcount-1].System_name;
  list_proxy.AddItem(xstr,Filtered_List[fcount-1]);
  // pointing the pointers
  p_index := fcount-1;
  pointer.SetSystem(Filtered_List[fcount-1]);

  list_proxy.ItemIndex := fcount-1;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// extra stuff
function StarList.GetProxy:StarProxy;
begin
  Result := pointer;
end;
//----------------------------------------------------------
// re numbers the id's
procedure StarList.ReID;
var I,idc:Integer;
begin
  idc := 1;
  for I := 0 to maincount - 1 do begin
    System_List[I].SetId(idc);
    Inc(idc);
  end;
end;
//---------------------------------------------------------
procedure StarList.ReSortByDistance;
var I,J:Integer;
    temp:StarSystem;
    dist1,dist2:Real;
    loc1,loc2:Location;
begin
  // the sorting loop
  for I := 0 to maincount - 2 do begin
    for J := I+1 to maincount - 1 do begin
      // getting the info we need to make a comparison
      if I = 0 then dist1 := 0
      else begin
        loc1 := System_List[I].GetLocation;
        dist1 := loc1.GetDistance(False);
      end;
      loc2 := System_List[J].GetLocation;
      dist2 := loc2.GetDistance(False);
      // we compare... and swap if need be
      if dist2<dist1 then begin
        temp := System_List[I];
        System_List[I] :=  System_List[J];
        System_List[J] := temp
      end;
    end;
  end;
  // we do the necessary display manipulations
  CopyToFiltered;
end;
//---------------------------------------------------------
function StarList.FindByString(inval:string; out where:Integer):Integer;
var I:Integer;
    xval:Boolean;
begin
  for I := 0 to fcount - 1 do begin
    xval := Filtered_List[I].Search(inval,where);
    if xval then Break;
  end;
  // finishing
  if (not xval) then Result := -1
  else Result := I;
end;
//-------------------------------------------------------
function StarList.FindByID(tofind:Integer):Integer;
var I:Integer;
    xval:Boolean;
begin
  for I := 0 to fcount - 1 do begin
    xval := (Filtered_List[I].GetId = tofind);
    if xval then Break;
  end;
  // finishing
  if (not xval) then Result := -1
  else Result := I;
end;
//-------------------------------------------------------
function StarList.RenameCatalogs(indata:string):Integer;
var newlist:TStringList;
    I,rtotal:Integer;
begin
  Result := -1;
  newlist := nil;
  rtotal := 0;
  // preparing the list of replacements
  newlist := ParseByChar(indata,';',false,false);
  if newlist=nil then Exit;
  if newlist.Count=0 then begin
    newlist.Free;
    Exit;
  end;
  if (newlist.Count mod 2) <> 0 then begin
    newlist.Free;
    Exit;
  end;
  // we loop and replace
  for I := 0 to maincount - 1 do begin
    rtotal := rtotal + System_List[I].CatRenames(newlist);
  end;
  // done
  newlist.Free;
  Result := rtotal;
end;
//---------------------------------------------------------
function StarList.AricnsDiff(xoutfile:TFileName):Integer;
var notaricns,aricnsnot:TStringList;
    I,J,scount,ncount:Integer;
    xcurrent:AricnsEntry;
    buf:string;
    faricnsdif:Text;
begin
  // we start
  notaricns := TStringList.Create;
  aricnsnot := TStringList.Create;
  adat.urldata.UncheckAll;
  // the main loop, looking for our stars in ARICNS
  for I := 1 to maincount - 1 do begin
    scount := System_List[I].GetCompC;
    for J := 1 to scount do begin
      xcurrent := FindAricnsEntry(System_List[I],J);
      if (xcurrent=nil) then begin
        buf := System_List[I].GetPreferredName;
        notaricns.Add(buf);
      end
      else xcurrent.SetChecked(true);
    end;
  end;
  // the second main loop
  ncount := adat.urldata.GetCount;
  for I := 0 to ncount - 1 do begin
    // we have an aricns star not in our list!
    if not (adat.urldata.IsChecked(I)) then begin
      xcurrent := adat.urldata.GetAtIndex(I);
      buf := GenerateDesignations(xcurrent,0);
      aricnsnot.Add(buf);
    end;
  end;
  // once here, all we really need to do is write to a file
  // preparing the new file
  AssignFile(faricnsdif,xoutfile);
  Rewrite(faricnsdif);
  // writing systems not in ARICNS
  buf := 'Systems apparently not in ARICNS :';
  Writeln(faricnsdif,buf);
  buf := '-------------------------------------------';
  Writeln(faricnsdif,buf);
  Flush(faricnsdif);
  // the header is done
  for I := 0 to notaricns.Count - 1 do begin
    Writeln(faricnsdif,notaricns[I]);
  end;
  Flush(faricnsdif);
  // writing aricns stars not in our list
  buf := ' ';
  Writeln(faricnsdif,buf);
  buf := 'ARICNS stars apprently not in the star list:';
  Writeln(faricnsdif,buf);
  buf := '--------------------------------------------';
  Writeln(faricnsdif,buf);
  Flush(faricnsdif);
  // the header is done
  for I := 0 to aricnsnot.Count - 1 do begin
    Writeln(faricnsdif,aricnsnot[I]);
  end;
  Flush(faricnsdif);
  // finishing
  Close(faricnsdif);
  Result := notaricns.Count + aricnsnot.Count;
  notaricns.Free;
  aricnsnot.Free;
end;
//---------------------------------------------------------
function StarList.ReLinkCluster(inclus:ClusterData):Integer;
var I,LC:Integer;
begin
  inclus.ClearLinked;
  LC := 0;
  for I := 0 to maincount - 1 do begin
    if inclus.IsLinked(System_List[I],linkdist_max,-1) then Inc(LC);
  end;
  Result := LC;
end;
//----------------------------------------------
function StarList.BoldAtIndex(theIndex:Integer):Boolean;
var prefNameType:TPrefName;
begin
  Result := False;
  if theIndex > (maincount-1) then Exit;
  prefNameType := System_List[theIndex].preferred;
  if (prefNameType > 0) and (prefNameType < 4) then Result := True;
end;
//---------------------------------------------
function StarList.BoldAtFilteredIndex(theIndex:Integer):Boolean;
var prefNameType:TPrefName;
begin
  Result := False;
  if theIndex > (fcount-1) then Exit;
  prefNameType := Filtered_List[theIndex].preferred;
  if (prefNameType > 0) and (prefNameType < 4) then Result := True;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// stuff implemented for mass nstars parallax setting
function StarList.GetIndex:Integer;
begin
  Result := p_index;
end;
//-----------------------------------------------------------
function StarList.GetCount:Integer;
begin
  Result := fcount;
end;
//-----------------------------------------------------------
function StarList.GetNSDes(index:Integer):string;
var names:StarName;
begin
  // checks
  Assert(index>=0);
  Assert(index<fcount);
  // getting the namne
  names := Filtered_List[index].GetNames;
  if not names.GetCatValue('NS',Result) then Result := '';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// list box changes
procedure StarList.LoadListBox;
var I:Integer;
    xstr:string;
begin
  list_proxy.Clear;
  for I := 0 to fcount - 1 do begin
    xstr := Filtered_List[I].GetPreferredName;
    list_proxy.Items.Add(xstr);
  end;
  if fcount = 0 then list_proxy.ItemIndex := -1
  else list_proxy.ItemIndex := 0;
end;
//-----------------------------------------------------------
procedure StarList.ChangeSystem(index:Integer);
begin
  Assert(index>=0);
  Assert(index<fcount);
  p_index := index;
  pointer.SetSystem(Filtered_List[p_index]);
end;
//---------------------------------------------------------
function StarList.LoadHereAndAfter(const startdex:Integer):Boolean;
var loaddex,listdex,lengthdiff:Integer;
    xstr:string;
begin
  // bad indexes
  Result := False;
  if startdex < 0 then Exit;
  if startdex > fcount then Exit;
  // setting the list up for changes
  listdex := list_proxy.ItemIndex;
  list_proxy.ItemIndex := -1;
  // annoyingly, there is no simple way to set the number of items...
  lengthdiff := fcount - list_proxy.Count;
  if lengthdiff < 0 then begin
    // discarding items at the end of the list (the usual case).
    lengthdiff := -lengthdiff;
    for loaddex := 0 to lengthdiff-1 do begin
      list_proxy.Items.Delete(list_proxy.Count-1);
    end;
  end
  else if lengthdiff > 0 then begin
    // adding new empty items
    for loaddex := 0 to lengthdiff-1 do list_proxy.Items.Add('');
  end;
  // simple case here...
  Result := True;
  if startdex = fcount then Exit;
  // after the list length is set, we loop to change the items
  for loaddex := startdex to (fcount -1) do begin
    xstr := Filtered_List[loaddex].GetPreferredName;
    list_proxy.Items[loaddex] := xstr;
  end;
  // setting the selected item afterwards...
  if listdex < fcount then list_proxy.ItemIndex := listdex;
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// list changes
//-----------------------------------------------------------
procedure StarList.ClearFiltered;
begin
  list_proxy.Clear;
  p_index := -1;
  SetLength(Filtered_List,0);
  fcount := 0;
end;
//-----------------------------------------------------------
procedure StarList.EmptyAll;
var I:Integer;
begin
  ClearFiltered;
  for I := 0 to maincount - 1 do System_List[I].Free;
  SetLength(System_List,0);
  maincount := 0;
  idcount := 0;
end;
//-----------------------------------------------------------
procedure StarList.CopyToFiltered;
var I:Integer;
begin
  ClearFiltered;
  // copying over
  SetLength(Filtered_List,maincount);
  for I := 0 to maincount - 1 do begin
    Filtered_List[I] := System_List[I];
  end;
  fcount := maincount;
  // setting the pointers
  p_index := 0;
  pointer.SetSystem(Filtered_List[0]);
  LoadListBox;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// file i/o
//-----------------------------------------------------------
procedure StarList.SaveToFile(fnin:TFileName);
var outfile:Text;
    I:Integer;
    buf:string;
    qres:TStringList;
    fninold:TFileName;
begin
  // making a backup file
  if FileExists(fnin) then begin
     fninold := fnin + '.old';
     FileUtil.CopyFile(fnin,fninold,True);
  end;
  // preparing the new file
  AssignFile(outfile,fnin);
  Rewrite(outfile);
  // writing the global data
  Writeln(outfile,'@@@'+name);
  Writeln(outfile,'@@@'+author);
  buf := '@@@' + AnsiReplaceStr(sources,#13#10,'$#@!#');
  Writeln(outfile,buf);
  buf := '@@@' + AnsiReplaceStr(global_notes,#13#10,'$#@!#');
  Writeln(outfile,buf);
  // clusters
  qres := MakeClusterNamesList;
  qres.Delimiter := ';';
  buf := '@@@' + qres.DelimitedText;
  qres.Free;
  Writeln(outfile,buf);
  // maximum link distance and bookmarks
  Str(linkdist_max:5:2,buf);
  buf := '@@@' + buf + ';False'; // extra reserved field
  for I := 0 to High(bookmarks) do begin
    buf += ';' + IntToStr(bookmarks[I]);
  end;
  Writeln(outfile,buf);
  // writing
  for I := 0 to maincount - 1 do begin
    System_List[I].WriteToTextFile(outfile);
    Flush(outfile);
  end;
  CloseFile(outfile);
  // setting filename
  fnameset := True;
  filename := fnin;
end;
//-----------------------------------------------------------
procedure StarList.SaveToCHview(fnin:TFileName; oparams:SysOutParams);
var outfile:Text;
    chdata:string;
    I:Integer;
begin
  // preparing the new file
  AssignFile(outfile,fnin);
  Rewrite(outfile);
  // writing
  for I := 0 to maincount - 1 do begin
    chdata := System_List[I].MakeChView(oparams);
    DeUnicode(chdata);
    chdata := Trim(chdata);
    Writeln(outfile,StringToLatin1(chdata));
    Flush(outfile);
  end;
  CloseFile(outfile);
end;
//-----------------------------------------------------------
procedure StarList.SaveToAstrosynthesis(fnin:TFileName; oparams:SysOutParams);
var outfile:TextFile;
    asdata:string;
    I:Integer;
begin
  // preparing the new file
  AssignFile(outfile,fnin);
  Rewrite(outfile);
  // writing
  for I := 0 to maincount - 1 do begin
    asdata := System_List[I].MakeAstroSynthesis(oparams);
    DeUnicode(asdata);
    asdata := Trim(asdata);
    Writeln(outfile,StringToLatin1(asdata));
    Flush(outfile);
  end;
  CloseFile(outfile);
end;
//-----------------------------------------------------------
procedure StarList.Save;
begin
  Assert(fnameset);
  SaveToFile(filename);
end;
//-----------------------------------------------------------
procedure StarList.OpenFromFile(fnin:TFileName);
var infile:text;
    I,xsysc,maxid,tcode:Integer;
    tnew:StarSystem;
    xstr:string;
    temp_list:array of StarSystem;
    temp_cluslist:TStringList;
begin
  // we start
  xsysc:=0;
  maxid := 0;
  AssignFile(infile,fnin);
  FileMode := fmOpenRead;
  Reset(infile);
  // reading the global data
  Readln(infile,xstr);
  Assert(AnsiStartsStr('@@@',xstr));
  name := copy(xstr,4,length(xstr)-3);
  Readln(infile,xstr);
  Assert(AnsiStartsStr('@@@',xstr));
  author := copy(xstr,4,length(xstr)-3);
  Readln(infile,xstr);
  Assert(AnsiStartsStr('@@@',xstr));
  xstr := AnsiReplaceStr(xstr,'$#@!#',#13#10);
  sources := copy(xstr,4,length(xstr)-3);
  Readln(infile,xstr);
  Assert(AnsiStartsStr('@@@',xstr));
  xstr := AnsiReplaceStr(xstr,'$#@!#',#13#10);
  global_notes := copy(xstr,4,length(xstr)-3);

  // reading in cluster data
  Readln(infile,xstr);
  Assert(AnsiStartsStr('@@@',xstr));
  xstr := copy(xstr,4,length(xstr)-3);
  // parsing cluster data
  temp_cluslist := TStringList.Create;
  temp_cluslist.Delimiter := ';';
  temp_cluslist.DelimitedText := xstr;
  // making the cluster list
  SetLength(cluster_list,temp_cluslist.Count);
  for I := 0 to temp_cluslist.Count - 1 do begin
    cluster_list[I] := ClusterData.Create(temp_cluslist[I]);
  end;
  // done...
  FreeAndNil(temp_cluslist);

  // max link distance in ly, and bookmarks
  // reading in the line and splitting it...
  Readln(infile,xstr);
  Assert(AnsiStartsStr('@@@',xstr));
  xstr := copy(xstr,4,length(xstr)-3);
  temp_cluslist := SplitWithDelim(xstr,';',1);
  Assert(temp_cluslist<>nil);
  // max link distance in ly
  Val(temp_cluslist[0],linkdist_max,tcode);
  Assert(tcode=0);
  // the bookmarks
  for I := 0 to High(bookmarks) do begin
    if (temp_cluslist.Count-2) <= I then bookmarks[I] := -1
    else begin
      Val(temp_cluslist[2+I],bookmarks[I],tcode);
      Assert(tcode=0);
    end;
  end;

  // the loading loop
  while not Eof(infile) do begin
    tnew := StarSystem.Create(1);
    tnew.ReadFromTextFile(infile);
    // if we get here without an error being thrown...
    if maxid<(tnew.GetId) then maxid := tnew.GetId;    
    SetLength(temp_list,xsysc+1);
    temp_list[xsysc] := tnew;
    Inc(xsysc);
    // we add cluster info
    ClustersLoadAdd(temp_list[xsysc-1]);

  end;
  // we've gotten to the end!
  CloseFile(infile);
  FileMode := fmOpenReadWrite;
  // we clear out the old stuff
  EmptyAll;
  // pay heed to the new stuff
  SetLength(System_List,xsysc);
  for I := 0 to xsysc - 1 do begin
    System_List[I] := temp_list[I];
  end;
  idcount := maxid;
  maincount := xsysc;
  // filenames
  filename := fnin;
  fnameset := True;
  // loading the interface
  CopyToFiltered;
end;
//-----------------------------------------------------------
procedure StarList.GetFromRecons(fnin:TFileName);
var I:Integer;
begin
  if ImportRecons(fnin,1) then begin
    // we clear out the old stuff
    EmptyAll;
    // pay heed to the new stuff
    SetLength(System_List,rl_systnum+1);
    for I := 1 to rl_systnum do begin
      System_List[I] := ReconsList[I-1];
    end;
    idcount := rl_systnum+1;
    maincount := rl_systnum+1;
    // we now set item 0, which is the sun
    System_List[0] := MakeSol;
    // filenames
    fnameset := False;
    filename := '';
    // loading the interface
    CopyToFiltered;
    // getting rid of recons
    rl_systnum:=0;
    SetLength(ReconsList,0);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// filters
//-------------------------------------------------------
procedure StarList.RadVIsZeroFilter;
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].RVZ then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//-------------------------------------------------------
procedure StarList.DistanceTest(indist:Real; targyear:Integer);
var I:Integer;
    coreloc:Location;
begin
  Assert(indist>0);
  coreloc := pointer.sysl;
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].LessThanLY(coreloc,indist,targyear) then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//-------------------------------------------------------
procedure StarList.CoolAsCats(maxcats:Integer);
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].NotEnoughCats(maxcats) then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.ParallaxUncertain;
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].ParallaxUncertain then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.NoCluster;
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].ClusterCount=0 then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.SearchSpectra(const inval:string);
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].SearchSpectra(inval) then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.SpectralProblems;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].UnknownSpectra then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.HasMultiples;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].HasMultiples then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//-----------------------------------------
procedure StarList.LuminosityProblems;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].LuminosityCloseToOne then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//---------------------------------------
procedure StarList.HasProblems;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].has_problems then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//-------------------------------------
procedure StarList.NoMassBrownDwarfs;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].NoMassBrownDwarf then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//---------------------------------------
procedure StarList.SearchNotes(tofind:string);
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].SearchNotes(tofind) then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//---------------------------------------
procedure StarList.SearchParallaxSource(tofind:string);
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].SearchParallaxSource(tofind) then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//---------------------------------------
procedure StarList.MultiHip();
var I,xcount:Integer;
    xolist:TStringList;
begin
  ClearFiltered;
  // copying over
  for I := 1 to maincount - 1 do begin
    // getting and checking the list of Hipparcos or Tycho IDs
    xolist := System_List[I].HipTycNames();
    if xolist = nil then Continue;
    xcount := xolist.Count;
    xolist.Free;
    // if we have multiple, we copy to the filtered list
    if xcount > 1 then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// additional misc methods
function StarList.FindViaSimbad(insim:SimbadData; out where:Integer):Integer;
var cvsid,curid:string;
    idlist:TStringList;
    iddex,idmax,foundex:Integer;
begin
  Result := -1;
  if insim = nil then Exit;
  // starting ...
  cvsid := insim.OutputCatalogCVS;
  idlist := SplitWithDelim(cvsid,',',1);
  if idlist = nil then Exit;
  idmax := idlist.Count - 1;
  foundex := -1;
  // looping over the list
  for iddex := 0 to idmax do begin
    curid := idlist[iddex];
    foundex := FindByString(curid,where);
    if foundex >= 0 then Break;
  end;
  idlist.Free;
  Result := foundex;
end;
//--------------------------------------------------------
function StarList.FindFromCatList(inlist:TStringList; out where:Integer):Integer;
var ciddex,foundex:Integer;
    curid:string;
begin
  // quick setup and reject
  Result := -1;
  foundex := -1;
  if inlist = nil then Exit;
  if inlist.Count = 0 then Exit;
  // looping over the list
  for ciddex := 0 to (inlist.Count-1) do begin
    curid := inlist[ciddex];
    foundex := FindByString(curid,where);
    if foundex >= 0 then Break;
  end;
  // done
  Result := foundex;
end;
//--------------------------------------------------------
function StarList.SystemAtIndex(in_index:Integer):StarSystem;
begin
  Result := nil;
  if in_index < 0 then Exit;
  if in_index >= maincount then Exit;
  Result := System_List[in_index];
end;
//---------------------------------------------------------
function StarList.FindStarIDMatches(const startdex:Integer; out reportstr:string):Integer;
var base_sys,check_sys:StarSystem;
    check_index:Integer;
begin
  // bad startdex checking
  Assert(startdex>=0);
  Assert(startdex<maincount);
  // we begin...
  Result := 0;
  reportstr := '';
  base_sys := System_List[startdex];
  // looping through all systems after startdex
  for check_index := (startdex+1) to maincount-1 do begin
    check_sys := System_List[check_index];
    if base_sys.CatalogMatch(check_sys) then begin
      if Result = 0 then reportstr := base_sys.SystemIDSummary;
      reportstr += ' ~~~ HAS A CATALOG ID MATCH IN : ' + sLineBreak;
      reportstr += check_sys.SystemIDSummary;
      Inc(Result);
    end;
  end;
  // done
end;
//--------------------------------------------------------
function StarList.FindPosPMMatch(const startdex:Integer;  max_dist,max_pmangdiff,
                  max_pmmagpdiff,max_pmmagdiff:Real; out reportstr:string):Integer;
var base_sys,check_sys:StarSystem;
    check_index:Integer;
    ppmm:Boolean;
begin
  // bad startdex checking
  Assert(startdex>=0);
  Assert(startdex<maincount);
  // we begin...
  Result := 0;
  reportstr := '';
  base_sys := System_List[startdex];
  // looping through all systems after startdex
  for check_index := (startdex+1) to maincount-1 do begin
    check_sys := System_List[check_index];
    ppmm := base_sys.PosPMMatch(check_sys,max_dist,max_pmangdiff,max_pmmagpdiff,max_pmmagdiff);
    if ppmm then begin
      if Result = 0 then reportstr := base_sys.SystemLocationSummary;
      reportstr += ' ~~~ Is POSITION and PROPER MOTION MATCHED with: ' + sLineBreak;
      reportstr += check_sys.SystemLocationSummary;
      Inc(Result);
    end;
  end;
  // done
end;
//----------------------------------------------------------
function StarList.MergeSystems(target,source:Integer):Boolean;
var targdex,sourcedex:Integer;
    sourcesys,targetsys:StarSystem;
    mergeok:Boolean;
begin
  // quick reject tests
  Result := False;
  if (target<2) or (source<2) then Exit;
  if target = source then Exit;
  // getting the indexes
  targdex := FindByID(target);
  if targdex < 0 then Exit;
  sourcedex := FindByID(source);
  if sourcedex < 0 then Exit;
  // getting the target
  targetsys := SystemAtIndex(targdex);
  // pulling out the source
  sourcesys := RemoveSystem(sourcedex);
  // inserting the contents
  mergeok := targetsys.MergeIntoSystem(sourcesys);
  Assert(mergeok);
  // done
  FreeAndNil(sourcesys);
  Result := True;
end;
//-------------------------------------------------------
function StarList.WriteEstimatorData():Integer;
var sysdex:Integer;
    addcount:Integer;
begin
  StartEstimateFiles;
  addcount := 0;
  for sysdex := 0 to (maincount-1) do begin
    addcount += System_List[sysdex].WriteEstimateData();
  end;
  EndEstimateFiles();
  Result := addcount;
end;
//-------------------------------------------------------
function StarList.SetTGASMatch(data_ptr:TGASCollection):Integer;
var sysdex:Integer;
    addcount:Integer;
begin
    Result := 0;
    if data_ptr = nil then Exit;
    for sysdex := 1 to (maincount-1) do begin
      addcount := System_List[sysdex].SetTGASMatches(data_ptr);
      Result += addcount;
    end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// bookmark methods
//---------------------------------------------
function StarList.GetBookmark(whichb:Integer):Integer;
begin
  Result := -1;
  if whichb<0 then Exit;
  if whichb > High(bookmarks) then Exit;
  Result := bookmarks[whichb];
end;
//---------------------------------------------
function StarList.SetBookmarkToCurrent(whichb:Integer):Boolean;
begin
  Result:= False;
  // bookmark out of range
  if whichb<0 then Exit;
  if whichb > High(bookmarks) then Exit;
  // current system out of range
  if current.sys = nil then Exit;
  // setting
  bookmarks[whichb] := current.sys.GetId;
  Result := True;
end;
//---------------------------------------------
procedure StarList.ClearBookmark(whichb:Integer);
begin
  if (whichb>0) and (whichb<High(bookmarks)) then begin
    bookmarks[whichb]:= -1;
  end;
end;
//---------------------------------------------
function StarList.GoToBookmark(whichb:Integer):Boolean;
var bookval,targetdex:Integer;
begin
  Result:= False;
  // bookmark out of range
  bookval := GetBookmark(whichb);
  if bookval < 1 then Exit;
  targetdex := FindByID(bookval);
  if targetdex < 0 then Exit;
  // moving to the bookmarked system
  p_index := targetdex;
  pointer.SetSystem(Filtered_List[targetdex]);
  list_proxy.ItemIndex := targetdex;
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// the destructor
destructor StarList.Destroy;
begin
  EmptyAll;
  pointer.Free;
  pointer := nil;
end;
//==========================================================
(*minpllx,maxpllxe:Real; *)
function HandleImportParams.TestParallax(inpllx:ImportedData):Boolean;
begin
  Result := False;
  if inpllx = nil then Exit;
  if inpllx.pllx < minpllx then Exit;
  if inpllx.pllx_err > maxpllxe then Exit;
  Result := True;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function MakeSol:StarSystem;
begin
  Result := StarSystem.Create(1);
  // names
  Result.System_name := 'Sol';
  Result.extra_names := 'The Sun';
  Result.preferred := 1;
end;
//----------------------------------------------------------
function TGASToStarSystem(inpllx:TGASData):StarSystem;
var simbx:SimbadData;
    newid,sstar_where:Integer;
    locat:Location;
    rapos,decpos:Real;
begin
  Result := nil;
  // getting the simbad data
  simbx := TGASToSimbad(inpllx);
  if simbx = nil then Exit;
  // if the simbad data could be part of a sytem, handle the star manually
  if simbx.inGroup then begin
    simbx.Free;
    Exit;
  end;
  // looking for cases where the star already exists in our list
  if primaryl.FindViaSimbad(simbx,sstar_where) >=0 then begin
    simbx.Free;
    Exit;
  end;
  (* if we get here, things are go to create a new system that is to be
  added to the list.*)
  newid := primaryl.NextID;
  Result := StarSystem.Create(newid);
  Result.AddSimbadData(1,simbx,False);
  simbx.Free;
  // setting the TGAS data and constellation
  locat := Result.GetLocation;
  locat.SetFromTGAS(inpllx);
  rapos := locat.GetDecimalRightAscension;
  decpos := locat.GetDecimalDeclination;
  Result.constellation := Const_Data.LocatePoint(rapos,decpos,locat.Epoch);
  Result.UpdateEstimates;
  //finally
  inpllx.matched := True;

end;
//-------------------------------------------------------------
function AddDataToStar(inpllx:ImportedData; thestar:NewStarBase; xparams:ImportParameters):Boolean;
var starptr:StarInfo;
    isbd:Boolean;
begin
  // preliminary checks
  Result := False;
  if inpllx = nil then Exit;
  if thestar = nil then Exit;
  // what actions we import depend upon the data and brown dwarf status
  isbd := thestar.isBrownDwarf;
  // mass and radius are for stars only
  if (not isbd) and ((xparams.useMass) or (xparams.useRadius)) then begin
    starptr := StarInfo(thestar);
    if starptr.ExtraInfo = nil then starptr.ExtraInfo := StarExtraData.Create;
    if (xparams.useMass) then begin
      if inpllx.mass <> '' then starptr.ExtraInfo.SetMass(inpllx.mass,' 0.000');
    end;
    if (inpllx.radius <> '') and (xparams.useRadius) then begin
      starptr.ExtraInfo.SetDiameter(inpllx.radius,inpllx.raderr); // aka radius
    end;
  end;
  // spectral type
  if xparams.useSpT then begin
    if inpllx.stype<>'' then thestar.SpectralClass := inpllx.stype;
  end;
  // flux/temp   etc
  if xparams.UseFluxesTemp then begin
    // star only...
    if (not isbd) then begin
      starptr := StarInfo(thestar);
      // visual luminosity
      if (inpllx.Vmag < 99) and xparams.useVRI_Flux then starptr.SetVisualMagnitude(inpllx.Vmag);
      // ultraviolet flux
      if (inpllx.Umag < 90) and xparams.useU_Flux then begin
        if starptr.fluxtemp = nil then starptr.fluxtemp := StarFluxPlus.Create;
        starptr.fluxtemp.ultraviolet_mag := inpllx.Umag;
      end;
      // blue flux
      if (inpllx.Bmag < 90) and xparams.useB_Flux then begin
        if starptr.fluxtemp = nil then starptr.fluxtemp := StarFluxPlus.Create;
        starptr.fluxtemp.blue_mag := inpllx.Bmag;
        starptr.fluxtemp.blue_err := inpllx.BmagE;
      end;
      // temperature
      if (inpllx.teff > 100) and xparams.useTeff then begin
        if starptr.fluxtemp = nil then starptr.fluxtemp := StarFluxPlus.Create;
        starptr.fluxtemp.EffectiveTemp := inpllx.teff;
      end;
    end;
    // stars and brown dwarves
    if xparams.useVRI_Flux then begin
      if thestar.fluxtemp = nil then thestar.fluxtemp := StarFluxPlus.Create;
      if inpllx.Rmag < 99 then thestar.fluxtemp.red_mag := inpllx.Rmag;
      if inpllx.Imag < 99 then thestar.fluxtemp.i_mag := inpllx.Imag;
    end;
  end;
  // bolometric luminosity
  if (not isbd) and xparams.useBLum then begin
    if starptr.ExtraInfo = nil then starptr.ExtraInfo := StarExtraData.Create;
    starptr.ExtraInfo.SetBolometric(inpllx.blum,inpllx.blume);
  end;
  // done
  Result := True;
end;
//-----------------------------------------------------------------------
function UpdateExistingImD(xparams:ImportParameters; inpllx:ImportedData; depoch:EpochType; targetIndex:Integer):Boolean;
var  currsys:StarSystem;
     locat:Location;
     compptr:NewStarBase;
begin
  Result := False;
  inpllx.ismatched := True;
  (* to avoid the mess of different parallaxes for different components, we
  do one location systems only *)
  currsys := primaryl.SystemAtIndex(targetIndex);
  if (not currsys.HasOneParallax) then begin
    inpllx.check := True;     Exit;
  end;
  // setting location
  locat := currsys.GetLocation;
  Result := locat.SetFromImported(inpllx,depoch,xparams.pllx_sourceid,False);
  if Result then begin
    compptr := currsys.GetNewStar(1);
    AddDataToStar(inpllx,compptr,xparams);
    if xparams.useRadv then begin
      if inpllx.radialv < 100000 then locat.SetRadialVelocity(inpllx.radialv);
    end;
    // updating some info
    currsys.BinaryLocationUpdate;
    currsys.UpdateEstimates;
    inpllx.used := True;
  end;
end;
//---------------------------------------------------------------------
function AddNewImportedSystem(xparams:ImportParameters; inpllx:ImportedData; insimb:SimbadData; picksysn:Boolean; depoch:EpochType):Boolean;
var  currsys:StarSystem;        newid:Integer;
     locat:Location;            nametest:StarName;
     compptr:NewStarBase;
     rapos,decpos:Real;
begin
  Result := False;
  newid := primaryl.NextID;
  currsys := StarSystem.Create(newid);
  currsys.AddSimbadData(1,insimb,False);
  // setting location and constellation
  locat := currsys.GetLocation;
  if xparams.useSimbadProperMotion then begin
    inpllx.SetProperMotionPartsM(insimb.pmra,insimb.pmdec);
  end;
  locat.SetFromImported(inpllx,depoch,xparams.pllx_sourceid,True);
  if xparams.useSimbadLocation then begin
    locat.SetPositionHMS(eJ2000,insimb.ra_coord,insimb.dec_coord);
  end;
  rapos := locat.GetDecimalRightAscension;
  decpos := locat.GetDecimalDeclination;
  if xparams.useRadv then begin
    if inpllx.radialv < 100000 then locat.SetRadialVelocity(inpllx.radialv);
  end;
  currsys.constellation := Const_Data.LocatePoint(rapos,decpos,locat.Epoch);
  // perhaps picking the system name...
  if picksysn and currsys.AreNotesEmpty then begin
    nametest := currsys.GetNames;
    if nametest <> nil then begin
      if nametest.GetCatalogCount<4 then begin
         currsys.System_name:= nametest.GetRandomCat;
      end;
    end;
  end;
  // adding data
  compptr := currsys.GetNewStar(1);
  AddDataToStar(inpllx,compptr,xparams);
  // updating some info
  currsys.UpdateEstimates;
  // adding the new system to the list
  primaryl.AppendSystem(currsys);
  inpllx.used := True;
  Result := True;
end;
//-----------------------------------------------------------------------
function HandleNewImported(inpllx:ImportedData; const_params:HandleImportParams;
                  out new_system:Boolean):Boolean;
var simbx:SimbadData;
    simloc,sstar_where:Integer;
    depoch:EpochType;
begin
  Result := False;
  Assert(const_params.minpllx>0);
  new_system := False;
  // quick exit
  if const_params.percentpllxe then begin
     if ((inpllx.pllx_err/inpllx.pllx)*100) > const_params.maxpllxe then Exit;
  end else begin
    if inpllx.pllx_err > const_params.maxpllxe then Exit;
  end;
  // epoch
  if const_params.xparams.epochdata = 2014 then depoch := zJ2014
  else if const_params.xparams.epochdata = 2015 then depoch := zJ2015
  else  depoch := eJ2000;
  // getting the simbad data
  simbx := ImportedDataToSimbad(inpllx);
  if simbx = nil then begin
    simloc := primaryl.FindFromCatList(inpllx.nameids,sstar_where);
    inpllx.ismatched := ( simloc >= 0 );
    inpllx.check:= True;  Exit;
  end;
  // is the star already in our list?
  simloc := primaryl.FindViaSimbad(simbx,sstar_where);
  // star already in our list ...
  if simloc >= 0 then begin
    simbx.Free;
    if sstar_where < 2 then begin
      Result := UpdateExistingImD(const_params.xparams,inpllx,depoch,simloc);
    end else inpllx.check:= True;
    inpllx.ismatched := True;
  end
  // star not in our list
  else begin
    (* If the simbad data could be part of a sytem, OR if the parallax is bad, handle the star manually. *)
    if (simbx.inGroup) or (not const_params.TestParallax(inpllx)) then begin
      if simbx.inGroup then inpllx.check := True;
      simbx.Free;
      Exit;
    end;
    // otherwise, create a new system...
    new_system := True;
    Result := AddNewImportedSystem(const_params.xparams,inpllx,simbx,const_params.picksysn,depoch);
    simbx.Free;
    Result := True;
  end;
end;
//---------------------------------------------------------------
(* Similar to HandleNewImported, except instead of always downloading Simbad Data,
this function tries to match using catalog ids in inpllx, and only if that fails
and if the parallax >= minpllx, does it download the Simbad data and add a new
system *)
function HandleNewImportedAlt(inpllx:ImportedData; const_params:HandleImportParams;
                  out new_system:Boolean):Boolean;
var simbx:SimbadData;
    idloc,sstar_where:Integer;
    depoch:EpochType;
begin
  Result := False;
  Assert(const_params.minpllx>0);
  new_system := False;
  // epoch
  if const_params.xparams.epochdata = 2014 then depoch := zJ2014
  else if const_params.xparams.epochdata = 2015 then depoch := zJ2015
  else  depoch := eJ2000;
  // looking to see if the system is already in the list
  idloc := primaryl.FindFromCatList(inpllx.nameids,sstar_where);
  if (not idloc < 0) then begin
    if sstar_where < 2 then begin
      Result := UpdateExistingImD(const_params.xparams,inpllx,depoch,idloc);
    end else inpllx.check:= True;
    inpllx.ismatched := True;
  end
  // star not in our list
  else begin
    (* Reject based on parallax *)
    if (not const_params.TestParallax(inpllx)) then Exit;
    // downloading the simbad object
    simbx := ImportedDataToSimbad(inpllx);
    if simbx = nil then begin
      inpllx.check:= True;  Exit;
    end;
    (* If the simbad data could be part of a sytem, handle the star manually. *)
    if (simbx.inGroup) then begin
      inpllx.check := True;
      simbx.Free;  Exit;
    end;
    // otherwise, create a new system...
    new_system := True;
    Result := AddNewImportedSystem(const_params.xparams,inpllx,simbx,const_params.picksysn,depoch);
    simbx.Free;
    Result := True;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function HandleSN35PhotometryImport(indata:Sn35Photometry; out notfound:Boolean):Boolean;
var opos,wherepos:Integer;
    simbad_data:SimbadData;
    currstar:NewStarBase;
    cstar:StarInfo;
    cf:StarFluxPlus;
    rvmt:Boolean;
begin
  Result := False;
  notfound := True;
  Assert(indata<>nil);
  // finding the location of the star we aim to modify
  opos := primaryl.FindByString(indata.name,wherepos);
  if opos < 0 then begin
    simbad_data := indata.FindSimbadData();
    if simbad_data = nil then Exit;
    opos := primaryl.FindViaSimbad(simbad_data,wherepos);
    FreeAndNil(simbad_data);
    if opos < 0 then Exit;
  end;
  // next, getting a pointer to the star/brown dwarf
  currstar := primaryl.SystemAtIndex(opos).GetNewStar(wherepos+1);
  if currstar = nil then Exit;
  notfound := False;
  // checking and setting the fluxes...
  // Red and Infrared
  if indata.HasRI then begin
    if currstar.fluxtemp = nil then currstar.fluxtemp := StarFluxPlus.Create;
    cf := currstar.fluxtemp;
    // Rc
    if indata.TestReplace(cf.red_mag,False) then begin
      cf.red_mag := indata.Rc;
      Result := True;
    end;
    // Ic
    if indata.TestReplace(cf.I_mag,True) then begin
      cf.I_mag := indata.Ic;
      Result := True;
    end;
  end;
  // Visual (stars only)
  if (indata.Vmag < 90) and (not currstar.isBrownDwarf) then begin
    cstar := StarInfo(currstar);
    if Abs(cstar.VisualMagnitude - indata.Vmag)>0.1 then begin
      rvmt := cstar.SetVisualMagnitude(indata.Vmag);
      Result:= (Result or rvmt);
    end;
  end
  // done
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  primaryl := nil;
  current := nil;
end.
