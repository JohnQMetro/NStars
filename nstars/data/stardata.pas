unit stardata;

{$MODE Delphi}

interface

uses SysUtils, Classes, df_strings, StrUtils,
 StarDataBase, newlocation, star_names (* namedata *), unitdata, simbad, sptfluxest,
 NewStar, tgas, constellation, StarEstimator, StarExt2, gaiadr2base, fluxtransform,
 utilities2, fluxgaia;

type
//----------------------------------------------------------
// some subrange types
TConstCount = 1..88;
TPrefName = 0..4;
TBayerRange = 0..67;

//----------------------------------------------------------
StarSystem = class (StarBase)
  protected
    id:Integer;
    new_components:array of NewStarBase;
    (* --- FICTIONAL STUFF --- *)
    clusters       :TStringList;
    (* -- PRIVATE METHODS -- *)
    function CountBayerSuperscript(b_sup:Integer):Integer;
    function DistToSol(index:Integer):string;
    function GetComponentParallax(compdex:Integer):Real;
    procedure AppendNewComponent(xadd:NewStarBase);
    function MaxCInd:Integer;
    function MultiParts:Boolean;
    // I/O related methods (Private helper)
    function GetComponentBaseLabel(is2300ad:Boolean):string;
    function GetIDString:string;
    // more private methods
    function SimbNamChk(simbad_in:SimbadData; whatc:string):Boolean;
    function StarSimbad(var thestar:StarInfo; simbad_in:SimbadData):Boolean;
    function PostRemovalCleanup:Boolean;
  public
    aricns_data,simbad_data:Boolean;
    // the main system label
    System_name:string;
    preferred:TPrefName;
    // more naming information
    constellation:TConstCount;
    bayer_desig:TBayerRange;
    flamsteed_number:Integer;
    extra_names:string;
    has_planets:Boolean;
    has_problems:Boolean;
    (* --- FICTIONAL STUFF --- *)
    fictional_names:string;
    political_ownrs:string;
    description:string;
    name2300ad:string;
    (* --- PUBLIC METHODS --- *)
    constructor Create(numid:Integer);
    (* basic star info *)
    function GetMinCount:Integer;
    function GetMaxCount:Integer;
    function GetCompC:Integer;
    function GetNewStar(index:Integer):NewStarBase;
    function AddStar:StarInfo;
    function AddBrownDwarf:BrownDwarfInfo;
    procedure AddNewSepLocation();
    function InsertStarAtIndex(index:Integer; out starPtr:StarInfo):Boolean;
    function DelStar(index:Integer):Boolean;
    function SwapPositions(stardex:Integer):Boolean;
    (* name stuff *)
    function GetPreferredName:string;
    function Get2300adName:string;
    function GetSpTList():string;
    function MakeBayerName:string;
    function MakeFlamsteedName:string;
    function MakeVariableName:string;
    function MakeBayerStarName(index:Integer):string;
    function MakeVariableStarName(index:Integer):string;
    function GetNameSet(index:Integer):StarNames;
    (* text i/o *)
    function MakeLine1:string;
    function FromLine1(inval:string):Integer;
    function MakeLine6:string;
    procedure FromLine6(inval:string);
    // Text File I/O
    function WriteToTextFile(var outfile:TextFile):Boolean;
    procedure ReadFromTextFile(var infile:TextFile);

    (* Simple info methods *)
    function GetId:Integer;
    procedure SetId(inval:Integer);
    function HasOneLocation:Boolean;
    function HasOneParallax:Boolean;
    function IsBrownDwarf(starindex:Integer):Boolean;
    (* Searching *)
    function Search(infind:string; out where:Integer):Boolean;
    function SearchSpectra(inspec:string):Boolean;
    function HasCatName(cc:string):Boolean;
    function SearchNotes(tofind:string):Boolean;
    function SearchParallaxSource(tofind:string):Boolean;
    (* methods relating to duplicate systems, or splitting/combining them *)
    function ExtractStarAsSystem(stardex,newid:Integer):StarSystem;
    function CatalogMatch(othersys:StarSystem):Boolean;
    function SystemIDSummary:string;
    function PosPMMatch(other:StarSystem; max_dist,max_pmangdiff,max_pmmagpdiff,max_pmmagdiff:Real):Boolean;
    function SystemLocationSummary:string;
    function GetStarsAndEmpty():ComponentList;
    function MergeIntoSystem(var otherSystem:StarSystem):Boolean;
    (* Position and parallax manipulation *)
    function CopyLocIfApplicable:Boolean;
    function TGAS2StarCheck(inlist:TGASList):Integer;
    function ApplyDualTGAS(inlist:TGASList; first:Boolean):Boolean;
    function SetTGASMatches(target:TGASCollection):Integer;
    function BinaryLocationUpdate:Boolean;
    (* Other *)
    function CatRenames(incats:TStringList):Integer;
    function AddSimbadData(stardex:Integer; simbad_in:SimbadData; fluxtempo:Boolean):Boolean;
    function SetLoggFromSimbad(stardex:Integer; simbad_in:SimbadData):Boolean;
    function ConvertToBrownDwarf(index:Integer):Boolean;
    procedure UpdateEstimates;
    function WriteEstimateData():Integer;
    function HipTycNames():TStringList;
    (* Gaia DR2 related *)
    function GetStuffForMatching(stardex:Integer; out starloc:Location; out stnames:StarNames; out sysnames:StarNames):Boolean;
    function ApplyGaiaObject(stardex:Integer; inobj:GaiaDR2Star; conditional:Boolean):Boolean;
    function ApplyGaiaNameMags(stardex:Integer; inobj:GaiaDR2Star):Boolean;
    function ContainsNonDR2Parallax():Boolean;
    function MissingDR2id():Boolean;
    function StarSummaryWhat(stardex:Integer):string;
    function StarSummaryIDPos(stardex:Integer):string;
    (* filters *)
    function RVZ:Boolean;
    function LessThanLY(otherpoint:Location; maxdist:Real; targyear:Integer):Boolean;
    function NotEnoughCats(maxcname:Integer):Boolean;
    function ParallaxUncertain:Boolean;
    function UnknownSpectra:Boolean;
    function HipOrTyc:Boolean;
    function HasMultiples:Boolean;
    function LuminosityCloseToOne:Boolean;
    function NoMassBrownDwarf:Boolean;
    function BadCompLetter:Boolean;
    function DifferingEpochs:Boolean;
    function InternalCatDups:Boolean;
    function InternalDistanceMoreThan(const MaxDist:Real):Boolean;
    function GaiaVCheck(const MaxDiff:Real):Boolean;
    (* clusters *)
    function InCluster(inclus:string):Boolean;
    function ClusterCount:Integer;
    procedure RemoveCluster(remclus:string);
    function AddCluster(inclus:string):Integer;
    procedure ClearClusters;
    (* destructor *)
    destructor Destroy; override;
end;

// output parameters
SysOutParams = class
  public
    idOffset,TargetYear:Integer;
    allcaplum:Integer;
    is2300ad,splitsys:Boolean;
    addpquest:Boolean;
    constructor Create;
    function SetTargYear(src:string):Boolean;
    function SetIDOffset(src:string):Boolean;
    function SetAllCapsLum(src:string):Boolean;
end;
//+++++++++++++++++++++++++++++++++++++++++++++
SystemOutputter = class
  protected
    // chview output
    function GetCHVNames(sindex,maxlen:Integer):string;
    function CHViewSpecHelp(inspec:string; var rxlen:Integer):String;
    function ToCHViewStar(sindex,targyear:Integer; plen:Integer):string;
    // output labels
    function GetOutputLabel(isCHView:Boolean):string;
    function MakeUncStr(opt:Boolean):string;
    // astrosynthesis output methods (private)
    function AstroCatIDs(stardex:Integer):string;
    function AsOffsetLocat(indexw:Integer):string;
    function MakeMultiStarAstroLine(stardex:Integer):string;
    function MakeAstrosynLine(compdata:string; stardex:Integer):string;
  public
    params:SysOutParams;
    system:StarSystem;

    function MakeChView():string;
    function MakeAstroSynthesis():string;


end;

function BrownDwarfSpectra(specstr:string):Boolean;

// a function that is meant to get a simbad object for a tgas entry
function TGASToSimbad(inpllx:TGASData):SimbadData;
// a function that is meant to get a simbad entry for a Gaia object
function GaiaToSimbad(ingaiaid:GaiaDR2IDs):SimbadData;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//=============================================================
(* the purpose of this odd helper method is to count how
many stars in the system have a particular Bayer superscript.
This is used in making the correct Bayer name for a particular
star: if the result is one, no letter at the end is needed *)
function StarSystem.CountBayerSuperscript(b_sup:Integer):Integer;
var I,bs:Integer;     starcount:Integer;
begin
  Result := 0;
  starcount := GetCompC;
  if starcount = 1 then Exit;
  for I := 0 to starcount - 1 do begin
    if not (new_components[I].HasNames) then bs := 0
    else bs := (new_components[I].GetNames).bayer_sup;
    if bs=b_sup then Inc(Result);
  end;
end;
//----------------------------------------------------------
function StarSystem.DistToSol(index:Integer):string;
begin
  Result := 'Dist to Sol :';
  if id = 1 then begin
    Result += '0 LY ';
    Exit;
  end;
  if new_components[index].HasLocation then begin
    Result += new_components[index].GetLocation.GetDistanceStr(3,-1,False);
  end
  else Result += the_location.GetDistanceStr(3,-1,False);
  Result := Result + ' LY ';
end;
//-------------------------------
function StarSystem.GetComponentParallax(compdex:Integer):Real;
var usingloc:Location;
begin
  Assert(compdex>=0);
  Assert(compdex<GetCompC);
  if new_components[compdex].HasLocation then usingloc := new_components[compdex].GetLocation
  else usingloc := the_location;
  Result := usingloc.ParallaxMAS;
end;
//-------------------------------
procedure StarSystem.AppendNewComponent(xadd:NewStarBase);
begin
  Assert(xadd <> nil);
  SetLength(new_components,GetCompC+1);
  new_components[MaxCInd] := xadd;
  // star component name stuff
  if (GetCompC=2) then new_components[0].Component:= 'A';
  new_components[MaxCInd].Component := ComponentStrings[GetCompC];
end;
//--------------------------
function StarSystem.MaxCInd:Integer;
begin
  Result := High(new_components);
end;
//---------------------------------------
function StarSystem.MultiParts:Boolean;
begin
  Result := Length(new_components)>1;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function StarSystem.GetComponentBaseLabel(is2300ad:Boolean):string;
begin
  // getting the name
  if is2300ad then Result := Get2300adName
  else Result := GetPreferredName;
end;
//------------------------------------------------------
function StarSystem.GetIDString:string;
begin
  Str(id,Result);
  Result := Trim(Result);
end;


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function StarSystem.SimbNamChk(simbad_in:SimbadData; whatc:string):Boolean;
var simbc:string;
begin
  Assert(simbad_in<>nil);
  Assert(Length(whatc) <> 0);
  Result := False;
  simbc := whatc + ' ' + 'Candidate';
  if simbad_in.NameContains(simbc) then Exit;
  Result:= simbad_in.NameContains(whatc);
end;
//------------------------------------------------
// more private methods
function StarSystem.StarSimbad(var thestar:StarInfo; simbad_in:SimbadData):Boolean;
var magvalue:Double;
begin
  // checking input
  Assert(thestar<>nil);
  Assert(simbad_in<>nil);
  // visual magnitude and luminosity
  magvalue := simbad_in.VisualMag;
  if (magvalue<> 9999) then begin
     thestar.SetVisualMagnitude(simbad_in.VisualMag);
  end;
  // special things in the simbad star name...
  thestar.premain := simbad_in.isprms;
  // arity
  thestar.Arity := simbad_in.Arity;
  // variability
  if simbad_in.NameContains('Variable of BY Dra') then thestar.VariableType := BY_DRACONIS
  else if simbad_in.NameContains('Variable Star of delta Sct') then thestar.VariableType := DETA_SCUTI
  else if simbad_in.NameContains('Pulsating White Dwarf') then thestar.VariableType := ZZ_CETI
  else if simbad_in.NameContains('Flare Star') then thestar.VariableType :=  UV_CETI
  else if simbad_in.NameContains(' Variable of RS CVn') then thestar.VariableType :=  RS_CAN_VENAT
  else if simbad_in.NameContains('Variable Star') then thestar.VariableType := VARIABLE;

  // stuff put in the notes
  if SimbNamChk(simbad_in,'Young Stellar Object') then thestar.AppndNote('Young Stellar Object. ',False)
  else if SimbNamChk(simbad_in,'Brown Dwarf') then thestar.AppndNote('Brown Dwarf?. ',False);

  Result := True;
end;
//------------------------------------------------------
function StarSystem.PostRemovalCleanup:Boolean;
var name_left:StarNames;
    cindex,ccount:Integer;
    ccat:string;
begin
  Result := False;
  if GetCompC > 1 then Exit;
  (* If there is only one star left, we move any names it has to the system
  level (old system, not new). *)
  new_components[0].Component := '';
  if new_components[0].HasNames then begin
    name_left := new_components[0].GetNames;
    // variable name
    if TLZ(nameset.var_designation) then begin
      nameset.var_designation:= name_left.var_designation;
    end;
    // bayer superscript
    if nameset.bayer_sup = 0 then nameset.bayer_sup := name_left.bayer_sup;
    // proper name (very rare)
    if not TLZ(name_left.proper_name) then begin
      if not TLZ(nameset.proper_name) then nameset.proper_name += ', ';
      nameset.proper_name += name_left.proper_name;
    end;
    // catalog ids...
    name_left.TransferCatalogs(nameset,'');
    // clearing the names for 0
    new_components[0].ClearNames;
  end;
  // done
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor StarSystem.Create(numid:Integer);
var buf2:string;
begin
  // basic id stuff
  id := numid;
  buf2 := '[[ System ID: ' + GetIDString + ' ]]';
  System_name := buf2;
  preferred := 0;
  // naming information
  constellation := 1; // andromeda by default
  bayer_desig := 0;
  flamsteed_number := 0;
  extra_names := '';
  the_location :=nil;
  nameset := nil;
  if numid<>1 then begin
    // location is a must *unless this is the Sun*, by default
    the_location := Location.Create;
    // an extra nameset is also non-optional(except for the sun)
    nameset := StarNames.Create;
  end;
  // finally, since a star system must have a star, we add one
  SetLength(new_components,1);
  new_components[0] := StarInfo.Create();
  has_problems:= false;
  has_planets := False;
  // some additional stuff
  clusters := nil;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* basic star info *)
//-----------------------------------------------------------
function StarSystem.GetMinCount:Integer;
var sysmax,sysdex:Integer;
begin
  Result := 0;
  sysmax := MaxCInd;
  for sysdex := 0 to sysmax do Result += new_components[sysdex].MinPartCount;
end;
//------------------------------------------------------------
function StarSystem.GetMaxCount:Integer;
var sysmax,sysdex:Integer;
begin
  Result := 0;
  sysmax := MaxCInd;
  for sysdex := 0 to sysmax do Result += new_components[sysdex].MaxPartCount;
end;
//----------------------------------------------------------
function StarSystem.GetCompC:Integer;
begin
  Result := Length(new_components);
end;
//---------------------------------------------------------
function StarSystem.GetNewStar(index:Integer):NewStarBase;
begin
  if (index<1) or (index>Length(new_components)) then Result := nil
  else Result := new_components[index-1];
end;
//----------------------------------------------------------
function StarSystem.AddStar:StarInfo;
begin
  AppendNewComponent(StarInfo.Create);
  Result := StarInfo(new_components[MaxCInd]);
end;
//----------------------------------------------------------
function StarSystem.AddBrownDwarf:BrownDwarfInfo;
begin
  AppendNewComponent(BrownDwarfInfo.Create);
  Result := BrownDwarfInfo(new_components[MaxCInd]);
end;
//----------------------------------------------------------
procedure StarSystem.AddNewSepLocation();
var nstar:StarInfo;
begin
  nstar := AddStar();
  nstar.CopyLocation(the_location,True);
end;
//---------------------------------------------------------
function StarSystem.InsertStarAtIndex(index:Integer; out starPtr:StarInfo):Boolean;
var tempstar:StarInfo;
    stardex,starmax:Integer;
begin
  // wrong cases
  starPtr := nil;
  Result := False;
  if index <= 0 then Exit;
  if index > (GetCompC+1) then Exit;
  // proceed...
  Result := True;
  // if we insert at GetCompC+1, then this is actually append
  if index = (GetCompC+1) then starPtr := AddStar
  // otherwise,
  else begin
    tempstar := StarInfo.Create;
    starPtr := tempstar;
    // we have to move the other stars to make room
    starmax := High(new_components);
    SetLength(new_components,GetCompC+1);
    for stardex := (starmax+1) downto index do begin
      new_components[stardex] := new_components[stardex-1];
    end;
    // inserting the new
    new_components[index-1] := tempstar;
  end;
end;
//----------------------------------------------------------
function StarSystem.DelStar(index:Integer):Boolean;
var I:Integer;
begin
  // checking the bounds
  if (GetCompC=1) or (index<1) or (index>GetCompC) then begin
    Result := False;
    Exit;
  end;
  // deleting the star
  new_components[index-1].Free;
  // moving whatever is left
  for I := index to (MaxCInd) do begin
    new_components[I-1] := new_components[I];
  end;
  SetLength(new_components,MaxCInd);
  // if there is only one system left, we move stuff to system level...
  PostRemovalCleanup;
  Result := True;
end;
//----------------------------------------------------
(* Swaps poition of two components. special treatment needed if swap involves
star 1 because of the system location... *)
function StarSystem.SwapPositions(stardex:Integer):Boolean;
var temp:NewStarBase;
    locat2:Location;
    locat2copy:Boolean;
    rapos,decpos:Real;
begin
  Result := False;
  if (stardex < 1) or (stardex >= GetCompC()) then Exit;
  if (stardex = 1) and (new_components[1].HasLocation) then begin
    // the parallax of star 2 will become the system parallax, it cannot be a binary copy
    locat2 := new_components[1].ExtractLocation;
    locat2copy := locat2.IsACopy;
    if locat2copy then locat2.MakeNotCopy;
    // moving the system parallax into star 1
    if locat2copy then the_location.binarycopy := True;
    new_components[0].InsertLocation(the_location);
    // moving star 2 location into the_location
    the_location := locat2;
    // unlikely, but the constellation might change
    rapos := the_location.GetDecimalRightAscension;
    decpos := the_location.GetDecimalDeclination;
    constellation := Const_Data.LocatePoint(rapos,decpos,the_location.Epoch);
  end;
  // the swap
  temp := new_components[stardex-1];
  new_components[stardex-1] := new_components[stardex];
  new_components[stardex] := temp;
  // post cleanup...
  if (stardex = 1) then BinaryLocationUpdate;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* name stuff *)
//----------------------------------------------------------
function StarSystem.GetPreferredName:string;
begin
  if id = 1 then Result := 'Sol'
  else if preferred=0 then Result := System_name
  else if preferred=1 then Result := nameset.proper_name
  else if preferred=2 then Result := MakeBayerName
  else if preferred=3 then Result := MakeFlamsteedName
  else if preferred=4 then Result := MakeVariableName
  else Assert(false);
end;
//-----------------------------------------------------------
function StarSystem.Get2300adName:string;
var buffer:string;
begin
  // looking for a 2300ad name
  buffer := Trim(name2300ad);
  if buffer<>'' then Result := buffer
  else begin
    // proper names or bayer names first
    if preferred=1 then Result := nameset.proper_name
    else if preferred=2 then Result := MakeBayerName
    // 2300ad uses 'DM' as a catalog name, a combination of BD, CD, and CPD
    else if nameset.GetCatValue('BD',buffer) then Result := 'DM ' + buffer
    else if nameset.GetCatValue('CD',buffer) then Result := 'DM ' + buffer
    else if nameset.GetCatValue('CPD',buffer) then Result := 'DM ' + buffer
    // 2300ad sometimes uses Catalogue Astrographique ids
    else if nameset.GetCatValue('AC',buffer) then Result := 'AC ' + buffer
    // if all else fails, we fall back on the normal way
    else Result := GetPreferredName;
  end;
end;
//-----------------------------------------------------------
function StarSystem.GetSpTList():string;
var xdex,xmax:Integer;
begin
  xmax := High(new_components);
  Result := new_components[0].SpectralClass;
  if xmax > 0 then begin
    for xdex := 1 to xmax do begin
      Result += ' | ' + new_components[xdex].SpectralClass;
    end;
  end;
end;
//----------------------------------------------------------
function StarSystem.MakeBayerName:string;
var buf1:string;
begin
  if bayer_desig=0 then Result := ''
  else begin
    // we convert the Bayer Superscript first
    buf1 := '';
    if nameset.bayer_sup<>0 then begin
      Str(nameset.bayer_sup,buf1);
      buf1 := ' ' + Trim(buf1);
    end;
    // now we make the rest
    Result := bayerdes[bayer_desig-1] + buf1;
    Result := Result + ' ' + constellations[(constellation-1)*3+1];
  end;
end;
//----------------------------------------------------------
function StarSystem.MakeFlamsteedName:string;
var buf1:string;
begin
  if flamsteed_number=0 then Result := ''
  else begin
    // we convert the number
    buf1 := '';
    Str(flamsteed_number,buf1);
    buf1 := Trim(buf1) + ' ';
    // now we make the rest
    Result := buf1 + constellations[(constellation-1)*3+1];
  end;
end;
//----------------------------------------------------------
function StarSystem.MakeVariableName:string;
begin
  if nameset.var_designation='' then Result := ''
  else begin
    Result := nameset.var_designation + ' ';
    // now we make the rest
    Result := Result + constellations[(constellation-1)*3+1];
  end;
end;
//----------------------------------------------------------
function StarSystem.MakeBayerStarName(index:Integer):string;
var thestar:NewStarBase;
    buf1:string;
    bs,bc:Integer;
    addletter:Boolean;
begin
  Result := '';
  // we do the first test
  thestar := GetNewStar(index);
  if (thestar = nil) then Exit;
  // another test
  if bayer_desig=0 then Exit;
  // bayer superscripts are tricky
  buf1 := '';
  if nameset.bayer_sup<>0 then bs := nameset.bayer_sup
  else bs := thestar.GetNames.bayer_sup;
  if bs<>0 then begin
    Str(bs,buf1);
    buf1 := ' ' + Trim(buf1) + ' ';
  end;
  // do we add a letter at the end?
  if (MultiParts) then addletter := True
  else addletter := False;
  bc := CountBayerSuperscript(bs);
  if (bs<>0) and (bc=1) then addletter := False;
  // we start puttng things together
  Result := bayerdes[bayer_desig-1] + buf1;
  Result := Result + ' ' + constellations[(constellation-1)*3+1];
  if addletter then begin
    Result := Result + ' ' + thestar.Component;
  end;
end;
//----------------------------------------------------------
function StarSystem.MakeVariableStarName(index:Integer):string;
var thestar:NewStarBase;
    vstard:string;
begin
  // we do the first test
  thestar := GetNewStar(index);
  if (thestar = nil) then begin
    Result := '';
    Exit;
  end;
  // second test
  if nameset.var_designation<>'' then begin
    Result := MakeVariableName;
    if MultiParts then
      Result := Result + ' ' + thestar.Component;
  end
  else if thestar.HasNames then begin
    vstard := Trim(thestar.GetNames.var_designation);
    // now we make the rest
    if vstard = '' then Result := ''
    else Result := vstard + ' ' + constellations[(constellation-1)*3+1];
  end
  else Result := '';
end;
//---------------------------------------------------
function StarSystem.GetNameSet(index:Integer):StarNames;
begin
  if (not new_components[index].HasNames) then Result := nameset
  else Result := new_components[index].GetNames;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* text i/o *)
function StarSystem.MakeLine1:string;
var buf1:string;
begin
  // the id
  Result := GetIDString + ';';
  // arity
  Str(GetCompC,buf1);
  Result := Result + Trim(buf1) + ';';
  // main name
  Result := Result + System_name + ';';
  // the preferred indicator
  Str(preferred,buf1);
  Result := Result + Trim(buf1) + ';';
  // the consellation
  Str(constellation,buf1);
  Result := Result + Trim(buf1) + ';';
  // the bayer designation number
  Str(bayer_desig,buf1);
  Result := Result + Trim(buf1) + ';';
  // the flamsteed number
  Str(flamsteed_number,buf1);
  Result := Result + Trim(buf1) + ';';
  // extra names
  Result := Result + extra_names + ';';
  // has a planetary system
  if has_planets then Result := Result + 'T;'
  else Result := Result + 'F;';  // been checked?
  // has issues
  Result += Bool2Str(has_problems) + ';';
  // aricns and simbad data
  Result += Bool2Str(aricns_data) +  ';' + Bool2Str(simbad_data) +';';
  // pre-main sequence
end;
//----------------------------------------------------------
function StarSystem.FromLine1(inval:string):Integer;
var indata:TStringList;
    int_val, sc:Integer;
begin
  indata := SplitWithDelim(inval,';',12);
  // indata := ParseByChar(inval,';',True,True);
  if indata = nil then
    raise Exception.Create('The system line must have 12 fields!');
  // the id
  Val(indata[0],int_val,sc);
  if sc<>0 then Raise Exception.Create('The id is invalid!');
  id := int_val;
  // the starcount
  Val(indata[1],int_val,sc);
  if (sc<>0) or (int_val<=0) then Raise Exception.Create('The arity is invalid!');
  Result := int_val;
  // the name
  System_name := indata[2];
  // preferred
  Val(indata[3],int_val,sc);
  if sc<>0 then Raise Exception.Create('The preferred ind is invalid!');
  preferred := int_val;
  // constellation
  Val(indata[4],int_val,sc);
  if sc<>0 then Raise Exception.Create('The constellation is invalid!');
  constellation := int_val;
  // bayer designation
  Val(indata[5],int_val,sc);
  if sc<>0 then Raise Exception.Create('The Bayer ID is invalid!');
  bayer_desig := int_val;
  // flamsteed
  Val(indata[6],int_val,sc);
  if sc<>0 then Raise Exception.Create('The Flamsteed Number is invalid!');
  flamsteed_number := int_val;
  // extra names
  extra_names := indata[7];
  // has planets
  if indata[8]='T' then has_planets := true
  else has_planets := false;
  // some simple Boolean Flags
  has_problems := Str2BoolR(indata[9]);
  aricns_data := Str2BoolR(indata[10]);
  simbad_data := Str2BoolR(indata[11]);
  // done
  indata.Free;

end;
//------------------------------------------------------------
function StarSystem.MakeLine6:string;
var buffer:string;
begin
  buffer := AnsiReplaceStr(fictional_names,#13#10,'$#@!#');
  Result := buffer + ';';
  buffer := AnsiReplaceStr(political_ownrs,#13#10,'$#@!#');
  Result := Result + buffer;
  buffer := AnsiReplaceStr(description,#13#10,'$#@!#');
  Result := Result + ';' + buffer;
  buffer := AnsiReplaceStr(name2300ad,#13#10,'$#@!#');
  Result := Result + ';' + buffer;
end;
//-------------------------------------------------------
procedure StarSystem.FromLine6(inval:string);
var tenmlist:TStringList;
begin
  // create delimited text
  tenmlist := TStringList.Create;
  tenmlist.Delimiter := ';';
  tenmlist.DelimitedText := inval;
  // we convert the names
  fictional_names := AnsiReplaceStr(tenmlist[0],'$#@!#',#13#10);
  political_ownrs := AnsiReplaceStr(tenmlist[1],'$#@!#',#13#10);
  description := AnsiReplaceStr(tenmlist[2],'$#@!#',#13#10);
  // the optional 2300ad name
  if tenmlist.Count = 4 then begin
    name2300ad := AnsiReplaceStr(tenmlist[3],'$#@!#',#13#10);
  end
  else name2300ad := '';
  // done
  tenmlist.Free;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Text File I/O
(* general star format:
Line 1 : ### <basic star info>
Line 2 : Extra Star Names
Line 3 : Star Location Data
Line 4  : Star Notes
Line 5  : Cluster Data
Line 6  : Fictional Data
(Multiple subsequent lines for individual strs)
*)
//-----------------------------------------------------------
function StarSystem.WriteToTextFile(var outfile:TextFile):Boolean;
var linebuf:string;
    I:Integer;
begin
  // writing (lines 1 to 4)
  linebuf := '###' +MakeLine1;
  Writeln(outfile,linebuf);
  if id = 1 then begin
    Writeln(outfile,'No Nameset for the Solar System');
    Writeln(outfile,'All positions are relative to the Solar System');
  end
  else begin
    linebuf := nameset.ToIOString;
    Writeln(outfile,linebuf);
    linebuf := the_location.ConvertAllToString;
    Writeln(outfile,linebuf);
  end;

  linebuf := GetNotesFile;
  Writeln(outfile,linebuf);
  // line 5
  if clusters<>nil then begin
    clusters.Delimiter := ';';
    linebuf := clusters.DelimitedText;
  end
  else linebuf := '';
  Writeln(outfile,linebuf);
  // line 6
  linebuf := MakeLine6;
  Writeln(outfile,linebuf);
  // writing stars
  if id <> 1 then begin
    for I := 0 to Length(new_components)-1 do begin
      new_components[I].WriteToTextFile(outfile);
    end;
  end;
  // done
  Result := False;
end;
//-----------------------------------------------------------
procedure StarSystem.ReadFromTextFile(var infile:TextFile);
var linebuf:string;   starcount:Integer;
    I:Integer;        cpllx:Real;
    cstar:StarInfo;
begin
  (* the first 4 lines *)
  Readln(infile,linebuf);
  if AnsiStartsStr('###',linebuf) then
    linebuf := copy(linebuf,4,length(linebuf)-3)
  else raise Exception.Create('Line does not start with ### !');
  starcount := FromLine1(linebuf);
  // line 2
  Readln(infile,linebuf);
  if id<>1 then begin
    SetupNameLocation(True,True);
    nameset.FromString(linebuf);
  end;
  // line 3
  Readln(infile,linebuf);
  if id<>1 then begin
    if not the_location.ConvertAllFromString(linebuf) then begin
      raise Exception.Create('Could not read location data! : '+ System_name);
    end;
  end;
  // line 4
  Readln(infile,linebuf);
  SetNotesFile(linebuf);
  // line 5
  Readln(infile,linebuf);
  if clusters<>nil then clusters.Free;
  if linebuf<>'' then begin
    clusters := TStringList.Create;
    clusters.Delimiter := ';';
    clusters.DelimitedText := linebuf;
  end;
  // line 6
  Readln(infile,linebuf);
  FromLine6(linebuf);
  (* reading in the star info *)
  if (id<>1) then begin
    SetLength(new_components,starcount);
    for I := 0 to starcount - 1 do begin
      new_components[I] := LoadStarBase(infile);
    end;
    (* after reading, I set up the estimators for each star *)
    for I := 0 to (starcount-1) do begin
      if (not new_components[I].isBrownDwarf) then begin
        cstar := StarInfo(new_components[I]);
        cpllx := GetComponentParallax(I);
        cstar.InitializeEstimation(cpllx);
      end;
    end;
  end;
  // done

end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Simple info methods *)
//------------------------------------------------
function StarSystem.GetId:Integer;
begin      Result := id;     end;
//---------------------------------------------------------
procedure StarSystem.SetId(inval:Integer);
begin      id := inval;      end;
//---------------------------------------------------------
(* returns true if there are no independent locations *)
function StarSystem.HasOneLocation:Boolean;
var arity,ardex:Integer;
begin
  Result := True;
  arity := GetCompC;
  if arity = 1 then Exit;
  Result := False;
  for ardex := 0 to (arity-1) do begin
    if new_components[ardex].HasLocation then Exit;
  end;
  Result := True;
end;
//-------------------------------------------------
// returns true if one star, or all component location sare binarycopy
function StarSystem.HasOneParallax:Boolean;
var arity,ardex:Integer;
    cloc:Location;
begin
  Result := True;
  arity := GetCompC;
  if arity = 1 then Exit;
  Result := False;
  for ardex := 0 to (arity-1) do begin
    cloc := new_components[ardex].GetLocation;
    if cloc <> nil then begin
      if not cloc.IsACopy then Exit;
    end;
  end;
  Result := True;
end;
//-------------------------------------------------
function StarSystem.IsBrownDwarf(starindex:Integer):Boolean;
begin
  Assert(starindex>0);
  Assert(starindex<=GetCompC);
  Result := new_components[starindex-1].isBrownDwarf;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(* Searching *)
//---------------------------------------------
// a star name/catalog name search function
function StarSystem.Search(infind:string; out where:Integer):Boolean;
var I,constell:Integer;
    TC:StarNames;
    buf:string;
begin
  Result := False;
  if id = 1 then Exit;
  constell := constellation;
  // we look thru the nameset of the system
  where := 0;
  Result := nameset.Search(infind,constell);
  if Result then Exit;
  // looking through the star names
  for I := 0 to MaxCInd do begin
    if not new_components[I].HasNames then Continue;
    TC := new_components[I].GetNames;
    Result := TC.Search(infind,constell);
    where := I+1;
    if Result then Exit;
  end;
  // system name and extra names
  Result := (System_name=infind);
  if Result then Exit;
  Result := AnsiContainsText(extra_names,infind);
  if Result then Exit;
  // variable star name
  buf := MakeVariableName();
  Result := (buf = infind);
  if Result then Exit;
  // bayer and flamsteed
  buf := MakeBayerName;
  Result := AnsiContainsText(buf,infind);
  if Result then Exit;
  buf := MakeFlamsteedName;
  Result := AnsiContainsText(buf,infind);
  if Result then Exit;
  // no result
  where := -1;
  Result := False;
end;
//----------------------------------------------------------
// checks if any of the stars in the system have the specified spectral type
function StarSystem.SearchSpectra(inspec:string):Boolean;
var I:Integer;
    sbuf:string;
begin
  Result := True;
  for I := 0 to MaxCInd do begin
    sbuf := new_components[I].SpectralClass;
    if AnsiContainsStr(sbuf,inspec) then Exit;
  end;
  Result := False;
end;
//------------------------------------------------------------
function StarSystem.HasCatName(cc:string):Boolean;
var I:Integer;
    names:StarNames;
begin
  // we first loop over substars
  Result := True;
  if MultiParts then begin
    for I := 0 to MaxCInd do begin
      if new_components[I].HasNames then begin
        names := new_components[I].GetNames;
        if names.HasCat(cc) then Exit;
      end;
    end;
  end;
  // we now do the main star list
  Result := nameset.HasCat(cc);
end;
//--------------------------------------------
function StarSystem.SearchNotes(tofind:string):Boolean;
var sdex:Integer;
begin
  Result := True;
  if AnsiContainsText(notes,tofind) then Exit;
  for sdex := 0 to High(new_components) do begin
    if AnsiContainsText(new_components[sdex].notes,tofind) then Exit;
  end;
  Result := False;
end;
//--------------------------------------------
function StarSystem.SearchParallaxSource(tofind:string):Boolean;
var ploc_src:string;
begin
  if (the_location = nil) then Result := False
  else begin
    ploc_src := the_location.source;
    Result := AnsiContainsText(ploc_src,tofind);
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* methods relating to duplicate systems, or splitting combining them *)
//-----------------------------------------------------------
function StarSystem.ExtractStarAsSystem(stardex,newid:Integer):StarSystem;
var nsys:StarSystem;
    extracted:NewStarBase;
    maxpos,compdex:Integer;
    rapos,decpos:Real;
begin
  // only allowed for multiple systems, stardex > 1
  Result := nil;
  if stardex = 1 then Exit;
  if stardex > GetCompC then Exit;
  //making the new system
  nsys := StarSystem.Create(newid);
  extracted := new_components[stardex-1];
  // setting the system location...
  if extracted.HasLocation then begin
    nsys.CopyLocation(extracted.GetLocation,False);
    extracted.ClearLocation;
  end
  else nsys.CopyLocation(the_location,False);
  // system names
  if extracted.HasNames then begin
    nsys.CopyNames(extracted.GetNames);
    extracted.ClearNames;
  end;
  // removing the star/brown dwarf from the array
  if GetCompC > stardex then begin
    maxpos := MaxCInd;
    for compdex := stardex to maxpos do new_components[compdex-1] := new_components[compdex];
    SetLength(new_components,maxpos);
  end
  else SetLength(new_components,stardex-1);
  // putting it in the new system
  extracted.Component := '';
  nsys.new_components[0].Free;
  nsys.new_components[0] := extracted;
  // locating the constellation for the new system
  rapos := nsys.GetLocation.GetDecimalRightAscension;
  decpos := nsys.GetLocation.GetDecimalDeclination;
  nsys.constellation := Const_Data.LocatePoint(rapos,decpos,nsys.GetLocation.Epoch);
  (* If there is only one star left, we move any names it has to the system
  level (old system, not new). *)
  PostRemovalCleanup;
  // done !
  Result := nsys;
end;
//-----------------------------------------------------------
function StarSystem.CatalogMatch(othersys:StarSystem):Boolean;
var cstar_name,other_name:StarNames;
    thisdex,otherdex:Integer;
begin
  // special bad cases
  Result := False;
  if othersys = nil then Exit;
  // checking at system name level
  Result := True;
  other_name := othersys.GetNames;
  if (nameset <> nil) and (other_name <> nil) then begin
    if nameset.AnyCatEquiv(other_name) then Exit;
  end;
  // checking components in this system vs the system names for the othr
  for thisdex := 1 to GetCompC do begin
    cstar_name := new_components[thisdex-1].GetNames;
    if cstar_name = nil then Continue;
    if other_name.AnyCatEquiv(cstar_name) then Exit;
  end;
  // checking system name vs components in the other system
  for otherdex := 1 to othersys.GetCompC do begin
    other_name := othersys.GetNewStar(otherdex).GetNames;
    if other_name = nil then Continue;
    if nameset.AnyCatEquiv(other_name) then Exit;
  end;
  // checking components vs component
  for thisdex := 1 to GetCompC do begin
    cstar_name := new_components[thisdex-1].GetNames;
    if cstar_name = nil then Continue;
    // inner loop
    for otherdex := 1 to othersys.GetCompC do begin
      other_name := othersys.GetNewStar(otherdex).GetNames;
      if other_name = nil then Continue;
      if cstar_name.AnyCatEquiv(other_name) then Exit;
    end;
  end;
  // if we get here, there are no matches
  Result := False;
end;
//-----------------------------------------------------
function StarSystem.SystemIDSummary:string;
var compdex:Integer;
    cnameset:StarNames;
begin
  Result := 'System ' + IntToStr(id) + ' ; ';
  Result += GetPreferredName + ' : ' + IntToStr(GetCompc);
  Result += 'Component';
  if GetCompC > 1 then Result += 's';
  Result += ' , Parallax ' + the_location.GetParallaxString(3,False) + ' mas';
  Result += sLineBreak + 'IDs : ' + nameset.ListOfCats + sLineBreak;
  if GetCompC > 1 then begin
    for compdex := 1 to GetCompC do begin
      Result += ' Component ' + IntToStr(compdex) + ' : ';
      cnameset := new_components[compdex-1].GetNames;
      if cnameset<>nil then Result += cnameset.ListOfCats;
      Result += sLineBreak;
    end;
  end;
  // done
end;
//------------------------------------------------------
function StarSystem.PosPMMatch(other:StarSystem; max_dist,max_pmangdiff,max_pmmagpdiff,max_pmmagdiff:Real):Boolean;
var zxo:Boolean;
    xstring:string;
begin
  // marginal edge case rejects
  Result := False;
  if the_location = nil then Exit;
  if id < 2 then Exit;
  if other = nil then Exit;
  if other.the_location = nil then Exit;
  if id = other.GetId then Exit;
  // checking for zero proper motion!
  xstring := GetPreferredName;
  zxo := (the_location.ProperMotionMagnitude = 0);
  if zxo then begin
    Result := False;
  end;
  // the real calculation
  Result := the_location.PositionProperMotionMatch(other.the_location,max_dist,
         max_pmangdiff,max_pmmagpdiff,max_pmmagdiff);
end;
//-------------------------------------------------------
function StarSystem.SystemLocationSummary:string;
begin
  Result := 'System ' + IntToStr(id) + ' ; ';
  Result += GetPreferredName + ' : ' + IntToStr(GetCompc) + sLineBreak;
  Result += 'RA: ' + the_location.RightAscensionHMS;
  Result += 'Dec: ' + the_location.DeclinationDMS + ' | ';
  Result += 'Parallax: ' + the_location.GetParallaxString(2,False) + '±';
  Result += the_location.GetParallaxErrString(2,False) + ' mas' + sLineBreak;
  Result += 'Proper Motion: ' + the_location.GetProperMotionMagStr(2) + 'mas/yr';
  Result += ' at ' + the_location.GetProperMotionAngleStr(1,False);
  Result += '°' + sLineBreak;
  // done
end;
//-------------------------------------------------------
function StarSystem.GetStarsAndEmpty():ComponentList;
var cstar:NewStarBase;
    cnames:StarNames;
    compdex,catdex:Integer;
    curcat:string;
begin
  Assert(id<>1);
  Result := ComponentList.Create(False);
  // things are simpler if there is only one component...
  if not MultiParts then begin
    cstar := new_components[0];
    // moving / copying names, and locations
    if nameset <> nil then cstar.CopyNames(nameset);
    cstar.InsertLocation(the_location);
    // finishing
    the_location := nil;
    Result.Add(cstar);
  end
  else begin
    for compdex := 0 to MaxCInd do begin
      cstar := new_components[compdex];
      // location
      if not cstar.HasLocation then cstar.CopyLocation(the_location,False)
      else cstar.GetLocation.MakeNotCopy;
      // nameset
      if nameset<>nil then begin
        cnames := cstar.MakeOrGetNames();
        nameset.TransferCatalogs(cnames,cstar.Component);
      end;
      // putting the component in the output list
      Result.Add(cstar);
    end;
  end;
  // common after math, get rid of the component list (the emptying part)
  SetLength(new_components,0);
end;
//-----------------------------------------------------------
function StarSystem.MergeIntoSystem(var otherSystem:StarSystem):Boolean;
var oclsit:ComponentList;
    addcount,ocount,addex:Integer;
    xlist:TStringList;
    oname:StarNames;
    testo:string;
begin
  // initial rejection tests...
  Result := False;
  if otherSystem = nil then exit;
  if id < 2 then Exit;
  if otherSystem.GetId < 2 then Exit;
  if id = otherSystem.GetId then Exit;
  // moving ahead ... getting the list of objects...
  oclsit := otherSystem.GetStarsAndEmpty();
  addcount := oclsit.Count;
  if addcount = 0 then begin
    FreeAndNil(oclsit);    Exit;
  end;
  // adding these objects to this system
  ocount := GetCompC;
  SetLength(new_components,ocount+addcount);
  for addex := 0 to (addcount-1) do begin
    new_components[ocount+addex] := oclsit[addex];
  end;
  FreeAndNil(oclsit);

  // copying over notes and other vital info
  AppndNote(sLineBreak + otherSystem.notes,True);
  if otherSystem.has_planets then has_planets := True;
  if otherSystem.aricns_data then aricns_data := True;
  if otherSystem.simbad_data then simbad_data := True;
  if otherSystem.has_problems then has_problems := True;

  // merging names and fictional data...
  AppendCommaNB(extra_names,otherSystem.extra_names);
  AppendCommaNB(fictional_names,otherSystem.fictional_names);
  AppendCommaNB(political_ownrs,otherSystem.political_ownrs);
  AppendCommaNB(description,otherSystem.description);
  AppendCommaNB(name2300ad,otherSystem.name2300ad);

  // moving names from system names if need be...
  if ocount = 1 then begin
    xlist := nameset.ExtractNonSystemCats;
    if (xlist <> nil) then begin
      oname := new_components[0].MakeOrGetNames;
      testo := xlist.DelimitedText;
      oname.SetMultipleCat(testo);
      xlist.Free;
    end;
    new_components[0].Component:= 'A';
    new_components[1].Component:= 'B';
  end;

  // finally
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Position and parallax manipulation *)
//--------------------------------------------
(* If any of the component stars have a location, we copy
location info that cannot be specified in the star location editor
(proper motion, radial velocity). *)
function StarSystem.CopyLocIfApplicable:Boolean;
var I:Integer;
begin
  if MultiParts then begin
    // looping thru the stars...
    for I := 0 to MaxCInd do begin
      if new_components[I].HasLocation then new_components[I].CopyMotion(the_location);
    end;
  end;
  Result := True;
end;
//---------------------------------------------------------

function StarSystem.TGAS2StarCheck(inlist:TGASList):Integer;
var corematch:Integer; starxname:StarNames;
begin
  // 0 : no 2 star match
  Result := 0;
  if GetCompC <> 2 then Exit;
  if inlist = nil then Exit;
  if inlist.Count <> 2 then Exit;
  corematch := 0;
  (* checking first star, the names are either system names or individual
  names for component 0. *)
  if inlist[0].NameMatch(nameset) then corematch := 1
  else if inlist[1].NameMatch(nameset) then corematch := 2
  else if new_components[0].HasNames then begin
    starxname := new_components[0].GetNames;
    if inlist[0].NameMatch(starxname) then corematch := 1
    else if inlist[1].NameMatch(starxname) then corematch := 2
    else Exit;
  end
  else Exit;
  // checking second star
  if new_components[1].HasNames then begin
    starxname := new_components[1].GetNames;
    if corematch = 1 then begin
      if inlist[1].NameMatch(starxname) then Result := corematch;
      Exit;
    end
    else if corematch = 2 then begin
      if inlist[0].NameMatch(starxname) then Result := corematch;
      Exit;
    end;
    Assert(False); // the program should never get here
  end
  else Exit;
end;
//------------------------------------------------------------
function StarSystem.ApplyDualTGAS(inlist:TGASList; first:Boolean):Boolean;
var xloc:Location;
begin
  // bad cases...
  Result := False;
  if inlist = nil then Exit;
  if GetCompC <> 2 then Exit;
  if inlist.Count<> 2 then Exit;
  // starting...
  if first then begin
    the_location.SetFromTGAS(inlist[0]);
    if (not new_components[1].HasLocation) then new_components[1].CopyLocation(the_location,False);
    xloc := new_components[1].GetLocation;
    xloc.SetFromTGAS(inlist[1]);
  end
  else begin
    the_location.SetFromTGAS(inlist[1]);
    if (not new_components[1].HasLocation) then new_components[1].CopyLocation(the_location,False);
    xloc := new_components[1].GetLocation;
    xloc.SetFromTGAS(inlist[0]);
  end;
  inlist[0].matched := True;
  inlist[1].matched := True;
  Result := True;
end;
//-----------------------------------------------------------
function StarSystem.SetTGASMatches(target:TGASCollection):Integer;
var cnameset:StarNames;
    clocat:Location;
    cname:string;
    sdex:Integer;
begin
  Result := 0;
  Assert(target<>nil);
  // checking if the primary location is TGAS
  if the_location.source = 'TGAS' then begin
     // looking for Hip or Tyc in the primary nameset
     cname := nameset.GetCat('Hip');
     if Length(cname)=0 then cname := nameset.GetCat('Tyc');
     if Length(cname)<>0 then begin
        // setting the match...
        if target.SetMatchByID(cname,True) then Inc(Result);
     end;
     // looking in the component nameset
     if new_components[0].HasNames then begin
       cnameset := new_components[0].GetNames();
       cname := cnameset.GetCat('Hip');
       if Length(cname)=0 then cname := cnameset.GetCat('Tyc');
       if Length(cname)<>0 then begin
          // setting the match...
          if target.SetMatchByID(cname,True) then Inc(Result);
       end;
     end;
  end;
  // possibly looking for more
  if GetCompC > 1 then begin
    // looping over individial components...
    for sdex := 1 to MaxCInd do begin
      if new_components[sdex].HasLocation then begin
         clocat := new_components[sdex].GetLocation();
         if clocat.IsACopy then Continue;
         if clocat.source = 'TGAS' then begin
           cnameset := new_components[sdex].GetNames;
           if cnameset = nil then Continue;
           // looking for Hip or Tyc in the nameset
           cname := cnameset.GetCat('Hip');
           if Length(cname)=0 then cname := cnameset.GetCat('Tyc');
           if Length(cname)<>0 then begin
              // setting the match...
              if target.SetMatchByID(cname,True) then Inc(Result);
           end;
         end;
      end;
    end;  // end for loop
  end;
  // done
end;
//-----------------------------------------------------------
function StarSystem.BinaryLocationUpdate:Boolean;
var cdex,ulocc:Integer;
    qloc:Location;
begin
  Result := False;
  // basic false conditions
  if id = 1 then Exit;
  if GetCompC = 1 then Exit;
  // looping thru components to update copied locations
  ulocc := 0;
  for cdex := 1 to MaxCInd do begin
    if new_components[cdex].HasLocation then begin
      qloc := new_components[cdex].GetLocation;
      if qloc.BinaryUpdate(the_location) then Inc(ulocc);
    end;
  end;
  // finally
  Result := ulocc > 0;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Other various mathods *)
//-----------------------------------------
// catalog renaming
function StarSystem.CatRenames(incats:TStringList):Integer;
var I:Integer;
    names:StarNames;
begin
  // basic checks
  Assert(incats<>nil);
  Assert((incats.Count mod 2) = 0);
  // we first loop over substars
  Result := 0;
  if MultiParts then begin
    for I := 0 to MaxCInd do begin
      if new_components[I].HasNames then begin
        names := new_components[I].GetNames;
        Result := Result + names.RenameCats(incats);
      end;
    end;
  end;
  // we now do the main star list
  Result := Result + nameset.RenameCats(incats);
end;
//-----------------------------------------------------------------
(* adds simbad data, this only really works for a single star *)
function StarSystem.AddSimbadData(stardex:Integer; simbad_in:SimbadData; fluxtempo:Boolean):Boolean;
var lok:Boolean;
    buffer:string;   radialv:Real;
    arity:Integer;   cstar:StarInfo;
    pllx:Real;
    xobj:NewStarBase;
begin
  // basic bad cases
  Result := False;
  if simbad_in = nil then Exit;
  arity := GetCompC();
  if stardex > arity then Exit;
  stardex := stardex -1;
  // some startup
  if (not new_components[stardex].isBrownDwarf) then begin
    cstar := StarInfo(new_components[stardex]);
  end
  else cstar := nil;
  // the following additions are not related to the new flux/temp/feh data object
  if (not fluxtempo) then begin
    // planet...
    if simbad_in.hasplanet then has_planets := True;
    // spectral type
    buffer := simbad_in.SpectralType;
    if Length(buffer)<>0 then new_components[stardex].SpectralClass := buffer;
    // radial velocity
    radialv := simbad_in.RadialVelocity;
    if (stardex = 0) and (radialv <> 0.0) then the_location.SetRadialVelocity(radialv);
    // star data only
    if (cstar<>nil) then StarSimbad(cstar,simbad_in);
    // names, o dear.
    buffer := Trim(simbad_in.Names);
    if Length(buffer)<>0 then begin
      if (arity = 1) then AppndNote(buffer,True)
      else new_components[stardex].AppndNote(buffer,True);
    end;
    (* Decision: for multiple stars, add the catalog ids to the notes instead
    of directly in the catalog id list. This allows manual sorting into
    'system catalog ids' vs 'catalog ids for individual stars'. *)
    if (arity > 1) then begin
      new_components[stardex].AppndNote(simbad_in.OutputCatalogCVS,True);
    end
    else begin
      nameset.SetMultipleCat(simbad_in.OutputCatalogCVS);
    end;
  end;
  // importing flux related data
  xobj := new_components[stardex];
  if xobj.fluxtemp = nil then xobj.fluxtemp := StarFluxPlus.Create;
  lok := xobj.fluxtemp.SetFromSimbad(simbad_in,xobj.isBrownDwarf);
  if (not lok) then FreeAndNil(xobj.fluxtemp);
  // updating estimates
  if cstar<>nil then begin
    pllx := GetComponentParallax(stardex);
    cstar.InitializeEstimation(pllx);
  end;
  // that should be it
  simbad_data := True;
  Result := True;
end;
//---------------------------------------------------------
function StarSystem.SetLoggFromSimbad(stardex:Integer; simbad_in:SimbadData):Boolean;
var arity:Integer;
    pllx:Real;
    xobj:NewStarBase;
    cstar:StarInfo;
begin
  // basic bad cases
  Result := False;
  if simbad_in = nil then Exit;
  arity := GetCompC();
  if stardex > arity then Exit;
  // some startup
  stardex := stardex -1;
  // importing log g
  xobj := new_components[stardex];
  if xobj.fluxtemp = nil then xobj.fluxtemp := StarFluxPlus.Create;
  xobj.fluxtemp.logg := simbad_in.logg;
  // for white dwarfs, we should update estimates
  if xobj.isWhiteDwarf then begin
    cstar := StarInfo(xobj);
    pllx := GetComponentParallax(stardex);
    cstar.NonSpectraChange(pllx);
  end;
  // that should be it
  simbad_data := True;
  Result := True;
end;
//---------------------------------------------------------
function StarSystem.ConvertToBrownDwarf(index:Integer):Boolean;
var new_browndwarf:BrownDwarfInfo;
    zstar:StarInfo;
    tnotes:string;
    mass_str,munc_str:string;
    mass,munc:Real;
    xloc:Location;
begin
  // bad cases
  Result := False;
  if (index<0) or (index>MaxCInd) then Exit;
  if new_components[index].isBrownDwarf then Exit;
  // creating the new object
  new_browndwarf := BrownDwarfInfo.Create;
  // copying location and name
  if new_components[index].HasNames then begin
    new_browndwarf.CopyNames(new_components[index].GetNames);
  end;
  if new_components[index].HasLocation then begin
    xloc := new_components[index].GetLocation;
    new_browndwarf.CopyLocation(xloc,xloc.IsACopy);
  end;
  // copying some basic data...
  new_browndwarf.Component := new_components[index].Component;
  new_browndwarf.SpectralClass := new_components[index].SpectralClass;
  // mass?
  zstar := StarInfo(new_components[index]);
  if zstar.ExtraInfo<>nil then begin
    zstar.GetMassAndMUnc(mass,munc);
    if mass<>0 then begin
      mass_str := Real2Str(mass*1047.56,2,1,True,False);
      munc_str := Real2Str(munc*1047.56,2,1,True,False);
      new_browndwarf.SetMassString(mass_str,munc_str);
    end;
  end;
  // extra flux object is just moved
  if zstar.fluxtemp<>nil then begin
    new_browndwarf.fluxtemp := zstar.fluxtemp;
    zstar.fluxtemp := nil;
  end;
  // Gaia magnitudes
  if zstar.dr2mags <> nil then begin
    new_browndwarf.dr2mags := zstar.dr2mags;
    zstar.dr2mags := nil;
  end;
  // notes
  tnotes := zstar.notes;
  tnotes := AnsiReplaceStr(tnotes,'Brown Dwarf?.','');
  if zstar.ValidVisualMagnitude then tnotes += ' V mag: ' + zstar.VisualMagnitudeString;
  new_browndwarf.notes:= Trim(tnotes);
  // getting rid of the old...
  new_components[index].Free;
  new_components[index] := new_browndwarf;
  Result := True;
end;
//---------------------------------------------------------
procedure StarSystem.UpdateEstimates;
var curstar:StarInfo;
    cpllx:Real;
    sxidx:Integer;
begin
  // looping thru the components
  for sxidx := 0 to MaxCInd do begin
    // skipping brown dwarfs
    if new_components[sxidx].isBrownDwarf then Continue;
    curstar := StarInfo(new_components[sxidx]);
    // parallax for
    cpllx := GetComponentParallax(sxidx);
    if curstar.estimator = nil then curstar.InitializeEstimation(cpllx)
    else curstar.NonSpectraChange(cpllx);
  end;
end;
//--------------------------------------------------------
function StarSystem.WriteEstimateData():Integer;
var curstar:StarInfo;
    sxidx:Integer;
    qnames:StarNames;
    oname:string;
begin
  Result := 0;
  // looping thru the components
  for sxidx := 0 to MaxCInd do begin
    // skipping brown dwarfs or estimator free stars
    if new_components[sxidx].isBrownDwarf then Continue;
    curstar := StarInfo(new_components[sxidx]);
    if curstar.estimator = nil then Continue;
    // getting a star label
    qnames := curstar.GetNames;
    if qnames <> nil then oname := qnames.GetDefaultCatalog
    else oname := GetComponentBaseLabel(False) + ' ' + curstar.Component;
    if Length(oname) = 0 then oname := GetComponentBaseLabel(False) + ' ' + curstar.Component;
    // writing
    curstar.estimator.WriteTest(oname);
    Inc(Result);
  end;
end;
//---------------------------------------------------------
function StarSystem.HipTycNames():TStringList;
var cnameset:StarNames;
    cname:string;
    sdex,startdex:Integer;
begin
  Result := TStringList.Create;
  // looking for Hip or Tyc in the primary nameset
  cname := nameset.GetCat('Hip');
  if Length(cname)=0 then cname := nameset.GetCat('Tyc');
  if Length(cname)<>0 then Result.Add(cname);
  // possibly looking for more
  if GetCompC > 1 then begin
    if Result.Count = 0 then startdex := 0
    else startdex := 1;
    // looping over individial namesets
    for sdex := startdex to MaxCInd do begin
      cnameset := new_components[sdex].GetNames;
      if cnameset = nil then Continue;
      cname := cnameset.GetCat('Hip');
      if Length(cname)=0 then cname := cnameset.GetCat('Tyc');
      if Length(cname)<>0 then Result.Add(cname);
    end;
  end;
  // we never return empty TStringList Objects
  if Result.Count = 0 then FreeAndNil(Result);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Gaia DR2 related *)
//------------------------------------
function StarSystem.GetStuffForMatching(stardex:Integer; out starloc:Location; out stnames:StarNames; out sysnames:StarNames):Boolean;
begin
  Result := False;
  if (stardex > GetCompC) or (stardex < 1) then Exit;
  sysnames := nameset;
  stnames := new_components[stardex-1].GetNames;
  if new_components[stardex-1].HasLocation then starloc := new_components[stardex-1].GetLocation
  else starloc := the_location;
  Result := True;
end;
//--------------------------------------------
function StarSystem.ApplyGaiaObject(stardex:Integer; inobj:GaiaDR2Star; conditional:Boolean):Boolean;
var cur_component:NewStarBase;
    cur_star:StarInfo;
    star_locat:Location;
    star_namez:StarNames;
    cok:Boolean;
    rap,decp:Real;
begin
  Result := False;
  if (stardex > GetCompC) or (stardex < 1) then Exit;
  if (inobj = nil) then Exit;
  if (not inobj.isValid) then Exit;
  // past the bad cases...
  cur_component := new_components[stardex-1];
  // location
  if (stardex = 1) then begin
    cok := the_location.SetFromGaiaDR2(inobj.astrometry,conditional);
    if (cok or (not conditional)) then BinaryLocationUpdate();
    // possibly updating the constellation
    if cok then begin
      rap := the_location.GetDecimalRightAscension;
      decp := the_location.GetDecimalDeclination;
      constellation := Const_Data.LocatePoint(rap,decp,the_location.Epoch);
    end;
  end else begin
    star_locat := cur_component.GetLocation;
    if (star_locat = nil) then begin
      star_locat := Location.Create;
      cur_component.InsertLocation(star_locat);
    end;
    cok := star_locat.SetFromGaiaDR2(inobj.astrometry,conditional);
  end;
  // magnitudes
  cur_component.dr2mags.Free;
  cur_component.dr2mags := inobj.mags.MakeCopy();
  // identifiers
  if (GetCompC = 1) then begin
    if (nameset = nil) then nameset := StarNames.Create;
    nameset.SetCat(inobj.GaiaID());
    nameset.SetMultipleCat(inobj.ids.IDStrings(false));
  end else begin
    star_namez := cur_component.MakeOrGetNames;
    star_namez.SetCat(inobj.GaiaID());
    star_namez.SetMultipleCat(inobj.ids.IDStrings(false));
  end;
  // TEff and perhaps variable type
  if (not cur_component.isBrownDwarf) and (inobj.extra <> nil) then begin
    cur_star := (cur_component as StarInfo);
    // most Gaia variable types are distant luminous types like Cepheids or RR Lyrae stars.
    if (inobj.extra.vartype <> '') then begin
      if inobj.extra.vartype = 'DSCT_SXPHE' then cur_star.VariableType := DETA_SCUTI
      else if inobj.extra.vartype = 'BYDra' then cur_star.VariableType := BY_DRACONIS
      else if cur_star.VariableType = NOT_VARIABLE then cur_star.VariableType := VARIABLE;
    end;
    // TEff
    if (inobj.extra.Teff > 0) then begin
      if cur_star.fluxtemp = nil then cur_star.fluxtemp := StarFluxPlus.Create;
      cur_star.fluxtemp.EffectiveTemp:= inobj.extra.Teff ;
    end;
  end;
  // done
  Result := True;
end;
//--------------------------------------------
function StarSystem.ApplyGaiaNameMags(stardex:Integer; inobj:GaiaDR2Star):Boolean;
var cur_component:NewStarBase;
    star_namez:StarNames;
    xloc:Location;
begin
  Result := False;
  if (stardex > GetCompC) or (stardex < 1) then Exit;
  if (inobj = nil) then Exit;
  if (not inobj.isValid) then Exit;
  // past the bad cases...
  cur_component := new_components[stardex-1];
  // magnitudes
  cur_component.dr2mags.Free;
  cur_component.dr2mags := inobj.mags.MakeCopy();
  // identifiers
  if (GetCompC = 1) then begin
    if (nameset = nil) then nameset := StarNames.Create;
    nameset.SetCat(inobj.GaiaID());
    nameset.SetMultipleCat(inobj.ids.IDStrings(false));
  end else begin
    star_namez := cur_component.MakeOrGetNames;
    star_namez.SetCat(inobj.GaiaID());
    star_namez.SetMultipleCat(inobj.ids.IDStrings(false));
  end;
  // parallax: sometime we add it to the *old* not used parallax
  if (stardex = 1) then  the_location.GaiaAddToOld(inobj.astrometry)
  else if (cur_component.HasLocation) then begin
    xloc := cur_component.GetLocation;
    xloc.GaiaAddToOld(inobj.astrometry);
  end;
  // done
  Result := True;
end;
//--------------------------------------------
function StarSystem.ContainsNonDR2Parallax():Boolean;
var cindex:Integer;
    cloc:Location;
begin
  Result := False;
  if id = 1 then Exit;
  Result := True;
  if the_location.source <> GAIA2_TAG then Exit;
  // looping
  if GetCompC > 1 then begin
    for cindex:= 1 to MaxCInd do begin
      cloc := new_components[cindex].GetLocation;
      if cloc <> nil then begin
        if cloc.source <> GAIA2_TAG then Exit;;
      end;
    end;
  end;
  // get here, all DR2!
  Result := False;
end;
//----------------------------------------------
function StarSystem.MissingDR2id():Boolean;
var cindex:Integer;
    cname:StarNames;
    cstar:StarInfo;
    stype:string;
begin
  Result := False;
  if id = 1 then Exit;
  // not missing because we have it
  if nameset.HasCat('Gaia DR2') then Exit;
  if (GetCompC = 1) then begin
    // not missing because cool brown dwarfs are too dim (as expected) for Gaia
    stype := new_components[0].SpectralClass;
    if StrStartswAny(stype,['T','Y','sdT','esdT','usdT']) then Exit;
    if (not new_components[0].isBrownDwarf) then begin
      // not missing because the star is too bright
      cstar := new_components[0] as StarInfo;
      if cstar.VisualMagnitude < 3 then Exit;
    end;
    // missing
    Result := True;
    Exit;
  end
  else begin
    for cindex:= 0 to MaxCInd do begin
      cname := new_components[cindex].GetNames;
      if cname <> nil then begin
        if cname.HasCat('Gaia DR2') then Continue;
      end;
      // not missing because cool brown dwarfs are too dim (as expected) for Gaia
      stype := new_components[cindex].SpectralClass;
      if StrStartswAny(stype,['T','Y','sdT','esdT','usdT']) then Continue;
      if (not new_components[cindex].isBrownDwarf) then begin
         // not missing because the star is too bright
         cstar := new_components[cindex] as StarInfo;
         if cstar.VisualMagnitude < 3 then Continue;
      end;
      // if we get here, then the Gaia DR2 designation is indeed 'missing'
      Result := True;
      Exit;
    end;
  end;
  // done, if we get here, the result is false;
  Result := False;
end;
//----------------------------------------------
function StarSystem.StarSummaryWhat(stardex:Integer):string;
var comp:NewStarBase;
    starc:StarInfo;
begin
  Assert((stardex >= 1) and (stardex <= GetCompC));
  comp := new_components[stardex-1];
  // name
  Result := GetPreferredName();
  if GetCompC > 1 then Result += ' ' + comp.Component;
  // spectral type
  Result += ' | ' + comp.SpectralClass + ' | ';
  if comp.isBrownDwarf then Result += 'Brown Dwarf'
  else begin
    starc := comp as StarInfo;
    Result += TypeLabels[Ord(starc.ClassifySpectralType())];
    if starc.VisualMagnitude < 90 then begin
      Result += ' | V:' + Trim(FloatToStrF(starc.VisualMagnitude,ffFixed,5,2));
    end;
  end;
end;
//----------------------------------------------
function StarSystem.StarSummaryIDPos(stardex:Integer):string;
var comp:NewStarBase;
    locx:Location;
    cname:StarNames;
begin
  Assert((stardex >= 1) and (stardex <= GetCompC));
  comp := new_components[stardex-1];
  // we start with identifiers
  Result := '';
  if GetCompC = 1 then cname := nameset
  else cname := comp.GetNames();
  if cname = nil then begin
    Result := 'SYSN: ';
    cname := nameset;
  end;
  Result += cname.LimitedListofCats(100) + sLineBreak;
  // position... get the location object we will use
  if (GetCompC = 0) or (stardex = 1) then locx := the_location
  else begin
    locx := comp.GetLocation;
    if locx = nil then locx := the_location;
  end;
  // added position and motion to the result
  Result += 'RA: ' + Trim(FloatToStrF(locx.GetDecimalRightAscension,ffFixed,9,5)) + '°';
  Result += ', Dec: ' + Trim(FloatToStrF(locx.GetDecimalDeclination,ffFixed,9,5)) + '°';
  Result += sLineBreak + 'Pllx: ' + locx.GetParallaxString(3,False) + '±';
  Result += locx.GetParallaxErrString(3,False) + sLineBreak;
  Result += 'Proper Motion: ' + locx.GetProperMotionMagStr(2) + ' at ';
  Result += locx.GetProperMotionAngleStr(1,False) + '°';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* true if the radial velocity is zero: we need that value set
for proper velocity and motion calculations *)
function StarSystem.RVZ:Boolean;
begin
  if id = 1 then Result := True
  else Result := (the_location.RadialVelocity = 0);
end;
//-----------------------------------------------------------
(* returns true if the distance between this system's primary
and the location given by otherpoints is >= maxdist *)
function StarSystem.LessThanLY(otherpoint:Location; maxdist:Real; targyear:Integer):Boolean;
var dist:Real;
begin
  // this is the sun
  if the_location = nil then begin
    Assert(otherpoint<>nil);
    if targyear < 0 then dist := otherpoint.GetDistance(False)
    else dist := otherpoint.GetDistanceAtYear(False,targyear);
  end
  else if otherpoint = nil then begin
    if targyear < 0 then dist := the_location.GetDistance(False)
    else dist := the_location.GetDistanceAtYear(False,targyear);
  end
  else begin
    dist := the_location.GetDistanceFrom(otherpoint,targyear);
  end;
  Result := (dist<=maxdist);
end;
//-----------------------------------------------------------
(* returns true if the number of catalog names is the nameset
is >= maxcname, often a sign we need to get more catnames *)
function StarSystem.NotEnoughCats(maxcname:Integer):Boolean;
var I:Integer;
begin
  if nameset = nil then I := 0
  else I:= nameset.CatalogCount;
  Result := (I<=maxcname);
end;
//---------------------------------------------------------
(* returns true if the parallax is untrused or not goodenough *)
function StarSystem.ParallaxUncertain:Boolean;
begin
  if id = 1 then Result := False
  else Result := the_location.uncertain;
end;
//---------------------------------------------
function StarSystem.UnknownSpectra:Boolean;
var sc:StarClass;
    strx:StarInfo;
    cdex:Integer;
begin
  Result := True;
  for cdex := 0 to MaxCInd do begin
    // quick unknown
    sc := new_components[cdex].ClassifySpectralType;
    if sc = UNKNOWN then Exit;
    // if that passes, we still check ...
    if (not new_components[cdex].isBrownDwarf) then begin
      strx := StarInfo(new_components[cdex]);
      if strx.estimator<>nil then begin
        if (not strx.estimator.SpectraOK) then Exit;
      end;
    end;
  end;
  Result := False;
end;

//---------------------------------------------
function StarSystem.HipOrTyc:Boolean;
var cdata:string;
begin
  Result := True;
  if nameset.GetCatValue('Hip',cdata) then Exit;
  if nameset.GetCatValue('Tyc',cdata) then Exit;
  Result := False;
end;
//---------------------------------------------
function StarSystem.HasMultiples:Boolean;
var cdex,cmin:Integer;
begin
  Result := True;
  for cdex := 0 to MaxCInd do begin
    cmin := new_components[cdex].MinPartCount;
    if cmin > 1 then Exit;
  end;
  Result := False;
end;
//----------------------------------------------
function StarSystem.LuminosityCloseToOne:Boolean;
var cdex:Integer;
    xstar:StarInfo;
    lumvalue:Real;
begin
  Result := True;
  for cdex := 0 to MaxCInd do begin
    if (not new_components[cdex].isBrownDwarf) then begin
      xstar := StarInfo(new_components[cdex]);
      if (not xstar.ValidVisualMagnitude) then Exit;
      lumvalue := xstar.estimator.VisualLuminosity;
      if (lumvalue <= 1.01) and (lumvalue >= 0.99) then Exit;
    end;
  end;
  Result := False;
end;
//-------------------------------------------------
function StarSystem.NoMassBrownDwarf:Boolean;
var cdex:Integer;
    xbrowndwarf:BrownDwarfInfo;
begin
  Result := True;
  for cdex := 0 to MaxCInd do begin
    if (new_components[cdex].isBrownDwarf) then begin
      xbrowndwarf := BrownDwarfInfo(new_components[cdex]);
      if xbrowndwarf.MassNotSet then Exit;
    end;
  end;
  Result := False;
end;
//------------------------------------------------
function StarSystem.BadCompLetter():Boolean;
var clet:string;
begin
  clet := new_components[0].Component;
  Result := (clet <> '') and (clet <> ' ') and (not AnsiStartsStr('A',clet));
end;
//----------------------------------------------
// returns true if component star locations do not have the same epochs...
function StarSystem.DifferingEpochs:Boolean;
var mepoch:EpochType;
    cdex:Integer;
    cloc:Location;
begin
  // trivial: only one star
  Result := False;
  if GetCompC = 1 then Exit;
  // otherwise, we get the system epoch and loop over the components
  Result := True;
  mepoch := the_location.Epoch;
  for cdex := 1 to (GetCompC-1) do begin
    cloc := new_components[cdex].GetLocation();
    if cloc = nil then Continue;
    if cloc.Epoch <> mepoch then Exit; // A true mismatch!
  end;
  // if we get here, no mismatch found
  Result := False;
end;
//------------------------------------------------
(* Looks for internal duplicates in catalog names *)
function StarSystem.InternalCatDups:Boolean;
var name1,name2:StarNames;
    cdexo,cdexi,cmax:Integer;
begin
  Result := False;
  if GetCompC = 1 then Exit;
  cmax := GetCompC -1;
  Result := True;
  // looping over using the system level
  if nameset <> nil then begin
    for cdexo := 0 to cmax do begin
      name2 := new_components[cdexo].GetNames;
      if nameset.AnyCatEquiv(name2) then Exit;
    end;
  end;
  // components vs component
  for cdexo := 0 to (cmax-1) do begin
    name1 := new_components[cdexo].GetNames;
    if name1 = nil then Continue;
    for cdexi := (cdexo+1) to cmax do begin
      name2 := new_components[cdexi].GetNames;
      if name1.AnyCatEquiv(name2) then Exit;
    end;
  end;
  // if we get here, no internal match found
  Result := False;
end;
//--------------------------------------------------
(* Calculates distances in LY between the components of the system, and returns
true if any of them are more than MaxDist. *)
function StarSystem.InternalDistanceMoreThan(const MaxDist:Real):Boolean;
var sloc,sloc2:Location;
    stardex_o,stardex_i:Integer;
    calcdist:Real;
begin
  Result := False;
  if HasOneLocation then Exit; // quick false case.
  // we first calculate and check distances between the system location and secondaries.
  Result := True;
  for stardex_o := 1 to MaxCInd do begin
    sloc := new_components[stardex_o].GetLocation();
    if sloc <> nil then begin
      calcdist := the_location.GetDistanceFrom(sloc,-1);
      if calcdist > MaxDist then Exit;
    end;
  end;
  // if we have more than 2 components, we check between them as well
  if GetCompC > 2 then begin
    for stardex_o := 1 to (MaxCInd-1) do begin
      sloc := new_components[stardex_o].GetLocation();
      if (sloc = nil) then Continue;
      for stardex_i := stardex_o to MaxCInd do begin
        sloc2 := new_components[stardex_i].GetLocation();
        if (sloc2 = nil) then Continue;
        calcdist := sloc.GetDistanceFrom(sloc2,-1);
        if calcdist > MaxDist then Exit;
      end;
    end;
  end;
  // done, if we get here, then no inner distance is more than MaxDist
  Result := False;
end;
//-----------------------------------------------------
(* Tries to calculate a Gaia derived V, and returns true if the difference
between it and the recorded V is more than MaxDiff. *)
function StarSystem.GaiaVCheck(const MaxDiff:Real):Boolean;
var stardex:Integer;
    cdr2:GaiaDR2Mags;
    vmagest,vdiff:Real;
    tres:Boolean;
    cstar:StarInfo;
    Jin:Currency;
begin
  Result := True;
  for stardex := 0 to MaxCInd do begin
    cdr2 := new_components[stardex].dr2mags;
    // checks to see if we can try and calculate V
    if cdr2 = nil then Continue;
    if new_components[stardex].isBrownDwarf then Continue;
    cstar := new_components[stardex] as StarInfo;
    if (cstar.VisualMagnitude > 90) then Continue;
    (* Here, we try and calcuate a v mag. using  menu transforms *)
    if new_components[stardex].fluxtemp = nil then Jin := 99.999
    else Jin := new_components[stardex].fluxtemp.J_mag;
    tres := GaiaToV(cdr2,Jin,vmagest);
    // if tres is false, skip
    if (not tres) then Continue;
    // comparing
    vdiff := Abs(vmagest - cstar.VisualMagnitude);
    if (vdiff > MaxDiff) then Exit;
  end;
  Result := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* clusters *)
//----------------------------------------------------
function StarSystem.InCluster(inclus:string):Boolean;
begin
  if ClusterCount = 0 then Result := False
  else Result := (clusters.IndexOf(inclus) <>-1);
end;
//----------------------------------------------------
function StarSystem.ClusterCount:Integer;
begin
  if clusters=nil then Result := 0
  else Result := clusters.Count;
end;
//----------------------------------------------------
procedure StarSystem.RemoveCluster(remclus:string);
var rpos:Integer;
begin
  rpos := clusters.IndexOf(remclus);
  if rpos >= 0 then begin
    clusters.Delete(rpos);
  end;   
end;
//----------------------------------------------------
function StarSystem.AddCluster(inclus:string):Integer;
begin
  if not InCluster(inclus) then begin
    // if clusters is empty, we make it
    if (clusters=nil) then begin
      clusters := TStringList.Create;
      clusters.Sorted := True;
    end;
    // we add
    clusters.Add(inclus);
  end;
  Result := clusters.Count;
end;
//----------------------------------------------------
procedure StarSystem.ClearClusters;
begin
  if (clusters<>nil) then begin
    clusters.Clear;
    clusters.Free;
    clusters := nil;
  end;             
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* destructor *)
destructor StarSystem.Destroy;
var I:Integer;
begin
  for I := 0 to MaxCInd do begin
    new_components[I].Free;
    new_components[I]:= nil;
  end;
  SetLength(new_components,0);
  nameset.Free;
  the_location.Free;
  inherited;
end;
//============================================================
function BrownDwarfSpectra(specstr:string):Boolean;
var firstchar:string;
begin
  Result := False;
  specstr := Trim(specstr);
  if Length(specstr)= 0 then Exit;
  firstchar := AnsiLeftStr(specstr,1);
  Result := AnsiContainsStr('LYT',firstchar);
end;
//=============================================================================
// a function that is meant to get a simbad object for a tgas entry
function TGASToSimbad(inpllx:TGASData):SimbadData;
var hipid,tycid,downurl:string;
    discardfail:Boolean;
begin
  Result := nil;
  if inpllx = nil then Exit;
  hipid := inpllx.GetHipparcos;
  tycid := inpllx.GetTycho2;
  if Length(hipid)<>0 then downurl := MakeSimbadIdLookupURL('Hip ' + hipid)
  else downurl := MakeSimbadIdLookupURL('Tyc ' + tycid);
  Result := GetSimbadDataURL(downurl,discardfail);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// a function that is meant to get a simbad entry for a Gaia object
function GaiaToSimbad(ingaiaid:GaiaDR2IDs):SimbadData;
var tmid,tycid,downurl:string;
    hipid:Integer;
    discardfail:Boolean;
begin
  Result := nil;
  if ingaiaid = nil then Exit;
  tmid := ingaiaid.TwoMASS;
  tycid := ingaiaid.Tycho2;
  hipid := ingaiaid.Hip;

  if Length(tycid)<>0 then downurl := MakeSimbadIdLookupURL('Tyc ' + tycid)
  else if Length(tmid)<>0 then downurl := MakeSimbadIdLookupURL('2MASS ' + tmid)
  else if hipid <> 0 then downurl := MakeSimbadIdLookupURL('Hip ' + IntToStr(hipid))
  else Exit;
  Result := GetSimbadDataURL(downurl,discardfail);
end;
//===========================================================================
constructor SysOutParams.Create;
begin
  idOffset := 10000;
  TargetYear := -1;
  is2300ad := False;
  splitsys := True;
  addpquest := True;
  allcaplum := 10;
end;
//----------------------------------------------
function SysOutParams.SetTargYear(src:string):Boolean;
var sc:Integer;
begin
  Result := False;
  if Length(src)=0 then Exit;
  Val(src,TargetYear,sc);
  Result := (sc=0);
end;
//----------------------------------------------
function SysOutParams.SetIDOffset(src:string):Boolean;
var sc:Integer;
begin
  Result := False;
  src := Trim(src);
  if Length(src)=0 then Exit;
  Val(src,idOffset,sc);
  Result := (sc=0);
end;
//----------------------------------------------
function SysOutParams.SetAllCapsLum(src:string):Boolean;
var sc:Integer;
begin
  Result := False;
  src := Trim(src);
  if Length(src)=0 then Exit;
  Val(src,allcaplum,sc);
  Result := (sc=0);
end;
//============================================================================
function SystemOutputter.GetCHVNames(sindex,maxlen:Integer):string;
var xname:StarNames;
begin
  xname := system.GetNameSet(sindex);
  Result := xname.LimitedListofCats(maxlen);
  Result := AnsiReplaceStr(Result,'/','|');
end;
//----------------------------------------
function SystemOutputter.CHViewSpecHelp(inspec:string; var rxlen:Integer):String;
begin
  Result := AnsiReplaceStr(inspec,'/','-');
  rxlen += Result.Length;
end;
//-----------------------------------------
function SystemOutputter.ToCHViewStar(sindex,targyear:Integer; plen:Integer):string;
var locatio:Location;
    starx:StarInfo;     bdwarfx:BrownDwarfInfo;
    sdata1,sdata2:EstimationParser;
    dist_val,distspec,buf1:string;
    constl_str,namez,coordstr:string;
    secondstar,splitres:Boolean;
    mass_str,xestname:string;
    runlen:Integer;
begin
  Assert(sindex>=0);
  Assert(sindex<=system.MaxCInd);
  // startup
  secondstar := False;
  Result := '/';
  runlen := plen -1;
  // preparing location
  if system.id > 1 then begin
    if not system.new_components[sindex].HasLocation then locatio := system.the_location
    else locatio := system.new_components[sindex].GetLocation;
    dist_val := locatio.GetDistanceStr(4,targyear,False);
  end else begin
    dist_val := '0';
  end;
  // constellation
  constl_str := constellations[(system.constellation-1)*3];
  // co-ordinates
  if system.id > 1 then begin
    if targyear < 0 then coordstr := locatio.GetXYZ_CSV(4)
    else coordstr := locatio.GetXYZatYear_CSV(targyear,4);
  end
  else coordstr := '0,0,0';
  // making the combo strings
  distspec := '/' + dist_val + '/';
  runlen += Length(distspec);
  runlen += 3 + Length(constl_str) + Length(coordstr);
  // combo2 := '/' + constl_str + '/' + namez + '/' + coordstr;

  // brown dwarf...
  if system.new_components[sindex].isBrownDwarf then begin
    bdwarfx := BrownDwarfInfo(system.new_components[sindex]);
    xestname := Trim(AnsiReplaceStr(system.GetPreferredName,'/',' '));
    xestname := xestname + ' ' + Trim(bdwarfx.Component);
    runlen += Length(xestname);
    Result += xestname;
    Result += distspec + CHViewSpecHelp(system.new_components[sindex].SpectralClass,runlen);
    Result += '/';
    Inc(runlen);
    // mass
    if bdwarfx.MassNotSet then xestname := '0.04'
    else xestname := bdwarfx.CHViewSafeMassInSuns;
    runlen += Length(xestname);
    Result += xestname;
    // names (the line length in CHView must be 255 or less bytes!)
    namez := GetCHVNames(sindex,253-runlen);
    // finishing
    Result += '/' + constl_str + '/' + namez + '/' + coordstr + sLineBreak;
  end
  // the sun...
  else if system.GetId = 1 then begin
    Result += 'Sol/0/G2V/1///0,0,0';
  end
  // normal stars
  else begin
    starx := StarInfo(system.new_components[sindex]);
    sdata1 := starx.estimator;
    sdata1.BackupMS();
    secondstar := (starx.MinPartCount >1);
    // second star splitting
    if secondstar then begin
       splitres := StarSplitGeneral(starx,locatio.ParallaxMAS,sdata2);
    end
    else begin
        sdata2 := nil;
        splitres := False;
    end;
    // writing what might be the only star...
    // component id, name, and dist/spec
    if (not secondstar) then buf1 := ' ' + starx.Component
    else buf1 := SplitComponent(starx.Component,True);
    xestname  := Trim(AnsiReplaceStr(system.GetPreferredName,'/','|'));
    xestname += ' ' + Trim(buf1);
    runlen += Length(xestname);
    Result += xestname;
    Result += distspec + CHViewSpecHelp(sdata1.StoredSpectra,runlen);
    if sdata1.LuminosityClass  = lGiant then begin
      Result += '*';
      Inc(runlen);
    end;
    Result += '/';
    Inc(runlen);
    // mass
    mass_str := starx.GetMassString;
    runlen += Length(mass_str);
    Result += mass_str;
    // names
    namez := GetCHVNames(sindex,255-runlen);
    Result += '/' + constl_str + '/' + namez + '/' + coordstr + sLineBreak;

    // writing the second star (if need be)
    if secondstar then begin
      // component id, name, and dist/spec
      buf1 := SplitComponent(starx.Component,False);
      xestname  := Trim(AnsiReplaceStr(system.GetPreferredName,'/','|'));
      xestname += ' ' + Trim(buf1);
      Result += '/' + xestname;
      Result += '/' + dist_val + '/';
      buf1 := sdata2.StoredSpectra;
      if buf1 = 'DA7??' then buf1 := 'DA??';
      Result += CHViewSpecHelp(buf1,runlen);
      // mass
      mass_str := sdata2.MassEstimateExport;
      Result += '/' + mass_str;
      Result += '/' + constl_str + '//' + coordstr + sLineBreak;

      // cleaning up afterwards...
      sdata2.Free;
      sdata1.RestoreMS(True);
    end;
    // end of star output
  end;
  // done
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* a helper function to generate ChView and Astrosynthesis
system labels *)
function SystemOutputter.GetOutputLabel(isCHView:Boolean):string;
var minc,maxc:Integer;   buf1:string;
    curstar:StarInfo;    highl:Boolean;
begin
  Assert(params<>nil);
  // we prepare the system name
  minc := system.GetMinCount;
  maxc := system.GetMaxCount;
  if maxc >1 then begin
    Str(maxc,buf1);
    buf1 := ' (' + Trim(buf1);
    if maxc <> minc then buf1 += '?';
    buf1 += ')';
  end;
  // uppercase luminosity
  highl := False;
  if params.allcaplum >= 0 then begin
    if system.new_components[0].isBrownDwarf then highl := False
    else if system.GetId = 1 then highl:= False
    else begin
      curstar := StarInfo(system.new_components[0]);
      highl := (curstar.estimator.VisualLuminosity) >= (params.allcaplum);
    end;
  end;
  // getting the name
  if params.is2300ad then begin
    if highl then Result := AnsiUpperCase(system.Get2300adName)
    else Result := system.Get2300adName;
  end
  else begin
    if highl then Result := AnsiUpperCase(system.GetPreferredName)
    else Result := system.GetPreferredName;
  end;
  if isCHView then Result := AnsiReplaceStr(Result,'/','|');
  // adding the arity
  if (maxc>1) then Result := Result + buf1;
  // question marks for the location
  if params.addpquest then Result := Result + ' ' + MakeUncStr(false);
end;
//-----------------------------------------------------------
(* we use thus string to encapsulate question marks concerning the
uncertainty in parallax *)
function SystemOutputter.MakeUncStr(opt:Boolean):string;
var lyrange:Real;
begin
  Result := '';
  if system.id = 1 then Exit;
  // version 1
  if opt then begin
    if system.the_location.GetLYRange(False) > 1 then Result := '?';
    if system.the_location.uncertain then Result := Result + '??';
  end
  // version 2
  else begin
    // parallax uncertainty
    lyrange := system.the_location.GetLYRange(True);
    if system.the_location.uncertain then Result += '?';
    if lyrange>0.3 then begin
      Result := '?';
      if lyrange>1 then Result := Result+'?';
      if lyrange>2 then Result := Result+'?';
      if lyrange>4 then Result := Result+'?';
      if lyrange>10 then Result := Result+'?';
    end;
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function SystemOutputter.AstroCatIDs(stardex:Integer):string;
var qcurr_names:StarNames;
    catlist:string;
begin
  // checks
  Assert(stardex>=0);
  Assert(stardex<system.GetCompC);
  catlist := '';
  // starting the result
  if system.GetCompC = 1 then begin
    if system.nameset <> nil then catlist := system.nameset.ListOfCats;
  end
  else begin
    if (not system.new_components[stardex].HasNames) then qcurr_names := nil
    else qcurr_names := system.new_components[stardex].GetNames;
    if qcurr_names <> nil then catlist := qcurr_names.ListOfCats;
  end;
  // finishing...
  if catlist = '' then Result := ''
  else Result := ' Catalog IDs : ' + catlist;
end;
//-------------------------------------------------
function SystemOutputter.AsOffsetLocat(indexw:Integer):string;
begin
  Assert(indexw>=0);
  if indexw = 0 then Result := '0,0,0'
  else if indexw = 1 then Result := '5,0,0'
  else if indexw = 2 then Result := '10,0,0'
  else Result := '15,0,0';
end;
//---------------------------------------------------------
function SystemOutputter.MakeMultiStarAstroLine(stardex:Integer):string;
var star_obj:StarInfo;
    xid,xname,starname1,starname2,buf2:string;
    sdata1,sdata2:EstimationParser;
    locat1,locat2:string;
    hloc,splitres:Boolean;
    theparallax:Double;
begin
  Assert(params<>nil);
  Assert(stardex>=0);
  Assert(stardex<system.GetCompC);
  Assert(not system.new_components[stardex].isBrownDwarf);
  Assert(system.new_components[stardex].MinPartCount>1);
  // starting off
  star_obj := StarInfo(system.new_components[stardex]);
  Str(system.id + params.idoffset,xid);
  sdata1 := star_obj.estimator;
  sdata1.BackupMS();
  // second star splitting
  theparallax := system.GetComponentParallax(stardex);
  splitres := StarSplitGeneral(star_obj,theparallax,sdata2);
  // the names
  if (not star_obj.GetProperName(xname,system.constellation)) then begin
    xname := system.GetComponentBaseLabel(params.is2300ad);
  end;
  starname1 := xname + ' ' + SplitComponent(star_obj.Component,True);
  starname2 := xname + ' ' + SplitComponent(star_obj.Component,False);
    // id and name
  Result += xid + ',';
  if (not system.new_components[stardex].GetProperName(xname,system.constellation)) then begin
    xname := system.GetComponentBaseLabel(params.is2300ad);
    if system.GetCompC > 1 then xname += ' ' + system.new_components[stardex].Component;
  end;
  Result += xname + ',';
  // the xyz location, which can be a bit tricky
  hloc := star_obj.HasLocation;
  // locations are in relative AU
  if (not hloc) then begin
    // here, the locations are just to separate the components
    locat1 := AsOffsetLocat(stardex);
    if stardex = 0 then locat2 := '2,0,0'
    else if stardex = 1 then locat2 := '7,0,0'
    else if stardex = 2 then locat2 := '12,0,0'
    else locat2 := '17,0,0';
  end
  else begin
    // here, we have an actual separate location
    locat1 := star_obj.GetLocation.MakeAstrosynXYZ_AUOffsetString(system.the_location,locat2);
  end;

  // producing Star 1
  if sdata1.LuminosityClass = lWhiteDwarf then Result := 'White Dwarf,'
  else Result := 'Star,';
  Result += xid + ',' + starname1 + ',' + locat1 + ',';
  // star 1 mass,radus,luminosity
  Result += star_obj.GetMassRadiusLuminosity + ',';
  // star1 spectral classification and color
  Result += sdata1.StoredSpectra + ',,';
  // field 12 (Notes)
  // finally, notes. I'll add distance and catalog ids for now
  Result += system.DistToSol(stardex);
  Result += AstroCatIDs(stardex);
  Result += sLineBreak;

  // Star 2 (the estimated star)
  Result += 'Star,' + xid + ',' + starname2 + ',' + locat2 + ',';
  // star 2 mass,radius,luminosity
  Result += sdata2.GetMassRadiusBLuminosity + ',';
  // star 2 spectral classification and color
  buf2 := sdata2.StoredSpectra;
  if buf2 = 'DA7??' then Result += 'DA??,,'
  else Result += buf2 + ',,';
  // field 12 (notes)
  Result += 'Binary with no data for components: many guesses here.';
  Result += sLineBreak;

  // cleaning up the split
  sdata1.RestoreMS(True);
  sdata2.Free;
end;
//-----------------------------------------------------------
function SystemOutputter.MakeAstrosynLine(compdata:string; stardex:Integer):string;
var xid,notusedl:string;
    xname:string;
    cloc:Location;
begin
  if system.new_components[stardex].ClassifySpectralType = WHITE_DWARF then Result := 'White Dwarf,'
  else Result := 'Star,';     // annoyingly, there is no special brown dwarf csv option
  Str(system.id + params.idoffset,xid);
  // id and name
  Result += xid + ',';
  if (not system.new_components[stardex].GetProperName(xname,system.constellation)) then begin
    xname := system.GetComponentBaseLabel(params.is2300ad);
    if system.GetCompC > 1 then xname += ' ' + system.new_components[stardex].Component;
  end;
  Result += xname + ',';
  // location
  if system.GetCompC > 1 then begin
    if (not system.new_components[stardex].HasLocation) then Result += AsOffsetLocat(stardex)
    else begin
      cloc := system.new_components[stardex].GetLocation;
      Result += cloc.MakeAstrosynXYZ_AUOffsetString(system.the_location,notusedl);
    end;
  end
  else if system.id = 1 then Result += '0,0,0'
  else begin
    if params.targetyear < 0 then Result += system.the_location.MakeAstrosynXYZString(4)
    else Result += system.the_location.GetAstrosynthesisXYZatYear_String(params.targetyear,4);
  end;
  // compdata (mass, diameter, luminosity)
  Result += ',' + compdata + ',';
  // spectra and color
  if system.id = 1 then Result += 'G2V,,'
  else Result += system.new_components[stardex].SpectralClass + ',,';
  // finally, notes. I'll add distance and catalog ids for now
  Result += system.DistToSol(stardex);
  Result += AstroCatIDs(stardex);
  Result += sLineBreak;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function SystemOutputter.MakeChView():string;
var I,plen:Integer;
begin
  Result := Trim(GetOutputLabel(True));
  plen := Length(Result);
  // we loop over the stars
  for I := 0 to system.MaxCInd do begin
    Result += ToCHViewStar(I,params.TargetYear,plen);
  end;
end;
//-----------------------------------------------------
function SystemOutputter.MakeAstroSynthesis():string;
var systemname:string;
    outidstr:string;  locationstr:string;
    aritydex:Integer; cstar:StarInfo;
    cbdwarf:BrownDwarfInfo;
    properties:string;
begin
  Result := '';
  // we prepare the system name and id string
  systemname := Trim(GetOutputLabel(False));
  Str(system.id + params.idoffset,outidstr);

  (* The output is very different depending on the number of stars, but each
  line always has 12 fields, separated by commas *)
  // if the system is a multiple one
  if system.GetMinCount>1 then begin
    // system location string
    if params.TargetYear < 0 then locationstr := system.the_location.MakeAstrosynXYZString(4)
    else locationstr := system.the_location.GetAstrosynthesisXYZatYear_String(params.TargetYear,4);
    // the first line is a 'system' one
    Result := 'Multiple,' + outidstr + ',' + systemname + ',' + locationstr;
    // no mass, radius, luminosity, spectral type, or colour
    Result := Result + ',,,,,,';
    // notes...
    Result += system.DistToSol(0) + 'Catalog IDs : ' + system.nameset.ListOfCats + sLineBreak;

    // now, output lines for each component
    for aritydex := 0 to system.MaxCInd do begin
      // brown dwarf
      if system.new_components[aritydex].isBrownDwarf then begin
        cbdwarf := BrownDwarfInfo(system.new_components[aritydex]);
        if cbdwarf.MassNotSet then properties := '0.04'
        else properties := cbdwarf.MassInSuns;
        properties += ',' + '0.1' + ',' + '0.0000001';
        Result += MakeAstrosynLine(properties,aritydex);
      end
      // single star
      else if system.new_components[aritydex].MinPartCount = 1 then begin
        cstar := StarInfo(system.new_components[aritydex]);
        properties := cstar.GetMassRadiusLuminosity;
        Result += MakeAstrosynLine(properties,aritydex);
      end
      // multiple star
      else begin
        Result += MakeMultiStarAstroLine(aritydex);
      end;
    end;
    // done with adding the various stars
  end
  // the single component version
  else begin
    // brown dwarf
    if system.new_components[0].isBrownDwarf then begin
      cbdwarf := BrownDwarfInfo(system.new_components[0]);
      if cbdwarf.MassNotSet then properties := '0.04'
      else properties := cbdwarf.MassInSuns;
      properties += ',' + '0.1' + ',' + '0.0000001';
      Result += MakeAstrosynLine(properties,0);
    end
    // single star
    else begin
      cstar := StarInfo(system.new_components[0]);
      properties := cstar.GetMassRadiusLuminosity;
      Result += MakeAstrosynLine(properties,0);
    end
  end;
  // done
end;
//==============================================================================

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end.
