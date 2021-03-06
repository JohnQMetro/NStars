unit collecdata;

{$MODE Delphi}

interface

uses SysUtils, StdCtrls, Classes, StrUtils, Dialogs, Controls, Forms, FileUtil,
  stardata, startproxy, cluster, Graphics, simbad, tgas, newlocation, df_strings,
  star_names, starestimator;

type

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
    procedure AddNewSystem(isbd:Boolean);
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
    function ReLinkCluster(inclus:ClusterData):Integer;
    function BoldAtIndex(theIndex:Integer):Boolean;
    function BoldAtFilteredIndex(theIndex:Integer):Boolean;
    function ColorAtFilteredIndex(theIndex:Integer):TColor;
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
    // filters
    procedure RadVIsZeroFilter;
    procedure RadVIsTooBig;
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
    procedure BadCompLet();
    procedure HasNonGaiaDR2Pllx();
    procedure MissingGaiaDR2id();
    procedure DifferingEpochs();
    procedure InternalCatalogDuplicates();
    procedure LargeInternalDistance(const maxDist:Real);
    procedure VDiffFromGaia(const maxDiff:Real);
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


function MakeSol:StarSystem;

var
  // interface info
  primaryl:StarList;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#@@@@@@@@@@@@
implementation
//============================================================================
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
procedure StarList.AddNewSystem(isbd:Boolean);
var xstr:string;
begin
  // setting the length of the mainlist
  Inc(idcount);
  Inc(maincount);
  SetLength(System_List,maincount);
  // creating the new star system
  System_List[maincount-1] := StarSystem.Create(idcount, isbd);
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
//---------------------------------------------
function StarList.ColorAtFilteredIndex(theIndex:Integer):TColor;
var fpst:string;
begin
  Result := clBlack;
  if theIndex > (fcount-1) then Exit;
  fpst := Filtered_List[theIndex].GetNewStar(1).SpectralClass;
  // M Type
  if StrStartswAny(fpst,['M','sdM','esdM','usdM']) then Result := $3C14DC
  // K type
  else if StrStartswAny(fpst,['K','sdK']) then Result := $008CFF
  // L type
  else if StrStartswAny(fpst,['L','sdL','esdL','usdL']) then Result := $2D52A0
  // G type
  else if StrStartswAny(fpst,['G','sdG']) or (theIndex = 0) then Result := $00D7FF
  // T Type
  else if StrStartswAny(fpst,['T','sdT','esdT']) then Result := $D670DA
  // F type
  else if StrStartswAny(fpst,['F','sdF']) then Result := $6BB7BD
  // A type
  else if StrStartswAny(fpst,['A','sdA']) then Result := $ED9564
  // Blue
  else if StrStartswAny(fpst,['O','B','W','sdO','sdB']) then Result := $E16941
  // carbon stars
  else if StrStartswAny(fpst,['C','dC','gC','R','N','S','MC','MS','dS']) then Result := $2222B2;
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
var names:StarNames;
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
    outputter:SystemOutputter;
begin
  // preparing the new file
  AssignFile(outfile,fnin);
  Rewrite(outfile);
  outputter := SystemOutputter.Create;
  outputter.params := oparams;
  // writing
  for I := 0 to maincount - 1 do begin
    outputter.system := System_List[I];
    chdata := outputter.MakeChView();
    DeUnicode(chdata);
    chdata := Trim(chdata);
    Writeln(outfile,StringToLatin1(chdata));
    Flush(outfile);
  end;
  CloseFile(outfile);
  FreeAndNil(outputter);
end;
//-----------------------------------------------------------
procedure StarList.SaveToAstrosynthesis(fnin:TFileName; oparams:SysOutParams);
var outfile:TextFile;
    asdata:string;
    I:Integer;
    outputter:SystemOutputter;
begin
  // preparing the new file
  AssignFile(outfile,fnin);
  Rewrite(outfile);
  outputter := SystemOutputter.Create;
  outputter.params := oparams;
  // writing
  for I := 0 to maincount - 1 do begin
    outputter.system := System_List[I];
    asdata := outputter.MakeAstroSynthesis();
    DeUnicode(asdata);
    asdata := Trim(asdata);
    Writeln(outfile,StringToLatin1(asdata));
    Flush(outfile);
  end;
  CloseFile(outfile);
  FreeAndNil(outputter);
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
    tnew := StarSystem.Create(1,False);
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
procedure StarList.RadVIsTooBig;
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].RVB then begin
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
var I:Integer;
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
var I:Integer;
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
var I:Integer;
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
var I:Integer;
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
var I:Integer;
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
//---------------------------------------------------------
procedure StarList.BadCompLet();
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].BadCompLetter then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.HasNonGaiaDR2Pllx();
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].ContainsNonDR2Parallax() then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
procedure StarList.MissingGaiaDR2id();
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].MissingDR2id() then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.DifferingEpochs();
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].DifferingEpochs then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//----------------------------------------------------------
procedure StarList.InternalCatalogDuplicates();
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].InternalCatDups() then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//--------------------------------------------
procedure StarList.LargeInternalDistance(const maxDist:Real);
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].InternalDistanceMoreThan(maxDist) then begin
      Inc(fcount);
      SetLength(Filtered_List,fcount);
      Filtered_List[fcount-1] := System_List[I];
    end;
  end;
  // setting the pointers
  SetAfterFilter;
end;
//--------------------------------------------
procedure StarList.VDiffFromGaia(const maxDiff:Real);
var I:Integer;
begin
  ClearFiltered;
  // copying over
  for I := 0 to maincount - 1 do begin
    if System_List[I].GaiaVCheck(maxDiff) then begin
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

//============================================================
function MakeSol:StarSystem;
begin
  Result := StarSystem.Create(1,False);
  // names
  Result.System_name := 'Sol';
  Result.extra_names := 'The Sun';
  Result.preferred := 1;
end;
//----------------------------------------------------------

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  primaryl := nil;
end.
