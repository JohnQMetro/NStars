unit cluster;

{$MODE Delphi}

interface

uses SysUtils,Classes,stardata,newlocation;

type

ClusterData = class
  protected
    cluster_systems:array of StarSystem;
    linked_systems:array of StarSystem;
    name:string;
  public
    constructor Create(newname:string);
    // name setting and getting
    function GetName:string;
    procedure SetName(instr:string);
    // getting counts
    function GetCount:Integer;
    function SharedCount:Integer;
    function LinkedCount:Integer;
    function StarMinCount:Integer;
    function StarMaxCount:Integer;
    function ComponentCount:Integer;
    function SpectrumCount(inspec:string):Integer;
    function SpectrumCountChar(inspec:Char):Integer;
    // adding and removing systems
    function AddSystem(insys:StarSystem):Boolean;
    function LoadAdd(insys:StarSystem):Boolean;
    function IncludesSystem(insys:StarSystem):Boolean;
    function RemoveSystem(insys:StarSystem):Boolean;
    (* test if a system is within a certain distance of
    the cluster systems, if so, add to linked_systems *)
    function IsLinked(tsys:StarSystem; dist:Real; targyear:Integer):Boolean;
    procedure ClearLinked;
    procedure SaveToCHview(fnin:TFileName; oparams:SysOutParams);
    // misc methods
    function MaxDistance(targyear:Integer):Real;
    function BuildReport:string;
    procedure MinMax(out minly,maxly:Real);
    // done
    destructor Destroy(); override;
end;

var cluster_list:array of ClusterData;
var linkdist_max:Real;

function MakeClusterNamesList:TStringList;
function ClustersLoadAdd(insys:StarSystem):Integer;
function ClusterNameExists(inname:string):Boolean;
function AddNewCluster(indata:string):Integer;

implementation
//****************************************************************************
(*    cluster_systems:array of StarSystem;
    linked_systems:array of StarSystem;
    name:string;
  public
  *)
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor ClusterData.Create(newname:string);
begin
  name := newname;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// name setting and getting
//--------------------------------------------------------------------
function ClusterData.GetName:string;
begin
  Result := name;
end;
//--------------------------------------------------------------------
procedure ClusterData.SetName(instr:string);
begin
  name := instr;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// getting counts
//--------------------------------------------------------------------
// returns the count of systems
function ClusterData.GetCount:Integer;
begin
  Result := Length(cluster_systems);
end;
//--------------------------------------------------------------------
// returns the number of systems that are shared with other clusters
function ClusterData.SharedCount:Integer;
var I,sc:Integer;
begin
  sc := 0;
  for I := 0 to GetCount - 1 do begin
    if cluster_systems[I].ClusterCount>1 then Inc(sc)
  end;
  Result := sc;
end;
//--------------------------------------------------------------------
// returns the number of systems in the linked systems list
function ClusterData.LinkedCount:Integer;
begin
  Result := Length(linked_systems);
end;
//--------------------------------------------------------------------
function ClusterData.StarMinCount:Integer;
var I,sc:Integer;
begin
  sc := 0;
  for I := 0 to GetCount - 1 do begin
    sc += cluster_systems[I].GetMinCount;
  end;
  Result := sc;
end;
//------------------------------------------------
function ClusterData.StarMaxCount:Integer;
var I,sc:Integer;
begin
  sc := 0;
  for I := 0 to GetCount - 1 do begin
    sc += cluster_systems[I].GetMaxCount;
  end;
  Result := sc;
end;
//------------------------------------------------
function ClusterData.ComponentCount:Integer;
var I,sc:Integer;
begin
  sc := 0;
  for I := 0 to GetCount - 1 do begin
    sc += cluster_systems[I].GetCompC;
  end;
  Result := sc;
end;
//--------------------------------------------------------------------
// returns the number of systems with a particular spectra
function ClusterData.SpectrumCount(inspec:string):Integer;
var I,sc:Integer;
begin
  sc := 0;
  for I := 0 to GetCount - 1 do begin
    if cluster_systems[I].SearchSpectra(inspec) then Inc(sc);
  end;
  Result := sc;
end;
function ClusterData.SpectrumCountChar(inspec:Char):Integer;
var I,sc:Integer;
    dstr:String;
begin
  sc := 0;
  for I := 0 to GetCount - 1 do begin
    if cluster_systems[I].SearchSpectra(inspec) then Inc(sc);
  end;
  Result := sc;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// adding and removing systems
//--------------------------------------------------------------------
// tries to add a system, return true if added, false if already included
function ClusterData.AddSystem(insys:StarSystem):Boolean;
begin
  Assert(insys<>nil);
  if IncludesSystem(insys) then Result := False
  else begin
    SetLength(cluster_systems,GetCount+1);
    cluster_systems[GetCount-1] := insys;
    insys.AddCluster(name);
    Result := True;
  end; 
end;
//------------------------------------------------------------------
(* Similar to AddSystem, but add only if IncludesSystem is true.
Intended for use while loading data *)
function ClusterData.LoadAdd(insys:StarSystem):Boolean;
begin
  if IncludesSystem(insys) then begin
    SetLength(cluster_systems,GetCount+1);
    cluster_systems[GetCount-1] := insys;
    insys.AddCluster(name);
    Result := True;
  end;
end;
//--------------------------------------------------------------------
// returns true if te system is within this cluster
function ClusterData.IncludesSystem(insys:StarSystem):Boolean;
begin
  Assert(insys<>nil);
  Result := insys.InCluster(name);
end;
//--------------------------------------------------------------------
// removes a system from the list
function ClusterData.RemoveSystem(insys:StarSystem):Boolean;
var I,J:Integer;
    found:Boolean;
begin
  Assert(insys<>nil);
  found := False;
  if not IncludesSystem(insys) then Result := False
  // the checking loop
  else begin
    for I := 0 to GetCount - 1 do begin
      if cluster_systems[I] = insys then begin
        found := True;
        Break;
      end;
    end;
    Assert(found);
    // we have located the element
    cluster_systems[I] := nil;
    insys.RemoveCluster(name);
    // reorganizing the list
    for J := I+1 to GetCount - 1 do begin
      cluster_systems[J-1] := cluster_systems[J];
    end;
    SetLength(cluster_systems,GetCount-1);
    Result := True;
  end; 
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* test if a system is within a certain distance of
the cluster systems, if so, add to linked_systems *)
function ClusterData.IsLinked(tsys:StarSystem; dist:Real; targyear:Integer):Boolean;
var I:Integer;
    found:Boolean;
    proxyloc:Location;
begin
  // we do not check systems already part of the cluster
  if IncludesSystem(tsys) then Result := False
  else begin
    // check to see if we have already linked this system
    found := false;
    for I := 0 to LinkedCount - 1 do begin
      found := (linked_systems[I]=tsys);
      if found then Break;
    end;
    // if we have not, we check the distance
    if not found then begin
      proxyloc := tsys.GetLocation;
      for I := 0 to GetCount - 1 do begin
        if cluster_systems[I].LessThanLY(proxyloc,dist,targyear) then begin
          SetLength(linked_systems,LinkedCount+1);
          linked_systems[LinkedCount-1] := tsys;
          Result := True;
          Break;
        end;
      end;
    end
    else Result := False;
  end;  
end;
//--------------------------------------------------------------------
// emptys the linked list
procedure ClusterData.ClearLinked;
begin
  SetLength(linked_systems,0);
end;
//--------------------------------------------------------------------
procedure ClusterData.SaveToCHview(fnin:TFileName; oparams:SysOutParams);
var outfile:Text;
    chdata:string;
    I,csc,lsc:Integer;
    outputter:SystemOutputter;
begin
  // preparing the new file
  AssignFile(outfile,fnin);
  Rewrite(outfile);
  csc := Length(cluster_systems);
  outputter := SystemOutputter.Create;
  outputter.params := oparams;
  // writing main systems
  for I := 0 to csc - 1 do begin
    outputter.system := cluster_systems[I];
    chdata := outputter.MakeChView();
    chdata := Trim(chdata);
    Writeln(outfile,chdata);
    Flush(outfile);
  end;
  // writing linked systems
  lsc := Length(linked_systems)-1;
  for I := 0 to lsc do begin
    outputter.system := linked_systems[I];
    chdata := outputter.MakeChView();
    chdata := Trim(chdata);
    Writeln(outfile,chdata);
    Flush(outfile);
  end;
  CloseFile(outfile);
  FreeAndNil(outputter);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// misc methods
//-----------------------------------------------------------------------
// calculates the biggest distance
function ClusterData.MaxDistance(targyear:Integer):Real;
var I,J:Integer;
    maxdist,cdist:Real;
    proxy1,proxy2:Location;
begin
  // starting up
  maxdist := 0;
  // the outer loop
  for I := 0 to GetCount - 2 do begin
    proxy1 := cluster_systems[I].GetLocation;
    for J := I+1 to GetCount - 1 do begin
      proxy2 := cluster_systems[J].GetLocation;
      cdist := proxy2.GetDistanceFrom(proxy1,targyear);
      if cdist>maxdist then maxdist := cdist;
    end;
  end;
  // done
  Result := maxdist;
end;
//--------------------------------------------------------------------------
function ClusterData.BuildReport:string;
var buf,qname:string;
    tval,I,J,qcount:Integer;
    qval1,qval2:Real;
    before:Boolean;
begin
  // the main name, and a list of systems
  Result := name + #13#10 + 'Systems: ';
  for I := 0 to GetCount - 1 do begin
    Result := Result + cluster_systems[I].GetPreferredName;
    if I<>(GetCount-1) then Result := Result + ', ';
  end;
  Result := Result + #13#10;
  // system and star acount
  tval := GetCount;
  Str(tval,buf);
  Result := Result + Trim(buf)+ ' Systems, ';
  tval := StarMinCount;
  Str(tval,buf);
  Result := Result + Trim(buf) + ' Stars/Brown Dwarfs.' + #13#10;
  // count of star types
  before := False;
  tval := SpectrumCount('A');
  if tval>0 then begin
      Str(tval,buf);
      Result := Result + Trim(buf) + ' A type star';
      if (tval>1) then Result := Result + 's';
      Before := True;
  end;
  tval := SpectrumCount('F');
  if tval>0 then begin
      if Before then Result := Result + ', ';      
      Str(tval,buf);
      Result := Result + Trim(buf) + ' F type star';
      if (tval>1) then Result := Result + 's';
      Before := True;
  end;
  tval := SpectrumCount('G');
  if tval>0 then begin
      if Before then Result := Result + ', ';      
      Str(tval,buf);
      Result := Result + Trim(buf) + ' G type star';
      if (tval>1) then Result := Result + 's';
      Before := True;
  end;
  tval := SpectrumCount('K');
  if tval>0 then begin
      if Before then Result := Result + ', ';      
      Str(tval,buf);
      Result := Result + Trim(buf) + ' K type star';
      if (tval>1) then Result := Result + 's';
      Before := True;
  end;
  Result := Result + #13#10#13#10;
  // distances
  tval := SharedCount;
  Str(tval,buf);
  Result := Result + Trim(buf)+ ' Systems shared with other clusters.'#13#10;
  MinMax(qval1,qval2);
  Str(qval1:5:2,buf);
  Result := Result + 'Minimum Distance: ' + buf + ' LY' + #13#10;
  Str(qval2:5:2,buf);
  Result := Result + 'Maximum Distance: ' + buf + ' LY' + #13#10;
  qval1 := MaxDistance(-1);
  Str(qval1:5:2,buf);
  Result := Result + 'Maximum intra-cluster distance: ' + buf + ' LY' + #13#10;
  // next up is links and linked systems
  Result := Result + #13#10 + 'Linked Systems:'#13#10;
  // we list the linked systems by cluster
  for I := 0 to Length(cluster_list) - 1 do begin
    // we start
    qname := cluster_list[I].GetName;
    if qname=name then Continue;    
    qcount := 0;
    buf := '';
    // looping the the linked items
    for J := 0 to LinkedCount - 1 do begin
      if linked_systems[J].InCluster(qname) then begin
        // the prefix string
        if qcount=0 then buf := qname + ' : '
        else buf := buf + ', ';
        Inc(qcount);
        // adding the name
        buf := buf + linked_systems[J].GetPreferredName;
      end;
    end;
    // we only add the return line if qcount is positive
    if (qcount>0) then buf := buf + #13#10;
    Result := Result + buf;
  end;
  // finally, we list liked systems of no cluster
  buf := '';
  qcount :=0;
  Result := Result + #13#10;
  // the loop
  for J := 0 to LinkedCount - 1 do begin
    if linked_systems[J].ClusterCount=0 then begin
      // the prefix string
      if qcount=0 then buf := '(No Cluster) : '
      else buf := buf + ', ';
      Inc(qcount);
      // adding the name
      buf := buf + linked_systems[J].GetPreferredName;
    end;
  end;
  Result := Result + buf;
end;
//-------------------------------------------------------------------------
procedure ClusterData.MinMax(out minly,maxly:Real);
var I:Integer;
    xloc:Location;
    dist:Real;
begin
  minly:=999999;
  maxly:=0;
  // looping
  for I := 0 to GetCount - 1 do begin
    xloc := cluster_systems[I].GetLocation;
    if xloc = nil then dist := 0
    else dist := xloc.GetDistance(False);
    if dist<minly then minly := dist;
    if dist>maxly then maxly := dist;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
destructor ClusterData.Destroy();
var I:Integer;
begin
  inherited;
  for I := 0 to GetCount - 1 do begin
    cluster_systems[I].RemoveCluster(name);
  end;
  SetLength(cluster_systems,0);
  SetLength(linked_systems,0);
end;
//****************************************************************************
function MakeClusterNamesList:TStringList;
var I:Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Length(cluster_list) - 1 do begin
    Result.Add(cluster_list[I].GetName);
  end;
end;
//***************************************************************************
function ClustersLoadAdd(insys:StarSystem):Integer;
var I,cll:Integer;
begin
  // starting
  cll := Length(cluster_list);
  Result :=0;
  // we loop thru the clusters
  for I := 0 to cll - 1 do begin
    if cluster_list[I].LoadAdd(insys) then Inc(Result);
  end; 
end;
//****************************************************************************
function ClusterNameExists(inname:string):Boolean;
var tlist:TStringList;
    oindex:Integer;
begin
  tlist := MakeClusterNamesList;
  oindex := tlist.IndexOf(inname);
  Result := (oindex >= 0);
  tlist.Free;
end;
//***************************************************************************
function AddNewCluster(indata:string):Integer;
var tlen:Integer;
    nclu:ClusterData;
begin
  tlen := Length(cluster_list);
  nclu := ClusterData.Create(indata);
  SetLength(cluster_list,tlen+1);
  cluster_list[tlen] := nclu;
  Result := tlen+1;
end;
//****************************************************************************
end.
