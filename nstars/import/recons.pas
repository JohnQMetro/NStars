unit recons;

{$MODE Delphi}

interface

uses sysUtils, StrUtils, stardata, namedata, newlocation,NewStar, df_strings,
     StarDataBase, Utilities2;

type

ReconsStar = class
  protected
    num_string:string;
    gliese_num:string;
    component:string;
    arity:string;
    lhs:string;
    ra,dec:string;
    pm,parallax:string;
    spectral_type:string;
    absvismag:string;
    mass_est:string;
    name:string;
    othername:string;
  public
    constructor Create(instr:string);
end;
//**********************************************************************
ReconsMaker = class
  protected
    sysidc:Integer;
    nosystem:Boolean;
    starcount:Integer;
    stars:array of ReconsStar;
    // processing functions
    procedure RemovePlanets;
    procedure SortByComponent;
    // get gliese name
    function SystemGlieseName:string;
    function GetStarLHSName(index:Integer):string;
  public
    constructor Create(startid:Integer);
    function TestHeader(const inval:string):Boolean;
    function TestLine(const inval:string):Boolean;
    procedure AddLine(const inval:string);
    function MakeStarSystem:StarSystem;
    procedure Clear;
    destructor Destroy; override;

end;
//******************************************************************

function ImportRecons(infile:TFileName; rstartid:Integer):Boolean;


var
  rl_systnum:Integer;
  infilename:TFileName;
  ReconsList:array of StarSystem;


implementation
//************************************************************************
constructor ReconsStar.Create(instr: string);
var len:Integer;
begin

  num_string  :=Copy(instr,1,3);
  // names and component basics
  if Copy(instr,6,2)<>'GJ' then begin
    gliese_num := '';
    othername := Copy(instr,6,14);
    component := Copy(instr,20,1);
  end
  else begin
    gliese_num  :=Copy(instr,9,6);
    component   :=Copy(instr,17,2);
    othername   := '';
  end;
  arity       :=Copy(instr,22,2);
  lhs         :=Copy(instr,26,4);
  // location
  ra          :=Copy(instr,33,10);
  dec         :=Copy(instr,44,10);
  pm          :=Copy(instr,57,12);
  parallax    :=Copy(instr,74,15);
  // other info
  spectral_type:=Trim(Copy(instr,96,9));
  absvismag   :=Copy(instr,117,5);
  mass_est    :=Trim(Copy(instr,124,5));
  // regular name
  len := length(instr);
  name        :=Copy(instr,153,(len-153)+1);
  if AnsiStartsStr('</b>',name) then name:=''; 

end;
//**********************************************************************
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// processing functions
//----------------------------------------------------------
procedure ReconsMaker.RemovePlanets;
var buf:string;
    I,SC:Integer;
begin
  (* after looking at the recons list, we'll assume all planets
  are listed after the stars, and there is at least one star *)
  if (starcount<=1) then Exit;
  SC := starcount-1;
  for I := 1 to SC do begin
    buf := Trim(stars[I].component);
    if (buf<>'') and (buf[1]='P') then begin
      stars[I].Free;
      stars[I] := nil;
      Dec(starcount);
    end;
  end;
  SetLength(stars,starcount);
end;
//----------------------------------------------------------
procedure ReconsMaker.SortByComponent;
var I,J,ZC:Integer;
    temp:ReconsStar;
    buf,buf2:string;
begin
  if starcount<=1 then Exit;
  (* in some multiple star systems, some componets have no
  label in RECONS, so here we make one up: 'Z' *)
  ZC := 0;
  for I := 0 to starcount - 1 do  begin
    buf := Trim(stars[I].component);
    if buf='' then begin
      stars[I].component := 'Z';
      Inc(ZC);
    end
    else stars[I].component := buf;
  end;
  // we sort using selection sort
  for I := 0 to starcount - 2 do begin
    for J := I+1 to starcount - 1 do begin
      buf2 := stars[J].component;
      buf := stars[I].component;
      // we compare and swap if need be
      if buf2<buf then begin
        temp := stars[I];
        stars[I] := stars[J];
        stars[J] := temp;
      end;
      // done here, on to the next item
    end;
  end;
  // done!
  // relettering the components
  if ZC>0 then begin
    for I := (starcount-ZC) to (starcount-1) do begin
      stars[I].component := Chr(I+65);
    end;
  end;
  // we are done!
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// get gliese name
function ReconsMaker.SystemGlieseName:string;
var xreal:Real;
    sc:Integer;
begin
  // getting the value
  Result := Trim(stars[0].gliese_num);
  if Result = '' then Exit;
  // examination and formatting
  Val(Result,xreal,sc);
  Assert(sc<>0);
  if (xreal<1000) or (xreal>=9001) then Result := 'Gl ' + Result
  else Result := 'GJ ' + Result;
end;
//----------------------------------------------------------
function ReconsMaker.GetStarLHSName(index:Integer):string;
begin
  Result := Trim(stars[index].lhs);
  if result = '' then Exit;
  Result := 'LHS ' + Result;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor ReconsMaker.Create(startid:Integer);
begin
  sysidc := startid;
  nosystem := True;
  starcount := 0;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function ReconsMaker.TestHeader(const inval: string):Boolean;
const pstr = '____________________________________________________________________________';
begin
  Result := AnsiStartsStr(pstr,inval);
end;
//--------------------------------------------------------------------------
function ReconsMaker.TestLine(const inval:string):Boolean;
var qch:char;
    ftestr:string;
begin
  Result := False;
  if length(inval)<143 then Exit;
  qch := inval[1];
  if (qch<>' ') and (qch<>'1')  then Exit;
  ftestr := Trim(inval);
  if ftestr='' then Exit;  
  if AnsiStartsStr('SUN',ftestr) then Exit;  
  Result := True;
end;
//----------------------------------------------------------
procedure ReconsMaker.AddLine(const inval:string);
begin
  if nosystem then Inc(sysidc);  
  Inc(starcount);
  SetLength(stars,starcount);
  stars[starcount-1] := ReconsStar.Create(inval);
  nosystem := False;
end;
//----------------------------------------------------------
function ReconsMaker.MakeStarSystem:StarSystem;
var rspec,buf:string;         I,sc:Integer;
    starlocx:Location;        nstar:StarInfo;
    namestuff:StarName;       nbdwarf:BrownDwarfInfo;
    xval, vismag:Real;
    massc, bdmass:Real;       thestar:NewStarBase;
    buf1,buf2:string;         testx:Boolean;
begin
  // starting
  Result := StarSystem.Create(sysidc);
  // initial processing
  RemovePlanets;
  SortByComponent;
  // getting parts
  starlocx := Result.GetLocation;
  namestuff := Result.GetNames;
  // name handling
  buf := SystemGlieseName;
  if buf<>'' then namestuff.SetCat(buf)
  else namestuff.SetCat(stars[0].othername);
  // the system name
  Result.System_name := Trim(stars[0].name);
  if Result.System_name='' then begin
    buf := Trim(stars[0].othername);
    if buf<>'' then Result.System_name := buf
    else Result.System_name := SystemGlieseName;
  end;
  // setting toplevel information
  for I := 0 to starcount-1 do begin
    rspec := stars[I].spectral_type;
    // creating based on spectra
    if BrownDwarfSpectra(rspec) then begin
      nbdwarf := Result.AddBrownDwarf;
      nbdwarf.SpectralClass := rspec;
      // brown dwarf mass
      Val(Trim(stars[I].mass_est),massc,sc);
      Assert(sc=0);
      bdmass := 1047.56 * massc;
      nbdwarf.SetMassString(Real2Str(bdmass,1,2,True,False),'00.0');
      thestar := nbdwarf;
    end
    else begin
      nstar := Result.AddStar;
      nstar.SpectralClass := rspec;
      // visual magnitude
      Val(Trim(stars[I].absvismag),xval,sc);
      if sc = 0 then begin
        vismag := CalculateVisMagnitude(xval,starlocx.ParallaxMAS);
        nstar.SetVisualMagnitude(vismag);
      end;
      // star mass
      Val(Trim(stars[I].mass_est),massc,sc);
      if (sc = 0) and (massc <> 0) then begin
        nstar.ExtraInfo := StarExtraData.Create;
        buf := Real2Str(massc,3,1,True,False);
        nstar.ExtraInfo.SetMass(buf,' 0.000');
      end;
      thestar := nstar;
    end;
    // now, the location
    buf1 := Trim(stars[I].ra);
    buf2 := Trim(stars[I].dec);
    starlocx.SetPositionHMS(eJ2000,buf1,buf2);
    // parallax
    buf2 := Trim(stars[I].parallax);
    testx := ExtractFirstWord(buf2,buf1);
    Assert(testx);
    testx := starlocx.SetParallaxArcSeconds(buf1,buf2);
    starlocx.source := 'Recons 100';
    starlocx.uncertain := (4 >= starlocx.ParallaxErrorMAS);
    // proper motion
    buf2 := Trim(stars[I].pm);
    testx := ExtractFirstWord(buf2,buf1);
    Assert(testx);
    testx := starlocx.SetProperMotionArcSeconds(buf1,buf2);
    Assert(testx);

    // copying the location
    if I<>0 then thestar.InsertLocation(starlocx);
    // setting the smallamount of stellar info
    buf := GetStarLHSName(I);
    if buf <> '' then begin
      if (starcount<>1) then namestuff := thestar.MakeOrGetNames
      else namestuff := Result.GetNames;
      namestuff.SetCat(buf);
    end;
    // component id
    if (starcount<>1) then begin
      thestar.Component := stars[I].component[1];
    end;
    // preparing for the next loop
    if (I<(starcount-1)) then starlocx := Location.Create;
    // the nest loop
  end;
  // done
  Clear;
end;
//-----------------------------------------------------------------------
procedure ReconsMaker.Clear;
var I:Integer;
begin
  for I := 0 to starcount - 1 do stars[I].Free;
  nosystem := true;
  SetLength(stars,0);
  starcount := 0;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
destructor ReconsMaker.Destroy;
begin
  Clear;
  inherited;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(*  rl_systnum:Integer;
  infilename:TFileName;
  ReconsList:array of StarSystem;
*)
function ImportRecons(infile:TFileName; rstartid:Integer):Boolean;
var recons_in:Text;
    themaker:ReconsMaker;
    cline:string;
    insystem:Boolean;
    theaderc:Integer;
begin
  // we start
  infilename := infile;
  rl_systnum := 0;
  theaderc:=0;
  insystem := False;
  // file manipulations
  AssignFile(recons_in,infile);
  FileMode := fmOpenRead;
  Reset(recons_in);
  // recons maker
  themaker := ReconsMaker.Create(rstartid);
  // the loop
  while not Eof(recons_in) do begin
    Readln(recons_in,cline);
    // header testing
    if themaker.TestHeader(cline) then begin
      Inc(theaderc);
      Continue;
    end;
    if theaderc<>1 then Continue;
    // we are in the game here
    if not themaker.TestLine(cline) then begin
      // here, we make a system
      if insystem then begin
        Inc(rl_systnum);
        SetLength(ReconsList,rl_systnum);
        ReconsList[rl_systnum-1] := themaker.MakeStarSystem;
        insystem := False;
      end;
    end
    // we are in actual data
    else begin
      insystem := True;
      themaker.AddLine(cline);
    end;
    // next loop!
  end;
  // closing the file
  CloseFile(recons_in);
  FileMode := fmOpenReadWrite;
  (* once here, we should check to see if there is anything the the in
  themaker *)
  if insystem then begin
    Inc(rl_systnum);
    SetLength(ReconsList,rl_systnum);
    ReconsList[rl_systnum-1] := themaker.MakeStarSystem;
    insystem := False;
  end;
  // done!
  Result :=True;
end;
//**********************************************************************

end.
