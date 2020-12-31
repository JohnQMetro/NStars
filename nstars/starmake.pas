unit starmake;

{$mode delphi}
(* Moving some of the star making functions from collecdata to here. *)
interface

uses Classes, SysUtils, newImports, tgas, stardata, NewStar, utilities2,
              simbad, extraimports, newlocation, collecdata, constellation,
              stardatabase, starext2, star_names, StrUtils, df_strings;
//*************************************************************************
type

HandleImportParams = class
  public
    xparams:ImportParameters;
    minpllx,maxpllxe:Real;
    percentpllxe:Boolean;
    picksysn:Boolean;
    function TestParallax(inpllx:ImportedData):Boolean;
end;

//--------------------------------------------------------------------
function BrownDwarfSpT(to_test:string):Boolean;
function UsedSpt(xparams:ImportParameters; inspt,simspt:string):string;

function TGASToStarSystem(inpllx:TGASData):StarSystem;

function AddDataToStar(inpllx:ImportedData; thestar:NewStarBase; xparams:ImportParameters):Boolean;
function UpdateExistingImD(xparams:ImportParameters; inpllx:ImportedData;
                      depoch:EpochType; targetIndex:Integer):Boolean;
function AddNewImportedSystem(xparams:ImportParameters; inpllx:ImportedData;
                      insimb:SimbadData; picksysn:Boolean; depoch:EpochType):Boolean;
function AddNewSystemNoSimbad(xparams:ImportParameters; inpllx:ImportedData;
                      picksysn:Boolean; depoch:EpochType):Boolean;
function HandleNewImported(inpllx:ImportedData; const_params:HandleImportParams;
                      out new_system:Boolean):Boolean;

function HandleNewImportedAlt(inpllx:ImportedData; const_params:HandleImportParams;
                      out new_system:Boolean):Boolean;

function HandleSN35PhotometryImport(indata:Sn35Photometry; out notfound:Boolean):Boolean;

//*************************************************************************
implementation
//=========================================================================
(*minpllx,maxpllxe:Real; *)
function HandleImportParams.TestParallax(inpllx:ImportedData):Boolean;
begin
  Result := False;
  if inpllx = nil then Exit;
  if inpllx.pllx < minpllx then Exit;
  if inpllx.pllx_err > maxpllxe then Exit;
  Result := True;
end;
//=========================================================================
function BrownDwarfSpT(to_test:string):Boolean;
var psp,inlen:Integer;
    prefix:string;
    bchar:char;
begin
  Result := False;
  inlen := Length(to_test);
  psp := PosSet('LTY',to_test);
  if (psp <= 0) then Exit;
  bchar := to_test[psp];
  Result := True;
  // starts with T or Y, yes
  if (psp = 1) and ((bchar = 'T') or (bchar = 'Y')) then Exit
  // starts with L, check if L6 or redder (stellar limit around L2.5 to L5)
  else if (psp = 1) and (inlen > 1) then begin
    bchar := to_test[2];
    if (bchar = '6') or (bchar = '7') or (bchar = '8') or (bchar = '9') then Exit
    else Result := False;
  end
  // stars with something else
  else begin
      prefix := copy(to_test,1,psp-1);
      if not StrEqAny(prefix,['sd','esd','usd','??','?','??/']) then Result := False
      else begin
          // after the valid prefix, starts with T or Y, yes
          if ((bchar = 'T') or (bchar = 'Y')) then Exit
          else if (inlen <= psp) then Result := False
          // L6 to L9 after prefix is brown dwarf
          else begin
              bchar := to_test[psp+1];
              if (bchar = '6') or (bchar = '7') or (bchar = '8') or (bchar = '9') then Exit
              else Result := False;
          end;
      end;
  end;

end;
//----------------------------------------------------------------
function UsedSpt(xparams:ImportParameters; inspt,simspt:string):string;
var ipspt:Boolean;
begin
  ipspt := xparams.useSpT or xparams.useSpT_emp;
  if not ipspt then Result := simspt
  else if (inspt = '') or (inspt = '??') or (inspt = '?') then Result := simspt
  else Result := inspt;
end;

//-------------------------------------------------------------------------
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
  Result := StarSystem.Create(newid,False);
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
    cspt:string;
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
  end else if xparams.useSpT_emp then begin
      cspt := thestar.SpectralClass;
      if (inpllx.stype<>'') and ((cspt = '') or (cspt = '??')) then
                                       thestar.SpectralClass := inpllx.stype;
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
    if xparams.useJHK_Flux then begin
      if thestar.fluxtemp = nil then thestar.fluxtemp := StarFluxPlus.Create;
      if inpllx.Jmag < 99 then thestar.fluxtemp.J_mag:= inpllx.Jmag;
      if inpllx.Hmag < 99 then thestar.fluxtemp.H_mag:= inpllx.Hmag;
      if inpllx.Ksmag < 99 then begin
        thestar.fluxtemp.K_mag := inpllx.Ksmag;
        thestar.fluxtemp.K_err := inpllx.KsmagE;
      end;
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
  Result := locat.SetFromImported(inpllx,depoch,xparams.pllx_sourceid,False,xparams.use_altid);
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
var  currsys:StarSystem;        newid,x:Integer;
     locat:Location;            nametest:StarNames;
     compptr:NewStarBase;       isbd:Boolean;
     rapos,decpos:Real;         usespt:string;
begin
  Result := False;
  newid := primaryl.NextID;
  usespt := UsedSpt(xparams,inpllx.stype,insimb.SpectralType);
  isbd := BrownDwarfSpT(usespt);
  currsys := StarSystem.Create(newid,isbd);
  currsys.AddSimbadData(1,insimb,False);
  // setting location and constellation
  locat := currsys.GetLocation;
  if xparams.useSimbadProperMotion then begin
    inpllx.SetProperMotionPartsM(insimb.pmra,insimb.pmdec);
  end;
  locat.SetFromImported(inpllx,depoch,xparams.pllx_sourceid,True,xparams.use_altid);
  if xparams.useSimbadLocation then begin
    locat.SetPositionHMS(eJ2000,insimb.ra_coord,insimb.dec_coord);
  end;
  rapos := locat.GetDecimalRightAscension;
  decpos := locat.GetDecimalDeclination;
  if xparams.useRadv then begin
    if inpllx.radialv < 100000 then locat.SetRadialVelocity(inpllx.radialv);
  end;
  currsys.constellation := Const_Data.LocatePoint(rapos,decpos,locat.Epoch);
  // adding some names
  nametest := currsys.MakeOrGetNames();
  if (inpllx.nameids <> nil) and (inpllx.nameids.Count > 0) then begin
    for x := 0 to (inpllx.nameids.Count - 1) do begin
      nametest.SetCat(inpllx.nameids[x]);
    end;
  end;

  // perhaps picking the system name...
  if picksysn and currsys.AreNotesEmpty then begin
    nametest := currsys.GetNames;
    if nametest <> nil then begin
      if nametest.CatalogCount < 4 then begin
         currsys.System_name:= nametest.GetRandomCat;
      end;
    end;
  end;
  // adding data
  compptr := currsys.GetNewStar(1);
  AddDataToStar(inpllx,compptr,xparams);
  compptr.SpectralClass := usespt;
  // updating some info
  currsys.UpdateEstimates;
  // adding the new system to the list
  primaryl.AppendSystem(currsys);
  inpllx.used := True;
  Result := True;
end;
//----------------------------------------------------------------------
// for new imported systems with no Simbad data!
function AddNewSystemNoSimbad(xparams:ImportParameters; inpllx:ImportedData; picksysn:Boolean; depoch:EpochType):Boolean;
var  currsys:StarSystem;        newid,x:Integer;
     locat:Location;            nametest:StarNames;
     compptr:NewStarBase;       isbd:Boolean;
     rapos,decpos:Real;
begin
  Result := False;
  newid := primaryl.NextID;
  isbd := False;
  if xparams.useSpT or xparams.useSpT_emp then begin
    isbd := BrownDwarfSpt(inpllx.stype);
  end;
  currsys := StarSystem.Create(newid, isbd);
  // setting location and constellation
  locat := currsys.GetLocation;
  locat.SetFromImported(inpllx,depoch,xparams.pllx_sourceid,True,xparams.use_altid);
  rapos := locat.GetDecimalRightAscension;
  decpos := locat.GetDecimalDeclination;
  if xparams.useRadv then begin
    if inpllx.radialv < 100000 then locat.SetRadialVelocity(inpllx.radialv);
  end;
  currsys.constellation := Const_Data.LocatePoint(rapos,decpos,locat.Epoch);
  // the names
  if (inpllx.nameids <> nil) and (inpllx.nameids.Count > 0) then begin
    nametest := currsys.MakeOrGetNames();
    for x := 0 to (inpllx.nameids.Count - 1) do begin
      nametest.SetCat(inpllx.nameids[x]);
    end;
    if picksysn then begin
      if nametest.CatalogCount < 4 then begin
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
    if inpllx.pllx < const_params.minpllx then Exit;
  end;
  // epoch
  if const_params.xparams.epochdata = 2014 then depoch := zJ2014
  else if const_params.xparams.epochdata = 2015 then depoch := zJ2015
  else if const_params.xparams.epochdata = 2015.5 then depoch := zJ2015h
  else if const_params.xparams.epochdata = 1991.25 then depoch := zJ1991q
  else  depoch := eJ2000;
  // getting the simbad data
  simbx := ImportedDataToSimbad(inpllx,const_params.xparams.epochdata);
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
  else if const_params.xparams.epochdata = 2015.5 then depoch := zJ2015h
  else if const_params.xparams.epochdata = 1991.25 then depoch := zJ1991q
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
    simbx := ImportedDataToSimbad(inpllx,const_params.xparams.epochdata);
    (* If the simbad data could be part of a sytem, handle the star manually. *)
    if (simbx <> nil) and (simbx.inGroup) then begin
      inpllx.check := True;
      simbx.Free;  Exit;
    end;
    // otherwise, create a new system...
    new_system := True;
    if simbx <> nil then begin
      Result := AddNewImportedSystem(const_params.xparams,inpllx,simbx,const_params.picksysn,depoch);
      simbx.Free;
    end else begin
      Result := AddNewSystemNoSimbad(const_params.xparams, inpllx,const_params.picksysn,depoch);
    end;
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
//*************************************************************************
end.

