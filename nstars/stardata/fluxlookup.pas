unit fluxlookup;

{$mode delphi}

interface

uses
  Classes, SysUtils, sptfluxest, df_strings, math;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

FluxGrid = class
  protected
    name:string;
    // copy of static data (midpoint fluxes per spectral type)
    startmidp:Real;
    brightmidp:array of TenArray; // normally OBAFGK
    mmidp:RealArray;          // M
    lmidp:RealArray;          // L
    shortm:Boolean;
    // the flux boundaries per spectral type (generated from midpoints)
    startindex:Word;
    brightstart:array of TenArray;
    mstart:RealArray;
    lstart:RealArray;

    function MStr(mdex:Integer):string;
    function FindML(value:Real; out SpT:string):Boolean;

  public
    constructor Create(in_name:string);
    // lookup methods
    function FindSptR(value:Real; out SpT:string):Boolean;
    function FindSptC(value:Currency; out SpT:string):Boolean;
    // adding midpoint data
    procedure setBrightMax(value:Real);
    procedure addBrightDwarfMidpoints(data:array of TenArray; lastm:Boolean);
    procedure addMDwarfMidpoints(indata:array of Real);
    procedure addLDwarfMidpoints(data:array of Real);
    // converting it to start data
    procedure makeStartPoints();
end;

procedure GMagLookupSetup();
procedure BPmRPLookupSetup();
procedure GmRPLookupSetup();

var
  gmagLookup:FluxGrid;
  bpmrpLookup:FluxGrid;
  gmrpLookup:FluxGrid;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//========================================================================
function FluxGrid.MStr(mdex:Integer):string;
var isodd:Boolean;
begin
  isodd := Odd(mdex);
  mdex := Trunc(mdex/2);
  Result := 'M' + IntToStr(mdex);
  if isodd then Result += '.5';
  Result += 'V';
end;

//------------------------------------
function FluxGrid.FindML(value:Real; out SpT:string):Boolean;
var hasl,hasm:Boolean;
    mdex:Integer;
begin
  hasl := Length(lstart) > 0;
  hasm := Length(mstart) > 0;
  Result := True;
  SpT := '';
  if hasl then begin
    // Looking in L
    if value >= lstart[High(lstart)] then begin
      SpT := 'L' + IntToStr(High(lstart)) + ' or redder.';
      Result := False;  Exit;
    end
    else if value >= lstart[0] then begin
      for mdex := 0 to (High(lstart)-1) do if value < lstart[mdex+1] then break;
      SpT := 'L' + IntToStr(mdex);  Exit;
    end;
  end;
  if hasm then begin
    // Looking in M
    if not hasl then begin
      if (value >= mstart[High(mstart)]) then begin
        SpT := MStr(High(mstart)) + ' or redder.';
        Result := False;  Exit;
      end;
    end;
    if value >= mstart[High(mstart)] then begin
      SpT := MStr(High(mstart));  Exit;
    end
    else if value >= mstart[0] then begin
      for mdex := 0 to (High(mstart)-1) do if value < mstart[mdex+1] then break;
      SpT := MStr(mdex);  Exit;
    end;
  end;
  Result := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++
constructor FluxGrid.Create(in_name:string);
begin
  startmidp := NaN;
  name := in_name;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++
// lookup methods
//-----------------------
function FluxGrid.FindSptR(value:Real; out SpT:string):Boolean;
var oindex,mindex:Integer;
    foundx:Boolean;
begin
  // Checking the M and L arrays via a separate method
  Result := FindML(value,SpT);
  if Result then Exit // found it!
  else if SpT <> '' then begin
    Result := False; // value is too red
    Exit;
  end;
  (* Searching the O to K arrays (in reverse). Does not work for very red stars
  if the mstart array is empty. *)
  Result := True;
  for oindex := High(brightstart) downto 0 do begin
    foundx := False;
    if value >= brightstart[oindex][0] then begin
      for mindex := 9 downto 0 do begin
        if (value >= brightstart[oindex][mindex]) then begin
          foundx := True;  Break;
        end;
      end;
      // setting the spectral string afterwards
      if foundx then begin
        SpT := scolors2[oindex + startindex] + IntToStr(mindex) + 'V';
        Exit;
      end else raise Exception.Create('FindSptR: failed to find a match in range!');
    end;
  end;
  // if we get here, without finding a value, then the star is too bright
  SpT := 'More Blue than ' + scolors2[startindex] + '0V';
  Result := False;
end;
//----------------------
function FluxGrid.FindSptC(value:Currency; out SpT:string):Boolean;
var rv:Real;
begin
  rv := value;
  Result := FindSptR(rv,SpT);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++
// adding midpoint data
//----------------------------------
procedure FluxGrid.setBrightMax(value:Real);
begin
  startmidp := value;
  shortm := True;
end;
//----------------------------------
(* This one is generally for bright stars O to K. lastm = True if data includes
those as well. *)
procedure FluxGrid.addBrightDwarfMidpoints(data:array of TenArray; lastm:Boolean);
var input_len,sdex:Integer;
begin
  input_len := Length(data);
  if input_len = 0 then raise Exception.Create('Bright Star grid data is empty!');
  SetLength(brightmidp,input_len);
  For sdex := 0 to (input_len - 1) do begin
      brightmidp[sdex] := data[sdex];
  end;
  startindex := (6 - input_len) +1;
  if lastm then Inc(startindex);
end;
//-----------------------------
procedure FluxGrid.addMDwarfMidpoints(indata:array of Real);
var input_len,xd:Integer;
begin
  input_len := Length(indata);
  if (input_len = 0) or (input_len > 20) then begin
    raise Exception.Create('Length of M midpoints is invalid!');
  end;
  SetLength(mmidp,input_len);
  for xd := 0 to High(indata) do mmidp[xd] := indata[xd];
  shortm := input_len < 20;
end;

//-----------------------------
procedure FluxGrid.addLDwarfMidpoints(data:array of Real);
var input_len,xd:Integer;
begin
  input_len := Length(data);
  if (input_len = 0) or (input_len > 10) then begin
    raise Exception.Create('Length of L midpoints is invalid!');
  end;
  SetLength(lmidp,input_len);
  for xd := 0 to High(data) do lmidp[xd] := data[xd];
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure FluxGrid.makeStartPoints();
var colordex1,colordex2:Integer;
    prev,mid:Real;
begin
  // calculating values Bright Stars
  SetLength(brightstart,Length(brightmidp));
  for colordex1 := 0 to High(brightmidp) do begin
    for colordex2 := 0 to 9 do begin
      // each value is a *start* (between the table value and the one before it)
      if colordex2 = 0 then begin
        if (colordex1 = 0) and IsNaN(startmidp) then prev := brightmidp[0][0]
        else if (colordex1 = 0) then prev := startmidp
        else prev := brightmidp[colordex1-1][9]
      end
      else prev := brightmidp[colordex1][colordex2-1];
      mid := brightmidp[colordex1][colordex2];
      // producing that value
      brightstart[colordex1][colordex2] := (prev+mid)/2;
    end;
  end;
  // calculating values for M
  if (Length(mmidp) = 0) then Exit;
  SetLength(mstart,Length(mmidp));
  mstart[0] := (brightmidp[High(brightmidp)][9]+ mmidp[0])/2;
  for colordex2 := 1 to High(mmidp) do begin
    mstart[colordex2] := (mmidp[colordex2-1]+mmidp[colordex2])/2;
  end;
  // calculating values for L
  if (shortm or (Length(lmidp) = 0)) then Exit;
  SetLength(lstart,Length(lmidp));
  lstart[0] := (mmidp[19]+ lmidp[0])/2;
  for colordex2 := 1 to High(lmidp) do begin
    lstart[colordex2] := (lmidp[colordex2-1]+lmidp[colordex2])/2;
  end;

end;
//=============================================================
procedure GMagLookupSetup();
begin
  gmagLookup := FluxGrid.Create('G Magnitude');
  gmagLookup.setBrightMax(GB95Mag);
  gmagLookup.addBrightDwarfMidpoints(StarGMagnitudes,False);
  gmagLookup.addMDwarfMidpoints(GMMagnitudes);
  gmagLookup.addLDwarfMidpoints(GLMagnitudes);
  gmagLookup.makeStartPoints();
end;
//---------------------------------
procedure BPmRPLookupSetup();
begin
  bpmrpLookup := FluxGrid.Create('BP−RP');
  bpmrpLookup.setBrightMax(B95BPmRP);
  bpmrpLookup.addBrightDwarfMidpoints(StarBPmRP,False);
  bpmrpLookup.addMDwarfMidpoints(MBPmRP);
  bpmrpLookup.makeStartPoints();
end;
//------------------------------------
procedure GmRPLookupSetup();
begin
  gmrpLookup := FluxGrid.Create('G−RP');
  gmrpLookup.setBrightMax(B95GmRP);
  gmrpLookup.addBrightDwarfMidpoints(StarGmRP,False);
  gmrpLookup.addMDwarfMidpoints(MGmRP);
  gmrpLookup.addLDwarfMidpoints(LGmRP);
  gmrpLookup.makeStartPoints();
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  GMagLookupSetup();
  BPmRPLookupSetup();
  GmRPLookupSetup();
end.

