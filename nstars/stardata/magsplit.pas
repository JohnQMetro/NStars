unit magsplit;

{$mode delphi}
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
interface

uses
  Classes, SysUtils, df_strings, NewStar, gaiadr2base, utilities, StarExt2,
  fluxtransform;
//==============================================================
(* Previously, I've been splitting magnitudes mostly-manually using
some rules of thumb, but this is getting too tedious. So some code to
do it semi-automatically. *)

type

MagSplitter = class
  protected
    deltav:Real;
    starA,starB:StarInfo;
    starAMags:RealArray;
    starBMags:RealArray;
    stage:Word;
    vsplit,osplit:Boolean;
    (* Split ratios: B,V,Rc,Ic,J,H,Ks (partly from Solar Neighborhood, partly
       est myself).*)
    const ratios:array[0..6] of Real = (1.08,1,0.943,0.735,0.599,0.578,0.556);
    const Gratio = 0.8;
    function splitMag(in_mag:Real; index:Integer):Boolean;
  public
    constructor Create(star_main,star_secondary:StarInfo);
    function SetDeltaG():Boolean;
    procedure SetDeltaV(vdiff:Real);
    procedure SetDeltaI(idiff:Real);
    procedure SetDeltaK(kdiff:Real);
    // spliting
    function CalculateSplit():Boolean;
    function ApplySplit():Boolean;
end;

function MakeSpliter(A,B:NewStarBase):MagSplitter;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//==============================================================
function MagSplitter.splitMag(in_mag:Real; index:Integer):Boolean;
var magA,magB:Real;
const x = 99.999;
begin
  if in_mag > 90 then begin
    starAMags[index] := x;
    starBMags[index] := x;
    Result := False;
  end else begin
    SplitMagnitude(in_mag,deltav*ratios[index],magA,magB);
    starAMags[index] := magA;
    starBMags[index] := magB;
    Result := True;
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor MagSplitter.Create(star_main,star_secondary:StarInfo);
begin
  stage := 0;
  starA := star_main;
  starB := star_secondary;
end;
//-----------------------------------------------
function MagSplitter.SetDeltaG():Boolean;
var gmagsA,gmagsB:GaiaDR2Mags;
    GDiff:Currency;
begin
  Result := False;
  if (starA = nil) or (starB = nil) then Exit;
  gmagsA := starA.dr2mags;
  gmagsB := starB.dr2mags;
  if (gmagsA = nil) or (gmagsB = nil) then Exit;
  if (gmagsA.G > 90) or (gmagsB.G > 90) then Exit;
  GDiff := gmagsB.G - gmagsA.G;
  deltav := CurrToReal(GDiff) / Gratio;
  stage := 1;
  Result := True;
end;
//-----------------------------------------------
procedure MagSplitter.SetDeltaV(vdiff:Real);
begin
  deltav := vdiff;
  stage := 1;
end;
//-----------------------------------------------
procedure MagSplitter.SetDeltaI(idiff:Real);
begin
  deltav := idiff/ratios[3];
  stage := 1;
end;
//-----------------------------------------------
procedure MagSplitter.SetDeltaK(kdiff:Real);
begin
  deltav := kdiff/ratios[6];
  stage := 1;
end;
//++++++++++++++++++++++++++++++++++++++++++
// spliting
//---------------------------
function MagSplitter.CalculateSplit():Boolean;
var srcA:StarFluxPlus;
    sc:Integer;
begin
  Result := False;
  vsplit := False;
  osplit := False;
  if stage <> 1 then Exit;
  srcA := starA.fluxtemp;
  if srcA = nil then Exit;
  // preparing
  SetLength(starAMags,7);
  SetLength(starBMags,7);
  sc := 0;

  // Splitting mags
  if splitMag(srcA.blue_mag,0) then Inc(sc); // B
  vsplit := splitMag(starA.VisualMagnitude,1); // V
  if splitMag(srcA.red_mag,2) then Inc(sc);  // Rc
  if splitMag(srcA.I_mag,3) then Inc(sc);    // Ic
  if splitMag(srcA.J_mag,4) then Inc(sc);    // J
  if splitMag(srcA.H_mag,5) then Inc(sc);    // H
  if splitMag(srcA.K_mag,6) then Inc(sc);    // Ks
  stage := 2;
  osplit := (sc > 0);
  Result := vsplit or osplit;
end;
//---------------------------
function MagSplitter.ApplySplit():Boolean;
var fluxA,fluxB:StarFluxPlus;
begin
  Result := False;
  if (stage <> 2) then exit;
  if not (vsplit or osplit) then Exit;
  if vsplit then begin
     starA.SetVisualMagnitude(starAMags[1]);
     starB.SetVisualMagnitude(starBMags[1]);
  end;
  if osplit then begin
     fluxB := starB.fluxtemp;
     if (fluxB = nil) then begin
        starB.fluxtemp := StarFluxPlus.Create;
        fluxB := starB.fluxtemp;
     end;
     fluxA := starA.fluxtemp;
     // now on to setting the mags
     fluxA.blue_mag := RoundConv(starAMags[0]);
     fluxB.blue_mag := RoundConv(starBMags[0]);
     fluxA.red_mag := RoundConv(starAMags[2]);
     fluxB.red_mag := RoundConv(starBMags[2]);
     fluxA.I_mag := RoundConv(starAMags[3]);
     fluxB.I_mag := RoundConv(starBMags[3]);
     fluxA.J_mag := RoundConv(starAMags[4]);
     fluxB.J_mag := RoundConv(starBMags[4]);
     fluxA.H_mag := RoundConv(starAMags[5]);
     fluxB.H_mag := RoundConv(starBMags[5]);
     fluxA.K_mag := RoundConv(starAMags[6]);
     fluxB.K_mag := RoundConv(starBMags[6]);
  end;
  Result := True;
end;
//============================================================
function MakeSpliter(A,B:NewStarBase):MagSplitter;
var sA,sB:StarInfo;
begin
  Result := nil;
  if (A <> nil) and (A is StarInfo) then sA := A as StarInfo
  else Exit;
  if (B <> nil) and (B is StarInfo) then sB := B as StarInfo
  else Exit;
  Result := MagSplitter.Create(sA,sB);
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.

