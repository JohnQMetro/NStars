unit locavg;

{$mode delphi}

(* for the rather messy task of making a 'system' parallax (as well as
proper motion and radial velocity. *)

interface

uses
  Classes, SysUtils, newlocation, Utilities2, DAMath;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

LocatAverager = class
  protected
    // accumulated values
    parallax:array of Real;
    parallax_err:array of Real;
    pllx_gaia:array of Boolean;
    pm_dec:array of Real;
    pm_ra:array of Real;
    radialv:array of Real;
    masses:array of Real;
    // weighted averages
    parallax_avg,pllxerr:Real;
    pmmag_avg:Real;
    pmang_avg:Real;
    rv_avg:Real;
    function accnt:Integer;
    function gpllx:Real;
  public
    property Count:Integer read accnt;
    property AveragePllx:Real read gpllx;
    procedure AddData(locat:Location; massx:Real);
    procedure ClearData();
    function ComputeAverages():Boolean;
    function GetAveragedLocation(position:Location):Location;
    function GetAvgString():string;
end;





//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//====================================================
function LocatAverager.accnt:Integer;
begin Result := Length(parallax);  end;
//------------------------------------------
function LocatAverager.gpllx:Real;
begin Result := parallax_avg;  end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
// copying over the motion/parallax data
procedure LocatAverager.AddData(locat:Location; massx:Real);
var qlen:Integer;
    pmmag,pmang:Real;
    costh,sinth:Real;
begin
  if locat = nil then Exit;
  if (locat.binarycopy) then Exit; // binary copy has no separate pllx/motion
  // sizing the arrays for the new data
  qlen := Length(parallax);
  SetLength(parallax,qlen+1);  SetLength(parallax_err,qlen+1);
  SetLength(pllx_gaia,qlen+1); SetLength(pm_dec,qlen+1);
  SetLength(pm_ra,qlen+1); SetLength(radialv,qlen+1);
  SetLength(masses,qlen+1);

  parallax[qlen]     := locat.ParallaxMAS;
  parallax_err[qlen] := locat.ParallaxErrorMAS;
  pllx_gaia[qlen]    := (locat.source = 'Gaia DR2');
  // proper motion will be in dec and ra components because it is easier to average
  pmmag := locat.ProperMotionMagnitude;
  pmang := locat.ProperMotionAngle;
  sinth := sind(pmang);
  costh := cosd(pmang);
  pm_ra[qlen]  := pmmag*sinth;
  pm_dec[qlen] := pmmag*costh;
  // radial velocity and mass
  radialv[qlen] := locat.RadialVelocity;
  masses[qlen]  := massx;
end;
//------------------------------------------
// clearing internal contents
procedure LocatAverager.ClearData();
begin
  SetLength(parallax,0);  SetLength(parallax_err,0);
  SetLength(pllx_gaia,0); SetLength(pm_ra,0);
  SetLength(pm_dec,0);  SetLength(radialv,0);
  SetLength(masses,0);
  parallax_avg := 0; pmmag_avg := 0;
  pmang_avg := 0;    rv_avg := 0;
end;

//------------------------------------------
// after adding components using AddData, call this to calculate the averaged values
function LocatAverager.ComputeAverages():Boolean;
var pllx_wsum, pllx_sum, cweight,cwplx:Real;
    cpllxe:Real;
    minpllxe,maxpllxe:Real;
    ldex_1,ldex_2,llen:Integer;
    pmrasum,pmdecsum,radialsum:Real;
    pmmsum,radmsum:Real;
begin
  Result := True;
  // the minimum and maximum parallax error is useful for the average
  minpllxe := 999999.9;   maxpllxe := 0;
  for ldex_1 := 0 to High(parallax_err) do begin
    cpllxe := parallax_err[ldex_1];
    if (cpllxe = 0) then cpllxe := 3;
    if cpllxe < minpllxe then minpllxe := cpllxe;
    if cpllxe > maxpllxe then maxpllxe := cpllxe;
  end;
  // preparing for the loop
  llen := Length(parallax);
  pllx_wsum := 0;  pllx_sum := 0;
  pmrasum := 0; pmdecsum := 0; pmmsum := 0;
  radialsum := 0; radmsum := 0;
  for ldex_2 := 0 to (llen-1) do begin
    // calculating the weighted parallaxes
    if parallax_err[ldex_2] = 0 then cweight := minpllxe / 3
    else cweight := minpllxe / parallax_err[ldex_2];
    if pllx_gaia[ldex_2] and (parallax_err[ldex_2] > 0.12) then cweight := cweight / 2;
    cwplx := cweight*parallax[ldex_2];
    pllx_wsum += cweight;
    pllx_sum += cwplx;
    // proper motion and radial velocity is weighted using masses
    if masses[ldex_2] > 0 then begin
       // proper motion sums
       pmrasum += pm_ra[ldex_2]*masses[ldex_2];
       pmdecsum += pm_dec[ldex_2]*masses[ldex_2];
       pmmsum += masses[ldex_2];
       // radial velocity (0 is not counted)
       if (radialv[ldex_2] <> 0) then begin
          radialsum += masses[ldex_2]*radialv[ldex_2];
          radmsum += masses[ldex_2];
       end;
    end;
  end;
  // the final averages
  parallax_avg := pllx_sum / pllx_wsum;
  pllxerr := maxpllxe;
  pmrasum := pmrasum / pmmsum;
  pmdecsum := pmdecsum / pmmsum;
  ProperMotionConvert(pmdecsum,pmrasum,pmmag_avg,pmang_avg);
  if (radialsum = 0) then rv_avg := 0
  else rv_avg := radialsum / radmsum;
end;

//------------------------------------------
(* Returns a new location object that uses the 2D RA and Dec in
the input position, but that averaged parallax, proper motion,
and radial V from this LocatAverager. *)
function LocatAverager.GetAveragedLocation(position:Location):Location;
begin
  Assert(position <> nil);
  Result := Location.Create(position,False);
  Result.SetParallax(parallax_avg,pllxerr);
  Result.SetProperMotion(pmmag_avg,pmang_avg);
  Result.SetRadialVelocity(rv_avg);
  Result.source := 'Average';
end;
//---------------------------------------------------------------
function LocatAverager.GetAvgString():string;
begin
  Result := Trim(FloatToStrF(parallax_avg,ffFixed,6,2)) + 'mas, ';
  Result += Trim(FloatToStrF(pmmag_avg,ffFixed,7,1)) + 'mas/yr at ';
  Result += Trim(FloatToStrF(pmang_avg,ffFixed,5,2)) + '°';
  if (rv_avg <> 0) then begin
     Result += ', rv:' + Trim(FloatToStrF(rv_avg,ffFixed,6,1));
     Result += 'km/s';
  end;
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

