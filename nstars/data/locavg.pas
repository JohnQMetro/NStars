unit locavg;

{$mode delphi}

(* for the rather messy task of making a 'system' parallax (as well as
proper motion and radial velocity. *)

interface

uses
  Classes, SysUtils, newlocation;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

LocatAverager = class
  protected
    // accumulated values
    parallax:array of Real;
    parallax_err:array of Real;
    pllx_gaia:array of Boolean;
    pm_magnitude:array of Real;
    pm_posang:array of Real;
    radialv:array of Real;
    masses:array of Real;
    // weighted averages
    parallax_avg:Real;
    pmmag_avg:Real;
    pmang_avg:Rea;
    rv_avg:Real;

  public
    procedure AddData(locat:Location, massx:Real);
    procedure ClearData();
    function ComputeAverages():Boolean;
    function GetAveragedLocation(position:Location):Location;
end;





//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//====================================================
procedure LocatAverager.AddData(locat:Location, massx:Real);
var qlen:Integer;
begin
  Assert(locat <> nil);  Assert(massx >= 0);
  if (locat.binarycopy) then Exit;
  qlen = Length(parallax);
  SetLength(parallax,qlen+1);  SetLength(parallax_err,qlen+1);
  SetLength(pllx_gaia,qlen+1); SetLength(pm_magnitude,qlen+1);
  SetLength(pm_posang,qlen+1); SetLength(radialv,qlen+1);
  SetLength(masses,qlen+1);

  parallax[qlen] = locat.;
    parallax_err:array of Real;
    pllx_gaia:array of Boolean;
    pm_magnitude:array of Real;
    pm_posang:array of Real;
    radialv:array of Real;
    masses:array of Real;

end;
//------------------------------------------
procedure LocatAverager.ClearData();
//------------------------------------------
function LocatAverager.ComputeAverages():Boolean;
//------------------------------------------
function LocatAverager.GetAveragedLocation(position:Location):Location;



//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

