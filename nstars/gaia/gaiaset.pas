unit gaiaset;

{$mode delphi}

(* String set for Gaia DR2 ids to get past that lingering error when I did my
original Gaia DR2 matching (and forgot to mark the stars as matched):

Added: Map for use in matching Gaia DR2 to NLTT *)

interface

uses
  Classes, SysUtils, contnrs;

type

(* This seems a bit simpler than implementing THashSet *)
AdHocStringSet = class
  protected
    dummy:TObject;
    hlist:TFPHashObjectList;
  public
    constructor Create(const capacity:Integer);
    destructor Destroy;
    function Contains(const item:string):Boolean;
    function Add(const item:string):Boolean;
    function Remove(const item:string):Boolean;
end;

var
  DR2_IDSet:AdHocStringSet;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//==============================================================================
(* dummy:TObject;
    hlist:TFPHashObjectList; *)
constructor AdHocStringSet.Create(const capacity:Integer);
begin
  dummy := TObject.Create;
  hlist := TFPHashObjectList.Create(False);
  hlist.Capacity:= capacity;
end;
//---------------------------------------
destructor AdHocStringSet.Destroy();
begin
  hlist.Clear;
  hlist.Free;
  dummy.Free;
end;

//---------------------------------------
function AdHocStringSet.Contains(const item:string):Boolean;
begin
  Result := (hlist.FindIndexOf(item) >= 0);
end;
//--------------------------------------
function AdHocStringSet.Add(const item:string):Boolean;
begin
  Result := False;
  if Contains(item) then Exit;
  hlist.Add(item,dummy);
  Result := True;
end;
//-------------------------------
function AdHocStringSet.Remove(const item:string):Boolean;
var iindex:Integer;
begin
  Result := False;
  iindex := hlist.FindIndexOf(item);
  if (iindex < 0) then Exit;
  hlist.Delete(iindex);
  Result := True;
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  DR2_IDSet := AdHocStringSet.Create(15000);
end.

