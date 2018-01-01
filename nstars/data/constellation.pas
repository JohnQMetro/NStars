unit constellation;

{$MODE Delphi}

interface

uses SysUtils, StrUtils,Classes, df_strings, Utilities2;

type

(*
This unit for automatically finding the consteallation from the
coordinate, we use decimal degrees fro RA and DEC.

for constellations that cross the 0/24 RA boundary (or 0/360
in decimal degrees), we identify and mark them before hand, and
all RA values for these that are less than 180 have 360 added
to them. This includes points to test.

the point in polygon test is taken from
http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
//--------------------------------------------------
Copyright (c) 1970-2003, Wm. Randolph Franklin

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

   1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimers.
   2. Redistributions in binary form must reproduce the above copyright
    notice in the documentation and/or other materials provided with the
     distribution.
   3. The name of W. Randolph Franklin may not be used to endorse or promote
    products derived from this Software without specific prior written
     permission.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
//---------------------------------------------------------------
The algorithm is translated into delphi

The data is taken from the IAU website , and is J2000

*)

// stores (and does the tests for) individual constellation boundaries
ConstellationBounds = class
  protected
    cname:string;
    crossing:Boolean;
    // box bounds
    ramax,ramin:Real;
    decmax,decmin:Real;
    // the vertices
    vcount:Integer;
    ra_cords:array of Real;
    dec_cords:array of Real;
    // helper methods
    function Transform(inv:Real):Real;
    function ConvertRA(idata:string):Real;
  public
    constructor Create(const inname:string; cross:Boolean);
    // entering data
    function AddPoints(indata:string):Boolean;
    procedure SetBoundBox;
    // testing
    function InBoundBox(inra,indec:Real):Boolean;
    function PointInPolygon(inra,indec:Real):Boolean;
    function InConstellation(inra,indec:Real):Boolean;
end;

// stores all the data
AllSkyBounds = class
  protected
    // internaldata
    thedata:array[1..89] of ConstellationBounds;
    cindex:Integer;
    // filedata
    fname:TFileName;
    infile:Text;
    // internal methods
    function LoadConstellation:Boolean;
    procedure NilArray;
  public
    constructor Create;
    // loading data
    procedure LoadData;
    // testing
    function LocatePoint(inra,indec:Real; xepoch:EpochType):Integer;
    // done
    destructor Destroy; override;
end;


var
  Const_Data:AllSkyBounds;

implementation
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
// does the point mapping mentioned above
function ConstellationBounds.Transform(inv:Real):Real;
begin
  if inv<180 then Result := inv+360
  else Result := inv;  
end;
//------------------------------------------------------------
// convert string RA in sexagemisal formal to decimal degrees
function ConstellationBounds.ConvertRA(idata:string):Real;
var str1,str2,str3:string;
    int_hrs, int_min, sc:Integer;
    real_sec, real_ir:Real;
begin
  // parsing
  if not ExtractFirstWord(idata,str1) then begin
    raise Exception.Create(cname+':Constellation Data RA is bad! (1)');
  end;
  if not ExtractFirstWord(idata,str2) then begin
    raise Exception.Create(cname+':Constellation Data RA is bad! (2)');
  end;
  str3 := idata;
  // converting values
  Val(str1,int_hrs,sc);
  if (sc<>0) or (int_hrs>24) or (int_hrs<0) then begin
    raise Exception.Create(cname+':Constellation Data RA is bad! (3)');
  end;
  Val(str2,int_min,sc);
  if (sc<>0) or (int_min>=60) or (int_min<0) then begin
    raise Exception.Create(cname+':Constellation Data RA is bad! (4)');
  end;
  Val(str3,real_sec,sc);
  if (sc<>0) or (real_sec>=60) or (real_sec<0) then begin
    raise Exception.Create(cname+':Constellation Data RA is bad! (5)');
  end;
  // we start transforming the values
  real_ir := real_sec/240+(int_min/4.0);
  real_ir := int_hrs*15 + real_ir;
  // done
  Result := real_ir;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor ConstellationBounds.Create(const inname:string; cross:Boolean);
begin
  cname := inname;
  crossing := cross;
  // we set them at the extremes to start with
  ramax := 0; ramin := 540;
  decmax := -90; decmin := 90;
  // almost done
  vcount := 0;
end;
//------------------------------------------------------------
// entering data
function ConstellationBounds.AddPoints(indata:string):Boolean;
var qstr:TStringList;
    raval,decval:Real;
    sc:Integer;
begin
  // top level parsing
  qstr := ParseByChar(indata,'|',true,true);
  if qstr.Count<>3 then begin
    qstr.Free;
    raise Exception.Create(cname+': Constellation line is malformed! (1)');
  end;
  // we process ra
  raval := ConvertRA(Trim(qstr[0]));
  // we convert the declination
  Val(qstr[1],decval,sc);
  if sc<>0 then begin
    qstr.Free;
    raise Exception.Create(cname+': Constellation line is malformed! (2)');
  end;
  // almost done, do we transform?
  if crossing then raval := Transform(raval);
  // adding to the list
  Inc(vcount);
  SetLength(ra_cords,vcount);
  SetLength(dec_cords,vcount);
  ra_cords[vcount-1] := raval;
  dec_cords[vcount-1] := decval;
  // done
  qstr.Free;
  Result := True;
end;
//------------------------------------------------------------
procedure ConstellationBounds.SetBoundBox;
var I:Integer;
begin
  for I := 0 to vcount - 1 do begin
    if ra_cords[I]>ramax then ramax := ra_cords[I];
    if ra_cords[I]<ramin then ramin := ra_cords[I];
    if dec_cords[I]>decmax then decmax := dec_cords[I];
    if dec_cords[I]<decmin then decmin := dec_cords[I];
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// testing
function ConstellationBounds.InBoundBox(inra,indec:Real):Boolean;
begin
  Result := False;
  if (inra<ramin) or (inra>ramax) then Exit;
  if (indec<decmin) or (indec>decmax) then Exit;
  Result := True;
end;
//------------------------------------------------------------
// does the point in polygon test
function ConstellationBounds.PointInPolygon(inra,indec:Real):Boolean;
var I,J:Integer;
    C,Q:Boolean;
    workval1,workval2:Real;
begin
  C := False;
  J := vcount-1;
  for I := 0 to vcount - 1 do begin
    // main body
    Q := ((dec_cords[I]>indec) <> (dec_cords[J]>indec));
    if Q then begin
      // calculating intermediate values
      workval1 := (ra_cords[J]-ra_cords[I])*(indec-dec_cords[I]);
      workval2 := (dec_cords[J]-dec_cords[I]);
      workval1 := workval1 / workval2 + ra_cords[I];
      // switching
      if inra < workval1 then C:= not C;
    end;
    J:=I;
  end;
  // done
  Result := C;
end;
//------------------------------------------------------------
function ConstellationBounds.InConstellation(inra,indec:Real):Boolean;
var inp_ra:Real;
begin
  // the overall test
  Result := False;
  // first, we Tranform if need be
  if crossing then inp_ra := Transform(inra)
  else inp_ra := inra;
  // next, we do boundbox testing
  if not InBoundBox(inp_ra,indec) then Exit;
  // if the above still passes, we use the polygon test
  Result := PointInPolygon(inp_ra,indec);
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function AllSkyBounds.LoadConstellation:Boolean;
var cline,buf:string;
    iscross:Boolean;
    linep:TStringList;
begin
  // reading in and processing line1
  if Eof(infile) then begin
    Result := False;
    Exit;
  end;
  Readln(infile,cline);
  if not AnsiStartsStr('###',cline) then
    raise Exception.Create('Constellation does not start with ###!');
  cline := copy(cline,4,length(cline)-3);
  linep := ParseByChar(cline,'|',true,true);
  if (linep.Count)<>2 then begin
    linep.Free;
    raise Exception.Create('Constellation name data is off!');
  end;
  buf := linep[1];
  iscross := (buf='Y');
  // we create and assign
  Inc(cindex);
  thedata[cindex] := ConstellationBounds.Create(linep[0],iscross);
  // we load the individual lines
  Readln(infile,cline);
  while not AnsiStartsStr('@@@',cline) do begin
    thedata[cindex].AddPoints(cline);
    if Eof(infile) then raise Exception.Create('Unexpected end of file!');
    Readln(infile,cline);
  end;
  // here, we are done
  Result := True;
end;
//-----------------------------------------------------------
procedure AllSkyBounds.NilArray;
var I:Integer;
begin
  for I := 1 to 89 do thedata[I] := nil;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor AllSkyBounds.Create;
begin
  cindex := 0;
  NilArray;
  fname := 'constellations.dat';
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// loading data
//-----------------------------------------------------------
procedure AllSkyBounds.LoadData;
var qtest:Boolean;
begin
  // opening the file
  qtest := FileExists(fname);
  Assert(qtest);
  AssignFile(infile,fname);
  FileMode := fmOpenRead;
  Reset(infile);
  // we do the reading
  while LoadConstellation do thedata[cindex].SetBoundBox;
  // closing the file
  CloseFile(infile);
  FileMode := fmOpenReadWrite;
  // some checking...
  if (cindex<>89) then raise Exception.Create('The constellation count is bad');
  // here we are, done the loading
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// testing
function AllSkyBounds.LocatePoint(inra,indec:Real; xepoch:EpochType):Integer;
var found:Boolean;
    I:Integer;
    tra,tdec:Real;
    tmatrix:RMatrix;
begin
  found := False;
  // transforming??
  // I'm pretty sure the constellation bounds are in J2000
  if (xepoch = eB1950) or (xepoch = eB1975) then begin
    tmatrix := GetTransformToJ2000(xepoch);
    UnchkMatrixTransform(inra,indec,tmatrix,tra,tdec);
  end else begin
    tra := inra; tdec := indec;
  end;
  // looking
  for I := 1 to 89 do begin
    found := thedata[I].InConstellation(inra,indec);
    if found then Break;
  end;
  // post processing
  if found then begin
    if I=89 then Result := 76
    else Result := I;
  end
  else Result :=-1;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// done
destructor AllSkyBounds.Destroy;
var I:Integer;
begin
  for I := 1 to 89 do thedata[I].Free;
  NilArray;
  inherited;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  Const_Data := AllSkyBounds.Create;
  Const_Data.LoadData;
end.
