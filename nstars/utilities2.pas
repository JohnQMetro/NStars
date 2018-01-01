unit Utilities2;

{$mode delphi}

interface

uses
  Classes, SysUtils,Math, DAMath;

type
  RMatrix = array of array of Real;

  // custom enum for epoch
  EpochType = (eB1950,eB1975,eJ2000,zJ2014,zJ2015,zJ2017);

(* Checks if the sizes of A and B allow them to be multiplied *)
function CheckMatrixDimensions(const A,B:RMatrix):Boolean;
(* makes a matrix multiplication Value (no checking) *)
function MatrixMulValue(const A,B:RMatrix; const arow,bcol:Integer):Real;
(* Unchecked Matrix multiplication *)
function UnchkMatrixMultiply(const A,B:RMatrix):RMatrix;
(* Matrix Multiply *)
function MatrixMultiply(const A,B:RMatrix; out MResult:RMatrix):Boolean;
(* Matrix Transpose *)
function MakeTransposedMatrix(const in_matrix:RMatrix):RMatrix;

(* adds a row to a matrix, no checking done *)
procedure AddMatrixRow(var target:RMatrix; RValues:array of Real);
(* creates an n rows by m colums matrix, no initialization of values *)
function MakeMatrix(const n,m:Integer):RMatrix;
(* sets the contents of a matrix row *)
function SetMatrixRow(var target:RMatrix; rowdex:Integer; RValues:array of Real):Boolean;
(* matrix to string *)
function MatrixToString(const inmatrix:RMatrix; nline:Boolean):string;

(* Makes a coordinate matrix for calaculating galatic space velocities *)
function MakeGalCoordMatrix(ra_deg,dec_deg:Real):RMatrix;
(* Makes a rectangular coordinate matrix (for precession) *)
function MakeRectCoordMatrix(ra_deg,dec_deg:Real):RMatrix;
(* The inverse of the above *)
function RectCoordToEq(const RC:RMatrix; out dec_ra,dec_deg:Real):Boolean;
(* makes a transform matrix for equatorial to ecliptic coordinates *)
function MakeEq2EclMatrix(obl_ecl:Real):RMatrix;

(* transformation using a matrix *)
function CheckTransformInputs(const inra_deg,indec_deg:Real; matuse:RMatrix):Boolean;
function UnchkHalfTransform(const inra_deg,indec_deg:Real; matuse:RMatrix):RMatrix;
function MatrixTransform(const inra_deg,indec_deg:Real; matuse:RMatrix; out outra_deg,outdec_deg:Real):Boolean;
procedure UnchkMatrixTransform(const inra_deg,indec_deg:Real; matuse:RMatrix; out outra_deg,outdec_deg:Real);

(* producing transformation matrixes from scratch *)
(* using methods from Fundamental Astronomy 6th ed (Karttunen+ 2017) via Google Books *)
procedure MakePrecessionAngles(const yrdiff:Real; out zeta_ang,z_ang,th_ang:Real);
procedure MakeNutationAngles(const yrdiff:Real; out moe_ang,delpsi_ang,delmoe_ang:Real);
function MakePrecessionMatrixFromAngles(const in_zeta,in_z,in_theta:Real):RMatrix;
function MakeNutationMatrixFromAngles(const in_moe,in_delpsi,in_delmoe:Real):RMatrix;
function MakePrecessionMatrix(const yrdiff:Real):RMatrix;

(* Getting right transform matric *)
function GetTransformToGalactic(sepoch:EpochType):RMatrix;
function GetTransformToJ2000(sepoch:EpochType):RMatrix;

(* Abberation *)
function MakeSunEclLong(const yrdiff:Real):Real;
procedure AberrationCorrection(const ra_deg,dec_deg,sunecl:Real; out cra,cdec:Real);

// more utilities
procedure ProperMotionConvert(const pmdec_mas, pmra_mas:Real; out pmmag, pmang:Real);

var
  B1950toJ2000:RMatrix;
  B1950toGalactic:RMatrix;
  J2000toGalactic:RMatrix;
  // B1975
  J2000toB1975:RMatrix;
  B1975toJ2000:RMatrix;
  B1975toGalactic:RMatrix;

  // default transform equatorial to ecliptic matrix
  Equatorial_to_Ecliptic:RMatrix;

  fsOut: TFileStream;
  outdata:string;
  testmatrix:RMatrix;
  testra,testdec:Real;

const
  KPS2LYY = 3.3355724583274757e-6;
  Obl_Ecl_g = 23.4392808333333;


//******************************************************************************
implementation

(* Checks if the sizes of A and B allow them to be multiplied *)
function CheckMatrixDimensions(const A,B:RMatrix):Boolean;
var arowlen,browlen,arowdex,browdex:Integer;
begin
  Result := False;
  // non empty matrixes only
  if (Length(A)=0) or (Length(B)=0) then Exit;
  // row length of A must equal Column Length (row count) of B
  arowlen := Length(A[0]);
  if ( arowlen <> Length(B)) then Exit;
  // checking consistency in lengths...
  for arowdex := 0 to Length(A)-1 do begin
    if Length(A[arowdex]) <> arowlen then Exit;
  end;
  browlen := Length(B[0]);
  if browlen = 0 then Exit;
  for browdex := 0 to Length(B)-1 do begin
    if Length(B[browdex]) <> browlen then Exit;
  end;
  // all done
  Result := True;
end;
//----------------------------------------------------------------
(* makes a matrix multiplication Value (no checking) *)
function MatrixMulValue(const A,B:RMatrix; const arow,bcol:Integer):Real;
var cdex,clen:Integer;
begin
  Result := 0;
  clen := Length(A[arow]);
  for cdex := 0 to (clen-1) do begin
    Result += (A[arow][cdex])*(B[cdex][bcol]);
  end;
end;
//-----------------------------------------------------
(* Unchecked Matrix multiplication *)
function UnchkMatrixMultiply(const A,B:RMatrix):RMatrix;
var arowc,bcolc,arowdex,bcoldex:Integer;
begin
  arowc := Length(A);
  bcolc := Length(B[0]);
  Result := MakeMatrix(arowc,bcolc);
  for arowdex:= 0 to (arowc-1) do begin
    for bcoldex := 0 to (bcolc-1) do begin
      Result[arowdex][bcoldex] := MatrixMulValue(A,B,arowdex,bcoldex);
    end;
  end;
end;
//----------------------------------------------------------
(* Matrix Multiply *)
function MatrixMultiply(const A,B:RMatrix; out MResult:RMatrix):Boolean;
begin
  Result := CheckMatrixDimensions(A,B);
  if not Result then Exit;
  MResult := UnchkMatrixMultiply(A,B);
end;
//---------------------------------------------------------
(* Matrix Transpose *)
function MakeTransposedMatrix(const in_matrix:RMatrix):RMatrix;
var rowcount,colcount,rowdex,coldex:Integer;
begin
  // checking the inputs
  rowcount := Length(in_matrix);
  Assert(rowcount>0);
  colcount := Length(in_matrix[0]);
  Assert(colcount>0);
  if rowcount > 1 then begin
    for rowdex := 1 to (rowcount-1) do begin
      Assert(Length(in_matrix[rowdex])=colcount);
    end;
  end;
  // producing the result
  Result := MakeMatrix(colcount,rowcount);
  for rowdex := 0 to (rowcount-1) do begin
    for coldex := 0 to (colcount-1) do begin
      Result[coldex][rowdex] := in_matrix[rowdex][coldex];
    end;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* adds a row to a matrix, no checking done *)
procedure AddMatrixRow(var target:RMatrix; RValues:array of Real);
var tlen,alen,adex:Integer;
begin
  tlen := Length(target);
  SetLength(target,tlen+1);
  alen := Length(RValues);
  if alen = 0 then Exit;
  SetLength(target[tlen],alen);
  for adex := 0 to (alen-1) do target[tlen][adex] := RValues[adex];
end;
//-----------------------------------------------------------------
(* creates an n rows by m colums matrix, no initialization of values *)
function MakeMatrix(const n,m:Integer):RMatrix;
var ndex:Integer;
begin
  Assert(n>0);
  Assert(m>0);
  SetLength(Result,n);
  for ndex := 0 to (n-1) do begin
    SetLength(Result[ndex],m);
  end;
end;
//------------------------------------------------------
(* sets the contents of a matrix row *)
function SetMatrixRow(var target:RMatrix; rowdex:Integer; RValues:array of Real):Boolean;
var rxlen,rxdex:Integer;
begin
  Result := False;
  if rowdex < 0 then Exit;
  if rowdex >= Length(target) then Exit;
  rxlen := Length(RValues);
  if Length(target[rowdex]) <> rxlen then Exit;
  for rxdex := 0 to (rxlen-1) do target[rowdex][rxdex] := RValues[rxdex];
  Result := True;
end;
//-------------------------------------------------
(* matrix to string *)
function MatrixToString(const inmatrix:RMatrix; nline:Boolean):string;
var rcount,ccount,rdex,cdex:Integer;
begin
  // basic info
  rcount := Length(inmatrix);
  if rcount = 0 then Exit;
  ccount := Length(inmatrix[0]);
  Result := '';
  // row looping
  for rdex := 0 to (rcount-1) do begin
    // row separator
    if rdex > 0 then begin
      if nline then Result += sLineBreak
      else Result += ' ';
    end;
    // the row
    Assert(Length(inmatrix[rdex])=ccount);
    Result += '[';
    for cdex := 0 to (ccount-1) do begin
      if cdex<>0 then Result += ',';
      Result += Trim(FloatToStrF(inmatrix[rdex][cdex],ffFixed,35,10));
    end;
    Result += ']';
  end;
  // done
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Makes a coordinate matrix for calaculating galatic space velocities *)
function MakeGalCoordMatrix(ra_deg,dec_deg:Real):RMatrix;
var racos,deccos,rasin,decsin:Real;
begin
  Assert(ra_deg>=0);
  Assert(ra_deg<360);
  Assert(dec_deg<=90);
  Assert(dec_deg>=-90);
  // intermediate values
  racos := cosd(ra_deg);
  deccos := cosd(dec_deg);
  rasin := sind(ra_deg);
  decsin := sind(dec_deg);
  // making the result matrix
  Result := MakeMatrix(3,3);
  // setting the contents
  Result[0][0] :=  racos*deccos;
  Result[0][1] := -rasin;
  Result[0][2] := -racos*decsin;
  Result[1][0] :=  rasin*deccos;
  Result[1][1] :=  racos;
  Result[1][2] := -rasin*decsin;
  Result[2][0] :=  decsin;
  Result[2][1] :=  0;
  Result[2][2] :=  deccos;
end;
//----------------------------------------------
(* Makes a rectangular coordinate matrix (for precession) *)
function MakeRectCoordMatrix(ra_deg,dec_deg:Real):RMatrix;
var deccos:Real;
begin
  Result := MakeMatrix(3,1);
  deccos := cosd(dec_deg);
  Result[0][0] := cosd(ra_deg) * deccos;
  Result[1][0] := sind(ra_deg) * deccos;
  Result[2][0] := sind(dec_deg);
end;
//---------------------------------------------
(* The inverse of the above *)
function RectCoordToEq(const RC:RMatrix; out dec_ra,dec_deg:Real):Boolean;
begin
  Result := False;
  if Length(RC)<>3 then Exit;
  if Length(RC[0])<>1 then Exit;
  // declination is simple to calculate
  dec_deg := arcsind(RC[2][0]);
  // right ascension is more complicated
  dec_ra := RadToDeg(arctan2(RC[1][0],RC[0][0]));
  Result := True;
end;
//--------------------------------------------
(* makes a transform matrix for equatorial to ecliptic coordinates *)
function MakeEq2EclMatrix(obl_ecl:Real):RMatrix;
var sinoe,cosoe:Real;
begin
  sinoe := sind(obl_ecl);
  cosoe := cosd(obl_ecl);
  // making the matrix
  Result := MakeMatrix(3,3);
  SetMatrixRow(Result,0,[1,0,0]);
  SetMatrixRow(Result,1,[0, cosoe,sinoe]);
  SetMatrixRow(Result,1,[0,-sinoe,cosoe]);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* transformation using a matrix *)
//--------------------------------------------
function CheckTransformInputs(const inra_deg,indec_deg:Real; matuse:RMatrix):Boolean;
var mdex:Integer;
begin
  // checking the inputs
  Result := False;
  // the coordinates must be in range
  if (inra_deg < 0) then Exit;
  if (inra_deg >= 360) then Exit;
  if (indec_deg < -90) then Exit;
  if (indec_deg > 90) then Exit;
  // the transform matrix must be 3 by 3
  if Length(matuse)<>3 then Exit;
  for mdex := 0 to 2 do begin
    if Length(matuse[mdex])<>3 then Exit;
  end;
  Result := True;
end;
//--------------------------------------------
function UnchkHalfTransform(const inra_deg,indec_deg:Real; matuse:RMatrix):RMatrix;
var recc:RMatrix;
begin
  recc := MakeRectCoordMatrix(inra_deg,indec_deg);
  Result := UnchkMatrixMultiply(matuse,recc);
end;
//---------------------------------------------
function MatrixTransform(const inra_deg,indec_deg:Real; matuse:RMatrix; out outra_deg,outdec_deg:Real):Boolean;
var mres:RMatrix;
begin
  // checking the inputs
  Result := CheckTransformInputs(inra_deg,indec_deg,matuse);
  if not Result then Exit;
  // multiplying and converting the output
  mres := UnchkHalfTransform(inra_deg,indec_deg,matuse);
  Result := RectCoordToEq(mres,outra_deg,outdec_deg);
  outra_deg := DegNormalize(outra_deg);
end;
//----------------------------------------------------------
procedure UnchkMatrixTransform(const inra_deg,indec_deg:Real; matuse:RMatrix; out outra_deg,outdec_deg:Real);
var mres:RMatrix;
begin
  // multiplying and converting the output
  mres := UnchkHalfTransform(inra_deg,indec_deg,matuse);
  RectCoordToEq(mres,outra_deg,outdec_deg);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* producing transformation matrixes from scratch *)
(* using methods from Fundamental Astronomy 6th ed (Karttunen+ 2017) via Google Books *)
//----------------------------------------------
procedure MakePrecessionAngles(const yrdiff:Real; out zeta_ang,z_ang,th_ang:Real);
var centdiff:Real;
    zeta_temp,z_temp,th_temp:Real;
const zeta_eval:array[0..3] of Real = (0,2306.2181, 0.30188, 0.017998);
      z_eval:array[0..3] of Real    = (0,2306.2181, 1.09468 , 0.018203);
      th_eval:array[0..3] of Real   = (0,2004.3109,-0.42665 ,-0.041833);
begin
  centdiff := yrdiff /100.0;
  // calaculating arcsecond versions of the angles
  zeta_temp := PolEval(centdiff,zeta_eval,4);
  z_temp    := PolEval(centdiff,z_eval   ,4);
  th_temp   := PolEval(centdiff,th_eval  ,4);
  // finishing off
  zeta_ang := zeta_temp/3600;
  z_ang := z_temp/3600;
  th_ang := th_temp/3600;
end;
//----------------------------------------------
procedure MakeNutationAngles(const yrdiff:Real; out moe_ang,delpsi_ang,delmoe_ang:Real);
var centdiff,daydiff:Real;
    moe_arcsec,C1_temp,C2_temp:Real;
const moe_eval:array[0..3] of Real = (84381.448,-46.815,-0.00059,0.001813);
begin
  // initial easy calculations
  centdiff := yrdiff / 100.0;
  daydiff := yrdiff*365.25;
  moe_arcsec:= PolEval(centdiff,moe_eval,4);
  // intermediate calculations
  C1_temp := 125 - 0.05295*daydiff;
  C2_temp := 200.9 + 1.97129*daydiff;
  // final values
  moe_ang := moe_arcsec / 3600;
  delpsi_ang := -0.0048*sind(C1_temp) - 0.0004*sind(C2_temp);
  delmoe_ang := 0.0026*cosd(C1_temp) + 0.002*cosd(C2_temp);
end;
//--------------------------------------------------------------------
function MakePrecessionMatrixFromAngles(const in_zeta,in_z,in_theta:Real):RMatrix;
var sinz,cosz,sinzeta,coszeta,sintheta,costheta:Real;
begin
  // basic trig calcs
  sinz     := sind(in_z);
  cosz     := cosd(in_z);
  sinzeta  := sind(in_zeta);
  coszeta  := cosd(in_zeta);
  sintheta := sind(in_theta);
  costheta := cosd(in_theta);
  // making the result
  Result := MakeMatrix(3,3);
  Result[0][0] := cosz*costheta*coszeta-sinz*sinzeta;
  Result[0][1] := -cosz*costheta*sinzeta-sinz*coszeta;
  Result[0][2] := -cosz*sintheta;
  Result[1][0] := sinz*costheta*coszeta+cosz*sinzeta;
  Result[1][1] := -sinz*costheta*sinzeta+cosz*coszeta;
  Result[1][2] := -sinz*sintheta;
  Result[2][0] := sintheta*coszeta;
  Result[2][1] := -sintheta*sinzeta;
  Result[2][2] := costheta;
end;
//-----------------------------------------------------
function MakeNutationMatrixFromAngles(const in_moe,in_delpsi,in_delmoe:Real):RMatrix;
var sinmoe,cosmoe,delmoe_rad,delpsi_rad:Real;
    interm1,interm2:Real;
begin
  // computing initial values
  sinmoe := sind(in_moe);
  cosmoe := cos(in_moe);
  delmoe_rad := DegToRad(in_delmoe);
  delpsi_rad := DegToRad(in_delpsi);
  // second intermediates
  interm1 := delpsi_rad*cosmoe;
  interm2 := delpsi_rad*sinmoe;
  // making the result
  Result[0][0] := 1;
  Result[0][1] := -interm1;
  Result[0][2] := -interm2;
  Result[1][0] := interm1;
  Result[1][1] := 1;
  Result[1][2] := -delmoe_rad;
  Result[2][0] := interm2;
  Result[2][1] := delmoe_rad;
  Result[2][2] := 1;
end;
//------------------------------------------------------------
function MakePrecessionMatrix(const yrdiff:Real):RMatrix;
var z_ang,zeta_ang,theta_ang:Real;
begin
  MakePrecessionAngles(yrdiff,zeta_ang,z_ang,theta_ang);
  Result := MakePrecessionMatrixFromAngles(zeta_ang,z_ang,theta_ang);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Getting right transform matric *)
//------------------------------------
function GetTransformToGalactic(sepoch:EpochType):RMatrix;
begin
  if sepoch = eB1950 then Result := B1950toGalactic
  else if sepoch = eB1975 then Result := B1975toGalactic
  else if sepoch = eJ2000 then Result := J2000toGalactic
  else if sepoch = zJ2014 then Result := J2000toGalactic
  else if sepoch = zJ2015 then Result := J2000toGalactic
  else if sepoch = zJ2017 then Result := J2000toGalactic
  else Assert(False);
end;
//------------------------------------
function GetTransformToJ2000(sepoch:EpochType):RMatrix;
begin
  if sepoch = eB1950 then Result := B1950toJ2000
  else if sepoch = eB1975 then Result := B1975toJ2000
  else Assert(False);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Abberation *)
function MakeSunEclLong(const yrdiff:Real):Real;
var daydiff,Gval,sing,sin2g:Real;
    elclval:Real;
begin
  // calculating intermediate values
  daydiff := yrdiff*365.25;
  Gval := 357.528 + 0.9856*daydiff;
  sing := sind(Gval);
  sin2g := sind(2*gval);
  // final value
  elclval := 280.46 + 0.985647*daydiff + 1.915*sing + 0.02*sin2g;
  Result := DegNormalize(elclval);
end;
//-------------------------------------------------------
procedure AberrationCorrection(const ra_deg,dec_deg,sunecl:Real; out cra,cdec:Real);
var sinra,cosra,sindec,cosdec,sinecl,cosecl:Real;
    deldec,delracosd:Real;
begin
  // computing trig values
  sinra := sind(ra_deg);
  cosra := cosd(ra_deg);
  sindec := sind(dec_deg);
  cosdec := cosd(dec_deg);
  sinecl := sind(sunecl);
  cosecl := cosd(sunecl);
  // corrections
  deldec := 20.5*cosra*sindec*sinecl+18.8*sinra*sindec*cosecl;
  deldec := (deldec - 8.1*cosdec*cosecl)/3600;
  delracosd := (-20.5*sinra*sinecl-18.8*cosra*cosecl)/3600;
  // finally
  // cdec :=
  // cra := DegNormalize(ra_deg + delracosd/cosdec);

end;
//============================================================================
procedure ProperMotionConvert(const pmdec_mas, pmra_mas:Real; out pmmag, pmang:Real);
var int1, int3 :Real;
begin
  // special cases
  if pmra_mas = 0 then begin
    pmmag := Abs(pmdec_mas);
    if (pmdec_mas < 0) then pmang := 180
    else pmang := 0;
    Exit;
  end;
  if pmdec_mas = 0 then begin
    pmmag := Abs(pmra_mas);
    if pmra_mas < 0 then pmang := 270
    else pmang := 90;
    Exit;
  end;
  // non special cases
  // calculating the magnitude change
  int3 := hypot(pmdec_mas,pmra_mas);
  pmmag := int3;
  // calculating the position angle
  int1 := arctand(pmdec_mas/pmra_mas);
  if pmra_mas < 0 then pmang := 270 - int1
  else pmang := 90 - int1;
end;
//******************************************************************************
begin
  // setting up B1950toJ2000 as per Murray 1989
  B1950toJ2000 := MakeMatrix(3,3);
  SetMatrixRow(B1950toJ2000,0,[0.9999256794956877,-0.0111814832204662,-0.0048590038153592]);
  SetMatrixRow(B1950toJ2000,1,[0.0111814832391717, 0.9999374848933135,-0.0000271625947142]);
  SetMatrixRow(B1950toJ2000,2,[0.0048590037723143,-0.0000271702937440, 0.9999881946023742]);
  // B1950 to Galactic as per Murray 1989 / Liu 2010
  B1950toGalactic := MakeMatrix(3,3);
  SetMatrixRow(B1950toGalactic,0,[-0.066988739410, -0.872755765850, -0.483538914637]);
  SetMatrixRow(B1950toGalactic,1,[ 0.492728466081, -0.450346958020,  0.744584633279]);
  SetMatrixRow(B1950toGalactic,2,[-0.867600811149, -0.188374601732,  0.460199784785]);
  // J2000 to Galactic as per Murray 1989 / Liu 2010
  J2000toGalactic := MakeMatrix(3,3);
  SetMatrixRow(J2000toGalactic,0,[-0.054875539390,-0.873437104725,-0.483834991775]);
  SetMatrixRow(J2000toGalactic,1,[ 0.494109453633,-0.444829594298, 0.746982248696]);
  SetMatrixRow(J2000toGalactic,2,[-0.867666135681,-0.198076389622, 0.455983794523]);

  (* for B1975, I could find no matrixes, so I will compute them as I did the
  above. Because this is backwards and from J to B, I am less confident about
  the accuracy/validity of these matrixes. *)
  J2000toB1975 := MakePrecessionMatrix(-25.0007446954141);
  B1975toJ2000 := MakeTransposedMatrix(J2000toB1975);
  MatrixMultiply(J2000toGalactic,B1975toJ2000,B1975toGalactic);

  Equatorial_to_Ecliptic := MakeEq2EclMatrix(Obl_Ecl_g);

  (*
  outdata := 'Matrix Data' + sLineBreak + '--------------------------' + sLineBreak;

  outdata += sLineBreak + 'B1950 to J2000' + sLineBreak;
  outdata += MatrixToString(B1950toJ2000,True) + sLineBreak;
  outdata += sLineBreak + 'B1950 to Galactic' + sLineBreak;
  outdata += MatrixToString(B1950toGalactic,True) + sLineBreak;
  outdata += sLineBreak + 'J2000 to Galactic' + sLineBreak;
  outdata += MatrixToString(J2000toGalactic,True) + sLineBreak;

  MatrixMultiply(J2000toGalactic,B1950toJ2000,testmatrix);
  outdata += sLineBreak + 'B1950 to Galactic (Test)' + sLineBreak;
  outdata += MatrixToString(testmatrix,True) + sLineBreak;

  fsOut := TFileStream.Create('matrix.txt', fmCreate);
  fsOut.WriteAnsiString(outdata);
  fsOut.Free;
  *)

end.

