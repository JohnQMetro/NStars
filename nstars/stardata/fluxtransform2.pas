unit fluxtransform2;

{$mode delphi}

interface

uses
  Classes, SysUtils, Math, DAMath, fluxtransform, Utilities, df_strings;
//******************************************************************
(* revised fits for UCAC4, URAT1, and CMC 15 *)

// I only find myself using UCAC4 if G is unreliable, so a simpler function...
function UCAC_to_VRI(UCAC4,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
// CMC 15 to VRI
function CMC15_to_VRI(CMC,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
// revised USNO B catalog I (and 2MASS J) to Ic
function ClassifyUSNO_I(UBI,J:Currency; out ij:Real):Word;
function USNO_IJ_Ic(UBI,J:Currency; out IcEst:Currency):Boolean;
//--------------------------------------------------------------
// revised APASS transforms (main improvement: ip is not necessary anymore
function APASS_to_RI(gp,rp,ip:Real; J:Currency; out RcEst,IcEst:Currency):Boolean;
function APASS_to_RIwd(gp,rp:Real; J:Currency; out RcEst,IcEst:Currency):Boolean;
function APASS_to_V(gp,rp,ip:Real; out Vest:Real):Boolean;
function APASS_to_B(gp,rp:Real; out Best:Real):Boolean;
function ParseAPASS(indata:string; var results:RealArray):Boolean;
function APASS_to_BVRI(const indata:RealArray; Jin:Currency; out Vest,Verr:Real; out Best,Berr,RcEst,IcEst:Currency):Boolean;
function sAPASS_to_BVRI(const indata:string; Jin:Currency; out Vest,Verr:Real; out Best,Berr,RcEst,IcEst:Currency):Boolean;
//******************************************************************
implementation
//==================================================================
// I only find myself using UCAC4 if G is unreliable, so a simpler function...
function UCAC_to_VRI(UCAC4,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
var ucacmj,vmj,rmj,imj,rj:Real;
const rcoff:array[0..2] of Real = ( -0.12126, 0.90737, -0.026637 );
      icoff:array[0..2] of Real = ( 0.53983, 0.14898, 0.022933 );
begin
  Result := False;
  if not MakeColorCheck(UCAC4,J,2.228,5.558,ucacmj) then Exit;
  vmj := 0.52007 + 0.94656*ucacmj;
  rmj := PolEval(ucacmj,rcoff,3);
  imj := PolEval(ucacmj,icoff,3);
  Result := True;
  rj := CurrToReal(J);
  Vest := vmj + rj;
  RcEst := RealToCurr(rmj + rj);
  RcEst := RoundCurrency(RcEst,False);
  IcEst := RealToCurr(imj + rj);
  IcEst := RoundCurrency(IcEst,False);
end;
//----------------------------------------------------
function CMC15_to_VRI(CMC,J:Currency; out Vest:Real; out RcEst,IcEst:Currency):Boolean;
var cmcmj,vmj,rmj,imj,rj:Real;
const rcoff:array[0..3] of Real = ( 0.73806, 0.2203, 0.18383, -0.019145 );
      icoff:array[0..2] of Real = ( 0.52485, 0.16874, 0.026694 );
begin
  Result := False;
  if not MakeColorCheck(CMC,J,2.088,5.294,cmcmj) then Exit;
  vmj := 0.46352 + 1.0449*cmcmj;
  rmj := PolEval(cmcmj,rcoff,4);
  imj := PolEval(cmcmj,icoff,3);
  Result := True;
  rj := CurrToReal(J);
  Vest := vmj + rj;
  RcEst := RealToCurr(rmj + rj);
  RcEst := RoundCurrency(RcEst,False);
  IcEst := RealToCurr(imj + rj);
  IcEst := RoundCurrency(IcEst,False);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// revised USNO B catalog I (and 2MASS J) to Ic
function ClassifyUSNO_I(UBI,J:Currency; out ij:Real):Word;
begin
  Result := 0;
  if not MakeColorCheck(UBI,J,-1,5,ij) then Exit;
  if (ij > (0.388*UBI - 2.208)) then Result := 1
  else if (UBI > 11.6) and (ij < (0.294*UBI - 3.41)) then Result := 3
  else if (UBI > 15) and (ij <(1.364*UBI - 18.46)) then Result := 4
  else Result := 2;
end;
(* USNO B-Catalog I to Ic. We use Magnitude-Color to sort the stars
into 4 groups, which then have different equations. *)
function USNO_IJ_Ic(UBI,J:Currency; out IcEst:Currency):Boolean;
var bimj,interm:Real; classn:Word;
const poly1:array[0..2] of Real = ( 0.078344, 0.36977, -0.010863 );
      poly4:array[0..2] of Real = ( -0.096615, 1.5505, -0.1991 );
begin
  Result := False;
  IcEst := UBI;
  // classifying
  classn := ClassifyUSNO_I(UBI,J,bimj);
  if classn = 0 then Exit;
  // Class 1: sensible looking line (~1100 stars, std err ~0.0573 at 98%)
  if (classn = 1) then begin
    if (bimj < 0.667) or (bimj > 4.731) then Exit;
    interm := PolEval(bimj,poly1,3);
  end
  // Class 2: the ugly blob (~5000 stars, std err ~0.153 at 98%)
  else if (classn = 2) then begin
    if (bimj < -0.243) or (bimj > 3.249) then Exit;
    interm := 1.1673 + 0.214*bimj;
  end
  // Class 3: mostly white dwarfs, I think (~60 stars, std err ~0.161 at 95%)
  else if (classn = 3) then begin
    if (bimj < -0.912) or (bimj >0.957) then Exit;
    interm := 0.18482 + 0.47801*bimj;
  end
  // Class 4: the blob's thin red tail (~60 stars, std err ~0.172 at 95%)
  else begin
    if (bimj < 1.535) or (bimj > 4.363) then Exit;
    interm := PolEval(bimj,poly4,3);
  end;
  // if we get here, we deliver a result
  Result := True;
  IcEst := RealToCurr(interm) + J;
  IcEst := RoundCurrency(IcEst,False)
end;
//----------------------------------------------------------------
// APASS g r i to Rc and Ic (ignore SDSS, no white dwarves)
function APASS_to_RI(gp,rp,ip:Real; J:Currency; out RcEst,IcEst:Currency):Boolean;
var gmr,gmj,gmi,rmi,imj,interm,interm2:Real;
    grok,gjok,giok,riok,ijok:Boolean;
const p1:array[0..2] of Real = ( 0.71901, -0.010664, 0.078028 );
      p2:array[0..2] of Real = ( 1.5129, -0.16686, 0.11315 );
      p3:array[0..2] of Real = ( 1.4632, -0.081343, 0.035903 );
      p4:array[0..2] of Real = ( 0.15266, 0.17237, 0.086884 );
      q1:array[0..2] of Real = ( -0.69547, 0.86253, -0.02117 );
      q2:array[0..2] of Real = ( -0.57788, 0.98715, -0.10663 );
begin
  Result := False;
  grok := MakeColorCheck(gp,rp,1.082,1.764,gmr);
  gjok := MakeColorCheck(gp,J,3.227,7.668,gmj);
  giok := MakeColorCheck(gp,ip,1.741,4.418,gmi);
  riok := MakeColorCheck(rp,ip,0.459,2.893,rmi);
  ijok := MakeColorCheck(ip,J,1.487,3.25,imj);
  // Rc. note that APASS i is often missing, and g-r alone gives poor results
  interm := 9999; interm2 := 9999;
  if (grok and gjok) then interm := -0.1267 + 0.66091*gmr + 0.23919*gmj
  else if grok and (gmr < 1.631) and giok then  begin
    interm := PolEval(gmi,p1,3) + 0.46969*gmr;
  end
  else if riok then interm2 := PolEval(rmi,p4,3)
  else if giok then interm := PolEval(gmi,p2,3)
  else if gjok then interm := PolEval(gmj,p3,3);
  RcEst := 99.999;
  if (interm < 9000) or (interm2 < 9000) then begin
    if (interm < 9000) then RcEst := RealToCurr(gp - interm)
    else RcEst := RealToCurr(rp - interm);
    RcEst := RoundCurrency(RcEst,False);
    Result := True;
  end;
  // Ic
  interm := 9999; interm2 := 9999;
  if (grok and gjok) then interm := 0.31563*gmr + PolEval(gmj,q1,3)
  else if (riok and ijok) then begin
      interm2 := 0.74373*rmi + PolEval(imj,q2,3);
  end else if gjok then interm := 0.14969 + 0.67197*gmj
  else if riok then interm2 := 0.45123 + 1.1443*rmi
  else if giok then interm := 0.29603 +  1.1297*gmi;
  IcEst := 99.999;
  if (interm < 9000) or (interm2 < 9000) then begin
    if (interm < 9000) then IcEst := RealToCurr(gp - interm)
    else IcEst := RealToCurr(rp - interm);
    IcEst := RoundCurrency(IcEst,False);
    Result := True;
  end;
end;
//------------------------------------------------------
function APASS_to_RIwd(gp,rp:Real; J:Currency; out RcEst,IcEst:Currency):Boolean;
var gmr,gmj,interm:Real;
    grok,gjok:Boolean;
begin
  Result := False;
  grok := MakeColorCheck(gp,rp,-0.382,0.727,gmr);
  gjok := MakeColorCheck(gp,J,-0.734,2.224,gmj);
  // Rc
  interm := 99.999; RcEst := 99.999;
  if grok and gjok and (gmj <= 1.999) then begin
    interm := 0.039267 + 0.33996*gmr + 0.33556*gmj;
  end
  else if grok then interm := 0.12623 + 1.1443*gmr
  else if gjok then interm := 0.015614 + 0.45885*gmj;
  if (interm < 99) then begin
    RcEst := RealToCurr(gp - interm);
    RcEst := RoundCurrency(RcEst,False);
    Result := True;
  end;
  // Ic
  interm := 99.999; IcEst := 99.999;
  if grok and gjok and (gmj <= 1.999) then begin
    interm := 0.10841 + 0.55215*gmr + 0.46886*gmj;
  end
  else if gjok then interm := 0.060125 + 0.68467*gmj;
  if (interm < 99) then begin
    IcEst := RealToCurr(gp - interm);
    IcEst := RoundCurrency(IcEst,False);
    Result := True;
  end;
end;
//----------------------------------------------------
// APASS to V
function APASS_to_V(gp,rp,ip:Real; out Vest:Real):Boolean;
var gmr,rmi,gmi,interm:Real;
    grok,riok,giok:Boolean;
begin
    grok := MakeColorCheck(gp,rp,1.082,1.764,gmr);
    riok := MakeColorCheck(rp,ip,0.459,2.893,rmi);
    giok := MakeColorCheck(gp,ip,1.741,4.418,gmi);
    Result := False;
    if grok then interm := -0.052467 + 0.58862*gmr
    else if riok then interm := 0.53331 + 0.055006*rmi
    else if giok then interm := 0.68096 + 0.021482*gmi
    else Exit;
    Result := True;
    if grok then Vest := gp - interm
    else if riok then Vest := interm + rp
    else Vest := gp -interm;
end;
//-------------------------------------------------------
(* APASS to B. Note that APASS B has a red leak, and for red stars I
have no other sources of B. So I'll have to trust Lupton 2005.
However, I will use my own equations to derive g ang g-r *)
function APASS_to_B(gp,rp:Real; out Best:Real):Boolean;
var gpmrp,gmr,gmrp:Real;
begin
    Result := True;
    if not MakeColorCheck(gp,rp,0.904,1.6,gpmrp) then Exit;
    // calculate g-r from gp-rp
    gmr := 0.98115 + 0.37271*gpmrp;
    // calculate g-rp from gp-rp
    gmrp := 0.82489 + 0.48423*gmrp;
    // finally
    Best := (gmrp + rp) + 0.313*gmr + 0.2271;
    Result := True;
end;
//---------------------------------------------------------------
// can take 3,5,6,9 or 10 numbers in a string
function ParseAPASS(indata:string; var results:RealArray):Boolean;
var maglist:RealArray;
    mlen:Integer;
    noe:Boolean;
begin
    Result := False;
    if not SplitWithSpacesToReal(indata,3,maglist) then Exit;
    mlen := Length(maglist);
    if (mlen = 4) or (mlen = 7) or (mlen = 8) or (mlen > 10) then Exit;
    // length assumptions:
    // 3: g r i, 5: V B g r i, 6: g r i with errors, 9,10: V B g r i with errors
    SetLength(results,7);
    if (mlen = 3) or (mlen = 6) then begin
      // clearing V and B
      results[0] := 99.999;  results[1] := 0;
      results[2] := 99.999;  results[3] := 0;
      // g r i
      results[4] := maglist[0];
      results[5] := ifthen(mlen = 3,maglist[1],maglist[2]);
      results[6] := ifthen(mlen = 3,maglist[2],maglist[4]);
    end
    else begin
      noe := (mlen = 5);
      // V and B
      results[0] := maglist[0];
      results[1] := ifthen(noe,0,maglist[1]);
      results[2] := ifthen(noe,maglist[1],maglist[2]);
      results[3] := ifthen(noe,0,maglist[3]);
      // gri
      results[4] := ifthen(noe,maglist[2],maglist[4]);
      results[5] := ifthen(noe,maglist[3],maglist[6]);
      results[6] := ifthen(noe,maglist[4],maglist[8]);
    end;
    // done
    Result := True;
end;
//----------------------------------------------------------------
function APASS_to_BVRI(const indata:RealArray; Jin:Currency; out Vest,Verr:Real; out Best,Berr,RcEst,IcEst:Currency):Boolean;
var hb,hv,bok,ixok:Boolean;
    besx:Real;
begin
    Result := False;
    if Length(indata) <> 7 then Exit;
    hv := indata[0] < 90;
    hb := indata[2] < 90;
    // V
    Vest := indata[0];
    Verr := indata[1];
    if not hv then begin
        Result := APASS_to_V(indata[4],indata[5],indata[6],Vest);
    end else Result := True;
    // B
    Best := RealToCurr(indata[2]);
    Berr := RealToCurr(indata[3]);
    if (not hb) then begin
       bok := APASS_to_B(indata[4],indata[5],besx);
       if bok then begin
         Best := RealToCurr(besx);
         Best := RoundCurrency(Best,False);
       end;
       Result := bok or Result;
    end else Result := True;
    // Rc and Ic
    ixok := APASS_to_RI(indata[4],indata[5],indata[6],Jin,RcEst,IcEst);
    Result := Result or ixok;
end;
//-----------------------------------------------------------------
function sAPASS_to_BVRI(const indata:string; Jin:Currency; out Vest,Verr:Real; out Best,Berr,RcEst,IcEst:Currency):Boolean;
var maglist:RealArray;
begin
  Result := ParseAPASS(indata,maglist);
  if (not Result) then Exit;
  Result := APASS_to_BVRI(maglist,Jin,Vest,Verr,Best,Berr,RcEst,IcEst);
end;

//******************************************************************
end.

