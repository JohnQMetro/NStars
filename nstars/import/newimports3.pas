unit newImports3;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils, newImports, df_strings, fluxtransform;

(* Importing from :
https://arxiv.org/abs/1802.08272
table_good.tex and table_north.tex
URAT South Parallax Results: Discovery of New Nearby Stars
~ 2018, Finch, Zacharias, Jao *)
function ParseURATS_Line(linein:string):ImportedData;
function MakeURATS_Const():ImportParameters;

var urats_params:ImportParameters;

//======================================================================
implementation
//---------------------------------------------------------------------
(* 'URAT South Parallax Results: Discovery of New Nearby Stars'
C. Finch, N. Zacharias, W-C. Jao (2018)
https://arxiv.org/abs/1802.08272 (Other formats)
parsing the tex files for tables 5 and 6, which are almost identical.
Tex files are almost but not quite identical *)

function ParseURATS_Line(linein:string):ImportedData;
var nameid,buffer1,buffer2:string;
    nlength,soff:Integer;
    isgood:Boolean;
    impres:ImportedData;
    gpv,rpv,ipv,vtemp:Real;
    btemp:Currency;
begin
  Result := nil;
  if Length(linein) < 243 then Exit;
  // checking whether we parsing a line from 'good' or 'north' tables
  if linein[32] = '&' then begin
      isgood := True;
      soff := 5;
      nlength := 31;
  end else begin
    soff := 0;
    nlength := 26;
  end;
  // extracting and cleaning extra spaces from the id.
  nameid := Trim(Copy(linein,1,nlength));
  nameid := DelSpace1(nameid);
  // position (in degrees)
  buffer1 :=  Trim(Copy(linein,29+soff,12));
  buffer2 :=  Trim(Copy(linein,44+soff,12));
  // setting up the output object
  impres := ImportedData.Create;
  impres.AddNameID('',nameid);

  if (not impres.SetDecimalRA(buffer1)) then begin
     impres.Free;   Exit;
  end;
  if (not impres.SetDecimalDec(buffer2)) then begin
     impres.Free;   Exit;
  end;
  // for some seeminly un-necessary reason, there is an extra space in the 'good' table
  if isgood then soff += 1;
  // parallax...
  buffer1 :=  Trim(Copy(linein,95+soff,7));
  buffer2 :=  Trim(Copy(linein,106+soff,5));
  if not StrToRealBoth(buffer1,buffer2,impres.pllx,impres.pllx_err) then begin
     impres.Free;   Exit;
  end;
  // proper motion
  buffer1 := Trim(Copy(linein,123+soff,9));
  buffer2 := Trim(Copy(linein,143+soff,9));
  if (not impres.SetProperMotionPartsM(buffer1,buffer2)) then begin
      impres.Free;   Exit;
  end;

  // B and V fluxes
  SubstrCurr(linein,194+soff,7,impres.Bmag); // can fail, okay
  buffer1 := Trim(Copy(linein,204+soff,7));
  StrToReal(buffer1,impres.Vmag);             // can fail, okay
  // g' r' i' fluxes
  buffer1 := Trim(Copy(linein,214+soff,7));
  buffer2 := Trim(Copy(linein,224+soff,7));
  if StrToRealBoth(buffer1,buffer2,gpv,rpv) then begin
     buffer1 := Trim(Copy(linein,234+soff,7));
     if StrToReal(buffer1,ipv) then begin
        // only here can we convert g' r' i' to BVRcIc
        Pgri_to_BVRI(gpv,rpv,ipv,btemp,vtemp,impres.Rmag,impres.Imag);
        if impres.Vmag > 99 then impres.Vmag := vtemp;
        if impres.Bmag > 99 then impres.Bmag := btemp;
     end;
  end;

  // done
  Result := impres;
end;


function MakeURATS_Const():ImportParameters;
begin
    Result := ImportParameters.Create;
    // extra write info
    Result.idheader := 'ID?Name';
    Result.parser := ParseURATS_Line;
    Result.lineskip := 66;

    Result.useB_Flux := True;
    Result.useVRI_Flux := True;
    Result.epochdata := 2015.5;

    // source info
    Result.pllx_sourceid := 'URATS';
    Result.fullname := 'URAT South Parallax Results: Discovery of New Nearby Stars (Finch+ 2018)';
    Result.paperurl := 'https://arxiv.org/abs/1802.08272';
    Result.fullout := 'urats.csv';
    Result.leftout := 'urats_leftover.csv';
end;

//=====================================================================
begin
  urats_params := MakeURATS_Const();
end.

