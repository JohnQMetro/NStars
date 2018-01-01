unit NewStar;

{$mode delphi}

(* intended to provide a replacements for the Star class that better match up
with the relative lack of hard information. *)

interface

uses
  Classes, SysUtils, StrUtils, Math, Character, fgl,
    StarDataBase, df_strings, StarEstimator, StarExt2,constellation, unitdata;

const ComponentStrings:array[0..21] of string = ('','A','B','C','D','AB','BC',
      'CD','Aa','Ab','Ac','Ba','Bb','Ca','Cb','Da','Db','Aab','AC','AD','E','CE');

const VStarType:array[0..6] of string = (' ','Variable','UV Ceti type',
          'BY Draconis type',  'Delta Scuti type', 'ZZ Ceti type','RS Can.Ven. type');
const TypeLabels:array[0..7] of string = ('Unknown', 'Brown Dwarf', 'White Dwarf',
      'SubDwarf Star', 'Red Dwarf','Ordinary Star','SubGiant Star','Giant Star');

type

StarClass = ( UNKNOWN, BROWN_DWARF, WHITE_DWARF, SUBDWARF, RED_DWARF, MAIN_SEQUENCE,
              SUBGIANT, GIANT );

VariableTypeEnum = ( NOT_VARIABLE, VARIABLE, UV_CETI, BY_DRACONIS, DETA_SCUTI,
                      ZZ_CETI, RS_CAN_VENAT);


// base class for stars and brown dwarves
NewStarBase = class(StarBase)
  protected
    completter:string;
    spectraltype:string;
    // property methods
    function GisBrownDwarf:Boolean; virtual; abstract;
    function GisWhiteDwarf:Boolean; virtual; abstract;
    function GetComp:string;
    procedure SetComp(RHS:string);
    function GetSpec:string;
    procedure SetSpec(RHS:string);
    function MinCC:Integer; virtual; abstract;
    function MaxCC:Integer; virtual; abstract;
    // helper method
    function MakeOutputLineOne(isBD:Boolean):string;
  public
    fluxtemp:StarFluxPlus;
    // properties
    property isBrownDwarf:Boolean read GisBrownDwarf;
    property isWhiteDwarf:Boolean read GisWhiteDwarf;
    property Component:string read GetComp write SetComp;
    property SpectralClass:string read GetSpec  write SetSpec;
    property MinPartCount:Integer read MinCC;
    property MaxPartCount:Integer read MaxCC;
    // convenience methods for the spectral type
    function isSpectralTypeEmpty:Boolean;
    function ClassifySpectralType:StarClass;
    // text file I/O
    function WriteToTextFile(var outfile:TextFile):Boolean; virtual; abstract;
    function ReadRestFromTextFile(var infile:TextFile; out errmsg:string):Boolean; virtual; abstract;
    // more convenience methods
    function GetProperName(out pname:string; inconstl:Integer):Boolean;
    // also
    destructor Destroy; override;

end;

// class for brown dwarves (and sub-brown dwarves)
BrownDwarfInfo = class(NewStarBase)
  protected
    (* mass in jupiters. since there are no calculations, stored as strings to
    make I/O easy and avoid decimal to binary and back precision loss. *)
    mass,munc:string;
    // property methods
    function GisBrownDwarf:Boolean; override;
    function GisWhiteDwarf:Boolean; override;
    function GetMM:string;
    function GetMU:string;
    function MMIsZ:Boolean;
    function GetSunMassStr:string;
    function MinCC:Integer; override;
    function MaxCC:Integer; override;
    // non-property method
    function CheckMassValues(const mass_in, munc_in:Real):Boolean;
  public
    // mass properties
    property MedianMass:string read GetMM;
    property MassUncertainty:string read GetMU;
    property MassNotSet:Boolean read MMIsZ;
    property MassInSuns:string read GetSunMassStr;
    // mass methods
    function SetMassString(mass_str,munc_str:string):Boolean;
    // implemented text I/O methods
    function WriteToTextFile(var outfile:TextFile):Boolean; override;
    function ReadRestFromTextFile(var infile:TextFile; out errmsg:string):Boolean; override;
end;

// class for stars
StarInfo = class(NewStarBase)
  protected
    // private data
    visual_magnitude:Real;
    secondary_mag:Real;
    // property methods
    function GisBrownDwarf:Boolean; override;
    function GisWhiteDwarf:Boolean; override;
    function GetVM:Real;
    function GetVMstr:string;
    function GetVMSet:Boolean;
    function MinCC:Integer; override;
    function MaxCC:Integer; override;
    function GSecMag:Real;
    function VSecMag:Boolean;
    function GSecMagStr:string;
  public
    // public data
    Arity:ArityType;
    VariableType:VariableTypeEnum;
    ExtraInfo:StarExtraData;
    premain:Boolean;
    // special
    estimator:EstimationParser;
    // properties
    property ValidVisualMagnitude:Boolean read GetVMSet;
    property VisualMagnitude:Real read GetVM;
    property VisualMagnitudeString:string read GetVMstr;
    property ValidSecondaryMagnitude:Boolean read VSecMag;
    property SecondaryMagnitude:Real read GSecMag;
    property SecondaryMagnitudeStr:string read GSecMagStr;
    // constructor
    constructor Create;
    destructor Destroy; override;
    // magnitude and luminosity methods
    function SetVisualMagnitude(invm:Real):Boolean;
    function SetVisualMagnitudeStr(invm:string):Boolean;
    function SetSecondaryMagnitudeStr(invm:string):Boolean;
    // estimation setup and change
    function InitializeEstimation(const parallax:Real):Boolean;
    function NonSpectraChange(const parallax:Real):Boolean;
    // getting values, estimated or specified
    procedure GetMassAndMUnc(out mass,munc:Real);
    function GetMassString:string;
    function GetBolometricString:string;
    function GetRadiusString:string;
    function GetMassRadiusLuminosity:string;
    // implemented text I/O methods
    function WriteToTextFile(var outfile:TextFile):Boolean; override;
    function ReadRestFromTextFile(var infile:TextFile; out errmsg:string):Boolean; override;
end;

ComponentList = TFPGObjectList<NewStarBase>;

// function that properly loads a brown dwarf or a star from a file
function LoadStarBase(var infile:TextFile):NewStarBase;

function SplitComponent(compin:string; first:Boolean):string;

var half_mag:Real;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation

//=============================================================
function NewStarBase.GetComp;
begin Result := completter; end;
//------------------------------------
procedure NewStarBase.SetComp(RHS:string);
begin
  if AnsiMatchStr(RHS,ComponentStrings) then completter := RHS;
end;
//------------------------------------
function NewStarBase.GetSpec:string;
begin Result := spectraltype;  end;
//------------------------------------
procedure NewStarBase.SetSpec(RHS:string);
var trimstr:string;  cchar:Char;
begin
  trimstr:= Trim(RHS);
  if AnsiContainsStr(trimstr,';') then Exit;
  if AnsiContainsStr(trimstr,#10) then Exit;
  if AnsiContainsStr(trimstr,#13) then Exit;
  cchar := Chr(133);
  if AnsiContainsStr(trimstr,cchar) then Exit;
  spectraltype := trimstr;
end;

//+++++++++++++++++++++++++++++++++++++++
function NewStarBase.MakeOutputLineOne(isBD:Boolean):string;
begin
  Result := '***;';
  Result += IfThen(isBD,'Brown Dwarf','Star') + ';';
  Result += completter + ';';
  Result += Bool2Str(nameset <> nil) + ';' + Bool2Str(the_location <> nil) + ';';
  Result += spectraltype + ';' + Bool2Str(fluxtemp <> nil);
end;

//+++++++++++++++++++++++++++++++++++++++
function NewStarBase.isSpectralTypeEmpty:Boolean;
begin
  Result := (Length(spectraltype)=0);
end;

(*UNKNOWN, BROWN_DWARF, WHITE_DWARF, SUBDWARF, RED_DWARF, MAIN_SEQUENCE,
              SUBGIANT, GIANT *)
function NewStarBase.ClassifySpectralType:StarClass;
var firstchar:string;
begin
  if Length(spectraltype) = 0 then Result := UNKNOWN
  else if AnsiStartsStr('D',spectraltype) then Result := WHITE_DWARF
  else if spectraltype = 'WD' then Result := WHITE_DWARF
  else if AnsiContainsStr(spectraltype,'VII') then Result := WHITE_DWARF
  else begin
    firstchar := AnsiLeftStr(spectraltype,1);
    if AnsiContainsStr('LYT',firstchar) then Result := BROWN_DWARF
    else if AnsiContainsStr(spectraltype,'VI') then Result := SUBDWARF
    else if AnsiContainsStr(spectraltype,'IV') then Result := SUBGIANT
    else if AnsiContainsStr(spectraltype,'III') then Result := GIANT
    else if firstchar = 'M' then Result := RED_DWARF
    else if AnsiContainsStr(spectraltype,'V') then Result := MAIN_SEQUENCE
    else if AnsiStartsStr('esd',spectraltype) then Result := SUBDWARF
    else if AnsiStartsStr('sd',spectraltype) then Result := SUBDWARF
    else Result := UNKNOWN
  end;
end;
//-----------------------------------------------------------
// more convenience methods
function NewStarBase.GetProperName(out pname:string; inconstl:Integer):Boolean;
var xname:string;
    drapos,ddecpos:Real;
    xconstl:Integer;
begin
  // getting the nameset, if any
  Result := False;
  if nameset = nil then Exit;
  pname := Trim(nameset.proper_name);
  // we use actual proper name, or variable name
  Result := True;
  if Length(pname)<>0 then Exit;
  xname := Trim(nameset.var_designation);
  // if we are using a variable name, we need the constellation...
  if Length(xname)<>0 then begin
    // getting constellation index
    if the_location<>nil then begin
      drapos := the_location.GetDecimalRightAscension;
      ddecpos := the_location.GetDecimalDeclination;
      xconstl := Const_Data.LocatePoint(drapos,ddecpos,the_location.Epoch);
    end
    else xconstl := inconstl;
    // building the full name
    pname := xname + ' ' + constellations[(xconstl-1)*3+1];
  end
  else Result := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// also
destructor NewStarBase.Destroy;
begin
  if nameset<>nil then nameset.Free;
  if the_location<>nil then the_location.Free;
  if fluxtemp<>nil then fluxtemp.Free;
  inherited;
end;

//=============================================================
// propery methods
//--------------------------
function BrownDwarfInfo.GisBrownDwarf:Boolean;
begin  Result := True;   end;
//---------------------------
function BrownDwarfInfo.GisWhiteDwarf:Boolean;
begin  Result := False;  end;
//----------------------------
function BrownDwarfInfo.GetMM:string;
begin  Result := mass;   end;
//---------------------------------
function BrownDwarfInfo.GetMU:string;
begin  Result := munc;    end;
//---------------------------------
function BrownDwarfInfo.MMIsZ:Boolean;
var sc:Integer;
    xm:Real;
begin
  Val(mass,xm,sc);
  Assert(sc=0);
  Result := (xm = 0);
end;
//--------------------------------
function BrownDwarfInfo.GetSunMassStr:string;
var sc:Integer;
    xm:Real;
begin
  Val(mass,xm,sc);
  Assert(sc=0);
  xm := xm /1047.56;
  Result := FloatToStrF(xm,ffGeneral,2,0);
end;
//---------------------------------
function BrownDwarfInfo.MinCC:Integer;
begin  Result := 1;    end;
//---------------------------------
function BrownDwarfInfo.MaxCC:Integer;
begin  Result := 1;    end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// mass methods
function BrownDwarfInfo.CheckMassValues(const mass_in, munc_in:Real):Boolean;
begin
  Result := False;
  if (mass_in < 0) then Exit;
  if (munc_in < 0) then Exit;
  if (mass_in >= 100) then Exit;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
function BrownDwarfInfo.SetMassString(mass_str,munc_str:string):Boolean;
var sc:Integer;  mass1, mass2:Real;
begin
  Result := False;
  if mass_str = '' then mass_str := '00.0'
  else if Trim(mass_str) = '.' then mass_str := '00.0';
  if munc_str = '' then munc_str := '00.0'
  else if Trim(munc_str) = '.' then munc_str := '00.0';
  Val(Trim(mass_str),mass1,sc);
  if sc<>0 then Exit;
  Val(Trim(munc_str),mass2,sc);
  if sc<>0 then Exit;
  if not CheckMassValues(mass1,mass2) then Exit;
  mass := mass_str;
  munc := munc_str;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++
// implemented text I/O methods
//----------------------------------
function BrownDwarfInfo.WriteToTextFile(var outfile:TextFile):Boolean;
var outline:string;
begin
  Result := True;
  // no checking to see if outfile is okay
  outline := MakeOutputLineOne(True);
  Writeln(outfile,outline);
  // the only unique thing about brown dwarves is the mass
  outline := mass + ';' + munc;
  Writeln(outfile,outline);
  // notes
  Writeln(outfile,GetNotesFile);
  // name and location, if need be
  if nameset <> nil then Writeln(outfile,nameset.ToIOString);
  if the_location <> nil then Writeln(outfile,the_location.ConvertAllToString);
  // fluxes, temperature, and Fe/H
  if fluxtemp <> nil then Writeln(outfile,fluxtemp.ToIOString);

end;
//----------------------------------
(* does not handle line one (since that might indicate different classes. *)
function BrownDwarfInfo.ReadRestFromTextFile(var infile:TextFile; out errmsg:string):Boolean;
var cline:string;  line2:TStringList;
begin
  Result := False;
  // mass and mass uncertainty line
  Readln(infile,cline);
  errmsg := 'Brown Dwarf mass line is empty';
  if Length(cline) = 0 then Exit;
  // parsing into the two fields...annoyingly complicated
  line2 := SplitWithDelim(cline,';',2);
  errmsg := 'Brown Dwarf mass line does not hold valid values!';
  if line2 = nil then Exit;
  if not SetMassString(line2[0],line2[1]) then begin
     line2.Free;    Exit;
  end
  else line2.Free;
  // the notes
  Readln(infile,cline);
  SetNotesFile(cline);
  // nameset...
  if (nameset<>nil) then begin
    Readln(infile,cline);
    nameset.FromString(cline);
  end;
  // location...
  if (the_location<>nil) then begin
    Readln(infile,cline);
    errmsg := 'Unable to convert inputted location data!';
    // if not the_location.ConvertAllFromStringOldC(cline,parentpllx,parentpllxe) then Assert(False);
    if not the_location.ConvertAllFromString(cline) then Assert(False);
  end;
  // fluxes, temperatures, and Fe/H
  if (fluxtemp<>nil) then begin
    Readln(infile,cline);
    errmsg := 'Unable to convert input fluxes/temperature/FeH!';
    if not fluxtemp.FromIOString(cline) then Exit;
  end;
  // done
  Result := True;
end;
//=============================================================
// property methods )not public)
//-------------------------
function StarInfo.GisBrownDwarf:Boolean;
begin   Result := False;    end;
//-------------------------
function StarInfo.GisWhiteDwarf:Boolean;
begin
  Result := AnsiStartsStr('D',spectraltype);
end;
//-------------------------
function StarInfo.GetVM:Real;
begin   Result := visual_magnitude;    end;
//-------------------------
function StarInfo.GetVMstr:string;
begin
  if (visual_magnitude = 9999.0) then Result := 'N'
  else Result := Real2Str(visual_magnitude,3,2,True,True);
end;
//-------------------------
function StarInfo.GetVMSet:Boolean;
begin   Result := (visual_magnitude <> 9999.0);   end;
//---------------------------------
function StarInfo.MinCC:Integer;
begin
  if Ord(Arity) >= 2 then Result := 2
  else Result := 1;
end;
//---------------------------------
function StarInfo.MaxCC:Integer;
begin
  if Arity = DOUBLE_OR_MULTIPE then Result := 3
  else if Arity = SINGLE then Result := 1
  else Result := 2;
end;
//---------------------------------------
function StarInfo.GSecMag:Real;
begin    Result := secondary_mag;   end;
//---------------------------------------
function StarInfo.VSecMag:Boolean;
begin
  Result := False;
  if arity = SINGLE then Exit;
  if arity = POSSIBLY_DOUBLE then Exit;
  if secondary_mag > 90 then Exit;
  Result :=  secondary_mag >= (visual_magnitude+half_mag);
end;
//------------------------------------------
function StarInfo.GSecMagStr:string;
begin
  Result := Trim(FloatToStrF(secondary_mag,ffFixed,2,1));
  if secondary_mag < 10 then Result := '0' + Result;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++
// constructor
constructor StarInfo.Create;
begin
  inherited;
  visual_magnitude := 9999.0;
  Arity := SINGLE;
  VariableType := NOT_VARIABLE;
  estimator := nil;
  fluxtemp := nil;
  secondary_mag := 99.9;
  premain := False;
end;
//-----------------------------
destructor StarInfo.Destroy;
begin
  if ExtraInfo <> nil then ExtraInfo.Free;
  estimator.Free;
  inherited;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++
// magnitude and luminosity methods
//-------------------------
function StarInfo.SetVisualMagnitude(invm:Real):Boolean;
begin
  Result := False;
  if (invm > 36  ) then Exit;  // probably too dim
  if (invm < -41 ) then Exit; // much brighter than Sirius, here for fantasy
  Result := True;
  visual_magnitude := invm;
end;
//-------------------------
function StarInfo.SetVisualMagnitudeStr(invm:string):Boolean;
var sc:Integer;   resval:Real;
begin
  Result := False;
  invm := Trim(invm);
  Val(invm,resval,sc);
  if sc<>0 then Exit;
  Result := SetVisualMagnitude(resval);
end;
//-------------------------------------------------
function StarInfo.SetSecondaryMagnitudeStr(invm:string):Boolean;
var sc:Integer;   resval:Real;
begin
  Result := False;
  invm := Trim(invm);
  Val(invm,resval,sc);
  if sc<>0 then Exit;
  if (resval < 90) and (resval < (visual_magnitude+half_mag)) then Exit;
  Result := True;
  if (resval < 90) then secondary_mag := resval;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// estimation setup and change
//--------------------------------
function StarInfo.InitializeEstimation(const parallax:Real):Boolean;
begin
  if estimator = nil then estimator := EstimationParser.Create();
  // two options, with extra data
  if fluxtemp <> nil then begin
    Result := estimator.SetAll(visual_magnitude,parallax,spectraltype,premain,fluxtemp);
  end
  // and without extra data
  else begin
    Result := estimator.SetSimple(visual_magnitude,parallax,spectraltype,premain);
  end;
end;
//--------------------------------
function StarInfo.NonSpectraChange(const parallax:Real):Boolean;
begin
  Result := False;
  if estimator = nil then Exit;
  // complex versus simple change
  if fluxtemp <> nil then begin
    Result := estimator.SetOther(visual_magnitude,parallax,fluxtemp);
  end
  else Result := estimator.SetSimpleOther(visual_magnitude,parallax);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure StarInfo.GetMassAndMUnc(out mass,munc:Real);
var munc_str:string;
    sc:Integer;
    massx,muncx:Real;
begin
  // null info
  mass := 0;  munc := 0;
  if ExtraInfo = nil then Exit;
  // actual mass
  munc_str := Trim(ExtraInfo.MassUncertainty);
  massx := ExtraInfo.MassN;
  Val(munc_str,muncx,sc);
  if (sc<>0) then Exit;
  // done
  mass := massx;
  munc := muncx;
end;
//-----------------------------------------------
function StarInfo.GetMassString:string;
var massval:Real;
begin
  Result := '';
  if ExtraInfo<>nil then begin
    massval := ExtraInfo.MassN;
    if (massval = 0) and (estimator<>nil) then Result := estimator.MassEstimateExport
    else if (massval<>0) then Result := FloatToStrF(massval,ffGeneral,3,0);
  end
  else if estimator<>nil then Result := estimator.MassEstimateExport;
end;
//-----------------------------------------------
function StarInfo.GetBolometricString:string;
var boloval:Real;
    bolok:Boolean;
begin
  Result := '';
  if ExtraInfo<>nil then begin
    boloval := ExtraInfo.BoloLumN;
    bolok := (boloval>0);
    if (not bolok) then Result := estimator.BoloLuminosityEstExport
    else Result := FloatToStrF(boloval,ffGeneral,3,0);
  end
  else if  (estimator<>nil) then Result := estimator.BoloLuminosityEstExport;
end;
//-----------------------------------------------
function StarInfo.GetRadiusString:string;
var radival:Real;
begin
  Result := '';
  if ExtraInfo<>nil then begin
    radival := ExtraInfo.DiameterN;
    if (radival = 0) and (estimator<>nil) then Result := estimator.RadiusEstimateExport
    else Result := FloatToStrF(radival,ffGeneral,3,0);
  end
  else if estimator<>nil then Result := estimator.RadiusEstimateExport;
end;
//-------------------------------------------------
function StarInfo.GetMassRadiusLuminosity:string;
var strm,strr,strl:string;
begin
  strm := GetMassString;
  if strm = '' then strm := '1';
  strr := GetRadiusString;
  if strr = '' then strr := '1';
  Result := strm + ',' + strr + ',';
  strl := GetBolometricString;
  if strl = '' then strl := '1';
  Result += strl;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++
// implemented text I/O methods
//-------------------------
function StarInfo.WriteToTextFile(var outfile:TextFile):Boolean;
var outline:string;
begin
   Result := True;
  // no checking to see if outfile is okay
  outline := MakeOutputLineOne(False);
  Writeln(outfile,outline);
  // star data includes magnitude, arity, pre-main seq, and perhaps secondary mag
  outline := VisualMagnitudeString + ';' + ArityLabels[Ord(Arity)] + ';';
  outline += Bool2Str(premain) + ';';
  outline += VStarType[Ord(VariableType)] + ';';
  outline += Bool2Str(ExtraInfo <> nil);
  if VSecMag then outline += ';' + FloatToStrF(secondary_mag,ffFixed,2,1);
  Writeln(outfile,outline);
  // notes
  Writeln(outfile,GetNotesFile);
  // name and location, if need be
  if nameset <> nil then Writeln(outfile,nameset.ToIOString);
  if the_location <> nil then Writeln(outfile,the_location.ConvertAllToString);
  // extra data
  if (ExtraInfo <> nil) then begin
    outline := ExtraInfo.ToOutputString;
    Writeln(outfile,outline);
  end;
  // fluxes, temperature, and Fe/H
  if fluxtemp <> nil then Writeln(outfile,fluxtemp.ToIOString);
end;

//-------------------------
function StarInfo.ReadRestFromTextFile(var infile:TextFile; out errmsg:string):Boolean;
var cline:string;  line2:TStringList;
    indx:Integer;
    xtest:Boolean;
begin
  Result := False;
  // standard star data
  Readln(infile,cline);
  errmsg := 'Star data line is empty';
  if Length(cline) = 0 then Exit;
  // parsing into fields
  line2 := SplitWithDelim(cline,';',5);
  errmsg := 'Star Data line lacking in fields!';
  if line2 = nil then Exit;
  if line2[0] = 'N' then visual_magnitude := 9999.0
  else if ( not SetVisualMagnitudeStr(line2[0]) ) then begin
     errmsg := 'Visual luminosty is bad!';
     line2.Free;    Exit;
  end;
  // arity
  indx := AnsiIndexStr(line2[1],ArityLabels);
  if (indx<0) then begin
    errmsg := 'Arity Label not recognized!';
    line2.Free;     Exit;
  end;
  Arity := ArityType(indx);
  // I've replaced the previous Flare Star with Pre-Main-Sequence
  xtest := Str2Bool(line2[2],premain);
  Assert(xtest);
  // variable type
  indx := AnsiIndexStr(line2[3],VStarType);
  if indx<0 then begin
    errmsg := 'Variable star not recognized!';
    Exit;
  end;
  VariableType := VariableTypeEnum(indx);
  // do we have extra data
  if Str2BoolR(line2[4]) then ExtraInfo := StarExtraData.Create;
  // optional secondary magnitude
  if (line2.Count >= 6) then StrToReal(line2[5],secondary_mag);
  line2.Free;
  // the notes
  Readln(infile,cline);
  SetNotesFile(cline);
  // nameset...
  if (nameset<>nil) then begin
    Readln(infile,cline);
    nameset.FromString(cline);
  end;
  // location...
  if (the_location<>nil) then begin
    Readln(infile,cline);
    errmsg := 'Unable to convert inputted location data!';
    if not the_location.ConvertAllFromString(cline) then Assert(False);
    // if not the_location.ConvertAllFromStringOldC(cline,parentpllx,parentpllxe) then Assert(False);
  end;
  // possible extra data
  if ExtraInfo <> nil then begin
    Readln(infile,cline);
    if not ExtraInfo.FromInputString(cline) then begin
      errmsg := 'Extra Data not correct!';
      Exit;
    end;
  end;
  // fluxes, temperatures, and Fe/H
  if (fluxtemp<>nil) then begin
    Readln(infile,cline);
    errmsg := 'Unable to convert input fluxes/temperature/FeH!';
    if not fluxtemp.FromIOString(cline) then Exit;
  end;
  // done
  Result := True;
end;

//=============================================================
// function that properly loads a brown dwarf or a star from a file
function LoadStarBase(var infile:TextFile):NewStarBase;
var cline,buffer:string;       clist:TStringList;
    bdw:BrownDwarfInfo; stw:StarInfo;
    isstar:Boolean;
    bool1,bool2:Boolean;
begin
  // reading and initial checking...
  Readln(infile,cline);
  if Length(cline) = 0 then raise Exception.Create('First star/brown dwarf line is empty!');
  clist := SplitWithDelim(cline,';',6);
  if clist = nil then begin
    clist.Free;
    raise Exception.Create('First star/brown dwarf line is lacking fields!');
  end;
  if clist[0] <> '***' then begin
    clist.Free;
    raise Exception.Create('First star/brown dwarf field MUST be *** !');
  end;
  // next: is this a star, or a brown dwarf?
  isstar := (clist[1] = 'Star');
  if isstar then stw := StarInfo.Create
  else bdw := BrownDwarfInfo.Create;
  if isstar then Result := stw
  else Result := bdw;
  // loading up the common values
  Result.completter := clist[2];
  // nameset and location
  bool1 := Str2BoolR(clist[3]);
  bool2 := Str2BoolR(clist[4]);
  Result.SetupNameLocation(bool1,bool2);
  // finishing off line 1
  buffer := clist[5];
  // if AnsiEndsStr('?',buffer) then buffer := Trim(Copy(buffer,1,Length(buffer)-1));
  Result.SpectralClass := buffer;
  // the seventh (optional, for now) shows whether we have extra flux/temp/feh
  if clist.Count >=7 then begin
    bool1 := Str2BoolR(clist[6]);
    if bool1 then Result.fluxtemp := StarFluxPlus.Create
    else Result.fluxtemp := nil;
  end
  else Result.fluxtemp := nil;
  clist.Free;
  // conversion from old to new only
(* Result.parentpllx := parent_pllx;
   Result.parentpllxe := parent_pllxe;*)
  // now, we go on to finish off the file loading
  if isstar then begin
    bool1 := stw.ReadRestFromTextFile(infile,cline);
    if not bool1 then begin
      stw.Free;
      Result := nil;
      raise Exception.Create('Star read error: ' + cline);
    end;
  end
  else begin
    bool1 := bdw.ReadRestFromTextFile(infile,cline);
    if not bool1 then begin
      bdw.Free;
      Result := nil;
      raise Exception.Create('Brown Dwarf read error : ' + cline);
    end;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++=
function SplitComponent(compin:string; first:Boolean):string;
var inlen:Integer;  seclow:Boolean;
begin
  inlen := Length(compin);
  if inlen = 0 then begin
    if first then Result := 'A'
    else Result := 'B';
  end
  else if inlen = 1 then begin
    if first then Result := compin + 'a'
    else Result := compin + 'b';
  end
  else if inlen = 2 then begin
    seclow := IsLower(compin[2]);
    if first and seclow then Result := compin + '1'
    else if seclow and (not first) then Result := compin + '2'
    else if first then Result := compin[1]
    else Result := compin[2];
  end
  else if compin = 'Aab' then begin
    if first then Result := 'Aa'
    else Result := 'Ab';
  end
  else Assert(False);
end;

//=============================================================
begin
  half_mag := 2.5*log10(2);
end.

