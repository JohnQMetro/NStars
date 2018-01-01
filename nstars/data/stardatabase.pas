unit StarDataBase;

{$mode delphi}

interface

uses
  Classes, SysUtils, StrUtils,
  namedata, df_strings, newlocation;

type

(* a base class. Stars and star systems can both have names and locations... *)
StarBase = class
  protected
    the_location:Location;
    nameset:StarName;
  public
    notes:string;
    // location
    function HasLocation:Boolean;
    function GetLocation:Location;
    procedure ClearLocation;
    function CopyLocation(inval:Location; bincp:Boolean):Boolean;
    procedure InsertLocation(inval:Location);
    function CopyMotion(inval:Location):Boolean;
    // nameset
    function HasNames:Boolean;
    function GetNames:StarName;
    function MakeOrGetNames:StarName;
    procedure ClearNames;
    function CopyNames(inval:StarName):Boolean;
    function TestClearNames:Boolean;
    // notes for file i/o
    function GetNotesFile:string;
    procedure SetNotesFile(inval:string);
    procedure AppndNote(const inval:string; inclnl:Boolean);
    function AreNotesEmpty:Boolean;
    function NotesConatins(inmatch:string):Boolean;
    // additional method
    procedure SetupNameLocation(doname,dolocat:Boolean);
end;

(* Extended data for stars, entered rather than calculated *)
StarExtraData = class
  protected
    // the data, numbers stored as strings
    mass,munc:string;
    bluminosity,blunc:string;
    age,agerange:string;
    dia,diarange:string;
    // converted values for quick access
    num_mass,num_age,num_blum,num_dia:Real;
    // property read functions
    function MassRead:string;
    function MUncRead:string;
    function BLumRead:string;
    function BLUncRead:string;
    function AgeRead:string;
    function AgeUncRead:string;
    function DiamRead:string;
    function DiaUncRead:string;
    // non string property read function
    function NumMassRead:Real;
    function NumAgeRead:Real;
    function NumBLumRead:Real;
    function NumDiaRead:Real;
  public
    // read-only properties
    property MassMedian:string read MassRead;
    property MassUncertainty:string read MUncRead;
    property BolometricLumMedian:string read BLumRead;
    property BolometricLumUncertainty:string read BLUncRead;
    property AgeMedian:string read AgeRead;
    property AgeUncertainty:string read AgeUncRead;
    property DiameterMedian:string read DiamRead;
    property DiameterUncertainty:string read DiaUncRead;
    // read-only numeric properties
    property MassN:Real read NumMassRead;
    property AgeN:Real read NumAgeRead;
    property BoloLumN:Real read NumBLumRead;
    property DiameterN:Real read NumDiaRead;
    // constructor
    constructor Create;
    // setting methods
    function SetMass(const median,uncertainty:string):Boolean;
    function SetBolometric(const median,uncertainty:string):Boolean;
    function SetAge(const median,uncertainty:string):Boolean;
    function SetDiameter(const median,uncertainty:string):Boolean;
    // test function
    function HasNoData():Boolean;
    // text io
    function ToOutputString:string;
    function FromInputString(const instring:string):Boolean;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
// location
function StarBase.HasLocation:Boolean;
begin
  Result := (the_location<>nil);
end;
//---------------------------------------------------------
function StarBase.GetLocation:Location;
begin
  Result := the_location;
end;
//---------------------------------------------------------
procedure StarBase.ClearLocation;
begin   FreeAndNil(the_location);   end;
//--------------------------------------------------------
function StarBase.CopyLocation(inval:Location; bincp:Boolean):Boolean;
begin
  Result := False;
  if (inval = nil) then Exit;
  if the_location<>nil then the_location.Free;
  the_location := Location.Create(inval,bincp);
  Result := True;
end;
//-------------------------------------------------------
procedure StarBase.InsertLocation(inval:Location);
begin
  Assert(inval<>nil);
  FreeAndNil(the_location);
  the_location := inval;
end;
//--------------------------------------------------------
function StarBase.CopyMotion(inval:Location):Boolean;
var pmmag, pmang:Real;
begin
  Result := False;
  if the_location = nil then Exit;
  if inval = nil then Exit;
  // copying proper motion
  pmmag := inval.ProperMotionMagnitude;
  pmang := inval.ProperMotionAngle;
  Result := the_location.SetProperMotion(pmmag,pmang);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// nameset
//---------------------------------------------------------
function StarBase.HasNames:Boolean;
begin
  Result := (nameset<>nil);
end;
//---------------------------------------------------------
function StarBase.GetNames:StarName;
begin
  Result := nameset;
end;
//---------------------------------------------------------
function StarBase.MakeOrGetNames:StarName;
begin
  if nameset = nil then nameset := StarName.Create;
  Result := nameset;
end;
//---------------------------------------------------------
procedure StarBase.ClearNames;
begin
  nameset.Free;
  nameset := nil;
end;
//---------------------------------------------------------
function StarBase.CopyNames(inval:StarName):Boolean;
begin
  Result := False;
  if (inval = nil) then Exit;
  if nameset<>nil then nameset.Free;
  nameset := StarName.Create(inval);
  Result := True;
end;
//-------------------------------------------------
function StarBase.TestClearNames:Boolean;
begin
  Result := False;
  if nameset = nil then Exit;
  if nameset.IsEmpty then begin
    nameset.Free;
    Exit;
  end;
  Result := True;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// notes for file i/o
//---------------------------------------------------------
function StarBase.GetNotesFile:string;
begin
  Result := AnsiReplaceStr(notes,#13#10,'$#@!#');
end;
//---------------------------------------------------------
procedure StarBase.SetNotesFile(inval:string);
begin
  notes := AnsiReplaceStr(inval,'$#@!#',#13#10);
end;
//-----------------------------------------------------------
procedure StarBase.AppndNote(const inval:string; inclnl:Boolean);
begin
  if inclnl then notes := notes +sLineBreak;
  notes := notes + inval;
  notes := Trim(notes);
end;
//-----------------------------------------------------
function StarBase.AreNotesEmpty:Boolean;
begin
  Result := (Length(notes)=0);
end;
//-----------------------------------------------------
function StarBase.NotesConatins(inmatch:string):Boolean;
begin
  Result := AnsiContainsStr(notes,inmatch);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// additional method
procedure StarBase.SetupNameLocation(doname,dolocat:Boolean);
begin
  if doname then nameset := StarName.Create
  else nameset := nil;
  if dolocat then the_location := Location.Create
  else the_location := nil;
end;

//==========================================================================
// property read functions
function StarExtraData.MassRead:string;
begin    Result := mass;    end;
function StarExtraData.MUncRead:string;
begin    Result := munc;    end;
function StarExtraData.BLumRead:string;
begin    Result := bluminosity;    end;
function StarExtraData.BLUncRead:string;
begin    Result := blunc;    end;
function StarExtraData.AgeRead:string;
begin    Result := age;    end;
function StarExtraData.AgeUncRead:string;
begin    Result := agerange;    end;
function StarExtraData.DiamRead:string;
begin    Result := dia;    end;
function StarExtraData.DiaUncRead:string;
begin    Result := diarange;    end;
//+++++++++++++++++++++++++++++++++++++++++++++
// non string property read function
function StarExtraData.NumMassRead:Real;
begin    Result := num_mass;    end;
function StarExtraData.NumAgeRead:Real;
begin    Result := num_age;    end;
function StarExtraData.NumBLumRead:Real;
begin    Result := num_blum;    end;
function StarExtraData.NumDiaRead:Real;
begin    Result := num_dia;    end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// constructor
constructor StarExtraData.Create;
begin
  mass := ' 0.000';
  munc := ' 0.000';
  age := ' 0.00';
  agerange := ' 0.00';
  dia := ' 00.000';
  diarange := ' 00.000';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// setting methods
//---------------------------
function StarExtraData.SetMass(const median,uncertainty:string):Boolean;
var val1,val2:Real;
begin
  Result := False;
  if not StrToRealBothNN(median,uncertainty,val1,val2) then Exit;
  if val1 > 300 then Exit;
  mass := median;
  munc := uncertainty;
  num_mass := val1;
  Result := True;
end;
//---------------------------
function StarExtraData.SetBolometric(const median,uncertainty:string):Boolean;
var val2:Real;    tsval1,tsval2:string;
begin
  Result := False;
  tsval1 := Trim(median);
  tsval2 := Trim(uncertainty);
  // handling empty input (equal to zero...)
  if Length(tsval1) = 0 then begin
    num_blum := 0;
    bluminosity := '';
    blunc := '';
  end
  // non-empty input
  else begin
    Result := StrToRealBothNN(tsval1,tsval2,num_blum,val2);
    if (not Result) then Exit;
    bluminosity := tsval1;
    blunc := tsval2;
  end;
  // done
  Result := True;
end;
//---------------------------
function StarExtraData.SetAge(const median,uncertainty:string):Boolean;
var val1,val2:Real;
begin
  Result := False;
  if not StrToRealBothNN(median,uncertainty,val1,val2) then Exit;
  if val1 >15 then Exit;
  age := median;
  agerange := uncertainty;
  num_age := val1;
  Result := True;
end;
//---------------------------
function StarExtraData.SetDiameter(const median,uncertainty:string):Boolean;
var val2:Real;
begin
  Result := False;
  if not StrToRealBothNN(median,uncertainty,num_dia,val2) then Exit;
  dia := median;
  diarange := uncertainty;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// test function
function StarExtraData.HasNoData():Boolean;
begin
  Result := False;
  if (num_age <> 0) or (num_mass <> 0) then Exit;
  if (num_blum <> 0) or (num_dia <> 0) then Exit;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(*  mass,munc:string;        bluminosity,blunc:string;        age,agerange:string;
    dia,diarange:string;     metal,metunc:string;               *)
// text io
function StarExtraData.ToOutputString:string;
begin
  Result := mass + ';' + munc + ';'+ bluminosity + ';' + blunc + ';';
  Result += age + ';' + agerange +';' + dia + ';' + diarange;
end;

function StarExtraData.FromInputString(const instring:string):Boolean;
var splitInput:TStringList;
    sc:Integer;
begin
  Result := False;
  splitInput := SplitWithDelim(instring,';',8);
  if splitinput = nil then Exit;
  mass := splitInput[0];          munc := splitInput[1];
  bluminosity := splitInput[2];   blunc := splitInput[3];
  age := splitInput[4];           agerange := splitInput[5];
  dia := splitInput[6];           diarange := splitInput[7];
  (* Length for diameter after decimal should be 3 digits, this is a change,
  so to make the save files compatible, I have to zero pad if there are too few
  digits. *)
  sc := Length(dia) - RPos('.',dia);
  if sc = 2 then dia += '0';
  sc := Length(diarange) - RPos('.',diarange);
  if sc = 2 then diarange += '0';
  // 2 extra fields are for metallicity, ignore
  // metal := splitInput[8];         metunc := splitInput[9];
  splitInput.Free;
  // now that loading is done, we convert numbers for quick access
  if (not StrToRealBoth(mass,age,num_mass,num_age)) then Exit;
  if (not StrToReal(dia,num_dia)) then Exit;
  if bluminosity <> '' then begin
     if (not StrToReal(bluminosity,num_blum)) then Exit;
  end else num_blum := 0;
  // done
  Result := True;
end;

//==========================================================================
end.

