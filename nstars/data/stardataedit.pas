unit stardataedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, MaskEdit,
  StrUtils, Dialogs,
  NewStar, namedata, newlocation, unitdata;

type

  { TCoreStarDataFrame }

  TCoreStarDataFrame = class(TFrame)
    ArityPicker: TComboBox;
    ArityLabel: TLabel;
    BayerBox: TMaskEdit;
    CompLabel: TLabel;
    BDMassLabel: TLabel;
    BDMassMEdit: TMaskEdit;
    BdmePM: TLabel;
    BDMuncEdit: TMaskEdit;
    BLumLabel: TLabel;
    TEffLabel: TLabel;
    SecMagEdit: TMaskEdit;
    MassEstLabel: TLabel;
    RadiusLabel: TLabel;
    NotesLabel: TLabel;
    VarTypeLabel: TLabel;
    StarNoteEdit: TMemo;
    VariableTypeCB: TComboBox;
    ComponentPicker: TComboBox;
    LuminosityDisplay: TLabel;
    VMagLabel: TLabel;
    VisualMagEdit: TMaskEdit;
    VStarDesigEdit: TLabeledEdit;
    SpectralClassEdit: TLabeledEdit;
    TypeLabel: TLabel;
    procedure ArityPickerChange(Sender: TObject);
    function GetParallax:Real;
    procedure LoadEstimates;
    procedure SecMagEditExit(Sender: TObject);
    procedure SpectralClassEditExit(Sender: TObject);
    procedure StarNoteEditExit(Sender: TObject);
    procedure VisualMagEditExit(Sender: TObject);
    procedure VisualMagEditKeyPress(Sender: TObject; var Key: char);
    procedure VStarDesigEditExit(Sender: TObject);
  private
    { private declarations }
    edited_item:NewStarBase;
    bdwarf_item:BrownDwarfInfo;
    star_item:StarInfo;
    toplocation:Location;
    sunMode:Boolean;
    // methods
    procedure ShowOrHideStarWidgets(showw:Boolean);
    procedure ShowOrHideBDwarfWidgets(showw:Boolean);
    function LoadCommonData:Boolean;
    procedure SetSTypeLabel;
    function LoadStarData:Boolean;
    function LoadBrownDwarfData:Boolean;
    function SaveCommonData:Boolean;
    function SaveNameStuff:Boolean;
    function SaveSecondaryMag(showmsg:Boolean):Boolean;
    function SaveStarData:Boolean;
    function SaveBrownDwarfData:Boolean;
    procedure ClearCommonData;
    procedure ClearStarData;
    procedure ChangeMode(tostar:Boolean);
    procedure DisableStarData(dodisable:Boolean);
    procedure MakeNil;

  public
    { public declarations }
    procedure SetupComboBoxes;
    procedure SaveData;
    function ChangeStar(item:NewStarBase; parentloc:Location; icount:Integer):Boolean;
    procedure ChangeToSun;
    function ReloadData:Boolean;
    procedure SaveSpectralClass;
    // externally called handlers
    procedure ExternalChange;

  end;

implementation

{$R *.lfm}
//------------------------------------
function TCoreStarDataFrame.GetParallax:Real;
begin
  if edited_item = nil then Result := -1
  else if edited_item.HasLocation then Result := edited_item.GetLocation.ParallaxMAS
  else Result := toplocation.ParallaxMAS;
end;

procedure TCoreStarDataFrame.ArityPickerChange(Sender: TObject);
begin
  SecMagEdit.Enabled := ArityPicker.ItemIndex >= Ord(SPECTROCOPIC_BINARY);
end;
//------------------------------------
procedure TCoreStarDataFrame.LoadEstimates;
var lumstr:string;     radstr:string;
    massest:string;    blumest:string;
    teffused:string;
begin
  // visual magnitude
  if (star_item.estimator <> nil) then begin
    // luminosity
     lumstr := star_item.estimator.LuminosityString;
     lumstr := 'V Luminosity: ' + lumstr;
     LuminosityDisplay.Caption := lumstr;
     // radius estimate
     radstr := star_item.estimator.RadiusEstimateString;
     radstr := 'Radius Est. : ' + radstr;
     RadiusLabel.Caption := radstr;
     //mass estimate
     massest := star_item.estimator.MassEstimateString;
     massest := 'Mass Est. : ' + massest;
     MassEstLabel.Caption := massest;
     // bolometric luminosity
     blumest := star_item.estimator.BoloLuminosityEstString;
     blumest := 'Bolo.Luminosity Est: ' + blumest;
     BLumLabel.Caption := blumest;
     // TEff
     teffused := star_item.estimator.TEffUsed;
     teffused := 'TEff Est: ' + teffused;
     TEffLabel.Caption := teffused;
  end
  else begin
    LuminosityDisplay.Caption := 'Unknown V Luminosity';
    RadiusLabel.Caption := 'Cannot guess radius';
    MassEstLabel.Caption := 'Cannot guess mass';
    BLumLabel.Caption := 'Cannot gues Bolo. Luminosity';
    TEffLabel.Caption := 'TEff unknown';
  end;
end;

procedure TCoreStarDataFrame.SecMagEditExit(Sender: TObject);
begin
  SaveSecondaryMag(True);
end;

//-------------------------------------------------
procedure TCoreStarDataFrame.SpectralClassEditExit(Sender: TObject);
begin
  SaveSpectralClass;
end;

procedure TCoreStarDataFrame.StarNoteEditExit(Sender: TObject);
begin
  if edited_item <> nil then begin
    edited_item.notes := Trim(StarNoteEdit.Text);
    StarNoteEdit.Modified := False;
  end;
end;

procedure TCoreStarDataFrame.VisualMagEditExit(Sender: TObject);
var string1:string;     pllx:Real;
begin
  // visual magnitude
  string1 := Trim(VisualMagEdit.Text);
  if (string1 <> '+99.999') then begin
    star_item.SetVisualMagnitudeStr(string1);
  end;
  VisualMagEdit.Modified := False;
  pllx := GetParallax;
  if (star_item.estimator = nil) then star_item.InitializeEstimation(pllx)
  else star_item.NonSpectraChange(pllx);
  LoadEstimates;
end;

procedure TCoreStarDataFrame.VisualMagEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := VisualMagEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TCoreStarDataFrame.VStarDesigEditExit(Sender: TObject);
begin
  SaveNameStuff;
end;

procedure TCoreStarDataFrame.ShowOrHideStarWidgets(showw:Boolean);
begin
  VariableTypeCB.Visible := showw;
  ArityPicker.Visible := showw;
  VMagLabel.Visible := showw;
  VisualMagEdit.Visible := showw;
  VStarDesigEdit.Visible := showw;
  ArityLabel.Visible := showw;
  LuminosityDisplay.Visible := showw;
  BayerBox.Visible := showw;
  VarTypeLabel.Visible := showw;
  RadiusLabel.Visible := showw;
  MassEstLabel.Visible := showw;
  BLumLabel.Visible := showw;
  SecMagEdit.Visible := showw;
  TEffLabel.Visible := showw;
end;
//-------------------------------
procedure TCoreStarDataFrame.ShowOrHideBDwarfWidgets(showw:Boolean);
begin
  BDMassLabel.Visible := showw;
  BDMassMEdit.Visible := showw;
  BdmePM.Visible := showw;
  BDMuncEdit.Visible := showw;
end;

//-------------------------------
function TCoreStarDataFrame.LoadCommonData:Boolean;
begin
  Result := False;
  if edited_item = nil then Exit;
  ComponentPicker.ItemIndex := AnsiIndexStr(edited_item.Component,ComponentStrings);
  SpectralClassEdit.Text := edited_item.SpectralClass;
  SetSTypeLabel;
  StarNoteEdit.Text := edited_item.notes;
  Result := True;
end;
//------------------------------------------
procedure TCoreStarDataFrame.SetSTypeLabel;
var xst:StarClass;
begin
  if edited_item <> nil then begin
    xst := edited_item.ClassifySpectralType;
    TypeLabel.Caption := TypeLabels[Ord(xst)];
  end;
end;
//-------------------------------
function TCoreStarDataFrame.LoadStarData:Boolean;
var sname:StarName;
    lumstr:string;
begin
  Result := False;
  if star_item = nil then Exit;
  // name related stuff
  if star_item.HasNames then begin
    sname := star_item.GetNames;
    VStarDesigEdit.Text:= sname.var_designation;
    if sname.bayer_sup = 0 then BayerBox.Text := ''
    else BayerBox.Text:= IntToStr(sname.bayer_sup);
  end
  else begin
    VStarDesigEdit.Text := '';
    BayerBox.Text := '';
  end;
  // visual magnitude
  if star_item.ValidVisualMagnitude then begin
     lumstr := star_item.VisualMagnitudeString;
     VisualMagEdit.Text := lumstr;
  end
  else VisualMagEdit.Text := '+99.999';
  LoadEstimates;
  // secondary magnitude
  SecMagEdit.Text := star_item.SecondaryMagnitudeStr;
  // arity
  ArityPicker.ItemIndex := Ord(star_item.Arity);
  SecMagEdit.Enabled := ArityPicker.ItemIndex >= Ord(SPECTROCOPIC_BINARY);
  // variable type
  VariableTypeCB.ItemIndex := Ord(star_item.VariableType);
  // done
  Result := True;
end;
//-------------------------------
function TCoreStarDataFrame.LoadBrownDwarfData:Boolean;
begin
  Result := False;
  if bdwarf_item = nil then Exit;
  BDMassMEdit.Text := bdwarf_item.MedianMass;
  BDMuncEdit.Text := bdwarf_item.MassUncertainty;
  Result := True;
end;

//-------------------------------
procedure TCoreStarDataFrame.SaveSpectralClass;
begin
  if edited_item <> nil then begin
    edited_item.SpectralClass:= SpectralClassEdit.Text;
    SpectralClassEdit.Modified := False;
    SetSTypeLabel;
    if star_item <> nil then begin
      star_item.InitializeEstimation(GetParallax);
      LoadEstimates;
    end;
  end;
end;
//-------------------------------
function TCoreStarDataFrame.SaveCommonData:Boolean;
var compstr:string;
begin
  Result := False;
  if edited_item = nil then Exit;
  SaveSpectralClass;
  compstr := ComponentPicker.Text;
  edited_item.Component:= compstr;
  edited_item.notes := Trim(StarNoteEdit.Text);
  Result := True;
end;
//-------------------------------
function TCoreStarDataFrame.SaveNameStuff:Boolean;
var string1,string2:string;    sc,bint:Integer;
    sname:StarName;
begin
  Result := False;
  if star_item = nil then Exit;
  // name related stuff
  string1 := Trim(VStarDesigEdit.Text);
  string2 := Trim(BayerBox.Text);
  if (Length(string1)<>0) or (Length(string2)<>0) then begin
    sname := star_item.MakeOrGetNames;
    sname.var_designation := string1;
    if Length(string2)<>0 then begin
      Val(string2,bint,sc);
      if sc<>0 then Exit;
      if bint <> 0 then sname.bayer_sup := bint;
    end;
  end
  else if star_item.HasNames then begin
    sname := star_item.GetNames;
    sname.var_designation := '';
    sname.bayer_sup := 0;
  end;
  Result := True;
end;
//-------------------------------
function TCoreStarDataFrame.SaveSecondaryMag(showmsg:Boolean):Boolean;
var xdata:string;
    savok:Boolean;
    outmsg:string;
begin
  Result := False;
  if star_item = nil then Exit;
  xdata := SecMagEdit.Text;
  savok := star_item.SetSecondaryMagnitudeStr(xdata);
  if (not savok) and showmsg then begin
    outmsg := 'The secondary magnitude must be half or less the' + sLineBreak;
    outmsg += 'brightness of the overall (Visual) magnitude!';
    ShowMessage(outmsg);
  end;
  Result := savok;
end;
//-------------------------------
function TCoreStarDataFrame.SaveStarData:Boolean;
var string1:string;
    pllx:Real;
    sok:Boolean;
begin
  Result := False;
  if star_item = nil then Exit;
  if not SaveNameStuff then Exit;
  // visual magnitude
  string1 := Trim(VisualMagEdit.Text);
  if (string1 <> '+99.999') then begin
    sok := star_item.SetVisualMagnitudeStr(string1);
    pllx := GetParallax;
    star_item.NonSpectraChange(pllx);
  end;
  // arity
  star_item.Arity := ArityType(ArityPicker.ItemIndex);
  // variable type
  star_item.VariableType := VariableTypeEnum(VariableTypeCB.ItemIndex);
  // secondary magnitude
  SaveSecondaryMag(True);
  // done
  Result := True;
end;
//-------------------------------
function TCoreStarDataFrame.SaveBrownDwarfData:Boolean;
var str1,str2:string;
begin
  Result := False;
  if bdwarf_item = nil then Exit;
  str1 :=  BDMassMEdit.Text;
  str2 :=  BDMuncEdit.Text;
  Result := bdwarf_item.SetMassString(str1,str2);
end;
//-------------------------------
procedure TCoreStarDataFrame.ClearCommonData;
begin
  TypeLabel.Caption := '';
  SpectralClassEdit.Text := '';
  ComponentPicker.ItemIndex := 0;
end;
//-------------------------------
procedure TCoreStarDataFrame.ClearStarData;
begin
  VStarDesigEdit.Text := '';
  BayerBox.Text := '';
  VisualMagEdit.Text := '+99.999';
  ArityPicker.ItemIndex := 0;
  VariableTypeCB.ItemIndex := 0;
end;
//------------------------------------------------------
procedure TCoreStarDataFrame.ChangeMode(tostar:Boolean);
begin
  if tostar then begin
    ShowOrHideBDwarfWidgets(False);
    ShowOrHideStarWidgets(True);
  end
  else begin
    ShowOrHideStarWidgets(False);
    ShowOrHideBDwarfWidgets(True);
  end;
end;
//------------------------------------------------------
procedure TCoreStarDataFrame.DisableStarData(dodisable:Boolean);
begin
  VariableTypeCB.Enabled := not dodisable;
  ArityPicker.Enabled := not dodisable;
  VisualMagEdit.Enabled := not dodisable;
  VStarDesigEdit.Enabled := not dodisable;
  BayerBox.Enabled := not dodisable;
  SpectralClassEdit.Enabled := not dodisable;
  ComponentPicker.Enabled := not dodisable;
  StarNoteEdit.Enabled := not dodisable;
  SecMagEdit.Enabled := not dodisable;
end;

//------------------------------------------------------
procedure TCoreStarDataFrame.MakeNil;
begin
  edited_item := nil;
  ClearCommonData;
  ChangeMode(True);
  star_item := nil;
  bdwarf_item := nil;
  toplocation := nil;
  ClearStarData;
  TypeLabel.Caption := 'Nothing';
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TCoreStarDataFrame.SetupComboBoxes;
begin
  if ComponentPicker.Items.Count = 0 then begin
    ComponentPicker.Items.AddStrings(ComponentStrings);
  end;
  if ArityPicker.Items.Count = 0 then begin
    ArityPicker.Items.AddStrings(unitdata.ArityLabels);
  end;
  if VariableTypeCB.Items.Count = 0 then begin
    VariableTypeCB.Items.AddStrings(VStarType);
  end;
end;
//------------------------------------------------------
procedure TCoreStarDataFrame.SaveData;
begin
  if edited_item<>nil then begin
     SaveCommonData;
     if star_item <> nil then begin
        SaveStarData;
     end
     else SaveBrownDwarfData;
  end;
end;
//------------------------------------------------------
function TCoreStarDataFrame.ChangeStar(item:NewStarBase; parentloc:Location; icount:Integer):Boolean;
var bd2:Boolean;
begin
  Result := False;
  // SaveData;
  sunMode := False;
  // clearing to no data at all
  if item = nil then begin
     MakeNil;
     DisableStarData(True);
  end
  // loading a star/brown dwarf
  else begin
    // common to both stars and brown dwarves
    if parentloc = nil then Exit;
    toplocation := parentloc;
    edited_item := item;
    DisableStarData(False);
    ComponentPicker.Enabled := True;
    LoadCommonData;
    bdwarf_item := nil;
    star_item := nil;
    // brown dwarf or star?
    bd2 := edited_item.isBrownDwarf;
    ChangeMode(not bd2);
    if bd2 then bdwarf_item := BrownDwarfInfo(item)
    else star_item := StarInfo(item);
    if (not bd2) then LoadStarData
    else LoadBrownDwarfData;
    // finishing
    if icount = 1 then ComponentPicker.Enabled := False;
    if (icount = 1) and (not bd2) then begin
      VStarDesigEdit.Enabled := False;
      BayerBox.Enabled := False;
    end
    else if (not bd2) then begin
      VStarDesigEdit.Enabled := True;
      BayerBox.Enabled := True;
    end;
  end;
  Result := True;
end;
//------------------------------------------------------
procedure TCoreStarDataFrame.ChangeToSun;
begin
  sunMode := True;
  MakeNil;
  TypeLabel.Caption := 'The Sun';
  SpectralClassEdit.Text := 'G2V';
  LuminosityDisplay.Caption:= 'V Luminosity: By definition, 1';
  RadiusLabel.Caption := 'Radius: By definition, 1';
  MassEstLabel.Caption := 'Mass : By definition, 1';
  BLumLabel.Caption := 'Bolo.Lum: By definition, 1';
  TEffLabel.Caption := 'TEff is 5772K';
  VisualMagEdit.Text := '-26.74';
  StarNoteEdit.Text := '';
  DisableStarData(True);
end;

//-------------------------------------
function TCoreStarDataFrame.ReloadData:Boolean;
var starmode:Boolean;
begin
  Result := False;
  if (edited_item = nil) then Exit;
  LoadCommonData;
  starmode := BayerBox.Visible;
  if starmode then LoadStarData
  else LoadBrownDwarfData;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// externally called handlers
//---------------------------------------
procedure TCoreStarDataFrame.ExternalChange;
var pllx:Real;
begin
  if star_item<>nil then begin
    pllx := GetParallax;
    star_item.NonSpectraChange(pllx);
    LoadEstimates;
  end;
end;
//==============================================================

end.

