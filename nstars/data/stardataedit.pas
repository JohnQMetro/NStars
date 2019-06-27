unit stardataedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, MaskEdit,
  StrUtils, Dialogs,
  NewStar, star_names, newlocation, unitdata, df_strings, stardata;

type

CSD_FrameType = (CSD_STAR, CSD_BD, CSD_WD);

  { TCoreStarDataFrame }

  TCoreStarDataFrame = class(TFrame)
    WDArityLabel: TLabel;
    ArityPicker: TComboBox;
    WDArityPicker: TComboBox;
    BayerBox: TMaskEdit;
    BDMassLabel: TLabel;
    BDMassMEdit: TMaskEdit;
    BdmePM: TLabel;
    BDMuncEdit: TMaskEdit;
    CBPulsating: TCheckBox;
    AtmosPicker: TComboBox;
    CompLabel: TLabel;
    BLumLabel: TLabel;
    BrownDwarfPanel: TPanel;
    LbAtmos: TLabel;
    WDVisMagEdit: TMaskEdit;
    WDVMagLabel: TLabel;
    WhiteDwarfPanel: TPanel;
    StarPanel: TPanel;
    TEffLabel: TLabel;
    SecMagEdit: TMaskEdit;
    MassEstLabel: TLabel;
    RadiusLabel: TLabel;
    NotesLabel: TLabel;
    StarNoteEdit: TMemo;
    ComponentPicker: TComboBox;
    LuminosityDisplay: TLabel;
    VariableTypeCB: TComboBox;
    VarTypeLabel: TLabel;
    VisualMagEdit: TMaskEdit;
    VMagLabel: TLabel;
    VStarDesigEdit: TLabeledEdit;
    SpectralClassEdit: TLabeledEdit;
    TypeLabel: TLabel;

    procedure ArityPickerChange(Sender: TObject);
    procedure AtmosPickerChange(Sender: TObject);
    procedure AtmosPickerExit(Sender: TObject);
    function GetParallax:Real;
    procedure LoadEstimates;
    procedure SecMagEditExit(Sender: TObject);
    procedure SpectralClassEditExit(Sender: TObject);
    procedure StarNoteEditExit(Sender: TObject);
    procedure VisualMagEditExit(Sender: TObject);
    procedure VisualMagEditKeyPress(Sender: TObject; var Key: char);
    procedure VStarDesigEditExit(Sender: TObject);
    procedure WDVisMagEditExit(Sender: TObject);
    procedure WDVisMagEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    edited_item:NewStarBase;
    bdwarf_item:BrownDwarfInfo;
    star_item:StarInfo;
    starsys:StarSystem;
    toplocation:Location;
    avgpllx:Real;
    sunMode:Boolean;
    panelMode:CSD_FrameType;
    // methods
    procedure ChangePanelType(newType:CSD_FrameType);
    procedure LabelShow(vis:Boolean);
    function LoadCommonData:Boolean;
    procedure SetSTypeLabel;
    function LoadStarData():Boolean;
    function LoadBrownDwarfData():Boolean;
    function LoadWhiteDwarfData():Boolean;
    function SaveCommonData:Boolean;
    function SaveNameStuff:Boolean;
    function SaveSecondaryMag(showmsg:Boolean):Boolean;
    function SaveStarData():Boolean;
    function SaveWhiteDwarfData():Boolean;
    function SaveBrownDwarfData():Boolean;
    procedure ClearCommonData;
    procedure ClearStarData;
    // procedure ChangeMode(tostar:Boolean);
    procedure DisableStarData(dodisable:Boolean);
    procedure MakeNil;
    procedure ArityTweak(ccount:Word);

  public
    { public declarations }
    constructor Create(AOwner: TComponent) ; override;
    procedure SetupComboBoxes;
    procedure SaveData;
    function ChangeStar(item:NewStarBase; parentsys:StarSystem; icount:Integer):Boolean;
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

procedure TCoreStarDataFrame.AtmosPickerChange(Sender: TObject);
var pllx:Real;
begin
  pllx := GetParallax;
  star_item.wda:= WDAtmosEnum(AtmosPicker.ItemIndex);
  star_item.InitializeEstimation(pllx,avgpllx);
  LoadEstimates;
end;

procedure TCoreStarDataFrame.AtmosPickerExit(Sender: TObject);
begin

end;

//------------------------------------
procedure TCoreStarDataFrame.LoadEstimates;
var lumstr:string;     radstr:string;
    massest:string;    blumest:string;
    teffused:string;
begin
  // visual magnitude
  if (star_item.estimator_i <> nil) then begin
    // luminosity
     lumstr := star_item.estimator_i.LuminosityString;
     lumstr := 'V Luminosity: ' + lumstr;
     LuminosityDisplay.Caption := lumstr;
     // radius estimate
     radstr := star_item.estimator_i.RadiusEstimateString;
     radstr := 'Radius Est. : ' + radstr;
     RadiusLabel.Caption := radstr;
     //mass estimate
     massest := star_item.estimator_i.MassEstimateString;
     massest := 'Mass Est. : ' + massest;
     MassEstLabel.Caption := massest;
     // bolometric luminosity
     blumest := star_item.estimator_i.BoloLuminosityEstString;
     blumest := 'Bolo.Luminosity Est: ' + blumest;
     BLumLabel.Caption := blumest;
     // TEff
     teffused := star_item.estimator_i.TEffUsed;
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
  if (star_item.estimator_i = nil) then star_item.InitializeEstimation(pllx,avgpllx)
  else star_item.NonSpectraChange(pllx,avgpllx);
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

procedure TCoreStarDataFrame.WDVisMagEditExit(Sender: TObject);
var string1:string;     pllx:Real;
begin
  // visual magnitude
  string1 := Trim(WDVisMagEdit.Text);
  if (string1 <> '+99.999') then begin
    star_item.SetVisualMagnitudeStr(string1);
  end;
  WDVisMagEdit.Modified := False;
  pllx := GetParallax;
  if (star_item.estimator_i = nil) then star_item.InitializeEstimation(pllx,avgpllx)
  else star_item.NonSpectraChange(pllx,avgpllx);
  LoadEstimates;
end;

procedure TCoreStarDataFrame.WDVisMagEditKeyPress(Sender: TObject; var Key: char );
var pressdex:Integer;
begin
  pressdex := WDVisMagEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TCoreStarDataFrame.ChangePanelType(newType:CSD_FrameType);
begin
  if newType = panelMode then Exit;
  // change to star
  if newType = CSD_STAR then begin
      BrownDwarfPanel.Visible := False;
      WhiteDwarfPanel.Visible := False;
      StarPanel.Visible := True;
      SecMagEdit.Visible := True;
      BayerBox.Visible := True;
      LabelShow(True);
  end
  // change to brown dwarf
  else if newType = CSD_BD then begin
      BrownDwarfPanel.Visible := True;
      WhiteDwarfPanel.Visible := False;
      StarPanel.Visible := False;
      SecMagEdit.Visible := False;
      BayerBox.Visible := False;
      LabelShow(False);
  end
  // change to white dwarf
  else begin
      BrownDwarfPanel.Visible := False;
      WhiteDwarfPanel.Visible := True;
      StarPanel.Visible := False;
      SecMagEdit.Visible := True;
      BayerBox.Visible := False;
      LabelShow(True);
  end;
  panelMode := newType;
end;
//------------------------------
procedure TCoreStarDataFrame.LabelShow(vis:Boolean);
begin
  LuminosityDisplay.Visible := vis;
  RadiusLabel.Visible := vis;
  MassEstLabel.Visible := vis;
  BLumLabel.Visible := vis;
  TEffLabel.Visible := vis;
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
function TCoreStarDataFrame.LoadStarData():Boolean;
var sname:StarNames;
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
  lumstr := star_item.SecondaryMagnitudeStr;
  // arity
  ArityPicker.ItemIndex := Ord(star_item.Arity);
  SecMagEdit.Enabled := ArityPicker.ItemIndex >= Ord(SPECTROCOPIC_BINARY);
  // variable type
  VariableTypeCB.ItemIndex := Ord(star_item.VariableType);
  // done
  Result := True;
end;
//-------------------------------
function TCoreStarDataFrame.LoadBrownDwarfData():Boolean;
begin
  Result := False;
  if bdwarf_item = nil then Exit;
  BDMassMEdit.Text := bdwarf_item.MedianMass;
  BDMuncEdit.Text := bdwarf_item.MassUncertainty;
  Result := True;
end;
//--------------------------------
function TCoreStarDataFrame.LoadWhiteDwarfData():Boolean;
var sname:StarNames;
    lumstr:string;
begin
  Result := False;
  if star_item = nil then Exit;
  // name related stuff
  if star_item.HasNames then begin
    sname := star_item.GetNames;
    VStarDesigEdit.Text:= sname.var_designation;
  end
  else VStarDesigEdit.Text := '';
  // visual magnitude
  if star_item.ValidVisualMagnitude then begin
     lumstr := star_item.VisualMagnitudeString;
     WDVisMagEdit.Text := lumstr;
  end
  else WDVisMagEdit.Text := '+99.999';
  LoadEstimates;
  // atmosphere type
  AtmosPicker.ItemIndex := Ord(star_item.wda);
  // arity
  if star_item.Arity = SINGLE then WDArityPicker.ItemIndex := 0
  else if star_item.Arity = POSSIBLY_DOUBLE then WDArityPicker.ItemIndex := 1
  else WDArityPicker.ItemIndex := 2;

  // secondary magnitude
  if WDArityPicker.ItemIndex = 2 then begin
     SecMagEdit.Enabled := True;
     SecMagEdit.Text := star_item.SecondaryMagnitudeStr;
  end else begin
     SecMagEdit.Text := '99.9';
     SecMagEdit.Enabled := False;
  end;

  // variable status
  CBPulsating.Checked := (star_item.VariableType <> NOT_VARIABLE);
  // done
  Result := True;
end;

//-------------------------------
procedure TCoreStarDataFrame.SaveSpectralClass;
var qtype:CSD_FrameType;
begin
  if edited_item <> nil then begin
    edited_item.SpectralClass:= SpectralClassEdit.Text;
    SpectralClassEdit.Modified := False;
    SetSTypeLabel;
    // possible switch type....
    if star_item <> nil then begin
        if StrStartswAny(star_item.SpectralClass,['D','?D','??D']) then qtype := CSD_WD
        else qtype := CSD_STAR;
        if (qtype <> panelMode) then begin
           ChangePanelType(qtype);
           ReloadData();
        end;
        star_item.InitializeEstimation(GetParallax,avgpllx);
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
var sname:StarNames;
    bdes:LongInt;
    vdesig,rawbay:string;
begin
  Result := False;
  if star_item = nil then Exit;
  // gathering the name data
  vdesig := Trim(VStarDesigEdit.Text);
  if panelMode = CSD_WD then rawbay := ''
  else rawbay := Trim(BayerBox.Text);
  if (Length(vdesig)<>0) or (Length(rawbay)<>0) then begin
    sname := star_item.MakeOrGetNames;
    sname.var_designation := vdesig;
    if not TryStrToInt(rawbay,bdes) then sname.bayer_sup := 0
    else sname.bayer_sup := bdes;
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
  if ArityPicker.ItemIndex < Ord(SPECTROCOPIC_BINARY) then Exit;
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
function TCoreStarDataFrame.SaveStarData():Boolean;
var string1:string;
    pllx:Real;
    sok:Boolean;
    vtpick:VariableTypeEnum;
begin
  Result := False;
  if star_item = nil then Exit;
  if not SaveNameStuff then Exit;
  // visual magnitude
  string1 := Trim(VisualMagEdit.Text);
  if (string1 <> '+99.999') then begin
    sok := star_item.SetVisualMagnitudeStr(string1);
    pllx := GetParallax;
    star_item.NonSpectraChange(pllx,avgpllx);
  end;
  // arity
  star_item.Arity := ArityType(ArityPicker.ItemIndex);
  // variable type
  sok := IndexToVarT(VariableTypeCB.ItemIndex,vtpick);
  if sok then star_item.VariableType := vtpick;
  // secondary magnitude
  SaveSecondaryMag(True);
  // done
  Result := True;
end;
//------------------------------
function TCoreStarDataFrame.SaveWhiteDwarfData():Boolean;
var string1:string;
    pllx:Real;
    sok:Boolean;
    adex:Integer;
begin
  Result := False;
  if star_item = nil then Exit;
  if not SaveNameStuff then Exit;
  // visual magnitude
  string1 := Trim(WDVisMagEdit.Text);
  if (string1 <> '+99.999') then begin
    sok := star_item.SetVisualMagnitudeStr(string1);
    pllx := GetParallax;
    star_item.NonSpectraChange(pllx,avgpllx);
  end;
  // atmosphere
  star_item.wda := WDAtmosEnum(AtmosPicker.ItemIndex);
  // arity
  adex := WDArityPicker.ItemIndex;
  if adex = 1 then star_item.Arity := POSSIBLY_DOUBLE
  else if adex = 2 then star_item.Arity := WHITE_DWARF_BINARY
  else star_item.Arity := SINGLE;
  // variable type
  if CBPulsating.Checked then star_item.VariableType := ZZ_CETI
  else star_item.VariableType := NOT_VARIABLE;
  // secondary magnitude
  SaveSecondaryMag(True);
  // done
  Result := True;
end;
//-------------------------------
function TCoreStarDataFrame.SaveBrownDwarfData():Boolean;
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
// For nil or the Sun, both of which use CSD_STAR panel type
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
  ChangePanelType(CSD_STAR);
  // ChangeMode(True);
  star_item := nil;
  bdwarf_item := nil;
  toplocation := nil;
  ClearStarData;
  TypeLabel.Caption := 'Nothing';
end;
//-------------------------------------------------------
// enables/disables some tems based on number of components (arity)
procedure TCoreStarDataFrame.ArityTweak(ccount:Word);
var sing:Boolean;
begin
    sing := (ccount = 1);
    if sing then ComponentPicker.Enabled := False;
    if sing and (panelMode <> CSD_BD) then begin
        // if the system is single, use the system name panel instead
        VStarDesigEdit.Enabled := False;
        BayerBox.Enabled := False;
    end else if (panelMode <> CSD_BD) then begin
        // if the system is multiple, components can differ in these
        VStarDesigEdit.Enabled := True;
        BayerBox.Enabled := True;
    end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor TCoreStarDataFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  panelMode := CSD_STAR;
  StarPanel.Color := Self.Color;
  BrownDwarfPanel.Color := Self.Color;
  WhiteDwarfPanel.Color := Self.Color;
  BrownDwarfPanel.Visible := False;
  WhiteDwarfPanel.Visible := False;
  StarPanel.Visible := True;
  avgpllx := 0;
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
    VariableTypeCB.Items.AddStrings(VStarType2);
  end;
end;
//------------------------------------------------------
procedure TCoreStarDataFrame.SaveData;
begin
  if edited_item<>nil then begin
      SaveCommonData;
      case panelMode of
          CSD_STAR : SaveStarData();
          CSD_BD   : SaveBrownDwarfData();
          CSD_WD   : SaveWhiteDwarfData();
      end;
  end;
end;
//------------------------------------------------------
function TCoreStarDataFrame.ChangeStar(item:NewStarBase; parentsys:StarSystem; icount:Integer):Boolean;
var bd2:Boolean;
    ntype:CSD_FrameType;
begin
  Result := False;
  // SaveData;
  sunMode := False;
  // clearing to no data at all
  if item = nil then begin
     MakeNil;
     DisableStarData(True);
  end
  // loading a star/white dwarf/brown dwarf
  else begin
    // common loads
    if parentsys = nil then Exit;
    avgpllx := parentsys.GetAvgPllx();
    starsys := parentsys;
    toplocation := parentsys.GetLocation;
    edited_item := item;
    DisableStarData(False);
    ComponentPicker.Enabled := True;
    LoadCommonData;
    bdwarf_item := nil;
    star_item := nil;
    // brown dwarf or not?
    bd2 := edited_item.isBrownDwarf;
    if bd2 then bdwarf_item := BrownDwarfInfo(item)
    else star_item := StarInfo(item);
    // loading based on type
    if bd2 then ntype := CSD_BD
    else if StrStartswAny(star_item.SpectralClass,['D','?D','??D']) then ntype := CSD_WD
    else ntype := CSD_STAR;
    ChangePanelType(ntype);
    case ntype of
        CSD_STAR : LoadStarData();
        CSD_BD   : LoadBrownDwarfData();
        CSD_WD   : LoadWhiteDwarfData();
    end;
    // finishing
    ArityTweak(icount);
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
begin
  Result := False;
  if (edited_item = nil) then Exit;
  LoadCommonData;
  case panelMode of
      CSD_STAR : LoadStarData();
      CSD_BD   : LoadBrownDwarfData();
      CSD_WD   : LoadWhiteDwarfData();
  end;
  ArityTweak(edited_item.MinPartCount);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// externally called handlers
//---------------------------------------
procedure TCoreStarDataFrame.ExternalChange;
var pllx:Real;
begin
  if star_item<>nil then begin
    pllx := GetParallax;
    avgpllx := starsys.GetAvgPllx();
    star_item.NonSpectraChange(pllx,avgpllx);
    LoadEstimates;
  end;
end;
//==============================================================

end.

