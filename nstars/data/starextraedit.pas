unit starextraedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, Dialogs,
  StarDataBase,NewStar;

type

  { TStarExtraDataFrame }

  TStarExtraDataFrame = class(TFrame)
    BolLumEdit: TEdit;
    BLUncEdit: TEdit;
    MassLabel: TLabel;
    MassPM: TLabel;
    BoloLumLabel: TLabel;
    BoloLumPM: TLabel;
    AgeLabel: TLabel;
    AgePM: TLabel;
    DiameterLabel: TLabel;
    DiamPM: TLabel;
    MassEdit: TMaskEdit;
    MUncEdit: TMaskEdit;
    AgeEdit: TMaskEdit;
    AgeUncEdit: TMaskEdit;
    DiameterEdit: TMaskEdit;
    DiamUncEdit: TMaskEdit;
    procedure AgeEditExit(Sender: TObject);
    procedure AgeUncEditExit(Sender: TObject);
    procedure BLUncEditKeyPress(Sender: TObject; var Key: char);
    procedure BolLumEditExit(Sender: TObject);
    procedure BolLumEditKeyPress(Sender: TObject; var Key: char);
    procedure DiameterEditExit(Sender: TObject);
    procedure DiamUncEditExit(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure MassEditExit(Sender: TObject);
  private
    { private declarations }
    ExistingData:Boolean;
    Star_ptr:StarInfo;
    ExtraData:StarExtraData;
    // private methods
    function SaveMass(showerr:Boolean):Boolean;
    function SaveBolometricLuminosity(showerr:Boolean):Boolean;
    function SaveAge(showerr:Boolean):Boolean;
    function SaveDiameter(showerr:Boolean):Boolean;
    procedure ClearBoxes;
    procedure EnableEditors(do_enable:Boolean);
    procedure FinalSave();
  public
    { public declarations }
    function SaveValues(showerr:Boolean):Boolean;
    function LoadValues():Boolean;
    procedure ChangeStarData(newstar:StarInfo; saveold:Boolean);
    procedure ChangeToSun(saveold:Boolean);
    procedure ChangeToBrownDwarf(saveold:Boolean);
    procedure Setup();

  end;

const
      tvw1 = 'The values for ';
      tvw2 = ' are incorrect!';

implementation

{$R *.lfm}

procedure TStarExtraDataFrame.BLUncEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [#8, '0'..'9', DefaultFormatSettings.DecimalSeparator]) then Key := #0
  else if (Key = DefaultFormatSettings.DecimalSeparator) and (Pos(Key, BLUncEdit.Text) > 0) then begin
    Key := #0;
  end;
end;

procedure TStarExtraDataFrame.BolLumEditExit(Sender: TObject);
var xres:Boolean;
begin
  Assert(ExtraData<>nil);
  xres := SaveBolometricLuminosity(True);
  if not xres then begin
    BolLumEdit.Text := ExtraData.BolometricLumMedian;
    BLUncEdit.Text := ExtraData.BolometricLumUncertainty;
  end

end;

procedure TStarExtraDataFrame.AgeEditExit(Sender: TObject);
var xres:Boolean;
begin
  Assert(ExtraData<>nil);
  xres := SaveAge(True);
  if not xres then begin
    AgeEdit.Text := ExtraData.AgeMedian;
    AgeUncEdit.Text := ExtraData.AgeUncertainty;
  end

end;

procedure TStarExtraDataFrame.AgeUncEditExit(Sender: TObject);
var xres:Boolean;
begin
  Assert(ExtraData<>nil);
  xres := SaveAge(True);
  if not xres then begin
    AgeEdit.Text := ExtraData.AgeMedian;
    AgeUncEdit.Text := ExtraData.AgeUncertainty;
  end

end;

procedure TStarExtraDataFrame.BolLumEditKeyPress(Sender: TObject; var Key: char );
begin
  if not (Key in [#8, '0'..'9', DefaultFormatSettings.DecimalSeparator]) then Key := #0
  else if (Key = DefaultFormatSettings.DecimalSeparator) and (Pos(Key, BolLumEdit.Text) > 0) then begin
    Key := #0;
  end;
end;

procedure TStarExtraDataFrame.DiameterEditExit(Sender: TObject);
var xres:Boolean;
begin
  Assert(ExtraData<>nil);
  xres := SaveDiameter(True);
  if not xres then begin
    DiameterEdit.Text := ExtraData.DiameterMedian;
    DiamUncEdit.Text := ExtraData.DiameterUncertainty;
  end

end;

procedure TStarExtraDataFrame.DiamUncEditExit(Sender: TObject);
var xres:Boolean;
begin
  Assert(ExtraData<>nil);
  xres := SaveDiameter(True);
  if not xres then begin
    DiameterEdit.Text := ExtraData.DiameterMedian;
    DiamUncEdit.Text := ExtraData.DiameterUncertainty;
  end

end;

procedure TStarExtraDataFrame.FrameClick(Sender: TObject);
begin
  if ExtraData <> nil then SaveValues(True);
end;

procedure TStarExtraDataFrame.MassEditExit(Sender: TObject);
var xres:Boolean;
begin
  Assert(ExtraData<>nil);
  xres := SaveMass(True);
  if not xres then begin
    MassEdit.Text := ExtraData.MassMedian;
    MUncEdit.Text := ExtraData.MassUncertainty;
  end

end;

{ TStarExtraDataFrame }

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TStarExtraDataFrame.SaveMass(showerr:Boolean):Boolean;
var temp1,temp2:string;
begin
  Assert(ExtraData<>nil);
  temp1 := MassEdit.Text;
  temp2 := MUncEdit.Text;
  Result := ExtraData.SetMass(temp1,temp2);
  if (not Result) and showerr then ShowMessage(tvw1 + 'Mass' + tvw2);
  MassEdit.Modified := False;
  MUncEdit.Modified := False;
end;
//---------------------------------
function TStarExtraDataFrame.SaveBolometricLuminosity(showerr:Boolean):Boolean;
var temp1,temp2:string;
begin
  Result := False;
  Assert(ExtraData<>nil);
  temp1 := Trim(BolLumEdit.Text);
  temp2 := Trim(BLUncEdit.Text);
  if temp1 = '' then Exit;
  if temp2 = '' then temp2 := '0';
  Result := ExtraData.SetBolometric(temp1,temp2);
  if (not Result) and showerr then ShowMessage(tvw1 + 'Bolometric Luminosity' + tvw2);
  BolLumEdit.Modified := False;
  BLUncEdit.Modified := False;
end;
//---------------------------------
function TStarExtraDataFrame.SaveAge(showerr:Boolean):Boolean;
var temp1,temp2:string;
begin
  Assert(ExtraData<>nil);
  temp1 := AgeEdit.Text;
  temp2 := AgeUncEdit.Text;
  Result := ExtraData.SetAge(temp1,temp2);
  if (not Result) and showerr then ShowMessage(tvw1 + 'Age' + tvw2);
  AgeEdit.Modified := False;
  AgeUncEdit.Modified := False;
end;
//---------------------------------
function TStarExtraDataFrame.SaveDiameter(showerr:Boolean):Boolean;
var temp1,temp2:string;
begin
  Assert(ExtraData<>nil);
  temp1 := DiameterEdit.Text;
  temp2 := DiamUncEdit.Text;
  Result := ExtraData.SetDiameter(temp1,temp2);
  if (not Result) and showerr then ShowMessage(tvw1 + 'Diameter' + tvw2);
  DiameterEdit.Modified := False;
  DiamUncEdit.Modified := False;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TStarExtraDataFrame.ClearBoxes;
begin
  MassEdit.Text := ' 0.000';
  MUncEdit.Text := ' 0.000';
  BolLumEdit.Text := '';
  BLUncEdit.Text := '';
  AgeEdit.Text := ' 0.00';
  AgeUncEdit.Text := ' 0.00';
  DiameterEdit.Text := ' 00.000';
  DiamUncEdit.Text := ' 00.000';
end;
//------------------------------------
procedure TStarExtraDataFrame.EnableEditors(do_enable:Boolean);
begin
  MassEdit.Enabled := do_enable;
  MUncEdit.Enabled := do_enable;
  BolLumEdit.Enabled := do_enable;
  BLUncEdit.Enabled := do_enable;
  AgeEdit.Enabled := do_enable;
  AgeUncEdit.Enabled := do_enable;
  DiameterEdit.Enabled := do_enable;
  DiamUncEdit.Enabled := do_enable;
end;
//--------------------------------------------
// ExtraData might or might not point to the existing data, and might
// consist entirely of zero values, discard...
procedure TStarExtraDataFrame.FinalSave();
begin
  Assert(ExtraData<>nil);
  // the 'saved' data is all zeros or empty
  if ExtraData.HasNoData() then begin
    if ExistingData then begin
      FreeAndNil(Star_ptr.ExtraInfo);
      ExistingData := False;
      ExtraData := nil; // new
    end;
  end
  // saved data is usable, but Extra Data is not attached to the star...
  else if (not ExistingData) then begin
    Star_ptr.ExtraInfo := ExtraData;
    ExistingData := True;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TStarExtraDataFrame.SaveValues(showerr:Boolean):Boolean;
begin
  Result := False;
  if Star_ptr = nil then Exit;
  if ExtraData = nil then Exit;
  // saving
  if not SaveMass(showerr) then Exit;
  if not SaveBolometricLuminosity(showerr) then Exit;
  if not SaveAge(showerr) then Exit;
  if not SaveDiameter(showerr) then Exit;
  // checking to see if we have anything...
  FinalSave();
  // done
  Result := True;
end;
//------------------------------------
function TStarExtraDataFrame.LoadValues():Boolean;
begin
  Result := False;
  if Star_ptr.ExtraInfo = nil then begin
    ExistingData := False;
    ClearBoxes;
    ExtraData := StarExtraData.Create;
  end else begin
    ExistingData := True;
    ExtraData := Star_ptr.ExtraInfo;
    MassEdit.Text := ExtraData.MassMedian;
    MUncEdit.Text := ExtraData.MassUncertainty;
    BolLumEdit.Text := ExtraData.BolometricLumMedian;
    BLUncEdit.Text := ExtraData.BolometricLumUncertainty;
    AgeEdit.Text := ExtraData.AgeMedian;
    AgeUncEdit.Text := ExtraData.AgeUncertainty;
    DiameterEdit.Text := ExtraData.DiameterMedian;
    DiamUncEdit.Text := ExtraData.DiameterUncertainty;
  end;
  Result := True;
end;
//--------------------------------
procedure TStarExtraDataFrame.ChangeStarData(newstar:StarInfo; saveold:Boolean);
begin
  Assert(newstar<>nil);
  if saveold then SaveValues(False);
  if (not ExistingData) and (ExtraData <> nil) then FreeAndNil(ExtraData);

  Star_ptr := newstar;
  if Star_ptr.ExtraInfo = nil then begin
    ExistingData := False;
    ClearBoxes;
    ExtraData := StarExtraData.Create;
    EnableEditors(True);
  end
  else begin
    ExistingData := True;
    ExtraData := Star_ptr.ExtraInfo;
    EnableEditors(True);
    LoadValues;
  end;
end;
//---------------------------
procedure TStarExtraDataFrame.ChangeToSun(saveold:Boolean);
begin
  if saveold then SaveValues(False);
  if (not ExistingData) and (ExtraData <> nil) then ExtraData.Free;
  Star_ptr := nil;
  ExtraData := nil;
  ExistingData := False;
  // entering sun values
  MassEdit.Text := ' 1.000';
  MUncEdit.Text := ' 0.000';
  BolLumEdit.Text := '1.0';
  BLUncEdit.Text := '0.0';
  AgeEdit.Text := ' 4.57';
  AgeUncEdit.Text := ' 0.10';
  DiameterEdit.Text := ' 01.00';
  DiamUncEdit.Text := ' 00.00';
  // finishing...
  EnableEditors(False);
end;
//-----------------------------
procedure TStarExtraDataFrame.ChangeToBrownDwarf(saveold:Boolean);
begin
  if saveold then SaveValues(False);
  // clearing the old data
  if (not ExistingData) and (ExtraData <> nil) then ExtraData.Free;
  Star_ptr := nil;
  ExtraData := nil;
  ExistingData := False;
  // loading nothing and disabling
  ClearBoxes;
  EnableEditors(False);
end;
//-----------------------------
procedure TStarExtraDataFrame.Setup();
begin
  ClearBoxes;
  ExistingData := False;
  EnableEditors(False);
end;

end.

