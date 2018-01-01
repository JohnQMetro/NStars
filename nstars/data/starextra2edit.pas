unit starextra2edit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, StrUtils, StarExt2, NewStar;

type

  StoreMagMethod = function(X:string):Boolean of object;

  { TStarExtras2Form }

  TStarExtras2Form = class(TForm)
    UseExtra2CB: TCheckBox;
    SimbadTempDisplay: TLabel;
    FluxInfoLabel: TLabel;
    TempEdit: TEdit;
    FluxGroup: TGroupBox;
    BPMLabel: TLabel;
    ILabel: TLabel;
    JLabel: TLabel;
    HLabel: TLabel;
    KLabel: TLabel;
    IEdit: TMaskEdit;
    JEdit: TMaskEdit;
    HEdit: TMaskEdit;
    KEdit: TMaskEdit;
    ULabel: TLabel;
    BLabel: TLabel;
    RLabel: TLabel;
    UEdit: TMaskEdit;
    BEdit: TMaskEdit;
    BErrEdit: TMaskEdit;
    REdit: TMaskEdit;
    procedure BEditChange(Sender: TObject);
    procedure BEditExit(Sender: TObject);
    procedure BErrEditExit(Sender: TObject);
    procedure FluxGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HEditExit(Sender: TObject);
    procedure IEditExit(Sender: TObject);
    procedure JEditExit(Sender: TObject);
    procedure KEditExit(Sender: TObject);
    procedure REditExit(Sender: TObject);
    procedure TempEditExit(Sender: TObject);
    procedure TempEditKeyPress(Sender: TObject; var Key: char);
    procedure UEditExit(Sender: TObject);
    procedure UseExtra2CBChange(Sender: TObject);
  private
    { private declarations }
    // pointers to data
    comp_pointer:NewStarBase;
    data_pointer:StarFluxPlus;
    has_data:Boolean;
    // private helper flux methods
    procedure MakBl(fe:TMaskEdit);
    procedure EnableUB(doen:Boolean);
    procedure EnableRIJHK(doen:Boolean);
    function LoadUB:Boolean;
    function LoadRIJHK:Boolean;
    procedure LoadUBBlank;
    procedure LoadRIJHKBlank;
    procedure LoadTempBlank;
    procedure LoadTemp;
    // private methods for saving fluxes
    function SaveMag(var from:TMaskEdit; withx:StoreMagMethod; band:Char):Boolean;
    function SaveUMag(showmsg:Boolean):Boolean;
    function SaveBMag(showmsg:Boolean):Boolean;
    function SaveRMag(showmsg:Boolean):Boolean;
    function SaveIMag(showmsg:Boolean):Boolean;
    function SaveJMag(showmsg:Boolean):Boolean;
    function SaveHMag(showmsg:Boolean):Boolean;
    function SaveKMag(showmsg:Boolean):Boolean;
    function SaveUB(showmsg:Boolean):Boolean;
    function SaveRIJHK(showmsg:Boolean):Boolean;
    // more saving methods
    function SaveTemp(showmsg:Boolean):Boolean;
    // higher level private methods
    procedure EnableAll(doit:Boolean);
    procedure LoadNothing;
    procedure LoadData;
    function SaveData(showmsg:Boolean):Boolean;
    procedure SetNoDataCB;
  public
    { public declarations }
    // changing data/state
    procedure SetToNothing;
    procedure SetToSun;
    function SetToComponent(newobj:NewStarBase):Boolean;
    // additional external changes
    procedure SaveExternal;
    procedure ReloadObject;
  end;

var
  StarExtras2Form: TStarExtras2Form;

implementation

{$R *.lfm}

procedure TStarExtras2Form.UEditExit(Sender: TObject);
begin
  SaveUMag(True);
end;

procedure TStarExtras2Form.UseExtra2CBChange(Sender: TObject);
var checkval:Boolean;
begin
  checkval := UseExtra2CB.Checked;
  if checkval<> has_data then begin
    if checkval then begin
      // creating the new object
      comp_pointer.fluxtemp := StarFluxPlus.Create;
      has_data := True;
      // loading it
      LoadData;
    end
    else begin
      // loading nothing
      EnableAll(True);
      LoadNothing;
      // getting rid of the data
      data_pointer := nil;
      comp_pointer.fluxtemp.Free;
      comp_pointer.fluxtemp := nil;
      has_data := False;
    end;
  end;
end;

procedure TStarExtras2Form.REditExit(Sender: TObject);
begin
  SaveRMag(True);
end;

procedure TStarExtras2Form.TempEditExit(Sender: TObject);
begin
  SaveTemp(True);
end;

procedure TStarExtras2Form.TempEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [#8, '0'..'9']) then Key := #0
  else inherited;
end;

procedure TStarExtras2Form.IEditExit(Sender: TObject);
begin
  SaveIMag(True);
end;

procedure TStarExtras2Form.HEditExit(Sender: TObject);
begin
  SaveHMag(True);
end;

procedure TStarExtras2Form.BEditExit(Sender: TObject);
begin
  SaveBMag(True);
end;

procedure TStarExtras2Form.BEditChange(Sender: TObject);
begin

end;

procedure TStarExtras2Form.BErrEditExit(Sender: TObject);
begin
  SaveBMag(True);
end;

procedure TStarExtras2Form.FluxGroupClick(Sender: TObject);
begin

end;

procedure TStarExtras2Form.FormCreate(Sender: TObject);
begin

end;

procedure TStarExtras2Form.JEditExit(Sender: TObject);
begin
  SaveJMag(True);
end;

procedure TStarExtras2Form.KEditExit(Sender: TObject);
begin
  SaveKMag(True);
end;

//=========================================================================
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TStarExtras2Form.MakBl(fe:TMaskEdit);
begin
  Assert(fe<>nil);
  fe.Text := '+99.999';
end;
//---------------------------------
procedure TStarExtras2Form.EnableUB(doen:Boolean);
begin
  UEdit.Enabled := doen;
  BEdit.Enabled := doen;
  BErrEdit.Enabled := doen;
end;
//---------------------------------
procedure TStarExtras2Form.EnableRIJHK(doen:Boolean);
begin
  REdit.Enabled := doen;
  IEdit.Enabled := doen;
  JEdit.Enabled := doen;
  HEdit.Enabled := doen;
  KEdit.Enabled := doen;
end;
//---------------------------------
function TStarExtras2Form.LoadUB:Boolean;
begin
  Result := False;
  if data_pointer = nil then Exit;
  // ultraviolet
  if (not data_pointer.Valid_UltravioletMag) then MakBl(UEdit)
  else UEdit.Text := data_pointer.UltravioletMagStr;
  UEdit.Modified := False;
  // blue
  if (not data_pointer.Valid_BlueMagnitude) then begin
    MakBl(BEdit);
    BErrEdit.Text:= '0.000';
  end
  else begin
    BEdit.Text := data_pointer.BlueMagnitudeStr;
    BErrEdit.Text := data_pointer.BlueMag_ErrorStr;
  end;
  BEdit.Modified := False;
  BErrEdit.Modified := False;
  // done
  Result := True;
end;
//---------------------------------
function TStarExtras2Form.LoadRIJHK:Boolean;
begin
  Result := False;
  if data_pointer = nil then Exit;
  // red
  if (not data_pointer.Valid_RedMagnitude) then MakBl(REdit)
  else REdit.Text := data_pointer.RedMagnitudeStr;
  REdit.Modified := False;
  // near infrared I
  if (not data_pointer.Valid_IMagnitude) then MakBl(IEdit)
  else IEdit.Text := data_pointer.I_MagnitudeStr;
  IEdit.Modified := False;
  // near infrared J
  if (not data_pointer.Valid_JMagnitude) then MakBl(JEdit)
  else JEdit.Text := data_pointer.J_MagnitudeStr;
  JEdit.Modified := False;
  // near infrared H
  if (not data_pointer.Valid_HMagnitude) then MakBl(HEdit)
  else HEdit.Text := data_pointer.H_MagnitudeStr;
  HEdit.Modified := False;
  // near infrared K
  if (not data_pointer.Valid_KMagnitude) then MakBl(KEdit)
  else KEdit.Text := data_pointer.K_MagnitudeStr;
  KEdit.Modified := False;
  // done
  Result := True;
end;
//---------------------------------
procedure TStarExtras2Form.LoadUBBlank;
begin
  MakBl(UEdit);  MakBl(BEdit);
  BErrEdit.Text := '0.000';
end;
//---------------------------------
procedure TStarExtras2Form.LoadRIJHKBlank;
begin
  MakBl(REdit);   MakBl(IEdit);
  MakBl(JEdit);   MakBl(HEdit);
  MakBl(KEdit);
end;
//--------------------------------
procedure TStarExtras2Form.LoadTempBlank;
begin
  TempEdit.Text := '0';
  SimbadTempDisplay.Caption := '';
end;
//--------------------------------
procedure TStarExtras2Form.LoadTemp;
begin
  if data_pointer <> nil then begin
    TempEdit.Text := data_pointer.EffectiveTempStr;
    SimbadTempDisplay.Caption := data_pointer.SimbadTempInfo;
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// private methods for saving fluxes
function TStarExtras2Form.SaveMag(var from:TMaskEdit; withx:StoreMagMethod; band:Char):Boolean;
var strsrc:string;
    failmsg:string;
begin
  // assert check
  Assert(from<>nil);
  Assert(data_pointer<>nil);
  Result := True;
  if (not from.Modified) then Exit;
  // getting and saving
  strsrc := from.Text;
  Result := withx(strsrc);
  // handling a negative result
  if (band<>'X') and (not Result) then begin
    failmsg := 'Saving ' + band + ' magnitude failed.' + sLineBreak;
    failmsg += 'Not a number or out of bounds.';
    ShowMessage(failmsg);
  end
  else if (Result) then from.Modified := False;
end;
//------------------------------------
function TStarExtras2Form.SaveUMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'U','X')[1];
  Result := SaveMag(UEdit,data_pointer.SetUMag,pchar);
end;
//------------------------------------
function TStarExtras2Form.SaveBMag(showmsg:Boolean):Boolean;
var bsrc,berr:string;
    failmsg:string;
begin
  Assert(data_pointer<>nil);
  Result := True;
  if (not BEdit.Modified) and (not BErrEdit.Modified) then Exit;
  bsrc := BEdit.Text;
  berr := BErrEdit.Text;
  Result := data_pointer.SetBMag(bsrc,berr);
  if (not Result) and showmsg then begin
    failmsg := 'Failed to save B magnitude.' + sLineBreak;
    failmsg += 'Out of bounds, or not a number';
    ShowMessage(failmsg);
  end
  else if Result then begin
    BEdit.Modified := False;
    BErrEdit.Modified := False;
  end;
end;
//------------------------------------
function TStarExtras2Form.SaveRMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'R','X')[1];
  Result := SaveMag(REdit,data_pointer.SetRMag,pchar);
end;
//------------------------------------
function TStarExtras2Form.SaveIMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'I','X')[1];
  Result := SaveMag(IEdit,data_pointer.SetIMag,pchar);
end;
//------------------------------------
function TStarExtras2Form.SaveJMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'J','X')[1];
  Result := SaveMag(JEdit,data_pointer.SetJMag,pchar);
end;
//------------------------------------
function TStarExtras2Form.SaveHMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'H','X')[1];
  Result := SaveMag(HEdit,data_pointer.SetHMag,pchar);
end;
//------------------------------------
function TStarExtras2Form.SaveKMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'K','X')[1];
  Result := SaveMag(KEdit,data_pointer.SetKMag,pchar);
end;
//------------------------------------
function TStarExtras2Form.SaveUB(showmsg:Boolean):Boolean;
begin
  Result := False;
  if data_pointer = nil then Exit;
  Result := SaveUMag(showmsg);
  if (not Result) then Exit;
  Result := SaveBMag(showmsg);
end;
//------------------------------------
function TStarExtras2Form.SaveRIJHK(showmsg:Boolean):Boolean;
begin
  Result := False;
  if data_pointer = nil then Exit;
  Result := SaveRMag(showmsg);
  if (not Result) then Exit;
  Result := SaveIMag(showmsg);
  if (not Result) then Exit;
  Result := SaveJMag(showmsg);
  if (not Result) then Exit;
  Result := SaveHMag(showmsg);
  if (not Result) then Exit;
  Result := SaveKMag(showmsg);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
// more saving methods
function TStarExtras2Form.SaveTemp(showmsg:Boolean):Boolean;
var texttemp:string;
begin
  Result := False;
  if data_pointer = nil then Exit;
  Result := True;
  if (not TempEdit.Modified) then Exit;
  texttemp := TempEdit.Text;
  Result := data_pointer.SetTemp(texttemp);
  if ((not Result) and showmsg) then begin
    ShowMessage('Effective Temperature not valid.');
  end
  else if Result then TempEdit.Modified := False;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
// higher level private methods
//-------------------------------
procedure TStarExtras2Form.EnableAll(doit:Boolean);
begin
  EnableUB(doit);
  EnableRIJHK(doit);
  TempEdit.Enabled := doit;
end;
//-------------------------------
procedure TStarExtras2Form.LoadNothing;
begin
  LoadUBBlank;
  LoadRIJHKBlank;
  TempEdit.Text := '0';
  EnableAll(False);
end;
//---------------------------------------
procedure TStarExtras2Form.LoadData;
begin
  // always necessary
  EnableAll(True);
  // checking the main pointer
  if comp_pointer = nil then data_pointer := nil
  else data_pointer := comp_pointer.fluxtemp;
  // loading nothing if nil...
  if data_pointer = nil then LoadNothing
  else begin
    LoadRIJHK;
    // for Brown Dwarfs, U B, and temperature, is not supported
    if comp_pointer.isBrownDwarf then begin
      LoadUBBlank;
      EnableUB(false);
      LoadTempBlank;
      TempEdit.Enabled := False;
    end
    else begin
      LoadUB;
      LoadTemp;
    end;
  end;
  // done
end;
//--------------------------------------------------
function TStarExtras2Form.SaveData(showmsg:Boolean):Boolean;
begin
  if data_pointer <> nil then begin
    Result := SaveRIJHK(showmsg);
    if (not Result) then Exit;
    // save only if not a brown dwarf
    if (not comp_pointer.isBrownDwarf) then begin
      Result := SaveTemp(showmsg);
      if (not Result) then Exit;
      Result := SaveUB(showmsg);
    end;
  end
  else Result := False;
end;
//------------------------------------
procedure TStarExtras2Form.SetNoDataCB;
begin
  has_data := False;
  UseExtra2CB.Enabled := True;
  UseExtra2CB.Checked := False;
  UseExtra2CB.Enabled := False;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++
// changing data/state
//----------------------------
procedure TStarExtras2Form.SetToNothing;
begin
  SetNoDataCB;
  comp_pointer := nil;
  // properly loading nothing
  LoadData;
end;
//----------------------------
procedure TStarExtras2Form.SetToSun;
begin
  SetNoDataCB;
  comp_pointer := nil;
  data_pointer := nil;
  EnableAll(True);
  TempEdit.Text := '5770';
  UEdit.Text := '-25.899';
  BEdit.Text := '-26.057';
  BErrEdit.Text := '0.000';
  REdit.Text := '-27.066';
  IEdit.Text := '-27.411';
  JEdit.Text := '-27.908';
  HEdit.Text := '-28.194';
  KEdit.Text := '-28.270';
  EnableAll(False);
end;

//----------------------------
function TStarExtras2Form.SetToComponent(newobj:NewStarBase):Boolean;
begin
  Assert(newobj<>nil);
  Result := False;
  UseExtra2CB.Enabled := True;
  comp_pointer := newobj;
  LoadData;
  has_data := (data_pointer<>nil);
  UseExtra2CB.Checked := has_data;
  Result := True;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// additional external changes
//-----------------------------
procedure TStarExtras2Form.SaveExternal;
begin
  if comp_pointer <> nil then SaveData(False);
end;
//-----------------------------
procedure TStarExtras2Form.ReloadObject;
begin
  if comp_pointer <> nil then begin
    UseExtra2CB.Enabled := True;
    // dealing with a change of whether the extra data exists...
    if (not has_data) and (comp_pointer.fluxtemp <> nil) then begin
      has_data := True;
      UseExtra2CB.Checked := True;
    end
    else if (has_data) and (comp_pointer.fluxtemp = nil) then begin
      has_data := False;
      UseExtra2CB.Checked := False;
    end;
    // finish off with the same load data for everyone
    LoadData;
  end;
end;

//=========================================================================

end.

