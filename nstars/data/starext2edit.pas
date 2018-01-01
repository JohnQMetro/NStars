unit starext2edit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, Dialogs,
  ExtCtrls, StrUtils, NewStar, StarExt2;

type

  StoreMagMethod = function(X:string):Boolean of object;

  { TStarExtras2Frame }

  TStarExtras2Frame = class(TFrame)
    BEdit: TMaskEdit;
    BErrEdit: TMaskEdit;
    BLabel: TLabel;
    BPMLabel: TLabel;
    BPMLabel1: TLabel;
    FluxInfoLabel: TLabel;
    HEdit: TMaskEdit;
    HLabel: TLabel;
    IEdit: TMaskEdit;
    ILabel: TLabel;
    JEdit: TMaskEdit;
    JLabel: TLabel;
    KEdit: TMaskEdit;
    KLabel: TLabel;
    FeHMEdit: TMaskEdit;
    FeHEEdit: TMaskEdit;
    FeHPMLabel: TLabel;
    FeHLabel: TLabel;
    EffectiveTempLabel: TLabel;
    KErrEdit: TMaskEdit;
    LoggLabel: TLabel;
    LogGEdit: TMaskEdit;
    SimbadFeHDisplay: TLabel;
    REdit: TMaskEdit;
    RLabel: TLabel;
    UEdit: TMaskEdit;
    ULabel: TLabel;
    SimbadTempDisplay: TLabel;
    TempEdit: TMaskEdit;
    UseExtra2CB: TCheckBox;
    FluxGroup: TGroupBox;
    procedure BEditExit(Sender: TObject);
    procedure BEditKeyPress(Sender: TObject; var Key: char);
    procedure BErrEditExit(Sender: TObject);
    procedure FeHMEditKeyPress(Sender: TObject; var Key: char);
    procedure FluxGroupClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure HEditExit(Sender: TObject);
    procedure HEditKeyPress(Sender: TObject; var Key: char);
    procedure IEditExit(Sender: TObject);
    procedure IEditKeyPress(Sender: TObject; var Key: char);
    procedure JEditExit(Sender: TObject);
    procedure JEditKeyPress(Sender: TObject; var Key: char);
    procedure KEditExit(Sender: TObject);
    procedure KEditKeyPress(Sender: TObject; var Key: char);
    procedure KErrEditChange(Sender: TObject);
    procedure KErrEditExit(Sender: TObject);
    procedure LogGEditKeyPress(Sender: TObject; var Key: char);
    procedure REditChange(Sender: TObject);
    procedure REditExit(Sender: TObject);
    procedure REditKeyPress(Sender: TObject; var Key: char);
    procedure TempEditExit(Sender: TObject);
    procedure TempEditKeyPress(Sender: TObject; var Key: char);
    procedure UEditExit(Sender: TObject);
    procedure UEditKeyPress(Sender: TObject; var Key: char);
    procedure UseExtra2CBChange(Sender: TObject);
  private
    { private declarations }
    // pointers to data
    comp_pointer:NewStarBase;
    data_pointer:StarFluxPlus;
    has_data:Boolean;
    // aslo
    ChangeHandler:TNotifyEvent;
    // private helper flux methods
    procedure MakBl(fe:TMaskEdit);
    procedure EnableUB(doen:Boolean);
    procedure EnableRIJHK(doen:Boolean);
    function LoadUB:Boolean;
    function LoadRIJHK:Boolean;
    procedure LoadUBBlank;
    procedure LoadRIJHKBlank;
    // other loading methods
    function MakeColorIndexes:string;
    procedure LoadColorIndexes;
    procedure LoadTempBlank;
    procedure LoadTemp;
    procedure LoadFeHBlank;
    procedure LoadFeH;
    procedure LoadLoggBlank;
    procedure LoadLogg;
    procedure EnableFeH(doen:Boolean);
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
    function SaveFeH(showmsg:Boolean):Boolean;
    function SaveLogg(showmsg:Boolean):Boolean;
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
    procedure SetFluxTempChangeHandler(newHandler:TNotifyEvent);
  end;

implementation

{$R *.lfm}

{ TStarExtras2Frame }

procedure TStarExtras2Frame.FluxGroupClick(Sender: TObject);
begin
  SaveData(True);
end;

procedure TStarExtras2Frame.FrameClick(Sender: TObject);
begin
  SaveData(True);
end;

procedure TStarExtras2Frame.HEditExit(Sender: TObject);
begin
  SaveHMag(True);
end;

procedure TStarExtras2Frame.HEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := HEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.IEditExit(Sender: TObject);
begin
  SaveIMag(True);
end;

procedure TStarExtras2Frame.IEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := IEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.JEditExit(Sender: TObject);
begin
  SaveJMag(True);
end;

procedure TStarExtras2Frame.JEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := JEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.KEditExit(Sender: TObject);
begin
  SaveKMag(True);
end;

procedure TStarExtras2Frame.KEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := KEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.KErrEditChange(Sender: TObject);
begin

end;

procedure TStarExtras2Frame.KErrEditExit(Sender: TObject);
begin
  SaveKMag(True);
end;

procedure TStarExtras2Frame.LogGEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := LogGEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.REditChange(Sender: TObject);
begin

end;

procedure TStarExtras2Frame.REditExit(Sender: TObject);
begin
  SaveRMag(True);
end;

procedure TStarExtras2Frame.REditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := REdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.TempEditExit(Sender: TObject);
begin
  SaveTemp(True);
end;

procedure TStarExtras2Frame.BEditExit(Sender: TObject);
begin
  SaveBMag(True);
end;

procedure TStarExtras2Frame.BEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := BEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.BErrEditExit(Sender: TObject);
begin
  SaveBMag(True);
end;

procedure TStarExtras2Frame.FeHMEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := FeHMEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.TempEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [#8, '0'..'9']) then Key := #0
  else inherited;
end;

procedure TStarExtras2Frame.UEditExit(Sender: TObject);
begin
  SaveUMag(True);
end;

procedure TStarExtras2Frame.UEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := UEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TStarExtras2Frame.UseExtra2CBChange(Sender: TObject);
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
      if Assigned(ChangeHandler) then ChangeHandler(Self);
    end;
  end;
end;
//=========================================================================
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TStarExtras2Frame.MakBl(fe:TMaskEdit);
begin
  Assert(fe<>nil);
  fe.Text := '+99.999';
end;
//---------------------------------
procedure TStarExtras2Frame.EnableUB(doen:Boolean);
begin
  UEdit.Enabled := doen;
  BEdit.Enabled := doen;
  BErrEdit.Enabled := doen;
end;
//---------------------------------
procedure TStarExtras2Frame.EnableRIJHK(doen:Boolean);
begin
  REdit.Enabled := doen;
  IEdit.Enabled := doen;
  JEdit.Enabled := doen;
  HEdit.Enabled := doen;
  KEdit.Enabled := doen;
  KErrEdit.Enabled := doen;
end;
//---------------------------------
function TStarExtras2Frame.LoadUB:Boolean;
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
function TStarExtras2Frame.LoadRIJHK:Boolean;
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
  if (not data_pointer.Valid_KMagnitude) then begin
    MakBl(KEdit);
    KErrEdit.Text := '0.000';
  end
  else begin
    KEdit.Text := data_pointer.K_MagnitudeStr;
    KerrEdit.Text:= data_pointer.KMag_ErrorStr;
  end;
  KEdit.Modified := False;
  KerrEdit.Modified := False;
  // done
  Result := True;
end;
//---------------------------------
procedure TStarExtras2Frame.LoadUBBlank;
begin
  MakBl(UEdit);  MakBl(BEdit);
  BErrEdit.Text := '0.000';
end;
//---------------------------------
procedure TStarExtras2Frame.LoadRIJHKBlank;
begin
  MakBl(REdit);   MakBl(IEdit);
  MakBl(JEdit);   MakBl(HEdit);
  MakBl(KEdit);   KErrEdit.Text := '0.000';
end;
//+++++++++++++++++++++++++++++++++++++++++++++++==
function TStarExtras2Frame.MakeColorIndexes:string;
var bdwarf:Boolean;
    cstar:StarInfo;
    vxmag:Real;
    qxval,cval:string;
begin
  if data_pointer = nil then Result := ''
  else begin
    // initial setup
    qxval := 'Colors: ';
    bdwarf := comp_pointer.isBrownDwarf;
    vxmag := 999;
    if (not bdwarf) then begin
      cstar := StarInfo(comp_pointer);
      if cstar.ValidVisualMagnitude then vxmag := cstar.VisualMagnitude;
    end
    else cstar := nil;
    // no UBV for brown dwarfs
    if (not bdwarf) then begin
      // U-B
      Result += data_pointer.UminusB;
      if (vxmag<>999) then begin
        cval := data_pointer.BminusV(vxmag); // B-V
        if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
        Result += cval;
        cval := ' ' + data_pointer.VminusR(vxmag); // V-R
        if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
        Result += cval;
        (*
        cval := ' ' + data_pointer.VminusI(vxmag);
        if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
        Result += cval;
        *)
        cval := ' ' + data_pointer.VminusK(vxmag); // V-K
        if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
        Result += cval;
      end;
    end;
    // all stars and brown dwarfs can have RIJHK
    cval := ' ' + data_pointer.RminusI; // R-I
    if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
    Result += cval;
    // I'll allow this for brown dwarfs only (for stars, not enough room)
    if bdwarf then begin
      cval := ' ' + data_pointer.IminusK;
      if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
      Result += cval;
    end;
    // all stars and brown dwarfs
    cval := ' ' + data_pointer.JminusH; // J-H
    if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
    Result += cval;
    cval := ' ' + data_pointer.HminusK; // H-K
    if (Length(cval)<>0) and (Length(Result)<>0) then Result += ', ';
    Result += cval;
    // finalizing
    Result := qxval + Trim(Result);
  end;
end;
//------------------------------------------
procedure TStarExtras2Frame.LoadColorIndexes;
begin
  FluxInfoLabel.Caption:= MakeColorIndexes;
end;
//------------------------------------------
procedure TStarExtras2Frame.LoadTempBlank;
begin
  TempEdit.Text := '0';
  SimbadTempDisplay.Caption := '';
end;
//--------------------------------
procedure TStarExtras2Frame.LoadTemp;
begin
  if data_pointer <> nil then begin
    TempEdit.Text := data_pointer.EffectiveTempStr;
    SimbadTempDisplay.Caption := data_pointer.SimbadTempInfo;
  end;
end;
//--------------------------------
procedure TStarExtras2Frame.LoadFeHBlank;
begin
  FeHMEdit.Text := '+9.99';
  FeHEEdit.Text := '0.00';
  SimbadFeHDisplay.Caption := '';
end;
//--------------------------------
procedure TStarExtras2Frame.LoadFeH;
begin
  if data_pointer <> nil then begin
    FeHMEdit.Text := data_pointer.FeHMedianStr;
    FeHEEdit.Text := data_pointer.FeHErrStr;
    SimbadFeHDisplay.Caption:= data_pointer.SimbadFeHStr;
  end;
end;
//--------------------------------
procedure TStarExtras2Frame.LoadLoggBlank;
begin LogGEdit.Text := '-9.99';   end;
//--------------------------------
procedure TStarExtras2Frame.LoadLogg;
begin
  if data_pointer <> nil then begin
    LogGEdit.Text := data_pointer.LogGStr;
  end;
end;
//---------------------------------------
procedure TStarExtras2Frame.EnableFeH(doen:Boolean);
begin
  FeHMEdit.Enabled := doen;
  FeHEEdit.Enabled := doen;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// private methods for saving fluxes
function TStarExtras2Frame.SaveMag(var from:TMaskEdit; withx:StoreMagMethod; band:Char):Boolean;
var strsrc:string;
    failmsg:string;
begin
  // assert check
  Assert(from<>nil);
  Assert(data_pointer<>nil);
  Result := True;
  // if (not from.Modified) then Exit;
  // getting and saving
  strsrc := from.Text;
  Result := withx(strsrc);
  // handling a negative result
  if (band<>'X') and (not Result) then begin
    failmsg := 'Saving ' + band + ' magnitude failed.' + sLineBreak;
    failmsg += 'Not a number or out of bounds.';
    ShowMessage(failmsg);
  end
  else if (Result) then begin
    from.Modified := False;
    if band<>'X' then LoadColorIndexes;
  end;
end;
//------------------------------------
function TStarExtras2Frame.SaveUMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'U','X')[1];
  Result := SaveMag(UEdit,data_pointer.SetUMag,pchar);
  if (showmsg and Result and Assigned(ChangeHandler)) then ChangeHandler(Self);
end;
//------------------------------------
function TStarExtras2Frame.SaveBMag(showmsg:Boolean):Boolean;
var bsrc,berr:string;
    failmsg:string;
begin
  Assert(data_pointer<>nil);
  Result := True;
  // if (not BEdit.Modified) and (not BErrEdit.Modified) then Exit;
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
    if (showmsg and Assigned(ChangeHandler)) then ChangeHandler(Self);
  end;
end;
//------------------------------------
function TStarExtras2Frame.SaveRMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'R','X')[1];
  Result := SaveMag(REdit,data_pointer.SetRMag,pchar);
  if (showmsg and Result and Assigned(ChangeHandler)) then ChangeHandler(Self);
end;
//------------------------------------
function TStarExtras2Frame.SaveIMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'I','X')[1];
  Result := SaveMag(IEdit,data_pointer.SetIMag,pchar);
  if (showmsg and Result and Assigned(ChangeHandler)) then ChangeHandler(Self);
end;
//------------------------------------
function TStarExtras2Frame.SaveJMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'J','X')[1];
  Result := SaveMag(JEdit,data_pointer.SetJMag,pchar);
  if (showmsg and Result and Assigned(ChangeHandler)) then ChangeHandler(Self);
end;
//------------------------------------
function TStarExtras2Frame.SaveHMag(showmsg:Boolean):Boolean;
var pchar:Char;
begin
  pchar := IfThen(showmsg,'H','X')[1];
  Result := SaveMag(HEdit,data_pointer.SetHMag,pchar);
  if (showmsg and Result and Assigned(ChangeHandler)) then ChangeHandler(Self);
end;
//------------------------------------
function TStarExtras2Frame.SaveKMag(showmsg:Boolean):Boolean;
var ksrc,kerr_src:string;
    failmsg:string;
begin
  Assert(data_pointer<>nil);
  Result := True;
  ksrc := KEdit.Text;
  kerr_src := KerrEdit.Text;
  if (ksrc <> data_pointer.K_MagnitudeStr) or (kerr_src <> data_pointer.KMag_ErrorStr) then begin
    Result := data_pointer.SetKMag(ksrc,kerr_src);
    if (not Result) and showmsg then begin
      failmsg := 'Failed to save K magnitude.' + sLineBreak;
      failmsg += 'Out of bounds, or not a number';
      ShowMessage(failmsg);
    end
    else if Result then begin
      KEdit.Modified := False;
      KErrEdit.Modified := False;
      if (showmsg and Assigned(ChangeHandler)) then ChangeHandler(Self);
    end;
  end;
end;
//------------------------------------
function TStarExtras2Frame.SaveUB(showmsg:Boolean):Boolean;
begin
  Result := False;
  if data_pointer = nil then Exit;
  Result := SaveUMag(showmsg);
  if (not Result) then Exit;
  Result := SaveBMag(showmsg);
end;
//------------------------------------
function TStarExtras2Frame.SaveRIJHK(showmsg:Boolean):Boolean;
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
function TStarExtras2Frame.SaveTemp(showmsg:Boolean):Boolean;
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
  else if Result then begin
    TempEdit.Modified := False;
    if (showmsg and Assigned(ChangeHandler)) then ChangeHandler(Self);
  end;
end;
//----------------------------------------------
function TStarExtras2Frame.SaveFeH(showmsg:Boolean):Boolean;
var med, err:string;
begin
  // initial checking
  Result := False;
  if data_pointer = nil then Exit;
  Result := True;
  // if (not FeHMEdit.Modified) and (not FeHEEdit.Modified) then Exit;
  // going ahead with saving
  med := FeHMEdit.Text;
  err := FeHEEdit.Text;
  Result := data_pointer.SetFeHDex(med,err);
  // post save
  if ((not Result) and showmsg) then begin
    ShowMessage('Fe/H not valid.');
  end
  else if Result then begin
    FeHMEdit.Modified := False;
    FeHEEdit.Modified := False;
    if (showmsg and Assigned(ChangeHandler)) then ChangeHandler(Self);
  end;
end;
//-----------------------------------------------
function TStarExtras2Frame.SaveLogg(showmsg:Boolean):Boolean;
var loggtext:string;
begin
  Result := False;
  if data_pointer = nil then Exit;
  loggtext := LogGEdit.Text;
  Result := data_pointer.SetLogG(loggtext);
  if ((not Result) and showmsg) then begin
    ShowMessage('Log g not valid.');
  end
  else if Result then begin
    LogGEdit.Modified := False;
    if (showmsg and Assigned(ChangeHandler)) then begin
      if comp_pointer.isWhiteDwarf then ChangeHandler(Self);
    end;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++
// higher level private methods
//-------------------------------
procedure TStarExtras2Frame.EnableAll(doit:Boolean);
begin
  EnableUB(doit);
  EnableRIJHK(doit);
  TempEdit.Enabled := doit;
  EnableFeH(doit);
  LogGEdit.Enabled := doit;
end;
//-------------------------------
procedure TStarExtras2Frame.LoadNothing;
begin
  LoadUBBlank;
  LoadRIJHKBlank;
  LoadTempBlank;
  LoadFeHBlank;
  LoadLoggBlank;
  LoadColorIndexes;
  EnableAll(False);
end;
//---------------------------------------
procedure TStarExtras2Frame.LoadData;
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
    LoadLogg;
    if comp_pointer.isWhiteDwarf then LoadFeHBlank
    else LoadFeH;
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
    LoadColorIndexes;
  end;
  // done
end;
//--------------------------------------------------
function TStarExtras2Frame.SaveData(showmsg:Boolean):Boolean;
begin
  if data_pointer <> nil then begin
    Result := SaveRIJHK(showmsg);
    if (not Result) then Exit;
    Result := SaveLogg(showmsg);
    if (not Result) then Exit;
    if not comp_pointer.isWhiteDwarf then begin
      Result := SaveFeH(showmsg);
      if (not Result) then Exit;
    end;
    // save only if not a brown dwarf
    if (not comp_pointer.isBrownDwarf) then begin
      Result := SaveTemp(showmsg);
      if (not Result) then Exit;
      Result := SaveUB(showmsg);
      if ((not showmsg) and Assigned(ChangeHandler)) then ChangeHandler(Self);
    end;
    LoadColorIndexes;
  end
  else Result := False;
end;
//------------------------------------
procedure TStarExtras2Frame.SetNoDataCB;
begin
  has_data := False;
  UseExtra2CB.Enabled := True;
  UseExtra2CB.Checked := False;
  UseExtra2CB.Enabled := False;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++
// changing data/state
//----------------------------
procedure TStarExtras2Frame.SetToNothing;
begin
  SetNoDataCB;
  comp_pointer := nil;
  // properly loading nothing
  LoadData;
end;
//----------------------------
procedure TStarExtras2Frame.SetToSun;
begin
  SetNoDataCB;
  comp_pointer := nil;
  data_pointer := nil;
  EnableAll(True);
  TempEdit.Text := '5772';
  UEdit.Text := '-25.899';
  BEdit.Text := '-26.057';
  BErrEdit.Text := '0.000';
  REdit.Text := '-27.066';
  IEdit.Text := '-27.411';
  JEdit.Text := '-27.908';
  HEdit.Text := '-28.194';
  KEdit.Text := '-28.270';
  KErrEdit.Text := '0.000';
  FeHMEdit.Text := '+0.00';
  FeHEEdit.Text := '0.00';
  LogGEdit.Text := '+4.44';
  EnableAll(False);
end;

//----------------------------
function TStarExtras2Frame.SetToComponent(newobj:NewStarBase):Boolean;
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
procedure TStarExtras2Frame.SaveExternal;
begin
  if comp_pointer <> nil then SaveData(False);
end;
//-----------------------------
procedure TStarExtras2Frame.ReloadObject;
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
//-----------------------------
procedure TStarExtras2Frame.SetFluxTempChangeHandler(newHandler:TNotifyEvent);
begin
  ChangeHandler := newHandler;
end;

//=========================================================================

end.

