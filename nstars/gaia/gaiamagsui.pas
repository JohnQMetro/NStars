unit gaiamagsui;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, MaskEdit, StdCtrls, Dialogs,
  gaiadr2base,NewStar, Graphics;

type

  { TGaiaMagsFrame }

  TGaiaMagsFrame = class(TFrame)
    GErrEdit: TMaskEdit;
    BPErrEdit: TMaskEdit;
    GaiaColorLabel: TLabel;
    RPErrEdit: TMaskEdit;
    BPMLbl1: TLabel;
    BPMLbl2: TLabel;
    BPMLbl3: TLabel;
    GEdit: TMaskEdit;
    BPEdit: TMaskEdit;
    RPEdit: TMaskEdit;
    GLabel: TLabel;
    GaiaMagsBox: TGroupBox;
    BPLabel: TLabel;
    RPLabel: TLabel;
    procedure BPEditExit(Sender: TObject);
    procedure BPEditKeyPress(Sender: TObject; var Key: char);
    procedure BPErrEditExit(Sender: TObject);
    procedure GaiaMagsBoxExit(Sender: TObject);
    procedure GEditExit(Sender: TObject);
    procedure GEditKeyPress(Sender: TObject; var Key: char);
    procedure GErrEditExit(Sender: TObject);
    procedure RPEditExit(Sender: TObject);
    procedure RPEditKeyPress(Sender: TObject; var Key: char);
    procedure RPErrEditChange(Sender: TObject);
    procedure RPErrEditExit(Sender: TObject);
  private
    starobj:NewStarBase;
    curmag:GaiaDR2Mags;
    issun:Boolean;

    const nomag = '+99.9999';
    const nomagerr = '0.0000';

    procedure ChangeEnabled(isEnabled:Boolean);
    procedure LoadNothing(var xmage:TMaskEdit; var xerr:TMaskEdit);
    procedure LoadG();
    procedure LoadBP();
    procedure LoadRP();
    procedure LoadBPRPLabel();
    procedure LoadMags();
    function TestSave(showerr:Boolean):Boolean;
    function SaveG(showerr:Boolean):Boolean;
    function SaveBP(showerr:Boolean):Boolean;
    function SaveRP(showerr:Boolean):Boolean;
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

implementation

{$R *.lfm}

{ TGaiaMagsFrame }

procedure TGaiaMagsFrame.GEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := GEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TGaiaMagsFrame.GErrEditExit(Sender: TObject);
begin
  SaveG(True);
end;

procedure TGaiaMagsFrame.RPEditExit(Sender: TObject);
begin
  if SaveRP(True) then LoadBPRPLabel();
end;

procedure TGaiaMagsFrame.RPEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := RPEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TGaiaMagsFrame.RPErrEditChange(Sender: TObject);
begin

end;

procedure TGaiaMagsFrame.RPErrEditExit(Sender: TObject);
begin
  SaveRP(True);
end;

procedure TGaiaMagsFrame.BPEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := BPEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TGaiaMagsFrame.BPErrEditExit(Sender: TObject);
begin
  SaveBP(True);
end;

procedure TGaiaMagsFrame.GaiaMagsBoxExit(Sender: TObject);
begin
  if SaveG(True) then begin
    if SaveBP(True) then SaveRP(True);
  end;
end;

procedure TGaiaMagsFrame.BPEditExit(Sender: TObject);
begin
  if SaveBP(True) then LoadBPRPLabel();
end;

procedure TGaiaMagsFrame.GEditExit(Sender: TObject);
begin
  SaveG(True);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaMagsFrame.ChangeEnabled(isEnabled:Boolean);
begin
  GEdit.Enabled := isEnabled;
  GerrEdit.Enabled := isEnabled;
  BPEdit.Enabled := isEnabled;
  BPErrEdit.Enabled := isEnabled;
  RPEdit.Enabled := isEnabled;
  RPErrEdit.Enabled := isEnabled;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaMagsFrame.LoadNothing(var xmage:TMaskEdit; var xerr:TMaskEdit);
begin
  xmage.Text := nomag;
  xerr.Text := nomagerr;
end;
//-----------------------------------------
procedure TGaiaMagsFrame.LoadG();
begin
  if curmag = nil then LoadNothing(GEdit,GErrEdit)
  else if curmag.G >= 90 then LoadNothing(GEdit,GErrEdit)
  else begin
    GEdit.Text := curmag.GString;
    GErrEdit.Text := Trim(CurrToStrF(curmag.Gerr,ffFixed,4));
  end;
end;
//-----------------------------------------
procedure TGaiaMagsFrame.LoadBP();
begin
  if curmag = nil then LoadNothing(BPEdit,BPErrEdit)
  else if curmag.BP >= 90 then LoadNothing(BPEdit,BPErrEdit)
  else begin
    BPEdit.Text := curmag.BPString;
    BPErrEdit.Text := Trim(CurrToStrF(curmag.BPerr,ffFixed,4));
  end;
end;
//-----------------------------------------
procedure TGaiaMagsFrame.LoadRP();
begin
  if curmag = nil then LoadNothing(RPEdit,RPErrEdit)
  else if curmag.RP >= 90 then LoadNothing(RPEdit,RPErrEdit)
  else begin
    RPEdit.Text := curmag.RPString;
    RPErrEdit.Text := Trim(CurrToStrF(curmag.RPerr,ffFixed,4));
  end;
end;
//-----------------------------------------
procedure TGaiaMagsFrame.LoadBPRPLabel();
var odisp:string;
    gmrp:Currency;
begin
  if issun then odisp := 'BP−RP: 0.809'
  else if curmag = nil then odisp := ''
  else if not curmag.ValidBPmRP then odisp := ''
  else begin
    odisp := 'BP−RP: ' + Trim(CurrToStrF(curmag.BPminRP,ffFixed,3));
    gmrp := curmag.G - curmag.RP;
    odisp += ' , G-RP: ' + Trim(CurrToStrF(gmrp,ffFixed,3));
  end;
  GaiaColorLabel.Caption := odisp;
end;
//-----------------------------------------
procedure TGaiaMagsFrame.LoadMags();
begin
  LoadG();
  LoadBP();
  LoadRP();
  LoadBPRPLabel();
  if curmag <> nil then begin
    if curmag.BadRatio then begin
        GEdit.Font.Color := clRed;
       BPEdit.Font.Color := clRed;
       RPEdit.Font.Color := clRed;
    end else begin
        GEdit.Font.Color := clDefault;
       BPEdit.Font.Color := clDefault;
       RPEdit.Font.Color := clDefault;
    end;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TGaiaMagsFrame.TestSave(showerr:Boolean):Boolean;
begin
  Result := False;
  // no star...
  if (starobj = nil) then begin
    if (showerr) then ShowMessage('No star, form should be disabled');
  end else Result := True;
end;
//-------------------------------------------------------
function TGaiaMagsFrame.SaveG(showerr:Boolean):Boolean;
var convg,convge:Currency;
begin
  Result := False;
  if not TestSave(showerr) then Exit;
  // trying to convert
  if not TryStrToCurr(GEdit.Text,convg) then begin
    if showerr then ShowMessage('G magnitude is not a number!');
    Exit;
  end;
  if not TryStrToCurr(GErrEdit.Text,convge) then begin
    if showerr then ShowMessage('G Mag error is not a number!');
    Exit;
  end;
  // if the converted mag is invalid, we still save if there is a mag object
  if (convg >= 90) and (curmag <> nil) then curmag.ClearG()
  else begin
    // if there is no mag object, we create one
    if curmag = nil then begin
      starobj.dr2mags := GaiaDR2Mags.Create();
      curmag := starobj.dr2mags;
    end;
    // saving g
    curmag.SetG_C(convg,convge);
  end;
  Result := True;
end;
//------------------------------------------
function TGaiaMagsFrame.SaveBP(showerr:Boolean):Boolean;
var convbp,convbpe:Currency;
begin
  Result := False;
  if not TestSave(showerr) then Exit;
  // trying to convert
  if not TryStrToCurr(BPEdit.Text,convbp) then begin
    if showerr then ShowMessage('BP magnitude is not a number!');
    Exit;
  end;
  if not TryStrToCurr(BPErrEdit.Text,convbpe) then begin
    if showerr then ShowMessage('BP Mag error is not a number!');
    Exit;
  end;
  // if the converted mag is invalid, we still save if there is a mag object
  if (convbp >= 90) and (curmag <> nil) then curmag.ClearBP()
  else begin
    // if there is no mag object, we create one
    if curmag = nil then begin
      starobj.dr2mags := GaiaDR2Mags.Create();
      curmag := starobj.dr2mags;
    end;
    // saving bp
    curmag.SetBP_C(convbp,convbpe);
  end;
  Result := True;
end;
//------------------------------------------
function TGaiaMagsFrame.SaveRP(showerr:Boolean):Boolean;
var convrp,convrpe:Currency;
begin
  Result := False;
  if not TestSave(showerr) then Exit;
  // trying to convert
  if not TryStrToCurr(RPEdit.Text,convrp) then begin
    if showerr then ShowMessage('RP magnitude is not a number!');
    Exit;
  end;
  if not TryStrToCurr(RPErrEdit.Text,convrpe) then begin
    if showerr then ShowMessage('RP Mag error is not a number!');
    Exit;
  end;
  // if the converted mag is invalid, we still save if there is a mag object
  if (convrp >= 90) and (curmag <> nil) then curmag.ClearRP()
  else begin
    // if there is no mag object, we create one
    if curmag = nil then begin
      starobj.dr2mags := GaiaDR2Mags.Create();
      curmag := starobj.dr2mags;
    end;
    // saving rp
    curmag.SetRP_C(convrp,convrpe);
  end;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaMagsFrame.SetToNothing;
begin
  ChangeEnabled(True);
  curmag := nil;
  starobj := nil;
  issun := False;
  LoadMags();
  ChangeEnabled(False);
end;
//---------------------------------------
procedure TGaiaMagsFrame.SetToSun;
begin
  ChangeEnabled(True);
  curmag := nil;
  starobj := nil;
  issun := True;
  // magnitudes estimated from 18 Scorpii
  GEdit.Text := '-26.8930';
  GerrEdit.Text := '0.0000';
  BPEdit.Text := '-26.5210';
  BPErrEdit.Text := '0.0000';
  RPEdit.Text:= '-27.3300';
  RPErrEdit.Text := '0.0000';
  LoadBPRPLabel();
  // done
  ChangeEnabled(False);
end;
//---------------------------------------
function TGaiaMagsFrame.SetToComponent(newobj:NewStarBase):Boolean;
begin
  if (newobj = nil) then SetToNothing()
  else begin
    starobj := newobj;
    curmag := newobj.dr2mags;
    issun := False;
    ChangeEnabled(True);
    LoadMags();
  end;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// additional external changes
//---------------------------------------
procedure TGaiaMagsFrame.SaveExternal;
begin
  SaveG(False);
  SaveBP(False);
  SaveRP(False);
  LoadBPRPLabel();
  if curmag <> nil then begin
    if (not curmag.ValidBPmRP) and (curmag.G > 90) then begin
      curmag := nil;
      FreeAndNil(starobj.dr2mags);
    end;
  end;
end;
//---------------------------------------
procedure TGaiaMagsFrame.ReloadObject;
begin
  if issun then SetToSun()
  else if (starobj = nil) then SetToNothing()
  else LoadMags();
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
end.

