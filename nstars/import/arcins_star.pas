unit arcins_star;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, ExtCtrls, MaskEdit, Arcins, stardata,
  newlocation, constellation, namedata, NewStar, Utilities;

type
  TArcinsDataDisplay = class(TForm)
    MV_GroupBox: TGroupBox;
    MV_ExistingLabel: TLabel;
    MV_ArcinsData: TLabeledEdit;
    MV_ExistDat: TLabel;
    MV_UseCB: TCheckBox;
    Spectra_GroupBox: TGroupBox;
    Label1: TLabel;
    Spectra_ExistDat: TLabel;
    Spectra_ArcinsData: TLabeledEdit;
    Spectra_UseCB: TCheckBox;
    RV_GroupBox: TGroupBox;
    Label2: TLabel;
    RV_ExistDat: TLabel;
    RV_ArcinsData: TLabeledEdit;
    RV_UseCB: TCheckBox;
    Parallax_GroupBox: TGroupBox;
    Label3: TLabel;
    Parallax_ExistDat: TLabel;
    Parallax_ArcinsData: TLabeledEdit;
    Parallax_UseCB: TCheckBox;
    Punc_GroupBox: TGroupBox;
    Label4: TLabel;
    Punc_ExistDat: TLabel;
    Punc_ArcinsData: TLabeledEdit;
    ArcinsStarLabel: TLabel;
    Arcins_StatusLabel: TLabel;
    A_SetConstellationCB: TCheckBox;
    UseDataBtn: TButton;
    CancelBtn: TButton;
    PMM_GroupBox: TGroupBox;
    Label5: TLabel;
    PMM_ExistDat: TLabel;
    PMM_ArcinsData: TLabeledEdit;
    PMM_useCB: TCheckBox;
    PMDir_GroupBox: TGroupBox;
    Label6: TLabel;
    PMDir_ExistDat: TLabel;
    PMDir_ArcinsData: TLabeledEdit;
    AddDesig_Edit: TLabeledEdit;
    SetLocCert_CB: TCheckBox;
    ArcinsIDEdit: TLabeledEdit;
    HipIDEdit: TLabeledEdit;
    YPCIDEdit: TLabeledEdit;
    procedure FormActivate(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure UseDataBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure EnableAll;
    procedure DisableAll;
  end;

ArcinsStarLoader = class(TThread)
  protected
    msg:string;
    parent:TArcinsDataDisplay;
    fstatus:Boolean;
    status_proxy:TLabel;
    ddata:string;
  public
    constructor Create(inf:TArcinsDataDisplay);
    procedure Execute; override;
    procedure LoadMsg;
    procedure LoadAllData(Sender: TObject);
  end;

var
  ArcinsDataDisplay: TArcinsDataDisplay;
  TheStarData:AricnsStar;
  starloader:ArcinsStarLoader;

implementation

{$R *.lfm}
//**************************************************************************
procedure TArcinsDataDisplay.EnableAll;
begin
  MV_GroupBox.Enabled := True;
  Spectra_GroupBox.Enabled := True;
  RV_GroupBox.Enabled := True;
  Parallax_GroupBox.Enabled := True;
  Punc_GroupBox.Enabled := True;
  PMM_GroupBox.Enabled := True;
  PMDir_GroupBox.Enabled := True;
  A_SetConstellationCB.Enabled := True;
  UseDataBtn.Enabled := True;
  AddDesig_Edit.Enabled := True;
  SetLocCert_CB.Enabled := True;
end;
//----------------------------------------------------------------------
procedure TArcinsDataDisplay.FormActivate(Sender: TObject);
begin
  starloader := ArcinsStarLoader.Create(Self);
  DisableAll;
  TheStarData := nil;
  starloader.Resume;
end;
//--------------------------------------------------------------
procedure TArcinsDataDisplay.UseDataBtnClick(Sender: TObject);
var localstar:NewStarBase;   sloc:Location;
    buffer:string;           sc:Integer;
    rapos,decpos:Real;
    hipname, ypcname:StarName;
    izstar:StarInfo;
    realdata1,realdata2:Real;
begin
  localstar := Aricns_CheckSystem.GetNewStar(Aricns_CheckStar);
  sloc:= Aricns_CheckSystem.GetLocation;
  // parallax
  if Parallax_UseCB.Checked then begin
    buffer := Trim(Parallax_ArcinsData.Text);
    Val(buffer,realdata1,sc);
    Assert(sc=0);
    buffer := Trim(Punc_ArcinsData.Text);
    Val(buffer,realdata2,sc);
    Assert(sc=0);
    sloc.UpdateParallax(realdata1,realdata2);
    sloc.source := 'Arcins';
  end;
  // visual magnitude
  if MV_UseCB.Checked then begin
    buffer := Trim(MV_ArcinsData.Text);
    Val(buffer,realdata1,sc);
    Assert(sc=0);
    if (not localstar.isBrownDwarf) then begin
      izstar := StarInfo(localstar);
      izstar.SetVisualMagnitude(realdata1);
    end;
  end;
  // spectral infomration
  if Spectra_UseCB.Checked then localstar.SpectralClass := Spectra_ArcinsData.Text;
  // radial velocity
  if RV_UseCB.Checked then begin
    buffer := Trim(RV_ArcinsData.Text);
    Val(buffer,realdata1,sc);
    Assert(sc=0);
    sloc.SetRadialVelocity(realdata1);
  end;

  // proper motion magnitude
  if PMM_UseCB.Checked then begin
    buffer := Trim(PMM_ArcinsData.Text);
    Val(buffer,realdata1,sc);
    Assert(sc=0);
    buffer := Trim(PMDir_ArcinsData.Text);
    Val(buffer,realdata2,sc);
    Assert(sc=0);
    sloc.SetProperMotion(realdata1,realdata2);
  end;
  // constellation
  if A_SetConstellationCB.Checked then begin
    rapos := sloc.GetDecimalRightAscension;
    decpos := sloc.GetDecimalDeclination;
    sc := Const_Data.LocatePoint(rapos,decpos,sloc.Epoch);
    Aricns_CheckSystem.constellation := sc;
  end;
  // certain location
  if SetLocCert_CB.Checked then begin
    sloc.uncertain := False;
  end;
  // adding a few designations
  if Aricns_CheckSystem.GetCompC = 1 then hipname := Aricns_CheckSystem.GetNames
  else hipname := localstar.GetNames;
  ypcname :=  Aricns_CheckSystem.GetNames;
  buffer := Trim(HipIDEdit.Text);
  if (buffer<>'') then hipname.SetCat('Hip '+buffer);
  buffer := Trim(YPCIDEdit.Text);
  if (buffer<>'') then ypcname.SetCat('YPC '+buffer);

  // the final, finishing touches
  if not AnsiContainsText(Aricns_CheckSystem.notes,'Arcins data.') then begin
    Aricns_CheckSystem.AppndNote('Arcins data.',true);
  end;
  TheStarData.Free;
  Release;
end;
//--------------------------------------------------------------------------
procedure TArcinsDataDisplay.CancelBtnClick(Sender: TObject);
begin
  TheStarData.Free;
  Release;
end;
//-----------------------------------------------------------------------

procedure TArcinsDataDisplay.DisableAll;
begin
  MV_GroupBox.Enabled := False;
  Spectra_GroupBox.Enabled := False;
  RV_GroupBox.Enabled := False;
  Parallax_GroupBox.Enabled := False;
  Punc_GroupBox.Enabled := False;
  A_SetConstellationCB.Enabled := False;
  UseDataBtn.Enabled := False;
  PMM_GroupBox.Enabled := False;
  PMDir_GroupBox.Enabled := False;
  SetLocCert_CB.Enabled := False;
end;
//***************************************************************************
constructor ArcinsStarLoader.Create(inf:TArcinsDataDisplay);
begin
  inherited Create(True);
  Assert(inf<>nil);
  parent := inf;
  msg := '';
  fstatus := False;
  status_proxy := parent.Arcins_StatusLabel;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure ArcinsStarLoader.Execute;
var geturl:string;
begin
  OnTerminate := LoadAllData;
  // starting
  msg := 'Initial Processing and Links...';
  Synchronize(LoadMsg);
  // we do a set of checks first
  Assert(Aricns_CheckSystem<>nil);
  Assert(Aricns_Checkstar>0);
  // getting the url
  geturl := FindAricns(Aricns_CheckSystem,Aricns_Checkstar);
  if geturl='' then begin
    msg := 'Unable to find information for the star!';
    Synchronize(LoadMsg);
    Exit;
  end;
  // we now try step 2: getting th page data
  msg := 'Getting Arcins Star data...';
  Synchronize(LoadMsg);
  ddata := GetMainPage(geturl);
  if ddata='' then begin
    msg := 'Unable to get information for the star!';
    Synchronize(LoadMsg);
    Exit;
  end;
  // we have got the data
  msg := 'Processing star information...';
  Synchronize(LoadMsg);
  TheStarData := AricnsStar.Create(ddata);
  fstatus := True;
  msg := 'loading Data...';
  Synchronize(LoadMsg);
end;
//-----------------------------------------------------------------------
procedure ArcinsStarLoader.LoadMsg;
begin
  status_proxy.Caption := msg;
end;
//------------------------------------------------------------------------
// this is usually called by the OnTerminate Event Handler
// it loads all the data into the forms
procedure ArcinsStarLoader.LoadAllData(Sender: TObject);
var buffer:string;        cstar:NewStarBase;
    tempreal:Real;        sloc:Location;
    paraarc,paracur:Real;
    apparent_mag:Real;
    intvalue:Real;        tcond:Boolean;
    stardata:StarInfo;
begin
  if fstatus then begin
    // loading the name
    if not TheStarData.GetCNSValue(buffer) then begin
      msg := 'Data Incomplete! (1)';
      LoadMsg;
      Exit;
    end;
    parent.ArcinsStarLabel.Caption := 'CNS Star: ' + buffer;
    // step 2...
    cstar := Aricns_CheckSystem.GetNewStar(Aricns_Checkstar);
    sloc:= Aricns_CheckSystem.GetLocation;
    // apparent visual magnitude
    if (not cstar.isBrownDwarf) then begin
      stardata := StarInfo(cstar);
      tempreal := stardata.VisualMagnitude;
      if (tempreal>4.80) and (tempreal<4.85) then parent.MV_UseCB.Checked := True;  //?
      Str(tempreal:6:2,buffer);
      parent.MV_ExistDat.Caption := Trim(buffer);
    end;

    if not TheStarData.GetApparentMag(tempreal) then begin
      msg := 'Data Incomplete! (2)';
      LoadMsg;
      Exit;
    end;
    Str(tempreal:6:2,buffer);
    parent.MV_ArcinsData.Text := Trim(buffer);
    apparent_mag := tempreal;
    // spectral type
    buffer := cstar.SpectralClass;
    tcond := ((buffer='#NAME?') or (buffer='') or (buffer='WD'));
    if tcond then parent.Spectra_UseCB.Checked := True;
    parent.Spectra_ExistDat.Caption := buffer;
    if not TheStarData.GetSpectra(buffer) then begin
      msg := 'Data Incomplete! (3)';
      LoadMsg;
      // spectral data is sometimes missing
      buffer := '??';
    end;
    parent.Spectra_ArcinsData.Text := buffer;
    // radial velocity
    tempreal := sloc.RadialVelocity;
    if tempreal=0 then parent.RV_UseCB.Checked := True;    
    Str(tempreal:7:2,buffer);
    parent.RV_ExistDat.Caption := Trim(buffer);
    if not TheStarData.GetRadialV(tempreal) then begin
      msg := 'Data Incomplete! (4)';
      LoadMsg;
      // radial velocity is somwtimes missing
      tempreal := 0;
    end;
    Str(tempreal:7:2,buffer);
    parent.RV_ArcinsData.Text := Trim(buffer);
    // parallax
    buffer := sloc.GetParallaxString(3,False);
    paracur := sloc.ParallaxMAS;
    parent.Parallax_ExistDat.Caption := Trim(buffer);
    if not TheStarData.GetParallax(tempreal) then begin
      msg := 'Data Incomplete! (5)';
      LoadMsg;
      Exit;
    end;
    Str(tempreal:8:6,buffer);
    parent.Parallax_ArcinsData.Text := Trim(buffer);
    paraarc := tempreal;
    // checking for Parallax discrepancies
    intvalue := paracur/tempreal -1;
    intvalue := Abs(intvalue);
    if intvalue>=0.1 then parent.Parallax_ExistDat.Color := clRed
    else if intvalue>=0.04 then parent.Parallax_ExistDat.Color := clYellow
    else if intvalue>=0.01 then parent.Parallax_ExistDat.Color := clGreen
    else parent.Parallax_ExistDat.Color := clLime;
    if intvalue<0.1 then parent.SetLocCert_CB.Checked := True;
    
    // parallax uncertainty
    buffer := FloatToStrF(sloc.ParallaxErrorMAS,ffFixed,3,3);
    parent.Punc_ExistDat.Caption := Trim(buffer);
    if not TheStarData.GetParaUnc(tempreal) then begin
      msg := 'Data Incomplete! (6)';
      LoadMsg;
      Exit;
    end;
    Str(tempreal:8:6,buffer);
    parent.Punc_ArcinsData.Text := Trim(buffer);
    // proper motion magnitude
    buffer := FloatToStrF(sloc.ProperMotionMagnitude,ffFixed,5,2);
    if (sloc.ProperMotionMagnitude)=0 then parent.PMM_UseCB.Checked := True;
    parent.PMM_ExistDat.Caption := Trim(buffer);
    if not TheStarData.GetPMMag(tempreal) then begin
      msg := 'Data Incomplete! (7)';
      LoadMsg;
      Exit;
    end;
    Str(tempreal:8:5,buffer);
    parent.PMM_ArcinsData.Text := Trim(buffer);
    // proper motion direction
    buffer := FloatToStrF(sloc.ProperMotionAngle,ffFixed,3,2);
    parent.PMDir_ExistDat.Caption := Trim(buffer);
    if not TheStarData.GetPMDir(tempreal) then begin
      msg := 'Data Incomplete! (8)';
      LoadMsg;
      Exit;
    end;
    Str(tempreal:7:3,buffer);
    parent.PMDir_ArcinsData.Text := Trim(buffer);
    // additional designations
    parent.AddDesig_Edit.Text := TheStarData.GetAddDesig;
    // constellation
    if Aricns_CheckSystem.constellation=1 then
      parent.A_SetConstellationCB.Checked := True;
    // hipparcos and yale catalog ids
    parent.HipIDEdit.Text := TheStarData.GetHip;
    parent.YPCIDEdit.Text := TheStarData.GetYPC;
    // done here
    msg := 'Data Loaded.';
    LoadMsg;
    parent.EnableAll;
  end;
end;
//***************************************************************************

end.
