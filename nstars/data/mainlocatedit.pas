unit MainLocatEdit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, Graphics,
  Dialogs,
  newlocation,Utilities2;

type

  { TMainLocatEditFrame }

  TMainLocatEditFrame = class(TFrame)
    BitL1: TLabel;
    BL2: TLabel;
    BL3: TLabel;
    BL4: TLabel;
    OldPllxEdit: TEdit;
    OldPllxLabel: TLabel;
    RadialVEdit: TEdit;
    RadialVLabel: TLabel;
    ParSrcLabel: TLabel;
    ParallaxSourceEdit: TEdit;
    UncertainLocationCB: TCheckBox;
    PMLabel: TLabel;
    PMMagEdit: TEdit;
    EpochPicker: TComboBox;
    EpochLabel: TLabel;
    DecLabel: TLabel;
    DecEdit: TMaskEdit;
    AngleEdit: TMaskEdit;
    ParallaxLabel: TLabel;
    PML: TLabel;
    PllxEdit: TMaskEdit;
    PllxErrEdit: TMaskEdit;
    RAEdit: TMaskEdit;
    RALabel: TLabel;
    LocationBox: TGroupBox;
    procedure AngleEditExit(Sender: TObject);
    procedure AngleEditKeyPress(Sender: TObject; var Key: char);
    procedure DecEditExit(Sender: TObject);
    procedure DecEditKeyPress(Sender: TObject; var Key: char);
    procedure FrameExit(Sender: TObject);
    procedure LocationBoxExit(Sender: TObject);
    procedure OldPllxEditExit(Sender: TObject);
    procedure ParallaxSourceEditExit(Sender: TObject);
    procedure PllxEditExit(Sender: TObject);
    procedure PllxErrEditExit(Sender: TObject);
    procedure PMMagEditExit(Sender: TObject);
    procedure PMMagEditKeyPress(Sender: TObject; var Key: char);
    procedure RadialVEditExit(Sender: TObject);
    procedure RadialVEditKeyPress(Sender: TObject; var Key: char);
    procedure RAEditExit(Sender: TObject);
    procedure RAEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    // variables
    LocData:Location;
    ParallaxChangedHandler:TNotifyEvent;
    // enable/disable widgets
    procedure EnableLocation(doen:Boolean);
    procedure EnableParallax(doen:Boolean);
    procedure EnableMotions(doen:Boolean);
    procedure EnableAll(doen:Boolean);
    // load data
    procedure LoadBlankLocation();
    procedure LoadBlankParallax();
    procedure LoadBlankMotions();
    procedure LoadBlankAll();
    function LoadLocation():Boolean;
    function LoadParallax():Boolean;
    function LoadMotions():Boolean;
    function LoadAll():Boolean;
    // save data
    function SaveLocation(showmsg:Boolean):Boolean;
    function SaveParallax(showmsg:Boolean):Boolean;
    function SaveProperMotion(showmsg:Boolean):Boolean;
    function SaveRadialVel(showmsg:Boolean):Boolean;
    procedure SaveParallaxSource;
    procedure SaveOldParallax;
  public
    { public declarations }
    procedure SetupStart(handler:TNotifyEvent);
    function ChangeToNothing:Boolean;
    function ChangeLocation(newloc:Location):Boolean;
    procedure Reload;
    function SaveAll(showmsg:Boolean):Boolean;
  end;

implementation

procedure TMainLocatEditFrame.RAEditExit(Sender: TObject);
begin
  SaveLocation(True);
end;
// trying to ensure only the right numbers get thru
procedure TMainLocatEditFrame.RAEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := RAEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '0'..'2'])) then Key := #0;
  if ((pressdex = 3) or (pressdex = 6)) and (not (Key in [#8, '0'..'5'])) then Key := #0;
end;

procedure TMainLocatEditFrame.DecEditExit(Sender: TObject);
begin
  SaveLocation(True);
end;

procedure TMainLocatEditFrame.DecEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := DecEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
  if ((pressdex = 4) or (pressdex = 7)) and (not (Key in [#8, '0'..'5'])) then Key := #0;

end;

procedure TMainLocatEditFrame.FrameExit(Sender: TObject);
begin
  SaveAll(True);
end;

procedure TMainLocatEditFrame.LocationBoxExit(Sender: TObject);
begin
  SaveAll(True);
end;

procedure TMainLocatEditFrame.OldPllxEditExit(Sender: TObject);
begin
  SaveOldParallax();
end;

procedure TMainLocatEditFrame.ParallaxSourceEditExit(Sender: TObject);
begin
  SaveParallaxSource();
end;

procedure TMainLocatEditFrame.AngleEditExit(Sender: TObject);
begin
  SaveProperMotion(True);
end;

procedure TMainLocatEditFrame.AngleEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := AngleEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '0'..'3'])) then Key := #0;
end;

procedure TMainLocatEditFrame.PllxEditExit(Sender: TObject);
begin
  SaveParallax(True);
end;

procedure TMainLocatEditFrame.PllxErrEditExit(Sender: TObject);
begin
  SaveParallax(True);
end;

procedure TMainLocatEditFrame.PMMagEditExit(Sender: TObject);
begin
  SaveProperMotion(True);
end;

procedure TMainLocatEditFrame.PMMagEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in [#8, '0'..'9', DefaultFormatSettings.DecimalSeparator]) then Key := #0
  else if (Key = DefaultFormatSettings.DecimalSeparator) and (Pos(Key, PMMagEdit.Text) > 0) then begin
    Key := #0;
  end;
end;

procedure TMainLocatEditFrame.RadialVEditExit(Sender: TObject);
begin
  SaveRadialVel(True);
end;

procedure TMainLocatEditFrame.RadialVEditKeyPress(Sender: TObject; var Key: char  );
begin
  if not (Key in [#8, '0'..'9','+','-', DefaultFormatSettings.DecimalSeparator]) then Key := #0
  else if (Key in ['+','-', DefaultFormatSettings.DecimalSeparator]) and (Pos(Key, RadialVEdit.Text) > 0) then begin
    Key := #0;
  end;
end;

//=================================================================
// enable/disable widgets
//--------------------------------
procedure TMainLocatEditFrame.EnableLocation(doen:Boolean);
begin
  EpochPicker.Enabled := doen;
  RAEdit.Enabled := doen;
  DecEdit.Enabled := doen;
end;
//--------------------------------
procedure TMainLocatEditFrame.EnableParallax(doen:Boolean);
begin
  PllxEdit.Enabled := doen;
  PllxErrEdit.Enabled := doen;
  UncertainLocationCB.Enabled := doen;
  ParallaxSourceEdit.Enabled := doen;
end;
//--------------------------------
procedure TMainLocatEditFrame.EnableMotions(doen:Boolean);
begin
  PMMagEdit.Enabled := doen;
  AngleEdit.Enabled := doen;
  RadialVEdit.Enabled := doen;
end;
//--------------------------------
procedure TMainLocatEditFrame.EnableAll(doen:Boolean);
begin
  EnableLocation(doen);
  EnableParallax(doen);
  EnableMotions(doen);
  OldPllxEdit.Enabled := doen;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// load data
//--------------------------------
procedure TMainLocatEditFrame.LoadBlankLocation();
begin
  EpochPicker.ItemIndex := Ord(eJ2000);
  RAEdit.Text := '00 00 00.000';
  DecEdit.Text := '+00 00 00.00';
end;
//--------------------------------
procedure TMainLocatEditFrame.LoadBlankParallax();
begin
  PllxEdit.Text := ' 00.000';
  PllxErrEdit.Text:= ' 00.000';
  PllxEdit.Font.Color := clDefault;
  PllxErrEdit.Font.Color := clDefault;
  UncertainLocationCB.Checked := False;
  ParallaxSourceEdit.Text := '';
end;
//--------------------------------
procedure TMainLocatEditFrame.LoadBlankMotions();
begin
  PMMagEdit.Text := '';
  AngleEdit.Text := '000.00';
  RadialVEdit.Text :='' ;
end;
//--------------------------------
procedure TMainLocatEditFrame.LoadBlankAll();
begin
  LoadBlankLocation();
  LoadBlankParallax();
  LoadBlankMotions();
  OldPllxEdit.Text := '';
end;
//--------------------------------
function TMainLocatEditFrame.LoadLocation():Boolean;
begin
  Result := False;
  if LocData = nil then Exit;
  EpochPicker.ItemIndex := Ord(LocData.Epoch);
  RaEdit.Text := LocData.RightAscensionHMS;
  DecEdit.Text:= LocData.DeclinationDMS;
  Result := True;
end;
//--------------------------------
function TMainLocatEditFrame.LoadParallax():Boolean;
begin
  Result := False;
  if LocData = nil then Exit;
  PllxEdit.Text := LocData.GetParallaxString(3,True);
  PllxErrEdit.Text:= LocData.GetParallaxErrString(3,True);
  if LocData.IsACopy then begin
    PllxEdit.Font.Color := clRed;
    PllxErrEdit.Font.Color := clRed;
  end
  else begin
    PllxEdit.Font.Color := clDefault;
    PllxErrEdit.Font.Color := clDefault;
  end;
  UncertainLocationCB.Checked := LocData.uncertain;
  ParallaxSourceEdit.Text := LocData.source;
  Result := True;
end;
//--------------------------------
function TMainLocatEditFrame.LoadMotions():Boolean;
begin
  Result := False;
  if LocData = nil then Exit;
  PMMagEdit.Text := LocData.GetProperMotionMagStr(2);
  AngleEdit.Text := LocData.GetProperMotionAngleStr(2,True);
  RadialVEdit.Text := LocData.GetRadialVStr(2) ;
  Result := True;
end;
//--------------------------------
function TMainLocatEditFrame.LoadAll():Boolean;
begin
  Result := False;
  if LocData = nil then Exit;
  Result := LoadLocation();
  if not Result then Exit;
  Result := LoadParallax();
  if not Result then Exit;
  Result := LoadMotions();
  if not Result then Exit;
  OldPllxEdit.Text:= LocData.oldparallax;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// save data
//--------------------------------
function TMainLocatEditFrame.SaveLocation(showmsg:Boolean):Boolean;
var radata,decdata:string;
    newepoch:EpochType;
    eqtest:Boolean;
begin
  Result := False;
  if LocData = nil then begin
    if showmsg then ShowMessage('No object to save to!');
    Exit;
  end;
  // the main saving...
  newepoch := EpochType(EpochPicker.ItemIndex);
  radata := RAEdit.Text;
  decdata := DecEdit.Text;
  // checking against the old
  eqtest := (newepoch = LocData.Epoch) and (radata = LocData.RightAscensionHMS);
  if eqtest and (decdata = LocData.DeclinationDMS) then begin
     Result := True;
     RAEdit.Modified := False;
     DecEdit.Modified := False;
     Exit;
  end;
  // we go ahead and save
  eqtest := LocData.SetPositionHMS(newepoch,radata,decdata);
  if (not eqtest) then begin
    if showmsg then ShowMessage('Right ascension or declination are incorrect!');
    Exit;
  end;
  RAEdit.Modified := False;
  DecEdit.Modified := False;
  Result := True;

end;
//--------------------------------
function TMainLocatEditFrame.SaveParallax(showmsg:Boolean):Boolean;
var pllxdat,pllxerrdat:string;
    uncert,resv:Boolean;
begin
  Result := False;
  if LocData = nil then begin
    if showmsg then ShowMessage('No object to save to!');
    Exit;
  end;
  // gathering the data to be saved...
  pllxdat := PllxEdit.Text;
  pllxerrdat := PllxErrEdit.Text;
  // save only if different...
  if not LocData.CheckEqParallax(pllxdat,pllxerrdat) then begin
    resv := LocData.SetParallax(pllxdat,pllxerrdat);
    if not resv then begin
      if showmsg then ShowMessage('Parallax values are not valid!');
      Exit;
    end;
    PllxEdit.Font.Color := clDefault;
    PllxErrEdit.Font.Color := clDefault;
    PllxEdit.Modified := False;
    PllxErrEdit.Modified := False;
    if Assigned(ParallaxChangedHandler) then ParallaxChangedHandler(Self);
  end;
  // also, save the uncertain parallax check
  uncert := UncertainLocationCB.Checked;
  if LocData.uncertain <> uncert then LocData.uncertain := uncert;
  // done
  Result := True;
end;
//--------------------------------
function TMainLocatEditFrame.SaveProperMotion(showmsg:Boolean):Boolean;
var pmmagdat,pmangdat:string;
    isok:Boolean;
begin
  Result := False;
  if LocData = nil then begin
    if showmsg then ShowMessage('No object to save to!');
    Exit;
  end;
  // this is simpler, so I will not check to see if there is any change
  pmmagdat := Trim(PMMagEdit.Text);
  pmangdat := AngleEdit.Text;
  if pmmagdat = '' then pmmagdat :='0';
  // saving
  isok := LocData.SetProperMotion(pmmagdat,pmangdat);
  if not isok then begin
    if showmsg then ShowMessage('Proper motion values are not valid!');
    Exit;
  end;
  // done
  PMMagEdit.Modified := False;
  AngleEdit.Modified := False;
  Result := True;
end;
//--------------------------------
function TMainLocatEditFrame.SaveRadialVel(showmsg:Boolean):Boolean;
var rvstring:string;  rvval:Real;
    sc:Integer;
begin
  Result := False;
  if LocData = nil then begin
    if showmsg then ShowMessage('No object to save to!');
    Exit;
  end;
  // getting the value and converting it to a number
  rvstring := Trim(RadialVEdit.Text);
  if rvstring = '' then rvval := 0.0
  else begin
    Val(rvstring,rvval,sc);
    if sc<>0 then begin
      if showmsg then ShowMessage('Radial velocity is not a number!');
      Exit;
    end;
  end;
  // if we get here, we try to save it
  if not LocData.SetRadialVelocity(rvval) then begin
    if showmsg then ShowMessage('Radial velocity is out of bounds!');
    Exit;
  end;
  // done
  RadialVEdit.Modified := False;
  Result := True;
end;
//--------------------------------
procedure TMainLocatEditFrame.SaveParallaxSource;
begin
  if LocData <> nil then begin
    LocData.source := Trim(ParallaxSourceEdit.Text);
  end;
end;
//--------------------------------
procedure TMainLocatEditFrame.SaveOldParallax;
begin
  if LocData <> nil then begin
    LocData.oldparallax := Trim(OldPllxEdit.Text);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
{ public declarations }
//--------------------------------
procedure TMainLocatEditFrame.SetupStart(handler:TNotifyEvent);
begin
  EpochPicker.Items.AddStrings(EpochNames);
  LocData := nil;
  LoadBlankAll();
  EnableAll(False);
  ParallaxChangedHandler := handler;
end;
//--------------------------------
function TMainLocatEditFrame.ChangeToNothing:Boolean;
begin
  Result := (LocData<>nil);
  LocData := nil;
  EnableAll(True);
  LoadBlankAll();
  EnableAll(False);
end;

//--------------------------------
function TMainLocatEditFrame.ChangeLocation(newloc:Location):Boolean;
begin
  Result := False;
  if newloc = nil then ChangeToNothing
  else begin
    Result := True;
    // changing and loading...
    LocData := newloc;
    EnableAll(True);
    LoadAll;
  end;
end;
//--------------------------------
procedure TMainLocatEditFrame.Reload;
begin
  if LocData<>nil then LoadAll
end;
//--------------------------------
function TMainLocatEditFrame.SaveAll(showmsg:Boolean):Boolean;
begin
  Result := False;
  if LocData = nil then begin
    if showmsg then ShowMessage('No object to save to!');
    Exit;
  end;
    // saving the stuff...
  if not SaveLocation(showmsg) then Exit;
  if not SaveParallax(showmsg) then Exit;
  if not SaveProperMotion(showmsg) then Exit;
  if not SaveRadialVel(showmsg) then Exit;
  SaveParallaxSource();
  if Assigned(ParallaxChangedHandler) then ParallaxChangedHandler(Self);
  SaveOldParallax();
  Result := True;
end;

{$R *.lfm}

end.

