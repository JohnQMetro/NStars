unit starlocatedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, ExtCtrls,
  Dialogs, Graphics, Menus, newlocation, Utilities2, df_strings;

type

  { TStarLocatFrame }

  TStarLocatFrame = class(TFrame)
    EpochPicker: TComboBox;
    EpochLabel: TLabel;
    DecEdit: TMaskEdit;
    EnterDegPosMI: TMenuItem;
    EnterPllxSourceMI: TMenuItem;
    EnterMPCompMI: TMenuItem;
    ShowPllSrcMI: TMenuItem;
    ShowPMMi: TMenuItem;
    SmallLocatMenu: TPopupMenu;
    PPMLabel: TLabel;
    PErrorEdit: TMaskEdit;
    PErrorLabel: TLabel;
    ParallaxEdit: TMaskEdit;
    ParallaxLabel: TLabel;
    RALabel: TLabel;
    DecLabel: TLabel;
    RAEdit: TMaskEdit;
    StarLocatGB: TGroupBox;
    procedure DecEditExit(Sender: TObject);
    procedure EnterDegPosMIClick(Sender: TObject);
    procedure EnterMPCompMIClick(Sender: TObject);
    procedure EnterPllxSourceMIClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure ParallaxEditExit(Sender: TObject);
    procedure PErrorEditExit(Sender: TObject);
    procedure RAEditExit(Sender: TObject);
    procedure ShowPllSrcMIClick(Sender: TObject);
    procedure ShowPMMiClick(Sender: TObject);
    procedure SmallLocatMenuPopup(Sender: TObject);
    procedure StarLocatGBClick(Sender: TObject);
  private
    { private declarations }
    LocationShown:Location;
    ParallaxChangedHandler:TNotifyEvent;
    function SavePosition:Boolean;
    function SaveRightAscension(showmsg:Boolean):Boolean;
    function SaveDeclination(showmsg:Boolean):Boolean;
    function SaveParallax(showmsg:Boolean):Boolean;
  public
    { public declarations }
    function SaveData(showmsg:Boolean):Boolean;
    function ReloadData:Boolean;
    procedure ClearEdits;
    function ChangeLocation(inloc:Location):Boolean;
    procedure Setup(handler:TNotifyEvent);
  end;

implementation

{$R *.lfm}

{ TStarLocatFrame }

procedure TStarLocatFrame.FrameClick(Sender: TObject);
begin
  SaveData(True);
end;

procedure TStarLocatFrame.ParallaxEditExit(Sender: TObject);
begin
  SaveParallax(True);
end;

procedure TStarLocatFrame.PErrorEditExit(Sender: TObject);
begin
  SaveParallax(True);
end;

procedure TStarLocatFrame.RAEditExit(Sender: TObject);
begin
  SaveRightAscension(True);
end;

procedure TStarLocatFrame.ShowPllSrcMIClick(Sender: TObject);
begin
  if LocationShown <> nil then begin
    ShowMessage('Parallax Source: ' + LocationShown.source)
  end;
end;

procedure TStarLocatFrame.ShowPMMiClick(Sender: TObject);
var pmstr:string;
begin
  if LocationShown <> nil then begin
    pmstr := 'Proper Motion: ';
    pmstr += Trim(FloatToStrF(LocationShown.ProperMotionMagnitude,ffFixed,7,1));
    pmstr += ' mas/yr at ';
    pmstr += Trim(FloatToStrF(LocationShown.ProperMotionAngle,ffFixed,5,1)) + '°';
    ShowMessage('Proper Motion: ' + pmstr);
  end;
end;

procedure TStarLocatFrame.SmallLocatMenuPopup(Sender: TObject);
var lshown,nbin:Boolean;
begin
  lshown := (LocationShown <> nil);
  nbin := lshown and (not LocationShown.IsACopy);
  EnterDegPosMI.Enabled := lshown;
  EnterPllxSourceMI.Enabled := nbin;
  EnterMPCompMI.Enabled := nbin;
  ShowPllSrcMI.Enabled := lshown;
  ShowPMMi.Enabled := lshown;
end;

procedure TStarLocatFrame.DecEditExit(Sender: TObject);
begin
  SaveDeclination(True);
end;

procedure TStarLocatFrame.EnterDegPosMIClick(Sender: TObject);
var datax,data1,data2:string;   rok:Boolean;
    pmra,pmdec,pmmag,pmang:Real;
    eptype:EpochType;
const entmsg = 'Enter RA and Dec (in degrees, separated by a space) below:';
begin
  if LocationShown <> nil then begin
    datax := InputBox('Position Entry',entmsg,'');
    rok := ExtractLocParts(datax,data1,data2);
    if rok then begin
      eptype := LocationShown.Epoch;
      rok := LocationShown.SetPositionDDeg(eptype,data1,data2);
      if rok then ReloadData();
    end;
    if rok then ShowMessage('Position set.')
    else ShowMessage('Position not set!');
  end;
end;

procedure TStarLocatFrame.EnterMPCompMIClick(Sender: TObject);
var data1,data2:string;   rok:Boolean;
    pmra,pmdec,pmmag,pmang:Real;
const entmsg = 'Enter RA and Dec Proper Motions components (in'+#13#10
  +'milli-arcsec, separated by a space) below:';
begin
  if LocationShown <> nil then begin
    data2 := Trim(InputBox('PM Entry',entmsg,''));
    rok := ExtractFirstWord(data2,data1);
    if rok then begin
      rok := StrToRealBoth(data1,data2,pmra,pmdec);
      if rok then begin
        ProperMotionConvert(pmdec,pmra,pmmag,pmang);
        rok := LocationShown.SetProperMotion(pmmag,pmang);
      end;
    end;
    if rok then ShowMessage('Proper Motion set.')
    else ShowMessage('Proper Motion not set!');
  end;
end;

procedure TStarLocatFrame.EnterPllxSourceMIClick(Sender: TObject);
var data:string;
const entmsg = 'Enter the Parallax Source below:';
begin
  if LocationShown <> nil then begin
    data := Trim(InputBox('Source Entry',entmsg,LocationShown.source));
    if Length(data) <> 0 then LocationShown.source := data
    else ShowMessage('Empty source not accepted.');
  end;
end;

procedure TStarLocatFrame.StarLocatGBClick(Sender: TObject);
begin
  SaveData(True);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TStarLocatFrame.SavePosition:Boolean;
var rastring,decstring:string;
    epochdex:Integer;
begin
  Result := False;
  if LocationShown = nil then Exit;
  epochdex := EpochPicker.ItemIndex;
  if epochdex < 0 then Exit;
  rastring := RAEdit.Text;
  decstring := DecEdit.Text;
  RAEdit.Modified := False;
  DecEdit.Modified := False;
  Result := LocationShown.SetPositionHMS(EpochType(epochdex),rastring,decstring);
end;
///---------------------------------------------------------
function TStarLocatFrame.SaveRightAscension(showmsg:Boolean):Boolean;
begin
  Result := SavePosition;
  if (not Result) then begin
    if showmsg then ShowMessage('Right Ascension is invalid!');
  end;
end;
//------------------------------------
function TStarLocatFrame.SaveDeclination(showmsg:Boolean):Boolean;
begin
  Result := SavePosition;
  if (not Result) then begin
    if showmsg then ShowMessage('Declination is invalid!');
  end;
  Result := False;
end;
//------------------------------------
function TStarLocatFrame.SaveParallax(showmsg:Boolean):Boolean;
var datastr1,datastr2:string;   xres:Boolean;
begin
  Result := False;
  if LocationShown = nil then Exit;
  datastr1 := ParallaxEdit.Text;
  ParallaxEdit.Modified := False;
  datastr2 := PErrorEdit.Text;
  PerrorEdit.Modified := False;
  xres := LocationShown.SetParallax(datastr1,datastr2);
  if (not xres) then begin
    if showmsg then ShowMessage('Parallax is invalid!');
    Exit;
  end;
  Result := True;
  LocationShown.uncertain := (LocationShown.ParallaxErrorMAS < 4);
  ParallaxEdit.Font.Color:= clDefault;
  PerrorEdit.Font.Color:= clDefault;
  if Assigned(ParallaxChangedHandler) then ParallaxChangedHandler(Self);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TStarLocatFrame.SaveData(showmsg:Boolean):Boolean;
begin
  Result := False;
  if not SaveRightAscension(showmsg) then Exit;
  if not SaveDeclination(showmsg) then Exit;
  if not SaveParallax(showmsg) then Exit;
  Result := True;
end;
//--------------------------------------------
function TStarLocatFrame.ReloadData:Boolean;
begin
  Result := False;
  if LocationShown <> nil then begin
    // epoch
    EpochPicker.ItemIndex:= Ord(LocationShown.Epoch);
    // position
    RAEdit.Text:= LocationShown.RightAscensionHMS;
    DecEdit.Text := LocationShown.DeclinationDMS;
    // parallax
    ParallaxEdit.Text := LocationShown.GetParallaxString(3,True);
    PerrorEdit.Text := LocationShown.GetParallaxErrString(3,True);
    if LocationShown.IsACopy then begin
      ParallaxEdit.Font.Color:= clRed;
      PerrorEdit.Font.Color:= clRed;
    end
    else begin
      ParallaxEdit.Font.Color:= clDefault;
      PerrorEdit.Font.Color:= clDefault;
    end;
    // done
    Result := True;
  end;
end;
//--------------------------------------------
procedure TStarLocatFrame.ClearEdits;
begin
  EpochPicker.ItemIndex := Ord(eJ2000);
  RAEdit.Text:= '00 00 00.000';
  DecEdit.Text := '+00 00 00.00';
  ParallaxEdit.Text := ' 00.000';
  PerrorEdit.Text := ' 00.000';
  ParallaxEdit.Font.Color:= clDefault;
  PerrorEdit.Font.Color:= clDefault;
end;
//--------------------------------------------
function TStarLocatFrame.ChangeLocation(inloc:Location):Boolean;
begin
  Result := True;
  if inloc = nil then begin
    LocationShown := nil;
    ClearEdits;
  end
  else begin
    LocationShown := inloc;
    Result := ReloadData;
  end;
end;
//--------------------------------------------
procedure TStarLocatFrame.Setup(handler:TNotifyEvent);
begin
  LocationShown := nil;
  ClearEdits;
  ParallaxChangedHandler := handler;
  EpochPicker.Items.AddStrings(EpochNames);
end;

end.

