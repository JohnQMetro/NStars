unit starlocatedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, MaskEdit, ExtCtrls,
  Dialogs, Graphics, newlocation, Utilities2;

type

  { TStarLocatFrame }

  TStarLocatFrame = class(TFrame)
    EpochPicker: TComboBox;
    EpochLabel: TLabel;
    DecEdit: TMaskEdit;
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
    procedure FrameClick(Sender: TObject);
    procedure ParallaxEditExit(Sender: TObject);
    procedure PErrorEditExit(Sender: TObject);
    procedure RAEditExit(Sender: TObject);
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

procedure TStarLocatFrame.DecEditExit(Sender: TObject);
begin
  SaveDeclination(True);
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

