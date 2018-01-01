unit PPMMatchForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, ComCtrls,
  df_strings,collecdata;

type

  { TPositionProperMotionMatchForm }

  TPositionProperMotionMatchForm = class(TForm)
    CancelButton: TButton;
    CheckInfoLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MMagAmEdit: TMaskEdit;
    MMagPcEdit: TMaskEdit;
    MPMAngDiffEdit: TMaskEdit;
    MaxSeparationEdit: TMaskEdit;
    MaxDiffGroupBox: TGroupBox;
    MatchProgressBar: TProgressBar;
    OKButton: TButton;
    PauseResumeBtn: TButton;
    ProgressLabel: TLabel;
    SystemCountLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PauseResumeBtnClick(Sender: TObject);
  private
    { private declarations }
    syscount,sysdone,sysindex:Integer;
    matchesfound:Integer;
    maxdiff,angdiff,magperc,magdiff:Real;
    outfile:TextFile;
    outfilename:TFilename;
    ispaused:Boolean;
    docancel:Boolean;
    working:Boolean;
    // the custom methods
    procedure ResetP;
    procedure EnableInputEntry(doit:Boolean);
    function SaveInputs:Boolean;
    procedure UpdateProgressLabel;
    procedure DoCurrentSystem;
    function DoMultipleSystems:Boolean;
  public
    { public declarations }

  end;

var
  PositionProperMotionMatchForm: TPositionProperMotionMatchForm;

implementation

{$R *.lfm}

{ TPositionProperMotionMatchForm }

procedure TPositionProperMotionMatchForm.FormCreate(Sender: TObject);
begin

end;

procedure TPositionProperMotionMatchForm.OKButtonClick(Sender: TObject);
begin
  ResetP;
  // starting
  OkButton.Enabled := False;
  PauseResumeBtn.Enabled := True;
  // converting the founds inputs
  if not SaveInputs then begin
    ShowMessage('Could not convert the Maximum differences!');
    OkButton.Enabled := True;
    PauseResumeBtn.Enabled := False;
    Exit;
  end;
  EnableInputEntry(False);
  // preparing the new file
  working := True;
  AssignFile(outfile,outfilename);
  Rewrite(outfile);
  // looping over the systems
  DoMultipleSystems;
end;

procedure TPositionProperMotionMatchForm.PauseResumeBtnClick(Sender: TObject);
begin
  if ispaused then begin
    PauseResumeBtn.Caption := 'Pause';
    ispaused := False;
    DoMultipleSystems;
  end
  else begin
    PauseResumeBtn.Caption := 'Resume';
    ispaused := True;
  end;
end;

procedure TPositionProperMotionMatchForm.FormActivate(Sender: TObject);
begin
  syscount := primaryl.TotalCount;
  outfilename := 'ppmmatch_report.txt';
  sysdone := 0;
  matchesfound := 0;
  ispaused := False;
  working := False;
  MatchProgressBar.Max := syscount;
  SystemCountLabel.Caption := IntToStr(syscount) + ' Systems found to check.';
  PauseResumeBtn.Enabled := False;
  UpdateProgressLabel;
end;

procedure TPositionProperMotionMatchForm.CancelButtonClick(Sender: TObject);
begin
  if working then begin
    Flush(outfile);
    CloseFile(outfile);
    docancel := True;
  end;
  working := False;
  Close();
end;
//================================================================
// the custom methods
//--------------------------------
procedure TPositionProperMotionMatchForm.ResetP;
begin
  sysdone := 0;
  matchesfound := 0;
  sysindex := 1;
  docancel := False;
  MatchProgressBar.Position := 1;
end;
//--------------------------------
procedure TPositionProperMotionMatchForm.EnableInputEntry(doit:Boolean);
begin
  MPMAngDiffEdit.Enabled := doit;
  MMagPcEdit.Enabled := doit;
  MMagAmEdit.Enabled := doit;
  MaxSeparationEdit.Enabled := doit;
end;
//--------------------------------
function TPositionProperMotionMatchForm.SaveInputs:Boolean;
var text_ang,text_magp,text_maga,text_sep:string;
begin
  // getting the contents of the edit boxes
  text_ang := MPMAngDiffEdit.Text;
  text_magp := MMagPcEdit.Text;
  text_maga := MMagAmEdit.Text;
  text_sep := MaxSeparationEdit.Text;
  // converting them
  Result := False;
  if not StrToReal(text_ang,angdiff) then Exit;
  if not StrToReal(text_magp,magperc) then Exit;
  if not StrToReal(text_maga,magdiff) then Exit;
  if not StrToReal(text_sep,maxdiff) then Exit;
  // done
  Result := True;
end;
//--------------------------------
procedure TPositionProperMotionMatchForm.UpdateProgressLabel;
var realp:Real;
    percentstr,thelabel:String;
begin
  if syscount < 2 then thelabel := 'Not enough Systems to check.'
  else if sysdone = 0 then thelabel := 'Nothing checked yet.'
  else begin
    thelabel := IntToStr(sysdone) + ' of ' + IntToStr(syscount);
    thelabel += ' Systems (';
    realp := (sysdone/syscount)*100;
    percentstr := Trim(FloatToStrF(realp,ffFixed,6,1)) + '%';
    thelabel += percentstr + ') done. ';
    thelabel += IntToStr(matchesfound) + ' matches found.';
  end;
  ProgressLabel.Caption := thelabel;
end;
//--------------------------------
procedure TPositionProperMotionMatchForm.DoCurrentSystem;
var curr_matches:Integer;
    curroutstr:string;
begin
  curr_matches := primaryl.FindPosPMMatch(sysindex,maxdiff,angdiff,magperc,magdiff,curroutstr);
  if curr_matches > 0 then begin
    matchesfound += curr_matches;
    curroutstr := Trim(curroutstr);
    WriteLn(outfile,curroutstr);
    WriteLn(outfile,'++++++++++++++++++++++++++++++++++++++++++++++++++++++');
  end;
  Inc(sysdone);
end;
//--------------------------------
function TPositionProperMotionMatchForm.DoMultipleSystems:Boolean;
var finalmsg:string;
begin
  // looping over the systems
  while sysindex <= (syscount-2) do begin
    DoCurrentSystem;
    MatchProgressBar.Position := sysdone;
    UpdateProgressLabel;
    Inc(sysindex);
    application.processmessages;
    if ispaused then Break;
    if docancel then Exit;
  end;
  // we may or may not be done after the loop
  Flush(outfile);
  // stuff do do when done
  if sysindex = (syscount-1) then begin
    working := False;
    CloseFile(outfile);
    finalmsg := 'Done. The results have been written to ' + outfilename;
    Inc(sysdone);
    MatchProgressBar.Position := sysdone;
    OkButton.Enabled := True;
    PauseResumeBtn.Enabled := False;
    ShowMessage(finalmsg);
    EnableInputEntry(True);
    Result := True;
  end
  else Result := False;
end;

begin
  PositionProperMotionMatchForm := nil;
end.

