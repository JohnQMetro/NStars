unit FindDuplInterF;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls,
  collecdata;

type

  { TFindCatDuplForm }

  TFindCatDuplForm = class(TForm)
    PauseResumeBtn: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    SystemCountLabel: TLabel;
    CheckInfoLabel: TLabel;
    ProgressLabel: TLabel;
    SystemProgressBar: TProgressBar;
    procedure CancelButtonClick(Sender: TObject);
    procedure PauseResumeBtnClick(Sender: TObject);
    procedure ResetP;
    procedure FormActivate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    syscount,sysdone,sysindex:Integer;
    dupsfound:Integer;
    outfile:TextFile;
    outfilename:TFilename;
    ispaused:Boolean;
    docancel:Boolean;
    working:Boolean;
    // the custom methods
    procedure UpdateProgressLabel;
    procedure DoCurrentSystem;
    function DoMultipleSystems:Boolean;
  public
    { public declarations }
  end;

var
  FindCatDuplForm: TFindCatDuplForm;

implementation

{$R *.lfm}

procedure TFindCatDuplForm.FormActivate(Sender: TObject);
begin
  syscount := primaryl.TotalCount;
  outfilename := 'duplicate_report.txt';
  sysdone := 0;
  dupsfound := 0;
  ispaused := False;
  working := False;
  SystemProgressBar.Max := syscount;
  SystemCountLabel.Caption := IntToStr(syscount) + ' Systems found to check.';
  PauseResumeBtn.Enabled := False;
  UpdateProgressLabel;
end;

procedure TFindCatDuplForm.OKButtonClick(Sender: TObject);
begin
  ResetP;
  // starting
  OkButton.Enabled := False;
  PauseResumeBtn.Enabled := True;
  // preparing the new file
  working := True;
  AssignFile(outfile,outfilename);
  Rewrite(outfile);
  // looping over the systems
  DoMultipleSystems;

end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TFindCatDuplForm.ResetP;
begin
  sysdone := 0;
  dupsfound := 0;
  sysindex := 1;
  docancel := False;
  SystemProgressBar.Position := 1;
end;

procedure TFindCatDuplForm.PauseResumeBtnClick(Sender: TObject);
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

procedure TFindCatDuplForm.CancelButtonClick(Sender: TObject);
begin
  if working then begin
    Flush(outfile);
    CloseFile(outfile);
    docancel := True;
  end;
  working := False;
  Close();
end;

//----------------------------------------------
procedure TFindCatDuplForm.UpdateProgressLabel;
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
    thelabel += IntToStr(dupsfound) + ' duplicates found.';
  end;
  ProgressLabel.Caption := thelabel;
end;

//--------------------------------------------------
procedure TFindCatDuplForm.DoCurrentSystem;
var currdupes:Integer;
    curroutstr:string;
begin
  currdupes := primaryl.FindStarIDMatches(sysindex,curroutstr);
  if currdupes > 0 then begin
    dupsfound += currdupes;
    curroutstr := Trim(curroutstr);
    WriteLn(outfile,curroutstr);
    WriteLn(outfile,'++++++++++++++++++++++++++++++++++++++++++++++++++++++');
  end;
  Inc(sysdone);
end;
//---------------------------------------------------------
function TFindCatDuplForm.DoMultipleSystems:Boolean;
var finalmsg:string;
    qcount:Integer;
begin
  // looping over the systems
  qcount := 0;
  while sysindex <= (syscount-2) do begin
    DoCurrentSystem;
    Inc(qcount);
    if qcount = 10 then begin
      SystemProgressBar.Position := sysdone;
      UpdateProgressLabel;
      qcount := 0;
    end;
    Inc(sysindex);
    application.processmessages;
    if ispaused then Break;
    if docancel then Exit;
  end;
  if (qcount <> 0) then begin
    SystemProgressBar.Position := sysdone;
    UpdateProgressLabel;
  end;
  // we may or may not be done after the loop
  Flush(outfile);
  // stuff do do when done
  if sysindex = (syscount-1) then begin
    working := False;
    CloseFile(outfile);
    finalmsg := 'Done. The results have been written to ' + outfilename;
    Inc(sysdone);
    SystemProgressBar.Position := sysdone;
    OkButton.Enabled := True;
    PauseResumeBtn.Enabled := False;
    ShowMessage(finalmsg);
    Result := True;
  end
  else Result := False;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++
begin
  FindCatDuplForm := nil;
end.

