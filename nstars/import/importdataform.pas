unit ImportDataForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, MaskEdit, StrUtils,newImports,collecdata, df_strings, newImports2,
  newImports3;

type

  { TImportForm }

  TImportForm = class(TForm)
    PercentPCB: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    MaxPEEdit: TMaskEdit;
    NoSimbadMatching: TCheckBox;
    LoadDataButton: TButton;
    ImportPickerLabel: TLabel;
    ImportInfoLabel2: TLabel;
    CutoffDistEdit: TMaskEdit;
    OpenSourceDialog: TOpenDialog;
    StartButton: TButton;
    CancelButton: TButton;
    PauseResumeButton: TButton;
    ImportSourcePicker: TComboBox;
    SystemCountLabel: TLabel;
    ProgressLabel: TLabel;
    ImportInfoLabel1: TLabel;
    ImportProgressBar: TProgressBar;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportSourcePickerSelect(Sender: TObject);
    procedure LoadDataButtonClick(Sender: TObject);
    procedure PauseResumeButtonClick(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
  private
    { private declarations }
    // counts
    syscount,sysdone,sysindex:Integer;
    new_count,updated_count,skipped_count:Integer;
    // working data
    typeindex:Integer;
    dialog_filter:string;
    thedata:ImportedDataList;
    const_params:HandleImportParams;
    nosimb_match:Boolean;
    // status
    ispaused:Boolean;
    docancel:Boolean;
    working:Boolean;
    // the custom methods
    procedure ResetP;
    function LoadFromFile():Boolean;
    procedure UpdateProgressLabel;
    procedure DoCurrentStar;
    function DoMultipleStars:Boolean;
    procedure ImportDoneMessage();
    procedure WriteToFile(unused_only:Boolean);
    procedure WriteTest;
  public
    { public declarations }
  end;

const checkmsg = 'Checking all of the Stars in the source (via Simbad) could take several ';

var
  ImportForm: TImportForm;

const
  CSVFILTER = 'Comma Separated Values (*.csv)|*.csv';
  DATFILTER = 'CDS Table Data (*.dat)|*.dat';
  TXTFILTER = 'Text File (*.txt)|*.txt';
  TEXFILTER = 'TeX File (*.tex)|*.tex';

implementation

{$R *.lfm}

procedure TImportForm.FormActivate(Sender: TObject);
begin
  ResetP();
  syscount := primaryl.TotalCount;
  ispaused := False;
  working := False;
  ImportProgressBar.Max := 5;
  ImportProgressBar.Position := 0;
  SystemCountLabel.Caption := IntToStr(syscount) + ' Systems found to check.';
  StartButton.Enabled := False;
  PauseResumeButton.Enabled := False;
  UpdateProgressLabel;
  thedata := nil;
  typeindex := -1;
  CutoffDistEdit.Text := '032.62';
  const_params := HandleImportParams.Create;
  const_params.minpllx:=32.62;
  const_params.picksysn := True;
  const_params.maxpllxe :=15;
  const_params.xparams := nil;
  const_params.percentpllxe := False;
  ImportSourcePicker.ItemIndex := 0;
  ImportSourcePickerSelect(Self);
  ImportInfoLabel1.Caption := checkmsg + 'minutes.';
  nosimb_match := False;
end;

procedure TImportForm.CancelButtonClick(Sender: TObject);
begin
  if working then begin
    docancel := True;
  end;
  working := False;
  Close();
  thedata.free;
end;

procedure TImportForm.FormCreate(Sender: TObject);
begin

end;

procedure TImportForm.ImportSourcePickerSelect(Sender: TObject);
begin
  typeindex := ImportSourcePicker.ItemIndex;
  if typeindex < 0 then Exit;
  if typeindex >= ImportSourcePicker.Items.Count then Exit;
  // setting some variables based on index
  // carnegie southern astrometric planet search parallaxes
  if typeindex = 0 then begin
    // I converted the data to a semicolon delimited text file
    dialog_filter := TXTFILTER;
    const_params.xparams := carnegie_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // CTIO/SMARTS parallaxes from RECONS, cut and pasted from pdf with corrections
  // The Solar Neighborhood XXXVIII. Results from the CTIO/SMARTS 0.9m: Trigonometric Parallaxes for 151 Nearby M Dwarf Systems
  else if typeindex = 1 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := sn38_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // MEarths parallax data from thier website
  else if typeindex = 2 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := mearth_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // from the paper 'The Solar Neighborhood XXXII. The Hydrogen Burning Limit'
  else if typeindex = 3 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := sn32_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // from the Database of Ultracool Parallaxes
  else if typeindex = 4 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := ultracool_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // from URAT parallax Catalog
  else if typeindex = 5 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := urat_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // from the paper 'The Solar Neighborhood XXXV: Distances to 1404 M Dwarf Systems Within 25 pc in the Southern Sky'
  else if typeindex = 6 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := sn35_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // Hipparcos, the new reduction
  else if typeindex = 7 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := newhip_params;
    nosimb_match := True;
    NoSimbadMatching.Checked := True;
  end
  // Spectroscopically Identified White Dwarfs
  else if typeindex = 8 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := siwd_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // SN 35, Only using Table 2, with Photometric distance estimates
  else if typeindex = 9 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := sn35_2_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // Lépine and Gaidos' 'All-sky catalog of bright M dwarfs' (2011)
  else if typeindex = 10 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := lepgad_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // UCAC4 Nearby Star Search
  else if typeindex = 11 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := ucac4nss_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // Solar Neighborhood XXXIX
  else if typeindex = 12 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := sn39_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // GALEX M Dwarf Table 3
  else if typeindex = 13 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := galex3_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // CONCH-SHELL Big Table
  else if typeindex = 14 then begin
    dialog_filter := DATFILTER;
    const_params.xparams := conchshell_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // RAVE DR5 Extract from Vizier
  else if typeindex = 15 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := rave5_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // Parallaxes from Naval Observatory Flagstaff (Dahn+ 2017)
  else if typeindex = 16 then begin
    dialog_filter := TXTFILTER;
    const_params.xparams := nofs17_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  // URAT South Parallax Results (Finch+ 2018)
  else if typeindex = 17 then begin
    dialog_filter := TEXFILTER;
    const_params.xparams := urats_params;
    nosimb_match := False;
    NoSimbadMatching.Checked := False;
  end
  else Assert(False);
  StartButton.Enabled := False;
  CutoffDistEdit.Enabled := True;
  LoadDataButton.Enabled := True;
end;


procedure TImportForm.LoadDataButtonClick(Sender: TObject);
var limt,maxpe:string;
    isok:Boolean;
begin
  LoadFromFile;
  limt := CutoffDistEdit.Text;
  maxpe := MaxPEEdit.Text;
  isok := StrToRealBoth(limt,maxpe,const_params.minpllx,const_params.maxpllxe);
  Assert(isok);
  CutoffDistEdit.Enabled := False;
  MaxPEEdit.Enabled := False;
  UpdateProgressLabel;
end;

procedure TImportForm.PauseResumeButtonClick(Sender: TObject);
begin
  if ispaused then begin
    PauseResumeButton.Caption := 'Pause';
    ispaused := False;
    DoMultipleStars;
  end
  else begin
    PauseResumeButton.Caption := 'Resume';
    ispaused := True;
  end;
end;

procedure TImportForm.StartButtonClick(Sender: TObject);
begin
  (*
  LoadDataButton.Enabled := False;
  WriteTest();
  LoadDataButton.Enabled := True;
  *)
  // starting
  StartButton.Enabled := False;
  LoadDataButton.Enabled := False;
  PauseResumeButton.Enabled := True;
  ImportSourcePicker.Enabled := False;
  nosimb_match := NoSimbadMatching.Checked;
  const_params.percentpllxe := PercentPCB.Checked;
  PercentPCB.Enabled := False;
  // preparing the new file
  working := True;
  thedata.ResetIndex;
  // looping over the systems
  DoMultipleStars;

end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
// the custom methods
//-------------------------------------------------
procedure TImportForm.ResetP;
begin
  sysdone := 0;    sysindex := 0;
  docancel := False;
  ImportProgressBar.Position := 0;
  if thedata<>nil then FreeAndNil(thedata);
  syscount := 0;
  new_count := 0;
  updated_count := 0;
  skipped_count := 0;
  StartButton.Enabled:= False;
  PercentPCB.Enabled := True;
  ImportInfoLabel1.Caption := checkmsg + 'minutes.';
end;
//-------------------------------------------------
function TImportForm.LoadFromFile():Boolean;
var qfile:TFileName;
    readok:Boolean;
begin
  // starting off
  Result := False;
  ResetP();
  OpenSourceDialog.Filter := dialog_filter;
  if (not OpenSourceDialog.Execute) then Exit;
  // we have a file name to import
  qfile := OpenSourceDialog.FileName;
  // reading the file into the special data structure
  Screen.Cursor := crHourGlass;
  if thedata <> nil then FreeAndNil(thedata);
  thedata := ImportedDataList.Create(const_params.xparams);
  readok := thedata.LoadFromFile(qfile);
  Screen.Cursor := crDefault;
  // aftermath of reading the file...
  if (not readok) then begin
    ShowMessage('Unable to read anything from the file!');
    Exit;
  end
  else begin
    syscount := thedata.ImportedStarCount;
    SystemCountLabel.Caption:= IntToStr(syscount) +' stars to check.';
  end;
  //done
  ImportProgressBar.Max := syscount;
  if syscount >= 15000 then ImportInfoLabel1.Caption := checkmsg + 'hours.';
  StartButton.Enabled := True;
  Result := True;
end;
//-------------------------------------------------
procedure TImportForm.UpdateProgressLabel;
var realp:Real;
    percentstr,thelabel:String;
begin
  if syscount < 2 then thelabel := 'Not enough Stars to check.'
  else if sysdone = 0 then thelabel := 'Nothing checked yet.'
  else begin
    thelabel := IntToStr(sysdone) + ' of ' + IntToStr(syscount);
    thelabel += ' Stars (';
    realp := (sysdone/syscount)*100;
    percentstr := Trim(FloatToStrF(realp,ffFixed,6,1)) + '%';
    thelabel += percentstr + ') done. ';
    thelabel += IntToStr(new_count) + ' Systems added, ';
    thelabel += IntToStr(updated_count) + ' Systems updated.';
  end;
  ProgressLabel.Caption := thelabel;
end;
//-------------------------------------------------
procedure TImportForm.DoCurrentStar;
var currstar:ImportedData;
    changedone,newsys:Boolean;
begin
  currstar := thedata.CurrentStar;
  if nosimb_match then begin
    changedone := HandleNewImportedAlt(currstar,const_params,newsys);
  end else begin
    changedone := HandleNewImported(currstar,const_params,newsys);
  end;
  if changedone then begin
    if newsys then Inc(new_count)
    else Inc(updated_count);
  end
  else Inc(skipped_count);
  Inc(sysdone);
end;
//-------------------------------------------------
function TImportForm.DoMultipleStars:Boolean;
var finalmsg:string;
begin
  // looping over the systems
  while sysindex < syscount do begin
    thedata.NextStar;
    DoCurrentStar;
    ImportProgressBar.Position := sysdone;
    UpdateProgressLabel;
    Inc(sysindex);
    application.processmessages;
    if ispaused then Break;
    if docancel then Exit;
  end;

  // stuff do do when done
  if sysindex = syscount then begin
    working := False;
    // write tofile
    WriteToFile(True);
    StartButton.Enabled := True;
    PauseResumeButton.Enabled := False;
    ImportDoneMessage;
    Result := True;
  end
  else Result := False;
end;
//------------------------------------------------------
procedure TImportForm.ImportDoneMessage();
var outmsg:string;
begin
  outmsg := 'All done. ' + IntToStr(updated_count) + ' stars updated, ';
  outmsg += IntToStr(new_count) + ' new systems added,' + sLineBreak;
  outmsg += 'and ' + IntToStr(skipped_count) + ' stars skipped (see ';
  outmsg += const_params.xparams.leftout + ').';
  ShowMessage(outmsg);
end;
//-----------------------------------------------------
procedure TImportForm.WriteToFile(unused_only:Boolean);
var filen:TFileName;
begin
  Assert(thedata<>nil);
  Screen.Cursor := crHourGlass;
  // preparation for output
  filen := IfThen(unused_only,const_params.xparams.leftout,const_params.xparams.fullout);

  thedata.WriteToFile(filen,unused_only);
  Screen.Cursor := crDefault;
end;
//------------------------------------------------------------
procedure TImportForm.WriteTest();
var omsg:string;
begin
  omsg := 'The stars will be written to ' + const_params.xparams.fullout;
  omsg += ' as a Test.';
  ShowMessage(omsg);
  Screen.Cursor := crHourGlass;
  WriteToFile(False);
  Screen.Cursor := crDefault;
  ShowMessage('Done');
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++
begin
  ImportForm := nil;

end.

