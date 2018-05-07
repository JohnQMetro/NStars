unit gaiadr2match;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, MaskEdit, LMessages, dr2matchthread, collecdata, gaiadr2holder, Types;

type

  { TGaiaDR2Picker }

  TGaiaDR2Picker = class(TForm)
    AutoMatchCB: TCheckBox;
    AutoMatchVmagCB: TCheckBox;
    aseclbl: TLabel;
    AddCompMatchBtn: TButton;
    RejectObjBtn: TButton;
    StartMatchBtn: TButton;
    MaxDistLabel: TLabel;
    MaxDistEdit: TMaskEdit;
    UseOldCB: TCheckBox;
    SkipDR2CB: TCheckBox;
    SkipMatchCB: TCheckBox;
    SkipIgnoreCB: TCheckBox;
    MatchGroupBox: TGroupBox;
    MatchSelectedBtn: TButton;
    SkipStarBtn: TButton;
    StarInfo2Lbl: TLabel;
    Star2MatchLbl: TLabel;
    StarInfo1Lbl: TLabel;
    MatchCountLbl: TLabel;
    StarListGrid: TStringGrid;
    procedure AddCompMatchBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MatchSelectedBtnClick(Sender: TObject);
    procedure MaxDistEditKeyPress(Sender: TObject; var Key: char);
    procedure RejectObjBtnClick(Sender: TObject);
    procedure SkipStarBtnClick(Sender: TObject);
    // procedure StarListGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure StarListGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure StartMatchBtnClick(Sender: TObject);
  private
    mparams:DR2MatchParams;
    dataToCheck:DR2MatchData;
    mthread:MatchToDR2Thread;
    thread_is_active:Boolean;
    starsmatched:Integer;
    syscount:Integer;

    // message handlers
    procedure HandleThreadStart(var Msg:TLMessage); message MSG_IOSTART;
    procedure HandleCannotStart(var Msg:TLMessage); message MSG_CANNOTSTART;
    procedure HandleStarMatched(var Msg:TLMessage); message MSG_STARMATCHED;
    procedure HandleCheckMatch(var Msg:TLMessage); message MSG_CHECKMATCH;
    procedure HandleDone(var Msg:TLMessage); message MSG_DONE;

    procedure LoadMatchParams();
    procedure SaveMatchParams();
    procedure EnableMatchParams(enabled:Boolean);
    function CreateAndStartThread():Boolean;
    procedure ThreadIsOver();
    procedure EnableButtons(enabled:Boolean);
    procedure SetMatchCountLabel();
    procedure StickStarsInGrid(starlist:GaiaList);
  public

  end;

var
  GaiaDR2Picker: TGaiaDR2Picker;
  sysstart:DWord;

implementation

{$R *.lfm}

{ TGaiaDR2Picker }

procedure TGaiaDR2Picker.MaxDistEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := MaxDistEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '0','1','2','3'])) then Key := #0;
end;

procedure TGaiaDR2Picker.RejectObjBtnClick(Sender: TObject);
var rcount:Integer;
begin
  // mark the object as permaReject and remove it from the list object
  rcount := dataToCheck.RejectObject(StarListGrid.Row);
  StarListGrid.Row := -1;
  // if there is nothing left, we move on, like a skip
  if (rcount = 0) then SkipStarBtnClick(Sender)
  // otherwise we reload the list box of stars
  else StickStarsInGrid(dataToCheck.matches);
end;

procedure TGaiaDR2Picker.SkipStarBtnClick(Sender: TObject);
begin
  EnableButtons(False);
  FreeAndNil(dataToCheck);
  StarListGrid.Row := -1;
  StarListGrid.RowCount := 0;
  mthread.WaitIsOver(False);
end;
//------------------------------------------------------
procedure TGaiaDR2Picker.StarListGridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  ATextStyle: TTextStyle;
begin
  ATextStyle := StarListGrid.Canvas.TextStyle;
  ATextStyle.SingleLine := false;
  ATextStyle.Wordbreak := true;
  StarListGrid.Canvas.TextStyle := ATextStyle;
end;
//------------------------------------------------------
procedure TGaiaDR2Picker.MatchSelectedBtnClick(Sender: TObject);
begin
  if StarListGrid.Row >= 0 then begin
    EnableButtons(False);
    dataToCheck.ApplyMatch(StarListGrid.Row,mparams.match_conditional);
    Inc(starsmatched);
    SetMatchCountLabel();
    FreeAndNil(dataToCheck);
    StarListGrid.Row := -1;
    StarListGrid.RowCount := 0;
    mthread.WaitIsOver(False);
  end;
end;
//-----------------------------------------------------------
procedure TGaiaDR2Picker.FormActivate(Sender: TObject);
begin
  InitDR2MatchParam(mparams,True);
  dataToCheck := nil;
  mthread := nil;
  thread_is_active := False;
  starsmatched := 0;
  syscount := 0;
end;

procedure TGaiaDR2Picker.AddCompMatchBtnClick(Sender: TObject);
begin
  dataToCheck.system.AddNewSepLocation();
end;

//------------------------------------------------
procedure TGaiaDR2Picker.StartMatchBtnClick(Sender: TObject);
begin
  if not thread_is_active then CreateAndStartThread()
  else begin
    StartMatchBtn.Enabled := False;
    mthread.WaitIsOver(True);
    EnableButtons(False);
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// message handlers
//-----------------------------------------
procedure TGaiaDR2Picker.HandleThreadStart(var Msg:TLMessage);
begin
  SetMatchCountLabel();
end;
//-----------------------------------------
procedure TGaiaDR2Picker.HandleCannotStart(var Msg:TLMessage);
var postr:PString;
    errmsg, outmsg:string;
begin
  postr := PString(Msg.wParam);
  errmsg := postr^;
  outmsg := 'FAILED: ' + errmsg;
  MatchCountLbl.Caption := outmsg;
  ThreadIsOver();
end;
//-----------------------------------------
procedure TGaiaDR2Picker.HandleStarMatched(var Msg:TLMessage);
begin
  Inc(starsmatched);
  SetMatchCountLabel();
end;
//-----------------------------------------
procedure TGaiaDR2Picker.HandleCheckMatch(var Msg:TLMessage);
begin
  dataToCheck := DR2MatchData(Msg.wParam);
  StickStarsInGrid(dataToCheck.matches);
  StarInfo1Lbl.Caption := dataToCheck.system.StarSummaryWhat(dataToCheck.stardex);
  StarInfo2Lbl.Caption := dataToCheck.system.StarSummaryIDPos(dataToCheck.stardex);
  EnableButtons(True);
end;
//-----------------------------------------
procedure TGaiaDR2Picker.HandleDone(var Msg:TLMessage);
begin
  ThreadIsOver();
  SetMatchCountLabel();
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaDR2Picker.LoadMatchParams();
var mstring:string;
begin
  SkipDR2CB.Checked := mparams.skip_dr2;
  SkipMatchCB.Checked := mparams.skip_matched;
  SkipIgnoreCB.Checked := mparams.skip_reject;
  UseOldCB.Checked := mparams.match_conditional;
  AutoMatchCB.Checked := mparams.auto_match_good;
  AutoMatchVmagCB.Checked := mparams.auto_match_v;
  mstring := Trim(FloatToStrF(mparams.max_search_dist,ffFixed,4,2));
  MaxDistEdit.Text := mstring;
end;
//-----------------------------------------
procedure TGaiaDR2Picker.SaveMatchParams();
var mstring:string;
begin
  mparams.skip_dr2 := SkipDR2CB.Checked;
  mparams.skip_matched := SkipMatchCB.Checked;
  mparams.skip_reject := SkipIgnoreCB.Checked;
  mparams.match_conditional := UseOldCB.Checked;
  mparams.auto_match_good := AutoMatchCB.Checked;
  mparams.auto_match_v := AutoMatchVmagCB.Checked;
  mstring := MaxDistEdit.Text;
  if not TryStrToFloat(mstring,mparams.max_search_dist) then begin
    ShowMessage('Cannot parse maximum distance, using 0.3 instead.');
    mparams.max_search_dist:= 0.3;
    MaxDistEdit.Text:='0.30';
  end;
end;
//-----------------------------------------
procedure TGaiaDR2Picker.EnableMatchParams(enabled:Boolean);
begin
  SkipDR2CB.Enabled := enabled;
  SkipMatchCB.Enabled := enabled;
  SkipIgnoreCB.Enabled := enabled;
  UseOldCB.Enabled := enabled;
  AutoMatchCB.Enabled := enabled;
  AutoMatchVmagCB.Enabled := enabled;
  MaxDistEdit.Enabled := enabled;
end;
//-----------------------------------------
function TGaiaDR2Picker.CreateAndStartThread():Boolean;
begin
  Result := False;
  if thread_is_active then Exit;
  // properly starting the thread...
  StartMatchBtn.Enabled:= False;
  starsmatched := 0;
  syscount := primaryl.GetCount - sysstart;
  SaveMatchParams();
  EnableMatchParams(False);
  mthread := MatchToDR2Thread.Create(True,mparams,sysstart,Handle);
  thread_is_active := True;
  StartMatchBtn.Caption := 'Stop Matching';
  mthread.Start;
end;
//-----------------------------------------------
procedure TGaiaDR2Picker.ThreadIsOver();
begin
  thread_is_active := False;
  EnableMatchParams(True);
  StartMatchBtn.Caption := 'Start Matching';
  StartMatchBtn.Enabled := True;
  mthread := nil;
end;
//-----------------------------------------------
procedure TGaiaDR2Picker.EnableButtons(enabled:Boolean);
begin
  StartMatchBtn.Enabled := enabled;
  MatchSelectedBtn.Enabled := enabled;
  SkipStarBtn.Enabled := enabled;
  RejectObjBtn.Enabled := enabled;
  AddCompMatchBtn.Enabled := enabled;
end;
//---------------------------------------------------
procedure TGaiaDR2Picker.SetMatchCountLabel();
begin
  if (not thread_is_active) and (starsmatched = 0) then begin
    MatchCountLbl.Caption := 'Thread not started';
  end else if (not thread_is_active) then begin
    MatchCountLbl.Caption := 'Thread over. ' + IntToStr(starsmatched) + ' stars matched.';
  end else if thread_is_active then begin
    if starsmatched = 0 then MatchCountLbl.Caption := 'Thread Started'
    else MatchCountLbl.Caption := IntToStr(starsmatched) + ' stars matched.';
  end;
end;
//---------------------------------------------------
procedure TGaiaDR2Picker.StickStarsInGrid(starlist:GaiaList);
var rowdex:Integer;
begin
  StarListGrid.RowCount := starlist.Count;
  for rowdex := 0 to (starlist.Count -1) do begin
      StarListGrid.Cells[0,rowdex] := starlist[rowdex].MakeSummaryString();
  end;
  StarListGrid.Row := 0;
end;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  sysstart := 1;
  GaiaDR2Picker := nil;
end.

