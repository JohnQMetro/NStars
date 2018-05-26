unit gaiadr2add;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LMessages,
  StdCtrls, ExtCtrls, MaskEdit, dr2addnew, gaiadr2base;

type

  { TGaiaDR2AddForm }

  TGaiaDR2AddForm = class(TForm)
    AddSettingsBox: TGroupBox;
    AddCountLabel: TLabel;
    AddInfoBox: TGroupBox;
    AddStarButton: TButton;
    AutoRqjLabel: TLabel;
    maslbl3: TLabel;
    AutoRejEdit: TMaskEdit;
    Viz2MCB: TCheckBox;
    DownSimCB: TCheckBox;
    SkipButton: TButton;
    RejectButton: TButton;
    ObjInfoLabel: TLabel;
    StartStopButton: TButton;
    SelACB: TCheckBox;
    SelCCB: TCheckBox;
    IgnoreRejectCB: TCheckBox;
    AutAddParBox: TGroupBox;
    AlertDoLbl: TLabel;
    maslbl2: TLabel;
    MaxParallaxErrLabel: TLabel;
    MinGMagLabel: TLabel;
    maslbl: TLabel;
    MinPllxLbl: TLabel;
    MinPllxEdit: TMaskEdit;
    GMagEdit: TMaskEdit;
    MaxPllxErrEdit: TMaskEdit;
    procedure AddStarButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RejectButtonClick(Sender: TObject);
    procedure SkipButtonClick(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
  private
    addedcount:Integer;
    mthread:AddFromDR2Thread;
    sett:DR2AddSettings;
    autoadd:DR2AutoAddSettings;
    checkobj:DR2AddBundle;
    thread_is_active:Boolean;

    // message handlers
    procedure HandleThreadStart(var Msg:TLMessage); message MSG_IOSTART;
    procedure HandleCannotStart(var Msg:TLMessage); message MSG_CANNOTSTART;
    procedure HandleStarAdded(var Msg:TLMessage); message MSG_STARADDED;
    procedure HandleCheckAdd(var Msg:TLMessage); message MSG_CHECKADD;
    procedure HandleDone(var Msg:TLMessage); message MSG_DONE;

    // other procedures
    procedure EnableParams(doEnable:Boolean);
    procedure EnableButtons(doEnable:Boolean);
    procedure SaveParams();
    procedure SaveAutoParams();
    procedure ThreadIsOver();
    procedure UpdateStarsAddedLabel();
    function CreateAndStartThread():Boolean;
  public

  end;

var
  GaiaDR2AddForm: TGaiaDR2AddForm;

implementation

{$R *.lfm}

procedure TGaiaDR2AddForm.StartStopButtonClick(Sender: TObject);
begin
  if not thread_is_active then CreateAndStartThread()
  else begin
    StartStopButton.Enabled := False;
    mthread.WaitIsOver(True);
    EnableButtons(False);
  end;
end;

procedure TGaiaDR2AddForm.AddStarButtonClick(Sender: TObject);
begin
  EnableButtons(False);
  checkobj.AddObject();
  Inc(addedcount);
  UpdateStarsAddedLabel();
  FreeAndNil(checkobj);
  ObjInfoLabel.Caption:='(Nothing...)';
  mthread.WaitIsOver(False);
end;

procedure TGaiaDR2AddForm.FormActivate(Sender: TObject);
begin

end;

procedure TGaiaDR2AddForm.FormCreate(Sender: TObject);
begin
  addedcount := 0;
  mthread := nil;
  checkobj := nil;
  thread_is_active := False
end;

procedure TGaiaDR2AddForm.RejectButtonClick(Sender: TObject);
begin
  EnableButtons(False);
  checkobj.RejectObject();
  FreeAndNil(checkobj);
  ObjInfoLabel.Caption:='(Nothing...)';
  mthread.WaitIsOver(False);
end;

procedure TGaiaDR2AddForm.SkipButtonClick(Sender: TObject);
begin
  EnableButtons(False);
  FreeAndNil(checkobj);
  ObjInfoLabel.Caption:='(Nothing...)';
  mthread.WaitIsOver(False);
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// message handlers
//-----------------------------------------
procedure TGaiaDR2AddForm.HandleThreadStart(var Msg:TLMessage);
begin
  AddCountLabel.Caption := 'Thread started.';
end;
//-----------------------------------------
procedure TGaiaDR2AddForm.HandleCannotStart(var Msg:TLMessage);
var postr:PString;
    errmsg, outmsg:string;
begin
  postr := PString(Msg.wParam);
  errmsg := postr^;
  outmsg := 'FAILED: ' + errmsg;
  AddCountLabel.Caption := outmsg;
  ThreadIsOver();
end;
//-----------------------------------------
procedure TGaiaDR2AddForm.HandleStarAdded(var Msg:TLMessage);
begin
  Inc(addedcount);
  UpdateStarsAddedLabel()
end;
//-----------------------------------------
procedure TGaiaDR2AddForm.HandleCheckAdd(var Msg:TLMessage);
begin
  checkobj := DR2AddBundle(Msg.wParam);
  ObjInfoLabel.Caption := checkobj.curobj.MakeSummaryString();
  EnableButtons(True);
end;
//-----------------------------------------
procedure TGaiaDR2AddForm.HandleDone(var Msg:TLMessage);
begin
  ThreadIsOver();
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaDR2AddForm.EnableParams(doEnable:Boolean);
begin
  MinPllxEdit.Enabled := doEnable;
  IgnoreRejectCB.Enabled := doEnable;
  DownSimCB.Enabled := doEnable;
  Viz2MCB.Enabled := doEnable;
  GMagEdit.Enabled := doEnable;
  MaxPllxErrEdit.Enabled := doEnable;
  SelACB.Enabled := doEnable;
  SelCCB.Enabled := doEnable;
  AutoRejEdit.Enabled := doEnable;
end;
//-------------------------------------------------
procedure TGaiaDR2AddForm.SaveParams();
begin
  sett.ignoreRejects:= IgnoreRejectCB.Checked;
  TryStrToFloat(MinPllxEdit.Text,sett.minpllx);
  sett.downSimbad:= DownSimCB.Checked;
  sett.down2mass:= Viz2MCB.Checked;
end;
//-------------------------------------------------
procedure TGaiaDR2AddForm.SaveAutoParams();
begin
  TryStrToCurr(GMagEdit.Text,autoadd.minGMag);
  TryStrToFloat(MaxPllxErrEdit.Text,autoadd.maxPllxError);
  autoadd.reqSelectionA:= SelACB.Checked;
  autoadd.reqSelectionC:= SelCCB.Checked;
  TryStrToFloat(AutoRejEdit.Text,autoadd.minRejectError);
end;
//-------------------------------------------------
procedure TGaiaDR2AddForm.EnableButtons(doEnable:Boolean);
begin
  AddStarButton.Enabled := doEnable;
  SkipButton.Enabled := doEnable;
  RejectButton.Enabled := doEnable;
  StartStopButton.Enabled := doEnable;
end;
//------------------------------------------------------------
procedure TGaiaDR2AddForm.ThreadIsOver();
var aclabel:string;
begin
  thread_is_active := False;
  EnableParams(True);
  aclabel := AddCountLabel.Caption;
  AddCountLabel.Caption := aclabel + ' All Done';
  ObjInfoLabel.Caption:='(Nothing, Done.)';
  StartStopButton.Caption := 'Start Adding';
  StartStopButton.Enabled := True;
  mthread := nil;
end;
//---------------------------------------------------------------
procedure TGaiaDR2AddForm.UpdateStarsAddedLabel();
begin
  AddCountLabel.Caption := IntToStr(addedcount) + ' stars/brown dwarfs added.';
end;
//-----------------------------------------------------------------------
function TGaiaDR2AddForm.CreateAndStartThread():Boolean;
begin
  Result := False;
  if thread_is_active then Exit;
  // properly starting the thread...
  StartStopButton.Enabled:= False;
  addedcount := 0;
  SaveParams();
  SaveAutoParams();
  EnableParams(False);
  mthread := AddFromDR2Thread.Create(True,sett,autoadd,Handle);
  thread_is_active := True;
  StartStopButton.Caption := 'Stop Adding';
  mthread.Start;
end;



//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
  GaiaDR2AddForm := nil;
end.

