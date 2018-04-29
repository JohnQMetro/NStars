unit dr2loadstore;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LMessages, dr2iothreads;

type

  { TGaiaDR2LoadStoreForm }

  TGaiaDR2LoadStoreForm = class(TForm)
    ActionStatusLabel: TLabel;
    MainActionLabel: TLabel;
    OpenGaiaDialog: TOpenDialog;
    FileOpProgress: TProgressBar;
    StoreGaiaDialog: TSaveDialog;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    storemode:Boolean;
    thread:LoadStoreDR2Thread;
    totalcount:Integer;

    // message handlers
    procedure HandleThreadStart(var Msg:TLMessage); message MSG_IOSTART;
    procedure HandleStarted(var Msg:TLMessage); message MSG_START;
    procedure HandleStartFail(var Msg:TLMessage); message MSG_STARTFAIL;
    procedure HandleCounts(var Msg:TLMessage); message MSG_COUNTS;
    procedure HandleDone(var Msg:TLMessage); message MSG_DONE;
    procedure HandleFail(var Msg:TLMessage); message MSG_INTFAIL;
  public

  end;

var
  GaiaDR2LoadStoreForm: TGaiaDR2LoadStoreForm;
  GaiaDR2LoadStoreFormModeStore:Boolean;

const CSVFILTER = 'Comma Separated Values (*.csv)|*.csv';

implementation

{$R *.lfm}

{ TGaiaDR2LoadStoreForm }

procedure TGaiaDR2LoadStoreForm.FormActivate(Sender: TObject);
var fxname:TFileName;
begin
  thread := nil;
  storemode := GaiaDR2LoadStoreFormModeStore;
  // loading from file
  if not storemode then begin
    OpenGaiaDialog.Filter := CSVFILTER;
    if OpenGaiaDialog.Execute() then begin
      fxname := OpenGaiaDialog.FileName;
    end else Close();
  // storing to file
  end else begin
    StoreGaiaDialog.Filter := CSVFILTER;
    if StoreGaiaDialog.Execute() then begin
      fxname := StoreGaiaDialog.FileName;
    end else Close();
  end;
  // we get here, we create a thread to load or store...
  thread := LoadStoreDR2Thread.Create(True,not storemode,fxname,Handle);
  if storemode then MainActionLabel.Caption := 'Storing DR2 Stars to ' + fxname
  else MainActionLabel.Caption := 'Loading DR2 Stars from ' + fxname;
  thread.Start;
end;

procedure TGaiaDR2LoadStoreForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if (thread <> nil) then begin
    FreeAndNil(thread);
  end;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// message handlers
//-----------------------------------
procedure TGaiaDR2LoadStoreForm.HandleThreadStart(var Msg:TLMessage);
begin
  ActionStatusLabel.Caption := 'File thread has started.';
end;
//-----------------------------------
procedure TGaiaDR2LoadStoreForm.HandleStarted(var Msg:TLMessage);
var disp:string;
begin
  disp := 'File opened for ';
  if storemode then disp += 'export. '
  else disp += 'reading. ';
  totalcount := Integer(Msg.wParam);
  disp += IntToStr(totalcount) + ' stars to ';
  if storemode then disp += 'write.'
  else disp += 'read.';
  ActionStatusLabel.Caption := disp;
end;
//-----------------------------------
procedure TGaiaDR2LoadStoreForm.HandleStartFail(var Msg:TLMessage);
var postr:PString;
    errmsg, outmsg:string;
begin
  postr := PString(Msg.wParam);
  errmsg := postr^;
  outmsg := 'File Opening has failed: ' + errmsg;
  ActionStatusLabel.Caption := outmsg;
  ShowMessage(outmsg + sLineBreak + 'This dialog will close.');
  Close();
end;
//-----------------------------------
procedure TGaiaDR2LoadStoreForm.HandleCounts(var Msg:TLMessage);
var omsg:string ;
    rcount,total:Integer;
    fracdone:Double;
begin
  rcount := Integer(Msg.wParam);
  total := Integer(Msg.lParam);
  omsg := IntToStr(rcount) + ' of ' + IntToStr(total) + ' stars ';
  if storemode then omsg += 'written.'
  else omsg += 'read.';
  fracdone := 200*(rcount / total);
  FileOpProgress.Position := Trunc(fracdone);
  ActionStatusLabel.Caption := omsg;
end;
//-----------------------------------
procedure TGaiaDR2LoadStoreForm.HandleDone(var Msg:TLMessage);
var oldcap,dispmsg:string;
begin
  oldcap := ActionStatusLabel.Caption;
  ActionStatusLabel.Caption := oldcap + ' DONE.';
  if storemode then dispmsg := 'Export'
  else dispmsg := 'Loading';
  dispmsg += ' done. This dialog will now close';
  FileOpProgress.Position := 200;
  ShowMessage(dispmsg);
  Close();
end;
//-----------------------------------
procedure TGaiaDR2LoadStoreForm.HandleFail(var Msg:TLMessage);
var postr:PString;
    errmsg, outmsg:string;
begin
  postr := PString(Msg.wParam);
  errmsg := postr^;
  if storemode then outmsg := 'Writing '
  else outmsg := 'Reading ';
  outmsg += 'has failed: ' + errmsg;
  ActionStatusLabel.Caption := outmsg;
  ShowMessage(outmsg + sLineBreak + 'This dialog will close.');
  Close();
end;
//=========================================================================

end.

