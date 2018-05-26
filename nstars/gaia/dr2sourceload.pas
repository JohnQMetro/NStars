unit dr2sourceload;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MaskEdit, ComCtrls, LMessages, dr2iothreads, gaiadr2holder, gaiadr2types;

type

  { TGaiaDR2LSourceForm }

  TGaiaDR2LSourceForm = class(TForm)
    FilepathLabel: TLabel;
    StarsLoadedLabel: TLabel;
    PBarOverlayLabel: TLabel;
    PickSourceBtn: TButton;
    LoadProgressBar: TProgressBar;
    StartImportBtn: TButton;
    implabel: TLabel;
    aslbl: TLabel;
    maslbl: TLabel;
    MaxIDDistLabel: TLabel;
    OpenCSVSource: TOpenDialog;
    PllxOffsetLbl: TLabel;
    MinIDDistEdit: TMaskEdit;
    PllxOffsetEdit: TMaskEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PickSourceBtnClick(Sender: TObject);
    procedure PllxOffsetEditKeyPress(Sender: TObject; var Key: char);
    procedure StartImportBtnClick(Sender: TObject);
  private
    loadThread:LoadDR2FromSourceThread;
    loadAmount:Integer;
    sparams:GaiaSourceParams;

    // message handlers
    procedure HandleThreadStart(var Msg:TLMessage); message MSG_IOSTART;
    procedure HandleStarted(var Msg:TLMessage); message MSG_START;
    procedure HandleStartFail(var Msg:TLMessage); message MSG_STARTFAIL;
    procedure HandleCount(var Msg:TLMessage); message MSG_COUNTS;
    procedure HandleSizes(var Msg:TLMessage); message MSG_SIZES;
    procedure HandleDone(var Msg:TLMessage); message MSG_DONE;
    procedure HandleFail(var Msg:TLMessage); message MSG_INTFAIL;

    // helper methods
    procedure EWidgetEnable(enabled:Boolean);
    function LoadParamNums():Boolean;

  public

  end;

var
  GaiaDR2LSourceForm: TGaiaDR2LSourceForm;

const CSVFILTER = 'Comma Separated Values (*.csv)|*.csv';

implementation

{$R *.lfm}

{ TGaiaDR2LSourceForm }

procedure TGaiaDR2LSourceForm.PllxOffsetEditKeyPress(Sender: TObject;
  var Key: char);
var pressdex:Integer;
begin
  pressdex := PllxOffsetEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;

procedure TGaiaDR2LSourceForm.StartImportBtnClick(Sender: TObject);
var vtest:Boolean;
begin
  vtest := LoadParamNums();
  if not vtest then begin
    ShowMessage('Unable to convert numbers (should be impossible');
    Exit;
  end;
  // creating the launch thread...
  loadThread := LoadDR2FromSourceThread.Create(True,sparams,Handle);
  EWidgetEnable(False);
  loadThread.Start;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaDR2LSourceForm.HandleThreadStart(var Msg: TLMessage);
begin
  StarsLoadedLabel.Caption := 'File thread has started.';
end;
//---------------------------------------------------------------
procedure TGaiaDR2LSourceForm.HandleStarted(var Msg: TLMessage);
begin
  StarsLoadedLabel.Caption := 'File opened for import.';
end;
//---------------------------------------------------------------
procedure TGaiaDR2LSourceForm.HandleStartFail(var Msg: TLMessage);
var postr:PString;
    errmsg, outmsg:string;
begin
  postr := PString(Msg.wParam);
  errmsg := postr^;
  outmsg := 'File Opening has failed: ' + errmsg;
  StarsLoadedLabel.Caption := outmsg;
  ShowMessage(outmsg);
end;
//---------------------------------------------------------------
procedure TGaiaDR2LSourceForm.HandleCount(var Msg: TLMessage);
var omsg:string;
begin
  loadAmount := Integer(Msg.wParam);
  omsg := IntToStr(loadAmount) + ' source objects loaded';
  StarsLoadedLabel.Caption := omsg;
end;
//---------------------------------------------------------------
procedure TGaiaDR2LSourceForm.HandleSizes(var Msg: TLMessage);
var omsg:string; percent:Double;
    current,total:Int64;
    limit:Integer;
begin
  // calculating percentages and progress bar amounts
  current := Integer(Msg.wParam);
  total := Integer(Msg.lParam);
  percent := 100.0 * (Double(current)/Double(total));
  limit := Trunc(10*percent);
  // setting the progress bar amount
  LoadProgressBar.Position := limit;
  // setting the label for the progress bar
  omsg := Trim(FloatToStrF(percent,ffFixed,5,1)) + '%';
  PBarOverlayLabel.Caption:= omsg;
end;
//----------------------------------------------------------------------
procedure TGaiaDR2LSourceForm.HandleDone(var Msg: TLMessage);
var oldcap,dispmsg:string;
begin
  oldcap := StarsLoadedLabel.Caption;
  StarsLoadedLabel.Caption := oldcap + ' DONE.';
  LoadProgressBar.Position := 1000;
  PBarOverlayLabel.Caption:= '100% DONE';
  ShowMessage('Finished. ' + IntToStr(loadAmount) + ' stars/brown dwarfs loaded');
  // enabling
  EWidgetEnable(True);
  FreeAndNil(loadThread);
end;
//---------------------------------------------------------------
procedure TGaiaDR2LSourceForm.HandleFail(var Msg: TLMessage);
var postr:PString;
    errmsg, outmsg:string;
begin
  postr := PString(Msg.wParam);
  errmsg := postr^;
  outmsg := 'Loading has failed: ' + errmsg;
  StarsLoadedLabel.Caption := outmsg;
  ShowMessage(outmsg + sLineBreak);
  EWidgetEnable(True);
  FreeAndNil(loadThread);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// helper methods
procedure TGaiaDR2LSourceForm.EWidgetEnable(enabled:Boolean);
begin
  MinIDDistEdit.Enabled := enabled;
  PllxOffsetEdit.Enabled := enabled;
  PickSourceBtn.Enabled := enabled;
  StartImportBtn.Enabled := enabled;
end;
//-------------------------------------
function TGaiaDR2LSourceForm.LoadParamNums():Boolean;
begin
  Result := False;
  if not TryStrToFloat(MinIDDistEdit.Caption,sparams.maxdist) then Exit;
  if not TryStrToFloat(PllxOffsetEdit.Caption,sparams.parallaxOffset) then Exit;
  Result := True;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TGaiaDR2LSourceForm.FormActivate(Sender: TObject);
begin

end;

procedure TGaiaDR2LSourceForm.FormCreate(Sender: TObject);
begin
    // setting up the progress bar overlay
   PBarOverlayLabel.Parent := LoadProgressBar;
   PBarOverlayLabel.BringToFront();
   PBarOverlayLabel.Top := 0;
   PBarOverlayLabel.Left:= 0;
   PBarOverlayLabel.Width := LoadProgressBar.ClientWidth;
   PBarOverlayLabel.Height:= LoadProgressBar.ClientHeight;
   // other stuff
   StartImportBtn.Enabled := False;
   loadThread := nil;
   loadAmount := 0;
end;

//------------------------------------------------------------------
procedure TGaiaDR2LSourceForm.PickSourceBtnClick(Sender: TObject);
begin
  OpenCSVSource.Filter := CSVFILTER;
  if OpenCSVSource.Execute() then begin
    sparams.infilename := OpenCSVSource.FileName;
    FilepathLabel.Caption := sparams.infilename;
    StartImportBtn.Enabled := True;
  end;
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
end.

