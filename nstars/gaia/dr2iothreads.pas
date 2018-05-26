unit dr2iothreads;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLIntf, LMessages, gaiadr2holder, gaiadr2types;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
type

LoadStoreDR2Thread = class(TThread)
  protected
    filepath:TFileName;
    loadMode:Boolean;
    msgTarget:THandle;
    procedure PostString(msgID:Cardinal; msgText:string);
    procedure Execute; override;
  public
    constructor Create(createSuspended:Boolean; inLoadMode:Boolean; filename:TFileName; mtarget:THandle);
end;

LoadDR2FromSourceThread = class(TThread)
protected
  msgTarget:THandle;
  sparams:GaiaSourceParams;
  procedure PostString(msgID:Cardinal; msgText:string);
  procedure PostSizes();
  procedure Execute; override;
public
  constructor Create(createSuspended:Boolean; params:GaiaSourceParams; mtarget:THandle);
end;

const
  // message constants
  MSG_IOSTART = LM_USER + 3001;
  MSG_START = LM_USER + 3002;
  MSG_STARTFAIL = LM_USER + 3003;
  MSG_COUNTS = LM_USER + 3004;
  MSG_SIZES = LM_USER + 3005;
  MSG_DONE = LM_USER + 3006;
  MSG_INTFAIL = LM_USER + 3007;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//=====================================================
(*LoadStoreDR2Thread = class(TThread)
  protected
    filepath:TFileName;
    loadMode:Boolean;
    msgTarget:THandle; *)
procedure LoadStoreDR2Thread.PostString(msgID:Cardinal; msgText:string);
var pcopy:PString;
begin
  New(pcopy);
  pcopy^ := msgText;
  PostMessage(msgTarget,msgID,Int64(pcopy),0);
end;
//--------------------------------------------------
procedure LoadStoreDR2Thread.Execute;
var err_msg:string;
    okay,isfinished:Boolean;
    fullcount,donecount:Integer;
begin
  PostMessage(msgTarget,MSG_IOSTART,0,0);
  // starting (including opening the file for i/o
  if loadMode then begin
    if DR2Data <> nil then FreeAndNil(DR2Data);
    DR2Data := GaiaDR2Collection.Create;
    okay := DR2Data.StartInput(filepath,err_msg);
  end else begin
    okay := DR2Data.StartOutput(filepath,err_msg);
  end;
  // handling after-start
  if okay then begin
    if loadmode then fullcount := DR2Data.AmountToGet()
    else fullcount := DR2Data.StarCount;
    PostMessage(msgTarget,MSG_START,fullcount,0)
  end else begin
    PostString(MSG_STARTFAIL,err_msg);
    Exit;
  end;
  // once we get here, we load or store the stars in batches
  isfinished := False;
  repeat
    if loadmode then okay := DR2Data.InputStars(1000,isfinished,err_msg)
    else okay := DR2Data.OutputStars(4000,isfinished,err_msg);
    if okay then begin
      if loadmode then donecount := DR2Data.StarCount
      else donecount := DR2Data.OutputtedAmount();
      PostMessage(msgTarget,MSG_COUNTS,donecount,fullcount);
    end else Break;
  until isfinished;
  // after the loop, we have succeeded or failed
  if not okay then PostString(MSG_INTFAIL,err_msg)
  else PostMessage(msgTarget,MSG_DONE,0,0);
end;
//-----------------------------------------------------
constructor LoadStoreDR2Thread.Create(createSuspended:Boolean; inLoadMode:Boolean; filename:TFileName; mtarget:THandle);
begin
  filepath := filename;
  msgTarget := mtarget;
  loadMode := inLoadMode;
  if (not loadMode) then Assert(DR2Data <> nil);
  inherited Create(createSuspended);
end;
//=====================================================
procedure LoadDR2FromSourceThread.PostString(msgID:Cardinal; msgText:string);
var pcopy:PString;
begin
  New(pcopy);
  pcopy^ := msgText;
  PostMessage(msgTarget,msgID,Int64(pcopy),0);
end;
//-------------------------------------------------
procedure LoadDR2FromSourceThread.PostSizes();
var current,total:Int64;
begin
  total := DR2Data.SourceReadAmount(True);
  current := DR2Data.SourceReadAmount(False);
  PostMessage(msgTarget,MSG_SIZES,current,total);
end;
//--------------------------------------------------
procedure LoadDR2FromSourceThread.Execute;
var err_msg:string;
    okay,isfinished:Boolean;
    fullcount,donecount:Integer;
const BATCHAMOUNT = 1000;
begin
  PostMessage(msgTarget,MSG_IOSTART,0,0);
  // starting (including opening the file for i/o)
  if DR2Data <> nil then FreeAndNil(DR2Data);
  DR2Data := GaiaDR2Collection.Create;
  okay := DR2Data.StartSourceInput(sparams,err_msg);
  // handling after-start
  if okay then PostSizes()
  else begin
    PostString(MSG_STARTFAIL,err_msg);
    Exit;
  end;
  // once we get here, we load or store the stars in batches
  isfinished := False;
  repeat
    okay := DR2Data.ReadSourceStars(BATCHAMOUNT,isfinished,err_msg);
    donecount := DR2Data.StarCount;
    PostMessage(msgTarget,MSG_COUNTS,donecount,0);
    PostSizes();
    if not okay then Break;
  until isfinished;
  // after the loop, we have succeeded or failed
  if not okay then PostString(MSG_INTFail,err_msg)
  else PostMessage(msgTarget,MSG_DONE,0,0);
end;
//-----------------------------------------------------
constructor LoadDR2FromSourceThread.Create(createSuspended:Boolean; params:GaiaSourceParams; mtarget:THandle);
begin
  sparams := params;
  msgTarget := mtarget;
  inherited Create(createSuspended);
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
begin
end.

