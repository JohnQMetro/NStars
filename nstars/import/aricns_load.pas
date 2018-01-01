unit aricns_load;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Arcins, Utilities, Types;

type

  { TAricnsLoadForm }

  TAricnsLoadForm = class(TForm)
    AricnsStatusLabel: TLabel;
    AricnsLoadProgressBar: TProgressBar;
    procedure AricnsLoadProgressBarContextPopup(Sender: TObject;
      MousePos: TPoint; var Handled: Boolean);
    procedure AricnsStatusLabelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

AricnsLoader = class(TThread)
  protected
    label_proxy:TLabel;
    progress_proxy:TProgressBar;
    msg:string;
    pbar_value:Integer;
    parent:TForm;
  public
    constructor Create(inlabel:TLabel; inbar:TProgressBar; inf:TForm);
    procedure Execute; override;
    procedure LoadMsg;
    procedure LoadPBar;
    procedure CompMsg(load:Boolean; cat:Integer);
    procedure TheTerminator(Sender: TObject);
end;

var
  AricnsLoadForm: TAricnsLoadForm;
  aloader:AricnsLoader;
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
implementation

{$R *.lfm}
//============================================================================
constructor AricnsLoader.Create(inlabel:TLabel; inbar:TProgressBar; inf:TForm);
begin
  inherited Create(True);
  Assert(inlabel<>nil);
  Assert(inbar<>nil);
  label_proxy := inlabel;
  progress_proxy := inbar;
  pbar_value :=0;
  msg := '';
  parent := inf;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure AricnsLoader.Execute;
var I:Integer;
    inurl,pagedata:string;
begin
  OnTerminate := TheTerminator;
  adat.urldata := AricnsListing.Create;
  // we load the catalogs loop by loop
  for I:=0 to 19 do begin
    // loading message
    CompMsg(true,I);
    Synchronize(LoadMsg);
    // getting the url an loading
    inurl := cat_urls[I];
    pagedata := GetMainPage(inurl);
    if pagedata='' then begin
      msg := 'Download Failed!';
      Synchronize(LoadMsg);
      Exit;
    end;
    Inc(pbar_value);
    Synchronize(LoadPBar);
    // processing message
    CompMsg(false,I);
    Synchronize(LoadMsg);
    // processing
    adat.catalogs[I] := CatalogLinks.Create(CatalogID(I),adat.urldata);
    adat.catalogs[I].LoadData(pagedata);
    Inc(pbar_value);
    Synchronize(LoadPBar);
  end;
  // once we are done...
  msg := 'All done!';
  Synchronize(LoadMsg);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure AricnsLoader.LoadMsg;
begin
  label_proxy.Caption := msg;
end;
//---------------------------------------------------------------
procedure AricnsLoader.LoadPBar;
begin
  progress_proxy.StepIt;
end;
//---------------------------------------------------------------
procedure AricnsLoader.CompMsg(load:Boolean; cat:Integer);
begin
  Assert(cat<20);
  if load then msg := 'Downloading '
  else msg := 'Processing ';
  // we now add the specific info
  msg := msg + cat_names[cat] + ' Catalog page from ARICNS.';
end;
//-----------------------------------------------------
procedure AricnsLoader.TheTerminator(Sender: TObject);
begin
  adat.loaded := True;
  parent.Release;
end;
//============================================================================
{ TAricnsLoadForm }
procedure TAricnsLoadForm.FormActivate(Sender: TObject);
begin
  aloader := AricnsLoader.Create(AricnsStatusLabel,AricnsLoadProgressBar,Self);
  aloader.Resume;
end;

procedure TAricnsLoadForm.AricnsStatusLabelClick(Sender: TObject);
begin

end;

procedure TAricnsLoadForm.AricnsLoadProgressBarContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TAricnsLoadForm.FormCreate(Sender: TObject);
begin

end;

//============================================================================

end.

