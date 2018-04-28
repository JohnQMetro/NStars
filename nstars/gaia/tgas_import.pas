unit tgas_import;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MaskEdit, ComCtrls, tgas;

type

  { TTGASImportForm }

  TTGASImportForm = class(TForm)
    ClearButton: TButton;
    ImportFileBtn: TButton;
    ImportDirBtn: TButton;
    ImportAlterGB: TGroupBox;
    ImportCountLabel: TLabel;
    maslabel2: TLabel;
    OffsetEdit: TMaskEdit;
    OffsetLabel: TLabel;
    maslabel: TLabel;
    MaxErrorEdit: TMaskEdit;
    MaxPELabel: TLabel;
    MaxLYE: TMaskEdit;
    MaxDistLabel: TLabel;
    LYLL: TLabel;
    ImportFileProgress: TProgressBar;
    TGASImportFileDialog: TOpenDialog;
    ParallaxChangeRG: TRadioGroup;
    TGASImportDirectoryDialog: TSelectDirectoryDialog;
    procedure ClearButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImportDirBtnClick(Sender: TObject);
    procedure ImportFileBtnClick(Sender: TObject);
    procedure OffsetEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    params:TGASAlterParams;
    oldcount:Integer;
    procedure UpdateCountLabel;
    procedure EnableParams(doit:Boolean);
    function LoadParamObject:Boolean;
    function LoadFile(fname:TFileName; showempty:Boolean):Boolean;
  public
    { public declarations }
  end;

const
  CSVFILTERX = 'Comma Separated Values (*.csv)|*.csv';

var
  TGASImportForm: TTGASImportForm;

implementation

{$R *.lfm}

{ TTGASImportForm }

procedure TTGASImportForm.FormCreate(Sender: TObject);
begin

end;

procedure TTGASImportForm.ImportDirBtnClick(Sender: TObject);
var xdir:TFileName;
    loadok,loadex:Boolean;
    CSVFiles:TStringList;
    fcount,fdex:Integer;
begin
  if not LoadParamObject then Exit;
  if TGASImportDirectoryDialog.Execute then begin
    Screen.Cursor := crHourGlass;
    EnableParams(False);
    xdir := TGASImportDirectoryDialog.FileName;
    CSVFiles := FindAllFiles(xdir,'*.csv',False);
    fcount := CSVFiles.Count;
    ImportFileProgress.Max := fcount;
    ImportFileProgress.Position := 0;
    loadok := False;
    for fdex := 0 to (fcount-1) do begin
      xdir := CSVFiles[fdex];
      loadex := LoadFile(xdir,False);
      ImportFileProgress.Position := fdex + 1;
      ImportFileProgress.Update;
      application.processmessages;
      loadok := (loadok or loadex);
    end;
    Screen.Cursor := crDefault;
    if loadok then ShowMessage('Files loaded');
    EnableParams(True);
  end;
end;

procedure TTGASImportForm.ImportFileBtnClick(Sender: TObject);
var xfile:TFileName;
    loadok:Boolean;
begin
  if not LoadParamObject then Exit;
  if TGASImportFileDialog.Execute then begin
    Screen.Cursor := crHourGlass;
    EnableParams(False);
    xfile := TGASImportFileDialog.FileName;
    loadok := LoadFile(xfile,True);
    Screen.Cursor := crDefault;
    if loadok then ShowMessage('File loaded');
    EnableParams(True);
  end;
end;

procedure TTGASImportForm.FormActivate(Sender: TObject);
begin
  params := nil;
  if tgas_main = nil then oldcount := 0
  else oldcount := tgas_main.StarCount;
  UpdateCountLabel;
  TGASImportFileDialog.Filter := CSVFILTERX;
end;

procedure TTGASImportForm.ClearButtonClick(Sender: TObject);
begin
  FreeAndNil(tgas_main);
  oldcount := 0;
  UpdateCountLabel;
end;

procedure TTGASImportForm.OffsetEditKeyPress(Sender: TObject; var Key: char);
var pressdex:Integer;
begin
  pressdex := OffsetEdit.SelStart;
  if (pressdex = 0) and (not (Key in [#8, '+','-'])) then Key := #0;
end;
//======================================================================
procedure TTGASImportForm.UpdateCountLabel;
var pstr:string;
begin
  if oldcount = 0 then pstr := 'No stars'
  else if oldcount = 1 then pstr := '1 star'
  else pstr := IntToStr(oldcount) + ' stars';
  ImportCountLabel.Caption := pstr
end;
//------------------------------------------
procedure TTGASImportForm.EnableParams(doit:Boolean);
begin
  ParallaxChangeRG.Enabled := doit;
  MaxLYE.Enabled := doit;
  MaxErrorEdit.Enabled := doit;
  OffsetEdit.Enabled := doit;
  ImportFileBtn.Enabled := doit;
  ImportDirBtn.Enabled := doit;
  ClearButton.Enabled := doit;
end;
//------------------------------------------
function TTGASImportForm.LoadParamObject: Boolean;
var pci:Integer;
begin
  Result := False;
  pci := ParallaxChangeRG.ItemIndex;
  if pci < 0 then begin
    ShowMessage('The Parallax change option is not set!');
    Exit;
  end;
  if params = nil then params := TGASAlterParams.Create;
  params.transform := pci;
  if (not params.SetNumbersFromText(MaxLYE.Text,MaxErrorEdit.Text,OffsetEdit.Text)) then begin
    FreeAndNil(params);
    ShowMessage('Unable to produce input parameters!');
    Exit;
  end;
  Result := True;
end;
//------------------------------------------
function TTGASImportForm.LoadFile(fname: TFileName; showempty:Boolean): Boolean;
var impok:Boolean;
    newcount:Integer;
    pstr:string;
begin
  // quick false case
  Result := False;
  if Length(fname) = 0 then begin
    ShowMessage('Empty filename!');
    Exit;
  end;
  if params = nil then begin
    ShowMessage('Empty parameters!');
    Exit;
  end;
  // loading
  if tgas_main = nil then tgas_main := TGASCollection.Create;
  tgas_main.ImportFromCSVFile(fname,params);
  newcount := tgas_main.StarCount;
  // no stars loaded case
  if newcount = oldcount then begin
    Screen.Cursor := crDefault;
    if showempty then ShowMessage('No Stars loaded!');
    Exit;
  end;
  // stars loaded
  oldcount := newcount;
  UpdateCountLabel;
  // done
  Result := True;
end;

begin
  TGASImportForm := nil;
end.

