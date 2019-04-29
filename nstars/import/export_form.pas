unit export_form;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, MaskEdit, StrUtils, stardata, collecdata;

type

  { TExportForm }

  TExportForm = class(TForm)
    AdjustPositionsCB: TCheckBox;
    AddPUQCB: TCheckBox;
    UseAvgCB: TCheckBox;
    LumLimEdit: TMaskEdit;
    ExportSaveDialog: TSaveDialog;
    VisLumACCB: TCheckBox;
    DoExportBtn: TButton;
    adLabel: TLabel;
    OffsetLabel: TLabel;
    OffsetEdit: TMaskEdit;
    Use2300NamCB: TCheckBox;
    ExportFormatRG: TRadioGroup;
    TargYearEdit: TMaskEdit;
    procedure AdjustPositionsCBChange(Sender: TObject);
    procedure DoExportBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure VisLumACCBChange(Sender: TObject);
  private
    { private declarations }
    // data
    parameters:SysOutParams;
    adjspace:Boolean;
    allcaps:Boolean;
    savename:TFileName;
    // private helper methods
    procedure EnableInputs(doit:Boolean);
    function SaveParameters:Boolean;
    function GetSaveFileName:Boolean;
  public
    { public declarations }
  end;

var
  ExportForm: TExportForm;

const
    QCSVFILTER = 'Comma Separated Values (*.csv)|*.csv';
    QLSTFILTER = 'CHView Import List (*.lst)|*.lst';

implementation

{$R *.lfm}

{ TExportForm }

procedure TExportForm.FormActivate(Sender: TObject);
begin
  adjspace := False;
  allcaps := True;
  parameters := nil;
end;

procedure TExportForm.VisLumACCBChange(Sender: TObject);
begin
  if allcaps <> (VisLumACCB.Checked) then begin
    allcaps := VisLumACCB.Checked;
    LumLimEdit.Enabled := allcaps;
  end;
end;

procedure TExportForm.AdjustPositionsCBChange(Sender: TObject);
begin
  if adjspace <> (AdjustPositionsCB.Checked) then begin
    adjspace := AdjustPositionsCB.Checked;
    TargYearEdit.Enabled := adjspace;
  end;
end;

procedure TExportForm.DoExportBtnClick(Sender: TObject);
begin
  if GetSaveFileName then begin
    Screen.Cursor := crHourGlass;
    if not SaveParameters then begin
       Screen.Cursor := crDefault;
       ShowMessage('There is a problem with the parameters!');
       Exit;
    end;
    // once we get here, we disable inputs and launch the save!
    EnableInputs(False);
    if ExportFormatRG.ItemIndex = 0 then begin
      primaryl.SaveToCHview(savename,parameters);
    end
    else if ExportFormatRG.ItemIndex = 1 then begin
      primaryl.SaveToAstrosynthesis(savename,parameters);
    end
    else Assert(False);
    // done!
    Screen.Cursor := crDefault;
    ShowMessage('Export done!');
    EnableInputs(True);
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
procedure TExportForm.EnableInputs(doit: Boolean);
begin
  ExportFormatRG.Enabled := doit;
  OffsetEdit.Enabled := doit;
  use2300NamCB.Enabled := doit;
  AddPUQCB.Enabled := doit;

  AdjustPositionsCB.Enabled := doit;
  if (not doit) then TargYearEdit.Enabled := False
  else TargYearEdit.Enabled := adjspace;

  VisLumACCB.Enabled := doit;
  if (not doit) then LumLimEdit.Enabled := False
  else LumLimEdit.Enabled := allcaps;

  DoExportBtn.Enabled := doit;
end;
//------------------------------------------------------
function TExportForm.SaveParameters: Boolean;
begin
  Result := False;
  if parameters = nil then parameters := SysOutParams.Create;
  if not parameters.SetIDOffset(OffsetEdit.Text) then Exit;
  if not AdjustPositionsCB.Checked then parameters.TargetYear := -1
  else begin
    if not parameters.SetTargYear(TargYearEdit.Text) then Exit;
  end;
  if not VisLumACCB.Checked then parameters.allcaplum:= -1
  else begin
    if not parameters.SetAllCapsLum(LumLimEdit.Text) then Exit;
  end;
  parameters.is2300ad := use2300NamCB.Checked;
  parameters.addpquest:= AddPUQCB.Checked;
  parameters.useavg := UseAvgCB.Checked;
  Result := True
end;
//--------------------------------------------------------
function TExportForm.GetSaveFileName:Boolean;
var owhich:Integer;
begin
  Result := False;
  owhich := ExportFormatRG.ItemIndex;
  if  owhich = 0 then ExportSaveDialog.Filter := QLSTFILTER
  else if owhich = 1 then ExportSaveDialog.Filter := QCSVFILTER
  else Assert(False);
  if ExportSaveDialog.Execute then begin
    savename := ExportSaveDialog.FileName;
    if (owhich = 0) and (not AnsiEndsText('.lst',savename)) then savename += '.lst';
    if (owhich = 1) and (not AnsiEndsText('.csv',savename)) then savename += '.csv';
    Result := True;
  end;
end;

//========================================================

begin
  ExportForm := nil;
end.

