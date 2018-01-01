unit gnotes_form;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,collecdata;

type
  TFileGlobalNotes = class(TForm)
    SL_NameEdit: TLabeledEdit;
    SL_AuthorEdit: TLabeledEdit;
    Label1: TLabel;
    SL_SourceMemo: TMemo;
    Label2: TLabel;
    SL_NotesMemo: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FileGlobalNotes: TFileGlobalNotes;

implementation

{$R *.lfm}

procedure TFileGlobalNotes.FormActivate(Sender: TObject);
begin
  SL_NameEdit.Text := primaryl.name;
  SL_AuthorEdit.Text := primaryl.author;
  SL_SourceMemo.Text := primaryl.sources;
  SL_NotesMemo.Text := primaryl.global_notes;
end;

procedure TFileGlobalNotes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  primaryl.name := SL_NameEdit.Text;
  primaryl.author := SL_AuthorEdit.Text;
  primaryl.sources := SL_SourceMemo.Text;
  primaryl.global_notes := SL_NotesMemo.Text;
end;

procedure TFileGlobalNotes.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  primaryl.name := SL_NameEdit.Text;
  primaryl.author := SL_AuthorEdit.Text;
  primaryl.sources := SL_SourceMemo.Text;
  primaryl.global_notes := SL_NotesMemo.Text;
  canClose := True;
end;

end.
