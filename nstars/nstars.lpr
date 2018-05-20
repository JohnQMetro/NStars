program nstars;

{$MODE Delphi}

uses
  Forms, Interfaces,
  nstarsmain in 'nstarsmain.pas' {NstarsMainForm},
  collecdata in 'collecdata.pas',
  sptfluxest in 'stardata/sptfluxest.pas',
  gnotes_form in 'gnotes_form.pas' {FileGlobalNotes},
  df_strings in 'df_strings.pas',
  cluster in 'cluster.pas',
  clusteredit in 'clusteredit.pas' {ClusterEditForm};

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'NStars';
  Application.CreateForm(TNStarsMainForm, NStarsMainForm);
  Application.CreateForm(TFileGlobalNotes, FileGlobalNotes);
  Application.CreateForm(TClusterEditForm, ClusterEditForm);
  Application.Run;
end.
