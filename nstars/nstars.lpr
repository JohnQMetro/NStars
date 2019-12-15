program nstars;

{$MODE Delphi}

uses
  Forms, Interfaces,
  nstarsmain in 'nstarsmain.pas' {NstarsMainForm},
  collecdata,
  sptfluxest in 'stardata/sptfluxest.pas',
  gnotes_form in 'gnotes_form.pas' {FileGlobalNotes},
  df_strings, cluster, nltt_match,
  clusteredit in 'clusteredit.pas' {ClusterEditForm};

{.$R *.res}

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.Title := 'NStars';
  Application.CreateForm(TNStarsMainForm, NStarsMainForm);
  Application.CreateForm(TFileGlobalNotes, FileGlobalNotes);
  Application.CreateForm(TClusterEditForm, ClusterEditForm);
  Application.Run;
end.
