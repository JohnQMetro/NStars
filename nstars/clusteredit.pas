unit clusteredit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MaskEdit{, Mask}, cluster, collecdata, StrUtils,
  stardata;

type
  TClusterEditForm = class(TForm)
    ClusterEditGroupBox: TGroupBox;
    ClusterListBox: TListBox;
    NewClusterBtn: TButton;
    DeleteClusterBtn: TButton;
    MakeCHViewBtn: TButton;
    ClusterReportMemo: TMemo;
    MaxLinkDistEdit: TMaskEdit;
    Label1: TLabel;
    SetLinksBtn: TButton;
    ClusterSaveDialog: TSaveDialog;
    procedure FormActivate(Sender: TObject);
    procedure MaxLinkDistEditExit(Sender: TObject);
    procedure NewClusterBtnClick(Sender: TObject);
    procedure SetLinksBtnClick(Sender: TObject);
    procedure ClusterListBoxClick(Sender: TObject);
    procedure ButtonsEnable(which:Boolean);
    procedure DeleteClusterBtnClick(Sender: TObject);
    procedure MakeCHViewBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClusterEditForm: TClusterEditForm;

implementation

{$R *.lfm}

procedure TClusterEditForm.ClusterListBoxClick(Sender: TObject);
var ii:Integer;
begin
  ii := ClusterListBox.ItemIndex;
  if ii < 0 then Exit;
  ClusterReportMemo.Text := cluster_list[ii].BuildReport;
  ButtonsEnable(true);

end;

procedure TClusterEditForm.DeleteClusterBtnClick(Sender: TObject);
var i,ii,J:Integer;
begin
  ii := ClusterListBox.ItemIndex;
  // removing from the list box, annoyingly, DeleteSelected is not fpc compatible
  if ClusterListBox.SelCount > 0 then begin
     for i:=ClusterListBox.Items.Count - 1 downto 0 do begin
      if ClusterListBox.Selected[i] then begin
         ii := i;
         ClusterListBox.Items.Delete(i);
         Break;
      end;
     end;
  end;
  // removing from the list
  cluster_list[ii].Destroy;
  cluster_list[ii] := nil;
  for J := ii+1 to length(cluster_list) - 1 do begin
    cluster_list[J-1] := cluster_list[J];
  end;
  SetLength(cluster_list,length(cluster_list)-1);
  // redoing the selected index
  // index remains the same, but the cluster changes
  if ClusterListBox.Count>ii then begin
    ClusterListBox.ItemIndex := ii;
    ClusterReportMemo.Text := cluster_list[ClusterListBox.ItemIndex].BuildReport;
  end
  // previous index was the last.. make it still the last
  else if ClusterListBox.Count=ii then begin
    ClusterListBox.ItemIndex := ClusterListBox.Count-1;
    ClusterReportMemo.Text := cluster_list[ClusterListBox.ItemIndex].BuildReport;
  end
  // nothing left
  else begin
    ClusterReportMemo.Text := '';
    ButtonsEnable(false);
  end;
end;

procedure TClusterEditForm.FormActivate(Sender: TObject);
var clist:TStringList;
    buf:string;
begin
  clist := MakeClusterNamesList;
  ClusterListBox.Items := clist;
  Str(linkdist_max:5:2,buf);
  MaxLinkDistEdit.Text := buf;

end;

procedure TClusterEditForm.MakeCHViewBtnClick(Sender: TObject);
var xfile:TFileName;
    cc:Integer;
    oparams:SysOutParams;
begin
  cc := ClusterListBox.ItemIndex;
  if ClusterSaveDialog.Execute then begin
    xfile := ClusterSaveDialog.FileName;
    oparams := SysOutParams.Create;
    if not AnsiEndsText('.lst',xfile) then xfile := xfile + '.lst';
    cluster_list[cc].SaveToCHview(xfile,oparams);
    FreeAndNil(oparams);
  end;
end;

procedure TClusterEditForm.MaxLinkDistEditExit(Sender: TObject);
var tcode:Integer;
begin
  Val(MaxLinkDistEdit.Text,linkdist_max,tcode);
  if tcode = 0 then MaxLinkDistEdit.Modified := False;
end;

procedure TClusterEditForm.NewClusterBtnClick(Sender: TObject);
var inname:string;
begin
  inname := 'New Cluster';
  if InputQuery('Cluster Name Entry','Enter a new cluster name:',inname) then begin
    // testing the input name
    if inname='' then ShowMessage('The Cluster Name cannot be empty!')
    else if ClusterNameExists(inname) then begin
      ShowMessage('The Cluster already exists')
    end
    // the name ik okay
    else begin
      AddNewCluster(inname);
      ClusterListBox.Items.Add(inname);
      ClusterListBox.ItemIndex := ClusterListBox.Count-1;
      ButtonsEnable(true);
    end;
  end;
end;

procedure TClusterEditForm.SetLinksBtnClick(Sender: TObject);
var ii:Integer;
begin
  SetLinksBtn.Enabled := False;
  ii := ClusterListBox.ItemIndex;
  primaryl.ReLinkCluster(cluster_list[ii]);
  ClusterReportMemo.Text := cluster_list[ii].BuildReport;
  SetLinksBtn.Enabled := True;
end;
//---------------------------------------------------------------------------
procedure TClusterEditForm.ButtonsEnable(which:Boolean);
begin
  DeleteClusterBtn.Enabled := which;
  MakeCHViewBtn.Enabled := which;
  SetLinksBtn.Enabled := which;
end;



end.
