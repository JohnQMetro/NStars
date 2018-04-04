unit nstarsmain;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, StrUtils, Types,
  CheckLst, MaskEdit,
  collecdata, stardata, namedata, newlocation, unitdata, aricns_load,
  arcins_star, Arcins, gnotes_form, cluster, clusteredit, tgas, simbad,
  NewStar, catalogedit, starextraedit, stardataedit, StarDataBase,
  starlocatedit, newImports,  starext2edit, FindDuplInterF, ImportDataForm,
  MainLocatEdit, df_strings, sptfluxest, PPMMatchForm, Utilities2, tgas_import,
  export_form, ExtraImports;

type

  { TNStarsMainForm }

  TNStarsMainForm = class(TForm)
    AddBDMI: TMenuItem;
    AricnsDataCB: TCheckBox;
    MenuItem1: TMenuItem;
    UCAC4MagEstMI: TMenuItem;
    MISwapParallax: TMenuItem;
    ShowPosDegMI: TMenuItem;
    PosSepSetMI: TMenuItem;
    PreMainSeqCB: TCheckBox;
    FluxesTEffTab: TTabSheet;
    HasPlanetsCB: TCheckBox;
    MainLocatEditFrame1: TMainLocatEditFrame;
    ProblemCB: TCheckBox;
    SimbadDataCB: TCheckBox;
    StarExtraDataFrame1: TStarExtraDataFrame;
    StarFluxTEffFrame: TStarExtras2Frame;
    StarLocatFrame1: TStarLocatFrame;
    SystemNotesMemo: TMemo;
    UseExtraDataCB: TCheckBox;
    UseSepLocatCB: TCheckBox;
    ExtraStarDataTabs: TPageControl;
    StarCatIDFrame: TCatalogIDEditFrame;
    StarDataCoreFrame: TCoreStarDataFrame;
    FlareStarCB: TCheckBox;
    ChangeStarToBDMI: TMenuItem;
    EnterPMPartsMI: TMenuItem;
    FindRemTGASMI: TMenuItem;
    GuessSpectraMI: TMenuItem;
    BadSpectraMI: TMenuItem;
    LuminosityProbMI: TMenuItem;
    ImportTGASLeftMI: TMenuItem;
    BrownDwarfZMMI: TMenuItem;
    FindIDMI: TMenuItem;
    HasProbMI: TMenuItem;
    ExportListMI: TMenuItem;
    GetLoggMI: TMenuItem;
    ImportFrSN35MI: TMenuItem;
    APASSwSimMI: TMenuItem;
    ComponentTab: TTabSheet;
    ComponentTabControl: TTabControl;
    ExtraDataTab: TTabSheet;
    WriteEstimatorMI: TMenuItem;
    TGASNameMatchMI: TMenuItem;
    ShowGalCoordMI: TMenuItem;
    TestVizDMI: TMenuItem;
    ShowUVWMI: TMenuItem;
    ParallaxEntryMI: TMenuItem;
    MergeIntoMI: TMenuItem;
    SetMergeSrcMI: TMenuItem;
    PPMMatchMI: TMenuItem;
    SimbadIDFluxMI: TMenuItem;
    SimbadDataFetch: TMenuItem;
    SimbURLCGID: TMenuItem;
    SimbadURLData: TMenuItem;
    MenuItem3: TMenuItem;
    InsertStarMI: TMenuItem;
    MenuItem4: TMenuItem;
    CatDupesScanMI: TMenuItem;
    MenuItem5: TMenuItem;
    APASSfluxMI: TMenuItem;
    MenuItem6: TMenuItem;
    LoadARICNSCatalogs1: TMenuItem;
    GetARICNSStarData: TMenuItem;
    GetARICNSDesignations1: TMenuItem;
    GetArcSystemName: TMenuItem;
    MenuItem7: TMenuItem;
    Tycho2MagMI: TMenuItem;
    SDSSMagEntryMI: TMenuItem;
    SearchNotesMI: TMenuItem;
    SearchPlxSrcMI: TMenuItem;
    MenuItem8: TMenuItem;
    PllxImportMI: TMenuItem;
    SetBM_1MI: TMenuItem;
    SetBM_2MI: TMenuItem;
    GotoBM_1MI: TMenuItem;
    GotoBM_2MI: TMenuItem;
    UnresBinaryMI: TMenuItem;
    TGASBinarySecMatchMI: TMenuItem;
    StarToSysMI: TMenuItem;
    WrUnmCSVMI: TMenuItem;
    CatalogIDEditSystem: TCatalogIDEditFrame;
    LoadTGASCSV: TMenuItem;
    LoadSimpleTGAS: TMenuItem;
    MenuItem2: TMenuItem;
    SimbadIDLook: TMenuItem;
    SimbadLocLook: TMenuItem;
    SimpleTGASCSVexp: TMenuItem;
    TGASFind: TMenuItem;
    SystemEditorBox: TGroupBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Syste1: TMenuItem;
    Star1: TMenuItem;
    ListOfStarSystems: TListBox;
    SystemEditPageSet: TPageControl;
    SystemNames: TTabSheet;
    MainSystemName: TLabeledEdit;
    PreferredGroup: TRadioGroup;
    ProperSystemName: TLabeledEdit;
    ExtraNamesEdit: TLabeledEdit;
    StaticText1: TStaticText;
    SystemConstellation: TListBox;
    StaticText2: TStaticText;
    BayerPickList: TComboBox;
    BayerSuperscriptEdit: TMaskEdit;
    StaticText3: TStaticText;
    FlamsteedNum: TMaskEdit;
    VariableStarNameEdit: TLabeledEdit;
    NewSystem1: TMenuItem;
    DeleteSystem1: TMenuItem;
    AddStar1: TMenuItem;
    RemoveStar1: TMenuItem;
    OpenStarList: TOpenDialog;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    SaveStarList: TSaveDialog;
    Import1: TMenuItem;
    Export1: TMenuItem;
    RECONSNearest1001: TMenuItem;
    ools1: TMenuItem;
    RenumberIDs1: TMenuItem;
    SortbyDistance1: TMenuItem;
    N2: TMenuItem;
    AddISDBCatalogs1: TMenuItem;
    N3: TMenuItem;
    AddISDBCatalogNames1: TMenuItem;
    FindConstellation1: TMenuItem;
    N4: TMenuItem;
    FindbyName1: TMenuItem;
    N5: TMenuItem;
    FilterMain1: TMenuItem;
    RemoveFilters1: TMenuItem;
    N6: TMenuItem;
    ZeroRadialVelocity1: TMenuItem;
    N7: TMenuItem;
    ClosetothisSystem1: TMenuItem;
    FewCatalogNames1: TMenuItem;
    N8: TMenuItem;
    ShowGlobalNotes1: TMenuItem;
    N9: TMenuItem;
    ShowDistance1: TMenuItem;
    LocationUncertian1: TMenuItem;
    HasSpectralType1: TMenuItem;
    RenameCatalogs1: TMenuItem;
    TabSheet1: TTabSheet;
    Label16: TLabel;
    ClusterCheckList: TCheckListBox;
    FictionalNamesEdit: TLabeledEdit;
    StaticText5: TStaticText;
    PoliticalStatusMemo: TMemo;
    Label17: TLabel;
    ExtraNotesMemo: TMemo;
    C1: TMenuItem;
    EditClusters1: TMenuItem;
    NoAssociatedCluster1: TMenuItem;
    Name2300adEdit: TLabeledEdit;
    (* special method *)
    procedure AddBDMIClick(Sender: TObject);
    procedure APASSfluxMIClick(Sender: TObject);
    procedure APASSwSimMIClick(Sender: TObject);
    procedure BadSpectraMIClick(Sender: TObject);
    procedure BrownDwarfZMMIClick(Sender: TObject);
    procedure CatDupesScanMIClick(Sender: TObject);
    procedure ChangeStarToBDMIClick(Sender: TObject);
    procedure ComponentTabControlChange(Sender: TObject);
    procedure EnterPMPartsMIClick(Sender: TObject);
    procedure ExportListMIClick(Sender: TObject);
    procedure FindIDMIClick(Sender: TObject);
    procedure FindRemTGASMIClick(Sender: TObject);
    procedure GetLoggMIClick(Sender: TObject);
    procedure GotoBM_1MIClick(Sender: TObject);
    procedure GotoBM_2MIClick(Sender: TObject);
    procedure GuessSpectraMIClick(Sender: TObject);
    procedure HasProbMIClick(Sender: TObject);
    procedure ImportFrSN35MIClick(Sender: TObject);
    procedure ImportRemTGAS_MIClick(Sender: TObject);
    procedure ImportTGASLeftMIClick(Sender: TObject);
    procedure IndepLocationCBChange(Sender: TObject);
    procedure InsertStarMIClick(Sender: TObject);
    procedure ListofStarSystemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    (* menu item actions *)
    procedure ListOfStarSystemsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadSimpleTGASClick(Sender: TObject);
    procedure LoadTGASCSVClick(Sender: TObject);
    procedure LuminosityProbMIClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MergeIntoMIClick(Sender: TObject);
    procedure MISwapParallaxClick(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure ParallaxEntryMIClick(Sender: TObject);
    procedure PllxImportMIClick(Sender: TObject);
    procedure PosSepSetMIClick(Sender: TObject);
    procedure PPMMatchMIClick(Sender: TObject);
    procedure PreMainSeqCBChange(Sender: TObject);
    procedure SaveAs2MIClick(Sender: TObject);
    procedure SDSSMagEntryMIClick(Sender: TObject);
    procedure SearchNotesMIClick(Sender: TObject);
    procedure SearchPlxSrcMIClick(Sender: TObject);
    procedure SetBM_1MIClick(Sender: TObject);
    procedure SetBM_2MIClick(Sender: TObject);
    procedure SetMergeSrcMIClick(Sender: TObject);
    procedure ShowGalCoordMIClick(Sender: TObject);
    procedure ShowPosDegMIClick(Sender: TObject);
    procedure ShowUVWMIClick(Sender: TObject);
    procedure SimbadDataFetchClick(Sender: TObject);
    procedure SimbadIDFluxMIClick(Sender: TObject);
    procedure SimbadIDLookClick(Sender: TObject);
    procedure SimbadLocLookClick(Sender: TObject);
    procedure SimbadURLDataClick(Sender: TObject);
    procedure SimbURLCGIDClick(Sender: TObject);
    procedure SimpleTGASCSVexpClick(Sender: TObject);
    procedure StarExtraDataFrame1Click(Sender: TObject);
    procedure StarExtraDataFrame1Exit(Sender: TObject);
    procedure StarLocatFrame1Exit(Sender: TObject);
    procedure StarToSysMIClick(Sender: TObject);
    procedure SystemEditPageSetChange(Sender: TObject);
    procedure TestVizDMIClick(Sender: TObject);
    procedure TGASBinarySecMatchMIClick(Sender: TObject);
    procedure TGASFindClick(Sender: TObject);
    procedure NewSystem1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SystemConstellationClick(Sender: TObject);
    procedure PreferredGroupClick(Sender: TObject);
    procedure ProperSystemNameExit(Sender: TObject);
    procedure BayerPickListChange(Sender: TObject);
    procedure TGASNameMatchMIClick(Sender: TObject);
    procedure Tycho2MagMIClick(Sender: TObject);
    procedure UCAC4MagEstMIClick(Sender: TObject);
    procedure UnresBinaryMIClick(Sender: TObject);
    procedure UseSepLocatCBChange(Sender: TObject);
    procedure VariableStarNameEditExit(Sender: TObject);
    procedure ExtraNamesEditExit(Sender: TObject);
    procedure BayerSuperscriptEditExit(Sender: TObject);
    procedure FlamsteedNumExit(Sender: TObject);
    procedure MainSystemNameExit(Sender: TObject);

    procedure SystemNotesMemoExit(Sender: TObject);
    procedure VisualMagEntryClick(Sender: TObject);
    procedure WriteEstimatorMIClick(Sender: TObject);
    procedure WrUnmCSVMIClick(Sender: TObject);
    procedure AddStar1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure RemoveStar1Click(Sender: TObject);
    procedure RECONSNearest1001Click(Sender: TObject);
    procedure ProblemCBClick(Sender: TObject);
    procedure RenumberIDs1Click(Sender: TObject);
    procedure SortbyDistance1Click(Sender: TObject);
    procedure HasPlanetsCBClick(Sender: TObject);
    procedure AddISDBCatalogs1Click(Sender: TObject);
    procedure AddISDBCatalogNames1Click(Sender: TObject);
    procedure FindConstellation1Click(Sender: TObject);
    procedure StarDisplaySetChange(Sender: TObject);
    procedure FindbyName1Click(Sender: TObject);
    procedure LoadARICNSCatalogs1Click(Sender: TObject);
    procedure GetARICNSStarDataClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure DeleteSystem1Click(Sender: TObject);
    procedure AddBinaryNote1Click(Sender: TObject);
    procedure RemoveFilters1Click(Sender: TObject);
    procedure ZeroRadialVelocity1Click(Sender: TObject);
    procedure ClosetothisSystem1Click(Sender: TObject);
    procedure FewCatalogNames1Click(Sender: TObject);
    procedure ShowGlobalNotes1Click(Sender: TObject);
    procedure ShowDistance1Click(Sender: TObject);
    procedure LocationUncertian1Click(Sender: TObject);
    procedure GetARICNSDesignations1Click(Sender: TObject);
    procedure HasSpectralType1Click(Sender: TObject);
    procedure RenameCatalogs1Click(Sender: TObject);
    procedure ARICNSDiff1Click(Sender: TObject);
    procedure UseNameBtnClick(Sender: TObject);
    procedure FictionalNamesEditExit(Sender: TObject);
    procedure PoliticalStatusMemoExit(Sender: TObject);
    procedure ExtraNotesMemoExit(Sender: TObject);
    procedure ClusterCheckListClickCheck(Sender: TObject);
    procedure EditClusters1Click(Sender: TObject);
    procedure NoAssociatedCluster1Click(Sender: TObject);
    procedure Name2300adEditExit(Sender: TObject);
    procedure GetArcSystemNameClick(Sender: TObject);
  private
    { Private declarations }
    supstab:Boolean;
    parallax_source:string;
    procedure StarParallaxChanged(Sender: TObject);
    procedure MainParallaxChange(Sender: TObject);
    procedure FluxTEffChange(Sender:TObject);
    function SimbadIDFetch(option:Integer):Boolean;
  public
    (* checking for mofocation and saving *)
    procedure StarModCheck(Sender: TObject);
    procedure SystemModCheck(Sender: TObject);
    procedure GeneralModCheck(Sender: TObject);
    (* enabling and diabling *)
    procedure EnablePN_AtIndex(p_index:Integer; value:Boolean);
    procedure PrefNameED;
    procedure LoadPrefNames;
    { Public declarations }
    // loading system data
    procedure SystemLoad1;
    procedure SystemLoad2;
    procedure SystemLoad3;
    procedure SystemLoad4;
    procedure SystemLoad5;
    procedure SystemEnableDisable;
    // loading star data
    procedure StarData1;
    procedure StarData3;
    procedure LoadAllStarData;
    // loading a whole system
    procedure ChangeSystem;
    // filters menu checks removal
    procedure UncheckFilters;
    procedure NewLocChangeCheck;
  end;

var
  NStarsMainForm: TNStarsMainForm;
  firststartup,loadlist:Boolean;
  checkbak:array of Boolean;

const
  CSVFILTER = 'Comma Separated Values (*.csv)|*.csv';
  NSL2FILTER = 'NStars List 2 (*.nsl2)|*.nsl2';
  TXTFILTER = 'Text File (*.txt)|*.txt';
  DATXFILTER = 'VizieR Data Table (*.dat)|*.dat';

implementation

{$R *.lfm}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
procedure TNStarsMainForm.ListofStarSystemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var okaydex, makebold:Boolean;
begin
  // checking to see if we can even get the star system to be displayed
  okaydex := (Index >= 0) and (primaryl <> nil);
  if okaydex then okaydex:= (Index < primaryl.GetCount);
  makebold := primaryl.BoldAtFilteredIndex(Index);
  //displaying
  if makebold then ListofStarSystems.Canvas.Font.Style := [fsBold]
  else ListofStarSystems.Canvas.Font.Style := [];
  ListofStarSystems.Canvas.FillRect(Rect);
  ListofStarSystems.Canvas.TextOut( Rect.Left + 8, Rect.Top, ListofStarSystems.Items[Index] );
end;

procedure TNStarsMainForm.IndepLocationCBChange(Sender: TObject);
var cshl:Boolean;
begin
  cshl := (current.cstarl <> nil);
  if cshl <> UseSepLocatCB.Checked then begin
    // eliminating existing location
    if cshl then begin
      StarLocatFrame1.ChangeLocation(nil);
      StarLocatFrame1.Enabled := False;
      current.ccomponent.ClearLocation;
      current.cstarl := nil;
    end
    // copying and setting new star location
    else begin
      current.ccomponent.CopyLocation(current.sys.GetLocation,True);
      current.cstarl := current.ccomponent.GetLocation;
      StarLocatFrame1.Enabled := True;
      StarLocatFrame1.ChangeLocation(current.cstarl);
    end;
  end;
end;

procedure TNStarsMainForm.InsertStarMIClick(Sender: TObject);
var arity,I:Integer;
    buf:string;
begin
  StarModCheck(Sender);
  // we add a star using the proxy
  current.InsertStar;
  arity := current.sys.GetCompC;
  // next, we alter the tab labels (including adding a new one)...
  ComponentTabControl.Tabs.Add('');
  for I := current.starindex to arity do begin
    Str(I,buf);
    if current.sys.IsBrownDwarf(I) then buf := 'Brown Dwarf ' + buf
    else buf := 'Star ' + buf;
    ComponentTabControl.Tabs[I-1] := buf;
  end;
  // reloading star data
  LoadAllStarData;
  RemoveStar1.Enabled := True;
  // disabing menus if necessary
  if (adat.loaded) then GetArcSystemName.Enabled := True;
end;

procedure TNStarsMainForm.AddBDMIClick(Sender: TObject);
var arity:Integer;
    buf:string;
begin
  StarModCheck(Sender);
  // we add a star using the proxy
  current.AddBrownDwarf;
  // next, we add a tab to the star info display
  arity := current.sys.GetCompC;
  Str(arity,buf);
  buf := 'Brown Dwarf ' + buf;
  ComponentTabControl.Tabs.Add(buf);
  // setting the value
  ComponentTabControl.TabIndex := (arity-1);
  // loading star data
  LoadAllStarData;
  RemoveStar1.Enabled := True;
  // disabing menus if necessary
  if (adat.loaded) then GetArcSystemName.Enabled := True;
end;

procedure TNStarsMainForm.APASSfluxMIClick(Sender: TObject);
var data:string;   rok:Boolean;
const entmsg = 'Enter APASS Magnitudes ([BV]g′r′i′ , with errors)' + sLineBreak + 'cut and pasted from VizieR below :';
begin
  if current.cstar <> nil then begin
    data := Trim(InputBox('APASS Entry',entmsg,''));
    if Length(data)<>0 then begin
       rok := current.APASS_Helper(data);
    end;
    if rok then begin
      // reloading after data has been set
      StarData1;
    end;
  end;
end;

procedure TNStarsMainForm.APASSwSimMIClick(Sender: TObject);
var vizres,sptchange:Boolean;
begin
  if current <> nil then begin
    vizres := current.VizierAPASSGet(True);
    if vizres then begin
      current.GuessSpectra(sptchange);
      if not sptchange then current.sys.UpdateEstimates;
      StarData1;
    end;
  end;
end;

procedure TNStarsMainForm.BadSpectraMIClick(Sender: TObject);
begin
  UncheckFilters;
  BadSpectraMI.Checked := True;
  primaryl.SpectralProblems;
  if current.sys <> nil then ChangeSystem;
end;

procedure TNStarsMainForm.BrownDwarfZMMIClick(Sender: TObject);
begin
  UncheckFilters;
  BrownDwarfZMMI.Checked := True;
  primaryl.NoMassBrownDwarfs;
  if current.sys <> nil then ChangeSystem;
end;

procedure TNStarsMainForm.CatDupesScanMIClick(Sender: TObject);
begin
  if FindCatDuplForm<>nil then FreeAndNil(FindCatDuplForm);
  FindCatDuplForm := TFindCatDuplForm.Create(Self);
  FindCatDuplForm.Show();
end;

procedure TNStarsMainForm.ChangeStarToBDMIClick(Sender: TObject);
var cok:Boolean;  tabindex:Integer;
    buf:string;
begin
  cok := current.CurrentToBrownDwarf;
  if cok then begin
    tabindex := current.starindex-1;
    Str(tabindex+1,buf);
    buf := 'Brown Dwarf ' + buf;
    ComponentTabControl.Tabs[tabindex] := buf;
    LoadAllStarData;
  end;
end;

procedure TNStarsMainForm.ComponentTabControlChange(Sender: TObject);
var ztabdex,ztabcount:Integer;
    xtext:string;
begin
  if supstab then Exit;
  xtext := ComponentTabControl.Tabs.Text;
  if xtext = '' then Exit;
  StarModCheck(Sender);
  ztabdex := ComponentTabControl.TabIndex;
  if ztabdex < 0 then Exit;
  ztabcount := ComponentTabControl.Tabs.Count;
  current.SetStar(ComponentTabControl.TabIndex+1);
  LoadAllStarData;
  PosSepSetMI.Enabled := (ComponentTabControl.TabIndex > 0);
end;


procedure TNStarsMainForm.EnterPMPartsMIClick(Sender: TObject);
var data1,data2:string;   rok:Boolean;
    pmra,pmdec,pmmag,pmang:Real;
const entmsg = 'Enter RA and Dec Proper Motions components (in'+#13#10
  +'milli-arcsec, separated by a space) below:';
begin
  if current.sysl <> nil then begin
    data2 := Trim(InputBox('PM Entry',entmsg,''));
    rok := ExtractFirstWord(data2,data1);
    if rok then begin
      rok := StrToRealBoth(data1,data2,pmra,pmdec);
      if rok then begin
        ProperMotionConvert(pmdec,pmra,pmmag,pmang);
        rok := current.sysl.SetProperMotion(pmmag,pmang);
      end;
      if rok then SystemLoad3;
    end;
    if rok then ShowMessage('Proper Motion set.')
    else ShowMessage('Proper Motion not set!');
  end;
end;

procedure TNStarsMainForm.ExportListMIClick(Sender: TObject);
begin
  if primaryl <> nil then begin
    if ExportForm = nil then ExportForm := TExportForm.Create(Self);
    ExportForm.Show;
  end;
end;

procedure TNStarsMainForm.FindIDMIClick(Sender: TObject);
var cdex,lookfor,sc:Integer;
    data:string;
    badinput:Boolean;
    // os:StarSystem;
begin
  GeneralModCheck(Sender);
  // we go looking
  data := InputBox('Search Box','Enter a system id to look for:','');
  data := Trim(data);
  // converting
  Val(data,lookfor,sc);
  badinput := (sc<>0);
  if (not badinput) then badinput := (lookfor < 2);
  if badinput then begin
    ShowMessage('Input is not a valid id!');
    Exit;
  end;
  // looking for it
  Screen.Cursor := crHourGlass;
  cdex := primaryl.FindByID(lookfor);
  // we have found it
  if cdex<>-1 then begin
    (* we change the current star system! *)
    // os := current.sys;
    primaryl.ChangeSystem(cdex);
    Screen.Cursor := crDefault;
    ChangeSystem;
    ListOfStarSystems.ItemIndex := cdex;
  end
  else begin
    Screen.Cursor := crDefault;
    ShowMessage('Nothing found!');
  end;
end;
//---------------------------------------------------------------

procedure TNStarsMainForm.FindRemTGASMIClick(Sender: TObject);
var pllxok:Boolean;       sc,sstar_where:Integer;
    pvalue:Real;          pstring:string;
    curtgas:TGASData;      idtofind:string;
const promptstr = 'Enter the minimum Parallax (in arcseconds).';
begin
  // getting the minimum parallax
  pllxok := not (tgas_main = nil);
  if pllxok then pllxok := (tgas_main.StarCount<>0);
  if not pllxok then begin
    ShowMessage('Nothing to check!');
    Exit;
  end;
  // getting maximum distance info
  pstring := Trim(InputBox('Parallax Entry',promptstr,''));
  pllxok :=(Length(pstring)<>0);
  if pllxok then begin
    Val(pstring,pvalue,sc);
    pllxok:= (sc=0);
    if pllxok then pllxok :=(pvalue>0);
  end;
  // possible displaying an error message
  if (not pllxok) then begin
    ShowMessage('Invalid Parallax');
    Exit;
  end;
  // make the remainder list
  Screen.Cursor := crHourGlass;
  pllxok := tgas_main.MakeUnmatched(pvalue);
  Screen.Cursor := crDefault;
  if (not pllxok) then begin
    ShowMessage('No unmatched TGAS positions left!');
    Exit;
  end;
  // looping over the unmatches tgas results...
  Screen.Cursor := crHourGlass;
  curtgas := tgas_main.NextUnmatched;
  while (curtgas<>nil) do begin
    // identifier to search for
    idtofind := curtgas.GetHipparcos;
    if Length(idtofind)<>0 then idtofind := 'Hip ' + idtofind
    else idtofind := 'Tyc ' + curtgas.GetTycho2;
    // searching
    sc := primaryl.FindByString(idtofind,sstar_where);
    if sc>=0 then begin
      // a system with the id has been found, we move to it...
      primaryl.ChangeSystem(sc);
      ChangeSystem;
      ListOfStarSystems.ItemIndex := sc;
      // we next fire up tgas matching
      Screen.Cursor := crDefault;
      TGASFindClick(Self);
      Screen.Cursor := crHourGlass;
    end;
    // next loop prep
    curtgas := tgas_main.NextUnmatched
  end;
  Screen.Cursor := crDefault;
  ShowMessage('All Done');
end;

procedure TNStarsMainForm.GetLoggMIClick(Sender: TObject);
begin
  SimbadIDFetch(2);
end;

procedure TNStarsMainForm.GotoBM_1MIClick(Sender: TObject);
begin
  GeneralModCheck(Sender);
  primaryl.GoToBookmark(0);
  ChangeSystem;
end;

procedure TNStarsMainForm.GotoBM_2MIClick(Sender: TObject);
begin
  GeneralModCheck(Sender);
  primaryl.GoToBookmark(1);
  ChangeSystem;
end;

procedure TNStarsMainForm.GuessSpectraMIClick(Sender: TObject);
var specres,xused:Boolean;
begin
  specres := False;
  if (current<> nil) then specres := current.GuessSpectra(xused);
  if (not specres) then ShowMessage('Unable to guess spectra!')
  else if xused then begin
    StarData1;
  end;
end;

procedure TNStarsMainForm.HasProbMIClick(Sender: TObject);
begin
  UncheckFilters;
  HasProbMI.Checked := True;
  primaryl.HasProblems;
  if current.sys <> nil then  ChangeSystem;
end;

procedure TNStarsMainForm.ImportFrSN35MIClick(Sender: TObject);
var filterbak,omsg:string;
    inputsource:TFileName;
    maindata:Sn35PhotoList;
    current:Sn35Photometry;
    ldok,nfound:Boolean;
begin
  // show file open to get filename
  filterbak := OpenStarList.Filter;
  OpenStarList.Filter := DATXFILTER;
  if OpenStarList.Execute then begin
    inputsource := OpenStarList.FileName;
    // create and load object
    Screen.Cursor := crHourGlass;
    maindata := Sn35PhotoList.Create;
    ldok := maindata.LoadFromFile(inputsource);
    if not ldok then begin
      FreeAndNil(maindata);
      Screen.Cursor := crDefault;
      ShowMessage('Unable to get anything from the file!');
    end else begin
      omsg := IntToStr(maindata.StarCount) + ' Entries will be checked for importing.';
      Screen.Cursor := crDefault;
      ShowMessage(omsg);
      Screen.Cursor := crHourGlass;
      // looping over the items
      while maindata.NextIndex do begin
        current := maindata.CurrentStar;
        ldok := HandleSN35PhotometryImport(current,nfound);
        maindata.UpdateCounts(ldok,nfound);
      end;
     // show final message
     omsg := 'The Import is done.   ' + IntToStr(maindata.updated) + ' stars ';
     omsg += 'have been updated,' + sLineBreak + IntToStr(maindata.notfound);
     omsg += 'entries have not been matched, and ' + IntToStr(maindata.skipped);
     omsg += ' stars have been skipped.';
     Screen.Cursor := crDefault;
     ShowMessage(omsg);
    end;
  end;
  OpenStarList.Filter := filterbak;
end;

procedure TNStarsMainForm.ImportRemTGAS_MIClick(Sender: TObject);
var pllxok:Boolean;       sc:Integer;
    pvalue:Real;          pstring:string;
    curtgas:TGASData;     msys:StarSystem;
    madex,nmadex:Integer;
const promptstr = 'Enter the minimum Parallax (in arcseconds).';
begin
  // getting the minimum parallax
  pllxok := not (tgas_main = nil);
  if pllxok then pllxok := (tgas_main.StarCount<>0);
  if not pllxok then begin
    ShowMessage('Nothing to check!');
    Exit;
  end;
  // getting maximum distance info
  pstring := Trim(InputBox('Parallax Entry',promptstr,''));
  pllxok :=(Length(pstring)<>0);
  if pllxok then begin
    Val(pstring,pvalue,sc);
    pllxok:= (sc=0);
    if pllxok then pllxok :=(pvalue>0);
  end;
  // possible displaying an error message
  if (not pllxok) then begin
    ShowMessage('Invalid Parallax');
    Exit;
  end;
  // make the remainder list
  Screen.Cursor := crHourGlass;
  pllxok := tgas_main.MakeUnmatched(pvalue);
  if (not pllxok) then begin
    Screen.Cursor := crDefault;
    ShowMessage('No unmatched TGAS positions left!');
    Exit;
  end;
  // looping over the unmatches tgas results...
  madex := 0;
  nmadex := 0;
  curtgas := tgas_main.NextUnmatched;
  while (curtgas<>nil) do begin
    // identifier to search for
    msys := TGASToStarSystem(curtgas);
    if msys = nil then Inc(nmadex)
    else begin
      primaryl.AppendSystem(msys);
      Inc(madex);
    end;
    // next loop prep
    curtgas := tgas_main.NextUnmatched
  end;
  Screen.Cursor := crDefault;
  ShowMessage('All Done. ' + IntToStr(madex) + ' stars added. '
                         + IntToStr(nmadex) + ' stars skipped.');
end;

procedure TNStarsMainForm.ImportTGASLeftMIClick(Sender: TObject);
var pllxok:Boolean;       sc:Integer;
    pvalue:Real;          pstring:string;
    curtgas:TGASData;     msys:StarSystem;
    madex,nmadex:Integer;
const promptstr = 'Enter the minimum Parallax (in arcseconds).';
begin
  // getting the minimum parallax
  pllxok := not (tgas_main = nil);
  if pllxok then pllxok := (tgas_main.StarCount<>0);
  if not pllxok then begin
    ShowMessage('Nothing to check!');
    Exit;
  end;
  // getting maximum distance info
  pstring := Trim(InputBox('Parallax Entry',promptstr,''));
  pllxok :=(Length(pstring)<>0);
  if pllxok then begin
    Val(pstring,pvalue,sc);
    pllxok:= (sc=0);
    if pllxok then pllxok :=(pvalue>0);
  end;
  // possible displaying an error message
  if (not pllxok) then begin
    ShowMessage('Invalid Parallax');
    Exit;
  end;
  // make the remainder list
  Screen.Cursor := crHourGlass;
  pllxok := tgas_main.MakeUnmatched(pvalue);
  if (not pllxok) then begin
    Screen.Cursor := crDefault;
    ShowMessage('No unmatched TGAS positions left!');
    Exit;
  end;
  // looping over the unmatches tgas results...
  madex := 0;
  nmadex := 0;
  curtgas := tgas_main.NextUnmatched;
  while (curtgas<>nil) do begin
    // identifier to search for
    msys := TGASToStarSystem(curtgas);
    if msys = nil then Inc(nmadex)
    else begin
      primaryl.AppendSystem(msys);
      Inc(madex);
    end;
    // next loop prep
    curtgas := tgas_main.NextUnmatched
  end;
  Screen.Cursor := crDefault;
  ShowMessage('All Done. ' + IntToStr(madex) + ' stars added. '
                         + IntToStr(nmadex) + ' stars skipped.');
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TNStarsMainForm.StarParallaxChanged(Sender: TObject);
begin
  StarDataCoreFrame.ExternalChange;
end;
//-------------------------------------------------
procedure TNStarsMainForm.MainParallaxChange(Sender: TObject);
var pupd:Boolean;
begin
  StarDataCoreFrame.ExternalChange;
  pupd := current.sys.BinaryLocationUpdate;
  if pupd then StarLocatFrame1.ReloadData;
end;
//--------------------------------------------------
procedure TNStarsMainForm.FluxTEffChange(Sender:TObject);
begin
  StarDataCoreFrame.ExternalChange;
end;
//--------------------------------------------------
function TNStarsMainForm.SimbadIDFetch(option:Integer):Boolean;
var catstring,simbadurl:string;
    doreload:Boolean;
begin
  // passing conditions
  Result := False;
  if current.ccomponent = nil then Exit;
  // starting to get the id...
  Screen.Cursor := crHourGlass;
  if current.cstarn <> nil then begin
    catstring := current.cstarn.GetDefaultCatalog;
    if Length(catstring) = 0 then catstring := current.sysn.GetDefaultCatalog;
  end
  else catstring := current.sysn.GetDefaultCatalog;
  // no catalog id!
  if Length(catstring) = 0 then begin
    Screen.Cursor := crDefault;
    ShowMessage('Star does not have the right catalog IDs!');
    Exit;
  end;
  // creating the url
  simbadurl := MakeSimbadIdLookupURL(catstring);
  // fetching and stting the data...
  if option<>2 then doreload := current.SimbadDownload(simbadurl,option=1)
  else doreload := current.SimbadDownloadLogg(simbadurl);
  //reloading data
  if doreload then begin
    if (option=0) then begin
      SystemLoad1;
      SystemLoad3;
      SystemLoad4;
    end;
    StarData1;
  end;
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TNStarsMainForm.StarModCheck(Sender: TObject);
begin
  // ne stuff
  // StarCoreDataFrame.SaveData;
  StarDataCoreFrame.SaveData;
  if current.sys.GetId <> 1 then begin
     StarExtraDataFrame1.SaveValues(False);
     if current.cstar <> nil then current.cstar.premain := PreMainSeqCB.Checked;
     StarLocatFrame1.SaveData(False);
     StarFluxTEffFrame.SaveExternal;
  end;
end;
//---------------------------------------------------
procedure TNStarsMainForm.SystemModCheck(Sender: TObject);
begin
  // location data
  MainLocatEditFrame1.SaveAll(False);
  // names
  if ProperSystemName.Modified then ProperSystemNameExit(Sender);
  if VariableStarNameEdit.Modified then VariableStarNameEditExit(Sender);
  if ExtraNamesEdit.Modified then ExtraNamesEditExit(Sender);
  if BayerSuperscriptEdit.Modified then BayerSuperscriptEditExit(Sender);
  if FlamsteedNum.Modified then FlamsteedNumExit(Sender);
  if MainSystemName.Modified then MainSystemNameExit(Sender);
  // notes
  if SystemNotesMemo.Modified then SystemNotesMemoExit(Sender);
  // fictional stuff
  if ExtraNotesMemo.Modified then ExtraNotesMemoExit(Sender);
  if FictionalNamesEdit.Modified then FictionalNamesEditExit(Sender);
  if Name2300adEdit.Modified then Name2300adEditExit(Sender);
  if PoliticalStatusMemo.Modified then PoliticalStatusMemoExit(Sender);

end;
//--------------------------------------------------------------
procedure TNStarsMainForm.GeneralModCheck(Sender: TObject);
begin
  StarModCheck(Sender);
  SystemModCheck(Sender);
end;
//-----------------------------------------------------------------
procedure TNStarsMainForm.GetARICNSDesignations1Click(Sender: TObject);
const catfound='Catalog designations found are listed below:';
var msgvalue:string;
    nummsg, bval:string;
    resvalue:Boolean;
    inarity,outarity, mval:Integer;
    Save_Cursor:TCursor;
begin
  // set the hourglass...
  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  // finding the specific arity
  inarity := current.sys.GetCompC;
  if (inarity = 1) then mval := 0
  else mval := 1;
  // value displaying and getting OK or cancel
  msgvalue := FindDesignations(current.sys,current.starindex, mval);
  if msgvalue='' then begin
    if (inarity=1) then begin
      // a common reason for not finding things is a mismatch is the arity
      outarity := FindAricnsArity(current.sys);
      // building the result message
      if (outarity = 0) then nummsg := 'The System was not found!'
      else if (outarity = 1) then nummsg := 'The System was found, but there is uncertainty in components. Check ARICNS manually'
      else begin
        Str(outarity,bval);
        nummsg := 'The arity of the System should be ' + Trim(bval) + '. Please increase the arity to match!';
      end;
    end
    else nummsg := 'No designations found for this component!';
    // showing a message
    Screen.Cursor := Save_Cursor;
    ShowMessage(nummsg);
  end
  else begin
    Screen.Cursor := Save_Cursor;
    resvalue := InputQuery('ARICNS Names',catfound,msgvalue);
    // if the conditions are met
    if resvalue and (inarity=1) then begin
      CatalogIDEditSystem.AddCatalogNamesExternal(msgvalue,False);
    end
    else if resvalue then begin
      StarCatIDFrame.AddCatalogNamesExternal(msgvalue,False);
    end;
  end;
end;

procedure TNStarsMainForm.GetARICNSStarDataClick(Sender: TObject);
begin
  loadlist := False;
  ARICNS_CheckSystem := current.sys;
  ARICNS_CheckStar := current.starindex;
  ArcinsDataDisplay := TArcinsDataDisplay.Create(Self);
  ArcinsDataDisplay.Show;
end;
//----------------------------------------------------------------------
procedure TNStarsMainForm.HasPlanetsCBClick(Sender: TObject);
begin
  current.sys.has_planets := HasPlanetsCB.Checked;
end;

procedure TNStarsMainForm.HasSpectralType1Click(Sender: TObject);
const prompt1='Enter a string to find in the spectra: ';
      prompt2='The value entered is not valid. Too Bad.';
var lystring:string;
begin
  // getting the data
  lystring:=InputBox('Spectral Search Entry', prompt1, '');
  lystring := Trim(lystring);
  if lystring<>'' then begin
    UncheckFilters;
    HasSpectralType1.Checked := True;
    primaryl.SearchSpectra(lystring);
    ChangeSystem;
  end
  else begin
    ShowMessage(prompt2);
  end;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TNStarsMainForm.ZeroRadialVelocity1Click(Sender: TObject);
begin
  UncheckFilters;
  ZeroRadialVelocity1.Checked := True;
  primaryl.RadVIsZeroFilter;
  if current.sys <> nil then ChangeSystem;
end;

//----------------------- -----------------------------
procedure TNStarsMainForm.PoliticalStatusMemoExit(Sender: TObject);
begin
  current.sys.political_ownrs := PoliticalStatusMemo.Text;
  PoliticalStatusMemo.Modified := False;
end;
//-----------------------------------------------------------------------
procedure TNStarsMainForm.PreferredGroupClick(Sender: TObject);
var id:Integer;
begin
  id := PreferredGroup.ItemIndex;
  current.sys.preferred := id;
  // changing the preferred name
  LoadPrefNames;
end;

// procedure to enable or disbale individual items in the PreferredGroup
procedure TNStarsMainForm.EnablePN_AtIndex(p_index:Integer; value:Boolean);
var rb:TRadioButton;
begin
  rb := (PreferredGroup.Controls[p_index] as TRadioButton);
  rb.Enabled := value;
end;

procedure TNStarsMainForm.PrefNameED;
begin
  if (current.sys.GetId = 1) then Exit;
  if current.sysn.proper_name='' then EnablePN_AtIndex(1,False)
  else EnablePN_AtIndex(1,True);
  if current.sys.bayer_desig=0 then EnablePN_AtIndex(2,False)
  else EnablePN_AtIndex(2,True);
  if current.sys.flamsteed_number=0 then EnablePN_AtIndex(3,False)
  else EnablePN_AtIndex(3,True);
  if current.sysn.var_designation='' then EnablePN_AtIndex(4,False)
  else EnablePN_AtIndex(4,True);
end;

procedure TNStarsMainForm.ProperSystemNameExit(Sender: TObject);
begin
  current.sysn.proper_name := ProperSystemName.Text;
  PrefNameED;
  LoadPrefNames;
  ProperSystemName.Modified := False;
end;

//-----------------------------------------------------

procedure TNStarsMainForm.RECONSNearest1001Click(Sender: TObject);
var qfile:TFileName;
begin
  GeneralModCheck(Sender);
  if OpenStarList.Execute then begin
    qfile := OpenStarList.FileName;
    primaryl.GetFromRecons(qfile);
    Save1.Enabled := false;
  end;
end;

procedure TNStarsMainForm.RemoveFilters1Click(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  UncheckFilters;
  primaryl.CopyToFiltered;
  ChangeSystem;
  Screen.Cursor := crDefault;
end;

procedure TNStarsMainForm.RemoveStar1Click(Sender: TObject);
var arity,I:Integer;
    buf:string;
begin
  StarModCheck(Sender);
  // trying to nullify to avoid issues
  StarFluxTEffFrame.SetToNothing;
  StarDataCoreFrame.ChangeStar(nil,nil,1);
  // we delete a star using the proxy
  current.DeleteStar(ComponentTabControl.TabIndex+1);
  supstab := True;
  // next, we redo the tabs for the star info display
  arity := current.sys.GetCompC;
  ComponentTabControl.Tabs.Text :='';
  for I := 1 to arity do begin
    Str(I,buf);
    if current.sys.IsBrownDwarf(I) then buf := 'Brown Dwarf ' + buf
    else buf := 'Star ' + buf;
    ComponentTabControl.Tabs.Add(buf);
  end;
  // setting the value
  ComponentTabControl.TabIndex := 0;
  if (arity=1) then RemoveStar1.Enabled := False;  
  // loading star data
  supstab := False;
  LoadAllStarData;
  // disabing menus if necessary
  arity := current.sys.GetCompC;
  if (arity = 1) then GetArcSystemName.Enabled := False;
end;

procedure TNStarsMainForm.RenameCatalogs1Click(Sender: TObject);
const prompt1 = 'Enter a semicolon delimited list of catalog pairs.'#13#10;
      prompt2 = 'The first the old name, the second the new one.';
      prompt3 = ' catalog entries were renamed.';
var   msgvalue,buf:string;
      tres:Integer;
      resvalue:Boolean;
begin
  msgvalue := '';
  // getting the list and getting OK or cancel
  resvalue := InputQuery('Rename Catalogs',prompt1+prompt2,msgvalue);
  // if the conditions are met
  if resvalue then begin
    tres := primaryl.RenameCatalogs(msgvalue);
    if tres<>-1 then begin
      Str(tres,buf);
      buf := Trim(buf) + prompt3;
      ShowMessage(buf);
      // reloading the catalog entries
      CatalogIDEditSystem.ChangeObject(current.sys);
    end
    else ShowMessage('The input was not properly formatted!');
  end;
end;

procedure TNStarsMainForm.RenumberIDs1Click(Sender: TObject);
begin
  primaryl.ReID;
end;

//-------------------------------------------------
procedure TNStarsMainForm.Open1Click(Sender: TObject);
var xfile:TFileName;
begin
  GeneralModCheck(Sender);
  if OpenStarList.Execute then begin
    Screen.Cursor := crHourGlass;
    xfile := OpenStarList.FileName;
    primaryl.OpenFromFile(xfile);
    NStarsMainForm.Caption := 'NStars - (' + xfile + ')';
    Save1.Enabled := True;
    ChangeSystem;
    Screen.Cursor := crDefault;
  end;
end;

//-------------------------------------------------
procedure TNStarsMainForm.MainSystemNameExit(Sender: TObject);
begin
  current.sys.System_name := MainSystemName.Text;
  LoadPrefNames;
  MainSystemName.Modified := False;
end;
//-----------------------------------------------------------------
procedure TNStarsMainForm.LoadPrefNames;
var id:Integer;
begin
  id := current.sys.preferred;
  // changing the preferred name
  MainSystemName.Text := current.sys.GetPreferredName;
  // disabling the display
  if (id>0) then MainSystemName.Enabled := False
  else MainSystemName.Enabled := True;
  // chaning the list box label
  ListOfStarSystems.Items[ListOfStarSystems.ItemIndex] := MainSystemName.Text;
  SystemEditorBox.Caption := MainSystemName.Text;
end;
//--------------------------------------------------------------
procedure TNStarsMainForm.LocationUncertian1Click(Sender: TObject);
begin
  UncheckFilters;
  LocationUncertian1.Checked := True;
  primaryl.ParallaxUncertain;
  if current.sys <> nil then  ChangeSystem;
end;

//------------------------------------------------------------
procedure TNStarsMainForm.NewSystem1Click(Sender: TObject);
begin
  GeneralModCheck(Sender);
  primaryl.AddNewSystem;
  ChangeSystem;
end;

procedure TNStarsMainForm.NoAssociatedCluster1Click(Sender: TObject);
begin
  UncheckFilters;
  NoAssociatedCluster1.Checked := True;
  primaryl.NoCluster;
  ChangeSystem;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++
// loading system data
//--------------------------------------------------
(* we break up loading info into parts for not much reason... *)
procedure TNStarsMainForm.SystemLoad1;
var id:Integer;
    buf:string;
begin
  // the constellation
  id := current.sys.constellation;
  SystemConstellation.ItemIndex := (id-1);
  // the proper name
  if (current.sys.GetId = 1) then ProperSystemName.Text := 'Sol'
  else ProperSystemName.Text := current.sysn.proper_name;
  // bayer info
  BayerPickList.ItemIndex := current.sys.bayer_desig;
  if (current.sys.GetId = 1) then buf := '0'
  else Str(current.sysn.bayer_sup,buf);
  BayerSuperscriptEdit.Text := Trim(buf);
  // the flamsteed number
  Str(current.sys.flamsteed_number,buf);
  FlamsteedNum.Text := Trim(buf);
  // issues
  ProblemCB.Checked := current.sys.has_problems;
  // has planets
  HasPlanetsCB.Checked := current.sys.has_planets;
  // aricns and simbad
  AricnsDataCB.Enabled:= True;
  AricnsDataCB.Checked:= current.sys.aricns_data;
  AricnsDataCB.Enabled:= False;
  SimbadDataCB.Enabled:= True;
  SimbadDataCB.Checked:= current.sys.simbad_data;
  SimbadDataCB.Enabled:= False;
end;
//--------------------------------------------------
procedure TNStarsMainForm.SystemLoad2;
var id:Integer;
    buf:string;
begin
  // variable star names
  if (current.sys.GetId = 1) then buf := ''
  else buf := current.sysn.var_designation;
  VariableStarNameEdit.Text := buf;
  // extra names
  ExtraNamesEdit.Text := current.sys.extra_names;
  // the preferred indicator
  id := current.sys.preferred;
  PreferredGroup.ItemIndex := id;
  // we now load the system name based on the preferred value
  MainSystemName.Text := current.sys.GetPreferredName;
  // next, we set the caption of the surrounding group box
  SystemEditorBox.Caption := ('Edit System : ' + MainSystemName.Text) + '  [ ';
  if (current.sys.GetId = 1) then begin
    SystemEditorBox.Caption := SystemEditorBox.Caption + 'G2V ]';
  end else begin
    SystemEditorBox.Caption := SystemEditorBox.Caption + (current.sys.GetSpTList());
    SystemEditorBox.Caption := SystemEditorBox.Caption + ' ] ';
  end;
end;
//--------------------------------------------------
procedure TNStarsMainForm.SystemLoad3;
begin
  // we get to work loading location data
  if current.sys.GetId = 1 then MainLocatEditFrame1.ChangeToNothing
  else MainLocatEditFrame1.ChangeLocation(current.sysl);
  // done!
end;
//--------------------------------------------------
procedure TNStarsMainForm.SystemLoad4;
begin
  // catalog names
  CatalogIDEditSystem.ChangeObject(current.sys);
  // we set the notes
  SystemNotesMemo.Text := current.sys.notes;
end;
//-------------------------------------------------------------------------
// loading fictional stuff
procedure TNStarsMainForm.SystemLoad5;
var cname:TStringList;
    I,cc:Integer;
    buf:string;
    ctest:Boolean;
begin
  // the simple bits: strings
  FictionalNamesEdit.Text := current.sys.fictional_names;
  Name2300adEdit.Text := current.sys.name2300ad;
  PoliticalStatusMemo.Text := current.sys.political_ownrs;
  ExtraNotesMemo.Text := current.sys.description;
  // the check box
  cname := MakeClusterNamesList;
  ClusterCheckList.Items := cname;
  cc := Length(cluster_list);
  SetLength(checkbak,cc);
  // setting the checked items
  for I := 0 to cc - 1 do begin
    buf := cname[I];
    ctest := current.sys.InCluster(buf);
    checkbak[I] := ctest;
    ClusterCheckList.Checked[I] := ctest; 
  end;
end;
//-------------------------------------------------------------------------

procedure TNStarsMainForm.SystemNotesMemoExit(Sender: TObject);
begin
  current.sys.notes := SystemNotesMemo.Text;
  SystemNotesMemo.Modified := False;
end;

procedure TNStarsMainForm.VisualMagEntryClick(Sender: TObject);
begin

end;

procedure TNStarsMainForm.WriteEstimatorMIClick(Sender: TObject);
begin
  if primaryl = nil then Exit;
  Screen.Cursor := crHourGlass;
  primaryl.WriteEstimatorData();
  Screen.Cursor := crDefault;
  ShowMessage('Estimator info written to estdata.csv and estdata.csv');
end;

procedure TNStarsMainForm.WrUnmCSVMIClick(Sender: TObject);
var xfile:TFileName;
    oldfilter:string;
    writeok:Boolean;
    pvalue:Real;  sc:Integer;
const promptstr = 'Enter the minimum Parallax (in arcseconds).';
begin
  // before we write, we check
  writeok := not (tgas_main = nil);
  if writeok then writeok := (tgas_main.StarCount<>0);
  if not writeok then begin
    ShowMessage('Nothing to write!');
    Exit;
  end;
  // getting maximum distance info
  oldfilter := Trim(InputBox('Parallax Entry',promptstr,''));
  writeok :=(Length(oldfilter)<>0);
  if writeok then begin
    Val(oldfilter,pvalue,sc);
    writeok := (sc=0);
    if writeok then writeok :=(pvalue>0);
  end;
  // possible displaying an error message
  if (not writeok) then begin
    ShowMessage('Invalid Parallax');
    Exit;
  end;
  // finally... progressing to writing a file
  oldfilter := SaveStarList.Filter;
  SaveStarList.Filter := CSVFILTER;
  if SaveStarList.Execute then begin
    Screen.Cursor := crHourGlass;
    xfile := SaveStarList.FileName;
    if not AnsiEndsText('.csv',xfile) then xfile := xfile + '.csv';
    sc := tgas_main.WriteUnmatchedToFile(pvalue,xfile);
    Screen.Cursor := crDefault;
    if sc = 0 then ShowMessage('File not created (nothing to write).');
  end;
  SaveStarList.Filter := oldfilter;
end;

procedure TNStarsMainForm.VariableStarNameEditExit(Sender: TObject);
begin
  current.sysn.var_designation := VariableStarNameEdit.Text;
  PrefNameED;
  LoadPrefNames;
  VariableStarNameEdit.Modified := False;
end;

//--------------------------------------------------
procedure TNStarsMainForm.SystemConstellationClick(Sender: TObject);
var id:Integer;
begin
  current.sys.constellation := SystemConstellation.ItemIndex+1;
  id := current.sys.preferred;
  if (id>1) then begin
    LoadPrefNames;
  end;  
end;

procedure TNStarsMainForm.SystemEnableDisable;
var validsys,currok:Boolean;
begin
  // primary name preference
  PrefNameED;
  // certain menu items
  validsys := current <> nil;
  if validsys then validsys := current.sys <> nil;
  // merging
  currok := validsys and ( 1 < current.sys.GetId);
  SetMergeSrcMI.Enabled := currok;
  MergeIntoMI.Enabled := currok and (primaryl.mergesource>1);
  // star items
  AddStar1.Enabled := currok;
  AddBDMI.Enabled := currok;
  InsertStarMI.Enabled:= currok;
  AddISDBCatalogNames1.Enabled := currok;
  if not currok then MISwapParallax.Enabled := False
  else MISwapParallax.Enabled := (current.OldParallaxCount() > 0);

end;
//++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TNStarsMainForm.Save1Click(Sender: TObject);
begin
  GeneralModCheck(Sender);
  primaryl.Save;
end;
//-----------------------------------------------------------------
procedure TNStarsMainForm.ShowDistance1Click(Sender: TObject);
var buflow,bufhigh:string;
    overall:string;
begin
  if (current.sysl = nil) then Exit;
  overall:='The distance to Sol is : ' + current.sysl.GetDistanceStr(3,-1,False) + ' ly.';
  // getting the value ranges
  buflow := current.sysl.GetDistanceExtremaStr(True,True,2);
  bufhigh := current.sysl.GetDistanceExtremaStr(False,True,2);
  // adding them to the message
  overall += sLineBreak + 'The lower estimate is : ';
  overall += buflow + ' ly.';
  overall += sLineBreak + 'The upper estimate is : ';
  overall += bufhigh + ' ly.';
  // showing the message
  ShowMessage(overall);
end;
//-----------------------------------------------------------------
procedure TNStarsMainForm.ShowGlobalNotes1Click(Sender: TObject);
var gnform:TFileGlobalNotes;
begin
  gnform := TFileGlobalNotes.Create(Self);
  gnform.Show;
end;
//-----------------------------------------------------------------
procedure TNStarsMainForm.SortbyDistance1Click(Sender: TObject);
begin
  primaryl.ReSortByDistance;
  ChangeSystem;
end;
//-----------------------------------------------------------------
procedure TNStarsMainForm.StarData1;
begin
  // new star panel
  if current.sys.GetId = 1 then begin
    //StarCoreDataFrame.ChangeToSun;
    StarDataCoreFrame.ChangeToSun;
    StarCatIDFrame.ChangeObject(nil);
    StarCatIDFrame.Enabled := False;
    StarFluxTEffFrame.SetToSun;
  end
  else begin
    StarCatIDFrame.Enabled := True;
    StarDataCoreFrame.ChangeStar(current.ccomponent,current.sysl,current.sys.GetCompC);
    StarCatIDFrame.ChangeObject(current.ccomponent);
    StarFluxTEffFrame.SetToComponent(current.ccomponent);
  end;
  // done
end;
//--------------------------------------------------
procedure TNStarsMainForm.StarData3;
begin

  if current.sys.GetId = 1 then begin
    StarExtraDataFrame1.Enabled := True;
    StarExtraDataFrame1.ChangeToSun(True);
    PreMainSeqCB.Checked := False;
    PreMainSeqCB.Enabled := False;
  end
  else begin
    if (not current.ccomponent.isBrownDwarf) then begin
      StarExtraDataFrame1.Enabled := True;
      StarExtraDataFrame1.ChangeStarData(current.cstar,False);
      PreMainSeqCB.Enabled := True;
      PreMainSeqCB.Checked := current.cstar.premain;
    end
    else begin
      StarExtraDataFrame1.ChangeToBrownDwarf(False);
      StarExtraDataFrame1.Enabled := False;
      PreMainSeqCB.Checked := False;
      PreMainSeqCB.Enabled := False;
    end;
  end;
end;
//--------------------------------------------------
procedure TNStarsMainForm.AddBinaryNote1Click(Sender: TObject);
begin
  current.sys.AppndNote(' According to the ISDB, this star is actually a ' +
  'close orbiting binary pair.',false);
end;
//----------------------------------------------------------
procedure TNStarsMainForm.AddISDBCatalogNames1Click(Sender: TObject);
const cpromt = 'Please enter a list of catalog designations, separated by commas.';
var xresult:Boolean; inval:string;
begin
  xresult := InputQuery('Enter Catalog Names',cpromt,inval);
  if xresult then begin
    StarCatIDFrame.AddCatalogNamesExternal(inval,True);
  end;
end;

procedure TNStarsMainForm.AddISDBCatalogs1Click(Sender: TObject);
const cpromt = 'Please enter a list of catalog designations, ISDB style, separated by commas.';
var xresult:Boolean;
    inval:string;
begin
  xresult := InputQuery('Enter Catalog Names',cpromt,inval);
  if xresult then CatalogIDEditSystem.AddCatalogNamesExternal(inval,True);
end;

procedure TNStarsMainForm.AddStar1Click(Sender: TObject);
var arity:Integer;
    buf:string;
begin
  StarModCheck(Sender);
  // we add a star using the proxy
  current.AddStar;
  // next, we add a tab to the star info display
  arity := current.sys.GetCompC;
  Str(arity,buf);
  buf := 'Star ' + buf;
  ComponentTabControl.Tabs.Add(buf);
  // setting the value
  ComponentTabControl.TabIndex := (arity-1);
  // loading star data
  LoadAllStarData;
  RemoveStar1.Enabled := True;
  // disabing menus if necessary
  if (adat.loaded) then GetArcSystemName.Enabled := True;
end;


procedure TNStarsMainForm.ARICNSDiff1Click(Sender: TObject);
const prompt1 = 'Finding the difference between this list and ARICNS.';
      prompt2 = 'Please Wait.';
      prompt3 = 'Done. Find the file ''arcdiff.txt'' for the results.';
begin
  // getting the list and getting OK or cancel
  ShowMessage(prompt1+#13#10+prompt2);
  primaryl.AricnsDiff('arcdiff.txt');
  ShowMessage(prompt3);
end;

procedure TNStarsMainForm.BayerPickListChange(Sender: TObject);
begin
  current.sys.bayer_desig := BayerPickList.ItemIndex;
  PrefNameED;
  LoadPrefNames;
end;

procedure TNStarsMainForm.TGASNameMatchMIClick(Sender: TObject);
var tgasr:Boolean;
begin
  if current <> nil then begin
    tgasr := current.ShowTGASMatches(True);
    if tgasr then begin
      SystemLoad3;
      SystemLoad4;
      NewLocChangeCheck;
    end;
  end;
end;

procedure TNStarsMainForm.Tycho2MagMIClick(Sender: TObject);
var data:string;   rok:Boolean;
const entmsg = 'Enter Tycho-2 Magnitudes (Bt Vt), with the values separated by spaces, below:';
begin
  if current.cstar <> nil then begin
    data := Trim(InputBox('Tycho-2 to Johnson (Mamajek+ 2002)',entmsg,''));
    if Length(data)<>0 then begin
       rok := current.Tycho2_Helper(data);
    end;
    if rok then begin
      // reloading after data has been set
      StarData1;
    end;
  end;
end;

procedure TNStarsMainForm.UCAC4MagEstMIClick(Sender: TObject);
var data:string;   rok:Boolean;
const entmsg = 'Enter the UCAC4 Model Fit Magnitude to estimate B and V' +
                      sLineBreak + '(also uses Ks, for KM dwarfs)';
begin
  if current.cstar <> nil then begin
    data := Trim(InputBox('UCAC4 to BV',entmsg,''));
    if Length(data)<>0 then begin
       rok := current.UCAC4_ToBV_Helper(data);
    end;
    if rok then begin
      // reloading after data has been set
      StarData1;
    end;
  end;
end;

procedure TNStarsMainForm.UnresBinaryMIClick(Sender: TObject);
begin
  UncheckFilters;
  UnresBinaryMI.Checked := True;
  primaryl.HasMultiples;
  if current.sys <> nil then ChangeSystem;
end;

procedure TNStarsMainForm.UseSepLocatCBChange(Sender: TObject);
begin

end;

procedure TNStarsMainForm.BayerSuperscriptEditExit(Sender: TObject);
var xint,sc:Integer;
begin
  Val(BayerSuperscriptEdit.Text,xint,sc);
  Assert(sc=0);
  current.sysn.bayer_sup := xint;
  PrefNameED;
  LoadPrefNames;
  BayerSuperscriptEdit.Modified := False;
end;

//------------------------------------------------
procedure TNStarsMainForm.StarDisplaySetChange(Sender: TObject);
begin
  StarModCheck(Sender);
end;

//--------------------------------------------------
procedure TNStarsMainForm.ListOfStarSystemsClick(Sender: TObject);
var ci:Integer;
begin
  ci := ListOfStarSystems.ItemIndex;
  if ci<0 then Exit;
  GeneralModCheck(Sender);
  (* we change the current star system! *)
  ci := ListOfStarSystems.ItemIndex;
  // os := current.sys;
  primaryl.ChangeSystem(ci);
  ComponentTabControl.TabIndex := 0;
  ChangeSystem;
end;

procedure TNStarsMainForm.LoadAllStarData;
begin
  // the 3 basic ones
  StarData1;
  StarData3;
  NewLocChangeCheck;
  // do we only have one star? the certian items are not needed
  if current.sys.GetCompC=1 then begin
    RemoveStar1.Enabled := False;
    PosSepSetMI.Enabled := False;
  end
  else begin
    RemoveStar1.Enabled := True;
  end;
end;
//--------------------------------------------------
procedure TNStarsMainForm.LoadARICNSCatalogs1Click(Sender: TObject);
var A:Integer;
begin
  ClearAdat;
  AricnsLoadForm := TAricnsLoadForm.Create(Self);
  AricnsLoadForm.Show;
  GetARICNSStarData.Enabled := True;
  GetARICNSDesignations1.Enabled := True;
  // ARICNSDiff1.Enabled := True;
  // the GetArcSystemName menu item
  A := current.sys.GetCompC;
  if A>1 then GetArcSystemName.Enabled := True
  else GetArcSystemName.Enabled := False;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TNStarsMainForm.ChangeSystem;
var I,A:Integer;
    buf1:string;
begin
  SystemEditPageSet.Enabled := True;
  // loading system data
  SystemLoad1;
  SystemLoad2;
  SystemLoad3;
  SystemLoad4;
  SystemLoad5;
  // star info
  supstab := True;
  ComponentTabControl.Tabs.Text := '';
  A := current.sys.GetCompC;
  for I := 1 to A do begin
    Str(I,buf1);
    if current.sys.IsBrownDwarf(I) then buf1 := 'Brown Dwarf ' + buf1
    else buf1 := 'Star ' + buf1;
    ComponentTabControl.Tabs.Add(buf1);
  end;
  ComponentTabControl.TabIndex := 0;
  supstab := False;
  LoadAllStarData;
  ListOfStarSystems.Items[ListOfStarSystems.ItemIndex] := MainSystemName.Text;
  // enabling or disabling certain items
  SystemEnableDisable;
  // we now enable or disbale the GetArcSystemName menu item
  if (adat.loaded) then begin
    if A>1 then GetArcSystemName.Enabled := True
    else GetArcSystemName.Enabled := False;
  end;
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
procedure TNStarsMainForm.UncheckFilters;
begin
  ZeroRadialVelocity1.Checked := False;
  ClosetothisSystem1.Checked := False;
  FewCatalogNames1.Checked := False;
  LocationUncertian1.Checked := False;
  HasSpectralType1.Checked := False;
  NoAssociatedCluster1.Checked := False;
  UnresBinaryMI.Checked:= False;
  BadSpectraMI.Checked := False;
  LuminosityProbMI.Checked := False;
  BrownDwarfZMMI.Checked := False;
  SearchNotesMI.Checked := False;
  SearchPlxSrcMI.Checked := False;
  HasProbMI.Checked := False;
end;
//------------------------------------------------------------
(* uses the current catalog name with some modifications as the
main syatem name *)
procedure TNStarsMainForm.UseNameBtnClick(Sender: TObject);
var newname:string;
begin
  // we get the catalog string
  newname := CatalogIDEditSystem.GetModifiedCurrentCatalog;
  // setting the name
  current.sys.System_name := newname;
  LoadPrefNames;
  MainSystemName.Modified := False;
end;

//----------------------------------------------------------
procedure TNStarsMainForm.ProblemCBClick(Sender: TObject);
begin
  if current.sys.has_problems <> ProblemCB.Checked then begin
    current.sys.has_problems := ProblemCB.Checked;
  end;
end;

procedure TNStarsMainForm.ClosetothisSystem1Click(Sender: TObject);
const prompt1='Enter the maximum distance in Light Years from the current system.';
      prompt2='The value entered is not valid. Too Bad.';
var lystring:string;
    realvalue:Real;
    sc:Integer;
begin
  // getting the data
  lystring:=InputBox('Distance Filter Data Entry', prompt1, '10');
  lystring := Trim(lystring);
  Val(lystring,realvalue,sc);
  if (sc=0) and (realvalue>0) then begin
    UncheckFilters;
    ClosetothisSystem1.Checked := True;
    primaryl.DistanceTest(realvalue,-1);
    ChangeSystem;
  end
  else begin
    ShowMessage(prompt2);
  end;
end;

procedure TNStarsMainForm.ClusterCheckListClickCheck(Sender: TObject);
var I, cc:Integer;
begin
  cc := ClusterCheckList.Count;
  for I := 0 to cc - 1 do begin
    if checkbak[I]<> ClusterCheckList.Checked[I] then begin
      if ClusterCheckList.Checked[I] then begin
        cluster_list[I].AddSystem(current.sys);
      end
      else begin
        cluster_list[I].RemoveSystem(current.sys);
      end;
    end;
  end;
end;

procedure TNStarsMainForm.DeleteSystem1Click(Sender: TObject);
var oldci,ci:Integer;
begin
  Screen.Cursor := crHourGlass;
  oldci := ListOfStarSystems.ItemIndex;
  // getting rid of the system
  primaryl.DeleteCurrentSystem;
  primaryl.LoadHereAndAfter(oldci);
  // calculating the new index
  if oldci<>0 then ci := oldci-1
  else ci:=0;
  // setting the new index
  if ListOfStarSystems.Items.Count>0 then begin
    ListOfStarSystems.ItemIndex := ci;
    primaryl.ChangeSystem(ci);
    ChangeSystem;
  end;
  Screen.Cursor := crDefault;

end;

//--------------------------------------------------------------

procedure TNStarsMainForm.EditClusters1Click(Sender: TObject);
var ceform:TClusterEditForm;
begin
  loadlist := False;
  ceform := TClusterEditForm.Create(Self);
  ceform.Show;
end;

procedure TNStarsMainForm.ExtraNamesEditExit(Sender: TObject);
begin
  current.sys.extra_names := ExtraNamesEdit.Text;
  ExtraNamesEdit.Modified := False;
end;

procedure TNStarsMainForm.ExtraNotesMemoExit(Sender: TObject);
begin
  current.sys.description := ExtraNotesMemo.Text;
  ExtraNotesMemo.Modified := False;
end;

procedure TNStarsMainForm.FewCatalogNames1Click(Sender: TObject);
const prompt1='Enter the maximum number of category designations.';
      prompt2='The value entered is not valid. Too Bad.';
var lystring:string;
    realvalue,sc:Integer;
begin
  // getting the data
  lystring:=InputBox('Distance Filter Data Entry', prompt1, '10');
  lystring := Trim(lystring);
  Val(lystring,realvalue,sc);
  if (sc=0) and (realvalue>=1) then begin
    UncheckFilters;
    FewCatalogNames1.Checked := True;
    primaryl.CoolAsCats(realvalue);
    ChangeSystem;
  end
  else begin
    ShowMessage(prompt2);
  end;
end;

procedure TNStarsMainForm.FictionalNamesEditExit(Sender: TObject);
begin
  current.sys.fictional_names := FictionalNamesEdit.Text;
  FictionalNamesEdit.Modified := False;
end;

procedure TNStarsMainForm.FindbyName1Click(Sender: TObject);
var ci,sstar_where:Integer;
    data:string;
    // os:StarSystem;
begin
  GeneralModCheck(Sender);
  // we go looking
  data := InputBox('Search Box','Enter a name to look for:','');
  data := Trim(data);
  ci := primaryl.FindByString(data,sstar_where);
  if ci<>-1 then begin
    (* we change the current star system! *)
    // os := current.sys;
    primaryl.ChangeSystem(ci);
    ChangeSystem;
    ListOfStarSystems.ItemIndex := ci;
    // changing the star...
    if sstar_where > 1 then begin
      ComponentTabControl.TabIndex := sstar_where - 1;
      current.SetStar(sstar_where);
      LoadAllStarData;
    end;
  end
  else ShowMessage('Nothing found!');
end;

procedure TNStarsMainForm.FindConstellation1Click(Sender: TObject);
var cres:Integer;
    qval,msg:string;
    rval:Word;
begin
  cres := current.LocateConst;
  if cres<>-1 then begin
    qval := constellations[(cres-1)*3];
    msg := 'The consteallation found is ' + qval + '.'#13#10;
    msg := msg + 'Do we change the constellation to this?';
    rval := MessageDlg(msg, mtConfirmation,[mbYes, mbNo],0);
    if rval=mrYes then begin
      current.sys.constellation := cres;
      SystemLoad1;
    end;
  end
  else begin
    ShowMessage('Error: cannot find constellation!');
  end;
end;

procedure TNStarsMainForm.FlamsteedNumExit(Sender: TObject);
var xint,sc:Integer;
begin
  Val(FlamsteedNum.Text,xint,sc);
  if (sc<>0) then Exit;
  current.sys.flamsteed_number := xint;
  PrefNameED;
  LoadPrefNames;
  FlamsteedNum.Modified := False;
end;

procedure TNStarsMainForm.FormActivate(Sender: TObject);
begin
  // widget setup
  if firststartup then begin
     supstab := False;
     CatalogIDEditSystem.SetSystem(True,UseNameBtnClick);
     CatalogIDEditSystem.ChangeObject(nil);
     MainLocatEditFrame1.SetupStart(MainParallaxChange);
     StarCatIDFrame.SetSystem(False,nil);
     StarCatIDFrame.ChangeObject(nil);
     StarDataCoreFrame.SetupComboBoxes;
     StarDataCoreFrame.ChangeStar(nil,nil,0);
     StarExtraDataFrame1.Setup();
     StarLocatFrame1.Setup(StarParallaxChanged);
     StarLocatFrame1.Enabled := False;
     StarFluxTEffFrame.SetToNothing;
     StarFluxTEffFrame.SetFluxTempChangeHandler(FluxTEffChange);
     firststartup := False;
  end;
  // loading initial data
  if loadlist then begin
    primaryl.LoadListBox;
    primaryl.ChangeSystem(0);
    ChangeSystem;
    loadlist := false;
  end
  else begin
      // SystemEditPageSet.Enabled := False;
      ChangeSystem;
  end;

end;

procedure TNStarsMainForm.FormCreate(Sender: TObject);
begin
  primaryl := StarList.Create(ListOfStarSystems);
  current := primaryl.GetProxy;
  // updating the interface
  tgas_main := TGASCollection.Create;
  firststartup := True;
  loadlist := True;
end;

procedure TNStarsMainForm.LoadTGASCSVClick(Sender: TObject);
begin
  GeneralModCheck(Sender);
  if TGASImportForm = nil then TGASImportForm := TTGASImportForm.Create(Self);
  TGASImportForm.Show;
end;

procedure TNStarsMainForm.LuminosityProbMIClick(Sender: TObject);
begin
  UncheckFilters;
  LuminosityProbMI.Checked := True;
  primaryl.LuminosityProblems;
  if current.sys <> nil then ChangeSystem;
end;

procedure TNStarsMainForm.MenuItem4Click(Sender: TObject);
begin

end;

procedure TNStarsMainForm.MergeIntoMIClick(Sender: TObject);
var mok:Boolean;
    targetid:Integer;
    sourceindex,newdex:Integer;
begin
  // initial checking
  targetid := current.sys.GetId;
  mok := (primaryl.mergesource > 1) and (targetid >1);
  mok := mok and (targetid<>primaryl.mergesource);
  sourceindex := primaryl.FindByID(primaryl.mergesource);
  mok := mok and (sourceindex > 0);
  if not mok then begin
    ShowMessage('Cannot merge (Invalid source or target.')
  end;
  // moving to sol
  Screen.Cursor := crHourGlass;
  GeneralModCheck(Sender);
  primaryl.ChangeSystem(0);
  ComponentTabControl.TabIndex := 0;
  ChangeSystem;
  // doing the merge
  mok := primaryl.MergeSystems(targetid,primaryl.mergesource);
  Assert(mok);
  // adjusting the list afterwards
  primaryl.LoadHereAndAfter(sourceindex);
  primaryl.mergesource := -1;
  // calculating the new index
  newdex := primaryl.FindByID(targetid);
  // setting the new index
  if ListOfStarSystems.Items.Count>0 then begin
    ListOfStarSystems.ItemIndex := newdex;
    primaryl.ChangeSystem(newdex);
    ChangeSystem;
  end;
  Screen.Cursor := crDefault;
end;

procedure TNStarsMainForm.MISwapParallaxClick(Sender: TObject);
var xres:Boolean;
begin
  if current = nil then Exit;
  xres := current.OldParallaxSwap();
  if xres then begin
    MainLocatEditFrame1.Reload;
  end;
end;

procedure TNStarsMainForm.New1Click(Sender: TObject);
var solsys:StarSystem;
    rval:Word;
const rmsg = 'Do you want to save the current list before makeing a new one?';
begin
  rval := mrNo;
  if (primaryl<>nil) then begin
    rval := MessageDlg(rmsg, mtConfirmation,[mbYes, mbNo],0);
    if (rval = mrYes) then begin
      GeneralModCheck(Sender);
      primaryl.Save;
    end;
  end;
  // clearing old, making new, and adding
  primaryl := StarList.Create(ListOfStarSystems);
  current := primaryl.GetProxy;
  // updating the interface
  primaryl.LoadListBox;
  Save1.Enabled := False;
  ChangeSystem;
end;

procedure TNStarsMainForm.ParallaxEntryMIClick(Sender: TObject);
var data1:string;   rok:Boolean;
const entmsg = 'Enter the Parallax Source, Parallax, and Parallax error, below:';
begin
  if current.sysl <> nil then begin
    data1 := Trim(InputBox('Parallax Entry',entmsg,parallax_source));
    if Length(data1) <> 0 then begin
      rok := current.sysl.SetParallaxPasted(data1,parallax_source);
      if rok then begin
        SystemLoad3;
        ShowMessage('Parallax set!');
      end
      else begin
        ShowMessage('Cannot set parallax with this input!');
      end;
    end;
  end;
end;


procedure TNStarsMainForm.PllxImportMIClick(Sender: TObject);
begin
  FreeAndNil(ImportForm);
  ImportForm := TImportForm.Create(Self);
  ImportForm.Show;
end;

procedure TNStarsMainForm.PosSepSetMIClick(Sender: TObject);
var cxResult:Boolean;
begin
    if current <> nil then begin
       cxResult := current.PosAngLocationSet();
       if cxResult then NewLocChangeCheck();
    end;
end;

procedure TNStarsMainForm.PPMMatchMIClick(Sender: TObject);
begin
  if PositionProperMotionMatchForm<>nil then FreeAndNil(PositionProperMotionMatchForm);
  PositionProperMotionMatchForm := TPositionProperMotionMatchForm.Create(Self);
  PositionProperMotionMatchForm.Show();
end;

procedure TNStarsMainForm.PreMainSeqCBChange(Sender: TObject);
begin
    if current.cstar <> nil then begin
      if current.cstar.premain <> PreMainSeqCB.Checked then begin
        current.cstar.premain:= PreMainSeqCB.Checked;
        StarDataCoreFrame.SaveSpectralClass;
      end;
    end;
end;

procedure TNStarsMainForm.SaveAs2MIClick(Sender: TObject);
var xfile:TFileName;      filbak:string;
begin
  // getting the new save file name
  GeneralModCheck(Sender);
  filbak := SaveStarList.Filter;
  SaveStarList.Filter := NSL2FILTER;
  SaveStarList.Execute;
  xfile := Trim(SaveStarList.FileName);
  // checking the name afterwards
  if Length(xfile) = 0 then Exit;
  if not AnsiEndsText('.nsl2',xfile) then xfile := xfile + '.nsl2';
  // saving
  primaryl.SaveToFile(xfile);
  // post save stuff
  NStarsMainForm.Caption := 'NStars - (' + xfile + ')';
  SaveStarList.Filter := filbak;
  Save1.Enabled := True;
end;

procedure TNStarsMainForm.SDSSMagEntryMIClick(Sender: TObject);
var data:string;   rok:Boolean;
const entmsg = 'Enter SDSS Magnitudes ([u] g r i z, optionally with errors),' +
           sLineBreak + 'with the values separated by spaces, below:';
begin
  if current.cstar <> nil then begin
    data := Trim(InputBox('SDSS to Johnson-Cousins (Lupton 2005)',entmsg,''));
    if Length(data)<>0 then begin
       rok := current.SDSS_Helper(data);
    end;
    if rok then begin
      // reloading after data has been set
      StarData1;
    end;
  end;
end;

procedure TNStarsMainForm.SearchNotesMIClick(Sender: TObject);
const prompt1='Enter a string to find in System/Star notes: ';
      prompt2='The value entered is empty!';
var lystring:string;
begin
  // getting the data
  lystring := InputBox('Notes Search Entry', prompt1, '');
  lystring := Trim(lystring);
  if lystring<>'' then begin
    UncheckFilters;
    SearchNotesMI.Checked:= True;
    primaryl.SearchNotes(lystring);
    ChangeSystem;
  end
  else begin
    ShowMessage(prompt2);
  end;
end;

procedure TNStarsMainForm.SearchPlxSrcMIClick(Sender: TObject);
const prompt1='Enter a string to find in the parallax source: ';
      prompt2='The value entered is empty!';
var lystring:string;
begin
  // getting the data
  lystring:=InputBox('Parallax source Search Entry', prompt1, '');
  lystring := Trim(lystring);
  if lystring<>'' then begin
    UncheckFilters;
    SearchPlxSrcMI.Checked := True;
    primaryl.SearchParallaxSource(lystring);
    ChangeSystem;
  end
  else begin
    ShowMessage(prompt2);
  end;
end;

procedure TNStarsMainForm.SetBM_1MIClick(Sender: TObject);
begin
  primaryl.SetBookmarkToCurrent(0);
end;

procedure TNStarsMainForm.SetBM_2MIClick(Sender: TObject);
begin
  primaryl.SetBookmarkToCurrent(1);
end;

procedure TNStarsMainForm.SetMergeSrcMIClick(Sender: TObject);
var sok:Boolean;
    mid:Integer;
begin
  sok := False;
  if current <>nil then begin
    if current.sys<>nil then begin
      mid := current.sys.GetId;
      if mid<>1 then begin
        sok := True;
        primaryl.mergesource := mid;
      end;
    end;
  end;
  if not sok then ShowMessage('There is no valid system to merge!');
end;

procedure TNStarsMainForm.ShowGalCoordMIClick(Sender: TObject);
var overall,latlongstr:string;
begin
  if (current = nil) then Exit;
  if (current.sysl = nil) then Exit;
  Screen.Cursor := crHourGlass;
  overall := 'The Galactic co-ordinates are : ' + sLineBreak;
  // getting the values
  latlongstr := current.sysl.GetGalacticCoordString();
  overall += latlongstr;
  Screen.Cursor := crDefault;
  ShowMessage(overall);
end;

procedure TNStarsMainForm.ShowPosDegMIClick(Sender: TObject);
var radeg,decdeg:Real;
    rastr,decstr:string;
    overall:string;
begin
  if (current = nil) then Exit;
  if (current.sysl = nil) then Exit;
  if current.sys.GetId = 1 then ShowMessage('Not applicable for the Sun.')
  else begin
     // applicable here!
    Screen.Cursor := crHourGlass;
    radeg := current.sysl.GetDecimalRightAscension;
    decdeg := current.sysl.GetDecimalDeclination;
    rastr := ' RA : ' + Trim(FloatToStrF(radeg,ffFixed,9,5)) + '°';
    decstr := 'Dec : ' + Trim(FloatToStrF(decdeg,ffFixed,9,5)) + '°';
    overall := 'The position in decimal degrees is ' + sLineBreak;
    overall += rastr + sLineBreak + decstr;
    // showing the message
    Screen.Cursor := crDefault;
    ShowMessage(overall);
  end;
end;

procedure TNStarsMainForm.ShowUVWMIClick(Sender: TObject);
var righto,lefto:string;
    overall:string;
begin
  if (current.sysl = nil) then Exit;
  Screen.Cursor := crHourGlass;
  overall := 'Space Velocities are : ' + sLineBreak;
  // getting the values
  righto := current.sysl.GetUVWString(False);
  overall += 'Right-handed: ' + righto + sLineBreak;
  lefto := current.sysl.GetUVWString(True);
  overall += 'Left-handed: ' + lefto;
  // showing the message
  Screen.Cursor := crDefault;
  ShowMessage(overall);
end;

//--------------------------------------------------------
procedure TNStarsMainForm.SimbadLocLookClick(Sender: TObject);
var rastring, decstring:string;
    simbadurl:string;
begin
  if (current.sysl <> nil) then begin
    Screen.Cursor := crHourGlass;
    // getting the values for making the url
    rastring := current.sysl.RightAscensionHMS;
    decstring := current.sysl.DeclinationDMS;
    // making the url
    simbadurl := MakeSimbadCoordLookupURL(rastring,decstring,2,False);
    Screen.Cursor := crDefault;
    // launching the browser to open this URL
    OpenURL(simbadurl);
  end;
end;
//-------------------------------------------------
procedure TNStarsMainForm.SimbadIDLookClick(Sender: TObject);
var catstring:string;
    simbadurl:string;
begin
  if (current.sysl <> nil) then begin
    Screen.Cursor := crHourGlass;
    // getting the values for making the url
    if current.cstarn <> nil then begin
      catstring := current.cstarn.GetDefaultCatalog;
      if Length(catstring) = 0 then catstring := current.sysn.GetDefaultCatalog;
    end
    else catstring := current.sysn.GetDefaultCatalog;
    if Length(catstring) = 0 then begin
      Screen.Cursor := crDefault;
      ShowMessage('System does not have the right catalog IDs!');
    end
    else begin
      simbadurl := MakeSimbadIdLookupURL(catstring);
      Screen.Cursor := crDefault;
      OpenURL(simbadurl);
    end;
  end;
end;
//-------------------------------------------------
procedure TNStarsMainForm.SimbadURLDataClick(Sender: TObject);
var urlentered:string;     doreload:Boolean;
const simbad_msg = 'Enter the Simbad URL for the star to get.';
      simbad_default = '';
begin
  // if we have a valid location, we ask for a visal magnitude input
    urlentered := InputBox('Simbad URL',simbad_msg,simbad_default);
    urlentered := Trim(urlentered);
    if Length(urlentered) = 0 then ShowMessage('Empty Input!')
    else begin
      doreload := current.SimbadDownload(urlentered,False);
      if doreload then begin
        SystemLoad1;
        SystemLoad3;   SystemLoad4;
        StarData1;
      end;
    end;
end;
//------------------------------------------------------
procedure TNStarsMainForm.SimbURLCGIDClick(Sender: TObject);
var urlentered:string;     doreload:Boolean;
const simbad_msg = 'Type in the Simbad URL for the page to check';
      simbad_default = '';
begin
  // if we have a valid location, we ask for a visal magnitude input
    urlentered := InputBox('Simbad URL',simbad_msg,simbad_default);
    urlentered := Trim(urlentered);
    if Length(urlentered) = 0 then ShowMessage('Empty Input!')
    else begin
      doreload := current.SimbadCatalogs(urlentered);
      if doreload then SystemLoad4;
    end;
end;

procedure TNStarsMainForm.SimbadDataFetchClick(Sender: TObject);
begin
  SimbadIDFetch(0);
end;

procedure TNStarsMainForm.SimbadIDFluxMIClick(Sender: TObject);
begin
  SimbadIDFetch(1);
end;

procedure TNStarsMainForm.LoadSimpleTGASClick(Sender: TObject);
var qfile:TFileName;
    scount,scoun2:Integer;
    countstr,countstr2,countmsg:string;
    filterbak:string;
begin
  GeneralModCheck(Sender);
  filterbak := OpenStarList.Filter;
  OpenStarList.Filter := CSVFILTER;
  if OpenStarList.Execute then begin
    qfile := OpenStarList.FileName;
    // loading the csv file
    Screen.Cursor := crHourGlass;
    OpenStarList.Filter := filterbak;
    tgas_main.ImportFromReducedCSVFile(qfile);
    // to fix, setting the match values based on contents
    scoun2 := primaryl.SetTGASMatch(tgas_main);
    // display importinfo dialog
    scount := tgas_main.StarCount;
    Str(scount,countstr);
    Str(scoun2,countstr2);
    countmsg := 'TGAS Data now has ' + countstr + ' stars. (' + countstr2 + ')';
    Screen.Cursor := crDefault;
    ShowMessage(countmsg);
    // done
    // Save1.Enabled := false;
  end
  else OpenStarList.Filter := filterbak;
end;

procedure TNStarsMainForm.SimpleTGASCSVexpClick(Sender: TObject);
var xfile:TFileName;
    oldfilter:string;
    writeok:Boolean;
begin
  oldfilter := SaveStarList.Filter;
  SaveStarList.Filter := CSVFILTER;
  if SaveStarList.Execute then begin
    Screen.Cursor := crHourGlass;
    xfile := SaveStarList.FileName;
    if not AnsiEndsText('.csv',xfile) then xfile := xfile + '.csv';
    writeok := tgas_main.WriteReducedToFile(xfile);
    Screen.Cursor := crDefault;
    if not writeok then ShowMessage('File not created (nothing to write?).');
  end;
  SaveStarList.Filter := oldfilter;
end;

procedure TNStarsMainForm.StarExtraDataFrame1Click(Sender: TObject);
begin

end;

procedure TNStarsMainForm.StarExtraDataFrame1Exit(Sender: TObject);
begin
  StarExtraDataFrame1.SaveValues(True);
end;

procedure TNStarsMainForm.StarLocatFrame1Exit(Sender: TObject);
begin
  if StarLocatFrame1.Enabled then StarLocatFrame1.SaveData(True);
end;

procedure TNStarsMainForm.StarToSysMIClick(Sender: TObject);
var extract_index, newid:Integer;
    extracted:StarSystem;
begin
  GeneralModCheck(Sender);
  // check to see  if this menu option is valid
  if current.sys = nil then Exit;
  if current.starindex < 2 then Exit;
  (* To avoid trying to save the star after it has been extracted, we
  switch to star 1. *)
  extract_index := current.starindex;
  current.SetStar(1);
  ComponentTabControl.TabIndex := 0;
  LoadAllStarData;
  // extracting the star...
  newid := primaryl.NextID;
  extracted := current.sys.ExtractStarAsSystem(extract_index,newid);
  Assert(extracted<>nil);
  // since we switch to the new system
  LoadAllStarData;
  // StarInfoDisplay.TabIndex := 0;
  // appending the new system and moving to it..
  primaryl.AppendSystem(extracted);
  ChangeSystem;
end;

procedure TNStarsMainForm.SystemEditPageSetChange(Sender: TObject);
begin

end;

procedure TNStarsMainForm.TestVizDMIClick(Sender: TObject);
var vizres:Boolean;
begin
  if current <> nil then begin
    vizres := current.VizierAPASSGet(False);
    if vizres then begin
      current.sys.UpdateEstimates;
      StarData1;
      ShowMessage('Done');
    end;
  end;
end;

procedure TNStarsMainForm.TGASBinarySecMatchMIClick(Sender: TObject);
var tgasr:Boolean;
begin
  if current <> nil then begin
    tgasr := current.TGASStarMatch;
    if tgasr then begin
      NewLocChangeCheck;
    end;
  end;
end;

procedure TNStarsMainForm.TGASFindClick(Sender: TObject);
var tgasr:Boolean;
begin
  if current <> nil then begin
    tgasr := current.ShowTGASMatches(False);
    if tgasr then begin
      SystemLoad3;
      SystemLoad4;
      NewLocChangeCheck;
    end;
  end;
end;

procedure TNStarsMainForm.FormDestroy(Sender: TObject);
begin
  primaryl.Free;
  current:=nil;
  tgas_main.Free;
end;

//++++++++++++++++++++++++++++++++++++++++++++++++++

procedure TNStarsMainForm.Name2300adEditExit(Sender: TObject);
begin
  current.sys.name2300ad := Name2300adEdit.Text;
  Name2300adEdit.Modified := False;
end;
//------------------------------------------------
procedure TNStarsMainForm.GetArcSystemNameClick(Sender: TObject);
const catfound='Catalog designations found are listed below:';
var msgvalue:string;
    nummsg, bval:string;
    resvalue:Boolean;
    inarity,outarity, mval:Integer;
    Save_Cursor:TCursor;
begin
  // set the hourglass...
  Save_Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  // finding the specific arity
  inarity := current.sys.GetCompC;
  assert(inarity>1);
  mval := 2;
  // value displaying and getting OK or cancel
  msgvalue := FindDesignations(current.sys,current.starindex, mval);
  if msgvalue='' then begin
    // a common reason for not finding things is a mismatch is the arity
    outarity := FindAricnsArity(current.sys);
    // building the result message
    if (outarity = 0) then nummsg := 'The System was not found!'
    else if (outarity = 1) then nummsg := 'The System was found, but there is uncertainty in components. Check ARICNS manually'
    else begin
      Str(outarity,bval);
      nummsg := 'The arity of the System should be ' + Trim(bval) + '. Please increase the arity to match!';
    end;
    // showing a message
    Screen.Cursor := Save_Cursor;
    ShowMessage(nummsg);
  end
  else begin
    Screen.Cursor := Save_Cursor;
    resvalue := InputQuery('ARICNS System Names',catfound,msgvalue);
    // if the conditions are met
    if resvalue then begin
      CatalogIDEditSystem.AddCatalogNamesExternal(msgvalue,False);
    end;
  end;
end;
//---------------------------------------------------
procedure TNStarsMainForm.NewLocChangeCheck;
var extrac,idl:Boolean;
begin
    // new...
  extrac := (current.sys.GetId = 1) or (current.starindex = 1);
  if extrac then begin
    StarLocatFrame1.ChangeLocation(nil);
    StarLocatFrame1.Enabled := False;
    UseSepLocatCB.Checked := False;
    UseSepLocatCB.Enabled := False;
  end
  else begin
    idl := current.ccomponent.HasLocation;
    UseSepLocatCB.Enabled := True;
    if idl then begin
      StarLocatFrame1.Enabled := True;
      StarLocatFrame1.ChangeLocation(current.cstarl);
      UseSepLocatCB.Checked := True;
    end
    else begin
      StarLocatFrame1.ChangeLocation(nil);
      StarLocatFrame1.Enabled := False;
      UseSepLocatCB.Checked := False;
    end;
  end;
end;
//================================================================================

end.
