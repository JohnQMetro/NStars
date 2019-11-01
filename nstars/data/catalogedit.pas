unit catalogedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Dialogs, Menus, Clipbrd,
  StarDataBase, star_names (* namedata *), unitdata, simbad, df_strings, LCLIntf;

type

  { TCatalogIDEditFrame }

  TCatalogIDEditFrame = class(TFrame)
    // elements
    CatNamesGB: TGroupBox;
    CatAddBtn: TButton;
    CatalogPopupMenu: TPopupMenu;
    CopyIDMI: TMenuItem;
    LaunchSearchID: TMenuItem;
    LaunchSimbadID: TMenuItem;
    MoveID2Sys: TMenuItem;
    RemCatBtn: TButton;
    CatIDListBox: TListBox;
    UseCatBtn: TButton;
    procedure CatAddBtnClick(Sender: TObject);
    procedure CatalogPopupMenuPopup(Sender: TObject);
    procedure CatIDListBoxClick(Sender: TObject);
    procedure CopyIDMIClick(Sender: TObject);
    procedure LaunchSearchIDClick(Sender: TObject);
    procedure LaunchSimbadIDClick(Sender: TObject);
    procedure MoveID2SysClick(Sender: TObject);
    procedure RemCatBtnClick(Sender: TObject);
    procedure UseCatBtnClick(Sender: TObject);
  private
    { private declarations }
    StarPointer:StarBase;
    NamePointer:StarNames;
    SystemLevel:Boolean;
    AddClickHandlerPointer:TNotifyEvent;
    MoveToSysHandlerPointer:TNotifyEvent;
    popup_index:Integer;
    popup_string:string;
    function GetCC:string;
    function SetPopupIndex:Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent) ; override;
    procedure ChangeObject(NewObject:StarBase);
    procedure Reload();
    procedure SetAsSystem(NewAddBtnHandler:TNotifyEvent);
    procedure SetAsStar(NewMoveToSysHandlerPointer:TNotifyEvent);
    function AddCatalogNamesExternal(inlist:string; isdb:Boolean):Boolean;
    property CurrentCatalog:string read GetCC;
    function GetModifiedCurrentCatalog:string;
    function ExtractCurrentCatalog():string;
  end;

implementation

{$R *.lfm}

procedure TCatalogIDEditFrame.CatAddBtnClick(Sender: TObject);
const cpromt = 'Please enter a list of catalog designations (abbreviated' +
               ' names only!), separated by commas.';
var xresult:Boolean;
    inval:string;
    outvalue:TStringList;
begin
  xresult := InputQuery('Enter Catalog Names',cpromt,inval);
  if xresult then begin
    if NamePointer = nil then NamePointer := StarPointer.MakeOrGetNames;
    NamePointer.SetMultipleCat(inval);
    outvalue := NamePointer.GetList;
    CatIDListBox.Items.Text := outvalue.Text;
  end;
end;

procedure TCatalogIDEditFrame.CatalogPopupMenuPopup(Sender: TObject);
begin
  SetPopupIndex;
  if popup_index = -1 then abort;
end;

procedure TCatalogIDEditFrame.CatIDListBoxClick(Sender: TObject);
begin
  if CatIDListBox.ItemIndex<>-1 then begin
    RemCatBtn.Enabled := True;
    if SystemLevel then UseCatBtn.Enabled := True;
  end
  else begin
    RemCatBtn.Enabled := False;
    UseCatBtn.Enabled := False;
  end;
end;
// copying to the clipboard is iffy
procedure TCatalogIDEditFrame.CopyIDMIClick(Sender: TObject);
var retrycount:Integer;
begin
  retrycount := 0;
  // we try in a loop
  while (retrycount < 4) do begin
    Clipboard.AsText := popup_string;
    Inc(Retrycount);
    Sleep(80)
  end;
end;

procedure TCatalogIDEditFrame.LaunchSearchIDClick(Sender: TObject);
var trans:string;
const gosrch = 'https://www.google.ca/search?q=';
begin
  trans := '%22' + StringToUrlQ(popup_string) + '%22' + '+star';
  // launching the browser ...
  OpenURL(gosrch + trans);
  // ShellExecuteA(0, 'open', PChar(gosrch + trans), nil, nil, SW_SHOWNORMAL);
end;

procedure TCatalogIDEditFrame.LaunchSimbadIDClick(Sender: TObject);
var simbadurl:string;
begin
  simbadurl := MakeSimbadIdLookupURL(popup_string);
  OpenURL(simbadurl);
end;

procedure TCatalogIDEditFrame.MoveID2SysClick(Sender: TObject);
begin
  if Assigned(MoveToSysHandlerPointer) then MoveToSysHandlerPointer(Self);
end;

procedure TCatalogIDEditFrame.RemCatBtnClick(Sender: TObject);
begin
  ExtractCurrentCatalog();
end;

procedure TCatalogIDEditFrame.UseCatBtnClick(Sender: TObject);
begin
  if Assigned(AddClickHandlerPointer) then AddClickHandlerPointer(Self);
end;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TCatalogIDEditFrame.GetCC:string;
begin
  if CatIDListBox.SelCount = 0 then Result := ''
  else Result := CatIDListBox.Items[CatIDListBox.ItemIndex];
end;
//---------------------------------------------------------
function TCatalogIDEditFrame.SetPopupIndex:Boolean;
var ppoint:TPoint;
begin
  ppoint := CatalogPopupMenu.PopupPoint;
  ppoint := CatIDListBox.ScreenToClient(ppoint);
  popup_index := CatIDListBox.ItemAtPos(ppoint,True);
  if popup_index < 0 then popup_string := ''
  else popup_string := CatIDListBox.Items[popup_index];
  Result := (popup_index >=0);
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
constructor TCatalogIDEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StarPointer := nil;
  NamePointer := nil;
  popup_index := -1;
end;
//-------------------------------------------------
procedure TCatalogIDEditFrame.ChangeObject(NewObject:StarBase);
var outvalue:TStringList;
begin
  StarPointer := NewObject;
  // stuff that we always do when a change is done
  CatIDListBox.Clear;
  UseCatBtn.Enabled := False;
  RemCatBtn.Enabled := False;
  // stuff that is done when the new object is nil
  if StarPointer = nil then begin
    CatAddBtn.Enabled := False;
    NamePointer := nil;
  end
  // otherwise
  else begin
    CatAddBtn.Enabled := True;
    // getting the names and
    if StarPointer.HasNames then begin
      NamePointer := StarPointer.GetNames;
      if not NamePointer.NoCatalogs then begin
        outvalue := NamePointer.GetList;
        CatIDListBox.Items.Text := outvalue.Text;
      end;
    end
    else NamePointer := nil;
  end;
end;
//-------------------------------------------------
procedure TCatalogIDEditFrame.Reload();
var outvalue:TStringList;
begin
  if StarPointer = nil then Exit;
  CatIDListBox.Clear;
  NamePointer := StarPointer.GetNames;
  // various no catalog cases
  CatAddBtn.Enabled := (NamePointer <> nil);
  if (NamePointer = nil) or (NamePointer.NoCatalogs) then begin
    UseCatBtn.Enabled := False;
    RemCatBtn.Enabled := False;
    Exit;
  end;
  // loading as normal
  outvalue := NamePointer.GetList;
  CatIDListBox.Items.Text := outvalue.Text;
end;
//-------------------------------------------------
function TCatalogIDEditFrame.AddCatalogNamesExternal(inlist:string;  isdb:Boolean):Boolean;
var outvalue:TStringList;
begin
  // bad cases
  Result := False;
  if StarPointer = nil then Exit;
  inlist := Trim(inlist);
  if Length(inlist) = 0 then Exit;
  // going ahead
  if NamePointer = nil then NamePointer := StarPointer.MakeOrGetNames;
  if isdb then NamePointer.SetISDBCat(inlist)
  else NamePointer.SetMultipleCat(inlist);
  outvalue := NamePointer.GetList;
  CatIDListBox.Items.Text := outvalue.Text;
  Result := True;
end;
//-------------------------------------------------
procedure TCatalogIDEditFrame.SetAsSystem(NewAddBtnHandler:TNotifyEvent);
begin
  SystemLevel := True;
  AddClickHandlerPointer := NewAddBtnHandler;
  UseCatBtn.Show;
  MoveToSysHandlerPointer := nil;
  MoveID2Sys.Visible := False;
end;
//-------------------------------------------------
procedure TCatalogIDEditFrame.SetAsStar(NewMoveToSysHandlerPointer:TNotifyEvent);
begin
  SystemLevel := False;
  AddClickHandlerPointer:= nil;
  UseCatBtn.Hide;
  MoveToSysHandlerPointer := NewMoveToSysHandlerPointer;
  MoveID2Sys.Visible := True;
end;
//-------------------------------------------------
function TCatalogIDEditFrame.GetModifiedCurrentCatalog:string;
var dindex:Integer;
    cparsed:CatalogID;
    tag:string;
begin
  // no result cases
  Result := '';
  if NamePointer = nil then Exit;
  dindex := CatIDListBox.ItemIndex;
  if dindex < 0 then Exit;
  // we get the catalog strings
  cparsed := CatalogID.Create;
  cparsed.InitFull(CatIDListBox.Items[dindex],tag);
  // we check the pairs
  NameExpand(tag);
  // finishing the name
  Result := tag + ' ' + cparsed.id;
  if (cparsed.comp <> '') then Result += ' ' + cparsed.comp;
  cparsed.Free;
end;
//----------------------------------------------------------------
function TCatalogIDEditFrame.ExtractCurrentCatalog():string;
var dindex,icount,i:Integer;
    itemstr:string;
begin
  // getting and deleting the item
  dindex := CatIDListBox.ItemIndex;
  if (dindex < 0) then Exit;
  itemstr := CatIDListBox.Items[dindex];
  Result := itemstr;
  NamePointer.DeleteThis(itemstr);
  // removing from the list box, annoyingly, DeleteSelected is not fpc compatible
  if CatIDListBox.SelCount > 0 then begin
     for i:=CatIDListBox.Items.Count - 1 downto 0 do begin
      if CatIDListBox.Selected[i] then begin
         CatIDListBox.Items.Delete(i);
         Break;
      end;
     end;
  end;
  // we now reset the index
  icount := NamePointer.CatalogCount;
  if icount=0 then begin
    RemCatBtn.Enabled := False;
    UseCatBtn.Enabled := False;
    CatIDListBox.ItemIndex := -1;
  end
  else begin
    if dindex<>0 then CatIDListBox.ItemIndex := dindex-1
    else CatIDListBox.ItemIndex := 0;
  end;
end;


//#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end.

