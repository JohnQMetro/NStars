unit catalogedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Dialogs, Menus, Clipbrd,
  StarDataBase, namedata, unitdata, simbad, df_strings, LCLIntf;

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
    RemCatBtn: TButton;
    CatIDListBox: TListBox;
    UseCatBtn: TButton;
    procedure CatAddBtnClick(Sender: TObject);
    procedure CatalogPopupMenuPopup(Sender: TObject);
    procedure CatIDListBoxClick(Sender: TObject);
    procedure CopyIDMIClick(Sender: TObject);
    procedure LaunchSearchIDClick(Sender: TObject);
    procedure LaunchSimbadIDClick(Sender: TObject);
    procedure RemCatBtnClick(Sender: TObject);
    procedure UseCatBtnClick(Sender: TObject);
  private
    { private declarations }
    StarPointer:StarBase;
    NamePointer:StarName;
    SystemLevel:Boolean;
    AddClickHandlerPointer:TNotifyEvent;
    popup_index:Integer;
    popup_string:string;
    function GetCC:string;
    function SetPopupIndex:Boolean;
  public
    { public declarations }
    constructor Create(AOwner: TComponent) ; override;
    procedure ChangeObject(NewObject:StarBase);
    procedure SetSystem(is_system:Boolean; NewAddBtnHandler:TNotifyEvent);
    function AddCatalogNamesExternal(inlist:string; isdb:Boolean):Boolean;
    property CurrentCatalog:string read GetCC;
    function GetModifiedCurrentCatalog:string;
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

procedure TCatalogIDEditFrame.RemCatBtnClick(Sender: TObject);
var dindex,icount,i:Integer;
begin
  // getting and deleting the item
  dindex := CatIDListBox.ItemIndex;
  NamePointer.DelByIndex(dindex);
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
  icount := NamePointer.GetCatalogCount;
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
      if NamePointer.GetCatalogCount <> 0 then begin
        outvalue := NamePointer.GetList;
        CatIDListBox.Items.Text := outvalue.Text;
      end;
    end
    else NamePointer := nil;
  end;
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
procedure TCatalogIDEditFrame.SetSystem(is_system:Boolean; NewAddBtnHandler:TNotifyEvent);
begin
  SystemLevel := is_system;
  if is_system then begin
    AddClickHandlerPointer := NewAddBtnHandler;
    UseCatBtn.Show
  end
  else begin
    AddClickHandlerPointer:= nil;
    UseCatBtn.Hide;
  end;
end;
//-------------------------------------------------
function TCatalogIDEditFrame.GetModifiedCurrentCatalog:string;
var dindex:Integer;
    s1,s2,s3:string;
begin
  // no result cases
  Result := '';
  if NamePointer = nil then Exit;
  dindex := CatIDListBox.ItemIndex;
  if dindex < 0 then Exit;
  // we get the catalog string
  s1 := NamePointer.GetCatalog(dindex);
  // to transform, we also need the parts
  s2 := NamePointer.GetCCode(s1);
  NamePointer.GetCatValue(s2,s3);
  // we check the pairs
  NameExpand(s2);
  // finishing the name
  Result := s2 + ' ' + s3;
end;

end.

