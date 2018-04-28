unit gaiadr2match;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TGaiaDR2Picker }

  TGaiaDR2Picker = class(TForm)
    AutoMatchCB: TCheckBox;
    MatchSelectedBtn: TButton;
    SkipStarBtn: TButton;
    StarInfo2Lbl: TLabel;
    Star2MatchLbl: TLabel;
    StarInfo1Lbl: TLabel;
    MatchCountLbl: TLabel;
    StarListGrid: TStringGrid;
    StartMatchBtn: TToggleBox;
  private

  public

  end;

var
  GaiaDR2Picker: TGaiaDR2Picker;

implementation

{$R *.lfm}

end.

