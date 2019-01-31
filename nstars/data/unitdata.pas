unit unitdata;

{$MODE Delphi}

interface

uses StrUtils;

type
  TSysCat = array[0..31] of string;

const
  constellations:array[0..263] of string = ('Andromeda','Andromedae','And',
    'Antlia','Antliae','Ant','Apus','Apodis','Aps','Aquarius','Aquarii','Aqr',
    'Aquila','Aquilae','Aql','Ara','Arae','Ara','Aries','Arietis','Ari',
    'Auriga','Aurigae','Aur','Boötes','Boötis','Boo','Caelum','Caeli','Cae',
    'Camelopardalis','Camelopardalis','Cam','Cancer','Cancri','Cnc',
    'Canes Venatici','Canum Venaticorum','CVn',
    'Canis Major','Canis Majoris','CMa','Canis Minor','Canis Minoris','CMi',
    'Capricornus','Capricorni','Cap','Carina','Carinae','Car',
    'Cassiopeia','Cassiopeiae','Cas','Centaurus','Centauri','Cen',
    'Cepheus','Cephei','Cep','Cetus','Ceti','Cet','Chamaeleon','Chamaeleontis','Cha',
    'Circinus','Circini','Cir','Columba','Columbae','Col',
    'Coma Berenices','Comae Berenices','Com',
    'Corona Australis','Coronae Australis','CrA',
    'Corona Borealis','Coronae Borealis','CrB','Corvus','Corvi','Crv',
    'Crater','Crateris','Crt','Crux','Crucis','Cru','Cygnus','Cygni','Cyg',
    'Delphinus','Delphini','Del','Dorado','Doradus','Dor',
    'Draco','Draconis','Dra','Equuleus','Equulei','Equ','Eridanus','Eridani','Eri',
    'Fornax','Fornacis','For','Gemini','Geminorum','Gem',
    'Grus','Gruis','Gru','Hercules','Herculis','Her',
    'Horologium','Horologii','Hor','Hydra','Hydrae','Hya',
    'Hydrus','Hydri','Hyi','Indus','Indi','Ind','Lacerta','Lacertae','Lac',
    'Leo','Leonis','Leo','Leo Minor','Leonis Minoris','LMi',
    'Lepus','Leporis','Lep','Libra','Librae','Lib','Lupus','Lupi','Lup',
    'Lynx','Lyncis','Lyn','Lyra','Lyrae','Lyr','Mensa','Mensae','Men',
    'Microscopium','Microscopii','Mic','Monoceros','Monocerotis','Mon',
    'Musca','Muscae','Mus','Norma','Normae','Nor','Octans','Octantis','Oct',
    'Ophiuchus','Ophiuchi','Oph','Orion','Orionis','Ori',
    'Pavo','Pavonis','Pav','Pegasus','Pegasi','Peg','Perseus','Persei','Per',
    'Phoenix','Phoenicis','Phe','Pictor','Pictoris','Pic',
    'Pisces','Piscium','Psc','Piscis Austrinus','Piscis Austrini','PsA',
    'Puppis','Puppis','Pup','Pyxis','Pyxidis','Pyx','Reticulum','Reticuli','Ret',
    'Sagitta','Sagittae','Sge','Sagittarius','Sagittarii','Sgr',
    'Scorpius','Scorpii','Sco','Sculptor','Sculptoris','Scl',
    'Scutum','Scuti','Sct','Serpens','Serpentis','Ser',
    'Sextans','Sextantis','Sex','Taurus','Tauri','Tau',
    'Telescopium','Telescopii','Tel','Triangulum','Trianguli','Tri',
    'Triangulum Australe','Trianguli Australis','TrA','Tucana','Tucanae','Tuc',
    'Ursa Major','Ursae Majoris','UMa','Ursa Minor','Ursae Minoris','UMi',
    'Vela','Velorum','Vel','Virgo','Virginis','Vir',
    'Volans','Volantis','Vol','Vulpecula','Vulpeculae','Vul');

  bayerdes:array[0..66] of string = ('Alpha','Beta','Gamma','Delta',
    'Epsilon','Zeta','Eta','Theta','Iota','Kappa','Lambada','Mu','Nu',
    'Xi','Omicron','Pi','Rho','Sigma','Tau','Upsilon','Phi','Chi',
    'Psi','Omega','a','b','c','d','e','f','g','h','i','j','k','l','m',
    'n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D',
    'E','F','G','H','I','J','K','L','M','N','O','P','Q');
  bayergreek:string = 'αβγδεζηθικλμνξοπστυφχψω';

  completters:array[0..11] of string = ('A','B','C','D','E','F','Aa','Ab',
    'Ba','Bb','Ca','Cb');

  vtype:array[0..5] of string = (' ','UV Ceti type','BY Draconis type',
        'Delta Scuti type','ZZ Ceti type','RS Can.Ven. type');

  catexp:array[0..61] of string = ('G','Giclas','Gl','Gliese','L','Luyten',
           'Vys','Vyssotsky','Sm','Smethells','Kr','Krüger','Wo','Woolley',
           'VB','Van Biesbroeck','Gr','Greenstein','Kui','Kuiper',
           'KUI','Kuiper','Kpr','Kuiper','Gmb','Groombridge','Zkh','Zakhozhaj',
           'Lal','Lalande','Lac','Lacaille','San','Sanduleak','Bu','Burnham',
           'Cin','Cincinnati','Cou','Couteau','H','Herschel','HJ','J.Herschel',
           'I','Innes','Mel','Melotte','Rob','Roberts','Wor','Worley',
           'Tou','Toulouse','Steph','Stephenson',
           'Tok','Tokovinin','Wg','Wegner','Ton','Tonantzintla');

  prefcats:array[0..33] of string = ('Gl','GJ','Wolf','Ross','HD','Hip', 'LHS',
        '2MASS','Tyc','BD','CD','L','WD','LP','LTT','NLTT','CPD','VB','EG','Gr',
        'UCAC4','Wo','G','Vys','DENIS','WISE','Gmb','Luhman','SCR','Sao','YPC',
        'Sm','WISEA','GaiaDR2');

  ArityLabels:array[0..6] of string = ('Single','Possibly Double',
      'Spectroscopic Binary', 'Other Binary', 'Double or Multiple',
      'Eclipsing Binary','White Dwarf Companion');

  const sys_cats:TSysCat = ('Gl','GJ','ADS','WDS','BD','HD','CD','CPD','Struve',
       'Vys','Bu','Kpr','LDS','Luhman','Sm','Wo','I','HJ','Wor', 'LSPM','H',
       'Kr','Tok','Balega','O.Struve','Stein','Tok','Rob', 'Scholz', 'Balega',
       'UPM','PM');

type

FType = (NONE,ZEROS,SPACES);

(* Because for so many multiples, we do not have data on individual parts,
I will instead let one object stand for many, with a tracking type... *)
ArityType =(SINGLE, POSSIBLY_DOUBLE, SPECTROCOPIC_BINARY, OTHER_BINARY,
                    DOUBLE_OR_MULTIPE, ECLIPSING_BINARY, WHITE_DWARF_BINARY);

TVtypeIndex = 0..(length(vtype)-1);

// white dwarf atmosphere type (WD_D means unspecified)
WDAtmosEnum = (WD_D,WD_H,WD_He);

function NameExpand(var namein:string):Boolean;

implementation
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@
function NameExpand(var namein:string):Boolean;
var arrindex:Integer;
begin
  Result := False;
  arrindex := AnsiIndexStr(namein,catexp);
  if arrindex < 0 then Exit;
  if Odd(arrindex) then Exit;
  Result:= True;
  namein := catexp[arrindex+1];
end;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end.
