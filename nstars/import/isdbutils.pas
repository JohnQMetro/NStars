unit isdbutils;

{$MODE Delphi}

interface

uses locationdata, estutils;

(* the purpose of this function is to produce an url that looks
up stars within a few arcminutes of the given location at ISDB *)
function MakeISDBSearchUrl(indata:LocationInfo; lypad:Real):string;

implementation
//----------------------------------------------------------------------
(* the purpose of this function is to produce an url that looks
up stars within 1 arcminutes of the given location at ISDB *)
function MakeISDBSearchUrl(indata:LocationInfo; lypad:Real):string;
const isdburl_start = 'http://www.stellar-database.com/Scripts/find_neighbors.exe?';
      xyzstring     = '&X=0.0&Y=0.0&Z=0.0';
var   buffer:string;
      lymax,tempv:Real;
      newsec:Real;
      newmin,newhr:Integer;
      neg:Boolean;
(*
http://www.stellar-database.com/Scripts/find_neighbors.exe?
ly=80.1
&X=0.0&Y=0.0&Z=0.0
&minRAHour=01&minRAMin=02&minRASec=0.0
&maxRAHour=01&maxRAMin=03&maxRASec=0
&minDecDeg=5&minDecMin=03&minDecSec=0
&maxDecDeg=5&maxDecMin=04&maxDecSec=0
*)

begin
  // we start building the string
  Result := isdburl_start + 'ly=';
  // converting the distance
  tempv := indata.GetLYDistance;
  tempv := tempv * lypad;
  Str(tempv:2,buffer);
  // adding the distance
  Result := Result + buffer;
  // we add the origin
  Result := Result + xyzstring;
  // next up, we calculate the min RA, by substracting 30 arcseconds...
  newsec := indata.RAget_Arcsec - 30;
  if newsec<0 then begin
    newmin := indata.RA_minutes -1;
    newsec := newsec + 60;
  end
  else newmin := indata.RA_minutes;
  if newmin<0 then begin
    newmin := 59;
    newhr := indata.RA_hours-1;
    if newhr<1 then newhr := 23;
  end
  else newhr := indata.RA_hours;
  // adding the min RA to the result string
  Result := Result + '&minRAHour=';
  Str(newhr,buffer);
  Result := Result + buffer;
  Result := Result + '&minRAMin=';
  Str(newmin,buffer);
  Result := Result + buffer;
  Result := Result + 'minRASec=';
  Str(newsec:2,buffer);
  Result := Result + buffer;
  // next up, we calculate the max RA, by adding 30 arcseconds...
  newsec := indata.RAget_Arcsec + 30;
  if newsec>=60 then begin
    newmin := indata.RA_minutes +1;
    newsec := newsec - 60;
  end
  else newmin := indata.RA_minutes;
  if newmin>=60 then begin
    newmin := 0;
    newhr := indata.RA_hours+1;
    if newhr=24 then newhr := 0;
  end
  else newhr := indata.RA_hours;
  // adding the min RA to the result string
  Result := Result + '&maxRAHour=';
  Str(newhr,buffer);
  Result := Result + buffer;
  Result := Result + '&maxRAMin=';
  Str(newmin,buffer);
  Result := Result + buffer;
  Result := Result + 'maxRASec=';
  Str(newsec:2,buffer);
  Result := Result + buffer;
  (* next is the declination, we deflect by a few degress since a second of
  declination is 15 times bigger than a second of RA *)
  tempv := indata.DECget_Value;
  tempv := tempv-420;
  if tempv<-324000 then tempv := -324000;
  neg := (tempv<0);  
  tempv := Abs(tempv);
  // splitting the value into degress, minutes, and seconds
  newhr := Trunc(tempv/3600);
  lymax := tempv - 3600*newhr;
  newmin := Trunc(lymax/60);
  lymax := lymax - 60*newmin;
  newsec:= lymax;
  if neg then newhr := -newhr;
  // outputting in the result
  Result := Result + '&minDecDeg=';
  Str(newhr,buffer);
  Result := Result + buffer;
  Result := Result + '&minDecMin=';
  Str(newmin,buffer);
  Result := Result + buffer;
  Result := Result + '&minDecSec=';
  Str(newsec,buffer);
  Result := Result + buffer;
  (* next is the declination, we deflect by a few degress since a second of
  declination is 15 times bigger than a second of RA *)
  tempv := indata.DECget_Value;
  tempv := tempv+420;
  if tempv>324000 then tempv := 324000;
  neg := (tempv<0);  
  tempv := Abs(tempv);
  // splitting the value into degress, minutes, and seconds
  newhr := Trunc(tempv/3600);
  lymax := tempv - 3600*newhr;
  newmin := Trunc(lymax/60);
  lymax := lymax - 60*newmin;
  newsec:= lymax;
  if neg then newhr := -newhr;
  // outputting in the result
  Result := Result + '&maxDecDeg=';
  Str(newhr,buffer);
  Result := Result + buffer;
  Result := Result + '&maxDecMin=';
  Str(newmin,buffer);
  Result := Result + buffer;
  Result := Result + '&maxDecSec=';
  Str(newsec,buffer);
  Result := Result + buffer;
end;

end.
