unit VizierPOST;

{$mode delphi}

interface

(* Making POST data to fetch stuff from VizieR *)

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
uses
  Classes, SysUtils, Synacode;

const
  POSTPART1 = '&-out.max=50&%2F%2FCDSportal=http%3A%2F%2Fcdsportal.u-strasbg.fr%2FStoreVizierData.html&-out.form=HTML+Table&-out.add=_r&-out.add=_RAJ%2C_DEJ&%2F%2Foutaddvalue=default&-sort=_r&-oc.form=sexa&-nav=';
  POSTPART2 = '%26HTTPPRM%3A%26-out.max%3D50%26-out.form%3DHTML+Table%26-out.add%3D_r%26-out.add%3D_RAJ%2C_DEJ%26-sort%3D_r%26-oc.form%3Dsexa%26&-c=&-c.eq=J2000&-c.r=++2&-c.u=arcmin&-c.geom=r';
  POSTPARTX = '%26HTTPPRM%3A%26-out.max%3D50%26-out.form%3DHTML+Table%26-out.add%3D_r%26-out.add%3D_RAJ%2C_DEJ%26-sort%3D_r%26-oc.form%3Dsexa%26&-c=';
  POSTPARTB = '%26-out.max%3D50%26-out.form%3DHTML+Table%26-out.add%3D_r%26-out.add%3D_RAJ%2C_DEJ%26-sort%3D_r%26-order%3DI%26-oc.form%3Dsexa%26-meta.foot%3D1%26-meta%3D1%26-meta.ucd%3D2%26-c.eq%3DJ2000%26-c.r%3D++2%26-c.u%3Darcmin%26-c.geom%3Dr%26-usenav%3D1%26-bmark%3DPOST%26&-c=&-c.eq=J2000&-c.r=++2&-c.u=arcmin&-c.geom=r';
  POSTPART3 = '&%2F%2Fnoneucd1p=on&-file=.&-meta.ucd=2&-meta=1&-meta.foot=1&-usenav=1&-bmark=POST';

// previous get magnitude stuff
function MakeVizAPASS_Params(targetname:string; radius:Real):string;
function MakeViz2MASS_Post(targetstr:string):string;
function MakeVizGaiaDR2_Post(targetstr:string):string;
// For getting identifiers
function MakeUCAC4_Post(const tmass:string; J,H,Ks:Currency):string;
function MakeGaiaDR1_Post(const gra,gdex:Real; const gbright,gdim:Real):string;
function MakeDENIS_Post(const tmass:string; const asec:Word; Je,Ke:Real):string;

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
implementation
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function MakeVizAPASS_Params(targetname:string; radius:Real):string;
begin
  Result := '-to=4&-from=-3&-this=-3&%2F%2Fsource=II%2F336%2Fapass9&%2F%2F';
  Result += 'tables=II%2F336%2Fapass9' + POSTPART1;
  Result += 'cat%3AII%2F336%26tab%3A%7BII%2F336%2Fapass9%7D%26key%3Asource%3D';
  Result += 'II%2F336%2Fapass9' + POSTPARTX + EncodeURLElement(targetname);
  Result += '&-c.eq=J2000&-c.r=' + Trim(FloatToStrF(radius,ffFixed,4,2));
  Result += '&-c.u=arcmin&-c.geom=r&-source=II%2F336%2Fapass9&-order=I';
  Result += '&-out.orig=standard&-out=recno&recno=&-out=RAJ2000&RAJ2000=';
  Result += '&-out=DEJ2000&DEJ2000=&e_RAJ2000=&e_DEJ2000=&-out=Field&Field=';
  Result += '&-out=nobs&nobs=&-out=mobs&mobs=&-out=B-V&B-V=&-out=e_B-V&e_B-V=';
  Result += '&-out=Vmag&Vmag=&-out=e_Vmag&e_Vmag=&u_e_Vmag=&-out=Bmag&Bmag=';
  Result += '&-out=e_Bmag&e_Bmag=&u_e_Bmag=&-out=g%27mag&g%27mag=&-out=e_g%27';
  Result += 'mag&e_g%27mag=&u_e_g%27mag=&-out=r%27mag&r%27mag=&-out=e_r%27mag';
  Result += '&e_r%27mag=&u_e_r%27mag=&-out=i%27mag&i%27mag=&-out=e_i%27mag';
  Result += '&e_i%27mag=&u_e_i%27mag=' + POSTPART3;
end;

function MakeViz2MASS_Post(targetstr:string):string;
begin
  Result := '-to=4&-from=-2&-this=-2&%2F%2Fsource=II%2F246&%2F%2Ftables=II';
  Result += '%2F246%2Fout&-out.max=50&%2F%2FCDSportal=http%3A%2F%2F';
  Result += 'cdsportal.u-strasbg.fr%2FStoreVizierData.html&-out.form=';
  Result += 'HTML+Table&%2F%2Foutaddvalue=default&-oc.form=sexa&-nav=cat%3AII';
  Result += '%2F246%26tab%3A%7BII%2F246%2Fout%7D%26key%3Asource';
  Result += '%3DII%2F246%26HTTPPRM%3A%26&-c=&-c.eq=J2000&-c.r=++2&-c.u=';
  Result += 'arcmin&-c.geom=r&-source=II%2F246%2Fout&-order=I&-out.orig=';
  Result += 'standard&-out=RAJ2000&RAJ2000=&-out=DEJ2000&DEJ2000=&errMaj=';
  Result += '&errMin=&errPA=&-out=2MASS&2MASS=' + EncodeURLElement(targetstr);
  Result += '&-out=Jmag&';
  Result += 'Jmag=&Jcmsig=&-out=e_Jmag&e_Jmag=&Jsnr=&-out=Hmag&Hmag=&Hcmsig=&';
  Result += '-out=e_Hmag&e_Hmag=&Hsnr=&-out=Kmag&Kmag=&Kcmsig=&-out=e_Kmag&';
  Result += 'e_Kmag=&Ksnr=&-out=Qflg&Qflg=&Rflg=&Bflg=&-out=Cflg&Cflg=&Ndet=';
  Result += '&prox=&pxPA=&pxCntr=&-out=Xflg&Xflg=&-out=Aflg&Aflg=&Cntr=&Hemis=';
  Result += '&Date=&Scan=&GLON=&GLAT=&Xscan=&JD=&Jpsfchi=&Hpsfchi=&Kpsfchi=';
  Result += '&Jstdap=&e_Jstdap=&Hstdap=&e_Hstdap=&Kstdap=&e_Kstdap=&edgeNS=';
  Result += '&edgeEW=&edge=&dup=&use=&opt=&Dopt=&PAopt=&Bmag=&Rmag=&Nopt=';
  Result += '&extKey=&scanKey=&coaddKey=&coadd=&-ignore=Opt%3D*&Opt=Opt';
  Result += POSTPART3;
end;

function MakeVizGaiaDR2_Post(targetstr:string):string;
begin
  Result := '-to=4&-from=-3&-this=-3&%2F%2Fsource=I%2F345%2Fgaia2&%2F%2F';
  Result += 'tables=I%2F345%2Fgaia2&-out.max=50&%2F%2FCDSportal=http%3A%2F%2F';
  Result += 'cdsportal.u-strasbg.fr%2FStoreVizierData.html&-out.form=';
  Result += 'HTML+Table&%2F%2Foutaddvalue=default&-oc.form=sexa&-nav=cat';
  Result += '%3AI%2F345%26tab%3A%7BI%2F345%2Fgaia2%7D%26key%3Asource%3D';
  Result += 'I%2F345%2Fgaia2%26HTTPPRM%3A%26&-c=&-c.eq=J2000&-c.r=++2&-c.u=';
  Result += 'arcmin&-c.geom=r&-source=I%2F345%2Fgaia2&-order=I&-out.orig=';
  Result += 'standard&DR2Name=&-out=RA_ICRS&RA_ICRS=&e_RA_ICRS=&-out=DE_ICRS';
  Result += '&DE_ICRS=&e_DE_ICRS=&SolID=&-out=Source&Source=';
  Result += EncodeURLElement(targetstr);
  Result += '&RandomI=&Epoch=&-out=Plx&Plx=&-out=e_Plx&e_Plx=&RPlx=&-out=pmRA';
  Result += '&pmRA=&e_pmRA=&-out=pmDE&pmDE=&e_pmDE=&RADEcor=&RAPlxcor=';
  Result += '&RApmRAcor=&RApmDEcor=&DEPlxcor=&DEpmRAcor=&DEpmDEcor=&PlxpmRAcor';
  Result += '=&PlxpmDEcor=&pmRApmDEcor=&NAL=&NAC=&NgAL=&NbAL=&gofAL=&chi2AL';
  Result += '=&epsi=&sepsi=&Solved=&APF=&WAL=&pscol=&e_pscol=&fvarpi=';
  Result += '&MatchObsA=&Nper=&amax=&type=&MatchObs=&Dup=&o_Gmag=&FG=&e_FG=';
  Result += '&RFG=&-out=Gmag&Gmag=&-out=e_Gmag&e_Gmag=&o_BPmag=&FBP=&e_FBP=';
  Result += '&RFBP=&-out=BPmag&BPmag=&-out=e_BPmag&e_BPmag=&o_RPmag=&FRP=';
  Result += '&e_FRP=&RFRP=&-out=RPmag&RPmag=&-out=e_RPmag&e_RPmag=';
  Result += '&E%28BR%2FRP%29=&Mode=&BP-RP=&BP-G=&G-RP=&-out=RV&RV=&-out=e_RV';
  Result += '&e_RV=&o_RV=&Tefftemp=&loggtemp=&%5BFe%2FH%5Dtemp=&Var=&GLON=';
  Result += '&GLAT=&ELON=&ELAT=&fPriam=&Teff=&b_Teff=&B_Teff=&AG=&b_AG=&B_AG';
  Result += '=&E%28BP-RP%29=&b_E%28BP-RP%29=&B_E%28BP-RP%29=&fFLAME=&Rad=&b_Rad';
  Result += '=&B_Rad=&Lum=&b_Lum=&B_Lum=&RAJ2000=&e_RAJ2000=&DEJ2000';
  Result += '=&e_DEJ2000=' + POSTPART3;
end;
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(* Used to find a UCAC4 id using 2MASS info *)
function MakeUCAC4_Post(const tmass:string; J,H,Ks:Currency):string;
var pos_string:string;
begin
  Result := '-to=4&-from=-2&-this=-2&%2F%2Fsource=ucac4&%2F%2Ftables=';
  Result += 'I%2F322A%2Fout' + POSTPART1 + 'cat%3AI%2F322A%26tab%3A%7BI';
  Result += '%2F322A%2Fout%7D%26key%3Asource%3Ducac4%26HTTPPRM%3A%26%26%26';
  Result += '-out.max%3D50%26-out.form%3DHTML+Table%26-out.add%3D_r%26';
  Result += '-out.add%3D_RAJ%2C_DEJ%26-sort%3D_r%26-order%3DI%26-oc.form%3D';
  Result += 'sexa%26-meta.foot%3D1%26-meta%3D1%26-meta.ucd%3D2%26-c.eq%3D';
  Result += 'J2000%26-c.r%3D++2%26-c.u%3Darcmin%26-c.geom%3Dr%26-usenav';
  Result += '%3D1%26-bmark%3DPOST%26&-c=' + EncodeURLElement(tmass);
  Result += '&-c.eq=J2000&-c.r=++30&-c.u=arcsec&-c.geom=r&-source=I%2F322A';
  Result += '%2Fout&-order=I&-out.orig=standard&-out=UCAC4&UCAC4=';
  Result += '&-out=RAJ2000&RAJ2000=&e_RAJ2000=&-out=DEJ2000&DEJ2000=';
  Result += '&e_DEJ2000=&-out=ePos&ePos=&EpRA=&EpDE=&f.mag=&a.mag=&e_a.mag=';
  Result += '&of=&db=&Na=&Nu=&Nc=&pmRA=&e_pmRA=&pmDE=&e_pmDE=&MPOS1=&UCAC2=';
  Result += '&Tycho-2=&2Mkey=&-out=Jmag&Jmag=' + CurrToStrF(J,ffFixed,3);
  Result += '&e_Jmag=&q_Jmag=&-out=Hmag&Hmag=' + CurrToStrF(H,ffFixed,3);
  Result += '&e_Hmag=&q_Hmag=&-out=Kmag&Kmag=' + CurrToStrF(Ks,ffFixed,3);
  Result += '&e_Kmag=&q_Kmag=&Bmag=&e_Bmag=&f_Bmag=&Vmag=&e_Vmag=&f_Vmag=';
  Result += '&gmag=&e_gmag=&f_gmag=&rmag=&e_rmag=&f_rmag=&imag=&e_imag=';
  Result += '&f_imag=&g=&c=&H=&A=&b=&h=&Z=&B=&L=&N=&S=&LEDA=&2MX=';
  Result += POSTPART3;
end;
//-----------------------------------
(* For getting Gaia DR1: search by position and magnitude *)
function MakeGaiaDR1_Post(const gra,gdex:Real; const gbright,gdim:Real):string;
var pos_string,mag_string:string;
begin
  pos_string := Trim(FloatToStrF(gra,ffFixed,10,6)) + ' ';
  pos_string := EncodeURLElement(pos_string + Trim(FloatToStrF(gdex,ffFixed,9,5)));
  mag_string := Trim(FloatToStrF(gbright,ffFixed,5,2)) + ' .. ';
  mag_string := EncodeURLElement(mag_string + Trim(FloatToStrF(gdim,ffFixed,5,2)));

  Result := '-to=4&-from=-3&-this=-3&%2F%2Fsource=I%2F337%2Fgaia&%2F%2F';
  Result += 'tables=I%2F337%2Fgaia' + POSTPART1;
  Result += 'cat%3AI%2F337%26tab%3A%7BI%2F337%2Fgaia%7D%26key%3Asource%3DI';
  Result += '%2F337%2Fgaia' + POSTPARTX + pos_string;
  Result += '&-c.eq=J2000&-c.r=6&-c.u=arcsec&-c.geom=r&-source=I%2F337%2Fgaia';
  Result += '&-order=I&-out.orig=standard&RA_ICRS=&e_RA_ICRS=&DE_ICRS=&';
  Result += 'e_DE_ICRS=&SolID=&-out=Source&Source=&RandomI=&Epoch=&Plx=&e_Plx=';
  Result += '&pmRA=&e_pmRA=&pmDE=&e_pmDE=&RADEcor=&RAPlxcor=&RApmRAcor=';
  Result += '&RApmDEcor=&DEPlxcor=&DEpmRAcor=&DEpmDEcor=&PlxpmRAcor=';
  Result += '&PlxpmDEcor=&pmRApmDEcor=&NAL=&NAC=&NgAL=&NgAC=&NbAL=&NbAC=&DQ=';
  Result += '&epsi=&sepsi=&APF=&ARF=&WAL=&WAC=&Apr=&MatchObs=&Dup=&sK1=&sK2=';
  Result += '&sK3=&sK4=&mK1=&mK2=&mK3=&mK4=&o_%3CGmag%3E=';
  Result += '&-out=%3CFG%3E&%3CFG%3E=&-out=e_%3CFG%3E&e_%3CFG%3E=';
  Result += '&-out=%3CGmag%3E&%3CGmag%3E=' + mag_string + '&Var=&GLON=&GLAT=';
  Result += '&ELON=&ELAT=' + POSTPART3;
end;
//-----------------------------------------
function MakeDENIS_Post(const tmass:string; const asec:Word; Je,Ke:Real):string;
var jmin,jmax,kmin,kmax:Real;
    jrange,krange:string;
begin
  jmin := Je - 0.35;  jmax := Je + 0.35;
  kmin := Ke - 0.35;  kmax := Ke + 0.35;
  jrange := Trim(FloatToStrF(jmin,ffFixed,6,2)) + ' .. ';
  jrange := EncodeURLElement(jrange + Trim(FloatToStrF(jmax,ffFixed,6,2)));
  krange := Trim(FloatToStrF(kmin,ffFixed,6,2)) + ' .. ';
  krange := EncodeURLElement(krange + Trim(FloatToStrF(kmax,ffFixed,6,2)));
  // building the post
  Result := '-to=4&-from=-3&-this=-3&%2F%2Fsource=B%2Fdenis&%2F%2Ftables=B%2F';
  Result += 'denis%2Fdenis' + POSTPART1 + 'cat%3AB%2Fdenis%26tab%3A%7BB%2F';
  Result += 'denis%2Fdenis%7D%26key%3Asource%3DB%2Fdenis' + POSTPARTX;
  Result += EncodeURLElement(tmass) + '&-c.eq=J2000&-c.r=' + IntToStr(asec);
  Result += '&-c.u=arcsec&-c.geom=r&-source=B%2Fdenis%2Fdenis&-order=I';
  Result += '&-out.orig=standard&Image=&Strip=&-out=RAJ2000&RAJ2000=';
  Result += '&-out=DEJ2000&DEJ2000=&-out=Imag&Imag=&-out=e_Imag&e_Imag=';
  Result += '&-out=Jmag&Jmag=' + jrange + '&-out=e_Jmag&e_Jmag=&-out=Kmag';
  Result += '&Kmag=' + krange + '&-out=e_Kmag&e_Kmag=&Imag5.5=&e_Imag5.5=';
  Result += '&Jmag5.5=&e_Jmag5.5=&Kmag5.5=&e_Kmag5.5=&Imag4.5=&e_Imag4.5=';
  Result += '&Jmag4.5=&e_Jmag4.5=&Kmag4.5=&e_Kmag4.5=&Imag3.5=&e_Imag3.5=';
  Result += '&Jmag3.5=&e_Jmag3.5=&Kmag3.5=&e_Kmag3.5=&Imag3.5c=&e_Imag3.5c=';
  Result += '&Jmag3.5c=&e_Jmag3.5c=&Kmag3.5c=&e_Kmag3.5c=&Imag2.5c=';
  Result += '&e_Imag2.5c=&Jmag2.5c=&e_Jmag2.5c=&Kmag2.5c=&e_Kmag2.5c=';
  Result += '&Imag1.5c=&e_Imag1.5c=&Jmag1.5c=&e_Jmag1.5c=&Kmag1.5c=';
  Result += '&e_Kmag1.5c=&Rmag=&Bmag=&q_Imag=&q_Jmag=&q_Kmag=&Ipsf=&Jpsf=';
  Result += '&Kpsf=&IMpsf=&JMpsf=&KMpsf=&Ix=&Iy=&Jx=&Jy=&Kx=&Ky=&A2RAdeg=';
  Result += '&A2DEdeg=&A2Ep=&Dist=&ObsJD=&Iflg=&Jflg=&Kflg=&mult=';
  Result += '&-ignore=Opt%3D*&Opt=Opt&-out=DENIS&DENIS=' + POSTPART3;
end;



//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

end.

