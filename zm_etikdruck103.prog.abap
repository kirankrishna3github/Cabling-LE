* ---------------------------------------------------------------------*
*  Report  ZM_ETIKDRUCK103                                             *
*                                                                      *
* ---------------------------------------------------------------------*
*             Dätwyler AG                                              *
* ---------------------------------------------------------------------*
*                                                                      *
*     Programm-Name....: ZM_ETIKDRUCK103                               *
*     Entwickler.......: Peter Huber                                   *
*     Erstell-Datum....: 05.06.2006                                    *
*     Version..........: 1.0                                           *

*     Zweck............: Aufbereitungsprogramm für Etikettendruck      *
*                        CODESOFT Handlingsunit im Versand             *
*                        Nachrichtenart: Z001                          *
*                        Nachrichtenschema: Z001                       *
*     Aenderungen:                                                     *
*     09.09.2010 M. Raffeiner
*                FTP-Übergabe neu in Charctermode wegen Unicode
*                                                                      *
* ---------------------------------------------------------------------

************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #124
************************************************************************

REPORT zm_etikdruck103.

* ---------------------------------------------------------------------*
* Tabellen                                                             *
* ---------------------------------------------------------------------*
TABLES: mara, marc, makt,  nast, tsp03d, vekp, vbpa, kna1,
        t320, t001w, likp, zmetik, adrc.

* ---------------------------------------------------------------------*
* Include                                                              *
* ---------------------------------------------------------------------*
*Beachte: owner von zincl_etikette_002 ist DKA
*Keine Aenderung vornehmen. Kopieren !
INCLUDE: zincl_etikette_002.

* ---------------------------------------------------------------------*
* Data                                                                 *
* ---------------------------------------------------------------------*
DATA: dsn(200) type c,
      tfdsn(80),
      return_code LIKE sy-subrc.

DATA: BEGIN OF nast_key,
          objky LIKE nast-objky,
      END OF nast_key.

DATA: itab_ze001 LIKE ze001 OCCURS 0.


*FTP-Deklarationen
DATA: user(64) TYPE c,
      pwd(64) TYPE c,
      dest LIKE rfcdes-rfcdest VALUE 'SAPFTP',
      desta LIKE rfcdes-rfcdest VALUE 'SAPFTPA',
      host(64) TYPE c,
      blob_length TYPE i,
      dstlen TYPE i,
      hdl TYPE i,
      slen type i,
      key TYPE i VALUE 26101957.

data: h_eol(4) type x value '0A'.
field-symbols: <F_EOL> type c.


*$smart (F) 2010-08-02 - #124 DESCRIBE benötigt einen Modus wenn die
*$smart (F) 2010-08-02 - #124 Länge oder der Abstand angefragt wird
*$smart (F) 2010-08-02 - #124 Replaced with recommended default value.
*$smart (F) 2010-08-02 - #124 (M)

DESCRIBE FIELD pwd LENGTH dstlen IN CHARACTER    "smart: 2010-08-02 #124
  MODE .                                         "smart: 2010-08-02 #124


* ---------------------------------------------------------------------*
* Konstanten                                                           *
* ---------------------------------------------------------------------*



*----------------------------------------------------------------------*
*Verarbeiten
*----------------------------------------------------------------------*

FORM main USING return_code us_screen.

  dsn = '<PATH>T.ETIK.Z001.<OBJKY>.<ERDAT>.<ERUHR>.TXT'.

  CLEAR return_code.

  SELECT SINGLE * FROM vekp WHERE venum = nast-objky.

  IF sy-subrc NE 0.
    return_code = sy-subrc.
    EXIT.
  ENDIF.

  clear: itab_ze001.

   if vekp-vstel = '8500'.
     exit.
   endif.

    write vekp-exidv            to   ze001-exidv.

* VEKP
  ze001-vpobjkey = vekp-vpobjkey.
  ze001-erdat    = vekp-erdat.

*Warenempfänger lesen
  select single * from vbpa where vbeln =  vekp-vpobjkey
                              and posnr = '000000'
                              and parvw = 'WE'.

*aus Adressverwaltung, da im Auftrag und Lieferung
*geändert werden kann.

select single * from adrc where addrnumber = vbpa-adrnr.

  ze001-wename1 = adrc-name1.
  ze001-wename2 = adrc-name2.
  ze001-wename3 = adrc-name3.
  ze001-wename4 = adrc-street.
  ze001-weort01 = adrc-city1.
  ze001-wepstlz = adrc-post_code1.
  ze001-weland1 = adrc-country.


*Werksadresse des Absenders
*Werksadresse müsste bei mehreren Lagernummern gleich sein

  select single * from likp where vbeln =  vekp-vpobjkey.
  select single * from t320 where lgnum = likp-lgnum.
  select single * from T001w where werks = t320-werks.

  ze001-wkname1 = t001w-name1.
  ze001-wkname2 = t001w-name2.
  ze001-wkort01 = t001w-ort01.
  ze001-wkpstlz = t001w-pstlz.
  ze001-wkland1 = t001w-land1.


*Materialkurztext der Verpackungshilfsmittels
  select single * from makt where matnr = vekp-vhilm
                              and spras = 'DE'.

  ze001-maktx = makt-maktx.

*----------------------------------------------------------------*
*Transfer vorbereiten + ausführen
*----------------------------------------------------------------*
*Etikettenlayout

   write 'DGUSPWA1' to ze001-etnr.


*Druckerzusatzdaten ermitteln

  select single * from tsp03d where padest = nast-ldest.
  select single * from zmetik where rspolname = tsp03d-name.

  ze001-drnr = zmetik-zcodesoftdrucker.
  host       = zmetik-zrfcdest.
  pwd        = zmetik-zrfcauth.
  user       = zmetik-zrfcuser.

  assign h_eol to <F_EOL> casting.
  move <F_EOL> to ze001-eol.

* Satzlänge für ftp ermitteln
      blob_length = strlen( ze001 ).

*Übergabe-Tabelle für FTP füllen
  APPEND ze001  TO itab_ze001.

  PERFORM ftp.

  CLEAR return_code.

ENDFORM.
*---------------------------------------------------------------------*
*      Form  ftp
*---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ftp.

***  CALL 'AB_RFC_X_SCRAMBLE_STRING'
***    ID 'SOURCE'      FIELD pwd    ID 'KEY'         FIELD key
***    ID 'SCR'         FIELD 'X'    ID 'DESTINATION' FIELD pwd
***    ID 'DSTLEN'      FIELD dstlen.

slen = strlen( pwd ).

call function 'HTTP_SCRAMBLE'
  exporting
    source      = pwd
    sourcelen   = slen
    key         = key
  importing
    destination = pwd.

*----------------------------------------------------------------*
* FTP-Verbindung
*----------------------------------------------------------------*

  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
            user            = user
            password        = pwd
            host            = host
            rfc_destination = desta
       IMPORTING
            handle          = hdl.

*----------------------------------------------------------------*
* Daten auf den Server stellen
*----------------------------------------------------------------*

*Filname aufbereiten
  REPLACE '<PATH>'  WITH ZMETIK-ZPATH INTO dsn.
  REPLACE '<OBJKY>' WITH nast-objky INTO dsn.
  REPLACE '<ERDAT>' WITH nast-erdat INTO dsn.
  REPLACE '<ERUHR>' WITH nast-eruhr INTO dsn.
  CONDENSE dsn NO-GAPS.

  CALL FUNCTION 'FTP_R3_TO_SERVER'
       EXPORTING
            handle      = hdl
            fname       = dsn
            character_mode = 'X'
       TABLES
            text        = itab_ze001.
  .

*----------------------------------------------------------------*
*                                FTP-Verbindung schliessen
*----------------------------------------------------------------*

  CALL FUNCTION 'FTP_DISCONNECT'
       EXPORTING
            handle = hdl.

ENDFORM.                    " anstoss_mit_ftp
