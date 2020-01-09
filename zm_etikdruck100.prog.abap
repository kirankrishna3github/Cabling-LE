
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #115 #124 #166
************************************************************************

Report ZM_ETIKDRUCK100.
************************************************************************
*Beschreibung:
************************************************************************
*Der Report selektiert zum Zeitpunkt der Wareneingangsbuchung Daten,
*welche in eine Uebergabestruktur gestellt werden und dann mittels
*Filetransfer auf den Server geschrieben werden.
*Im Ausgangsverzeichnis werden diese von UC4 abgeholt und CODESOFT
*zur Verfügung gestellt.
*
*03.02.2005; SAP Stäfa, Peter Huber

************************************************************************
*Aenderungen
************************************************************************
*27.04.2005; Peter Huber
*Die Etikettenanzahl wird nicht mehr aus der Pos.menge bestimmt, sondern
*aus der Cust.tabelle ZMETIK2.
*09.09.2010; M. Raffeiner
*FTP-Übergabe neu in Charctermode wegen Unicode


************************************************************************
*Tabellen
************************************************************************
TABLES: mara, marc, makt, mkpf, mseg, mch1, mchb, mard, lfa1,
        nast, tsp03d, klah,
        zmetik,           "Customizingtabelle
*        zmm_anz_etik,    "Anzahl Etiketten aus WE Buchung
        zmmdrucker,       "Customizing Drucker & Layout
        ZMETIK2.          "Zusatzdaten Etikettierung

*Uebergabestrukur
INCLUDE: zincl_etikette_100.

************************************************************************
*Konstanten
************************************************************************
*Pfad, Name und Extension der Datei
CONSTANTS: dsn(200) VALUE
   '<PATH>T.ETIK.ZE01.<OBJKY>.<ERDAT>.<ERUHR>.<TEST>'.

*Benutzerparameter ZM_CODESOFT_TEST = 'X' ermöglicht download auf
*CODESOFT Server, ohne das UC4 die Datei abholt !
CONSTANTS: tmp(3) VALUE 'TMP',
           txt(3) VALUE 'TXT'.

************************************************************************
*Workfelder
************************************************************************
DATA: wlifnr(10),
      wmatnr(18).

************************************************************************
*Data
************************************************************************
DATA: drucker_name(30)  TYPE c,
       tfdsn(200).

*FTP-Deklarationen
DATA: user(64) TYPE c,
      pwd(64) TYPE c,
      host(64) TYPE c,
      blob_length TYPE i,
      dest LIKE rfcdes-rfcdest VALUE 'SAPFTP',
      desta LIKE rfcdes-rfcdest VALUE 'SAPFTPA',
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


*$smart (W) 2010-08-02 - #166 Datendefinition bezieht sich auf einen
*$smart (W) 2010-08-02 - #166 obsoleten Datentyp. (A)

DATA REPID TYPE REPID.                           "smart: 2010-08-02 #166

************************************************************************
*Interne Tabellen
************************************************************************

DATA: BEGIN OF nast_key,
          mblnr LIKE mkpf-mblnr,
          mjahr LIKE mkpf-mjahr,
          zeile LIKE mseg-zeile,
        END OF nast_key.
DATA: return_code LIKE sy-subrc.

DATA: itab_ze100 LIKE ze100 OCCURS 0.

*Hilfsfelder
DATA: BEGIN OF h,
        drnr LIKE ze100-drnr,
      END OF h.


* ---------------------------------------------------------------------*
*FORM main
* ---------------------------------------------------------------------*
FORM main USING return_code us_screen.

  get parameter id 'ZM_CODESOFT_TEST' field repid.


*Materialbeleg lesen
  CLEAR: return_code, mkpf, mseg, mchb.

  SELECT SINGLE * FROM mkpf WHERE mblnr = nast-objky+0(10)
                            AND   mjahr = nast-objky+10(4).

  SELECT SINGLE * FROM mseg WHERE mblnr = mkpf-mblnr
                            AND   zeile = nast-objky+14(4)
                            AND   mjahr = mkpf-mjahr.

*Uebergabesegment füllen
  REFRESH itab_ze100.

  MOVE mkpf-budat+6(2)  TO ze100-buch(2).
  MOVE '.'              TO ze100-buch+2(1).
  MOVE mkpf-budat+4(2)  TO ze100-buch+3(2).
  MOVE '.'              TO ze100-buch+5(1).
  MOVE mkpf-budat(4)    TO ze100-buch+6(4).
  MOVE ' / '            TO ze100-buch+10(3).
  MOVE mkpf-cputm(2)    TO ze100-buch+13(2).
  MOVE ':'              TO ze100-buch+15(1).
  MOVE mkpf-cputm+2(2)  TO ze100-buch+16(2).
  MOVE ':'              TO ze100-buch+18(1).
  MOVE mkpf-cputm+4(2)  TO ze100-buch+19(2).

  MOVE mseg-mblnr       TO ze100-mbnr(10).
  MOVE '.'              TO ze100-mbnr+10(1).
  MOVE mseg-zeile       TO ze100-mbnr+11(4).

  MOVE mseg-menge       TO ze100-meng.
  MOVE mseg-meins       TO ze100-mebe.
  MOVE mseg-tbnum       TO ze100-tbnr.
  MOVE mseg-charg       TO ze100-char.
  MOVE mseg-ebeln       TO ze100-ebeln.
  MOVE mseg-ebelp       TO ze100-ebelp.

*Materialnr. ohne führende Nullen
  clear: wmatnr.
  write mseg-matnr to wmatnr.
  while wmatnr+0(1) = '0'.
    shift wmatnr left IN CHARACTER MODE .        "smart: 2010-08-02 #115
  endwhile.
  MOVE wmatnr    TO ze100-matnr.

*Lieferantenr. ohne führende Nullen
  clear: wlifnr.
  write mseg-lifnr to wlifnr.
  while wlifnr+0(1) = '0'.
    shift wlifnr left IN CHARACTER MODE .        "smart: 2010-08-02 #115
  endwhile.
  MOVE wlifnr    TO ze100-lifnr.

*Chargendaten
  clear: mch1.
  SELECT SINGLE * FROM mch1 WHERE matnr = mseg-matnr
                            AND   charg = mseg-charg.

  concatenate mch1-vfdat+6(2) '.'
              mch1-vfdat+4(2) '.'
              mch1-vfdat+0(4)
             into ze100-vfdat IN CHARACTER MODE ."smart: 2010-08-02 #101
  MOVE mch1-licha   TO ze100-licha.

*Materialdaten
  CLEAR: mara, makt, mard.
  SELECT SINGLE * FROM makt WHERE matnr = mseg-matnr
                            AND   spras = sy-langu.
  MOVE makt-maktx      TO ze100-mktx.

  SELECT SINGLE * from mard where matnr = mseg-matnr
                              and werks = mseg-werks
                              and lgort = mseg-lgort.
  MOVE mard-lgpbe      TO ze100-lgpbe.

* Lieferantendaten
  clear: lfa1.
  SELECT single * from lfa1 where lifnr eq mseg-lifnr.
  MOVE lfa1-name1      TO  ze100-name1.



* Materialklassierung lesen
*Zur Zeit keine Bedarf . .  .
*  PERFORM lesen_materialklassierung.

*Etikettenzusatzdaten ermitteln
  select single * from ZMETIK2 where werks eq mseg-werks
                                 and lgort eq mseg-lgort
                                 and insmk eq mseg-insmk
                                 and lgpbe eq mard-lgpbe+0(2).
  if sy-subrc = 0. "Download

* Etikettenlayout & -anzahl übergeben
    write zmetik2-layout   TO ze100-etnr.
    write zmetik2-anzahl   TO ze100-anze.



*Druckerzusatzdaten für Codesoft ermitteln
    SELECT SINGLE * FROM zmetik WHERE rspolname = zmetik2-RSPOLNAME.

    ze100-drnr = zmetik-zcodesoftdrucker.
    host       = zmetik-zrfcdest.
    user       = zmetik-zrfcuser.
    pwd        = zmetik-zrfcauth.

    assign h_eol to <F_EOL> casting.
    move <F_EOL> to ze100-eol.

* Satzlänge für ftp ermitteln
    blob_length = strlen( ze100 ).

*Übergabe-Tabelle für FTP füllen
    APPEND ze100  TO itab_ze100.

    PERFORM anstoss_mit_ftp.

    CLEAR return_code.
  else.   "Download nur, wenn Detaildaten ZMETIK2 vorhanden !!
  endif.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  anstoss_mit_ftp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM anstoss_mit_ftp.

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


*FTP-Verbindung
  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
            user            = user
            password        = pwd
            host            = host
            rfc_destination = desta
       IMPORTING
            handle          = hdl.

*Filname zusammensetzen.
  tfdsn = dsn.
  REPLACE '<PATH>'  WITH zmetik-zpath INTO tfdsn.
  REPLACE '<OBJKY>' WITH nast-objky INTO tfdsn.
  REPLACE '<ERDAT>' WITH nast-erdat INTO tfdsn.
  REPLACE '<ERUHR>' WITH nast-eruhr INTO tfdsn.
  IF repid = ' '.
    REPLACE '<TEST>'  WITH txt        INTO tfdsn.
*TMP wird von UC4 nicht abgeholt
  ELSEIF repid = 'X'.
    REPLACE '<TEST>'  WITH tmp        INTO tfdsn.
  ENDIF.

  CONDENSE tfdsn NO-GAPS.

  CALL FUNCTION 'FTP_R3_TO_SERVER'
       EXPORTING
            handle      = hdl
            fname       = tfdsn
            character_mode = 'X'
       TABLES
            text        = itab_ze100.

*FTP-Verbindung schliessen
  CALL FUNCTION 'FTP_DISCONNECT'
       EXPORTING
            handle = hdl.

ENDFORM.                    " anstoss_mit_ftp
*&---------------------------------------------------------------------*
*&      Form  lesen_materialklassierung
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM lesen_materialklassierung.
*
*  g_objek = mseg-matnr.
*
*  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
*       EXPORTING
*            classtext          = 'X'
*            classtype          = '001'
*            object             = g_objek
*       TABLES
*            t_class            = g_t_sclass
*            t_objectdata       = g_t_clobjdat
*       EXCEPTIONS
*            no_classification  = 1
*            no_classtypes      = 2
*            invalid_class_type = 3
*            OTHERS             = 4.
*ENDFORM.                    " lesen_materialklassierung
