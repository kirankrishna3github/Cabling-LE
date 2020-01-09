
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #124
************************************************************************

REPORT ZM_ETIKDRUCK06 message-id ZDLE.

*---------------------------------------------------------------------*
* Report   ZM_ETIKDRUCK06                                             *
*                                                                     *
*---------------------------------------------------------------------*
*            Dätwyler AG                                              *
*---------------------------------------------------------------------*
*                                                                     *
*    Programm-Name....: ZM_ETIKDRUCK06                                *
*    Entwickler.......: Peter Huber                                   *
*    Erstell-Datum....: 30.03.2005                                    *
*    Version..........: 1.0                                           *
*    Zweck............: Etikettierung von Platzbeständen              *
*                       Filetransfer für CODESOFT                     *
*    Input............: DB-Tabellen:                                  *
*    Output...........: File für die Uebergabe an CODESOFT            *
*                                                                     *
*    Bemerkung........: Transaktion ZME3                              *
*    Aenderungen:                                                     *
*     09.09.2010 M. Raffeiner
*                FTP-Übergabe neu in Charctermode wegen Unicode
*                                                                     *
*                                                                     *


*---------------------------------------------------------------------*
*Tabellen                                                             *
*---------------------------------------------------------------------*
TABLES: mara, makt,tsp03d, lqua, marm,
        zmetik, marc, mkpf, mseg, mch1, mchb, mard, lfa1,
        nast, klah.
*---------------------------------------------------------------------*
*interne Tabellen                                                     *
*---------------------------------------------------------------------*
data: begin of ilqua occurs 0.
        include structure lqua.
data: anzahl(10) type n,
      umrez like marm-umrez,
      umren like marm-umren,
      umanz(10) type n.
data: end of ilqua.

data: begin of iprint occurs 0.
        include structure ilqua.
data: end of iprint.

data: begin of iftp occurs 0.
        include structure ilqua.
data: end of iftp.

DATA: BEGIN OF nast_key,
          mblnr LIKE mkpf-mblnr,
          mjahr LIKE mkpf-mjahr,
          zeile LIKE mseg-zeile,
        END OF nast_key.

*Uebergabestrukur
INCLUDE: zincl_etikette_100.

DATA: itab_ze100 LIKE ze100 OCCURS 0.

*---------------------------------------------------------------------*
*Data                                                                 *
*---------------------------------------------------------------------*
data: ch1(1),
      ch1X(1) value 'X',
      tfdsn(200),
      return_code LIKE sy-subrc,
      drucker_name(30)  TYPE c,
      dsn(200),
      user(64) TYPE c,
      pwd(64) TYPE c,
      host(64) TYPE c,
      blob_length TYPE i,
      dest LIKE rfcdes-rfcdest VALUE 'SAPFTP',
      desta LIKE rfcdes-rfcdest VALUE 'SAPFTPA',
      dstlen TYPE i,
      hdl TYPE i,
      slen type i,
      key TYPE i VALUE 26101957.
*---------------------------------------------------------------------*
*Konstanten                                                           *
*---------------------------------------------------------------------*
CONSTANTS: tmp(3) VALUE 'TMP',
           txt(3) VALUE 'TXT'.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* Testverarbeitung
*kann für Test auf 'X' geändert werden im debugger.
*Datei wird dann auf Fileserver gesichert und NICHT von UC4 abgeholt.
DATA: test(1) VALUE 'X'.
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*---------------------------------------------------------------------*
*Workfelder                                                           *
*---------------------------------------------------------------------*

DATA: BEGIN OF h,
        drnr LIKE ze100-drnr,
      END OF h.

data: h_eol(4) type x value '0A'.
field-symbols: <F_EOL> type c.

*---------------------------------------------------------------------*
*Benutzeroberfläche                                                   *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
select-options: smatnr for lqua-matnr default '700467',
                scharg for lqua-charg.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
parameters:     pwerks like lqua-werks obligatory default '7000',
                plgort like lqua-lgort obligatory default '7000',
                plgnum like lqua-lgnum obligatory default '700',
                plgtyp like lqua-lgtyp obligatory default '001'.

select-options: slgpla for lqua-lgpla default 'PH1'.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
parameters:    panz(3) type n,
               pdru like zmetik-rspolname obligatory
                                           default 'ZZETIK100'.
SELECTION-SCREEN END OF BLOCK B3.

*---------------------------------------------------------------------*
*Verarbeitung                                                         *
*---------------------------------------------------------------------*

*Ausgabedrucker muss definiert sein !
if pdru ca '*'.
 message E001.
exit.
endif.


start-of-selection.
  select * into table ilqua from lqua where matnr in smatnr
                                        and charg in scharg
                                        and werks eq pwerks
                                        and lgort eq plgort
                                        and lgnum eq plgnum
                                        and lgtyp eq plgtyp
                                        and lgpla in slgpla.

  loop at ilqua.
    select single * from marm where matnr eq ilqua-matnr
                                and meinh eq 'ZET'.
    if sy-subrc = 0.
      ilqua-umren = marm-umren.
      ilqua-umrez = marm-umrez.
      ilqua-umanz = ilqua-gesme / ( marm-umrez / marm-umren ).
      if panz is initial.
        if ilqua-umanz ge 1.
          ilqua-anzahl = ilqua-umanz.
        else.
          ilqua-anzahl = '1'.
        endif.
      else.
        ilqua-anzahl = '1'.
      endif.
      modify ilqua.
    endif.
  endloop.

  loop at ilqua.
    write:/ ch1 as checkbox,
            ilqua-anzahl input on no-zero,
            ilqua-umanz no-zero,
            ilqua-matnr,
            ilqua-charg,
            ilqua-werks,
            ilqua-lgnum,
            ilqua-lgort,
            ilqua-lgtyp,
            ilqua-lgpla,
            ilqua-gesme,
            ilqua-wenum,
            ilqua-wepos.

  endloop.

  set pf-status 'LQUA'.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'MOVE'.  perform move.
    WHEN 'FTP'.   perform ftp.
    WHEN 'BEND'.  leave to transaction 'ZME3'.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  move
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move.

  refresh iprint.
  do.
    read line sy-index.
    if sy-subrc ne 0.
      exit.
    endif.

    read line sy-index field value ilqua-matnr  into iprint-matnr.
    read line sy-index field value ilqua-charg  into iprint-charg.
    read line sy-index field value ilqua-werks  into iprint-werks.
    read line sy-index field value ilqua-lgort  into iprint-lgort.
    read line sy-index field value ilqua-lgnum  into iprint-lgnum.
    read line sy-index field value ilqua-lgtyp  into iprint-lgtyp.
    read line sy-index field value ilqua-lgpla  into iprint-lgpla.
    read line sy-index field value ilqua-anzahl into iprint-anzahl.
    read line sy-index field value ilqua-wenum  into iprint-wenum.
    read line sy-index field value ilqua-wepos  into iprint-wepos.

    if sy-lisel+0(1) = 'X'.
      append iprint.
    endif.
  enddo.

  perform print.

ENDFORM.                    " print
*&---------------------------------------------------------------------*
*&      Form  print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print.

  set pf-status 'PRINT'.
  loop at iprint.
    write:/ ch1x as checkbox ,
            iprint-anzahl input on no-zero,
            iprint-matnr,
            iprint-charg,
            iprint-werks,
            iprint-lgort,
            iprint-lgnum,
            iprint-lgtyp,
            iprint-lgpla,
            iprint-wenum,
            iprint-wepos.
  endloop.


ENDFORM.                    " print
*&---------------------------------------------------------------------*
*&      Form  ftp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ftp.

  refresh iftp.
  do.
    read line sy-index.
    if sy-subrc ne 0.
      exit.
    endif.

    read line sy-index field value iprint-matnr  into iftp-matnr.
    read line sy-index field value iprint-charg  into iftp-charg.
    read line sy-index field value iprint-lgpla  into iftp-lgpla.
    read line sy-index field value iprint-anzahl into iftp-anzahl.
    read line sy-index field value iprint-wenum  into iftp-wenum.
    read line sy-index field value iprint-wepos  into iftp-wepos.

    if sy-lisel+0(1) = 'X'.
      append iftp.
    endif.
  enddo.


  loop at iftp.
    perform iftp2.
  endloop.

  set pf-status 'FINI'..
  write:/ 'Etiketten werden gedruckt . . . . . . '.

ENDFORM.                    " ftp
*&---------------------------------------------------------------------*
*&      Form  iftp2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iftp2.

*Materialbeleg lesen
  CLEAR: return_code, mkpf, mseg, mchb.
  SELECT SINGLE * FROM mseg WHERE mblnr eq iftp-wenum
                              AND zeile eq iftp-wepos.
  select single * from mkpf where mblnr eq mseg-mblnr.

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
  MOVE mseg-matnr       TO ze100-matnr.
  MOVE mseg-lifnr       TO ze100-lifnr.

*Chargendaten
  clear: mch1.
  SELECT SINGLE * FROM mch1 WHERE matnr = mseg-matnr
                            AND   charg = mseg-charg.
  MOVE mch1-vfdat   TO ze100-vfdat.
  MOVE mch1-licha   TO ze100-licha.

*Materialdaten
  CLEAR: mara, makt, mard.
  SELECT SINGLE * FROM makt WHERE matnr = mseg-matnr
                            AND   spras = sy-langu.
  MOVE makt-maktx      TO ze100-mktx.

  SELECT SINGLE * from mard where matnr = mseg-matnr
                              and werks = mseg-werks
                              and lgort = mseg-lgort.
  MOVE iftp-lgpla      TO ze100-lgpbe.

* Lieferantendaten
  clear: lfa1.
  SELECT single * from lfa1 where lifnr eq mseg-lifnr.
  MOVE lfa1-name1      TO  ze100-name1.

* Anzahl Etiketten übergeben
  move iftp-anzahl to ze100-anze.

* Variable Etikettenlayout definieren
  MOVE 'DGU01'    TO ze100-etnr.

*Druckerzusatzdaten ermitteln
  SELECT SINGLE * FROM zmetik WHERE rspolname = pdru.

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


*$smart (F) 2010-08-02 - #124 DESCRIBE benötigt einen Modus wenn die
*$smart (F) 2010-08-02 - #124 Länge oder der Abstand angefragt wird
*$smart (F) 2010-08-02 - #124 Replaced with recommended default value.
*$smart (F) 2010-08-02 - #124 (M)

      DESCRIBE FIELD pwd LENGTH dstlen IN        "smart: 2010-08-02 #124
        CHARACTER MODE .                         "smart: 2010-08-02 #124

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

*Pfad, Name und Extension der Datei
  write '<PATH>T.ETIK.ZE01.<OBJKY>.<ERDAT>.<ERUHR>.<TEST>' to dsn.

*Filname aufbereiten
  tfdsn = dsn.
  REPLACE '<PATH>'  WITH zmetik-zpath INTO tfdsn.
  REPLACE '<OBJKY>' WITH nast-objky INTO tfdsn.
  REPLACE '<ERDAT>' WITH nast-erdat INTO tfdsn.
  REPLACE '<ERUHR>' WITH nast-eruhr INTO tfdsn.
  IF test = ' '.
    REPLACE '<TEST>'  WITH txt        INTO tfdsn.
  ELSEIF test = 'X'.
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
