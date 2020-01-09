* ---------------------------------------------------------------------*
*  Report  ZM_ETIKDRUCK04                                              *
*                                                                      *
* ---------------------------------------------------------------------*
*             Dätwyler AG                                              *
* ---------------------------------------------------------------------*
*                                                                      *
*     Programm-Name....: ZM_ETIKDRUCK04                                *
*     Entwickler.......: Peter Huber                                   *
*     Erstell-Datum....: 28.11.2003                                    *
*     Version..........: 1.0                                           *
*     Zweck............: Aufbereitungsprogramm für Etikettendruck      *
*                        CODESOFT DINTER Rahmenetikette                *
*                        Nachrichtenart: ZE01                          *
*                        Nachrichtenschema: V10002                     *
*                                                                      *
*     Aenderungen:                                                     *
*     09.09.2010 M. Raffeiner
*                FTP-Übergabe neu in Charctermode wegen Unicode
*
* ---------------------------------------------------------------------*

************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #124
************************************************************************

REPORT zm_etikdruck04.

* ---------------------------------------------------------------------*
* Tabellen                                                             *
* ---------------------------------------------------------------------*
TABLES: vbak, vbap, kna1, mvke, nast, tsp03d, zmetik, vbkd, stxh,
        vbpa.

* ---------------------------------------------------------------------*
* Include                                                              *
* ---------------------------------------------------------------------*
INCLUDE: zincl_etikette_004.

* ---------------------------------------------------------------------*
* Data                                                                 *
* ---------------------------------------------------------------------*
DATA: dsn(200) type c,
      tfdsn(80),
      return_code LIKE sy-subrc.

DATA: BEGIN OF nast_key,
          objky LIKE nast-objky,
      END OF nast_key.

data: begin of ictab occurs 0.
        include structure CONF_OUT.
data: end of ictab.

DATA: itab_ze001 LIKE ze001 OCCURS 0.

data: wvbeln(10) type c,
      wposnr(6)  type c,
      wtdname(16) type c,
      textlines like tline occurs 0 with header line,
      wtexta(25),
      wtextb(25),
      wtextc(25),
      wtextd(25),
      wtextp1(25),
      wtextp2(25),
      sep(3) value ' / '.


* ---------------------------------------------------------------------*
*FTP-Deklarationen
DATA: user(64) TYPE c,
      pwd(64) TYPE c,
      dest LIKE rfcdes-rfcdest VALUE 'SAPFTPA',
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
constants: tmp(3) value 'TMP',
           txt(3) value 'TXT'.

* ---------------------------------------------------------------------*
* Testverarbeitung                                                     *
* ---------------------------------------------------------------------*
*kann für Test auf 'X' geändert werden im debugger.
*Datei wird dann auf Fileserver gesichert und NICHT von UC4 abgeholt.
data: test(1) value ' '.

* ---------------------------------------------------------------------*
* Verarbeiten                                                          *
* ---------------------------------------------------------------------*
FORM main USING return_code us_screen.

  dsn = '<PATH>T.ETIK.Z002.<OBJKY>.<ERDAT>.<ERUHR>.<TEST>'.

  CLEAR return_code.

  clear: wvbeln, wposnr.
  write nast-objky+0(10) to wvbeln.
  write nast-objky+10(6) to wposnr.

  SELECT SINGLE * FROM vbap WHERE vbeln = wvbeln
                              and posnr = wposnr.

  clear: itab_ze001.

* Kunde zum Kundenauftrag
  select single * from vbak where vbeln = vbap-vbeln.

* Rahmenetikette (prat7 = 'X').

  select single * from mvke where matnr = vbap-matnr
                            and   vkorg = vbak-vkorg
                            and   vtweg = vbak-vtweg.

* Materialstamm bestimmt, ob gedruckt wird
  IF mvke-prat7 eq 'X'.

* Kundenanschrift
    select single * from vbpa where vbeln = vbak-vbeln
                                and posnr = '000000'
                                and parvw = 'WE'.
    select single * from kna1 where kunnr  = vbpa-kunnr.
    ze001-name1 = kna1-name1.
    ze001-pstlz = kna1-pstlz.
    ze001-ort01 = kna1-ort01.
    ze001-land1 = kna1-land1.

* Kundenauftragsposition
    ze001-vbeln  = wvbeln.
    ze001-posnr  = wposnr.
    ze001-kwmeng = vbap-kwmeng.
    ze001-vrkme  = vbap-vrkme.

* Bestellnummer des Kunden
    select single * from vbkd where vbeln = wvbeln.
    ze001-bstkd  = vbkd-bstkd.

* Bestellposition des Kunden
    select single * from vbap where vbeln = wvbeln
                                and posnr = wposnr.

    ze001-posex  = vbap-posex.

* Vertriebstext einlesen; erste 4 Zeilen übernehmen
    concatenate vbap-vbeln vbap-posnr into wtdname
IN CHARACTER MODE .                              "smart: 2010-08-02 #101

    stxh-TDOBJECT = 'VBBP'.
    stxh-TDNAME   = wtdname.
    stxh-TDID     = '0001'.
    stxh-TDSPRAS  = nast-spras.

    refresh textlines.
    perform read_texte.
    loop at textlines.
      case: sy-tabix.
        when 1. write textlines+2(25) to ze001-text1.
        when 2. write textlines+2(25) to ze001-text2.
        when 3. write textlines+2(25) to ze001-text3.
        when 4. write textlines+2(25) to ze001-text4.
      endcase.
    endloop.

* Konfiguration
    if not vbap-cuobj is initial.
      clear: ze001-text2, ze001-text3, ze001-text4.
      perform lesen_konfiguration.
      loop at ictab.
        case: ictab-atnam.
          when 'ZDAG_PROFIL'.   wtextp1 = ictab-atwrt.
          when 'ZDAG_PROFIL_2'. wtextp2 = ictab-atwrt.
          when 'ZDAG_MASS_A'.   wtexta  = ictab-atwrt.
          when 'ZDAG_MASS_B'.   wtextb  = ictab-atwrt.
          when 'ZDAG_MASS_C'.   wtextc  = ictab-atwrt.
          when 'ZDAG_MASS_D'.   wtextd  = ictab-atwrt.
        endcase.
      endloop.
      concatenate wtextp1 wtextp2 into ze001-text2 separated by sep
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
      concatenate wtexta  wtextb into ze001-text3  separated by sep
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
      concatenate wtextc  wtextd into ze001-text4  separated by sep
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    endif.
*----------------------------------------------------------------*
*Transfer vorbereiten + ausführen
*----------------------------------------------------------------*
*Etikettenlayout
    write 'DIN01' to ze001-etnr .

*Anzahl Etiketten
    ze001-anz = nast-anzal.                           "CSJAN2004

*Druckerzusatzdaten ermitteln
    select single * from tsp03d where padest = nast-ldest.
    select single * from zmetik where rspolname = tsp03d-name.

    ze001-drnr = zmetik-zcodesoftdrucker.
    host       = zmetik-zrfcdest.
    pwd        = zmetik-zrfcauth.
    user       = zmetik-zrfcuser.

    assign h_eol to <F_EOL> casting.
    move <F_EOL> to ze001-eol.

*Übergabe-Tabelle für FTP füllen
    APPEND ze001  TO itab_ze001.

* Satzlänge für ftp ermitteln
    blob_length = strlen( ze001 ).

    PERFORM ftp.

    CLEAR return_code.
  endif.
ENDFORM.
*---------------------------------------------------------------------*
*      Form  ftp
*---------------------------------------------------------------------*
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
            rfc_destination = dest
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
  if test = ' '.
    REPLACE '<TEST>'  WITH txt        INTO dsn.
  elseif test = 'X'.
    REPLACE '<TEST>'  WITH tmp        INTO dsn.
  endif.
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

*&---------------------------------------------------------------------*
*&      Form  read_texte
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_texte.
  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            ID              = stxh-tdid
            LANGUAGE        = stxh-tdspras
            NAME            = stxh-tdname
            OBJECT          = stxh-tdobject
       TABLES
            LINES           = textlines
       EXCEPTIONS
            ID              = 1
            LANGUAGE        = 2
            NAME            = 3
            NOT_FOUND       = 4
            OBJECT          = 5
            REFERENCE_CHECK = 6
            OTHERS          = 8.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  lesen_konfiguration
*&---------------------------------------------------------------------*
FORM lesen_konfiguration.
  refresh ictab.
  CALL FUNCTION 'VC_I_GET_CONFIGURATION'
       EXPORTING
            INSTANCE            = vbap-cuobj
       TABLES
            CONFIGURATION       = ictab
       EXCEPTIONS
            INSTANCE_NOT_FOUND  = 1
            INTERNAL_ERROR      = 2
            NO_CLASS_ALLOCATION = 3
            INSTANCE_NOT_VALID  = 4
            OTHERS              = 5.
ENDFORM.                    " lesen_konfiguration
