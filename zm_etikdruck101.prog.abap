*---------------------------------------------------------------------*
* Report  ZM_ETIKDRUCK101                                             *
*---------------------------------------------------------------------*
*   Dätwyler AG                                                       *
*---------------------------------------------------------------------*
*                                                                     *
*    Programm-Name....: ZM_ETIKDRUCK101                               *
*    Entwickler.......: Peter Huber                                   *
*    Erstell-Datum....: 3.2006                                        *
*    Version..........: 1.0                                           *
*    Zweck............: Aufbereitungsprogramm für Etikettendruck      *
*                       CODESOFT  Nachdruck                           *
*    Input............: DB-Tabellen:                                  *
*    Output...........: File für die Uebergabe an CODESOFT            *
*                                                                     *
*    Bemerkung........:                                               *
*    Aenderungen:                                                     *
*                                                                     *
*      12.05.2006: Markus Raffeiner                                   *
*                  Usererparameter 'ZM_CODESOFT_TEST' für             *
*                  Etikettendruck verwenden:                          *
*                  ZM_CODESOFT_TEST = 'X' ermöglicht download auf     *
*                  CODESOFT Server, ohne das UC4 die Datei abholt     *
*---------------------------------------------------------------------*

************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #102 #115 #124 #142 #166
************************************************************************

REPORT ZM_ETIKDRUCK101.
*---------------------------------------------------------------------*
*Tabellen                                                             *
*---------------------------------------------------------------------*
TABLES: makt, tsp03d, zmetik, lqua, mard.

************************************************************************
*Konstanten
************************************************************************
*Pfad, Name und Extension der Datei
CONSTANTS: dsn(200) VALUE
   '<PATH>T.ETIK.<ETNR>.<LGNUM>.<LQNUM>.<ERDAT>.<ERUHR>.<TEST>'.

*---------------------------------------------------------------------*
*Selektion                                                            *
*---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
select-options: s_lgnum for lqua-lgnum obligatory,
                s_lgtyp for lqua-lgtyp default '001' obligatory,
                s_lgpla for lqua-lgpla.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
select-options: s_matnr for lqua-matnr,
                s_charg for lqua-charg.
selection-screen skip 1.
parameters:     p_gesme as checkbox default 'X'.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
parameters: psunsor radiobutton group 4,
            psnlpla radiobutton group 4,
            pscharg radiobutton group 4,
            psnlenr radiobutton group 4.
SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
parameters:    p_druck as checkbox.
selection-screen skip 1.
parameters:    s_dru like zmetik-rspolname obligatory.

selection-screen skip 1.
parameters: pwait(2) default '1' obligatory.
SELECTION-SCREEN END OF BLOCK B3.
data: i type i.
field-symbols <VARFLD>.
data: z_menge(13),                               "smart: 2010-08-02 #102
      wlenum(20).
*---------------------------------------------------------------------*
*Include                                                              *
*---------------------------------------------------------------------*
INCLUDE: zincl_etikette_101.
*---------------------------------------------------------------------*
* Konstanten                                                          *
*---------------------------------------------------------------------*

  constants: tmp(3) value 'TMP',
             txt(3) value 'TXT'.

*---------------------------------------------------------------------*
* Data                                                                *
*---------------------------------------------------------------------*
DATA: tfdsn(200),
      return_code LIKE sy-subrc,
      itab_ze101 LIKE ze101 OCCURS 0,
      ize101 LIKE ze101 occurs 0 with header line,
      ftp_out LIKE ze101 occurs 0 with header line,
      counter type p.

*Hilfsfelder
DATA: wmatnr(10).

*FTP-Deklarationen
DATA: user(64) TYPE c,
      pwd(64) TYPE c,
      host(64) TYPE c,
      dest LIKE rfcdes-rfcdest VALUE 'SAPFTP',
      blob_length TYPE i,
      dstlen TYPE i,
      hdl TYPE i,
      slen type i,
      key TYPE i VALUE 26101957.


*$smart (W) 2010-08-02 - #166 Datendefinition bezieht sich auf einen
*$smart (W) 2010-08-02 - #166 obsoleten Datentyp. (A)

DATA: repid TYPE REPID,                          "smart: 2010-08-02 #166
      plgnum like lqua-lgnum.



* ---------------------------------------------------------------------*
* Inizialisierung                                                      *
* ---------------------------------------------------------------------*
INITIALIZATION.
  get parameter ID 'LGN' FIELD plgnum.
MOVE: 'I'      TO s_lgnum-SIGN,
       'EQ'    TO s_lgnum-OPTION,
       plgnum  TO s_lgnum-LOW.
 APPEND s_lgnum.

* Etikettendrucker aus Userparameter vorschlagen (Transaktion SU01)
  get parameter ID 'ND9' FIELD s_dru.
  if s_dru is initial.
  s_dru = 'ZZETIK*'.
  endif.



*$smart (F) 2010-08-02 - #124 DESCRIBE benötigt einen Modus wenn die
*$smart (F) 2010-08-02 - #124 Länge oder der Abstand angefragt wird
*$smart (F) 2010-08-02 - #124 Replaced with recommended default value.
*$smart (F) 2010-08-02 - #124 (M)

  DESCRIBE FIELD pwd LENGTH dstlen IN CHARACTER  "smart: 2010-08-02 #124
    MODE .                                       "smart: 2010-08-02 #124

* Benutzerparameter für Etikettendruck
get parameter id 'ZM_CODESOFT_TEST' field repid.
* ---------------------------------------------------------------------*
* Testverarbeitung                                                     *
* ---------------------------------------------------------------------*
*kann für Test auf 'X' geändert werden im debugger.
*Datei wird dann auf Fileserver gesichert und NICHT von UC4 abgeholt.
  data: test(1) value ' '.

*---------------------------------------------------------------------*
* Verarbeitung                                                        *
*---------------------------------------------------------------------*
start-of-selection.
  if s_lgnum is initial  and
     s_lgtyp is initial  and
     s_lgpla is initial  and
     s_charg is initial.

    MESSAGE ID 'ZM01' TYPE 'I' NUMBER '000'.
    exit.
  endif.

  select * from lqua where lgnum in s_lgnum
                       and lgtyp in s_lgtyp
                       and lgpla in s_lgpla
                       and charg in s_charg
                       and matnr in s_matnr.

*Clear ohne Pipes in der Struktur
    clear:    ze101-etnr,  ze101-drnr,  ze101-anze,  ze101-wdatu,
              ze101-werks, ze101-lgnum, ze101-lgtyp, ze101-lgort,
              ze101-nlpla, ze101-mblnr, ze101-mbpos, ze101-matnr,
              ze101-maktx, ze101-charg, ze101-menge, ze101-meins,
              ze101-ebeln, ze101-ebelp, ze101-aufnr, ze101-tanum,
              ze101-eol .

*Bestand vorhanden ?
    if p_gesme = 'X'.
      check: lqua-gesme gt 0.
    endif.

*Uebergabesegment füllen

*Etikettenlayout vorbelegen
    case: lqua-lgnum.
    when '800'. ze101-etnr  = 'DRCWMWE1'. "DRC
    when others. ze101-etnr = 'DGUSPWE1'. "DGU
    endcase.

    ze101-anze  = 1.

*Datum
    concatenate lqua-wdatu+6(2) '.' lqua-wdatu+4(2)
                '.' lqua-wdatu+0(4) into ze101-wdatu
IN CHARACTER MODE .                              "smart: 2010-08-02 #101

    if lqua-wenum+0(2) = '50'.
      ze101-werks = lqua-werks.
    endif.

    ze101-lgnum = lqua-lgnum.
    ze101-lgtyp = lqua-lgtyp.
    ze101-lgort = lqua-lgort.
    ze101-nlpla = lqua-lgpla.

    if lqua-wenum+0(2) = '50'.
      ze101-mblnr = lqua-wenum.
      ze101-mbpos = lqua-wepos.
    endif.

*Materialnr. ohne führende Nullen
    clear: wmatnr.
    write lqua-matnr to wmatnr.
    while wmatnr+0(1) = '0'.
      shift wmatnr left IN CHARACTER MODE .      "smart: 2010-08-02 #115
    endwhile.
    ze101-matnr = wmatnr.

    clear: makt.
    select single * from makt where matnr = lqua-matnr
                                 and spras = 'DE'.

    ze101-maktx = makt-maktx.
    ze101-charg = lqua-charg.
    ze101-menge = lqua-gesme.
    ze101-meins = lqua-meins.
    ze101-ebeln = '-'.
    ze101-ebelp = '-'.
    ze101-aufnr = '-'.
    ze101-tanum = '-'.

* Lagereinheitennummer ohne führende Nullen
 clear: wlenum.
 wlenum = lqua-lenum.
 while wlenum+0(1) = '0'.
 wlenum+0(1) = ' '.
 shift wlenum left IN CHARACTER MODE .           "smart: 2010-08-02 #115
 endwhile.

    ze101-nlenr = wlenum.
    MOVE CL_ABAP_CHAR_UTILITIES=>CR_LF+0(2) TO ze101-eol.         "smart(M): 2010-08-05

*Druckerzusatzdaten ermitteln
    SELECT SINGLE * FROM tsp03d WHERE name = s_dru.
    SELECT SINGLE * FROM zmetik WHERE rspolname = tsp03d-name.

    ze101-drnr = zmetik-zcodesoftdrucker.
    host       = zmetik-zrfcdest.
    user       = zmetik-zrfcuser.
    pwd        = zmetik-zRFCAUTH.

* Satzlänge für ftp ermitteln
    blob_length = strlen( ze101 ).

*Ausschlüsse

*Übergabe-Tabelle für FTP füllen
    append ze101 to ize101.

*Counter
    counter = counter + 1.
  endselect.


*Lokaler Download
if repid = 'X'.

*$smart (F) 2010-08-02 - #142 Der nicht Unicode konforme GUI
*$smart (F) 2010-08-02 - #142 Upload/Download Funktionsbaustein wurde
*$smart (F) 2010-08-02 - #142 mit vorgeschlagenem kompatiblen
*$smart (F) 2010-08-02 - #142 Funktionsbaustein ersetzt (A)

DATA V_filename1 TYPE string.                    "smart: 2010-08-02 #142
DATA V_path1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_default_filename1 TYPE string.            "smart: 2010-08-02 #142
DATA V_fullpath1 TYPE string.                    "smart: 2010-08-02 #142
DATA V_user_action1 TYPE I.                      "smart: 2010-08-02 #142
DATA V_file_table1 TYPE filetable.               "smart: 2010-08-02 #142
DATA V_default_extension1 TYPE string.           "smart: 2010-08-02 #142
DATA V_rc1 TYPE i.                               "smart: 2010-08-02 #142
DATA V_window_title1 TYPE string.                "smart: 2010-08-02 #142
DATA V_file_filter1 TYPE string.                 "smart: 2010-08-02 #142
V_DEFAULT_FILENAME1 = 'c:\temp\Etikette'.        "smart: 2010-08-02 #142
SHIFT V_DEFAULT_FILENAME1 RIGHT DELETING TRAILING"smart: 2010-08-02 #142
   '\'.                                          "smart: 2010-08-02 #142
SHIFT V_DEFAULT_FILENAME1 RIGHT DELETING TRAILING"smart: 2010-08-02 #142
   '/'.                                          "smart: 2010-08-02 #142
SHIFT V_DEFAULT_FILENAME1 LEFT DELETING LEADING  "smart: 2010-08-02 #142
  SPACE .                                        "smart: 2010-08-02 #142
V_DEFAULT_EXTENSION1 = 'ASC'.                    "smart: 2010-08-02 #142
CALL METHOD                                      "smart: 2010-08-02 #142
  cl_gui_frontend_services=>file_save_dialog     "smart: 2010-08-02 #142
  EXPORTING DEFAULT_EXTENSION  =                 "smart: 2010-08-02 #142
    V_DEFAULT_EXTENSION1                         "smart: 2010-08-02 #142
  DEFAULT_FILE_NAME  = V_DEFAULT_FILENAME1       "smart: 2010-08-02 #142
  INITIAL_DIRECTORY  = V_DEFAULT_FILENAME1       "smart: 2010-08-02 #142
  CHANGING FILENAME = V_FILENAME1                "smart: 2010-08-02 #142
    PATH = V_PATH1                               "smart: 2010-08-02 #142
    FULLPATH = V_FULLPATH1                       "smart: 2010-08-02 #142
    USER_ACTION = V_USER_ACTION1.                "smart: 2010-08-02 #142
CHECK V_USER_ACTION1 EQ 0.                       "smart: 2010-08-02 #142
CALL METHOD                                      "smart: 2010-08-02 #142
  cl_gui_frontend_services=>gui_download         "smart: 2010-08-02 #142
 EXPORTING
   FILENAME = V_FULLPATH1                        "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
    DATA_TAB                      = ize101[]     "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142
endif.

*----------------------------------------------------------------*
* Passwort
*----------------------------------------------------------------*
***  call 'AB_RFC_X_SCRAMBLE_STRING'
***    id 'SOURCE'      field pwd    id 'KEY'         field key
***    id 'SCR'         field 'X'    id 'DESTINATION' field pwd
***    id 'DSTLEN'      field dstlen.

slen = strlen( pwd ).

call function 'HTTP_SCRAMBLE'
  exporting
    source      = pwd
    sourcelen   = slen
    key         = key
  importing
    destination = pwd.


*Sort nach Chargennummer
if pscharg = 'X'.
sort ize101 by charg.
*Sort nach Lagereinheit
elseif psnlenr = 'X'.
sort ize101 by nlenr.
*Sort nach Lagerplatz
elseif psnlpla = 'X'.
sort ize101 by nlpla.
else.
"nach Selekt Quanten aufsteigend
endif.


  loop at ize101.

*Druckausgabe
    if p_druck = 'X'.
      write:/ ize101-lgnum, ize101-lgort, ize101-lgtyp, ize101-nlpla,
              ize101-matnr, ize101-charg, ize101-menge, ize101-meins.
    endif.

*Wait wegen Problemen mit Druckerpufferung
*bei normaler Druckgeschwindigkeit ca. 4 Sekunden

*Drucker laufen heiss !
    if counter ge 50.
      pwait = 3.
    endif.

    wait up to pwait seconds.

*Verarbeiten
    refresh: ftp_out.
    move-corresponding ize101 to ftp_out.
    append ftp_out.
    PERFORM ftp.
  endloop.
  skip.

*Verarbeitungsprotokoll
  write:/ 'Anzahl gedruckter Etiketten: ', counter.


*----------------------------------------------------------------*
* FTP-Verbindung
*----------------------------------------------------------------*
FORM ftp.

* FTP Connect
  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
            user            = user
            password        = pwd
            host            = host
            rfc_destination = dest
       IMPORTING
            handle          = hdl.


*Filename aufbereiten

  tfdsn = dsn.
  REPLACE '<PATH>'  WITH zmetik-zpath    INTO tfdsn.
  REPLACE '<LGNUM>' WITH lqua-lgnum      INTO tfdsn.
  REPLACE '<LQNUM>' WITH lqua-lqnum      INTO tfdsn.
  REPLACE '<ETNR>'  WITH ze101-etnr      INTO tfdsn.
  REPLACE '<ERDAT>' WITH sy-datum        INTO tfdsn.
  REPLACE '<ERUHR>' WITH sy-uzeit        INTO tfdsn.

  IF repid = ' '.
    REPLACE '<TEST>' WITH txt        INTO tfdsn.
*
**TMP wird von UC4 nicht abgeholt
  ELSEIF repid = 'X'.
    REPLACE '<TEST>' WITH tmp        INTO tfdsn.
  ENDIF.

  CONDENSE tfdsn NO-GAPS.

* Daten auf den Server stellen
  CALL FUNCTION 'FTP_R3_TO_SERVER'
       EXPORTING
            handle      = hdl
            fname       = tfdsn
            blob_length = blob_length
       TABLES
            blob        = ftp_out.
*----------------------------------------------------------------*
* FTP-Verbindung schliessen
*----------------------------------------------------------------*
  CALL FUNCTION 'FTP_DISCONNECT'
       EXPORTING
            handle = hdl.
endform.
