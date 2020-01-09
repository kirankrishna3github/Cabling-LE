
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #111 #115 #142
************************************************************************

REPORT Z200_MM_MATSTA_01.

*Beschreibung:
*
*Teil 1: upload
*Report lädt die im Format .txt gespeicherte Datei vom PC hoch.
*in die input.
*Es wird eine output Datei erzeugt, welche den Strukturen entspricht.
*Die physische Datei wird auf dem Appl.server gesichert.
*
*Teil 2: check
*Prüfung mittels AL11.
*resp. inhaltliche Beurteilung mittels
*Transaktion: SXDA
*Objekttyp: BUS1001006
*Programmtyp: DINP
*Programm: RMDATIND
*Dateityp: physisch
*Dateiname: dito dsno
*
*Teil 3: Verarbeitung
*zum Test: Report RMDATIND
*für Produktivdaten: Transaktion BMV0 resp. Report


*Tabellen
tables: mara, marc, marm.
tables: mlgn.

*Strukturen der Datenmigrations-Workbench
tables: bgr00, bmm00, bmmh1, bmmh6.

*Felder
data: counter type p.
data: xfilename like rlgrap-filename.
data: xmess(100).
data: wmatnr(18),
      wtcode like sy-tcode.

*Benutzeroberfläche
parameters: pmm01 radiobutton group r1,
            pmm02 radiobutton group r1.
selection-screen skip 1.
parameters: dsni like rlgrap-filename lower case
                  default 'h:/materialstamm.txt'
                  obligatory.
parameters: dsno(60) lower case
default '/usr/sap/trans_R3/MATERIAL.DAT' obligatory.
selection-screen skip 1.
parameters: testlauf as checkbox default 'X'.

*Struktur EXCEL Input Datei
data: begin of input occurs 0,
matnr(8),    "Material
xchpf(10),    "Chargenpflicht
mhdrz(3),     "Restlaufzeit
insmk(10),    "Q Kennzeichen
end of input.

data: begin of imatnr occurs 0,
matnr(18),    "Material
end of imatnr.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.




start-of-selection.

************************************************************************
*Lagerverwaltungsicht anlegen im Nummerbereich 700000 - 799999 bei
*Matnr.welche im Werk 7000 angelegt sind

  if pMM01 = 'X'.


*$smart (F) 2010-08-02 - #111 OPEN DATASET benötigt mode und encoding
*$smart (F) 2010-08-02 - #111 Spezifikation Adding encoding to OPEN
*$smart (F) 2010-08-02 - #111 DATASET statement (A)

    open dataset dsno for output in text mode message xmess
ENCODING UTF-8 .                                 "smart: 2010-08-02 #111
    write: / dsno, xmess.
*Mappenvorsatz
    clear bgr00.
    bgr00-stype = '0'.
    bgr00-group = 'MATUEB'.
    bgr00-mandt = sy-mandt.
    bgr00-usnam = sy-uname.
    bgr00-nodata = ' '.
    if testlauf is initial.
      transfer bgr00 to dsno.
    endif.

    select * from mara where matnr
                       ge  '000000000000700000'.
      check: mara-matnr le '000000000000799999'.
      select single * from mlgn where matnr eq mara-matnr
                                  and lgnum eq '700'.
      if sy-subrc ne 0.
      select single * from marc where matnr eq mara-matnr
                                  and werks eq '7000'.
      check: sy-subrc = 0.
        if testlauf = ' '.
*Matnr auf 8 stellen reduzieren
      clear: wmatnr.
      wmatnr = mara-matnr.
        while wmatnr+0(1) = '0'.
          shift wmatnr left IN CHARACTER MODE .  "smart: 2010-08-02 #115
        endwhile.

          perform verarbeiten_mm01.
        endif.
        write:/ mara-matnr, ' wird  angelegt'.
        counter = counter + 1.
      else.
        write:/ mara-matnr, ' bereits angelegt'.
      endif.
    endselect.

    skip 1.
    uline.
    write: / 'Anzahl Datensätze: ', counter.
  endif.
*Ende MM01

*****************************************************************
*Materialstamm ändern
  if pmm02 = 'X'.

*Upload Input Datei

*$smart (F) 2010-08-02 - #142 Der nicht Unicode konforme GUI
*$smart (F) 2010-08-02 - #142 Upload/Download Funktionsbaustein wurde
*$smart (F) 2010-08-02 - #142 mit vorgeschlagenem kompatiblen
*$smart (F) 2010-08-02 - #142 Funktionsbaustein ersetzt (A)

DATA V_file1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_path1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_user_action1 TYPE I.                      "smart: 2010-08-02 #142
DATA V_file_table1 TYPE filetable.               "smart: 2010-08-02 #142
DATA V_default_extension1 TYPE string.           "smart: 2010-08-02 #142
DATA V_rc1 TYPE i.                               "smart: 2010-08-02 #142
DATA V_window_title1 TYPE string.                "smart: 2010-08-02 #142
DATA V_file_filter1 TYPE string.                 "smart: 2010-08-02 #142
V_FILE1 = dsni.                                  "smart: 2010-08-02 #142
SHIFT V_FILE1 RIGHT DELETING TRAILING '\'.       "smart: 2010-08-02 #142
SHIFT V_FILE1 RIGHT DELETING TRAILING '/'.       "smart: 2010-08-02 #142
SHIFT V_FILE1 LEFT DELETING LEADING SPACE .      "smart: 2010-08-02 #142
V_DEFAULT_EXTENSION1 = 'DAT'.                    "smart: 2010-08-02 #142
CALL METHOD                                      "smart: 2010-08-02 #142
  cl_gui_frontend_services=>file_open_dialog     "smart: 2010-08-02 #142
  EXPORTING DEFAULT_EXTENSION  =                 "smart: 2010-08-02 #142
    V_DEFAULT_EXTENSION1                         "smart: 2010-08-02 #142
  DEFAULT_FILENAME  = V_FILE1                    "smart: 2010-08-02 #142
  INITIAL_DIRECTORY  = V_FILE1                   "smart: 2010-08-02 #142
  CHANGING FILE_TABLE = V_FILE_TABLE1            "smart: 2010-08-02 #142
    RC = V_RC1                                   "smart: 2010-08-02 #142
    USER_ACTION = V_USER_ACTION1.                "smart: 2010-08-02 #142
CHECK V_USER_ACTION1 EQ 0.                       "smart: 2010-08-02 #142
CHECK V_RC1 GT 0.                                "smart: 2010-08-02 #142
READ TABLE V_file_table1 INDEX 1 INTO V_file1 .  "smart: 2010-08-02 #142
XFILENAME = V_FILE1.                             "smart: 2010-08-02 #142
CALL METHOD cl_gui_frontend_services=>gui_upload "smart: 2010-08-02 #142
         EXPORTING
              FILENAME = V_FILE1                 "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
              DATA_TAB     = input[]             "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142

    IF XFILENAME IS INITIAL.
      PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
    ENDIF.
    IF NOT XFILENAME CS '.txt'.
      PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
    ENDIF.



*$smart (F) 2010-08-02 - #111 OPEN DATASET benötigt mode und encoding
*$smart (F) 2010-08-02 - #111 Spezifikation Adding encoding to OPEN
*$smart (F) 2010-08-02 - #111 DATASET statement (A)

    open dataset dsno for output in text mode message xmess
ENCODING UTF-8 .                                 "smart: 2010-08-02 #111
    write: / dsno, xmess.
*Mappenvorsatz
    clear bgr00.
    bgr00-stype = '0'.
    bgr00-group = 'MATUEB'.
    bgr00-mandt = sy-mandt.
    bgr00-usnam = sy-uname.
    bgr00-nodata = ' '.
    if testlauf is initial.
      transfer bgr00 to dsno.
    endif.


    loop at input.
      clear: wmatnr.
      wmatnr = input-matnr.
      if wmatnr co '0123456789 '.
        while wmatnr+17(1) = ' '.
          shift wmatnr right IN CHARACTER MODE . "smart: 2010-08-02 #115
          wmatnr+0(1) = '0'.
        endwhile.
      endif.

      select single * from mara where matnr eq wmatnr.

      if sy-subrc = 0.
        if testlauf = ' '.
          perform verarbeiten_mm02.
        endif.
        counter = counter + 1.
        write:/ wmatnr, ' wird geändert.'.
      else.
        write:/ wmatnr, ' - nicht angelegt !'.
      endif.
    endloop.
    write: / 'Anzahl Datensätze, welche geändert werden: ', counter.
  endif.
*Ende MM02



* Test Programmberechtigung
  include zincl_progber.



*****************************************************************

*---------------------------------------------------------------------*
*       FORM verarbeiten_mm01                                         *
*---------------------------------------------------------------------*
form verarbeiten_mm01.
  clear: bmmh6.

*Transaktionsdaten
  bmm00-stype = '1'.
  bmm00-tcode = 'MM01'.          "auch bei neuer MEINS ändern
  bmm00-matnr = wmatnr.
  bmm00-lgnum = '700'.
  bmm00-xeis1 = 'X'.             "Lagerverwaltung

  if testlauf is initial.
    transfer bmm00 to dsno.
  endif.

*Hauptdaten (Mussegment !)
  bmmh1-stype = '2'.
  if testlauf is initial.
    transfer bmmh1 to dsno.
  endif.
  endform.
*---------------------------------------------------------------------*
*       form verarbeiten_mm02.                                        *
*---------------------------------------------------------------------*
form verarbeiten_mm02.
  clear: bmmh1.

*Transaktionsdaten
  bmm00-stype = '1'.
  bmm00-tcode = 'MM02'.          "auch bei neuer MEINS ändern
  bmm00-matnr = input-matnr.
  bmm00-werks = '7000'.
  bmm00-xeil1 = 'X'.             "Lagerung
  bmm00-xeie1 = 'X'.             "Einkauf

  if testlauf is initial.
    transfer bmm00 to dsno.
  endif.

*Hauptdaten (Musssegment !)
  bmmh1-stype = '2'.
  bmmh1-insmk  = input-insmk.
  bmmh1-xchpf = input-xchpf.
  bmmh1-mhdrz = input-mhdrz.
  bmmh1-iprkz = 'T'.
  if testlauf is initial.
    transfer bmmh1 to dsno.
  endif.
endform.
*---------------------------------------------------------------------*
*       FORM ABBRUCH                                                  *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM ABBRUCH USING TEXT TYPE CLIKE.              "smart: 2010-08-02 #105
  FORMAT COLOR 6 INTENSIFIED ON.
  SKIP 1.
  WRITE: / '**************  A B B R U C H  ***************************'.
  WRITE: /(58) TEXT.
  WRITE: / '**********************************************************'.
  STOP.
enDFORM.
