
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105 #111 #115 #142
************************************************************************

REPORT Z200_MM_BESTAND_OUT_01.

************************************************************************
*MM Bestände, welche in einer EXCEL Datei bereitgestellt werden,
*übernehmen.
************************************************************************
*Vorgehen:

*Schritt: 1
*Vorbereitung:
*Logische Datei anlegen (ZBESTAND)
*Transaktion: FILE
*log. Datei      ZBESTAND_OUT
*phys. Datei     /usr/sap/trans_R3/BESTAND_OUT.DAT

*Schritt: 2
*Report Z200_MM_BESTAND_OUT_01 ausführen und Daten in ZBESTAND_OUT
*sichern

*Schritt 3
*Protkoll Datei sichern !

*Schritt: 4
*Report RM07MMBL ausführen und Batch-Input Mappe anlegen

*Schritt: 5
*Mappe abspielen
************************************************************************
*Tabellen:
tables: mara, marc, mard, mbew.
tables: bmseg.

************************************************************************
*Felder
data: dataset(70).
data: count type p,
      xfilename like rlgrap-filename,
      xmess(100),
      count_1 type p,
      wmatnr(18),
      wbsquali(18),
      xmenge type p decimals 3.

************************************************************************
*Interne Tabelle

data: begin of imard occurs 0.
        include structure mard.
data: xlabst(1).
data: xspeme(1).
data: xinsme(1).
data: wmatnr(8).
data: end of imard.

*Download
data: begin of imard2 occurs 0.
        include structure imard.
data: end of imard2.

************************************************************************
*Benutzeroberfläche

select-options: smatnr for mara-matnr,
                swerks for mard-werks default '7000',
                slgort for mard-lgort default '7000'.

parameters: dsno(60) lower case
                default '/usr/sap/trans_R3/BESTAND_OUT.DAT'.

parameters: dsni like rlgrap-filename lower case default
'H:\Bestand_out.txt' obligatory.

parameters: p_datum like sy-datum default sy-datum.
parameters: test as checkbox default 'X'.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



************************************************************************
*Verarbeitung
start-of-selection.


*relevante Bestände in imard.
*Frei
clear: mard, imard.
  select * from mard where matnr in smatnr
                       and werks in swerks
                       and lgort in slgort.
    check: not mard-labst is initial.
    move-corresponding mard to imard.
    imard-xlabst = 'X'.
    append imard.
  endselect.
*Quali

clear: mard, imard.
  select * from mard where matnr in smatnr
                       and werks in swerks
                       and lgort in slgort.
    check: not mard-insme is initial.
    move-corresponding mard to imard.
    imard-xinsme = 'X'.
    append imard.
  endselect.

*Sperr
clear: mard, imard.
  select * from mard where matnr in smatnr
                       and werks in swerks
                       and lgort in slgort.
    check: not mard-speme is initial.
    move-corresponding mard to imard.
    imard-xspeme = 'X'.
    append imard.
  endselect.




*$smart (F) 2010-08-02 - #111 OPEN DATASET benötigt mode und encoding
*$smart (F) 2010-08-02 - #111 Spezifikation Adding encoding to OPEN
*$smart (F) 2010-08-02 - #111 DATASET statement (A)

  open dataset dsno for output in text mode message xmess
ENCODING UTF-8 .                                 "smart: 2010-08-02 #111
  if test = 'X'.
    write: / dsno, xmess.
  endif.
  detail.

*Datenbestand abloopen
  loop at imard.
    clear: wmatnr.
    wmatnr = imard-matnr.
    while wmatnr+0(1) = '0'.
      shift wmatnr left IN CHARACTER MODE .      "smart: 2010-08-02 #115
    endwhile.
    imard-wmatnr = wmatnr.
    modify imard.
    select single * from mara where matnr eq imard-matnr.


    write:/ imard-wmatnr.
    if imard-xlabst = 'X'.
      write: imard-labst, 'FREI   '.
    elseif imard-xinsme = 'X'.
      write: imard-insme, 'Qualiät'.
    elseif imard-xspeme = 'X'.
      write: imard-speme, 'SPERR  '.
    endif.
    write: ' ', mara-meins.


    perform verarbeiten_input.
    count_1 = count_1 + 1.
  endloop.

  skip 1.
  write: / 'Anzahl Datensätze: ', count_1.


*Bestandestabelle hochladen

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
V_DEFAULT_FILENAME1 = dsni.                      "smart: 2010-08-02 #142
SHIFT V_DEFAULT_FILENAME1 RIGHT DELETING TRAILING"smart: 2010-08-02 #142
   '\'.                                          "smart: 2010-08-02 #142
SHIFT V_DEFAULT_FILENAME1 RIGHT DELETING TRAILING"smart: 2010-08-02 #142
   '/'.                                          "smart: 2010-08-02 #142
SHIFT V_DEFAULT_FILENAME1 LEFT DELETING LEADING  "smart: 2010-08-02 #142
  SPACE .                                        "smart: 2010-08-02 #142
V_DEFAULT_EXTENSION1 = 'DAT'.                    "smart: 2010-08-02 #142
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
            FILENAME = V_FULLPATH1               "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
            DATA_TAB     = imard[]               "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142

  IF XFILENAME IS INITIAL.
    PERFORM ABBRUCH USING 'Download vom Benutzer abgebrochen'.
  ENDIF.
  IF NOT XFILENAME CS '.txt'.
    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
  ENDIF.



* Test Programmberechtigung
  include zincl_progber.



*---------------------------------------------------------------------*
*       FORM verarbeiten_input                                        *
*---------------------------------------------------------------------*
form verarbeiten_input.

  clear bmseg.
  bmseg-mappe = 'MATBESTOUT'.
  bmseg-tcode = 'MB11'.
  bmseg-bldat =  P_DATUM.
  bmseg-budat =  P_DATUM.
  concatenate  'Projekt LVS OUT ' p_datum ' ' into bmseg-bktxt
  separated by space IN CHARACTER MODE .         "smart: 2010-08-02 #101
  concatenate  'Projekt LVS OUT ' p_datum imard-wmatnr into bmseg-sgtxt
    separated by space IN CHARACTER MODE .       "smart: 2010-08-02 #101

*Bewegungsart setzen
  if imard-xlabst = 'X'.
    bmseg-bwart = '562'.                  "frei
  elseif imard-xinsme = 'X'.
    bmseg-bwart = '564'.                  "Quali
  elseif imard-xspeme = 'X'.
    bmseg-bwart = '566'.                  "Sperr
  endif.
  bmseg-matnr = imard-wmatnr.
  bmseg-werks = imard-werks.
  bmseg-lgort = imard-lgort.

*nach Bewegungsart Bestandsfeld übergeben
  if imard-xlabst = 'X'.
    bmseg-erfmg = imard-labst.
  elseif imard-xinsme = 'X'.
    bmseg-erfmg = imard-insme.
  elseif imard-xspeme = 'X'.
    bmseg-erfme = imard-speme.
  endif.

  if test is initial.
    transfer bmseg to dsno.
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
