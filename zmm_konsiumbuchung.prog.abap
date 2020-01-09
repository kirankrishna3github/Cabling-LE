
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #142
************************************************************************

REPORT ZMM_KONSIUMBUCHUNG.

tables: mara, marc, mbew, mkpf, msku, rfcu9.

select-options: swerks for msku-werks no intervals
                                      default 2100 obligatory,
                skunnr for msku-kunnr no intervals
                                      default 0000030685 obligatory,
                smatnr for msku-matnr,
                scharg for msku-charg.

parameters:     pwerks like msku-werks default 3100 obligatory.


parameters:     pumrech like RFCU9-kursp default '0.66225'.

parameters:     pbktxt like mkpf-bktxt obligatory
                            default 'Konsiumbuchung_2100'.

parameters: dsni like rlgrap-filename lower case
                     default 'C:\AUSSCHLUSS.TXT'
                     obligatory.
parameters: testlauf as checkbox default 'X'.


*Data
data: count type p,
      xfilename like rlgrap-filename,
      wpsstat like sy-subrc,
      wmill   like sy-subrc,
      wmatnr  like mara-matnr,
      wertein like mbew-vmkum,
      wertaus like mbew-vmkum,
      wdiff   like mbew-vmkum,
      wverme(15),
      werfme  like lqua-meins,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

*Ausschlusstabelle
data: begin of itab occurs 0,
         matnr(18),
      end of itab.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



start-of-selection.

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
            FILENAME = V_FILE1                   "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
            DATA_TAB     = itab[]                "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142

  IF XFILENAME IS INITIAL.
    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
  ENDIF.
  IF NOT XFILENAME CS '.txt'.
    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
  ENDIF.

*Mappe öffnen
  if testlauf = ' '.
    perform open_group.
  endif.

*Verarbeitung
  select * from msku where werks in swerks
                       and kunnr in skunnr
                       and sobkz eq 'W'
                       and kulab gt 0
                       and matnr in smatnr
                       and charg in scharg.

  perform write.

*Materialnummer ohne führende Null aufbereiten
clear: wmatnr.
write msku-matnr to wmatnr no-zero.

*Ausschluss prüfen
    read table itab with key matnr = wmatnr.
    if sy-subrc ne 0.

*Materialstamm Sichten für Einlagerung vorhanden ?
      select single * from marc where matnr eq msku-matnr
                                  and werks eq pwerks.
      clear: wpsstat.
      if sy-subrc = 0.
      if marc-pstat ca 'B'.     "Buchhaltung
         wpsstat = 0.
      else.
        wpsstat = 1.
        perform wsichten.
      endif.
      else.
       wpsstat = 1.
        perform wsichten.
      endif.

*Bewertungspreise gleich ?
      clear: wertein, wertaus.

      "Bewertung im einlagernden Werk
      select single * from mbew where matnr eq msku-matnr
                                  and bwkey eq pwerks.
      if sy-subrc = 0.
        if mbew-vprsv = 'V'.
          wertein = ( mbew-verpr * pumrech / mbew-peinh * msku-kulab ).
        else.
          wertein = ( mbew-stprs * pumrech / mbew-peinh * msku-kulab ).
        endif.
      endif.

      write: mbew-vprsv.

      "Bewertung im auslagernden Werk
      select single * from mbew where matnr eq msku-matnr
                                  and bwkey eq msku-werks.

      if sy-subrc = 0.
        if mbew-vprsv = 'V'.
          wertaus = ( mbew-verpr  / mbew-peinh * msku-kulab ).
        else.
          wertaus = ( mbew-stprs / mbew-peinh * msku-kulab ).
        endif.
      endif.

      clear: wdiff.
      wdiff = wertaus - wertein.
      write: 'Wertdiff.: ', wdiff, ' EUR'.


*MARA-MEINS
select single * from mara where matnr eq msku-matnr.


*Batch Input Mappen anlegen
if testlauf = ' ' and wpsstat = '0'.
      count = count + 1.

*Workfelder für Batchinput Eingaben der Auslagerung
clear: wverme.
write msku-kulab to wverme no-zero.

*Auslagern MM
      perform transaktion_562.

*Einlagern MM
perform transaktion_561.


*Ende Verarbeitung
endif.
*Protokoll Ausschlussmaterialnr.
    else.
      perform ausschluss.
    endif.

*Ende der Verarbeitung
  endselect.

  write: / count, 'Transaktionen'.


*Mappe schliessen
  if testlauf = ' '.
    perform close_group.
  endif.



* Test Programmberechtigung
  include zincl_progber.



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

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = 'WERK_UML'
            USER   = sy-uname.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM BDC_TRANSACTION USING TCODE TYPE CLIKE.     "smart: 2010-08-02 #105
  if testlauf = ' '.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              TCODE     = TCODE
         TABLES
              DYNPROTAB = BDCDATA.
  endif.
  REFRESH BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM BDC_DYNPRO USING PROGRAM TYPE CLIKE DYNPRO  "smart: 2010-08-02 #105
  TYPE CLIKE.                                    "smart: 2010-08-02 #105
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM BDC_FIELD USING FNAM TYPE CLIKE FVAL TYPE CLIKE.
                                                 "smart: 2010-08-02 #105

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  wsichten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM wsichten.
 write: 'BUHA Sicht fehlt: '.
ENDFORM.                    " wsichten
*&---------------------------------------------------------------------*
*&      Form  ausschluss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausschluss.
 write: 'Ausgeschlossen !'.

ENDFORM.                    " ausschluss
*&---------------------------------------------------------------------*
*&      Form  write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write.
write:/ msku-werks, msku-kunnr,
        msku-matnr, msku-charg, msku-kulab, mara-meins.
ENDFORM.                    " write

*&---------------------------------------------------------------------*
*&      Form  transaktion_561
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transaktion_561.

perform bdc_dynpro      using 'SAPMM07M' '0400'.
perform bdc_field       using 'MKPF-BKTXT' pbktxt.
perform bdc_field       using 'RM07M-BWARTWA' '561'.
perform bdc_field       using 'RM07M-SOBKZ' 'W'.
perform bdc_field       using 'RM07M-WERKS' pwerks.
perform bdc_field       using 'XFULL' 'X'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'MSEGK-KUNNR' msku-kunnr.
perform bdc_field       using 'MSEG-MATNR(01)' wmatnr.
perform bdc_field       using 'MSEG-ERFMG(01)' wverme.
perform bdc_field       using 'MSEG-CHARG(01)' msku-charg.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_OKCODE' '=BU'.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_transaction using 'MB11'.
clear: bdcdata.

ENDFORM.                    " transaktion_561

*&---------------------------------------------------------------------*
*&      Form  transaktion_562
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transaktion_562.

perform bdc_dynpro      using 'SAPMM07M' '0400'.
perform bdc_field       using 'MKPF-BKTXT'     pbktxt.
perform bdc_field       using 'RM07M-BWARTWA' '562'.
perform bdc_field       using 'RM07M-SOBKZ' 'W'.
perform bdc_field       using 'RM07M-WERKS' msku-werks.
perform bdc_field       using 'XFULL' 'X'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'MSEGK-KUNNR'    msku-kunnr.
perform bdc_field       using 'MSEG-MATNR(01)' wmatnr.
perform bdc_field       using 'MSEG-ERFMG(01)' wverme.
perform bdc_field       using 'MSEG-CHARG(01)' msku-charg.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_OKCODE' '=BU'.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_transaction using 'MB11'.
clear: bdcdata.

ENDFORM.                    " transaktion_562
