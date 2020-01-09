
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105 #142
************************************************************************

REPORT ZMM_WERKUMBUCHUNG.

*Report bucht von Lagerort 2100 nach 3100 innerhalb der Lagrnr. 392 um.
*Dabei werden nur die Matnr. einbezogen, welche in der Uploadtabelle
*aufgenommen worden sind.
*
*siehe Dokumentation
*
*Antragsteller: Ivan Ilicic im November 2004
*
*SAP Stäfa, P. Huber, 12.11.04
************************************************************************


tables: mara, marc, mbew, mkpf, lqua, rfcu9.

select-options: slgnum for lqua-lgnum no intervals
                                      default 392 obligatory,
                swerks for lqua-werks no intervals
                                      default 2100 obligatory,
                slgort for lqua-lgort no intervals
                                      default 2110 obligatory,
                smatnr for lqua-matnr,
                scharg for lqua-charg.

parameters:     pwerks like lqua-werks default '3100' obligatory,
                plgort like lqua-lgort default '3920',
                plgnum like lqua-lgnum default '392' obligatory.


parameters:     pumrech like RFCU9-kursp obligatory.

parameters:     pbktxt like mkpf-bktxt obligatory.

parameters: dsni like rlgrap-filename lower case
                     default 'C:\MATERIALNUMMERNLISTE.TXT'
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
      wdatum  like sy-datum,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

*Ausschlusstabelle
data: begin of itab occurs 0,
         matnr(18),
      end of itab.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



*Kopftext füllen
concatenate 'Uml_3100_2100_' sy-datum into pbktxt
IN CHARACTER MODE .                              "smart: 2010-08-02 #101

*Verarbeitung
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
  select * from lqua where lgnum in slgnum
                       and lgtyp eq '001'
                       and werks in swerks
                       and matnr in smatnr
                       and charg in scharg.

*Bestandesquali initial
check: lqua-bestq is initial.

  perform write.

*Materialnummer ohne führende Null aufbereiten
clear: wmatnr.
write lqua-matnr to wmatnr no-zero.

*Prüfen, ob Matnr. umgebucht werden soll
    read table itab with key matnr = wmatnr.
    if sy-subrc eq 0.

*Materialstamm Sichten für Einlagerung vorhanden ?
      select single * from marc where matnr eq lqua-matnr
                                  and werks eq pwerks.
      clear: wpsstat.
      if sy-subrc = 0.
      if marc-pstat ca 'B'     "Buchhaltung
        and marc-pstat ca 'L'  "Lagerung
        and marc-pstat ca 'S'. "Lagerverwaltung
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
      select single * from mbew where matnr eq lqua-matnr
                                  and bwkey eq pwerks.
      if sy-subrc = 0.
        if mbew-vprsv = 'V'.
          wertein = ( mbew-verpr * pumrech / mbew-peinh * lqua-verme ).
        else.
          wertein = ( mbew-stprs * pumrech / mbew-peinh * lqua-verme ).
        endif.
      endif.

      write: mbew-vprsv.

      "Bewertung im auslagernden Werk
      select single * from mbew where matnr eq lqua-matnr
                                  and bwkey eq lqua-werks.

      if sy-subrc = 0.
        if mbew-vprsv = 'V'.
          wertaus = ( mbew-verpr  / mbew-peinh * lqua-verme ).
        else.
          wertaus = ( mbew-stprs / mbew-peinh * lqua-verme ).
        endif.
      endif.

      wdiff = wertaus - wertein.
      write: 'Wertdiff.: ', wdiff, ' EUR'.


*Stückchargenpflicht im MM ?
select single * from mara where matnr eq lqua-matnr
                            and dpcbt eq 'C'.

wmill = sy-subrc.

*Batch Input Mappen anlegen
if testlauf = ' ' and wpsstat = '0'.
      count = count + 1.

*Workfelder für Batchinput Eingaben der Auslagerung
clear: wverme.
write lqua-verme to wverme no-zero.

*Auslagern MM
      perform transaktion_561.

*Workfelder für Batchinput Eingaben TA Anlegen
clear: wverme.
write lqua-verme to wverme no-zero.

*Auslagern WM
      perform transaktion_lt01aus.

*Einlagern MM
perform transaktion_562.

*Einlagern WM
perform transaktion_lt01ein.

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
perform bdc_field       using 'RM07M-BWARTWA' '562'.
perform bdc_field       using 'RM07M-WERKS' lqua-werks.
perform bdc_field       using 'RM07M-LGORT' lqua-lgort.
perform bdc_field       using 'XFULL' 'X'.

perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR' 'MSEG-CHARG(01)'.
perform bdc_field       using 'MSEG-MATNR(01)' wmatnr.
*Stückchargen mit Stück statt Meter ausbuchen
if wmill = 0.
 wverme = 1.
 werfme = 'ST'.
else.
 werfme = lqua-meins.
endif.
perform bdc_field       using 'MSEG-ERFMG(01)' wverme.
perform bdc_field       using 'MSEG-ERFME(01)' werfme.
perform bdc_field       using 'MSEG-CHARG(01)' lqua-charg.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR' 'MSEG-ERFMG(01)'.
perform bdc_field       using 'BDC_OKCODE' '=BU'.
*perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_transaction using 'MB11'.
clear: bdcdata.

ENDFORM.                    " transaktion_561
*&---------------------------------------------------------------------*
*&      Form  wsichten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM wsichten.
 write: 'Sicht fehlt: '.
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
write:/ lqua-lgnum, lqua-werks, lqua-lgort, lqua-lgtyp, lqua-lgpla,
        lqua-matnr, lqua-charg, lqua-verme, lqua-meins.
ENDFORM.                    " write
*&---------------------------------------------------------------------*
*&      Form  transaktion_lt01aus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transaktion_lt01aus.

perform bdc_dynpro      using 'SAPML03T' '0101'.
perform bdc_field       using 'LTAK-LGNUM' lqua-lgnum.
perform bdc_field       using 'LTAK-BWLVS' '999'.
perform bdc_field       using 'LTAP-MATNR' wmatnr.
perform bdc_field       using 'RL03T-ANFME' wverme.
perform bdc_field       using 'LTAP-WERKS' lqua-werks.
perform bdc_field       using 'LTAP-LGORT' lqua-lgort.
perform bdc_field       using 'LTAP-CHARG' lqua-charg.

perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'LTAP-VLTYP' lqua-lgtyp.
perform bdc_field       using 'LTAP-VLPLA' lqua-lgpla.
perform bdc_field       using 'LTAP-NLTYP' '998'.
perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
perform bdc_transaction using 'LT01'.
clear: bdcdata.

ENDFORM.                    " transaktion_lt01
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
perform bdc_field       using 'MKPF-BKTXT' pbktxt.
perform bdc_field       using 'RM07M-BWARTWA' '561'.
perform bdc_field       using 'RM07M-WERKS' pwerks.
perform bdc_field       using 'RM07M-LGORT' plgort.
perform bdc_field       using 'XFULL' 'X'.

perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'MSEG-MATNR(01)' wmatnr.
perform bdc_field       using 'MSEG-ERFMG(01)' wverme.
perform bdc_field       using 'MSEG-CHARG(01)' lqua-charg.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'..
perform bdc_field       using 'BDC_OKCODE' '=BU'.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_transaction using 'MB11'.
clear: bdcdata.
ENDFORM.                    " transaktion_562
*&---------------------------------------------------------------------*
*&      Form  transaktion_lt01ein
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transaktion_lt01ein.

perform bdc_dynpro      using 'SAPML03T'    '0101'.
perform bdc_field       using 'LTAK-LGNUM'  plgnum.
perform bdc_field       using 'LTAK-BWLVS'  '999'.
perform bdc_field       using 'LTAP-MATNR'  wmatnr.
perform bdc_field       using 'RL03T-ANFME' wverme.
perform bdc_field       using 'LTAP-WERKS'  pwerks.
perform bdc_field       using 'LTAP-LGORT'  plgort.
perform bdc_field       using 'LTAP-CHARG'  lqua-charg.

perform bdc_dynpro      using 'SAPML03T'    '0102'.
perform bdc_field       using 'LTAP-VLTYP'  '998'.
perform bdc_field       using 'LTAP-VLPLA'  'AUFNAHME'.
perform bdc_field       using 'LTAP-NLTYP'  lqua-lgtyp.
perform bdc_field       using 'LTAP-NLPLA'  lqua-lgpla.
perform bdc_transaction using 'LT01'.
clear: bdcdata.

ENDFORM.                    " transaktion_lt01ein
