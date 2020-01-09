
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT ZLE_PLATZAENDERUNG.

tables: mara, marc, mbew, mkpf, lqua, rfcu9.

select-options: slgnum for lqua-lgnum no intervals
                                      default 391 obligatory,
                swerks for lqua-werks no intervals
                                      default 3000 obligatory,
                slgort for lqua-lgort no intervals
                                      default 3910 obligatory,
                slgtyp for lqua-lgtyp no intervals
                                      default 001  obligatory,
                slgpla for lqua-lgpla no intervals
                                       default 'PH' obligatory,
                smatnr for lqua-matnr,
                scharg for lqua-charg.

parameters:     plgtyp like lqua-lgtyp default '916',
                plgpla like lqua-lgpla obligatory.


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
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
      wlgpla  like lqua-lgpla.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



*Mappe öffnen
  if testlauf = ' '.
    perform open_group.
  endif.

*Verarbeitung
  select * from lqua where lgnum in slgnum
                       and lgtyp in slgtyp
                       and lgpla in slgpla
                       and werks in swerks
                       and matnr in smatnr
                       and charg in scharg
                       and gesme gt 0.

*Bestandesquali initial
check: lqua-bestq is initial.

clear: Wlgpla.
wlgpla = lqua-lgpla.


*nur wenn keine Einlagerung, Auslagerung
check: lqua-gesme = lqua-verme.

  perform write.

*Materialnummer ohne führende Null aufbereiten
clear: wmatnr.
write lqua-matnr to wmatnr no-zero.


*Workfelder für Batchinput Eingaben der Auslagerung
clear: wverme.
write lqua-verme to wverme no-zero.

*Umbuchen WM
      if testlauf = ' '.
       perform transaktion_lt01.
      endif.

*Ende der Verarbeitung
  endselect.

*Mappe schliessen
  if testlauf = ' '.
    perform close_group.
  endif.



* Test Programmberechtigung
  include zincl_progber.



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
FORM transaktion_lt01.

perform bdc_dynpro      using 'SAPML03T' '0101'.
perform bdc_field       using 'LTAK-BETYP' 'L'.
perform bdc_field       using 'LTAK-BENUM' plgpla.
perform bdc_field       using 'LTAK-LGNUM' lqua-lgnum.
perform bdc_field       using 'LTAK-BWLVS' '903'.
perform bdc_field       using 'LTAP-MATNR' wmatnr.
perform bdc_field       using 'RL03T-ANFME' wverme.
perform bdc_field       using 'LTAP-WERKS' lqua-werks.
perform bdc_field       using 'LTAP-LGORT' lqua-lgort.
perform bdc_field       using 'LTAP-CHARG' lqua-charg.

perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'LTAP-VLTYP' lqua-lgtyp.
perform bdc_field       using 'LTAP-VLPLA' lqua-lgpla.
perform bdc_field       using 'LTAP-NLTYP' plgtyp.
perform bdc_field       using 'LTAP-NLPLA' plgpla.
perform bdc_transaction using 'LT01'.
clear: bdcdata.

ENDFORM.                    " transaktion_lt01

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = 'LT01_UMB'
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
