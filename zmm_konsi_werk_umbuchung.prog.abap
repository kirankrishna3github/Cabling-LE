
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT ZMM_KONSI_WERK_UMBUCHUNG.

*Beschreibung:
*Auflösung des Konsilagers AMEA.
*Bestandesübernahme nach Werk 5000 Lagerort 5100
*
*Antragsteller: Angelo Bacchi
*
*SAP Stäfa, 2.2005, Peter Huber

tables: mara, marc, mbew, mkpf, msku, rfcu9.

select-options: swerks for msku-werks no intervals
                                      default 3100 obligatory,
                skunnr for msku-kunnr no intervals
                                      default 0000033647 obligatory,
                smatnr for msku-matnr,
                scharg for msku-charg.

parameters:     pwerks like msku-werks default 5000 obligatory,
                plgort like lqua-lgort default 5910 obligatory,
                plgnum like lqua-lgnum obligatory default '591'.
parameters:     pbktxt like mkpf-bktxt obligatory
                            default 'AMEA_an_W5000'.

parameters:     pumrech like RFCU9-kursp default '0.66225'.

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



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



start-of-selection.

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
          wertein = ( mbew-verpr / pumrech / mbew-peinh * msku-kulab ).
        else.
          wertein = ( mbew-stprs / pumrech / mbew-peinh * msku-kulab ).
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
      wdiff =  wertein - wertaus.
      write: 'Wertdiff.: ', wdiff, ' CHF'.
*MARA-MEINS
select single * from mara where matnr eq msku-matnr.
*Batch Input Mappen anlegen
if testlauf = ' ' and wpsstat = '0'.
      count = count + 1.
*Workfelder für Batchinput Eingaben der Auslagerung
clear: wverme.
write msku-kulab to wverme no-zero.
write msku-matnr to wmatnr no-zero.
*Auslagern MM
perform transaktion_562.
*Einlagern MM
perform transaktion_561.
*Einlagern WM
perform transaktion_lt01ein..
*Ende Verarbeitung
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



*----------------------------------------------------------------------
*
*   create batchinput session
*
*----------------------------------------------------------------------
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = 'WERK_UML'
            USER   = sy-uname.
ENDFORM.
*----------------------------------------------------------------------
*
*   end batchinput session
*
*----------------------------------------------------------------------
FORM CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.
*----------------------------------------------------------------------
*
*        Start new transaction according to parameters
*
*----------------------------------------------------------------------

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
*----------------------------------------------------------------------
*
*        Start new screen
*
*----------------------------------------------------------------------

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
*----------------------------------------------------------------------
*
*        Insert field
*
*----------------------------------------------------------------------
*

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
perform bdc_field       using 'RM07M-WERKS' pwerks.
perform bdc_field       using 'RM07M-LGORT' plgort.
perform bdc_field       using 'XFULL' 'X'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
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
perform bdc_field       using 'LTAP-LGORT'  '5910'.
perform bdc_field       using 'LTAP-CHARG'  msku-charg.
perform bdc_dynpro      using 'SAPML03T'    '0102'.
perform bdc_field       using 'LTAP-VLTYP'  '998'.
perform bdc_field       using 'LTAP-VLPLA'  'AUFNAHME'.
perform bdc_field       using 'LTAP-NLTYP'  '001'.
perform bdc_field       using 'LTAP-NLPLA'  '5000'.
perform bdc_transaction using 'LT01'.
clear: bdcdata.
ENDFORM.                   " transaktion_lt01ein
