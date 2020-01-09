
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT ZLE_METER_STUECK_KORR.

tables: lqua, mbew.

data: counter type p,
      xfilename like rlgrap-filename,
      zmatnr(18),
      zgesme(13),
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
      wfirst.

data: begin of w,
      $alt  like mbew-salk3,
      $neu  like mbew-salk3,
      $altt like mbew-salk3,
      $neut like mbew-salk3,
      $delta like mbew-salk3,
      end of w.


select-options: smatnr for lqua-matnr obligatory no intervals,
                swerks for lqua-werks obligatory default '3000'
                                      no intervals,
                slgort for lqua-lgort obligatory default '3910'
                                      no intervals,
                slgpla for lqua-lgpla,
                sgesme for lqua-gesme obligatory default '100'
                                      no intervals.

parameters:     pmatnr like lqua-matnr obligatory
                            default '2xxxxx'.


parameters: testlauf as checkbox default 'X'.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



if testlauf = ' '.
    perform open_group.
  endif.

select * from lqua where matnr in smatnr
                     and lgort in slgort
                     and werks in swerks
                     and lgtyp = '001'
                     and lgpla in slgpla
                     and gesme in sgesme.

  check: lqua-verme eq lqua-gesme.


 if wfirst is initial.
 clear: w.
*alter Bew.preis
 select single * from mbew where matnr eq lqua-matnr
                             and bwkey eq lqua-werks.

 if mbew-vprsv = 'S'.
 w-$alt = mbew-stprs / mbew-peinh.
 else.
 w-$alt = mbew-verpr / mbew-peinh.
 endif.

*neuer Bew.preis.
 select single * from mbew where matnr eq pmatnr
                             and bwkey eq lqua-werks.

 if mbew-vprsv = 'S'.
 w-$neu = mbew-stprs / mbew-peinh.
 else.
 w-$neu = mbew-verpr / mbew-peinh.
 endif.

 wfirst = '1'.
endif.
*Best.quantenwert



  write:/ lqua-matnr, lqua-lgort, lqua-werks, lqua-lgpla, lqua-gesme,
          w-$alt, w-$neu, ' mal Länge !'.
  counter = counter + 1.

  if testlauf = ' '.
   perform buchen.
  endif.
endselect.

  if testlauf = ' '.
    perform close_group.
  endif.

skip 2.
write: 'Anzahl Stück: ', counter, ' zum Einbuchen'.



* Test Programmberechtigung
  include zincl_progber.



*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = 'UMBUCH_M_ST'
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
*&      Form  buchen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buchen.

*MB1C



perform bdc_dynpro      using 'SAPMM07M' '0400'.
perform bdc_field       using 'RM07M-BWARTWA' '562'.
perform bdc_field       using 'RM07M-WERKS' lqua-werks.
perform bdc_field       using 'RM07M-LGORT' lqua-lgort.
perform bdc_field       using 'XFULL' 'X'.
perform bdc_field       using 'RM07M-WVERS2' 'X'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
clear: zmatnr.
write lqua-matnr to zmatnr no-zero.
perform bdc_field       using 'MSEG-MATNR(01)' zmatnr.
clear: zgesme.
write lqua-gesme to zgesme no-zero.
perform bdc_field       using 'MSEG-ERFMG(01)' zgesme.
perform bdc_field       using 'MSEG-ERFME(01)' lqua-meins.
perform bdc_field       using 'MSEG-CHARG(01)' lqua-charg.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR' 'MSEG-ERFMG(01)'.
perform bdc_field       using 'BDC_OKCODE' '=BU'.
perform bdc_field       using 'DKACB-FMORE' 'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE' '=ENTE'.
perform bdc_transaction using 'MB11'.

refresh bdcdata.

*LT01

perform bdc_dynpro      using 'SAPML03T' '0101'.
perform bdc_field       using 'LTAK-LGNUM' lqua-lgnum.
perform bdc_field       using 'LTAK-BWLVS' '999'.
perform bdc_field       using 'LTAP-MATNR' zmatnr.
perform bdc_field       using 'RL03T-ANFME' zgesme.
perform bdc_field       using 'LTAP-WERKS' lqua-werks.
perform bdc_field       using 'LTAP-LGORT' lqua-lgort.
perform bdc_field       using 'LTAP-CHARG' lqua-charg.
perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'LTAP-VLTYP' '001'.
perform bdc_field       using 'LTAP-VLPLA' lqua-lgpla.
perform bdc_field       using 'LTAP-NLTYP' '998'.
perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
perform bdc_transaction using 'LT01'.

clear: lqua.

ENDFORM.                    " buchen
