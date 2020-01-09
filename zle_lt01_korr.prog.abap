
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT ZLE_LT01_KORR.

tables: lqua.

parameters: ppos radiobutton group 1,
            pneg radiobutton group 1.

parameters: testlauf as checkbox default 'X'.

data: count type p,
      xfilename like rlgrap-filename,
      xmatnr like mara-matnr,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

data: wmatnr(10),
      wgesme(10).

data: begin of ilqua occurs 0.
      include structure lqua.
data: platz(10),
      gesmeum like lqua-gesme.
data: end of ilqua.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



select * from lqua where lgnum = '211'
                     and lgtyp = '998'.

 check: lqua-gesme eq lqua-verme.

ilqua-gesmeum = lqua-gesme.
*möglicher Platz ermitteln
*negativer 998er
if pneg = 'X'.
check: ilqua-gesmeum lt 0.
if not lqua-charg is initial.
select single * from lqua where lgnum = '211'
                            and lgtyp = '001'
                            and werks = '2000'
                            and matnr = lqua-matnr
                            and charg = lqua-charg.

elseif lqua-charg is initial.
  select single * from lqua where lgnum = '211'
                            and lgtyp = '001'
                            and werks = '2000'
                            and matnr = lqua-matnr.

  clear: ilqua-platz.
  if sy-subrc = 0.
   ilqua-platz = lqua-lgpla.
  else.
   "nichts
  endif.
 endif.

elseif ppos = 'X'.
check: ilqua-gesmeum gt 0.
if not lqua-charg is initial.
select single * from lqua where lgnum = '211'
                            and lgtyp = '001'
                            and werks = '2000'
                            and matnr = lqua-matnr
                            and charg = lqua-charg.

elseif lqua-charg is initial.
select single * from lqua where lgnum = '211'
                            and lgtyp = '001'
                            and werks = '2000'
                            and matnr = lqua-matnr.

if sy-subrc = 0.
 ilqua-platz = lqua-lgpla.
else.
 ilqua-platz = 'KORR'.
endif.
endif.
endif.

move-corresponding lqua to ilqua.
append ilqua.

endselect.


start-of-selection.

  if testlauf = ' '.
    perform open_group.
  endif.

  loop at ilqua.
  check: not ilqua-platz is initial.
   if ppos = 'X'.
      perform transaktion_lt01p.
   elseif pneg = 'X'.
      perform transaktion_lt01n.
   endif.
  endloop.

  write: / count, 'Transaktionen'.

  if testlauf = ' '.
    perform close_group.
  endif.



* Test Programmberechtigung
  include zincl_progber.



*---------------------------------------------------------------------*
*       FORM transaktion_lt01p                                        *
*---------------------------------------------------------------------*
form transaktion_lt01p.

clear: wmatnr, wgesme.
write ilqua-matnr+10(8) to wmatnr.
write ilqua-gesmeum to wgesme.

perform bdc_dynpro      using 'SAPML03T' '0101'.
perform bdc_field       using 'LTAK-LGNUM' '211'.
perform bdc_field       using 'LTAK-BWLVS' '999'.
perform bdc_field       using 'LTAP-MATNR' wmatnr.
perform bdc_field       using 'RL03T-ANFME' wgesme.
perform bdc_field       using 'LTAP-WERKS' ilqua-werks.
perform bdc_field       using 'LTAP-LGORT' ilqua-lgort.
if not ilqua-charg is initial.
perform bdc_field       using 'LTAP-CHARG' ilqua-charg.
endif.
perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'LTAP-VLTYP' '998'.
perform bdc_field       using 'LTAP-VLPLA' 'AUFNAHME'.
perform bdc_field       using 'LTAP-NLTYP' '001'.
perform bdc_field       using 'LTAP-NLPLA' ilqua-platz.
perform bdc_transaction using 'LT01'.

  count = count + 1.
endform.

*---------------------------------------------------------------------*
*       FORM transaktion_lt01n                                        *
*---------------------------------------------------------------------*
form transaktion_lt01n.

clear: wmatnr, wgesme.
write ilqua-matnr+10(8) to wmatnr.
ilqua-gesmeum = ilqua-gesmeum * ( -1 ).
write ilqua-gesmeum to wgesme.

perform bdc_dynpro      using 'SAPML03T' '0101'.
perform bdc_field       using 'LTAK-LGNUM' '211'.
perform bdc_field       using 'LTAK-BWLVS' '999'.
perform bdc_field       using 'LTAP-MATNR' wmatnr.
perform bdc_field       using 'RL03T-ANFME' wgesme.
perform bdc_field       using 'LTAP-WERKS' ilqua-werks.
perform bdc_field       using 'LTAP-LGORT' ilqua-lgort.
if not ilqua-charg is initial.
perform bdc_field       using 'LTAP-CHARG' ilqua-charg.
endif.
perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'LTAP-VLTYP' '001'.
perform bdc_field       using 'LTAP-VLPLA' ilqua-platz.
perform bdc_field       using 'LTAP-NLTYP' '998'.
perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
perform bdc_transaction using 'LT01'.

  count = count + 1.
endform.
*---------------------------------------------------------------------*
*       FORM transaktion_lt01x                                        *
*---------------------------------------------------------------------*
form transaktion_lt01x.
perform bdc_dynpro      using 'SAPML03T' '0101'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTAP-CHARG'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'LTAK-LGNUM'
                              '391'.
perform bdc_field       using 'LTAK-BWLVS'
                              '999'.
perform bdc_field       using 'LTAP-MATNR'
                              '166317'.
perform bdc_field       using 'RL03T-ANFME'
                              '1000'.
perform bdc_field       using 'LTAP-WERKS'
                              '3000'.
perform bdc_field       using 'LTAP-LGORT'
                              '3910'.
perform bdc_field       using 'LTAP-CHARG'
                              '0000000137'.
perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTAP-NLPLA'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RL03T-ANFME'
                              '1,000.000'.
perform bdc_field       using 'LTAP-ALTME'
                              'M'.
perform bdc_field       using 'LTAP-WDATU'
                              '24.05.2004'.
perform bdc_field       using 'LTAP-VLTYP'
                              '998'.
perform bdc_field       using 'LTAP-VLPLA'
                              'aufnahme'.
perform bdc_field       using 'LTAP-NLTYP'
                              '001'.
perform bdc_field       using 'LTAP-NLPLA'
                              'ph'.
perform bdc_transaction using 'LT01'.



  count = count + 1.
endform.
*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = 'LT01_KORR'
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
