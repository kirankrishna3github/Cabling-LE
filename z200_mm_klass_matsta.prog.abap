
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #115
************************************************************************

REPORT ZMBSTUEB.

tables: mara.

select-options:   smatnr for mara-matnr,
                  smtart for mara-mtart obligatory default 'FERT'.
parameters: testlauf as checkbox default 'X'.

data: count type p,
      wmatnr(18),
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

data: begin of itab occurs 0,
         matnr like mara-matnr,
      end of itab.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



select * from mara where matnr in smatnr
                     and mtart in smtart.
  if testlauf = ' '.
    itab-matnr = mara-matnr.
    append itab.
  endif.
endselect.


if testlauf = ' '.
  perform open_group.
endif.

loop at itab.
clear: wmatnr.
wmatnr = itab-matnr.
while wmatnr+0(1) = '0'.
shift wmatnr left IN CHARACTER MODE .            "smart: 2010-08-02 #115
endwhile.
  perform transaktion_cl20n.
endloop.

write: / count, 'Transaktionen'.

if testlauf = ' '.
  perform close_group.
endif.



* Test Programmberechtigung
  include zincl_progber.



*---------------------------------------------------------------------*
*       FORM transaktion_cl20n                                        *
*---------------------------------------------------------------------*
form transaktion_cl20n.

  perform bdc_dynpro      using 'SAPLCLFM' '1100'.
  perform bdc_field       using 'BDC_CURSOR' 'RMCLF-KLART'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RMCLF-KLART' '023'.
  perform bdc_dynpro      using 'SAPLCLFM' '1100'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RMCLF-KLART' '023'.
*perform bdc_field       using 'BDC_CURSOR' 'RMCBC-MATNR'.
  perform bdc_field       using 'RMCBC-MATNR' wmatnr.   "Aufbereitete
                                                        "Matnr.
  perform bdc_dynpro      using 'SAPLCLFM' '0500'.
*perform bdc_field       using 'BDC_CURSOR' 'RMCLF-CLASS(01)'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RMCLF-CLASS(01)' 'ZCHARGEN'.
  perform bdc_dynpro      using 'SAPLCTMS' '0109'.
  perform bdc_field       using 'BDC_CURSOR' 'RCTMS-MNAME(01)'.
  perform bdc_field       using 'BDC_OKCODE' '=BACK'.
  perform bdc_dynpro      using 'SAPLCLFM' '0500'.
  perform bdc_field       using 'BDC_CURSOR' 'RMCLF-CLASS(01)'.
  perform bdc_field       using 'BDC_OKCODE' '=SAVE'.
  perform bdc_field       using 'RMCLF-PAGPOS' '1'.
  perform bdc_dynpro      using 'SAPLCLFM' '1100'.
  perform bdc_field       using 'BDC_OKCODE' '=SAVE'.
  perform bdc_field       using 'RMCLF-PAGPOS' '1'.
  perform bdc_field       using 'BDC_CURSOR' 'RCTMS-MWERT(01)'.
  perform bdc_transaction using 'CL20N'.

  count = count + 1.
endform.

*---------------------------------------------------------------------*
*       FORM ABBRUCH                                                  *
*---------------------------------------------------------------------*
FORM ABBRUCH USING TEXT.
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
            GROUP  = 'CL20N'
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
