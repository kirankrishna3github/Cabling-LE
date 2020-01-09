
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #115
************************************************************************

REPORT Z200_MM_KLASS_CHARG.

tables: mara, mcha.

select-options:   smatnr for mara-matnr,
                  smtart for mara-mtart obligatory default 'FERT',
                  swerks for mcha-werks obligatory default '7000'.
parameters: testlauf as checkbox default 'X'.

data: count type p,
      wmatnr(18),
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

data: begin of itab occurs 0,
         matnr like mara-matnr,
         charg like mcha-charg,
         werks like mcha-werks,
      end of itab.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



clear: itab.

select * from mara where matnr in smatnr
                     and mtart in smtart.

  check: mara-mtart eq 'FERT' or
         mara-mtart eq 'HAWA' or
         mara-mtart eq 'HALB'.

  select * from mcha where matnr eq mara-matnr
                       and werks in swerks.

    count = count + 1.

    if testlauf = ' '.
      itab-matnr = mcha-matnr.
      itab-charg = mcha-charg.
      itab-werks = mcha-werks.
      append itab.
    endif.

  endselect.
endselect.


if testlauf = ' '.
  perform open_group.
endif.

loop at itab.

  clear: wmatnr.
  wmatnr = itab-matnr.
  while wmatnr+0(1) = '0'.
    shift wmatnr left IN CHARACTER MODE .        "smart: 2010-08-02 #115
  endwhile.
  perform transaktion_msc2.
endloop.

write: / count, 'Transaktionen'.

if testlauf = ' '.
  perform close_group.
endif.



* Test Programmberechtigung
  include zincl_progber.



*---------------------------------------------------------------------*
*       FORM transaktion_msc2                                        *
*---------------------------------------------------------------------*
form transaktion_msc2.

  perform bdc_dynpro      using 'SAPMM03S' '0105'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RM03S-MATNR' wmatnr.
  perform bdc_field       using 'RM03S-CHARG' itab-charg.
  perform bdc_field       using 'RM03S-WERKS' itab-werks.
  perform bdc_dynpro      using 'SAPMM03S' '0200'.
  perform bdc_field       using 'BDC_CURSOR' 'MCHA-VERAB'.
  perform bdc_field       using 'BDC_OKCODE' '=BU'.
  perform bdc_transaction using 'MSC2'.

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
            GROUP  = 'MSC2'
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
