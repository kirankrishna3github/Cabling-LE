
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT ZM_MATBEL_STORNO_KORR .

tables: mkpf, mseg.

data: count type p,
      xfilename like rlgrap-filename,
      xmatnr like mara-matnr,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.


select-options: smblnr for mkpf-mblnr obligatory.

parameters: p561 radiobutton group 1,
            p562 radiobutton group 1.

parameters: pbudat(8) default '20040611',
            testlauf as checkbox default 'X'.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



if testlauf = ' '.
    perform open_group.
  endif.

select * from mkpf where usnam eq 'I020676'
                     and mblnr in smblnr
                     and mjahr eq '2004'
                     and budat eq pbudat.

  select single * from mseg where mblnr eq mkpf-mblnr
                              and bwart eq '561'
                              and zeile eq '0001'.

  if sy-subrc = 0.
    perform write.
    if p562 = 'X'.
    perform buchen_562.
    endif.
  else.
    select single * from mseg where mblnr eq mkpf-mblnr
                              and bwart eq '562'
                              and zeile eq '0001'.
    if sy-subrc = 0.
      perform write.
      if p561 = 'X'.
      perform buchen_561.
      endif.
    endif.
  endif.

endselect.

  if testlauf = ' '.
    perform close_group.
  endif.



* Test Programmberechtigung
  include zincl_progber.



*&---------------------------------------------------------------------*
*&      Form  buchen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buchen_562.

perform bdc_dynpro      using 'SAPMM07M' '0460'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM07M-MBLNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RM07M-MBLNR' mseg-mblnr.
perform bdc_field       using 'RM07M-MJAHR'
                              '2004'.

perform bdc_field       using 'XFULL'
                              'X'.
perform bdc_field       using 'RM07M-WVERS2'
                              'X'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM07M-XSELK(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'DKACB-FMORE'
                              'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM07M-XSELK(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'DKACB-FMORE'
                              'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_transaction using 'MBST'.


ENDFORM.                    " buchen
*&---------------------------------------------------------------------*
*&      Form  write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write.
  write:/ mkpf-usnam, mkpf-budat,
             mseg-mblnr, mseg-bwart.

ENDFORM.                    " write

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = 'MB-STORNO'
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
*&      Form  buchen_561
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buchen_561.

perform bdc_dynpro      using 'SAPMM07M' '0460'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM07M-MBLNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RM07M-MBLNR' mseg-mblnr.
perform bdc_field       using 'RM07M-MJAHR'
                              '2004'.
perform bdc_field       using 'XFULL'
                              'X'.
perform bdc_field       using 'RM07M-WVERS2'
                              'X'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM07M-XSELK(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'DKACB-FMORE'
                              'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM07M-XSELK(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'DKACB-FMORE'
                              'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_field       using 'DKACB-FMORE'
                              'X'.
perform bdc_dynpro      using 'SAPLKACB' '0002'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTE'.
perform bdc_transaction using 'MBST'.

ENDFORM.                    " buchen_561
