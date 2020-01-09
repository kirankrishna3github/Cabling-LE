
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #115
************************************************************************

REPORT ZMBSTUEB.

tables: mara, marc, mbew.

*parameters: dsni like rlgrap-filename lower case
*            default 'H:\Datenübernahmen\Setartikel\SETARTIKEL_ZVK.txt'
*                     obligatory.
select-options: smatnr for mara-matnr obligatory.

parameters: testlauf as checkbox default 'X'.

data: countok type p,
      counte1 type p,
      counte2 type p,
      xfilename like rlgrap-filename,
      xmatnr like mara-matnr,
      xmat(18),
      wsubrc like sy-subrc,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

data: begin of itab occurs 0,
         matnr like mara-matnr,
      end of itab.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



*start-of-selection.
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            FILENAME     = dsni
*            FILEtype     = 'DAT'
*       IMPORTING
*            ACT_FILENAME = XFILENAME
*       TABLES
*            DATA_TAB     = itab.
*
*  IF XFILENAME IS INITIAL.
*    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
*  ENDIF.
*  IF NOT XFILENAME CS '.txt'.
*    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
*  ENDIF.

select * from mara where matnr in smatnr
                     and mtart eq 'ZKKO'.
  itab-matnr = mara-matnr.
  append itab.
  write:/ mara-matnr.
endselect.

  if testlauf = ' '.
    perform open_group.
  endif.

  loop at itab.
*Matnr. aufbereiten
    clear: xmatnr.
    check not itab-matnr is initial.
    check itab-matnr co ' 0123456789'.
    xmatnr = itab-matnr.
    while xmatnr+17(1) = ' '.
      shift xmatnr right IN CHARACTER MODE .     "smart: 2010-08-02 #115
      xmatnr+0(1) = '0'.
    endwhile.
*Mat.art FERT ?
    select single * from mara where matnr eq xmatnr.
    if sy-subrc = 0.
      if mara-mtart ne 'ZKKO'.
        write:/ mara-matnr, mara-mtart, ' - kein ZKKO !'.
        counte1 = counte1 + 1.
      else.

*Material mit Bestand ?
        clear: wsubrc.
        select * from mbew where matnr eq xmatnr
                             and lbkum ne 0.
          if sy-subrc eq 0.
        wsubrc = sy-subrc.
        write:/ mbew-matnr, mbew-bwkey, mbew-lbkum,  ' - hat Bestand !'.
            counte2 = counte2 + 1.
          endif.
        endselect.

*workfeld Matnr für Dynproübergabe
        clear: xmat.
        write mara-matnr to xmat.

*Mappe erstellen
        if testlauf = ' ' and wsubrc eq 0.

          perform transaktion_xxx.
        endif.
        countok = countok + 1.
      endif.
    endif.
  endloop.

  write: / countok, 'Fehlerfreie Transaktionen'.
  write: / counte1, 'Fehlerhafte Materialarten'.
  write: / counte2, 'Materialnr. mit Bestand auf Werk'.

  if testlauf = ' '.
    perform close_group.
  endif.



* Test Programmberechtigung
  include zincl_progber.



*---------------------------------------------------------------------*
*       FORM transaktion_xxx                                         *
*---------------------------------------------------------------------*
form transaktion_xxx.

  perform bdc_dynpro      using 'SAPMM03Z' '0100'.
  perform bdc_field       using 'BDC_CURSOR' 'RM03Z-NMTAR'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RM03Z-MATNR' xmat.
  perform bdc_field       using 'RM03Z-NMTAR' 'HAWA'.
  perform bdc_dynpro      using 'SAPMM03Z' '0100'.
  perform bdc_field       using 'BDC_CURSOR' 'RM03Z-MATNR'.
  perform bdc_field       using 'BDC_OKCODE' '=LOS'.
  perform bdc_field       using 'RM03Z-MATNR' xmat.
  perform bdc_field       using 'RM03Z-NMTAR' 'HAWA'.
  perform bdc_transaction using 'MMAM'.

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
            GROUP  = 'MTART_HAWA'
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
