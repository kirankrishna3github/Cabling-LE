*&---------------------------------------------------------------------*
*& Report  ZMM_EC_PCA_BESTAND_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_EC_PCA_BESTAND_UPLOAD.

tables: lqua, ZMM_TRANS_UPLOAD,
        mbew.

parameters: dsni like rlgrap-filename lower case
                     default 'J:\Cabling\DKA\SAP_PC_Projekt\900_Migration\QC2 25112013 UEB.txt'
                     obligatory.
select-options: smatnr for lqua-matnr.
parameters: ptest as checkbox default 'X',
            pretest as checkbox default 'X'.

data: count type p,
      xfilename like rlgrap-filename,
      xmatnr like lqua-matnr,
      xcharg like lqua-charg.



data: begin of itab occurs 0,
         matnr(18),
         werks(4),
         charg(10),
         lqnum(10),
         lgpla(10),
         menge(13),
         meins(3),
      end of itab.

data: begin of imbewo occurs 0.
        include structure mbew.
data: end of imbewo.

data: begin of imbewi occurs 0.
        include structure mbew.
data: end of imbewi.

data: begin of imarc occurs 0.
        include structure marc.
data: end of imarc.

start-of-selection.
  CALL FUNCTION 'UPLOAD'
    EXPORTING
      FILENAME     = dsni
      FILEtype     = 'DAT'
    IMPORTING
      ACT_FILENAME = XFILENAME
    TABLES
      DATA_TAB     = itab.

  IF XFILENAME IS INITIAL.
    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
  ENDIF.
  IF NOT XFILENAME CS '.txt'.
    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
  ENDIF.


  select * from mbew into table imbewo where bwkey = 3000.
  select * from mbew into table imbewi where bwkey = 3001.
  select * from marc into table imarc where werks = 3001.

if pretest = 'X'.
  perform pretest2.
endif.

  loop at itab where matnr in smatnr.

    clear: xmatnr.
    xmatnr = itab-matnr.
    while xmatnr+17(1) = ' '.
      shift xmatnr right.
      xmatnr+0(1) = '0'.
    endwhile.

          clear: xcharg.
    if not itab-charg is initial.
      xcharg = itab-charg.
      while xcharg+9(1) = ' '.
        shift xcharg right.
        xcharg+0(1) = '0'.
      endwhile.
    endif.


    if pretest = 'X'.
      perform pretest.
    endif.

    perform insert.

  endloop.

  write: / count, 'Transaktionen'.



*---------------------------------------------------------------------*
*       FORM insert                                     *
*---------------------------------------------------------------------*
form insert.


    clear: zmm_trans_upload.
    zmm_trans_upload-mandt = sy-mandt.
    zmm_trans_upload-matnr = xmatnr.
    ZMM_TRANS_UPLOAD-werks = itab-werks.
    zmm_trans_upload-charg = xcharg.
    zmm_trans_upload-menge = itab-menge.
    zmm_trans_upload-meins = itab-meins.
    zmm_trans_upload-lqnum = itab-lqnum.
    select single * from lqua where lgnum = 391
                                and lqnum = itab-lqnum.

    if sy-subrc = 0 and lqua-lgtyp = 001
    and itab-lgpla = lqua-lgpla.

      zmm_trans_upload-lgpla = itab-lgpla.
       if ptest is initial.
      insert zmm_trans_upload.
        endif.

      count = count + 1.


    else.
      Write:/ itab-matnr, itab-charg, itab-lqnum, itab-lgpla, ' -wird nicht übernommen'.
    endif.

endform.                    "insert

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
enDFORM.                    "ABBRUCH


*&---------------------------------------------------------------------*
*&      Form  PRETEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRETEST .

    read table imbewi with key matnr = xmatnr
                               bwkey = 3001
                               bwtar = '          '.
    if sy-subrc ne 0.
      write:/ itab-matnr, ' - Fehlendes Bewertungssegment im Werk 3001'.
    endif.


    read table imarc with key matnr = xmatnr
                               werks = 3001.
    if sy-subrc ne 0.
      write:/ itab-matnr, ' - Fehlendes MARC Segment im Werk 3001'.
    endif.

    if sy-subrc = 0 and not imarc-prctr = '101PC02'.
      write:/ itab-matnr, ' - Falsches PC im Werk 3001', imarc-prctr, ' statt ', '101PC02'.
    endif.


ENDFORM.                    " PRETEST
*&---------------------------------------------------------------------*
*&      Form  PRETEST2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRETEST2 .


  Write:/ ' **** Bewertungssegmenet identisch ? ***'.

  loop at imbewi.
    read table imbewo with key matnr = imbewi-matnr
                               bwkey = 3000
                               bwtar = '          '.
    if not ( imbewi-vprsv = imbewo-vprsv )
        or not ( imbewi-stprs = imbewo-stprs )
        or not ( imbewi-peinh = imbewo-peinh )
        or not ( imbewi-bklas = imbewo-bklas ).
      write:/ imbewi-matnr, imbewi-bwkey, imbewi-vprsv, imbewi-stprs, imbewi-peinh, imbewi-bklas,
                            imbewo-bwkey, imbewo-vprsv, imbewo-stprs, imbewo-peinh, imbewo-bklas,
                            ' - Abweichende Bewertung !'.
    endif.
  endloop.
  Write:/' ******************************************'.
  skip 2.

ENDFORM.                    " PRETEST2
