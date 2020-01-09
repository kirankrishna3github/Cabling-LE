
REPORT Z_WM_LAGERPLATZ_UEB.

************************************************************************
*Lagerplätze DKS Lokallager im WM anlegen mittels Batch Input
*Lagerplätze aus PC2 / Lagernr. 392 ins 221 übernehmen
************************************************************************
*SAP Stäfa, P. Huber, 04.07.2011
************************************************************************

tables: lagp.

parameters: p_lgnum like lagp-lgnum default '221' obligatory,
            p_lgtyp like lagp-lgtyp default '001' obligatory,
            p_lgber like lagp-lgber default '001' obligatory,
            p_lptyp like lagp-lptyp.

parameters: dsni like rlgrap-filename lower case
                     default 'C:\Temp\Lagerplaetze_PC2_392.txt'
                     obligatory.
parameters: testlauf as checkbox default 'X'.

data: count type p,
      xfilename like rlgrap-filename,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

data: begin of itab occurs 0,
      lgpla like lagp-lgpla,
      end of itab.

data: w_error(1) type c.



INITIALIZATION.





start-of-selection.


DATA V_file1 TYPE string.
DATA V_path1 TYPE string.
DATA V_user_action1 TYPE I.
DATA V_file_table1 TYPE filetable.
DATA V_default_extension1 TYPE string.
DATA V_rc1 TYPE i.
DATA V_window_title1 TYPE string.
DATA V_file_filter1 TYPE string.
V_FILE1 = dsni.
SHIFT V_FILE1 RIGHT DELETING TRAILING '\'.
SHIFT V_FILE1 RIGHT DELETING TRAILING '/'.
SHIFT V_FILE1 LEFT DELETING LEADING SPACE .
V_DEFAULT_EXTENSION1 = 'DAT'.
CALL METHOD
  cl_gui_frontend_services=>file_open_dialog
  EXPORTING DEFAULT_EXTENSION  =
    V_DEFAULT_EXTENSION1
  DEFAULT_FILENAME  = V_FILE1
  INITIAL_DIRECTORY  = V_FILE1
  CHANGING FILE_TABLE = V_FILE_TABLE1
    RC = V_RC1
    USER_ACTION = V_USER_ACTION1.
CHECK V_USER_ACTION1 EQ 0.
CHECK V_RC1 GT 0.
READ TABLE V_file_table1 INDEX 1 INTO V_file1 .
XFILENAME = V_FILE1.
CALL METHOD cl_gui_frontend_services=>gui_upload
       EXPORTING
            FILENAME = V_FILE1
    FILETYPE = 'ASC'
  CHANGING
            DATA_TAB     = itab[]
  EXCEPTIONS
    OTHERS = 1.

  IF XFILENAME IS INITIAL.
    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
  ENDIF.
  IF NOT XFILENAME CS '.txt'.
    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
  ENDIF.

  if testlauf = ' '.
    perform open_group.
  endif.

  loop at itab.

  condense itab-lgpla no-gaps.
  check itab-lgpla ne space.

    write:/ itab-lgpla.


*Sonderzeichen / durch - ersetzen
if itab-lgpla ca '/'.
  replace '/' with '-' into itab-lgpla.
  write: 'Sonderzeichen / durch - ersetzt !'.
endif.

*Lagerplatz schon angelegt ?
    select single * from lagp where lgnum eq p_lgnum
                                and lgtyp eq p_lgtyp
                                and lgpla eq itab-lgpla.
    if sy-subrc eq 0.
      write: 'bereits angelegt !'.
    else.

*Verarbeiten ?
         if testlauf = ' '.
        perform transaktion_LS01.
      endif.
    endif. " select single * from lagp

endloop.

write: / count, 'Transaktionen'.

if testlauf = ' '.
  perform close_group.
endif.



*---------------------------------------------------------------------*
*       FORM transaktion_LS01                                         *
*---------------------------------------------------------------------*
form transaktion_LS01.

perform bdc_dynpro      using 'SAPML01S' '0100'.
perform bdc_field       using 'LAGP-LGNUM' p_lgnum.
perform bdc_field       using 'LAGP-LGTYP' p_lgtyp.
perform bdc_field       using 'LAGP-LGPLA' itab-lgpla.
perform bdc_dynpro      using 'SAPML01S' '0101'.
perform bdc_field       using 'BDC_OKCODE' '=BU'.
perform bdc_field       using 'LAGP-LGBER' p_lgber.
perform bdc_field       using 'LAGP-LPTYP' p_lptyp.
perform bdc_transaction using 'LS01'.

  count = count + 1.
endform.

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
            GROUP  = 'Lagerplatz'
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
