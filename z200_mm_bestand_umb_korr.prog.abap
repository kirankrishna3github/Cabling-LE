
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #115 #142
************************************************************************

report: Z200_MM_BESTAND_IN_01.



*Tabellen:
tables: mara, marc, mard, mbew, mlgn.  "zum Materialstamm
tables: lagp, ltap, apqi, ausp, inob, lqua.
tables: bmseg.

************************************************************************
*Felder
data: dataset(70).
data: count type p,
      xfilenew like rlgrap-filename,
      xmess(100),
      count_1 type p,
      xmatnr like mara-matnr,
      xmenge(13) type c,

      wmatnr(8),
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
      wgroup like APQI-GROUPID,
      g_objek LIKE ausp-objek.

*Bestände auf Plätzen
data: begin of inew occurs 0,
      MATNR(8),
      MENGE(13),
      CHARG(10),
      LGPLAOLD(10),
      LENUMOLD(20),
      LGPLANEW(10),
      LENUMNEW(20),
end of inew.


************************************************************************
*Benutzeroberfläche
parameters: pout radiobutton group 1,
            pin  radiobutton group 1.

parameters: dsninew like rlgrap-filename lower case
               default 'C:\UMBUCHUNG.txt' obligatory.
parameter: pwerks like mard-werks obligatory default '8000',
           plgnum like mlgn-lgnum obligatory default '800',
           plgtyp like ltap-nltyp obligatory default '001'.

select-options: smatnr for lqua-matnr obligatory.


parameters: p_datum like sy-datum default sy-datum.
selection-screen skip 1.
parameters: test as checkbox default 'X'.


INITIALIZATION.

************************************************************************
*Verarbeitung
start-of-selection.

  if test = ' '.
    perform open_group.
  endif.


  if pout = 'X'.
    select * from lqua where werks = pwerks
                         and lgnum = '800'
                         and lgtyp = '001'
                         and matnr in smatnr.

        xmatnr = lqua-matnr.
        if xmatnr co '0123456789 '.
          while xmatnr+0(1) = '0'.
            xmatnr+0(1) = ' '.
            shift xmatnr left IN CHARACTER MODE ."smart: 2010-08-02 #115
          endwhile.
        endif.

write lqua-verme to xmenge.

     write:/ xmatnr, xmenge, lqua-charg, lqua-lgpla, lqua-lenum.
        if test = ' '.
          perform verarbeiten_out.
          write: '- ausbuchen'.
        else.
          write: '- testlauf'.
        endif.
      endselect.
    endif.


    if pin = 'X'.
*Bestandestabelle mit Lagerplätzen) hochladen

*$smart (F) 2010-08-02 - #142 Der nicht Unicode konforme GUI
*$smart (F) 2010-08-02 - #142 Upload/Download Funktionsbaustein wurde
*$smart (F) 2010-08-02 - #142 mit vorgeschlagenem kompatiblen
*$smart (F) 2010-08-02 - #142 Funktionsbaustein ersetzt (A)

DATA V_file1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_path1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_user_action1 TYPE I.                      "smart: 2010-08-02 #142
DATA V_file_table1 TYPE filetable.               "smart: 2010-08-02 #142
DATA V_default_extension1 TYPE string.           "smart: 2010-08-02 #142
DATA V_rc1 TYPE i.                               "smart: 2010-08-02 #142
DATA V_window_title1 TYPE string.                "smart: 2010-08-02 #142
DATA V_file_filter1 TYPE string.                 "smart: 2010-08-02 #142
V_FILE1 = dsninew.                               "smart: 2010-08-02 #142
SHIFT V_FILE1 RIGHT DELETING TRAILING '\'.       "smart: 2010-08-02 #142
SHIFT V_FILE1 RIGHT DELETING TRAILING '/'.       "smart: 2010-08-02 #142
SHIFT V_FILE1 LEFT DELETING LEADING SPACE .      "smart: 2010-08-02 #142
V_DEFAULT_EXTENSION1 = 'DAT'.                    "smart: 2010-08-02 #142
CALL METHOD                                      "smart: 2010-08-02 #142
  cl_gui_frontend_services=>file_open_dialog     "smart: 2010-08-02 #142
  EXPORTING DEFAULT_EXTENSION  =                 "smart: 2010-08-02 #142
    V_DEFAULT_EXTENSION1                         "smart: 2010-08-02 #142
  DEFAULT_FILENAME  = V_FILE1                    "smart: 2010-08-02 #142
  INITIAL_DIRECTORY  = V_FILE1                   "smart: 2010-08-02 #142
  CHANGING FILE_TABLE = V_FILE_TABLE1            "smart: 2010-08-02 #142
    RC = V_RC1                                   "smart: 2010-08-02 #142
    USER_ACTION = V_USER_ACTION1.                "smart: 2010-08-02 #142
CHECK V_USER_ACTION1 EQ 0.                       "smart: 2010-08-02 #142
CHECK V_RC1 GT 0.                                "smart: 2010-08-02 #142
READ TABLE V_file_table1 INDEX 1 INTO V_file1 .  "smart: 2010-08-02 #142
XFILENEW = V_FILE1.                              "smart: 2010-08-02 #142
CALL METHOD cl_gui_frontend_services=>gui_upload "smart: 2010-08-02 #142
           EXPORTING
                FILENAME = V_FILE1               "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
                DATA_TAB     = inew[]            "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142

      IF XFILENEW IS INITIAL.
        PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
      ENDIF.
      IF NOT XFILENEW CS '.txt'.
        PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
      ENDIF.


      clear: count.
      loop at inew.
*        xmatnr = inew-matnr.
*        if xmatnr co '0123456789 '.
*          while xmatnr+0(1) = '0'.
*            xmatnr+0(1) = ' '.
*            shift xmatnr left.
*          endwhile.
*        endif.

        write:/ inew-matnr, inew-menge, inew-charg, inew-lgplaold,
  inew-lenumold, inew-lgplanew, inew-lenumnew.


        if test = ' '.
          perform verarbeiten_in.
        endif.
      endloop.
    endif.

    if test = ' '.
      perform close_group.
    endif.
*
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
*&---------------------------------------------------------------------*
*&      Form  wert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM wert.
*
*
*  read table imbew with key matnr = wmatnr
*                            bwkey = '7000'.
*
*  if imbew-vprsv = 'V'.
*    wwdiff = wmdiff * ( imbew-verpr / imbew-peinh ).
*  elseif imbew-vprsv = 'S'.
*    wwdiff = wmdiff * ( imbew-stprs / imbew-peinh ).
*  endif.
*
*  wwsum = wwsum + wwdiff.
*ENDFORM.                    " wert
*&---------------------------------------------------------------------*
*&      Form  verarbeiten_WM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verarbeiten_in.
  perform bdc_dynpro      using 'SAPML03T' '0101'.
  perform bdc_field       using 'LTAK-LGNUM' '800'.
  perform bdc_field       using 'LTAK-BWLVS' '999'.    "interne Bew.art
  perform bdc_field       using 'LTAP-MATNR' inew-matnr.
  perform bdc_field       using 'LTAP-CHARG' inew-charg.
  perform bdc_field       using 'RL03T-ANFME' inew-menge.
  perform bdc_field       using 'LTAP-WERKS' '8000'.
  perform bdc_field       using 'LTAP-LGORT' '8000'..
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform bdc_field       using 'LTAP-VLTYP' '998'.
  perform bdc_field       using 'LTAP-LETYP' '001'..
  perform bdc_field       using 'LTAP-VLPLA' 'AUFNAHME'.
  perform bdc_field       using 'LTAP-NLTYP' '001'.
  perform bdc_field       using 'LTAP-NLPLA' inew-lgplanew.
  perform bdc_field       using 'LTAP-NLENR' inew-lenumnew.
  perform bdc_transaction using 'LT01'.
ENDFORM.                    "

*---------------------------------------------------------------------*
*&      Form  verarbeiten_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verarbeiten_out.

  perform bdc_dynpro      using 'SAPML03T' '0101'.
  perform bdc_field       using 'LTAK-LGNUM' '800'.
  perform bdc_field       using 'LTAK-BWLVS' '999'.     "interne Bew.art
  perform bdc_field       using 'LTAP-MATNR' xmatnr.
  perform bdc_field       using 'RL03T-ANFME' xmenge..
  perform bdc_field       using 'LTAP-WERKS' lqua-werks.
  perform bdc_field       using 'LTAP-LGORT' lqua-lgort.
  perform bdc_field       using 'LTAP-CHARG' lqua-charg.
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform bdc_field       using 'LTAP-VLTYP' lqua-lgtyp.
  perform bdc_field       using 'LTAP-LETYP' lqua-letyp.
  perform bdc_field       using 'LTAP-VLPLA' lqua-lgpla.
  perform bdc_field       using 'LTAP-NLTYP' '998'.
  perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
  perform bdc_transaction using 'LT01'.
ENDFORM.                    " verarbeiten_WM


*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  wgroup = 'WM_Bestand'.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = wgroup
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
  if test = ' '.
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
