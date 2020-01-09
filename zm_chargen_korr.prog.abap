
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #115 #142
************************************************************************

REPORT ZMBSTUEB.

tables: mara, mch1.

select-options: smatnr for mara-matnr.

parameters: dsni like rlgrap-filename lower case
                     default 'C:\Packstueck.txt'
                     obligatory.
parameters: test as checkbox default 'X'.

data: fc1 type p,
      fc2 type p,
      fc3 type p,
      okc type p,
      xfilename like rlgrap-filename,
      xmatnr like mara-matnr,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
      wmatnr(18),
      wcharg(10).

data: begin of itab occurs 0,
         matnr(18),
         charg(10),
         zzexidv   like mch1-zzexidv,
         zzrollnr like mch1-zzrollnr,
         lgnum(3),
         lgtyp(3),
         lgpla(10),
         menge(1),
      end of itab.


start-of-selection.

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
V_FILE1 = dsni.                                  "smart: 2010-08-02 #142
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
XFILENAME = V_FILE1.                             "smart: 2010-08-02 #142
CALL METHOD cl_gui_frontend_services=>gui_upload "smart: 2010-08-02 #142
       EXPORTING
            FILENAME = V_FILE1                   "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
            DATA_TAB     = itab[]                "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142

  IF XFILENAME IS INITIAL.
    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
  ENDIF.
  IF NOT XFILENAME CS '.txt'.
    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
  ENDIF.


  loop at itab.
    clear: wmatnr.
    write itab-matnr to wmatnr.
    while wmatnr+17(1) eq ' '.
      shift wmatnr right IN CHARACTER MODE .     "smart: 2010-08-02 #115
      wmatnr+0(1) = '0'.
    endwhile.

    clear: wcharg.
    write itab-charg to wcharg.
    if itab-charg co '0123456789 '.
    while wcharg+9(1) eq ' '.
    shift wcharg right IN CHARACTER MODE .       "smart: 2010-08-02 #115
    wcharg+0(1) = '0'.
    endwhile.
    endif.

    select * from mch1 where matnr eq wmatnr
                         and charg eq wcharg
                         and matnr in smatnr.

      if sy-subrc = 0.
        if mch1-zzexidv is initial.
          write:/ itab-matnr, itab-charg, ' - wird geändert'.
          if test = ' '.
            mch1-zzexidv = itab-zzexidv.
            okc = okc + 1.
          endif.
        else.
          write:/ mch1-matnr, mch1-zzexidv, mch1-zzrollnr,
                  ' - Charge hat bereits die Packstücknr.',
                  mch1-zzexidv.
          fc1 = fc1 + 1.
        endif.

      else.
        write:/ itab-matnr, itab-charg, ' - nicht angelegt.'.
      endif.
    endselect.
  endloop.

  write:/ fc1, '- Charge hat bereits die Packstücknr.' .
  write:/ okc, '- korrekte Datensätze'.

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
