*&---------------------------------------------------------------------*
*& Report  ZLE_UPLOAD_EMONS
*&
*&---------------------------------------------------------------------*
*& Upload Relationen für Spediteur Emons
*  - Inputfile: Relationen mit Land, PLZ-VON, PLZ-BIS und Relation
*    im CSV-Format
*  - SAP-Outputtabelle: ZTREL_EMONS
*&---------------------------------------------------------------------*
*&
*& Änderungen:
*&
*&---------------------------------------------------------------------*


REPORT  ZLE_UPLOAD_EMONS message-id 00.

tables: ztrel_emons.

parameters: p_titel as checkbox default 'X'.
parameters: p_fname like rlgrap-filename default 'C:\Temp\XXX.csv'.

types: begin of t_input,
         land1 like ztrel_emons-land1,
         plz_from like ztrel_emons-plz_from,
         plz_to like ztrel_emons-plz_to,
         relbez like ztrel_emons-relbez,
       end of t_input.

* Inputtabelle des Upload-Files
* Inputtabelle des Upload-Files
types: t_upload type c length 2000.

data: gs_upload type t_upload.
data: gt_upload type standard table of t_upload.

data: gs_in type t_input.
data: gt_in type standard table of t_input.

data: gs_rel_emons type ztrel_emons.

data: gv_answer type c.
data: gv_subrc type sy-subrc.
data: gv_loop_start type i.

data: gv_anz_rel_del type i.
data: gv_anz_rel_sel type i.
data: gv_anz_rel_ins type i.
data: gv_anz_rel_ins_err type i.



DATA: BEGIN OF gs_maske,
        ',',
        f01(30),
        ',',
        '*.*',
        ',',
        f02(30),
        ',',
        '*.*',
        '.',
      END OF gs_maske.

constants: gc_delim type c value ';'.
constants: gc_on type c value ';'.

*----------------------------------------------------------------------*
initialization.
*----------------------------------------------------------------------*

  perform test_berechtigung.


*----------------------------------------------------------------------*
*   at selection screen: Inputfile Mistral-Postleitzahl                                               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = gs_maske
    IMPORTING
      filename         = p_fname
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

* Bei Update: Weiterverarbeitung bestätigen
  perform popup_to_confirm using text-p01
                                 ' '
                        changing gv_answer.

  if gv_answer ne '1'. "Nein-Fall
    message s025(ab). "Meldung 'Verarbeitung abgebrochen'
    leave list-processing.
  endif.

*----------------------------------------------------------------------*
* Upload Inputfile
*----------------------------------------------------------------------*
  perform ws_upload changing gv_subrc.

*  Fehler beim Upload
  IF gv_subrc ne 0.
    message s398 with 'Error on open' p_fname.
    leave list-processing.
  endif.

* Anzahl zu löschende Daten ermitteln
  select count(*) from ztrel_emons into gv_anz_rel_del.

* alte Daten vorgängig aus SAP-Tabelle löschen
  delete from ztrel_emons.

  if p_titel eq gc_on.
    gv_loop_start = 2.
  else.
    gv_loop_start = 1.
  endif.

* Eingabe verarbeiten
  loop at gt_in from gv_loop_start into gs_in.

    gv_anz_rel_sel = gv_anz_rel_sel + 1.

*   Verarbeitung
    perform processing_record.

  endloop.  "gt_in

*----------------------------------------------------------------------*
* Ende Verarbeitung der Daten für Relationen
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*

  perform nachlauf.

* Test Programmberechtigung
  include zincl_progber.

*&---------------------------------------------------------------------*
*&      Form  WS_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WS_UPLOAD CHANGING pv_subrc TYPE sy-subrc.

  DATA LV_LEN TYPE I.
  DATA LV_FILENAME1 TYPE STRING.
  DATA LV_FILETYPE1 TYPE C LENGTH 10.
  DATA LV_MATNR TYPE MATNR.

  CLEAR gt_upload.

  LV_FILENAME1 = P_FNAME.
  LV_FILETYPE1 = 'DAT'.
*  L_FILETYPE1 = 'ASC'.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
         EXPORTING
*         CODEPAGE            = ' '
           FILENAME           = LV_FILENAME1
           FILETYPE           = LV_FILETYPE1
           HAS_FIELD_SEPARATOR = 'X'  "TAB getrennt
*         HEADLEN             = ' '
*         LINE_EXIT           = ' '
*         TRUNCLEN            = ' '
*         USER_FORM           = ' '
*         USER_PROG           = ' '
         IMPORTING
           FILELENGTH          = LV_LEN
         CHANGING
            DATA_TAB           = gt_upload[]
         EXCEPTIONS
            BAD_DATA_FORMAT    = 1
            FILE_OPEN_ERROR    = 2
            FILE_READ_ERROR    = 3
            INVALID_TYPE       = 5
            NO_BATCH           = 6
            UNKNOWN_ERROR      = 7
            OTHERS             = 8.

  pv_subrc = sy-subrc.

  check sy-subrc eq 0.

* Inputtabelle aufbereiten
  clear gt_in.

  loop at gt_upload into gs_upload.

    split gs_upload at gc_delim into gs_in-land1
                                     gs_in-plz_from
                                     gs_in-plz_to
                                     gs_in-relbez.

    append gs_in to gt_in.

  endloop.

ENDFORM.                    " WS_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_0245   text
*      <--P_H_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING pv_text TYPE CLIKE
                            pv_cancel TYPE CLIKE
                   CHANGING pv_answer LIKE gv_answer.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
       TEXT_QUESTION               = pv_text
       TEXT_BUTTON_1               = text-yes
       TEXT_BUTTON_2               = text-no1
       DEFAULT_BUTTON              = '2'
       DISPLAY_CANCEL_BUTTON       = pv_cancel
     IMPORTING
        ANSWER                     = pv_answer
     EXCEPTIONS
         OTHERS                    = 2.

ENDFORM.                    " popup_to_confirm

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PLZ  text
*----------------------------------------------------------------------*
FORM UPLOAD_FILE USING P_FNAME TYPE RLGRAP-FILENAME.

  DATA LV_LEN TYPE I.
  DATA LV_FILENAME1 TYPE STRING.
  DATA LV_FILETYPE1 TYPE C LENGTH 10.

  CLEAR gt_upload.

  LV_FILENAME1 = P_FNAME.
  LV_FILETYPE1 = 'DAT'.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
         EXPORTING
*         CODEPAGE            = ' '
           FILENAME           = LV_FILENAME1
           FILETYPE           = LV_FILETYPE1
*         HEADLEN             = ' '
*         LINE_EXIT           = ' '
*         TRUNCLEN            = ' '
*         USER_FORM           = ' '
*         USER_PROG           = ' '
         IMPORTING
           FILELENGTH          = LV_LEN
         CHANGING
            DATA_TAB           = GT_UPLOAD[]
         EXCEPTIONS
            BAD_DATA_FORMAT    = 1
            FILE_OPEN_ERROR    = 2
            FILE_READ_ERROR    = 3
            INVALID_TYPE       = 5
            NO_BATCH           = 6
            UNKNOWN_ERROR      = 7
            OTHERS             = 8.

  IF SY-SUBRC <> 0.
    MESSAGE E398 WITH 'error on open' P_FNAME.
  ENDIF.

ENDFORM.                               " UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  PROCESSING_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSING_RECORD .

  clear gs_rel_emons.

  gs_rel_emons-land1 = gs_in-land1.

  gs_rel_emons-plz_from = gs_in-plz_from.
  condense gs_in-plz_from no-gaps.

  gs_rel_emons-plz_to = gs_in-plz_to.
  condense gs_in-plz_to no-gaps.

  gs_rel_emons-relbez = gs_in-relbez.

* Element in Tabelle ZTREL_EMONS einfügen
  insert ztrel_emons from gs_rel_emons.

  if sy-subrc eq 0.
    gv_anz_rel_ins =  gv_anz_rel_ins + 1.
  else.
    gv_anz_rel_ins_err =  gv_anz_rel_ins_err + 1.
  endif.

ENDFORM.                    " PROCESSING_RECORD


*&---------------------------------------------------------------------*
*&      Form  nachlauf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nachlauf.

  write: / text-p20,  sy-sysid, sy-mandt, sy-datum, sy-uzeit.
  skip 1.

  write: / gv_anz_rel_del, text-p22.
  write: / gv_anz_rel_sel, text-p21.
  write: / gv_anz_rel_ins, text-p23.
  write: / gv_anz_rel_ins_err, text-p24.

ENDFORM.                    " nachlauf
