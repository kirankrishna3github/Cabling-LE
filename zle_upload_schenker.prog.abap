*---------------------------------------------------------------------*
* Report  ZLE_UPLOAD_SCHENKER
*
*---------------------------------------------------------------------*
*     Programm-Name....: ZLE_UPLOAD_SCHENKER
*     Entwickler.......: Markus Raffeiner
*     Erstell-Datum....: 15.11.2010
*     Version..........: 1.0
*     Zweck............: Upload Schenker-Tabellen
*                        - Inputfile: nlplz.txt (Postleitzahlen)
*                          SAP-Outputabelle: ZTPLZ_SCHENKER
*                        - Inputfile: nlplzrel.txt (Routingwerte)
*                          SAP-Outputabelle: ZTROUT_SCHENKER
*                        - Inputfile: nlplzrelHUB.txt (MISTRAl-HUB-Kettenwerte)
*                          SAP-Outputabelle: ZTMIHU_SCHENKER
*
*
*     Bemerkung........: Die Inputfiles werden von Schenker periodisch
*                        via Mail gesendet
*     Aenderungen:
*     TTMMJJJJ, <Vorname, Name>
*               <Aenderung>
*
*---------------------------------------------------------------------*

REPORT  ZLE_UPLOAD_SCHENKER message-id 00.

tables: ztmihu_schenker, ztplz_schenker, ztrout_schenker.

parameters: p_plz like rlgrap-filename default 'C:\Temp\nlplz.txt'.
parameters: p_rout like rlgrap-filename default 'C:\Temp\nlplzrel.txt'.
parameters: p_mihu like rlgrap-filename default 'C:\Temp\nlplzrelHUB.txt'.


types: t_irec(20).

* Inputtabelle der Upload-Files
data: itab type table of t_irec.

* Inputstruktur MISTRAL-PLZ-Datei
data: begin of wa_plz.
        include structure ztplz_schenker.
data:  end of wa_plz.

* Inputstruktur Routing-Datei
data: begin of wa_ro.
        include structure ztrout_schenker.
data:  end of wa_ro.

* Inputstruktur MISTRAL-HUB-Kettendatei
data: begin of wa_mihu.
        include structure ztmihu_schenker.
data:  end of wa_mihu.

constants: c_delim value ';'.

DATA: BEGIN OF maske,
        ',',
        f01(30),
        ',',
        '*.txt',
        ',',
        f02(30),
        ',',
        '*.*',
        '.',
      END OF maske.


data: begin of h,
        answer,
        anz_rec_plz_del type i,
        anz_rec_plz_ins type i,
        anz_rec_rout_del type i,
        anz_rec_rout_ins type i,
        anz_rec_mihu_del type i,
        anz_rec_mihu_ins type i,
      end of h.

*----------------------------------------------------------------------*
initialization.

  perform test_berechtigung.


*----------------------------------------------------------------------*
*   at selection screen: Inputfile Mistral-Postleitzahl                                               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_plz.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\TEMP\'
      mask             = maske
    IMPORTING
      filename         = p_plz
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

*----------------------------------------------------------------------*
*   at selection screen: Inputfile Routing                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rout.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\TEMP\'
      mask             = maske
    IMPORTING
      filename         = p_rout
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.


*----------------------------------------------------------------------*
*   at selection screen: Inputfile Mistral-HUB                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_mihu.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\TEMP\'
      mask             = maske
    IMPORTING
      filename         = p_mihu
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
                        changing h-answer.

  if h-answer ne '1'. "Nein-Fall
    message s025(ab). "Meldung 'Verarbeitung abgebrochen'
    exit.
  endif.



*----------------------------------------------------------------------*
* Beginn Verarbeitung der MISTRAL-PLZ Daten
*----------------------------------------------------------------------*
* Upload Input File "MISTRAL-PLZ"
  perform upload_file using p_plz.

* Inputdaten in SAP-Tabelle einfügen
  perform insert_plz_in_db.

* Fehlerfall, Inputzähler für Protokoll sichern

*----------------------------------------------------------------------*
* Ende Verarbeitung der MISTRAL-PLZ Daten
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Beginn Verarbeitung der Routing Daten
*----------------------------------------------------------------------*
* Upload Input File "MISTRAL-PLZ"
  perform upload_file using p_rout.

* Inputdaten in SAP-Tabelle einfügen
  perform insert_routing_in_db.

* Fehlerfall, Inputzähler für Protokoll sichern

*----------------------------------------------------------------------*
* Ende Verarbeitung der Routing Daten
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Beginn Verarbeitung der MISTRAL-HUB Daten
*----------------------------------------------------------------------*
* Upload Input File "MISTRAL-PLZ"
  perform upload_file using p_mihu.

* Inputdaten in SAP-Tabelle einfügen
  perform insert_mistral_hub_in_db.

* Fehlerfall, Inputzähler für Protokoll sichern

*----------------------------------------------------------------------*
* Ende Verarbeitung der MISTRAL-HUB Daten
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
END-OF-SELECTION.

  perform nachlauf.

* Test Programmberechtigung
  include zincl_progber.


*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_0245   text
*      <--P_H_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING p_text TYPE CLIKE
                            p_cancel TYPE CLIKE
                   CHANGING P_answer LIKE h-answer.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
       TITLEBAR                    = text-t01
       TEXT_QUESTION               = text-t02
       TEXT_BUTTON_1               = text-yes
       TEXT_BUTTON_2               = text-no1
       DEFAULT_BUTTON              = '2'
       DISPLAY_CANCEL_BUTTON       =  p_cancel
     IMPORTING
        ANSWER                     = p_answer
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

  DATA LEN TYPE I.
  DATA L_FILENAME1 TYPE STRING.
  DATA L_FILETYPE1 TYPE C LENGTH 10.

  REFRESH itab.
  L_FILENAME1 = P_FNAME.
  L_FILETYPE1 = 'DAT'.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
         EXPORTING
*         CODEPAGE            = ' '
           FILENAME           = L_FILENAME1
           FILETYPE           = L_FILETYPE1
*         HEADLEN             = ' '
*         LINE_EXIT           = ' '
*         TRUNCLEN            = ' '
*         USER_FORM           = ' '
*         USER_PROG           = ' '
         IMPORTING
           FILELENGTH          = LEN
         CHANGING
            DATA_TAB           = ITAB[]
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
*&      Form  INSERT_PLZ_IN_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_PLZ_IN_DB.

* Anzahl zu löschende Daten ermitteln
  select count(*) from ztplz_schenker into h-anz_rec_plz_del.

* alte Daten vorgängig aus SAP-Tabelle löschen
  delete from ztplz_schenker.

  loop at itab into wa_plz.

*   Record in SAP-Tabelle einfügen.
    insert ztplz_schenker from wa_plz.

    h-anz_rec_plz_ins = h-anz_rec_plz_ins + 1.

  endloop.

ENDFORM.                    " INSERT_PLZ_IN_DB



*&---------------------------------------------------------------------*
*&      Form  INSERT_ROUTING_IN_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_ROUTING_IN_DB.

* Anzahl zu löschende Daten ermitteln
  select count(*) from ztrout_schenker into h-anz_rec_rout_del.

* alte Daten vorgängig aus SAP-Tabelle löschen
  delete from ztrout_schenker.

  loop at itab into wa_ro.

*   Record in SAP-Tabelle einfügen.
    insert ztrout_schenker from wa_ro.

    h-anz_rec_rout_ins = h-anz_rec_rout_ins + 1.

  endloop.


ENDFORM.                    " INSERT_PLZ_IN_DB


*&---------------------------------------------------------------------*
*&      Form  INSERT_MISTRAL_HUB_IN_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_MISTRAL_HUB_IN_DB.

  data: l_wa(20).

* Anzahl zu löschende Daten ermitteln
  select count(*) from ztmihu_schenker into h-anz_rec_mihu_del.

* alte Daten vorgängig aus SAP-Tabelle löschen
  delete from ztmihu_schenker.

  loop at itab into l_wa.

    split l_wa at c_delim into: wa_mihu-zzgestl wa_mihu-zzkettnr wa_mihu-zzreghu.

*   Record in SAP-Tabelle einfügen.
    insert ztmihu_schenker from wa_mihu.

    h-anz_rec_mihu_ins = h-anz_rec_mihu_ins + 1.


  endloop.

ENDFORM.                    " INSERT_MISTRAL_HUB_IN_DB




*&---------------------------------------------------------------------*
*&      Form  nachlauf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nachlauf.

  skip 2.

  write: / 'Ausgabestatistik', sy-datum, sy-uzeit, sy-mandt, sy-sysid.

  write: / 'Anzahl Records in ZTPLZ_SCHENKER delete:     ', h-anz_rec_plz_del.
  write: / 'Anzahl Records in ZTPLZ_SCHENKER insert:     ', h-anz_rec_plz_ins.

  write: / 'Anzahl Records in ZTROUT_SCHENKER delete:     ', h-anz_rec_rout_del.
  write: / 'Anzahl Records in ZTROUT_SCHENKER insert:     ', h-anz_rec_rout_ins.

  write: / 'Anzahl Records in ZTMIHU_SCHENKER delete:     ', h-anz_rec_mihu_del.
  write: / 'Anzahl Records in ZTMIHU_SCHENKER insert:     ', h-anz_rec_mihu_ins.


ENDFORM.                    " nachlauf
