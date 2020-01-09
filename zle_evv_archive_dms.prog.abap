*&---------------------------------------------------------------------*
*& Report  ZLE_EVV_ARCHIVE_DMS
*&
*&---------------------------------------------------------------------*
*& Autor: Markus Raffeiner
*& Erstellt am: 03.10.2014
*&---------------------------------------------------------------------*
*&
*& Mit diesem Programm können fehlende EVV's ins DMS als Originale
*& in bestehende Dokumente übernommen werden. Die Dokumente können mit
*& der Transaktion CV03N kontrolliert werden.
*&
*& (Siehe Report TEST_API_CHECKIN_KPRO)
*&
*&---------------------------------------------------------------------*
*& Änderungen
*&
*&---------------------------------------------------------------------*

REPORT  ZLE_EVV_ARCHIVE_DMS.

tables: draw.

SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE text-f10.
parameters: p_dokar type dokar default 'ZED'.
select-options: s_doktl for draw-doktl obligatory.
select-options s_doknr for draw-doknr.
parameters: p_storca type CV_STORAGE_CAT default 'ZDMS' obligatory.
select-options s_dappl for draw-dappl.
SELECTION-SCREEN END OF BLOCK b10.


SELECTION-SCREEN BEGIN OF BLOCK b20 WITH FRAME TITLE text-f20.

* Workstation
parameters: p_pathws LIKE rlgrap-filename default 'C:\ECD\evv\'.
parameters: p_sim as checkbox default 'X'.


SELECTION-SCREEN END OF BLOCK b20.

*----------------------------------------------------------------------*
*       CLASS lcx_scan_exceptions DEFINITION
*----------------------------------------------------------------------*
*       Exceptions for source scanning
*----------------------------------------------------------------------*
CLASS lcx_scan_exceptions DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.                    "lcx_scan_exceptions DEFINITION


* Nur Belege, welche noch keine Archivbelege haben, dürfen archiviert werden
* Umgang mit mehreren Dokumentversionen pro Beleg
*  - Beispiel: 0000102089_EVV siehe Tabelle DRAW


* evtl. Logik aus FB Z_WF_WRITE_ALINK_TO_WINDOWS kopieren

* Globale Definitionen
data: gv_subrc type sysubrc.
data: gv_anz_files type i.


data: gv_path like sapb-sappfad.
data: gv_dir_in like sapb-sappfad.
data: gv_dir_file_in like sapb-sappfad.
data: gv_file_size type i.
data: gv_mask(10) type c value '*'.
data: gv_doknr type doknr.
data: gv_beme type c length 40.

types: begin of t_draw,
         dokar type dokar,
         doknr type doknr,
         dokvr type dokvr,
         doktl type doktl_d,
         dwnam type dwnam,
         dokst type dokst,
       end of t_draw.

data: gs_draw type t_draw.
data: gt_draw type standard table of t_draw.

types: begin of t_flist,
        dir type string,
        filename type string,
        filename_no_ext type string,  "Filename ohne Extension
       end of t_flist.

data: gt_file_list type standard table of sdokpath.
data: gs_file_list type sdokpath.

data: gt_file_tab type standard table of sdokpath.
data: gs_file_tab type sdokpath.
data: gv_file_count type i.

data: gv_filename_dms type string.
data: gv_doktl type doktl_d.
data: gv_dappl type dappl.

data: gv_process type c.

* Protokoll
types: begin of t_prot,
         dokar type dokar,
         doknr type doknr,
         dokvr type dokvr,
         doktl type doktl_d,
         dwnam type dwnam,
         dokst type dokst,
         pathname type c length 100,
         filename_dms type string,
         beme like gv_beme,
         subrc type sysubrc,  "0 = Dokument eingefügt
       end of t_prot.

data: gs_prot type t_prot.
data: gt_prot type standard table of t_prot.

* Struktur für Fileroutinen C_XXX
data: begin of gs_file,
        name(75)    TYPE C,
        type(10)    TYPE C,
        len(8)      TYPE P,
        owner(8)    TYPE C,
        mtime(6)    TYPE P,
        mode(9)     TYPE C,
        errno(3)    TYPE C,
        errmsg(40)  TYPE C,
      end of gs_file.

***data: gt_BINARCHIVOBJECT type TBL1024 occurs 0 with HEADER LINE.
***data: gt_ARCHIVOBJECT type DOCS occurs 0 with HEADER LINE.

data: gv_answer type c.

* Konstanten
constants: gc_on type c value 'X'.
constants: gc_off type c value space.
constants: gc_request type doktl value 'REQ'.
constants: gc_response type doktl value 'RSP'.
constants: gc_cancel_button type c value 'X'.
constants: gc_popup_yes type c value '1'.

INITIALIZATION.

* Test Programmberechtigung
  PERFORM test_berechtigung.

  PERFORM initialisierung.


*----------------------------------------------------------------------*
*   Programmstart
*----------------------------------------------------------------------*
start-of-selection.

* Bei Simulationslauf muss keine Popup-Bestätigung verlangt werden
  if p_sim eq gc_on.
    gv_answer = gc_popup_yes.
  else.
    perform popup_to_confirm using 'Achtung, wollen Sie die Dokumente ins DMS übernehmen?'(040)
                                    gc_cancel_button "Cancel-button auch anzeigen
                             changing gv_answer.

  endif.

* Verarbeitung erwünscht
  if gv_answer eq gc_popup_yes.
    perform processing.
  endif.

*----------------------------------------------------------------------*
*   End der Selection
*----------------------------------------------------------------------*
end-of-selection.

  perform ausgabe_protokoll.

* Test Programmberechtigung
  INCLUDE zincl_progber.


*&---------------------------------------------------------------------*
*&      Form  INITIALISIERUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALISIERUNG .

  refresh s_doktl.
  move 'I' to s_doktl-sign.
  move 'EQ' to s_doktl-option.
  move gc_request to s_doktl-low.
  append s_doktl.

  move gc_response to s_doktl-low.
  append s_doktl.

ENDFORM.                    " INITIALISIERUNG


*----------------------------------------------------------------------*
*      -->P_TEXT     text
*      -->P_CANCEL   text
*      -->P_ANSWER   text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING    p_question TYPE CLIKE
                               p_cancel TYPE CLIKE
                      CHANGING p_answer type c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = p_question
      TEXT_BUTTON_1         = 'Ja'(020)
      TEXT_BUTTON_2         = 'Nein'(021)
      DEFAULT_BUTTON        = '2'
      DISPLAY_CANCEL_BUTTON = p_cancel
    IMPORTING
      ANSWER                = p_answer
    EXCEPTIONS
      OTHERS                = 2.

ENDFORM.                    " popup_to_confirm


*&---------------------------------------------------------------------*
*&      Form  PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSING .

* Vorlauf
  perform vorlauf.

* -------------------------------------------------------------------
* Dokumenttyp (z.B. PDF) zum Archivobjet holen
* -------------------------------------------------------------------
*perform get_doc_type.

* -------------------------------------------------------------------
* zu verarbeitende Files holen
* -------------------------------------------------------------------
  perform get_directory_list_from_ws.

* Bildschirm-Selektionen berücksichtigen, Files entfernen, z.B.
* Signaturen
  perform selection_file_list.

* Files verarbeiten
  loop at gt_file_list into gs_file_list.

* Inputpfad und Filename zusammensetzen
    concatenate gv_path gs_file_list-pathname into gv_dir_file_in.

    clear gv_beme.

    perform get_docnumber using gs_file_list-pathname
                       changing gv_doknr.

    if gv_doknr is initial.
      move  'Ungültiges File' to gv_beme.
      perform protokoll.
*      write: / 'Ungültiges File', gs_file_list.
      continue.
    endif.

*   Filename, Teildokument (REQ, RSQ) und Applikation (XMl, HTM)
*   für das DMS ermitteln
    perform set_data_for_dms changing gv_doktl
                                      gv_filename_dms
                                      gv_dappl.

    if gv_filename_dms is initial.
      move  'Ungültiges File' to gv_beme.
      perform protokoll.
      continue.
    endif.

    check gv_dappl in s_dappl.

*   Vorhandene Dokumente ermitteln
    perform get_data_draw.

    loop at gt_draw into gs_draw.

*     Test ob Dokument ohne PDF- oder XML-Dokumente vorhanden ist
      perform test_documents_exists changing gv_beme
                                             gv_subrc.

      perform protokoll.

      check gv_subrc eq 0.

*     Nur Simulationslauf
      if p_sim eq gc_on.
        continue.
      endif.

*     Dokument ins DMS einchecken
      perform document_checkin_into_dms changing gv_subrc.

      if gv_subrc ne 0.
        message e021(zdfi_wf) with gs_file-errmsg.
      endif.

    endloop.  "gt_draw

* Protokollausgabe auf dem Bildschirm
**  perform protokoll.

  endloop.  "gt_file_list

* Anzahl verarbeitete Files
  describe table gt_file_list lines gv_anz_files.
  if gv_anz_files gt 0.
    write: / 'Total Input-Files processed:'(P03), gv_anz_files.
  endif.

ENDFORM.                    " PROCESSING


*&---------------------------------------------------------------------*
*&      Form  VORLAUF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VORLAUF .

  gv_path = p_pathws.

ENDFORM.                    " VORLAUF


*&---------------------------------------------------------------------*
*&      Form  GET_DIRECTORY_INPUT_OUPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*----------------------------------------------------------------------*
FORM GET_DIRECTORY_INPUT_OUTPUT.

  data: lv_archiv_id type saearchivi.

  clear: gv_path, gv_dir_in.

  select single evvpath from /rz1/edecparam
    into gv_path
    where sysid = sy-sysid.

* Ungültiger Pfad
  if gv_path is initial.
    message e002(aq) with text-f01.
  endif.


ENDFORM.                    " GET_DIRECTORY_INPUT_OUTPUT


*&---------------------------------------------------------------------*
*&      Form  GET_DIRECTORY_LIST_FROM_WS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*----------------------------------------------------------------------*
FORM GET_DIRECTORY_LIST_FROM_WS.

  data: lv_pathws type string.

  lv_pathws = p_pathws.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES
    EXPORTING
      DIRECTORY                   = lv_pathws
    CHANGING
***      FILE_TABLE                  = GT_FILE_TAB
      FILE_TABLE                  = GT_FILE_LIST
      COUNT                       = GV_FILE_COUNT
    EXCEPTIONS
      CNTL_ERROR                  = 1
      DIRECTORY_LIST_FILES_FAILED = 2
      WRONG_PARAMETER             = 3
      ERROR_NO_GUI                = 4
      OTHERS                      = 5.

ENDFORM.                    " GET_DIRECTORY_LIST_FROM_WS


*&---------------------------------------------------------------------*
*&      Form  SET_DATA_FOR_DMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_FILENAME_DMS  text
*----------------------------------------------------------------------*
FORM SET_DATA_FOR_DMS  CHANGING  P_DOKTL type doktl_d
                                 P_FILENAME_DMS type string
                                 P_DAPPL type dappl.

  data: lv_file_ext type c length 4.
  data: lt_results TYPE match_result_tab.
  data: ls_results like line of lt_results.
  data: lv_offset type i.
  data: lv_length type i.
  data: lv_search_str type string.
  constants: lc_rs type c length 2 value 'rs'.
  constants: lc_rq type c length 2 value 'rq'.
  constants: lc_htm_xml type c length 7 value 'htm.xml'.
  constants: lc_xml type c length 3 value 'xml'.
  constants: lc_html type c length 4 value 'html'.
  constants: lc_evv type c length 3 value 'evv'.

  clear: p_doktl, p_filename_dms, p_dappl.
  clear lv_file_ext.

* ----------------------------------------------------------
* Filename, am Beispiel mit der Transportnummer 0000103301:
* Für REQ:
*	  Inputfile: 0000103301_EVV__14CHEE000184968288_rq.xml
*   Outputfile für DMS: 0000103301_evv.xml
*Für RSP:
*	 Inputfile: 0000103301_EVV__14CHEE000184968288_rs.xml
*  Outputfile für DMS: 0000103301_evv.xml
*	 Inputfile: 0000103301_EVV__14CHEE000184968288_rs_htm.xml
*  Outputfile für DMS: 0000103301_evv.html
* ----------------------------------------------------------


* ----------------------------------------------------------
* Ermittlung des Teildokumentes aus dem Filenamen:
* rq = 'REQ', rs = 'RSP'
* ----------------------------------------------------------

* Suchstring zusammenbasteln (entweder, oder), damit in
* einem Durchgang gesucht werden kann
  concatenate lc_rs '|' lc_rq into lv_search_str.

  perform search_string using lv_search_str
                              gs_file_list-pathname
                     changing lv_offset
                              lv_length.

  check lv_offset gt 0 and lv_length gt 0.

  case gs_file_list-pathname+lv_offset(lv_length).
    when lc_rs.
      p_doktl = gc_response.
    when lc_rq.
      p_doktl = gc_request.
  endcase.

* ----------------------------------------------------------
* Ermittlung der Fileendung gemäss folgender Reihenfolge
* 1. htm.xml ==> Endung = html
* 2. xml     ==> Endung = xml
* ----------------------------------------------------------
  clear lv_search_str.
  lv_search_str = lc_htm_xml.

  perform search_string using lv_search_str
                              gs_file_list-pathname
                     changing lv_offset
                              lv_length.

  if lv_offset gt 0 and lv_length gt 0.
***    lv_file_ext = lc_html.   "DMS akzeptiert HTML nicht
    lv_file_ext = lc_xml.
  else.  "Suche endung xml
    clear lv_search_str.
    lv_search_str = lc_xml.
    perform search_string using lv_search_str
                                gs_file_list-pathname
                       changing lv_offset
                                lv_length.
    if lv_offset gt 0 and lv_length gt 0.
      lv_file_ext = lc_xml.
    endif.
  endif.  "lv_offset gt 0 and lv_length gt 0


* Filenamen zusammensetzen (Kleinbuchstaben)
  concatenate gv_doknr '.' lv_file_ext into p_filename_dms.
  translate p_filename_dms to lower case.

* Workstation-Applikation: 3 Stellen von der Fileendung und in Grossbuchstaben
* ausgeben
  move lv_file_ext(3) to p_dappl.
  translate p_dappl to upper case.

ENDFORM.                    " SET_DATA_FOR_DMS


*&---------------------------------------------------------------------*
*&      Form  DOCUMENT_CHECKIN_INTO_DMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOCUMENT_CHECKIN_INTO_DMS CHANGING P_SUBRC type sysubrc.

  DATA: lf_doctype    LIKE bapi_doc_draw-documenttype,
        lf_docnumber  LIKE bapi_doc_draw-documentnumber,
        lf_docpart    LIKE bapi_doc_draw-documentpart,
        lf_docversion LIKE bapi_doc_draw-documentversion.
  DATA: lsx_message  LIKE messages,
        lt_originals LIKE BAPI_DOC_FILES2 OCCURS 0 WITH HEADER LINE.
  DATA: lv_statusextern type BAPI_DOC_DRAW-STATUSEXTERN.
  DATA: ls_return type bapiret2.

  lf_doctype = gs_draw-dokar.
  lf_docnumber = gs_draw-doknr.
  lf_docversion = gs_draw-dokvr.
  lf_docpart    = gs_draw-doktl.

  lv_statusextern = 'FR'.

  lt_originals-documenttype = p_dokar.
  lt_originals-documentnumber = lf_docnumber.
  lt_originals-documentpart = lf_docpart.
  lt_originals-documentversion = lf_docversion.


  lt_originals-wsapplication = gv_dappl.
  lt_originals-storagecategory = p_storca.

***  concatenate gv_path gs_file_list-pathname into lt_originals-docfile.
  lt_originals-docfile = gv_dir_file_in.

  append lt_originals.

CALL FUNCTION 'BAPI_DOCUMENT_CHECKIN2'
***  CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
    EXPORTING
      DOCUMENTTYPE            = lf_doctype
      DOCUMENTNUMBER          = lf_docnumber
      DOCUMENTPART            = lf_docpart
      DOCUMENTVERSION         = lf_docversion
*   HOSTNAME                = ' '
*   STATUSINTERN            = ' '
*    STATUSEXTERN            = lv_statusextern
*   STATUSLOG               = ' '
*   REVLEVEL                = ' '
*   AENNR                   = ' '
*   PF_HTTP_DEST            = ' '
*   PF_FTP_DEST             = ' '
   IMPORTING
     RETURN                  = ls_return
    TABLES
      DOCUMENTFILES           = lt_originals
*   COMPONENTS              =
*   DOCUMENTSTRUCTURE       =
            .

  IF ls_return-type CA 'EA'.

    ROLLBACK WORK.

    MESSAGE ID '26' TYPE 'I' NUMBER '000'

    WITH ls_return-message.

  ELSE.

**damit die Änderungen auch gespeichert werden, braucht man ein:

    COMMIT WORK.

  ENDIF.


* Returncode zurückgeben
  p_subrc = sy-subrc.

ENDFORM.                    " DOCUMENT_CHECKIN_INTO_DMS

*&---------------------------------------------------------------------*
*&      Form  GET_DOCNUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_DOKNR  text
*----------------------------------------------------------------------*
FORM GET_DOCNUMBER USING p_file_list type SDOK_FILNM
                   CHANGING P_DOKNR type doknr.

  data: lt_results TYPE match_result_tab.
  data: ls_results like line of lt_results.
  data: lv_offset type i.
  data: lv_length type i.
  data: lv_search_str type string.

  clear p_doknr.

  lv_search_str = 'EVV_'.
  perform search_string using lv_search_str
                              p_file_list
                     changing lv_offset
                              lv_length.

  lv_length = lv_offset + lv_length - 1.

  if lv_length gt 0.
    p_doknr = gs_file_list(lv_length).
  endif.


*****
  exit.
*****

  TRY.
      FIND ALL OCCURRENCES OF REGEX 'EVV_' IN p_file_list
        IN CHARACTER MODE
        IGNORING CASE
        RESULTS lt_results.
    CATCH cx_sy_regex.
*             invalid regex -> stop processing
***        MESSAGE s384 WITH wa_suchstring-low DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE lcx_scan_exceptions.
  ENDTRY.

  read table lt_results into ls_results index 1.
  check sy-subrc eq 0.

  lv_length = ls_results-offset + ls_results-length - 1.
  if lv_length gt 0.
    p_doknr = gs_file_list(lv_length).
  endif.

***** Dokumentnummer = &Transortnummer&_EVV
****      p_doknr = gs_file_list(14).
****
****      if gs_file_list+10(4) ne '_EVV'.
****        write: / 'Ungültiges File', gs_file_list.
****        continue.
****      endif.


***  p_doknr =

ENDFORM.                    " GET_DOCNUMBER

*&---------------------------------------------------------------------*
*&      Form  TEST_DOCUMENTS_EXISTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*----------------------------------------------------------------------*
FORM TEST_DOCUMENTS_EXISTS  CHANGING  P_BEME like gv_beme
                                      P_SUBRC type sysubrc.

  DATA: lv_doctype    LIKE bapi_doc_draw-documenttype,
        lv_docnumber  LIKE bapi_doc_draw-documentnumber,
        lv_docpart    LIKE bapi_doc_draw-documentpart,
        lv_docversion LIKE bapi_doc_draw-documentversion.
  DATA: lsx_message  LIKE messages,
        lt_originals LIKE cvapi_doc_file OCCURS 0 WITH HEADER LINE,

        lt_comp like CVAPI_DOC_COMP OCCURS 0 WITH HEADER LINE.

* Siehe Report TEST_API_CHECKIN_KPRO

  clear p_beme.
  p_subrc = 4.

  lv_doctype = gs_draw-dokar.
  lv_docnumber = gs_draw-doknr.
  lv_docversion = gs_draw-dokvr.
  lv_docpart    = gs_draw-doktl.

  refresh lt_originals.

* Read Originals contained in the document info record
  CALL FUNCTION 'CVAPI_DOC_GETDETAIL'
      EXPORTING
          pf_batchmode     = 'X'
          pf_hostname      = ' '
          pf_dokar        = lv_doctype
          pf_doknr        = lv_docnumber
          pf_dokvr        = lv_docversion
          pf_doktl        = lv_docpart
          pf_active_files = 'X'
          pf_read_comp    = 'X'
*         PF_READ_COMP    = GETCOMPONENTS
*     IMPORTING
*          psx_draw        = psx_draw
*         PFX_DESCRIPTION =
     TABLES
          pt_files        = lt_originals
          PT_COMP         = LT_COMP
     EXCEPTIONS
          not_found       = 1
          no_auth         = 2
          error           = 3
          OTHERS          = 4.

  p_subrc = sy-subrc.

* Falls kein Dokument vorhanden: File nicht anhängen
  if sy-subrc ne 0.
    gv_beme = 'Dokument in DMS nicht vorhanden'.
    exit.
  endif.

* Falls bereits ein File angehängt ist, darf kein weiteres angehängt
* werden

  read table lt_originals with key
*    dappl = gv_dappl.  "XML, HTM
     filename = gv_dir_file_in.

  if sy-subrc eq 0.  "File bereits vorhanden
    concatenate 'Attachement schon vorhanden:' gv_dappl into gv_beme.
    p_subrc = 4.
  else.
    concatenate 'File eingefügt:' gv_dappl into gv_beme.
    p_subrc = 0.
  endif.

ENDFORM.                    " TEST_DOCUMENTS_EXISTS

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_DRAW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_DRAW .

  clear gt_draw.

  check gv_doktl in s_doktl.

  select dokar doknr dokvr doktl dwnam dokst from draw
    into table gt_draw
    where dokar = p_dokar and
          doknr = gv_doknr and
          doktl = gv_doktl.

ENDFORM.                    " GET_DATA_DRAW

*&---------------------------------------------------------------------*
*&      Form  SELECTION_FILE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION_FILE_LIST .

  delete gt_file_list
    where pathname cs 'sig' or
          pathname not in s_doknr.

  exit.

  loop at gt_file_list into gs_file_list.

    if gs_file_list-pathname in s_doknr.
    else.
      delete gt_file_list.
    endif.

  endloop.

ENDFORM.                    " SELECTION_FILE_LIST

*&---------------------------------------------------------------------*
*&      Form  SEARCH_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_SEARC_STR  text
*      <--P_LV_OFFSET  text
*      <--P_LV_LENGTH  text
*----------------------------------------------------------------------*
FORM SEARCH_STRING  USING    P_SEARCH_STR type string "Suchstring
                             P_SEARCH_IN type SDOK_FILNM "Suche in
                    CHANGING P_OFFSET type i
                             P_LENGTH type i.

  clear: p_offset, p_length.

* Ermittlung der Stelle und Länge des Ssuchstrings
  TRY.
***      FIND ALL OCCURRENCES OF REGEX p_search_str IN gs_file_list-pathname
      FIND ALL OCCURRENCES OF REGEX p_search_str IN p_search_in
        IN CHARACTER MODE
        IGNORING CASE
        MATCH OFFSET p_offset
        MATCH LENGTH p_length.
    CATCH cx_sy_regex.
*        invalid regex -> stop processing
*        MESSAGE s384 WITH wa_suchstring-low DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE lcx_scan_exceptions.
  ENDTRY.

ENDFORM.                    " SEARCH_STRING


*&---------------------------------------------------------------------*
*&      Form  PROTOKOLL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROTOKOLL .

  data: lv_length type i.

  clear gs_prot.

  move gs_draw-dokar         to gs_prot-dokar.
  move gs_draw-doknr         to gs_prot-doknr.
  move gs_draw-dokvr         to gs_prot-dokvr.
  move gs_draw-doktl         to gs_prot-doktl.
  move gs_draw-dwnam         to gs_prot-dwnam.
  move gs_draw-dokst         to gs_prot-dokst.
  move gs_file_list-pathname to gs_prot-pathname.
  move gv_filename_dms       to gs_prot-filename_dms.
  move gv_beme               to gs_prot-beme.
  move gv_subrc              to gs_prot-subrc.

  append gs_prot to gt_prot.

  lv_length = strlen( gs_file_list-pathname ).

***  write: / gv_doktl, gs_file_list-pathname(lv_length), gv_filename_dms.

ENDFORM.                    " PROTOKOLL
*&---------------------------------------------------------------------*
*&      Form  AUSGABE_PROTOKOLL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUSGABE_PROTOKOLL .

  if p_sim = gc_on.
    write: / '*** Achtung: Nur Simulationslauf! ***'.
    skip 2.
  endif.

  loop at gt_prot into gs_prot.

    write: / gs_prot-doknr,gs_prot-dokvr, gs_prot-doktl, gs_prot-beme.

  endloop.

ENDFORM.                    " AUSGABE_PROTOKOLL
