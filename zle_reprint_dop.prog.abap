*&---------------------------------------------------------------------*
*& Report  ZLE_REPRINT_DOP
*&
*&---------------------------------------------------------------------*
*&    Aenderungen:
*&
*&    13.03.2017 CHG0031547: Default Dateiname wie folgt setzen:
*&    DoP-"Art.-No.-"Date"-"vendor identification"-"language".pdf
*&   10.04.2017 CHG0031503: Logo nur bei PDF-Nachdruck auf das Formular
*&   andrucken
*&   21.06.2017 96405 ANE-Project: Werksadresse via ZTMM-LABELING-WERKS_DOP
*&   Materialnummer 9-Stellig drucken und speichern
*&   12.04.2018 CHG0032653: Anpassungen der Selektionsfelder via
*&   Fertigungsauftrag
*&
*&---------------------------------------------------------------------*

REPORT zle_reprint_dop.

TABLES: afko, mch1, vlpma, ztmm_labeling, zsdfs1, t001w, nast, tnapr.

TYPE-POOLS: ICON.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.

* ---------------------------------------------------------------------
* Selektionsgruppe 1
* ---------------------------------------------------------------------
*PARAMETERS: p_rbsel1 RADIOBUTTON GROUP rb2.

* Fertigungsauftrag / Sprachcode für Formular
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rbauf RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: COMMENT 3(29) text-s01.
PARAMETERS: p_aufnr LIKE afko-aufnr.
SELECTION-SCREEN: POSITION 67.
*PARAMETERS: p_auftxt TYPE char50.
*PARAMETERS: p_spauf LIKE nast-spras DEFAULT sy-langu no-DISPLAY.
SELECTION-SCREEN END OF LINE.

* Kabel-ID / Sprachcode für Formular
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rbkaid RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: COMMENT 3(29) text-s02.
PARAMETERS: p_kabid LIKE clobjdat-ausp1.
SELECTION-SCREEN: POSITION 67.
*PARAMETERS: p_kabtxt TYPE char50.
*PARAMETERS: p_spkabi LIKE nast-spras DEFAULT sy-langu no-display.
SELECTION-SCREEN END OF LINE.

* Charge / Sprachcode für Formular
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rbchrg RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: COMMENT 3(29) text-s03.
PARAMETERS: p_charg LIKE mch1-charg.
SELECTION-SCREEN: POSITION 67.
*PARAMETERS: p_chrtxt TYPE char50.
*PARAMETERS: p_spchrg LIKE nast-spras DEFAULT sy-langu no-display.
SELECTION-SCREEN END OF LINE.
* Sprachcode für Formular
*>>>PARAMETERS: p_spras1 LIKE nast-spras DEFAULT 'EN'.
* Drucker
***PARAMETERS: p_ldes1 LIKE nast-ldest DEFAULT 'LOCS'.
SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN ULINE.

* Lieferung / Pos
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rbvbel RADIOBUTTON GROUP rb1.
SELECTION-SCREEN: COMMENT 3(29) text-s04.
PARAMETERS: p_vbeln LIKE likp-vbeln.
SELECTION-SCREEN: POSITION 49.
PARAMETERS: p_posnr LIKE lips-posnr NO-DISPLAY.
***SELECTION-SCREEN: POSITION 67.
***PARAMETERS: p_vbltxt TYPE char50.
SELECTION-SCREEN END OF LINE.
* Drucker
*PARAMETERS: p_ldes1 LIKE nast-ldest DEFAULT 'LOCS'.

SELECTION-SCREEN ULINE.

* ---------------------------------------------------------------------
* Selektionsgruppe 2
* ---------------------------------------------------------------------
PARAMETERS: p_rbsel2 RADIOBUTTON GROUP rb1.
* Material
SELECT-OPTIONS: s_matnr FOR ztmm_labeling-matnr NO INTERVALS.
* Lieferung
PARAMETERS: p_lifnr LIKE ztmm_labeling-lifnr.
* Produktionsdatum
PARAMETERS: p_datam LIKE zsmm_labeling-datam DEFAULT sy-datum.
* Sprachcode für Formular
*>>>PARAMETERS: p_spras LIKE nast-spras DEFAULT 'EN'.
* Drucker
***PARAMETERS: p_ldest LIKE nast-ldest DEFAULT 'LOCS'.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN PUSHBUTTON 3(45) pb_sel USER-COMMAND list_dop.

SELECTION-SCREEN ULINE.

* Drucker
*PARAMETERS: p_ldest LIKE sh_prin-lname. "nast-ldest DEFAULT 'LOCS'.
* Sprachcode für Formular
PARAMETERS: p_spras LIKE nast-spras DEFAULT 'EN'.
PARAMETERS: p_ldest LIKE nast-ldest.
*SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.

* ---------------------------------------------------------------------
* Selektion für PDF-Ablage
* ---------------------------------------------------------------------
PARAMETERS: p_rbsel3 RADIOBUTTON GROUP rb1.
SELECT-OPTIONS: s_matnr2 FOR ztmm_labeling-matnr.
SELECT-OPTIONS: s_lifnr2 FOR ztmm_labeling-lifnr.
SELECT-OPTIONS: s_spras2 FOR nast-spras NO INTERVALS.
PARAMETERS: p_path TYPE STRING  DEFAULT 'C:\Temp\'.
*PARAMETERS: p_path LIKE rlgrap-filename DEFAULT 'C:\Temp\'.
PARAMETERS: p_fimask(60) AS LISTBOX VISIBLE LENGTH 60.

SELECTION-SCREEN END OF BLOCK b1.

* ---------------------------------------------------------------------
* Informationen zur Selektion
* ---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b03.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(4) ICON_INF. "text-c01.
SELECTION-SCREEN: COMMENT 5(75) text-c01.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: SKIP.
PARAMETERS: p_infmat TYPE matnr.
PARAMETERS: p_infauf LIKE afko-aufnr.
PARAMETERS: p_infeda LIKE afvgd-ssevd.
PARAMETERS: p_infchr TYPE charg_d.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_infebe LIKE ekko-ebeln.
PARAMETERS: p_infebp LIKE ekpo-ebelp.
PARAMETERS: p_infwda LIKE ekbe-budat.
SELECTION-SCREEN END OF BLOCK b2.


DATA: gv_subrc TYPE sysubrc.
DATA: gv_werks TYPE werks_d.
DATA: gv_text TYPE string.
DATA gs_labeling TYPE ztmm_labeling.
DATA: gv_vkorg TYPE vkorg.
DATA: gv_retcode TYPE sysubrc.
DATA: gv_okcode LIKE sy-ucomm.
DATA: gv_save_ok LIKE sy-ucomm.
DATA: gv_ucomm LIKE sy-ucomm.
DATA: xscreen(1) TYPE c.               "Output on printer or screen

DATA: BEGIN OF adr_werk,
        name1    TYPE adrc-name1,
        stras    TYPE adrc-street,
        pstlz    TYPE adrc-post_code1,
        ort01    TYPE adrc-city1,
        land1    TYPE adrc-county,
        landx    TYPE t005t-landx,
        plz_land TYPE c LENGTH 72,
      END OF adr_werk.

DATA: gv_sign TYPE tdobname.
DATA: labtx_de1 TYPE tdline.
DATA: labtx_de2 TYPE tdline.
DATA: labtx_en1 TYPE tdline.
DATA: labtx_en2 TYPE tdline.
DATA: gv_return_code TYPE c.
DATA: gv_us_screen TYPE c.
* Feld zur Steuerung des Logodruckes
DATA: gv_print_logo TYPE char1.

* Felder für DFynpro 2000
DATA: BEGIN OF gs_dyn_2000,
        aufnr  TYPE aufnr,
        plnbez TYPE matnr,
        prodat TYPE c LENGTH 10,
      END OF gs_dyn_2000.

TYPES: BEGIN OF t_in,
         date TYPE dats,
         matnr TYPE matnr,
       END OF t_in.

DATA: gs_in TYPE t_in.

* Dieses Feld wird für die Textsprache des Formulares verwendet
DATA: gv_spras_text TYPE spras.

DATA: gs_dop_sel TYPE zsdop_sel.
DATA: gt_dop_sel TYPE STANDARD TABLE OF zsdop_sel.

DATA: gs_event TYPE slis_alv_event.
DATA: gt_event TYPE slis_t_event.
DATA: gv_box TYPE slis_fieldname VALUE 'BOX'.
DATA: gv_boxtab TYPE slis_tabname VALUE 'GT_DOP_SEL'.
DATA: gv_repid TYPE sy-repid.
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: gs_layout TYPE slis_layout_alv.
DATA: gv_msgtxt TYPE natxt.
DATA: gv_counter TYPE i.
DATA: gv_lname TYPE rspolname.

* Dieses Feld wird für das Formular verwendet, da 10-stellige Nummern
* nur 9-stellig angedruckt werden
DATA: gv_matnr TYPE matnr.

DATA:    xnast LIKE vnast OCCURS 20 WITH HEADER LINE.

TYPES: BEGIN OF t_lina,
         vbeln    TYPE vbeln_vl,
         posnr    TYPE posnr_vl,
         uecha    TYPE uecha,
         lifnr    TYPE elifn,
         kappl    TYPE sna_kappl,
         objky    TYPE na_objkey,
         kschl    TYPE sna_kschl,
         erdat    TYPE na_erdat,
         eruhr    TYPE na_eruhr,
         optarcnr TYPE na_optar,
         objtype  TYPE oj_name,
       END OF t_lina.

DATA: gs_lina TYPE t_lina.
DATA: gt_lina TYPE STANDARD TABLE OF t_lina.

TYPES: BEGIN OF t_nast,
         kappl    TYPE sna_kappl,
         objky    TYPE na_objkey,
         kschl    TYPE sna_kschl,
         erdat    TYPE na_erdat,
         eruhr    TYPE na_eruhr,
         optarcnr TYPE na_optar,
         objtype  TYPE oj_name,
       END OF t_nast.

DATA: gs_nast TYPE t_nast.

DATA: retcode   LIKE sy-subrc.         "Returncode

DATA: gt_otfdata TYPE TABLE OF itcoo WITH HEADER LINE.

* Felder für Listbox (Filemaskierung)
DATA: gt_list TYPE vrm_values.
DATA: gs_list TYPE vrm_value.
DATA: gv_fimask TYPE string.
DATA: gv_index TYPE i.


* Diverse Kosntanten
CONSTANTS: gc_form TYPE tdform VALUE 'ZDOP_300'.
CONSTANTS: gc_kschl_zdop TYPE kschl VALUE 'ZDOP'.
CONSTANTS: gc_on TYPE c VALUE 'X'.
CONSTANTS: gc_aktiv TYPE c VALUE 'X'.
CONSTANTS: gc_mark TYPE c VALUE 'X'.
CONSTANTS: gc_structure LIKE dd02l-tabname VALUE 'ZSDOP_SEL'.
CONSTANTS: gc_pfstatus TYPE slis_formname VALUE 'SET_PF_STATUS'.
CONSTANTS: gc_datbi_aktuell TYPE char10 VALUE '99991231'.
CONSTANTS: gc_anz_stellen TYPE i VALUE 9.


* Mägliche Werte für Filemaskierung (PDF-Ablage)
CONSTANTS: gc_filemask_dop TYPE string VALUE 'DOP'.
CONSTANTS: gc_filemask_matnr TYPE string VALUE 'MATNR'.
CONSTANTS: gc_filemask_lifke TYPE string VALUE 'LIFKE'.
*constants: gc_filemask_lifke_minus type string value 'LIFKE-'.
CONSTANTS: gc_filemask_datab TYPE string VALUE 'VALID_FROM_DATE'.
CONSTANTS: gc_filemask_spras TYPE string VALUE 'LANGU'.

CONSTANTS: gc_filemask_sep TYPE c VALUE '-'.
CONSTANTS: gc_format TYPE c LENGTH 3 VALUE 'PDF'.
CONSTANTS: gc_pdf_einzel TYPE c VALUE 'E'.
CONSTANTS: gc_pdf_multi TYPE c VALUE 'M'.

INITIALIZATION.

WRITE ICON_INFORMATION AS ICON TO icon_inf.

*Druckervorschlag für DOP-druck
 GET PARAMETER ID 'ZPRINTER_DOP' FIELD gv_lname.

 IF sy-subrc EQ 0.
   SELECT SINGLE padest FROM tsp03l INTO p_ldest
     WHERE lname = gv_lname.
 ENDIF.

*AT SELECTION-SCREEN OUTPUT.

***  IF NOT p_ldest IS INITIAL.
***    SELECT SINGLE spld FROM usr01 INTO p_ldest
***                       WHERE bname EQ sy-uname.
***  ENDIF.

  MOVE text-100 TO pb_sel.

  PERFORM set_values_filmask.

  PERFORM set_screen_fields.


* Syntax von MASKE siehe Doku FB WS_FILENAME_GET
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


*----------------------------------------------------------------------*
*   at selection screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
***  CALL FUNCTION 'WS_FILENAME_GET'
***    EXPORTING
****     def_path         = 'H:\'
***      mask             = gs_maske
***    IMPORTING
***      filename         = p_path
***    EXCEPTIONS
***      inv_winsys       = 1
***      no_batch         = 2
***      selection_cancel = 3
***      selection_error  = 4
***      OTHERS           = 5.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      SELECTED_FOLDER = p_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF p_path IS NOT INITIAL.
    CONCATENATE p_path '\' INTO p_path IN CHARACTER MODE.
  ENDIF.

* ----------------------------------------------------------------
* Selection-Screen
* ----------------------------------------------------------------
AT SELECTION-SCREEN.

  CLEAR: ztmm_labeling, gs_dyn_2000, gv_matnr,
         p_infmat, p_infauf, p_infchr, p_infeda,
         p_infebe, p_infebp, p_infwda.

  gv_ucomm = sy-ucomm.

  CLEAR sy-ucomm.

  CASE gv_ucomm.
    WHEN 'LIST_DOP'.
      CLEAR: p_rbauf, p_rbkaid, p_rbchrg, p_rbvbel, p_rbsel3.
      p_rbsel2 = gc_on.
      PERFORM ausgabe_vorhandene_dop.
    WHEN 'ONLI' OR SPACE.
      CASE gc_on.
        WHEN p_rbsel2. "Material/Lieferant
          PERFORM eingaben_2.
        WHEN p_rbsel3. "PDF-Ablage
          PERFORM eingaben_3.
        WHEN others.   "Fert.Auftrag, Kabel-Nr, Charge, Lief./Pos
          PERFORM eingaben_1.
      ENDCASE.

  ENDCASE.

* ----------------------------------------------------------------
* Selection-Screen output
* ----------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  PERFORM set_screen_fields.

* ----------------------------------------------------------------
* Programmstart
* ----------------------------------------------------------------
START-OF-SELECTION.

* ----------------------------------------------------------------
* Processing
* ----------------------------------------------------------------
  PERFORM processing.

*end-of-selection.

*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.

  SET PF-STATUS '2000'.
  SET TITLEBAR '2000'.

ENDMODULE.                 " STATUS_2000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.

  gv_save_ok = gv_okcode.
  CLEAR gv_okcode.

  CASE gv_save_ok.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                 " EXIT_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

* OKcode Verarbeitung
  CASE gv_okcode.
*   Werte in Tabelle ZSDAGNFDATA updaten
    WHEN 'SAVE'.
*      PERFORM processing.
      CLEAR gv_okcode.
      SET SCREEN 0.
      LEAVE SCREEN.
*   Keine Anpassung der Daten
    WHEN 'CANCEL'.
      CLEAR gs_dyn_2000.
      CLEAR gv_okcode.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

  CLEAR gv_okcode.

ENDMODULE.                 " USER_COMMAND_2000  INPUT

*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS.

  LOOP AT SCREEN.

* Felder nur ausgabefähig machen
    if screen-name = 'P_INFMAT' OR
       screen-name = 'P_INFAUF' OR
       screen-name = 'P_INFCHR' OR
       screen-name = 'P_INFEDA' OR
       screen-name = 'P_INFEBE' OR
       screen-name = 'P_INFEBP' OR
       screen-name = 'P_INFWDA'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_SCREEN_FIELDS


*&---------------------------------------------------------------------*
*&      Form  PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing.

  CLEAR: gs_dyn_2000.


* ---------------------------------------------------------------------
* DoP-Daten aus Tabelle ZTMM_LABELIN ermitteln
* ---------------------------------------------------------------------

  CASE gc_on.
    WHEN p_rbsel3. "PDF-Ablage
      PERFORM selection_material_fuer_pdf CHANGING gv_subrc
                                                   gv_text.
    WHEN p_rbvbel. "Lieferung / Pos
      PERFORM selection_via_lieferung CHANGING gv_subrc.
      IF gv_subrc NE 0.
        MESSAGE s001(aq) WITH text-e17.
      ENDIF.
    WHEN OTHERS.
     PERFORM nachdruck_via_material USING gv_spras_text.
  ENDCASE.

ENDFORM.                    " PROCESSING

*&---------------------------------------------------------------------*
*&      Form  PROCESSING_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_dop USING pv_matnr TYPE matnr
                        pv_werks TYPE werks_d
                        pv_lifnr TYPE elifn
                        pv_date TYPE DATS
               CHANGING ps_labeling TYPE ztmm_labeling.

  CLEAR ps_labeling.

  SELECT SINGLE * FROM ztmm_labeling INTO ps_labeling
    WHERE matnr = pv_matnr AND
          werks = pv_werks AND
          lifnr = pv_lifnr AND
          datab LE pv_date AND
          datbi GE pv_date AND
          actsg = gc_on.

ENDFORM.                    " GET_DATA_DOP


*&---------------------------------------------------------------------*
*&      Form  READ_ADR_WERK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTMMLABELING_WERKS  text
*----------------------------------------------------------------------*
FORM read_adr_werk  USING    p_werks TYPE werks_d.

  DATA: ls_adrc TYPE adrc.

  CLEAR: adr_werk, ls_adrc.

* Absender (via Werk)
  CLEAR t001w.
  SELECT SINGLE * FROM t001w WHERE werks = p_werks.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * FROM adrc INTO ls_adrc
    WHERE addrnumber = t001w-adrnr.

* Englische Bezeichnung des Landes
  SELECT SINGLE landx FROM t005t INTO adr_werk-landx
    WHERE spras = 'E' AND
          land1 = ls_adrc-country.

  MOVE ls_adrc-name1      TO adr_werk-name1.

* Strasse und Nummer
  CONCATENATE ls_adrc-street ls_adrc-house_num1
  INTO adr_werk-stras SEPARATED BY space
  IN CHARACTER MODE.

  MOVE ls_adrc-city1      TO adr_werk-ort01.
  MOVE ls_adrc-post_code1 TO adr_werk-pstlz.
  MOVE ls_adrc-country    TO adr_werk-land1.

* Landkürzel, Postleitzahl, Ort und Land
  CONCATENATE ls_adrc-post_code1 ls_adrc-city1 ',' adr_werk-landx
  INTO adr_werk-plz_land SEPARATED BY space IN CHARACTER MODE.


ENDFORM.                    " READ_ADR_WERK

*&---------------------------------------------------------------------*
*&      Form  READ_ETIK_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LIPS_MATNR  text
*----------------------------------------------------------------------*
FORM read_etik_text  USING    p_ls_lips_matnr.

  DATA: lt_tline  TYPE tline OCCURS 0 WITH HEADER LINE,
        lv_tdname TYPE stxh-tdname.

  CONSTANTS: c_tdid  LIKE stxh-tdid VALUE 'ZLAB',
             c_tdobj LIKE stxh-tdobject VALUE 'MATERIAL'.

  CLEAR: lt_tline, lv_tdname.

  lv_tdname = p_ls_lips_matnr.

*--- Text Deutsch auslesen.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = c_tdid
      language                = 'D'
      name                    = lv_tdname
      object                  = c_tdobj
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc EQ 0.
    READ TABLE lt_tline INDEX 1 INTO labtx_de1.
    READ TABLE lt_tline INDEX 2 INTO labtx_de2.
  ENDIF.

  CLEAR lt_tline.

*--- Text Englisch auslesen.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = c_tdid
      language                = 'E'
      name                    = lv_tdname
      object                  = c_tdobj
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc EQ 0.
    READ TABLE lt_tline INDEX 1 INTO labtx_en1.
    READ TABLE lt_tline INDEX 2 INTO labtx_en2.
  ENDIF.


ENDFORM.                    " READ_ETIK_TEXT


*&---------------------------------------------------------------------*
*&      Form  OPEN_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM open_form .

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      form                        = gc_form
      language                    = p_spras
      dialog                      = 'X'
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      codepage                    = 11
      OTHERS                      = 12.

  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.                    " OPEN_FORM

*---------------------------------------------------------------------*
*       FORM ITEM_PRINT_FS1                                           *
*---------------------------------------------------------------------*
*       Printout of the items                                         *
*---------------------------------------------------------------------*
FORM item_print_fs1.

* Print first header
  PERFORM print_element USING 'TITLE' 'MAIN'.

  CALL FUNCTION 'WRITE_FORM'          "Activate header
    EXPORTING
      element = 'TITLE'
      type    = 'TOP'
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.


ENDFORM.                  "ITEM_PRINT_FS1

*&---------------------------------------------------------------------*
*&      Form  PRINT_ELEMENT
*&---------------------------------------------------------------------*
FORM print_element  USING  pa_elem pa_wind.
* generelle Routine zum Druck eines Elementes
* in einem beliebigen Fenster
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = pa_elem
      window  = pa_wind
    EXCEPTIONS
      element = 1
      window  = 2.
  IF sy-subrc NE 0.
    PERFORM protocol_update.
  ENDIF.
ENDFORM.                    " PRINT_ELEMENT

*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*
FORM protocol_update.

  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.                  "PROTOCOL_UPDATE

*---------------------------------------------------------------------*
*       FORM END_PRINT                                                *
*---------------------------------------------------------------------*
*       Total, Texte usw. am Ende ausgeben                            *
*---------------------------------------------------------------------*
FORM end_print.

* Variablen Standardtext "NACH POSITIONSBLOCK" ausgeben      "N100f
  IF NOT zsdfs1-text_nachpos IS INITIAL.                    "N100f
    PERFORM print_element USING 'SPACE_LINE' 'MAIN'.        "N100f
    PERFORM print_element USING 'TEXT_NACH_POS' 'MAIN'.     "N100f
  ENDIF.                                                    "N100f

* Variablen Standardtext "ABSCHLUSSTEXT" ausgeben            "N100f
  IF NOT zsdfs1-text_end IS INITIAL.                        "N100f
    PERFORM print_element USING 'SPACE_LINE' 'MAIN'.        "N100f
    PERFORM print_element USING 'TEXT_ABSCHLUSS' 'MAIN'.    "N100f
  ENDIF.                                                    "N100f

*--- Footer Drucken
  PERFORM print_element USING 'ADDRESS' 'FOOTER'.

ENDFORM.                             "end_print

*---------------------------------------------------------------------*
*       FORM FORM_CLOSE                                               *
*---------------------------------------------------------------------*
*       End of printing the form                                      *
*---------------------------------------------------------------------*
FORM form_close.

  CALL FUNCTION 'CLOSE_FORM'           "...Ende Formulardruck
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    gv_retcode = 1.
    PERFORM protocol_update.
  ENDIF.
  SET COUNTRY space.

ENDFORM.                    "form_close

*&---------------------------------------------------------------------*
*&      Form  GET_ZSDFS1_CUSTOMIZING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_zsdfs1_customizing CHANGING p_subrc TYPE sysubrc.

* Selektion ZSDFS1
  CLEAR zsdfs1.
  SELECT SINGLE * FROM zsdfs1 WHERE kschl = gc_kschl_zdop
                                AND vkorg = gv_vkorg.


ENDFORM.                    " GET_ZSDFS1_CUSTOMIZING

*&---------------------------------------------------------------------*
*&      Form  SELECTION_VIA_LIEFERUNG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*----------------------------------------------------------------------*
FORM selection_via_lieferung  CHANGING pv_subrc TYPE sy-subrc.

  TYPES: BEGIN OF t_nast,
           kappl TYPE sna_kappl,
           objky TYPE na_objkey,
           kschl TYPE sna_kschl,
           spras TYPE na_spras,
           parnr TYPE na_parnr,
           erdat TYPE na_erdat,
           eruhr TYPE na_eruhr,
           optarcnr TYPE na_optar,
           objtype TYPE oj_name,
         END OF t_nast.

  DATA: ls_nast TYPE t_nast.
  DATA: ls_nast_save TYPE t_nast.
  DATA: lt_nast TYPE STANDARD TABLE OF t_nast.
  DATA: lv_objky TYPE na_objkey.
  DATA: lv_vbeln TYPE vbeln_vl.
  DATA: lv_posnr TYPE posnr_vl.
  DATA: lv_lifnr TYPE ELIFN.

  TYPES: BEGIN OF t_likp_lips,
           kunag TYPE kunag,
           kunnr TYPE kunwe,
           vbeln TYPE vbeln_vl,
           posnr TYPE posnr_vl,
           matnr TYPE matnr,
           charg TYPE charg_d,
           uecha TYPE uecha,
           mbdat TYPE mbdat,
         END OF t_likp_lips.

  DATA: ls_likp_lips TYPE t_likp_lips.
  DATA: lt_likp_lips TYPE STANDARD TABLE OF t_likp_lips.
  DATA: lv_anz TYPE i.

  TYPES: BEGIN OF t_sel_nast,
           objky TYPE na_objkey,
         END OF t_sel_nast.

  RANGES: r_objky FOR lv_objky.

  pv_subrc = 4.

  CLEAR: lt_nast, gt_dop_sel, lv_objky, r_objky.

* Benutzer gibt i.d.R. die Hauptposition ein, daher muss via VGPOS
* gelesen werden, um den Chargensplitt ebenfalls zu berücksichtigen
  IF p_posnr IS NOT INITIAL.
    SELECT vbeln posnr FROM lips INTO (lv_vbeln, lv_posnr)
      WHERE vbeln = p_vbeln AND
            ( posnr = p_posnr OR
              uecha = p_posnr ).
***            vgpos = p_posnr.
      CONCATENATE lv_vbeln lv_posnr INTO r_objky-low IN CHARACTER MODE.
      MOVE 'I' TO r_objky-sign.
      MOVE 'EQ' TO r_objky-option.
      APPEND r_objky.
    ENDSELECT.

*   Evtl. Eingabe Chargensplitt-Position (9*): Ermittlung via POSNR
***    IF lt_nast[] IS INITIAL.
***      SELECT vbeln posnr FROM lips INTO (lv_vbeln, lv_posnr)
***        WHERE vbeln = p_vbeln AND
***              posnr = p_posnr.
***        CONCATENATE lv_vbeln lv_posnr INTO r_objky-low IN CHARACTER MODE.
***        MOVE 'I' TO r_objky-sign.
***        MOVE 'EQ' TO r_objky-option.
***        APPEND r_objky.
***      ENDSELECT.
***    ENDIF.
**    CONCATENATE p_vbeln p_posnr INTO lv_objky IN CHARACTER MODE.
  ELSE.
*    CONCATENATE p_vbeln '%' INTO lv_objky IN CHARACTER MODE.
    CONCATENATE p_vbeln '*' INTO r_objky-low IN CHARACTER MODE.
    MOVE 'I' TO r_objky-sign.
    MOVE 'CP' TO r_objky-option.
    APPEND r_objky.
  ENDIF.

  IF r_objky[] IS INITIAL.
    pv_subrc = 4.
    EXIT.
  ENDIF.

  SELECT kappl objky kschl spras parnr erdat eruhr optarcnr objtype
    FROM nast INTO TABLE lt_nast
    WHERE kappl = 'V2' AND
*          objky LIKE lv_objky AND
          objky IN r_objky AND
          kschl = 'ZDOP' AND
          vstat = '1'.

  pv_subrc = sy-subrc.

  CHECK sy-subrc = 0.

* neueste Nachricht ermitteln
  SORT lt_nast BY objky erdat eruhr ASCENDING.

  LOOP AT lt_nast INTO ls_nast.

    ls_nast_save = ls_nast.
    AT END OF objky.

      CLEAR: ls_likp_lips, lv_lifnr.
      lv_vbeln = ls_nast-objky(10).
      lv_posnr = ls_nast-objky+10(6).
      SELECT SINGLE likp~kunag likp~kunnr lips~vbeln lips~posnr
                    lips~matnr lips~charg lips~uecha lips~mbdat
        INTO ls_likp_lips
        FROM likp INNER JOIN lips
        ON likp~vbeln = lips~vbeln
        WHERE likp~vbeln = lv_vbeln AND
              lips~posnr = lv_posnr.

        IF ls_likp_lips-charg IS NOT INITIAL.
          SELECT SINGLE lifnr FROM mch1 INTO lv_lifnr
            WHERE matnr = ls_likp_lips-matnr AND
                  charg = ls_likp_lips-charg.
        ENDIF.

    CLEAR gs_dop_sel.

    gs_dop_sel-matnr = ls_likp_lips-matnr.
    gs_dop_sel-kunag = ls_likp_lips-kunag.
    gs_dop_sel-kunnr = ls_likp_lips-kunnr.
    gs_dop_sel-vbeln = ls_likp_lips-vbeln.
    gs_dop_sel-posnr = ls_likp_lips-posnr.
    gs_dop_sel-charg = ls_likp_lips-charg.
    gs_dop_sel-uecha = ls_likp_lips-uecha.
    gs_dop_sel-lifnr = lv_lifnr.
    gs_dop_sel-mbdat = ls_likp_lips-mbdat.

    IF ls_likp_lips-charg IS NOT INITIAL.
      SELECT SINGLE zzaufnr zzebeln zzebelp FROM mch1
        INTO (gs_dop_sel-aufnr, gs_dop_sel-ebeln, gs_dop_sel-ebelp)
        WHERE matnr = ls_likp_lips-matnr AND
              charg = ls_likp_lips-charg.
    ENDIF.

    gs_dop_sel-kappl = ls_nast_save-kappl.
    gs_dop_sel-objky = ls_nast_save-objky.
    gs_dop_sel-kschl = ls_nast_save-kschl.
    gs_dop_sel-erdat = ls_nast_save-erdat.
    gs_dop_sel-eruhr = ls_nast_save-eruhr.
    gs_dop_sel-optarcnr = ls_nast_save-optarcnr.
    gs_dop_sel-objtype = ls_nast_save-objtype.
    APPEND gs_dop_sel TO gt_dop_sel.

    ENDAT.

  ENDLOOP.

  CLEAR lv_anz.

  DESCRIBE TABLE gt_dop_sel LINES lv_anz.

  IF lv_anz EQ 0.
    pv_subrc = 4.
    EXIT.
  ELSE.
    pv_subrc = 0.
    SORT gt_dop_sel BY VBELN UECHA POSNR CHARG ERDAT ERUHR.
  ENDIF.

***  IF lv_anz EQ 1.
***    PERFORM printview_dop USING space.
***  ELSE.
****   Ausgabe über Listviewer
***    PERFORM setup_alv.
***    PERFORM display_list.
***  ENDIF.

* Ausgabe über Listviewer
  PERFORM setup_alv.
  PERFORM display_list.



ENDFORM.                    " SELECTION_VIA_LIEFERUNG



*&---------------------------------------------------------------------*
*&      Form  setup_alv
*&---------------------------------------------------------------------*
FORM setup_alv.

* Layout
  gs_layout-zebra                 = 'X'.
  gs_layout-colwidth_optimize     = 'X'.
  gs_layout-box_fieldname         = gv_box.

* Events: Titel/Ueberschrift
  CLEAR gs_event.
  gs_event-name = 'TOP_OF_PAGE'.
  gs_event-form = 'LISTVIEW_HEADER'.
  APPEND gs_event TO gt_event.
* Events: Usercommands
  CLEAR gs_event.
  gs_event-name = 'USER_COMMAND'.
  gs_event-form = 'EXECUTE_USERCOMMANDS'.
  APPEND gs_event TO gt_event.

  PERFORM create_fldcat USING gc_structure CHANGING gt_fieldcat[].

  gv_repid = sy-repid.

ENDFORM.                    " setup_alv

*&---------------------------------------------------------------------*
*&      Form  create_fldcat
*&---------------------------------------------------------------------*
FORM create_fldcat USING pa_structure
                   CHANGING ct_fldcat TYPE slis_t_fieldcat_alv.

  CLEAR gt_fieldcat. REFRESH: gt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = pa_structure
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF  sy-subrc EQ 0.
    ct_fldcat[]  =  gt_fieldcat[].
  ENDIF.

ENDFORM.                    " create_fldcat


*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list.

* FB für Listausgabe
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_pf_status_set = gc_pfstatus
      i_structure_name         = gc_structure
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      i_save                   = 'A'         "Standard+Benutzerspez.
*     IS_VARIANT               = p_variant
      it_events                = gt_event[]
    TABLES
      t_outtab                 = gt_dop_sel
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc NE 0.
    MESSAGE s203(s7).
  ENDIF.

ENDFORM.                    " display_list

*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
FORM set_pf_status USING lt_extab TYPE slis_t_extab.
* Spezieller PF-Status setzen
  SET PF-STATUS 'STANDARD_EXTENDED'.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  EXECUTE_USERCOMMANDS
*&---------------------------------------------------------------------*
FORM execute_usercommands USING pa_ucomm LIKE sy-ucomm
                                pa_info TYPE slis_selfield.

  CASE pa_ucomm.
*   Update via doppelclick
    WHEN '&PDF'.
      PERFORM printview_dop USING gc_mark.

***      perform update_nachfassinfo_single using pa_info.
*   Update via Button
    WHEN 'DATA'.
***      perform nachfassen_pflegen using pa_info.
****   Langtext Angebotsinfo pflegen
***    when 'LTAI'.
***      perform aginfotxt_pflegen TABLES it_agtext
***                                USING '9010' it_data-infot pa_info.
****   Langtext Verloren-an pflegen
***    when 'LTVA'.
***      perform verltxt_pflegen TABLES it_vatext
***                              USING '9011' it_data-verlo pa_info.
****   Springen in Belegdaten
***    when 'VBLN'.
***      perform verzweigen_va23 using pa_info.
****   Verzweigen in Debitor
***    when 'DEBI'.
***      perform verzweigen_vd03 using pa_info.
****   SAPmail Expressnachricht versenden                  "N03
***    when 'MAIL'.                                        "N03
***      perform send_sapmail.                             "N03
  ENDCASE.

ENDFORM.                    " EXECUTE_USERCOMMANDS

*&---------------------------------------------------------------------*
*&      Form  PRINTVIEW_DOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printview_dop USING pv_mark TYPE char1.

  DATA: lv_anz_lines TYPE i.

  CLEAR lv_anz_lines.

  LOOP AT gt_dop_sel INTO gs_dop_sel
    WHERE box EQ pv_mark.
    lv_anz_lines = lv_anz_lines + 1.
  ENDLOOP.

  IF sy-subrc NE 0. "Kine Zeile wurde markiert
    MESSAGE s098(26).
    EXIT.
  ENDIF.

  PERFORM archive_anzeige_neu USING pv_mark. "Dokument anzeigen

***  IF lv_anz_lines EQ 1.
***    PERFORM archive_anzeige_neu USING pv_mark. "Dokument anzeigen
***  ELSE.
***    MESSAGE s329(26).
***    EXIT.
***  ENDIF.

ENDFORM.                    " PRINTVIEW_DOP

*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_ANZEIGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_anzeige_neu USING pv_mark TYPE c.

* Verarbeitung aus Formroutine archiv_anzeige in program SAPDV70A.


  DATA: lv_objky TYPE na_objkey.

  DATA: ls_nast TYPE nast.
  DATA: lt_nast TYPE STANDARD TABLE OF nast.

  DATA: object_id  LIKE sapb-sapobjid,
        object_na  LIKE t681z-oj_name,
        object_na2 LIKE t681z-oj_name,
        l_flag     TYPE saearstat VALUE 'X',
        BEGIN OF xvbkey,
          vbeln LIKE vbak-vbeln,
          posnr LIKE vbap-posnr,
        END OF xvbkey.
  DATA: ls_object     TYPE objectconn,
        lt_object     TYPE TABLE OF objectconn,
        lt_parameters TYPE TABLE OF parameters.


  DATA: BEGIN OF xobjid,
          objky LIKE nast-objky,
          arcnr LIKE nast-optarcnr,
        END OF xobjid.

  LOOP AT gt_dop_sel INTO gs_dop_sel
    WHERE box EQ pv_mark.


  IF gs_dop_sel-optarcnr IS INITIAL.
    MESSAGE i048(vn) WITH gs_dop_sel-kschl.
    EXIT.
  ENDIF.

  xobjid-objky = gs_dop_sel-objky.
* Ermitteln der Objektart
  CALL FUNCTION 'WFMC_GET_ARCHIVE_OBJECT_TYPE'
    EXPORTING
      pi_kappl        = gs_dop_sel-kappl
      pi_objtype      = gs_dop_sel-objtype
    IMPORTING
      pe_object_type  = object_na
      pe_object_type2 = object_na2
    CHANGING
      pc_objky        = gs_dop_sel-objky.

* Aufbau OBJECT_ID
  xobjid-arcnr = gs_dop_sel-optarcnr.
  object_id = xobjid.
  ls_object-objecttype = object_na.
  ls_object-objectid   = object_id.

  APPEND ls_object TO lt_object.

  IF NOT object_na2 IS INITIAL.
    ls_object-objecttype = object_na2.
    APPEND ls_object TO lt_object.
  ENDIF.

  CALL FUNCTION 'OBJECT_DISPLAY_CONNECTIONS'
*   EXPORTING
*     OBJECTTYPE       = ' '
*     OBJECTID         = ' '
    TABLES
      object    = lt_object
      parameter = lt_parameters
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

 ENDLOOP.

ENDFORM.                    " ARCHIVE_ANZEIGE_NEU


*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_ANZEIGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_anzeige .

* Verarbeitung aus Formroutine archiv_anzeige in program SAPDV70A.


  DATA: lv_objky TYPE na_objkey.

  DATA: ls_nast TYPE nast.
  DATA: lt_nast TYPE STANDARD TABLE OF nast.

  DATA: object_id  LIKE sapb-sapobjid,
        object_na  LIKE t681z-oj_name,
        object_na2 LIKE t681z-oj_name,
        l_flag     TYPE saearstat VALUE 'X',
        BEGIN OF xvbkey,
          vbeln LIKE vbak-vbeln,
          posnr LIKE vbap-posnr,
        END OF xvbkey.
  DATA: ls_object     TYPE objectconn,
        lt_object     TYPE TABLE OF objectconn,
        lt_parameters TYPE TABLE OF parameters.


  DATA: BEGIN OF xobjid,
          objky LIKE nast-objky,
          arcnr LIKE nast-optarcnr,
        END OF xobjid.


***  CONCATENATE gs_dop_sel-vbeln gs_dop_sel-vgpos INTO lv_objky.
  CONCATENATE gs_dop_sel-vbeln gs_dop_sel-posnr INTO lv_objky.

  CLEAR: nast, xnast, lt_nast.

* ZDOP-Nachricht ermitteln
  CLEAR lt_nast.

  SELECT  * FROM nast INTO TABLE lt_nast
    WHERE kappl = 'V2' AND
          objky = lv_objky AND
          kschl = 'ZDOP'.

  CHECK NOT lt_nast IS INITIAL.

*  immer älteste Nachrich ausgeben
  SORT lt_nast BY erdat eruhr.

  CLEAR ls_nast.
  READ TABLE lt_nast INTO ls_nast INDEX 1.

  CLEAR xnast.
  REFRESH xnast.

  MOVE-CORRESPONDING ls_nast TO xnast.
  APPEND xnast.

  IF xnast-optarcnr IS INITIAL.
    MESSAGE i048(vn) WITH xnast-kschl.
    EXIT.
  ENDIF.

  xobjid-objky = ls_nast-objky.
* Ermitteln der Objektart
  CALL FUNCTION 'WFMC_GET_ARCHIVE_OBJECT_TYPE'
    EXPORTING
      pi_kappl        = ls_nast-kappl
      pi_objtype      = ls_nast-objtype
    IMPORTING
      pe_object_type  = object_na
      pe_object_type2 = object_na2
    CHANGING
      pc_objky        = xobjid-objky.

* Aufbau OBJECT_ID
  xobjid-arcnr = xnast-optarcnr.
  object_id = xobjid.
  ls_object-objecttype = object_na.
  ls_object-objectid   = object_id.
  APPEND ls_object TO lt_object.
  IF NOT object_na2 IS INITIAL.
    ls_object-objecttype = object_na2.
    APPEND ls_object TO lt_object.
  ENDIF.

  CALL FUNCTION 'OBJECT_DISPLAY_CONNECTIONS'
*   EXPORTING
*     OBJECTTYPE       = ' '
*     OBJECTID         = ' '
    TABLES
      object    = lt_object
      parameter = lt_parameters
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " ARCHIVE_ANZEIGE

*&---------------------------------------------------------------------*
*&      Form  GET_NACHRICHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_OBJKY  text
*      <--P_GS_NAST  text
*----------------------------------------------------------------------*
FORM get_nachricht  USING    pv_vbeln TYPE vbeln_vl
                             pv_uecha TYPE uecha
                             pv_posnr TYPE posnr_vl
                             pv_lifnr TYPE elifn
                    CHANGING ps_nast TYPE t_nast.

  DATA: lv_objky TYPE na_objkey.
  DATA: ls_nast TYPE t_nast.
  DATA: lt_nast TYPE STANDARD TABLE OF t_nast.

  DATA: ls_lina TYPE t_lina.

  CLEAR ps_nast.

* Nachricht ermitteln
  READ TABLE gt_lina INTO ls_lina WITH KEY
    vbeln = pv_vbeln
    uecha = pv_uecha
    lifnr = pv_lifnr.

* nachricht gefunden
  IF sy-subrc EQ 0.
    ps_nast-kappl = ls_lina-kappl.
    ps_nast-objky = ls_lina-objky.
    ps_nast-kschl = ls_lina-kschl.
    ps_nast-erdat = ls_lina-erdat.
    ps_nast-eruhr = ls_lina-eruhr.
    ps_nast-optarcnr = ls_lina-optarcnr.
    ps_nast-objtype = ls_lina-objtype.
    EXIT.
  ENDIF.

* evtl. Nachricht in Tabele NAST vorhanden
  CLEAR lt_nast.

  CONCATENATE pv_vbeln pv_uecha INTO lv_objky.

  SELECT kappl objky kschl erdat eruhr optarcnr
    FROM nast INTO TABLE lt_nast
    WHERE kappl = 'V2' AND
          objky = lv_objky AND
          kschl = 'ZDOP' AND
          vstat = '1'.  "erfolgreich verarbeitet

  CHECK sy-subrc EQ 0.

* älteste Nachricht ermitteln
  SORT lt_nast BY erdat eruhr.

  READ TABLE lt_nast INTO ls_nast INDEX 1.

  CLEAR ls_lina.

*  Neue Nachricht in Tabelle gt_lina hinzufügen
  ls_lina-vbeln = pv_vbeln.
  ls_lina-posnr = pv_posnr.
  ls_lina-lifnr = pv_lifnr.
  ls_lina-objky = ls_nast-objky.
  ls_lina-erdat = ls_nast-erdat.
  ls_lina-eruhr = ls_nast-eruhr.
  ls_lina-optarcnr = ls_nast-optarcnr.
  ls_lina-objtype = ls_nast-objtype.

  APPEND ls_lina TO gt_lina.

*  Nachricht zurückgeben
  ps_nast = ls_nast.

ENDFORM.                    " GET_NACHRICHT


*&---------------------------------------------------------------------*
*&      Form  SELECTION_MATERIAL_FUER_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*      <--P_GV_TEXT  text
*----------------------------------------------------------------------*
FORM selection_material_fuer_pdf  CHANGING pv_subrc TYPE sysubrc
                                           pv_text TYPE string.


* https://de.scribd.com/doc/454814/SAPSCRIPT-to-PDF

  DATA: ls_options TYPE itcpo.
  DATA: lt_datab TYPE TABLE OF itcoo WITH HEADER LINE.
  DATA: ls_result TYPE itcpp.
  DATA: lt_pdftab TYPE TABLE OF tline WITH HEADER LINE.
  DATA: lt_pdftab_all TYPE TABLE OF tline WITH HEADER LINE.
  DATA: lv_binfilesize TYPE i.
  DATA: lv_file_name TYPE string.
  DATA: lv_file_path TYPE string.
  DATA: lv_full_path TYPE string.
  DATA: pdf_data         TYPE xstring.
  DATA: pdf_size         TYPE i.
  DATA: us_screen.
  DATA: return_code.
  DATA: lv_answer TYPE c.
  DATA: lv_default_extension TYPE string.
  DATA: lv_default_file_name  TYPE string.
  DATA: lv_matnr TYPE matnr.
  DATA: lv_lifnr TYPE elifn.
  DATA: lv_spras_2 TYPE char2.
  DATA: lv_vgl_matnr TYPE matnr.
  DATA: lv_filemask_lifke_sep TYPE string.
  DATA: lt_otfdata_all TYPE TABLE OF itcoo WITH HEADER LINE.
  DATA: lv_len TYPE i.
  DATA: lv_anz_stellen TYPE i.


  CONSTANTS: lc_popup_yes TYPE c VALUE '1'.
  CONSTANTS: lc_spras_form TYPE spras VALUE 'D'.

  TYPES: BEGIN OF t_t002,
           spras TYPE spras,
         END OF t_t002.

  DATA: ls_t002 TYPE t_t002.
  DATA: lt_t002 TYPE TABLE OF t_t002.

  DATA: ls_labeling TYPE ztmm_labeling.
  DATA: lt_labeling TYPE TABLE OF ztmm_labeling.

  CLEAR: ztmm_labeling, lt_labeling, lt_t002, pv_text.

* Filemaskierung ermitteln.
  IF p_fimask IS INITIAL.
    gv_index = 1.
  ELSE.
    gv_index = p_fimask.
  ENDIF.

  READ TABLE gt_list INTO gs_list INDEX gv_index.
  gv_fimask = gs_list-text.

  CONCATENATE gc_filemask_lifke gc_filemask_sep INTO lv_filemask_lifke_sep
  IN CHARACTER MODE.

  CLEAR tnapr.

  SELECT SINGLE * FROM tnapr
    WHERE kschl = gc_kschl_zdop AND
          nacha = '1' AND
          kappl = 'V2'.

  SELECT * FROM ztmm_labeling INTO TABLE lt_labeling
    WHERE matnr IN s_matnr2 AND
          lifnr IN s_lifnr2 AND
          datbi EQ gc_datbi_aktuell AND  "31.12.9999
          actsg EQ gc_aktiv.

  SORT lt_labeling BY matnr werks lifnr.

* Selektierte Sprachen aufbereiten, nötig, falls keine Einzelwerte
* eingegeben werden. Bei Einzelwerten wird nach der Eingabenreihenfolge
* sortiert.
  CLEAR: gv_counter, lt_t002.

***  select spras from t002 into table lt_t002
***    where spras in s_spras2.

* DoP-Tabelle: im SAPScript-Formular wird die Struktur ztmm_labeling
* angesprochen
  LOOP AT lt_labeling INTO ztmm_labeling.

    lv_len = strlen( ztmm_labeling-matnr ).

*   Es werden nur 9 Stellen bei den 10-stellgen Materialien berücksichtigt,
*   daher nur 1x pro 10 Stellen
    IF lv_len EQ 10.
      lv_anz_stellen = 9.
      gv_matnr = ztmm_labeling-matnr(9).
    ELSE.
      lv_anz_stellen = 18.
      gv_matnr = ztmm_labeling-matnr.
    ENDIF  .

    CHECK ztmm_labeling-matnr(lv_anz_stellen) NE lv_vgl_matnr(lv_anz_stellen).

    lv_vgl_matnr = gv_matnr.

*   Sprachen
*    loop at lt_t002 into ls_t002.
    LOOP AT s_spras2.

      ls_t002-spras = s_spras2-low.

      CLEAR nast.

      nast-kschl = gc_kschl_zdop.
      nast-spras = ls_t002-spras.
      gv_spras_text = ls_t002-spras.
      nast-ldest = p_ldest.
      gv_print_logo = gc_mark.

*     Signatur zusammensetzen
      CONCATENATE 'SIGN_' ztmm_labeling-bname INTO gv_sign.

      CLEAR retcode.
      xscreen = us_screen.

      PERFORM get_data_nachdruck_fs1.

* ---------------------------------------------------------------------
*     Umwandlung OTF nach PDF
* ---------------------------------------------------------------------
*     in PDF konvertieren und speichern als Dialog
      CLEAR ls_options.
      ls_options-tddest = nast-ldest.
      ls_options-tdnoprev = 'X'.
      ls_options-tdgetotf = 'X'.  "OTF-Tabelle zurückgeben
      PERFORM processing_via_material USING ls_options.

*----------------------------------------------------------------------
*   SAPScript OTF in PDF umwandeln
*----------------------------------------------------------------------

      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = gc_format
          pdf_preview           = 'X'
        IMPORTING
          bin_filesize          = lv_binfilesize
*         bin_file              = pdf_data
        TABLES
          otf                   = gt_otfdata[]
          lines                 = lt_pdftab[]
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          OTHERS                = 5.

      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

*       vollständigen Filenamen ermitteln
      PERFORM get_full_path USING    ls_t002-spras
                            CHANGING lv_full_path.

*----------------------------------------------------------------------
*       Sichern als einzelnes PDF
*----------------------------------------------------------------------
      PERFORM save_as_pdf TABLES lt_pdftab
                           USING lv_binfilesize
                                 lv_full_path.

*       SAP-Script pro Material und selektierte Sprachen
      LOOP AT gt_otfdata.
        APPEND gt_otfdata TO lt_otfdata_all.
      ENDLOOP.

    ENDLOOP.  "lt_t002

*----------------------------------------------------------------------
*   Sichern PDF: Mehrere Sprachen pro Material in ein PDF
*----------------------------------------------------------------------
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = gc_format
        pdf_preview           = 'X'
      IMPORTING
        bin_filesize          = lv_binfilesize
*       bin_file              = pdf_data
      TABLES
        otf                   = lt_otfdata_all[]
        lines                 = lt_pdftab_all[]
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

*   vollständigen Filenamen ermitteln
    PERFORM get_full_path USING    space "ohne Sprache
                          CHANGING lv_full_path.


    PERFORM save_as_pdf TABLES lt_pdftab_all
                         USING lv_binfilesize
                               lv_full_path.

    CLEAR: lt_otfdata_all, lt_pdftab_all, lv_full_path.
    REFRESH: lt_otfdata_all, lt_pdftab_all.

  ENDLOOP.  "lt_labeling

ENDFORM.                    " SELECTION_MATERIAL_FUER_PDF


*---------------------------------------------------------------------*
*       FORM PROCESSING_FS1                                           *
*---------------------------------------------------------------------*
*       Ablaufsteuerung für den Lieferschein                          *
*---------------------------------------------------------------------*
*FORM processing_nachdruck_fs1 USING proc_screen.
FORM nachdruck_via_material USING pv_spras TYPE na_spras.

* https://de.scribd.com/doc/454814/SAPSCRIPT-to-PDF

  DATA: ls_options TYPE itcpo.
  DATA: lt_datab TYPE TABLE OF itcoo WITH HEADER LINE.
  DATA: ls_result TYPE itcpp.
  DATA: lt_pdftab TYPE TABLE OF tline WITH HEADER LINE.
  DATA: lv_binfilesize TYPE i.
  DATA: lv_file_name TYPE string.
  DATA: lv_file_path TYPE string.
  DATA: lv_full_path TYPE string.
  DATA:  pdf_data         TYPE xstring.
  DATA:  pdf_size         TYPE i.
  DATA: us_screen.
  DATA: return_code.
  DATA:lv_answer TYPE c.
  DATA: lv_default_extension TYPE string.
  DATA: lv_default_file_name  TYPE string.
  DATA: lv_matnr TYPE matnr.
  DATA: lv_lifnr TYPE elifn.
  DATA: lv_spras_2 TYPE char2.
  DATA: lv_len TYPE i.

  CONSTANTS: lc_popup_yes TYPE c VALUE '1'.
  CONSTANTS: lc_spras_form TYPE spras VALUE 'D'.

  CLEAR: nast, tnapr.

  nast-kschl = gc_kschl_zdop.
  nast-spras = pv_spras.
  gv_spras_text = pv_spras.
  nast-ldest = p_ldest.
  gv_print_logo = gc_mark.

* 10-stellige Materialnummer nur 9-stellig auf dem Formular ausgeben
  gv_matnr = ztmm_labeling-matnr.

  lv_len = strlen( gv_matnr ).

  IF lv_len EQ 10.
    gv_matnr = gv_matnr(9).
  ENDIF.

  CLEAR tnapr.

  SELECT SINGLE * FROM tnapr
    WHERE kschl = gc_kschl_zdop AND
          nacha = '1' AND
          kappl = 'V2'.

*  --- Signatur zusammensetzen.
  CONCATENATE 'SIGN_' ztmm_labeling-bname INTO gv_sign.

  CLEAR retcode.
  xscreen = us_screen.

  PERFORM get_data_nachdruck_fs1.

* ---------------------------------------------------------------------
* Ansicht auf Bildschirm
* ---------------------------------------------------------------------
  ls_options-tdcopies = '1'.
  ls_options-tddest = nast-ldest.
  ls_options-tdpreview = 'X'.
  ls_options-tdarmod = '1'.
  ls_options-tdnewid = 'X'.  "Neuer Spoolauftrag
  ls_options-tdimmed = 'X'.  "Sofort Drucken
  PERFORM processing_via_material USING ls_options.

* ---------------------------------------------------------------------
* Soll PDF erzeugt und gesichert werden?
* ---------------------------------------------------------------------
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = 'DoP als PDF sichern?'(P01)
      text_button_1  = 'Ja'(P02)
      text_button_2  = 'Nein'(P03)
      default_button = '2'
*     DISPLAY_CANCEL_BUTTON = lv_cancel
    IMPORTING
      answer         = lv_answer
    EXCEPTIONS
      OTHERS         = 2.

  CHECK lv_answer EQ lc_popup_yes.

* ---------------------------------------------------------------------
* Umwandlung OTF nach PDF
* ---------------------------------------------------------------------
* in PDF konvertieren und speichern als Dialog
  CLEAR ls_options.
  ls_options-tddest = nast-ldest.
  ls_options-tdnoprev = 'X'.
  ls_options-tdgetotf = 'X'.  "OTF-Tabelle zurückgeben
  PERFORM processing_via_material USING ls_options.

*----------------------------------------------------------------------
* SAPScript OTF in PDF umwandeln
*----------------------------------------------------------------------

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = gc_format
      pdf_preview           = 'X'
***    MAX_LINEWIDTH = 132
***    ARCHIVE_INDEX = ' '
***    COPYNUMBER = 0
***    ASCII_BIDI_VIS2LOG = ' '
***    PDF_DELETE_OTFTAB = ' '
    IMPORTING
      bin_filesize          = lv_binfilesize
*     bin_file              = pdf_data
    TABLES
      otf                   = gt_otfdata[]
      lines                 = lt_pdftab[]
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* Vorschlagswerte
  WRITE gv_matnr TO lv_matnr NO-ZERO.

* Sprachcode von 1- nach 2-stellig umwandeln
  CLEAR lv_spras_2.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
    EXPORTING
      input  = p_spras
    IMPORTING
      output = lv_spras_2.


  IF ztmm_labeling-lifke IS INITIAL.
    CONCATENATE 'DOP-' lv_matnr '-' ztmm_labeling-datab '-' lv_spras_2
    INTO lv_default_file_name IN CHARACTER MODE.
  ELSE.
    CONCATENATE 'DOP-' lv_matnr '-' ztmm_labeling-datab '-'
                 ztmm_labeling-lifke '-' lv_spras_2
    INTO lv_default_file_name IN CHARACTER MODE.
  ENDIF.

  lv_default_extension = gc_format.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension = lv_default_extension
      default_file_name = lv_default_file_name
    CHANGING
      filename          = lv_file_name
      path              = lv_file_path
      fullpath          = lv_full_path.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------
* Sichern
*----------------------------------------------------------------------
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_binfilesize
      filename                = lv_full_path
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_pdftab[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

ENDFORM.                     "PROCESSING_FS1

*---------------------------------------------------------------------*
*       FORM GET_DATA_NACHDRUCK_FS1                                   *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM get_data_nachdruck_fs1.

  DATA: lv_subrc TYPE sysubrc.

***  CLEAR: komk, vbco3, tvbdpl, tvbplk, tvbplp,
***         komp, vbdkl, tvbpls, tvbdkl, svbpla, gv_sign, gv_spras_text.
***  REFRESH: tvbdpl, tvbplk, tvbplp, tvbpls.

* Vkorg über Werkstabelle t001w ermitteln
  CLEAR gv_vkorg.

  SELECT SINGLE vkorg FROM t001w INTO gv_vkorg
    WHERE werks = ztmm_labeling-werks_dop.
***   WHERE werks = ztmm_labeling-werks.

* Bereitstellung der Customizing-Parameter in ZSDFS1
***  PERFORM get_zsdfs1_customizing USING gv_vkorg
***                                       nast-kschl.
  PERFORM get_zsdfs1_customizing CHANGING lv_subrc.

*Adresse des Werks holen.
*** PERFORM read_adr_werk USING ztmm_labeling-werks.
  PERFORM read_adr_werk USING ztmm_labeling-werks_dop.

*--- Signatur zusammensetzen.
  CONCATENATE 'SIGN_' ztmm_labeling-bname INTO gv_sign.

**SELECT SINGLE SIGN_TXT FROM ZDOP_WRKSIGN
**  INTO GV_SIGN
**  WHERE WERKS = ztmm_labeling-werks.

* Etikettentext ermitteln.
  PERFORM read_etik_text USING ztmm_labeling-matnr.

* fill address key --> necessary for emails
******  addr_key-addrnumber = vbdkl-adrnr.
******  addr_key-persnumber = vbdkl-adrnp.
******  addr_key-addr_type  = vbdkl-address_type.

*--- Sprachcode des Warenempfängers für Formulartexte ermitteln
  gv_spras_text = nast-spras.

*>>>  gv_spras_text = 'G'.

ENDFORM.               "GET_DATA_NACHDRUCK_FS1

*&---------------------------------------------------------------------*
*&      Form  PROCESSING_VIA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OPTIONS  text
*----------------------------------------------------------------------*
FORM processing_via_material  USING    ps_options TYPE itcpo.

  DATA: ls_result TYPE itcpp.
  DATA: us_screen.
  DATA: return_code.
  CONSTANTS: lc_spras_form TYPE spras VALUE 'D'.

* Formular speziell für Dialog öffnen
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device                      = 'PRINTER'
      form                        = tnapr-fonam
      language                    = lc_spras_form "Formular in Sprachee DE hinterlegt
      options                     = ps_options
      dialog                      = space
    IMPORTING
      result                      = ls_result
    EXCEPTIONS
      canceled                    = 1
      device                      = 2
      form                        = 3
      options                     = 4
      unclosed                    = 5
      mail_options                = 6
      archive_error               = 7
      invalid_fax_number          = 8
      more_params_needed_in_batch = 9
      spool_error                 = 10
      codepage                    = 11
      OTHERS                      = 12.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  PERFORM item_print_fs1.
  CHECK retcode = 0.
  PERFORM end_print.
  CHECK retcode = 0.

* Formular schliessen
  CALL FUNCTION 'CLOSE_FORM'
    TABLES
      otfdata                  = gt_otfdata[]
    EXCEPTIONS
      unopened                 = 1
      bad_pageformat_for_print = 2
      send_error               = 3
      spool_error              = 4
      codepage                 = 5
      OTHERS                   = 6.

ENDFORM.                    " PROCESSING_VIA_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  AUSGABE_VORHANDENE_DOP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausgabe_vorhandene_dop .

  DATA: lt_out TYPE STANDARD TABLE OF zsmm_labeling_alv.
  DATA: ls_out TYPE zsmm_labeling_alv.

  DATA: ls_labeling TYPE ztmm_labeling.
  DATA: ls_layout_lvc TYPE lvc_s_layo.

  IF s_matnr IS INITIAL.
    SET CURSOR FIELD 'S_MATNR-LOW'.
    MESSAGE e001(aq) WITH text-e08.
  ENDIF.

  CLEAR lt_out.

  IF p_lifnr IS INITIAL AND
     p_datam IS INITIAL.
*   Eingabe Materialnummer
    SELECT * FROM ztmm_labeling INTO ls_labeling
      WHERE matnr IN s_matnr.
      MOVE-CORRESPONDING ls_labeling TO ls_out.
      APPEND ls_out TO lt_out.
    ENDSELECT.
  ELSEIF p_lifnr IS INITIAL.
*   Eingabe Materialnummer und Datum gültig am
    SELECT * FROM ztmm_labeling INTO ls_labeling
      WHERE matnr IN s_matnr AND
            datab LE p_datam  AND
            datbi GE  p_datam.
      MOVE-CORRESPONDING ls_labeling TO ls_out.
      APPEND ls_out TO lt_out.
    ENDSELECT.
  ELSEIF p_datam IS INITIAL.
*   Eingabe Materialnummer und Lieferantennummer
    SELECT * FROM ztmm_labeling INTO ls_labeling
      WHERE matnr IN s_matnr AND
            lifnr EQ  p_lifnr.
      MOVE-CORRESPONDING ls_labeling TO ls_out.
      APPEND ls_out TO lt_out.
    ENDSELECT.
  ELSE.
*   Eingabe Materialnummer, Lieferantennummer und Datum gltig ab
    SELECT * FROM ztmm_labeling INTO ls_labeling
      WHERE matnr IN s_matnr AND
            lifnr EQ  p_lifnr AND
            datab LE p_datam  AND
            datbi GE  p_datam.
      MOVE-CORRESPONDING ls_labeling TO ls_out.
      APPEND ls_out TO lt_out.
    ENDSELECT.
  ENDIF.

   IF sy-subrc NE 0.
     MESSAGE e001(aq) WITH text-e13.
   ENDIF.

  CLEAR ls_layout_lvc.

* Optimale Spaltenbreite setzen
  MOVE gc_on TO ls_layout_lvc-cwidth_opt.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_structure_name = 'ZSMM_LABELING_ALV'
      is_layout_lvc    = ls_layout_lvc
    TABLES
      t_outtab         = lt_out
    EXCEPTIONS
      OTHERS           = 2.

ENDFORM.                    " AUSGABE_VORHANDENE_DOP

*&---------------------------------------------------------------------*
*&      Form  EINGABEN_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*      <--P_GV_MSGTEXT  text
*----------------------------------------------------------------------*
FORM eingaben_1.

  DATA: lv_aufnr TYPE aufnr.
  DATA: lv_objek TYPE cuobn.
  DATA: lv_matnr TYPE matnr.
  DATA: lv_charg TYPE charg_d.
  DATA: lv_sel TYPE c.
  DATA: lv_atinn TYPE atinn.
  DATA: lv_padest TYPE rspopshort.

  TYPES: BEGIN OF t_mch1_mchb,
           matnr TYPE matnr,
           werks TYPE werks_d,
           lgort TYPE lgort_d,
           charg TYPE charg_d,
           zzaufnr TYPE aufnr,
           zzebeln TYPE ebeln,
           zzebelp TYPE ebelp,

         END OF t_mch1_mchb.

  DATA: ls_mch1_mchb TYPE t_mch1_mchb.
  DATA: lt_mch1_mchb TYPE TABLE OF t_mch1_mchb.
  DATA: lv_anz_lines TYPE i.

  CLEAR: gs_in, ztmm_labeling.

  IF p_ldest IS INITIAL.
    SET CURSOR FIELD 'P_LDEST'.
    MESSAGE e124(po).
  ELSE.
    SELECT SINGLE padest FROM tsp03l INTO lv_padest
      WHERE padest = p_ldest.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'P_LDEST'.
        MESSAGE e366(po) WITH p_ldest.
      ENDIF.
  ENDIF.

  CASE gc_on.

* ---------------------------------------------------------------------
*   Fertigungsauftrag
* ---------------------------------------------------------------------
    WHEN p_rbauf.
      IF p_aufnr IS INITIAL.
        SET CURSOR FIELD 'P_AUFNR'.
        MESSAGE e001(aq) WITH text-e01.
      ENDIF.

      IF p_spras IS INITIAL.
        SET CURSOR FIELD 'P_SPRAS'.
        MESSAGE e001(aq) WITH text-e05.
      ENDIF.

***      IF p_spras1 IS INITIAL.
***        SET CURSOR FIELD 'P_SPRAS1'.
***        MESSAGE e001(aq) WITH text-e05.
***      ENDIF.

      gv_spras_text = p_spras.
***      gv_spras_text = p_spras1.

***      IF p_ldes1 IS INITIAL.
***        SET CURSOR FIELD 'P_LDES1'.
***        MESSAGE e124(po).
***      ENDIF.

      SELECT SINGLE afpo~matnr INTO gs_in-matnr
        FROM afko INNER JOIN afpo
        ON afko~aufnr = afpo~aufnr
        WHERE afko~aufnr = p_aufnr.

      IF sy-subrc NE 0.
        MESSAGE e017(co) WITH p_aufnr.
      ENDIF.

      MOVE p_aufnr     TO p_infauf.
      MOVE gs_in-matnr TO p_infmat.

      PERFORM get_datum_via_vorgang USING p_aufnr
                                 CHANGING gs_in-date.

      IF gs_in-date IS INITIAL.
        MESSAGE e001(aq) WITH text-e12.
      ELSE.
        MOVE gs_in-date  TO p_infeda.
      ENDIF.

      PERFORM get_data_dop USING gs_in-matnr
                                 space
                                 space
                                 gs_in-date
                       CHANGING  ztmm_labeling.

      IF ztmm_labeling IS INITIAL.
        MESSAGE e001(aq) WITH text-e13.
      ENDIF.

* ---------------------------------------------------------------------
*   Kabel-ID: Diese kann in verschiedenen Chargen mit derselben
*   Materialnummer sein, welche nur einenm Fertigungsauftrag zugewiesen
*   sind
* ---------------------------------------------------------------------
    WHEN p_rbkaid.
      IF p_kabid IS INITIAL.
        SET CURSOR FIELD 'P_KABID'.
        MESSAGE e001(aq) WITH text-e02.
      ENDIF.

      IF p_spras IS INITIAL.
        SET CURSOR FIELD 'P_SPRAS'.
        MESSAGE e001(aq) WITH text-e05.
      ENDIF.

***      IF p_spras1 IS INITIAL.
***        SET CURSOR FIELD 'P_SPRAS1'.
***        MESSAGE e001(aq) WITH text-e05.
***      ENDIF.

      gv_spras_text = p_spras.
***      gv_spras_text = p_spras1.

***      IF p_ldes1 IS INITIAL.
***        SET CURSOR FIELD 'P_LDES1'.
***        MESSAGE e124(po).
***      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          INPUT         = 'ZDAG_ID'
       IMPORTING
         OUTPUT        = lv_atinn.

      SELECT SINGLE inob~objek INTO lv_objek
        FROM ausp INNER JOIN inob
        ON ausp~objek = inob~cuobj
        WHERE ausp~atinn =  lv_atinn AND
              ausp~klart = '023' AND
              ausp~atwrt = p_kabid.

      IF sy-subrc NE 0 OR
       lv_objek IS INITIAL.
        MESSAGE e001(aq) WITH text-e14.
      ENDIF.

      lv_matnr = lv_objek(18).
      lv_charg = lv_objek+18(10).

      IF lv_charg IS INITIAL.
        MESSAGE e135(w_cb) WITH lv_matnr lv_charg.
      ENDIF.

      SELECT SINGLE zzaufnr FROM mch1 INTO lv_aufnr
        WHERE matnr = lv_matnr AND
              charg = lv_charg.

      IF sy-subrc NE 0 OR
       lv_objek IS INITIAL.
        MESSAGE e001(aq) WITH text-e14.
      ENDIF.

      SELECT SINGLE afpo~matnr INTO gs_in-matnr
        FROM afko INNER JOIN afpo
        ON afko~aufnr = afpo~aufnr
        WHERE afko~aufnr = lv_aufnr.

      IF sy-subrc NE 0.
        MESSAGE e017(co) WITH p_aufnr.
      ENDIF.

      MOVE gs_in-matnr TO p_infmat.
      MOVE lv_aufnr    TO p_infauf.
      MOVE lv_charg    TO p_infchr.

      PERFORM get_datum_via_vorgang USING lv_aufnr
                                 CHANGING gs_in-date.

      IF gs_in-date IS INITIAL.
        MESSAGE e001(aq) WITH text-e12.
      ELSE.
        MOVE gs_in-date  TO p_infeda.
      ENDIF.

      PERFORM get_data_dop USING gs_in-matnr
                                 space
                                 space
                                 gs_in-date
                        CHANGING ztmm_labeling.

      IF ztmm_labeling IS INITIAL.
        MESSAGE e001(aq) WITH text-e13.
      ENDIF.

* ---------------------------------------------------------------------
*   Charge
* ---------------------------------------------------------------------
    WHEN p_rbchrg.
      IF p_charg IS INITIAL.
        SET CURSOR FIELD 'P_CHARG'.
        MESSAGE e001(aq) WITH text-e03.
      ENDIF.

      IF p_spras IS INITIAL.
        SET CURSOR FIELD 'P_SPRAS'.
        MESSAGE e001(aq) WITH text-e05.
      ENDIF.

***      IF p_spras1 IS INITIAL.
***        SET CURSOR FIELD 'P_SPRAS1'.
***        MESSAGE e001(aq) WITH text-e05.
***      ENDIF.

      gv_spras_text = p_spras.
***      gv_spras_text = p_spras1.

***      IF p_ldes1 IS INITIAL.
***        SET CURSOR FIELD 'P_LDES1'.
***        MESSAGE e124(po).
***      ENDIF.

      CLEAR lt_mch1_mchb.

      SELECT mch1~matnr mchb~werks mchb~lgort mch1~charg
             mch1~zzaufnr zzebeln zzebelp
        INTO table lt_mch1_mchb
        FROM mch1 INNER JOIN mchb
        ON mch1~matnr = mchb~matnr AND
           mch1~charg = mchb~charg
        WHERE mch1~charg = p_charg.

      DESCRIBE TABLE lt_mch1_mchb LINES lv_anz_lines.

      CASE lv_anz_lines.
        WHEN 0. "keine Daten
          MESSAGE e260(aq).
***        WHEN 1. "Charge eindeutig
        WHEN OTHERS.
          READ TABLE lt_mch1_mchb INTO ls_mch1_mchb INDEX 1.

*         Fertigungsauftrag
          IF ls_mch1_mchb-zzaufnr IS NOT INITIAL.
            SELECT SINGLE afpo~matnr INTO gs_in-matnr
              FROM afko INNER JOIN afpo
              ON afko~aufnr = afpo~aufnr
              WHERE afko~aufnr = ls_mch1_mchb-zzaufnr.

            IF sy-subrc NE 0.
              MESSAGE e017(co) WITH p_aufnr.
            ENDIF.

            MOVE gs_in-matnr          TO p_infmat.
            MOVE ls_mch1_mchb-zzaufnr TO p_infauf.

            MOVE gs_in-date           TO p_infeda.

            PERFORM get_datum_via_vorgang USING ls_mch1_mchb-zzaufnr
                                       CHANGING gs_in-date.

            IF gs_in-date IS INITIAL.
              MESSAGE e001(aq) WITH text-e12.
            ELSE.
              MOVE gs_in-date  TO p_infeda.
            ENDIF.

            MOVE ls_mch1_mchb-charg   TO p_infchr.

*           Bestellung
            ELSEIF ls_mch1_mchb-zzebeln IS NOT INITIAL.
              SELECT SINGLE ekpo~matnr ekbe~budat
                INTO (gs_in-matnr, gs_in-date)
                FROM ekpo INNER JOIN ekbe
                ON ekpo~ebeln = ekbe~ebeln AND
                   ekpo~ebelp = ekbe~ebelp
                WHERE ekpo~ebeln = ls_mch1_mchb-zzebeln AND
                      ekpo~ebelp = ls_mch1_mchb-zzebelp AND
                      ekbe~bewtp = 'E' AND
                      ekbe~charg = p_charg.

            IF sy-subrc NE 0.
              MESSAGE e040(grpg) WITH ls_mch1_mchb-zzebeln ls_mch1_mchb-zzebelp.
            ENDIF.

            MOVE gs_in-matnr          TO p_infmat.
            MOVE gs_in-date           TO p_infwda.
            MOVE ls_mch1_mchb-charg   TO p_infchr.
            MOVE ls_mch1_mchb-zzebeln TO p_infebe.
            MOVE ls_mch1_mchb-zzebelp TO p_infebp.

            IF gs_in-date IS INITIAL.
              MESSAGE e001(aq) WITH text-e12.
            ENDIF.

          ELSE.
            MESSAGE e001(aq) WITH text-e15.

          ENDIF.

***        WHEN OTHERS. "Charge nicht eindeutig
***          MESSAGE e001(aq) WITH text-e11.
      ENDCASE.

    PERFORM get_data_dop USING gs_in-matnr
                               space
                               space
                               gs_in-date
                     CHANGING  ztmm_labeling.

    IF ztmm_labeling IS INITIAL.
      MESSAGE e001(aq) WITH text-e13.
   ENDIF.

* ---------------------------------------------------------------------
*   Lieferung / Pos
* ---------------------------------------------------------------------
    WHEN p_rbvbel.
      IF p_vbeln IS INITIAL.
        SET CURSOR FIELD 'P_VBELN'.
        MESSAGE e001(aq) WITH text-e04.
      ENDIF.

***      PERFORM selection_via_lieferung CHANGING gv_subrc.
***
***      IF gv_subrc NE 0.
***        MESSAGE e001(aq) WITH text-e17.
***      ENDIF.
***

  ENDCASE.

ENDFORM.                    " EINGABEN_1

*&---------------------------------------------------------------------*
*&      Form  EINGABEN_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*      <--P_GV_MSGTEXT  text
*----------------------------------------------------------------------*
FORM eingaben_2.

  DATA: lv_len TYPE i.
  DATA: lv_padest TYPE rspopshort.

  IF s_matnr IS INITIAL.
    SET CURSOR FIELD 'P_MATNR-LOW'.
    MESSAGE e001(aq) WITH text-e08.
  ENDIF.

  IF p_datam IS INITIAL.
    SET CURSOR FIELD 'P_DATAM'.
    MESSAGE e001(aq) WITH text-e16.
  ENDIF.

  IF p_spras IS INITIAL.
    SET CURSOR FIELD 'P_SPRAS'.
    MESSAGE e001(aq) WITH text-e05.
  ENDIF.

  gv_spras_text = p_spras.

  IF p_ldest IS INITIAL.
    SET CURSOR FIELD 'P_LDEST'.
    MESSAGE e124(po).
  ELSE.
    SELECT SINGLE padest FROM tsp03l INTO lv_padest
      WHERE padest = p_ldest.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'P_LDEST'.
        MESSAGE e366(po) WITH p_ldest.
      ENDIF.
  ENDIF.

  CLEAR: ztmm_labeling, gv_matnr.

  SELECT SINGLE * FROM ztmm_labeling
    WHERE matnr IN s_matnr AND
          lifnr EQ p_lifnr AND
          datab LE p_datam  AND
          datbi GE  p_datam AND
          actsg EQ gc_aktiv.

* Keine DoP-Daten in Tabelle ztmm_labeling vorhanden
  IF sy-subrc NE 0.
    MESSAGE e001(aq) WITH text-e09.
  ENDIF.

* Flag "Aktiv in Bentzung" nicht gesetzt
  IF ztmm_labeling-actsg IS INITIAL.
    MESSAGE e001(aq) WITH text-e16.
  ENDIF.

* Werk-Ersteller DoP fehlt
  IF ztmm_labeling-werks_dop IS INITIAL.
    MESSAGE e001(aq) WITH text-e10.
  ENDIF.

* 10-stellige Materialnummer nur 9-stellig auf dem Formular ausgeben
  gv_matnr = ztmm_labeling-matnr.

  lv_len = strlen( gv_matnr ).

  IF lv_len EQ 10.
    gv_matnr = gv_matnr(9).
  ENDIF.

  WRITE ztmm_labeling-matnr TO gs_dyn_2000-plnbez NO-ZERO.
  gs_dyn_2000-prodat = p_datam.

ENDFORM.                    " EINGABEN_2

*&---------------------------------------------------------------------*
*&      Form  CHECK_EINGABEN_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*      <--P_GV_MSGTEXT  text
*----------------------------------------------------------------------*
FORM eingaben_3.

  IF s_matnr2[] IS INITIAL.
    SET CURSOR FIELD 'S_MATNR2-LOW'.
    MESSAGE e001(aq) WITH text-e08.
  ENDIF.

  IF s_spras2[] IS INITIAL.
    SET CURSOR FIELD 'S_SPRAS2-LOW'.
    MESSAGE e001(aq) WITH text-e05.
  ENDIF.

 IF p_path IS INITIAL.
    SET CURSOR FIELD 'P_PATH'.
    MESSAGE e044(sctc_sc).
  ENDIF.

ENDFORM.                    " EINGABEN_3


*&---------------------------------------------------------------------*
*&      Form  GET_DATUM_VIA_VORGANG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_datum_via_vorgang USING pv_aufnr TYPE AUFNR
                        CHANGING pv_date TYPE dats.

  DATA: ls_afvv TYPE afvv.
  DATA: lt_afvv TYPE STANDARD TABLE OF afvv.

  DATA: lv_tabix TYPE i.

  CLEAR pv_date.

  CALL FUNCTION 'CO_SF_AFVG_READ'
   EXPORTING
     AUFNR_IMP               = pv_aufnr
   TABLES
     TAFVV_EXP               = lt_afvv
   EXCEPTIONS
     NOT_FOUND               = 1
     MISSING_PARAMETER       = 2
     OTHERS                  = 3.

  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.


* Ermitteln Datum Spätestes Ende Vorgang:
* - Tabelle rückwärts lesen, bis Datum gefunden
  DESCRIBE TABLE lt_afvv LINES lv_tabix.

  WHILE lv_tabix GT 0.
    READ TABLE lt_afvv INTO ls_afvv INDEX lv_tabix.

    IF ls_afvv-iedd IS NOT INITIAL.
      pv_date = ls_afvv-iedd.
      lv_tabix = 0.
    ELSE.
      lv_tabix = lv_tabix - 1.
    ENDIF.

  ENDWHILE.

ENDFORM.                    " GET_DATUM_VIA_VORGANG

*&---------------------------------------------------------------------*
*&      Form  SET_VALUES_FILMASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_values_filmask .

  DATA: ls_list TYPE vrm_value.

  CHECK gt_list IS INITIAL.

* ---------------------------------------------------------------------
* Verschiedene Sortierungsmöglichkeiten bezüglich Filename
* ---------------------------------------------------------------------
  ls_list-key = '1'.
  CONCATENATE gc_filemask_dop gc_filemask_matnr gc_filemask_lifke gc_filemask_datab
              gc_filemask_spras
  INTO ls_list-text IN CHARACTER MODE SEPARATED BY gc_filemask_sep.
  APPEND ls_list TO gt_list.

  ls_list-key = '2'.
  CONCATENATE gc_filemask_dop gc_filemask_spras gc_filemask_matnr gc_filemask_lifke
              gc_filemask_datab
  INTO ls_list-text IN CHARACTER MODE SEPARATED BY gc_filemask_sep.
  APPEND ls_list TO gt_list.

  ls_list-key = '3'.
  CONCATENATE gc_filemask_dop gc_filemask_spras gc_filemask_lifke gc_filemask_matnr
              gc_filemask_datab
  INTO ls_list-text IN CHARACTER MODE SEPARATED BY gc_filemask_sep.
  APPEND ls_list TO gt_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_FIMASK'
      values = gt_list
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc EQ 0.
*   Default-Maskierung auf Bildschirm setzen
    p_fimask = '1'.
  ENDIF.

ENDFORM.                    " SET_VALUES_FILMASK

*&---------------------------------------------------------------------*
*&      Form  SAVE_AS_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PDFTAB  text
*      -->P_LV_FULL_PATH  text
*      -->P_ENDLOOP  text
*----------------------------------------------------------------------*
FORM save_as_pdf  TABLES pt_pdftab
                  USING  pv_binfilesize TYPE i
                         pv_full_path TYPE string.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = pv_binfilesize
      filename                = pv_full_path
      filetype                = 'BIN'
    CHANGING
      data_tab                = pt_pdftab[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

ENDFORM.                    " SAVE_AS_PDF

*&---------------------------------------------------------------------*
*&      Form  GET_FULL_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LV_FULL_PATH  text
*----------------------------------------------------------------------*
FORM get_full_path  USING pv_spras TYPE spras
                 CHANGING pv_full_path TYPE string.

  DATA: lv_matnr TYPE matnr.
  DATA: lv_spras_2 TYPE char2.
  DATA: lv_default_file_name  TYPE string.
  DATA: lv_temp TYPE string.

  CLEAR: pv_full_path.

  WRITE gv_matnr TO lv_matnr NO-ZERO.

  CLEAR lv_spras_2.

  IF NOT pv_spras IS INITIAL.
*   Sprachcode von 1- nach 2-stellig umwandeln
    CLEAR lv_spras_2.

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = pv_spras
      IMPORTING
        output = lv_spras_2.
  ENDIF.

  WRITE ztmm_labeling-matnr TO lv_matnr NO-ZERO.
  lv_default_file_name = gv_fimask.

  IF ztmm_labeling-lifke IS INITIAL.
    REPLACE gc_filemask_lifke WITH space INTO lv_default_file_name.
    CONDENSE lv_default_file_name NO-GAPS.
  ENDIF.

  REPLACE gc_filemask_matnr WITH lv_matnr(gc_anz_stellen) INTO lv_default_file_name.
  REPLACE gc_filemask_lifke WITH ztmm_labeling-lifke      INTO lv_default_file_name.
  REPLACE gc_filemask_datab WITH ztmm_labeling-datab      INTO lv_default_file_name.
  REPLACE gc_filemask_spras WITH lv_spras_2               INTO lv_default_file_name.
  CONDENSE lv_default_file_name NO-GAPS.

* Vollen Pfad zurückgeben
  CONCATENATE p_path lv_default_file_name '.' gc_format
    INTO pv_full_path IN CHARACTER MODE.

  REPLACE ALL OCCURRENCES OF '--' IN pv_full_path WITH '-'.
  CONDENSE pv_full_path NO-GAPS.

  REPLACE '-.' WITH '.' INTO pv_full_path.
  CONDENSE pv_full_path NO-GAPS.

ENDFORM.                    " GET_FULL_PATH
