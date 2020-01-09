*&
*& Report  ZLE_LIEFERUNG_INFO
*&
*&
*&
*&    Programm-Name....: ZLE_LIEFERUNG_INFO
*&    Entwickler.......: Markus Raffeiner
*&    Erstell-Datum....: 01.10.2008
*&    Version..........: 1.0
*&
*&    Zweck............: Lieferungen-Infos
*&                       Beleg-Nachdruck aus ALV-GRID
*&                       Für den Nachdruck muss jeweils eine ganze Zeile
*&                       markiert werden
*&
*&    Input............: Tabellen: VBRK
*&
*&    Output...........: ALV-GRID
*&
*&    Bemerkung........: Wird aus der Transaktion ZVREPRINT angestossen
*&
*&    Aenderungen:
*&
*&    xx.xx.xxxx: xxx



************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105 #115 #166
************************************************************************

REPORT ZLE_LIEFERUNG_INFO.

tables: likp, lips, vbak, makt.


TABLES:    vbrk,                  "Faktura: Kopfdaten
           vbkd,                  "Verkaufsbeleg: Kaufmännische Daten
           vbpa,                  "Vertriebsbeleg: Partner
           komk,                  "Preisfindung Kommunikations-Kopf
           kna1,                  "Kundenstamm (allgemeiner Teil)
           knvp,                  "Kundenstamm Partnerrollen
**           pa0003,                "Gener. View für Matchcode ID PREM
**           m_premn,               "Gener. View für Matchcode ID PREM
           nast,                  "Nachrichtenstatus
           tvv3t.                 "Kundengruppe 3:  Objektstatus


* Bildschirm-Definition
SELECTION-SCREEN   BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
select-options: s_vkorg for likp-vkorg,
                s_vbeln for likp-vbeln,
                s_erdat for likp-erdat,
                s_matnr for lips-matnr,
***                s_vkbur for lips-vkbur,
                s_vgbel for lips-vgbel,
                s_kunag for likp-kunag,
                s_kunnr for likp-kunnr.
SELECTION-SCREEN   END OF BLOCK b1.

selection-screen skip 1.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
***parameters:    p_kschl like nast-kschl default 'BL00'.
***parameters:    p_kschl like nast-kschl.
parameters:    p_vari like disvariant-variant default '/STANDARD'.
parameters:    p_dru like nast-ldest obligatory default 'LOCS'.
SELECTION-SCREEN END OF BLOCK B3.


TYPES: begin of t_gt_otab.
        INCLUDE STRUCTURE ZSLE_LIEFERUNG_INFO.
TYPES:  END OF t_gt_otab.
TYPES: begin of t_gt_otab2.
        INCLUDE STRUCTURE ZSBC_REPRINT.
TYPES:  END OF t_gt_otab2.

types: zzcolor(4).

DATA:  gt_otab TYPE TABLE OF t_gt_otab,
       gt_otab2 TYPE TABLE OF t_gt_otab2,
       wa_outrec type t_gt_otab.

data: wa_otab type t_gt_otab.


DATA: es_layout type lvc_s_layo,
      es_s_filcat type lvc_s_fcat,
      es_t_filcat type lvc_t_fcat.


DATA  BEGIN OF t_komv OCCURS 0.
        INCLUDE STRUCTURE komv.
DATA  END   OF t_komv.


DATA  BEGIN OF t_komvd OCCURS 0.
        INCLUDE STRUCTURE komvd.
DATA  END   OF t_komvd.

DATA  BEGIN OF t_infot OCCURS 0.
        INCLUDE STRUCTURE tline.
DATA  END   OF t_infot.

DATA  BEGIN OF t_verlo OCCURS 0.
        INCLUDE STRUCTURE tline.
DATA  END   OF t_verlo.

DATA: BEGIN OF itab_lief OCCURS 0,
        vbeln like likp-vbeln,
        posnr like lips-posnr,
        vgbel like lips-vgbel,
        vgpos like lips-vgpos,
        vkorg like likp-vkorg,
***        vkbur like lips-vkbur,
        kunag like likp-kunag,
        kunnr like likp-kunnr,
        matnr like lips-matnr,
     END of itab_lief.


DATA: x_save,  "for parameter I_SAVE: modus for saving a layout
      x_layout TYPE disvariant,
      g_exit TYPE c.  "is set if the user has aborted a layout popup

* The variables 'def_layout' and 'spec_layout' are set during
* interactions on the selection screen.
* 'gs_variant' finally holds the chosen layout.
DATA: def_layout  TYPE disvariant,     "default layout
      default TYPE c VALUE ' ',
      spec_layout TYPE disvariant,     "specific layout
      gs_variant   TYPE disvariant.     "finally chosen layout


DATA:      cflag(1)     VALUE 'X',           "Flip-Flop
           sw_ausw(1).                       "Test Auswahlkriterien


DATA:      BEGIN OF h,
             fieldname(30),                  "mit Cursor markiertes Feld
             bstnk(35),                      "Suchstring (* + bstnk)
             rest       TYPE i,              "Restfeld
             fkwrt      LIKE komk-fkwrt,     "Endbetrag
             datum(10)  TYPE c,
             vbeln      LIKE vbak-vbeln,     "zum Lesen mit führ. 0
             pernr(8)   TYPE c,              "für left-justified
             id         LIKE thead-tdid,
             language   LIKE thead-tdspras,
             name       LIKE thead-tdname,
             object     LIKE thead-tdobject,
             lines type i,
             get_daten,
             get_daten2,
             rcode like sy-subrc,
             kschl like nast-kschl,
             fehltext(100),
          END OF h.


DATA:      BEGIN OF z,
             anz_itab   TYPE i,              "Anzahl itab-elemente
           END OF z.

data: c_nachdruck type sy-ucomm value 'NACHDRUCK'.
data: c_nachdruck_mass type sy-ucomm value 'NACHDRUCK_MASS'.
* Konstanten
constants: C_STATUS_0    LIKE NAST-VSTAT     VALUE '0',
           C_STATUS_1    LIKE NAST-VSTAT     VALUE '1',
           C_STATUS_2    LIKE NAST-VSTAT     VALUE '2',
           C_ICON_GREEN  LIKE DV70A-STATUSICON VALUE 'ICON_GREEN_LIGHT',
          C_ICON_YELLOW LIKE DV70A-STATUSICON VALUE 'ICON_YELLOW_LIGHT',
           C_ICON_RED    LIKE DV70A-STATUSICON VALUE 'ICON_RED_LIGHT'.




*======================================================================
* Klassendefinition für Eventhandling (Beispiel-ABAP: BCALV_GRID_03)
*======================================================================

CLASS lcl_event_receiver DEFINITION DEFERRED.

include <icon>.

DATA: ok_code LIKE sy-ucomm,
      g_container TYPE scrfname VALUE 'BCALV_GRID_LIEFERUNG_INFO',
      g_container2 TYPE scrfname VALUE 'BCALV_GRID_REPRINT',
      grid1  TYPE REF TO cl_gui_alv_grid,
      grid2  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_custom_container2 TYPE REF TO cl_gui_custom_container,
      event_receiver TYPE REF TO lcl_event_receiver,

*$smart (W) 2010-08-02 - #166 Datendefinition bezieht sich auf einen
*$smart (W) 2010-08-02 - #166 obsoleten Datentyp. (A)

      g_repid TYPE REPID,                        "smart: 2010-08-02 #166
      gs_toolbar  TYPE stb_button.



****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
*===============================================================
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
*
* Definition:
* ~~~~~~~~~~~
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:

     handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

     handle_menu_button
        FOR EVENT menu_button OF cl_gui_alv_grid
            IMPORTING e_object e_ucomm,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row e_column.


  PRIVATE SECTION.

ENDCLASS.
*
* lcl_event_receiver (Definition)
*===============================================================

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class lcl_event_receiver (Implementation)
*
* In this example, only event DOUBLE_CLICK is caught
*
CLASS lcl_event_receiver IMPLEMENTATION.

*=================================================================
* Methode für double_click
*=================================================================
  METHOD handle_double_click.
    DATA: ls_otab LIKE LINE OF gt_otab.
    DATA: l_vbak like vbak.

* § 4.The event DOUBLE_CLICK provides parameters for row and column
*   of the click. Use row parameter to select a line of the
*   corresponding internal table.

* read selected row from internal table gt_sflight
    READ TABLE gt_otab INDEX e_row-index INTO ls_otab.

    CASE e_column.
      WHEN 'VBELN'.
        SET PARAMETER ID 'AUN' FIELD ls_otab-vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      WHEN 'KUNAG'.
        SET PARAMETER ID 'KUN' FIELD ls_otab-kunag.
        h-vbeln = ls_otab-vbeln.
        WHILE h-vbeln+9(1) = ' '.
          SHIFT h-vbeln RIGHT IN CHARACTER MODE ."smart: 2010-08-02 #115
          h-vbeln+0(1) = '0'.
        ENDWHILE.
        SELECT SINGLE * FROM   vbak into l_vbak
                        WHERE  vbeln EQ  h-vbeln.
        SET PARAMETER ID: 'VKO' FIELD l_vbak-vkorg,
                          'VTW' FIELD l_vbak-vtweg,
                          'SPA' FIELD l_vbak-spart.
        CALL TRANSACTION 'VD03' AND SKIP FIRST SCREEN.
      WHEN 'KUNWE'.
        SET PARAMETER ID 'KUN' FIELD ls_otab-kunwe.
        h-vbeln = ls_otab-vbeln.
        WHILE h-vbeln+9(1) = ' '.
          SHIFT h-vbeln RIGHT IN CHARACTER MODE ."smart: 2010-08-02 #115
          h-vbeln+0(1) = '0'.
        ENDWHILE.
        SELECT SINGLE * FROM   vbak into l_vbak
                        WHERE  vbeln EQ  h-vbeln.
        SET PARAMETER ID: 'VKO' FIELD l_vbak-vkorg,
                          'VTW' FIELD l_vbak-vtweg,
                          'SPA' FIELD l_vbak-spart.
        CALL TRANSACTION 'VD03' AND SKIP FIRST SCREEN.
      WHEN 'VBELN_VL'.
        SET PARAMETER ID 'VL' FIELD ls_otab-vbeln_vl.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.                           "handle_double_click


*=================================================================
* Methode für handle_toolbar
*=================================================================
  METHOD handle_toolbar.
* § 2.At event TOOLBAR define a toolbar element of type 2 by using
*    event paramenter E_OBJECT. Remember its function code.
*.......
* Part I: Define a menu button including a function code that
*         is evaluated in 'handle_MENU_BUTTON
*.......


* append a separator to normal toolbar
    CLEAR gs_toolbar.
    MOVE 3 TO gs_toolbar-butn_type.
    APPEND gs_toolbar TO e_object->mt_toolbar.

* append a menut o switch between detail levels.

    CLEAR gs_toolbar.
    MOVE c_nachdruck TO gs_toolbar-function.
* --> This function code is evaluated in 'handle_menu_button'
    MOVE icon_detail TO gs_toolbar-icon.
    MOVE text-005    TO gs_toolbar-quickinfo. "Nachdruck
    MOVE 2 TO gs_toolbar-butn_type.
    MOVE space TO gs_toolbar-disabled.
    APPEND gs_toolbar TO e_object->mt_toolbar.


* Massendruck
    CLEAR gs_toolbar.
    MOVE c_nachdruck_mass TO gs_toolbar-function.
* --> This function code is evaluated in 'handle_menu_button'
    MOVE icon_detail TO gs_toolbar-icon.
    MOVE text-010    TO gs_toolbar-quickinfo. "Nachdruck
    MOVE 2 TO gs_toolbar-butn_type.
    MOVE space TO gs_toolbar-disabled.
    APPEND gs_toolbar TO e_object->mt_toolbar.


  ENDMETHOD.

*=================================================================
* Methode für handle_menu_button
*=================================================================
  METHOD handle_menu_button.
* § 3.At event MENU_BUTTON query your function code and define a
*     menu in the same way as a context menu.
*..........
* Part II: Evaluate 'e_ucomm' to see which menu button of the toolbar
*          has been clicked on.
*          Define then the corresponding menu.
*          The menu contains function codes that are evaluated
*          in 'handle_user_command'.
*...........

* handle own menubuttons
    CASE e_ucomm.
      WHEN c_nachdruck.
        CALL METHOD e_object->add_function
                    EXPORTING fcode   = c_nachdruck "Nachdruck
                              text    = text-005. "Overview
      WHEN c_nachdruck_mass.
        CALL METHOD e_object->add_function
                    EXPORTING fcode   = c_nachdruck_mass "Mehrfachdruck
                              text    = text-010. "Overview
    ENDCASE.

  ENDMETHOD.

*=================================================================
* Methode für handle_user_command
*=================================================================
  METHOD handle_user_command.
* § 4.At event USER_COMMAND query the function code of each function
*     defined in step 3.
*.........
* Part III : Evaluate user command to invoke the corresponding
*            function.
*.........

    DATA: lt_rows TYPE lvc_t_row.
    DATA: ls_wa_row LIKE LINE OF lt_rows.
    DATA: ls_otab LIKE LINE OF gt_otab.
    DATA: ls_otab2 LIKE LINE OF gt_otab2.
    DATA: lt_cells type LVC_T_CELL.
    DATA: l_row_index like sy-index.
    data: l_vbeln like vbrk-vbeln.
    data: l_text1(40).

* Nachdruck
    CASE e_ucomm.
      WHEN c_nachdruck.

* Selektierte Zeile holen
        CALL METHOD grid1->get_selected_rows
                 IMPORTING et_index_rows = lt_rows.

* Nur eine Selektion daher nur ein Eintrag (Zeilennummer)
        read table lt_rows index 1 into ls_wa_row.
        l_row_index = ls_wa_row-index.


* Zeile aus ALV-Grid besorgen und VBELN zum Nachdruck ermitteln
        read table gt_otab index l_row_index into ls_otab.

* Keine Zeile selektiert
        if sy-subrc ne 0.
          MESSAGE s001(aq) with text-006.
          LEAVE TO SCREEN 100.
        endif.

* Führende Nullen erzeugen
        unpack ls_otab-vbeln_vl to l_vbeln.


* ALV-Grid für Nachdruck
**********************
        clear gt_otab2.
        move l_vbeln to ls_otab2-vbeln.
        append ls_otab2 to gt_otab2.

        call screen 110 starting at 10 10
                        ending at 130 20.
***                        ending at 80 20.


* Mehrfachdruck
      WHEN c_nachdruck_mass.
        perform nachdruck_lieferschein_mass.


***        clear gt_otab2.
***        append ls_otab to gt_otab2.

***     call screen 110.
**********************
***        perform nachdruck using 'V2'
***                                l_vbeln
***                                'LD00'
***                                h-rcode
***                                h-fehltext.

* Fehler aufgetreten
***        if h-rcode ne 0.
***          MESSAGE s003(aq) with h-fehltext l_vbeln h-kschl.
***        endif.


**        CALL FUNCTION 'POPUP_TO_INFORM'
**             EXPORTING
**                  titel = g_repid
**                  txt2  = sy-subrc
**                  txt1  = l_text1.
**
***break-point 01.
***        LEAVE TO SCREEN 100.
    ENDCASE.

  ENDMETHOD.                           "handle_user_command


ENDCLASS.
*
* lcl_event_receiver (Implementation)


INITIALIZATION.
  g_repid = sy-repid.
  x_save = 'A'.   "The user may save all types of a layout

  PERFORM init_selektionsmaske.

*&---------------------------------------------------------------------*
*& Positionsauswahl
*&---------------------------------------------------------------------*
at user-command.
  break-point 01.
  case sy-ucomm.
    when 'ABBR'.
      leave to transaction sy-tcode.
**    when 'SELE'.
**      if not ilips-posnr is initial.
**        ea-posnr = ilips-posnr.
**        loop at ilips.
**          if ilips-posnr ne ea-posnr.
**            delete ilips.
**          endif.
**        endloop.
**        leave list-processing.
**        leave screen.
**      endif.
    when 'CANCEL'.
***      leave to transaction sy-tcode.
      leave to screen 110.
    when others.
  endcase.
  clear sy-ucomm.




*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
at selection-screen output.
* The default layout is fetched the first time the PBO of the
* selection screen is called.
* If a default layout exist, its identification
* is saved in 'def_layout'.
*
  if default = ' '.
    clear def_layout.
    move g_repid to def_layout-report.
    call function 'LVC_VARIANT_DEFAULT_GET'
         EXPORTING
              i_save     = x_save
         CHANGING
              cs_variant = def_layout
         EXCEPTIONS
              not_found  = 2.
    if sy-subrc = 2.
**        p_no_v = 'X'.
**        p_def_v = ' '.
**        p_spec_v = ' '.
**>          perform deactivate_def_v.
      exit.
    else.
* set name of layout on selection screen
**>        perform activate_def_v.
      p_vari = def_layout-variant.
      default = 'X'.
    endif.
  endif.                             "default IS INITIAL

*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
at selection-screen on value-request for p_vari.
* popup F4 help to select a layout

  clear x_layout.
  move sy-repid to g_repid.
  move g_repid to x_layout-report.

  call function 'LVC_VARIANT_F4'
       EXPORTING
            is_variant = x_layout
            i_save     = x_save
       IMPORTING
            e_exit     = g_exit
            es_variant = spec_layout
       EXCEPTIONS
            not_found  = 1
            others     = 2.
  if sy-subrc ne 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if g_exit ne 'X'.
* set name of layout on selection screen
      p_vari    = spec_layout-variant.
    endif.
  endif.


at selection-screen.
* test if specified layout exist
*  if p_spec_v = 'X'.
  clear spec_layout.
  move p_vari  to spec_layout-variant.
  move g_repid to spec_layout-report.

  call function 'LVC_VARIANT_EXISTENCE_CHECK'
       EXPORTING
            i_save        = x_save
       CHANGING
            cs_variant    = spec_layout
       EXCEPTIONS
            wrong_input   = 1
            not_found     = 2
            program_error = 3
            others        = 4.
  if sy-subrc <> 0.
**>    message id sy-msgid type sy-msgty number sy-msgno
**>            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
*  endif.
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  move-corresponding spec_layout to gs_variant.


*** Verarbeitung
START-OF-SELECTION.

* ALV-GRID
  call screen 100.

*&---------------------------------------------------------------------*
*&      Module  PBO_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_100 OUTPUT.


  check h-get_daten eq ' '.

  clear z-anz_itab.
  refresh gt_otab.

* Lieferungen selektieren (Kopf, Positionen)
  select likp~vbeln posnr vgbel vgpos vkorg kunag kunnr matnr
         into table itab_lief
         from likp inner join lips
         on likp~vbeln = lips~vbeln
         where likp~vkorg in s_vkorg and
               likp~vbeln in s_vbeln and
               likp~erdat in s_erdat and
               likp~lfart = 'LF' and
               lips~matnr in s_matnr and
               lips~vgbel in s_vgbel and
***               lips~vkbur in s_vkbur and
               lips~uecha = 0.


  loop at itab_lief.

    clear wa_otab.

*    move itab_lief-vkorg to wa_otab-vkorg.
    move itab_lief-vbeln to wa_otab-vbeln_vl.
    move itab_lief-posnr to wa_otab-posnr_vl.
    move itab_lief-vgbel to wa_otab-vbeln.
    move itab_lief-vgpos to wa_otab-posnr.
    move itab_lief-vkorg to wa_otab-vkorg.
***    move itab_lief-vkbur to wa_otab-vkbur.

    move itab_lief-kunag to wa_otab-kunag.
    perform get_name_aus_kna1 using    itab_lief-kunag
                              changing wa_otab-name_ag.

    move itab_lief-kunnr to wa_otab-kunwe.
    perform get_name_aus_kna1 using    itab_lief-kunnr
                              changing wa_otab-name_we.

    move itab_lief-matnr to wa_otab-matnr.

    perform get_materialkurztext using itab_lief-matnr
                              changing wa_otab-maktx.

    append wa_otab to gt_otab.

  endloop.


  perform ausgabe_alv_grid.

  h-get_daten = 'X'.


ENDMODULE.                 " PBO_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_100 INPUT.

*   to react on oi_custom_events:
  CALL METHOD cl_gui_cfw=>dispatch.

  if sy-ucomm eq 'BACK' or
     sy-ucomm eq 'END' or
     sy-ucomm eq 'CANCEL'.
    if sy-tcode eq 'ZVREPRINT'.
      leave to transaction sy-tcode.
    else.
      SET SCREEN 0.
      LEAVE SCREEN.
    endif.

  endif.



ENDMODULE.                 " PAI_100  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
***  describe table itab_vbak lines z-anz_itab.
  SET TITLEBAR 'ZLELIEFINFO'.

ENDMODULE.                 " STATUS_0100  OUTPUT








*&---------------------------------------------------------------------*
*&      Form  init_selektionsmaske
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_selektionsmaske.

  if not s_vkorg-low is initial.
    MOVE 'I'         TO s_vkorg-sign.
    MOVE 'EQ'        TO s_vkorg-option.
    GET PARAMETER ID 'VKO' FIELD s_vkorg-low.
    APPEND s_vkorg.
  endif.

ENDFORM.                    " init_selektionsmaske



*&---------------------------------------------------------------------*
*&      Form  ausgabe_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausgabe_alv_grid.

* Falls keine Daten vorhanden: Meldung ausgeben
  describe table gt_otab lines h-lines.

  if h-lines eq 0.
    message i260(aq).
    SET SCREEN 0.
    leave screen.
  endif.

  SET PF-STATUS 'MAIN100'.

  IF g_custom_container IS INITIAL.

* Falls als Hintergrundjob eingeplant, darf kein ALV-Grid erzeugt werden
* (Listenaufbereitung)
    IF cl_gui_alv_grid=>offline( ) is initial.
      CREATE OBJECT g_custom_container
             EXPORTING container_name = g_container.
    ENDIF.

    CREATE OBJECT grid1
           EXPORTING i_parent = g_custom_container.
*           i_appl_events  = 'X'. "Für Ereignisse im ALV-Grid

    es_layout-CWIDTH_OPT = 'X'.
*    es_layout-zebra = 'X'.
    es_layout-info_fname = 'ZZCOLOR'.


* ALV-Grid für Ausgabe vorbereiten
    perform alv_vorbereiten using 'ZSLE_LIEFERUNG_INFO'.

    CALL METHOD grid1->set_table_for_first_display
         EXPORTING i_structure_name  = 'ZSLE_LIEFERUNG_INFO'
                   is_variant       = gs_variant
**                   is_variant       = spec_layout
                   i_save           = x_save
                   i_default        = ' '
         CHANGING  it_outtab         =  gt_otab
                   it_fieldcatalog   =  es_t_filcat.


    CREATE OBJECT event_receiver.

    SET HANDLER event_receiver->handle_double_click FOR grid1.

    SET HANDLER event_receiver->handle_user_command
                event_receiver->handle_menu_button
***                event_receiver->handle_toolbar FOR ALL INSTANCES.
                event_receiver->handle_toolbar FOR grid1.


* raise event TOOLBAR:
    CALL METHOD grid1->set_toolbar_interactive.


* Falls einfärben
*         CHANGING  it_outtab        = gt_plandaten_out
*                   it_fieldcatalog  = es_t_filcat.

  ENDIF.

  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid1.


ENDFORM.                    " ausgabe_alv_grid


*&---------------------------------------------------------------------*
*&      Form  ausgabe_alv_grid2
*&---------------------------------------------------------------------*
*       text

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausgabe_alv_grid2.

  data: l_otab2_save type table of t_gt_otab2.
  data: ls_otab2 type t_gt_otab2.
  data: l_statusicon like DV70A-STATUSICON.
  data: l_vbeln like likp-vbeln.

* Falls keine Daten vorhanden: Meldung ausgeben
  describe table gt_otab2 lines h-lines.

  if h-lines eq 0.
    message i260(aq).
    SET SCREEN 0.
    leave screen.
  endif.

* Tabelle merken
  l_otab2_save[] = gt_otab2[].

  clear gt_otab2.

  READ TABLE l_otab2_save INDEX 1 INTO ls_otab2.
  move ls_otab2-vbeln to l_vbeln.

* bereits erstellte Nachrichten holen

  select * from nast where kappl = 'V2' and
                                  objky = ls_otab2-vbeln and
                                  nacha lt '3'.
    clear ls_otab2.

    perform set_icon using    nast-vstat
                     changing l_statusicon.
    move l_statusicon to ls_otab2-ampel_i.

    move nast-kschl to ls_otab2-kschl.

    select single vtext from t685t into ls_otab2-vtext
      where spras eq sy-langu and
            kappl = 'V2' and
            kschl = nast-kschl.

    move l_vbeln    to ls_otab2-vbeln.
    move nast-datvr to ls_otab2-datvr.
    move nast-uhrvr to ls_otab2-uhrvr.
    move nast-spras to ls_otab2-spras.

    append ls_otab2 to gt_otab2.

  endselect.


  SET PF-STATUS 'MAIN100'.

  IF g_custom_container2 IS INITIAL.

* Falls als Hintergrundjob eingeplant, darf kein ALV-Grid erzeugt werden
* (Listenaufbereitung)
    IF cl_gui_alv_grid=>offline( ) is initial.
      CREATE OBJECT g_custom_container2
             EXPORTING container_name = g_container2.
    ENDIF.

    CREATE OBJECT grid2
           EXPORTING i_parent = g_custom_container2.
*           i_appl_events  = 'X'. "Für Ereignisse im ALV-Grid

    es_layout-CWIDTH_OPT = 'X'.
*    es_layout-zebra = 'X'.
    es_layout-info_fname = 'ZZCOLOR'.


* ALV-Grid für Ausgabe vorbereiten
    perform alv_vorbereiten using 'ZSBC_REPRINT'.

    CALL METHOD grid2->set_table_for_first_display
         EXPORTING i_structure_name  = 'ZSBC_REPRINT'
                   is_variant       = gs_variant
**                   is_variant       = spec_layout
                   i_save           = x_save
                   i_default        = ' '
         CHANGING  it_outtab         =  gt_otab2
                   it_fieldcatalog   =  es_t_filcat.


    CREATE OBJECT event_receiver.

***    SET HANDLER event_receiver->handle_double_click FOR grid2.

***>>>    SET HANDLER event_receiver->handle_user_command
***>>>                event_receiver->handle_menu_button.
***>>>                event_receiver->handle_toolbar FOR ALL INSTANCES.
***                event_receiver->handle_toolbar FOR grid2.


* raise event TOOLBAR:
    CALL METHOD grid2->set_toolbar_interactive.


* Falls einfärben
*         CHANGING  it_outtab        = gt_plandaten_out
*                   it_fieldcatalog  = es_t_filcat.

  ENDIF.

  CALL METHOD grid2->refresh_table_display.
  CALL METHOD cl_gui_control=>set_focus EXPORTING control = grid2.
  CALL METHOD cl_gui_cfw=>flush.



ENDFORM.                    " ausgabe_alv_grid2


*&---------------------------------------------------------------------*
*&      Form  alv_vorbereiten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM alv_vorbereiten using p_structure_name TYPE CLIKE.
                                                 "smart: 2010-08-02 #105


  DATA ls_fcat TYPE lvc_s_fcat.

  refresh es_t_filcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = p_structure_name
       CHANGING
            ct_fieldcat      = es_t_filcat.

* Spaltentitel
  perform spaltentitel_aufbereiten.


ENDFORM.                    " alv_vorbereiten


*&---------------------------------------------------------------------*
*&      Form  spaltentitel_aufbereiten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM spaltentitel_aufbereiten.

* Angebotsinfo (linksbündig)
  perform spaltentitel_alv_modifizieren using 'ZZINFOT'
                                               text-t01
                                               'L'.
* Verloren am (linksbündig)
  perform spaltentitel_alv_modifizieren using 'ZZVERLO'
                                               text-t02
                                               'L'.


ENDFORM.                    " spaltentitel_aufbereiten
*&---------------------------------------------------------------------*
*&      Form  spaltentitel_alv_modifizieren
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0862   text
*      -->P_TEXT_T02  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM spaltentitel_alv_modifizieren USING p_fieldname
TYPE CLIKE                                       "smart: 2010-08-02 #105
                                         p_title "smart: 2010-08-02 #105
                                           TYPE  "smart: 2010-08-02 #105
                                           CLIKE "smart: 2010-08-02 #105
                                         p_just  "smart: 2010-08-02 #105
                                           TYPE CLIKE.
                                                 "smart: 2010-08-02 #105



  DATA ls_fcat TYPE lvc_s_fcat.

  LOOP AT es_t_filcat INTO ls_fcat where fieldname eq p_fieldname.

    ls_fcat-reptext = p_title.
    ls_fcat-SCRTEXT_L = p_title.
    ls_fcat-SCRTEXT_M = p_title.
    ls_fcat-SCRTEXT_S = p_title.
* Titel ausricht (links- oder rechtsbündig)
    ls_fcat-just = p_just.
    modify es_t_filcat from ls_fcat.

  endloop.


ENDFORM.                    " spaltentitel_alv_modifizieren


*&---------------------------------------------------------------------*
*&      Form  nachdruck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM nachdruck_ausloesen using p_kappl TYPE CLIKE"smart: 2010-08-02 #105
                     p_vbeln TYPE VBRK-VBELN     "smart: 2010-08-02 #105
                     p_kschl TYPE                "smart: 2010-08-02 #105
                       ZSBC_REPRINT-KSCHL        "smart: 2010-08-02 #105
            changing p_subrc TYPE SYST-SUBRC     "smart: 2010-08-02 #105
                     p_fehltext LIKE h-fehltext. "smart: 2010-08-02 #105

  data: begin of l_nast occurs 0.
          include structure nast.
  data: end of l_nast.

  DATA:    XNAST LIKE VNAST OCCURS 20 WITH HEADER LINE.

  DATA:    YNAST LIKE NAST OCCURS 20 WITH HEADER LINE.

  clear: p_subrc, p_fehltext.

  select single * from nast where kappl = p_kappl and
                                  objky = p_vbeln and
                                  kschl = p_kschl.

* Nachricht nicht vorhanden
  if sy-subrc ne 0.
    p_subrc = sy-subrc.
    p_fehltext = text-f01.
    exit.
  endif.

  refresh ynast.
  move-corresponding nast to ynast.
  append ynast.

  IF nast-vstat = '1'.
    nast-erdat = sy-datum.
    nast-eruhr = sy-uzeit.
    nast-usnam = sy-uname.
    nast-manue = 'X'.
    nast-vstat = '0'.
    nast-ldest = p_dru.
    CLEAR NAST-DATVR.
    CLEAR NAST-UHRVR.
    clear nast-cmfpnr.
  ENDIF.

* Nachdruck als PDF: Information für LOGO-Druck an SAP-Script
* übergeben
  if p_dru eq 'LOCS'.
    nast-pfld1 = 'PDF'.
  endif.

*      PERFORM einzelnachricht_dialog(rsnast00) USING h-rcode.
  perform EINZELNACHRICHT_OHNE_UPDATE(rsnast00) using h-rcode.

  if h-rcode ne 0.
    p_subrc = h-rcode.
    p_fehltext = text-f02.
    exit.
  endif.


ENDFORM.                    " nachdruck


*&---------------------------------------------------------------------*
*&      Form  get_name_aus_kna1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_LIEF_KUNAG  text
*      <--P_WA_OTAB_NAME_AG  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_name_aus_kna1 USING    p_kunnr TYPE     "smart: 2010-08-02 #105
  CLIKE                                          "smart: 2010-08-02 #105
                       CHANGING p_name1 TYPE CLIKE.
                                                 "smart: 2010-08-02 #105


  if p_kunnr eq kna1-name1.
    move kna1-name1 to p_name1.
    exit.
  endif.

  clear p_name1.

  select single name1 from kna1 into p_name1
    where kunnr = p_kunnr.

ENDFORM.                    " get_name_aus_kna1


*&---------------------------------------------------------------------*
*&      Form  get_materialkurztext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_LIEF_MATNR  text
*      <--P_WA_OTAB_MAKTX  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_materialkurztext USING    p_matnr TYPE  "smart: 2010-08-02 #105
  LIPS-MATNR                                     "smart: 2010-08-02 #105
                          CHANGING p_maktx TYPE ZSLE_LIEFERUNG_INFO-MAKTX.
                                                 "smart: 2010-08-02 #105


  if p_matnr eq makt-matnr.
    move makt-maktx to p_maktx.
    exit.
  endif.

  clear p_maktx.

  select single maktx from makt into p_maktx
    where matnr = p_matnr and
          spras = sy-langu.


ENDFORM.                    " get_materialkurztext


*&---------------------------------------------------------------------*
*&      Module  PBO_110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_110 OUTPUT.


  perform ausgabe_alv_grid2.


ENDMODULE.                 " PBO_110  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  PAI_110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_110 INPUT.

  case sy-ucomm.
    when 'EA_REPRINT'.
      perform nachdruck.
    when others.
      LEAVE TO SCREEN 0.
  endcase.

ENDMODULE.                 " PAI_110  INPUT


*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAST_VSTAT  text
*      -->P_L_STATUSICON  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM set_icon USING p_vstat TYPE NAST-VSTAT      "smart: 2010-08-02 #105
              changing p_icon TYPE DV70A-STATUSICON.
                                                 "smart: 2010-08-02 #105


* Aus Include DV70AF0S / form set_icon

  DATA: L_INFO LIKE ICONT-QUICKINFO.

  CASE p_vstat.
    WHEN C_STATUS_0. "Gelb
      move icon_yellow_light to p_icon.
    WHEN C_STATUS_1. "Grün
      move icon_green_light to p_icon.
    WHEN C_STATUS_2. "Rot
      move icon_red_light to p_icon.
  endcase.

  exit.


  CASE p_vstat.
    WHEN C_STATUS_0.
      L_INFO = TEXT-010.
      CALL FUNCTION 'ICON_CREATE'
           EXPORTING
                NAME   = C_ICON_YELLOW
                INFO   = L_INFO
           IMPORTING
                RESULT = P_ICON
           EXCEPTIONS
                OTHERS = 0.
    WHEN C_STATUS_1.
      L_INFO = TEXT-011.
      CALL FUNCTION 'ICON_CREATE'
           EXPORTING
                NAME   = C_ICON_GREEN
                INFO   = L_INFO
           IMPORTING
                RESULT = P_ICON
           EXCEPTIONS
                OTHERS = 0.
    WHEN C_STATUS_2.
      L_INFO = TEXT-012.
      CALL FUNCTION 'ICON_CREATE'
           EXPORTING
                NAME   = C_ICON_RED
                INFO   = L_INFO
           IMPORTING
                RESULT = P_ICON
           EXCEPTIONS
                OTHERS = 0.
  ENDCASE.



ENDFORM.                    " set_icon

*&---------------------------------------------------------------------*
*&      Form  nachdruck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nachdruck.

  DATA: lt_rows TYPE lvc_t_row.
  DATA: ls_wa_row LIKE LINE OF lt_rows.
  DATA: ls_otab2 LIKE LINE OF gt_otab2.
  DATA: lt_cells type LVC_T_CELL.
  DATA: l_row_index like sy-index.
  data: l_vbeln like vbrk-vbeln.
  data: l_text1(40).

  CALL METHOD grid2->get_selected_rows
           IMPORTING et_index_rows = lt_rows.

* Nur eine Selektion daher nur ein Eintrag (Zeilennummer)
  read table lt_rows index 1 into ls_wa_row.
  l_row_index = ls_wa_row-index.


* Zeile aus ALV-Grid besorgen und VBELN zum Nachdruck ermitteln
  read table gt_otab2 index l_row_index into ls_otab2.

* Keine Zeile selektiert
  if sy-subrc ne 0.
    MESSAGE s001(aq) with text-006.
    LEAVE TO SCREEN 110.
  endif.

* Führende Nullen erzeugen
  unpack ls_otab2-vbeln to l_vbeln.

  perform nachdruck_ausloesen using 'V2'
                              l_vbeln
                              ls_otab2-kschl
                              h-rcode
                              h-fehltext.

ENDFORM.                    " nachdruck

*&---------------------------------------------------------------------*
*&      Form  nachdruck_mass
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nachdruck_lieferschein_mass.

  DATA: lt_rows TYPE lvc_t_row.
  DATA: ls_wa_row LIKE LINE OF lt_rows.
  DATA: lt_otab TYPE STANDARD TABLE OF t_gt_otab.
  DATA: ls_otab LIKE LINE OF gt_otab.

  DATA: lt_cells type LVC_T_CELL.
  DATA: l_row_index like sy-index.
  data: l_vbeln like vbrk-vbeln.
  data: l_text1(40).

  lt_otab[] = gt_otab[].

* Lieferschein nur einmal drucken
  DELETE ADJACENT DUPLICATES FROM lt_otab COMPARING vbeln_vl.

  loop at lt_otab into ls_otab.

    unpack ls_otab-vbeln_vl to l_vbeln.

    perform nachdruck_ausloesen using 'V2'
                                l_vbeln
                                'ZLD1'
                                h-rcode
                                h-fehltext.
  endloop.

ENDFORM.                    " nachdruck_mass
