************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*&---------------------------------------------------------------------*
*&  Include           ZPPA_ALVINCL                                     *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA                                                                 *
*----------------------------------------------------------------------*
INCLUDE <icon>.
INCLUDE <symbol>.

TABLES sscrfields.

TYPE-POOLS: slis, kkblo.

TYPES: g_ty_t_icon  TYPE STANDARD TABLE OF icon,
       g_ty_t_icont TYPE STANDARD TABLE OF icont.

TYPES: BEGIN OF t_alvpar,
         excp_yes                 TYPE char1,
         excp_display             TYPE char1,
         excp_condense            TYPE char1,
         layo_title               TYPE lvc_s_layo-grid_title,
         layo_zebra               TYPE char1,
         layo_no_vert_lines       TYPE char1,
         layo_no_horiz_lines      TYPE char1,
         layo_cell_merge          TYPE char1,
         layo_count               TYPE char1,
         layo_no_colhead          TYPE char1,
         layo_column_optimize     TYPE char1,
         layo_keys_hotspot        TYPE char1,
         layo_no_keyfix           TYPE char1,
         layo_box                 TYPE char1,
         layo_hotspot_field       TYPE slis_fieldname,
         layo_no_hotspot          TYPE char1,
         layo_no_scrolling        TYPE char1,
         layo_no_authcheck        TYPE char1,
         layo_no_min_linesize     TYPE char1,
         layo_min_linesize        TYPE sylinsz,
         layo_max_linesize        TYPE sylinsz,
         layo_group_change_edit   TYPE char1,
         layo_get_selinfo         TYPE char1,
         layo_confirmation_prompt TYPE char1,
         layo_f2code              TYPE syucomm,
         layo_reprep              TYPE char1,
         sum_before               TYPE char1,
         sum_no_sumline           TYPE char1,
         sum_no_sumchoice         TYPE char1,
         sum_numc                 TYPE char1,
         sum_no_unitsplit         TYPE char1,
         sum_totals_only          TYPE char1,
         sum_totals_text          TYPE symsgli,
         sum_no_subtotals         TYPE char1,
         sum_no_subchoice         TYPE char1,
         sum_subtotals_text       TYPE symsgli,
         vari_none                TYPE char1,
         vari_save                TYPE char1,
         vari_default             TYPE char1,
         vari_layout              TYPE disvariant-variant,
         prnt_print               TYPE char1,
         prnt_reserve_lines       TYPE lvc_s_prnt-reservelns,
         prnt_no_listinfo         TYPE char1,
         prnt_no_selinfo          TYPE char1,
         prnt_with_title          TYPE char1,
         prnt_printinfo           TYPE char1,
         prnt_no_coverpage        TYPE char1,
         prnt_no_new_page         TYPE char1,
         prnt_no_change_print_params TYPE char1,
         sett_minimize_tol        TYPE char1,
         sett_mimimize_eol        TYPE char1,
         sett_top_only_print      TYPE char1,
         sett_eol_only_print      TYPE char1,
         sett_no_colopt_print     TYPE char1,
         pri_params_set           TYPE char1,
         pri_archive_id           TYPE arc_params-archiv_id,
         pri_archive_info         TYPE arc_params-info,
         pri_archive_mode         TYPE pri_params-armod,
         pri_archive_text         TYPE arc_params-arctext,
         pri_archive_object       TYPE arc_params-ar_object,
         pri_authority            TYPE pri_params-prber,
         pri_copy                 TYPE pri_params-prcop,
         pri_coverpage            TYPE pri_params-prbig,
         pri_dataset              TYPE pri_params-prdsn,
         pri_department           TYPE pri_params-prabt,
         pri_destination          TYPE pri_params-pdest,
         pri_expiration           TYPE pri_params-pexpi,
         pri_immediatly           TYPE pri_params-primm,
         pri_layout               TYPE pri_params-paart,
         pri_line_count           TYPE pri_params-linct,
         pri_line_size            TYPE pri_params-linsz,
         pri_list_name            TYPE pri_params-plist,
         pri_list_text            TYPE pri_params-prtxt,
         pri_new_list_id          TYPE pri_params-prnew,
         pri_receiver             TYPE pri_params-prrec,
         pri_release              TYPE pri_params-prrel,
         pri_sap_coverpage        TYPE pri_params-prsap,
         pri_host_coverpage       TYPE pri_params-prunx,
         pri_priority             TYPE pri_params-priot,
         pri_sap_object           TYPE arc_params-sap_object,
         pri_type                 TYPE pri_params-ptype,
         pri_footline             TYPE pri_params-footl,
         buffer_active            TYPE char1,
         bypassing_buffer         TYPE char1,
         current_display          TYPE i,
       END OF t_alvpar.
DATA:  w_alvpar TYPE t_alvpar.




* Tabelle mit Sortierinformationen
DATA:  i_sorttab TYPE slis_t_sortinfo_alv.

* Tabelle mit Header-Informationen
DATA:  i_hlines TYPE slis_t_listheader,
       w_hlines TYPE slis_listheader.

CONSTANTS: con_true         TYPE char1 VALUE 'X',
           con_display_full TYPE i VALUE 3,

           con_exit TYPE sy-ucomm VALUE 'EXIT',
           con_canc TYPE sy-ucomm VALUE 'CANC',
           con_back TYPE sy-ucomm VALUE 'BACK'.


*$smart (W) 2010-08-02 - #166 Datendefinition bezieht sich auf einen
*$smart (W) 2010-08-02 - #166 obsoleten Datentyp. (A)

DATA: g_repid    TYPE REPID,                     "smart: 2010-08-02 #166
      g_variant  LIKE disvariant,
      gx_variant LIKE disvariant.

DATA: g_field     TYPE lvc_s_fcat-fieldname,
      g_int_field TYPE i.

DATA: g_okcode TYPE sy-ucomm.

DATA: l_event_html_top   TYPE slis_alv_event-form.

DATA: lt_fcat   TYPE slis_t_fieldcat_alv.

*----------------------------------------------------------------------*
* SELECT-OPTIONS
*----------------------------------------------------------------------*
PARAMETERS: p_vari LIKE disvariant-variant.
*----------------------------------------------------------------------*
* FORM ZZZ_START_ALV_FULLSCREEN
*----------------------------------------------------------------------*
FORM zzz_start_alv_fullscreen.

  l_event_html_top = 'Z_ALV_EVENT_HTML_TOP_OF_PAGE'.
  w_hlines-typ = 'H'.
  w_hlines-info = 'HEADERINFO'.
  INSERT w_hlines INTO TABLE i_hlines.

  PERFORM zzz_call_fullscreen.


ENDFORM.

*----------------------------------------------------------------------*
* FORM ZZZ_ALV_DEFAULT_VARIANTE
*----------------------------------------------------------------------*
FORM zzz_alv_default_variante.

  g_repid = sy-repid.
  CLEAR: g_variant.
  g_variant-report = g_repid.

  gx_variant = g_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            i_save     = w_alvpar-vari_save
       CHANGING
            cs_variant = gx_variant
       EXCEPTIONS
            not_found  = 2.

  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f01_call_fullscreen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_call_fullscreen.

  DATA: ls_vari   TYPE disvariant,
        ls_layo   TYPE slis_layout_alv,
*        lt_fcat   type LVC_T_FCAT,
       ls_prnt   TYPE slis_print_alv,
        lt_evts   TYPE slis_t_event,
        ls_sett   TYPE lvc_s_glay.



  PERFORM zzz_alv_variante    CHANGING ls_vari.
  PERFORM zzz_alv_layout      CHANGING ls_layo.
  PERFORM zzz_alv_feldkatalog CHANGING lt_fcat.
  PERFORM zzz_alv_druck       CHANGING ls_prnt.
  PERFORM zzz_alv_ereignisse  CHANGING lt_evts.
  PERFORM zzz_alv_eigenschaft CHANGING ls_sett.

  w_alvpar-current_display = con_display_full.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
      i_bypassing_buffer                = w_alvpar-bypassing_buffer
      i_buffer_active                   = w_alvpar-buffer_active
      i_callback_program                = 'ZLE_GOODMOVE_1'
     i_callback_pf_status_set          = 'F01_ALV_EVENT_PF_STATUS_SET'
     i_callback_user_command           = 'F01_ALV_EVENT_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE            = ' '
      i_callback_html_top_of_page       = l_event_html_top
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
      i_background_id                   = 'ALV_BACKGROUND_KT_AG'
*     I_GRID_TITLE                      =
      i_grid_settings                   = ls_sett
      is_layout                         = ls_layo
      it_fieldcat                       = lt_fcat
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      it_sort                           = i_sorttab
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
      i_default                         = w_alvpar-vari_default
      i_save                            = w_alvpar-vari_save
      is_variant                        = ls_vari
      it_events                         = lt_evts
*     IT_EVENT_EXIT                     =
      is_print                          = ls_prnt
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = i_outtab
    EXCEPTIONS
      program_error                     = 1
      OTHERS                            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f01_prepare_alv_call


*&---------------------------------------------------------------------*
*&      Form  zzz_alv_variante
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_alv_variante CHANGING cs_vari TYPE disvariant.

  MOVE-CORRESPONDING g_variant TO cs_vari.

* cs_vari-report      = sy-repid.
* cs_vari-handle      = space.
* cs_vari-log_group   = space.
* cs_vari-username    = space.
* cs_vari-variant     = space.
* cs_vari-text        = space.
* cs_vari-dependvars  = space.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zzz_alv_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_alv_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = w_alvpar-layo_column_optimize.
  cs_layo-no_colhead             = w_alvpar-layo_no_colhead.
  cs_layo-no_hotspot             = w_alvpar-layo_no_hotspot.
  cs_layo-zebra                  = w_alvpar-layo_zebra.
  cs_layo-no_vline               = w_alvpar-layo_no_vert_lines.
  cs_layo-cell_merge             = w_alvpar-layo_cell_merge.
  cs_layo-no_min_linesize        = w_alvpar-layo_no_min_linesize.
  cs_layo-min_linesize           = w_alvpar-layo_min_linesize.
  cs_layo-max_linesize           = w_alvpar-layo_max_linesize.
  cs_layo-window_titlebar        = w_alvpar-layo_title.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = 'X'.
  cs_layo-edit_mode              = space.
*... Exceptions
  IF w_alvpar-excp_yes           <> space.
    cs_layo-lights_fieldname       = 'LIGHTS'.
  ENDIF.
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = w_alvpar-excp_condense.
*... Sums
  cs_layo-no_sumchoice           = w_alvpar-sum_no_sumchoice.
  cs_layo-no_totalline           = w_alvpar-sum_no_sumline.
  cs_layo-totals_before_items    = w_alvpar-sum_before.
  cs_layo-totals_only            = w_alvpar-sum_totals_only.
  cs_layo-totals_text            = w_alvpar-sum_totals_text.
  cs_layo-no_subchoice           = w_alvpar-sum_no_subchoice.
  cs_layo-no_subtotals           = w_alvpar-sum_no_subtotals.
  cs_layo-subtotals_text         = w_alvpar-sum_subtotals_text.
  cs_layo-numc_sum               = w_alvpar-sum_numc.
  cs_layo-no_unit_splitting      = w_alvpar-sum_no_unitsplit.
*... Interaction
  IF w_alvpar-layo_box EQ con_true.
    cs_layo-box_fieldname          = 'BOX'.
  ENDIF.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = w_alvpar-layo_hotspot_field.
  cs_layo-no_input               = space.
  cs_layo-f2code                 = w_alvpar-layo_f2code.
  cs_layo-confirmation_prompt    = w_alvpar-layo_confirmation_prompt.
  cs_layo-key_hotspot            = w_alvpar-layo_keys_hotspot.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = w_alvpar-layo_reprep.
  cs_layo-group_buttons          = space.
  cs_layo-no_keyfix              = w_alvpar-layo_no_keyfix.
  cs_layo-get_selinfos           = w_alvpar-layo_get_selinfo.
  cs_layo-group_change_edit      = w_alvpar-layo_group_change_edit.
  cs_layo-no_scrolling           = w_alvpar-layo_no_scrolling.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = w_alvpar-layo_no_authcheck.
*... Detailed screen
  cs_layo-detail_popup           = space.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = space.
*... others
  cs_layo-list_append            = space.

ENDFORM.                    " f01_set_layo


*&---------------------------------------------------------------------*
*&      Form  zzz_alv_hole_symbol
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_alv_hole_symbol CHANGING ct_symbol TYPE g_ty_t_icon.

  DATA: ls_symbol TYPE icon.

  DEFINE mac_get_symbol.
    ls_symbol-id   = &2.
    ls_symbol-name = &1.
    append ls_symbol to ct_symbol.
  END-OF-DEFINITION.

  mac_get_symbol sym_space              ' ' ."  SPACE
  mac_get_symbol sym_plus_box           '!' ."  box with plus inside
  mac_get_symbol sym_minus_box          '"' ."  box with minus inside
  mac_get_symbol sym_plus_circle        '#' ."  circle with plus inside
  mac_get_symbol sym_minus_circle       '$' ."  circle with minus inside
  mac_get_symbol sym_filled_square      '%' ."  filled square
  mac_get_symbol sym_half_filled_square '&' ."  half-filled square
  mac_get_symbol sym_square            '''' ."  empty square
  mac_get_symbol sym_filled_circle      '(' ."  filled circle
  mac_get_symbol sym_half_filled_circle ')' ."  half-filled circle
  mac_get_symbol sym_circle             '*' ."  empty circle
  mac_get_symbol sym_filled_diamond     '+' ."  filled diamond
  mac_get_symbol sym_diamond            ',' ."  empty diamond
  mac_get_symbol sym_bold_x             '.' ."  diagonal cross (cancel)
  mac_get_symbol sym_note               '/ '."  word balloon, note
  mac_get_symbol sym_document           '0' ."  document
  mac_get_symbol sym_checked_document   '1' ."  document with checkmark
  mac_get_symbol sym_documents          '2 '."  double documents
  mac_get_symbol sym_folder             '3 '."  folder
  mac_get_symbol sym_plus_folder        '4 '."  folder with plus inside
  mac_get_symbol sym_minus_folder       '5 '."  folder with minus inside
  mac_get_symbol sym_open_folder        '6 '."  open folder
  mac_get_symbol sym_bold_minus         '7' ."  bold minus sign
  mac_get_symbol sym_bold_plus          '8' ."  bold plus sign
  mac_get_symbol sym_checkbox           '9 '."  selected checkbox
  mac_get_symbol sym_radiobutton        ': '."  selected radiobutton
  mac_get_symbol sym_left_triangle      ';' ."  triangle pointing left
  mac_get_symbol sym_right_triangle     '<' ."  triangle pointing right
  mac_get_symbol sym_up_triangle        '=' ."  triangle pointing up
  mac_get_symbol sym_down_triangle      '>' ."  triangle pointing down
  mac_get_symbol sym_left_hand          '? '."  hand pointing left
  mac_get_symbol sym_left_arrow         'A' ."  left arrow
  mac_get_symbol sym_right_arrow        'B' ."  right arrow
  mac_get_symbol sym_up_arrow           'C' ."  up arrow
  mac_get_symbol sym_down_arrow         'D' ."  down arrow
  mac_get_symbol sym_check_mark         'E' ."  check mark
  mac_get_symbol sym_pencil             'F' ."  pencil
  mac_get_symbol sym_glasses            'G '."  glasses
  mac_get_symbol sym_locked             'H' ."  closed padlock
  mac_get_symbol sym_unlocked           'I' ."  open padlock
  mac_get_symbol sym_phone              'J '."  telephone
  mac_get_symbol sym_printer            'K '."  printer
  mac_get_symbol sym_fax                'L '."  fax machine
  mac_get_symbol sym_asterisk           'M' ."  asterisk
  mac_get_symbol sym_right_hand         'N '."  hand pointing right
  mac_get_symbol sym_sorted_up          'O '."  sorted ascending
  mac_get_symbol sym_sorted_down        'P '."  sorted descending
  mac_get_symbol sym_cumulated          'Q '."  cumulated
  mac_get_symbol sym_delete             'R' ."  delete mark
  mac_get_symbol sym_executable         'S '."  executable
  mac_get_symbol sym_workflow_item      'T '."  workflow-object
  mac_get_symbol sym_caution            'U '."  caution
  mac_get_symbol sym_flash              'V' ."  express, urgent
  mac_get_symbol sym_large_square       'W '."  large empty square
  mac_get_symbol sym_ellipsis           'X'. "  ellipsis (...)

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zzz_alv_druck
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_alv_druck CHANGING cs_prnt TYPE slis_print_alv.

  cs_prnt-print                  = w_alvpar-prnt_print.
  cs_prnt-prnt_title             = w_alvpar-prnt_with_title.
  cs_prnt-prnt_info              = w_alvpar-prnt_printinfo.
  cs_prnt-no_print_selinfos      = w_alvpar-prnt_no_selinfo.
  cs_prnt-no_print_listinfos     = w_alvpar-prnt_no_listinfo.
  cs_prnt-reserve_lines          = w_alvpar-prnt_reserve_lines.
  cs_prnt-no_coverpage           = w_alvpar-prnt_no_coverpage.
  cs_prnt-no_new_page            = w_alvpar-prnt_no_new_page.
  cs_prnt-no_change_print_params = w_alvpar-prnt_no_change_print_params.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zzz_alv_ereignisse
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_alv_ereignisse CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type     = 0
       IMPORTING
            et_events       = ct_events
       EXCEPTIONS
            list_type_wrong = 1
            OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'SUBTOTAL_TEXT'
                       AND name NE 'GROUPLEVEL_CHANGE'.


    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'Z_ALVEVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form IN        "smart: 2010-08-02 #101
                    CHARACTER MODE .             "smart: 2010-08-02 #101
    ENDLOOP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*  FORM zzz_alv_eigenschaft
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM zzz_alv_eigenschaft CHANGING cs_sett TYPE lvc_s_glay.

  cs_sett-coll_top_p = w_alvpar-sett_minimize_tol.
  cs_sett-coll_end_l = w_alvpar-sett_mimimize_eol.
  cs_sett-top_p_only = w_alvpar-sett_top_only_print.
  cs_sett-eol_p_only = w_alvpar-sett_eol_only_print.
  cs_sett-no_colwopt = w_alvpar-sett_no_colopt_print.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  f01_f4_fcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f01_f4_fcode CHANGING c_value TYPE lvc_s_fcat-fieldname.

  DATA: lt_fcat TYPE slis_t_fieldcat_alv,
        ls_fcat TYPE slis_fieldcat_alv.

*  perform f01_set_fcat changing lt_fcat.

  DATA: lt_values TYPE TABLE OF seahlpres,
        lt_fields TYPE TABLE OF dfies,
        lt_return TYPE TABLE OF ddshretval,
        ls_value  TYPE seahlpres,
        ls_field  TYPE dfies,
        ls_return TYPE ddshretval.

  CLEAR ls_field.
  ls_field-fieldname = 'FIELDNAME'.
  ls_field-intlen    = 30.
  ls_field-leng      = 30.
  ls_field-outputlen = 30.
  ls_field-scrtext_s = ls_field-fieldname.
  ls_field-scrtext_m = ls_field-fieldname.
  ls_field-scrtext_l = ls_field-fieldname.
  ls_field-reptext   = ls_field-fieldname.
  APPEND ls_field TO lt_fields.

  LOOP AT lt_fcat INTO ls_fcat WHERE tech EQ space.
    ls_value-string = ls_fcat-fieldname.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
*     DDIC_STRUCTURE         = ' '
     retfield               = 'FIELDNAME'
*     PVALKEY                = ' '
*     DYNPPROG               = ' '
*     DYNPNR                 = ' '
*     DYNPROFIELD            = ' '
*     STEPL                  = 0
*     WINDOW_TITLE           =
*     VALUE                  = ' '
*     VALUE_ORG              = 'C'
*     MULTIPLE_CHOICE        = ' '
     display                = space
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
   TABLES
     value_tab              = lt_values
     field_tab              = lt_fields
     return_tab             = lt_return
*     DYNPFLD_MAPPING        =
   EXCEPTIONS
     parameter_error        = 1
     no_values_found        = 2
     OTHERS                 = 3.
  IF sy-subrc <> 0 AND sy-subrc NE 3.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_return INTO ls_return
             WITH KEY fieldname = 'FIELDNAME'.
  IF sy-subrc EQ 0.
    c_value = ls_return-fieldval.
  ENDIF.

ENDFORM.                    " f01_f4_fcode

*---------------------------------------------------------------------*
*  FORM f01_alv_event_pf_status_set
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM f01_alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.
                                                            "#EC *

  SET PF-STATUS 'STANDARD_ALV' . " EXCLUDING rt_extab.
  SET TITLEBAR 'STANDARD_ALV'.

ENDFORM.                    "f01_alv_event_pf_status_set


*---------------------------------------------------------------------*
*  FORM F01_ALV_EVENT_USER_COMMAND
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*

FORM f01_alv_event_user_command USING r_ucomm LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.

  DATA: i_ucom LIKE sy-ucomm.
  DATA: w_fieldcat TYPE slis_fieldcat_alv.

  i_ucom = r_ucomm.

  CASE i_ucom.

    WHEN 'ALL'.
      LOOP AT i_outtab INTO w_outtab.
        w_outtab-sel = 'X'.
        MODIFY i_outtab FROM w_outtab.
      ENDLOOP.

    WHEN 'DEM'.
      LOOP AT i_outtab INTO w_outtab.
        w_outtab-sel = ''.
        MODIFY i_outtab FROM w_outtab.
      ENDLOOP.

    WHEN 'ABBR'.
      REFRESH i_outtab.
      EXIT.
    WHEN 'UMBU'.
      DELETE i_outtab WHERE sel <> 'X'.
      LOOP AT lt_fcat INTO w_fieldcat.

        w_fieldcat-edit = ''.
        MODIFY lt_fcat FROM w_fieldcat.
      ENDLOOP.

  ENDCASE.
  rs_selfield-refresh = 'X'.



ENDFORM.                    "f01_alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM z_alvevent_end_of_page
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM z_alvevent_end_of_page.

  WRITE: / text-t02.

  WRITE: / sy-pagno.

ENDFORM.                    "f01_alv_event_end_of_page

*&---------------------------------------------------------------------*
*&      Form  zzz_set_print_params
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_set_print_params .

  CALL FUNCTION 'SET_PRINT_PARAMETERS'
       EXPORTING
            archive_id      = w_alvpar-pri_archive_id
            archive_info    = w_alvpar-pri_archive_info
            archive_mode    = w_alvpar-pri_archive_mode
            archive_text    = w_alvpar-pri_archive_text
            ar_object       = w_alvpar-pri_archive_object
            authority       = w_alvpar-pri_authority
            copies          = w_alvpar-pri_copy
            cover_page      = w_alvpar-pri_coverpage
            data_set        = w_alvpar-pri_dataset
            department      = w_alvpar-pri_department
            destination     = w_alvpar-pri_destination
            expiration      = w_alvpar-pri_expiration
            immediately     = w_alvpar-pri_immediatly
            layout          = w_alvpar-pri_layout
            line_count      = w_alvpar-pri_line_count
            line_size       = w_alvpar-pri_line_size
            list_name       = w_alvpar-pri_list_name
            list_text       = w_alvpar-pri_list_text
            new_list_id     = w_alvpar-pri_new_list_id
            receiver        = w_alvpar-pri_receiver
            release         = w_alvpar-pri_release
            sap_cover_page  = w_alvpar-pri_sap_coverpage
            host_cover_page = w_alvpar-pri_host_coverpage
            priority        = w_alvpar-pri_priority
            sap_object      = w_alvpar-pri_sap_object
            type            = w_alvpar-pri_type
            foot_line       = w_alvpar-pri_footline.

ENDFORM.
*---------------------------------------------------------------------*
*  FORM ZZZ_GET_PRINT_PARAMS
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM zzz_get_print_params.

  DATA: ls_arc_params TYPE arc_params,
        ls_pri_params TYPE pri_params,
        l_valid       TYPE char1.

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
         archive_id                  = w_alvpar-pri_archive_id
         archive_info                = w_alvpar-pri_archive_info
         archive_mode                = w_alvpar-pri_archive_mode
         archive_text                = w_alvpar-pri_archive_text
         ar_object                   = w_alvpar-pri_archive_object
         authority                   = w_alvpar-pri_authority
         copies                      = w_alvpar-pri_copy
         cover_page                  = w_alvpar-pri_coverpage
         data_set                    = w_alvpar-pri_dataset
         department                  = w_alvpar-pri_department
         destination                 = w_alvpar-pri_destination
         expiration                  = w_alvpar-pri_expiration
         immediately                 = w_alvpar-pri_immediatly
         layout                      = w_alvpar-pri_layout
         line_count                  = w_alvpar-pri_line_count
         line_size                   = w_alvpar-pri_line_size
         list_name                   = w_alvpar-pri_list_name
         list_text                   = w_alvpar-pri_list_text
         new_list_id                 = w_alvpar-pri_new_list_id
         no_dialog                   = 'X'
         receiver                    = w_alvpar-pri_receiver
         release                     = w_alvpar-pri_release
         sap_cover_page              = w_alvpar-pri_sap_coverpage
         host_cover_page             = w_alvpar-pri_host_coverpage
         priority                    = w_alvpar-pri_priority
         sap_object                  = w_alvpar-pri_sap_object
         type                        = w_alvpar-pri_type
*        USER                        = SY-UNAME
   IMPORTING
         out_archive_parameters       = ls_arc_params
         out_parameters               = ls_pri_params
         valid                        = l_valid
   EXCEPTIONS
         archive_info_not_found       = 1
         invalid_print_params         = 2
         invalid_archive_params       = 3
         OTHERS                       = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  MOVE-CORRESPONDING ls_arc_params TO w_alvpar.
  MOVE-CORRESPONDING ls_pri_params TO w_alvpar.

ENDFORM.

*---------------------------------------------------------------------*
*  FORM ZZZ_ALV_F4_FUER_VARIANTE.                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM zzz_alv_f4_fuer_variante.

  DATA: w_exit(1).
*
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = g_variant
             i_save             = w_alvpar-vari_save
*           it_default_fieldcat =
       IMPORTING
            e_exit              = w_exit
            es_variant          = gx_variant
       EXCEPTIONS
            not_found = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF w_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form  zzz_alv_check_variante
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zzz_alv_check_variante.
*
  IF NOT p_vari IS INITIAL.
    MOVE g_variant TO gx_variant.
    MOVE p_vari TO gx_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
         EXPORTING
              i_save     = w_alvpar-vari_save
         CHANGING
              cs_variant = gx_variant.
    g_variant = gx_variant.
  ELSE.
    CLEAR g_variant.
    g_variant-report = sy-repid.
  ENDIF.
ENDFORM.                               " PAI_OF_SELECTIO


***********************************************************************
*  FORM z_alvevent_top_of_page
***********************************************************************
FORM z_alvevent_top_of_page.

  CHECK sy-linsz > 79.

ENDFORM.

***********************************************************************
*  FORM z_alvevent_subtotal_text
***********************************************************************
FORM z_alvevent_subtotal_text USING pi_outtab STRUCTURE w_outtab
                                    pio_subtext TYPE slis_subtot_text.

  DATA: w_subtext TYPE slis_subtot_text-display_text_for_subtotal.
  DATA: s_text1(80),
        s_text2(80),
        s_text3(80).

ENDFORM.
***********************************************************************
*  FORM z_alvevent_grouplevel_change
***********************************************************************
FORM z_alvevent_grouplevel_change   USING rs_lineinfo
                                             TYPE slis_lineinfo
                                             rs_groups
                                             TYPE kkblo_grouplevels.

ENDFORM.


************************************************************************
* Form ZZZ_ALV_PARAMETER_SETZEN
************************************************************************
FORM zzz_alv_parameter_setzen.

  w_alvpar-vari_save    = 'A'.
  w_alvpar-vari_default = 'X'.
  w_alvpar-layo_group_change_edit = 'X'.
  w_alvpar-layo_zebra   = 'X'.
  w_alvpar-pri_params_set = 'X'.
  w_alvpar-layo_title = text-001.

ENDFORM.

************************************************************************
* FORM ZZZ_ALV_SORTIERUNG_SETZEN
************************************************************************
FORM zzz_alv_sortierung_setzen CHANGING pio_sorttab
                               TYPE slis_t_sortinfo_alv.

  DATA: w_sorttab TYPE slis_sortinfo_alv.

*** Sortierung nach Name
**  w_sorttab-fieldname = 'NACHN'.
**  w_sorttab-spos      = '1'.
**  w_sorttab-up        = 'X'.
**  APPEND w_sorttab TO pio_sorttab.
**

ENDFORM.

************************************************************************
* Form zzz_alv_feldkatalog
***********************************************************************
FORM zzz_alv_feldkatalog USING i_fieldcat TYPE slis_t_fieldcat_alv .
  ".

  DATA: w_fieldcat TYPE slis_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name       = 'ZLE_GOOD_ALV_OUT'
       CHANGING
            ct_fieldcat            = i_fieldcat
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT i_fieldcat INTO w_fieldcat.
    w_fieldcat-edit = ''.
    CASE w_fieldcat-fieldname.
      WHEN 'SEL'.
        w_fieldcat-edit = 'X'.
        w_fieldcat-checkbox = 'X'.
      WHEN 'NALP'.
        w_fieldcat-edit = 'X'.
    ENDCASE.
    MODIFY i_fieldcat FROM w_fieldcat.

  ENDLOOP.


ENDFORM.
************************************************************************
*      Form  zzz_alv_html_top
************************************************************************
*       text
************************************************************************
FORM z_alv_event_html_top_of_page USING cl_dd
                      TYPE REF TO cl_dd_document.


*  DATA: lt_texts TYPE sdydo_text_table,
*        w_text1(255),
*        w_dch01(10),
*        w_dch02(10).
*
*  CALL METHOD cl_dd->add_gap
*             EXPORTING width = 130.
*
*  CALL METHOD cl_dd->add_text
*              EXPORTING text         = text-f01
*                        sap_style    = cl_dd_document=>heading
*                        sap_color    = cl_dd_document=>list_heading_int
*                        sap_fontsize = cl_dd_document=>large
*                        sap_emphasis = cl_dd_document=>strong
*                        style_class  = space.
*
*  CALL METHOD cl_dd->new_line
*              EXPORTING repeat = 2.
*
*  CALL METHOD cl_dd->add_gap
*             EXPORTING width = 60.
*
*  WRITE w_begda TO w_dch01.
*  CONCATENATE text-f02 w_dch01 INTO w_text1
*  SEPARATED BY space.
*  CONDENSE w_text1.
*  CALL METHOD cl_dd->add_text
*              EXPORTING text         = w_text1
*                        sap_style    = space
*                        sap_color    = space
*                        sap_fontsize = cl_dd_document=>medium
*                        sap_emphasis = cl_dd_document=>strong
*                        style_class  = space.

ENDFORM.                               " f01_alv_event_html_top_of_page
