program zle_goodmove_01.

call screen 100.
include: zle_goodmove_01_data.
include: zle_goodmove_01_sel.

include: zle_goodmove_01_f00.

include zle_goodmove_btci.
include zle_goodmove_peti.

*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
module pbo output.
  data: w_fieldcat type line of lvc_t_fcat.

  set pf-status 'MAIN100'.
  set titlebar 'MAIN100'.
  if g_custom_container is initial.
    create object g_custom_container
           exporting container_name = g_container.
    create object grid1
           exporting i_parent = g_custom_container.


    call function 'LVC_FIELDCATALOG_MERGE'
     exporting
*   I_BUFFER_ACTIVE              =
       i_structure_name             = 'ZLE_GOOD_ALV_OUT'
*   I_CLIENT_NEVER_DISPLAY       = 'X'
*   I_BYPASSING_BUFFER           =
      changing
        ct_fieldcat                  = gt_fieldcat
     exceptions
       inconsistent_interface       = 1
       program_error                = 2
       others                       = 3
              .
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.


*    LOOP AT  gt_fieldcat  INTO w_fieldcat WHERE fieldname = 'SEL'.
*      w_fieldcat-checkbox = 'X'.
*      MODIFY gt_fieldcat FROM w_fieldcat.
*    ENDLOOP.

    loop at  gt_fieldcat  into w_fieldcat.
      w_fieldcat-no_out = 'X'.
      case w_fieldcat-fieldname.
        when 'SEL'.
          w_fieldcat-checkbox = 'X'.
          modify gt_fieldcat from w_fieldcat.
        when 'MATNR'.
          w_fieldcat-no_out = ''.
        when 'MAKTX'.
          w_fieldcat-no_out = ''.
        when 'LGPLA'.
          w_fieldcat-no_out = ''.
        when 'CHARG'.
          w_fieldcat-no_out = ''.
        when 'BESTQ'.
          w_fieldcat-no_out = ''.
        when 'VERME'.
          w_fieldcat-no_out = ''.
        when 'MEINS'.
          w_fieldcat-no_out = ''.
        when 'WERKS'.
          w_fieldcat-no_out = ''.
        when 'LGORT'.
          w_fieldcat-no_out = ''.
        when 'LGNUM'.
          w_fieldcat-no_out = ''.
        when 'MANME'.
          w_fieldcat-no_out = ''.
        when 'WDATU'.
          w_fieldcat-no_out = ''.
      endcase.
      modify gt_fieldcat from w_fieldcat.

    endloop.

    assign gt_outtab to <itab>.
*    PERFORM select_data.
    perform select_data_and_init_style.

*§3.Provide the fieldname of the celltab field by using field
*   STYLEFNAME of the layout structure.
    gs_layout-stylefname = 'CELLTAB'.


***** ALV anbinden
**  CREATE OBJECT grid1
**        EXPORTING
**          i_parent          = g_custom_container
**          i_appl_events     = 'X'
**        EXCEPTIONS
**          OTHERS            = 5.
**
*** Edit-Event extra registrieren
    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.


*** ...und EventHandler zuweisen
    create object ref_alv_events.
    set handler ref_alv_events->handle_data_changed for grid1.


* set substate of editable cells to deactivated
    call method grid1->set_ready_for_input
          exporting i_ready_for_input = 1.

    call method grid1->set_table_for_first_display
         exporting i_structure_name = 'ZLE_GOOD_ALV_OUT'
                   is_layout        = gs_layout
         changing  it_outtab        = gt_outtab[]
                   it_fieldcatalog  = gt_fieldcat[].

  endif.
endmodule.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
module pai input.

  save_ok = ok_code.
  clear ok_code.
  case save_ok.

    when 'EXIT'.
      perform exit_program.

    when 'ABBR'.
      perform exit_program.

    when 'SWITCH'.
      perform switch_edit_mode.

    when 'REFR'.
      perform refresh_data.

    when 'SUBM'.
      perform tabelle_bereinigen.
      perform selekt.

    when 'AFRNQU'.
      g_bestkz_1 = ' '.
      g_bestkz_2 = 'Q'.
      g_bewart = '322'.
      g_bewart_2 = '322'.    "Bewegungsart manuelle Gewichtseingabe
      perform selekt.
      perform alle_frei_nach_q.
      perform tabelle_bereinigen.
      perform refresh_data.

    when 'AQUNFR'.
      g_bestkz_1 = 'Q'.
      g_bestkz_2 = ' '.
      g_bewart =  '321'.
      g_bewart_2 = '321'.    "Bewegungsart manuelle Gewichtseingabe
      perform selekt.
      perform alle_q_nach_frei.
      perform tabelle_bereinigen.
      perform refresh_data.

    when 'ANLAP'.
      g_bwlvs = '998'.
      g_bewart_2 = '998'.    "Bewegungsart manuelle Gewichtseingabe
      perform selekt.
      perform wait.
      perform alle_neuer_lgpl.
      perform tabelle_bereinigen.
      perform refresh_data.

    when 'ENLAP'.
      g_bwlvs = '998'.
      g_bewart_2 = '998'.    "Bewegungsart manuelle Gewichtseingabe
      perform wait.
      perform selekt.
      perform wait.
      perform einzeln_neuer_lgpl.
      perform tabelle_bereinigen.
      perform refresh_data.

    when 'FREISPER'.
      g_bestkz_1 = ' '.
      g_bestkz_2 = 'S'.
      g_bewart = '344'.
      g_bewart_2 = '344'.    "Bewegungsart manuelle Gewichtseingabe

      perform selekt.
      perform wait.
      perform frei_an_sper.
      perform tabelle_bereinigen.
      perform refresh_data.

    when 'SPERFREI'.
      g_bestkz_1 = 'S'.
      g_bestkz_2 = ' '.
      g_bewart = '343'.
      g_bewart_2 = '343'.    "Bewegungsart manuelle Gewichtseingabe
      perform selekt.
      perform wait.
      perform sperr_an_frei.
      perform tabelle_bereinigen.
      perform refresh_data.

    when others.
*     do nothing

  endcase.
endmodule.
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
form exit_program.

  leave to screen 0.
*  LEAVE PROGRAM.
endform.
*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PT_CELLTAB  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form fill_celltab using value(p_mode) TYPE CLIKE "smart: 2010-08-02 #105
                  changing pt_celltab type lvc_t_styl.
  data: ls_celltab type lvc_s_styl,
        l_mode type raw4.
* This forms sets the style of column 'PRICE' editable
* according to 'p_mode' and the rest to read only either way.

  if p_mode eq 'RW'.
*§2a.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_ENABLED to set a cell
*    to status "editable".
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  else. "p_mode eq 'RO'
*§2b.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_DISABLED to set a cell
*    to status "non-editable".
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  endif.

  ls_celltab-fieldname = 'SEL'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.
  ls_celltab-fieldname = 'NALP'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.
  ls_celltab-fieldname = 'MANME'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.


endform.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form switch_edit_mode.

  if grid1->is_ready_for_input( ) eq 0.
* set edit enabled cells ready for input
    call method grid1->set_ready_for_input
                     exporting i_ready_for_input = 1.

  else.
* lock edit enabled cells against input
    call method grid1->set_ready_for_input
                    exporting i_ready_for_input = 0.
  endif.
endform.                               " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  wait
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form wait.

  wait up to 2 seconds.

endform.                    " wait
