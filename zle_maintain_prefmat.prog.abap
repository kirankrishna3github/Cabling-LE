*&--------------------------------------------------------------------*
*& Report  ZLE_MAINTAIN_PREFMAT
*&
*& Developer:        Markus Raffeiner
*& Creation of date: 01.04.2014
*& Version:          1.0
*&--------------------------------------------------------------------*
*& Maintenance of table ZLE_PREF_MAT via Excel upload or via screen
*  input
*&--------------------------------------------------------------------*
*& Changes
*&
*&--------------------------------------------------------------------*

REPORT ZLE_MAINTAIN_PREFMAT.

data: ok_code like sy-ucomm,
      save_ok like sy-ucomm,
      gv_ucomm like sy-ucomm,
      g_container type scrfname value 'ALV_GRID_0100',
      g_grid  type ref to cl_gui_alv_grid,
***      g_alv_evt_recv     type ref to lcl_event_receiver,
      g_custom_container type ref to cl_gui_custom_container,
      gt_fieldcat type lvc_t_fcat,
      gs_layout type lvc_s_layo,
      g_max type i value 100,
      g_success type c.

*local class to handle semantic checks
class lcl_event_receiver definition deferred.

data: g_verifier type ref to lcl_event_receiver.

types: begin of t_out.
        include structure zsprefmat.
types:  stat_init type i_status.
***types:  updkz_init type updkz_d.
***types:  updkz type updkz_d.
types: celltab type lvc_t_styl.
types: end of t_out.

data: gs_out type t_out.
data: gt_out type standard table of t_out.
data: gt_out_save type standard table of t_out.

data: gv_edit type c.
data: gv_new type c.
data: gv_refresh type c.
data: gv_answer type c.
data: gv_mtart type mtart.

data: gs_stable      type lvc_s_stbl.
data: gv_soft_refresh type char01.

DATA: gv_subrc TYPE sy-subrc.
***DATA: gs_msg TYPE bal_s_msg.
***DATA: gt_msg TYPE STANDARD TABLE OF bal_s_msg.
DATA: gv_loghandle TYPE balloghndl.
data: gv_loop_start type i.
data: gv_cancel_process type c.

data: gv_anz_read type i.
data: gv_anz_ins type i.
data: gv_anz_del type i.
data: gv_anz_no_del type i.

data: gv_anz_pref_eu_ins_err type i.
data: gv_anz_pref_eu_upd type i.
data: gv_anz_pref_eu_ins type i.

data: gv_anz_pref_tr_ins_err type i.
data: gv_anz_pref_tr_upd type i.
data: gv_anz_pref_tr_ins type i.

data: gv_anz_pref_il_ins_err type i.
data: gv_anz_pref_il_upd type i.
data: gv_anz_pref_il_ins type i.

data: gv_anz_pref_ae_ins_err type i.
data: gv_anz_pref_ae_upd type i.
data: gv_anz_pref_ae_ins type i.
data: gv_anz_missing_mat type i.

data: gv_any_changes type c.
data: gv_pfstatus type syst_pfkey.
data: gt_ucomm type standard table of sy-ucomm.

* Eingabestruktur für uploadfile
types: begin of t_input,
         matnr type matnr, "Materialnummer
         maktx type maktx, "Materialkurztext
         prdha type prodh_d, "Produkthierarchie
         eu_fremd type c length 8, "Fremdanteil EU
         pref_eu type c length 4, "Präferenzberechtigt für EU
         noneu_fremd type c length 8, "Fremdanteil nicht EU
         pref_cn type c length 4, "Präferenzberechtigt für China
         pref_il type c length 4, "Präferenzberechtigt für israel
         pref_ae type c length 4, "Präferenzberechtigt für Dubai
       end of t_input.

data: gs_in type t_input.
data: gt_in type standard table of t_input.

* Inputtabelle des Upload-Files
types: t_upload type c length 2000.

data: gs_upload type t_upload.
data: gt_upload type standard table of t_upload.

TYPES: BEGIN OF t_allowed_mtart,
         mtart TYPE mtart,
       END OF t_allowed_mtart.

DATA: gs_allowed_mtart TYPE t_allowed_mtart.
DATA: gt_allowed_mtart TYPE STANDARD TABLE OF t_allowed_mtart.

DATA: gs_usauth TYPE zpref_usauth.
DATA: gt_usauth TYPE STANDARD TABLE OF zpref_usauth.

DATA: gs_log TYPE zsprefmat_log.
DATA: gt_log TYPE STANDARD TABLE OF zsprefmat_log.

***data: begin of gt_out occurs 0.     "with header line
***        include structure zsprefmat.
***data: celltab type lvc_t_styl.
***data: end of gt_out.


***data: g_carrid like sflight-carrid,
***      g_connid like sflight-connid.

data: BEGIN of gs_maske,
        ',',
        f01(30),
        ',',
        '*.csv',
        ',',
        f02(30),
        ',',
        '*.*',
        '.',
      END OF gs_maske.

constants: gc_yes type c value '1'.
constants: gc_no type c value '2'.
constants: gc_cancel type c value 'A'.
constants: gc_cancel_button type c value 'X'.
constants: gc_on type c value 'X'.
constants: gc_insert type updkz_d value 'I'.
constants: gc_update type updkz_d value 'U'.
constants: gc_delete type updkz_d value 'D'.
constants: gc_display type updkz_d value 'S'.
constants: gc_delim type c value ';'.
constants: gc_input_ja type c length 4 value 'JA'.
constants: gc_input_nein type c length 4 value 'NEIN'.


*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
class lcl_event_receiver definition.

*.............

  public section.

    types: begin of zle_pref_mat_key.
    types:   matnr type matnr.
    types:   xegld type xegld.
    types:   land type land1.
    types: end of zle_pref_mat_key.

    types: zle_pref_mat_keys type standard table of zle_pref_mat_key,
           zle_pref_mat_table type standard table of zle_pref_mat.

    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed.

    methods:
      handle_user_command
         for event user_command of cl_gui_alv_grid
            importing e_ucomm.

    methods:
      handle_toolbar
         for event toolbar of cl_gui_alv_grid
            importing e_object e_interactive.

    methods:
      build_toolbar
         importing i_object type ref to cl_alv_event_toolbar_set.

    methods:
      get_inserted_rows
           exporting
              inserted_rows type zle_pref_mat_keys.

    methods:
      get_deleted_rows
          exporting
              deleted_rows type zle_pref_mat_table.

    methods:
       refresh_delta_tables.

    methods:
       set_table_is_initial.

    methods:
       set_table_is_not_initial.

    methods:
       table_is_initial
                returning value(initial) type char01.

*..............

  private section.

* §4.Define internal tables to remember inserted and deleted lines,
*    thus the delta between input made after the last saving.

    data: inserted_rows type zle_pref_mat_keys,
          deleted_rows type standard table of zle_pref_mat.


* This flag is set if any error occured in one of the
* following methods:
    data: error_in_data type c.

* This flag signals that no records were read for the flight
* table initially:
    data: initial_table type c.
** Methods to modularize event handler method HANDLE_DATA_CHANGED:
*
    methods:
      check_double_entries
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

    methods:
      check_if_mtart_is_allowed
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

    methods:
      update_delta_tables
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

    methods:
      perform_semantic_checks
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

    methods:
      get_cell_values
           importing
             row_id          type int4
             pr_data_changed type ref to cl_alv_changed_data_protocol
           exporting
             key             type zle_pref_mat_key.

    methods:
      refresh_grid.

    methods:
      popup_to_confirm
           importing
             question      type clike
             cancel        type clike
           exporting
             answer        type c.

endclass.
**---------------------------------------------------------
*
**---------------------------------------------------------
class lcl_event_receiver implementation.

  method handle_data_changed.
*
    data: ls_good type lvc_s_modi,
          ls_celltab type lvc_s_styl,
          l_price type s_price,
          ls_new type lvc_s_moce.

    error_in_data = space.

*   check if there exist double entries
    call method check_double_entries( er_data_changed ).

*   check if user has the authority to maintain the material type
    call method check_if_mtart_is_allowed( er_data_changed ).

*   remember new or deleted lines for saving
    call method update_delta_tables( er_data_changed ).

*   check mt_good_cells semantically
    call method perform_semantic_checks( er_data_changed ).

    if error_in_data = 'X'.
      call method er_data_changed->display_protocol.
    endif.

   gs_stable-row  = 'X'.
   gs_stable-col  = 'X'.
   gv_soft_refresh = space.

   call method g_grid->refresh_table_display
     EXPORTING
       is_stable      = gs_stable
       i_soft_refresh = space.

  endmethod.

*-----------------------------------------------------------------------
  method handle_user_command.

  data: ls_celltab type lvc_s_styl,
        l_mode type raw4,
        lt_sel_rows type lvc_t_row,
        ls_sel_row type lvc_s_row,
        ls_out type t_out,
        lv_tabix type sy-tabix,
        ls_cells TYPE lvc_s_cell,
        lv_found type c,
        lv_row_id type i.


  gs_stable-row  = 'X'.
  gs_stable-col  = 'X'.
  gv_soft_refresh = space.

  case e_ucomm.
* ---------------------------------------------------------------------
*   New
* ---------------------------------------------------------------------
    when 'NEW'.
      clear gs_out.

      clear ls_celltab.
      l_mode = cl_gui_alv_grid=>mc_style_enabled.
      ls_celltab-fieldname = 'MATNR'.
      ls_celltab-style = l_mode.
      insert ls_celltab into table gs_out-celltab.

      gs_out-stat_init = icon_create.
      gs_out-stat = icon_create.
      gs_out-stat_init = icon_create.
***      gs_out-updkz_init = gc_insert.
***      gs_out-updkz = gc_insert.
      append gs_out to gt_out.

      CALL METHOD g_verifier->refresh_grid.
      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable      = gs_stable
          i_soft_refresh = space.

*    Set Cursor
     clear ls_cells.

     describe table gt_out lines lv_row_id.

     ls_cells-row_id-index = lv_row_id.
     ls_cells-col_id-fieldname = 'MATNR'.

     CALL METHOD g_grid->set_current_cell_via_id
       EXPORTING
         is_row_id    = ls_cells-row_id
         is_column_id = ls_cells-col_id.

     CALL METHOD g_grid->set_focus
       EXPORTING
         control = g_grid.

     gs_stable-row  = 'X'.
     gs_stable-col  = 'X'.
     gv_soft_refresh = space.

     call method g_grid->refresh_table_display
       EXPORTING
         is_stable      = gs_stable
         i_soft_refresh = space.

* ---------------------------------------------------------------------
*   Edit
* ---------------------------------------------------------------------
    when 'EDIT'.
       IF g_grid->is_ready_for_input( ) EQ 0.
*  §4.Use SET_READY_FOR_INPUT to switch between the substates.
         CALL METHOD g_grid->set_ready_for_input
                          EXPORTING i_ready_for_input = 1.
       ELSE.
         CALL METHOD g_grid->set_ready_for_input
                          EXPORTING i_ready_for_input = 0.
       ENDIF.

* ---------------------------------------------------------------------
*   Save
* ---------------------------------------------------------------------
    when 'SAVE'.

* ---------------------------------------------------------------------
*   Delete row
* ---------------------------------------------------------------------
    when 'DELETE_ROW'.
     clear lv_found.

     CALL METHOD g_grid->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = lt_sel_rows.

*    Only allow new entries to delete, which are not stored in DB table
*    yet
     LOOP AT lt_sel_rows INTO ls_sel_row.
       READ table gt_out INTO gs_out INDEX ls_sel_row-index.
       IF gs_out-stat eq icon_create.
         lv_found = gc_yes.
         EXIT.
       ENDIF.
     ENDLOOP.

     IF lv_found = gc_yes. "Entry found
       CALL METHOD popup_to_confirm
         EXPORTING
           question       = text-pc4
           cancel         = gc_cancel_button
         IMPORTING
           answer = gv_answer.

       IF gv_answer = gc_yes.
         LOOP AT lt_sel_rows INTO ls_sel_row.
           READ table gt_out INTO gs_out INDEX ls_sel_row-index.
           IF sy-subrc EQ 0.
             gs_out-stat = icon_delete.
*  **          gs_out-updkz = gc_delete.
             DELETE gt_out INDEX ls_sel_row-index.
           ENDIF.
         ENDLOOP.
        ENDIF.
      ELSE.
        message s001(aq) with text-m02. "No entry exists for deletion
      ENDIF.

     gs_stable-row  = 'X'.
     gs_stable-col  = 'X'.
     gv_soft_refresh = space.

     call method g_grid->refresh_table_display
       EXPORTING
         is_stable      = gs_stable
         i_soft_refresh = space.


* ---------------------------------------------------------------------
*   Delete: Set dicon of deleiton in order to delte avter saving
* ---------------------------------------------------------------------
    when 'DELETE'.
      CALL METHOD g_grid->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = lt_sel_rows.

      LOOP AT lt_sel_rows INTO ls_sel_row.
        READ table gt_out INTO gs_out INDEX ls_sel_row-index.
        IF sy-subrc EQ 0.
          gs_out-stat = icon_delete.
***          gs_out-updkz = gc_delete.
          MODIFY gt_out FROM gs_out INDEX ls_sel_row-index.
        ENDIF.
      ENDLOOP.

     gs_stable-row  = 'X'.
     gs_stable-col  = 'X'.
     gv_soft_refresh = space.

     call method g_grid->refresh_table_display
       EXPORTING
         is_stable      = gs_stable
         i_soft_refresh = space.

* ---------------------------------------------------------------------
*   Undo deletion
* ---------------------------------------------------------------------
    when 'DELETE_UNDO'.
      CALL METHOD g_grid->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = lt_sel_rows.

      LOOP AT lt_sel_rows INTO ls_sel_row.
        READ table gt_out INTO gs_out INDEX ls_sel_row-index.
        IF sy-subrc EQ 0 AND
          gs_out-stat = icon_delete.
          gs_out-stat = gs_out-stat_init.
          MODIFY gt_out FROM gs_out INDEX ls_sel_row-index.
        ENDIF.
      ENDLOOP.

     gs_stable-row  = 'X'.
     gs_stable-col  = 'X'.
     gv_soft_refresh = space.

     call method g_grid->refresh_table_display
       EXPORTING
         is_stable      = gs_stable
         i_soft_refresh = space.

    endcase.

  endmethod.


*-----------------------------------------------------------------------
  method handle_toolbar.

    call method build_toolbar
      exporting
        i_object = e_object.

  endmethod.

*-----------------------------------------------------------------------
  method build_toolbar.
    DATA: ls_toolbar  TYPE stb_button.

*   add separator
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO i_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'NEW'.
    ls_toolbar-quickinfo = text-t01.
    ls_toolbar-icon = icon_create.
*    ls_toolbar-text = text-t01.
    APPEND ls_toolbar TO i_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'EDIT'.
    ls_toolbar-quickinfo = text-t02.
    ls_toolbar-icon = icon_toggle_display_change.
    APPEND ls_toolbar TO i_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'DELETE_ROW'.
    ls_toolbar-quickinfo = text-t03.
    ls_toolbar-icon = icon_delete_row.
    APPEND ls_toolbar TO i_object->mt_toolbar.

*   Seperator
    ls_toolbar-function  = 'DUMMY'.
    ls_toolbar-butn_type = '3'.
    append ls_toolbar to i_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 0.
    ls_toolbar-function = 'DELETE'.
    ls_toolbar-quickinfo = text-t04.
    ls_toolbar-icon = icon_delete.
    APPEND ls_toolbar TO i_object->mt_toolbar.

    CLEAR ls_toolbar.
    ls_toolbar-function = 'DELETE_UNDO'.
    ls_toolbar-quickinfo = text-t05.
    ls_toolbar-icon = icon_system_undo.
    APPEND ls_toolbar TO i_object->mt_toolbar.


***    CLEAR ls_toolbar.
***    ls_toolbar-function = 'SAVE'.
***    ls_toolbar-quickinfo = text-t02.
***    ls_toolbar-icon = icon_system_save.
***    APPEND ls_toolbar TO i_object->mt_toolbar.

***    CLEAR ls_toolbar.
***    ls_toolbar-function = 'REFRESH'.
***    ls_toolbar-quickinfo = text-t01.
***    ls_toolbar-icon = icon_refresh.
***    APPEND ls_toolbar TO i_object->mt_toolbar.



  endmethod.


*-----------------------------------------------------------------------
  method check_double_entries.
    data: lt_good_cells type lvc_t_modi,
          ls_good type lvc_s_modi,
          ls_key type zle_pref_mat_key,
          ls_zle_pref_mat type zle_pref_mat,
          l_matnr type matnr,
          lv_matnr type matnr,
          l_del_row type lvc_s_moce,
          ls_out like line of gt_out,
          l_reentered type c.

* Check if there exist already other records with equal key fields.

* Check if the user has entered two new lines where the key fields
* are equal.

*The check is restrained to field MATNR.
*
* Algorithm: Copy all entries in MT_GOOD_CELLS to a dummy table.
*            During the copying procedure check if there exists
*            already a line with the same material number.
*

* ---------------------------------------------------------------------
* Copy MT_GOOD_CELLS into lt_good_cells and check if already a line
* with the same material number exists
* ---------------------------------------------------------------------
    loop at pr_data_changed->mt_good_cells into ls_good.

      case ls_good-fieldname.
        when 'MATNR'.

          call method pr_data_changed->get_cell_value
                      exporting
                            i_row_id = ls_good-row_id
                            i_fieldname = ls_good-fieldname
                      importing e_value = l_matnr.

          read table lt_good_cells
            with key value = l_matnr
            transporting no fields.

          if sy-subrc = 0.
*           There exists already a line with the same material number
            write l_matnr to lv_matnr no-zero.
            call method pr_data_changed->add_protocol_entry
                         exporting
              i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
              i_msgv1 = lv_matnr
              i_msgv2 = text-m01
              i_fieldname = ls_good-fieldname
              i_row_id = ls_good-row_id.

            error_in_data = 'X'.
          else.  "no double entry  -->insert into table lt_good_cells
            ls_good-value = l_matnr.
            append ls_good to lt_good_cells.
          endif.

      endcase.

    endloop.

* ---------------------------------------------------------------------
*   Check if any new entries already exist in gt_out.
*   At this point, lt_good_cells contains only lines with
*   FIELDNAME = 'MATNR'.
* ---------------------------------------------------------------------
    loop at lt_good_cells into ls_good.

      l_matnr = ls_good-value.      "Material number

      read table gt_out with key
                    matnr = l_matnr
                    transporting no fields.

      if sy-subrc = 0.
*       Check if this entry was deleted before, i.e. it is in the table
*       of deleted rows. If so, the entry does not exist twice. The user
*       has deleted a line and then reentered it.
        l_reentered = space.

        loop at pr_data_changed->mt_deleted_rows into l_del_row.
          read table gt_out into ls_out index l_del_row-row_id.
          if sy-subrc ne 0.
            message i000(0k) with text-e01."Fehler beim Löschen
          elseif
***                    ls_outtab-carrid eq g_carrid
***                and ls_outtab-connid eq g_connid
***                and ls_outtab-fldate eq ls_good-value.
            l_reentered = 'X'.
          endif.
        endloop.

        if l_reentered ne 'X'.
          write l_matnr to lv_matnr no-zero.
          call method pr_data_changed->add_protocol_entry
                         exporting
              i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
              i_msgv1 = lv_matnr
              i_msgv2 = text-m01
              i_fieldname = ls_good-fieldname
              i_row_id = ls_good-row_id.
          error_in_data = 'X'.
        endif.
      endif.
    endloop.

*   In this demo report you may prevent the selection
*   of data by setting parameter 'p_ds'.
*   If this is done, the next check is required:
*****    if me->table_is_initial( ) eq 'X'.
*****      call method get_cell_values
*****           exporting row_id          = 1
*****                     pr_data_changed = pr_data_changed
*****           importing key             = ls_key.
*****
*****      select single * from zle_pref_mat into ls_zle_pref_mat
*****                where matnr = ls_key-matnr
*****                  and xegld = ls_key-xegld
*****                  and land = ls_key-land.
*****
*****      if sy-subrc = 0.
*****        call method pr_data_changed->add_protocol_entry
*****                       exporting
*****            i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
*****            i_msgv1 = text-m01
*****            i_fieldname = 'MATNR'
*****            i_row_id = 1.
*****
*****        error_in_data = 'X'.
*****      endif.
******   flag initial_table is reset in method 'update_delta_tables'
*****    endif.

  endmethod.

*-----------------------------------------------------------------------
  method check_if_mtart_is_allowed.

    data: lt_good_cells type lvc_t_modi,
          ls_good type lvc_s_modi,
          ls_key type zle_pref_mat_key,
          ls_zle_pref_mat type zle_pref_mat,
          l_matnr type matnr,
          lv_matnr type matnr,
          lv_mtart type mtart,
          l_del_row type lvc_s_moce,
          ls_out like line of gt_out,
          l_reentered type c.


* ---------------------------------------------------------------------
* Copy MT_GOOD_CELLS into lt_good_cells and check if already a line
* with the same material number exists
* ---------------------------------------------------------------------
    loop at pr_data_changed->mt_good_cells into ls_good.

      case ls_good-fieldname.
        when 'MATNR'.

          call method pr_data_changed->get_cell_value
                      exporting
                            i_row_id = ls_good-row_id
                            i_fieldname = ls_good-fieldname
                      importing e_value = l_matnr.

          clear lv_mtart.

          select single mtart from mara into lv_mtart
            where matnr = l_matnr.

          read table gt_allowed_mtart into gs_allowed_mtart
            with key mtart = lv_mtart.

          if sy-subrc ne 0.
*           There exists already a line with the same material number
            write l_matnr to lv_matnr no-zero.
            call method pr_data_changed->add_protocol_entry
                         exporting
              i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
              i_msgv1 = lv_matnr
              i_msgv2 = lv_mtart
              i_msgv3 = text-p03
              i_fieldname = ls_good-fieldname
              i_row_id = ls_good-row_id.

            error_in_data = 'X'.
          else.  "no double entry  -->insert into table lt_good_cells
            ls_good-value = l_matnr.
            append ls_good to lt_good_cells.
          endif.

      endcase.

    endloop.


  endmethod.

*-------------------------------------------------------
  method update_delta_tables.
    data: l_ins_row type lvc_s_moce,
          l_del_row type lvc_s_moce,
          ls_key type zle_pref_mat_key,
          ls_prefmat type zsprefmat,
          ls_out like line of gt_out.

* §6.Use protocol attributes MT_DELETED_ROWS and MT_INSERTED_ROWS
*    to remember which lines where deleted or inserted. Save this
*    information in your internal tables.

*..........
* deleted rows
*.............
    loop at pr_data_changed->mt_deleted_rows into l_del_row.
      read table gt_out into ls_out index l_del_row-row_id.
      if sy-subrc ne 0.
        message i000(0k) with text-e01."Fehler beim Löschen
      else.
        move-corresponding ls_out to ls_prefmat.
* It should no be possible that the same line is deleted twice,
* so we just add the new key line to 'deleted_rows'.
        append ls_prefmat to deleted_rows.
* If this line was inserted just before it is deleted:
        delete me->inserted_rows
             where matnr = ls_out-matnr.
      endif.
    endloop.

*..........
* inserted rows
* At this point ALV has not added new lines
* to gt_out, so you can not access their values
* by reading gt_out.
* Table MT_GOOD_CELLS holds new values that can be
* referenced using the ROW_ID.
*..........
    if me->table_is_initial( ) eq 'X'.
* No materials were selected initially. This is the first new line.
      call method get_cell_values
            exporting row_id          = 1
                      pr_data_changed = pr_data_changed
            importing key             = ls_key.

      append ls_key to inserted_rows.
      call method me->set_table_is_not_initial.
    endif.

    loop at pr_data_changed->mt_inserted_rows into l_ins_row.
      call method get_cell_values
              exporting row_id          = l_ins_row-row_id
                        pr_data_changed = pr_data_changed
              importing key             = ls_key.

*      READ TABLE gt_out INTO ls_out INDEX l_ins_row-row_id.

* Just insert the new row regardless if the input is wrong
      append ls_key to inserted_rows.
    endloop.

  endmethod.
*---------------------------------------------------------
  method get_cell_values.
* get values of key cells of row ROW_ID

* MATNR
    call method pr_data_changed->get_cell_value
          exporting
                 i_row_id    = row_id
                 i_fieldname = 'MATNR'
               importing
                 e_value = key-matnr.

    if sy-subrc ne 0.
      message i000(0k) with text-e02.  "Fehler beim Einfügen
    endif.

  endmethod.

*---------------------------------------------------------
  method perform_semantic_checks.

    data: ls_good type lvc_s_modi,
          lv_matnr type matnr,
          ls_pref_mat type zle_pref_mat,
          lt_pref_mat type standard table of zle_pref_mat,
          lv_record_exists type c,
          lv_tabix type sy-tabix,
          lv_first type c,
          lv_mode type raw4,
          lt_good type LVC_T_MODI,
          ls_celltab type lvc_s_styl,
          lt_celltab type lvc_t_styl.

    clear lt_good.

    loop at pr_data_changed->mt_good_cells into ls_good.

      lv_tabix = sy-tabix.

      clear gs_out.
      read table gt_out into gs_out index ls_good-row_id.

      if sy-subrc eq 0.
*       case gs_out-stat_vorher.
        case gs_out-stat.

* ---------------------------------------------------------------------
*         New entry
* ---------------------------------------------------------------------
          when icon_create.
            clear: ls_pref_mat, lv_record_exists, lt_pref_mat.

            append ls_good to lt_good.

            select * from zle_pref_mat into table lt_pref_mat
              where matnr = ls_good-value.

*           If entry always exists in table zöe_pref_mat
*           get data from table and set status as display
            loop at lt_pref_mat into ls_pref_mat.

*              at first.
               gs_out-matnr = ls_pref_mat-matnr.
               gs_out-erdat = ls_pref_mat-erdat.
               gs_out-erzet = ls_pref_mat-erzet.
               gs_out-ernam = ls_pref_mat-ernam.

               gs_out-laeda = ls_pref_mat-laeda.
               gs_out-laeze = ls_pref_mat-laeze.
               gs_out-aenam = ls_pref_mat-aenam.
               gs_out-stat = icon_display.
               gs_out-stat_init = icon_create.
*              endat.

               case ls_pref_mat-land.
                 when 'AE'.
                   gs_out-pref_ae = ls_pref_mat-pref.
                 when 'TR'.
                   gs_out-pref_tr = ls_pref_mat-pref.
                 when 'IL'.
                   gs_out-pref_il = ls_pref_mat-pref.
                 when others.
                   if ls_pref_mat-xegld = 'X'.
                     gs_out-pref_eu = ls_pref_mat-pref.
                   endif.
               endcase.

            endloop.

            select single mtart from mara into gs_out-mtart
              where matnr = ls_good-value.

            select single maktx from makt into gs_out-maktx
              where matnr = ls_good-value and
                    spras = sy-langu.

            refresh lt_celltab.
            clear ls_celltab.

            loop at gs_out-celltab into ls_celltab
              where fieldname = 'MATNR'.
              lv_mode = cl_gui_alv_grid=>mc_style_disabled.
              ls_celltab-style = lv_mode.
              modify gs_out-celltab from ls_celltab index sy-tabix.
            endloop.

          when space.
            gs_out-erdat = sy-datum.
            gs_out-erzet = sy-uzeit.
            gs_out-ernam = sy-uname.
            gs_out-stat = icon_create.

          when icon_display or
               icon_system_save or
               icon_defect.
            gs_out-stat = icon_change.
**            gs_out-updkz = gc_update.
        endcase.

        modify gt_out from gs_out index ls_good-row_id.

**        if gs_out-stat ne icon_create.
**          gs_out-stat = icon_change.
**          modify gt_out from gs_out index ls_good-row_id.
**        endif.
      endif.

      case ls_good-fieldname.
        when 'MATNR'.
          call method pr_data_changed->get_cell_value
             exporting
               i_row_id = ls_good-row_id
               i_fieldname = ls_good-fieldname
             importing
               e_value = lv_matnr.

          if lv_matnr is not initial.

            select single matnr from mara into lv_matnr
                          where matnr = lv_matnr.

            if sy-subrc ne 0.
              call method pr_data_changed->add_protocol_entry
                 exporting
                   i_msgid = 'M3' i_msgno = '305'  i_msgty = 'E'
                   i_msgv1 = lv_matnr
***                   i_msgv1 = text-m02
                   i_fieldname = ls_good-fieldname
                   i_row_id = ls_good-row_id.

              error_in_data = 'X'.
            else.

              call method pr_data_changed->modify_cell
                exporting i_row_id    = ls_good-row_id
                          i_fieldname = 'MATNR'
                          i_value     = lv_matnr.
            endif.
        endif.
      endcase.

    endloop.

    loop at lt_good into ls_good.

      lv_tabix = sy-tabix.

      read table gt_out into gs_out index ls_good-row_id.

      check sy-subrc eq 0 or
            gs_out-stat = icon_display.

      call method pr_data_changed->modify_style
        exporting i_row_id    = ls_good-row_id
                  i_fieldname = 'MATNR'
                  i_style     = lv_mode.

    endloop.

  endmethod.

*------------------------------------------------------

  method get_inserted_rows.
    inserted_rows = me->inserted_rows.
  endmethod.
*------------------------------------------------------

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.
*------------------------------------------------------
  method refresh_delta_tables.
    clear me->inserted_rows[].
    clear me->deleted_rows[].
  endmethod.
*------------------------------------------------------
  method set_table_is_initial.
    initial_table = 'X'.
  endmethod.
*------------------------------------------------------
  method set_table_is_not_initial.
    initial_table = space.
  endmethod.

*------------------------------------------------------
  method table_is_initial.
    if initial_table = 'X'.
      initial = 'X'.
    else.
      initial = space.
    endif.
  endmethod.

*------------------------------------------------------
  method refresh_grid.

  endmethod.


  method popup_to_confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        TEXT_QUESTION         = question
        TEXT_BUTTON_1         = 'Ja'(020)
        TEXT_BUTTON_2         = 'Nein'(021)
        DEFAULT_BUTTON        = '2'
        DISPLAY_CANCEL_BUTTON = cancel
      IMPORTING
        ANSWER                = answer
      EXCEPTIONS
        OTHERS                = 2.


  endmethod.

endclass.

**************************************************************



****************************************************************

***************************************************************
* Selection Screen
*-------------------------------------------
tables: mara, zle_pref_mat.

*selection-screen begin of screen 1010 as subscreen.
selection-screen begin of block b1 with frame title text-f01.
select-options: s_matnr for zle_pref_mat-matnr.
select-options: s_mtart for mara-mtart.
select-options: s_erdat for zle_pref_mat-erdat.
select-options: s_ernam for zle_pref_mat-ernam.
select-options: s_laeda for zle_pref_mat-laeda.
select-options: s_aenam for zle_pref_mat-aenam.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-f02.
selection-screen uline.
* ---------------------------------------------------------------------
* Manual maintenance
* ---------------------------------------------------------------------
parameters: p_manu type c radiobutton group rb01 default 'X'
            user-command cb_rb01. "Manuelle Pflege
selection-screen uline.
* ---------------------------------------------------------------------
* File upload
* ---------------------------------------------------------------------
parameters: p_upload type c radiobutton group rb01.  "Upload via File
*selection-screen skip 1.
parameters: p_cbmod type c radiobutton group rb02 default 'X'. "Aktualisieren ohne löschen
parameters: p_cbdel type c radiobutton group rb02. "Löschen und Neuaufbau
selection-screen skip 1.

parameters: p_onlerr as checkbox. " default 'X'.
parameters: p_titel as checkbox default 'X'.
parameters: p_fname like rlgrap-filename default 'C:\Temp\XXX.csv'.
selection-screen uline.
selection-screen end of block b2.

selection-screen  begin of block b20 with frame title text-f04.
selection-screen: comment 1(4) ICON_INF.
selection-screen: comment 5(75) text-c01.
***selection-screen: comment /5(75) text-p12.
*selection-screen: skip 1.
parameters: p_almart type string.
selection-screen  end of block b20.


*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
 INITIALIZATION.

   WRITE ICON_INFORMATION AS ICON TO icon_inf.
   PERFORM load_table_allowed_mtart CHANGING gv_subrc.
   PERFORM SET_PROPERTIES_SCREEN_1000.
   PERFORM show_allowed_mtart.

  AT SELECTION-SCREEN.

    save_ok = sy-ucomm.

    gv_ucomm = sy-ucomm.

    clear ok_code.

    clear: gt_out, gt_log.

    case save_ok.

      when 'CHANGE'.
        perform select_data changing gt_out[].
        if gt_out[] is initial.
          message e017(m5).
        else.
          call screen 200.
        endif.

      when 'UPLOAD'.
        perform processing_upload.

      when 'CREATE'.
        if p_manu eq gc_on.
          perform init_alv_table.
          call screen 200.
        endif.

      when 'SIMU_UPLO'.
        perform check_uploadfile changing gv_subrc.

        if gv_subrc ne 0.
          set cursor field 'P_FNAME'.
          message e151(0D) with p_fname.
        endif.

      when 'CB_RB01'.  "Manual maintenance
       if p_manu eq gc_on.
         set pf-status 'MAIN1000'.
         LOOP AT SCREEN.
           IF SCREEN-NAME = 'P_CBMOD' OR
             SCREEN-NAME = 'P_CBDEL'.
             SCREEN-INVISIBLE = '1'.
             SCREEN-INPUT = '0'.
             MODIFY SCREEN.
           ENDIF.
         ENDLOOP.
       else.
         set pf-status 'MAIN200' excluding 'SIMU_UPLO' IMMEDIATELY.
       endif.

*     Variants
      when 'SSET'.
        perform selection_sets.

      when 'BACK' or 'EXIT'.
       SET SCREEN 0.
       LEAVE SCREEN.

    endcase.

*----------------------------------------------------------------------*
*   at selection screen: Outputfile für download
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
*      def_path         = 'C:\TEMP\'
      mask             = gs_maske
    IMPORTING
      filename         = p_fname
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

AT SELECTION-SCREEN OUTPUT..

  PERFORM SET_PROPERTIES_SCREEN_1000.
  PERFORM show_allowed_mtart.

  clear gt_ucomm.
  gv_pfstatus = 'MAIN1000'.

* Exclusions of commands in GUI-status
 if p_manu eq gc_on. "manual maintenance
   append 'SIMU_UPLO' to gt_ucomm.
   append 'UPLOAD' to gt_ucomm.
 else. "File upload
   append 'CREATE' to gt_ucomm.
   append 'CHANGE' to gt_ucomm.
endif.

CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
  EXPORTING
    P_STATUS        = gv_pfstatus
*   P_PROGRAM       = ' '
  TABLES
    P_EXCLUDE       = gt_ucomm.

  IF p_manu EQ gc_on.
    LOOP AT SCREEN.
      IF SCREEN-NAME = 'P_CBMOD' OR
        SCREEN-NAME = 'P_CBDEL'.
        SCREEN-INVISIBLE = '1'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
module pbo output.
  set pf-status 'MAIN200'.
  set titlebar 'MAIN200'.
  if g_custom_container is initial.
    perform create_and_init_alv changing gt_out[]
                                         gt_fieldcat.

  endif.

endmodule.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
module pai input.
  save_ok = ok_code.
  clear ok_code.

  case save_ok.
    when 'SAVE'.
      perform save_data.
    when 'BACK'.
      perform test_any_changes_alv01 changing gv_answer.
      case gv_answer.
        when gc_yes.
          perform save_data.
          perform destroy_instances.
            SET SCREEN 0.
            LEAVE SCREEN.
        when gc_no.  "ohne sichern
          perform destroy_instances.
          SET SCREEN 0.
          LEAVE SCREEN.
        when gc_cancel.
    endcase.
* §10.Unlock your database table.
***      perform unlock_sflight.
***      leave to screen 0.
    when 'EXIT'.
      perform destroy_instances.
      perform exit_program.
    when others.
*     do nothing
  endcase.
endmodule.

*&---------------------------------------------------------------------*
*&      Form  SET_PROPERTIES_SCREEN_1000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PROPERTIES_SCREEN_1000.

  LOOP AT SCREEN.
* Felder nur ausgabefähig machen
    if screen-name = 'P_ALMART'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_PROPERTIES_SCREEN_1000

*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
form exit_program.
  leave program.
endform.

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
form build_fieldcat changing pt_fieldcat type lvc_t_fcat.

  data ls_fcat type lvc_s_fcat.
  data: lv_edit type c.
  data: lv_edit_keyfield type c.

  clear: lv_edit, lv_edit_keyfield.

  case gv_ucomm.
    when 'CREATE'.
      lv_edit = 'X'.
      lv_edit_keyfield = 'X'.
    when 'CHANGE'.
      lv_edit = 'X'.
  endcase..

  clear pt_fieldcat.

  call function 'LVC_FIELDCATALOG_MERGE'
       exporting
            i_structure_name = 'ZSPREFMAT'
       changing
            ct_fieldcat      = pt_fieldcat.

  loop at pt_fieldcat into ls_fcat.

    case ls_fcat-fieldname.
      when 'PREF_EU' or
           'PREF_TR' or
           'PREF_IL' or
           'PREF_AE'.
        ls_fcat-checkbox = 'X'.
        ls_fcat-outputlen = 7.
        ls_fcat-edit = lv_edit.

      when 'MATNR'.
        ls_fcat-edit = lv_edit_keyfield.

      when 'XEGLD' or
           'LAND'.
        ls_fcat-edit = lv_edit.

      when 'STAT'.
        ls_fcat-outputlen = 5.
    endcase.

*   Do not check foreign keys
    ls_fcat-checktable = '!'.

*   Use field AUTO_VALUE of the fieldcatalog to preset values when new
*   lines are added.
    ls_fcat-auto_value = 'X'.
*   do not check foreign key relations
    ls_fcat-checktable = '!'.

*   Modify element in table
    modify pt_fieldcat from ls_fcat.

  endloop.

endform.

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*      <--P_GT_FIELDCAT  text
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
form create_and_init_alv changing pt_out like gt_out[]
                                  pt_fieldcat type lvc_t_fcat.

  data: lt_exclude type ui_functions.
  data: ls_fieldcat type lvc_s_fcat.

  create object g_custom_container
         exporting container_name = g_container.
  create object g_grid
         exporting i_parent = g_custom_container.

* Create Objekt to verify input values.
* (This object is already needed in form SELECT_DATA).
  create object g_verifier.
  set handler g_verifier->handle_data_changed for g_grid.
  set handler g_verifier->handle_user_command for g_grid.
  set handler g_verifier->handle_toolbar for g_grid.

* Build fieldcat
  perform build_fieldcat changing pt_fieldcat.
  perform exclude_tb_functions changing lt_exclude.

  gs_layout-stylefname = 'CELLTAB'.
*  gs_layout-cwidth_opt = 'X'.
  gs_layout-col_opt = 'X'.

  call method g_grid->set_table_for_first_display
    exporting it_toolbar_excluding  = lt_exclude
              is_layout             = gs_layout
    changing  it_fieldcatalog       = pt_fieldcat
              it_outtab             = pt_out[].

* Set editable cells to ready for input initially
  call method g_grid->set_ready_for_input
   exporting
    i_ready_for_input = 1.

*  Registering the EDIT Event
  CALL METHOD g_grid->register_edit_event
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.

endform.                               "CREATE_AND_INIT_ALV

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*      <--P_GT_FIELDCAT  text
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
form create_and_init_alv_old changing pt_out like gt_out[]
                                  pt_fieldcat type lvc_t_fcat.

  data: lt_exclude type ui_functions.

  create object g_custom_container
         exporting container_name = g_container.
  create object g_grid
         exporting i_parent = g_custom_container.

* Create Objekt to verify input values.
* (This object is already needed in form SELECT_DATA).
  create object g_verifier.
  set handler g_verifier->handle_data_changed for g_grid.
  set handler g_verifier->handle_user_command for g_grid.
  set handler g_verifier->handle_toolbar for g_grid.

  perform select_data changing pt_out[].

* Build fieldcat and set columns PLANETYPE and SEATSOCC
* edit enabled.
  perform build_fieldcat changing pt_fieldcat.
  perform exclude_tb_functions changing lt_exclude.

  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-cwidth_opt = 'X'.

  call method g_grid->set_table_for_first_display
       exporting it_toolbar_excluding  = lt_exclude
                 is_layout             = gs_layout
       changing  it_fieldcatalog       = pt_fieldcat
                 it_outtab             = pt_out[].

* Set editable cells to ready for input initially
  call method g_grid->set_ready_for_input
   exporting
    i_ready_for_input = 1.

*  Registering the EDIT Event
  CALL METHOD g_grid->register_edit_event
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED
    EXCEPTIONS
      ERROR      = 1
      OTHERS     = 2.

endform.                               "CREATE_AND_INIT_ALV

*&---------------------------------------------------------------------*
*&      Form  init_alv_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form init_alv_table.

  data: ls_out like line of gt_out.
  data: lv_index type sy-tabix.
  data: ls_celltab type lvc_s_styl,
        lt_celltab type lvc_t_styl        ,
        l_mode type raw4.

  clear: ls_out, lv_index.

  refresh lt_celltab.

  l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ls_celltab-fieldname = 'MATNR'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table lt_celltab.

* Create 40 empty lines
  do 40 times.
    lv_index = lv_index + 1.

    clear ls_out.
    ls_out-stat = icon_create.
    ls_out-stat_init = icon_create.

*   Copy your celltab to the celltab of the current row of gt_out
    insert lines of lt_celltab into table ls_out-celltab.
    append ls_out to gt_out.

  enddo.

* Select all the allowed material types for the user
*>>>  PERFORM load_table_allowed_mtart CHANGING gv_subrc.

endform.                               "INIT_ALV_TABLE


*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*----------------------------------------------------------------------*
form select_data changing pt_out like gt_out[].

  data: lt_pref_mat type table of zle_pref_mat,
        ls_pref_mat type zle_pref_mat,
        ls_out like line of gt_out,
        l_index type i,
        lt_celltab type lvc_t_styl.

  clear: pt_out, gt_out.

* Select all the allowed material types for the user
*>>>  PERFORM load_table_allowed_mtart CHANGING gv_subrc.

* Select data from zle_pref_mat
  select * from zle_pref_mat into ls_pref_mat
    where matnr in s_matnr and
          erdat in s_erdat and
          ernam in s_ernam and
          laeda in s_laeda and
          aenam in s_aenam.

*   check if material type may be selecte
    perform check_if_mtart_is_allowed using ls_pref_mat-matnr
                                   changing gv_subrc
                                            gv_mtart.

    check gv_subrc eq 0.  "material type allowed
    check gv_mtart in s_mtart.

    append ls_pref_mat to lt_pref_mat.
  endselect.

* move corresponding fields from lt_pref_mat to gt_out
  sort lt_pref_mat by matnr ascending.

  clear ls_out.

  loop at lt_pref_mat into ls_pref_mat.

    ls_out-matnr = ls_pref_mat-matnr.

    case ls_pref_mat-land.
      when 'AE'.
        ls_out-pref_ae = ls_pref_mat-pref.
      when 'TR'.
        ls_out-pref_tr = ls_pref_mat-pref.
      when 'IL'.
        ls_out-pref_il = ls_pref_mat-pref.
      when others.
        if ls_pref_mat-xegld = 'X'.
          ls_out-pref_eu = ls_pref_mat-pref.
        endif.
    endcase.

    ls_out-erdat = ls_pref_mat-erdat.
    ls_out-erzet = ls_pref_mat-erzet.
    ls_out-ernam = ls_pref_mat-ernam.
    ls_out-laeda = ls_pref_mat-laeda.
    ls_out-laeze = ls_pref_mat-laeze.
    ls_out-aenam = ls_pref_mat-aenam.
    ls_out-stat = icon_display.
    ls_out-stat_init = icon_display.
***      ls_out-updkz = gc_display.
***      ls_out-updkz_init = gc_display.

    at end of matnr.

      select single mtart from mara into ls_out-mtart
        where matnr = ls_pref_mat-matnr.

      select single maktx from makt into ls_out-maktx
        where matnr = ls_pref_mat-matnr and
              spras = sy-langu.

      append ls_out to pt_out.
      clear ls_out.

    endat.

  endloop.

  gt_out_save[] = gt_out[].

******
exit.
******
* Set all cells of the table non-editable by using the style table.
  loop at pt_out into ls_out.

    l_index = sy-tabix.
    refresh lt_celltab.
    perform fill_celltab using 'RO'
                      changing lt_celltab.
*   Copy your celltab to the celltab of the current row of gt_outtab.
    insert lines of lt_celltab into table ls_out-celltab.
    modify pt_out from ls_out index l_index.

  endloop.

endform.                               " select_data

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*----------------------------------------------------------------------*
form select_data_old changing pt_out like gt_out[].

  data: lt_pref_mat type table of zle_pref_mat,
        ls_pref_mat type zle_pref_mat,
        ls_out like line of gt_out,
        l_index type i,
        lt_celltab type lvc_t_styl.

  clear: pt_out, gt_out.

  if gv_ucomm = 'CREATE'.
    clear ls_out.
    do 10 times.
      ls_out-stat = icon_create.
      ls_out-stat_init = icon_create.
***      ls_out-updkz = icon_create.
***      ls_out-updkz_init = icon_create.
      append ls_out to gt_out.
    enddo.
    exit.
  endif.

* Select data from zle_pref_mat
  select * from zle_pref_mat into table lt_pref_mat
    where matnr in s_matnr and
          erdat in s_erdat and
          ernam in s_ernam.

  if sy-subrc ne 0.
* no data were found!
* We provide some default values for the first line that is entered:
    clear ls_out.
* set fields FLDATE, PRICE and PLANETYPE to editable
    perform fill_celltab using 'RW'
                         changing lt_celltab.
    insert lines of lt_celltab into table ls_out-celltab.
    append ls_out to pt_out.

* Tell Verify-Objekt that the table was initial
    call method g_verifier->set_table_is_initial.
  else.
    call method g_verifier->set_table_is_not_initial.
* move corresponding fields from lt_pref_mat to gt_out
    sort lt_pref_mat by matnr ascending.

    clear ls_out.

    loop at lt_pref_mat into ls_pref_mat.

      ls_out-matnr = ls_pref_mat-matnr.

      case ls_pref_mat-land.
        when 'AE'.
          ls_out-pref_ae = ls_pref_mat-pref.
        when 'TR'.
          ls_out-pref_tr = ls_pref_mat-pref.
        when 'IL'.
          ls_out-pref_il = ls_pref_mat-pref.
        when others.
          if ls_pref_mat-xegld = 'X'.
            ls_out-pref_eu = ls_pref_mat-pref.
          endif.
      endcase.

      ls_out-erdat = ls_pref_mat-erdat.
      ls_out-erzet = ls_pref_mat-erzet.
      ls_out-ernam = ls_pref_mat-ernam.
      ls_out-laeda = ls_pref_mat-laeda.
      ls_out-laeze = ls_pref_mat-laeze.
      ls_out-aenam = ls_pref_mat-aenam.
      ls_out-stat = icon_display.
      ls_out-stat_init = icon_display.
***      ls_out-updkz = gc_display.
***      ls_out-updkz_init = gc_display.

      at end of matnr.

        select single mtart from mara into ls_out-mtart
          where matnr = ls_pref_mat-matnr.

        select single maktx from makt into ls_out-maktx
          where matnr = ls_pref_mat-matnr and
                spras = sy-langu.


        append ls_out to pt_out.
        clear ls_out.
      endat.

    endloop.

* §3.Set all cells of the table non-editable by using the style table.

    loop at pt_out into ls_out.
      l_index = sy-tabix.
      refresh lt_celltab.
      perform fill_celltab using 'RO'
                        changing lt_celltab.
* Copy your celltab to the celltab of the current row of gt_outtab.
      insert lines of lt_celltab into table ls_out-celltab.
      modify pt_out from ls_out index l_index.
    endloop.
  endif.

endform.                               " select_data


*&---------------------------------------------------------------------*
*&      Form  fill_celltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_OUTTAB  text
*----------------------------------------------------------------------*
form fill_celltab using value(p_mode)
                  changing pt_celltab type lvc_t_styl.

  data: ls_celltab type lvc_s_styl,
        l_mode type raw4.

* This forms sets the style of columns 'ERDAT', ERZET, ERNAM,
* LAEDA, LAEZE and AENAM editable

  clear pt_celltab.

****
exit.
****
  if p_mode eq 'RW'.
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  else.                                "p_mode eq 'RO'
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  endif.

  ls_celltab-fieldname = 'ERDAT'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'ERZET'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'ERNAM'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'LAEDA'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'LAEZE'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'AENAM'.
  ls_celltab-style = l_mode.
  insert ls_celltab into table pt_celltab.

endform.                               " FILL_CELLTAB

*&---------------------------------------------------------------------*
*&      Form exclude_tb_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form exclude_tb_functions changing pt_exclude type ui_functions.
*
  data ls_exclude type ui_func.

    ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_check.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_mb_sum.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_info.
    APPEND ls_exclude TO pt_exclude.
    ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND ls_exclude TO pt_exclude.

*****
exit.
*****

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append ls_exclude to pt_exclude.

endform.                               " EXCLUDE_TB_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form save_data.
  data: l_valid type c.
* §7.Check if any errors exist in protocol by using method
*    CHECK_CHANGED_DATA of your ALV Grid instance.

* The method CHECK_CHANGED_DATA checks all new cells syntactically,
* raises event DATA_CHANGED and looks then for any entries
* in the error protocol. If any exist the parameter e_valid
* is initial ('X' in the other case).
*
  call method g_grid->check_changed_data
               importing e_valid = l_valid.

  if l_valid is initial.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = text-i06
              txt1  = text-i07
              txt2  = text-i08
              txt3  = text-i09.

  else.
    perform update_database changing gv_any_changes.
    if gv_any_changes is initial.
      message s015(mz).  "no changes
    else.
      message s000(0k) with text-s01.
    endif.
  endif.

endform.                               " SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form update_database
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
form update_database changing pv_any_changes type c.

  data: lt_del_rows type standard table of zle_pref_mat,
        lt_ins_keys type g_verifier->zle_pref_mat_keys,
        l_ins_key type g_verifier->zle_pref_mat_key,
        ls_sflight type sflight,
        ls_outtab like line of gt_out,
        lt_instab type table of zle_pref_mat,
        lv_xegld type xegld,
        lv_land type land1,
        lv_msgtext type symsgv,
        lv_stat_ret type char4,
        lv_tabix type sy-tabix.

  data: ls_pref_mat type zle_pref_mat.

  types: begin of t_stat_ret,
           stat type char4,
         end of t_stat_ret.

  data: lt_stat_ret type standard table of t_stat_ret.
  data: ls_stat_ret type t_stat_ret.

  constants: lc_land_ae TYPE land1 value 'AE'.
  constants: lc_land_il TYPE land1 value 'IL'.
  constants: lc_land_tr TYPE land1 value 'TR'.
  constants: lc_land_eu TYPE land1 value 'EU'.

  clear: pv_any_changes, gt_log.

* ---------------------------------------------------------------------
* Delete all entries with empty material number
* ---------------------------------------------------------------------
  delete gt_out where matnr is initial.

* ---------------------------------------------------------------------
* Check if any changes
* ---------------------------------------------------------------------
  clear pv_any_changes.
  loop at gt_out into gs_out
    where stat ne icon_display and
          stat ne icon_system_save and
          stat ne icon_defect.
    pv_any_changes = gc_on.
    exit.
  endloop.

  check pv_any_changes = gc_on.
* ---------------------------------------------------------------------
* Delete
* ---------------------------------------------------------------------
  loop at gt_out into gs_out
    where stat = icon_delete.

    lv_tabix = sy-tabix.

    delete from zle_pref_mat
    where matnr = gs_out-matnr.

    if sy-subrc eq 0.
      delete gt_out index lv_tabix.
    else.
      gs_out-stat = icon_defect.
      modify gt_out from gs_out index lv_tabix.

      clear gs_log.
      gs_log-stat = icon_red_light.
      gs_log-matnr = gs_out-matnr.
      gs_log-descr = text-p01.
      append gs_log to gt_log.
    endif.

  endloop.

* ---------------------------------------------------------------------
* Insert, Change
* ---------------------------------------------------------------------
  loop at gt_out into gs_out
    where stat = icon_create or
          stat = icon_change.

    lv_tabix = sy-tabix.

    clear lt_stat_ret.

*   Init
    clear ls_pref_mat.

    ls_pref_mat-matnr = gs_out-matnr.

    if gs_out-stat = icon_create.
      ls_pref_mat-erdat = sy-datum.
      ls_pref_mat-erzet = sy-uzeit.
      ls_pref_mat-ernam = sy-uname.
    else.
      ls_pref_mat-erdat = gs_out-erdat.
      ls_pref_mat-erzet = gs_out-erzet.
      ls_pref_mat-ernam = gs_out-ernam.

      ls_pref_mat-laeda = sy-datum.
      ls_pref_mat-laeze = sy-uzeit.
      ls_pref_mat-aenam = sy-uname.
    endif.

*   EU
    ls_pref_mat-xegld = 'X'.
    ls_pref_mat-pref = gs_out-pref_eu.
    perform save_record_in_db using gs_out-stat
                                    ls_pref_mat
                           changing lv_stat_ret.

    append lv_stat_ret to lt_stat_ret.

*   AE
    clear ls_pref_mat-xegld.
    ls_pref_mat-land = lc_land_ae.
    ls_pref_mat-pref = gs_out-pref_ae.
    perform save_record_in_db using gs_out-stat
                                    ls_pref_mat
                           changing lv_stat_ret.

    append lv_stat_ret to lt_stat_ret.

*   IL
    clear ls_pref_mat-xegld.
    ls_pref_mat-land = lc_land_il.
    ls_pref_mat-pref = gs_out-pref_il.
    perform save_record_in_db using gs_out-stat
                                    ls_pref_mat
                           changing lv_stat_ret.

    append lv_stat_ret to lt_stat_ret.

*   TR
    clear ls_pref_mat-xegld.
    ls_pref_mat-land = lc_land_tr.
    ls_pref_mat-pref = gs_out-pref_tr.
    perform save_record_in_db using gs_out-stat
                                    ls_pref_mat
                           changing lv_stat_ret.

    append lv_stat_ret to lt_stat_ret.

    lv_stat_ret = icon_system_save.
    loop at lt_stat_ret into ls_stat_ret
      where stat = icon_defect.
        lv_stat_ret = icon_defect.
        exit.
    endloop.

    gs_out-stat = lv_stat_ret.
    modify gt_out from gs_out index lv_tabix.

  endloop.

   gs_stable-row  = 'X'.
   gs_stable-col  = 'X'.
   gv_soft_refresh = space.

   call method g_grid->refresh_table_display
     EXPORTING
       is_stable      = gs_stable
       i_soft_refresh = space.

* Log
  PERFORM log.

endform.
*--------------------------------------------------------
form lock_prefmat_table changing p_success type c.

***  p_success = space.
***
***  call function 'ENQUEUE_ESFLIGHT'
***       exporting
***            carrid         = g_carrid
***            connid         = g_connid
***            _scope         = '3'
***       exceptions
***            foreign_lock   = 1
***            system_failure = 2
***            others         = 3.
***
***  if sy-subrc ne 0.
***    p_success = space.
***  else.
***    p_success = 'X'.
***  endif.

endform.

*--------------------------------------------------------
*
*--------------------------------------------------------
form unlock_sflight.

***  call function 'DEQUEUE_ESFLIGHT'
***       exporting
***            carrid = g_carrid
***            connid = g_connid
***            _scope = '3'.

endform.

*&---------------------------------------------------------------------*
*&      Form  SAVE_RECORD_IN_DB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_RECORD_IN_DB USING pv_stat_in TYPE char4
                             ps_pref_mat TYPE zle_pref_mat
                    CHANGING pv_stat_out TYPE char4.

  data: lv_text type string.
  data: lv_icon type char4.
  data: lv_subrc type sy-subrc.

  clear: lv_text, pv_stat_out.

  case pv_stat_in.
    when icon_change.
      update zle_pref_mat from ps_pref_mat.
      lv_text = text-p09.
      lv_subrc = sy-subrc.
      if sy-subrc eq 0.
        lv_icon = icon_system_save.
      else.
        lv_icon = icon_defect.
      endif.
    when icon_create.
      lv_text = text-p02.
      insert zle_pref_mat from ps_pref_mat.
      lv_subrc = sy-subrc.
      if sy-subrc eq 0.
        lv_icon = icon_system_save.
      else.
        lv_icon = icon_defect.
      endif.
    when others.
      lv_text = text-p10.
      lv_subrc = 4.
      lv_icon = icon_defect.
  endcase.

  if lv_subrc ne 0.  "update not sucessful
    clear gs_log.
    gs_log-stat = icon_red_light.
    write ps_pref_mat-matnr to gs_log-matnr no-zero.

    if ps_pref_mat-xegld = gc_on.
      concatenate lv_text '(' ps_pref_mat-xegld ')'
      into gs_log-descr in character mode separated by space.
    else.
      concatenate lv_text '(' ps_pref_mat-land ')'
      into gs_log-descr in character mode separated by space.
    endif.

***    gs_log-msgv2 = lv_text. "ps_pref_mat-land.

    append gs_log to gt_log.
  endif.

  pv_stat_out = lv_icon.

ENDFORM.                    " SAVE_RECORD_IN_DB


*&---------------------------------------------------------------------*
*&      Form  TEST_ANY_CHANGES_ALV01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_NEXT_SCREEN  text
*----------------------------------------------------------------------*
FORM TEST_ANY_CHANGES_ALV01  CHANGING P_ANSWER type c.

  data: lv_answer type c length 1.
  data: lv_valid type c.
  data: lv_refresh type c.
  data: lv_data_changed type c.


  p_answer = gc_no.
  lv_data_changed = gc_no.

  loop at gt_out into gs_out
    where not matnr is initial and
          ( stat eq icon_change or
            stat eq icon_create or
            stat eq icon_delete ).
    lv_data_changed = gc_yes.
    exit.
  endloop.

* Bestätigung vor Löschen fordern
  if lv_data_changed = gc_yes.
    perform popup_to_confirm using 'Achtung, Daten gehen verloren, sichern?'(040)
                                   gc_cancel_button  "Cancel-Button auch anzeigen
                          changing p_answer.
  endif.

ENDFORM.                    " TEST_ANY_CHANGES_ALV01


*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT     text
*      -->P_CANCEL   text
*      -->P_ANSWER   text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING    p_question TYPE CLIKE
                               p_cancel TYPE CLIKE
                      CHANGING p_answer LIKE gv_answer.

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
*&      Form  LOG_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM log.

  DATA: l_s_log TYPE bal_s_log.
  DATA: ls_display_profile TYPE bal_s_prof.

  CHECK gt_log[] IS NOT INITIAL.

  data: ls_layout_lvc type LVC_S_LAYO.

  clear ls_layout_lvc.

* Optimale Spaltenbreite setzen
  move gc_on to ls_layout_lvc-cwidth_opt.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_structure_name = 'ZSPREFMAT_LOG'
      IS_LAYOUT_LVC    = ls_layout_lvc
    TABLES
      t_outtab         = gt_log
    EXCEPTIONS
      OTHERS           = 2.

ENDFORM.                    " LOG_CREATE

*&---------------------------------------------------------------------*
*&      Form  DESTROY_INSTANCES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESTROY_INSTANCES .

  if not g_custom_container is initial.
    CALL METHOD g_custom_container->free.
    free g_custom_container.
    free g_grid.
  endif.

  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " DESTROY_INSTANCES


*&---------------------------------------------------------------------*
*&      Form  PROCESSING_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSING_UPLOAD .

  DATA: lv_exists TYPE abap_bool.
  DATA: lv_file TYPE string.

*----------------------------------------------------------------------*
* Check if file exists
*----------------------------------------------------------------------*
  lv_file = p_fname.

  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_file
    RECEIVING
      result               = lv_exists
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

    IF sy-subrc <> 0.
      MESSAGE e150(0D) WITH p_fname.
    ELSE.
      IF lv_exists = abap_false.
        SET CURSOR FIELD 'P_FNAME'.
        MESSAGE e302(fg).
      ENDIF.
    ENDIF.

*----------------------------------------------------------------------*
* Vollständiges Löschen und Neuload gewählt?
*----------------------------------------------------------------------*
  if p_cbdel eq gc_on.  "Vollständiges löschen
    perform delete_all changing gv_cancel_process.
*   Verarbeitung mittels Popup abgebrochen?
    if gv_cancel_process eq gc_on.
      message s025(ab). "Meldung 'Verarbeitung abgebrochen'
     exit.
    endif.
  else.
*   Bei Update: Weiterverarbeitung bestätigen
    perform popup_to_confirm using text-pc3
                                   ' '
                          changing gv_answer.

    if gv_answer ne '1'. "Nein-Fall
      message s025(ab). "Meldung 'Verarbeitung abgebrochen'
      exit.
    endif.
  endif.  "p_cbdel.

*----------------------------------------------------------------------*
* * Select all the allowed material types for the user
*----------------------------------------------------------------------*
*>>>  PERFORM load_table_allowed_mtart CHANGING gv_subrc.

*----------------------------------------------------------------------*
* Upload Inputfile
*----------------------------------------------------------------------*
  perform ws_upload changing gv_subrc.

*  Fehler beim Upload
  IF gv_subrc ne 0.
****    MESSAGE S398 WITH 'Error on open' P_FNAME.
****    LEAVE LIST-PROCESSING.
  ENDIF.

  if p_titel eq gc_on.
    gv_loop_start = 2.
  else.
    gv_loop_start = 1.
  endif.

* Eingabe verarbeiten
  loop at gt_in into gs_in.

    gv_anz_read = gv_anz_read + 1.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT              = gs_in-matnr
     IMPORTING
        OUTPUT             = gs_in-matnr
     EXCEPTIONS
       LENGTH_ERROR       = 1
       OTHERS             = 2.

    IF SY-SUBRC <> 0.
      clear gs_log.
      gs_log-stat = icon_red_light.
      gs_log-matnr = gs_in-matnr.
      gs_log-descr = text-p08.
      append gs_log to gt_log.
    ENDIF.

*   Nur Selektierte Materialien verarbeiten
    check gs_in-matnr in s_matnr.

*   Input überprüfen
    perform check_input changing gv_subrc.
    check gv_subrc eq 0.

*   Verarbeitung
    perform verarbeitung_preference.

  endloop.  "gt_in

  perform log.

ENDFORM.                    " PROCESSING_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  DELETE_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_CANCEL_PROCESS  text
*----------------------------------------------------------------------*
FORM DELETE_ALL  CHANGING PV_CANCEL_PROCESS type c.

  data: lv_text_popup type c length 100.
  data: ls_pref_nodel type zle_pref_nodel.
  data: lt_pref_nodel type standard table of zle_pref_nodel.
  data: ls_pref_mat type zle_pref_mat.

  data: begin of ls_mara,
          matnr type matnr,
          mtart type mtart,
        end of ls_mara.

  clear pv_cancel_process.

* Bestätigung vor Löschen fordern: Im Hintergrund nicht möglich
  if sy-batch eq gc_on.  "läuft im Hintergrund
    gv_answer = '1'.
  else.
    perform popup_to_confirm using text-pc1
                                   ' '
                          changing gv_answer.
  endif.

  if gv_answer ne '1'. "Nein-Fall
    pv_cancel_process = gc_on. "Verarbeitung abbrechen
    exit.
  endif.

* ---------------------------------------------------------------------
* Alles aus der Tabelle zle_pref_mat löschen, ausser diejenigen
* Materialarten, welche in der Tabelle ZLE_PREF_NODEL vorhanden sind
* ---------------------------------------------------------------------

* Alle Materialarten in interne Tabelle laden, welche nicht gelöscht
* werden dürfen
  clear: lt_pref_nodel, ls_mara.

*
  select * from zle_pref_mat into ls_pref_mat.

    if ls_pref_mat-matnr ne ls_mara-matnr.
      select single matnr mtart from mara into ls_mara
        where matnr = ls_pref_mat-matnr.
    endif.

    loop at gt_allowed_mtart into gs_allowed_mtart
      where mtart = ls_mara-mtart.
      exit.
    endloop.

    if sy-subrc eq 0. "Materialart darf gelöscht werden
      delete zle_pref_mat from ls_pref_mat.
      gv_anz_del = gv_anz_del + 1.  "Anzahl gelöschte Records
    endif.

  endselect.

*>>>  commit work and wait.

* Anzahl nicht gelöschte Sätze
  clear gv_anz_no_del.

  select count( * ) from zle_pref_mat.
  gv_anz_no_del = sy-dbcnt.

ENDFORM.                    " DELETE_ALL

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_INPUT  CHANGING PV_SUBRC TYPE sysubrc.

  data: lv_matnr type matnr.
  data: lv_maktx type maktx.
  data: lv_mtart type mtart.

  pv_subrc = 0.

* Alles in Grossbuchstaben umwandeln, damit Gross- und Kleinschreibung
* zugelassen werden kann
  translate gs_in-pref_eu to upper case.
  translate gs_in-pref_il to upper case.
  translate gs_in-pref_ae to upper case.

  if gs_in-pref_eu ne gc_input_ja and
    gs_in-pref_eu ne gc_input_nein.
    pv_subrc = 4.
    clear gs_log.
    write gs_in-matnr to lv_matnr no-zero.
    gs_log-stat = icon_red_light.
    gs_log-matnr = lv_matnr.
    gs_log-descr = text-p05.
    append gs_log to gt_log.
  endif.

  if gs_in-pref_il ne gc_input_ja and
    gs_in-pref_il ne gc_input_nein.
    pv_subrc = 4.
    clear gs_log.
    write gs_in-matnr to lv_matnr no-zero.
    gs_log-stat = icon_red_light.
    gs_log-matnr = lv_matnr.
    gs_log-descr = text-p06.
    append gs_log to gt_log.
  endif.

  if gs_in-pref_ae ne gc_input_ja and
    gs_in-pref_ae ne gc_input_nein.
    clear gs_log.
    write gs_in-matnr to lv_matnr no-zero.
    gs_log-stat = icon_red_light.
    gs_log-matnr = lv_matnr.
    gs_log-descr = text-p07.
    append gs_log to gt_log.
  endif.

ENDFORM.                    " CHECK_INPUT

*&---------------------------------------------------------------------*
*&      Form  VERARBEITUNG_PREFERENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VERARBEITUNG_PREFERENCE .

  data: ls_pref_mat type zle_pref_mat.
  data: lv_datab type datab.
  data: lv_text_kzupd type string.
  data: lv_fehltext type string.
  data: lv_len type i.
  data: lv_mat_exists type c.
*  data: lv_matnr type matnr.
  data: lv_pref type c.
  data: lv_anz_upd type i.
  data: lv_anz_ins type i.

*   Test ob Material bereits vorhanden ist.
  clear: ls_pref_mat, lv_mat_exists, lv_text_kzupd.

***  if not gs_in-matnr is initial.
***
***    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
***      EXPORTING
***        INPUT              = gs_in-matnr
***      IMPORTING
***        OUTPUT             = lv_matnr
***     EXCEPTIONS
***       LENGTH_ERROR       = 1
***       OTHERS             = 2.
***
***    IF SY-SUBRC <> 0.
***      clear gs_msg.
***      gs_msg-msgty = 'E'.
***      gs_msg-msgid = 'AQ'.
***      gs_msg-msgno = '003'.
***      gs_msg-msgv1 = gs_in-matnr.
***      gs_msg-msgv2 = text-p08.
***      gs_msg-msgv3 = sy-subrc.
***      append gs_msg to gt_msg.
***    ENDIF.
***
***  endif.
***    if gs_in-matnr co '01234567890 '.
***      unpack gs_in-matnr to lv_matnr.
***    else.
***      lv_matnr = gs_in-matnr.
***    endif.
***  endif.

* ---------------------------------------------------------------------
* Präferenzberechtigt für EU
* ---------------------------------------------------------------------
  clear lv_pref.

  if gs_in-pref_eu eq gc_input_ja.
    lv_pref = gc_on.
  endif.

  select single * from zle_pref_mat into ls_pref_mat
    where matnr = gs_in-matnr and
          xegld = gc_on.

  if sy-subrc eq 0.
    if ls_pref_mat-pref ne lv_pref.  "Präferenzflag hat geändert
      ls_pref_mat-pref = lv_pref.
      ls_pref_mat-laeda = sy-datum.
      ls_pref_mat-laeze = sy-uzeit.
      ls_pref_mat-aenam = sy-uname.
      update zle_pref_mat from ls_pref_mat.
      gv_anz_pref_eu_upd = gv_anz_pref_eu_upd + 1.
    endif.
  else.
    clear ls_pref_mat.
    ls_pref_mat-matnr = gs_in-matnr.
    ls_pref_mat-xegld = gc_on.
    ls_pref_mat-pref = lv_pref.
    ls_pref_mat-erdat = sy-datum.
    ls_pref_mat-erzet = sy-uzeit.
    ls_pref_mat-ernam = sy-uname.
    insert zle_pref_mat from ls_pref_mat.
    gv_anz_pref_eu_ins = gv_anz_pref_eu_ins + 1.
  endif.


* ---------------------------------------------------------------------
* Präferenzberechtigt für Türkei: gleiche Regel wie EU
* ---------------------------------------------------------------------
  clear lv_pref.

  if gs_in-pref_eu eq gc_input_ja.
    lv_pref = gc_on.
  endif.

  perform aufbereitung_via_land using gs_in-matnr
                                      'TR'
                                      lv_pref
                            changing  lv_anz_upd
                                      lv_anz_ins.

  gv_anz_pref_tr_upd = gv_anz_pref_tr_upd + lv_anz_upd.
  gv_anz_pref_tr_ins = gv_anz_pref_tr_ins + lv_anz_ins.

* ---------------------------------------------------------------------
* Präferenzberechtigt für Israel
* ---------------------------------------------------------------------
  clear lv_pref.

  if gs_in-pref_il eq gc_input_ja.
    lv_pref = gc_on.
  endif.

  perform aufbereitung_via_land using gs_in-matnr
                                      'IL'
                                      lv_pref
                            changing  lv_anz_upd
                                      lv_anz_ins.

  gv_anz_pref_il_upd = gv_anz_pref_il_upd + lv_anz_upd.
  gv_anz_pref_il_ins = gv_anz_pref_il_ins + lv_anz_ins.

* ---------------------------------------------------------------------
* Präferenzberechtigt für Dubai
* ---------------------------------------------------------------------
  clear lv_pref.

  if gs_in-pref_ae eq gc_input_ja.
    lv_pref = gc_on.
  endif.

  perform aufbereitung_via_land using gs_in-matnr
                                      'AE'
                                      lv_pref
                            changing  lv_anz_upd
                                      lv_anz_ins.

  gv_anz_pref_ae_upd = gv_anz_pref_ae_upd + lv_anz_upd.
  gv_anz_pref_ae_ins = gv_anz_pref_ae_ins + lv_anz_ins.


* Simulation
*  if p_simul eq gc_on.
*    rollback work.
*  endif.

  clear lv_fehltext.

  select single matnr from mara into gs_in-matnr
    where matnr = gs_in-matnr.

  if sy-subrc ne 0.
    clear gs_log.
    gs_log-stat = icon_red_light.
    gs_log-matnr = gs_in-matnr.
    gs_log-descr = text-p04.
    append gs_log to gt_log.
    gv_anz_missing_mat = gv_anz_missing_mat + 1.
  endif.

*  perform ausgabe_bildschirm using ls_pref_mat
*                                   lv_text_kzupd
*                                   lv_fehltext.

ENDFORM.                    " VERARBEITUNG_PREFERENCE


*&---------------------------------------------------------------------*
*&      Form  AUFBEREITUNG_VIA_LAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM AUFBEREITUNG_VIA_LAND  USING    PV_MATNR TYPE MATNR
                                     PV_LAND TYPE LAND1
                                     PV_PREF TYPE P_99S_PFLAG
                            CHANGING PV_ANZ_UPD TYPE I
                                     PV_ANZ_INS TYPE I.

  data: lv_anz_upd type i.
  data: lv_anz_ins type i.
  data: ls_pref_mat type zle_pref_mat.

  clear: pv_anz_upd, pv_anz_ins, ls_pref_mat.

  select single * from zle_pref_mat into ls_pref_mat
    where matnr = pv_matnr and
          land = pv_land.

  if sy-subrc eq 0.
    if ls_pref_mat-pref ne pv_pref.  "Präferenzflag hat geändert
      ls_pref_mat-pref = pv_pref.
      ls_pref_mat-laeda = sy-datum.
      ls_pref_mat-laeze = sy-uzeit.
      ls_pref_mat-aenam = sy-uname.
      update zle_pref_mat from ls_pref_mat.
      pv_anz_upd = 1.
    endif.
  else.
    ls_pref_mat-matnr = pv_matnr.
    ls_pref_mat-land = pv_land.
    ls_pref_mat-pref = pv_pref.
    ls_pref_mat-erdat = sy-datum.
    ls_pref_mat-erzet = sy-uzeit.
    ls_pref_mat-ernam = sy-uname.
    insert zle_pref_mat from ls_pref_mat.
    pv_anz_ins = 1.
  endif.

ENDFORM.                    " AUFBEREITUNG_VIA_LAND

*&---------------------------------------------------------------------*
*&      Form  CHECK_UPLOADFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_UPLOADFILE CHANGING pv_subrc TYPE sy-subrc.

  DATA: lv_subrc TYPE sy-subrc.

  pv_subrc = 0.

  PERFORM ws_upload CHANGING lv_subrc.

  pv_subrc = lv_subrc.

  IF gt_log[] IS NOT INITIAL.
    PERFORM log.
  ELSE.
    MESSAGE S084(5Z).
  ENDIF.

ENDFORM.                    " CHECK_UPLOADFILE


*&---------------------------------------------------------------------*
*&      Form  LOAD_TABLE_ALLOWED_MTART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_TABLE_ALLOWED_MTART CHANGING pv_subrc TYPE sy-subrc.

  DATA: lv_subrc TYPE sy-subrc.

  CLEAR gt_allowed_mtart.

  pv_subrc = 0.

   SELECT  zpref_param~vakey2 INTO TABLE gt_allowed_mtart
    FROM  zpref_usauth INNER JOIN zpref_param
    ON    zpref_usauth~const_id = zpref_param~const_id
    WHERE zpref_usauth~uname = sy-uname AND
          zpref_param~vakey1 = 'MTART'.

ENDFORM.                    " LOAD_TABLE_ALLOWED_MTART


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
**  DATA LV_MATNR TYPE MATNR.
  DATA: LV_SUBRC TYPE SY-SUBRC.

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

  if p_titel eq gc_on.
    gv_loop_start = 2.
  else.
    gv_loop_start = 1.
  endif.

  loop at gt_upload from gv_loop_start into gs_upload.

    split gs_upload at gc_delim into gs_in-matnr
                                     gs_in-maktx
                                     gs_in-prdha
                                     gs_in-eu_fremd
                                     gs_in-pref_eu
                                     gs_in-noneu_fremd
                                     gs_in-pref_cn
                                     gs_in-pref_il
                                     gs_in-pref_ae.


    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT              = gs_in-matnr
     IMPORTING
        OUTPUT             = gs_in-matnr
     EXCEPTIONS
       LENGTH_ERROR       = 1
       OTHERS             = 2.

    IF SY-SUBRC = 0.
      CHECK gs_in-matnr IN s_matnr.
    ELSE.
      clear gs_log.
      gs_log-stat = icon_red_light.
      gs_log-matnr = gs_in-matnr.
      gs_log-descr = text-p08.
      append gs_log to gt_log.
    ENDIF.

*   check if material type is allowed to select
    perform check_if_mtart_is_allowed using gs_in-matnr
                                   changing lv_subrc
                                            gv_mtart.

    check lv_subrc eq 0.
    check gv_mtart in s_mtart.

    append gs_in to gt_in.

  endloop.

ENDFORM.                    " WS_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  CHECK_IF_MTART_IS_ALLOWED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_IF_MTART_IS_ALLOWED USING pv_matnr TYPE matnr
                            CHANGING pv_subrc TYPE sy-subrc
                                     pv_mtart TYPE mtart .

  DATA: lv_matnr TYPE matnr.

  write pv_matnr to lv_matnr no-zero.

  clear: gs_log, pv_mtart.

  select single mtart from mara into pv_mtart
    where matnr = pv_matnr.

  pv_subrc = sy-subrc.

* Material does not exist
  if sy-subrc ne 0.
    clear gs_log.
    write pv_matnr to lv_matnr no-zero.
    gs_log-stat  = icon_red_light.
    gs_log-matnr = lv_matnr.
    gs_log-mtart = pv_mtart.
    gs_log-descr = text-p04.
    append gs_log to gt_log.
    exit.
  endif.

  read table gt_allowed_mtart into gs_allowed_mtart
    with key mtart = pv_mtart.

  pv_subrc = sy-subrc.

  IF sy-subrc EQ 0.
    IF p_onlerr ne gc_on.  "Do not log error message
      clear gs_log.
      gs_log-stat = icon_green_light.
      gs_log-matnr = lv_matnr.
      gs_log-mtart = pv_mtart.
      perform get_maktx USING pv_matnr
                     CHANGING gs_log-spras
                              gs_log-maktx.
      gs_log-descr = text-p11.
      append gs_log to gt_log.
    ENDIF.
  ELSE.
    clear gs_log.
    gs_log-stat  = icon_red_light.
    gs_log-matnr = lv_matnr.
    gs_log-mtart = pv_mtart.
    perform get_maktx USING pv_matnr
                   CHANGING gs_log-spras
                            gs_log-maktx.

    gs_log-descr = text-p03.
    append gs_log to gt_log.
  ENDIF.

ENDFORM.                    " CHECK_IF_MTART_IS_ALLOWED

*---------------------------------------------------------------------*
*       FORM SELECTION_SETS                                           *
*---------------------------------------------------------------------*
FORM selection_sets.

***  tables: rs38m.
  data: ls_rs38m type rs38m.

  clear ls_rs38m.

  ls_rs38m-programm = sy-repid.

  CALL FUNCTION 'RS_VARIANT_SCREEN'
    EXPORTING
      report  = ls_rs38m-programm
    IMPORTING
      report  = ls_rs38m-programm
      variant = ls_rs38m-selset.

  SET PARAMETER ID 'RID' FIELD ls_rs38m-programm.

ENDFORM.                    "selection_sets

*---------------------------------------------------------------------*
*       FORM GET_MAKTX                                           *
*---------------------------------------------------------------------*
FORM get_maktx USING pv_matnr TYPE MATNR
            CHANGING pv_spras TYPE spras
                     pv_maktx TYPE maktx.

  CLEAR: pv_spras, pv_maktx.

  SELECT SINGLE spras maktx FROM makt INTO ( pv_spras, pv_maktx )
    WHERE matnr = pv_matnr AND
          spras = sy-langu.

* Without language
  IF sy-subrc NE 0.
    SELECT SINGLE spras maktx FROM makt INTO ( pv_spras, pv_maktx )
      WHERE matnr = pv_matnr.
  ENDIF.

ENDFORM.                    "GET_MAKTX

*&---------------------------------------------------------------------*
*&      Form  SHOW_ALLOWED_MTART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_ALLOWED_MTART.

  CLEAR p_almart.

  LOOP AT gt_allowed_mtart INTO GS_ALLOWED_MTART.
    CONCATENATE gs_allowed_mtart p_almart INTO p_almart
    SEPARATED BY SPACE
    IN CHARACTER MODE.
  ENDLOOP.

ENDFORM.                    " SHOW_ALLOWED_MTART
