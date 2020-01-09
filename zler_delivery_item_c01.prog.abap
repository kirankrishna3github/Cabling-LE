*&---------------------------------------------------------------------*
*&  Include           ZLER_DELIVERY_ITEM_C01
*&---------------------------------------------------------------------*

class lcl_del_item definition.

  public section.

    class-methods: entry importing is_nast        type nast
                                   is_tnapr       type tnapr
                                   i_preview      type boole_d
                         returning value(e_subrc) type sy-subrc.


  private section.

    constants: begin of _wc_data,
                 ref_doc_type       type vbfa-vbtyp_n             value 'C',
                 spras_de           type sy-langu                 value 'DE',
                 tdobject_material  type stxh-tdobject            value 'MATERIAL',
                 tdid_grun          type stxh-tdid                value 'GRUN',
                 tdid_prue          type stxh-tdid                value 'PRUE',
                 objecttable_mara   type bapi1003_key-objecttable value 'MARA',
                 classtype_001      type bapi1003_key-classtype   value '001',
                 char_type_internal type cabn-atnam               value 'ZDAG_TYPI',
               end of _wc_data.

    class-data: begin of _ws_data,
                  begin of s_message,
                    s_nast           type nast,
                    s_tnapr          type tnapr,
                  end of s_message,
                  begin of s_print_parameters,
                    preview          type boole_d,
                    s_toa_dara       type toa_dara,
                    s_arc_params     type arc_params,
                    s_composer_param type ssfcompop,
                    s_control_param  type ssfctrlop,
                    s_recipient      type swotobjid,
                    s_sender         type swotobjid,
                    s_addr_key       type addr_key,
                  end of s_print_parameters,
                  s_print_data type ZCL_LE_PRINT_DELIVERY_HELPER=>TY_S_PRINT_DELIVERY_ITEM,
                end of _ws_data.

    class-methods: _protocol_update,
                   _get_data             returning value(e_subrc) type sy-subrc,
                   _set_print_parameters importing i_preview      type boole_d
                                         returning value(e_subrc) type sy-subrc,
                   _print                returning value(e_subrc) type sy-subrc,
                   _get_sales_order_data importing i_vbeln        type vbap-vbeln
                                                   i_posnr        type vbap-posnr,
                   _get_material_characteristics,
                   _get_bom_info,
                   _get_address_data,
                   _get_additional_data,
                   _get_texts,
                   _clear.

endclass.

class lcl_del_item implementation.

  method: entry.

    _clear( ).

    _ws_data-s_message-s_nast           = is_nast.
    _ws_data-s_message-s_tnapr          = is_tnapr.
    _ws_data-s_print_parameters-preview = i_preview.
    _ws_data-s_print_data-vbeln         = _ws_data-s_message-s_nast-objky(10).
    _ws_data-s_print_data-posnr         = _ws_data-s_message-s_nast-objky+10(6).

    e_subrc = _get_data( ).
    if e_subrc is initial.
      e_subrc = _set_print_parameters( i_preview ).
      if e_subrc is initial.
        e_subrc = _print( ).
      endif.
    endif.

  endmethod.

  method: _set_print_parameters.

  data: ls_itcpo     type itcpo,
        l_repid      type sy-repid,
        l_device     type tddevice.


  l_repid = sy-repid.

  call function 'WFMC_PREPARE_SMART_FORM' exporting  pi_nast       = _ws_data-s_message-s_nast
                                                     pi_addr_key   = _ws_data-s_print_parameters-s_addr_key
                                                     pi_repid      = l_repid
                                          importing  pe_returncode = e_subrc
                                                     pe_itcpo      = ls_itcpo
                                                     pe_device     = l_device
                                                     pe_recipient  = _ws_data-s_print_parameters-s_recipient
                                                     pe_sender     = _ws_data-s_print_parameters-s_sender.
  if e_subrc is initial.
    move-corresponding ls_itcpo to _ws_data-s_print_parameters-s_composer_param.
    _ws_data-s_print_parameters-s_control_param-device    = l_device.
    _ws_data-s_print_parameters-s_control_param-no_dialog = abap_true.
    _ws_data-s_print_parameters-s_control_param-preview   = _ws_data-s_print_parameters-preview.
    _ws_data-s_print_parameters-s_control_param-getotf    = ls_itcpo-tdgetotf.
    _ws_data-s_print_parameters-s_control_param-langu     = _ws_data-s_message-s_nast-spras.
  endif.

  endmethod.

  method: _get_data.

  data: ls_delivery_key        type leshp_delivery_key,
        ls_print_data_to_read  type ledlv_print_data_to_read,
        ls_dlv_delnote         type ledlv_delnote.


    ls_delivery_key-vbeln                 = _ws_data-s_print_data-vbeln.

    ls_print_data_to_read-hd_gen          = abap_true.
    ls_print_data_to_read-hd_adr          = abap_true.
    ls_print_data_to_read-hd_org          = abap_true.
    ls_print_data_to_read-hd_org_adr      = abap_true.
    ls_print_data_to_read-it_gen          = abap_true.
    ls_print_data_to_read-it_gen_descript = abap_true.
    ls_print_data_to_read-it_org          = abap_true.
    ls_print_data_to_read-it_org_descript = abap_true.
    ls_print_data_to_read-it_ref          = abap_true.
    ls_print_data_to_read-it_reford       = abap_true.
    ls_print_data_to_read-it_sched        = abap_true.

    call function 'LE_SHP_DLV_OUTP_READ_PRTDATA' exporting  is_delivery_key       = ls_delivery_key
                                                            is_print_data_to_read = ls_print_data_to_read
                                                            if_parvw              = _ws_data-s_message-s_nast-parvw
                                                            if_parnr              = _ws_data-s_message-s_nast-parnr
                                                            if_language           = _ws_data-s_message-s_nast-spras
                                                 importing  es_dlv_delnote        = ls_dlv_delnote
                                                 exceptions records_not_found     = 1
                                                            records_not_requested = 2
                                                            others                = 3.
    if not sy-subrc is initial.
      e_subrc = sy-subrc.
      _protocol_update( ).
    else.
      _ws_data-s_print_data-s_header-vkorg_adr = ls_dlv_delnote-hd_org-salesorg_adr.

      read table ls_dlv_delnote-hd_adr assigning field-symbol(<ls_hd_adr>) with key deliv_numb = _ws_data-s_print_data-vbeln
                                                                                    partn_role = _ws_data-s_message-s_nast-parvw.
      if sy-subrc is initial and <ls_hd_adr> is assigned.
        _ws_data-s_print_parameters-s_addr_key-addrnumber = <ls_hd_adr>-addr_no.
        _ws_data-s_print_parameters-s_addr_key-persnumber = <ls_hd_adr>-person_numb.
        _ws_data-s_print_parameters-s_addr_key-addr_type  = <ls_hd_adr>-address_type.
      endif.

      read table ls_dlv_delnote-it_gen assigning field-symbol(<ls_it_gen>) with key deliv_numb = _ws_data-s_print_data-vbeln
                                                                                    itm_number = _ws_data-s_print_data-posnr.
      if sy-subrc is initial and <ls_it_gen> is assigned.
        _ws_data-s_print_data-s_item-matnr = <ls_it_gen>-material.
      endif.

      read table ls_dlv_delnote-it_ref assigning field-symbol(<ls_it_ref>) with key deliv_numb   = _ws_data-s_print_data-vbeln
                                                                                    itm_number   = _ws_data-s_print_data-posnr
                                                                                    ref_doc_type = _wc_data-ref_doc_type.
      if sy-subrc is initial and <ls_it_ref> is assigned.

        _get_sales_order_data( i_vbeln = <ls_it_ref>-ref_doc i_posnr = <ls_it_ref>-ref_doc_it ).

        if _ws_data-s_print_data-s_item-prod_werks is initial.
          read table ls_dlv_delnote-it_org assigning field-symbol(<ls_it_org>) with key deliv_numb = _ws_data-s_print_data-vbeln
                                                                                        itm_number = _ws_data-s_print_data-posnr.
          if sy-subrc is initial and <ls_it_org> is assigned.
            _ws_data-s_print_data-s_item-prod_werks = <ls_it_org>-plant.
          endif.
        endif.

        if not _ws_data-s_print_data-s_item-prod_werks is initial.
          select single adrnr from t001w into _ws_data-s_print_data-s_item-prod_werks_adr where werks = _ws_data-s_print_data-s_item-prod_werks.
        endif.

        _get_material_characteristics( ).

        _get_bom_info( ).

      endif.

      _get_address_data( ).

      _get_texts( ).

      _get_additional_data( ).

    endif.

  endmethod.

  method: _get_texts.

    data: begin of ls_text,
            tdobject type stxh-tdobject,
            tdspras  type stxh-tdspras,
            tdid     type stxh-tdid,
            tdname   type stxh-tdname,
          end of ls_text.

    select single maktx into _ws_data-s_print_data-s_item-cable_maktx from makt
                       where matnr = _ws_data-s_print_data-s_item-cable_matnr
                         and spras = _ws_data-s_message-s_nast-spras.
    if not sy-subrc is initial.
      select single maktx into _ws_data-s_print_data-s_item-cable_maktx from makt
                         where matnr = _ws_data-s_print_data-s_item-cable_matnr
                           and spras = _wc_data-spras_de.
    endif.

    ls_text-tdobject = _wc_data-tdobject_material.
    ls_text-tdname   = _ws_data-s_print_data-s_item-cable_matnr.
    ls_text-tdid     = _wc_data-tdid_grun.
    ls_text-tdspras  = _ws_data-s_message-s_nast-spras.
    select single tdname as name tdobject as object tdid as id tdspras as spras
            into corresponding fields of _ws_data-s_print_data-s_item-s_basic_text
            from stxh where tdobject = ls_text-tdobject
                        and tdname   = ls_text-tdname
                        and tdid     = ls_text-tdid
                        and tdspras  = ls_text-tdspras.
    if not sy-subrc is initial.
      ls_text-tdspras  = _wc_data-spras_de.
      select single tdname as name tdobject as object tdid as id tdspras as spras
              into corresponding fields of _ws_data-s_print_data-s_item-s_basic_text
              from stxh where tdobject = ls_text-tdobject
                          and tdname   = ls_text-tdname
                          and tdid     = ls_text-tdid
                          and tdspras  = ls_text-tdspras.
    endif.

    ls_text-tdobject = _wc_data-tdobject_material.
    ls_text-tdname   = _ws_data-s_print_data-s_item-cable_matnr.
    ls_text-tdid     = _wc_data-tdid_prue.
    ls_text-tdspras  = _ws_data-s_message-s_nast-spras.
    select single tdname as name tdobject as object tdid as id tdspras as spras
            into corresponding fields of _ws_data-s_print_data-s_item-s_inspection_text
            from stxh where tdobject = ls_text-tdobject
                        and tdname   = ls_text-tdname
                        and tdid     = ls_text-tdid
                        and tdspras  = ls_text-tdspras.
    if not sy-subrc is initial.
      ls_text-tdspras  = _wc_data-spras_de.
      select single tdname as name tdobject as object tdid as id tdspras as spras
              into corresponding fields of _ws_data-s_print_data-s_item-s_inspection_text
              from stxh where tdobject = ls_text-tdobject
                          and tdname   = ls_text-tdname
                          and tdid     = ls_text-tdid
                          and tdspras  = ls_text-tdspras.
    endif.

  endmethod.

  method: _get_additional_data.

    call function 'CONVERSION_EXIT_LDATE_OUTPUT' exporting input = _ws_data-s_message-s_nast-erdat
                                                 importing output = _ws_data-s_print_data-s_item-date_long.
    shift _ws_data-s_print_data-s_item-date_long left deleting leading '0'.
    case _ws_data-s_print_data-s_item-date_long(2).
      when '1.'.   replace '.' into _ws_data-s_print_data-s_item-date_long with text-001.
      when '2.'.   replace '.' into _ws_data-s_print_data-s_item-date_long with text-002.
      when '3.'.   replace '.' into _ws_data-s_print_data-s_item-date_long with text-003.
      when others. replace '.' into _ws_data-s_print_data-s_item-date_long with text-004.
    endcase.

    select single mseht from t006a into _ws_data-s_print_data-s_item-cable_mseht where spras = _ws_data-s_message-s_nast-spras
                                                                                   and msehi = _ws_data-s_print_data-s_item-cable_meins.
    if not sy-subrc is initial.
      select single mseht from t006a into _ws_data-s_print_data-s_item-cable_mseht where spras = _wc_data-spras_de
                                                                                     and msehi = _ws_data-s_print_data-s_item-cable_meins.
    endif.

  endmethod.

  method: _get_address_data.

  data: ls_addresses type szadr_addr1_complete.


    call function 'ADDR_GET_COMPLETE' exporting  addrnumber              = _ws_data-s_print_data-s_item-prod_werks_adr
                                      importing  addr1_complete          = ls_addresses
                                      exceptions parameter_error         = 1
                                                 address_not_exist       = 2
                                                 internal_error          = 3
                                                 wrong_access_to_archive = 4
                                                 address_blocked         = 5
                                                 others                  = 6.
    if sy-subrc is initial.
      loop at ls_addresses-addr1_tab assigning field-symbol(<ls_addr1>) where data-date_from <= _ws_data-s_message-s_nast-erdat
                                                                          and data-date_to   >= _ws_data-s_message-s_nast-erdat.

        _ws_data-s_print_data-s_item-prod_werks_city = <ls_addr1>-data-city1.
        select single landx from t005t into _ws_data-s_print_data-s_item-prod_werks_ctr
                                      where t005t~land1 = <ls_addr1>-data-country
                                        and t005t~spras = _ws_data-s_message-s_nast-spras.
        _ws_data-s_print_data-s_item-prod_werks_dsc = <ls_addr1>-data-name1.
        exit.
      endloop.
    endif.

  endmethod.

  method: _get_material_characteristics.

  data: lt_return type table of bapiret2,
        lt_list   type table of bapi1003_alloc_list,
        lt_char   type table of bapi1003_alloc_values_char,
        ls_key    type bapi1003_key.


    ls_key-object      = _ws_data-s_print_data-s_item-cable_matnr.
    ls_key-objecttable = _wc_data-objecttable_mara.
    ls_key-classtype   = _wc_data-classtype_001.
    call function 'BAPI_OBJCL_GETCLASSES' exporting  objectkey_imp   = ls_key-object
                                                     objecttable_imp = ls_key-objecttable
                                                     classtype_imp   = ls_key-classtype
                                                     read_valuations = abap_true
                                          tables     alloclist       = lt_list
                                                     allocvalueschar = lt_char
                                                     return          = lt_return.
    try.
      _ws_data-s_print_data-s_item-s_chars-type_internal = lt_char[ charact = _wc_data-char_type_internal ]-value_char.
      catch cx_sy_itab_line_not_found.
    endtry.

  endmethod.

  method: _get_sales_order_data.

  data: lt_components type zcl_harnessing_core=>zif_harnessing_core~ty_components_attr_t.

    data(lo_so_item) = new zrucl_so_item_info( iv_read_vbak_from_db = abap_true
                                               iv_read_vbap_from_db = abap_true
                                               iv_vbeln             = i_vbeln
                                               iv_posnr             = i_posnr ).
    if lo_so_item is bound.
      lo_so_item->get_cust_po_info( importing ev_bstkd = _ws_data-s_print_data-s_header-bstkd ).
      lo_so_item->get_first_prod_order( exporting iv_spras = _ws_data-s_message-s_nast-spras
                                        importing ev_first_prod_order = _ws_data-s_print_data-s_item-aufnr ).
      if not _ws_data-s_print_data-s_item-aufnr is initial.
        select single werks from aufk into _ws_data-s_print_data-s_item-prod_werks where aufnr = _ws_data-s_print_data-s_item-aufnr.
      endif.

      lo_so_item->get_simulated_bom_v1( exporting iv_aufnr = _ws_data-s_print_data-s_item-aufnr ).

      loop at lo_so_item->gt_bom_used assigning field-symbol(<ls_bom_used>).
        append value #( matnr = <ls_bom_used>-idnrk werks = <ls_bom_used>-werks ) to lt_components.
      endloop.

      zcl_harnessing_core=>zif_harnessing_core~material_cable_plug_attr_get( exporting iv_cable_dispo_xfilter = abap_true
                                                                             changing  ct_mat_attr            = lt_components ).
      loop at lt_components assigning field-symbol(<ls_component>) where cable = abap_true.
        _ws_data-s_print_data-s_item-cable_matnr = <ls_component>-matnr.
        select single resb~charg resb~bdmng resb~meins into (_ws_data-s_print_data-s_item-cable_charg,_ws_data-s_print_data-s_item-cable_menge,_ws_data-s_print_data-s_item-cable_meins)
                 from resb where rsnum = ( select rsnum from afko where aufnr = _ws_data-s_print_data-s_item-aufnr )
                             and matnr = <ls_component>-matnr.
      endloop.

    endif.

  endmethod.

  method: _get_bom_info.

  data: lt_components type zcl_harnessing_core=>zif_harnessing_core~ty_components_attr_t,
        lt_stpo       type table of stpo_api02,
        begin of ls_bom,
          matnr type mara-matnr,
          menge type stpo_api02-comp_qty,
          meins type stpo_api02-comp_unit,
        end of ls_bom,
        lt_bom  like table of ls_bom,
        l_matnr40 type CSAP_MBOM-MATNR.


    if _ws_data-s_print_data-s_item-cable_matnr is initial.
      call function 'CONVERSION_EXIT_MATN2_OUTPUT' exporting input  = _ws_data-s_print_data-s_item-matnr
                                                   importing output = l_matnr40.

      call function 'CSAP_MAT_BOM_READ' exporting  material  = l_matnr40
                                                   plant     = _ws_data-s_print_data-s_item-prod_werks
                                                   bom_usage = '1'
                                        tables     t_stpo    = lt_stpo
                                        exceptions error     = 1
                                                   others    = 2.
      if sy-subrc is initial and not lt_stpo is initial.
        loop at lt_stpo assigning field-symbol(<ls_stpo>).
          clear: ls_bom.
          call function 'CONVERSION_EXIT_MATN2_INPUT' exporting  input            = <ls_stpo>-component
                                                      importing  output           = ls_bom-matnr
                                                      exceptions number_not_found = 1
                                                                 length_error     = 2
                                                                 others           = 3.

          append value #( matnr = ls_bom-matnr menge = <ls_stpo>-comp_qty meins = <ls_stpo>-comp_unit ) to lt_bom.
          append value #( matnr = ls_bom-matnr werks = _ws_data-s_print_data-s_item-prod_werks ) to lt_components.
        endloop.

        zcl_harnessing_core=>zif_harnessing_core~material_cable_plug_attr_get( exporting iv_cable_dispo_xfilter = abap_true
                                                                               changing  ct_mat_attr            = lt_components ).
        loop at lt_components assigning field-symbol(<ls_component>) where cable = abap_true.
          _ws_data-s_print_data-s_item-cable_matnr = <ls_component>-matnr.
          read table lt_bom assigning field-symbol(<ls_bom>) with key matnr = <ls_component>-matnr.
          if sy-subrc is initial and <ls_bom> is assigned.
            _ws_data-s_print_data-s_item-cable_menge = <ls_bom>-menge.
            _ws_data-s_print_data-s_item-cable_meins = <ls_bom>-meins.
          endif.
*          _ws_data-s_print_data-s_item-cable_menge = <ls_component>-menge.
        endloop.

      endif.

    endif.

  endmethod.

  method: _clear.

    clear: _ws_data.

  endmethod.

  method: _protocol_update.

    call function 'NAST_PROTOCOL_UPDATE' exporting  msg_arbgb              = syst-msgid
                                                    msg_nr                 = syst-msgno
                                                    msg_ty                 = syst-msgty
                                                    msg_v1                 = syst-msgv1
                                                    msg_v2                 = syst-msgv2
                                                    msg_v3                 = syst-msgv3
                                                    msg_v4                 = syst-msgv4
                                         exceptions message_type_not_valid = 1
                                                    no_sy_message          = 2
                                                    others                 = 3.

  endmethod.

  method: _print.

    data: l_fm_name type rs38l_fnam.


      call function 'SSF_FUNCTION_MODULE_NAME' exporting  formname           = _ws_data-s_message-s_tnapr-sform
                                               importing  fm_name            = l_fm_name
                                               exceptions no_form            = 1
                                                          no_function_module = 2
                                                          others             = 3.
      if not sy-subrc is initial.
        e_subrc = sy-subrc.
        _protocol_update( ).

      else.

        call function l_fm_name exporting  archive_index      = _ws_data-s_print_parameters-s_toa_dara
                                           archive_parameters = _ws_data-s_print_parameters-s_arc_params
                                           control_parameters = _ws_data-s_print_parameters-s_control_param
                                           mail_recipient     = _ws_data-s_print_parameters-s_recipient
                                           mail_sender        = _ws_data-s_print_parameters-s_sender
                                           output_options     = _ws_data-s_print_parameters-s_composer_param
                                           user_settings      = ' '
                                           is_nast            = _ws_data-s_message-s_nast
                                           is_data            = _ws_data-s_print_data
                                exceptions formatting_error   = 1
                                           internal_error     = 2
                                           send_error         = 3
                                           user_canceled      = 4
                                           others             = 5.
        if not sy-subrc is initial.
          e_subrc = sy-subrc.
          _protocol_update( ).
        endif.
      endif.

  endmethod.

endclass.
