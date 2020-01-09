************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_1_F00                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  output_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form output_selection.
*

endform.                    " output_selection
*&---------------------------------------------------------------------*
*&      Form  selekt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form selekt.

  delete  gt_outtab where sel = ''.
  call method grid1->set_ready_for_input
            exporting i_ready_for_input = 0.

  call method grid1->refresh_table_display.

endform.                    " selekt

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data_and_init_style .

  data: lt_sflight type table of sflight with header line,
        lt_celltab type lvc_t_styl,
        l_index type i.


  s_lgtyp-sign = 'E'.
  s_lgtyp-option = 'BT'.
  s_lgtyp-low = '900'.
  s_lgtyp-high = '999'.

  append s_lgtyp.

  delete adjacent duplicates from s_lgtyp.

  select * from lqua into table t_lqua
            where lgnum in s_lgnum
              and matnr in s_matnr
              and werks in s_werks
              and lgort in s_lgort
              and bestq in s_bestq
              and charg in s_charg
              and lgtyp in s_lgtyp
              and lgpla in s_lgpla
              and wenum in s_wenum.

  loop at t_lqua into w_lqua.
    clear w_outtab.
    move-corresponding w_lqua to w_outtab.
    w_outtab-nlgtyp = '001'.
    call function 'K_MATERIAL_GET'
         exporting
              par_matnr          = w_outtab-matnr
              par_spras          = sy-langu
         importing
              par_maktx          = w_outtab-maktx
         exceptions
              material_not_found = 1
              others             = 2.
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
    append w_outtab to gt_outtab.

  endloop.

*§2.After selecting data, set edit status for each row in a loop
*   according to field SEATSMAX.
  loop at gt_outtab.

    l_index = sy-tabix.
    refresh lt_celltab.
    perform fill_celltab using 'RW'
                         changing lt_celltab.

*§2c.Copy your celltab to the celltab of the current row of gt_outtab.
    insert lines of lt_celltab into table gt_outtab-celltab.
    modify gt_outtab index l_index.
  endloop.
endform.                               " SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
*&      Form  refresh_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_data.

  clear gt_outtab.
  refresh gt_outtab.
  perform select_data_and_init_style.

  call method grid1->set_ready_for_input
      exporting i_ready_for_input = 1.

  call method grid1->refresh_table_display.

endform.                    " refresh_data
*&---------------------------------------------------------------------*
*&      Form  tabelle_bereinigen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form tabelle_bereinigen.

  data: i_itab type  lvc_t_row.
  data: w_itab type line of lvc_t_row.

  call method  grid1->get_selected_rows
     importing
         et_index_rows  =   i_itab .

*  CLEAR gt_outtab-sel.
*  MODIFY gt_outtab TRANSPORTING sel WHERE sel = 'X'.

  if not i_itab is initial.
    loop at i_itab into w_itab.

      read table gt_outtab index w_itab-index.

      if     gt_outtab-sel = 'X'.
        gt_outtab-sel = ''.
      else.
        gt_outtab-sel = 'X'.
      endif.
      modify gt_outtab index w_itab-index transporting sel .
    endloop.
  endif.

endform.                    " tabelle_bereinigen
*&---------------------------------------------------------------------*
*&      Form  alle_frei_nach_Q
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alle_frei_nach_q.


*g_bewart = '999'
  clear t_imseg.
  refresh t_imseg.
  delete gt_outtab where bestq <> space.

  if not gt_outtab is initial.
    loop at gt_outtab into w_outtab .
      perform check_teil.
      modify gt_outtab from w_outtab.
      if w_outtab-sel = 'X'.
        perform buchen_frei_nach_q using w_outtab..
      else.
        message id 'ZDLE' type 'S' number '000' with text-302.
      endif.
    endloop.
  else.
    message id 'ZDLE' type 'S' number '000' with text-051.
    write: / text-051.
    exit.
  endif.
  perform create_goods_movement.
endform.                    " alle_frei_nach_Q

*&---------------------------------------------------------------------*
*&      Form  alle_q_nach_frei
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alle_q_nach_frei.
  clear t_imseg.
  refresh t_imseg.

  delete gt_outtab where bestq <> 'Q'.
  if not gt_outtab is initial.
    loop at gt_outtab into w_outtab.
      perform check_teil.
      modify gt_outtab from w_outtab.
      if w_outtab-sel = 'X'.
        perform buchen_q_nach_frei using w_outtab..
      else.
        message id 'ZDLE' type 'S' number '000' with text-302.
      endif.
    endloop.
  else.
    message id 'ZDLE' type 'S' number '000' with text-051.
    write: / text-051.
    exit.
  endif.
  perform create_goods_movement.

endform.                    " alle_q_nach_frei

*&---------------------------------------------------------------------*
*&      Form  buchen_frei_nach_Q
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form buchen_frei_nach_q using w_outtab type zle_good_alv_out .

  if w_outtab-manme <> 0.
    if w_outtab-gesme <> w_outtab-manme.

      w_imseg-erfmg = w_outtab-manme.
    else.
      w_imseg-erfmg = w_outtab-gesme.
    endif.
  else.
    w_imseg-erfmg = w_outtab-gesme.
  endif.

  w_imseg-bwart = g_bewart.
  w_imseg-kzbew = ''.
  w_imseg-erfme = w_outtab-meins.
  w_imseg-meins = w_outtab-meins.
  w_imseg-charg = w_outtab-charg.
  w_imseg-matnr = w_outtab-matnr.
  w_imseg-werks = w_outtab-werks.
  w_imseg-lgort = w_outtab-lgort.


  append w_imseg to t_imseg.

endform.                    " buchen_frei_nach_Q

*&---------------------------------------------------------------------*
*&      Form  buchen_q_nach_frei
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_OUTTAB  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form buchen_q_nach_frei using    p_w_outtab TYPE ZLE_GOOD_ALV_OUT.
                                                 "smart: 2010-08-02 #105


  if w_outtab-manme <> 0.
    if w_outtab-gesme <> w_outtab-manme.
      w_imseg-erfmg = w_outtab-manme.
    else.
      w_imseg-erfmg = w_outtab-gesme.
    endif.
  else.
    w_imseg-erfmg = w_outtab-gesme.
  endif.

  w_imseg-bwart = g_bewart.

  w_imseg-kzbew = ''.
  w_imseg-erfme = w_outtab-meins.
  w_imseg-meins = w_outtab-meins.
  w_imseg-charg = w_outtab-charg.
  w_imseg-matnr = w_outtab-matnr.
  w_imseg-werks = w_outtab-werks.
  w_imseg-lgort = w_outtab-lgort.
*  w_imseg-erfmg = w_outtab-gesme.

  append w_imseg to t_imseg.

endform.                    " buchen_q_nach_frei


*&---------------------------------------------------------------------*
*&      Form  alle_neuer_LGPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alle_neuer_lgpl.

  data:  chk(1) type c.


  data:lt_outtab type table of zle_good_alv_out with header line.

  data: l_gesme1 type lqua_ausme,
        l_gesme2 type lqua_ausme.


  scantitel = text-300.     "Alle in gleichen Lagerbereich
  clear g_scode.

  if not gt_outtab is initial.

    do.
      perform scan_input_window using 'C'.
      chk = '1'.
      if g_scode = '1'.
        exit.
      endif.
      perform check_lgber changing chk .
      if chk is initial.
        exit.
      endif.

    enddo.
    check g_scode is initial.

    loop at gt_outtab into w_outtab.
      clear:  l_gesme1,  l_gesme2.
      if w_outtab-manme <> 0.
        if w_outtab-gesme <> w_outtab-manme.
          l_gesme1 = w_outtab-manme.
          l_gesme2 = w_outtab-gesme - w_outtab-manme.
          w_outtab-gesme = l_gesme1.
          w_outtab-bwlvs = '999'.
          w_outtab-nalp = ''.
          w_outtab-nlber = g_nlber.

          modify gt_outtab from w_outtab.

*           Anfügen Buchungssatz LT01 für Restbestand (Etikete
*           DRucken LgPl an gleichen LGPL

          w_outtab-gesme = l_gesme2.
          w_outtab-bwlvs = '998'.
          w_outtab-nalp = w_outtab-lgpla.
          w_outtab-fldiff = 'X'.
          append w_outtab to lt_outtab.
          clear w_outtab.
        else.
          w_outtab-bwlvs = '999'.
          w_outtab-nalp = ''.
          w_outtab-nlber = g_nlber.
          modify gt_outtab from w_outtab.
        endif.
      else.
        w_outtab-bwlvs = '999'.
        w_outtab-nalp = ''.
        w_outtab-nlber = g_nlber.
        modify gt_outtab from w_outtab.

      endif.
    endloop.
    append lines of lt_outtab to gt_outtab..

    clear gt_printab[].

    loop at gt_outtab into w_outtab.
*      w_outtab-nalp = ''.
*      w_outtab-nlber = g_nlber.
*      MODIFY gt_outtab FROM w_outtab.
      if w_outtab-fldiff = 'X'.
        perform buchen_alle_lt01 using w_outtab 'C'..
      else.
        perform buchen_alle_lt01 using w_outtab 'C'..
      endif.
      commit work.
      wait up to 1 seconds.
    endloop.
    perform print_ruest.
  else.
    message id 'ZDLE' type 'S' number '000' with text-051.
    write: / text-051.
    exit.
  endif.

endform.                    " alle_neuer_LGPL

*&---------------------------------------------------------------------*
*&      Form  einzeln_neuer_LGPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form einzeln_neuer_lgpl.

  data: chk(1) type c.

  scantitel = text-301.     "Alle auf gleichen Lagerplatz
  clear g_scode.

  if not gt_outtab is initial.

    loop at gt_outtab into w_outtab.
      check g_scode is initial.

      do.
        chk = '1'.
        perform scan_input_window using 'E'.
        if g_scode = '1'.
          exit.
        endif.
        perform check_lgpl changing chk .
        if chk is initial.
          exit.
        endif.
      enddo.

      check g_scode is initial.
      w_outtab-nalp = g_nalp.
      modify gt_outtab from w_outtab .
      perform buchen_alle_lt01 using w_outtab 'E'..
    endloop.
  else.
    message id 'ZDLE' type 'S' number '000' with text-051.
    write: / text-051.
    exit.
  endif.

endform.                    " einzeln_neuer_LGPL

*---------------------------------------------------------------------*
*       FORM create_goods_movement                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form create_goods_movement.

  data: l_imkpf like imkpf.
  data: t_emseg like table of emseg.
  data: l_emseg like emseg.
  data: l_emkpf like emkpf.
  data: l_emkpf1 like emkpf.
  data:  l_text(100) type c.
  data:lt_outtab type table of zle_good_alv_out with header line.

  data: l_gesme1 type lqua_ausme,
        l_gesme2 type lqua_ausme.

  data:  l_lgb(3) type c,
         l_lgpla(10) type c,
         l_memid(20) type c.

  l_memid = 'M_LGBER'.

  l_imkpf-bldat = sy-datum.
  l_imkpf-budat = sy-datum.

  call function 'MB_CREATE_GOODS_MOVEMENT'
       exporting
            imkpf = l_imkpf
            ctcod = 'MB1B'
       importing
            emkpf = l_emkpf
       tables
            emseg = t_emseg
            imseg = t_imseg.

  if l_emkpf-subrc = '1'.
    call function 'MB_POST_GOODS_MOVEMENT'
         importing
              emkpf = l_emkpf.
    if l_emkpf-subrc is initial.
      commit work.
      wait up to 2 seconds.
      write: / text-060.



      loop at gt_outtab into w_outtab where sel = 'X'.
        clear:  l_gesme1,  l_gesme2.

        if w_outtab-lgber is initial.

          select single lgber from lagp into w_outtab-lgber
                 where lgnum = w_outtab-lgnum
                  and  lgtyp = '001'
                  and  lgpla = w_outtab-lgpla.
        endif.


        if w_outtab-manme <> 0.
          if w_outtab-gesme <> w_outtab-manme.
            w_outtab-sel = ' '.
            modify gt_outtab from w_outtab.
            check w_outtab-lgber = 'SL' or
                  w_outtab-lgber = 'FL' or
                  w_outtab-lgber = 'BO' or
                  w_outtab-lgber = 'BL'.

            w_outtab-sel = 'X'.
            l_gesme1 = w_outtab-manme.
            l_gesme2 = w_outtab-gesme - w_outtab-manme.
            w_outtab-gesme = l_gesme1.
            w_outtab-bwlvs = '999'.
            w_outtab-fldiff = 'Y'.

            modify gt_outtab from w_outtab.


*           Anfügen Buchungssatz LT01 für Restbestand (Etikete
*           DRucken LgPl an gleichen LGPL

            w_outtab-gesme = l_gesme2.
            w_outtab-bwlvs = '998'.
            w_outtab-nalp = w_outtab-lgpla.
            w_outtab-fldiff = 'X'.
            append w_outtab to lt_outtab.
            clear w_outtab.
          else.
            w_outtab-bwlvs = '999'.
            modify gt_outtab from w_outtab.

          endif.
        else.
          w_outtab-bwlvs = '999'.
          modify gt_outtab from w_outtab.

        endif.
      endloop.
      append lines of lt_outtab to gt_outtab..

      loop at gt_outtab into w_outtab where sel = 'X'.
        perform lt01_bu .
      endloop.

*      PERFORM lt06_batch USING l_emkpf-mblnr l_emkpf-mjahr.
    else.
      message id 'ZDLE' type 'S' number '000' with text-051.
      write: / text-051.
    endif.

  else.
    call function 'MASS_MESSAGE_GET'
         exporting
              arbgb             = l_emkpf-msgid
              msgnr             = l_emkpf-msgno
              msgv1             = l_emkpf-msgv1
              msgv2             = l_emkpf-msgv2
              msgv3             = l_emkpf-msgv3
              msgv4             = l_emkpf-msgv4
         importing
              msgtext           = l_text
         exceptions
              message_not_found = 1
              others            = 2.
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

    write: /.
    write: / text-051.
    write: / l_text.
    message id 'ZDLE' type 'S' number '000' with  l_text. .

    loop at t_emseg into l_emseg.
      if not l_emseg-msgid is initial.
        call function 'MASS_MESSAGE_GET'
             exporting
                  arbgb             = l_emseg-msgid
                  msgnr             = l_emseg-msgno
                  msgv1             = l_emseg-msgv1
                  msgv2             = l_emseg-msgv2
                  msgv3             = l_emseg-msgv3
                  msgv4             = l_emseg-msgv4
             importing
                  msgtext           = l_text
             exceptions
                  message_not_found = 1
                  others            = 2.
        if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        endif.
        message id 'ZDLE' type 'S' number '000' with l_text. .

        write: / l_text.
      endif.
    endloop.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  LT06_batch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EMKPF_MBLNR  text
*----------------------------------------------------------------------*
form lt06_batch using    p_emkpf_mblnr p_emkpf_mjahr.

  data: i_jahr(4) type c.
  data: begin of messtab occurs 10.
          include structure bdcmsgcoll.
  data: end of messtab.
  data:  l_text(100) type c.
  data: i_mess(1) type c.

  i_jahr = p_emkpf_mjahr.

  clear    bdcdata.
  refresh  bdcdata.

***********************************************************************
*Achtung: nicht ändern. Wenn System und Netzwerk zu stark belastet ist,
*         geht der LT06 auf die Bretter.....
  wsy_subrc = 1.
  while wsy_subrc ne 0.
    wait up to 1 seconds.
    select single * from mkpf where mblnr = p_emkpf_mblnr
                                and mjahr = i_jahr.
    wsy_subrc = sy-subrc.
  endwhile.
  wait up to 8 seconds.
***********************************************************************


* call New Dynpro
  perform start_dynpro using 'SAPML02B' '0203' 'X'.
  perform fill_dynpro using p_emkpf_mblnr   'RL02B-MBLNR'.
  perform fill_dynpro using i_jahr          'RL02B-MJAHR'.
  perform fill_dynpro using 'D'             'RL02B-DUNKL'.
  perform fill_dynpro using '/00'           'BDC_OKCODE'.

  call transaction 'LT06'
        using bdcdata
        mode 'N'
        messages into messtab.

  clear i_mess.
  loop at messtab.
    if messtab-msgtyp = 'A'.
      i_mess = '1'.
      call function 'MASS_MESSAGE_GET'
           exporting
                arbgb             = messtab-msgid
                msgnr             = messtab-msgnr
                msgv1             = messtab-msgv1
                msgv2             = messtab-msgv2
                msgv3             = messtab-msgv3
                msgv4             = messtab-msgv4
           importing
                msgtext           = l_text
           exceptions
                message_not_found = 1
                others            = 2.
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.
      message id 'ZDLE' type 'S' number '000' with l_text. .
      write: / l_text.
      message id 'ZDLE' type 'S' number '000' with l_text .
    endif.
  endloop.

  if i_mess is initial.
    message id 'ZDLE' type 'S' number '011' .
  endif.

endform.                    " LT06_batch
*&---------------------------------------------------------------------*
*&      Form  LT01_BU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form lt01_bu.

  data: begin of messtab occurs 10.
          include structure bdcmsgcoll.
  data: end of messtab.
  data:  l_text(100) type c.
  data: i_mess(1) type c.
  data: i_menge(10) type c.
  data: i_bwlvs(3) type c.


  data:  l_lgb(3) type c,
         l_lgpla(10) type c,
         l_memid(20) type c.

  l_memid = 'M_LGBER'.

  i_bwlvs = g_bewart.
  i_bwlvs+0(1) = '9'.


*****  if  w_outtab-fldiff is initial.
******Lagerbereich ins Memory
*****    free memory id l_memid.
*****  else.
*****    l_lgb = '3'.
*****    free memory id l_memid.
*****    export l_lgb to memory id l_memid.
*****  endif.

  clear    bdcdata.
  refresh  bdcdata.

  write w_outtab-gesme to i_menge.

* call New Dynpro
  perform start_dynpro using 'SAPML03T' '0101' 'X'.
  perform fill_dynpro using w_outtab-lgnum   'LTAK-LGNUM'.
  if w_outtab-fldiff = 'X'.
    perform fill_dynpro using '998'          'LTAK-BWLVS'.
    perform fill_dynpro using w_outtab-bestq 'LTAP-BESTQ'.

  else.
    perform fill_dynpro using i_bwlvs        'LTAK-BWLVS'.
    perform fill_dynpro using g_bestkz_1       'LTAP-BESTQ'.

  endif.
  perform fill_dynpro using w_outtab-matnr   'LTAP-MATNR'.
  perform fill_dynpro using i_menge          'RL03T-ANFME'.
  perform fill_dynpro using w_outtab-werks   'LTAP-WERKS'.
  perform fill_dynpro using w_outtab-charg   'LTAP-CHARG'.
  perform fill_dynpro using w_outtab-lgtyp   'LTAP-LETYP'.
  perform fill_dynpro using w_outtab-lgort   'LTAP-LGORT'.
  perform fill_dynpro using '/00'            'BDC_OKCODE'.

* call New Dynpro
  perform start_dynpro using 'SAPML03T' '0102' 'X'.
  perform fill_dynpro using '/00'             'BDC_OKCODE'.
  perform fill_dynpro using i_menge           'RL03T-ANFME'.
  perform fill_dynpro using  'X'              'RL03T-SQUIT'.
  perform fill_dynpro using w_outtab-lgtyp    'LTAP-VLTYP'.
  perform fill_dynpro using w_outtab-lgpla    'LTAP-VLPLA'.
  if w_outtab-fldiff = 'X'.
    perform fill_dynpro using w_outtab-lgtyp  'LTAP-NLTYP'.
    perform fill_dynpro using w_outtab-lgber  'LTAP-NLBER'.
  else.
    perform fill_dynpro using '922'             'LTAP-NLTYP'.
    perform fill_dynpro using 'TR-ZONE'         'LTAP-NLPLA'.
  endif.
  call transaction 'LT01'
        using bdcdata
        mode 'N'
        messages into messtab.

  wait up to 1 seconds.
  clear i_mess.
  loop at messtab.
    if messtab-msgtyp = 'A'.
      i_mess = '1'.
      call function 'MASS_MESSAGE_GET'
           exporting
                arbgb             = messtab-msgid
                msgnr             = messtab-msgnr
                msgv1             = messtab-msgv1
                msgv2             = messtab-msgv2
                msgv3             = messtab-msgv3
                msgv4             = messtab-msgv4
           importing
                msgtext           = l_text
           exceptions
                message_not_found = 1
                others            = 2.
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.
      message id 'ZDLE' type 'S' number '000' with l_text. .
      write: / l_text.
      message id 'ZDLE' type 'S' number '000' with l_text .
    endif.
  endloop.

  if i_mess is initial.
    message id 'ZDLE' type 'S' number '003' .
  endif.

*Als Q von TR-Lager zürückholen
  clear    bdcdata.
  refresh  bdcdata.
  if w_outtab-fldiff <> 'X'.

* call New Dynpro
    perform start_dynpro using 'SAPML03T' '0101' 'X'.
    perform fill_dynpro using w_outtab-lgnum   'LTAK-LGNUM'.
    perform fill_dynpro using i_bwlvs          'LTAK-BWLVS'.
    perform fill_dynpro using w_outtab-matnr   'LTAP-MATNR'.
    perform fill_dynpro using i_menge          'RL03T-ANFME'.
    perform fill_dynpro using w_outtab-werks   'LTAP-WERKS'.
    perform fill_dynpro using w_outtab-charg   'LTAP-CHARG'.
    perform fill_dynpro using w_outtab-lgtyp   'LTAP-LETYP'.
    perform fill_dynpro using w_outtab-lgort   'LTAP-LGORT'.
    perform fill_dynpro using g_bestkz_2       'LTAP-BESTQ'.
    perform fill_dynpro using '/00'            'BDC_OKCODE'.

* call New Dynpro
    perform start_dynpro using 'SAPML03T' '0102' 'X'.
    perform fill_dynpro using '/00'             'BDC_OKCODE'.
    perform fill_dynpro using i_menge           'RL03T-ANFME'.
    perform fill_dynpro using  'X'              'RL03T-SQUIT'.
    perform fill_dynpro using '001'             'LTAP-LETYP'.
    perform fill_dynpro using '922'             'LTAP-VLTYP'.
    perform fill_dynpro using 'TR-ZONE'         'LTAP-VLPLA'.
    perform fill_dynpro using w_outtab-lgtyp    'LTAP-NLTYP'.
    perform fill_dynpro using w_outtab-lgber    'LTAP-NLBER'.

    call transaction 'LT01'
          using bdcdata
          mode 'N'
          messages into messtab.

    wait up to 1 seconds.
    clear i_mess.
    loop at messtab.
      if messtab-msgtyp = 'A'.
        i_mess = '1'.
        call function 'MASS_MESSAGE_GET'
             exporting
                  arbgb             = messtab-msgid
                  msgnr             = messtab-msgnr
                  msgv1             = messtab-msgv1
                  msgv2             = messtab-msgv2
                  msgv3             = messtab-msgv3
                  msgv4             = messtab-msgv4
             importing
                  msgtext           = l_text
             exceptions
                  message_not_found = 1
                  others            = 2.
        if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        endif.
        message id 'ZDLE' type 'S' number '000' with l_text. .
        write: / l_text.
        message id 'ZDLE' type 'S' number '000' with l_text .
      endif.


    endloop.

    if i_mess is initial.
      message id 'ZDLE' type 'S' number '003' .
    endif.
  endif.

endform.                                                    " LT01_BU

*&---------------------------------------------------------------------*
*&      Form  scan_input_window
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

form scan_input_window using l_dt TYPE CLIKE..   "smart: 2010-08-02 #105

  data: input(50) type c.

  clear sy-ucomm.
  clear g_scode.
  clear g_scanval.

  if l_dt = 'C'.
    call screen 1001 starting at 2 2 ending at 54 6.
    if g_scanval = 'EXIT' or g_scanval = 'ABBR'.
      g_scode = '1'.
      exit.
    else.
      g_nlber = g_scanval.
    endif.
  elseif l_dt = 'E'.
    call screen 1002 starting at 2 2 ending at 54 15.
    if g_scanval = 'EXIT' or g_scanval = 'ABBR'.
      g_scode = '1'.
      exit.
    else.
      g_nalp = g_scanval.
    endif.

  endif.

endform.                    " scan_input_window
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1001 input.
  perform user_command_1001.

endmodule.                 " USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*&      Form  user_command_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_command_1001.

  data: ok_code type syucomm.         " return code from screen

  ok_code = sy-ucomm.

  case g_scanval.
    when 'ABBR'.
      ok_code = 'ABBR'.
    when 'EXIT'.
      ok_code = 'EXIT'.
    when 'ENTR'.
      ok_code = 'ENTR'.
      clear g_scanval.
  endcase.

  case ok_code.
    when 'ENTR'.
      clear: sy-ucomm.
      leave to screen 0.
    when 'ABBR'.
      g_scanval = 'ABBR'.
      g_scode = '1'.
      clear: sy-ucomm.
      leave to screen 0.
    when 'EXIT'.
      g_scanval = 'EXIT'.
      g_scode = '1'.
      clear: sy-ucomm.
      leave to screen 0.
  endcase.

endform.                    " user_command_1001
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_1001 output.
  set pf-status 'INP_1A'.
  set titlebar 'INP_1A'.

endmodule.                 " STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_1002 output.
  set pf-status 'INP_1A'.
  set titlebar 'INP_1A'.

endmodule.                 " STATUS_1002  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_1002 input.

  ok_code = sy-ucomm.

  case g_scanval.
    when 'ABBR'.
      ok_code = 'ABBR'.
    when 'EXIT'.
      ok_code = 'EXIT'.
    when 'ENTR'.
      ok_code = 'ENTR'.
      clear g_scanval.
  endcase.

  case ok_code.
    when 'ENTR'.
      clear: sy-ucomm.
      leave to screen 0.
    when 'ABBR'.
      g_scanval = 'ABBR'.
      g_scode = '1'.
      clear: sy-ucomm.
      leave to screen 0.
    when 'EXIT'.
      g_scanval = 'EXIT'.
      g_scode = '1'.
      clear: sy-ucomm.
      leave to screen 0.
  endcase.

endmodule.                 " USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*&      Form  buchen_ALLE_LT01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_OUTTAB  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form buchen_alle_lt01 using    w_outtab type zle_good_alv_out flag
TYPE CLIKE.                                      "smart: 2010-08-02 #105

  data: begin of messtab occurs 10.
          include structure bdcmsgcoll.
  data: end of messtab.
  data:  l_text(100) type c.
  data: t_meng(15) type c.
  data: i_mess(1) type c.

  data: l_lgb(3) type c.
  data: l_memid(20) type c.
  data: l_lgb1(3) type c.
  data: l_bwlvs type bwlvs.
  data: l_lgpla(10) type c..

  data:lt_outtab type table of zle_good_alv_out with header line.

  data: l_gesme1 type lqua_ausme,
        l_gesme2 type lqua_ausme.

  clear    bdcdata.
  refresh  bdcdata.

*  IF w_outtab-manme <> 0.
*    IF w_outtab-gesme <> w_outtab-manme.
*      l_bwlvs = g_bwlvs_2.
**      WRITE w_outtab-manme TO t_meng.
*    ELSE.
*      l_bwlvs = g_bwlvs.
**      WRITE w_outtab-gesme TO t_meng.
*    ENDIF.
*  ELSE.
*    l_bwlvs = g_bwlvs.
**    WRITE w_outtab-gesme TO t_meng.
*  ENDIF.


  t_meng = w_outtab-gesme..
  l_bwlvs = w_outtab-bwlvs.

  case: l_bwlvs.
    when '998'.                    "mit  Etikettierung Codesoft
    when others.  l_bwlvs = '999'. "ohne Etikettierung Codesoft
  endcase.

*  write w_outtab-gesme to t_meng.
* call New Dynpro
  perform start_dynpro using 'SAPML03T' '0101' 'X'.
  perform fill_dynpro using w_outtab-lgnum   'LTAK-LGNUM'.
  perform fill_dynpro using l_bwlvs          'LTAK-BWLVS'.
  perform fill_dynpro using w_outtab-matnr   'LTAP-MATNR'.
  perform fill_dynpro using t_meng           'RL03T-ANFME'.
  perform fill_dynpro using w_outtab-werks   'LTAP-WERKS'.
  perform fill_dynpro using w_outtab-lgort   'LTAP-LGORT'.
  perform fill_dynpro using w_outtab-charg   'LTAP-CHARG'.
  perform fill_dynpro using w_outtab-bestq   'LTAP-BESTQ'.
  perform fill_dynpro using '/00'            'BDC_OKCODE'.
* call New Dynpro
  perform start_dynpro using 'SAPML03T' '0102' 'X'.
  perform fill_dynpro using '/00'                'BDC_OKCODE'.
  perform fill_dynpro using t_meng               'RL03T-ANFME'.
  perform fill_dynpro using 'X'                  'RL03T-SQUIT'.
  perform fill_dynpro using w_outtab-lgtyp       'LTAP-VLTYP'.
  perform fill_dynpro using w_outtab-lgpla       'LTAP-VLPLA'.
  perform fill_dynpro using w_outtab-lgtyp       'LTAP-NLTYP'.
  if flag = 'C'.
    perform fill_dynpro using w_outtab-nlber       'LTAP-NLBER'.
  endif.
  if flag = 'E'.
    perform fill_dynpro using w_outtab-nalp        'LTAP-NLPLA'.
  endif.

*ins Memory
  l_memid = 'M_LGBER_ZLEGM10'.
  l_lgb = w_outtab-nlber.
  free memory id l_memid.
  export l_lgb to memory id l_memid.

  call transaction 'LT01'
        using bdcdata
        mode 'N'
        messages into messtab.

  wait up to 2 seconds.
  if sy-subrc = 0.

* Wenn nicht Differenzmenge Rüstschein Drucken.
    if w_outtab-fldiff is initial..
      if w_outtab-nalp is initial.
* Lesen Memory für Lagerplatz nach Buchung.
* Wird im Userexit LT01 geschrieben.
        l_memid = 'M_NLGPLA'.
        clear l_lgpla.
        import l_lgpla to l_lgpla from memory id l_memid.
        w_outtab-nalp = l_lgpla.
      endif.
      if w_outtab-nlber is initial.
        select single lgber from lagp into w_outtab-nlber
               where lgnum = w_outtab-lgnum
                and  lgtyp = '001'
                and  lgpla = w_outtab-nalp.
      endif.
      if w_outtab-lgber is initial.

        select single lgber from lagp into w_outtab-lgber
               where lgnum = w_outtab-lgnum
                and  lgtyp = '001'
                and  lgpla = w_outtab-lgpla.
      endif.
      w_outtab-gesme = t_meng.
      append w_outtab to gt_printab.
*      perform print_eti.
    endif.
    clear i_mess.
    loop at messtab.
      if messtab-msgtyp = 'A'.
        i_mess = '1'.
        call function 'MASS_MESSAGE_GET'
             exporting
                  arbgb             = messtab-msgid
                  msgnr             = messtab-msgnr
                  msgv1             = messtab-msgv1
                  msgv2             = messtab-msgv2
                  msgv3             = messtab-msgv3
                  msgv4             = messtab-msgv4
             importing
                  msgtext           = l_text
             exceptions
                  message_not_found = 1
                  others            = 2.
        if sy-subrc <> 0.

        endif.
        message id 'ZDLE' type 'S' number '000' with l_text. .
        write: / l_text.
        message id 'ZDLE' type 'S' number '000' with l_text .
      endif.
    endloop.

    if i_mess is initial.
      message id 'ZDLE' type 'S' number '003' .
    endif.
  else.
    message id 'ZDLE' type 'S' number '004' .
  endif.

endform.                    " buchen_ALLE_LT01
*&---------------------------------------------------------------------*
*&      Form  check_lgpl
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

form check_lgpl changing chk TYPE CLIKE.         "smart: 2010-08-02 #105

  select single * from lagp where lgnum = w_outtab-lgnum
                          and   lgtyp = w_outtab-nlgtyp
                          and   lgpla = g_nalp.



  if sy-subrc = 0.
    clear chk.
    if  w_outtab-lgnum = '600'.
      if lagp-anzqu = 0.
        clear chk.
      else.
*      MESSAGE
        message id 'ZDLE' type 'I' number '030'.
        chk = '1'.
      endif.
    endif.
  else.
    chk = '1'.
    message id 'ZDLE' type 'I' number '031'.
  endif.


endform.                    " check_lgpl
*&---------------------------------------------------------------------*
*&      Form  check_lgber
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CHK  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form check_lgber changing chk TYPE CLIKE.        "smart: 2010-08-02 #105

  select single * from lagp where lgnum = w_outtab-lgnum
                          and   lgtyp = w_outtab-nlgtyp
                          and   lgber = g_nlber.
  if sy-subrc = 0.
    clear chk.
  else.
    message id 'ZDLE' type 'I' number '032' with g_nlber.
    chk = '1'.
  endif.

endform.                    " check_lgber
*&---------------------------------------------------------------------*
*&      Form  frei_an_sper
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form frei_an_sper.


  clear t_imseg.
  refresh t_imseg.

  delete gt_outtab where bestq <> space.
  if not gt_outtab is initial.
    loop at gt_outtab into w_outtab..
      perform check_teil.
      modify gt_outtab from w_outtab.
      if w_outtab-sel = 'X'.
        clear w_imseg.
*      w_imseg-bwart = g_bewart.
        perform get_imseg using w_outtab..
      else.
        message id 'ZDLE' type 'S' number '000' with text-302.
      endif.

    endloop.
  else.
    message id 'ZDLE' type 'S' number '000' with text-051.
    write: / text-051.
    exit.
  endif.
  perform create_goods_movement.

endform.                    " frei_an_sper
*&---------------------------------------------------------------------*
*&      Form  buchen_frei_an_sperr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_OUTTAB  text
*----------------------------------------------------------------------*
form buchen_frei_an_sperr using    p_w_outtab.



endform.                    " buchen_frei_an_sperr
*&---------------------------------------------------------------------*
*&      Form  get_imseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_OUTTAB  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form get_imseg using    p_w_outtab TYPE ZLE_GOOD_ALV_OUT.
                                                 "smart: 2010-08-02 #105



  if w_outtab-manme <> 0.
    if w_outtab-gesme <> w_outtab-manme.
      w_imseg-bwart = g_bewart_2.
      w_imseg-erfmg = w_outtab-manme.
    else.
      w_imseg-bwart = g_bewart.
      w_imseg-erfmg = w_outtab-gesme.
    endif.
  else.
    w_imseg-bwart = g_bewart.
    w_imseg-erfmg = w_outtab-gesme.
  endif.

  w_imseg-kzbew = ''.
  w_imseg-erfme = w_outtab-meins.
  w_imseg-meins = w_outtab-meins.
  w_imseg-charg = w_outtab-charg.
  w_imseg-matnr = w_outtab-matnr.
  w_imseg-werks = w_outtab-werks.
  w_imseg-lgort = w_outtab-lgort.
*  w_imseg-erfmg = w_outtab-gesme.

  append w_imseg to t_imseg.

endform.                    " get_imseg
*&---------------------------------------------------------------------*
*&      Form  sperr_an_frei
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sperr_an_frei.

  clear t_imseg.
  refresh t_imseg.

  delete gt_outtab where bestq <> 'S'.

  if not gt_outtab is initial.
    loop at gt_outtab into w_outtab..

      perform check_teil.
      modify gt_outtab from w_outtab.
      if w_outtab-sel = 'X'.
        clear w_imseg.
*      w_imseg-bwart = g_bewart.
        perform get_imseg using w_outtab..
      else.
        message id 'ZDLE' type 'S' number '000' with text-302.
      endif.
    endloop.
  else.
    message id 'ZDLE' type 'S' number '000' with text-051.
    write: / text-051.
    exit.
  endif.
  perform create_goods_movement.

endform.                    " sperr_an_frei
*&---------------------------------------------------------------------*
*&      Form  check_teil
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_teil.

  if w_outtab-lgber is initial.

    select single lgber from lagp into w_outtab-lgber
           where lgnum = w_outtab-lgnum
            and  lgtyp = '001'
            and  lgpla = w_outtab-lgpla.
  endif.


  if w_outtab-manme <> 0.
    if w_outtab-gesme <> w_outtab-manme.
      w_outtab-sel = ' '.
      check w_outtab-lgber = 'SL' or
            w_outtab-lgber = 'FL' or
            w_outtab-lgber = 'BO' or
            w_outtab-lgber = 'BL'.
      w_outtab-sel = 'X'.
    endif.
  endif.




endform.                    " check_teil
