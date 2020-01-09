************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_F1C                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  init_M03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form init_m03.

  clear g_scode.

  clear w_lt01_batch.

  w_lt01_batch-lgnum = '620'.
  w_lt01_batch-betyp = 'F'.
  w_lt01_batch-werks = '6000'.
  w_lt01_batch-lgort = '6200'.
  w_lt01_batch-bwlvs = '911'.


endform.                                                    " init_M03
*&---------------------------------------------------------------------*
*&      Form  scan_Fertauf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form scan_fertauf.

  data: l_benum(20)  type c.

* scannen Fertigungsauftrag.

  clear g_scanval.

  scantitel = text-300.     "Fertigungsauftrag.

  perform scan_input_window using 'C' c_fert.

  if g_scanval = 'EXIT' or g_scanval = 'ABBR'.
    g_scode = '1'.
    message id 'ZDLE' type 'S' number '012' into  g_mestxt.
    write: / g_mestxt.
    message id 'ZDLE' type 'S' number '012' .
    exit.
  else.
    w_lt01_batch-benum = g_scanval.

* AUFTRAGSNUMMER UND NACHLAGERPLATZ MIT NULLEN AUFFÜLLEN
    concatenate '0000000000' w_lt01_batch-benum into l_benum
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    shift l_benum right deleting trailing ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_lt01_batch-benum = l_benum+10(10).

    w_lt01_batch-nlpla = w_lt01_batch-benum.

  endif.

endform.                    " scan_Fertauf
*&---------------------------------------------------------------------*
*&      Form  scan_Masch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form scan_masch.

* scannen Maschinenummer.

  clear g_scanval.

  scantitel = text-301.     "Maschinenummer.

  perform scan_input_window using 'C' c_masch.

  if g_scanval = 'EXIT' or g_scanval = 'ABBR'.
    g_scode = '1'.
    message id 'ZDLE' type 'S' number '012' into  g_mestxt.
    write: / g_mestxt.
    message id 'ZDLE' type 'S' number '012' .
    exit.
  else.
    w_lt01_batch-nlber = g_scanval.
  endif.

endform.                    " scan_Masch
*&---------------------------------------------------------------------*
*&      Form  scan_comp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form scan_comp.

* scannen Compount / Matnr.

  data:  l_matnr(36) type c.

  clear g_scanval.

  scantitel = text-302.     "Compount Nr.

  perform scan_input_window using 'C' c_comp.

  if g_scanval = 'EXIT' or g_scanval = 'ABBR'.
    g_scode = '1'.
    message id 'ZDLE' type 'S' number '012' into  g_mestxt.
    write: / g_mestxt.
    message id 'ZDLE' type 'S' number '012' .
    exit.
  else.

    w_lt01_batch-matnr = g_scanval.

    concatenate '000000000000000000'  w_lt01_batch-matnr into l_matnr
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    shift l_matnr right deleting trailing ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_lt01_batch-matnr = l_matnr+18(18).

  endif.

endform.                    " scan_comp
*&---------------------------------------------------------------------*
*&      Form  charge_bestimmen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form charge_bestimmen.

  data: l_lqua like table of lqua.
  data: w_lqua like lqua,
        lv_vfdat type mch1-vfdat.                     "CSJUL2010


  select * from lqua
           into table l_lqua
           where
           lgnum = '620'   and
           lgort = '6200'  and
           lgtyp = '001'   and
           bestq = ''      and
           matnr = w_lt01_batch-matnr.

  if sy-subrc <> '0'.
    g_scode = '1'.
   write: 'Bestandestabelle enthält keinen Eintrag zu diesen Kriterien'.
    message id 'ZDLE' type 'S' number '401' .
    exit.
  endif.

  loop at l_lqua into w_lqua.

    if w_lqua-gesme <> w_lqua-verme.
      delete l_lqua.
    endif.

  endloop.

* Verfalldatum aus Charge Nachlesen falls Update durch Benutzer

  loop at l_lqua into w_lqua.                               "CSJUL2010
*   Verfalldatum aus Charge dazulesen                       "CSJUL2010
    select single vfdat from mch1 into lv_vfdat             "CSJUL2010
      where matnr = w_lqua-matnr
        and charg = w_lqua-charg.                           "CSJUL2010
    if sy-subrc = 0 and not lv_vfdat is initial.            "CSJUL2010
      w_lqua-vfdat = lv_vfdat.                              "CSJUL2010
      modify l_lqua from w_lqua index sy-tabix.             "CSJUL2010
    endif.                                                  "CSJUL2010

  endloop.                                                  "CSJUL2010



  sort l_lqua by vfdat ascending charg descending.

  read table l_lqua index 1 into w_lqua.

  if sy-subrc = 0.

    write w_lqua-verme to w_lt01_batch-anfme.
    write w_lqua-charg to w_lt01_batch-charg.
    write w_lqua-lqnum to w_lt01_batch-ltap.
    .
  else.

    g_scode = '1'.
    write: 'Keine Bestände zu diesem Material am Lager.'.
    message id 'ZDLE' type 'S' number '401' .
  endif.

endform.                    " charge_bestimmen
*&---------------------------------------------------------------------*
*&      Form  Auftr_lagerpl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form auftr_lagerpl.

  data: begin of messtab occurs 10.
          include structure bdcmsgcoll.
  data: end of messtab.
  data:  l_text(100) type c.
  data: i_mess(1) type c.

  clear    bdcdata.
  refresh  bdcdata.

* call new Dynpro
  perform start_dynpro using 'SAPML01S' '0400' 'X'.
  perform fill_dynpro using w_lt01_batch-lgnum   'LAGP-LGNUM'.
  perform fill_dynpro using '100'                'LAGP-LGTYP'.
  perform fill_dynpro using w_lt01_batch-nlpla   'LAGP-LGPLA'.
  perform fill_dynpro using '=REFR'              'BDC_OKCODE'.

* call new Dynpro
  perform start_dynpro using 'SAPML01S' '0400' 'X'.
  perform fill_dynpro using w_lt01_batch-lgnum   'LAGP-LGNUM'.
  perform fill_dynpro using '100'                'LAGP-LGTYP'.
  perform fill_dynpro using w_lt01_batch-nlpla   'LAGP-LGPLA'.
  perform fill_dynpro using w_lt01_batch-nlber   'LAGP-LGBER'.
  perform fill_dynpro using '=BU'              'BDC_OKCODE'.

  call transaction 'LS01N'
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
    message id 'ZDLE' type 'S' number '403' .
  endif.


endform.                    " Auftr_lagerpl
*&---------------------------------------------------------------------*
*&      Form  Transportauftrag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form transportauftrag.

  data: begin of messtab occurs 10.
          include structure bdcmsgcoll.
  data: end of messtab.
  data:  l_text(100) type c.
  data: i_mess(1) type c.

  clear    bdcdata.
  refresh  bdcdata.

* call new Dynpro
  perform start_dynpro using 'SAPML03T' '0101' 'X'.
  perform fill_dynpro using w_lt01_batch-lgnum   'LTAK-LGNUM'.
  perform fill_dynpro using w_lt01_batch-benum   'LTAK-BENUM'.
  perform fill_dynpro using w_lt01_batch-betyp   'LTAK-BETYP'.
  perform fill_dynpro using w_lt01_batch-bwlvs   'LTAK-BWLVS'.
  perform fill_dynpro using w_lt01_batch-matnr   'LTAP-MATNR'.
  perform fill_dynpro using w_lt01_batch-anfme   'RL03T-ANFME'.
  perform fill_dynpro using w_lt01_batch-werks   'LTAP-WERKS'.
  perform fill_dynpro using w_lt01_batch-charg   'LTAP-CHARG'.
  perform fill_dynpro using '/00'                'BDC_OKCODE'.

* call new Dynpro
  perform start_dynpro using 'SAPML03T' '0102' 'X'.
  perform fill_dynpro using '/00'                'BDC_OKCODE'.
  perform fill_dynpro using w_lt01_batch-anfme   'RL03T-ANFME'.
  perform fill_dynpro using '001'                'LTAP-LETYP'.
  perform fill_dynpro using ''                   'LTAP-VLENR'.
  perform fill_dynpro using w_lt01_batch-ltap    'LTAP-VLQNR'.
  perform fill_dynpro using w_lt01_batch-nlber   'LTAP-NLBER'.
  perform fill_dynpro using w_lt01_batch-nlpla   'LTAP-NLPLA'.


  call transaction 'LT01'
        using bdcdata
        mode 'N'
        messages into messtab.

  clear i_mess.
  loop at messtab.
    if messtab-msgtyp = 'A' or
        messtab-msgtyp = 'E'.
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


*
endform.                    " Transportauftrag
