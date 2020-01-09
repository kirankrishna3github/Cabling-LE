************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
***INCLUDE LZDLE_PACKF01 .
*----------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  delete_doppelte_interne_hu
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_doppelte_interne_hu.

  data: ls_vepo_in like vepo.
  data: ls_vepo_out like vepo.
  data: lt_vepo TYPE HUM_HU_ITEM_T.

  refresh lt_vepo.


*$smart (F) 2010-08-02 - #109 Inkompatible Variablen in Ausdrücken (z.
*$smart (F) 2010-08-02 - #109 Bsp MOVE, COMPUTE, etc.) (M)

  loop at gt_vepo into ls_vepo_in.


*$smart (F) 2010-08-02 - #109 Inkompatible Variablen in Ausdrücken (z.
*$smart (F) 2010-08-02 - #109 Bsp MOVE, COMPUTE, etc.) (M)

    read table lt_vepo into ls_vepo_out
      with key venum = ls_vepo_in-venum.

* Bereits vorhanden?
    if sy-subrc ne 0.

*$smart (F) 2010-08-02 - #109 Inkompatible Variablen in Ausdrücken (z.
*$smart (F) 2010-08-02 - #109 Bsp MOVE, COMPUTE, etc.) (M)

      append ls_vepo_in to lt_vepo.
    endif.

  endloop.


  refresh gt_vepo.
  gt_vepo[] = lt_vepo[].

ENDFORM.                    " delete_doppelte_interne_hu



*&---------------------------------------------------------------------*
*&      Form  load_lieferpositionen
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

FORM load_lieferpositionen using p_vbeln TYPE SHIP_DELIV-VBELN.
                                                 "smart: 2010-08-02 #105


  select vbeln posnr uecha from lips appending table gt_lips
    where vbeln = p_vbeln.



ENDFORM.                    " load_lieferpositionen
*&---------------------------------------------------------------------*
*&      Form  get_uecha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_VEKP_VBELN  text
*      -->P_LS_VEPO_POSNR  text
*      <--P_L_UECHA  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_uecha USING    p_vbeln TYPE VEPO-VBELN  "smart: 2010-08-02 #105
                        p_posnr TYPE VEPO-POSNR  "smart: 2010-08-02 #105
               CHANGING p_uecha TYPE LIPS-UECHA. "smart: 2010-08-02 #105

  clear p_uecha.

  read table gt_lips with key vbeln = p_vbeln
                              posnr = p_posnr.

  if sy-subrc eq 0.
    move gt_lips-uecha to p_uecha.
  endif.


ENDFORM.                    " get_uecha


*&---------------------------------------------------------------------*
*&      Form  get_materialkurztext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_VEKP_VHILM  text
*      <--P_T_PACK_MAKTX  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_materialkurztext USING    p_matnr TYPE  "smart: 2010-08-02 #105
  VEKP-VHILM                                     "smart: 2010-08-02 #105
                                   p_spras TYPE  "smart: 2010-08-02 #105
                                     SYST-LANGU  "smart: 2010-08-02 #105
                          CHANGING p_maktx TYPE ZSLE_PACK-MAKTX.
                                                 "smart: 2010-08-02 #105


  clear p_maktx.

  select single maktx from makt into p_maktx
    where matnr = p_matnr and
          spras = p_spras.

ENDFORM.                    " get_materialkurztext
