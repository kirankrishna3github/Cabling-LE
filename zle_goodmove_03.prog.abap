*&---------------------------------------------------------------------*
*& Report  ZLE_GOODMOVE                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Original funktionsfähig ohne Ergänzungen zum Prozess                *
*& Schneidemaschine'                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*


************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105 #115 #116
************************************************************************

report  zle_goodmove .


* Data definiton
include zle_goodmove_03_data.
*INCLUDE: zle_goodmove_data.

* Selections Dynpros
include zle_goodmove_03_sel.
*INCLUDE: zle_goodmove_sel.

* Common functions
include zle_goodmove_03_f00.
*INCLUDE: zle_goodmove_f00.

* WE zu Bestellung
include zle_goodmove_03_f1a.
*INCLUDE: zle_goodmove_f1a.

* WE zu PP
include zle_goodmove_03_f1b.
*INCLUDE: zle_goodmove_f1b.

* PV aus Rohmateriallager
include zle_goodmove_03_f1c.
*INCLUDE: zle_goodmove_f1c.

* Rücklagerung PV
include zle_goodmove_03_f1d.
*INCLUDE: zle_goodmove_f1d.

* Lagerplatzumbuchung WM Rohmaterial
include zle_goodmove_03_f1e.
*INCLUDE: zle_goodmove_f1e.

* Lagerplatzumbchung WM Fertigware
include zle_goodmove_03_f1f.
*INCLUDE: zle_goodmove_f1f.

include zle_goodmove_03_btci.
*INCLUDE: zle_goodmove_btci.



start-of-selection.

  do.

    if s_sel is initial.
      call screen 200.
    endif.

*  perform start_screen.

    case s_sel.
      when 'ZLEGM01'.     " Einlagern Wareneingang Rohmaterial
        perform init_m01.
        perform scan_ebln.
        check g_scode is initial.
        perform scan_matnr.
        check g_scode is initial.
        perform scan_vdat.
        check g_scode is initial.
        perform scan_mge.
        check g_scode is initial.
        perform we_zu_best.
        check g_scode is initial.
        perform scan_charge.

      when 'ZLEGM02'.     " Einlagern Wareneingang Fertigmaterial
        perform init_m02.
        perform scan_ferta.
        perform mhd. "Mat. mit Haltbarkeitsdaten ?
        check g_scode is initial.
        perform scan_mge_1b.
        check g_scode is initial.
        perform verbuchen_m02.

      when 'ZLEGM03'.    " Vom Rohmat. an Presse
        perform init_m03.
        perform scan_fertauf.
        check g_scode is initial.
        perform scan_masch.
        check g_scode is initial.
        perform scan_comp.
        check g_scode is initial.
        perform charge_bestimmen.
        check g_scode is initial.
        perform auftr_lagerpl.
        perform transportauftrag.

      when 'ZLEGM04'.    " Vom Presse an Rohmat
        perform init_m04.
        perform scan_fertauf_m04.
        check g_scode is initial.
        perform scan_masch_m04.
        check g_scode is initial.
        perform scan_comp_m04.
        check g_scode is initial.
        perform gew_best.
        check g_scode is initial..
        perform ta_m04.

      when 'ZLEGM05'.    " Lagerplatz WM An Rohmat

      when 'ZLEGM06'.    " LAgerplatz WM an Fertigung

      when 'EXIT'.
        exit.
      when others.
        clear s_sel.
    endcase.
    commit work.
  enddo.
