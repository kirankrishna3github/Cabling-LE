*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_1_DATA                                        *
*----------------------------------------------------------------------*

tables:  lqua.
tables:  mkpf.
tables:  makt.
tables:  lagp.
tables:  ltak.


tables: itcpo.


data: t_lqua like table of lqua,
      w_lqua like lqua.

data:  gt_fieldcat type lvc_t_fcat.

data: scantitel(50) type c.
data: g_scanval(50) type c.
data:   g_scode(1) type c.
data: i_outtab type table of zle_good_alv_out.
data: w_outtab type zle_good_alv_out.
data: g_nalp type lgpla.
data: g_nlber type ltap_nlber.
data: wsy_subrc like sy-subrc.

* Workfelder
data: w_begda like prel-begda,    "Beginn Selektionszeitraum
      w_endda like prel-endda,    "Ende Selektionszeitraum
      w_seite type p.             "Seitenzähler
data: x_dialog(1).
* Hilfsfelder
data: h_text1(132).            "Hilfsfeld für Text


data: gt_outtab type table of zle_good_alv_out with header line.
data: gt_printab type table of zle_good_alv_out with header line.


data: ok_code like sy-ucomm,
      save_ok like sy-ucomm,
      g_bestkz_1(1) type c,
      g_bwlvs type bwlvs,
      g_bwlvs_2 type bwlvs,
      g_bewart(3) type c,
      g_bewart_2(3) type c,
      g_bestkz_2(1) type c,
      g_container type scrfname value 'BCALV_GRID_DEMO_0100_CONT1',
      grid1  type ref to cl_gui_alv_grid,
      g_custom_container type ref to cl_gui_custom_container,
      gs_layout type lvc_s_layo,
      g_max type i value 100.

data: t_imseg like table of imseg.
data: w_imseg like imseg.

*  Data zu Batchinput
data: begin of bdcdata occurs 0.
        include structure bdcdata.
data: end of bdcdata.


*** Feld-Symbole
field-symbols:
*** Feldkatalog
  <fcat>      type lvc_s_fcat,
*** Feld der Struktur
  <feld>      type any,
*** Arbeitsbereich der internen Tabelle
  <wa>        type any,
*** Die interne Tabelle mit Struktur aus GV_SNAME
  <itab>      type zle_good_alv_out.
*  OF zle_good_alv_out WITH HEADER LINE.



*----------------------------------------------------------------------*
*       CLASS lcl_events DEFINITION
*----------------------------------------------------------------------*
class lcl_events definition.

  public section.
    methods:
      handle_data_changed
            for event data_changed  of  cl_gui_alv_grid
            importing er_data_changed sender.

endclass.                    "lcl_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_rohr_eventhandler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_events implementation.

  method handle_data_changed.
******************************************
*** HandleDataChanged                  ***
******************************************
    data: ls_good                type lvc_s_modi.

*** alle Inhalte der geänderten Zellen in die interne Tabelle schreiben
    loop at er_data_changed->mt_good_cells into ls_good.
*** Dazu auf die richtige Zeile in der ITAB positionieren:
      read table gt_outtab assigning <wa> index ls_good-row_id.
      if sy-subrc = 0.
*** Und das geänderte Feld dem Feldsymbol zuweisen
        assign component ls_good-fieldname of structure <wa> to <feld>.
        if sy-subrc = 0.
*** Feldwert zuweisen
          <feld> = ls_good-value.
        endif.
      endif.
    endloop.
  endmethod.                    "handle_data_changed

endclass.                    "lcl_events IMPLEMENTATION

*** Variablen ALV
data:
  ref_alv_events type ref to lcl_events.
