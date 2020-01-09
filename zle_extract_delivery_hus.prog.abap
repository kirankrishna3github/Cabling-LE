*&---------------------------------------------------------------------*
*& Report  ZLE_EXTRACT_DELIVERY_HUS
*&
*&---------------------------------------------------------------------*
*& Autor: Markus Raffeiner
*& Erstellt am: 16.09.2015
*&
*& Mit diesem Programm werden Handling-units mit den zugehörigen
*& Lieferungen extrahiert und im ALV ausgegeben.
*&
*&---------------------------------------------------------------------*
*& Änderungen
*&
*&---------------------------------------------------------------------*

REPORT ZLE_EXTRACT_DELIVERY_HUS.

tables: likp, mara, zsle_delihus.

select-options: s_vbeln for likp-vbeln.
select-options: s_werks for zsle_delihus-werks.
select-options: s_lfart for likp-lfart.
select-options: s_mtart for mara-mtart.
select-options: s_wadat for likp-wadat.

types: begin of t_likp,
         vbeln type vbeln_vl,
         lfart type lfart,
         lfdat type dats,
         kunnr type kunnr,
         wadat type wadat,
         werks type werks_d,
         vgbel type vgbel,
       end of t_likp.

data: gs_likp type t_likp.
data: gt_likp type standard table of t_likp.

types: begin of t_vekp,
        venum type venum,
        exidv type exidv,
        brgew type brgew_vekp,
        gewei type gewei,
        vhilm type vhilm,
       end of t_vekp.

types: begin of t_vepo,
         venum type venum,
         vemng type vemng,
         vemeh type vemeh,
         charg type charg_d,
         matnr type matnr,
       end of t_vepo.

data: gs_vekp type t_vekp.
data: gt_vekp type standard table of t_vekp.
data: gs_vepo type t_vepo.
data: gt_vepo type standard table of t_vepo.


data: gv_sel type c.
data: gv_werks type werks_d.
data: gv_key_num type key_num value 0.

* Ausgabetabelle
data: gt_out type standard table of zsle_delihus.
data: gs_out type zsle_delihus.

constants: gc_structname type tabname value 'ZSLE_DELIHUS'.
constants: gc_on type c length 1 value 'X'.
constants: gc_ok type c length 1 value 'X'.
constants: gc_not_ok type c length 1 value space.


* ---------------------------------------------------------------------
* Programmstart
* ---------------------------------------------------------------------
start-of-selection.

* Tabellenjoin likp-lips (Lieferkopf-Position) wegen Selektionen in Lips
  select likp~vbeln likp~lfart likp~lfdat likp~kunnr likp~wadat lips~werks lips~vgbel
     from likp inner join lips
     on likp~vbeln = lips~vbeln
     into table gt_likp
    where likp~vbeln in s_vbeln and
          likp~lfart in s_lfart and
          lips~werks in s_werks and
          likp~wadat in s_wadat.

* Duplikate löschen
  delete adjacent duplicates from gt_likp comparing vbeln.

  loop at gt_likp into gs_likp.

****    perform test_selektion using gv_sel.
****    check gv_sel eq gc_ok.

*   HU's ermitteln
    perform get_handling_units.

*   Ausgabe aufbereiten
    perform output_aufbereiten.

  endloop.

* ---------------------------------------------------------------------
* Programmende
* ---------------------------------------------------------------------
  end-of-selection.

* Ausgabe im ALV
  perform alvgrid_ausgabe.

*&---------------------------------------------------------------------*
*&      Form  GET_HANDLING_UNITS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_HANDLING_UNITS .

  clear: gt_vekp, gt_vepo.

* Handling Unit Kopftabelle
  select venum exidv brgew gewei vhilm from vekp into table gt_vekp
    where vpobjkey = gs_likp-vbeln.

* Verpacken: Handling Unit Position (Inhalt)
  if not gt_vekp[] is initial.  "damit nicht alles gelesen wird
    select venum vemng vemeh charg matnr from vepo into table gt_vepo
      for all entries in gt_vekp
      where venum = gt_vekp-venum.
  endif.

* Sortierung
  sort gt_vekp by vhilm exidv.

ENDFORM.                    " GET_HANDLING_UNITS_NEW

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_AUFBEREITEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTPUT_AUFBEREITEN .

  data: ls_vekp type t_vekp.
  data: ls_vepo type t_vepo.
  data: begin of ls_mara,
          matnr type matnr,
          mtart type mtart,
          prdha type prodh_d,
        end of ls_mara.

  data: ls_t179t type t179t.

  data: lv_vgbel type vbeln_vl.
  data: lv_first_item type c.

* ---------------------------------------------------------------------
* HUs (Kopf)
* ---------------------------------------------------------------------
  loop at gt_vekp into ls_vekp.

*   Gewicht pro HU nur einmal ausgeben
    lv_first_item = gc_on.
* ---------------------------------------------------------------------
* HUs (Positionen)
* ---------------------------------------------------------------------
    loop at gt_vepo into ls_vepo
      where venum = ls_vekp-venum.

      clear gs_out.

*     Materialdaten
      clear ls_mara.

      if ls_mara-matnr ne ls_vepo-matnr.
        clear ls_mara.
        select single matnr mtart prdha from mara into ls_mara
          where matnr = ls_vepo-matnr.
      endif.

*     Test Materialart
      check ls_mara-mtart in s_mtart.

*     Terminauftrag
      move gs_likp-vgbel to gs_out-vbeln_va.

*     Liefernummer
      move gs_likp-vbeln to gs_out-vbeln_vl.

*     Lieferart
      move gs_likp-lfart to gs_out-lfart.

*     Werk
      move gs_likp-werks to gs_out-werks.

*     Warenausgangsdatum
      move gs_likp-wadat to gs_out-wadat.

*     Kundennummer
      move gs_likp-kunnr to gs_out-kunnr.

*     Materialnummer
      move ls_vepo-matnr to gs_out-matnr.

*     Produkthierarchie
      move ls_mara-prdha(1) to gs_out-prodh.

*     Produkthierarchie (Bezeichnung)
      if ls_t179t-prodh(1) ne ls_mara-prdha(1).
        clear ls_t179t.
        select single * from t179t into ls_t179t
          where spras = sy-langu and
                prodh = ls_mara-prdha(1).
      endif.

      move ls_t179t-vtext to gs_out-vtext.

*     Materialart
      move ls_mara-mtart to gs_out-mtart.

*     Verpackte Basismenge
      move ls_vepo-vemng to gs_out-vemng.

*     Verpackte Basismengeneinheit
      move ls_vepo-vemeh to gs_out-vemeh.

*     Rollennummer
      if not ls_vepo-charg is initial.
        select single zzrollnr from mch1 into gs_out-zzrollnr
          where matnr = ls_vepo-matnr and
                charg = ls_vepo-charg.
      endif.

*     Externe Handling Unit Identifikation
      move ls_vekp-exidv to gs_out-exidv.

*     Packmittel
      move ls_vekp-vhilm to gs_out-vhilm.

*    Bezeichnung von HU ermitteln
     select single maktx from makt into gs_out-maktx
       where matnr = ls_vekp-vhilm and
             spras = sy-langu.

*    Gewicht pro HU nur einmal ausgeben
     if lv_first_item = gc_on.
       clear lv_first_item.
       move ls_vekp-brgew to gs_out-brgew.
       move ls_vekp-gewei to gs_out-gewei.
     endif.

*    Element in ALV-Tabelle einfügen
     append gs_out to gt_out.

    endloop.  "gt_vepo

  endloop.  "gt_vekp

ENDFORM.                    " OUTPUT_AUFBEREITEN

*&---------------------------------------------------------------------*
*&      Form  alvgrid_ausgabe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alvgrid_ausgabe.

  data: ls_layout_lvc type LVC_S_LAYO.

  clear ls_layout_lvc.

* Optimale Spaltenbreite setzen
  move gc_on to ls_layout_lvc-cwidth_opt.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_structure_name = gc_structname
      IS_LAYOUT_LVC    = ls_layout_lvc
    TABLES
      t_outtab         = gt_out
    EXCEPTIONS
      OTHERS           = 2.

ENDFORM.                    "alvgrid_ausgabe

*&---------------------------------------------------------------------*
*&      Form  TEST_SELEKTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_SEL_OK  text
*----------------------------------------------------------------------*
FORM TEST_SELEKTION  USING    P_SEL type c.

  move gc_not_ok to p_sel.

* Werk wird im Tabellenjoin lik-lips berücksichtigt

**** Test Werkselektion
***  clear gv_werks.
***
***  select single werks from lips into gv_werks
***    where vbeln = gs_likp-vbeln and
***          werks in s_werks.
***
***  check sy-subrc eq 0.
***
***  check gv_werks in s_werks.

* Record weiterverarbeiten
  move gc_ok to p_sel.

ENDFORM.                    " TEST_SELEKTION
