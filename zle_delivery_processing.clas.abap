class ZLE_DELIVERY_PROCESSING definition
  public
  final
  create public .

*"* public components of class ZLE_DELIVERY_PROCESSING
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .

  data LV_DEF type XFELD .
  data LV_VSART type LIKP-VSART .
protected section.
*"* protected components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
private section.
*"* private components of class ZLE_DELIVERY_PROCESSING
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZLE_DELIVERY_PROCESSING IMPLEMENTATION.


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER .

  data: lv_x type xfeld,
        ls_lips type lips,
        lt_likp type STANDARD TABLE OF likp.

  CLEAR: lv_x.

*--- Beim Anlegen und nur am Anfang eine Vorbelegung der Versandart vornehmen.
*--- lv_def (Attribut der Instanz) wird nach der Vorbelegung gesetzt und erst beim Sichern der Auslieferung wieder gelöscht,
*--- damit die Versandart beliebig manuell überschrieben werden kann.
IF lv_def IS INITIAL AND
   ( IF_TCODE = 'VL01N' OR "Dialog
     IF_TCODE = 'VL04' ).  "Hintergrund

*--- Nur bei Lieferungen ab Deutschland automatisch eine Versandart ermitteln.
  IF cs_likp-vstel EQ 'S220' OR cs_likp-vstel EQ '2200' OR cs_likp-vstel EQ '3920'.

*--- Falls alle Positionen Stückmat. sind: lv_x='X' setzen
    LOOP AT it_xlips into ls_lips WHERE UPDKZ NE 'D'. "gelöschte Pos. nicht berücksichtigen

      IF ls_lips-MEINS EQ 'ST'.
        lv_x = 'X'.
      ELSE.
        clear lv_x.
        EXIT.
      ENDIF.

      lv_def = 'X'.

    ENDLOOP.
  ENDIF.
ENDIF.

*--- Wenn nur Stückmaterial vorhanden, die Versandart U1 setzen.
  IF lv_x = 'X'.

    CS_LIKP-vsart = 'U1'.

  ENDIF.

*--- Instanzattribut Versandart schreiben.

  lv_vsart = cs_likp-vsart.


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES .

* Example: Deactivate the function 'Copy picked quantity as delivery
* quantity'
  data: ls_cua_exclude type shp_cua_exclude.

  ls_cua_exclude-function = 'KOMU_T'.
  append ls_cua_exclude to ct_cua_exclude.

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES


method IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES .
* Start of Note 1882238
*  data: ls_field_attributes type shp_screen_attributes,
*        ls_xvbup            type vbupvb.
*
** Example 1: The field 'Actual goods-movement date' should not be
** changed by the user
*  ls_field_attributes-name  = 'LIKP-WADAT_IST'.
*  ls_field_attributes-input = 0.
*  append ls_field_attributes to ct_field_attributes.
*
** Example 2: The material description should not be changed for a
** certain group of materials after completion of the picking process
*  if is_lips-matnr cs 'ZZ'.
*    read table it_xvbup into ls_xvbup with key mandt = is_lips-mandt
*                                               vbeln = is_lips-vbeln
*                                               posnr = is_lips-posnr
*                        binary search.
*    if ls_xvbup-kosta eq 'C'.
*      ls_field_attributes-name  = 'LIKP-WADAT_IST'.
*      ls_field_attributes-input = 0.
*      append ls_field_attributes to ct_field_attributes.
*    endif.
*  endif.
* End of Note 1882238
endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES


method IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION .

* Example: Refuse deletion of an item if it contains a certain material


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION


method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION .

* Example: Delete delivery dependend data from the global memory of an
* own function group

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION


method IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK


method IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH


method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER


method IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM


method IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY .

* Example: Initialize the data in the global memory of an own
* function group

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY


method IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION .

* Example: Delete item dependend data from the global memory of an own
* function group


endmethod.                    "IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION


method IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM


method IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY .

endmethod.                    "IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.
endmethod.


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE .

  data: ls_vbpa type VBPAVB,
        ls_vbadr type SADRVB,
        ls_likp type LIKPVB,
        lv_adrnr type adrnr,
        lv_lifnr TYPE lifnr,
        lv_vstel type vstel.

  CLEAR lv_def. "Variable für Setzen der Default-Werte (1. Aufruf) wieder löschen.

  clear: lv_adrnr, ls_vbpa, ls_likp, lv_vstel.

  LOOP at ct_xlikp into ls_likp.
    lv_vstel = ls_likp-vstel.
  ENDLOOP.

* Default-Spediteur pro Versandstelle / Versandart ermitteln
  SELECT SINGLE LIFNR FROM ZLE_DEF_SPED
    INTO lv_lifnr
    WHERE vstel = ls_likp-vstel
      AND vsart = ls_likp-vsart.
***    WHERE vstel = lv_vstel   "die beiden Felder sind bei Hinterausführung leer
***      AND vsart = lv_vsart.

IF sy-subrc = 0 AND
  ( IF_TCODE = 'VL01N' OR  "Dialog
    IF_TCODE = 'VL04' ).   "Hintergrund

*--- Adressnummer zum neuen Partner holen.
  SELECT SINGLE adrnr FROM lfa1 INTO lv_adrnr
  WHERE lifnr = lv_lifnr.

  LOOP AT CT_XVBPA INTO ls_vbpa
   WHERE parvw = 'SP'.

*--- Originalzustand in YVBPA.
   MODIFY ct_yvbpa FROM ls_vbpa index sy-tabix.

*   ls_vbpa-updkz = 'D'.
   ls_vbpa-lifnr = lv_lifnr.
   ls_vbpa-adrnr = lv_adrnr.

   MODIFY ct_xvbpa FROM ls_vbpa index sy-tabix.

  ENDLOOP.

*--- Wenn noch kein Spediteur vorhanden ist.
  IF sy-subrc = 4.

   CLEAR: ls_vbpa.

*--- Als Vorlagepartner den Warenempfänger holen.
 LOOP AT CT_XVBPA INTO ls_vbpa
   WHERE parvw = 'WE'.
 ENDLOOP.

   clear ls_vbpa-kunnr.

   ls_vbpa-lifnr = lv_lifnr.
   ls_vbpa-adrnr = lv_adrnr.
   ls_vbpa-parvw = 'SP'.
   ls_vbpa-updkz = 'I'.

   IF NOT lv_adrnr IS INITIAL.
     APPEND ls_vbpa TO ct_xvbpa.
   ENDIF.

 ENDIF.

ENDIF.

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE
ENDCLASS.
