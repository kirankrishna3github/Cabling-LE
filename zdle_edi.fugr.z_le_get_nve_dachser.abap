FUNCTION Z_LE_GET_NVE_DACHSER.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(IS_HU_HEADER) TYPE  VEKPVB
*"  EXPORTING
*"     REFERENCE(EV_NVE) TYPE  CHAR20
*"  TABLES
*"      T_VBPA STRUCTURE  VBPAVB
*"----------------------------------------------------------------------

  data: lv_lifnr type lifnr.
  data: lv_nve_fix type zznve.
  data: lv_number type char7.
  data: lv_objecttype type char10.
  data: lv_objectkey type swo_typeid.
  data: ls_vbpa type vbpavb.
  data: lt_vbpa type vsep_t_vbpa.

* Ermitteln NVE-Nummer für Dachser
  clear: ev_nve, gv_nve.

* Spediteur holen
  clear ls_vbpa.
  loop at t_vbpa into ls_vbpa
    where parvw = 'SP'.
  endloop.

  check not ls_vbpa is initial.

* Eintrag in Tabelle zedi_gln vorhanden?
  select single nve from zedi_gln into lv_nve_fix
    where lifnr = ls_vbpa-lifnr.

  check sy-subrc eq 0.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR                  = '01'
      OBJECT                       = 'ZNVE001'
   IMPORTING
     NUMBER                        = lv_number
   EXCEPTIONS
     INTERVAL_NOT_FOUND            = 1
     NUMBER_RANGE_NOT_INTERN       = 2
     OBJECT_NOT_FOUND              = 3
     QUANTITY_IS_0                 = 4
     QUANTITY_IS_NOT_1             = 5
     INTERVAL_OVERFLOW             = 6
     BUFFER_OVERFLOW               = 7
     OTHERS                        = 8.

  check sy-subrc eq 0.

* Prüfziffer ermitteln
  concatenate lv_nve_fix lv_number into gv_nve.

* Exportwert zurückgeben
  perform get_nve_mit_pruefziffer using gv_nve
                              changing ev_nve.

* Nummer in Tabelle zedi_lief_nve einfügen
***  move lv_objectkey to ls_zedi_lief_nve-vbeln.
***  move ev_nve       to ls_zedi_lief_nve-nve.
***
***  insert zedi_lief_nve from ls_zedi_lief_nve.
*  commit work and wait.

ENDFUNCTION.
