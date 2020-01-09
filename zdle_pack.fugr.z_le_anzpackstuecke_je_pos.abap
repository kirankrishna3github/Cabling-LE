************************************************************************
* 2010-08-02   smartShift project

************************************************************************

FUNCTION Z_LE_ANZPACKSTUECKE_JE_POS.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(I_LANGU) LIKE  SY-LANGU
*"  TABLES
*"      T_DELIV STRUCTURE  SHIP_DELIV
*"      T_PACK STRUCTURE  ZSLE_PACK
*"----------------------------------------------------------------------

*data: wa_header like HUM_HU_HEADER_T.

  data: ls_vekp like vekp.
  data: ls_vepo like vepo.

  data: l_uecha like lips-uecha.
  data: l_maktx like makt-maktx.

  refresh t_pack.

* Handling Units zu Lieferungen ermitteln
  CALL FUNCTION 'SD_SHIPMENT_DELIVERY_HUS'
       IMPORTING
            ET_HEADER   = GT_VEKP  "Handling Unit Kopftabelle
            ET_ITEMS    = GT_VEPO  "Handling Unit Positiontabelle
       TABLES
            I_DELIV     = T_DELIV  "Liefernummern
       EXCEPTIONS
            HU_CHANGED  = 1
            FATAL_ERROR = 2
            OTHERS      = 3.
  .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* doppelte "interne Handling Unit Nummer" eliminieren, da 1 Packstück
* einer HU entspricht
  perform delete_doppelte_interne_hu.

* zugehörige Lieferpoitionen laden
  refresh gt_lips.

* Falls mehrere Liefernummern vorhanden
  loop at t_deliv.
    perform load_lieferpositionen using t_deliv-vbeln.
  endloop.


*$smart (F) 2010-08-02 - #109 Inkompatible Variablen in Ausdrücken (z.
*$smart (F) 2010-08-02 - #109 Bsp MOVE, COMPUTE, etc.) (M)

  loop at gt_vekp into ls_vekp.


*$smart (F) 2010-08-02 - #109 Inkompatible Variablen in Ausdrücken (z.
*$smart (F) 2010-08-02 - #109 Bsp MOVE, COMPUTE, etc.) (M)

    loop at gt_vepo into ls_vepo
      where venum = ls_vekp-venum.

* Packtabelle aufbereiten
      clear t_pack.

      move ls_vepo-vbeln to t_pack-vbeln.

* Chargensplitt
      if ls_vepo-posnr(1) = '9'.
        perform get_uecha using ls_vepo-vbeln
                                ls_vepo-posnr
                       changing l_uecha.
         move l_uecha to t_pack-posnr.
      else.
         move ls_vepo-posnr to t_pack-posnr.
      endif.

      move ls_vekp-vhilm to t_pack-vhilm.

      perform get_materialkurztext using ls_vekp-vhilm
                                         i_langu
                                changing t_pack-maktx.

      move 1 to t_pack-anzpk.

* Anzahl Packstücke zusammenziehen: (Je Lieferung, Pos, Material)
      collect t_pack.

    endloop.

  endloop.


ENDFUNCTION.
