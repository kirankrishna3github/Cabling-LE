*----------------------------------------------------------------------*
***INCLUDE ZXTRKF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZUSATZSEGMENT_ZE1ADRE101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IDOC_DATA  text
*      -->P_SEGMENT_NAME  text
*      <--P_LV_APPEND  text
*----------------------------------------------------------------------*
FORM ZUSATZSEGMENT_ZE1ADRE101 TABLES PT_IDOC_DATA
                              USING  PS_SEGMENT_NAME TYPE EDILSEGTYP.

* Routine ist für Spediteur Dachser

  data: ls_idoc_data type edidd.
  data: lt_idoc_data type standard table of edidd.
  data: ls_e1adrm1 type e1adrm1.
  data: ls_e1edl20 type e1edl20.
  data: ls_ze1adre101 type ze1adre101.
  data: lv_gln type cif_gln.
  data: lv_eikto type eikto_k.
  data: lv_ind type i.
  data: lv_zzagtr type zzagtr.
  data: lv_bukrs type bukrs.
  data: lv_tabix type syst-tabix.

  lt_idoc_data[] = pt_idoc_data[].

  clear ls_idoc_data.
  lv_ind = LINES( lt_idoc_data ).

* letzte Zeile der Tabelle holen
  read table lt_idoc_data index lv_ind into ls_idoc_data.
  check ls_idoc_data-segnam = ps_segment_name.

  ls_e1adrm1 = ls_idoc_data-sdata.

  case ls_e1adrm1-partner_q.

* ---------------------------------------------------------------------
*   Spediteur
* ---------------------------------------------------------------------
    when 'SP'.

*   GLN-Nummer vorhanden
    clear lv_gln.

    select single gln from zedi_gln into lv_gln
      where lifnr = ls_e1adrm1-partner_id.

    check sy-subrc eq 0.

    clear: ls_idoc_data, lv_bukrs.

*   Verkaufsorganisation aus Kopfsegment E1EDL20 zur Ermittlung
*   des Buchungskreises
    read table lt_idoc_data into ls_idoc_data
       with key segnam = 'E1EDL20'.

    check sy-subrc eq 0.

*   Buchungskreis ermitteln
    ls_e1edl20 = ls_idoc_data-sdata.

    select single bukrs from tvko into lv_bukrs
      where vkorg = ls_e1edl20-vkorg.

    check sy-subrc eq 0.

*   Feld eikto (Unsere Kontonummer beim Kreditor)
    clear lv_eikto.

    select single eikto from zedi_eikto into lv_eikto
      where lifnr = ls_e1adrm1-partner_id and
            bukrs = lv_bukrs.

    check sy-subrc eq 0.

*   Auftragsgruppen für Transport (EDI) ermitteln
    perform get_auftragsgruppe_transport tables pt_idoc_data
                                         changing lv_zzagtr.

*   Neues Segment ZE1ADRE101 in Tabelle PT_IDOC_DATA hinzufügen
    clear: ls_idoc_data, ls_ze1adre101.
    move 'ZE1ADRE101'   to ls_idoc_data-segnam.
    move 'Z1'           to ls_ze1adre101-extend_q.
    move lv_gln         to ls_ze1adre101-zzgln.
    move lv_eikto       to ls_ze1adre101-zzeikto.
    move lv_zzagtr      to ls_ze1adre101-zzagtr.

    move ls_ze1adre101  to ls_idoc_data-sdata.

    append ls_idoc_data to pt_idoc_data.

* ---------------------------------------------------------------------
*   Warenempfänger
* ---------------------------------------------------------------------
***    when 'WE'.
***      read table lt_idoc_data into ls_idoc_data
***         with key segnam = 'ZE1ADRE101'.
***
***      check sy-subrc eq 0.
***
***      ls_ze1adre101 = ls_idoc_data-sdata.
***
***      lv_tabix = sy-tabix.
***
****     Auftragsgruppe (A,B, C) aus dem Land des Warenempfängers ermitteln:
****     A: Alles ausser Österreich (exkl. GB und Irland)
****     B: Nur Österreich
****     C: GB und Irland
***      clear lv_zzagtr.
***
***      if ls_e1adrm1-partner_q eq 'WE'.
***        case ls_e1adrm1-country1.
***          when 'AT'.
***            lv_zzagtr = 'B'.
***          when others.
***            lv_zzagtr = 'A'.
***        endcase.
***        endif.
***
***      move lv_zzagtr      to ls_ze1adre101-zzagtr.
***
***      move ls_ze1adre101  to ls_idoc_data-sdata.
***
***      modify pt_idoc_data from ls_idoc_data index lv_tabix.

  endcase.

ENDFORM.                    " ZUSATZSEGMENT_ZE1ADRE101


*----------------------------------------------------------------------*
***INCLUDE ZXTRKF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_AUFTRAGSGRUPPE_TRANSPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IDOC_DATA  text
*      -->P_SEGMENT_NAME  text
*      <--P_LV_APPEND  text
*----------------------------------------------------------------------*
FORM GET_AUFTRAGSGRUPPE_TRANSPORT TABLES PT_IDOC_DATA
                                  USING  PV_AGTR TYPE ZZAGTR.

  data: ls_idoc_data type edidd.
  data: lt_idoc_data type standard table of edidd.
  data: ls_e1adrm1 type e1adrm1.

  lt_idoc_data[] = pt_idoc_data[].

  clear pv_agtr.

  loop at lt_idoc_data into ls_idoc_data
    where segnam = 'E1ADRM1'.

    ls_e1adrm1 = ls_idoc_data-sdata.

    check ls_e1adrm1-partner_q = 'AG'.

    select single agtr from zedi_agtr into pv_agtr
      where land1 = ls_e1adrm1-country1.

    if sy-subrc ne 0.
      select single agtr from zedi_agtr into pv_agtr
        where land1 = space.
    endif.

    exit.

  endloop.

ENDFORM.                    " GET_AUFTRAGSGRUPPE_TRANSPORT
