
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #102 #105 #115
************************************************************************

REPORT Z200_FORMROUTINEN .

*Form-Routinen, welche aus dem Formular
*aufgerufen werden können.


tables: vbak,                "Kundenauftragskopf
        vbkd,                "Kaufm. Daten
        vbap,                "Kundenauftragsposition
        vbfa,                "Belegfluss
        vbup,                "Status Position
        vbpa,                "Belegpartner
        tvlkt,               "Lieferungsarten
        ekbe,                "Bestellentwicklung
        marc,                "Werksdaten Material
        mvke,                "Verkaufsdaten zum Material
        tvgrt,               "Verkäufergruppentext
        vbrp,                "Fakturapositionen
        tvap,                "Positionstypen
        likp,                "Lieferkopf
        lips,                "Lieferpos.
        cabn,                "Merkmaldefinitionen
        t006b,               "Mengeneinheit Sprache
        t178t,               "Materialgruppe Text
        tvsbt,               "Versandbedingung Text
        kna1,                "Kundenstamm allg. Daten
        adrc,                "Adressdaten
        T005T,               "Ländertabellentext
        T173T,               "Versandartentext
        TINCT,               "Incotermtexte
        LTAP,                "TA Positionen
        ITOB,                "Equipment
        IFLO,                "Technischer Platz
        VBCO6.               "Schlüsselfelder Vertriebsbelegzugriffe


DATA: BEGIN OF t_vbfa OCCURS 0.
        INCLUDE STRUCTURE VBFA.
DATA: END OF t_vbfa.

data: begin of h,
        beleg_typ like vbuk-vbtyp,
      end of h.

************************************************************************
*Beginn: Zusatzdaten Header Transportauftragsdruck DGU
*SAP Stäfa, 23.06.2006, Peter Huber
************************************************************************

form READ_HEADER tables in_tab structure itcsy
                       out_tab structure itcsy.

  data: vbeln(10) type n,
        wkunnr(10),
        wlifnr(10),
        wvbeln(10),
        wvbelnl(10),
        werdat(10),
        wwadat(10),
        wlfdat(10).


  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'LTAK-VBELN'.
        VBELN  = in_tab-value.
        wvbelnl = in_tab-value.
    endcase.
  endloop.

*  while vbeln+9(1) = ' '.
*    shift vbeln right.
*    vbeln+0(1) = '0'.
*  endwhile.

  clear: vbfa.
  select single * from vbfa where vbeln = vbeln
                              and vbtyp_n = 'J'.
  if sy-subrc = 0.

    clear: vbak.
    select single * from vbak where vbeln = vbfa-vbelv.
    if sy-subrc = 0.

*     Kundennummer des Auftraggebers
      wkunnr = vbak-kunnr.
      while wkunnr+0(1) = '0'.
        shift wkunnr LEFT IN CHARACTER MODE .    "smart: 2010-08-02 #115
      endwhile.
      out_tab-name = 'WKUNAG_KUNNR'.
      out_tab-value = wkunnr.
      append out_tab.

*     Adresse Auftraggeber: Name1
      clear: kna1.
      select single * from kna1 where kunnr = vbak-kunnr.
      if sy-subrc = 0.

        out_tab-name = 'WKUNAG_NAME1'.
        out_tab-value = kna1-name1.
        append out_tab.

*      Adresse Auftraggeber: Name2
        out_tab-name = 'WKUNAG_NAME2'.
        out_tab-value = kna1-name2.
        append out_tab.

*      Sprachschlüssel des Auftraggebers
*      für Lesen Kopf- & Pos.texte
        out_tab-name = 'WKUNAG_SPRAS'.
        out_tab-value = kna1-spras.
        append out_tab.

*      Weitere AG Infos aus Adressverwaltung
        clear: adrc.
        select single * from adrc where addrnumber = kna1-adrnr.

*      Adresse Auftraggeber: stras
        out_tab-name = 'WKUNAG_STRAS'.
        out_tab-value = adrc-street.
        append out_tab.

*      Adresse Auftraggeber: ort01
        out_tab-name = 'WKUNAG_ORT01'.
        out_tab-value = adrc-mc_city1.
        append out_tab.

*      Adresse Auftraggeber: pstlz
        out_tab-name = 'WKUNAG_PSTLZ'.
        out_tab-value = adrc-post_code1.
        append out_tab.

*      Adresse Auftraggeber: Land1
        clear: t005t.
        select single * from t005t where land1 = adrc-country
                                     and spras = 'DE'.
        out_tab-name = 'WKUNAG_LANDX'.
        out_tab-value = t005t-landx.
        append out_tab.

*      Auftragsart Kundenauftrag
        out_tab-name = 'WVBAK_AUART'.
        out_tab-value = vbak-auart.
        append out_tab.

*      Auftragsnummer Kundenauftrag
        wvbeln = vbak-vbeln.
        while wvbeln+0(1) = '0'.
          shift wvbeln LEFT IN CHARACTER MODE .  "smart: 2010-08-02 #115
        endwhile.
        out_tab-name = 'WVBAK_VBELN'.
        out_tab-value = wvbeln.
        append out_tab.

*      Erstellungdatum Kundenauftrag
        clear: werdat.
        concatenate vbak-erdat+6(2) '.' vbak-erdat+4(2) '.'
                    vbak-erdat+0(4) into werdat  "smart: 2010-08-02 #101
                      IN CHARACTER MODE .        "smart: 2010-08-02 #101
        out_tab-name = 'WVBAK_ERDAT'.
        out_tab-value = werdat.
        append out_tab.

*      Kundenauftrag Erstellt durch
        out_tab-name = 'WVBAK_ERNAM'.
        out_tab-value = vbak-ernam.
        append out_tab.

      endif.
    endif.

  else.
*      Zusatzdaten für Lieferungsarten LB und NL
*      in Felder "Auftragsart" und "Erstellt durch" übergeben.
*      Antragsteller: Y.Zanin

    select single * from likp where vbeln = vbeln.

    if likp-lfart = 'LB' or likp-lfart = 'NL'.

      select single * from TVLKT where spras = 'D'
                                   and lfart = likp-lfart.

*     in Feld Auftragsart die Beschreibung der Lief.art
      out_tab-name = 'WVBAK_AUART'.
      out_tab-value = tvlkt-vtext.
      append out_tab.

      select single * from ekbe where belnr = vbeln.
*     in Feld Kundenauftrag die Bestellungsnummer
*     steht für LB nicht zur Verfügung...
      out_tab-name = 'WVBAK_ERNAM'.
      out_tab-value = ekbe-ebeln.
      append out_tab.

    endif.

  endif.


*     Warenempfänger
  clear: likp.
  select single * from likp where vbeln = vbeln.
*     Kundennummer des Warenempfänger
  wkunnr = likp-kunnr.
  while wkunnr+0(1) = '0'.
    shift wkunnr LEFT IN CHARACTER MODE .        "smart: 2010-08-02 #115
  endwhile.
  out_tab-name = 'WKUNWE_KUNNR'.
  out_tab-value = wkunnr.
  append out_tab.

  clear: vbpa.
  select single * from vbpa where vbeln = vbeln
                              and posnr = '000000'
                              and parvw = 'WE'.

  clear: adrc.
  select single * from adrc where addrnumber = vbpa-adrnr.
*      Adresse Warenempfänger: Name1
  out_tab-name = 'WKUNWE_NAME1'.
  out_tab-value = adrc-name1.
  append out_tab.

*      Adresse Warenempfänger: Name2
  out_tab-name = 'WKUNWE_NAME2'.
  out_tab-value = adrc-name2.
  append out_tab.

*      Adresse Warenempfänger: stras
  out_tab-name = 'WKUNWE_STRAS'.
  out_tab-value = adrc-street.
  append out_tab.

*      Adresse Warenempfänger: ort01
  out_tab-name = 'WKUNWE_ORT01'.
  out_tab-value = adrc-mc_city1.
  append out_tab.

*      Adresse Warenempfänger: pstlz
  out_tab-name = 'WKUNWE_PSTLZ'.
  out_tab-value = adrc-post_code1.
  append out_tab.

*      Adresse Warenempfänger: Land1
  clear: t005t.
  select single * from t005t where land1 = adrc-country
                               and spras = 'DE'.
  out_tab-name = 'WKUNWE_LANDX'.
  out_tab-value = t005t-landx.
  append out_tab.



*      Spediteur
  clear: vbpa.
  select single * from vbpa where vbeln = vbak-vbeln
                              and posnr = '000000'
                              and parvw = 'SP'.

*      Nummer Spediteur
  wlifnr = vbpa-lifnr.
  while wlifnr+0(1) = '0'.
    shift wlifnr LEFT IN CHARACTER MODE .        "smart: 2010-08-02 #115
  endwhile.
  out_tab-name = 'WKUNSP_LIFNR'.
  out_tab-value = wlifnr.
  append out_tab.


  clear: adrc.
  select single * from adrc where addrnumber = vbpa-adrnr.
*      Adresse Spediteur: Name1
  out_tab-name = 'WKUNSP_NAME1'.
  out_tab-value = adrc-name1.
  append out_tab.

*      Adresse Spediteur: Name2
  out_tab-name = 'WKUNSP_NAME2'.
  out_tab-value = adrc-name2.
  append out_tab.

*      Adresse Spediteur: stras
  out_tab-name = 'WKUNSP_STRAS'.
  out_tab-value = adrc-street.
  append out_tab.

*      Adresse Spediteur: ort01
  out_tab-name = 'WKUNSP_ORT01'.
  out_tab-value = adrc-mc_city1.
  append out_tab.

*      Adresse Spediteur: pstlz
  out_tab-name = 'WKUNSP_PSTLZ'.
  out_tab-value = adrc-post_code1.
  append out_tab.

*      Adresse Spediteur: Land1
  clear: t005t.
  select single * from t005t where land1 = adrc-country
                               and spras = 'DE'.
  out_tab-name = 'WKUNSP_LANDX'.
  out_tab-value = t005t-landx.
  append out_tab.



*      Lieferungsnummer (ohne führende Nullen)
  out_tab-name = 'WLIKP_VBELN'.
  out_tab-value = wvbelnl.
  append out_tab.

*      Erstellungdatum Lieferung

  clear: werdat.
  concatenate likp-erdat+6(2) '.' likp-erdat+4(2) '.'
              likp-erdat+0(4) into werdat IN     "smart: 2010-08-02 #101
                CHARACTER MODE .                 "smart: 2010-08-02 #101
  out_tab-name = 'WLIKP_ERDAT'.
  out_tab-value = werdat.
  append out_tab.

*      Lieferung Erstellt durch
  out_tab-name = 'WLIKP_ERNAM'.
  out_tab-value = likp-ernam.
  append out_tab.

*      Versandart der Lieferung
  out_tab-name = 'WLIKP_VSART'.
  out_tab-value = likp-vsart.
  append out_tab.

*      Versandartentext
  clear: t173t.
  select single * from t173t where vsart = likp-vsart
                               and spras = 'DE'.
  out_tab-name = 'WT173T_BEZEI'.
  out_tab-value = T173T-BEZEI.
  append out_tab.

*      Incoterm 1 der Lieferung (für Dätwyler "Versandbedingung")
  out_tab-name = 'WLIKP_INCO1'.
  out_tab-value = likp-inco1.
  append out_tab.

*      Incoterm 2 der Lieferung (für Dätwyler "Versandbedingung")
  out_tab-name = 'WLIKP_INCO2'.
  out_tab-value = likp-inco2.
  append out_tab.

*      Incotermtext 1
  clear: tinct.
  select single * from tinct where inco1 = likp-inco1
                               and spras = 'DE'.
  out_tab-name = 'WTINCT_BEZEI'.
  out_tab-value = TINCT-BEZEI.
  append out_tab.

*      Warenausgangsdatum
  clear: wwadat.
  concatenate likp-wadat+6(2) '.' likp-wadat+4(2) '.'
              likp-wadat+0(4) into wwadat IN     "smart: 2010-08-02 #101
                CHARACTER MODE .                 "smart: 2010-08-02 #101

  out_tab-name = 'WLIKP_WADAT'.
  out_tab-value = wwadat.
  append out_tab.

*      Lieferungsdatum
  clear: wlfdat.
  concatenate likp-lfdat+6(2) '.' likp-lfdat+4(2) '.'
              likp-lfdat+0(4) into wlfdat IN     "smart: 2010-08-02 #101
                CHARACTER MODE .                 "smart: 2010-08-02 #101

  out_tab-name = 'WLIKP_LFDAT'.
  out_tab-value = wlfdat.
  append out_tab.


*      endif.
*    endif.
*  endif.
endform.                    "READ_HEADER


************************************************************************
*Ende: Zusatzdaten Header Transportauftragsdruck DGU
************************************************************************

*Spache des Auftraggebers
form READ_WKUNAG_SPRAS tables in_tab structure itcsy
                             out_tab structure itcsy.

  data: vbeln like lips-vbeln.

  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'XVBLKP-VBELN'. vbeln = in_tab-value.
    endcase.
  endloop.

  while vbeln+9(1) = ' '.
    shift vbeln right IN CHARACTER MODE .        "smart: 2010-08-02 #115
    vbeln+0(1) = '0'.
  endwhile.

* Wegen schlechter Performance wird der Belegfluss ermittelt und nicht
* Direktzugriff auf VBFA
  perform belegfluss using vbeln.

  read table t_vbfa with key vbeln = vbeln.
  select single * from vbak where vbeln = t_vbfa-vbelv.
***  select single * from vbfa where vbeln = vbeln.
***  select single * from vbak where vbeln = vbfa-vbelv.
  select single * from kna1 where kunnr = vbak-kunnr.

  if sy-subrc ne 0.
    kna1-spras = 'DE'.
  endif.

  out_tab-name = 'WKUNAG_SPRAS'.
  out_tab-value = kna1-spras.

  append out_tab.
endform.                    "READ_WKUNAG_SPRAS

*Erste TA-Position zur Lieferungsposition ?
*Mit dieser Information wird die Druckausgabe im MAIN massgeblich
*beeinflusst, da sobald schon einen TA Position gedruckt wurde
*zur Lieferungsposition, der Ausgabeumgang reduziert wird.
*SAP Stäfa, Peter Huber, 23.06.2006
form READ_FIRST_POSNR tables in_tab structure itcsy
                             out_tab structure itcsy.

  data: vbeln like lips-vbeln,
        posnr like lips-posnr,
        lgnum like ltap-lgnum,
        tanum like ltap-tanum,
        tapos like ltap-tapos,
        wtapos like ltap-tapos.


  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'XVBLKP-VBELN'. vbeln = in_tab-value.
*      when 'XVBLKP-POSNR'. posnr = in_tab-value.
      when 'LTAP-LGNUM'.   lgnum = in_tab-value.
      when 'LTAP-TANUM'.   tanum = in_tab-value.
      when 'LTAP-TAPOS'.   tapos = in_tab-value.
      when 'LTAP-POSNR'.   posnr = in_tab-value.
    endcase.
  endloop.

  while vbeln+9(1) = ' '.
    shift vbeln right IN CHARACTER MODE .        "smart: 2010-08-02 #115
    vbeln+0(1) = '0'.
  endwhile.

*bestimmen Vorgänger TA-Positionsnr.
  if tapos gt 1.
    clear wtapos.
    wtapos = tapos - 1.
  endif.

  if tapos = 0001.
*TA-Posnr. = 0001 immer FIRST
    out_tab-name = 'FIRST_POSNR'.
    out_tab-value = 'F'.         "Frist
    append out_tab.

*sonst bestimmen, ob zur Lieferungspos. bereits eine TA-Posnr.
*gedruckt wurde

  else.

    select single * from ltap where lgnum = lgnum
                                and tanum = tanum
                                and tapos = wtapos.

    if ltap-posnr ne posnr.
      out_tab-name = 'FIRST_POSNR'.
      out_tab-value = 'F'.         "Frist
      append out_tab.
    else.
      out_tab-name = 'FIRST_POSNR'.
      out_tab-value = 'N'.         "Next
      append out_tab.
    endif.
  endif.
endform.                    "READ_FIRST_POSNR

*Workfeld TANUM TAPOS für Barcodedruck
form READ_TANUMPOS tables in_tab structure itcsy
                         out_tab structure itcsy.

  data: tanum like ltap-tanum,
        tapos like ltap-tapos,
        wtanumpos(14).

  refresh: out_tab.
  clear:   out_tab, WTANUMPOS.


  loop at in_tab.
    case in_tab-name.
      when 'LTAP-TANUM'.
        tanum = in_tab-value.
      when 'LTAP-TAPOS'.
        tapos = in_tab-value.
    endcase.
  endloop.


  out_tab-name = 'WTANUMPOS'.
  concatenate TANUM TAPOS into WTANUMPOS IN      "smart: 2010-08-02 #101
    CHARACTER MODE .                             "smart: 2010-08-02 #101
  out_tab-value = WTANUMPOS.
  append out_tab.
endform.                    "READ_TANUMPOS


*tdnamep der Position
form read_tdnamep tables in_tab structure itcsy
                             out_tab structure itcsy.

  data: vbeln like lips-vbeln,
        posnr like lips-posnr,
        tdnamep type char16.

  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'XVBLKP-VBELN'.
        vbeln = in_tab-value.
      when 'XVBLKP-POSNR'.
        posnr = in_tab-value.
    endcase.
  endloop.

  while vbeln+9(1) = ' '.
    shift vbeln right IN CHARACTER MODE .        "smart: 2010-08-02 #115
    vbeln+0(1) = '0'.
  endwhile.

  out_tab-name = 'TDNAMEP'.
  concatenate vbeln posnr into tdnamep IN        "smart: 2010-08-02 #101
    CHARACTER MODE .                             "smart: 2010-08-02 #101
  out_tab-value = tdnamep.
  append out_tab.
endform.                    "read_tdnamep

*tdnamek des Kopfes
form read_tdnamek tables in_tab structure itcsy
                             out_tab structure itcsy.

  data: vbeln like lips-vbeln,
        tdnamek type char16.

  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'XVBLKP-VBELN'.
        vbeln = in_tab-value.
    endcase.
  endloop.

  while vbeln+9(1) = ' '.
    shift vbeln right IN CHARACTER MODE .        "smart: 2010-08-02 #115
    vbeln+0(1) = '0'.
  endwhile.

  out_tab-name = 'TDNAMEK'.
  tdnamek = vbeln.
  out_tab-value = tdnamek.
  append out_tab.
endform.                    "read_tdnamek

*Anzahl Verpackungsstücke ermitteln
form read_wmenge tables in_tab structure itcsy
                             out_tab structure itcsy.

  data: matnr like lips-matnr,
        werks like lips-werks,
        lgmng like lips-lgmng,
        wmenge(20),
        h_value like itcsy-value.                "smart: 2010-08-02 #102

  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'XVBLKP-MATNR'.
        MATNR = in_tab-value.
      when 'XVBLKP-WERKS'.
        WERKS = in_tab-value.
      when 'XVBLKP-LGMNG'.
        replace ',' with '' into h_value.        "smart: 2010-08-02 #102
        condense h_value no-gaps.                "smart: 2010-08-02 #102
        LGMNG = h_value.                         "smart: 2010-08-02 #102
    endcase.
  endloop.


  while matnr+17(1) = ' '.
    shift matnr right IN CHARACTER MODE .        "smart: 2010-08-02 #115
    matnr+0(1) = '0'.
  endwhile.


  select single * from MARC where MATNR = MATNR
                              and werks = werks.

  if sy-subrc = 0.
    if not marc-bstrf is initial.
      wmenge = lgmng / marc-bstrf.
    else.
      wmenge = lgmng.
    endif.
    out_tab-name = 'WMENGE'.
    out_tab-value = WMENGE.
    append out_tab.

  endif.
endform.                    "read_wmenge



*Lesen Versandart-Bezeichnung
form read_vsart_bez tables  in_tab structure itcsy
                           out_tab structure itcsy.

  data: vsart like likp-vsart,
        spras like nast-spras.


  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'ILIKP-VSART'.
        vsart = in_tab-value.
    endcase.
  endloop.

  select single * from t173t  where vsart = vsart
                                and spras = 'DE'.

  if sy-subrc = 0.
    out_tab-name = 'IT173T-BEZEI'.
    out_tab-value =  t173t-bezei.
    append out_tab.
  endif.

endform.                    "read_vsart_bez



*Prüfkennzeichen Material ermitteln
form read_pruefkz tables  in_tab structure itcsy
                         out_tab structure itcsy.

  data: matnr like lips-matnr,
        vbeln like likp-vbeln.

  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'XVBLKP-MATNR'.
        matnr = in_tab-value.
      when 'LTAP-NLPLA'.
        vbeln = in_tab-value.
    endcase.
  endloop.

  while matnr+17(1) = ' '.
    shift matnr right IN CHARACTER MODE .        "smart: 2010-08-02 #115
    matnr+0(1) = '0'.
  endwhile.

  clear: likp.
  select single * from likp where vbeln = vbeln.

  clear: mvke.
  select single * from MVKE where matnr = matnr
                              and vkorg = likp-vkorg
                              and vtweg = '01'.

  if sy-subrc = 0.
    if mvke-prat1 ne space.
      out_tab-name = 'MVKE-PRAT1'.
      out_tab-value = 'X'.
    endif.
    append out_tab.
  endif.

endform.                    "read_pruefkz

*Lesen übergeordnetes Equipment für PM
form READ_WITOB_HEQUI tables  in_tab structure itcsy
                              out_tab structure itcsy.

  data: equnr(18) type n,
        tplnr(18),
        wkostl(10),
        whequi(18),
        ilart(3).

  data: l_hequi like equz-hequi.

  data: begin of iitob_e occurs 0.
          include structure itob.
  data: end of iitob_e.

    data: begin of iitob_h occurs 0.
          include structure itob.
  data: end of iitob_h.

  refresh: out_tab.
  clear:   out_tab.

  loop at in_tab.
    case in_tab-name.
      when 'RIWO1-EQUNR'.  EQUNR = in_tab-value.
      when 'RIWO1-TPLNR'.  TPLNR = in_tab-value.
      when 'CAUFVD-ILART'. ILART = in_tab-value.
    endcase.
  endloop.

  select * into table iitob_e from itob where equnr = equnr.
  clear: iitob_h.
  sort iitob_e descending by aedat. "nach Aenderungen letzter Org.eintrag
  loop at iitob_e.
    if sy-tabix = 1.
      exit.
    endif.
  endloop.

* Übergeordnetes Equipment aus Tabelle equz (Equipment Zeitsegment), damit
* beim Umhängen eines Equipments auch das neue erscheint
  clear l_hequi.
  select single hequi from equz into l_hequi where equnr = equnr and
                                                   datbi = '99991231'.

  clear: iitob_h.
  select * into table iitob_h from itob where equnr = l_hequi.

    sort iitob_h descending by aedat. "nach Aenderungen letzter Org.eintrag
  loop at iitob_h.
    if sy-tabix = 1.
      exit.
    endif.
  endloop.


  if sy-subrc = 0. "übergeordnetes Equipment
*--- Kostenstelle aus Equipment selbst andrucken, nicht vom übergeordneten Equipment.
*--- mit aktuellstem Record (gültig bis 31.12.9999) via EQUZ aus der ILOA holen.

    SELECT SINGLE KOSTL INTO wkostl
      FROM EQUZ INNER JOIN ILOA ON EQUZ~ILOAN = ILOA~ILOAN
      WHERE EQUZ~EQUNR = equnr
        AND EQUZ~DATBI = '99991231'.

    while wkostl+(1) = '0'.
      shift wkostl left IN CHARACTER MODE .      "smart: 2010-08-02 #115
    endwhile.

    whequi = iitob_h-equnr.
    while whequi+(1) = '0'.
      shift whequi left IN CHARACTER MODE .      "smart: 2010-08-02 #115
    endwhile.

    out_tab-name = 'WITOB_HEQUI'.
    out_tab-value =  whequi.
    append out_tab.

    out_tab-name = 'WITOB_KOSTL'.
    out_tab-value =  wkostl.
    append out_tab.

    out_tab-name = 'WITOB_INVNR'.
    out_tab-value =  iitob_h-invnr.
    append out_tab.

    out_tab-name = 'WITOB_SHTXT'.
    out_tab-value =  iitob_h-shtxt.
    append out_tab.
  else.  "Technischer Platz
    select single * from iflo where tplnr = tplnr.

*--- Kostenstelle aus Equipment selbst andrucken, nicht vom übergeordneten Equipment.
*--- mit aktuellstem Record (gültig bis 31.12.9999) via EQUZ aus der ILOA holen.

    SELECT SINGLE KOSTL INTO wkostl
      FROM EQUZ INNER JOIN ILOA ON EQUZ~ILOAN = ILOA~ILOAN
      WHERE EQUZ~EQUNR = equnr
        AND EQUZ~DATBI = '99991231'.

    if sy-subrc = 0.
*      wkostl = iitob_e-kostl.
      while wkostl+(1) = '0'.
        shift wkostl left IN CHARACTER MODE .    "smart: 2010-08-02 #115
      endwhile.

      whequi = iflo-tplnr.

      out_tab-name = 'WITOB_HEQUI'.
      out_tab-value =  whequi.
      append out_tab.

*--- Kostenstelle aus Equipment selbst andrucken, nicht vom technischen Platz.
      out_tab-name = 'WITOB_KOSTL'.
      out_tab-value =  wkostl.
      append out_tab.

      out_tab-name = 'WITOB_INVNR'.
      out_tab-value =  iflo-invnr.
      append out_tab.

      out_tab-name = 'WITOB_SHTXT'.
      out_tab-value =  iflo-pltxt.
      append out_tab.
    endif.
  endif.

* Instandhaltungsleistungsart Bezeichnung ermitteln
  out_tab-name = 'T353I_T-ILATX'.
  select single ilatx from t353i_t into out_tab-value
    where spras = sy-langu and
          ilart = ilart.

  if sy-subrc eq 0.
    append out_tab.
  endif.

* übergeordnetes Equipment für PM
  read table out_tab with key name = 'WITOB_HEQUI'.

* Lesen nächsthöhere Stufe für PM
  if sy-subrc eq 0.
    select single * from itob where equnr = iitob_h-hequi.
    if sy-subrc eq 0.
      whequi = iitob_h-hequi.
      while whequi+(1) = '0'.
        shift whequi left IN CHARACTER MODE .    "smart: 2010-08-02 #115
      endwhile.
      out_tab-name = 'WITOB_HEQUI_0'.
      out_tab-value =  whequi.
      append out_tab.
      out_tab-name = 'WITOB_SHTXT_0'.
      out_tab-value =  itob-shtxt.
      append out_tab.
    endif.
  endif.

endform.                    "READ_WITOB_HEQUI


*endform.
*


*&---------------------------------------------------------------------*
*&      Form  belegfluss
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

FORM belegfluss using p_vbeln TYPE LIPS-VBELN.   "smart: 2010-08-02 #105

  clear vbco6.
  vbco6-vbeln = p_vbeln.

  refresh t_vbfa.

  CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
    EXPORTING
      COMWA         = VBCO6
      BELEGTYP      = h-beleg_typ
    IMPORTING
      BELEGTYP_BACK = h-beleg_typ
    TABLES
      VBFA_TAB      = t_vbfa.


ENDFORM.                    " belegfluss
