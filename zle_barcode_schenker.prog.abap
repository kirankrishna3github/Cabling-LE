*----------------------------------------------------------------------*
* Report  ZLE_BARCODE_SCHENKER
*
*----------------------------------------------------------------------*
*     Programm-Name....: ZLE_BARCODE_SCHENKER
*     Entwickler.......: Markus Raffeiner
*     Erstell-Datum....: 23.11.2010
*     Version..........: 1.0
*     Zweck............: Barcode-Label und Routenermittlung für Spediteur
*                        Schenker. Dieses Programm wird vom Progeamm
*                        ZM_ETIKDRUCK02 angesprungen.
*
*     Bemerkung........: -Die Umsetzung erfolgte gemäss Dokumentation von
*                         Schenker, welche in einem externen Worddokument
*                         verfasst ist (Version 3.0)
*                        -Das Sonderkennzeichen Termin (Doku 5.1) kann in
*                         der Tabelle ztskte_schenker gepflegt werden
*
*     Aenderungen:
*     03.02.2011, Markus Raffeiner: Ermitteln der Mistralnummer aus dem
*     Lieferkopf. Diese wird im Include MV50AFZ1 ermittelt
*     01.12.2011, Markus Raffeiner: Das Feld likp-berot wird neu im
*     Include MV50AFZ1 aufbereitet
*     04.01.2012, Markus Raffeiner: Ermitteln der Mistralnummer beim
*     Etikettendruck. Diese wird in die Tabelle ztlief_schenker
*     geschrieben und für das IDOc an Schenker zur Verfügung gestellt
*     Das Feld likp-berot wird wieder in diesem Programm aufbereitet
*
*----------------------------------------------------------------------*

REPORT  ZLE_BARCODE_SCHENKER.

tables: ztlief_schenker, ztmihu_schenker, ztplz_schenker, ztrout_schenker.

include zincl_etikette_002.

* globale Variablen
data: g_likp like likp.
data: g_vekp like vekp.
data: g_agestl like ztrout_schenker-zzgestl.
data: g_plz like kna1-pstlz.
data: g_land like kna1-land1.

data: h_gestl like ztrout_schenker-zzgestl.
data: h_gestl_42 like ztrout_schenker-zzgestl.
data: h_pstlz like ztrout_schenker-zzpstlz.
data: h_relzu like ztrout_schenker-zzrelzu.
data: h_zugriff_next(1).
data: h_sw_plz_vorhanden(1).


*----------------------------------------------------------------------*
* Schenker-Routinginformationen: Felder: 1a, 1b, 1c, 2a, 2b, 2c, 3a, 3b
* (Doku 4.3)
*----------------------------------------------------------------------*
data: begin of rout,
*       2. Empfangs-GSt. (Klartext)
        feld1a(3),
*       1. Empfangs-GSt. (Klartext)
        feld1b(2),
*       Naverkehrstour bzw. Sondertour (Klartext)
        feld1c(3),
*       (Stelle 9 - 10) 1. Empfangs-GSt.
        feld2a(2),
*       (Stelle 11 - 12) 2. Empfangs-GSt.
        feld2b(2),
*       (Stelle 13 - 15) Nahverkehrstour bzw. Sondertour
        feld2c(3),
*       HUB-Relation in Klammern (Klartext)
        feld3a(4),
*       HUB-Kette in Klammern (Klartext)
        feld3b(4),
      end of rout.

* Barcode 26-Stellig
*data: ze001-barc(26).
*data: ze001-barc_kt1(2).
*data: ze001-barc_kt2(10).
*data: ze001-barc_kt3(3).

* Felder im Klartext für Barcode
data: begin of bc_ktext,
*       Feld1a
        f1(2),
*       Feld3a, Feld3b, Feld1b
        f2(10),
*       Feld1c
        f3(3),
      end of bc_ktext.

*----------------------------------------------------------------------*
* Strichcode
*----------------------------------------------------------------------*
data: begin of h_barc,
*       Stelle 1-2:   MISTRAL Geschäftsstellen Nummer
        migestl(2),
*       Stelle 3-8:   MISTRAL-Auftragsnummer
        miaufnr(6),
*       Stelle 9-10:  1. Empfangs-GSt.
        feld2a(2),
*       Stelle 11-12: 2. Empfangs-GSt.
        feld2b(2),
*       Stelle 13-15: Zustelltour
        feld2c(3),
*       Stelle 16:    Gefahrgut
        gefgut(1),
*       Stelle 17:    Speed
        speed(1),
*       Stelle 18:    Termin
        term(1),
*       Stelle 19:    Tag bei Termin
        tagte(2),
*       Stelle 21-23: Gesamtanzahl Colli
        gesco(3),
*       Stelle 24-26: Colli x von y/Packstückzähler
        packz(3),
      end of h_barc.

* Felder für MISTRAL-Auftragsnummer
data: h_number(6) type n.
data: h_rangenr like inri-nrrangenr.


* Konstanten
constants: c_on value 'X'.


*&---------------------------------------------------------------------*
*&      Form  SCHENKERDATEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCHENKERDATEN using p_vekp structure vekp
                         p_agestl
                         p_plz
                         p_land
                changing ze001 structure ze001.


* Variablen initialisieren
  move p_vekp   to g_vekp.
  move p_agestl to g_agestl.
  move p_plz    to g_plz.
  move p_land   to g_land.

* Struktur Routinginformationen initialisieren
  clear rout.

* Lieferung via vep-vpobjkey lesen
  clear g_likp.
  select single * from likp into g_likp
    where vbeln = g_vekp-vpobjkey.

* Schenkerdaten initialisieren
  clear: ze001-barc_s, ze001-ktext1_s, ze001-ktext2_s, ze001-ktext3_s,
         ze001-ktterm_s, ze001-gsmi_s.
  clear: h_barc.


*----------------------------------------------------------------------*
* Fortlaufende MISTRAL-Auftragsnummer
*----------------------------------------------------------------------*
  perform mistral_auftragsnummer changing h_barc-miaufnr.

*----------------------------------------------------------------------*
* Für die IDoc-Übergabe wird das Feld likp-berot (Bereitstellungsort)
* verwendet, damit die Mistralnummer an Schenker mit EDI übergeben
* werden kann. Tabelle likp wird aktualisiert
*----------------------------------------------------------------------*
  if g_likp-berot is initial.
    g_likp-berot = h_barc-miaufnr.
    update likp from g_likp.
  endif.

*----------------------------------------------------------------------*
* Auftragsanlegende Geschäftsstelle
*----------------------------------------------------------------------*
  move p_agestl to h_barc-migestl.

*----------------------------------------------------------------------*
* Prüfen, ob Empfangs-PLZ vorhanden (Doku 4.1)
*----------------------------------------------------------------------*
  perform test_empfangs_plz_vorhanden using g_plz(5)
                                   changing h_sw_plz_vorhanden.

*----------------------------------------------------------------------*
* Falls Empfangs-PLZ vorhanden (h_sw_plz_vorhanden = c_on
* Routing: Inland- oder Exportauftrag (Doku 4.3)
*----------------------------------------------------------------------*
  if h_sw_plz_vorhanden = c_on.
    case p_land.
      when 'DE'.
*       Routingermittlung für Inlandaufträge (Doku 4.3)
        perform routing_inlandauftrag.
      when others.
*       Routingermittlung für Exportaufträge (Doku 4.5)
        perform routing_exportauftrag.
      endcase.
  endif.

  move rout-feld2a to h_barc-feld2a.
  move rout-feld2b to h_barc-feld2b.
  move rout-feld2c to h_barc-feld2c.

*----------------------------------------------------------------------*
* Kennzeichen Gefahrengut, Speed, Termin (Doku 5.1)
*----------------------------------------------------------------------*
  perform gefahrengut_speed_termin changing h_barc-gefgut
                                            h_barc-speed
                                            h_barc-term.

*----------------------------------------------------------------------*
* Tag bei Termin (Doku 5.2)
*----------------------------------------------------------------------*
  perform tag_bei_termin  using h_barc-term
                          changing h_barc-tagte.

*----------------------------------------------------------------------*
* Klartext für Termin: FIX-, AB-, BIS-TERMIN + Tag
*----------------------------------------------------------------------*
  perform klartext_termin using h_barc-term
                                h_barc-tagte
                       changing ze001-ktterm_s.

*----------------------------------------------------------------------*
* Gesamtzahl Colli (Doku 5.3)
*----------------------------------------------------------------------*
  perform gesamtzahl_colli changing h_barc-gesco.

*----------------------------------------------------------------------*
* Colli x von y/Packstückzähler (Doku 5.4)
*----------------------------------------------------------------------*
  perform packstueckzaehler changing h_barc-packz.

* Barcode und Variablen für Klartextfelder
  move h_barc to ze001-barc_s.

* Klartextfelder
  move rout-feld1a to ze001-ktext1_s.
  concatenate rout-feld3a rout-feld3b rout-feld1b into ze001-ktext2_s.
  move rout-feld1c to ze001-ktext3_s.

*----------------------------------------------------------------------*
*  Auftragsanlegende Geschäftsstelle, '/', MISTRAL-Auftragsnummer
*----------------------------------------------------------------------*
  concatenate p_agestl '/' h_barc-miaufnr into ze001-gsmi_s
  separated by space.


ENDFORM.                    " SCHENKERDATEN


*&---------------------------------------------------------------------*
*&      Form  MISTRAL_AUFTRAGSNUMMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MISTRAL_AUFTRAGSNUMMER changing p_miaufnr.

  data: l_zzmistralnr like ztlief_Schenker-zzmistralnr.

  data: l_number(6) type n.
  data: l_rangenr like inri-nrrangenr.

  clear: p_miaufnr, l_zzmistralnr.

* Mistralnummer aus Tabelle ztlief_schenker holen
  select single zzmistralnr from ztlief_schenker into l_zzmistralnr
    where vbeln = g_likp-vbeln.

* Falls die Lieferung mit der Mistralnummer in der Tabelle
* ztlief_schenker vorhanden ist, wird diese zurückgegeben.
  if sy-subrc eq 0.
    p_miaufnr = l_zzmistralnr.
    exit.  "Routine verlassen
  endif.


* Neue Mistralnummer lösen und in Tabelle ztlief_schenker einfügen
  clear l_number.
  l_rangenr = '01'.

* Nummer aus dem internen Nummernkreis ziehen
  call function 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = l_rangenr
      object      = 'Z_MISTRAL'
      quantity    = '1'
    IMPORTING
      number      = l_number.

  if sy-subrc eq 0.

*   Tabelle ztlief_schenker aufbereiten und neuen Satz einfügen
    clear ztlief_schenker.
    ztlief_schenker-vbeln = g_likp-vbeln.
    ztlief_schenker-zzmistralnr = l_number.
    insert ztlief_schenker.

*   MISTRAL-Auftragsnummer zurückgeben
    p_miaufnr = l_number.

  endif.  "sy-subrc eq 0


ENDFORM.                    " MISTRAL_AUFTRAGSNUMMER


*&---------------------------------------------------------------------*
*&      Form  TEST_EMPFANGS_PLZ_VORHANDEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_PLZ(5)  text
*      <--P_H_SW_PLZ_VORHANDEN  text
*----------------------------------------------------------------------*
FORM TEST_EMPFANGS_PLZ_VORHANDEN  USING    p_plz
                                  CHANGING p_sw_vorhanden.

  clear p_sw_vorhanden.

*----------------------------------------------------------------------*
* Test ob PLZ in Tabelle ztplz_schenker vorhanden
* Falls nicht vorhanden, Felder 1c, 2a, 2b, 2c initialisieren und
* Routingermittlung ist damit beendet (Doku 4.1)
*----------------------------------------------------------------------*
  select single * from ztplz_schenker
    where zzpstlz = g_plz(5).

  if sy-subrc ne 0. "PLZ in Tab. ztplz_schenker nicht vorhanden (Doku 4.1)
    move '950' to rout-feld1c.
    move '00'  to rout-feld2a.
    move '00'  to rout-feld2b.
    move '950' to rout-feld2c.
    p_sw_vorhanden = c_on.
  endif.

  p_sw_vorhanden = c_on.


ENDFORM.                    " TEST_EMPFANGS_PLZ_VORHANDEN


*&---------------------------------------------------------------------*
*&      Form  ROUTING_INLANDAUFTRAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ROUTING_INLANDAUFTRAG.

  data: l_gestl like ztrout_schenker-zzgestl.


*----------------------------------------------------------------------*
* Prüfen der Versand-GSt. und Empfangs-PLZ in Routingdatei (Doku 4.2)
* Eingabe ist die auftragsgebende Geschäftsstelle (g_agestl, g_plz)
*
* 1. Zugriff auf die Routingdatei
*----------------------------------------------------------------------*
  perform routing_ermitteln using  g_agestl
                                   g_plz
                         changing  h_gestl
                                   h_relzu.

* Wert für HUB-Zugehörigkeit merken
  move h_relzu+1(2) to h_gestl_42.

*----------------------------------------------------------------------*
* 1. Zugriff auf die Routingdatei verarbeiten (Doku 4.3.1)
*----------------------------------------------------------------------*
  perform zugriff1_verarbeiten using h_relzu
                            changing h_zugriff_next.

  if h_zugriff_next ne c_on.  "Kein 2. Zugriff
*   HUB-Zugehörigkeit (Doku 4.4.1)
    perform hub_zugehoerigkeit using h_gestl_42.
    exit. "Routing beendet
  endif.

*----------------------------------------------------------------------*
* 2. Zugriff auf die Routingdatei (Doku 4.3.2)
*----------------------------------------------------------------------*
  move h_relzu+1(2) to l_gestl.

  perform routing_ermitteln using  l_gestl
                                   g_plz
                         changing  h_gestl
                                   h_relzu.

*----------------------------------------------------------------------*
* 2. Zugriff auf die Routingdatei verarbeiten (Doku 4.3.2)
*----------------------------------------------------------------------*
  perform zugriff2_verarbeiten using h_relzu
                            changing h_zugriff_next.

  if h_zugriff_next ne c_on. "Kein 3. Zugriff
    exit. "Routing beendet
  endif.

*----------------------------------------------------------------------*
* 3. Zugriff auf die Routingdatei (Doku 4.3.3)
*----------------------------------------------------------------------*
  move h_relzu+1(2) to l_gestl.

  perform routing_ermitteln using  l_gestl
                                   g_plz
                         changing  h_gestl
                                   h_relzu.

*----------------------------------------------------------------------*
* 3. Zugriff auf die Routingdatei verarbeiten (Doku 4.3.3)
*----------------------------------------------------------------------*
  perform zugriff3_verarbeiten using h_relzu.


ENDFORM.                    " ROUTING_INLANDAUFTRAG


*&---------------------------------------------------------------------*
*&      Form  ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ROUTING_EXPORTAUFTRAG.


* Struktur Routinginformationen initialisieren
  clear rout.

  move '386' to rout-feld1a.
  move '386' to rout-feld1c.
  move '00'  to rout-feld2a.
  move '00'  to rout-feld2b.
  move '386' to rout-feld2c.

ENDFORM.                    " ROUTING_EXPORTAUFTRAG


*&---------------------------------------------------------------------*
*&      Form  ROUTING_ERMITTELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_GESTL  text
*      -->P_P_PLZ  text
*----------------------------------------------------------------------*
FORM ROUTING_ERMITTELN  USING    P_GESTL
                                 P_PLZ
                      changing   h_gestl
                                 h_relzu.


  data: begin of l_it occurs 0.
          include structure ztrout_schenker.
  data: end of l_it.


  data: l_anz type i.

*----------------------------------------------------------------------*
* Ermitteln der PLZ via Geschäftsstelle und p_plz
* Falls die PLZ nicht gefunden wird, wird die nächstkleinere PLZ zur
* Eingabe-PLZ (p_plz) gesucht
*----------------------------------------------------------------------*
  clear: h_gestl, h_pstlz.
  clear ztrout_schenker.

  select * from ztrout_schenker into table l_it
    where zzgestl = p_gestl and
          zzpstlz le p_plz.


  check sy-subrc eq 0.

* letztes Element holen: PLZ, welche kleiner oder gleich der Input-PLZ ist
  describe table l_it lines l_anz.
  read table l_it index l_anz.

  move l_it-zzgestl to h_gestl.
  move l_it-zzrelzu to h_relzu.


ENDFORM.                    " ROUTING_ERMITTELN


*&---------------------------------------------------------------------*
*&      Form  ZUGRIFF1_VERARBEITEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_H_ZUGRIFF_NEXT  text
*----------------------------------------------------------------------*
FORM ZUGRIFF1_VERARBEITEN USING P_RELZU
                       CHANGING P_ZUGRIFF_NEXT.

  data: l_relzu  like ztrout_schenker-zzrelzu.


* Returnvariable initialisieren
  clear: p_zugriff_next.

  move p_relzu to l_relzu.

*----------------------------------------------------------------------*
* 1. Zugriff auf die Routingdatei (Doku 4.3.1)
**  perform routing_ermitteln using p_gestl
**                                  g_plz
**                         changing h_gestl
**                                  h_pstlz
**                                  l_relzu.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 1. Fall Sonderrelation:  99 < h_relzu < 900
* - Felder initialisieren, Routingermittlung ist beendet
  if l_relzu gt 99 and l_relzu lt 900.
    move l_relzu to rout-feld2c.
    move l_relzu to rout-feld1a.
    move l_relzu to rout-feld1c.
    move '00' to rout-feld2a.
    move '00' to rout-feld2b.
    exit.
  endif.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 2. Fall:  h_relzu > 899
* - Felder initialisieren, Routingermittlung ist beendet
  if l_relzu gt 899.
    move l_relzu to rout-feld2c.
    move l_relzu to rout-feld1c.
    move '00' to rout-feld2a.
    move '00' to rout-feld2b.
    exit.
  endif.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 3. Fall:  h_relzu < 100
* - Felder initialisieren, 2. Zugriff auf die Routingdatei
  move l_relzu+1(2) to rout-feld2b.
  move l_relzu+1(2) to rout-feld1a.


* Variable für 2. Zugriff setzen
  p_zugriff_next = c_on.
*----------------------------------------------------------------------*


ENDFORM.                    " ZUGRIFF1_VERARBEITEN


*&---------------------------------------------------------------------*
*&      Form  ZUGRIFF2_AUF_ROUTINGDATEI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_H_ZUGRIFF_NEXT  text
*----------------------------------------------------------------------*
FORM ZUGRIFF2_VERARBEITEN USING P_RELZU
                       CHANGING P_ZUGRIFF_NEXT.

  data: l_relzu  like ztrout_schenker-zzrelzu.

* Returnvariable initialisieren
  clear: p_zugriff_next.

  move p_relzu to l_relzu.


**----------------------------------------------------------------------*
** 2. Zugriff auf die Routingdatei (Doku 4.3.2)
*  perform routing_ermitteln using  p_gestl
*                                   g_plz
*                         changing  h_gestl
*                                   h_pstlz
*                                   l_relzu.
**----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 1. Fall Sonderrelation:  99 < h_relzu < 900
* - Felder initialisieren, Routingermittlung ist beendet
  if l_relzu gt 99 and l_relzu lt 900.
    move l_relzu to rout-feld2c.
    move l_relzu to rout-feld1c.

*   HUB-Zugehörigkeit (Doku 4.4.1)
    perform hub_zugehoerigkeit using h_gestl.

    exit.
  endif.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 2. Fall:  h_relzu > 899
* - Felder initialisieren, Routingermittlung ist beendet
  if l_relzu gt 899.
    move l_relzu to rout-feld2c.
    move l_relzu to rout-feld1c.

*   HUB-Zugehörigkeit (Doku 4.4.1)
    perform hub_zugehoerigkeit using h_gestl.

    exit.
  endif.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 3. Fall:  h_relzu < 100
* - Felder initialisieren, 2. Zugriff auf die Routingdatei
  move l_relzu+1(2) to rout-feld2a.
  move l_relzu+1(2) to rout-feld1b.

* Variable für 3. Zugriff setzen
  p_zugriff_next = c_on.
*----------------------------------------------------------------------*


ENDFORM.                    " ZUGRIFF2_AUF_ROUTINGDATEI


*&---------------------------------------------------------------------*
*&      Form  ZUGRIFF3_VERARBEITEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_H_ZUGRIFF_NEXT  text
*----------------------------------------------------------------------*
FORM ZUGRIFF3_VERARBEITEN USING P_RELZU.

  data: l_relzu  like ztrout_schenker-zzrelzu.


  move p_relzu to l_relzu.


**----------------------------------------------------------------------*
** 3. Zugriff auf die Routingdatei (Doku 4.3.3)
*  perform routing_ermitteln using  p_gestl
*                                   g_plz
*                         changing  h_gestl
*                                   h_pstlz
*                                   l_relzu.
**----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 1. Fall Sonderrelation:  99 < h_relzu < 900
* - Felder initialisieren, Routingermittlung ist beendet
  if l_relzu gt 99 and h_relzu lt 900.
    move l_relzu to rout-feld2c.
    move l_relzu to rout-feld1c.
    exit.
  endif.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 2. Fall:  h_relzu > 899
* - Felder initialisieren, Routingermittlung ist beendet
  if l_relzu gt 899.
    move l_relzu to rout-feld2c.
    move l_relzu to rout-feld1c.
    exit.
  endif.
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* 3. Fall:  h_relzu < 100
* - Felder initialisieren, 2. Zugriff auf die Routingdatei
  move '00'  to rout-feld2a.
  move '00'  to rout-feld2b.
  move '950' to rout-feld1c.
  move '950' to rout-feld2c.
*----------------------------------------------------------------------*

ENDFORM.                    " ZUGRIFF3_VERARBEITEN


*&---------------------------------------------------------------------*
*&      Form  HUB_ZUGEHOERIGKEIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HUB_ZUGEHOERIGKEIT using p_gestl.

  data: l_reghu1 like ztmihu_schenker-zzreghu.
  data: l_reghu2 like ztmihu_schenker-zzreghu.

*----------------------------------------------------------------------*
* Zuordnung der HUB-Kette (Doku 4.4.1)
* -Zugriff mit der ermittelten Empfangs-GSt. unter Punkt 4.2
*----------------------------------------------------------------------*
  clear ztmihu_schenker.

  select single * from ztmihu_schenker
    where zzgestl = h_gestl_42.

  if sy-subrc = 0.
    concatenate '(-' ztmihu_schenker-zzkettnr ')' into rout-feld3b.
  endif.

*----------------------------------------------------------------------*
* Ermittlung Regional-HUB für Empfangs-GSt. (Doku 4.4.2)
* - Zugriff mit der ermittelten Empfangs-GSt. im Routing (p_gestl)
*   Der gefundene Wert wird in die Variable l_reghu1 gespeichert
*----------------------------------------------------------------------*
  clear l_reghu1.
  clear ztmihu_schenker.

  select single zzreghu from ztmihu_schenker into l_reghu1
    where zzgestl = p_gestl.

*----------------------------------------------------------------------*
* Ermittlung Regional-HUB für die Versand-GSt. (Doku 4.4.3)
* - Zugriff mit der auftragsanlengenden Geschäftsstelle
*   Der gefundene Wert wird in die Variable l_reghu2 gespeichert
*----------------------------------------------------------------------*
  clear l_reghu2.
  clear ztmihu_schenker.

  select single zzreghu from ztmihu_schenker into l_reghu2
    where zzgestl = g_agestl.

*----------------------------------------------------------------------*
* Andruck des HUB/Regional-HUB auf dem Barcode (Doku 4.4.4)
*----------------------------------------------------------------------*
* beide Felder gleich
  if l_reghu1 = l_reghu2.
    move l_reghu1 to rout-feld2a.

    if l_reghu1 = '31' or l_reghu1 = '81'.
      concatenate '(' l_reghu1 ')' into rout-feld3a.
    endif.
*  beide Felder blank
  elseif l_reghu1 is initial and l_reghu2 is initial.
    move '91' to rout-feld2a.
*  beide Felder unterschiedlich
  else.
    move '91' to rout-feld2a.
  endif.


ENDFORM.                    " HUB_ZUGEHOERIGKEIT


*&---------------------------------------------------------------------*
*&      Form  GEFAHRENGUT_SPEED_TERMIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GEFAHRENGUT_SPEED_TERMIN changing p_gefgut
                                       p_speed
                                       p_term.

  data: l_vsart like likp-vsart.

* Stelle 16: Gefahrengut-Auftrag: Gefahrengut ausschliessen
  move '0' to p_gefgut.

* Stelle 17: SPEED-Auftrag
  move '0' to p_speed.


* Stelle 18: 1=Ab-Termin, 2=Bis-Termin, 3=Fixtermin
* Der Wert wird via Versandart aus der Liferung in der Tabelle ZTSKTE_SCHENKER
* ermittelt

  clear p_term.
  clear l_vsart.

** Versandart via Lieferung ermitteln
*  select single vsart from likp into l_vsart
*    where vbeln = g_vekp-vpobjkey.

* Sonderkennzeichen Termin via Versandart (likp-vsart) aus Tabelle ztskte_schenker ermitteln
  select single zzskterm from ztskte_schenker into p_term
    where vsart = g_likp-vsart.

* Falls Eintrag in Tabelle ztskte_schenker nicht gefunden, handelt es sich um
* einen Fixtermin
  if sy-subrc ne 0.
    move '1' to p_term.
  endif.


ENDFORM.                    " GEFAHRENGUT_SPEED_TERMIN


*&---------------------------------------------------------------------*
*&      Form  TAG_BEI_TERMIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TAG_BEI_TERMIN using p_term
                 changing p_tagte.


  data: l_fabrdat like sy-datum.
  data: l_fabkl like t001w-fabkl.
  data: l_days type i.


*----------------------------------------------------------------------*
* Bei Termin = 2 (Versandart: D0, D1, D2, D9):
*==> Liefertermin (likp-lfdat) + 1 Arbeitstag
*----------------------------------------------------------------------*
  if p_term = '2'.

*** Ermitteln der Fabrikkalender-ID via Versandstelle
**    clear l_fabkl.
**    select single fabkl from tvst into l_fabkl
**      where vstel = g_likp-vstel.

* Fabrikkalender-ID = '01' (Deutschland)
  move '01' to l_fabkl.

* Anzahl Arbeitstage = 1
    move 1 to l_days.

    CALL FUNCTION 'FKK_ADD_WORKINGDAY'
      EXPORTING
        I_DATE      = g_likp-lfdat "Lieferdatum
        I_DAYS      = l_days
        I_CALENDAR1 = l_fabkl
      IMPORTING
        E_DATE      = l_fabrdat.

    move l_fabrdat(2) to p_tagte.

    exit.

  endif.  "p_term = '2'


*----------------------------------------------------------------------*
* Bei Termin = 3 (Versandart: D4, D6):
* ==> Liefertermin (likp-lfdat)
*----------------------------------------------------------------------*
  if p_term = '3'.
    move g_likp-lfdat+6(2) to p_tagte.
    exit.
  endif.


*----------------------------------------------------------------------*
* Mit Tagesdatum initialisieren
*----------------------------------------------------------------------*
  move sy-datum(2) to p_tagte.
*----------------------------------------------------------------------*



ENDFORM.                    " TAG_BEI_TERMIN


*&---------------------------------------------------------------------*
*&      Form  KLARTEXT_TERMIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_H_BARC_TERM  text
*      -->P_H_BARC_TAGTE  text
*      <--P_H_BARC_KTTERM  text
*      <--P_PERFORM  text
*      <--P_GESAMTZAHL_COLLI  text
*      <--P_H_BARC_GESCO  text
*----------------------------------------------------------------------*
FORM KLARTEXT_TERMIN  USING    P_TERM
                               P_TAGTE
                      CHANGING P_KTTERM.

  clear p_ktterm.

  case p_term.
    when '1'. "AB-TERMIN
      concatenate text-te1 p_tagte into p_ktterm separated by space.
    when '2'. "BIS-TERMIN
      concatenate text-te2 p_tagte into p_ktterm separated by space.
    when '3'. "FIX-TERMIN
      concatenate text-te3 p_tagte into p_ktterm separated by space.
  endcase.


ENDFORM.                    " KLARTEXT_TERMIN



*&---------------------------------------------------------------------*
*&      Form  GESAMTZAHL_COLLI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GESAMTZAHL_COLLI changing p_gesco.

* Anzahl Colli immer mit "000" füllen
  move '000' to p_gesco.

ENDFORM.                    " GESAMTZAHL_COLLI


*&---------------------------------------------------------------------*
*&      Form  PACKSTUECKZAEHLER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PACKSTUECKZAEHLER changing p_packz.

* Packstückzähler immer mit "001" füllen
  move '001' to p_packz.

ENDFORM.                    " PACKSTUECKZAEHLER
