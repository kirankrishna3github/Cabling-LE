*&---------------------------------------------------------------------*
*& Report  SDPACKLI                                                    *
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Nachricht: Packliste mit SAPscript (Formular: ZSD_PACKING_LIST)      *
*----------------------------------------------------------------------*


************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT sdpackli.


*----------------------------------------------------------------------*
* Aufbau                                                               *
*----------------------------------------------------------------------*

* Der Report besteht aus folgenden Teilen:
*   A. Allgemein: Nachricht
*      Es wird der Ablauf als Nachrichten-Report realisiert. Wie
*      üblich wird eine Einstiegsroutine (ENTRY) definiert, die
*      in die Verarbeitungsroutine (PROCESSING) verzweigt. Von
*      dort werden die verschiedene Verarbeitungschritte aufgerufen.
*      Dieser allgemeine Teil dient im wesentlichen zur Status-
*      fortschreibung.
*   B. Speziell: Packliste
*      Der spezifische Teil realisiert die Datenbeschaffung, sowie
*      die eigentliche Ausgabe. Dazu werden die entsprechenden Text-
*      elemente des zugehörigen Formulars (s.o.) ausgegeben. Außerdem
*      ist zu Beginn der grundlegende Aufbau und die Realsierung der
*      der Verpackung von Lieferungen beschrieben.


*----------------------------------------------------------------------*
* Änderungen                                                           *
*----------------------------------------------------------------------*
*                                                                      *
* ES22042008      Ernst Sommer, 22.04.2008                             *
*        300      Anzahl Colis und Gewicht müssen addiert und am       *
*                 Schluss als Total ausgegeben werden.                 *
* MR13022012      Markus Raffeiner, Mistralnummer zusätzlich bei Sped. *
*                 Schenker andrucken                                   *
* PA10072013      Flickroutine TEST_ALTE_NACHRICHT                     *
*                 Neue Nachrichtenart bei vorhandener alter            *
*                 Nachrichtenart nicht ausdrucken, der Drucker wird    *
*                 auf DUMM (Dummy-Drucker) gesetzt                     *
* MR30032016      Markus Raffeiner, CR 20160304_1130                   *
*                 Ausgabe Stat.Warennummer / Import-CodeNr             *
* MR16122016      Markus Raffeiner, CHG0030213                         *
*                 Bei Dachser muss EXIDV2 anstatt EXIDV auf der        *
*                 Packliste ausgegeben werden                          *
* MR10012017      Markus Raffeiner, CR 20160304_1130                   *
*                 Ausgabe Stat.Warennummer / Import-CodeNr inaktiv     *
*                 gesetzt, da sonst nachfolgende Entwicklungen         *
*                 blockiert werden (siehe Version 11)                  *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Daten
*----------------------------------------------------------------------*

INCLUDE rvadtabl.

TABLES: vbuk,    "Vertriebsbeleg (Kopf)-Status
        vbco3,   "Schlüsselfelder Vertriebsbeleg
        vbdkl,   "Dokumentenkopfview Lieferschein
        vbpla,   "Allgemeine Transportdaten
        vbplk,   "Versandelement Kopfdaten
        vbplp,   "Versandelement Positionsdaten
        vbpls,   "Verpackung Summendaten
        vbkd,    "Verkaufsbeleg: Kaufmännische Daten
        tvsakt.  "Sonderabwicklungskennzeichen: Texte


TABLES: mch1.    "Chargenstamm

TABLES: ztlief_schenker. "Lieferungen / Mistralnummer für Schenker


DATA: BEGIN OF xvbplk OCCURS 10.
        INCLUDE STRUCTURE vbplk.
DATA: END OF xvbplk.

DATA: BEGIN OF xvbplp OCCURS 50.
        INCLUDE STRUCTURE vbplp.
DATA: END OF xvbplp.

DATA: BEGIN OF xvbpls OCCURS 10.
        INCLUDE STRUCTURE vbpls.
DATA: END OF xvbpls.

DATA: BEGIN OF xvbdkl OCCURS 10.
        INCLUDE STRUCTURE vbdkl.
DATA: END OF xvbdkl.

DATA: retcode LIKE sy-subrc.
DATA: xscreen(1) TYPE c.         "Ausgabe auf Drucker oder Bildschirm

*--- begin of insert -------------------------------- ES22042008 ---*
DATA: z_anz_coli  LIKE  vbplk-anzgl,
      z_tot_brgew LIKE  vbplk-brgew.

*--- end of insert ---------------------------------- ES22042008 ---*

DATA: BEGIN OF gs_eipo,
        stawn TYPE stawn,
      END OF gs_eipo.

DATA: gv_exidv TYPE exidv.

* Spediteurnummer für Schenker
CONSTANTS: c_sped_schenker(10) VALUE '0000097060'.


*----------------------------------------------------------------------*
* A. Nachricht (allgemein)                                             *
*----------------------------------------------------------------------*

FORM entry USING return_code us_screen.

  CLEAR retcode.
  xscreen = us_screen.
  PERFORM processing USING us_screen.
  IF retcode NE 0.
    return_code = 1.
  ELSE.
    return_code = 0.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROC_SCREEN                                                   *
*---------------------------------------------------------------------*
FORM processing USING proc_screen.

  PERFORM get_data.      " <-- speziell
  CHECK retcode = 0.
* Falls eine alte Nachricht vorhanden ist, wird der          "N102
* Drucker auf Dummy gesetzt                                  "N102
  PERFORM test_alte_nachricht.                              "N102

  PERFORM form_open USING proc_screen vbpla-land1.
  CHECK retcode = 0.
  PERFORM check_repeat.
  PERFORM text_print.    " <-- speziell
  CHECK retcode = 0.
  PERFORM form_close.
  CHECK retcode = 0.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FORM_OPEN                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  US_SCREEN                                                     *
*  -->  US_COUNTRY                                                    *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM form_open USING us_screen us_country TYPE vbpla-land1.
  "smart: 2010-08-02 #105


  INCLUDE rvadopfo.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FORM_CLOSE                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM form_close.

  CALL FUNCTION 'CLOSE_FORM'           "...Ende Formulardruck
    EXCEPTIONS
      OTHERS = 1.
  IF sy-subrc NE 0.
    retcode = 1.
    PERFORM protocol_update.
  ENDIF.
  SET COUNTRY space.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM CHECK_REPEAT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM check_repeat.

  SELECT * INTO *nast FROM nast WHERE kappl = nast-kappl
                                AND   objky = nast-objky
                                AND   kschl = nast-kschl
                                AND   spras = nast-spras
                                AND   parnr = nast-parnr
                                AND   parvw = nast-parvw
                                AND   nacha BETWEEN '1' AND '4'.
    CHECK *nast-vstat = '1'.
    CALL FUNCTION 'WRITE_FORM'
      EXPORTING
        element = 'REPEAT'
        window  = 'REPEAT'
      EXCEPTIONS
        element = 1
        window  = 2.
    IF sy-subrc NE 0.
      PERFORM protocol_update.
    ENDIF.
    EXIT.
  ENDSELECT.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM protocol_update.

  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.


*----------------------------------------------------------------------*
* B. Packliste (speziell)                                              *
*----------------------------------------------------------------------*

* Die Verpackung von Lieferungen wird durch Versandelemente (VSE)
* realisiert. Technisch ist ein Versandelement ein Beleg mit Kopf-
* und Positionsdaten (Strukturen VBPLK/XVBPLK und VBPLP/XVBPLP).
* Eine Position in einem VSE ist entweder wieder ein VSE oder Teil
* eine Lieferposition (also mit den zum VSE gehörenden Materialen/
* Versandhilftsmitteln verpackt). Der Zusammenhang zur Lieferung
* wird durch die beiden Felder VBELN (Lieferung) und POSNR (Position)
* in der Struktur VBPLP realisiert. Es ist zu beachten, daß ein VSE
* keine Positionsdaten enthalten muß: Die Lieferung wurde dann nicht
* vollständig verpackt.
* Mathematisch gesehen stellen die VSE einen Wald (aus Bäumen) dar.
* An den Knoten können noch Lieferpositionsdaten hängen. Um zu
* erkennen ob ein Knoten eine Wurzel oder (und) ein Blatt ist, gibt
* es im VSE-Kopf Kennzeichen. Ist KZOBE gesetzt, so ist das VSE ein
* oberes Element, also eine Wurzel. Ist KZUNT gesetzt, stellt das VSE
* ein unteres Element dar, also ein Blatt. Ist keines der beiden
* Kennzeichen gesetzt dann ist das VSE ein innerer Knoten. Es ist
* zu beachten, daß beiden Kennzeichen gesetzt sein können: Der Baum
* besteht aus einem Knoten. Die Tiefe eines Knotens durch das Feld
* TIVEL gegeben.
* Weiterhin ist der Baum doppelt verkettet. Es ist also möglich,
* nicht nur einen Weg von der Wurzel (eines Teilbaums) zu einem
* Blatt zu finden, sondern auch umgekehrt von einem Blatt zur Wurzel
* zu gelangen. Dazu existiert (im VSE-Kopf) das Feld UEVEL, also
* das übergeordnete VSE.
* Um schließlich zu entscheiden, ob ein VSE-Position ein VSE oder
* eine Lieferposition ist, wird das Feld POSNR ausgewertet. Ist
* das Feld initial, stellt die Position ein VSE dar, im anderen Fall
* enthält es die Lieferposition. Ist die Position ein VSE, steht im
* Feld UNVEL das zugehörige untergeordnete VSE.


FORM get_data.

  CLEAR vbco3.                                              "n_457174
  vbco3-vbeln = nast-objky.
  vbco3-spras = nast-spras.
  vbco3-kunde = nast-parnr.
  vbco3-parvw = nast-parvw.

  CALL FUNCTION 'SD_PACKING_PRINT_VIEW'
    EXPORTING
      comwa     = vbco3
    IMPORTING
      vbpla_wa  = vbpla
    TABLES
      vbplk_tab = xvbplk
      vbplp_tab = xvbplp
      vbpls_tab = xvbpls
    EXCEPTIONS
      OTHERS    = 01.

* fill address key --> necessary for emails   "v N440369
  addr_key-addrnumber = vbpla-adrnr.
  addr_key-persnumber = vbpla-adrnp.
  addr_key-addr_type  = vbpla-address_type.                 "^ N440369

  LOOP AT xvbplk WHERE kzobe = 'X'.
    EXIT.
  ENDLOOP.

  IF sy-subrc > 0.
    syst-msgid = 'VL'.
    syst-msgno = '490'.
    syst-msgty = 'E'.
    PERFORM protocol_update.
    retcode = 1.
  ENDIF.

* TDNAME ermitteln  für Hebebühne-Text
  vbdkl-tdname = vbpla-vbeln.

* Mistralnummer für Spediteur Schenker ermitteln.
  CLEAR ztlief_schenker.

  IF vbpla-spdnr = c_sped_schenker.
    SELECT SINGLE * FROM ztlief_schenker
      WHERE vbeln = vbco3-vbeln.
  ENDIF.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM TEXT_PRINT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM text_print.
* Die Kopfdaten werden implizit ausgegeben, ...
* ... dann die Überschriften der Positionen ...

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEADER'
      type    = 'TOP'.


* ... und schließlich die Positionen ausgeben

*--- begin of insert -------------------------------- ES22042008 ---*
  CLEAR: z_anz_coli, z_tot_brgew.
*--- end of insert ---------------------------------- ES22042008 ---*


  LOOP AT xvbplk WHERE kzobe = 'X'.
    PERFORM packing_tree USING xvbplk-venum.
  ENDLOOP.

*--- begin of insert -------------------------------- ES22042008 ---*
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TOTAL'.
*--- end of insert ---------------------------------- ES22042008 ---*

* text sonderabwicklung ermitteln
  CLEAR:  vbkd, tvsakt.
  SELECT  SINGLE *  FROM vbkd
          WHERE     vbeln =  vbplp-vgbel.
  IF  sy-subrc = 0.
    SELECT  SINGLE *  FROM tvsakt
            WHERE     sdabw =  vbkd-sdabw.
  ENDIF.

ENDFORM.


* Rekursive Prozedur

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM packing_tree USING VALUE(shenr) TYPE clike. "smart: 2010-08-02 #105

  DATA: lv_exnum TYPE exnum.
  DATA: lv_lifnr TYPE lifnr.

* Daten des Versandelements shenr ausgeben, also Wurzel des Teilbaums
  MOVE space TO xvbplk.
  xvbplk-venum = shenr.
  READ TABLE xvbplk.
  vbplk = xvbplk.

*--- begin of insert -------------------------------- es22042008 ---*
  z_anz_coli  = z_anz_coli  + 1.
  z_tot_brgew = z_tot_brgew + vbplk-brgew.
*--- end of insert ---------------------------------- ES22042008 ---*

*--- begin of insert -------------------------------- CHG0030213 ---*
* Falls ein Eintrag in der Tabelle zedi_gln (z.B. Dachser) vorhanden ist,
* dann muss die NVE-Nummer gedruckt werden, welche im Feld VBPLK-EXIDV2
* vorhanden ist. Diese wird beim Warenausgang auf HU-Ebene erzeugt.

  CLEAR: gv_exidv, lv_lifnr.

  SELECT SINGLE lifnr FROM zedi_gln INTO lv_lifnr
    WHERE lifnr = vbpla-spdnr.

  IF sy-subrc EQ 0.
    gv_exidv = vbplk-exidv2.
  ELSE.
    gv_exidv = vbplk-exidv.
  ENDIF.

*--- end of insert ---------------------------------- CHG0030213 ---*

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'SHELEM'.

* Nun die Positionen abarbeiten (Teilbäume durchlaufen).
* Hier wäre eventuell ein Sortierung zwischen Lieferpositionen
* und weiteren Versandelementen sinnvoll. Alle nachgeordneten
* Positionen liegen eine Stufe tiefer.
  LOOP AT xvbplp WHERE venum = shenr.
*Zusatzdaten Charge Dätwyler lesen
    CLEAR: mch1.
    SELECT SINGLE * FROM mch1 WHERE matnr EQ xvbplp-matnr
                                AND charg EQ xvbplp-charg.

* Beginn CR 20160304_1130
**** Stat.WarenNr / Import-CodeNr für Außenhandel
***    CLEAR: gs_eipo, lv_exnum.
***
***    IF NOT xvbplp-posnr IS INITIAL.
***
***      SELECT SINGLE exnum FROM likp INTO lv_exnum
***        WHERE vbeln EQ vbco3-vbeln.
***
***      IF sy-subrc EQ 0.
***        SELECT SINGLE stawn FROM eipo INTO gs_eipo-stawn
***          WHERE exnum EQ lv_exnum AND
***                expos EQ xvbplp-posnr.
***      ENDIF.
***
***    ENDIF.  "xvbplp-posnr
* Ende CR 20160304_1130

    IF xvbplp-posnr IS INITIAL.
*     Versandelement
      PERFORM packing_tree USING xvbplp-unvel.
    ELSE.
*     Lieferposition (Anteil)
      vbplp = xvbplp.
      CALL FUNCTION 'WRITE_FORM'
        EXPORTING
          element = 'DELPOS'.
    ENDIF.
  ENDLOOP.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  TEST_ALTE_NACHRICHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test_alte_nachricht .

  DATA: l_nast TYPE nast.
  DATA: lv_kschl_alt LIKE nast-kschl.

  CLEAR lv_kschl_alt.

  CASE nast-kschl.
    WHEN 'ZPL1'.
      lv_kschl_alt = 'PL00'.
  ENDCASE.

  CHECK NOT lv_kschl_alt IS INITIAL.

* Test ob alte Nachrichten vorhanden sind
  SELECT SINGLE * FROM nast INTO l_nast
    WHERE kappl = 'V2' AND
          objky = nast-objky AND
          kschl = lv_kschl_alt.

* Falls alte Nachricht vorhanden, wird der Drucker auf Dummy gesetzt
  IF sy-subrc EQ 0.
    MOVE 'DUMM' TO nast-ldest.
  ENDIF.

ENDFORM.                    " TEST_ALTE_NACHRICHT
