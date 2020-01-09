*----------------------------------------------------------------------*
*   INCLUDE ZXF06U07                                                   *
*----------------------------------------------------------------------*

*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(IDOC_CONTRL_INDEX)
*"       EXPORTING
*"             VALUE(I_FIMSG) LIKE  FIMSG STRUCTURE  FIMSG
*"       TABLES
*"              IDOC_CONTRL STRUCTURE  EDIDC
*"              IDOC_DATA STRUCTURE  EDIDD
*"              DOCUMENT_DATA STRUCTURE  FTPOST1
*"              TAX_DATA STRUCTURE  FTTAX
*"              ADDITIONAL_DATA STRUCTURE  FTPOST1
*"----------------------------------------------------------------------
*"  Änderungshistorie:
*"----------------------------------------------------------------------
* Heinrich Eggenberger, SAPEGG Consulting, 27.09.2010
* Durch den Upgrade auf ECC 6.0 werden neu die MWST-Informationen in
* den Beleg übertragen, obwohl in Transaktion OBCE das Flag "Steuer
* rechnen" aktiv ist. In OSS-Hinweis 1032469 sind mögliche Lösungen er-
* wähnt, welche in diesem Fall nicht anwendbar sind (1) bzw. nicht an-
* gewendet werden sollen (2). Daher werden die MWST-Daten durch diesen
* Exit wieder gelöscht.
* (1) Steuerbeträge können nicht im IDoc mitgegeben werden, da die zu
*     verwendenden Steuerkennzeichen erst durch diesen Exit bestimmt
*     werden.
* (2) Eine Änderung im Konditionsschema ist nicht gewünscht, da sich
*     solche Änderungen auch an anderen Stellen auswirken können
*
* Peter Huber, 28092009; W.Santschi, M.Hofmann
* Implementierung BKR 0110, DSO
*
* SAP Stäfa/DKA, Heinrich Eggenberger/Marco Hofmann, 15.11.2006
* Mehrwertsteuercodes in Abhängigkeit des Buchungsdatums nach Erhöhung
* der Mehrwertsteuer von 16% auf 19% (siehe 20061115)
*
* DKA, Zgraggen Beat, Einführung SD bei DUK (siehe 20060101)
*      Werner Santschi  Änderung ausgeführt 02.01.2006
* SAP Stäfa/DKA, Heinrich Eggenberger/Luzia Schuler, 11.08.2004
* Korrekturen einbauen für DKO gemäss Mail vom 03.08.2004
* (siehe 20040811)
*
* SAP Stäfa/DKA, Heinrich Eggenberger/Beat Zgraggen, 11.06.2004
* Einfügen IV DKS an DKA bei Verkauf ab Werk 2000/2100 von DKA
* (siehe 20040611)
*
* SAP Stäfa/DKA, Heinrich Eggenberger/Beat Zgraggen, 16.04.2004
* Korrekturen in der Findung der Steuerkennzeichen (siehe 20040416)
*
*"----------------------------------------------------------------------
* SAP Stäfa, Peter Huber, 04.2004
* Steuerkennzeichen der kreditorischen Eingangrechnung bestimmen
*
* Wie kann der EXIT im debugger aufgerufen werden ?
*    Break-Point setzen im EXIT
*
* Bei Fakturierung IV wird ein IDOC (Nr. suchen mittels WE05)
* geschrieben.
* Mit WE19 kann die IDOC Verbuchung wiederholt werden.
*    IDOC Nr. eingeben
*    AUSFUEHREN
*    Eingang Funktionsbaustein
*    F'baustein: IDOC_INPUT_INVOIC_FI
*    Hellablauf, im Debugger Modus
*
* 09.05.2011; Markus Raffeiner, Konstante Mandantenabfragen elimineren
*
* 12.07.2011; Markus Raffeiner, Neues Werk 2200: Abfrageergänzung wo
*                               bereits auf Werk=2100 abgefragt wird
* 20.12.2011; Markus Raffeiner, Anpassungen für DKO SetUp
* 20.12.2011; Markus Raffeiner, 20120628_1135
* Ermittlung des MWST-Kennzeichen bei DKO analog DKS, jedoch bei
* Werk 2200 muss das MWST-Kennzeichen = 'DP' gesetzt werden
* 24.10.2012; Markus Raffeiner, 20120920_1600: IV DME (BUKRS = '0106")
* 10.09.2013; Markus Raffeiner, 20130709_1623: IV DMM (BUKRS = '0114")
* 06.11.2013; Markus Raffeiner, CR 20131010_1000:
* Anpassungen Projekt EC-PCA
* 03.02.2014; Markus Raffeiner, MWST-Kennzeichen 'LZ' für DMM
*(BUKRS = '0114') wieder aktiviert
* 09.12.2015; Markus Raffeiner, 20151021_1033: IV DKA (BUKRS = '0101")
* 29.08.2016; Markus Raffeiner, 20151021_1033: IV DKA (BUKRS = '0101")
* 02.01.2017; Markus Raffeiner, 20151021_1033 IV-Prozess ab Werk 2200,
* Anpassungen wieder rückgängig gemacht (siehe Version 28)
*"----------------------------------------------------------------------

************************************************************************
*Tabellen
************************************************************************
TABLES: t005.
************************************************************************
*intene Tabellen
************************************************************************
DATA: BEGIN OF itab OCCURS 0,
        count    LIKE document_data-count,
        bukrs(4),
        werks(4),
        land(3),
        mwskz(2),
      END OF itab.
DATA: budat1(8),                                            "20061115
      budat2 LIKE sy-datum.                                 "20061115

FIELD-SYMBOLS <fs_we> TYPE edidd.

DATA ls_e1edpa1 TYPE e1edpa1 .

DATA: lv_docclass TYPE doccls,
      lv_land     TYPE land,
      lv_burks    TYPE bukrs_vf,
      lv_blart    TYPE blart,
      lv_lastdate TYPE sy-datum,
      lv_postdate TYPE sy-datum.

CONSTANTS: lc_land  TYPE string VALUE 'IT',
           lc_burks TYPE string VALUE '0109',
           lc_budat TYPE string VALUE 'BKPF-BUDAT'.

************************************************************************
*Verarbeitung
************************************************************************
*Lieferwerk, Land des Kunden und fakturierende Verkaufsorganisation
*ermitteln und in itab ergänzen
CLEAR itab.

LOOP AT document_data.
* Buchungsdatum                                        "20061115
  IF document_data-fnam = 'BKPF-BUDAT'.                     "20061115
    budat1 = document_data-fval.                            "20061115
  ENDIF.                                                    "20061115
* Buchungskreis, Werk, Land des Kunden                 "20061115
  IF document_data-fnam = 'BSEG-RKE_BUKRS'.
    itab-count = document_data-count.
    itab-bukrs = document_data-fval.
    IF document_data-fval EQ lc_burks.
      lv_burks = document_data-fval.
    ENDIF.
  ENDIF.
  IF document_data-fnam = 'BSEG-RKE_WERKS'.
    itab-count = document_data-count.
    itab-werks = document_data-fval.
  ENDIF.
  IF document_data-fnam = 'BSEG-RKE_LAND1'.
    itab-count = document_data-count.
    itab-land =  document_data-fval.
    IF document_data-fval EQ lc_land.
      lv_land = document_data-fval.
    ENDIF.
  ENDIF.
  IF document_data-fnam = 'BKPF-BLART'.
    lv_blart = document_data-fval.
  ENDIF.

  AT NEW count.
    IF NOT itab-bukrs IS INITIAL.
      APPEND itab.
    ENDIF.
    CLEAR itab.
  ENDAT.
ENDLOOP.

SELECT SINGLE doccls
  INTO lv_docclass
  FROM t003_i
  WHERE land1 = lv_land
  AND blart = lv_blart.

IF sy-subrc EQ 0 AND lv_docclass IS NOT INITIAL.

  SELECT SINGLE lastdate
    INTO lv_lastdate
    FROM ofnum_it_1
    WHERE bukrs = lv_burks
    AND docclass = lv_docclass.

  IF sy-subrc EQ 0 AND lv_lastdate IS NOT INITIAL.
    WRITE lv_lastdate USING EDIT MASK '________' TO lv_lastdate.
    MOVE budat1 TO lv_postdate.
    IF lv_lastdate GT lv_postdate.
      MOVE lv_lastdate TO budat1.
      READ TABLE document_data[] ASSIGNING FIELD-SYMBOL(<fs_doc_data>) WITH KEY fnam = lc_budat.
      IF sy-subrc EQ 0.
        <fs_doc_data>-fval = budat1.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

* Buchungsdatum in Format JJJJMMTT konvertieren        "20061115
budat2(4)   = budat1+4(4).                                  "20061115
budat2+4(2) = budat1+2(2).                                  "20061115
budat2+6(2) = budat1(2).                                    "20061115

*Korrektes Steuerkennzeichen zu Lieferwerk, Land des Kunden
*und fakturierende Verkaufsorganisation Faktura F2
*ermitteln und in itab ergänzen
SORT itab BY count.
LOOP AT itab.
  SELECT SINGLE * FROM t005 WHERE land1 = itab-land.
  IF sy-subrc = 0.
    CASE: itab-bukrs.

      WHEN '0101'.       "DKA                          "20040611
***        if itab-werks = '2000' or itab-werks = '2100'. "20040611
***          if budat2 < '20070101'.                      "20061115
***            itab-mwskz = 'DW'.                         "20040611 16%
***          else.                                        "20061115
***            itab-mwskz = 'DP'.                         "20061115 19%
***          endif.                                       "20061115
***        endif.                                         "20040611

        CASE itab-werks. " Plant CZ Decin
          WHEN '3300' .

            CASE itab-land.
              WHEN 'CH' .
                itab-mwskz = 'IM'.
              WHEN 'CZ' .
                itab-mwskz = 'MP'.
              WHEN 'DE' .
                itab-mwskz = 'DR'.
              WHEN OTHERS .

                IF t005-xegld = 'X'.
                  READ TABLE idoc_data ASSIGNING <fs_we> WITH KEY segnam = 'E1EDPA1' .
                  IF sy-subrc = 0 .
                    MOVE <fs_we>-sdata TO ls_e1edpa1 .
                    IF ls_e1edpa1-land1 = 'DE' .
                      itab-mwskz = 'DR'.
                    ELSE.
                      itab-mwskz = 'DL'.
                    ENDIF.
                  ENDIF.
                ELSE.
                  itab-mwskz = 'V0' .
                ENDIF.
            ENDCASE.

          WHEN '2200'.
            IF itab-land = 'CH'.
              itab-mwskz = 'IM'.
***            elseif itab-land = 'GB'.
***              itab-mwskz = '??'.
***            elseif itab-land = 'IL'.
***              itab-mwskz = '??'.
***            else.
***              itab-mwskz = '??'.
            ENDIF.
          WHEN OTHERS.
            IF itab-werks = '2000' OR itab-werks = '2100'.  "20040611
              IF budat2 < '20070101'.                       "20061115
                itab-mwskz = 'DW'.                         "20040611 16%
              ELSE.                                         "20061115
                itab-mwskz = 'DP'.                         "20061115 19%
              ENDIF.                                        "20061115
            ENDIF.                                          "20040611
        ENDCASE.  "itab-werks

      WHEN '0103'.       "DUK
        IF itab-werks = '3000' OR
           itab-werks = '3001'.
          itab-mwskz = 'GZ'.
        ELSEIF itab-land = 'AT'.                            "20060101
          itab-mwskz = 'GX'.                                "20060101
        ELSEIF itab-land = 'DE'.                            "20060101
          IF budat2 < '20070101'.                           "20061115
            itab-mwskz = 'DW'.                         "20060101 16%
          ELSE.                                             "20061115
            itab-mwskz = 'DP'.                         "20061115 19%
          ENDIF.                                            "20061115

        ELSE.

          IF t005-xegld = 'X'.       "EU
            IF itab-werks = '2000' OR
               itab-werks = '2100' OR
               itab-werks = '2200' OR
               itab-werks = '3100'.
*             itab-mwskz = 'GX'.                       "20060101
              itab-mwskz = 'GU'.                            "20060101
            ENDIF.

          ELSEIF t005-xegld = ' '.   "nicht EU
            IF itab-werks = '2000' OR
               itab-werks = '2100' OR
               itab-werks = '2200' OR
               itab-werks = '3100'.
              itab-mwskz = 'GZ'.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN '0105'.       "DKS

*       if itab-werks = '2000' or itab-werks = '2100'. "20040416
*         itab-mwskz = 'DS'.                           "20040416
*       endif.                                         "20040416
*       if itab-werks = '3000' or itab-werks = '3100'. "20040416
*         itab-mwskz = 'DW'.                           "20040416
*       endif.                                         "20040416
        IF itab-werks = '3000' OR
           itab-werks = '3001'.                             "20040416
          itab-mwskz = 'DS'.                                "20040416
        ENDIF.                                              "20040416
        IF itab-werks = '3100'.                             "20040416
          IF budat2 < '20070101'.                           "20061115
            itab-mwskz = 'DW'.                         "20040416 16%
          ELSE.                                             "20061115
            itab-mwskz = 'DP'.                         "20061115 19%
          ENDIF.                                            "20061115
        ENDIF.                                              "20040416

      WHEN '0108'.       "DKO
        IF itab-werks = '2200'.                             "20040416
          itab-mwskz = 'DP'.                                "20040416
        ENDIF.                                              "20040416
        IF itab-werks = '3000' OR
           itab-werks = '3001'.                             "20040416
          itab-mwskz = 'DS'.                                "20040416
        ENDIF.                                              "20040416
        IF itab-werks = '3100'.                             "20040416
          IF budat2 < '20070101'.                           "20061115
            itab-mwskz = 'DW'.                         "20040416 16%
          ELSE.                                             "20061115
            itab-mwskz = 'DP'.                         "20061115 19%
          ENDIF.                                            "20061115
        ENDIF.                                              "20040416

      WHEN '0116'.      "DKN
        IF itab-werks = '3000' OR
           itab-werks = '3001'.                             "20040416
          itab-mwskz = 'HZ'.                                "20040416
        ELSEIF itab-land = 'NL'.                            "20040416
          itab-mwskz = 'HY'.                                "20040416
        ELSE.                                               "20040416
*       if itab-land = 'BE'.                           "20040416
*         itab-mwskz = 'HU'.                           "20040416
*       else.                                          "20040416

          IF t005-xegld = 'X'.       "EU
            IF itab-werks = '2000' OR
               itab-werks = '2100' OR
               itab-werks = '2200' OR
               itab-werks = '3100'.
*             itab-mwskz = 'HY'.                       "20040416
              itab-mwskz = 'HU'.                            "20040416
            ENDIF.

          ELSEIF t005-xegld = ' '.   "nicht EU
            IF itab-werks = '2000' OR
               itab-werks = '2100' OR
               itab-werks = '2200' OR
               itab-werks = '3100'.
              itab-mwskz = 'HZ'.
            ENDIF.
          ENDIF.
        ENDIF.

      WHEN '0110'.       "DSO                         28092009
        itab-mwskz = 'kW'. "Eingangsst. null %

      WHEN '0106'.       "DME                         24102012
        itab-mwskz = 'LZ'. "Eingangsst. null %

      WHEN '0114'.       "DMM                         10092013

*>>> START MOD-001 CHG0032675
        itab-mwskz = 'VX'. "Eingangsst. null %
*        itab-mwskz = 'I1'. "I1
*<<< END MOD-001 CHG0032675

* CHG0034338 - Formation legal entity in Italy(CHG0034269)
      WHEN '0109'.       "DCI                         DC2K918672
        itab-mwskz = 'OW'. "Eingangsst. null %

    ENDCASE.
    MODIFY itab.
  ENDIF.
ENDLOOP.

*Document_data den korrekten MWSKZ verpassen . . .
SORT itab.
LOOP AT document_data.
  IF document_data-fnam = 'BSEG-MWSKZ' AND
     document_data-fval NE '**'.

    READ TABLE itab WITH KEY count = document_data-count.
    IF sy-subrc  = 0.
      document_data-fval = itab-mwskz.
      MODIFY document_data.
    ENDIF.
  ENDIF.
ENDLOOP.


* MWST-Daten werden gelöscht (27.09.2010), da
* a) das Steuerkennzeichen in diesem Exit noch geändert werden kann
* b) das Flag "Steuer rechnen" (Trans. OBCE) gesetzt ist
* Werden die MWST-Daten nicht gelöscht, können die erzeugten Mappen
* nicht automatisiert abgespielt werden (SM35)
REFRESH tax_data.
