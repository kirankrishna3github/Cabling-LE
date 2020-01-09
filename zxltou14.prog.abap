*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU14                                                   *
*----------------------------------------------------------------------*
*Lagerplatz Findung:
*Für die Cable Lösung von Dätwyler findet die Kommissionierung Lieferung
*aus dem Lagertyp 916 statt.Dies, da der Bestand durch Kundenfunktionen
*bis auf Lagertyp 916, Lagerbereich 001, Lagerplatz = Lieferungsnummer
*gebucht wird.
*Die Kommissionierung ist notwendig für die Statusveränderung in der
*Lieferung.
*
*
*SAP Stäfa, P. Huber, 15082003
*Markus Raffeiner, 24.10.2013, CR 20131017_1500
*Erweiterung Werk 3200 (Decin): zusätzliche Abfrage auf Lagernummer 397
*Erweiterung Lagernummer 840 ( China) CSAPR2014

IF ( i_ltap-lgnum EQ '391' OR
     i_ltap-lgnum EQ '392' OR
     i_ltap-lgnum EQ '397' OR
     i_ltap-lgnum EQ '211' OR
     i_ltap-lgnum EQ '840' OR                             "CSAPR2014
     i_ltap-lgnum EQ '221' )
 AND i_ltak-betyp EQ 'L'
 "601 = normale Lieferung, 255 = Konsignationsbeschickung
 AND  ( i_ltak-bwlvs EQ '601' OR i_ltak-bwlvs EQ '255' ).

  IF i_ltap-nltyp EQ '916'.

    SELECT SINGLE * FROM mara                               "CS29042004
     WHERE matnr = i_ltap-matnr.                            "CS29042004

    SELECT * FROM lqua WHERE matnr = i_ltap-matnr
                       AND   lgnum = i_ltap-lgnum
                       AND   lgtyp = i_ltap-nltyp
                       AND   lgpla = i_ltap-nlpla.
      CLEAR t_ltapa.
      t_ltapa-anfml = lqua-gesme.
      t_ltapa-anfme = lqua-gesme.
      t_ltapa-vltyp = lqua-lgtyp.
      t_ltapa-vlpla = lqua-lgpla.
      t_ltapa-vlqnr = lqua-lqnum.
      t_ltapa-charg = lqua-charg.

      IF sy-binpt = 'X'.
        IF NOT lqua-charg IS INITIAL.          "Chargenpflichtig.
          SELECT * FROM lips WHERE vbeln = i_ltap-nlpla
                             AND   charg = lqua-charg.
            t_ltapa-anfml = t_ltapa-anfml - lips-lfimg.
            t_ltapa-anfme = t_ltapa-anfme - lips-lfimg.
          ENDSELECT.
        ELSE.
          SELECT * FROM vbfa WHERE vbelv = i_ltap-nlpla
                             AND   matnr = i_ltap-matnr
                             AND   vbtyp_n = 'Q'.
            " Jann Rompf, trimaster AG, 28.10.2013
            " Plus-Minus-Kennzeichen Vertriebsbelegfluss berücksichtigen
            IF vbfa-plmin = '-'.
              t_ltapa-anfml = t_ltapa-anfml + vbfa-rfmng.
              t_ltapa-anfme = t_ltapa-anfme + vbfa-rfmng.
            ELSE.
              t_ltapa-anfml = t_ltapa-anfml - vbfa-rfmng.
              t_ltapa-anfme = t_ltapa-anfme - vbfa-rfmng.
            ENDIF.
            " Jann Rompf, trimaster AG, 28.10.2013
          ENDSELECT.
        ENDIF.
*     Bei Einzelcharge prüfen, ob schon auf LIEF -> vergessen
      ELSE.                                                 "CS29042004
        IF mara-dpcbt = 'C'.                                "CS29042004
          SELECT SINGLE * FROM lips                         "CS29042004
          WHERE vbeln = i_ltak-vbeln                        "CS29042004
             AND posnr = i_ltap-posnr                       "CS29042004
             AND charg = lqua-charg.                        "CS29042004
          IF sy-subrc = 0.                                  "CS29042004
            CLEAR t_ltapa-anfml.                            "CS29042004
          ELSE.                                             "CS29042004
            SELECT SINGLE * FROM lips                       "CS29042004
            WHERE vbeln  = i_ltak-vbeln                     "CS29042004
              AND matnr = i_ltap-matnr                      "CS14052004
*             and uecha = i_ltap-posnr                      "CS29042004
              AND charg = lqua-charg.                       "CS29042004
            IF sy-subrc = 0.                                "CS29042004
              CLEAR t_ltapa-anfml.                          "CS29042004
            ENDIF.                                          "CS29042004
          ENDIF.                                            "CS29042004
        ENDIF.                                              "CS29042004
      ENDIF.                                                "CS29042004

      IF NOT t_ltapa-anfml IS INITIAL.
        APPEND t_ltapa.
      ENDIF.

* --------------------------------------------------------------------------
* FO Trunk Propose Data ZMOB
* --------------------------------------------------------------------------
      IF lqua-sobkz IS NOT INITIAL AND lqua-sonum IS NOT INITIAL .
        IF lqua-sobkz EQ i_ltap-sobkz AND
           lqua-sonum EQ i_ltap-sonum.
          CLEAR t_ltapa.
          t_ltapa-anfml = lqua-gesme.
          t_ltapa-anfme = lqua-gesme.
          t_ltapa-vltyp = lqua-lgtyp.
          t_ltapa-vlpla = lqua-lgpla.
          t_ltapa-vlqnr = lqua-lqnum.
          t_ltapa-charg = lqua-charg.
          APPEND t_ltapa.
        ENDIF.
      ENDIF.
    ENDSELECT.
  ENDIF.
ENDIF.
