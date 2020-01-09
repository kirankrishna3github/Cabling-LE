*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU03                                                   *
*----------------------------------------------------------------------*
"----------------------------------------------------------------------
*"*"Globale Schnittstelle:
*"       IMPORTING
*"              I_LTAK LIKE  LTAK STRUCTURE  LTAK
*"              I_LTAP LIKE  LTAP STRUCTURE  LTAP
*"              I_MLVS LIKE  MLVS STRUCTURE  MLVS
*"              I_MGEF LIKE  MGEF STRUCTURE  MGEF
*"              I_T331 LIKE  T331 STRUCTURE  T331
*"              I_T333 LIKE  T333 STRUCTURE  T333
*"              I_T340D LIKE  T340D STRUCTURE  T340D
*"              I_VORGA LIKE  LTAP-VORGA
*"       EXPORTING
*"              E_NLPLA LIKE  LTAP-NLPLA
*"              E_NPPOS LIKE  LTAP-NPPOS
*"              E_NKDYN LIKE  LTAP-NKDYN
*"              E_NLENR LIKE  LTAP-NLENR
*"              E_SUBRC LIKE  SY-SUBRC
*"              E_MSGID LIKE  SY-MSGID
*"              E_MSGNO LIKE  SY-MSGNO
*"              E_MSGV1 LIKE  SY-MSGV1
*"              E_MSGV2 LIKE  SY-MSGV2
*"              E_MSGV3 LIKE  SY-MSGV3
*"              E_MSGV4 LIKE  SY-MSGV4
*"----------------------------------------------------------------------
*  Aenderungen:
*
*  1652010;  consource, Peter Huber
*            Lagerbereichsfindung für Report ZLE_GOODMOVE_01
*
*  1052010;  consource, Peter Huber
*            Lagerebereichsfindung - einlesen Memory und Entscheid
*            welcher Bereich
*
*  21032010; consource, Peter Huber
*            Lagernummer 600 FF Lager mit eigener Einl.strategie
*            Zuerst auf Freiplätze, dann auf Reserveplätze R..
*
*  16032010; consource, Peter Huber
*            Rohmat.lager DRX ebenfalls Fixplatzlösung
*
*  15112006, SAP Stäfa, Peter Huber
*            Der Fixplatz zum Material wird als Fixplatz in der
*            Einlagerung vorgeschlagen.Soll diese Logik mit Leerplatz-
*            strategie kombiniert werden, muss der Exit angepasst
*            werden.Das anleinige Eintragen von 'L' reicht nicht !!
*"----------------------------------------------------------------------

TABLES: mlgt,
        mlgn,
        t334b.

DATA: BEGIN OF ilagp OCCURS 0.
        INCLUDE STRUCTURE lagp.
DATA: END OF ilagp.

DATA: counter TYPE n,
      wlgber(3) TYPE c.

DATA:  l_lgb(3) TYPE c,
       l_lgpla(10) type c,
       l_memid(20) TYPE c.


*15112006; Beginn:
CASE: i_ltak-lgnum.
  WHEN 800 OR 620. "DRC und Mexico Rohmateriallager
    CASE: i_ltap-nltyp.
      WHEN 001. "LGTYP 001 kein Fixplatzlagertyp
        "
        SELECT SINGLE * FROM mlgt WHERE matnr = i_ltap-matnr
                                    AND lgnum = i_ltap-lgnum
                                    AND lgtyp = i_ltap-nltyp.

        IF sy-subrc = 0 AND NOT mlgt-lgpla IS INITIAL.

          "Fixplatz aus Materialstamm, Lagernr., Lagertyp übergeben
          e_nlpla = mlgt-lgpla.

        ENDIF.
    ENDCASE.

*15112006; Ende:

*23032010; Beginn
  WHEN 600.

    if I_ltap-nlber is initial and I_ltap-nlpla is initial.

* Auslesen Memory für Lagerbereichsfindung.
* Wird im ZLE_GOODMOVE gesetzt.
      l_memid = 'M_LGBER'.
      CLEAR l_lgb.
      IMPORT l_lgb TO l_lgb FROM MEMORY ID l_memid.

      CASE l_lgb.

        WHEN '1'.
*     Fall 1: Leerplatzsuche gemäss Materialstamm, sofern Memorywert 1
*             Einlagertypkennzeichen und Lagerbereichkz.
*             Damit suche der gültigen Lagerbereiche in der t334b.
          SELECT SINGLE * FROM mlgn WHERE matnr = i_ltap-matnr
                                            AND lgnum = i_ltap-lgnum.
          IF sy-subrc = 0 AND NOT mlgn-lgbkz IS INITIAL
                          AND NOT mlgn-ltkze IS INITIAL.

            SELECT SINGLE * FROM t334b WHERE lgnum = i_ltak-lgnum
                                         AND lgtyp = mlgn-ltkze
                                         AND lgbkz = mlgn-lgbkz.
            IF sy-subrc = 0.
              counter = 0.

              DO 10 TIMES.
                CHECK: e_nlpla IS INITIAL.
                CLEAR: wlgber.
                CASE: counter.
                  WHEN 0. wlgber  = t334b-lgbe0.
                  WHEN 1. wlgber  = t334b-lgbe1.
                  WHEN 2. wlgber  = t334b-lgbe2.
                  WHEN 3. wlgber  = t334b-lgbe3.
                  WHEN 4. wlgber  = t334b-lgbe4.
                  WHEN 5. wlgber  = t334b-lgbe5.
                  WHEN 6. wlgber  = t334b-lgbe6.
                  WHEN 7. wlgber  = t334b-lgbe7.
                  WHEN 8. wlgber  = t334b-lgbe8.
                  WHEN 9. wlgber  = t334b-lgbe9.
                ENDCASE.

                CLEAR: ilagp.
         SELECT * INTO TABLE ilagp FROM lagp WHERE lgnum = i_ltap-lgnum
                                                 AND lgtyp = mlgn-ltkze
                                                     AND lgber = wlgber
                                                          AND anzqu = 0
                           .
                IF sy-subrc = 0.
                  SORT ilagp BY sorlp lgpla.

                  LOOP AT ilagp.
                    e_nlpla = ilagp-lgpla.
                    EXIT.
                  ENDLOOP.
                ELSE.
                  counter = counter + 1.
                ENDIF.

              ENDDO.
            ELSE.
              e_nlpla = 'ingnoto!'.
            ENDIF.

          ELSE.
            e_nlpla = 'ingnoto!'.
          ENDIF.


        WHEN '2'.
*     Fall 2: Leerplatzsuche im Save Lounge Bereich
*             Einlagertypkennzeichen und Lagerbereichkz. gem. Material
*             Damit suche der gültigen Lagerplätze im SL
          SELECT SINGLE * FROM mlgn WHERE matnr = i_ltap-matnr
                                      AND lgnum = i_ltap-lgnum.

          IF sy-subrc = 0 AND NOT mlgn-lgbkz IS INITIAL
                          AND NOT mlgn-ltkze IS INITIAL.

         SELECT * INTO TABLE ilagp FROM lagp WHERE lgnum = i_ltap-lgnum
                                                 AND lgtyp = mlgn-ltkze
                                                      AND lgber = 'SL'
                                                      AND anzqu = 0.

            IF sy-subrc = 0.
              SORT ilagp BY sorlp lgpla.

              LOOP AT ilagp.
                e_nlpla = ilagp-lgpla.
                EXIT.
              ENDLOOP.
            ELSE.
              e_nlpla = 'ingnoto!'.
            ENDIF.
          ELSE.
            e_nlpla = 'ingnoto!'.
          ENDIF.

        WHEN OTHERS. "bis auf weiteres wie 1

          SELECT SINGLE * FROM mlgn WHERE matnr = i_ltap-matnr
                                            AND lgnum = i_ltap-lgnum.
          IF sy-subrc = 0 AND NOT mlgn-lgbkz IS INITIAL
                          AND NOT mlgn-ltkze IS INITIAL.

            SELECT SINGLE * FROM t334b WHERE lgnum = i_ltak-lgnum
                                         AND lgtyp = mlgn-ltkze
                                         AND lgbkz = mlgn-lgbkz.
            IF sy-subrc = 0.
              counter = 0.

              DO 10 TIMES.
                CHECK: e_nlpla IS INITIAL.
                CLEAR: wlgber.
                CASE: counter.
                  WHEN 0. wlgber  = t334b-lgbe0.
                  WHEN 1. wlgber  = t334b-lgbe1.
                  WHEN 2. wlgber  = t334b-lgbe2.
                  WHEN 3. wlgber  = t334b-lgbe3.
                  WHEN 4. wlgber  = t334b-lgbe4.
                  WHEN 5. wlgber  = t334b-lgbe5.
                  WHEN 6. wlgber  = t334b-lgbe6.
                  WHEN 7. wlgber  = t334b-lgbe7.
                  WHEN 8. wlgber  = t334b-lgbe8.
                  WHEN 9. wlgber  = t334b-lgbe9.
                ENDCASE.

                CLEAR: ilagp.
         SELECT * INTO TABLE ilagp FROM lagp WHERE lgnum = i_ltap-lgnum
                                                 AND lgtyp = mlgn-ltkze
                                                     AND lgber = wlgber
                                                          AND anzqu = 0
                           .
                IF sy-subrc = 0.
                  SORT ilagp BY sorlp lgpla.

                  LOOP AT ilagp.
                    e_nlpla = ilagp-lgpla.
                    EXIT.
                  ENDLOOP.
                ELSE.
                  counter = counter + 1.
                ENDIF.

              ENDDO.
            ELSE.
              e_nlpla = 'ingnoto!'.
            ENDIF.

          ELSE.
            e_nlpla = 'ingnoto!'.
          ENDIF.
*
      ENDCASE.
    elseif not I_ltap-nlber is initial.
      SELECT * INTO TABLE ilagp FROM lagp WHERE lgnum = i_ltap-lgnum
                                            AND lgtyp = I_ltap-nltyp
                                            AND lgber = I_ltap-nlber
                                            AND anzqu = 0.

      IF sy-subrc = 0.
        SORT ilagp BY sorlp lgpla.

        LOOP AT ilagp.
          e_nlpla = ilagp-lgpla.
          EXIT.
        ENDLOOP.
      ELSE.
        e_nlpla = 'ingnoto!'.
      ENDIF.
    else.
      "Lagerplatz muss sitzen
    endif.
ENDCASE.


CASE: i_ltak-lgnum(1).
  WHEN '6'.
* Auslesen Memory für Lagerbereichsfindung. Wird im
* ZLE_GOODMOVE_01 gesetzt. Dem Umbuchungsreport
    l_memid = 'M_LGBER_ZLEGM10'.
    CLEAR l_lgb.
    IMPORT l_lgb TO l_lgb FROM MEMORY ID l_memid.

    IF NOT l_lgb IS INITIAL.

*   Leerplatzsuche im Bereich

      SELECT SINGLE * FROM mlgn WHERE matnr = i_ltap-matnr
                                  AND lgnum = i_ltap-lgnum.

      IF sy-subrc = 0 AND NOT mlgn-lgbkz IS INITIAL
                      AND NOT mlgn-ltkze IS INITIAL.

        SELECT * INTO TABLE ilagp FROM lagp WHERE lgnum = i_ltap-lgnum
                                                AND lgtyp = mlgn-ltkze
                                                 AND lgber = l_lgb
                                                 AND anzqu = 0.

        IF sy-subrc = 0.
          SORT ilagp BY sorlp lgpla.

          LOOP AT ilagp.
            e_nlpla = ilagp-lgpla.
            EXIT.
          ENDLOOP.
        ELSE.
          e_nlpla = 'ingnoto!'.
        ENDIF.
      ELSE.
        e_nlpla = 'ingnoto!'.
      ENDIF.
    ENDIF.

*23032010; Ende

ENDCASE.

* schreiben Memory für Lagerplatz nach Buchung.
* Wird im ZLE_GOODMOVE_01  gelesen für Rüstliste.
l_memid = 'M_NLGPLA'.
l_lgpla = e_nlpla.
EXPORT l_lgpla TO MEMORY ID l_memid.
