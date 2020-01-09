
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT RLVSDR10 NO STANDARD PAGE HEADING MESSAGE-ID L3.

************************************************************************
*Kundenspezifische Erweiterungen und Aenderungen:
*
* 20031212, Include ersetzt, SAP Stäfa, P. Huber
* 20040130, Druckansteuerung auf 2 Schächte
*           Formular aus Customizing ZLVSTALISTE wird ersetzt durch
*           ZLVSTALISTE2 (T329F-FORMU).
*
************************************************************************
*
* Feb 95       T329P_LESEN korrigiert
* Jan 95       Sondernummer 24-stellig anzeigen
* Korr 12/94   Drucken abhängig von Lagerbewegung bei Sammellliste
* KORR 11/94   Anhängen oder Neuer Eintrag in Spool
* K 8/94     1. Im RLVSMGEF initialisieren Gefahrenvermerke
*            2. bei ABAP-Druck mehrere Seiten drucken können
* Korr 20.1.94   Platz-Positionen werden gemischt falls Platzaufteilung
*                vorhanden
* Korr  4.7.94   ITCPO-Reportname  -> OPEN_FORM
*                -> Reportname wird nachgelesen aus T340D
*---------------------------------------------------------------------*
*  Report RLVSDR10: Druck TA's über Formular                          *
*---------------------------------------------------------------------*
*     Anstoß zum Druck aus TA-Erstellung                              *
*     Anstoß zum Druck aus Transaktion manuell                        *
*---------------------------------------------------------------------*
*  Wahlweise Druck über SAPSCRIPT oder ABAP:                          *
*     dazu nach START-OF-SELECTION  nicht gewünschtes Aussternen      *
*---------------------------------------------------------------------*
*  Modifzieren des Druckoutputs:                                      *
*  1. Um kein ausgeliefertes Programm zu ändern, kopieren Sie sich den*
*     Druckreport unter einem anderen Namen. Diesen Namen hinterlegen *
*     Sie dann im Customizing (Vorgänge-Transporte-Drucksteuerung:    *
*     Druckreport/Lagernummer).                                       *
*                                                                     *
*  2. Die Ansteuerung der SAPSRCIPT-Formulare bzw. die WRITE-Anweisun-*
*     gen beim ABAP-Druck finden sich in folgenden Includes:          *
*         RLVSDR90     SAPSCRIPT-Druck für TA-Einzeldruck             *
*         RLVSDR91     SAPSCRIPT-Druck für TA-Sammelliste             *
*         RLVSDR92     SAPSCRIPT-Druck für TA-Umbuchanweisungen       *
*         RLVSDR93     ABAP-Druck      für TA-Einzeldruck             *
*         RLVSDR94     ABAP-Druck      für TA-Sammelliste             *
*         RLVSDR95     ABAP-Druck      für TA-Umbuchanweisungen.      *
*                                                                     *
*     2.1 Arbeiten Sie mit SAPSCRIPT und wollen Sie lediglich ein     *
*         neues Formular mit der Transaktion SE71 anlegen, brauchen   *
*         Sie 1. nicht durchzuführen. Es genügt nach dem Erstellen des*
*         Formulars dieses in den entsprechenden Tabellen der         *
*         Drucksteuerung im Customizing anzugeben.                    *
*                                                                     *
*     2.2 Arbeiten Sie mit ABAP-Druck, so sollten Sie sich den        *
*         Include, den Sie modifizieren wollen, unter anderem Namen   *
*         z.B. XYZ... kopieren und erst dann ändern.                  *
*                                                                     *
*        Den Include müssen Sie im kopierten Druckreport von 1. be-   *
*         kanntgeben. Suchen Sie mit FIND INCLUDES und fügen Sie      *
*         INCLUDE XYZ... hinzu.                                       *
*                                                                     *
*        Damit das Programm  XYZ... auch anspringt, modifizieren Sie  *
*         den Include RLVSCASE. Dort fügen Sie eine neue DATA-Anwei-  *
*         sung (entsprechend einem neuen Formular-Namen)              *
*         und in den FORM-Routinen neue WHEN ... -Bedingungen         *
*         hinzu.  (Denken Sie daran. daß im Falle von Sammellisten    *
*         oder Kommissionierlisten der Kopf der Liste sowie der Fuss  *
*         eigene CASE-Anweisungen in RLVSCASE haben, die Sie jeweils  *
*         modifizieren müssen.)                                       *
*                                                                     *
*        Da RLVSCASE sowohl vom Druckreport RLVSDR10 als auch vom     *
*         Druckreport für die Kommilisten verwendet wird, müssen Sie  *
*         leider den Include XYZ... auch im Report RLKOMM00 einfügen. *
*                                                                     *
*        Ihr letzter Schritt betrifft die Ansteuerung Ihres neu def.  *
*         Include. Dazu gehen Sie im Customizing Vorgänge-Transporte- *
*         Drucksteuerung auf Druckkennzeichen und geben Sie für ein   *
*         bestimmtes Druckkennzeichen unter Formular den neuen        *
*         'Formular-Namen' ein.                                       *
*                                                                     *
*  3. In der gleichen Tabelle sehen Sie eine Spalte 'Sort'. Die       *
*     Zahlen beziehen sich auf die Nummer der Sortier-Routine im      *
*     Include RLVSINSE. Dort können Sie entweder neue Sortier-rou-    *
*     tinen definieren oder Sie können die Standards benützen.        *
*     Standards sind:                                                 *
*          1 (Routine 1) Keine bes. Sortierung, dh. nach TA-Position  *
*          2             Sort. nach VON-Lagertyp und -Lagerplatz      *
*          3             Sort. nach NACH-Lagertyp und -Lagerplatz     *
*          4             Sort. nach Materialnummer                    *
*          5 - 10        frei                                         *
*                                                                     *
*  4. Tip zum Testen: Der Report RLVSDR10 ist auch direkt startbar    *
*     mit der Möglichkeit den Druck auf den Bildschirm umzuleiten     *
*---------------------------------------------------------------------*
*       Zeitpunkte:                                                   *
*         AT-SELECTION-SCREEN                                         *
*         START-OF-SELECTION                                          *
*         GET LTAK                                                    *
*         GET LTAP                                                    *
*         END-OF-SELECTION                                            *
*                                                                     *
*       Unterroutinen:                                                *
*                                                                     *
*---------------------------------------------------------------------*


TABLES: LTAK, LTAP, *LTAP, RLDRU, *RLDRU, MGEF, ITCPO, USR01,
        T300, T300T, T331, T333B, T646G, T646H, TSP03,
        T329F, T329D, T329P, *T329P, T329A, T301T, *T301T, LQUA,
        RL03T, *RL03T, LEIN, T302T, *T302T, T340D.
DATA:   DRUCKVERBUCHER            TYPE C.  "Steuert ob Druck aus Verbu.

*>>>>>>>>>BEGIN_INSERTION HP_300480>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data: humla type c.
*<<<<<<<<<END_INSERTION HP_300408<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

PARAMETERS:
        DRUCKKZ  LIKE RLDRU-DRUKZ,
        EDRUCKER LIKE RLDRU-LDEST,
        SPOOLPAR LIKE RLDRU-SPOOL,
        DRUCKEN(1)    TYPE C DEFAULT 'X'.    "Kz Liste drucken

SELECTION-SCREEN SKIP 1.

PARAMETERS:
        TASCH          DEFAULT 'X',    "Druck TA-Schein
        LESCH          DEFAULT 'X',    "Druck LE-Schein
        LETASCH        DEFAULT 'X',    "Druck LE-TA-Schein
        LEINH          DEFAULT 'X'.    "Druck LE-Inhaltsverzeichnis

*---------------------------------------------------------------------*
*        Interne Tabellen                                             *
*---------------------------------------------------------------------*

*........Tabelle der TA-Positionen.....................................

DATA:   BEGIN OF TAP OCCURS 50.
          INCLUDE STRUCTURE LTAP.
          INCLUDE STRUCTURE LTAP1.
DATA:   END OF TAP.

DATA: BEGIN OF LSPERR OCCURS 50.
        INCLUDE STRUCTURE LEIN.
DATA: END OF LSPERR.

DATA: BEGIN OF QSPERR OCCURS 100.
        INCLUDE STRUCTURE LQUA.
DATA: END OF QSPERR.

*........Interne Tabelle zur Aufnahme der Druckinformationen zu dem.....
*........Druck von Palettenscheinen. ...................................
*........(Achtung: wird auch gebraucht im RLKOMM00-Kommilistendruck)....

DATA: BEGIN OF INT_LEDRUCK OCCURS 100,
        DRUKZ      LIKE T329F-DRUKZ,
        FORMU      LIKE T329F-FORMU,
        TDDELETE   LIKE T329P-TDDELETE,
        TDNEWID    LIKE T329P-TDNEWID,                      "KORR 11/94
        TDIMMED    LIKE T329P-TDIMMED,
        TDDATASET  LIKE T329P-TDDATASET,
        TDCOPIES   LIKE T329P-TDCOPIES,
        LDEST      LIKE T329D-LDEST,
        LE_SCHTYP,
        SORCO(4),
        LENUM      LIKE LEIN-LENUM,
        VLTYP      LIKE LTAP-VLTYP,
        VLBER      LIKE LTAP-VLBER,
        VLPLA      LIKE LTAP-VLPLA,
        NLTYP      LIKE LTAP-NLTYP,
        NLBER      LIKE LTAP-NLBER,
        NLPLA      LIKE LTAP-NLPLA,
        LGNUM      LIKE LTAP-LGNUM,
        NIDR2      LIKE T329D-NIDR2,
        NIDR3      LIKE T329D-NIDR3,
        NIDR4      LIKE T329D-NIDR4,
        WDATU      LIKE LTAP-WDATU,
        WENUM      LIKE LTAP-WENUM,
        WEPOS      LIKE LTAP-WEPOS,
        VFDAT      LIKE LTAP-VFDAT,
        ZEUGN      LIKE LTAP-ZEUGN,
      END OF INT_LEDRUCK.

*---------------------------------------------------------------------*
*        Field-Groups                                                 *
*---------------------------------------------------------------------*

FIELD-GROUPS: HEADER, POSTEN.

*---------------------------------------------------------------------*
*        Einzelfelder                                                 *
*---------------------------------------------------------------------*
data etikett type c.
DATA: FLG_LGTYP_BARCODE(1)     TYPE C VALUE ' '.
DATA:  DRUCK_ID(8)         TYPE C   VALUE 'LVSDRUCK'.
DATA:   FLG_TADRUCK_AUT           TYPE C,
        FLG_TADRUCK_MAN           TYPE C,
        FLG_TADRUCK_WIE           TYPE C,       " Sapscript oder Abap
        FLG_TAPLG1                TYPE C,
        FLG_TAPLG2                TYPE C,
        FLG_DRUCK_UMBUCH          TYPE C,
        FLG_ERSTER_UMBUCH         TYPE C,
        ERSTER_DURCHLAUF          TYPE C.

DATA:   KZ_SORT_VONLAGERTYP       TYPE C,
        KZ_NEUE_SEITE             TYPE C.


DATA:   HLP_TAPOS                 LIKE LTAP-TAPOS,
        HLP_TAPOS_UMBUCH          LIKE LTAP-TAPOS,
        SAV_TAPOS                 LIKE LTAP-TAPOS,
        CNT_PAGE(4)               TYPE C,            "f.Sammelliste
        CNT_SAMML                 TYPE P.            "Anz. Pos./Blatt

*........Hilfsfelder für das Lesen der Tabelle T329P..................

DATA:   DRUCK_LGNUM               LIKE LTAK-LGNUM,
        DRUCK_VLTYP               LIKE LTAP-VLTYP,
        DRUCK_NLTYP               LIKE LTAP-NLTYP.

*........Fleder wegen gemeinsamer Benutzung von RLVSDR96 mit RLKOMM00...

DATA:   FORMULAR                  LIKE T329F-FORMU.

*---------------------------------------------------------------------*
*        Konstanten                                                   *
*---------------------------------------------------------------------*

DATA:   DRUCKAUFBEREITUNG(16)     TYPE C VALUE 'X_PAPER_NT      ',
        KZINV_PE                  LIKE T331-KZINV VALUE 'PE',
        KZINV_PN                  LIKE T331-KZINV VALUE 'PN',
        CON_NULKO_JA_S            LIKE LTAP-NULKO VALUE '1',
        CON_NULKO_LEER_S          LIKE LTAP-NULKO VALUE '3',
        CON_NULKO_BELEGT_S        LIKE LTAP-NULKO VALUE '5',
        CON_BLANK                 LIKE LTAP-VORGA VALUE '  ',
        CON_EINS                  LIKE LTAP-TAPOS VALUE '0001',
        CON_X                     LIKE LTAP-VORGA VALUE 'X',
        CON_TB                    LIKE LTAP-VORGA VALUE 'TB',
        CON_TRUE                  TYPE C          VALUE 'X',
        CON_FALSE                 TYPE C          VALUE ' ',
        CON_U1                    LIKE LTAP-VORGA VALUE 'U1',
        CON_U2                    LIKE LTAP-VORGA VALUE 'U2',
        CON_ULINE(78)             TYPE C          VALUE '_____________',
        CON_LEIN_BILDEN           LIKE LEIN-STATU VALUE '4',
        CON_SCHEIN_LE             VALUE '1',      "LE-Schein
        CON_SCHEIN_LETA           VALUE '2',      "LE-TA-Schein
        CON_SCHEIN_LEI            VALUE '3',      "LE-Inhaltsverzeichnis
        CON_LEIN_UMLAGERN         VALUE '1',
        CON_LEIN_UMLAGERN_A       VALUE 'A',
        CON_LEIN_UMBUCHEN         VALUE '5',
        CON_VORGA_TL              LIKE T342-VORGA VALUE 'TL',
        RFLTN(18)                 TYPE C,
        TAPLG(19)                 TYPE C,
       *TAPLG(19)                 TYPE C,
        TALGN(15)                 TYPE C,
        H_KOMMA                   TYPE C          VALUE '''',
        MASK3(3)                  TYPE C          VALUE '***',
        SAPSCRIPT                 TYPE C          VALUE 'S',
        ABAP                      TYPE C          VALUE 'A'.
*       CON_PROGRAM_DATA          LIKE SY-REPID   VALUE 'RLVSDR10'.
                                               "Korr 4.7.94
*---------------------------------------------------------------------*
*        Kundenspezifische Erweiterungen                              *
*---------------------------------------------------------------------*
*
constants:   ZFORMU   LIKE T329F-FORMU value 'ZLVSTALISTE2'.  "20040130

*---------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                          *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION_SCREEN.

*---------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM START_OF_SELECTION.

*---------------------------------------------------------------------*
*        GET LTAK                                                     *
*---------------------------------------------------------------------*
GET LTAK.
  PERFORM GET_LTAK.

*---------------------------------------------------------------------*
*        GET LTAP                                                     *
*---------------------------------------------------------------------*
GET LTAP.
  PERFORM GET_LTAP.

*---------------------------------------------------------------------*
*        END-OF-SELECTION                                             *
*---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM END_OF_SELECTION.

*---------------------------------------------------------------------*
*       Unterprogramme, die zu den einzelnen Reportereignissen        *
*       aufgerufen werden:                                            *
*                                                                     *
*       AT_SELECTION_SCREEN                                           *
*       START_OF_SELECTION                                            *
*       GET_LTAK                                                      *
*       GET_LTAP                                                      *
*       END_OF_SELECTION                                              *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       AT SELECTION-SCREEN                                           *
*---------------------------------------------------------------------*
FORM AT_SELECTION_SCREEN.

*........Prüfen das eingegebene Druckkennzeichen.......................

  IF DRUCKKZ NE SPACE.
    PERFORM T329F_LESEN USING T4_LGNUM DRUCKKZ.
    IF SY-SUBRC NE 0.
      MESSAGE A118 WITH T4_LGNUM DRUCKKZ.
    ENDIF.
  ENDIF.

*........Prüfen den eingegebenen Drucker...............................

  IF EDRUCKER NE SPACE.
    PERFORM TSP03_LESEN USING EDRUCKER.
    IF SY-SUBRC NE 0.
      MESSAGE A119 WITH EDRUCKER.
    ENDIF.
  ENDIF.

*........Prüfen das eingegebene Spoolparameterkennzeichen..............

  IF SPOOLPAR NE SPACE.
    PERFORM T329P_LESEN USING T4_LGNUM SPOOLPAR.
    IF SY-SUBRC NE 0.
      MESSAGE A116 WITH T4_LGNUM SPOOLPAR.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
FORM START_OF_SELECTION.

*........Schalter, ob TA-Druck über SAPSCRIPT oder ABAP.................

* FLG_TADRUCK_WIE = ABAP.         " ABAP
  FLG_TADRUCK_WIE = SAPSCRIPT.    " SAPSCRIPT

*........Prüfen, ob TA-Druck manuell oder aus TA-Erstellung............

  MOVE CON_X TO FLG_TADRUCK_MAN.
  IMPORT FLG_TADRUCK_AUT FROM MEMORY ID DRUCK_ID.
  IF NOT FLG_TADRUCK_AUT IS INITIAL.
    CLEAR FLG_TADRUCK_MAN.
  ENDIF.

*........Importieren von QPSERR und LSPERR aus Memory

  IF NOT T4_KZMEM IS INITIAL.
*   import qsperr
*          lsperr from memory.
    IMPORT QSPERR
           LSPERR FROM MEMORY ID DRUCK_ID.
  ENDIF.

*........Definition der Posten für Field-Groups.........................

  PERFORM POSTEN_INSERT.

ENDFORM.

*---------------------------------------------------------------------*
*       GET LTAK                                                      *
*---------------------------------------------------------------------*
FORM GET_LTAK.

*........Read T340D-REPID once to def. Work-area for data -> open_form
  PERFORM T340D_LESEN USING LTAK-LGNUM.
*........Druckkennzeichen..............................................

  IF DRUCKKZ IS INITIAL.

*........Druckkennzeichen aus dem TA-Kopf..............................
    IF LTAK-DRUKZ IS INITIAL.
      MESSAGE A114.
    ELSE.

      PERFORM T329F_LESEN USING LTAK-LGNUM LTAK-DRUKZ.
      IF SY-SUBRC NE 0.
        MESSAGE A118 WITH LTAK-LGNUM LTAK-DRUKZ.
      ENDIF.

    ENDIF.
  ELSE.

    LTAK-DRUKZ = DRUCKKZ.
*........Druckkennzeichen manuell selekt. ( geprüft bei AT SELECTION ).
  ENDIF.

*........Lesen d.Inserts in die Field-groups zwecks Sortierreihenfolge
*........Erst hier wird die T329F_SORNR bekannt........................

  PERFORM T329F-SORNR OF SORT01
                         SORT02
                         SORT03
                         SORT04
                         SORT05
                         SORT06
                         SORT07
                         SORT08
                         SORT09
                         SORT10.

*........Spoolparameter................................................

  IF SPOOLPAR IS INITIAL.

*........Spoolparameter aus Tabelle T329F..............................
    IF T329F-SPOOL IS INITIAL.
      MESSAGE A115 WITH T329F-LGNUM T329F-DRUKZ.
    ELSE.

      PERFORM T329P_LESEN USING LTAK-LGNUM T329F-SPOOL.
      IF SY-SUBRC NE 0.
        MESSAGE A116 WITH LTAK-LGNUM T329F-SPOOL.
      ENDIF.

    ENDIF.
  ELSE.

*........Spoolparameter manuell selekt. ( geprüft bei AT SELECTION )...
  ENDIF.

  *T329P = T329P.

*........Prüfen, ob Wiederholdruck ....................................

  IF NOT LTAK-DRUCK IS INITIAL.        " Mimik für Wiederholdruck
    MOVE TEXT-002 TO RLDRU-WIEDR.      " Druckwiederholung
  ENDIF.

*........Lesen Text zur Transportart..................................

  PERFORM T333B_LESEN USING LTAK-LGNUM LTAK-TRART.

ENDFORM.

*---------------------------------------------------------------------*
*       GET LTAP                                                      *
*---------------------------------------------------------------------*
FORM GET_LTAP.

*........Nur drucken, falls TA manuell oder aus TB, nicht drucken,.....
*........wenn TA für Umbuchanweisung. Dafür gibts separaten Druck......

  CASE LTAP-VORGA.
    WHEN CON_BLANK.
    WHEN CON_TB.
    WHEN CON_U1.
      MOVE CON_TRUE TO FLG_DRUCK_UMBUCH.
      *LTAP-TAPOS = LTAP-TAPOS.
      HLP_TAPOS_UMBUCH = LTAP-TAPOS.
      *LTAP-LDEST = LTAP-LDEST.
    WHEN CON_U2.
      MOVE CON_TRUE TO FLG_DRUCK_UMBUCH.
      HLP_TAPOS = LTAP-TAPOS - *LTAP-TAPOS.
      IF HLP_TAPOS = CON_EINS.
        LTAP-LDEST = *LTAP-LDEST.
        HLP_TAPOS_UMBUCH  = *LTAP-TAPOS.
      ELSE.
        MESSAGE A121.   " Bitte Sortierung überprüfen
      ENDIF.
  ENDCASE.

*........Ohne Druckervorschlag nicht drucken, außer manueller Druck....

  IF FLG_TADRUCK_MAN IS INITIAL AND FLG_DRUCK_UMBUCH IS INITIAL.
    IF LTAP-LDEST IS INITIAL.
      EXIT.
    ENDIF.
  ENDIF.

*.......Dient TA als Inventurbeleg, dann Andrucken Hinweis..............

  IF LTAP-KZINV = KZINV_PE AND NOT LTAP-IVNUM IS INITIAL.
    MOVE TEXT-003 TO RLDRU-TEXT2.               " Nachplatz inventieren
  ELSE.

*........Nullkontrolle andrucken wenn systemseitig erkannt..............

    IF LTAP-NULKO = CON_NULKO_JA_S   OR
       LTAP-NULKO = CON_NULKO_LEER_S OR
       LTAP-NULKO = CON_NULKO_BELEGT_S.
      MOVE TEXT-004 TO RLDRU-TEXT2.             " Vonplatz inventieren
    ELSE.
      CLEAR RLDRU-TEXT2.
    ENDIF.
  ENDIF.

*........Wenn kein Sammeldruck ist Formular aus T329D erlaubt...........

  IF T329F-SAMML IS INITIAL.

    PERFORM T329D_EINTRAG USING LTAP-LGNUM LTAP-VLTYP LTAP-NLTYP.

*........Ist das Spoolkz in T329D, überschreibt es gegf. das aus T329F..

    T329P = *T329P.
    IF NOT T329D-SPOOL IS INITIAL.
      PERFORM T329P_LESEN USING LTAP-LGNUM T329D-SPOOL.
    ENDIF.
    IF NOT T329D-SPOO2 IS INITIAL.
      T329F-SPOO2 = T329D-SPOO2.
    ENDIF.
    IF NOT T329D-SPOO3 IS INITIAL.
      T329F-SPOO3 = T329D-SPOO3.
    ENDIF.
    IF NOT T329D-SPOO4 IS INITIAL.
      T329F-SPOO4 = T329D-SPOO4.
    ENDIF.

*........T329D-Eintrag überschreibt gegf. T329F-Formulare..............

    IF NOT T329D-FORMU IS INITIAL.
      MOVE T329D-FORMU TO T329F-FORMU.
    ENDIF.
    IF NOT T329D-FORM2 IS INITIAL.
      MOVE T329D-FORM2 TO T329F-FORM2.
    ENDIF.
    IF NOT T329D-FORM3 IS INITIAL.
      MOVE T329D-FORM3 TO T329F-FORM3.
    ENDIF.
    IF NOT T329D-FORM4 IS INITIAL.
      MOVE T329D-FORM4 TO T329F-FORM4.
    ENDIF.

  ENDIF.

*........Drucker.......................................................

*........Drucker manuell selektiert....................................

  IF NOT EDRUCKER IS INITIAL.
    MOVE EDRUCKER TO LTAP-LDEST.
  ENDIF.

*........Keine Drucker in der TA-Pos vorhanden, muss ermittelt werden..

  IF NOT T329F-SAMML IS INITIAL.                            "KORR 12/94
    PERFORM T329D_EINTRAG USING LTAP-LGNUM LTAP-VLTYP LTAP-NLTYP.  "Korr
  ENDIF.                                                    "KORR 12/94

  IF LTAP-LDEST IS INITIAL.
    IF NOT T329F-SAMML IS INITIAL.
      PERFORM DRUCKER_MAN_BESTIMMEN.
    ENDIF.
  ENDIF.

  CHECK NOT LTAP-LDEST IS INITIAL.

*........Nach Bestimmen des "Pooldruckers": Bestimmen der Drucker für...
*........den Druck der verschiedenen Scheine bzgl. LE-Verwaltung........

  PERFORM T329A_LESEN USING LTAP-LGNUM LTAP-LDEST.
  IF SY-SUBRC = 0.
    IF T329A-LDES2 IS INITIAL. T329A-LDES2 = LTAP-LDEST. ENDIF.
    IF T329A-LDES3 IS INITIAL. T329A-LDES3 = LTAP-LDEST. ENDIF.
    IF T329A-LDES4 IS INITIAL. T329A-LDES4 = LTAP-LDEST. ENDIF.
  ELSE.
    T329A-LDES2 = LTAP-LDEST.
    T329A-LDES3 = LTAP-LDEST.
    T329A-LDES4 = LTAP-LDEST.
  ENDIF.

*........Platz-Positionen-mischen.......................................

  PERFORM PLATZ_POSITIONEN_MISCHEN.                  "Korr 20.1.94

*........Sondernummer ggf. in externes Format bringen ......... "Jan 95

  CLEAR RL03T-LSONR.                                            "Jan 95
  PERFORM SONUM_CONV_INT_EXT(SAPFL000) USING LTAP-SOBKZ         "Jan 95
                                             LTAP-SONUM         "Jan 95
                                             RL03T-LSONR.       "Jan 95

  IF NOT LTAP-SONUM IS INITIAL AND RL03T-LSONR IS INITIAL.      "Jan 95
    MOVE LTAP-SONUM TO RL03T-LSONR.                             "Jan 95
  ENDIF.                                                        "Jan 95
*........Extract........................................................

  EXTRACT POSTEN.

ENDFORM.

*---------------------------------------------------------------------*
*       END-OF-SELECTION                                              *
*---------------------------------------------------------------------*
FORM END_OF_SELECTION.

  MOVE CON_X TO ERSTER_DURCHLAUF.
  CNT_PAGE = 1.                      "Seitenzähler für Sammelliste

*.......Sortieren Extract nach den Feldern im Header....................

  SORT.

*---------------------------------------------------------------------*
*........Ausdruck der TA-Scheine......................................*
*........(wenn Parameter "Druck TA-Schein" gesetzt ist)...............*
*---------------------------------------------------------------------*

  IF TASCH = CON_X.

*.......Einzelne TA-Positionen drucken BZW Sammelliste.................

    IF FLG_DRUCK_UMBUCH NE CON_TRUE.

      LOOP.

*........Prüfen, ob die TA-Position ünerhaupt gemäß T329D-Einstellung...
*........gedruckt werden darf. .........................................

        CHECK T329D-NIDRU <> CON_X.

*.......Wechsel VON-Lagertyp ( nur bei Sammellisten )...................

        AT NEW LTAP-VLTYP.
          IF KZ_SORT_VONLAGERTYP EQ CON_X AND
             NOT T329F-SAMML IS INITIAL   AND
             ERSTER_DURCHLAUF    NE CON_X.
            PERFORM NEUE_SEITE.
          ENDIF.
        ENDAT.

*.......Beim Wechsel von Druckparameter und  Drucker....................
*.......bzw. Formular den Druck ansteuern...............................

        AT NEW LTAP-LDEST.
          IF T329F-SAMML IS INITIAL.
            PERFORM DRUCK_ANSTEUERN.
          ENDIF.
        ENDAT.

        AT NEW LTAK-TANUM.
          IF NOT T329F-SAMML IS INITIAL.
            PERFORM DRUCK_ANSTEUERN.
          ENDIF.
        ENDAT.

*.......Drucken Einzeldruck oder in Sammelliste.........................

        IF NOT T329F-SAMML IS INITIAL.
          PERFORM SAMMELLISTE_DRUCKEN.    " Drucken Pos in Sammelliste
        ELSE.
          PERFORM EINZELTA_DRUCKEN.       " Drucken Positionen einzeln
        ENDIF.
        MOVE SPACE TO ERSTER_DURCHLAUF.
        MOVE SPACE TO KZ_NEUE_SEITE.

*.......Beim Ende von Druckparameter und Drucker - Formular schliessen..

        AT END OF LTAK-TANUM.
          IF NOT T329F-SAMML IS INITIAL.
            PERFORM FORMULAR_SCHLIESSEN.
          ENDIF.
        ENDAT.
      ENDLOOP.


************************************************************************
* Zweites Mal mit anderem Formular wegen Schachtsteuerung
************************************************************************
*Begin: 20040130
LOOP.

write zformu to T329F-FORMU.

*........Prüfen, ob die TA-Position ünerhaupt gemäß T329D-Einstellung...
*........gedruckt werden darf. .........................................

        CHECK T329D-NIDRU <> CON_X.

*.......Wechsel VON-Lagertyp ( nur bei Sammellisten )...................

        AT NEW LTAP-VLTYP.
          IF KZ_SORT_VONLAGERTYP EQ CON_X AND
             NOT T329F-SAMML IS INITIAL   AND
             ERSTER_DURCHLAUF    NE CON_X.
            PERFORM NEUE_SEITE.
          ENDIF.
        ENDAT.

*.......Beim Wechsel von Druckparameter und  Drucker....................
*.......bzw. Formular den Druck ansteuern...............................

        AT NEW LTAP-LDEST.
          IF T329F-SAMML IS INITIAL.
            PERFORM DRUCK_ANSTEUERN.
          ENDIF.
        ENDAT.

        AT NEW LTAK-TANUM.
          IF NOT T329F-SAMML IS INITIAL.
            PERFORM DRUCK_ANSTEUERN.
          ENDIF.
        ENDAT.

*.......Drucken Einzeldruck oder in Sammelliste.........................

        IF NOT T329F-SAMML IS INITIAL.
          PERFORM SAMMELLISTE_DRUCKEN.    " Drucken Pos in Sammelliste
        ELSE.
          PERFORM EINZELTA_DRUCKEN.       " Drucken Positionen einzeln
        ENDIF.
        MOVE SPACE TO ERSTER_DURCHLAUF.
        MOVE SPACE TO KZ_NEUE_SEITE.

*.......Beim Ende von Druckparameter und Drucker - Formular schliessen..

        AT END OF LTAK-TANUM.
          IF NOT T329F-SAMML IS INITIAL.
            PERFORM FORMULAR_SCHLIESSEN.
          ENDIF.
        ENDAT.
      ENDLOOP.

************************************************************************
* Zweites Mal mit anderem Formular wegen Schachtsteuerung
* ENDE: 20040130
************************************************************************

*........Endverarbeitung für Einzel-TA's und Sammelliste...............

      IF ERSTER_DURCHLAUF <> CON_X.
        IF T329F-SAMML IS INITIAL.
          PERFORM FORMULAR_SCHLIESSEN.
        ENDIF.
      ENDIF.

    ELSE.

*.......Drucken Umbuchanweisung........................................

      IF FLG_DRUCK_UMBUCH = CON_TRUE.
        MOVE CON_TRUE TO FLG_ERSTER_UMBUCH.     " Nur f. Umbuchungen rel
        LOOP.
          PERFORM UMBUCHUNGEN_DRUCKEN.
        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDIF. "TASCH = CON_X

*........Druck der Scheine zu den Lagereinheiten......................*
*........(wenn entsprechende Parameter gesetzt sind)..................*

  IF LESCH = CON_X OR LETASCH = CON_X OR LEINH = CON_X.
    PERFORM LE_DRUCK.  "in Include-RLVSDR96
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        INCLUDES                                                     *
*---------------------------------------------------------------------*

*........Definition der Field-Groups Inserts ( HEADER + POSTEN ).......
INCLUDE RLVSINSE.

*........Verzweigen auf verschiedene Möglichkeiten des ABAP-Drucks.....
INCLUDE RLVSCASE.

*........Zulesen der Gefahrenvermerke...................................
INCLUDE RLVSMGEF.

*........Externer Aufruf des Druckprogramms.............................
INCLUDE RLVSEXTE.

*........Hier Einfügen neue Includes für Eigen-Entwicklung.............
*........Zulesen der Druckroutine  ....                   .............
*  INCLUDE XYZ...  .


*........Zulesen der Druckroutine  für SAPSCRIPT-Druck Einzel-TA.......
INCLUDE RLVSDR90.

*........Zulesen der Druckroutine  für ABAP-Druck Einzel-TA............
INCLUDE RLVSDR93.

*........Drucken von Lagereinheiten. ..................................
INCLUDE RLVSDR96.

*........Zulesen der Druckroutine  für SAPSCRIPT-Druck Umbuchungen.....
INCLUDE RLVSDR92.

*........Zulesen der Druckroutine  für ABAP-Druck Umbuchungen..........
INCLUDE RLVSDR95.

*........Zulesen der Druckroutine  für SAPSCRIPT-Druck Sammelliste.....
*20031212, Beginn, SAP Stäfa, P. Huber
*ZRLVSDR91 ersetzt die RLVSDR91
INCLUDE ZRLVSDR91.
*20031212, Ende, SAP Stäfa, P. Huber

*........Zulesen der Druckroutine  für ABAP-Druck Sammelliste .........
INCLUDE RLVSDR94.

*........DruckRouine für ABAP-Druck der Kommiliste 1 ..................*
INCLUDE RLKOMM92.

*........DruckRouine für ABAP-Druck der Kommiliste 2 ..................*
INCLUDE RLKOMM93.

*---------------------------------------------------------------------*
*        Unterroutinen:                                               *
*        --------------                                               *
*                                                                     *
*        FORM BARCODE_TAPLG               Verscchl. Barcode TAPLG     *
*        FORM DRUCKEN_ABAP_MAIN           Verzweigungsroutinen für    *
*        FORM DRUCKEN_ABAP_KOPF           verschiedene Möglichkeiten  *
*        FORM DRUCKEN_ABAP_FUSS           des zB. Einzeldrucks in ABAP*
*        FORM EINZELTA_DRUCKEN            gemeinsames f.ABAP bzw.SScr.*
*        FORM EINZELTA_DRUCKEN_SAPSCRIPT                              *
*        FORM EINZELTA_DRUCKEN_ABAP                                   *
*        FORM SAMMELLISTE_DRUCKEN                                     *
*        FORM SAMMELLISTE_DRUCKEN_SAPSCRIPT                           *
*        FORM SAMMELLISTE_DRUCKEN_ABAP                                *
*        FORM SAMMELLISTE_KOPF                                        *
*        FORM SAMMELLISTE_KOPF_SAPSCRIPT                              *
*        FORM SAMMELLISTE_KOPF_ABAP                                   *
*        FORM SAMMELLISTE_ENDE                                        *
*        FORM UMBUCHUNGEN_DRUCKEN                                     *
*        FORM UMBUCHUNGEN_DRUCKEN_SAPSCRIPT                           *
*        FORM UMBUCHUNGEN_DRUCKEN_ABAP                                *
*                                                                     *
*        FORM DRUCK_ANSTEUERN                                         *
*        FORM FORMULAR_OEFFNEN                                        *
*        FORM FORMULAR_SCHLIESSEN                                     *
*        FORM NEUE_SEITE                                              *
*                                                                     *
*        FORM DRUCKER_MAN_BESTIMMEN                                   *
*        FORM T329D_EINTRAG                                           *
*                                                                     *
*        FORM TSP03_LESEN                                             *
*        FORM T300_LESEN                                              *
*        FORM T329D_LESEN                                             *
*        FORM T329F_LESEN                                             *
*        FORM T329P_LESEN                                             *
*        FORM T331_LESEN                                              *
*        FORM T333B_LESEN                                             *
*        FORM USR01_LESEN                                             *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*        FORM BARCODE_TAPLG                                           *
*---------------------------------------------------------------------*
*        Barcodeverschlüsselung mit TA-Nummer,-Position und Lgnum     *
*---------------------------------------------------------------------*
FORM BARCODE_TAPLG.
  WRITE LTAK-TANUM TO RLDRU-TAPLG(10).   " EinzelPositionsDruck
  WRITE LTAP-TAPOS TO RLDRU-TAPLG+10(4).
  WRITE LTAK-LGNUM TO RLDRU-TAPLG+14(3).

  IF FLG_TADRUCK_WIE = ABAP.
    WRITE H_KOMMA TO TAPLG(1).             "sonst andere Print-Contr.
    WRITE RLDRU-TAPLG TO TAPLG+1(17).
    WRITE H_KOMMA TO TAPLG+18(1).
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*        FORM EINZELTA_DRUCKEN                                        *
*---------------------------------------------------------------------*
*        Druckvorbereitung                                            *
*---------------------------------------------------------------------*
FORM EINZELTA_DRUCKEN.

*........Gefahrenvermerke ermitteln....................................

    PERFORM T300_LESEN USING LTAK-LGNUM.  " Regionalkennzeichen zulesen
    PERFORM GEF_VERMERKE.                 " Gefahrenvermerke
    IF SY-SUBRC = 0.
      MOVE TEXT-001 TO RLDRU-TEXT1.
    ENDIF.

*........Für Barcodeverschlüsselung ein Feld für TA- und Lagernummer...

    PERFORM BARCODE_TAPLG.

   CASE FLG_TADRUCK_WIE.
     WHEN SAPSCRIPT.
       PERFORM EINZELTA_DRUCKEN_SAPSCRIPT.
     WHEN ABAP.
       PERFORM DRUCKEN_ABAP_MAIN.    "allgemeine Druckroutine für ABAP
   ENDCASE.                          "dort Verzweigg. in versch. Listen


ENDFORM.

*---------------------------------------------------------------------*
*        FORM UMBUCHUNGEN_DRUCKEN                                     *
*---------------------------------------------------------------------*
*        Druckvorbereitung                                            *
*---------------------------------------------------------------------*
*        Anstoßen des Drucks der Umbuchungsanweisung                  *
*        Zwei zusammengehörige Tapos haben gleiches HLP_TAPOS_UMBUCH  *
*        Damit ist gewährleistet, daß sie hintereinander stehen.      *
*        Für die allererste muß das FLG_ERSTER_UMBUCH gesetzt sein    *
*---------------------------------------------------------------------*
FORM UMBUCHUNGEN_DRUCKEN.

*........Ausdruck und Bearbeitung 2.Position der Umbuchanweisung......*
  IF FLG_ERSTER_UMBUCH = CON_FALSE AND HLP_TAPOS_UMBUCH = HLP_TAPOS.
*........Barcodeverschlüsselung von 2. Umbuchungsposition.............*
    IF NOT LTAP-PQUIT IS INITIAL.   "Ist Position quittiert ??
      FLG_TAPLG2 = SPACE.
    ELSE.                           "sonst nicht quittiert
      MOVE 1 TO FLG_TAPLG2.    "Drucken nur wenn PQUIT nicht gesetzt
      PERFORM BARCODE_TAPLG.
    ENDIF.


*........Umbuchung am Platz ?.........................................*

    IF LTAP-NLPLA = *LTAP-VLPLA.
      MOVE TEXT-005 TO RLDRU-TEXT1.
    ELSE.
      MOVE SPACE TO RLDRU-TEXT1.
    ENDIF.
*........Verzweigen zu ABAP- oder SAPSCRIPT-Druck.....................*
    CASE FLG_TADRUCK_WIE.
      WHEN SAPSCRIPT.
        PERFORM UMBUCHUNGEN_DRUCKEN_SAPSCRIPT.
      WHEN ABAP.
        PERFORM DRUCKEN_ABAP_MAIN.
    ENDCASE.

*........Weitere Verarbeitung.........................................*
     FLG_ERSTER_UMBUCH = CON_TRUE.
  ELSE.

*........Verarbeitung 1. Umbuchungsposition einer Anweisung...........*
    *LTAP = LTAP.
    *RL03T = RL03T.
    HLP_TAPOS = HLP_TAPOS_UMBUCH.
    FLG_ERSTER_UMBUCH = CON_FALSE.

    IF *LTAP-RLTYP = SPACE.
      MOVE SPACE TO *LTAP-MEINS.   "nur Rücklag.,sonst LTAP-MEINS anz.
      MOVE SPACE TO *LTAP-ALTME.   "nur Rücklag.,sonst LTAP-altme anz.
    ENDIF.
*........Barcodeverschlüsselung von 1. Umbuchungsposition.............*
    IF NOT LTAP-PQUIT IS INITIAL.
      FLG_TAPLG1 = SPACE.
    ELSE.
      MOVE 1 TO FLG_TAPLG1.    "Drucken nur wenn PQUIT nicht gesetzt
      PERFORM BARCODE_TAPLG.
      MOVE TAPLG TO *TAPLG.               " für ABAP
      MOVE RLDRU-TAPLG TO *RLDRU-TAPLG.   " für SAPSCRIPT
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM SAMMELLISTE_DRUCKEN                                     *
*---------------------------------------------------------------------*
*        Vorbereitung Sammeldruck                                     *
*---------------------------------------------------------------------*
FORM SAMMELLISTE_DRUCKEN.

*........Verzweigen zu ABAP- oder SAPSCRIPT-Druck.....................*
    CASE FLG_TADRUCK_WIE.
      WHEN SAPSCRIPT.
        PERFORM SAMMELLISTE_DRUCKEN_SAPSCRIPT.
      WHEN ABAP.
        PERFORM DRUCKEN_ABAP_MAIN.
    ENDCASE.
ENDFORM.
*---------------------------------------------------------------------*
*        FORM SAMMELLISTE_KOPF                                        *
*---------------------------------------------------------------------*
*        Vorbereitung Sammeldruck Kopfdaten                           *
*---------------------------------------------------------------------*
FORM SAMMELLISTE_KOPF.

  CHECK NOT T329F-SAMML IS INITIAL.         " nur Sammelliste

*.........Füllen der Multifunktionsfelder zwecks Barcodeausgabe.........

  WRITE:                                    "ListenDruck
        LTAK-TANUM TO RLDRU-TALGN(10),      "..TA-Nummer
        LTAK-LGNUM TO RLDRU-TALGN+10(3).    "..Lagernummer

*........Verzweigen zu ABAP- oder SAPSCRIPT-Druck.....................*
    CASE FLG_TADRUCK_WIE.
      WHEN SAPSCRIPT.
        PERFORM SAMMELLISTE_KOPF_SAPSCRIPT.
      WHEN ABAP.
        WRITE H_KOMMA TO TALGN(1).        "für ABAP-Druck bes. Format,
        WRITE RLDRU-TALGN TO TALGN+1(13). "sonst andere Print-Controls
        WRITE H_KOMMA TO TALGN+14(1).

        PERFORM DRUCKEN_ABAP_KOPF.     "dort Verzweigg. f. versch.
    ENDCASE.                           "Sammellisten
ENDFORM.

*---------------------------------------------------------------------*
*        FORM SAMMELLISTE_ENDE   "for later  use                      *
*---------------------------------------------------------------------*
*        Aufbereitung der Formulardaten für Sammelliste, die am       *
*        Ende des Formulars angedruckt werden                         *
*---------------------------------------------------------------------*
FORM SAMMELLISTE_ENDE.

  CHECK NOT T329F-SAMML IS INITIAL.         " nur Sammelliste

ENDFORM.

*---------------------------------------------------------------------*
*        FORM DRUCK_ANSTEUERN                                         *
*---------------------------------------------------------------------*
*        Anstoßen des DRUCKS  über SAPSCRIPT                          *
*---------------------------------------------------------------------*
FORM DRUCK_ANSTEUERN.

*........Das abgearbeitete Formular abschliessen.......................

  IF ERSTER_DURCHLAUF NE CON_X.
    PERFORM SAMMELLISTE_ENDE.
    IF T329F-SAMML IS INITIAL.
      PERFORM FORMULAR_SCHLIESSEN.
    ENDIF.
  ENDIF.

*........Das neue Formular eroeffnen
  PERFORM FORMULAR_OEFFNEN.
  PERFORM SAMMELLISTE_KOPF.

ENDFORM.
*.......DUMMY-FORM wegen Vernetzung mit RLKOMM00
FORM DRUCK_ANSTEUERN_LE.                                      "Korr 9/94
  IF ERSTER_DURCHLAUF NE CON_X.
    PERFORM SAMMELLISTE_ENDE.
    PERFORM FORMULAR_SCHLIESSEN.
  ENDIF.

*........Das neue Formular eroeffnen
  PERFORM FORMULAR_OEFFNEN.
  PERFORM SAMMELLISTE_KOPF.
ENDFORM.

*---------------------------------------------------------------------*
*        FORM FORMULAR_OEFFNEN                                        *
*---------------------------------------------------------------------*
*        Formular oeffnen                                             *
*---------------------------------------------------------------------*
FORM FORMULAR_OEFFNEN.

  MOVE CON_X TO KZ_NEUE_SEITE.

*........ ABAP- oder SAPSCRIPT-Logik..................................*
    CASE FLG_TADRUCK_WIE.
      WHEN SAPSCRIPT.
*........Füllen ITCPO aus Extrakt.....................................

*.........Anhängen an Spool oder Neueintrag ? ..............KORR 11/94
        IF T329P-TDNEWID = CON_X.                          "KORR 11/94
          MOVE T329P-TDNEWID TO ITCPO-TDNEWID.             "KORR 11/94
        ENDIF.                                             "KORR 11/94

        MOVE:
               LTAP-LDEST      TO ITCPO-TDDEST,
               T329P-TDDELETE  TO ITCPO-TDDELETE,
               T329P-TDIMMED   TO ITCPO-TDIMMED ,
               T329P-TDDATASET TO ITCPO-TDDATASET,
               T329P-TDCOPIES  TO ITCPO-TDCOPIES.

*........Füllen ITCPO-TDPROGRAM mit dem Programmamen, in dessen Work-...
*........area die Variablen für das SAPscript-Formular stehen. .........

        ITCPO-TDPROGRAM = T340D-REPID.

*........Eröffnen Formular..............................................
        IF NOT DRUCKEN IS INITIAL.
          CALL FUNCTION 'OPEN_FORM'
               EXPORTING FORM     = T329F-FORMU
                       DEVICE     = 'PRINTER'
                       OPTIONS    = ITCPO
                       DIALOG     = ' '.
        ELSE.
          CALL FUNCTION 'OPEN_FORM'
               EXPORTING FORM     = T329F-FORMU
                       DEVICE     = 'SCREEN'
                       OPTIONS    = ITCPO.
        ENDIF.
      WHEN ABAP.
        IF T329P-TDDELETE = CON_X.   " Kz delete aus Spool nach Druck
          CLEAR T329P-TDDELETE.
        ELSE.
          T329P-TDDELETE = CON_X.
        ENDIF.

*........Druck beginnt.................................................
        IF NOT DRUCKEN IS INITIAL.
          NEW-PAGE PRINT ON KEEP IN SPOOL T329P-TDDELETE
                              IMMEDIATELY T329P-TDIMMED    "Druck sofort
                              LIST NAME   T329P-TDDATASET   "Datasetname
                              DESTINATION LTAP-LDEST
                              COPIES      T329P-TDCOPIES       "K 8/94
                     NEW LIST IDENTIFICATION T329P-TDNEWID  "KORR 11/94
                              LAYOUT      DRUCKAUFBEREITUNG
                              NO DIALOG.
        ENDIF.
    ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*        FORM FORMULAR_SCHLIESSEN                                     *
*---------------------------------------------------------------------*
*        Formular schliessen                                          *
*---------------------------------------------------------------------*
FORM FORMULAR_SCHLIESSEN.

*........ ABAP- oder SAPSCRIPT-Logik..................................*
    CASE FLG_TADRUCK_WIE.
      WHEN SAPSCRIPT.
        CALL FUNCTION 'CLOSE_FORM'.
      WHEN ABAP.
        IF NOT T329F-SAMML IS INITIAL.
          PERFORM DRUCKEN_ABAP_FUSS.           "bei Sammellisten Verzw.
        ENDIF.                                 "in versch. Listen
        CLEAR CNT_SAMML.
        CNT_PAGE = CNT_PAGE + 1.
        IF NOT DRUCKEN IS INITIAL.
          NEW-PAGE PRINT OFF.
        ENDIF.
    ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*        FORM NEUE_SEITE                                              *
*---------------------------------------------------------------------*
*        Neue Seite:                                                  *
*         - Nur beim Sammeldruck bei jedem neuen Vonlagertyp          *
*         - grundsätzlich nie beim ersten Durchlauf                   *
*---------------------------------------------------------------------*
FORM NEUE_SEITE.

  IF KZ_NEUE_SEITE EQ SPACE.

*........ ABAP- oder SAPSCRIPT-Logik..................................*
    CASE FLG_TADRUCK_WIE.
      WHEN SAPSCRIPT.
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING COMMAND = 'NEW-PAGE'.
      WHEN ABAP.
        IF NOT T329F-SAMML IS INITIAL.
          PERFORM DRUCKEN_ABAP_FUSS.
        ENDIF.
        IF NOT CNT_SAMML IS INITIAL.    "sonst doppelter new-page hier
          NEW-PAGE.                     "u. falls gerade mit 10 Pos.
          CNT_PAGE = CNT_PAGE + 1.      "für Seite der Sammelliste
          PERFORM DRUCKEN_ABAP_KOPF.
        ENDIF.                          "fertig
        CLEAR CNT_SAMML.
    ENDCASE.

  ENDIF.
  MOVE CON_X TO KZ_NEUE_SEITE.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM DRUCKER_MAN_BESTIMMEN                                   *
*---------------------------------------------------------------------*
*        Bestimmen des Druckers bei Nachdrucken von TAs bzw.          *
*        Umbuchanweisungen                                            *
*---------------------------------------------------------------------*
FORM DRUCKER_MAN_BESTIMMEN.

  CHECK NOT FLG_TADRUCK_MAN IS INITIAL AND
        LTAP-LDEST IS INITIAL.

  IF NOT T329D-LDEST IS INITIAL.

*........Drucker aus der Tabelle T329P uebernehmen.....................
    MOVE T329D-LDEST TO LTAP-LDEST.
  ELSE.

    IF NOT T329D-T331D IS INITIAL.

*........Drucker aus der Tabelle T331 des VON-Lagertyps uebernehmen....
      PERFORM T331_LESEN USING LTAK-LGNUM LTAP-VLTYP.
      IF T331-LDEST IS INITIAL.
        PERFORM USR01_LESEN USING SY-UNAME.
        MOVE USR01-SPLD TO LTAP-LDEST.
      ELSE.
        MOVE T331-LDEST TO LTAP-LDEST.
      ENDIF.
    ELSE.

*........Drucker aus dem USER-Stamm....................................
      PERFORM USR01_LESEN USING SY-UNAME.
      MOVE USR01-SPLD TO LTAP-LDEST.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T329D_EINTRAG                                           *
*---------------------------------------------------------------------*
*        Maskiertes Lesen der TA-Druck Formulare u. Drucker/Lagertyp  *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T329D_EINTRAG USING P_LGNUM TYPE LTAP-LGNUM "smart: 2010-08-02 #105
  P_VLTYP TYPE LTAP-VLTYP P_NLTYP TYPE LTAP-NLTYP.
                                                 "smart: 2010-08-02 #105


  SY-SUBRC = 0.

*........Prüfen, ob die Tabelle T329D bereits gelesen wurde............

  IF P_LGNUM NE DRUCK_LGNUM OR
     P_VLTYP NE DRUCK_VLTYP OR
     P_NLTYP NE DRUCK_NLTYP.
    MOVE:
         P_LGNUM TO DRUCK_LGNUM,
         P_VLTYP TO DRUCK_VLTYP,
         P_NLTYP TO DRUCK_NLTYP.
  ELSE.
    EXIT.
  ENDIF.

*........Tabelle T329D lesen...........................................

  PERFORM T329D_LESEN USING P_LGNUM
                            P_VLTYP
                            P_NLTYP.

  IF SY-SUBRC > 0.
*........Tabelle 329P mit maskierten VON-Lagertyp......................
    PERFORM T329D_LESEN USING P_LGNUM
                                MASK3
                              P_NLTYP.
  ENDIF.
  IF SY-SUBRC > 0.
*........Tabelle 329P mit maskierten NACH-Lagertyp.....................
    PERFORM T329D_LESEN USING LTAP-LGNUM
                              LTAP-VLTYP
                                   MASK3.
  ENDIF.
  IF SY-SUBRC > 0.
*........Tabelle 329P mit maskierten VON- u. NACH-Lagertyp..............
    PERFORM T329D_LESEN USING LTAP-LGNUM
                                   MASK3
                                   MASK3.
  ENDIF.
  IF SY-SUBRC > 0.
    MESSAGE A101.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM TSP03_LESEN                                             *
*---------------------------------------------------------------------*
*        Lesen des Druckers in TSP03                                  *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM TSP03_LESEN USING VALUE(P_PADEST) TYPE RLDRU-LDEST.
                                                 "smart: 2010-08-02 #105


  SELECT SINGLE * FROM TSP03
   WHERE PADEST = P_PADEST.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T300_LESEN                                              *
*---------------------------------------------------------------------*
*        Zulesen des Regionalkennzeichens                             *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T300_LESEN USING P_LGNUM TYPE LTAK-LGNUM.   "smart: 2010-08-02 #105

  SY-SUBRC = 0.

  CHECK P_LGNUM NE T300-LGNUM.

  SELECT SINGLE * FROM T300
     WHERE LGNUM = LTAK-LGNUM.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T301T_LESEN                                             *
*---------------------------------------------------------------------*
*        Zulesen des Lagertyp-Textes                                  *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T301T_LESEN USING P_LGNUM TYPE LTAP-LGNUM   "smart: 2010-08-02 #105
  P_LGTYP TYPE CLIKE.                            "smart: 2010-08-02 #105

  SELECT SINGLE * FROM T301T
     WHERE SPRAS = SY-LANGU
       AND LGNUM = P_LGNUM
       AND LGTYP = P_LGTYP.

  IF SY-SUBRC <> 0.
    MESSAGE A249 WITH P_LGTYP P_LGNUM.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T302T_LESEN                                             *
*---------------------------------------------------------------------*
*        Zulesen des Lagerbereich-Textes                              *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T302T_LESEN USING P_LGNUM TYPE LTAP-LGNUM   "smart: 2010-08-02 #105
  P_LGTYP TYPE CLIKE P_LGBER TYPE CLIKE.         "smart: 2010-08-02 #105

  SELECT SINGLE * FROM T302T
     WHERE SPRAS = SY-LANGU
       AND LGNUM = P_LGNUM
       AND LGTYP = P_LGTYP
       AND LGBER = P_LGBER.

*........Wenn nicht gefunden ==> Löschen des T302T-Satzes...............
*........(Grund: Nicht zu jedem Lagerbereich muß ein Text vorhanden.....
*........sein (dynamische Lagerplatzvergabe))...........................

  IF SY-SUBRC <> 0.
    CLEAR T302T.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T329D_LESEN                                             *
*---------------------------------------------------------------------*
*        Lesen der TA-Druck Formulare und Drucker pro Lagertyp        *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T329D_LESEN USING P_LGNUM TYPE LTAP-LGNUM   "smart: 2010-08-02 #105
  P_VLTYP TYPE CLIKE P_NLTYP LIKE MASK3.         "smart: 2010-08-02 #105

  SELECT SINGLE * FROM T329D
    WHERE LGNUM = P_LGNUM
      AND VLTYP = P_VLTYP
      AND NLTYP = P_NLTYP.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T329F_LESEN                                             *
*---------------------------------------------------------------------*
*        Lesen der TA-Druck Formulare und Sortierroutinen             *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T329F_LESEN USING P_LGNUM TYPE LTAK-LGNUM   "smart: 2010-08-02 #105
  P_DRUKZ TYPE CLIKE.                            "smart: 2010-08-02 #105

  SY-SUBRC = 0.

  CHECK P_LGNUM NE T329F-LGNUM OR
        P_DRUKZ NE T329F-DRUKZ.

  SELECT SINGLE * FROM T329F
    WHERE LGNUM = P_LGNUM
      AND DRUKZ = P_DRUKZ.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T329P_LESEN                                              *
*---------------------------------------------------------------------*
*        Lesen der TA-Druck Spoolparameter                            *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T329P_LESEN USING P_LGNUM TYPE CLIKE P_SPOOL"smart: 2010-08-02 #105
   TYPE CLIKE.                                   "smart: 2010-08-02 #105

  SY-SUBRC = 0.

* CHECK P_LGNUM NE T329P-LGNUM OR                       "Korr Feb 95
*       P_SPOOL NE T329P-SPOOL.                         "Korr Feb 95

  SELECT SINGLE * FROM T329P
    WHERE LGNUM = P_LGNUM
      AND SPOOL = P_SPOOL.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T331_LESEN                                              *
*---------------------------------------------------------------------*
*        Lesen der Lagertypeigenschaften                              *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T331_LESEN USING P_LGNUM TYPE LTAK-LGNUM    "smart: 2010-08-02 #105
  P_LGTYP TYPE LTAP-VLTYP.                       "smart: 2010-08-02 #105

  CHECK P_LGNUM NE T331-LGNUM OR
        P_LGTYP NE T331-LGTYP.

  SELECT SINGLE * FROM T331
    WHERE LGNUM = P_LGNUM
      AND LGTYP = P_LGTYP.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T333B_LESEN
*---------------------------------------------------------------------*
*        Lesen der Transporartbezeichnung
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T333B_LESEN USING P_LGNUM TYPE LTAK-LGNUM   "smart: 2010-08-02 #105
  P_TRART TYPE LTAK-TRART.                       "smart: 2010-08-02 #105

  CHECK P_LGNUM NE T333B-LGNUM OR
        P_TRART NE T333B-TRART.

  SELECT SINGLE * FROM T333B
    WHERE SPRAS = SY-LANGU
      AND LGNUM = P_LGNUM
      AND TRART = P_TRART.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM T340D_LESEN                                              *
*---------------------------------------------------------------------*
*        Read TO-Print-Program name                                   *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM T340D_LESEN USING P_LGNUM TYPE LTAK-LGNUM.  "smart: 2010-08-02 #105
  CHECK T340D-REPID IS INITIAL.

  SELECT SINGLE * FROM T340D
    WHERE LGNUM = P_LGNUM.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM USR01_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines Benuterstammes                                    *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM USR01_LESEN USING VALUE(P_BNAME) TYPE SYST-UNAME.
                                                 "smart: 2010-08-02 #105


  CHECK P_BNAME NE USR01-BNAME.

  SELECT SINGLE * FROM USR01
    WHERE BNAME = P_BNAME.

  IF SY-SUBRC NE 0.
    MESSAGE A149 WITH P_BNAME.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM PLATZ_POSITIONEN_MISCHEN USING LTAP                     *
*---------------------------------------------------------------------*
*        Mischen Platzpositionen falls es eine Platzaufteilung gubt   *
*---------------------------------------------------------------------*
FORM PLATZ_POSITIONEN_MISCHEN.

   IF NOT LTAP-VPPOS IS INITIAL.                   "Korr 20.1.94
     CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'      "Korr 20.1.94
         EXPORTING LGPLA = LTAP-VLPLA              "Korr 20.1.94
                   PLPOS = LTAP-VPPOS              "Korr 20.1.94
         IMPORTING O_LGPLA = LTAP-VLPLA.           "Korr 20.1.94
   ENDIF.                                          "Korr 20.1.94

   IF NOT LTAP-NPPOS IS INITIAL.                   "Korr 20.1.94
     CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'      "Korr 20.1.94
         EXPORTING LGPLA = LTAP-NLPLA              "Korr 20.1.94
                   PLPOS = LTAP-NPPOS              "Korr 20.1.94
         IMPORTING O_LGPLA = LTAP-NLPLA.           "Korr 20.1.94
   ENDIF.                                          "Korr 20.1.94

   IF NOT LTAP-RPPOS IS INITIAL.                   "Korr 20.1.94
     CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'      "Korr 20.1.94
         EXPORTING LGPLA = LTAP-RLPLA              "Korr 20.1.94
                   PLPOS = LTAP-RPPOS              "Korr 20.1.94
         IMPORTING O_LGPLA = LTAP-RLPLA.           "Korr 20.1.94
   ENDIF.                                          "Korr 20.1.94
ENDFORM.                                           "Korr 20.1.94
