REPORT rlvsdr40 NO STANDARD PAGE HEADING MESSAGE-ID l3.
*---------------------------------------------------------------------*
*  Report RLVSDR40: Druck TA's über Formular  / Printing TO´s         *
*---------------------------------------------------------------------*
*     Anstoß zum Druck aus TA-Erstellung / via TO creation            *
*     Anstoß zum Druck aus Transaktion manuell / manually             *
*---------------------------------------------------------------------*

************************************************************************
*Anpassungen gegenüber dem Orig.druckprogramm                          *
*04102004; Peter Huber, SAP Stäfa.                                     *
*         Funktionsbaustein L_PRINT_TO_MULTIPLE modifiziert für        *
*         zusätzliche Datenausgabe                                     *
*02022005; Peter Huber, SAP Stäfa                                      *
*         Exit ersetzt durch Funktionsbaustein ZEXIT_RLVSDR40_001,     *
*         welcher den Include ZXDRUU01 aufruft                         *
*                                                                      *
* ES29072005      Ernst Sommer, 29.07.2005                             *
*        200      Wenn Verkäufergruppe im Kopf (VBAK) vorhanden,       *
*                 müssen 'Unsere Referenz / Telefon / Fax / E-Mail'    *
*                 über diese ermittelt werden.                         *
*                                                                      *
*23062006; Peter Huber, SAP Stäfa                                      *
*          Aenderung vom 02022005 zurückgenommen                       *
*          ZEXIT_RLVSDR40_001 durch CALL CUSTOMER-FUNCTION '001'       *
*          ersetzt. Somit wieder Standard.                             *
*          Auftraggeber: F.Heinzer & W. Santschi                       *
*          Ausbau des 2fachen Druckes auf 2 unterschiedliche Ausgabe-  *
*          Geräte mit unterschiedlichen Formulare.                     *
*                                                                      *
*24102006; Markus Raffeiner                                            *
*          Neuer Parameter in EXIT_RLVSDR40_001:                       *
*          XLTHU (Zuordnung von Pick-HU's zu Transportaufträgen)       *
*
*23092010; Markus Raffeiner                                            *
*          Abfrage sy-sysid an neues System anpassen
************************************************************************


*........Reportspezifische Parameter und Select-Options.................
SELECTION-SCREEN  BEGIN OF BLOCK xxx WITH FRAME TITLE text-010.
PARAMETERS:
        druckkz  LIKE rldru-drukz,
        edrucker LIKE rldru-ldest,
        spoolpar LIKE rldru-spool,
        drucken  LIKE rldru-druck  DEFAULT 'X',
        explizit LIKE rldru-ausdr.
SELECTION-SCREEN END OF BLOCK xxx.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN  BEGIN OF BLOCK yyy WITH FRAME TITLE text-167.
PARAMETERS:
        tasch AS CHECKBOX   DEFAULT 'X',    "Druck TA-Schein
        lesch AS CHECKBOX   DEFAULT 'X',    "Druck LE-Schein
        letasch AS CHECKBOX DEFAULT 'X',    "Druck LE-TA-Schein
        leinh AS CHECKBOX   DEFAULT 'X',    "Druck LE-Inhaltsverzeichnis
        humla AS CHECKBOX   DEFAULT 'X',    "Pick-HU-Scheine     "HUM98
        etikett AS CHECKBOX DEFAULT 'X'.    "Druck Etiketten

SELECTION-SCREEN END OF BLOCK yyy.

INCLUDE rlvsdtop.
INCLUDE mllvskon.

*04102004
*Zusatzdaten.
INCLUDE zrlvstop.
*2te Druckerdestination
IF sy-sysid = 'PC2'.
*  drucker2 = 'GB32'.
*  vorübergehend muss auf gbfl1 abgefragt werden (Wasserschaden DAG)
 Drucker2 = 'GBFL1'.
ELSE.
  drucker2 = 'locl'.
ENDIF.
*Counter für Drucker2
dru2 = 1.

*---------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                          *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

*---------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*---------------------------------------------------------------------*
START-OF-SELECTION.
*........compatibility because of new parameter etikett and RLVSEXTE
  MOVE etikett TO fix_etike.
  PERFORM start_of_selection.

*---------------------------------------------------------------------*
*        GET LTAK                                                     *
*---------------------------------------------------------------------*
GET ltak.
  PERFORM get_ltak.

*---------------------------------------------------------------------*
*        GET LTAP                                                     *
*---------------------------------------------------------------------*
GET ltap.
  PERFORM get_ltap.

*---------------------------------------------------------------------*
*        END-OF-SELECTION                                             *
*---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM end_of_selection.
  PERFORM message_output.
*---------------------------------------------------------------------*
*        INCLUDES                                                     *
*---------------------------------------------------------------------*
*........Externer Aufruf des Druckprogramms.............................
*........Call of printing from external i.e. out of posting TOs
  INCLUDE rlvsexte.
*........mutual form routeens with multiple processing.................
  INCLUDE rlvsdfor.
*---------------------------------------------------------------------*
*       Subrouteens except multiple processing                        *
*                                                                     *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
FORM start_of_selection.
  CLEAR:                 druck_ok_to_single,
                         druck_ok_po,
                         druck_ok_multi,
                         druck_ok_su,
                         druck_ok_hu,                       "HUM98
                         druck_ok_labels.
*........Is used in mutual include RLVSEXTE...........................*
  flg_tadruck_wie = sapscript.
*........Initialize internal table out ...............................*
  REFRESH: irldrh, irldri, irldrc, irldru.

  kzmem = t4_kzmem.        "for mutual routeens
*........Importieren von QPSERR und LSPERR aus Memory

  IF NOT t4_kzmem IS INITIAL.
    IMPORT qsperr
           lsperr FROM MEMORY ID druck_id.
    SORT qsperr.
    IMPORT rldru-druck rldru-ausdr FROM MEMORY ID druck_id.
    IF rldru-druck IS INITIAL.
      CLEAR drucken.
    ENDIF.
    IF NOT rldru-ausdr IS INITIAL.
      MOVE rldru-ausdr TO explizit.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       GET LTAK                                                      *
*---------------------------------------------------------------------*
FORM get_ltak.
*........check of  DRUKZ ..............................................
  IF ltak-drukz IS INITIAL.
    MOVE druckkz TO ltak-drukz.
  ENDIF.
  CHECK NOT ltak-drukz IS INITIAL.
*** OR 131098 HUM
*........Reset Flag for reading HU-Data once per TO...................
  CLEAR flg_hudruck.
***
*........rest together with multiple processing.......................
  PERFORM get_ltak_mutual.

ENDFORM.

*---------------------------------------------------------------------*
*       GET LTAP                                                      *
*---------------------------------------------------------------------*
FORM get_ltap.
  CLEAR:  rldru, irldru, irldri.
  PERFORM get_ltap_mutual.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       Dummy bbecause of RLVSEXTE and upwards compatibility
*----------------------------------------------------------------------*
FORM at_selection_screen.

ENDFORM.                    " AT_SELECTION_SCREEN

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_OUTPUT
*&---------------------------------------------------------------------*
*       only when report is manually executed, not when called by ml03t
*----------------------------------------------------------------------*
FORM message_output.
  CHECK t4_kzmem IS INITIAL.
*.........at least one print was successful............................
  IF druck_ok_to_single = 1 OR druck_ok_multi = 1 OR
      druck_ok_su = 1 OR
      druck_ok_po = 1 OR druck_ok_labels = 1 OR             "HUM98
      druck_ok_hu = 1.                                      "HUM98
    MESSAGE s133.  "Druck erfolgt
    EXIT.
  ENDIF.
ENDFORM.                    " MESSAGE_PUTPUT
*&---------------------------------------------------------------------*
*&      Form  END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM end_of_selection.
*........Sort of additional tables....................................
  PERFORM sorting.
*........necessary to avoid too many lists............................
  SORT irldri BY ldesi formi spooi tanum tapos.
*........Call Customer function MWMD0001..............................
  PERFORM user_exit.
*........call of MULTIPLE print ......................................
  PERFORM multi_print.
*  CLEAR irldrh-lgnum.                                       "CSOKT2004
*  READ TABLE irldrh INDEX 1.                                "CSOKT2004
*  IF irldrh-lgnum = '491'.                                  "CSOKT2004
**04102004; Beginn: 2 mal Drucken auf unterschiedliche Drucker
**Der zweite Druck erfolgt auf 2ten Schacht des Druckers xyz, welcher
**im SAP als eigenes Ausgabegerät definiert ist.
*    LOOP AT irldrc.
*      irldrc-ldest = drucker2.
*      irldrc-ldes1 = drucker2.
*      irldrc-ldes2 = drucker2.
*      irldrc-ldes3 = drucker2.
*      irldrc-ldes4 = drucker2.
*      irldrc-ldes5 = drucker2.
*      irldrc-ldes7 = drucker2.
**     Formular dito ZLVSTALISTE_200, statt TRY01 aber TRY02
*      irldrc-formu = 'ZLVSTALISTE2_200'.
*      MODIFY irldrc.
*    ENDLOOP.
*
*    PERFORM user_exit.
*    PERFORM multi_print.
*  ENDIF.                                                    "CSOKT2004
*04102004; Ende
*........Call of printing function modules ...........................
  PERFORM call_function_modules_mutual.

ENDFORM.                    " END_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  USER_EXIT
*&---------------------------------------------------------------------*
*       you will get all relevant data:   Exit:  MWMD0001 -> /nCMOD
*----------------------------------------------------------------------*
FORM user_exit.

*.......in standard, always printing is always allowed.................
  MOVE: con_x TO print_single,
        con_x TO print_po,
        con_x TO print_multi,
        con_x TO print_su,
        con_x TO print_hu,                                  "HUM98
        con_x TO print_label,
        con_x TO print_multi_ref.



  CALL FUNCTION 'EXIT_RLVSDR40_001'

       TABLES    xrldrc       =   irldrc
                 xvblkk       =   ivblkk
                 xvblkp       =   ivblkp
                 xsernr       =   isernr
                 xrldrh       =   irldrh
                 xrldri       =   irldri
                 xrldrp       =   irldrp
                 xrldru       =   irldru
                 xt329p       =   it329p
                 xresb        =   iresb
                 xrlvek       =   irlvek
                 xreftab      =   reftab
                 xlthu        =   ilthu
       CHANGING
                 c_t312s      =   t312s
                 c_lesch      =   lesch
                 c_letasch    =   letasch
                 c_leinh      =   leinh
                 c_single     =   print_single
                 c_po         =   print_po
                 c_su         =   print_su
                 c_label      =   print_label
                 c_multi      =   print_multi
                 c_multi_ref  =   print_multi_ref           "HUM98
                 c_humla      =   print_hu.                 "HUM98





ENDFORM.                    " USER_EXIT
*&---------------------------------------------------------------------*
*&      Form  MULTI_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM multi_print.

tables:       tvgrt.             "Org.-Einheit: Verkäufergr. Texte


*0410200; Beginn Zusatzdaten lesen und Tabellen füllen für Uebergabe
*Auftraggeber
*  SELECT SINGLE * FROM lips WHERE vbeln = irldrh-vbeln.
*  IF sy-subrc = 0.
*    SELECT SINGLE * FROM vbak WHERE vbeln = lips-vgbel.
*    IF sy-subrc = 0.
*      CLEAR: ivbak.
*      MOVE-CORRESPONDING vbak TO ivbak.              "Auftragskopf
*      IF NOT vbak-vkgrp IS INITIAL.
*        SELECT SINGLE * FROM tvgrt
*               WHERE vkgrp = vbak-vkgrp.
*        IF sy-subrc EQ 0.
*          ivbak-ernam = tvgrt-zzernam.
*        ENDIF.
*      ENDIF.
*      SELECT SINGLE * FROM kna1 WHERE kunnr = vbak-kunnr.
*      IF sy-subrc = 0.
*        CLEAR: ikna1ag.
*        MOVE-CORRESPONDING kna1 TO ikna1ag.
*        SELECT SINGLE * FROM t005t WHERE land1 = kna1-land1.
*        CLEAR: it005tag.
*        it005tag-landx = t005t-landx.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
**Warenempfänger
*  SELECT SINGLE * FROM lips WHERE vbeln = irldrh-vbeln.
*  IF sy-subrc = 0.
*    SELECT SINGLE * FROM vbpa WHERE vbeln EQ lips-vgbel
*                                  AND parvw EQ 'WE'
*                                  AND posnr EQ '000000'.
*    IF sy-subrc NE 0.
*      SELECT SINGLE * FROM likp WHERE vbeln = irldrh-vbeln.
*      IF sy-subrc = 0.
*        SELECT SINGLE * FROM vbpa WHERE vbeln EQ likp-vbeln
*                                  AND   parvw EQ 'WE'
*                                  AND   posnr EQ '000000'.
*      ENDIF.
*    ENDIF.
*
*
*    SELECT SINGLE * FROM adrc WHERE addrnumber EQ vbpa-adrnr.
*    CLEAR: ikna1we.
*    ikna1we-kunnr = vbpa-kunnr.
*    ikna1we-name1 = adrc-name1.
*    ikna1we-name2 = adrc-name2.
*    ikna1we-stras = adrc-street.
*    ikna1we-pstlz = adrc-post_code1.
*    ikna1we-ort01 = adrc-city1.
*    ikna1we-land1 = adrc-country.
*
*
*    SELECT SINGLE * FROM t005t WHERE land1 = adrc-country.
*    CLEAR: it005twe.
*    it005twe-landx = t005t-landx.
*  ENDIF.
*
**Lieferungskopf
*
*  SELECT SINGLE * FROM likp WHERE vbeln = irldrh-vbeln.
*  IF sy-subrc = 0.
*    CLEAR: ilikp.
*    MOVE-CORRESPONDING  likp TO ilikp.
*  ENDIF.
*
**Incoterm
*  SELECT SINGLE * FROM tinct WHERE inco1 = likp-inco1
*                               AND spras = 'DE'.
*
*  IF sy-subrc = 0.
*    CLEAR: itinct.
*    MOVE-CORRESPONDING  tinct TO itinct.
*  ENDIF.
*0410200; Ende Zusatzdaten lesen

*.......Printing of Multiple TOs:  with sorting .......................
  IF tasch = con_x.
    IF print_multi = con_x.
      CALL FUNCTION 'L_PRINT_TO_MULTIPLE'
           EXPORTING
                i_druck  = drucken
           IMPORTING
                e_return = druck_ok_ref
           TABLES
                xrldrc   = irldrc
                xvblkk   = ivblkk
                xvblkp   = ivblkp
                xsernr   = isernr
                xrldrh   = irldrh
                xrldri   = irldri
                xrldrp   = irldrp
                xrldru   = irldru
                xt329p   = it329p
                xresb    = iresb
                xrlvek   = irlvek
*                ikna1ag  = ikna1ag                          "04102004
*                it005tag = it005tag                         "04102004
*                ikna1we  = ikna1we                          "04102004
*                it005twe = it005twe                         "04102004
*                ilikp    = ilikp                            "04102004
*                ivbak    = ivbak                            "04102004
*                itinct   = itinct                           "04102004
*                ithead   = ithead                           "04102004
                xlthu    = ilthu.
      .

    ENDIF.
  ENDIF.
ENDFORM.                    " MULTI_PRINT
