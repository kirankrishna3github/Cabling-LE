
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #166
************************************************************************

REPORT zle_inventur MESSAGE-ID fb.

*-----------------------------------------------------------------------
* Programmbeschreibung
*-----------------------------------------------------------------------
*
*Aufbereitung der Lagerplatzbestände für die Inventur.
*Die Ausgabe kann sowohl im ALV als auch mittels SAP Script erfolgen.


*SAP Stäfa, Peter Huber, 21.10.2004

***********************************************************************
* Tabellen
***********************************************************************
TABLES: mara, lqua, makt,  mch1, itcpo.
TABLES: zle_inventurhist, zle_inventur.

DATA: BEGIN OF work OCCURS 0.
        INCLUDE STRUCTURE zle_inventur.
DATA: END OF work.

DATA: BEGIN OF work2 OCCURS 0.
DATA: wexidv LIKE mcha-zzexidv.
DATA: wgesme LIKE lqua-gesme.
        INCLUDE STRUCTURE zle_inventur.
DATA: END OF work2.
***********************************************************************
* Hilfsfelder & Tabellen
***********************************************************************

*$smart (W) 2010-08-02 - #166 Datendefinition bezieht sich auf einen
*$smart (W) 2010-08-02 - #166 obsoleten Datentyp. (A)

DATA: w_callback_program TYPE repid.             "smart: 2010-08-02 #166
DATA: w_isvariant        TYPE disvariant.
DATA: belnr TYPE num10.
DATA: xdialog.
DATA: wposnr LIKE lqua-ivpos.
DATA: wdatum TYPE datum.
DATA: wlgpla LIKE lqua-lgpla,
      wmatnr LIKE lqua-matnr,
      wmaktx LIKE makt-maktx,
      wcharg LIKE lqua-charg,
      wrolln LIKE zle_inventur-rolln,
      wexidv TYPE exidv,
      wgesme TYPE p DECIMALS 3,
      wmeins LIKE lqua-meins,
      wtp    LIKE lqua-gesme.

***********************************************************************
* Formulardefinition für Ausgabe mittels SAP Script
***********************************************************************
DATA: xform(16) VALUE 'Z&&&_INVENTUR'.

***********************************************************************
* ALV Listaufbereitungsfelder (Tabellenüberschriften)
***********************************************************************
TYPE-POOLS:    slis.
DATA:          it_fieldcatalog   TYPE slis_t_fieldcat_alv.
FIELD-SYMBOLS: <wa_fieldcatalog> LIKE LINE OF it_fieldcatalog.
DATA:          c_variant LIKE disvariant,
               layout    TYPE slis_layout_alv,
               a_save    TYPE c VALUE 'A'.
***********************************************************************
* Selektionensparameter
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME.
SELECT-OPTIONS:   slgnum FOR lqua-lgnum,
                  slgtyp FOR lqua-lgtyp DEFAULT '001',
                  slgpla FOR lqua-lgpla,
                  swerks FOR lqua-werks,
                  slgort FOR lqua-lgort.
PARAMETERS:       pbestq LIKE lqua-bestq,
                  psobkz LIKE lqua-sobkz.
SELECTION-SCREEN SKIP 1.
PARAMETERS:       ptage(3) DEFAULT '340',  "letzte Inventur vor Tagen
                  ptcheck  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK s1.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME.
SELECT-OPTIONS:   smatnr FOR lqua-matnr,
                  smtart FOR mara-mtart,
                  scharg FOR lqua-charg,
                  sexidv FOR zle_inventur-exidv,              "Packstück
                  srolln FOR zle_inventur-rolln NO INTERVALS. "Rollnr
SELECTION-SCREEN END OF BLOCK s2.

SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE text-001.
PARAMETERS:       pall  RADIOBUTTON GROUP 1,
                  ppack RADIOBUTTON GROUP 1,
                  proll RADIOBUTTON GROUP 1,
                  pstk  RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF BLOCK s3.

SELECTION-SCREEN BEGIN OF BLOCK s5 WITH FRAME TITLE text-005.
PARAMETERS:       s1 RADIOBUTTON GROUP 5,
                  s2 RADIOBUTTON GROUP 5.
SELECTION-SCREEN END OF BLOCK s5.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE text-002.
PARAMETERS:       pscript AS CHECKBOX DEFAULT 'X'.
PARAMETERS:       dest LIKE usr01-spld.
SELECTION-SCREEN SKIP 1.
PARAMETERS:       ptot AS CHECKBOX.   "Packstück Total ausgeben
SELECTION-SCREEN END OF BLOCK s4.

SELECTION-SCREEN SKIP 1.
PARAMETERS:       pecht AS CHECKBOX.


***********************************************************************
* Beginn der Verarbeitung
***********************************************************************
START-OF-SELECTION.

*Formularname
  REPLACE '&&&' INTO xform WITH sy-mandt.

*ALV vorbelegen
  w_isvariant-report  = 'Inventur in der Lagerverwaltung'.
  w_isvariant-variant = 'VAR1'.
  w_callback_program  = sy-repid.

*Belegnummer lösen
  IF pecht = 'X'.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZDAG_INVE'
        quantity                = '1'
      IMPORTING
        number                  = belnr
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE a003(zdmm) WITH sy-subrc.
    ENDIF.
  ELSE.
    belnr = 'Test'.
  ENDIF.

*Posnr.
  wposnr = '1'.

  SELECT * FROM lqua WHERE lgnum IN slgnum
                       AND lgtyp IN slgtyp
                       AND lgpla IN slgpla
                       AND matnr IN smatnr
                       AND werks IN swerks
                       AND lgort IN slgort
                       AND charg IN scharg
                       AND bestq EQ pbestq
                       AND sobkz EQ psobkz.

    SELECT SINGLE * FROM mara WHERE matnr = lqua-matnr
                              AND   mtart IN smtart.

    CHECK sy-subrc EQ 0.


*berücksichtigung der letzten Inventur auf dem Quant
    IF ptcheck = 'X'.
      wdatum = sy-datum - ptage.
      CHECK: lqua-idatu LT wdatum.
    ENDIF.

*Materialkurztext
    CLEAR: makt.
    SELECT SINGLE * FROM makt WHERE matnr EQ lqua-matnr
                                AND spras EQ 'DE'.
    IF sy-subrc = 0.
      "nix
    ELSE.
      SELECT SINGLE * FROM makt WHERE matnr EQ lqua-matnr.
    ENDIF.
*Trommelnr. und Packstücknummer
    CLEAR: mch1.
    IF NOT lqua-charg IS INITIAL.
      SELECT SINGLE * FROM mch1 WHERE matnr EQ lqua-matnr
                                  AND charg EQ lqua-charg.
    ENDIF.

*Prüfen gegen Rollnr. und Packstücknr.
    IF NOT srolln IS INITIAL OR
       NOT sexidv  IS INITIAL.
      CHECK: mch1-zzexidv IN sexidv.
      CHECK: mch1-zzrollnr IN srolln.
    ENDIF.

*Ausschlüsse berücksichtigen
    IF pall = 'X'.
      "nix
    ELSEIF proll = 'X'.
      CHECK: NOT mch1-zzrollnr IS INITIAL.
      CHECK:     mch1-zzexidv  IS INITIAL.
    ELSEIF ppack = 'X'.
      CHECK: NOT mch1-zzexidv IS INITIAL.
    ELSEIF pstk  = 'X'.
      CHECK: lqua-meins = 'ST'.
    ENDIF.

*Worktabelle für Uebergabe füllen
    CLEAR: work.
    work-ivnum = belnr.
    work-ivpos = wposnr.
    work-lgnum = lqua-lgnum.
    work-lqnum = lqua-lqnum.
    work-lgtyp = lqua-lgtyp.
    work-lgpla = lqua-lgpla.
    work-werks = lqua-werks.
    work-lgort = lqua-lgort.
    work-matnr = lqua-matnr.
    work-maktx = makt-maktx.
    work-charg = lqua-charg.
    work-bestq = lqua-bestq.
    work-sobkz = lqua-sobkz.
    work-gesme = lqua-gesme.
    work-verme = lqua-verme.
    work-einme = lqua-einme.
    work-ausme = lqua-ausme.
    work-meins = lqua-meins.
    work-exidv = mch1-zzexidv.
    work-rolln = mch1-zzrollnr.
    work-ivnumold  = lqua-ivnum.
    work-ivposold  = lqua-ivpos.
    work-ivdatuold = lqua-idatu.
    APPEND work.
  ENDSELECT.

**********************************************************************
* CHG0033861 - Enhance program that generates inventory list for Cables
* CHANGED BY AJL@SYSWISE
* 17/07/2018
**********************************************************************
* START OF CHANGE
  IF slgort IS NOT INITIAL AND swerks IS NOT INITIAL.

    CLEAR: mara, lqua, makt,  mch1.

    DATA: lt_lgort TYPE STANDARD TABLE OF mard,
          ls_lgort TYPE mard,
          lt_mard  TYPE STANDARD TABLE OF mard,
          ls_mard  TYPE mard,
          ls_mchb  TYPE mchb,
          lt_mchb  TYPE STANDARD TABLE OF mchb,
          lv_charg TYPE mchb-charg.

* Get records by Plant/Storage Location
    SELECT werks lgort
      FROM mard
      INTO CORRESPONDING FIELDS OF TABLE lt_lgort
      WHERE werks IN swerks
      AND lgort IN slgort.

    SORT lt_lgort BY werks lgort.
    DELETE ADJACENT DUPLICATES FROM lt_lgort COMPARING werks lgort.

    LOOP AT lt_lgort INTO ls_lgort.

      SELECT * FROM lqua WHERE lgnum IN slgnum
                           AND lgtyp IN slgtyp
                           AND lgpla IN slgpla
                           AND matnr IN smatnr
                           AND werks EQ ls_lgort-werks
                           AND lgort EQ ls_lgort-lgort
                           AND charg IN scharg
                           AND bestq EQ pbestq
                           AND sobkz EQ psobkz.
      ENDSELECT.

* Get records not in LQUA
      IF sy-subrc NE 0.

        SELECT *
          FROM mard
          INTO CORRESPONDING FIELDS OF TABLE lt_mard
          WHERE matnr IN smatnr
          AND werks EQ ls_lgort-werks
          AND lgort EQ ls_lgort-lgort.

        LOOP AT lt_mard INTO ls_mard.

          SELECT SINGLE * FROM mara WHERE matnr = ls_mard-matnr
                              AND   mtart IN smtart.

          CHECK sy-subrc EQ 0.

          SELECT *
            FROM mchb
            INTO CORRESPONDING FIELDS OF TABLE lt_mchb
            WHERE matnr EQ ls_mard-matnr
            AND werks EQ ls_mard-werks
            AND lgort EQ ls_mard-lgort.

* Get records with batch stocks
          IF sy-subrc EQ 0.

            LOOP AT lt_mchb INTO ls_mchb.

              CLEAR work.

              work-charg = ls_mchb-charg." (in case there are batch stocks, otherwise empty).
              work-gesme = ls_mchb-clabs + ls_mchb-cumlm + ls_mchb-cinsm + ls_mchb-ceinm + ls_mchb-cspem + ls_mchb-cretm.

              IF work-gesme NE 0.

                CLEAR: mch1.

                SELECT SINGLE * FROM mch1 WHERE matnr EQ ls_mard-matnr
                                            AND charg EQ ls_mchb-charg.

* Get additional information
                CLEAR makt.
                SELECT SINGLE * FROM makt WHERE matnr EQ ls_mard-matnr
                                      AND spras EQ 'DE'.
                IF sy-subrc = 0.
                ELSE.
                  SELECT SINGLE * FROM makt WHERE matnr EQ ls_mard-matnr.
                ENDIF.

                SELECT SINGLE * FROM mara WHERE matnr EQ ls_mard-matnr.

                work-werks = ls_mard-werks.
                work-lgort = ls_mard-lgort.
                work-matnr = ls_mard-matnr.
                work-maktx = makt-maktx.
*              work-gesme = ls_mard-labst + ls_mard-umlme + ls_mard-insme + ls_mard-einme + ls_mard-speme + ls_mard-retme.
                work-verme = ls_mchb-clabs.
                work-meins = mara-meins.
                work-exidv = mch1-zzexidv.
                work-rolln = mch1-zzrollnr.
                work-ivdatuold = ls_mard-dlinl.

                IF work-gesme NE 0.
                  APPEND work.
                ENDIF.

                CLEAR: mara, makt,  mch1, ls_mchb.

              ENDIF.

              CLEAR work.

            ENDLOOP.

          ELSE.

            CLEAR: mch1.

            SELECT SINGLE * FROM mch1 WHERE matnr EQ ls_mard-matnr
                                        AND charg EQ ls_mchb-charg.

* Get additional information
            CLEAR makt.
            SELECT SINGLE * FROM makt WHERE matnr EQ ls_mard-matnr
                                  AND spras EQ 'DE'.
            IF sy-subrc = 0.
            ELSE.
              SELECT SINGLE * FROM makt WHERE matnr EQ ls_mard-matnr.
            ENDIF.

            SELECT SINGLE * FROM mara WHERE matnr EQ ls_mard-matnr.

            CLEAR work.
            work-werks = ls_mard-werks.
            work-lgort = ls_mard-lgort.
            work-matnr = ls_mard-matnr.
            work-maktx = makt-maktx.
            work-gesme = ls_mard-labst + ls_mard-umlme + ls_mard-insme + ls_mard-einme + ls_mard-speme + ls_mard-retme.
            work-verme = ls_mard-labst.
            work-meins = mara-meins.
            work-exidv = mch1-zzexidv.
            work-rolln = mch1-zzrollnr.
            work-ivdatuold = ls_mard-dlinl.

            IF work-gesme NE 0.
              APPEND work.
            ENDIF.

            CLEAR: mara, makt,  mch1, ls_mchb.

          ENDIF.

        ENDLOOP.
      ENDIF.

      CLEAR: mara, lqua, makt, mch1, lt_mard.

    ENDLOOP.
  ENDIF.
* END OF CHANGE
**********************************************************************

*Sortieren
  IF s1 = 'X'.
    SORT work BY lgpla matnr exidv rolln charg.
  ELSEIF s2 = 'X'.
    SORT work BY matnr exidv rolln charg lgpla.
  ENDIF.

*Positionnr. einfügen nach Sort
  LOOP AT work.
    work-ivpos = wposnr.
    MODIFY work.
    wposnr = wposnr + 1.

*Tabelle für Inventurhistorie füllen
    IF pecht = 'X'.
      CLEAR: zle_inventurhist.
      zle_inventurhist-ivnum    = work-ivnum.
      zle_inventurhist-ivpos    = work-ivpos.
      zle_inventurhist-idatu    = sy-datum.
      zle_inventurhist-uname    = sy-uname.
      zle_inventurhist-werks    = work-werks.
      zle_inventurhist-lgnum    = work-lgnum.
      zle_inventurhist-lgtyp    = work-lgtyp.
      zle_inventurhist-lgpla    = work-lgpla.
      zle_inventurhist-lqnum    = work-lqnum.
      zle_inventurhist-matnr    = work-matnr.
      zle_inventurhist-charg    = work-charg.
      zle_inventurhist-rolln    = work-rolln.
      zle_inventurhist-exidv    = work-exidv.
      zle_inventurhist-ivnumold = work-ivnumold.
      zle_inventurhist-ivposold = work-ivposold.
      zle_inventurhist-idatuold = work-ivdatuold.
      zle_inventurhist-state    = 'P'.     "immer Pendent beim Einfügen
      INSERT zle_inventurhist.
    ENDIF.
  ENDLOOP.

*Ausdruck mit SAP Script
  IF pscript = 'X'.
    PERFORM formular_oeffnen.

    IF ptot = ' '.  "ohne Packstücktotal
      LOOP AT work.
        AT FIRST.
          PERFORM ausgabe_beleg_head.
        ENDAT.
        CLEAR: wlgpla, wmatnr, wcharg, wrolln, wexidv, wgesme, wmeins.
        wlgpla = work-lgpla.
        wmatnr = work-matnr.
        wmaktx = work-maktx.
        wcharg = work-charg.
        wrolln = work-rolln.
        wexidv = work-exidv.
        wgesme = work-gesme.
        wmeins = work-meins.
        PERFORM ausgabe_beleg_pos.
      ENDLOOP.

    ELSE.  "mit Packstücktotal
      LOOP AT work.
        MOVE-CORRESPONDING work TO work2.
        work2-wgesme = work-gesme.
        work2-wexidv  = work-exidv.
        APPEND work2.
      ENDLOOP.

      LOOP AT work2.
        AT FIRST.
          PERFORM ausgabe_beleg_head.
        ENDAT.
        CLEAR: wlgpla, wmatnr, wcharg, wrolln, wexidv, wgesme, wmeins.
        wlgpla = work2-lgpla.
        wmatnr = work2-matnr.
        wmaktx = work2-maktx.
        wcharg = work2-charg.
        wrolln = work2-rolln.
        wexidv = work2-exidv.
        wgesme = work2-gesme.
        wmeins = work2-meins.
        PERFORM ausgabe_beleg_pos.

        AT END OF wexidv.
          SUM.
          wtp = work2-gesme.
          PERFORM ausgabe_total_packstueck.

        ENDAT.
      ENDLOOP.

    ENDIF.
    CALL FUNCTION 'CLOSE_FORM'.
  ENDIF.

***********************************************************************
* Listformataufbereitung (Tabellenüberschriften)
***********************************************************************

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZLE_INVENTUR'
    CHANGING
      ct_fieldcat            = it_fieldcatalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  READ TABLE it_fieldcatalog
    ASSIGNING <wa_fieldcatalog>
    WITH KEY fieldname = 'EXIDV'.
  IF sy-subrc IS INITIAL.
    <wa_fieldcatalog>-seltext_l    = 'Packstücknr.'.
    <wa_fieldcatalog>-seltext_m    = 'Packstücknr.'.
    <wa_fieldcatalog>-seltext_s    = 'Packstücknr.'.
    <wa_fieldcatalog>-reptext_ddic = 'Packstücknr.'.
  ENDIF.


  READ TABLE it_fieldcatalog
  ASSIGNING <wa_fieldcatalog>
  WITH KEY fieldname = 'CHAR15'.
  IF sy-subrc IS INITIAL.
    <wa_fieldcatalog>-seltext_l    = 'Zählmenge'.
    <wa_fieldcatalog>-seltext_m    = 'Zählmenge'.
    <wa_fieldcatalog>-seltext_s    = 'Zählmenge'.
    <wa_fieldcatalog>-reptext_ddic = 'Zählmenge'.
  ENDIF.


***********************************************************************
* Datenausgabe
***********************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'ZLE_INVENTUR'
      i_save           = 'A'
      is_variant       = w_isvariant
      it_fieldcat      = it_fieldcatalog
    TABLES
      t_outtab         = work
    EXCEPTIONS
      program_error    = 1.
*&---------------------------------------------------------------------*
*&      Form  formular_oeffnen
*&---------------------------------------------------------------------*
FORM formular_oeffnen.
  IF dest IS INITIAL.
    xdialog = 'X'.
  ELSE.
    CLEAR xdialog.
  ENDIF.
  itcpo-tddest = dest.
  itcpo-tdnewid = 'X'.
  itcpo-tdimmed = 'X'.
  itcpo-tddelete = ' '.

  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      device   = 'PRINTER'
      dialog   = xdialog
      form     = xform
      language = sy-langu
      options  = itcpo
    EXCEPTIONS
      canceled = 01
      device   = 02
      form     = 03
      options  = 04
      unclosed = 05.

ENDFORM.                    " formular_oeffnen
*&---------------------------------------------------------------------*
*&      Form  ausgabe_beleg_pos
*&---------------------------------------------------------------------*
FORM ausgabe_beleg_pos.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'POS'
      window  = 'MAIN'.
ENDFORM.                    " ausgabe_beleg_pos

*&---------------------------------------------------------------------*
*&      Form  ausgabe_beleg_head
*&---------------------------------------------------------------------*
FORM ausgabe_beleg_head.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'HEAD'
      window  = 'KOPF'.
ENDFORM.                    " ausgabe_beleg_head

*&---------------------------------------------------------------------*
*&      Form  ausgabe_total_packstueck
*&---------------------------------------------------------------------*
FORM ausgabe_total_packstueck.
  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      element = 'TP'
      window  = 'MAIN'.
ENDFORM.                    " ausgabe_total_packstueck
