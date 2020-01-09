*---------------------------------------------------------------------*
*--- Report      : ZWM_LIEF_UMB
*--- Datum       : 30.10.2013
*--- Entwickler  : Peter Huber, consource GmbH
*---------------------------------------------------------------------*
*--- Funktion    : Verbuchen von Umlagerungen nach 916er WM Zone
*---

*---------------------------------------------------------------------*
REPORT ZWM_LIEF_UMB.

INCLUDE   : ICONS.
TYPE-POOLS: SLIS.

TABLES: LQUA
      , MCH1
      , APQI
      , LAGP
      , MARM
      .

CONSTANTS: C_X TYPE C LENGTH 01 VALUE 'X'
         , C_A TYPE C LENGTH 01 VALUE 'A'
         , C_LGTYP TYPE LGTYP VALUE '916'
         , C_LGBER TYPE LGBER VALUE '001'
         , GROUP(12) VALUE 'WM-UMB-916'  "group name of session
         , KEEP(1) VALUE 'X'
         , HOLDDATE TYPE datum value '99991230'.
.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:   E_GROUP_OPENED.
DATA:   subrc type subrc.
DATA:   bi_menge(13).
DATA:   wvbeln(10) type n.
DATA:   wmenge(13) type n.
DATA:   wmenge2(13) type n.

PARAMETERS: USER LIKE SY-UNAME  no-display.


*----------------------------------------------------------------------*
*   data selection
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ALLES WITH FRAME TITLE TEXT-000.
SELECTION-SCREEN BEGIN OF BLOCK BSEL WITH FRAME TITLE TEXT-004.
PARAMETERS:   P_MATNR  type LQUA-MATNR OBLIGATORY
              .

SELECTION-SCREEN BEGIN OF BLOCK VON.
PARAMETERS    : P_WERKS  TYPE WERKS_D OBLIGATORY
              , P_LGORT  TYPE LGORT_D OBLIGATORY
              , P_LGNUM  type LGNUM OBLIGATORY
.
SELECTION-SCREEN END   OF BLOCK VON.

SELECT-OPTIONS: S_CHARG  FOR LQUA-CHARG
              , S_PACKST FOR MCH1-ZZEXIDV
              , S_TROMM  FOR MCH1-ZZROLLNR
              .
SELECTION-SCREEN END   OF BLOCK BSEL.


SELECTION-SCREEN BEGIN OF BLOCK ZIEL WITH FRAME TITLE TEXT-005.
PARAMETERS    : PZ_VBELN like likp-vbeln obligatory
              , PZ_SPRAS LIKE SY-LANGU DEFAULT SY-LANGU no-display
              .
SELECTION-SCREEN END   OF BLOCK ZIEL.

*SELECTION-SCREEN BEGIN OF BLOCK VERB WITH FRAME TITLE TEXT-006.
PARAMETERS    : P_SIMU   TYPE XFELD DEFAULT C_X no-display
              , P_ZNBU   TYPE XFELD DEFAULT C_X no-display.
.
*SELECTION-SCREEN END   OF BLOCK VERB.
SELECTION-SCREEN END   OF BLOCK ALLES.

*--- Klasse für Event-Handler ---*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

TYPES: BEGIN OF TY_MARC
     ,   MATNR TYPE MATNR
     ,   WERKS TYPE WERKS_D
     , END   OF TY_MARC
     , BEGIN OF TY_MARD
     ,   MATNR TYPE MATNR
     ,   WERKS TYPE WERKS_D
     ,   LGORT TYPE LGORT_D
     , END   OF TY_MARD
     , BEGIN OF TY_MBEW
     ,   MATNR TYPE MATNR
     ,   BWKEY TYPE BWKEY
     ,   STPRS TYPE STPRS
     , END   OF TY_MBEW
     .

DATA: GV_MARM TYPE MARM.

*--- ALV-Bereiche ---*
DATA: OK_CODE            LIKE SY-UCOMM
    , GV_REPID           LIKE SY-REPID
    , GV_LAYOUT          TYPE LVC_S_LAYO
    , IT_ALV             TYPE TABLE OF ZWM_LIEF_UMB
    , GV_ALV             LIKE LINE OF IT_ALV
    , GV_ALV_2           LIKE LINE OF IT_ALV
    , GV_TABIX_1         TYPE SYTABIX
    , GV_TABIX_2         TYPE SYTABIX
    , IT_ROWS            TYPE LVC_T_ROW
    , GV_ROWS            LIKE LINE OF IT_ROWS
    , CONT_ON_MAIN       TYPE SCRFNAME VALUE 'TROMMELN'
    , GRID1              TYPE REF TO CL_GUI_ALV_GRID
    , CUSTOM_CONTAINER1  TYPE REF TO CL_GUI_CUSTOM_CONTAINER
    , GV_SUBRC           TYPE SYSUBRC
    , GV_GROUPID         TYPE APQI-GROUPID
    , IT_FCAT            TYPE LVC_T_FCAT
    , GV_FCAT            LIKE LINE OF IT_FCAT
    , GV_VARIANT         TYPE DISVARIANT
    , GV_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER
    , GV_GM_HEAD         TYPE BAPI2017_GM_HEAD_01
    , IT_GM_ITEM         TYPE TABLE OF BAPI2017_GM_ITEM_CREATE
    , GV_GM_ITEM         LIKE LINE OF IT_GM_ITEM
    , IT_RETURN	         TYPE TABLE OF BAPIRET2
    , GV_RETURN          LIKE LINE OF IT_RETURN
    , GV_GM_MRET         TYPE BAPI2017_GM_HEAD_RET
    , GV_LIGHTS_NAME     TYPE LVC_CIFNM VALUE 'MESS_LIGHT'
    , D100_W_VON         TYPE WERKS_D
    , D100_L_VON         TYPE LGORT
    , D100_W_NACH        TYPE WERKS_D
    , D100_L_NACH        TYPE LGORT
    , D100_SIMU          TYPE C LENGTH 4
    , GV_STRING          TYPE STRING
    , GV_ROW_ID          TYPE LVC_S_ROID
    , GV_ROW             TYPE LVC_S_ROW
    , GV_COL             TYPE LVC_S_COL
    , IT_SPOPLI          TYPE TABLE OF SPOPLI
    , GV_SPOOLI          LIKE LINE OF IT_SPOPLI
    , IT_ZMM_LGORT_SPLIT TYPE TABLE OF ZMM_LGORT_SPLIT
    , GV_ZMM_LGORT_SPLIT LIKE LINE  OF IT_ZMM_LGORT_SPLIT
    , GV_ANSWER          TYPE C LENGTH 1
    , GV_STPRS_V         TYPE STPRS
    , GV_STPRS_N         TYPE STPRS
    , IT_DYNPFIELDS      TYPE TABLE OF DYNPREAD
    , GV_DYNPFIELDS      LIKE LINE  OF IT_DYNPFIELDS
    , IT_MARC            TYPE TABLE OF TY_MARC
    , GV_MARC            LIKE LINE  OF IT_MARC
    , IT_MARD            TYPE TABLE OF TY_MARD
    , GV_MARD            LIKE LINE  OF IT_MARD
    , IT_MBEW            TYPE TABLE OF TY_MBEW
    , GV_MBEW            LIKE LINE  OF IT_MBEW
    , GV_VAL01           TYPE BDCDATA-FVAL
    , GV_VAL02           TYPE BDCDATA-FVAL
    , gv_date            type char10
    , gv_time            type char08
    , gv_valid           type char1.
.


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS HANDLE_HOTSPOT_CLICK
      FOR EVENT HOTSPOT_CLICK  OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HANDLE_HOTSPOT_CLICK.

    READ TABLE IT_ALV INTO GV_ALV INDEX E_ROW_ID.
    CHECK SY-SUBRC IS INITIAL.

    CASE E_COLUMN_ID.

      WHEN 'MBLNR'. "--> MB03, Materialbeleg anzeigen
        CHECK NOT GV_ALV-MBLNR IS INITIAL.
        SET PARAMETER ID 'MBN'  FIELD GV_ALV-MBLNR.
        SET PARAMETER ID 'MJA'  FIELD GV_ALV-MJAHR.
        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

      WHEN 'MATNR'. "--> MM03, Materialstamm Grunddaten
        CHECK NOT GV_ALV-MATNR IS INITIAL.
        SET PARAMETER ID 'MAT'  FIELD GV_ALV-MATNR.
        SET PARAMETER ID 'MXX'  FIELD 'K'.
        CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

      WHEN 'CHARG'. "--> MSC3N, Charge anzeigen
        CHECK NOT GV_ALV-CHARG IS INITIAL.
        SET PARAMETER ID 'MAT'   FIELD GV_ALV-MATNR.
        SET PARAMETER ID 'CHA'   FIELD GV_ALV-CHARG.
        SET PARAMETER ID 'WRK'   FIELD P_WERKS.
        SET PARAMETER ID 'LAG'   FIELD P_LGORT.
        CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.

      WHEN 'GESME'. "--> MMBE; Bestandesübersicht
        CHECK NOT GV_ALV-GESME IS INITIAL.
        SET PARAMETER ID 'MAT'  FIELD GV_ALV-MATNR.
        SET PARAMETER ID 'WRK'  FIELD P_WERKS.
        SET PARAMETER ID 'LAG'  FIELD P_LGORT.
        CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

    ENDCASE.

    CLEAR: E_ROW_ID
         , E_COLUMN_ID
         , SY-UCOMM
         .

  ENDMETHOD.                    "HOTSPOT_CLICK

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION


*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_WERKS.

  CLEAR: IT_SPOPLI
       , IT_ZMM_LGORT_SPLIT
       , GV_ANSWER
       , IT_DYNPFIELDS
       .

  SELECT * FROM ZMM_LGORT_SPLIT INTO TABLE IT_ZMM_LGORT_SPLIT.

  LOOP AT IT_ZMM_LGORT_SPLIT INTO GV_ZMM_LGORT_SPLIT.

    CONCATENATE GV_ZMM_LGORT_SPLIT-WERKS '  '
                GV_ZMM_LGORT_SPLIT-LGORT '  --->  '
                'Lagertyp 001'
                'nach '
                'Lagertyp 916'
                PZ_VBELN
       INTO GV_SPOOLI-VAROPTION RESPECTING BLANKS.
    APPEND GV_SPOOLI TO IT_SPOPLI.

  ENDLOOP.

  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
      START_COL = 36
      START_ROW = 5
      TEXTLINE1 = TEXT-014
      TITEL     = TEXT-015
    IMPORTING
      ANSWER    = GV_ANSWER
    TABLES
      T_SPOPLI  = IT_SPOPLI.

  IF NOT GV_ANSWER = C_A.
    READ TABLE IT_ZMM_LGORT_SPLIT INTO GV_ZMM_LGORT_SPLIT
    INDEX GV_ANSWER.

    CLEAR GV_DYNPFIELDS.
    GV_DYNPFIELDS-FIELDNAME  = 'S_WERKS'.
    GV_DYNPFIELDS-FIELDVALUE = GV_ZMM_LGORT_SPLIT-WERKS.
    APPEND GV_DYNPFIELDS TO IT_DYNPFIELDS.

    CLEAR GV_DYNPFIELDS.
    GV_DYNPFIELDS-FIELDNAME  = 'S_LGORT'.
    GV_DYNPFIELDS-FIELDVALUE = GV_ZMM_LGORT_SPLIT-LGORT.
    APPEND GV_DYNPFIELDS TO IT_DYNPFIELDS.

    CLEAR GV_DYNPFIELDS.
    GV_DYNPFIELDS-FIELDNAME  = 'PZ_WERKS'.
    GV_DYNPFIELDS-FIELDVALUE = GV_ZMM_LGORT_SPLIT-WERKS_N.
    APPEND GV_DYNPFIELDS TO IT_DYNPFIELDS.

    CLEAR GV_DYNPFIELDS.
    GV_DYNPFIELDS-FIELDNAME  = 'PZ_LGORT'.
    GV_DYNPFIELDS-FIELDVALUE = GV_ZMM_LGORT_SPLIT-LGORT_N.
    APPEND GV_DYNPFIELDS TO IT_DYNPFIELDS.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        DYNAME               = 'ZWM_LIEF_UMB'
        DYNUMB               = '1000'
      TABLES
        DYNPFIELDS           = IT_DYNPFIELDS
      EXCEPTIONS
        INVALID_ABAPWORKAREA = 1
        INVALID_DYNPROFIELD  = 2
        INVALID_DYNPRONAME   = 3
        INVALID_DYNPRONUMMER = 4
        INVALID_REQUEST      = 5
        NO_FIELDDESCRIPTION  = 6
        UNDEFIND_ERROR       = 7
        OTHERS               = 8.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*

  PERFORM MAT_CHECK.
  PERFORM DATEN_LESEN.
  CALL SCREEN 100.


*---------------------------------------------------------------------*
*      Form  DATEN_LESEN
*---------------------------------------------------------------------*
FORM DATEN_LESEN.

  SELECT
    MCH1~ZZEXIDV
    LQUA~MATNR
    MARA~MTART
    MCH1~CHARG
    MCH1~ZZROLLNR
    LQUA~LGNUM
    LQUA~LGPLA
    MARA~IHIVI
    MAKT~MAKTX
    LQUA~GESME
    LQUA~MEINS
    ZMKDISP1~VBELN

  INTO TABLE IT_ALV

  FROM LQUA AS LQUA

  INNER JOIN MAKT AS MAKT
    ON MAKT~MATNR = LQUA~MATNR

  INNER JOIN MARA AS MARA
    ON MARA~MATNR = LQUA~MATNR

  INNER JOIN MARM AS MARM
    ON MARM~MATNR = LQUA~MATNR

  INNER JOIN LAGP AS LAGP
    ON  LAGP~LGNUM = LQUA~LGNUM
    AND LAGP~LGTYP = LQUA~LGTYP
    AND LAGP~LGPLA = LQUA~LGPLA

  INNER JOIN MCH1 AS MCH1
    ON  MCH1~MATNR  = LQUA~MATNR
    AND MCH1~CHARG  = LQUA~CHARG

  LEFT OUTER JOIN ZMKDISP1 AS ZMKDISP1
    ON  ZMKDISP1~MATNR EQ LQUA~MATNR
    AND ZMKDISP1~CHARG EQ LQUA~CHARG

  WHERE LQUA~MATNR EQ P_MATNR
  AND   LQUA~WERKS EQ P_WERKS
  AND   LQUA~LGORT EQ P_LGORT
  AND   LQUA~CHARG IN S_CHARG
  AND   LQUA~LGTYP EQ '001'
  AND   LQUA~VERME EQ LQUA~VERME
  AND   LQUA~VERME GE 0
  AND   LQUA~GESME GE 0
  AND   LQUA~MEINS EQ 'M'
  AND   MAKT~SPRAS EQ PZ_SPRAS
  AND   MARM~ATINN EQ 'CS_CONV_MPP'
  AND   LAGP~SKZUA EQ SPACE
  AND   LAGP~SKZUE EQ SPACE
  AND   LAGP~SKZSA EQ SPACE
  AND   LAGP~SKZSE EQ SPACE
  AND   LAGP~SKZSI EQ SPACE
  AND   LAGP~SPGRU EQ SPACE
  AND   MCH1~ZZEXIDV  IN S_PACKST
  AND   MCH1~ZZROLLNR IN S_TROMM.

  SORT IT_ALV.

  CLEAR: IT_MARC
       , IT_MARD
       , IT_MBEW
       .
*--- Daten zur Überprüfung vorlesen ---*


*--- Und noch sonstige Prüfungen ---*
  LOOP AT IT_ALV INTO GV_ALV
    WHERE MESSAGE IS INITIAL.
    GV_TABIX_1 = SY-TABIX.

    IF NOT SY-SUBRC IS INITIAL.
      GV_ALV-MESSAGE = TEXT-011.
      MODIFY IT_ALV FROM GV_ALV INDEX GV_TABIX_1.
      CHECK 1 = 2.
    ENDIF.

*--- Kein Packstück: MCH1-IHIVI = space; also darf auch keine ---*
*--- Packstücknummer da sein. Wenn doch, Fehler ---*
*--- Solche Fälle sind im QC2 vorhanden.... ---*
    IF GV_ALV-IHIVI = SPACE.
      IF NOT GV_ALV-ZZEXIDV IS INITIAL.
        GV_ALV-MESSAGE = TEXT-001.
        MODIFY IT_ALV FROM GV_ALV INDEX GV_TABIX_1.
        CHECK 1 = 2.
      ENDIF.
    ENDIF.

*--- Es darf keine Zuteilungen auf einer Charge sein ---*
    IF NOT GV_ALV-VBELN IS INITIAL.
      GV_ALV-MESSAGE = TEXT-002.
      MODIFY IT_ALV FROM GV_ALV INDEX GV_TABIX_1.
      CHECK 1 = 2.
    ENDIF.

*--- Stückmengenrechnung - GESME / Standardlänge
    GV_ALV-MENGE = GV_ALV-GESME / GV_MARM-UMREZ.
    MODIFY IT_ALV FROM GV_ALV INDEX GV_TABIX_1.
    CHECK 1 = 2.

*--- Packstück: MCH1-IHIVI = X
*--- Checken, ob zu einem Packstück def. keine Charge disponiert ist ---*
    IF GV_ALV-IHIVI = C_X.

      LOOP AT IT_ALV TRANSPORTING NO FIELDS
        WHERE MATNR   = GV_ALV-MATNR
        AND   ZZEXIDV = GV_ALV-ZZEXIDV
        AND   NOT VBELN IS INITIAL.
      ENDLOOP.

*--- Habe zum Packstück Dispositionen gefunden, also für alle ---*
*--- darin enthaltenen Chragen keine Verarbeitung ---*
      IF SY-SUBRC = 0.

        LOOP AT IT_ALV INTO GV_ALV_2
          WHERE MATNR   = GV_ALV-MATNR
          AND   ZZEXIDV = GV_ALV-ZZEXIDV.
          GV_TABIX_2 = SY-TABIX.
          GV_ALV_2-MESSAGE = TEXT-003.
          MODIFY IT_ALV FROM GV_ALV_2 INDEX GV_TABIX_2.
        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDLOOP.

*--- Buchbare Zeilen die Ampel auf Gelb setzen ---*
  LOOP AT IT_ALV INTO GV_ALV.

    CHECK GV_ALV-MESSAGE IS INITIAL.

    GV_TABIX_1 = SY-TABIX.
    GV_ALV_2   = GV_ALV.

*--- Bei Packstücken die erste Zeile ist immer OK ---*
    AT NEW MATNR.
      GV_ALV_2-MESS_LIGHT = 2.
      MODIFY IT_ALV FROM GV_ALV_2 INDEX GV_TABIX_1.
    ENDAT.

*--- Bei NICHT-Packstückgeführten ist an dieser Stelle sowieso OK ---*
    IF GV_ALV_2-IHIVI IS INITIAL.
      GV_ALV_2-MESS_LIGHT = 2.
      MODIFY IT_ALV FROM GV_ALV_2 INDEX GV_TABIX_1.
    ENDIF.

  ENDLOOP.

*--- Sollen die nicht buchbaren angezeigt werden ---*
*--- Nur für Entwicklungstest...
  IF NOT P_ZNBU IS INITIAL.
    DELETE IT_ALV WHERE NOT MESSAGE IS INITIAL.
  ENDIF.

ENDFORM.                    "DATEN_LESEN


*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM EXIT_PROGRAM.

  CALL METHOD CUSTOM_CONTAINER1->FREE.
  CALL METHOD CL_GUI_CFW=>FLUSH.
  LEAVE TO SCREEN 0.

ENDFORM.                    "EXIT_PROGRAM


*---------------------------------------------------------------------*
*      Module  PBO_100  OUTPUT
*---------------------------------------------------------------------*
MODULE PBO_100 OUTPUT.

  CLEAR: GV_LAYOUT
       , GV_VARIANT
       .
*--- Die Druck, resp. Funktionstaste für Buchen setzten ---*
  SET PF-STATUS 'MAIN100'.

  write sy-datum to gv_date DD/MM/YYYY.
  write sy-uzeit to gv_time using EDIT MASK '__:__:__'.
  GV_STRING = text-000.
  SET TITLEBAR 'MAIN100' WITH GV_STRING.


*--- Programm-ID für Returnaufruf merken ---*
  GV_REPID = SY-REPID.

*--- Container-Control "TROMMELN" erzeugen ---*
  IF CUSTOM_CONTAINER1 IS INITIAL.

    CREATE OBJECT CUSTOM_CONTAINER1
      EXPORTING
        CONTAINER_NAME              = CONT_ON_MAIN
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          TITEL = GV_REPID
          TXT2  = SY-SUBRC
          TXT1  = TEXT-007.
      EXIT.
    ENDIF.

*--- ALV-Control für GRID generieren ---*
    CREATE OBJECT GRID1
      EXPORTING
        I_PARENT = CUSTOM_CONTAINER1.

*--- Layoutangaben abfüllen ---*
    GV_LAYOUT-GRID_TITLE = TEXT-000.
    GV_LAYOUT-COL_OPT    = C_X.

*--- Layoutangaben abfüllen ---*
    GV_LAYOUT-SEL_MODE   = C_A.
    GV_LAYOUT-EXCP_FNAME = GV_LIGHTS_NAME.
    GV_VARIANT-REPORT    = GV_REPID.

*--- Und nun die Liste ins ALV schiessen ---*
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_STRUCTURE_NAME   = 'ZWM_LIEF_UMB'
        IS_LAYOUT          = GV_LAYOUT
        I_SAVE             = C_A
        I_BYPASSING_BUFFER = C_X
        I_DEFAULT          = C_X
        IS_VARIANT         = GV_VARIANT
      CHANGING
        IT_OUTTAB          = IT_ALV.

*--- Den Focus aufs GRID setzten ---*
    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
      EXPORTING
        CONTROL = GRID1.

*--- Ereignis erzeugen und registrieren ---*
    CREATE OBJECT GV_EVENT_RECEIVER.
    SET HANDLER GV_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR GRID1.

*--- Den Feldkatalog retour lesen ---*
    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = IT_FCAT.

*--- Die Hotspots erzeugen ---*
    LOOP AT IT_FCAT INTO GV_FCAT
      WHERE FIELDNAME = 'MBLNR'
      OR    FIELDNAME = 'MATNR'
      OR    FIELDNAME = 'CHARG'
      OR    FIELDNAME = 'GESME'.
      GV_FCAT-HOTSPOT = C_X.
      MODIFY IT_FCAT FROM GV_FCAT INDEX SY-TABIX.
    ENDLOOP.

*--- Buchungsmenge im GRID eingebabereit machen ---*
    LOOP AT IT_FCAT INTO GV_FCAT
  WHERE FIELDNAME = 'AUSME'.
*      GV_FCAT-NO_ZERO = C_X.
*      GV_FCAT-NO_OUT  = C_X.
      GV_FCAT-EDIT    = C_X.
      MODIFY IT_FCAT FROM GV_FCAT INDEX SY-TABIX.
    ENDLOOP.

*--- Und Feldkatalog mit Hotspots setzten ---*
    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
      EXPORTING
        IT_FIELDCATALOG = IT_FCAT.

*--- Und notabene das Ding nochmals senden ---*
    CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*--- Werk und Lagerort ins Dynpro setzten ---*
    D100_W_VON  = P_WERKS.
    D100_L_VON  = P_LGORT.

  ENDIF.

ENDMODULE.                             " PBO_100  OUTPUT


*----------------------------------------------------------------------*
*  MODULE PAI_100 INPUT
*----------------------------------------------------------------------*
MODULE PAI_100 INPUT.

  CASE OK_CODE.

    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.


    WHEN 'CHECK'.
      PERFORM CHECK.

*--- So so, tatsächlich umbuchen ---*
    WHEN 'BUCHEN'.

*--- Buchungsmengen AUSME für Verbuchung übernehmen ---*
      grid1->check_changed_data( importing e_valid = gv_valid ).

*--- Aber dann müssen selektierte Zeilen da sein ---*
      CLEAR IT_ROWS.
      CALL METHOD GRID1->GET_SELECTED_ROWS
        IMPORTING
          ET_INDEX_ROWS = IT_ROWS.

*--- Und Speicherbereich grad wieder löschen ---*
      CALL METHOD CL_GUI_CFW=>FLUSH.
      IF SY-SUBRC NE 0.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            TITEL = GV_REPID
            TXT2  = SY-SUBRC
            TXT1  = 'Error in Flush'(500).
      ELSE.

*--- Die marierten Zeilen abarbeiten ---*
        IF NOT IT_ROWS IS INITIAL.
          PERFORM CHECK.
          PERFORM DATEN_AUFBEREITEN.

          CALL METHOD GRID1->GET_SCROLL_INFO_VIA_ID
            IMPORTING
              ES_ROW_NO   = GV_ROW_ID
              ES_ROW_INFO = GV_ROW
              ES_COL_INFO = GV_COL.

*--- Die Anzeige neu ausgeben mit den gebuchten Belegen ---*
          CALL METHOD GRID1->REFRESH_TABLE_DISPLAY
            EXCEPTIONS
              FINISHED = 1
              OTHERS   = 2.
          IF SY-SUBRC <> 0.
            MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.

        ENDIF.
      ENDIF.

  ENDCASE.


  CLEAR OK_CODE.

ENDMODULE.                             " PAI_100  INPUT


*---------------------------------------------------------------------*
*      Form  DATEN_AUFBEREITEN
*---------------------------------------------------------------------*
FORM DATEN_AUFBEREITEN .

  CLEAR: GV_GM_HEAD
       , IT_GM_ITEM
       .

  LOOP AT IT_ROWS INTO GV_ROWS.
    GV_TABIX_2 = GV_TABIX_1 = GV_ROWS-INDEX.

    READ TABLE IT_ALV INTO GV_ALV INDEX GV_TABIX_1.

*--- Kontrolle, ob das Ding überhaupt buchbar ist ---*
    IF GV_ALV-MESS_LIGHT = 2  "<- unberührt
    OR GV_ALV-MESS_LIGHT = 1. "<- fehlerhaft gebucht

*--- Handelt es sich um ein Packstück, dann alle dazu buchen*
      IF NOT GV_ALV-IHIVI IS INITIAL.
        GV_ALV_2 = GV_ALV.

        LOOP AT IT_ALV INTO GV_ALV
          WHERE ZZEXIDV = GV_ALV_2-ZZEXIDV
          AND   MATNR   = GV_ALV_2-MATNR.

          GV_TABIX_1 = SY-TABIX.
          PERFORM UMBUCHEN.

        ENDLOOP.

      ELSE.

*--- Trommeln buchen ---*
        PERFORM UMBUCHEN.

      ENDIF.
    ENDIF.
  ENDLOOP.

  leave to transaction 'ZWM_LIEF_UMB'.

ENDFORM.                    " DATEN_AUFBEREITEN
*---------------------------------------------------------------------*
*      Form  UMBUCHEN
*---------------------------------------------------------------------*
FORM UMBUCHEN.

  CLEAR: GV_GM_ITEM
       , IT_GM_ITEM
       .
*"----------------------------------------------------------------------

*Dynamischer Lagerplatz zur Lieferung anlegen...
  select single * from lagp where lgnum = p_lgnum
                              and lgtyp = '916'
                              and lgpla = pz_vbeln.

  if sy-subrc ne 0.

    lagp-lgnum = p_lgnum.
    lagp-lgtyp = c_lgtyp.
    lagp-lgpla = pz_vbeln.
    lagp-lgber = c_lgber.
    insert lagp.

  endif.

* --- Batch Input Menge aufbereiten wegen Feldlänge
  clear: bi_menge.
  write gv_alv-ausme to bi_menge.


  perform bdc_dynpro      using 'SAPML03T' '0101'.
  perform bdc_field       using 'BDC_CURSOR' 'RL03T-DUNKL'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'LTAK-LGNUM' p_lgnum.
  perform bdc_field       using 'LTAK-BWLVS' '999'.
  perform bdc_field       using 'LTAP-MATNR' GV_ALV-MATNR.
  perform bdc_field       using 'RL03T-ANFME' bi_menge.
  perform bdc_field       using 'LTAP-WERKS' P_WERKS.
  perform bdc_field       using 'LTAP-LGORT' P_LGORT.
  perform bdc_field       using 'LTAP-CHARG' GV_ALV-CHARG.
  perform bdc_field       using 'RL03T-DUNKL' 'H'.
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform bdc_field       using 'BDC_CURSOR' 'RL03T-SQUIT'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RL03T-ANFME' bi_menge.
  perform bdc_field       using 'LTAP-ALTME' GV_ALV-MEINS.
  perform bdc_field       using 'RL03T-SQUIT' 'X'.
  perform bdc_field       using 'LTAP-VLTYP' '001'.
  perform bdc_field       using 'LTAP-VLPLA' GV_ALV-LGPLA.
  perform bdc_field       using 'LTAP-NLTYP' '916'.
  perform bdc_field       using 'LTAP-NLPLA' PZ_VBELN.


  CALL TRANSACTION 'LT01'
        USING         BDCDATA
        MODE          'N'
        UPDATE        'L'
        MESSAGES INTO MESSTAB.

  CLEAR SUBRC.
  LOOP AT MESSTAB WHERE MSGTYP CO 'EA'.
    SUBRC = 04.
  ENDLOOP.

  IF NOT SUBRC IS INITIAL.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        CLIENT   = SY-MANDT
        GROUP    = GROUP
        USER     = sy-uname
        KEEP     = KEEP
        HOLDDATE = HOLDDATE.

    CALL FUNCTION 'BDC_INSERT'
      EXPORTING
        TCODE     = 'LT01'
      TABLES
        DYNPROTAB = BDCDATA.

    CALL FUNCTION 'BDC_CLOSE_GROUP'.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.

  ENDIF.

  REFRESH: BDCDATA
         , MESSTAB
         .


ENDFORM.                    "UMBUCHEN
**----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT = SY-MANDT
      GROUP  = 'ECPCA-WMTA'
      USER   = sy-uname.
ENDFORM.                    "OPEN_GROUP

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.                    "CLOSE_GROUP

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      TCODE     = TCODE
    TABLES
      DYNPROTAB = BDCDATA.

  REFRESH BDCDATA.
ENDFORM.                    "BDC_TRANSACTION

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  MAT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAT_CHECK .

  clear: gv_marm.
  select single * from marm into gv_marm where matnr = p_matnr
                                           and meinh = 'ST '.
  if sy-subrc ne 0.
    MESSAGE I398(00) WITH TEXT-018.
    leave to transaction 'ZWM_LIEF_UMB'.
  endif.

ENDFORM.                    " MAT_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK .

  CLEAR: GV_GM_HEAD
       , IT_GM_ITEM
       .

  LOOP AT IT_ROWS INTO GV_ROWS.
    GV_TABIX_2 = GV_TABIX_1 = GV_ROWS-INDEX.

    READ TABLE IT_ALV INTO GV_ALV INDEX GV_TABIX_1.

*--- Kontrolle, ob das Ding überhaupt buchbar ist ---*
    IF GV_ALV-MESS_LIGHT = 2  "<- unberührt
    OR GV_ALV-MESS_LIGHT = 1. "<- fehlerhaft gebucht
    endif.

    if gv_alv-gesme LT gv_ALV-AUSME.
      MESSAGE E398(00) WITH TEXT-019.

    ENDIF.

*--- Prüfen, ob Menge eine Packeinheit entspricht
    clear: wmenge, wmenge2.
    wmenge = gv_ALV-ausme / gv_marm-umrez.
    wmenge2 = wmenge * gv_marm-umrez.
    if not wmenge2 = gv_alv-ausme.
      MESSAGE E398(00) WITH TEXT-020.
    endif.

*--- Buchungsmenge kann nicht kleiner als Packmenge sein
    if gv_marm-umrez gt gv_alv-ausme.
      MESSAGE E398(00) WITH TEXT-021.
    endif.


  ENDLOOP.

ENDFORM.                    " CHECK
