************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_F1C                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  init_M03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_m03.

  CLEAR g_scode.

  CLEAR w_lt01_batch.

  w_lt01_batch-lgnum = '620'.
  w_lt01_batch-betyp = 'F'.
  w_lt01_batch-werks = '6000'.
  w_lt01_batch-lgort = '6200'.
  w_lt01_batch-bwlvs = '911'.


ENDFORM.                                                    " init_M03
*&---------------------------------------------------------------------*
*&      Form  scan_Fertauf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_fertauf.

  DATA: l_benum(20)  TYPE c.

* scannen Fertigungsauftrag.

  CLEAR g_scanval.

  scantitel = text-300.     "Fertigungsauftrag.

  PERFORM scan_input_window USING 'C'.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
    w_lt01_batch-benum = g_scanval.

* AUFTRAGSNUMMER UND NACHLAGERPLATZ MIT NULLEN AUFFÜLLEN
    CONCATENATE '0000000000' w_lt01_batch-benum INTO l_benum
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    SHIFT l_benum RIGHT DELETING TRAILING ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_lt01_batch-benum = l_benum+10(10).

    w_lt01_batch-nlpla = w_lt01_batch-benum.

  ENDIF.



ENDFORM.                    " scan_Fertauf
*&---------------------------------------------------------------------*
*&      Form  scan_Masch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_masch.

* scannen Maschinenummer.

  CLEAR g_scanval.

  scantitel = text-301.     "Maschinenummer.

  PERFORM scan_input_window USING 'C'.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.


    w_lt01_batch-nlber = g_scanval.
  ENDIF.





ENDFORM.                    " scan_Masch
*&---------------------------------------------------------------------*
*&      Form  scan_comp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_comp.

* scannen Compount / Matnr.


  DATA:  l_matnr(36) TYPE c.

  CLEAR g_scanval.

  scantitel = text-302.     "Compount Nr.

  PERFORM scan_input_window USING 'C'.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.

    w_lt01_batch-matnr = g_scanval.

    CONCATENATE '000000000000000000'  w_lt01_batch-matnr INTO l_matnr
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    SHIFT l_matnr RIGHT DELETING TRAILING ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_lt01_batch-matnr = l_matnr+18(18).

  ENDIF.

ENDFORM.                    " scan_comp
*&---------------------------------------------------------------------*
*&      Form  charge_bestimmen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM charge_bestimmen.


  DATA: l_lqua LIKE TABLE OF lqua.
  DATA: w_lqua LIKE lqua.

  SELECT * FROM lqua
           INTO TABLE l_lqua
           WHERE
           lgnum = '620'   AND
           lgort = '6200'  AND
           lgtyp = '001'   AND
           bestq = ''      AND
           matnr = w_lt01_batch-matnr.

  IF sy-subrc <> '0'.
    g_scode = '1'.
   WRITE: 'Bestandestabelle enthält keinen Eintrag zu diesen Kriterien'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '400' .
    EXIT.
  ENDIF.

  LOOP AT l_lqua INTO w_lqua.
    IF w_lqua-gesme <> w_lqua-verme.
      DELETE l_lqua.
    ENDIF.
  ENDLOOP.

  SORT l_lqua BY vfdat ASCENDING charg DESCENDING.

  READ TABLE l_lqua INDEX 1 INTO w_lqua.

  IF sy-subrc = 0.

    WRITE w_lqua-verme TO w_lt01_batch-anfme.
    WRITE w_lqua-charg TO w_lt01_batch-charg.
    WRITE w_lqua-lqnum TO w_lt01_batch-ltap.
    .
  ELSE.

    g_scode = '1'.
    WRITE: 'Keine Bestände zu diesem Material am Lager.'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '401' .
  ENDIF.

ENDFORM.                    " charge_bestimmen
*&---------------------------------------------------------------------*
*&      Form  Auftr_lagerpl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM auftr_lagerpl.

  DATA: BEGIN OF messtab OCCURS 10.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF messtab.
  DATA:  l_text(100) TYPE c.
  DATA: i_mess(1) TYPE c.

  CLEAR    bdcdata.
  REFRESH  bdcdata.



* call Transaktion
  PERFORM start_dynpro USING 'SAPML01S' '0400' 'X'.

* call IT9000 Anstellung
  PERFORM fill_dynpro USING w_lt01_batch-lgnum   'LAGP-LGNUM'.
  PERFORM fill_dynpro USING '100'                'LAGP-LGTYP'.
  PERFORM fill_dynpro USING w_lt01_batch-nlpla   'LAGP-LGPLA'.
  PERFORM fill_dynpro USING '=REFR'              'BDC_OKCODE'.

* call Transaktion
  PERFORM start_dynpro USING 'SAPML01S' '0400' 'X'.

* call IT9000 Anstellung
  PERFORM fill_dynpro USING w_lt01_batch-lgnum   'LAGP-LGNUM'.
  PERFORM fill_dynpro USING '100'                'LAGP-LGTYP'.
  PERFORM fill_dynpro USING w_lt01_batch-nlpla   'LAGP-LGPLA'.
  PERFORM fill_dynpro USING w_lt01_batch-nlber   'LAGP-LGBER'.
  PERFORM fill_dynpro USING '=BU'              'BDC_OKCODE'.

  CALL TRANSACTION 'LS01N'
        USING bdcdata
        MODE 'N'
        MESSAGES INTO messtab.

  CLEAR i_mess.
  LOOP AT messtab.
    IF messtab-msgtyp = 'A'.
      i_mess = '1'.
      CALL FUNCTION 'MASS_MESSAGE_GET'
           EXPORTING
                arbgb             = messtab-msgid
                msgnr             = messtab-msgnr
                msgv1             = messtab-msgv1
                msgv2             = messtab-msgv2
                msgv3             = messtab-msgv3
                msgv4             = messtab-msgv4
           IMPORTING
                msgtext           = l_text
           EXCEPTIONS
                message_not_found = 1
                OTHERS            = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH l_text. .
      WRITE: / l_text.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH l_text .
    ENDIF.
  ENDLOOP.

  IF i_mess IS INITIAL.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '403' .
  ENDIF.


ENDFORM.                    " Auftr_lagerpl
*&---------------------------------------------------------------------*
*&      Form  Transportauftrag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transportauftrag.

  DATA: BEGIN OF messtab OCCURS 10.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF messtab.
  DATA:  l_text(100) TYPE c.
  data: i_mess(1) type c.

  CLEAR    bdcdata.
  REFRESH  bdcdata.

* call Transaktion
  PERFORM start_dynpro USING 'SAPML03T' '0101' 'X'.

* call IT9000 Anstellung
  PERFORM fill_dynpro USING w_lt01_batch-lgnum   'LTAK-LGNUM'.
  PERFORM fill_dynpro USING w_lt01_batch-benum   'LTAK-BENUM'.
  PERFORM fill_dynpro USING w_lt01_batch-betyp   'LTAK-BETYP'.
  PERFORM fill_dynpro USING w_lt01_batch-bwlvs   'LTAK-BWLVS'.
  PERFORM fill_dynpro USING w_lt01_batch-matnr   'LTAP-MATNR'.
  PERFORM fill_dynpro USING w_lt01_batch-anfme   'RL03T-ANFME'.
  PERFORM fill_dynpro USING w_lt01_batch-werks   'LTAP-WERKS'.
  PERFORM fill_dynpro USING w_lt01_batch-charg   'LTAP-CHARG'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.

* call Transaktion
  PERFORM start_dynpro USING 'SAPML03T' '0102' 'X'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.
  PERFORM fill_dynpro USING w_lt01_batch-anfme   'RL03T-ANFME'.
  PERFORM fill_dynpro USING '001'                'LTAP-LETYP'.


** call Transaktion
*  PERFORM start_dynpro USING 'SAPML03T' '0102' 'X'.
*  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.
  PERFORM fill_dynpro USING ''                   'LTAP-VLENR'.
  PERFORM fill_dynpro USING w_lt01_batch-ltap    'LTAP-VLQNR'.
  PERFORM fill_dynpro USING w_lt01_batch-nlber   'LTAP-NLBER'.
  PERFORM fill_dynpro USING w_lt01_batch-nlpla   'LTAP-NLPLA'.


  CALL TRANSACTION 'LT01'
        USING bdcdata
        MODE 'N'
        MESSAGES INTO messtab.

  CLEAR i_mess.
  LOOP AT messtab.
    IF messtab-msgtyp = 'A'.
      i_mess = '1'.
      CALL FUNCTION 'MASS_MESSAGE_GET'
           EXPORTING
                arbgb             = messtab-msgid
                msgnr             = messtab-msgnr
                msgv1             = messtab-msgv1
                msgv2             = messtab-msgv2
                msgv3             = messtab-msgv3
                msgv4             = messtab-msgv4
           IMPORTING
                msgtext           = l_text
           EXCEPTIONS
                message_not_found = 1
                OTHERS            = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH l_text. .
      WRITE: / l_text.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH l_text .
    ENDIF.
  ENDLOOP.

  IF i_mess IS INITIAL.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '003' .
  ENDIF.


*
ENDFORM.                    " Transportauftrag
