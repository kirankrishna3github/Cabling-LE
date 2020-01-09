*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_F1D                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  init_m04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_m04.

  CLEAR g_scode.

  CLEAR w_lt01_batch.
  CLEAR w_imseg.
  REFRESH t_imseg.


  w_lt01_batch-lgnum = '620'.
  w_lt01_batch-betyp = 'F'.
  w_lt01_batch-werks = '6000'.
  w_lt01_batch-lgort = '6200'.
  w_lt01_batch-bwlvs = '913'.

ENDFORM.                                                    " init_m04

*&---------------------------------------------------------------------*
*&      Form  scan_fertauf_M04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_fertauf_m04.


  DATA: l_benum(24)  TYPE c.

* scannen Fertigungsauftrag.

  CLEAR g_scanval.
  CLEAR g_aufnr.

  scantitel = text-300.     "Fertigungsauftrag.

  PERFORM scan_input_window USING 'C' c_fert.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
    g_aufnr = g_scanval.

* AUFTRAGSNUMMER UND NACHLAGERPLATZ MIT NULLEN AUFFÜLLEN
    CONCATENATE '000000000000' g_aufnr INTO l_benum.
    SHIFT l_benum RIGHT DELETING TRAILING ' '.
    g_aufnr = l_benum+12(12).
    g_lgpla = l_benum+14(10).
    w_lt01_batch-benum = g_lgpla.
    w_lt01_batch-nlpla = g_lgpla.

  ENDIF.

* Status Ermitteln   wenn nicht Tech. abgesch. keinen Buchung

  CHECK NOT g_aufnr IS INITIAL.
  CHECK     g_scode IS INITIAL..

  SELECT SINGLE * FROM aufk WHERE aufnr = g_aufnr.

  IF sy-subrc = 0.
    CLEAR jest.

    SELECT SINGLE * FROM jest WHERE
               objnr = aufk-objnr
           AND stat = 'I0045'.
    IF sy-subrc = 0.
      IF jest-inact = 'X'.
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '0010'.
        g_scode = '1'.  "ACHTUNG  MUSS PRODUKTIV WIEDER AKTIVIERT WERDE
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '0007'.
    g_scode = '1'.
    EXIT.

  ENDIF.

ENDFORM.                    " scan_fertauf_M04

*&---------------------------------------------------------------------*
*&      Form  scan_masch_M04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_masch_m04.

* scannen Maschinenummer.

  CLEAR g_scanval.

  scantitel = text-301.     "Maschinenummer.

  PERFORM scan_input_window USING 'C' c_masch.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
    w_lt01_batch-nlber = g_scanval.
  ENDIF.
ENDFORM.                    " scan_masch_M04

*&---------------------------------------------------------------------*
*&      Form  gew_best
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gew_best.

  DATA: i_anfme LIKE lqua-verme.

  SELECT SINGLE * FROM lqua INTO g_lqua
     WHERE      lgnum = w_lt01_batch-lgnum
       AND   matnr = w_lt01_batch-matnr
       AND   werks = w_lt01_batch-werks
       AND   lgort = w_lt01_batch-lgort    "CS15JUN2010
       AND   lgtyp = '100'
       AND   lgpla = w_lt01_batch-nlpla    "CS15JUN2010
       AND   charg = w_lt01_batch-charg.   "CS15JUN2010

  IF sy-subrc = 0.
    g_spvbc = g_lqua-verme.

    PERFORM disp_input_m04.

    IF g_scode  IS INITIAL.
      i_anfme = g_ipvbc.
      IF g_lqua-verme = i_anfme.            "CS15JUN2010
        w_lt01_batch-anfme = i_anfme.
      ELSE.
        w_lt01_batch-anfme = i_anfme.
        PERFORM differenzmenge_buchen       "CS15JUN2010
         USING g_lqua-verme                 "CS15JUN2010
               i_anfme.                     "CS22JUN2010
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '0009'.
    g_scode = '1'.
    EXIT.
  ENDIF.

ENDFORM.                    " gew_best

*&---------------------------------------------------------------------*
*&      Form  disp_input_m04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_input_m04.

* scannen Maschinenummer.

  CLEAR g_ipvbc.

*  scantitel = text-301.     "Maschinenummer.

  CALL SCREEN 2001 STARTING AT 2 2 ENDING AT 70 30.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
*    G_IPVBC.
  ENDIF.
ENDFORM.                    " disp_input_m04

*&---------------------------------------------------------------------*
*&      Form  scan_comp_m04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_comp_m04.

* scannen Compount / Matnr.

  DATA:  l_matnr(36) TYPE c.

  CLEAR g_scanval.

  scantitel = text-302.     "Compount Nr.

  PERFORM scan_input_window USING 'C' c_comp.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.

    w_lt01_batch-matnr = g_scanval.

    CONCATENATE '000000000000000000'  w_lt01_batch-matnr INTO l_matnr.
    SHIFT l_matnr RIGHT DELETING TRAILING ' '.
    w_lt01_batch-matnr = l_matnr+18(18).

  ENDIF.
ENDFORM.                    " scan_comp_m04
* Routine neu CS15JUN2010
*---------------------------------------------------------------------*
*       FORM scan_charg_m04                                CS14JUN2010*
*---------------------------------------------------------------------*
*       Chargennummer einlesen                                        *
*---------------------------------------------------------------------*
FORM scan_charg_m04.
* scannen Charge

  DATA:  l_charg(10) TYPE c.

  CLEAR g_scanval.

  scantitel = text-304.     "Compount Nr.

  PERFORM scan_input_window USING 'C' c_charg.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
*    IF g_scanval IS INITIAL.
*      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '405'.
*      g_scode = '1'.
*      EXIT.
*    ENDIF.
    l_charg = g_scanval.
    IF l_charg CO '0123456789 '.
      UNPACK l_charg TO w_lt01_batch-charg.
    ELSE.
      w_lt01_batch-charg = l_charg.
    ENDIF.

    PERFORM check_charge_m04.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  differenzmenge_buchen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM differenzmenge_buchen                              "CS15JUN2010
  USING iv_soll
        iv_ist.

  DATA i_meng_p LIKE imseg-menge.
  DATA: i_some LIKE lqua-verme.
  DATA: i_isme LIKE lqua-verme.

  CLEAR: w_imseg.
  CLEAR  t_imseg[].                  "CS22JUN2010

  w_imseg-erfme = 'KG'.
  w_imseg-meins = 'KG'.

  IF iv_soll < iv_ist.               "CS22JUN2010
    w_imseg-bwart = '262'.           "CS22JUN2010
    i_meng_p = iv_ist - iv_soll.     "CS22JUN2010
  ELSE.                              "CS22JUN2010
    w_imseg-bwart = '261'.           "CS22JUN2010
    i_meng_p = iv_soll - iv_ist.     "CS22JUN2010
  ENDIF.                             "CS22JUN2010

  w_imseg-werks = w_lt01_batch-werks.
  w_imseg-lgort = w_lt01_batch-lgort.

  w_imseg-menge = i_meng_p.
  w_imseg-erfmg = i_meng_p.

  w_imseg-matnr = g_lqua-matnr.
  w_imseg-werks = g_lqua-werks.
  w_imseg-charg = g_lqua-charg.
  w_imseg-lgort = g_lqua-lgort.
  w_imseg-aufnr = g_aufnr.

  APPEND w_imseg TO t_imseg.

  PERFORM buchen_diff.

ENDFORM.                    " differenzmenge_buchen

*&---------------------------------------------------------------------*
*&      Form  ta_M04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ta_m04.

  DATA: BEGIN OF messtab OCCURS 10.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF messtab.
  DATA:  l_text(100) TYPE c.

  DATA: l_lgb(1) TYPE c.
  DATA: l_memid(7) TYPE c.
  DATA: l_lgb1(1) TYPE c.


  CLEAR    bdcdata.
  REFRESH  bdcdata.

* call new Dynpro
  PERFORM start_dynpro USING 'SAPML03T' '0101' 'X'.
  PERFORM fill_dynpro USING w_lt01_batch-lgnum   'LTAK-LGNUM'.
  PERFORM fill_dynpro USING w_lt01_batch-benum   'LTAK-BENUM'.
  PERFORM fill_dynpro USING w_lt01_batch-betyp   'LTAK-BETYP'.
  PERFORM fill_dynpro USING w_lt01_batch-bwlvs   'LTAK-BWLVS'.
  PERFORM fill_dynpro USING w_lt01_batch-matnr   'LTAP-MATNR'.
  PERFORM fill_dynpro
   USING w_lt01_batch-charg  'LTAP-CHARG'.       "CSJUN2010
  PERFORM fill_dynpro USING w_lt01_batch-anfme   'RL03T-ANFME'.
  PERFORM fill_dynpro USING w_lt01_batch-werks   'LTAP-WERKS'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.

* call new Dynpro
  PERFORM start_dynpro USING 'SAPML03T' '0102' 'X'.
  PERFORM fill_dynpro USING '001'                'LTAP-LETYP'.
  PERFORM fill_dynpro USING w_lt01_batch-nlber   'LTAP-VLBER'.
  PERFORM fill_dynpro USING w_lt01_batch-nlpla   'LTAP-VLPLA'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.

* call new Dynpro
  PERFORM start_dynpro USING 'SAPML03T' '0102' 'X'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.

*Lagerbereich ins Memory
  l_memid = 'M_LGBER'.
  l_lgb = w_lt01_batch-nlber.
  FREE MEMORY ID l_memid.
  EXPORT l_lgb TO MEMORY ID l_memid.

  CALL TRANSACTION 'LT01'
        USING bdcdata
        MODE 'N'
        MESSAGES INTO messtab.

  IF sy-subrc = 0.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '003' .
  ELSE.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '004'.
    LOOP AT messtab.

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
*      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                                                    " ta_M04

*&---------------------------------------------------------------------*
*&      Module  STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2001 OUTPUT.
  SET PF-STATUS 'RM_RUECK'.
  SET TITLEBAR 'RM_RUECK'.

ENDMODULE.                 " STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2001 INPUT.

  ok_code = sy-ucomm.

  CASE g_scanval.
    WHEN 'ABBR'.
      ok_code = 'ABBR'.
    WHEN 'ENTR'.
      ok_code = 'ENTR'.
      CLEAR g_scanval.
  ENDCASE.


  CASE ok_code.
    WHEN 'ENTR'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'ABBR'.
      g_scode = '1'.
      CLEAR: sy-ucomm.
      CLEAR s_sel.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2001  INPUT
*&---------------------------------------------------------------------*
*&      Module  outp_prep_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE outp_prep_2001 OUTPUT.

ENDMODULE.                 " outp_prep_2001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  buchen_diff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buchen_diff.

  PERFORM we_buchen.

ENDFORM.                    " buchen_diff
* Routine neu CS15JUN2010
*&---------------------------------------------------------------------*
*&      Form  check_charge_m04
*&---------------------------------------------------------------------*
*      Charge prüfen M04
*----------------------------------------------------------------------*
FORM check_charge_m04.

  DATA: w_lqua LIKE lqua.

  SELECT SINGLE * FROM lqua
           INTO w_lqua
           WHERE
           lgnum = '620'               AND
           werks = '6000'              AND
           lgort = '6200'              AND
           lgtyp = '100'               AND
           bestq = ''                  AND
           lgpla = w_lt01_batch-nlpla  AND
           matnr = w_lt01_batch-matnr  AND
           charg = w_lt01_batch-charg.


  IF sy-subrc <> '0'.
    g_scode = '1'.
*   Message e404.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '404' .
    EXIT.
  ENDIF.

ENDFORM.                    " check_charge_m04
