************************************************************************
* 2010-08-02   smartShift project

************************************************************************

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

  PERFORM scan_input_window USING 'C'.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
    g_aufnr = g_scanval.

* AUFTRAGSNUMMER UND NACHLAGERPLATZ MIT NULLEN AUFFÜLLEN
    CONCATENATE '000000000000' g_aufnr INTO l_benum
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    SHIFT l_benum RIGHT DELETING TRAILING ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
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

*  g_iwef = '123.4'.
*  g_swef = '213.4'.
*  g_dwef = g_iwef - g_swef.
*
*  g_iwec = '567.8'.
*  g_swec = '321.8'.
*  g_dwec = g_iwec - g_swec.



  SELECT SINGLE * FROM lqua INTO g_lqua
                       WHERE lgnum = w_lt01_batch-lgnum
                            AND   matnr = w_lt01_batch-matnr
                            AND   werks = w_lt01_batch-werks
                            AND   lgtyp = '100'
                            AND   lgpla = g_lgpla.
  IF sy-subrc = 0.
    g_spvbc = g_lqua-verme.

    PERFORM disp_input_m04.

    IF g_scode  IS INITIAL.
      i_anfme = g_ipvbc.
      IF g_spvbc = i_anfme.
        w_lt01_batch-anfme = i_anfme.
      ELSE.
        w_lt01_batch-anfme = i_anfme.
        PERFORM difgferenzmenge_buchen.
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



ENDFORM.                    " scan_comp_m04
*&---------------------------------------------------------------------*
*&      Form  fill_W_IMSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_w_imseg.

  DATA i_meng_p LIKE imseg-menge.
  DATA: i_some LIKE lqua-verme.
  DATA: i_isme LIKE lqua-verme.


  w_imseg-erfme = 'KG'.
  w_imseg-meins = 'KG'.

  i_some = g_spvbc.
  i_isme = w_lt01_batch-anfme.


  IF i_some < i_isme.
    w_imseg-bwart = '262'.
    i_meng_p = i_isme - i_some.

  ELSEIF i_some > i_isme.
    w_imseg-bwart = '261'.
    i_meng_p = i_some - i_isme.
  ENDIF.

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



ENDFORM.                    " fill_W_IMSEG

*&---------------------------------------------------------------------*
*&      Form  difgferenzmenge_buchen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM difgferenzmenge_buchen.

  CLEAR: w_imseg.
  PERFORM fill_w_imseg.
  PERFORM buchen_diff.

ENDFORM.                    " difgferenzmenge_buchen

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
*  PERFORM fill_dynpro USING w_lt01_batch-charg   'LTAP-CHARG'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.

* call Transaktion
  PERFORM start_dynpro USING 'SAPML03T' '0102' 'X'.

* call IT9000 Anstellung
  PERFORM fill_dynpro USING '001'                'LTAP-LETYP'.
  PERFORM fill_dynpro USING w_lt01_batch-nlber   'LTAP-VLBER'.
  PERFORM fill_dynpro USING w_lt01_batch-nlpla   'LTAP-VLPLA'.
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.

* call Transaktion
  PERFORM start_dynpro USING 'SAPML03T' '0102' 'X'.

* call IT9000 Anstellung
  PERFORM fill_dynpro USING '/00'                'BDC_OKCODE'.


  CALL TRANSACTION 'LT01'
        USING bdcdata
        MODE 'N'
        MESSAGES INTO messtab.

  IF sy-subrc = 0.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '003' .
  ELSE.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '004'.
    LOOP AT messtab.
*      IF messtab-msgtyp = 'A'.


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

*



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
