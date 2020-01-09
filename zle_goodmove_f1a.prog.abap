************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*---------------------------------------------------------------------*
*       FORM we_zu_best                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM we_zu_best.

  PERFORM search_pos USING w_imseg-ebeln w_imseg-matnr
                           w_imseg-ebelp g_scode.


ENDFORM.                    " we_zu_best
*&---------------------------------------------------------------------*
*&      Form  scan_ebln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_ebln.

  DATA: l_benum(20) TYPE c.

* scannen der Einkaufsbest. Nr.
**  g_scanval =  p_ebeln.
  CLEAR g_scanval.

  scantitel = text-100.     "Bestellnummer.

  PERFORM scan_input_window USING 'C' c_best.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
    w_imseg-ebeln = g_scanval.

    CONCATENATE '0000000000' w_imseg-ebeln INTO l_benum
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    SHIFT l_benum RIGHT DELETING TRAILING ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_imseg-ebeln = l_benum+10(10).



    SELECT SINGLE * FROM ekpo WHERE ebeln = w_imseg-ebeln.
    IF sy-subrc <> 0.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '022' WITH w_imseg-ebeln.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '022' INTO  g_mestxt.
      WRITE: / g_mestxt.
    ELSE.
      w_imseg-insmk = ekpo-insmk.
    ENDIF.

  ENDIF.


ENDFORM.                    " scan_ebln
*&---------------------------------------------------------------------*
*&      Form  scan_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_matnr.

  DATA: l_matnr(36) TYPE c.

***  g_scanval =  p_matnr.
  CLEAR g_scanval.
  scantitel = text-101.     "Materialnummer.

  PERFORM scan_input_window USING 'C' c_mat.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.
    w_imseg-matnr = g_scanval.

    CONCATENATE '000000000000000000' w_imseg-matnr INTO l_matnr
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    SHIFT l_matnr RIGHT DELETING TRAILING ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_imseg-matnr = l_matnr+18(18).

    SELECT SINGLE * FROM ekpo WHERE ebeln = w_imseg-ebeln
                               AND matnr =   w_imseg-matnr.
    IF sy-subrc <> 0.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '021'
                        WITH w_imseg-ebeln w_imseg-matnr.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '021' INTO  g_mestxt.
      WRITE: / g_mestxt.
    ENDIF.

  ENDIF.

ENDFORM.                    " scan_matnr
*&---------------------------------------------------------------------*
*&      Form  scan_mge
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_mge.

***  g_scanval =  p_menge.
  CLEAR g_scanval.

  scantitel = text-102.     "Totalmenge.

  DO.
    CLEAR g_scanval.

    PERFORM scan_input_window USING 'C' C_TOT.
    IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      WRITE: / g_mestxt.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
      EXIT.
    ELSE.
      IF g_scanval NP '++.++.++++'.
        g_menge = g_scanval.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.                    " scan_mge
*&---------------------------------------------------------------------*
*&      Form  scan_vdat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_vdat.

  DATA: i_vfdat(10) TYPE c.

  WRITE  p_vfdat TO i_vfdat.

***  g_scanval =  i_vfdat.
  CLEAR g_scanval.
  scantitel = text-103.     "Verfalldatum.

  DO.
    CLEAR g_scanval.

    PERFORM scan_input_window USING 'C' c_verf.

    IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      WRITE: / g_mestxt.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
      EXIT.
    ELSE.

      IF g_scanval CP '++.++.++++'.

        w_imseg-vfdat+0(4) = g_scanval+6(4).
        w_imseg-vfdat+4(2) = g_scanval+3(2).
        w_imseg-vfdat+6(2) = g_scanval+0(2).
        EXIT.
      ENDIF.

    ENDIF.
  ENDDO.

ENDFORM.                    " scan_vdat
*&---------------------------------------------------------------------*
*&      Form  scan_charge
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_charge.

  DATA: i_stop(1) TYPE c.
  DATA: i_batch LIKE w_imseg-charg,
        i_meng_c(15) TYPE c,           "  LIKE w_imseg-menge,
        i_meng_p LIKE imseg-menge,
        i_meng_t LIKE imseg-menge.

  CLEAR i_meng_t.

  DO .
* Hier einbauen Scan der einzelnen Chargen
* Als Beispiel P_BATCH1.
    CLEAR g_scode.
    CLEAR: w_imseg-charg, w_imseg-menge.

    CLEAR g_scanval.
    scantitel = text-104.     "Charge.

    PERFORM scan_input_window USING 'C' C_CHARG.

    IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'..
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      WRITE: / g_mestxt.
      EXIT.
    ELSE.
*      BREAK-POINT.

      SPLIT g_scanval AT ' ' INTO i_batch i_meng_c
IN CHARACTER MODE .                              "smart: 2010-08-02 #116
      MOVE i_meng_c TO i_meng_p.

      IF i_meng_p IS INITIAL OR i_batch IS INITIAL.
        WRITE: / text-050.
        g_scode = '1'.
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '013' .
        CONTINUE.
      ENDIF.

      w_imseg-charg = i_batch.
      w_imseg-menge = i_meng_p.
      w_imseg-erfmg = i_meng_p.

      PERFORM chargen_check.
      CHECK g_scode IS INITIAL.

      PERFORM chargen_dupli CHANGING i_stop..
      IF NOT i_stop IS INITIAL.
        CLEAR i_stop.
        CONTINUE.
      ELSE.
        i_meng_t = i_meng_t + i_meng_p.
        WRITE:/ w_imseg-charg, w_imseg-menge, i_meng_t, g_menge.
        APPEND w_imseg TO t_imseg.
        CLEAR: w_imseg-charg, w_imseg-menge.
      ENDIF.

      IF i_meng_t > g_menge.
        MESSAGE ID 'ZDLE' TYPE 'I' NUMBER '020' WITH i_meng_t g_menge.
        EXIT.
      ELSEIF i_meng_t = g_menge.
        EXIT.
      ELSE.
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '014' .
      ENDIF.
    ENDIF.
  ENDDO.

  IF i_meng_t = g_menge.

*  ALLES KLAR, Kann gebucht werden
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '015' .
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '015' INTO  g_mestxt.
    WRITE: / g_mestxt.
    PERFORM we_buchen.
  ELSE.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '016' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '016' .
  ENDIF.

ENDFORM.                    " scan_charge
*&---------------------------------------------------------------------*
*&      Form  chargen_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chargen_check.

  DATA: i_lifnr TYPE lifnr.

* Suchen Lieferantennummer.
  SELECT SINGLE * FROM ekko WHERE ebeln = w_imseg-ebeln.
  IF sy-subrc = 0.
    i_lifnr = ekko-lifnr.

**  Einbau der Präfix Prüfung

    SELECT SINGLE * FROM zle_gm_chargen_p WHERE lifnr = i_lifnr.
    IF sy-subrc = 0.
      IF w_imseg-charg CS zle_gm_chargen_p-praefix.
        IF sy-fdpos > 0.
          g_scode = '1'.
          MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '017' .

          MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '017' INTO  g_mestxt.
          WRITE: / g_mestxt.
        ENDIF.
      ENDIF.
    ELSE.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '018' INTO  g_mestxt.
      WRITE: / g_mestxt.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '018' .

    ENDIF.

  ELSE.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '019' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '019' .

  ENDIF.

ENDFORM.                    " chargen_check
*&---------------------------------------------------------------------*
*&      Form  chargen_dupli
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_I_STOP  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM chargen_dupli CHANGING p_stop TYPE CLIKE.   "smart: 2010-08-02 #105

  DATA: l_imseg LIKE w_imseg.

  p_stop = '1'.

  READ TABLE t_imseg INTO l_imseg
       WITH KEY charg = w_imseg-charg.

  IF sy-subrc <> 0.
    CLEAR p_stop.
  ELSE.
    p_stop = '1'.

  ENDIF.

ENDFORM.                    " chargen_dupli
*&---------------------------------------------------------------------*
*&      Form  init_M01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_m01.

  CLEAR w_imseg.
  CLEAR g_scode.
  REFRESH t_imseg.

  w_imseg-bwart = '101'.
  w_imseg-kzbew = 'B'.
  w_imseg-erfme = 'KG'.
  w_imseg-meins = 'KG'.

ENDFORM.                                                    " init_M01

*&---------------------------------------------------------------------*
*&      Form  LT06_batch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EMKPF_MBLNR  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM lt06_batch USING    p_emkpf_mblnr TYPE      "smart: 2010-08-02 #105
  EMKPF-MBLNR p_emkpf_mjahr TYPE EMKPF-MJAHR.    "smart: 2010-08-02 #105

  DATA: i_jahr(4) TYPE c.
  DATA: BEGIN OF messtab OCCURS 10.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF messtab.
  DATA:  l_text(100) TYPE c.
  DATA: i_mess(1) TYPE c,
        ls_ltbk   type ltbk.

  i_jahr = p_emkpf_mjahr.

  CLEAR    bdcdata.
  REFRESH  bdcdata.

  CASE  s_sel.
    WHEN 'ZLEGM01'.

***********************************************************************
*Achtung: nicht ändern. Wenn System und Netzwerk zu stark belastet ist,
*         geht der LT06 auf die Bretter.....
      wsy_subrc = 1.
      WHILE wsy_subrc NE 0.
        WAIT UP TO 1 SECONDS.
        SELECT SINGLE * FROM mkpf WHERE mblnr = p_emkpf_mblnr
                                    AND mjahr = i_jahr.
        wsy_subrc = sy-subrc.
      ENDWHILE.
      WAIT UP TO 8 SECONDS.
***********************************************************************

* call Transaktion
      PERFORM start_dynpro USING 'SAPML02B' '0203' 'X'.

* call IT9000 Anstellung
*'RL02B-DUNKL muss D sein !
      PERFORM fill_dynpro USING p_emkpf_mblnr   'RL02B-MBLNR'.
      PERFORM fill_dynpro USING i_jahr          'RL02B-MJAHR'.
      PERFORM fill_dynpro USING 'D'             'RL02B-DUNKL'.
      PERFORM fill_dynpro USING '/00'           'BDC_OKCODE'.

*Mode muss E sein !
      CALL TRANSACTION 'LT06'
            USING bdcdata
            MODE 'E'
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
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '011' .
      ENDIF.


    WHEN 'ZLEGM02'.
***********************************************************************
*Achtung: nicht ändern. Wenn System und Netzwerk zu stark belastet ist,
*         geht der LT06 auf die Bretter.....
      wsy_subrc = 1.
      WHILE wsy_subrc NE 0.
        WAIT UP TO 1 SECONDS.
        SELECT SINGLE * FROM mkpf WHERE mblnr = p_emkpf_mblnr
                                    AND mjahr = i_jahr.
        wsy_subrc = sy-subrc.
      ENDWHILE.
      WAIT UP TO 8 SECONDS.
***********************************************************************


* call Transaktion
*'RL02B-DUNKL muss D sein !
      PERFORM start_dynpro USING 'SAPML02B' '0203' 'X'.
      PERFORM fill_dynpro USING p_emkpf_mblnr   'RL02B-MBLNR'.
      PERFORM fill_dynpro USING i_jahr          'RL02B-MJAHR'.
      PERFORM fill_dynpro USING 'D'             'RL02B-DUNKL'.
      PERFORM fill_dynpro USING '/00'           'BDC_OKCODE'.

*MODE muss E sein !
      CALL TRANSACTION 'LT06'
            USING bdcdata
            MODE 'E'
*            UPDATE 'S'
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
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '011' .
      ENDIF.


    WHEN 'ZLEGM04'.
*     Bewegungsart 261 erzeugt keinen Transportbedarf -> LT06 ist hier
*     überflüssig!
      exit.                                           "CS15JUN2010

***********************************************************************
*Achtung: nicht ändern. Wenn System und Netzwerk zu stark belastet ist,
*         geht der LT06 auf die Bretter.....
      wsy_subrc = 1.
      WHILE wsy_subrc NE 0.
        WAIT UP TO 1 SECONDS.
        SELECT SINGLE * FROM mkpf WHERE mblnr = p_emkpf_mblnr
                                    AND mjahr = i_jahr.
        wsy_subrc = sy-subrc.
      ENDWHILE.
      WAIT UP TO 8 SECONDS.
***********************************************************************

* call Transaktion
*'RL02B-DUNKL muss D sein !
      PERFORM start_dynpro USING 'SAPML02B' '0203' 'X'.
      PERFORM fill_dynpro USING p_emkpf_mblnr   'RL02B-MBLNR'.
      PERFORM fill_dynpro USING i_jahr          'RL02B-MJAHR'.
      PERFORM fill_dynpro USING 'D'             'RL02B-DUNKL'.
      PERFORM fill_dynpro USING '/00'           'BDC_OKCODE'.

*Mode muss E sein !
      CALL TRANSACTION 'LT06'
            USING bdcdata
            MODE 'E'
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
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '011' .
      ENDIF.



  ENDCASE.

ENDFORM.                    " LT06_batch
