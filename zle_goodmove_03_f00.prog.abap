************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_F00                                           *
*----------------------------------------------------------------------*

FORM create_goods_movement.

  DATA: l_imkpf LIKE imkpf.
  DATA: t_emseg LIKE TABLE OF emseg.
  DATA: l_emseg LIKE emseg.
  DATA: l_emkpf LIKE emkpf.
  DATA: l_emkpf1 LIKE emkpf.
  DATA:  l_text(100) TYPE c.

  l_imkpf-bldat = sy-datum.
  l_imkpf-budat = sy-datum.

  CALL FUNCTION 'MB_CREATE_GOODS_MOVEMENT'
       EXPORTING
            imkpf = l_imkpf
            ctcod = 'MIGO'
       IMPORTING
            emkpf = l_emkpf
       TABLES
            emseg = t_emseg
            imseg = t_imseg.

  IF l_emkpf-subrc = '1'.
    CALL FUNCTION 'MB_POST_GOODS_MOVEMENT'
         IMPORTING
              emkpf = l_emkpf.
    IF l_emkpf-subrc IS INITIAL.
      COMMIT WORK.
      WAIT UP TO 2 SECONDS.
      WRITE: / text-060.
      PERFORM lt06_batch USING l_emkpf-mblnr l_emkpf-mjahr.
    ELSE.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH text-051.
      WRITE: / text-051.
    ENDIF.

  ELSE.
    CALL FUNCTION 'MASS_MESSAGE_GET'
         EXPORTING
              arbgb             = l_emkpf-msgid
              msgnr             = l_emkpf-msgno
              msgv1             = l_emkpf-msgv1
              msgv2             = l_emkpf-msgv2
              msgv3             = l_emkpf-msgv3
              msgv4             = l_emkpf-msgv4
         IMPORTING
              msgtext           = l_text
         EXCEPTIONS
              message_not_found = 1
              OTHERS            = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    WRITE: /.
    WRITE: / text-051.
    WRITE: / l_text.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' with  l_text. .

    LOOP AT t_emseg INTO l_emseg.
      IF NOT l_emseg-msgid IS INITIAL.
        CALL FUNCTION 'MASS_MESSAGE_GET'
             EXPORTING
                  arbgb             = l_emseg-msgid
                  msgnr             = l_emseg-msgno
                  msgv1             = l_emseg-msgv1
                  msgv2             = l_emseg-msgv2
                  msgv3             = l_emseg-msgv3
                  msgv4             = l_emseg-msgv4
             IMPORTING
                  msgtext           = l_text
             EXCEPTIONS
                  message_not_found = 1
                  OTHERS            = 2.
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' with l_text. .

        WRITE: / l_text.
      ENDIF.


    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.


  PERFORM user_command_1001.

ENDMODULE.                 " USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'SEL_P'.
  SET TITLEBAR 'SEL_P'.

ENDMODULE.                 " STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET PF-STATUS 'INP_1A'.
  SET TITLEBAR 'INP_1A'.

ENDMODULE.                 " STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  search_pos
*&---------------------------------------------------------------------*
*       Suchen der EK-Position mit Matnr. und EK Nr.
*----------------------------------------------------------------------*
*      -->P_P_EBLN  text
*      -->P_P_MATNR  text
*      -->P_W_IMSEG_EBPOS  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM search_pos USING    p_ebln TYPE IMSEG-EBELN "smart: 2010-08-02 #105
                         p_matnr TYPE IMSEG-MATNR"smart: 2010-08-02 #105
                         w_imseg_ebelp TYPE      "smart: 2010-08-02 #105
                           IMSEG-EBELP           "smart: 2010-08-02 #105
                         i_scode LIKE g_scode.   "smart: 2010-08-02 #105

  DATA: l_ebln(20)  TYPE c,
        l_matnr(36) TYPE c.


  CONCATENATE '0000000000' p_ebln INTO l_ebln IN "smart: 2010-08-02 #101
    CHARACTER MODE .                             "smart: 2010-08-02 #101
  SHIFT l_ebln RIGHT DELETING TRAILING ' ' IN    "smart: 2010-08-02 #115
    CHARACTER MODE .                             "smart: 2010-08-02 #115
  p_ebln = l_ebln+10(10).

  CONCATENATE '000000000000000000' p_matnr INTO l_matnr
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
  SHIFT l_matnr RIGHT DELETING TRAILING ' ' IN   "smart: 2010-08-02 #115
    CHARACTER MODE .                             "smart: 2010-08-02 #115
  p_matnr = l_matnr+18(18).


  SELECT SINGLE * FROM ekpo WHERE ebeln = p_ebln AND matnr =  p_matnr.

  IF sy-subrc = 0.
    w_imseg_ebelp = ekpo-ebelp.
  ELSE.
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '021' with p_ebln  p_matnr .

    i_scode = '1'.
    WRITE:  'Fehler  !!!!!! ebelp nicht vorhanden'.
  ENDIF.


ENDFORM.

" search_pos
*&---------------------------------------------------------------------*
*&      Form  scan_input_window
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM scan_input_window USING l_dt TYPE CLIKE..   "smart: 2010-08-02 #105

  DATA: input(50) TYPE c.

  CLEAR sy-ucomm.

  IF l_dt = 'C'.
    CALL SCREEN 1001 STARTING AT 2 2 ENDING AT 54 6.
  ELSEIF l_dt = 'D'.
    CALL SCREEN 1002 STARTING AT 2 2 ENDING AT 54 6.
  ENDIF.

  input = g_scanval.

ENDFORM.                    " scan_input_window
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_command_1001.

  DATA: ok_code TYPE syucomm.         " return code from screen

  ok_code = sy-ucomm.

  CASE g_scanval.
    WHEN 'ABBR'.
      ok_code = 'ABBR'.
    WHEN 'EXIT'.
      ok_code = 'EXIT'.
    WHEN 'ENTR'.
      ok_code = 'ENTR'.
      CLEAR g_scanval.
  ENDCASE.


  CASE ok_code.
    WHEN 'ENTR'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'ABBR'.
      g_scanval = 'ABBR'.
      g_scode = '1'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      g_scanval = 'EXIT'.
      g_scode = '1'.
      CLEAR s_sel.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.


  ENDCASE.

ENDFORM.                    " USER_COMMAND_1001
*&---------------------------------------------------------------------*
*&      Form  we_buchen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM we_buchen.

  wait up to 2 seconds.
  PERFORM create_goods_movement.

ENDFORM.                    " we_buchen
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: ok_code TYPE syucomm.         " return code from screen

  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'ENTR'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'ABBR'.
      g_scode = '1'.
      CLEAR: sy-ucomm.
      s_sel = 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.



ENDMODULE.                 " USER_COMMAND_0200  INPUT
