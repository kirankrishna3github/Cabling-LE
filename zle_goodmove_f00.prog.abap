*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_F00                                           *
*----------------------------------------------------------------------*
* CSJUN2010 Schneidevorgang mit automatischem Abbuchen der Komponente
*-----------------------------------------------------------------------
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
      COMMIT WORK and wait.                                 "CSJUNI2010
      WRITE: / text-060.

      PERFORM lt06_batch USING l_emkpf-mblnr l_emkpf-mjahr.

*     Schneideaufträge und precut behandeln
      IF s_sel = 'ZLEGM02'.                                 "CSJUNI2010
        READ TABLE t_imseg INTO w_imseg INDEX 1.            "CSJUNI2010
        IF NOT w_imseg-aufnr IS INITIAL.                    "CSJUNI2010
          PERFORM treat_cutting_orders                      "CSJUNI2010
            USING l_emkpf                                   "CSJUNI2010
                  w_imseg-aufnr.                            "CSJUNI2010
                                                            "CSJUNI2010
        ENDIF.                                              "CSJUNI2010
      ENDIF.                                                "CSJUNI2010
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
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH  l_text. .

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
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '000' WITH l_text. .

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

  dy_tran1 = 'ZLEGM01'.
  dy_tran2 = 'ZLEGM02'.
  dy_tran3 = 'ZLEGM03'.
  dy_tran4 = 'ZLEGM04'.


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
FORM search_pos USING    p_ebln
                         p_matnr
                         w_imseg_ebelp
                         i_scode.

  DATA: l_ebln(20)  TYPE c,
        l_matnr(36) TYPE c.


  CONCATENATE '0000000000' p_ebln INTO l_ebln.
  SHIFT l_ebln RIGHT DELETING TRAILING ' '.
  p_ebln = l_ebln+10(10).

  CONCATENATE '000000000000000000' p_matnr INTO l_matnr.
  SHIFT l_matnr RIGHT DELETING TRAILING ' '.
  p_matnr = l_matnr+18(18).


  SELECT SINGLE * FROM ekpo WHERE ebeln = p_ebln AND matnr =  p_matnr.

  IF sy-subrc = 0.
    w_imseg_ebelp = ekpo-ebelp.
  ELSE.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '021' WITH p_ebln  p_matnr .

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
FORM scan_input_window USING l_dt value(iv_type).

  DATA: input(50) TYPE c.

  CLEAR sy-ucomm.
  gv_type = iv_type.

  IF l_dt = 'C'.
    CALL SCREEN 1001 STARTING AT 2 2 ENDING AT 75 6.
  ELSEIF l_dt = 'D'.
    CALL SCREEN 1002 STARTING AT 2 2 ENDING AT 75 6.
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

  WAIT UP TO 2 SECONDS.
  PERFORM create_goods_movement.

ENDFORM.                    " we_buchen
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: ok_code TYPE syucomm.         " return code from screen
  data: lv_fname type fieldname.
  field-symbols <F>.

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
    when 'PICK'.
      get cursor field lv_fname.
      if lv_fname(7) = 'DY_TRAN'.
        assign (lv_fname) to <f>.
        s_sel = <f>.
        leave to screen 0.
      endif.
  ENDCASE.



ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  treat_cutting_orders
*&---------------------------------------------------------------------*
*       WA bei Schneideaufträgen verbuchen
*----------------------------------------------------------------------*
*       Vor der Presserei werden die Felle geschnitten.
*       Beim Wareneingang des Zwischenprodukts soll die Charge des Fells
*       mit demselben Gewicht abgebucht werden, wie dasjenige des
*       Zwischenprodukts. Die Chargennummer des Fells entspricht
*       derjenigen des Zwischenprodukts.
*       Die Verbuchung erfolgt mittels CALL Transaction, weil im Fehler-
*       Fall sichtbar abgespielt werden soll.
*----------------------------------------------------------------------*
FORM treat_cutting_orders USING    iv_emkpf STRUCTURE emkpf
                                   iv_aufnr.

  DATA: lv_subrc TYPE subrc.

  CALL FUNCTION 'Z_MM_WA_PPMEX'
       EXPORTING
            iv_aufnr = iv_aufnr
            iv_mjahr = iv_emkpf-mjahr
            iv_mblnr = iv_emkpf-mblnr
       IMPORTING
            ev_subrc = lv_subrc.
  .
  if lv_subrc <> 0.
    message s406.
  endif.

ENDFORM.                    " treat_cutting_orders
