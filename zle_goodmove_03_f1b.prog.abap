************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_F1B                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  init_m02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_m02.

  CLEAR w_imseg.
  CLEAR g_scode.
  REFRESH t_imseg.


  w_imseg-bwart = '101'.
  w_imseg-kzbew = 'F'.
  w_imseg-erfme = 'ST'.
  w_imseg-meins = 'ST'.


ENDFORM.                                                    " init_m02
*&---------------------------------------------------------------------*
*&      Form  scan_ferta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_ferta.


  DATA: l_benum(24)  TYPE c.


* scannen Fertigungsauftrag.

  CLEAR g_scanval.

  scantitel = text-300.     "Fertigungsauftrag.

  PERFORM scan_input_window USING 'C'.

  IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
    g_scode = '1'.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
    WRITE: / g_mestxt.
    MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
    EXIT.
  ELSE.

    CHECK NOT g_scanval IS INITIAL.


* AUFTRAGSNUMMER UND NACHLAGERPLATZ MIT NULLEN AUFFÜLLEN
    w_imseg-aufnr = g_scanval.

    CONCATENATE '000000000000' w_imseg-aufnr INTO l_benum
IN CHARACTER MODE .                              "smart: 2010-08-02 #101
    SHIFT l_benum RIGHT DELETING TRAILING ' ' IN "smart: 2010-08-02 #115
      CHARACTER MODE .                           "smart: 2010-08-02 #115
    w_imseg-aufnr = l_benum+12(12).

    CLEAR g_caufv.

    CALL FUNCTION 'CO_DB_HEADER_READ'
      EXPORTING
        aufnr           = w_imseg-aufnr
*   VSNMR           =
     IMPORTING
*   AFPOWA          =
       caufvwa         =   g_caufv
*   VSNMR_EXP       =
   EXCEPTIONS
     not_found       = 1
     OTHERS          = 2
              .
    IF sy-subrc <> 0.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '007' .
      g_scode = '1'.
    ENDIF.
*    w_imseg-charg = g_caufv
  ENDIF.




ENDFORM.                    " scan_ferta
*&---------------------------------------------------------------------*
*&      Form  scan_mge_1B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_mge_1b.

  DATA: i_count TYPE i.
  DATA: i_menge LIKE g_menge.

***  g_scanval =  p_menge.
  CLEAR g_scanval.

  scantitel = text-102.     "Totalmenge.

  SELECT SINGLE * FROM mlgn
         WHERE matnr = g_caufv-stlbez
         AND   lgnum = '600'.


  DO.
    CLEAR g_scanval.

    PERFORM scan_input_window USING 'C'.
    IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      WRITE: / g_mestxt.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
      EXIT.
    ELSE.
      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
           EXPORTING
                string             = g_scanval
           EXCEPTIONS
                exponent_too_big   = 1
                exponent_too_small = 2
                string_not_fltp    = 3
                too_many_decim     = 4
                OTHERS             = 5.
      IF sy-subrc = 0.
        g_menge = g_scanval.
        IF g_menge <> mlgn-lhmg1.
          i_count = 1.
          WHILE i_count > 0..
            MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '008' .

            CLEAR g_scanval.
            PERFORM scan_input_window USING 'C'.
            IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
              g_scode = '1'.
              MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
              WRITE: / g_mestxt.
              MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
              EXIT.
            ELSE.
              CALL FUNCTION 'CHAR_FLTP_CONVERSION'
                   EXPORTING
                        string             = g_scanval
                   EXCEPTIONS
                        exponent_too_big   = 1
                        exponent_too_small = 2
                        string_not_fltp    = 3
                        too_many_decim     = 4
                        OTHERS             = 5.
              IF sy-subrc = 0.
                i_menge = g_scanval.
                IF   i_menge = g_menge.
                  i_count = i_count - 1.
                ELSE.
                  g_menge = i_menge.
                ENDIF.
              ELSE.
                MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '006' .
              ENDIF.
            ENDIF.
          ENDWHILE.
          w_imseg-menge = g_menge.
          w_imseg-erfmg = g_menge.
          EXIT.
        ELSE.
          w_imseg-menge = g_menge.
          w_imseg-erfmg = g_menge.

          EXIT.
        ENDIF.
      ELSE.
        MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '006' .
      ENDIF.
    ENDIF.
  ENDDO.


ENDFORM.                    " scan_mge_1B
*&---------------------------------------------------------------------*
*&      Form  verbuchen_m02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verbuchen_m02.


  APPEND w_imseg TO t_imseg.

  PERFORM we_buchen.

ENDFORM.                    " verbuchen_m02

*&---------------------------------------------------------------------*
*&      Form  mhd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mhd.
  CLEAR: w_imseg-hsdat, w_imseg-vfdat.
  SELECT SINGLE * FROM mara WHERE matnr = g_caufv-plnbez.
  IF sy-subrc = 0 AND NOT mara-mhdhb IS INITIAL.
    w_imseg-hsdat = sy-datum.
    w_imseg-vfdat = sy-datum + mara-mhdhb.
  ENDIF.

ENDFORM.                    " mhd
