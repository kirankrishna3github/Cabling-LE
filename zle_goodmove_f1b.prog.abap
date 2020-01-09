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

  PERFORM scan_input_window USING 'C' c_fert.

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

    w_imseg-erfme = g_caufv-gmein.
    w_imseg-meins = g_caufv-gmein.
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

  SELECT SINGLE * FROM afpo WHERE aufnr =  w_imseg-aufnr AND
                  posnr = '1'.

  SELECT SINGLE * FROM t320 WHERE werks = afpo-pwerk
                            AND lgort = afpo-lgort.

  SELECT SINGLE * FROM mlgn
         WHERE matnr = g_caufv-stlbez
         AND   lgnum = t320-lgnum.
  DO.
    CLEAR g_scanval.

    PERFORM scan_input_window USING 'C' C_MENG.
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
            PERFORM scan_input_window USING 'C' C_MENG.
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

data: lv_insmk type insmk.

* Bestandesqualifikation nachlesen
clear lv_insmk.                                  "CSJUN2010
  select single insmk from afpo into lv_insmk    "CSJUN2010
   where aufnr = w_imseg-aufnr                   "CSJUN2010
     and posnr = '0001'.                         "CSJUN2010

  if sy-subrc = 0.                               "CSJUN2010
    w_imseg-insmk = lv_insmk.                    "CSJUN2010
  endif.                                         "CSJUN2010

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

*  mch1 - vfdat Vorschlag für Verfalldatum
*  Key  Chargennummer / Compund Nummer

  SELECT SINGLE * FROM mch1 WHERE
                       charg =   w_imseg-charg AND
                       matnr =   g_caufv-stlbez.

  CLEAR: w_imseg-hsdat, w_imseg-vfdat.
  SELECT SINGLE * FROM mara WHERE matnr = g_caufv-stlbez.
  IF mara-xchpf = 'X' AND NOT mara-mhdrz IS INITIAL.
    IF NOT mara-mhdhb IS INITIAL.
      w_imseg-hsdat = sy-datum.
      w_imseg-vfdat = sy-datum + mara-mhdhb.
    ELSE.
      scantitel = text-103.     "Verfalldatum.

      w_imseg-hsdat = sy-datum.
      WRITE mch1-vfdat TO g_scanval.
      DO.
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
    ENDIF.
  ENDIF.

ENDFORM.                    " mhd
*&---------------------------------------------------------------------*
*&      Form  chargen_nr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chargen_nr.


  SELECT SINGLE * FROM mara WHERE matnr = g_caufv-plnbez.

* Components und Preload bzw. Cut sind Chargenpflichtig
  IF mara-mtart = 'ZCOM'
  or g_caufv-sfcpf = c_sfcpf_cut                            "CSJUN2010
  or g_caufv-sfcpf = c_sfcpf_preload.                       "CSJUN2010
*  Chargen Dynpro abfragen.
*  Verarbeitete Charge entspricht der Liefercharge

    CLEAR g_scanval.
    scantitel = text-104.     "Charge.

    PERFORM scan_input_window USING 'C' c_charg.

    IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'..
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      WRITE: / g_mestxt.
      EXIT.
    ELSE.
*      BREAK-POINT.
      w_imseg-charg = g_scanval.
    ENDIF.

  ENDIF.
ENDFORM.                    " chargen_nr
*&---------------------------------------------------------------------*
*&      Form  scan_lagbe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM scan_lagbe.

  DATA: l_lgb(1) TYPE c.
  DATA: l_memid(7) TYPE c.
  DATA: l_lgb1(1) TYPE c.

  l_memid = 'M_LGBER'.

* scannen Fertigungsauftrag.
  CLEAR g_scanval.

  scantitel = text-303.     "Lagerbereich

  DO.
    PERFORM scan_input_window USING 'C' c_lgber.

    IF g_scanval = 'EXIT' OR g_scanval = 'ABBR'.
      g_scode = '1'.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' INTO  g_mestxt.
      WRITE: / g_mestxt.
      MESSAGE ID 'ZDLE' TYPE 'S' NUMBER '012' .
      EXIT.
    ELSE.

      IF g_scanval = 'UBI'.
        l_lgb = '1'.
        FREE MEMORY ID l_memid.
        EXPORT l_lgb TO MEMORY ID l_memid.
        EXIT.
      ELSEIF g_scanval = 'SL'.
        l_lgb = '2'.
        FREE MEMORY ID l_memid.
        EXPORT l_lgb TO MEMORY ID l_memid.
        EXIT.
      ENDIF.

    ENDIF.
  ENDDO.
  COMMIT WORK.
  l_memid = 'M_LGBER'.
  CLEAR l_lgb.
  IMPORT l_lgb TO l_lgb FROM MEMORY ID l_memid.

ENDFORM.                    " scan_lagbe
