*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_1_F00                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  SELECT * FROM lqua INTO TABLE t_lqua
            WHERE lgnum IN s_lgnum
              AND matnr IN s_matnr
              AND werks IN s_werks
              AND lgort IN s_lgort
              AND bestq IN s_bestq
              AND charg IN s_charg
              AND lgtyp IN s_lgtyp
              AND lgpla IN s_lgpla.


  LOOP AT t_lqua INTO w_lqua.
    CLEAR w_outtab.
    MOVE-CORRESPONDING w_lqua TO w_outtab.
    APPEND w_outtab TO i_outtab.

  ENDLOOP.
  .

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  output_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM output_selection.


  PERFORM zzz_alv_parameter_setzen.
  PERFORM zzz_alv_sortierung_setzen CHANGING i_sorttab.
  PERFORM zzz_start_alv_fullscreen.



ENDFORM.                    " output_selection
