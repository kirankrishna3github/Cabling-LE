*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_1_SEL                                         *
*----------------------------------------------------------------------*


select-options:  s_lgnum for lqua-lgnum ,
             s_matnr for lqua-matnr,
             s_werks for lqua-werks
                     default '6000'  no intervals no-display,
             s_lgort for lqua-lgort,
             s_bestq for lqua-bestq,
             s_charg for lqua-charg,
             s_lgtyp for lqua-lgtyp,
             s_lgpla for lqua-lgpla,
             s_wenum for lqua-wenum.

          parameters:     p_print1 type rspopname matchcode object prin.
