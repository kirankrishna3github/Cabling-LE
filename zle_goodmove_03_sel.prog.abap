*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_SEL                                           *
*----------------------------------------------------------------------*


PARAMETERS: s_sel(10) TYPE c NO-DISPLAY. .


* input Best. Nr.
  PARAMETERS: p_ebeln TYPE ebeln   no-display.

* input Art. Nr.
   parameters: p_matnr TYPE matnr  no-display.

* input gesammtgewicht
  parameters: p_menge TYPE bstmg  no-display.

* input verfalldatum
  parameters: p_VFDAT like sy-datum  no-display.

* input Batch-nr (loop bis fertig)
  parameters: p_batch1(40) type c  no-display.
  parameters: p_batch2(40) type c  no-display.
  parameters: p_batch3(40) type c  no-display.
  parameters: p_batch4(40) type c  no-display.
.
