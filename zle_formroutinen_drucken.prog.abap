
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #115
************************************************************************

REPORT zle_formroutinen_drucken .

TABLES: ltap, marc, ekko, lfa1, makt, mara, mch1, mseg.


* Workfeld für Textzugriff
FORM read_tdnamep TABLES in_tab STRUCTURE itcsy
                       out_tab STRUCTURE itcsy.

  DATA: vbeln(10) TYPE n,
        posnr LIKE ltap-posnr.

  REFRESH: out_tab.
  CLEAR:   out_tab, vbeln, posnr.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'VBLKK-VBELN'.
        vbeln = in_tab-value.
      WHEN 'VBLKP-POSNR'.
        posnr = in_tab-value.
    ENDCASE.
  ENDLOOP.


  out_tab-name = 'TDNAMEP'.
  CONCATENATE vbeln posnr INTO out_tab-value IN  "smart: 2010-08-02 #101
    CHARACTER MODE .                             "smart: 2010-08-02 #101
  APPEND out_tab.
ENDFORM.


* Workfeld für Verpackungsmenge
FORM read_wmenge TABLES in_tab STRUCTURE itcsy
                       out_tab STRUCTURE itcsy.

  DATA: matnr(18),
        werks LIKE  vblkp-werks,
        wmenge LIKE vblkp-lgmng,
        lgmng LIKE vblkp-lgmng.


  REFRESH: out_tab.
  CLEAR:   out_tab, wmenge, matnr, werks, lgmng.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'VBLKP-MATNR'.
        matnr = in_tab-value.
      WHEN 'VBLKP-WERKS'.
        werks = in_tab-value.
      WHEN 'VBLKP-LGMNG'.
        lgmng = in_tab-value.
    ENDCASE.
  ENDLOOP.

  WHILE matnr+17(1) = ' '.
    SHIFT matnr RIGHT IN CHARACTER MODE .        "smart: 2010-08-02 #115
    matnr+0(1) = '0'.
  ENDWHILE.

  SELECT SINGLE * FROM marc WHERE matnr EQ matnr
                                  AND werks EQ werks.

  IF NOT marc-bstrf IS INITIAL.
    wmenge = lgmng / marc-bstrf.
  ELSE.
    wmenge = lgmng.
  ENDIF.


  out_tab-name = 'WMENGE'.
  WRITE wmenge TO out_tab-value.
  APPEND out_tab.
ENDFORM.



* Lieferantendaten lesen
FORM read_lifnr TABLES in_tab STRUCTURE itcsy
                       out_tab STRUCTURE itcsy.

  DATA: benum(10) TYPE n.

  REFRESH: out_tab.
  CLEAR:   out_tab, benum.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'LTAK-BENUM'.
        benum = in_tab-value.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE * FROM ekko WHERE ebeln = benum.
  out_tab-value = ekko-lifnr.
  out_tab-name = 'LIFNR'.
  APPEND out_tab.

  SELECT SINGLE * FROM lfa1 WHERE lifnr = ekko-lifnr.
  out_tab-value = lfa1-name1..
  out_tab-name = 'NAME1'.
  APPEND out_tab.
ENDFORM.



* Lieferantendaten aus MSEG lesen
FORM read_lifnr_aus_mseg TABLES in_tab STRUCTURE itcsy
                       out_tab STRUCTURE itcsy.

  DATA: lgnum LIKE ltap-lgnum,
        tanum LIKE ltap-tanum,
        tapos LIKE ltap-tapos.

  REFRESH: out_tab.
  CLEAR:   out_tab, tanum, tapos.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'LTAP-LGNUM'.
        lgnum = in_tab-value.
      WHEN 'LTAP-TANUM'.
        tanum = in_tab-value.
      WHEN 'LTAP-TAPOS'.
        tapos = in_tab-value.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE * FROM ltap
         WHERE lgnum EQ lgnum
         AND   tanum EQ tanum
         AND   tapos EQ tapos.

  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM mseg
           WHERE mblnr EQ ltap-wenum
           AND   zeile EQ ltap-wepos.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM lfa1
             WHERE lifnr EQ mseg-lifnr.
    ENDIF.
  ENDIF.

  out_tab-name = 'LIFNR'.
  out_tab-value = mseg-lifnr.
  APPEND out_tab.
  out_tab-name = 'NAME1'.
  out_tab-value = lfa1-name1.
  APPEND out_tab.

ENDFORM.



* Materialdaten lesen
FORM read_material TABLES in_tab STRUCTURE itcsy
                       out_tab STRUCTURE itcsy.

  DATA: matnr(18) TYPE n,
        werks(4)  TYPE n.

  REFRESH: out_tab.
  CLEAR:   out_tab, matnr.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'LTAP-MATNR'. matnr = in_tab-value.
      WHEN 'LTAP-WERKS'. werks = in_tab-value.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE * FROM makt WHERE matnr EQ matnr
                              AND spras EQ sy-langu.
  out_tab-value = makt-maktx.
  out_tab-name = 'MAKTX'.
  APPEND out_tab.

  SELECT SINGLE * FROM mara WHERE matnr EQ matnr.
  out_tab-value = mara-normt.
  out_tab-name = 'NORMT'.
  APPEND out_tab.

  SELECT SINGLE * FROM marc WHERE matnr EQ matnr
                              AND werks EQ werks.
  out_tab-value = marc-insmk.
  out_tab-name = 'INSMK'.
  APPEND out_tab.

ENDFORM.

* Chargendaten lesen
FORM read_charg TABLES in_tab STRUCTURE itcsy
                       out_tab STRUCTURE itcsy.

  DATA: matnr(18) TYPE n,
        charg(10) TYPE c.

  REFRESH: out_tab.
  CLEAR:   out_tab, matnr, charg.

  LOOP AT in_tab.
    CASE in_tab-name.
      WHEN 'LTAP-MATNR'.
        matnr = in_tab-value.
      WHEN 'LTAP-CHARG'.
        charg = in_tab-value.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE * FROM mch1 WHERE matnr EQ matnr
                              AND charg EQ charg.
  out_tab-value = mch1-licha.
  out_tab-name = 'LICHA'.
  APPEND out_tab.

ENDFORM.
