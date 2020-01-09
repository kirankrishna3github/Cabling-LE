*&---------------------------------------------------------------------*
*& Report  ZMM_EC_PCA_BESTAND_OUT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_EC_PCA_BESTAND_OUT line-SIZE 130.
************************************************************************
*Bestandsmigrationsreport:
*Anwendung im Zusammenhang mit Projekt EC-PCA
*
*Kurbeschreibung:
*Bucht mit ZMM_EC_PCA_BESTAND_OUT aus. Die Bestände werden in Tabelle
*ZMM_TRANS_MSEG geschrieben. Die Einbuchung erfolgt mit ZMM_EC_PCA_BESTAND_IN
*
************************************************************************
*Tabellen                                                              *
************************************************************************
tables:
        t001k,
        t024,    "Einkäufergruppen
        t024d,   "Disponenten
        t006,    "Mengeneinheiten
        mard,
        ZMM_TRANS_UPLOAD. "relevante Bestände aus Excel-Upload
"ZMM_TRANS_MSEG Mat.beleg Tabelle

*"Start-BAPI_GOODSMVT_CREATE--------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(GOODSMVT_HEADER) LIKE  BAPI2017_GM_HEAD_01
*"                             STRUCTURE  BAPI2017_GM_HEAD_01
data: goodsmvt_header type bapi2017_gm_head_01.
*"             VALUE(GOODSMVT_CODE) LIKE  BAPI2017_GM_CODE
*"                             STRUCTURE  BAPI2017_GM_CODE
data: goodsmvt_code type bapi2017_gm_code.
*"             VALUE(TESTRUN) LIKE  BAPI2017_GM_GEN-TESTRUN
*"                             DEFAULT SPACE
data: testrun type bapi2017_gm_gen-testrun .
*"       EXPORTING
*"             VALUE(GOODSMVT_HEADRET) LIKE  BAPI2017_GM_HEAD_RET
*"                             STRUCTURE  BAPI2017_GM_HEAD_RET
data: goodsmvt_headret type bapi2017_gm_head_ret.
*"             VALUE(MATERIALDOCUMENT) TYPE
*"                             BAPI2017_GM_HEAD_RET-MAT_DOC
data: materialdocument type bapi2017_gm_head_ret-mat_doc.
*"             VALUE(MATDOCUMENTYEAR) TYPE
*"                             BAPI2017_GM_HEAD_RET-DOC_YEAR
data: matdocumentyear type bapi2017_gm_head_ret-doc_year.
*"       TABLES
*"              GOODSMVT_ITEM STRUCTURE  BAPI2017_GM_ITEM_CREATE
data: t_goodsmvt_item type standard table of bapi2017_gm_item_create.
data: wa_goodsmvt_item type bapi2017_gm_item_create.

data: begin of item,
        material   type bapi2017_gm_item_create-material,
        plant      type bapi2017_gm_item_create-plant,
        stge_loc   type bapi2017_gm_item_create-stge_loc,
        batch      type bapi2017_gm_item_create-batch,
        move_type  type bapi2017_gm_item_create-move_type,
        stck_type  type bapi2017_gm_item_create-stck_type,
        spec_stock type bapi2017_gm_item_create-spec_stock, "Sonderbestand
        vendor     type bapi2017_gm_item_create-vendor,
        customer   type bapi2017_gm_item_create-customer,
        entry_qnt(13)  type c,
        entry_uom(3)  type c,
        pstng_date type budat,
        difflab type menge_bi,
      end of item.

DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
data: x_entry_qnt type labst.
data: wmenge(13).
data: wmatnr type matnr.
data: wcharg type CHARG_D.
data: t_goodsmvt_item_vb type standard table of bapi2017_gm_item_create. "Verbuchte
data: t_goodsmvt_item_nvb type standard table of bapi2017_gm_item_create. "nicht Verbuchte
*"              GOODSMVT_SERIALNUMBER
*"                             STRUCTURE  BAPI2017_GM_SERIALNUMBER
*"                             OPTIONAL
*"              RETURN STRUCTURE  BAPIRET2
data: t_return  type standard table of bapiret2.
data: wa_return  type  bapiret2.
data: t_returnprot  type standard table of bapiret2.
data: wa_returnprot  type  bapiret2.
data: t_ZMM_TRANS_UPLOAD type standard table of ZMM_TRANS_UPLOAD.
data: gt_zmm_trans_mseg2 type table of zmm_trans_mseg.
data: gs_zmm_trans_mseg2 type zmm_trans_mseg.
data: gs_lqua type lqua.
data: gs_trans_upload type zmm_trans_upload.

data: gs_zmm_trans_upload type zmm_trans_upload.
data: gt_zmm_trans_upload2 type zmm_trans_upload.
data: gs_zmm_trans_upload2 type zmm_trans_upload.
data: gs_trans_mseg type zmm_trans_mseg.
data: gt_trans_mseg type table of zmm_trans_mseg.
data: gt_trans_upload type table of zmm_trans_upload.




*"Ende-BAPI_GOODSMVT_CREATE--------------------------------------------

data: begin of iw occurs 0,
      matnr(18) type c,
      werks(4) type c,
      vprsv(1)  type c,
      verpr(17) type c,
      peinh(5) type c,
      maktx(40) type c,
      meins(3) type c.

data:  ok(1),                      "ok zur Verbuchung = Y
       errorw(1).
data: end of iw.

data: gt_mara type table of mara,
      gs_mara type mara.

data: gt_marc type table of marc,
      gs_marc type marc,
      gt_mbew type table of mbew,
      gs_mbew type mbew,
      gt_mchb type table of mchb,
      gs_mchb type mchb,
      gt_msku type table of msku,
      gs_msku type msku,
      gt_mslb type table of mslb,
      gs_mslb type mslb,
      gt_mard type table of mard,
      gs_mard type mard.


*Werke und Lagerorte
data: begin of it001l occurs 0.
        include structure t001l.
data: end of it001l.

data: gv_trans     type i,
      gv_errlines  type i,
      gv_alllines  type i,
      gv_goodlines type i.
*Counter:


*Statt Benutzerparameter
data: p_subc(1).

data: gv_numpos type i.    "Materialbeleg positionen
************************************************************************
*Felder                                                                *
************************************************************************
data: count_1 type p.
data: xfilename like rlgrap-filename.
data: xmess(100).
data: w_matnr like mara-matnr.
data: wtabix type SYTABIX.

data: gs_mseg type zmm_trans_mseg.
************************************************************************
*Benutzeroberläche                                                     *
************************************************************************


selection-screen begin of block b1 with frame title text-001.
parameters: p_test as checkbox default 'X'. "
parameters: p_matbel radiobutton group 1.
selection-screen skip 2.
parameters: p_wmta radiobutton group 1.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
parameters: p_budatl type mkpf-budat default sy-datum obligatory.
parameters: p_bldatl type mkpf-bldat default sy-datum OBLIGATORY.
parameters: p_bwart type bwart default '562'.
parameters: p_anzmbp type i default 20.
parameters: p_anzbel type i.
selection-screen end of block b2.


selection-screen begin of block b3 with frame title text-003.
selection-screen skip 1.
select-options: smatnr for gs_mara-matnr.
parameters:     p_werks type werks_d obligatory default '3000'.
select-options: s_lgort for mard-lgort.

parameters: p_batch radiobutton group 3,
            p_norm  radiobutton group 3.

selection-screen end of block b3.

selection-screen begin of block b4 with frame title text-004.
selection-screen skip 1.
select-options: smatnrwm for gs_mara-matnr,
                schargwm for gs_mchb-charg.
selection-screen end of block b4.




************************************************************************
* Batchinput Routinen
*INCLUDE bdcrecx1.
************************************************************************
initialization.



************************************************************************
*Verarbeitung                                                          *
************************************************************************

start-of-selection.
  if p_matbel = 'X'.
    perform t_ZMM_TRANS_UPLOAD.
    perform initialize_mseg_table.
    if p_subc = 'X'.
      perform generate_subcont_movements.
    endif.
    if p_norm = 'X'.
      perform generate_normal_movements.
    endif.
    if p_batch = 'X'.
      perform generate_batch_movements.
    endif.
  endif.

  if p_wmta = 'X'.
    perform generate_wmta_movements.
  endif.



*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
* Protokoll ausgeben.

* ------------------------------
  skip 2.
  gv_alllines = gv_alllines - gv_goodlines.
  write:/ 'Total   Documents', 25 gv_trans,
        / 'Good Movements  ', 25 gv_goodlines,
        / 'Bad  Movements  ', 25 gv_alllines.


*---------------------------------------------------------------------*
*       FORM ABBRUCH                                                  *
*---------------------------------------------------------------------*
form abbruch using text.
  format color 6 intensified on.
  skip 1.
  write: / '**************  A B B R U C H  ***************************'.
  write: /(58) text.
  write: / '**********************************************************'.
  stop.
endform.                    "ABBRUCH

*
*
*&---------------------------------------------------------------------*
*&      Form  goodsmvt
*&---------------------------------------------------------------------*
*      Warenbewegungen erzeugen
*----------------------------------------------------------------------*

form goodsmvt using iv_test .

  data: lv_lines type i.


  clear materialdocument.
  clear gv_numpos.
  gv_trans = gv_trans + 1 .
  if p_anzbel > 0.
    if gv_trans > p_anzbel.
      stop.
    endif.
  endif.

  clear t_return.
  call function 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = goodsmvt_header
      goodsmvt_code    = goodsmvt_code
      testrun          = iv_test
    IMPORTING
      goodsmvt_headret = goodsmvt_headret
      materialdocument = materialdocument
    TABLES
      goodsmvt_item    = t_goodsmvt_item
      return           = t_return.

  describe table t_goodsmvt_item lines lv_lines.
  gv_alllines = gv_alllines + lv_lines.

  if not materialdocument is initial.

    gv_goodlines = lv_lines + gv_goodlines.
    commit work and wait.

    loop at t_goodsmvt_item into wa_goodsmvt_item.
      clear gs_mseg.
      gs_mseg-matnr = wa_goodsmvt_item-material.
      gs_mseg-werks = wa_goodsmvt_item-plant.
      gs_mseg-lgort  = wa_goodsmvt_item-stge_loc.
      gs_mseg-charg  = wa_goodsmvt_item-batch.
      gs_mseg-sobkz  = wa_goodsmvt_item-spec_stock.
      gs_mseg-insmk  = wa_goodsmvt_item-stck_type.
      gs_mseg-bwart  = wa_goodsmvt_item-move_type.
      gs_mseg-mblnrv = materialdocument.
      gs_mseg-lifnr  = wa_goodsmvt_item-vendor.
      gs_mseg-menge  = wa_goodsmvt_item-entry_qnt.
      gs_mseg-meins  = wa_goodsmvt_item-entry_uom.
      gs_mseg-sgtxt = wa_goodsmvt_item-item_text.
      insert zmm_trans_mseg from gs_mseg.
      if sy-subrc <> 0.
        write:/ gs_mseg-matnr, gs_mseg-werks,
                gs_mseg-lgort, gs_mseg-charg, gs_mseg-sobkz,
                gs_mseg-insmk,
                gs_mseg-lifnr,
                gs_mseg-menge,
                gs_mseg-meins,
                gs_mseg-sgtxt,
                'Duplicate Record'.
      endif.
    endloop.
    commit work and wait.
  else.
    commit work and wait.
    loop at t_return into wa_return.
      check wa_return-type = 'E' or wa_return-type = 'A'.
      if wa_return-row > 0.
        read table t_goodsmvt_item into wa_goodsmvt_item
         index wa_return-row.
        if sy-subrc = 0.
          write: /       wa_goodsmvt_item-material ,
                         wa_goodsmvt_item-plant  ,
                         wa_goodsmvt_item-stge_loc  ,
                         wa_goodsmvt_item-batch,
                         wa_goodsmvt_item-move_type,
                         wa_goodsmvt_item-stck_type,
                         wa_goodsmvt_item-spec_stock,
                         wa_goodsmvt_item-vendor,
                         wa_goodsmvt_item-entry_qnt,
                         wa_goodsmvt_item-entry_uom,
                         wa_goodsmvt_item-item_text.
          write:/        wa_return-message(100) under wa_goodsmvt_item-plant.
          gv_errlines = gv_errlines + 1.
        endif.
      endif.
    endloop.
    if p_test = 'X'.
      if t_return is initial.
        gv_goodlines = lv_lines + gv_goodlines.
        loop at t_goodsmvt_item into wa_goodsmvt_item.
          clear gs_mseg.
          gs_mseg-matnr = wa_goodsmvt_item-material.
          gs_mseg-werks = wa_goodsmvt_item-plant.
          gs_mseg-lgort  = wa_goodsmvt_item-stge_loc.
          gs_mseg-charg  = wa_goodsmvt_item-batch.
          gs_mseg-sobkz  = wa_goodsmvt_item-spec_stock.
          gs_mseg-insmk  = wa_goodsmvt_item-stck_type.
          gs_mseg-bwart  = wa_goodsmvt_item-move_type.
          gs_mseg-mblnrv = materialdocument.
          gs_mseg-lifnr  = wa_goodsmvt_item-vendor.
          gs_mseg-menge  = wa_goodsmvt_item-entry_qnt.
          gs_mseg-meins  = wa_goodsmvt_item-entry_uom.
          gs_mseg-sgtxt = wa_goodsmvt_item-item_text.
          insert zmm_trans_mseg from gs_mseg.
          if sy-subrc <> 0.
            write:/ gs_mseg-matnr, gs_mseg-werks,
                    gs_mseg-lgort, gs_mseg-charg, gs_mseg-sobkz,
                    gs_mseg-insmk,
                    gs_mseg-lifnr,
                    gs_mseg-menge,
                    gs_mseg-meins,
                    gs_mseg-sgtxt,
                    'Duplicate Record'.
          endif.
        endloop.
        commit work and wait.
      endif.
    endif.
  endif.

  clear t_goodsmvt_item[].


endform.                    " goodsmvt
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SUBCONT_MOVEMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form generate_subcont_movements .



endform.                    " GENERATE_SUBCONT_MOVEMENTS
*&---------------------------------------------------------------------*
*&      Form  GENERATE_MSKU_MOVEMENTS
*&---------------------------------------------------------------------*
*       Kunden Konsignation
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form generate_msku_movements .


  clear gt_mara[].

  select * from msku into table gt_msku
     where matnr in smatnr
       and werks = p_werks.



endform.                    " GENERATE_msku_MOVEMENTS
*&---------------------------------------------------------------------*
*&      Form  GENERATE_NORMAL_MOVEMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form generate_normal_movements .

  select * into table gt_mara from mara    "A-Segment
           for all entries in gt_mard
           where matnr = gt_mard-matnr.

  select * from zmm_trans_mseg into table gt_trans_mseg.

  select * from zmm_trans_upload into table gt_trans_upload
                       where matnr in smatnr
                       and charg = '          '.

  loop at gt_trans_upload into gs_trans_upload.
    wtabix = sy-tabix.
    read table gt_trans_mseg into gs_trans_mseg with key
                           matnr = gs_trans_upload-matnr
                           werks = 3000
                           lgort = 3910
                           charg = '          '
                           insmk = ' '
                           sobkz = ' '
                           lifnr = '          '
                           kunnr = '          '
                           bwart = 562
                           sgtxt = gs_trans_upload-lgpla.
    if sy-subrc = 0 and not gs_trans_mseg-mblnrv is initial.
      delete gt_trans_upload index wtabix.
    endif.
  endloop.

  if gt_trans_upload[] is initial.
    return.
  endif.

* Header füllen
  clear goodsmvt_header.
  goodsmvt_header-pstng_date = p_budatl.
  goodsmvt_header-doc_date = p_bldatl.
  goodsmvt_header-pr_uname = sy-uname.
  goodsmvt_header-header_txt = 'EC-PCA OUT'.


  goodsmvt_code = '05'.


  gv_numpos = 0.
  clear gv_numpos.
* Lagerbestand nicht Chargenmaterial ausbuchen.
  loop at gt_trans_upload into gs_trans_upload.
*    read table gt_mara into gs_mara
*    with key matnr = gs_mard-matnr.
*    check sy-subrc = 0.
    clear wa_goodsmvt_item.
*  t_goodsmvt_item
    wa_goodsmvt_item-material   = gs_trans_upload-matnr.
    wa_goodsmvt_item-plant      = gs_trans_upload-werks.
    wa_goodsmvt_item-stge_loc   = '3910'.
    wa_goodsmvt_item-move_type  = p_bwart.
    wa_goodsmvt_item-stck_type  = space.
    wa_goodsmvt_item-entry_uom  = gs_trans_upload-meins.
    wa_goodsmvt_item-ITEM_TEXT  = gs_trans_upload-LGPLA.

* LABST
    wa_goodsmvt_item-entry_qnt  = gs_trans_upload-menge.
    append wa_goodsmvt_item to t_goodsmvt_item.
    gv_numpos = gv_numpos + 1.

    if gv_numpos >= p_anzmbp.
      perform goodsmvt using p_test.
      clear gv_numpos.
    endif.
  endloop.

  if gv_numpos > 0.
    clear gv_numpos.
    perform goodsmvt using p_test.
  endif.
endform.                    " GENERATE_NORMAL_MOVEMENTS
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BATCH_MOVEMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form generate_batch_movements .


  select * from zmm_trans_mseg into table gt_trans_mseg.

  select * from zmm_trans_upload into table gt_trans_upload
                       where matnr in smatnr
                       and not charg = '          '.

  loop at gt_trans_upload into gs_trans_upload.
    wtabix = sy-tabix.
    read table gt_trans_mseg into gs_trans_mseg with key
                           matnr = gs_trans_upload-matnr
                           werks = '3000'
                           lgort = '3910'
                           charg = gs_trans_upload-charg
                           insmk = ' '
                           sobkz = ' '
                           lifnr = '          '
                           kunnr = '          '
                           bwart = '562'
                           sgtxt = gs_trans_upload-lgpla.
    if sy-subrc = 0 and not gs_trans_mseg-mblnrv is initial.
      delete gt_trans_upload index wtabix.
    endif.
  endloop.


  if gt_trans_upload is initial.
    return.
  endif.

* Header füllen
  clear goodsmvt_header.
  goodsmvt_header-pstng_date = p_budatl.
  goodsmvt_header-doc_date = p_bldatl.
  goodsmvt_header-pr_uname = sy-uname.
  goodsmvt_header-header_txt = 'EC-PCA OUT'.

  goodsmvt_code = '05'.

  gv_numpos = 0.

  clear gv_numpos.
  loop at gt_trans_upload  into gs_trans_upload.

    add 1 to gv_numpos.
    clear wa_goodsmvt_item.                                "CSNOV2013
    wa_goodsmvt_item-material  = gs_trans_upload-matnr.
    wa_goodsmvt_item-plant     = gs_trans_upload-werks.
    wa_goodsmvt_item-stge_loc  = '3910'.
    wa_goodsmvt_item-move_type  = p_bwart.
    wa_goodsmvt_item-stck_type  = space.
    wa_goodsmvt_item-batch      = gs_trans_upload-charg.
    wa_goodsmvt_item-entry_qnt  = gs_trans_upload-menge.
    wa_goodsmvt_item-entry_uom  = gs_trans_upload-meins.
    wa_goodsmvt_item-ITEM_TEXT  = gs_trans_upload-LGPLA.

    append wa_goodsmvt_item to t_goodsmvt_item.


    if gv_numpos => p_anzmbp.
      perform goodsmvt using p_test.
      gv_numpos = 0.
    endif.
  endloop.

  if not t_goodsmvt_item is initial.
    perform goodsmvt using p_test.
    gv_numpos = 0.
  endif.
endform.                    " GENERATE_BATCH_MOVEMENTS
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_MSEG_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initialize_mseg_table .
  clear gs_mseg-mblnrv.
  delete from zmm_trans_mseg where mblnrv = gs_mseg-mblnrv.
  commit work.
endform.                    " INITIALIZE_MSEG_TABLE
*&---------------------------------------------------------------------*
*&      Form  T_ZMM_TRANS_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T_ZMM_TRANS_UPLOAD .
  select * from ZMM_TRANS_UPLOAD into table t_ZMM_TRANS_UPLOAD.
ENDFORM.                    " T_ZMM_TRANS_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  GENERATE_WMTA_MOVEMENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERATE_WMTA_MOVEMENTS .


  perform open_group.


  select * from zmm_trans_mseg into table gt_zmm_trans_mseg2
                               where matnr in smatnrwm
                                 and charg in schargwm
                                 and werks = '3000'
                                 and lgort = '3910'
                                 and mblnrv ge 1
                                 and mblnrn = '          '.

  loop at gt_zmm_trans_mseg2 into gs_zmm_trans_mseg2.

    if not gs_zmm_trans_mseg2-charg is initial. "Chargengeführt
      select single * from zmm_trans_upload into gs_zmm_trans_upload2
                     where matnr = gs_zmm_trans_mseg2-matnr
                       and werks = '3000'
                       and charg = gs_zmm_trans_mseg2-charg.

      if sy-subrc = 0.
        PERFORM create_wmta_batch_movements_bi.
      endif.
    endif.



    if gs_zmm_trans_mseg2-charg is initial. "nicht Chargengeführt
      select * from zmm_trans_upload into gs_zmm_trans_upload2
                     where matnr = gs_zmm_trans_mseg2-matnr
                       and werks = '3000'.

        if sy-subrc = 0.
          PERFORM create_wmta_norm_movements_bi.
        endif.
      endselect.
    endif.
  endloop.

  perform close_group.

ENDFORM.                    " GENERATE_WMTA_MOVEMENTS

*
**---------------------------------------------------------------------*
**       FORM ABBRUCH                                                  *
**---------------------------------------------------------------------*
*FORM ABBRUCH USING TEXT.
*  FORMAT COLOR 6 INTENSIFIED ON.
*  SKIP 1.
*  WRITE: / '**************  A B B R U C H  ***************************'.
*  WRITE: /(58) TEXT.
*  WRITE: / '**********************************************************'.
*  STOP.
*enDFORM.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT = SY-MANDT
      GROUP  = 'ECPCA-TA-O'
      USER   = sy-uname.
ENDFORM.                    "OPEN_GROUP

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.                    "CLOSE_GROUP

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      TCODE     = TCODE
    TABLES
      DYNPROTAB = BDCDATA.

  REFRESH BDCDATA.
ENDFORM.                    "BDC_TRANSACTION

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form create_wmta_batch_movements_bi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_wmta_batch_movements_bi .

  clear: wmenge.
  write gs_zmm_trans_mseg2-menge to wmenge.

  select single * from lqua into gs_lqua where lgnum = 391
                                           and matnr = gs_zmm_trans_mseg2-matnr
                                           and werks = '3000'
                                           and lgtyp = '001'.

  if gs_lqua-gesme = gs_zmm_trans_mseg2-menge.

    perform bdc_dynpro      using 'SAPML03T' '0101'.
    perform bdc_field       using 'BDC_CURSOR' 'RL03T-DUNKL'.
*perform bdc_field       using 'BDC_OKCODE' '/00'.
    perform bdc_field       using 'LTAK-LGNUM' '391'.
    perform bdc_field       using 'LTAK-BWLVS' '999'.
    perform bdc_field       using 'LTAP-MATNR'  gs_zmm_trans_mseg2-matnr.
    perform bdc_field       using 'RL03T-ANFME' wmenge.
    perform bdc_field       using 'LTAP-WERKS'  gs_zmm_trans_mseg2-werks.
    perform bdc_field       using 'LTAP-LGORT'  gs_zmm_trans_mseg2-lgort.
    perform bdc_field       using 'LTAP-CHARG'  gs_zmm_trans_mseg2-charg.
    perform bdc_field       using 'RL03T-DUNKL' 'H'.
    perform bdc_dynpro      using 'SAPML03T' '0102'.
*perform bdc_field       using 'BDC_CURSOR' 'RL03T-SQUIT'.
    perform bdc_field       using 'BDC_OKCODE' '/00'.
    perform bdc_field       using 'RL03T-ANFME' wmenge.
    perform bdc_field       using 'LTAP-ALTME'  gs_zmm_trans_mseg2-meins.
    perform bdc_field       using 'RL03T-SQUIT' 'X'.
*  perform bdc_field       using 'LTAP-WDATU' sy-datum.
    perform bdc_field       using 'LTAP-VLTYP' '001'.
    perform bdc_field       using 'LTAP-VLPLA' gs_zmm_trans_upload2-lgpla.
    perform bdc_field       using 'LTAP-NLTYP' '998'.
    perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
    perform bdc_transaction using 'LT01'.

  endif.

ENDFORM.                    " create_wmta_batch_movements_bi
*&---------------------------------------------------------------------*
*&      Form  CREATE_WMTA_NORM_MOVEMENTS_BI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_WMTA_NORM_MOVEMENTS_BI .

  clear: wmenge.
  write gs_zmm_trans_upload2-menge to wmenge.

  select single * from lqua into gs_lqua where lgnum = 391
                                           and matnr = gs_zmm_trans_mseg2-matnr
                                           and werks = '3000'
                                           and lgtyp = '001'.

  if gs_lqua-gesme le gs_zmm_trans_mseg2-menge.

    perform bdc_dynpro      using 'SAPML03T' '0101'.
    perform bdc_field       using 'BDC_CURSOR' 'RL03T-DUNKL'.
*perform bdc_field       using 'BDC_OKCODE' '/00'.
    perform bdc_field       using 'LTAK-LGNUM' '391'.
    perform bdc_field       using 'LTAK-BWLVS' '999'.
    perform bdc_field       using 'LTAP-MATNR'  gs_zmm_trans_mseg2-matnr.
    perform bdc_field       using 'RL03T-ANFME' wmenge.
    perform bdc_field       using 'LTAP-WERKS'  gs_zmm_trans_mseg2-werks.
    perform bdc_field       using 'LTAP-LGORT'  gs_zmm_trans_mseg2-lgort.
*  perform bdc_field       using 'LTAP-CHARG'  gs_zmm_trans_mseg2-charg.
    perform bdc_field       using 'RL03T-DUNKL' 'H'.
    perform bdc_dynpro      using 'SAPML03T' '0102'.
*perform bdc_field       using 'BDC_CURSOR' 'RL03T-SQUIT'.
    perform bdc_field       using 'BDC_OKCODE' '/00'.
    perform bdc_field       using 'RL03T-ANFME' wmenge.
    perform bdc_field       using 'LTAP-ALTME'  gs_zmm_trans_mseg2-meins.
    perform bdc_field       using 'RL03T-SQUIT' 'X'.
*  perform bdc_field       using 'LTAP-WDATU' sy-datum.
    perform bdc_field       using 'LTAP-VLTYP' '001'.
    perform bdc_field       using 'LTAP-VLPLA' gs_zmm_trans_upload2-lgpla.
    perform bdc_field       using 'LTAP-NLTYP' '998'.
    perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
    perform bdc_transaction using 'LT01'.

  endif.

ENDFORM.                    " CREATE_WMTA_NORM_MOVEMENTS_BI
