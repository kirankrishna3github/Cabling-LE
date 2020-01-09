*&---------------------------------------------------------------------*
*& Report  ZMM_EC_PCA_BESTAND_WM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_EC_PCA_BESTAND_WM.

************************************************************************
*Bestandsmigrationsreport:
*Anwendung im Zusammenhang mit Projekt EC-PCA
*
*Kurbeschreibung:
*Der Report bucht die WM Bestände auf den bestandsführenden Platzen im
*Lagertyp 001 auf Basis der Tabelle ZMM_TRANS_UPLOAD
*aus im Werk 3000, ein im Werk 3001.
*
*
*
Tables: ZMM_TRANS_UPLOAD,
        marc,
        mch1,
        lqua.

data: begin of itrans_mseg occurs 0.
        include structure zmm_trans_mseg.
data: end of itrans_mseg.

data: ilqua998 type table of lqua,
      gs_ilqua998 type lqua.
data: ilqua001 type table of lqua,
gs_ilqua001 type lqua.

data: wmenge(13).
data: counter(8) type n.


DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.

selection-screen begin of block b1 with frame title text-001.
parameters: pout radiobutton group g1,
            pin  radiobutton group g1.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
parameters: pwrkout type werks default '3000',
            plgoout type lgort_d default '3910'.
select-options: smatnr for marc-matnr,
                swerks for marc-werks,
                scharg for mch1-charg,
                slgpla for lqua-lgpla.
parameters: pwrkin type werks default '3001',
            plgoin type lgort_d default '3911'.
selection-screen end of block b2.


perform open_group.

if pout = 'X'.
  select * into table ilqua001 from lqua  where lgnum = '391'
                                     and werks = pwrkout
                                     and lgtyp = '001'.
elseif pin = 'X'.
  select * into table ilqua998 from lqua where lgnum = '391'
                                     and werks = pwrkin
                                     and lgtyp = '998'.
endif.

select * from zmm_trans_mseg into table itrans_mseg
                         where matnr in smatnr
                           and werks in swerks
                           and charg in scharg
                           and mblnrv gt 1
                           and mblnrn gt 1.

if pout = 'X'.
  loop at itrans_mseg.
    clear: gs_ilqua001.
*    read table ilqua001 into gs_ilqua001 with key lgnum = 391
*                                 matnr = itrans_mseg-matnr
*                                 werks = pwrkout
*                                 charg = itrans_mseg-charg
*                                 lgpla = itrans_mseg-sgtxt.
    loop at ilqua001 into gs_ilqua001 where lgnum = 391
                                       and matnr = itrans_mseg-matnr
                                       and werks = pwrkout
                                       and charg = itrans_mseg-charg
                                       and lgpla = itrans_mseg-sgtxt.
      if sy-subrc = 0.
        exit.
      endif.
    endloop.
    if itrans_mseg-menge le gs_ilqua001-gesme.
      perform create_wmta_out.
      counter = counter + 1.
    else.
      write:/ itrans_mseg-matnr, itrans_mseg-charg, itrans_mseg-sgtxt, ' von Platz Bestand zu klein.'.
    endif.
  endloop.
endif.

if pin = 'X'.
  loop at itrans_mseg.
    clear: gs_ilqua998.
*      read table ilqua998 into gs_ilqua998 with key lgnum = 391
*                                   matnr = itrans_mseg-matnr
*                                   werks = pwrkin
*                                   charg = itrans_mseg-charg
*                                   lgpla = itrans_mseg-sgtxt.
*      if sy-subrc = 0 and itrans_mseg-menge le gs_ilqua998-gesme.
    loop at ilqua998 into gs_ilqua998 where lgnum = 391
                                       and matnr = itrans_mseg-matnr
                                       and werks = pwrkin
                                       and charg = itrans_mseg-charg
                                       and lgpla = 'AUFNAHME'.
      if sy-subrc = 0. exit. endif.
    endloop.
    if itrans_mseg-menge le gs_ilqua998-gesme.
      perform create_wmta_in.
      counter = counter + 1.
    else.
      write:/ itrans_mseg-matnr, itrans_mseg-charg, itrans_mseg-sgtxt, ' von Platz Bestand zu klein.'.
    endif.
  endloop.
endif.

write:/ 'Anzahl Datensätze: ', counter.
perform close_group.
*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      CLIENT = SY-MANDT
      GROUP  = 'ECPCA-WMTA'
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
*&      Form create_wmta_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_wmta_out.

  clear: wmenge.
  write itrans_mseg-menge to wmenge.

  perform bdc_dynpro      using 'SAPML03T' '0101'.
  perform bdc_field       using 'BDC_CURSOR' 'RL03T-DUNKL'.
  perform bdc_field       using 'LTAK-LGNUM' '391'.
  perform bdc_field       using 'LTAK-BWLVS' '999'.
  perform bdc_field       using 'LTAP-MATNR'  itrans_mseg-matnr.
  perform bdc_field       using 'RL03T-ANFME' wmenge.
  perform bdc_field       using 'LTAP-WERKS'  pwrkout.
  perform bdc_field       using 'LTAP-LGORT'  plgoout.

  if not itrans_mseg-charg is initial.
    perform bdc_field       using 'LTAP-CHARG'  itrans_mseg-charg.
  endif.

  perform bdc_field       using 'RL03T-DUNKL' 'H'.
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RL03T-ANFME' wmenge.
  perform bdc_field       using 'LTAP-ALTME'  itrans_mseg-meins.
  perform bdc_field       using 'RL03T-SQUIT' 'X'.
  perform bdc_field       using 'LTAP-VLTYP' '001'.
  perform bdc_field       using 'LTAP-VLPLA' itrans_mseg-sgtxt.
  perform bdc_field       using 'LTAP-NLTYP' '998'.
  perform bdc_field       using 'LTAP-NLPLA' 'AUFNAHME'.
  perform bdc_transaction using 'LT01'.

ENDFORM.                    " create_wmta_batch_movements_bi
*&---------------------------------------------------------------------*
*&      Form  CREATE_WMTA_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_WMTA_IN .

  clear: wmenge.
  write itrans_mseg-menge to wmenge.

  perform bdc_dynpro      using 'SAPML03T' '0101'.
  perform bdc_field       using 'BDC_CURSOR' 'RL03T-DUNKL'.
  perform bdc_field       using 'LTAK-LGNUM' '391'.
  perform bdc_field       using 'LTAK-BWLVS' '999'.
  perform bdc_field       using 'LTAP-MATNR'  itrans_mseg-matnr.
  perform bdc_field       using 'RL03T-ANFME' wmenge.
  perform bdc_field       using 'LTAP-WERKS'  pwrkin.
  perform bdc_field       using 'LTAP-LGORT'  plgoin.

  if not itrans_mseg-charg is initial.
    perform bdc_field       using 'LTAP-CHARG'  itrans_mseg-charg.
  endif.

  perform bdc_field       using 'RL03T-DUNKL' 'H'.
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using 'RL03T-ANFME' wmenge.
  perform bdc_field       using 'LTAP-ALTME'  itrans_mseg-meins.
  perform bdc_field       using 'RL03T-SQUIT' 'X'.
  perform bdc_field       using 'LTAP-VLTYP' '998'.
  perform bdc_field       using 'LTAP-VLPLA' 'AUFNAHME'.
  perform bdc_field       using 'LTAP-NLTYP' '001'.
  perform bdc_field       using 'LTAP-NLPLA' itrans_mseg-sgtxt.
  perform bdc_transaction using 'LT01'.

ENDFORM.                    " CREATE_WMTA_IN
