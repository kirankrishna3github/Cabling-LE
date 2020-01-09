*---------------------------------------------------------------------*
* Report  ZM_ETIKDRUCK08
*
*---------------------------------------------------------------------*
* Description:
* Printing Label type C (e.g. C3 for Schindler with output type ZHD4)
*
*---------------------------------------------------------------------*
* Änderungen:
*
*---------------------------------------------------------------------*

REPORT ZM_ETIKDRUCK08.

  tables: nast.

***  data: gs_lips type lips.
  data: gs_kna1 type kna1.


***  data: gv_vbeln TYPE vbeln_vl.
  data: gv_lifnr TYPE lifnr.
  data: gv_vsart type versart.
  data: gv_vsart_ups type versart.
  data: gv_anz_print type i.

  types: begin of ty_xml,
           line(256),
        end of ty_xml.

  data: gt_xml type standard table of ty_xml.
  data: gs_xml TYPE ty_xml.

  data: gs_label type zpp_harness_label.

  types: t_out  TYPE c LENGTH 1024.

  data: gt_out type standard table of t_out,
        gs_out type t_out.

* File-Definitionen
  data: gv_path type string.
  data: gv_filename type string.
  data: gv_file type c length 50.

  data: gv_retcode type sysubrc.
  data: gv_repid type repid.


  constants: gc_ext_xml(3) type c value 'xml'.
  constants: gc_ext_tmp(3) type c value 'tmp'.
  constants: gc_posnr_h type posnr value '000000'.

  CONSTANTS: BEGIN OF GCS_ADDRESS_GROUP,
               MAN_ADDRESS   LIKE   ADRG-ADDR_GROUP VALUE 'SD01',
               ORG_UNIT      LIKE   ADRG-ADDR_GROUP VALUE 'CA01',
             END OF GCS_ADDRESS_GROUP.


* ---------------------------------------------------------------------*
*       FORM main
* ---------------------------------------------------------------------*
FORM entry USING return_code us_screen.

  CLEAR return_code.

  PERFORM processing USING us_screen.

  return_code = gv_retcode.

ENDFORM.                    "main

*---------------------------------------------------------------------*
*       FORM PROCESSING_FS1                                           *
*---------------------------------------------------------------------*
*       Ablaufsteuerung für den Lieferschein                          *
*---------------------------------------------------------------------*
FORM processing USING proc_screen.

* Daten besorgen
  perform get_data.

  check gv_retcode = 0.

* error handling ???

* Label
  case gs_label-etiar.
    when 'C3'.
      perform print_label_c3.
  endcase.

* error handling ???

ENDFORM.                     "PROCESSING

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*
FORM get_data.

  types: begin of t_kna1,
           name1 type ad_name1,
           city type ad_city1,
         end of t_kna1.

  data: ls_vbpa_we type vbpa.
  data: ls_vbpa type vbpa.
  data: ls_kna1 type kna1.

  types: begin of t_likp,
           vbeln type vbeln_vl,
           wadat_ist type wadat_ist,
         end of t_likp.

  data: ls_likp type t_likp.

  types: begin of t_lips,
           vbeln type vbeln_vl,
           posnr type posnr_vl,
           matnr type matnr,
           werks type werks_d,
           lfimg type lfimg,
           vgbel type vgbel,
           vgpos type vgpos,
         end of t_lips.

  data: ls_lips type t_lips.

  types: begin of t_vbak,
           vbeln type vbeln_va,
           vkorg type vkorg,
           vtweg type vtweg,
           kunnr type kunag,
           zz_jobnr type zswde0101,
           zz_ordnr type zswde0104,
         end of t_vbak.

  data: ls_vbak type t_vbak.

  data: ls_addr1_val like addr1_val.
  data: ls_addr1_sel like addr1_sel.

  gv_retcode = 0.

  clear gs_label.

*---------------------------------------------------------------------
* Delivery (header)
*---------------------------------------------------------------------
  clear ls_likp.

  select single vbeln wadat_ist from likp into ls_likp
    where vbeln = nast-objky(10).

  if sy-subrc ne 0.
    gv_retcode = sy-subrc.
    sy-msgid = 'VL'.
    sy-msgty = 'E'.
    sy-msgno = '302'.
    sy-msgv1 = nast-objky(10).
    perform protocol_update.
  endif.

  check gv_retcode eq 0.

  gs_label-datum = ls_likp-wadat_ist.

*---------------------------------------------------------------------
* Delivery (item)
*---------------------------------------------------------------------
  clear ls_lips.

  select single vbeln posnr matnr werks lfimg vgbel vgpos from lips into ls_lips
    where vbeln = nast-objky(10) and
          posnr = nast-objky+10(6).

  if sy-subrc ne 0.
    gv_retcode = sy-subrc.
    sy-msgid = 'VL'.
    sy-msgty = 'E'.
    sy-msgno = '302'.
    sy-msgv1 = nast-objky(10).
    perform protocol_update.
  endif.

  check gv_retcode eq 0.
  gv_anz_print = ls_lips-lfimg.

  gs_label-werks = ls_lips-werks.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = ls_lips-matnr
    IMPORTING
      output = gs_label-matnr.

*---------------------------------------------------------------------
* Label type
*---------------------------------------------------------------------
  select single etiar from mara into gs_label-etiar
    where matnr = ls_lips-matnr.
*---------------------------------------------------------------------
* Data of sales order (Header)
*---------------------------------------------------------------------
  clear ls_vbak.
  select single vbeln vkorg vtweg kunnr zz_jobnr zz_ordnr
    from vbak into ls_vbak
    where vbeln = ls_lips-vgbel.

   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = ls_vbak-vbeln
    IMPORTING
      output = gs_label-vbeln.

  gs_label-jobnr = ls_vbak-zz_jobnr.

*---------------------------------------------------------------------
* Data of sales order (item)
*---------------------------------------------------------------------
  select single kdmat from vbap into gs_label-cmat
    where vbeln = ls_lips-vgbel and
          posnr = ls_lips-vgpos.

*---------------------------------------------------------------------
* Partner-ID 'WE' (Ship-to party)
*---------------------------------------------------------------------
  clear ls_vbpa_we.

  select single * from vbpa into ls_vbpa_we
    where vbeln = ls_lips-vbeln and
          posnr = gc_posnr_h and
          parvw = 'WE'.

  if sy-subrc ne 0.
    gv_retcode = sy-subrc.
    sy-msgid = 'AD_SUBCON'.
    sy-msgty = 'E'.
    sy-msgno = '155'.
    sy-msgv1 = ls_lips-vbeln .
    perform protocol_update.
  endif.

  check gv_retcode eq 0.

*---------------------------------------------------------------------
* Address of ship-to party (see Include LV56KF5D)
*---------------------------------------------------------------------
  clear: ls_addr1_sel, ls_addr1_val.

  ls_addr1_sel-addrnumber = ls_vbpa_we-adrnr.

  CALL FUNCTION 'ADDR_GET'
       EXPORTING
            ADDRESS_SELECTION       = LS_ADDR1_SEL
            ADDRESS_GROUP           = GCS_ADDRESS_GROUP-ORG_UNIT
       IMPORTING
          ADDRESS_VALUE           = LS_ADDR1_VAL
       EXCEPTIONS PARAMETER_ERROR    = 1
                  ADDRESS_NOT_EXIST  = 2
                  VERSION_NOT_EXIST  = 3
                  INTERNAL_ERROR     = 4
                  OTHERS             = 5.

  if sy-subrc ne 0.
    gv_retcode = sy-subrc.
    syst-msgid = 'VN'.
    syst-msgno = '203'.
    syst-msgty = 'E'.
    syst-msgv1 = 'SADR'.
    syst-msgv2 = syst-subrc.
    perform protocol_update.
  endif.

  CALL FUNCTION 'ADDR_COMPOSE_STREET_LINE'
    EXPORTING
      STREET                   = LS_ADDR1_VAL-STREET
      HOUSE_NUMBER             = LS_ADDR1_VAL-HOUSE_NUM1
      HOUSE_NUMBER2            = LS_ADDR1_VAL-HOUSE_NUM2
      LENGTH                   = 40
      COUNTRY                  = LS_ADDR1_VAL-COUNTRY
      LANGUAGE                 = LS_ADDR1_VAL-LANGU
    IMPORTING
      STREET_LINE              = LS_ADDR1_VAL-STREET.

  gs_label-name = ls_addr1_val-name1.

  check gv_retcode eq 0.

*---------------------------------------------------------------------
* Customer-Material
*---------------------------------------------------------------------
  select single kdmat from knmt into gs_label-cmat
    where vkorg = ls_vbak-vkorg and
          vtweg = ls_vbak-vtweg and
          kunnr = ls_vbpa_we-kunnr and
          matnr = ls_lips-matnr.

  IF ls_vbak-kunnr = zif_harnessing_core=>gc_locarno_kunnr.
    gs_label-lifnr = zif_harnessing_core=>gc_dat_locarno_lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_label-lifnr
      IMPORTING
        output = gs_label-lifnr.
  ENDIF.

*---------------------------------------------------------------------
* Data of material short text
*---------------------------------------------------------------------
  select single maktx from makt into gs_label-descr
    where matnr = ls_lips-matnr and
          spras = ls_addr1_val-langu.

*---------------------------------------------------------------------
* Barcode
*---------------------------------------------------------------------
***  PERFORM get_barcode USING gs_label-matnr
***                            ls_lips-werks
***                   CHANGING gs_label-bcode.

***  TRY.
***    CALL METHOD zcl_harnessing_core=>zif_harnessing_core~get_label_barcode
***      EXPORTING
***        iv_type  = lv_type
***      CHANGING
***        is_label = gs_label.
***  ENDTRY.

ENDFORM.                     "GET_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_BARCODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BARCODE USING pv_matnr TYPE matnr
                       pv_werks TYPE werks_d
              CHANGING pv_bcode TYPE  zpp_harness_label-bcode.

  DATA: lv_num   TYPE num5.
  DATA: lv_seq   TYPE char5.
  DATA:lv_kzkfg TYPE mara-kzkfg.

  CLEAR pv_bcode.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZH_LAB_NR'
      subobject               = gs_label-werks
      quantity                = '1'
    IMPORTING
      number                  = lv_num
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
    UNPACK lv_num TO lv_seq.

***    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
***      EXPORTING
***        input        = pv_-matnr
***      IMPORTING
***        output       = lv_matnr
***      EXCEPTIONS
***        length_error = 1
***        OTHERS       = 2.
***
***    IF sy-subrc <> 0.
***    ENDIF.

    SELECT SINGLE kzkfg FROM mara INTO lv_kzkfg
      WHERE matnr = pv_matnr.

    IF lv_kzkfg IS NOT INITIAL AND gs_label-jobnr NE 'N/A'.
      CONCATENATE gs_label-cmat gs_label-jobnr gs_label-lifnr gs_label-datum lv_seq INTO pv_bcode.
    ELSE.
      CONCATENATE gs_label-cmat gs_label-lifnr gs_label-datum lv_seq INTO pv_bcode.
    ENDIF.

    CONDENSE pv_bcode.

  ENDIF.

ENDFORM.                     "GET_BARCODE


*&---------------------------------------------------------------------*
*&      Form  PRINT_LABEL_C3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LABEL_C3 .

* Aus dem DR2-System: ZP_WARENBEW_XML_CTECT

  DATA: lv_brgew TYPE char15.
  DATA: lv_printer TYPE rspolname.
  DATA: ls_zmetik TYPE zmetik.
  DATA: lv_copies TYPE tdsfcopies.
  DATA: lv_file_codesoft TYPE c LENGTH 100.
  DATA: lv_field  TYPE char20.


  DATA:            BEGIN OF ze020,
*        Layout
                     layout_txt(20)   VALUE '<LAYOUT>:=',
                     layout(20),
*        Printer
                     printer_txt(20)  VALUE '<PRINTER>:=',
                     printer(50),
*        Number of copies
                     labanz_txt(20)   VALUE '<LABANZ>:=',
                     labanz(3),
*        Sold-to party
                     name_txt(20)     VALUE '<name>:=',
                     name             TYPE name_ag,
*        Material number
                     matnr_txt(20)    VALUE '<matnr>:=',
                     matnr            TYPE matnr,
*        Material Number Used by Customer
                     cmat_txt(20)     VALUE '<cmat>:=',
                     cmat             TYPE matnr_ku,
*        Barcode
                     bcode_txt(20)    VALUE '<bcode>:=',
                     bcode(40),
*        Barcode Number
                     bcode_no_txt(20) VALUE '<bcode_no>:=',
                     bcode_no(40),
*        Description
                     descr_txt(20)    VALUE '<descr>:=',
                     descr(50),
*        Comment
                     doc_txt(20)      VALUE '<doc>:=',
                     doc(50),
*        Job number
                     jobnr_txt(20)    VALUE '<jobnr>:=',
                     jobnr            TYPE zswde0101,
*        Order number
                     aufnr_txt(20)    VALUE '<aufnr>:=',
                     aufnr            TYPE aufnr,
*        Sales Order number
                     vbeln_txt(20)    VALUE '<vbeln>:=',
                     vbeln            TYPE vbeln,
*        Date
                     datum_txt(20)    VALUE '<datum>:=',
                     datum(10),
*        Account Number of Vendor or Creditor
                     lifnr_txt(20)    VALUE '<lifnr>:=',
                     lifnr            TYPE lifnr,
*        End-of-structure
                     eol(20)          VALUE 'zzz_end',
                   END OF ze020.


* Printer
  CLEAR lv_printer.

  CALL FUNCTION 'CONVERSION_EXIT_SPDEV_OUTPUT'
    EXPORTING
      INPUT         = nast-ldest
   IMPORTING
     OUTPUT        = lv_printer.

  CLEAR ls_zmetik.

  SELECT SINGLE * FROM zmetik INTO ls_zmetik
     WHERE rspolname = lv_printer.

  DO gv_anz_print TIMES.

*   Init
    CLEAR: ze020-layout, ze020-printer, ze020-labanz,
    ze020-name, ze020-matnr, ze020-cmat, ze020-bcode, ze020-bcode_no, ze020-descr,
    ze020-doc, ze020-jobnr, ze020-vbeln, ze020-datum, ze020-lifnr.

*   Layout
    ze020-layout = zcl_harnessing_core=>zif_harnessing_core~constant_values_get( iv_constant_id = 'HARN_PROD_LABEL'  iv_vakey1 = 'LAYOUTC3'  iv_vakey2 = '' iv_type = 'P').
    CLEAR gs_out.
    CONCATENATE ze020-layout_txt ze020-layout INTO gs_out.
    APPEND gs_out TO gt_out.

*   Printer
    ze020-printer = ls_zmetik-zcodesoftdrucker.

    CLEAR gs_out.
    CONCATENATE ze020-printer_txt ze020-printer INTO gs_out.
    APPEND gs_out TO gt_out.

*   Numbers of labels
    IF nast-anzal EQ 0.
      lv_copies = 1.
    ELSE.
      lv_copies = nast-anzal.
    ENDIF.

    SHIFT lv_copies LEFT DELETING LEADING '0'.

    ze020-labanz = lv_copies.
    CLEAR gs_out.
    CONCATENATE ze020-labanz_txt ze020-labanz INTO gs_out.
    APPEND gs_out TO gt_out.

*   Date
    CONCATENATE gs_label-datum+6(2) '/' gs_label-datum+4(2) '/' gs_label-datum(4) INTO ze020-datum.
    CLEAR gs_out.
    CONCATENATE ze020-datum_txt ze020-datum INTO gs_out.
    APPEND gs_out TO gt_out.

*   Sold-to party
    ze020-name = gs_label-name.
    CLEAR gs_out.
    CONCATENATE ze020-name_txt ze020-name INTO gs_out.
    APPEND gs_out TO gt_out.

*   Material number
    ze020-matnr = gs_label-matnr.
    CLEAR gs_out.
    CONCATENATE ze020-matnr_txt ze020-matnr INTO gs_out.
    APPEND gs_out TO gt_out.

*   Material number of customer
    ze020-cmat = gs_label-cmat.
    CLEAR gs_out.
    CONCATENATE ze020-cmat_txt ze020-cmat INTO gs_out.
    APPEND gs_out TO gt_out.

*   Barcode
    PERFORM get_barcode USING gs_label-matnr
                              gs_label-werks
                     CHANGING gs_label-bcode.

    ze020-bcode = gs_label-bcode.
    CLEAR gs_out.
    CONCATENATE ze020-bcode_txt ze020-bcode INTO gs_out.
    APPEND gs_out TO gt_out.

*   Barcode with two stars
    CONCATENATE '*' gs_label-bcode '*' INTO ze020-bcode_no.
    CLEAR gs_out.
    CONCATENATE ze020-bcode_no_txt ze020-bcode_no INTO gs_out.
    APPEND gs_out TO gt_out.

*   Material description
    ze020-descr = gs_label-descr.
    CLEAR gs_out.
    CONCATENATE ze020-descr_txt ze020-descr INTO gs_out.
    APPEND gs_out TO gt_out.

*   Job number
    ze020-jobnr = gs_label-jobnr.
    CLEAR gs_out.
    CONCATENATE ze020-jobnr_txt ze020-jobnr INTO gs_out.
    APPEND gs_out TO gt_out.

*   Sales order
    ze020-vbeln = gs_label-vbeln.
    CLEAR gs_out.
    CONCATENATE ze020-vbeln_txt ze020-vbeln INTO gs_out.
    APPEND gs_out TO gt_out.

*   Supplier
    ze020-lifnr = gs_label-lifnr.
    CLEAR gs_out.
    CONCATENATE ze020-lifnr_txt ze020-lifnr INTO gs_out.
    APPEND gs_out TO gt_out.

*   Eol
    CLEAR gs_out.
    gs_out = ze020-eol.
    APPEND gs_out TO gt_out.

  ENDDO.

* File generation
  lv_file_codesoft = 'T.ETIK.<MATNR>.<OBJKY>.<ERDAT>.<ERUHR>.<EXT>'.

  WRITE gs_label-matnr TO lv_field NO-ZERO.
  REPLACE '<MATNR>' WITH lv_field  INTO lv_file_codesoft.

  WRITE nast-objky TO lv_field NO-ZERO.
  REPLACE '<OBJKY>' WITH lv_field  INTO lv_file_codesoft.

  REPLACE '<ERDAT>' WITH sy-datum  INTO lv_file_codesoft.
  REPLACE '<ERUHR>' WITH sy-uzeit  INTO lv_file_codesoft.
  CONDENSE lv_file_codesoft NO-GAPS.


*
  PERFORM create_file USING ls_zmetik lv_file_codesoft.


ENDFORM.                    " PRINT_LABEL_C3


*&---------------------------------------------------------------------*
*&      Form  CREATE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_ZMETIK  text
*      -->IV_FILE_CODESOFT  text
*----------------------------------------------------------------------*
FORM create_file  USING    iv_zmetik TYPE zmetik
                           iv_file_codesoft TYPE char100.

  DATA: lv_path TYPE string.
  DATA: lv_filename TYPE string.
  DATA: lv_parva TYPE xuvalue.
  DATA: co_line_feed TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONSTANTS: lc_ext_tmp(3) TYPE c VALUE 'tmp'.
  CONSTANTS: lc_ext_txt(3) TYPE c VALUE 'txt'.

* Dateipfad ermitteln
  CLEAR lv_path.

  SELECT SINGLE dirname FROM user_dir INTO lv_path
  WHERE aliass EQ 'DIR_C-SENTI_P'.

  IF sy-subrc NE 0.
    gv_retcode = sy-subrc.
    sy-msgid = '56'.
    sy-msgty = 'E'.
    sy-msgno = '194'.
    sy-msgv1 = 'USER_DIR'.
    PERFORM protocol_update.
  ENDIF.

  CHECK gv_retcode EQ 0.

* Pfad mit sysid ergänzen
  CONCATENATE lv_path '/' sy-sysid '/' INTO lv_path
  IN CHARACTER MODE.

* Filename aufbereiten
  CONCATENATE lv_path iv_zmetik-zrfcdest iv_file_codesoft
  INTO lv_filename IN CHARACTER MODE.

* Testhalber Parameter ZM_CODESOFT_TEST setzen: Wert = 'X'
* File wird von codesoft nur mit Extension=txt gedruckt
* leider funktioniert get parameter id 'ZM_CODESOFT_TEST' field
* gv_repid nicht
* Parameter lesen, dieser kann mit Transaktion SU3 gesetzt werden
  SELECT SINGLE parva FROM usr05 INTO lv_parva
  WHERE bname = sy-uname AND
  parid = 'ZM_CODESOFT_TEST'.

  IF lv_parva EQ 'X'.
    REPLACE '<EXT>' WITH lc_ext_tmp INTO lv_filename.
  ELSE.
    REPLACE '<EXT>' WITH lc_ext_txt INTO lv_filename.
  ENDIF.

* Leerzeichen eliminieren
  CONDENSE lv_filename NO-GAPS.

* Outputfile öffnen
  OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
  IF sy-subrc NE 0.
    gv_retcode = sy-subrc.
    sy-msgid = 'TL'.
    sy-msgty = 'E'.
    sy-msgno = '410'.
    sy-msgv1 = gv_filename.
    PERFORM protocol_update.
  ELSE.
*   File erzeugen
    LOOP AT gt_out INTO gs_out.
      CONCATENATE gs_out co_line_feed INTO gs_out.
      TRANSFER gs_out TO lv_filename.
    ENDLOOP.
    CLOSE DATASET lv_filename.
    IF sy-subrc NE 0.
        gv_retcode = sy-subrc.
        sy-msgid = 'TL'.
        sy-msgty = 'E'.
        sy-msgno = '411'.
        sy-msgv1 = gv_filename.
    ENDIF.
  ENDIF.

ENDFORM.                    " CREATE_FILE

*&---------------------------------------------------------------------*
*&      Form  GET_FILEPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FILEPATH .

  clear gv_path.

* Pfad via Verzeichnisparameter 'DIR_C_SENTII_P' ermitteln
  perform get_path using 'DIR_C-SENTI_P'
                 changing gv_path.

* Pfad mit sysid ergänzen
  concatenate gv_path '/' sy-sysid '/' into gv_path
  in character mode.

ENDFORM.                    " GET_FILEPATH

*---------------------------------------------------------------------
* Pfad via Verzeichnisparameter ermitteln
*---------------------------------------------------------------------
include zincl_getpath.


*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETER_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PARAMETER_ID USING p_parid type memoryid
                   changing p_parva type xuvalue.

  clear p_parva.

* Parameter lesen, dieser kann mit Transaktion SU3 gesetzt werden
  select single parva from usr05 into p_parva
    where bname = sy-uname and
          parid = p_parid.

ENDFORM.                    " GET_PARAMETER_ID

*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       save protocol                                                 *
*---------------------------------------------------------------------*
FORM PROTOCOL_UPDATE.

*  CHECK FSCOUT = SPACE.

  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
       EXPORTING
            MSG_ARBGB = SYST-MSGID
            MSG_NR    = SYST-MSGNO
            MSG_TY    = SYST-MSGTY
            MSG_V1    = SYST-MSGV1
            MSG_V2    = SYST-MSGV2
            MSG_V3    = SYST-MSGV3
            MSG_V4    = SYST-MSGV4
       EXCEPTIONS
            OTHERS    = 1.

ENDFORM.                    " PROTOCOL_UPDATE
