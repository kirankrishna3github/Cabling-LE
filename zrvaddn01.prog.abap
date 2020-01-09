*----------------------------------------------------------------------*
*              Print of a delivery note by SAPscript                   *
*----------------------------------------------------------------------*

************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

report rvaddn01 line-count 100.

tables: vbco3,                         "Communicationarea for view
        vbdkl,                         "Headerview
        vbdpl,                         "Itemview
        komser,                        "Communicationarea Serialnumbers
        conf_out,                      "Configuration data
        tvko,                          "Sales organization
        tvst,                          "Shipping points
        t001g,                         "Company codes dependend texts
        rdgprint,                      "Dangerous goods All of Data
        rdgtxtprt,                     "undepend. Texts
        komk,                          "Communicationarea for conditions
        komp,                          "Communicationarea for conditions
        komvd.                         "Communicationarea for conditions
include rvadtabl.

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

data: retcode   like sy-subrc.         "Returncode
data: xscreen(1) type c.               "Output on printer or screen

data: begin of tvbdpl occurs 0.        "Internal table for items
        include structure vbdpl.
data: end of tvbdpl.

data: begin of tkomv occurs 50.
        include structure komv.
data: end of tkomv.

data: begin of tkomvd occurs 50.
        include structure komvd.
data: end of tkomvd.

data: begin of tkomcon occurs 50.      "...  for configuration data
        include structure conf_out.
data: end of tkomcon.

data: begin of tkomser occurs 5.
        include structure riserls.
data: end   of tkomser.

data: begin of tkomser_print occurs 5.
        include structure komser.
data: end   of tkomser_print.

data: begin of tkombat occurs 50.      " configuration data for batches
        include structure conf_out.
data: end   of tkombat.

data:  address_selection like addr1_sel.                    "MOS

data: pr_kappl(01)   type c value 'V'. "Application for pricing
data: print_mwskz.

data: price(1) type c.                 "price switch

data: begin of rdgprint_tab occurs 0.
        include structure rdgprint.
data: end   of rdgprint_tab.

data: i_undep_txt like rdgtxtprt occurs 0 with header line,"undepend Tex
      l_spras_txt like rdgtxtprt occurs 0 with header line,"undepend Tex
      i_idname_text like rdgtxtprt occurs 0 with header line.

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------

form entry using return_code us_screen.

  clear retcode.
  clear price.
  xscreen = us_screen.
  perform processing using us_screen.
  if retcode ne 0.
    return_code = 1.
  else.
    return_code = 0.
  endif.

endform.

form entry_price using return_code us_screen.

  clear retcode.
  price = 'X'.
  xscreen = us_screen.
  perform processing using us_screen.
  if retcode ne 0.
    return_code = 1.
  else.
    return_code = 0.
  endif.

endform.

*---------------------------------------------------------------------*
*       FORM PROCESSING                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROC_SCREEN                                                   *
*---------------------------------------------------------------------*
form processing using proc_screen.

  perform get_data.
  check retcode = 0.
  perform form_open using proc_screen vbdkl-land1.
  check retcode = 0.
  perform check_repeat.
  perform header_data_print.
  check retcode = 0.
  perform header_text_print.
  check retcode = 0.
  perform item_print.
  check retcode = 0.
  perform end_print.
  check retcode = 0.
  perform form_close.
  check retcode = 0.

endform.

***********************************************************************
*       S U B R O U T I N E S                                         *
***********************************************************************

*---------------------------------------------------------------------*
*       FORM CHECK_REPEAT                                             *
*---------------------------------------------------------------------*
*       A text is printed, if it is a repeat print for the document.  *
*---------------------------------------------------------------------*

form check_repeat.

  select * into *nast from nast where kappl = nast-kappl
                                and   objky = nast-objky
                                and   kschl = nast-kschl
                                and   spras = nast-spras
                                and   parnr = nast-parnr
                                and   parvw = nast-parvw
                                and   nacha between '1' and '4'.
    check *nast-vstat = '1'.
    call function 'WRITE_FORM'
         exporting
              element = 'REPEAT'
              window  = 'REPEAT'
         exceptions
              element = 1
              window  = 2.
    if sy-subrc ne 0.
      perform protocol_update.
    endif.
    exit.
  endselect.

endform.

*---------------------------------------------------------------------*
*       FORM END_PRINT                                                *
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*

form end_print.

  if price = 'X'.
    perform get_header_prices.
    call function 'CONTROL_FORM'
         exporting
              command = 'PROTECT'.
    perform header_price_print.
    call function 'WRITE_FORM'
         exporting
              element = 'END_VALUES'
         exceptions
              others  = 1.
    call function 'CONTROL_FORM'
         exporting
              command = 'ENDPROTECT'.
  endif.

  call function 'WRITE_FORM'
       exporting
            element = 'SUPPLEMENT_TEXT'
       exceptions
            element = 1
            window  = 2.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.

* print standard texts for dangerous goods
  perform dg_print_undep_text.

endform.

*---------------------------------------------------------------------*
*       FORM FORM_CLOSE                                               *
*---------------------------------------------------------------------*
*       End of printing the form                                      *
*---------------------------------------------------------------------*

form form_close.

  call function 'CLOSE_FORM'           "...Ende Formulardruck
       exceptions others = 1.
  if sy-subrc ne 0.
    retcode = 1.
    perform protocol_update.
  endif.
  set country space.

endform.


*---------------------------------------------------------------------*
*       FORM FORM_OPEN                                                *
*---------------------------------------------------------------------*
*       Start of printing the form                                    *
*---------------------------------------------------------------------*
*  -->  US_SCREEN  Output on screen                                   *
*                  ' ' = printer                                      *
*                  'X' = screen                                       *
*  -->  US_COUNTRY County for telecommunication and SET COUNTRY       *
*---------------------------------------------------------------------*


*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form form_open using us_screen us_country TYPE VBDKL-LAND1.
                                                 "smart: 2010-08-02 #105


  include rvadopfo.

endform.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       General provision of data for the form                        *
*---------------------------------------------------------------------*

form get_data.

  call function 'RV_PRICE_PRINT_REFRESH'    "v_n_485145
       tables
            tkomv = tkomv.
  clear komk.
  clear komp.                               "^_n_485145


  vbco3-spras = nast-spras.
  vbco3-vbeln = nast-objky.
  vbco3-kunde = nast-parnr.
  vbco3-parvw = nast-parvw.

  call function 'RV_DELIVERY_PRINT_VIEW'
       exporting
            comwa = vbco3
       importing
            kopf  = vbdkl
       tables
            pos   = tvbdpl.

* fill address key --> necessary for emails
  addr_key-addrnumber = vbdkl-adrnr.
  addr_key-persnumber = vbdkl-adrnp.
  addr_key-addr_type  = vbdkl-address_type.

* Data selection for dangerous goods
  perform dg_data_select using vbdkl.
  perform sender.

endform.

*---------------------------------------------------------------------*
*       FORM GET_HEADER_PRICES                                        *
*---------------------------------------------------------------------*
*       In this routine the price data for the header is fetched from *
*       the database.                                                 *
*---------------------------------------------------------------------*

form get_header_prices.

  call function 'RV_PRICE_PRINT_HEAD'
       exporting
            comm_head_i = komk
            language    = nast-spras
       importing
            comm_head_e = komk
            comm_mwskz  = print_mwskz
       tables
            tkomv       = tkomv
            tkomvd      = tkomvd.

endform.

*---------------------------------------------------------------------*
*       FORM GET_ITEM_CHARACTERISTICS                                 *
*---------------------------------------------------------------------*
*       In this routine the configuration data item is fetched from   *
*       the database.                                                 *
*---------------------------------------------------------------------*

form get_item_characteristics.

  data da_t_cabn like cabn occurs 10 with header line.
  data: begin of da_key,
          mandt like cabn-mandt,
          atinn like cabn-atinn,
        end   of da_key.

  refresh tkomcon.
  check not vbdpl-cuobj is initial.

  call function 'VC_I_GET_CONFIGURATION'
       exporting
            instance      = vbdpl-cuobj
            language      = nast-spras
            print_sales   = 'X'                "n_503517
       tables
            configuration = tkomcon
       exceptions
            others        = 4.

  ranges : da_in_cabn for da_t_cabn-atinn.
  clear da_in_cabn. refresh da_in_cabn.
    loop at tkomcon.
      da_in_cabn-option = 'EQ'.
      da_in_cabn-sign   = 'I'.
      da_in_cabn-low    = tkomcon-atinn.
      append da_in_cabn.
    endloop.

  clear da_t_cabn. refresh da_t_cabn.
  call function 'CLSE_SELECT_CABN'
    tables
         in_cabn                      = da_in_cabn
         t_cabn                       = da_t_cabn
    exceptions
         no_entry_found               = 1
         others                       = 2.

* Preisfindungsmerkmale / Merkmale auf VCSD_UPDATE herausnehmen
  sort da_t_cabn.
  loop at tkomcon.
    da_key-mandt = sy-mandt.
    da_key-atinn = tkomcon-atinn.
    read table da_t_cabn with key da_key binary search.
    if sy-subrc <> 0 or
         ( ( da_t_cabn-attab = 'SDCOM' and
            da_t_cabn-atfel = 'VKOND'       ) or
          ( da_t_cabn-attab = 'VCSD_UPDATE' ) ) .
      delete tkomcon.
    endif.
  endloop.

endform.

*---------------------------------------------------------------------*
*       FORM GET_ITEM_CHARACTERISTICS_BATCH                           *
*---------------------------------------------------------------------*
*       In this routine the configuration data for batches is fetched *
*       from the database                                             *
*---------------------------------------------------------------------*

form get_item_characteristics_batch.

  refresh tkombat.
  check not vbdpl-charg is initial.

  call function 'VB_BATCH_VALUES_FOR_OUTPUT'
       exporting
            material       = vbdpl-matnr
            plant          = vbdpl-werks
            batch          = vbdpl-charg
            language       = nast-spras
       tables
            classification = tkombat
       exceptions
            others         = 4.

  if sy-subrc ne 0.
    perform protocol_update.
  endif.

endform.

*---------------------------------------------------------------------*
*       FORM GET_ITEM_PRICES                                          *
*---------------------------------------------------------------------*
*       In this routine the price data for the item is fetched from   *
*       the database.                                                 *
*---------------------------------------------------------------------*

form get_item_prices.

  clear: komp,
         tkomv.

  if komk-knumv ne vbdkl-knump.
    clear komk.
    komk-mandt = sy-mandt.
    komk-kalsm = vbdkl-kalsp.
    komk-kappl = pr_kappl.
    komk-waerk = vbdkl-waerk.
    komk-knumv = vbdkl-knump.
    komk-vbtyp = vbdkl-vbtyp.
  endif.
  komp-kposn = vbdpl-posnr.

  call function 'RV_PRICE_PRINT_ITEM'
       exporting
            comm_head_i = komk
            comm_item_i = komp
            language    = nast-spras
       importing
            comm_head_e = komk
            comm_item_e = komp
       tables
            tkomv       = tkomv
            tkomvd      = tkomvd.

endform.

*---------------------------------------------------------------------*
*       FORM GET_SERIAL_NO                                            *
*---------------------------------------------------------------------*
*       In this routine the serialnumbers are fetched from the        *
*       database.                                                     *
*---------------------------------------------------------------------*

form get_serial_no.

  refresh tkomser.
  refresh tkomser_print.
  check vbdpl-anzsn > 0.
* Read the Serialnumbers of a Position.
  call function 'SERIAL_LS_PRINT'
       exporting
            vbeln  = vbdkl-vbeln
            posnr  = vbdpl-posnr
       tables
            iserls = tkomser.

* Process the stringtable for Printing.
  call function 'PROCESS_SERIALS_FOR_PRINT'
       exporting
            i_boundary_left             = '(_'
            i_boundary_right            = '_)'
            i_sep_char_strings          = ',_'
            i_sep_char_interval         = '_-_'
            i_use_interval              = 'X'
            i_boundary_method           = 'C'
            i_line_length               = 50
            i_no_zero                   = 'X'
            i_alphabet                  = sy-abcde
            i_digits                    = '0123456789'
            i_special_chars             = '-'
            i_with_second_digit         = ' '
       tables
            serials                     = tkomser
            serials_print               = tkomser_print
       exceptions
            boundary_missing            = 01
            interval_separation_missing = 02
            length_to_small             = 03
            internal_error              = 04
            wrong_method                = 05
            wrong_serial                = 06
            two_equal_serials           = 07
            serial_with_wrong_char      = 08
            serial_separation_missing   = 09.

  if sy-subrc ne 0.
    perform protocol_update.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_PRINT
*&---------------------------------------------------------------------*
*       Printing of the header data like terms, weights                *
*----------------------------------------------------------------------*

form header_data_print.

  call function 'WRITE_FORM'
       exporting
            element = 'HEADER_DATA'
       exceptions
            element = 1
            window  = 2.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.

endform.                               " HEADER_DATA_PRINT

*---------------------------------------------------------------------*
*       FORM HEADER_PRICE_PRINT                                       *
*---------------------------------------------------------------------*
*       Printout of the header prices                                 *
*---------------------------------------------------------------------*

form header_price_print.

  loop at tkomvd.

    at first.
      if komk-supos ne 0.
        call function 'WRITE_FORM'
             exporting
                  element = 'ITEM_SUM'
             exceptions
                  element = 1
                  window  = 2.
      else.
        call function 'WRITE_FORM'
             exporting
                  element = 'UNDER_LINE'
             exceptions
                  element = 1
                  window  = 2.
        if sy-subrc ne 0.
          perform protocol_update.
        endif.
      endif.
    endat.

    komvd = tkomvd.
    if print_mwskz = space.
      clear komvd-mwskz.
    endif.

    if komvd-koaid = 'D'.
      call function 'WRITE_FORM'
           exporting
                element = 'TAX_LINE'
           exceptions
                element = 1
                window  = 2.
    else.
      call function 'WRITE_FORM'
           exporting
                element = 'SUM_LINE'
           exceptions
                element = 1
                window  = 2.
    endif.
  endloop.
  describe table tkomvd lines sy-tfill.
  if sy-tfill = 0.
    call function 'WRITE_FORM'
         exporting
              element = 'UNDER_LINE'
         exceptions
              element = 1
              window  = 2.
    if sy-subrc ne 0.
      perform protocol_update.
    endif.
  endif.

endform.

*---------------------------------------------------------------------*
*       FORM HEADER_TEXT_PRINT                                        *
*---------------------------------------------------------------------*
*       Printout of the headertexts                                   *
*---------------------------------------------------------------------*

form header_text_print.

  call function 'WRITE_FORM'
       exporting
            element = 'HEADER_TEXT'
       exceptions
            element = 1
            window  = 2.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.

endform.

*---------------------------------------------------------------------*
*       FORM ITEM_PRINT                                               *
*---------------------------------------------------------------------*
*       Printout of the items                                         *
*---------------------------------------------------------------------*

form item_print.

* <<< START_OF_INSERTION_HP_327026 >>>
  data: lf_init_characteristics_done type xfeld.
* <<< END_OF_INSERTION_HP_327026 >>>
  call function 'WRITE_FORM'           "First header
       exporting  element = 'ITEM_HEADER'
       exceptions others  = 1.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.
  call function  'WRITE_FORM'          "Activate header
       exporting  element = 'ITEM_HEADER'
                  type    = 'TOP'
       exceptions others  = 1.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.

  loop at tvbdpl.
    vbdpl = tvbdpl.
* <<< START_OF_INSERTION_HP_327026 >>>
    if ( lf_init_characteristics_done is initial ) and
       ( ( not vbdpl-charg is initial ) or
         ( not vbdpl-cuobj is initial ) ).
       call function 'CTMS_DDB_INIT'.
       lf_init_characteristics_done = 'X'.
    endif.
* <<< END_OF_INSERTION_HP_327026 >>>
    if vbdpl-uecha is initial.
* <<< START_OF_INSERTION_HP_377469 >>>
      perform item_print_oi.
* <<< END_OF_INSERTION_HP_377469 >>>
      call function 'CONTROL_FORM'
           exporting
                command = 'PROTECT'.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE'.
      call function 'CONTROL_FORM'
           exporting
                command = 'ENDPROTECT'.
* Seitenumbruch, wenn Positionstexte nicht auf eine Seite passen.
       call function 'CONTROL_FORM'
           exporting
                command = 'PROTECT'.
      perform item_text_print.
      perform dg_print_data_get.
      perform dg_data_print.
      if price = 'X'.
        perform get_item_prices.
        perform item_price_print.
      endif.
      perform get_serial_no.
      perform item_serial_no_print.
      perform get_item_characteristics.
      perform item_characteristics_print.
      perform get_item_characteristics_batch.
      perform item_characteristics_batch.
      if vbdpl-vbeln_vauf ne space and
         vbdpl-vbeln_vauf ne vbdkl-vbeln_vauf.
        call function 'WRITE_FORM'
             exporting
                  element = 'ITEM_REFERENCE'
             exceptions
                  element = 1
                  window  = 2.
        if sy-subrc ne 0.
          perform protocol_update.
        endif.
      endif.
      if vbdpl-qmnum ne space and
         vbdpl-qmnum ne vbdkl-qmnum.
        call function 'WRITE_FORM'
             exporting
                  element = 'ITEM_QNUMBER'
             exceptions
                  element = 1
                  window  = 2.
        if sy-subrc ne 0.
          perform protocol_update.
        endif.
      endif.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_PURCHASE_DATA'
           exceptions
                element = 1
                window  = 2.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
      call function 'CONTROL_FORM'
           exporting
                command = 'ENDPROTECT'.
    else.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_BATCH'.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
      if price = 'X'.                                       "v_n_577292
        perform get_item_prices.
        perform item_price_print.
      endif.                                                "^_n_577292
      perform get_serial_no.
      perform item_serial_no_print.
      perform get_item_characteristics_batch.
      perform item_characteristics_batch.
    endif.
  endloop.

  call function  'WRITE_FORM'          "Deactivate Header
       exporting  element  = 'ITEM_HEADER'
                  function = 'DELETE'
                  type     = 'TOP'
       exceptions others   = 1.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.

endform.

*---------------------------------------------------------------------*
*       FORM ITEM_CHARACERISTICS_BATCH                                *
*---------------------------------------------------------------------*
*       Printout of the item characteristics for batches              *
*---------------------------------------------------------------------*

form item_characteristics_batch.

  loop at tkombat.
    conf_out = tkombat.
    if sy-tabix = 1.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_CONFIGURATION_BATCH_HEADER'
           exceptions
                others  = 1.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    else.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_CONFIGURATION_BATCH'
           exceptions
                others  = 1.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    endif.
  endloop.

endform.

*---------------------------------------------------------------------*
*       FORM ITEM_CHARACERISTICS_PRINT                                *
*---------------------------------------------------------------------*
*       Printout of the item characteristics -> configuration         *
*---------------------------------------------------------------------*

form item_characteristics_print.

  loop at tkomcon.
    conf_out = tkomcon.
    if sy-tabix = 1.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_CONFIGURATION_HEADER'
           exceptions
                others  = 1.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    else.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_CONFIGURATION'
           exceptions
                others  = 1.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    endif.
  endloop.

endform.

*---------------------------------------------------------------------*
*       FORM ITEM_PRICE_PRINT                                         *
*---------------------------------------------------------------------*
*       Printout of the item prices                                   *
*---------------------------------------------------------------------*

form item_price_print.

  loop at tkomvd.
    komvd = tkomvd.
    if print_mwskz = space.
      clear komvd-mwskz.
    endif.
    if sy-tabix = 1.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_PRICE_QUANTITY'
           exceptions
                element = 1
                window  = 2.
    else.
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_PRICE_TEXT'
           exceptions
                element = 1
                window  = 2.
    endif.
  endloop.

endform.

*---------------------------------------------------------------------*
*       FORM ITEM_SERIAL_NO_PRINT                                     *
*---------------------------------------------------------------------*
*       Printout of the item serialnumbers                            *
*---------------------------------------------------------------------*

form item_serial_no_print.

  loop at tkomser_print.
    komser = tkomser_print.
    if sy-tabix = 1.
*     Output of the Headerline
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_SERIAL_NO_HEADER'
           exceptions
                element = 1
                window  = 2.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    else.
*     Output of the following printlines
      call function 'WRITE_FORM'
           exporting
                element = 'ITEM_LINE_SERIAL_NO'
           exceptions
                element = 1
                window  = 2.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    endif.
    at last.
      call function 'CONTROL_FORM'
           exporting
                command = 'NEW-LINE'.
      if sy-subrc ne 0.
        perform protocol_update.
      endif.
    endat.
  endloop.

endform.

*---------------------------------------------------------------------*
*       FORM PROTOCOL_UPDATE                                          *
*---------------------------------------------------------------------*
*       The messages are collected for the processing protocol.       *
*---------------------------------------------------------------------*

form protocol_update.

  check xscreen = space.
  call function 'NAST_PROTOCOL_UPDATE'
       exporting
            msg_arbgb = syst-msgid
            msg_nr    = syst-msgno
            msg_ty    = syst-msgty
            msg_v1    = syst-msgv1
            msg_v2    = syst-msgv2
            msg_v3    = syst-msgv3
            msg_v4    = syst-msgv4
       exceptions
            others    = 1.

endform.

*---------------------------------------------------------------------*
*       FORM SENDER                                                   *
*---------------------------------------------------------------------*
*       This routine determines the address of the sender             *
*---------------------------------------------------------------------*

form sender.

  select single * from tvko  where vkorg = vbdkl-vkorg.
  if sy-subrc ne 0.
    syst-msgid = 'VN'.
    syst-msgno = '203'.
    syst-msgty = 'W'.
    syst-msgv1 = 'TVKO'.
    syst-msgv2 = syst-subrc.
    perform protocol_update.
  endif.
  select single * from tvst  where vstel = vbdkl-vstel.
  if sy-subrc ne 0.
    syst-msgid = 'VN'.
    syst-msgno = '203'.
    syst-msgty = 'W'.
    syst-msgv1 = 'TVST'.
    syst-msgv2 = syst-subrc.
    perform protocol_update.
  endif.
  select single * from t001g where bukrs    = vbdkl-bukrs
                             and   programm = 'RVADDN01'
                             and   txtid    = space.
  if sy-subrc ne 0.
    syst-msgid = 'VN'.
    syst-msgno = '203'.
    syst-msgty = 'W'.
    syst-msgv1 = 'T001G'.
    syst-msgv2 = syst-subrc.
    perform protocol_update.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  ITEM_TEXT_PRINT
*&---------------------------------------------------------------------*
form item_text_print.

  call function 'WRITE_FORM'
       exporting
            element = 'ITEM_TEXT'
       exceptions
            element = 1
            window  = 2.
  if sy-subrc ne 0.
    perform protocol_update.
  endif.

endform.                    " ITEM_TEXT_PRINT

* <<< START_OF_INSERTION_HP_377469 >>>
include rvaddn01_dg.

include rvaddn01_oi.
* <<< END_OF_INSERTION_HP_377469 >>>
