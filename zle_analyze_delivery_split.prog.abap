*&---------------------------------------------------------------------*
*& Report  ZLE_ANALYZE_DELIVERY_SPLIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105
************************************************************************

Report  ZLE_ANALYZE_DELIVERY_SPLIT.

type-pools: vlikp.

data: gs_likp1 like likp,
      gs_likp2 like likp,
      gt_vbpa1 like vbpa occurs 0 with header line,
      gt_vbpa2 like vbpa occurs 0 with header line,
      gt_analyse type vlikp_t_analyse,
      gs_analyse type vlikp_s_analyse,
      gf_msgtxt(40) type c,
      gf_delivery(10) type c,
      gt_likp_fields like dfies occurs 0,
      gs_likp_fields like dfies,
      gf_parvw_converted(4) type c.

ranges: gt_checked_partner for vbpa-parvw.

field-symbols: <gf_value1>, <gf_value2>.

parameters: delivry1 like likp-vbeln,
            delivry2 like likp-vbeln.

start-of-selection.

  select single * from likp
                  into gs_likp1
                  where vbeln eq delivry1.
  if sy-subrc ne 0.
    write delivry1 to gf_delivery no-zero.
    concatenate 'Delivery' gf_delivery 'does not exist' into gf_msgtxt
                separated by space IN CHARACTER  "smart: 2010-08-02 #101
                  MODE .                         "smart: 2010-08-02 #101
    condense gf_msgtxt.
    message s001(vl) with gf_msgtxt.
    exit.
  endif.

  select single * from likp
                  into gs_likp2
                  where vbeln eq delivry2.
  if sy-subrc ne 0.
    write delivry2 to gf_delivery no-zero.
    concatenate 'Delivery' gf_delivery 'does not exist' into gf_msgtxt
                separated by space IN CHARACTER  "smart: 2010-08-02 #101
                  MODE .                         "smart: 2010-08-02 #101
    condense gf_msgtxt.
    message s001(vl) with gf_msgtxt.
    exit.
  endif.

  select * from vbpa
           into table gt_vbpa1
           where vbeln eq delivry1.

  select * from vbpa
           into table gt_vbpa2
           where vbeln eq delivry2.

  call function 'DDIF_FIELDINFO_GET'
       exporting
            tabname   = 'LIKP'
       tables
            dfies_tab = gt_likp_fields
       exceptions
            not_found = 1
            others    = 2.

  write: / 'Fieldname',
        12 'Description',
        55 'Delivery 1', 95 'Delivery 2'.
  uline.
  do.
    assign component sy-index of structure gs_likp1 to <gf_value1>.
    if sy-subrc ne 0.
      exit.
    endif.

    assign component sy-index of structure gs_likp2 to <gf_value2>.

    if <gf_value1> ne <gf_value2>.
      read table gt_likp_fields into gs_likp_fields index sy-index.
      check sy-subrc = 0.

      if ( gs_likp_fields-datatype eq 'CHAR' or
           gs_likp_fields-datatype eq 'DATS' or
           gs_likp_fields-datatype eq 'NUMC' or
           gs_likp_fields-datatype eq 'TIMS' or
           gs_likp_fields-datatype eq 'CUKY' or
           gs_likp_fields-datatype eq 'DEC' ) and
         not ( gs_likp_fields-fieldname eq 'VBELN' or
               gs_likp_fields-fieldname eq 'ERNAM' or
               gs_likp_fields-fieldname eq 'ERDAT' or
               gs_likp_fields-fieldname eq 'ERZET' or
               gs_likp_fields-fieldname eq 'AENAM' or
               gs_likp_fields-fieldname eq 'AEDAT' or
               gs_likp_fields-fieldname eq 'BLDAT' or
               gs_likp_fields-fieldname eq 'WERKS' or
               gs_likp_fields-fieldname eq 'LPRIO' or
               gs_likp_fields-fieldname eq 'KNUMV' or
               gs_likp_fields-fieldname eq 'KNUMP' or
               gs_likp_fields-fieldname eq 'GRULG' or
               gs_likp_fields-fieldname eq 'TCODE' or
               gs_likp_fields-fieldname eq 'TSEGTP' or
               gs_likp_fields-fieldname eq 'TSEGFL' or
               gs_likp_fields-fieldname eq 'HANDLE' ).
        format color col_negative inverse intensified.
      else.
        format color off inverse off.
      endif.

      write: / gs_likp_fields-fieldname,
            12 gs_likp_fields-fieldtext(40),
            55 <gf_value1>, 95 <gf_value2>.

    endif.
  enddo.

* Check partners
  loop at gt_vbpa1.
    gt_checked_partner-sign   = 'I'.
    gt_checked_partner-option = 'EQ'.
    gt_checked_partner-low    = gt_vbpa1-parvw.
    append gt_checked_partner.
    read table gt_vbpa2 with key parvw = gt_vbpa1-parvw.
    if sy-subrc ne 0.
      call function 'CONVERSION_EXIT_PARVW_OUTPUT'
           exporting
                input  = gt_vbpa1-parvw
           importing
                output = gf_parvw_converted.
      perform build_splitprot using    space
                                       'Deviating role'
                                       gf_parvw_converted
                                       space
                              changing gt_analyse.
      continue.
    endif.
*   Partner exists check whether it is the same
    if gt_vbpa1-lifnr ne gt_vbpa2-lifnr.
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Partner number'
                                       gt_vbpa1-lifnr
                                       gt_vbpa2-lifnr
                              changing gt_analyse.
      continue.
    endif.
    if gt_vbpa1-kunnr ne gt_vbpa2-kunnr.
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Partner number'
                                       gt_vbpa1-kunnr
                                       gt_vbpa2-kunnr
                              changing gt_analyse.
      continue.
    endif.
    if gt_vbpa1-parnr ne gt_vbpa2-parnr.
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Partner number'
                                       gt_vbpa1-parnr
                                       gt_vbpa2-parnr
                              changing gt_analyse.
      continue.
    endif.
    if gt_vbpa1-pernr ne gt_vbpa2-pernr.
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Partner number'
                                       gt_vbpa1-pernr
                                       gt_vbpa2-pernr
                              changing gt_analyse.
      continue.
    endif.
*   Check whether address data are matching
    if ( gt_vbpa1-adrda ca 'AD ' and
         gt_vbpa2-adrda ca 'BCEF' ) or
       ( gt_vbpa2-adrda ca 'AD ' and
         gt_vbpa1-adrda ca 'BCEF' ).
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Address sign'
                                       gt_vbpa1-adrda
                                       gt_vbpa2-adrda
                              changing gt_analyse.
      continue.
    endif.
    if gt_vbpa1-adrda ca 'BCEF' and
       gt_vbpa2-adrda ca 'BCEF' and
       gt_vbpa1-adrnr ne gt_vbpa2-adrnr.
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Address number'
                                       gt_vbpa1-adrnr
                                       gt_vbpa2-adrnr
                              changing gt_analyse.
      continue.
    endif.
*   Deviating address numbers are possible even master addresses are
*   used (stock transfer processing)
    if gt_vbpa1-adrda ca 'AD' and
       gt_vbpa2-adrda ca 'AD' and
       gt_vbpa1-adrnr ne gt_vbpa2-adrnr.
      perform build_splitprot using    gt_vbpa1-parvw
                                       'Role &1: Address number'
                                       gt_vbpa1-adrnr
                                       gt_vbpa2-adrnr
                              changing gt_analyse.
      continue.
    endif.
*   Ship-to-party: Unloading point
    if gt_vbpa1-parvw eq 'WE'.
      if not gt_vbpa1-ablad is initial and
         not gt_vbpa1-ablad is initial and
         gt_vbpa1-ablad ne gt_vbpa2-ablad.
        perform build_splitprot using    gt_vbpa1-parvw
                                         'Unloading point'
                                         gt_vbpa1-ablad
                                         gt_vbpa2-ablad
                                changing gt_analyse.
      endif.
    endif.
  endloop.

* Do we have partners in gt_vbpa2 that don't exist in gt_vbpa1?
  loop at gt_vbpa2 where not parvw in gt_checked_partner.
    exit.
  endloop.
  if sy-subrc eq 0.
    call function 'CONVERSION_EXIT_PARVW_OUTPUT'
         exporting
              input  = gt_vbpa2-parvw
         importing
              output = gf_parvw_converted.
    perform build_splitprot using    space
                                     'Deviating role'
                                     space
                                     gf_parvw_converted
                            changing gt_analyse.
  endif.

  uline.
  format color off inverse off.
  loop at gt_analyse into gs_analyse.
    if sy-tabix eq 1.
      write: / 'Compare delivery partners:'.
      uline.
      write: /1 'Object',
             40 'Delivery 1',
             80 'Delivery 2'.
      uline.
    endif.
    write: /1 gs_analyse-feld,
           40 gs_analyse-feldwert1,
           80 gs_analyse-feldwert2.
  endloop.
  if sy-subrc ne 0.
    write: /'Partners are identical'.
  endif.


*&---------------------------------------------------------------------*
*&      Form  build_splitprot
*&---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

form build_splitprot using    if_parvw  like vbpa-parvw
                              if_text TYPE CLIKE "smart: 2010-08-02 #105
                              if_value1 TYPE     "smart: 2010-08-02 #105
                                CLIKE            "smart: 2010-08-02 #105
                              if_value2 TYPE     "smart: 2010-08-02 #105
                                CLIKE            "smart: 2010-08-02 #105
                     changing ct_analyse type vlikp_t_analyse.

  data: ls_analyse type vlikp_s_analyse,
        lf_parvw_converted like vbpa-parvw.

  if if_parvw ne space.
    call function 'CONVERSION_EXIT_PARVW_OUTPUT'
         exporting
              input  = if_parvw
         importing
              output = lf_parvw_converted.
  endif.

  ls_analyse-feld = if_text.
  if lf_parvw_converted ne space.
    replace '&1' with lf_parvw_converted into ls_analyse-feld.
  endif.

  ls_analyse-feldwert1 = if_value1.
  ls_analyse-feldwert2 = if_value2.
  append ls_analyse to ct_analyse.

endform.                               " build_splitprot
