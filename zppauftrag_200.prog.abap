
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105 #115
************************************************************************

REPORT psfcsplt MESSAGE-ID co.
*----------------------------------------------------------------------*
*                                                                      *
* PPS-Print: Operation-split                                           *
*                                                                      *
*----------------------------------------------------------------------*
*Kopie von Report PSFC_STD_LAYOUT
*SAP Stäfa, Peter Huber, 11.2006
*
*Aenderungen:
*14112006; SAP Stäfa, Peter Huber
*27.12.2006: SAP Stäfa, Marcel Binder
*            Zusatzdaten Material zu Vorgang einfügen und RESB nachlesen
*            TR1K915243
*29052007    Nachdruck CZ
*----------------------------------------------------------------------*

* DATA-Statements general
INCLUDE ppcoincl.
* DATA-Statements specific for production orders
INCLUDE codrgt10.
* memory Schnittstelle Nachdruck
INCLUDE zincl_ppdruck_cz.                                   "CS29052007
INCLUDE zincl_ppdruck_mx.                                  "DGU/krb1003

* Start of "TR1K915243
* Work-Bereich für Änderungen
DATA: wafpo TYPE afpo.
DATA: flg_head.
DATA: flg_opr_info.
DATA: aplzl_sav LIKE afvgd-aplzl.
DATA: waufnrbc TYPE zmmd_czuml_header-aufnrvorg.
DATA: BEGIN OF index_tab OCCURS 0,
        index_cmp LIKE sy-tabix,
        index_seq LIKE sy-tabix,
        index_opr LIKE sy-tabix,
        plnfl     LIKE resbd-plnfl,
        vornr     LIKE resbd-vornr,
        lgort     LIKE resbd-lgort,
        aobar     LIKE resbd-aobar,
        aufst     LIKE resbd-aufst,
        aufwg     LIKE resbd-aufwg,
        baust     LIKE resbd-baust,
        posnr     LIKE resbd-posnr,
        matnr     LIKE resbd-matnr,
        rspos     LIKE resbd-rspos,
      END OF index_tab.
"DGU/krb1003
TABLES: klah,                                              "DGU/krb1003
        t320,
        afpo.
DATA: x_bdmng TYPE bdmng,                                  "DGU/krb1003
      g_objek LIKE ausp-objek.                             "DGU/krb1003
"DGU/krb1003
DATA  BEGIN OF g_t_sclass OCCURS 1.                        "DGU/krb1003
        INCLUDE STRUCTURE sclass.                          "DGU/krb1003
DATA  END   OF g_t_sclass.                                 "DGU/krb1003
"DGU/krb1003
DATA  BEGIN OF g_t_clobjdat OCCURS 1.                      "DGU/krb1003
        INCLUDE STRUCTURE clobjdat.                        "DGU/krb1003
DATA  END   OF g_t_clobjdat.                               "DGU/krb1003
"DGU/krb1003
DATA: x_print_co   TYPE print_co.                          "DGU/krb1003
DATA: x_mlgn       TYPE mlgn.                              "DGU/krb1003
DATA: x_resbd      TYPE resbd.                             "DGU/krb1003
DATA: x_afvgd      TYPE afvgd.                             "DGU/krb1003
DATA: x_crhd       TYPE crhd.                              "DGU/krb1003
DATA: x_mara       TYPE mara.                              "DGU/krb1003
DATA: z_mara       TYPE mara.                              "DGU/krb1003
DATA: x_prvbe      TYPE prvbe.                             "DGU/krb1003
DATA: x_pag_tit    TYPE i.                                 "DGU/krb1003
DATA: x_anz_repo(12) TYPE p decimals 3.                    "DGU/krb1003
DATA: x_pag_repo   TYPE i.                                 "DGU/krb1003
DATA: x_anz_ubic(12) TYPE p decimals 3.                    "DGU/krb1003
DATA: x_pag_ubic   TYPE i.                                 "DGU/krb1003
DATA: x_anz_print  TYPE i.                                 "DGU/krb1003
DATA: x_index_c(1) TYPE c.                                 "DGU/krb1003
DATA: x_mtart      type mara-matnr.
"DGU/krb1003
* End of "TR1K915243

* entry to print
PERFORM print_sub.

*---------------------------------------------------------------------*
*       FORM PRINT_SUB                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM print_sub.
  DATA use_default.

* Document-tables
  INCLUDE lcodrinc.

  CLEAR zpp_dru_anzdru.                                     "CS29052007
  IMPORT zpp_dru_anzdru FROM MEMORY ID zppdru_cz.           "CS29052007
  IF sy-subrc = 0.                                          "CS29052007
    IF zpp_dru_anzdru-auf_anz = 0.                          "CS29052007
      EXIT.                                                 "CS29052007
    ENDIF.                                                  "CS29052007
    zpp_dru_anzdru-found    = 'X'.                          "CS29052007
    print_co-desti            = zpp_dru_anzdru-desta.       "CS29052007
    print_co-copys            = zpp_dru_anzdru-auf_anz.     "CS29052007
  ENDIF.                                                    "CS29052007
  "DGU/krb1003
  CLEAR zpp_dru1_anzdru.                                   "DGU/krb1003
  IMPORT zpp_dru1_anzdru FROM MEMORY ID zppdru_mx.         "DGU/krb1003
  IF sy-subrc = 0.                                         "DGU/krb1003
    zpp_dru1_anzdru-found    = 'X'.                        "DGU/krb1003
    print_co-desti            = zpp_dru1_anzdru-desti.     "DGU/krb1003
  ENDIF.                                                   "DGU/krb1003

  LOOP AT itab_tdr WHERE object = obj-pos
                   OR    object = obj-sop
                   AND   aufnr  = print_co-aufnr.
    EXIT.
  ENDLOOP.

  CHECK sy-subrc IS INITIAL.

* fill workarea of header (probably more than 1 order is to be printed)

  READ TABLE itab_tdr WITH KEY object = obj-alt
                               aufnr  = print_co-aufnr.
  CHECK sy-subrc IS INITIAL.
* Save Indextable of header
  itab_ord = itab_tdr.

  READ TABLE caufvd_tab WITH KEY aufnr = itab_ord-aufnr.
  CHECK sy-subrc IS INITIAL.

  PERFORM pppr_std_init_order USING caufvd_tab.

  PERFORM pppr_collect_destinations USING  print_co.

  CLEAR: x_print_co, x_mlgn, z_mara.                       "DGU/krb1003
  CLEAR: x_prvbe, x_anz_repo, x_pag_repo.                  "DGU/krb1003
  CLEAR: x_anz_ubic, x_pag_ubic.                           "DGU/krb1003
  CLEAR: x_anz_print, x_index_c.                           "DGU/krb1003
  CLEAR: x_bdmng, x_resbd, x_mara, x_afvgd, x_crhd.        "DGU/krb1003
  IF caufvd-werks = '6000'.            " MEXIKO            "DGU/krb1003
*                                        =======           "DGU/krb1003
    x_print_co = print_co.        " Orig.Drucksteuerung    "DGU/krb1003
** Read Einzelmenge aus Klassifizierung                     "DGU/krb1003
*    g_objek = caufvd-plnbez.                               "DGU/krb1003
*                                                           "DGU/krb1003
*    klah-class = 'ZDGU_PP_ALLGEMEIN'.                      "DGU/krb1003
*    klah-klart = '001'.                                    "DGU/krb1003
*                                                           "DGU/krb1003
*    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'         "DGU/krb1003
*         EXPORTING                                         "DGU/krb1003
*              class              = klah-class              "DGU/krb1003
*              classtype          = klah-klart              "DGU/krb1003
*              object             = g_objek                 "DGU/krb1003
*              objecttable        = 'MARA'                  "DGU/krb1003
*         TABLES                                            "DGU/krb1003
*              t_class            = g_t_sclass              "DGU/krb1003
*              t_objectdata       = g_t_clobjdat            "DGU/krb1003
*         EXCEPTIONS                                        "DGU/krb1003
*              no_classification  = 1                       "DGU/krb1003
*              no_classtypes      = 2                       "DGU/krb1003
*              invalid_class_type = 3                       "DGU/krb1003
*              OTHERS             = 4.                      "DGU/krb1003
*                                                           "DGU/krb1003
*    IF sy-subrc = 0.                                       "DGU/krb1003
*      READ TABLE g_t_clobjdat                              "DGU/krb1003
*       WITH KEY atnam = 'ZDGU_MM_EINZELMENGE'.             "DGU/krb1003
*      IF sy-subrc = 0.                                     "DGU/krb1003
*        IF g_t_clobjdat-ausp1(1) <> '?'.                   "DGU/krb1003
*          x_bdmng = g_t_clobjdat-ausp1.                    "DGU/krb1003
*        ENDIF.                                             "DGU/krb1003
*      ENDIF.                                               "DGU/krb1003
*    ENDIF.                                                 "DGU/krb1003
* Maschinen-Nummer lesen aus Vorgangstabelle               "DGU/krb1003
    LOOP AT afvgd_tab  INTO x_afvgd.                       "DGU/krb1003
      SELECT SINGLE * FROM crhd  INTO x_crhd               "DGU/krb1003
             WHERE objty = 'A'                             "DGU/krb1003
              AND  objid = x_afvgd-arbid                   "DGU/krb1003
              AND  prvbe NE ' '.                           "DGU/krb1003
      x_prvbe = x_crhd-prvbe.                              "DGU/krb1003
      EXIT.                                                "DGU/krb1003
    ENDLOOP.                                               "DGU/krb1003

* Anzahl Seiten 'Formato Reposicion'                       "DGU/krb1003
"    x_mtart = 'ZCOM'.  "Mischungen.
    perform formato_rep.

* Anzahl Seiten 'Ubicar Producto Terminado'                "DGU/krb1003
    SELECT SINGLE * FROM mara  INTO z_mara                 "DGU/krb1003
           WHERE matnr = caufvd-matnr.                     "DGU/krb1003
    select single * from afpo where aufnr = caufvd-aufnr.
    if not afpo-lgort is initial.
      select single * from t320 where werks = afpo-pwerk
                                  and lgort = afpo-lgort.
      if sy-subrc = 0.
     SELECT SINGLE * FROM mlgn  INTO x_mlgn                 "DGU/krb1003
               WHERE matnr = caufvd-matnr
                      AND  lgnum = T320-lgnum.
     IF sy-subrc     =  0   AND                             "DGU/krb1003
        x_mlgn-lhmg1 NE 0.                                  "DGU/krb1003
       x_anz_ubic = ceil( caufvd-gamng                      "DGU/krb1003
                  / x_mlgn-lhmg1 ) + 1.                     "DGU/krb1003
         x_pag_ubic = ceil( x_anz_ubic  / 6 ).              "DGU/krb1003
     ENDIF.                                                 "DGU/krb1003
      endif.
    endif.

* Falls Aufruf aus Test-Report: Parameter übernehmen       "DGU/krb1003
    IF  zpp_dru1_anzdru-found    = 'X'.                    "DGU/krb1003
      x_pag_tit  = zpp_dru1_anzdru-tit_anz.                "DGU/krb1003
      x_pag_repo = zpp_dru1_anzdru-rep_anz.                "DGU/krb1003
      x_pag_ubic = zpp_dru1_anzdru-ubi_anz.                "DGU/krb1003
    ELSE.                                                  "DGU/krb1003
      x_pag_tit = 1.                                       "DGU/krb1003
    ENDIF.                                                 "DGU/krb1003

* Druck aller Seiten des Auftrags: 1/2, 3, 4               "DGU/krb1003
    DO 3 TIMES.                                            "DGU/krb1003
      x_index_c = sy-index.                                "DGU/krb1003
      CASE sy-index.                                       "DGU/krb1003
        WHEN 1.                                            "DGU/krb1003
          CHECK x_pag_tit  NE 0.                           "DGU/krb1003
          x_anz_print = x_pag_tit.                         "DGU/krb1003
        WHEN 2.                                            "DGU/krb1003
          CHECK x_pag_repo NE 0.                           "DGU/krb1003
          x_anz_print = x_pag_repo.                        "DGU/krb1003
        WHEN 3.                                            "DGU/krb1003
          CHECK x_pag_ubic NE 0.                           "DGU/krb1003
          x_anz_print = x_pag_ubic.                        "DGU/krb1003
      ENDCASE.                                            "DGU/krb1003
      CONCATENATE x_print_co-forml '_' x_index_c           "DGU/krb1003
             INTO print_co-forml IN CHARACTER    "smart: 2010-08-02 #101
               MODE .                            "smart: 2010-08-02 #101
               "DGU/krb1003                      "smart: 2010-08-02 #101
      DO x_anz_print TIMES.                                "DGU/krb1003

* get destination                                          "DGU/krb1003
        LOOP AT destination_tab.                           "DGU/krb1003
          pr_options-tddest = destination_tab-dest.        "DGU/krb1003
* call OPEN_FORM to open formular                          "DGU/krb1003
          PERFORM pppr_open_form USING 'FIRST'.            "DGU/krb1003
* Print 3 parts of mexican form                            "DGU/krb1003
          PERFORM print_mx.                                "DGU/krb1003
* call CLOSE_FORM to finish formular                       "DGU/krb1003
          PERFORM pppr_close_form.                         "DGU/krb1003
        ENDLOOP.                                           "DGU/krb1003
      ENDDO.                                               "DGU/krb1003
    ENDDO.                                                 "DGU/krb1003

* Weitere Stücklistenkomponten wie Einlegeeile ??
    x_mtart = 'ZCOM'. "alles ausser ZCOM
    perform formato_rep_others.

    "DGU/krb1003
  ELSE.                           " TSCHECHIEN usw.        "DGU/krb1003
*                                   ===============        "DGU/krb1003
    DO print_co-copys TIMES.
      IF sy-index GT 1.
        MOVE text-dup TO print_co-drtxt.
      ENDIF.
* get destination
     use_default = 'X'.                 " default destination in 1st run
      LOOP AT destination_tab.
        pr_options-tddest = destination_tab-dest.
* call OPEN_FORM to open formular
        PERFORM pppr_open_form USING 'MAIN'.
* Print split tickets
        PERFORM split_ticket USING print_co-use_wcp
                                   destination_tab-dest
                                   use_default.
* call CLOSE_FORM to finish formular
        PERFORM pppr_close_form.
       CLEAR use_default.          " default destination only in 1st run
      ENDLOOP.
    ENDDO.
  ENDIF.                                                   "DGU/krb1003
ENDFORM.                                                   "DGU/krb1003
*----------------------------------------------------------"DGU/krb1003
*       FORM PRINT_MX                                      "DGU/krb1003
*----------------------------------------------------------"DGU/krb1003
*       ........                                           "DGU/krb1003
*----------------------------------------------------------"DGU/krb1003
FORM print_mx.                                             "DGU/krb1003
**************                                             "DGU/krb1003
  CASE x_index_c.                                          "DGU/krb1003
    WHEN '1'.                                              "DGU/krb1003
      CALL FUNCTION 'WRITE_FORM'                           "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                element = 'DUMMY'                          "DGU/krb1003
           EXCEPTIONS                                      "DGU/krb1003
                OTHERS  = 0.                               "DGU/krb1003
      CALL FUNCTION 'CONTROL_FORM'                         "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                command = 'NEW-PAGE SECOND'.               "DGU/krb1003
      CALL FUNCTION 'WRITE_FORM'                           "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                element = 'DUMMY'                          "DGU/krb1003
           EXCEPTIONS                                      "DGU/krb1003
                OTHERS  = 0.                               "DGU/krb1003
    WHEN '2'.                                              "DGU/krb1003
      CALL FUNCTION 'WRITE_FORM'                           "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                element = 'DUMMY'                          "DGU/krb1003
           EXCEPTIONS                                      "DGU/krb1003
                OTHERS  = 0.                               "DGU/krb1003
    WHEN '3'.                                              "DGU/krb1003
      CALL FUNCTION 'WRITE_FORM'                           "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                element = 'DUMMY'                          "DGU/krb1003
           EXCEPTIONS                                      "DGU/krb1003
                OTHERS  = 0.                               "DGU/krb1003
  ENDCASE.                                                 "DGU/krb1003
  "DGU/krb1003
ENDFORM.                                                   "DGU/krb1003
"DGU/krb1003
*---------------------------------------------------------------------*
*       FORM SPLIT_TICKET                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM split_ticket USING use_wcp     LIKE t496p-use_wcp
                        destination LIKE t496p-drdest
                        use_default TYPE CLIKE.  "smart: 2010-08-02 #105
  DATA: counter_abs(4) TYPE n.
  DATA: number_of_tickets(4) TYPE n.
  DATA: BEGIN OF b OCCURS 0.
          INCLUDE STRUCTURE kbedp.
  DATA: END OF b.
  DATA: BEGIN OF a.
          INCLUDE STRUCTURE afvgd.
  DATA: END OF a.
  DATA flg_act_read.
  DATA: w_first(1) TYPE c.                                  "TR1K915243

  CLEAR counter_abs.
  CLEAR: wafpo, w_first.                                    "TR1K915243
  LOOP AT itab_tdr WHERE object = obj-pos
                   OR    object = obj-sop
                   AND   aufnr  = itab_ord-aufnr.
* Save ITAB of operation
    itab_vrg = itab_tdr.
* Get sequence (only if changed)
    IF itab_vrg-aplfl NE affld-plnfl OR itab_vrg-aufnr NE affld-aufnr.
      READ TABLE affld_tab INDEX itab_tdr-index_plfl.
      affld = affld_tab.
      PERFORM pppr_get_tables USING drpart-seq.

*     Charge einlesen                                       "TR1K915243
      PERFORM get_charge_kopf USING caufvd-aufnr            "TR1K915243
                              CHANGING wafpo.               "TR1K915243
*      SHIFT wafpo-charg LEFT DELETING LEADING '0'.          "TR1K915243
      w_first = 'X'.                                        "TR1K915243
    ELSE.                                                   "TR1K915243
      CLEAR: w_first.                                       "TR1K915243
    ENDIF.
* initialize operation
    PERFORM pppr_std_init_operation
            USING x_field space const-flg_yes space.
    CHECK NOT x_field IS INITIAL.
* output only for work center printer
    CHECK use_wcp IS INITIAL                  "no use of wcp
    OR    destination = afvgd-pdest           "wcp requested
    OR    ( afvgd-pdest IS INITIAL            "if wcp not given ...
            AND NOT use_default IS INITIAL ). "... use default printer

*   Barcode für Auftrag/Vorgang erstellen
    IF NOT print_co-barco IS INITIAL.                       "TR1K915243
      CONCATENATE caufvd-aufnr afvgd-vornr                  "TR1K915243
                  INTO waufnrbc IN CHARACTER MODE"smart: 2010-08-02 #101
                     .                           "smart: 2010-08-02 #101
                                                 "smart: 2010-08-02 #101
                    "TR1K915243                  "smart: 2010-08-02 #101
      SHIFT waufnrbc LEFT DELETING LEADING '0' IN"smart: 2010-08-02 #115
         CHARACTER MODE .             "TR1K915243"smart: 2010-08-02 #115
    ENDIF.                                                  "TR1K915243

    CLEAR flg_act_read.
    READ TABLE kbedp_tab
         WITH KEY bedid = afvgd-bedid
                  bedzl = afvgd-bedzl
         BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      LOOP AT kbedp_tab FROM sy-tabix.
        IF kbedp_tab-bedid <> afvgd-bedid
        OR kbedp_tab-bedzl <> afvgd-bedzl.
          EXIT.
        ENDIF.
        kbedp = kbedp_tab.
        MOVE-CORRESPONDING kbedp TO a.
* calculation of the activities using the operation-formalism
        PERFORM get_activity(saplcodr)
                USING a
                      rcr01
                      ttl_activ
                      afvgd-mgvrg
                      caufvd.
        ADD 1 TO counter_abs.
* check wether number of tickets on page greater than possible
        IF NOT print_co-azabs IS INITIAL.
          IF counter_abs > print_co-azabs.
            CALL FUNCTION 'CONTROL_FORM'
                 EXPORTING
                      command = 'NEW-PAGE'.
            CLEAR counter_abs.
            ADD 1 TO counter_abs.
          ENDIF.
        ENDIF.

* preserve form of skip
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  command = 'PROTECT'.
* print header info
        CALL FUNCTION 'WRITE_FORM'                          "TR1K915243
             EXPORTING                                      "TR1K915243
                  element = 'HEADER'                        "TR1K915243
                  window  = 'HEADER'.                       "TR1K915243
* print infos of header
        IF w_first = 'X'.                                   "TR1K915243
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'HDR_STD'
                    window  = 'MAIN'.
        ENDIF.                                              "TR1K915243
* print infos of operation
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_SPLT_HDR'
                  window  = 'MAIN'.
* print operation info (optional with barcode)
        IF print_co-barco IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'OPR_SPLT'
                    window  = 'MAIN'.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'OPR_SPLT_BC'
                    window  = 'MAIN'.
        ENDIF.
* print operation text
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_TEXT_SHORT'
                  window  = 'MAIN'.
* print activities
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_ACT_TYP'
                  window  = 'MAIN'.
* print infos depending on the type of split
        IF kbedp-ename IS INITIAL.
* print infos of machine-split
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'OPR_TYP_MACH'
                    window  = 'MAIN'.
        ELSE.
* print infos of pers-split
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'OPR_TYP_PERS'
                    window  = 'MAIN'.
        ENDIF.
* print mask for manual entries
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_MASK'
                  window  = 'MAIN'.
* end preservation of slip
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  command = 'ENDPROTECT'.
      ENDLOOP.
* no kbeds found
    ELSE.
* calculation of the activities using the operation-formalism
      PERFORM get_activity(saplcodr)
              USING afvgd
                    rcr01
                    ttl_activ
                    afvgd-mgvrg
                    caufvd.
      IF afvgd-spanz = 0.
        afvgd-spanz = 1.
      ENDIF.
      DO afvgd-spanz TIMES.
        kbedp-split = sy-index.
        ADD 1 TO counter_abs.
* check wether number of tickets on page greater than possible
        IF NOT print_co-azabs IS INITIAL.
          IF counter_abs > print_co-azabs.
            CALL FUNCTION 'CONTROL_FORM'
                 EXPORTING
                      command = 'NEW-PAGE'.
            CLEAR counter_abs.
            ADD 1 TO counter_abs.
          ENDIF.
        ENDIF.

* preserve form of skip
        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  command = 'PROTECT'.
* print header info
        CALL FUNCTION 'WRITE_FORM'                          "TR1K915243
             EXPORTING                                      "TR1K915243
                  element = 'HEADER'                        "TR1K915243
                  window  = 'HEADER'.                       "TR1K915243
* print infos of header
        IF w_first = 'X'.                                   "TR1K915243
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'HDR_STD'
                    window  = 'MAIN'.
        ENDIF.                                              "TR1K915243
* print infos of operation
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_DATA_HDR'
                  window  = 'MAIN'.
* print operation info (optional with barcode)
        IF print_co-barco IS INITIAL.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'OPR_DATA'
                    window  = 'MAIN'.
        ELSE.
          CALL FUNCTION 'WRITE_FORM'
               EXPORTING
                    element = 'OPR_DATA_BC'
                    window  = 'MAIN'.
        ENDIF.
* print operation text
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_TEXT_SHORT'
                  window  = 'MAIN'.
* print activities
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_ACT_TYP'
                  window  = 'MAIN'.
* print mask for manual entries
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'OPR_MASK'
                  window  = 'MAIN'.
        IF w_first = 'X'.
* Reservationen einlesen                                    "TR1K915243
          PERFORM get_reservation.                          "TR1K915243
        ENDIF.                                              "TR1K915243
* print reservation                                         "TR1K915243
        PERFORM print_reservation USING afvgd-vornr.        "TR1K915243
* end preservation of slip                                  "TR1K915243

        CALL FUNCTION 'CONTROL_FORM'
             EXPORTING
                  command = 'ENDPROTECT'.
      ENDDO.
    ENDIF.
  ENDLOOP.
ENDFORM.

* Start of "TR1K915243
*&---------------------------------------------------------------------*
*&      Form  get_charge_kopf
*&---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_charge_kopf USING    p_aufnr TYPE       "smart: 2010-08-02 #105
  CAUFVD-AUFNR                                   "smart: 2010-08-02 #105
                     CHANGING p_aufpo TYPE AFPO. "smart: 2010-08-02 #105


  SELECT SINGLE * INTO  p_aufpo
                  FROM  afpo
                  WHERE aufnr EQ p_aufnr
                  AND   posnr EQ '0001'.

ENDFORM.                    " get_charge_kopf

*&---------------------------------------------------------------------*
*&      Form  get_reservation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_reservation.

  CLEAR: flg_head, flg_opr_info, aplzl_sav.
  REFRESH: index_tab.

* build short table for sort of components
  LOOP AT itab_tdr WHERE object = obj-mat
                         AND aufnr EQ itab_ord-aufnr.
* Fill DDIC-Strutcure of component
    READ TABLE resbd_tab INDEX itab_tdr-index_plmz.
* found ?
    CHECK sy-subrc IS INITIAL
* only print if amount of component greater than 0
    AND   resbd_tab-bdmng GT 0
* not printed will be: Dummy's
    AND   resbd_tab-dumps IS INITIAL
* not printed will be by-products:
    AND   resbd_tab-shkzg NE 'S'.
    index_tab-index_cmp = itab_tdr-index_plmz.
    index_tab-index_seq = itab_tdr-index_plfl.
    index_tab-index_opr = itab_tdr-index_plpo.
    MOVE-CORRESPONDING resbd_tab TO index_tab.
    APPEND index_tab.
  ENDLOOP.

  DESCRIBE TABLE index_tab LINES sy-dbcnt.

  CHECK sy-dbcnt GT 0.

* sort index_tab by ...
  SORT index_tab BY plnfl vornr lgort  " order of ITAB
                    aobar aufst aufwg baust
                    posnr matnr rspos.

ENDFORM.                    " get_reservation

*&---------------------------------------------------------------------*
*&      Form  print_reservation
*&---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM print_reservation USING p_vornr TYPE AFVGD-VORNR.
                                                 "smart: 2010-08-02 #105

  DATA: header_printed(1) TYPE c,
        w_index TYPE i.

  CLEAR: header_printed, w_index.

* Print all materials for the current order (now in sorted order)
  LOOP AT index_tab WHERE vornr EQ p_vornr.
    w_index = w_index + 1.
* Fill DDIC-Strutcure of component
    READ TABLE resbd_tab INDEX index_tab-index_cmp.
    resbd = resbd_tab.

    IF header_printed IS INITIAL.
      CALL FUNCTION 'CONTROL_FORM'
           EXPORTING
                command = 'PROTECT'.
      IF print_co-barco IS INITIAL.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'CMP_DATA_HDR'
                  window  = 'MAIN'.
      ELSE.
        CALL FUNCTION 'WRITE_FORM'
             EXPORTING
                  element = 'CMP_DATA_BC_HDR'
                  window  = 'MAIN'.
      ENDIF.
*      CALL FUNCTION 'CONTROL_FORM'
*           EXPORTING
*                command = 'ENDPROTECT'.
      header_printed = 'X'.
    ENDIF.

* Get sequence
    IF resbd-plnfl NE affld-plnfl OR resbd-aufnr NE caufvd-aufnr.
      READ TABLE affld_tab INDEX index_tab-index_seq.
      affld = affld_tab.
* Read ATAB-Tables for sequence
      PERFORM pppr_get_tables USING drpart-seq.
    ENDIF.

* Get operation
    IF resbd-aplzl NE afvgd-aplzl OR resbd-aufnr NE afvgd-aufnrd.
      READ TABLE afvgd_tab INDEX index_tab-index_opr.
      afvgd = afvgd_tab.
* Read ATAB-Tables for sequence
      PERFORM pppr_get_tables USING drpart-opr.
* set flag 'write info of operation'
      CLEAR flg_opr_info.
      aplzl_sav = afvgd-aplzl.
    ENDIF.

* Get ATAB-tables of component
    PERFORM pppr_get_components.

* print data of components
    IF w_index GT 1.
      CALL FUNCTION 'CONTROL_FORM'
           EXPORTING
                command = 'PROTECT'.
    ENDIF.

    IF print_co-barco IS INITIAL.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = 'CMP_DATA'
                window  = 'MAIN'.
    ELSE.
      CALL FUNCTION 'WRITE_FORM'
           EXPORTING
                element = 'CMP_DATA_BC'
                window  = 'MAIN'.
    ENDIF.
    CALL FUNCTION 'CONTROL_FORM'
         EXPORTING
              command = 'ENDPROTECT'.
*    IF print_co-barco IS INITIAL.
*      CALL FUNCTION 'WRITE_FORM'
*           EXPORTING
*                element  = 'CMP_DATA_HDR'
*                function = 'DELETE'
*                type     = 'TOP'
*                window   = 'MAIN'.
*    ELSE.
*      CALL FUNCTION 'WRITE_FORM'
*           EXPORTING
*                element  = 'CMP_DATA_BC_HDR'
*                function = 'DELETE'
*                type     = 'TOP'
*                window   = 'MAIN'.
*    ENDIF.
*   print material text
*    PERFORM pppr_print_cmp_text.
  ENDLOOP.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            element = 'LINE'
            window  = 'MAIN'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            element = 'LINE_EMPTY'
            window  = 'MAIN'.

ENDFORM.                    " print_reservation
* End of "TR1K915243


* INCLUDE for ATAB-Table-Read
INCLUDE codrgett.     "PPPR-Form-Routinen: pppr_get_tables
INCLUDE codrif01.     "PPPR-Form-Routinen: Druck-Parts lesen
INCLUDE codrif02.     "PPPR-Form-Routinen: Open/Close Form
INCLUDE codrif04.     "PPPR-Form-Routinen: read_mat
*INCLUDE codrif14.     "PSFC-Form-Routinen: pppr_print_cmp_to_opr
INCLUDE codrif15.     "PPPR-form-Routines: Print Components
INCLUDE codrif17.     "PSFC-Form-Routinen: std_init_operation
INCLUDE codrif20.     "PSFC-Form-Routinen: pppr_collect_destinations
*&---------------------------------------------------------------------*
*&      Form  formato_rep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM formato_rep.

  LOOP AT resbd_tab  INTO x_resbd.                        "DGU/krb1003
*    SELECT SINGLE * FROM mara  INTO x_mara               "DGU/krb1003
*           WHERE matnr = x_resbd-matnr.                  "DGU/krb1003
*    CHECK sy-subrc = 0   AND                             "DGU/krb1003
*          x_mara-mtart = x_mtart.                        "DGU/krb1003

* Read Einzelmenge aus Klassifizierung                     "DGU/krb1003
    g_objek = x_resbd-matnr.                               "DGU/I020676
    klah-class = 'ZDGU_PP_ALLGEMEIN'.                      "DGU/krb1003
    klah-klart = '001'.                                    "DGU/krb1003
    "DGU/krb1003
    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'         "DGU/krb1003
         EXPORTING                                         "DGU/krb1003
              class              = klah-class              "DGU/krb1003
              classtype          = klah-klart              "DGU/krb1003
              object             = g_objek                 "DGU/krb1003
              objecttable        = 'MARA'                  "DGU/krb1003
         TABLES                                            "DGU/krb1003
              t_class            = g_t_sclass              "DGU/krb1003
              t_objectdata       = g_t_clobjdat            "DGU/krb1003
         EXCEPTIONS                                        "DGU/krb1003
              no_classification  = 1                       "DGU/krb1003
              no_classtypes      = 2                       "DGU/krb1003
              invalid_class_type = 3                       "DGU/krb1003
              OTHERS             = 4.                      "DGU/krb1003
    "DGU/krb1003
    IF sy-subrc = 0.                                       "DGU/krb1003
      READ TABLE g_t_clobjdat                              "DGU/krb1003
       WITH KEY atnam = 'ZDGU_MM_EINZELMENGE'.             "DGU/krb1003
      IF sy-subrc = 0.                                     "DGU/krb1003
        IF g_t_clobjdat-ausp1(1) <> '?'.                   "DGU/krb1003
          x_bdmng = g_t_clobjdat-ausp1.                    "DGU/krb1003
        ENDIF.                                             "DGU/krb1003
      ENDIF.                                               "DGU/krb1003
    ENDIF.                                                 "DGU/krb1003

    IF x_bdmng eq 0.
      x_bdmng = 10000.                    "DGU/I020676
    else.                                                "DGU/krb1003
      x_anz_repo = ceil( x_resbd-bdmng / x_bdmng ) + 2.  "DGU/krb1003
      x_pag_repo = ceil( x_anz_repo  / 6 ).              "DGU/krb1003
      EXIT.                         " OK ->              "DGU/krb1003
    ENDIF.                                               "DGU/krb1003
    "die erste Position der Reservationen ist für die Frontseite
    "unter No. Compuesto auszugeben. Die Reihenfolge der Pos.
    "entscheidet.....
    exit.
  ENDLOOP.                                               "DGU/krb1003

ENDFORM.                    " formato_rep
*&---------------------------------------------------------------------*
*&      Form  formato_rep_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM formato_rep_others.

  LOOP AT resbd_tab  INTO x_resbd.
    check: sy-tabix ne 1.
    clear: x_mara, g_t_sclass, g_t_clobjdat, g_objek,
           g_t_clobjdat-ausp1, x_bdmng,
           x_anz_repo, x_pag_repo.
    SELECT SINGLE * FROM mara  INTO x_mara
           WHERE matnr = x_resbd-matnr.
    CHECK sy-subrc = 0   AND
          x_mara-mtart ne x_mtart.

* Read Einzelmenge aus Klassifizierung
    g_objek = x_resbd-matnr.
    klah-class = 'ZDGU_PP_ALLGEMEIN'.
    klah-klart = '001'.
    "DGU/krb1003
    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
         EXPORTING
              class              = klah-class
              classtype          = klah-klart
              object             = g_objek
              objecttable        = 'MARA'
         TABLES
              t_class            = g_t_sclass
              t_objectdata       = g_t_clobjdat
         EXCEPTIONS
              no_classification  = 1
              no_classtypes      = 2
              invalid_class_type = 3
              OTHERS             = 4.
    "DGU/krb1003
    IF sy-subrc = 0.
      READ TABLE g_t_clobjdat
       WITH KEY atnam = 'ZDGU_MM_EINZELMENGE'.
      IF sy-subrc = 0.
        IF g_t_clobjdat-ausp1(1) <> '?'.
          x_bdmng = g_t_clobjdat-ausp1.
        ENDIF.
      ENDIF.
    ENDIF.

    IF x_bdmng eq 0.
      x_bdmng = 10000.
    else.
      x_anz_repo = ceil( x_resbd-bdmng / x_bdmng ) + 2.
      x_pag_repo = ceil( x_anz_repo  / 6 ).
    ENDIF.

* get destination
    LOOP AT destination_tab.
      pr_options-tddest = destination_tab-dest.
* call OPEN_FORM to open formular
      print_co-forml = 'ZPPAUFT_GM_200_4'.
      PERFORM pppr_open_form USING 'FIRST'.
* Print 3 parts of mexican form
*          PERFORM print_mx.
      CALL FUNCTION 'WRITE_FORM'                           "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                element = 'DUMMY'                          "DGU/krb1003
           EXCEPTIONS                                      "DGU/krb1003
                OTHERS  = 0.                               "DGU/krb1003
      CALL FUNCTION 'CONTROL_FORM'                         "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                command = 'NEW-PAGE SECOND'.               "DGU/krb1003
      CALL FUNCTION 'WRITE_FORM'                           "DGU/krb1003
           EXPORTING                                       "DGU/krb1003
                element = 'DUMMY'                          "DGU/krb1003
           EXCEPTIONS                                      "DGU/krb1003
                OTHERS  = 0.
* call CLOSE_FORM to finish formular
      PERFORM pppr_close_form.
    ENDLOOP.

  ENDLOOP.
ENDFORM.                    " formato_rep_others
