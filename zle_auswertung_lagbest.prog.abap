*& Report  ZLE_AUSWERTUNG_LAGBEST
*&
*&
*&            Dätwyler AG
*&
*&
*&    Programm-Name....: ZLE_AUSWERTUNG_LAGBEST
*&    Entwickler.......: Markus Raffeiner
*&    Erstell-Datum....: 20.05.2010
*&    Version..........: 1.0
*&    Zweck............: Ausgabe Lagerbestand je:
*&                       - Werk, Lagerplatz, Material, Charge
*&
*&    Input............: Tabellen LQUA (Lagerquants), MARA (Material)
*&
*&
*&    Output...........: Excel-Ausgabe
*&
*&    Regeln...........:
*&
*&
*&    Bemerkung........: Wird aus der Transaktion ZLG_LAGBEST ange-
*&                       stossen
*&
*&
*&    Aenderungen:
*&
*&    24.08.2010: M.Raffeiner, numerische Felder durch Charakter-Felder
*&                für Excel ersetzen
*&    22.11.2010: M.Raffeiner, Übersetzung auf Englisch
*&    24.06.2014: M.Raffeiner, Neues Feld Trommelnummer CR 20140616_0919
*&    20.06.2014: M.Raffeiner, Selektion zwischen Ausgabe
*&               "Werk einzel" und "Werk(e) zusammengefasst


************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105
************************************************************************

REPORT ZLE_AUSWERTUNG_LAGBEST line-size 500.


tables: lagp, lqua, mara, makt, mard, mbew, zmkdisp1, ztgbt, ztsbt,
        ztstbt, mch1.

*parameters: p_lgnum like lqua-lgnum.
*select-options: s_lgtyp for lqua-lgtyp.
*select-options: s_lgpla for lqua-lgpla.


select-options: s_werks for lqua-werks obligatory.
select-options: s_lgnum for lqua-lgnum.
select-options: s_lgtyp for lqua-lgtyp.
select-options: s_lgpla for lqua-lgpla.
select-options: s_rollnr for mch1-zzrollnr.
select-options: s_matnr for lqua-matnr.
select-options: s_mtart for mara-mtart.
select-options: s_prdha for mara-prdha.
select-options: s_zzgb for mara-zzgb.


SELECTION-SCREEN BEGIN OF LINE.
***SELECTION-SCREEN COMMENT 1(31) TEXT-S01.
PARAMETERS:   p_DAUVO LIKE RLIST-DAUVO no-display.
***SELECTION-SCREEN POSITION 58.
PARAMETERS:   p_DAUBI LIKE RLIST-DAUBI  DEFAULT '999999' no-display.
SELECTION-SCREEN END OF LINE.

* Werk einzel ausgeben
parameters: p_rbwke radiobutton group rb1.
* Werke zusammengefasst
parameters: p_rbwkz radiobutton group rb1.

parameters: p_filout LIKE rlgrap-filename
            default 'C:\TEMP\AUSWERT_LAGBEST'.


data: gv_subrc type sysubrc.

data: begin of gs_mch1,
        zzrollnr type zzrollnr,
      end of gs_mch1.

data: begin of h,
        DAUER LIKE RL01S-DAUER,
        anz_elem type i,
        stprs like mbew-stprs,
        salk3 like mbew-salk3,
        peinh like mbew-peinh,
      end of h.

data: begin of it occurs 0,
        werks like marc-werks,
        lgtyp like lqua-lgtyp,
        lgpla like lagp-lgpla,
        charg like lqua-charg,
        matnr like mara-matnr,
        gesme like lqua-gesme,
        edatu like lqua-edatu,
      end of it.


data: begin of it_zmkdisp1 occurs 0,
        matnr like zmkdisp1-matnr,
        charg like zmkdisp1-charg,
        vbeln like zmkdisp1-vbeln,
        erdat like zmkdisp1-erdat,
        bdmng like zmkdisp1-bdmng,
        bdter like zmkdisp1-bdter,
      end of it_zmkdisp1.


data: wa_zmkdisp1 like it_zmkdisp1.


* Ausgabetabelle
data: begin of ot occurs 0,
        werks like marc-werks,
        lgtyp(8),
        lgpla like lagp-lgpla,
        matnr like mara-matnr,
        maktx like makt-maktx,
        prdha like mara-prdha,
        zzgb like mara-zzgb,
        gbtxt like ztgbt-vtext,
        sbtxt like ztsbt-vtext,
        stbtxt like ztstbt-vtext,
        mtart like mara-mtart,
        charg like lqua-charg,
        zzrollnr like mch1-zzrollnr,
        dauer(6),
        labst(13),
        meins like mara-meins,
        vprsv(2),
        stprs(11),
        peinh(5),
        waers like t001-waers,
        salk3(13),
        bdmng(13),
***        bdmng like zmkdisp1-bdmng,
        vbeln like zmkdisp1-vbeln,
***        ernam like vbak-ernam,
        ernam(30),
        erdat(10),
        bdter(10),
      end of ot.

* Summentabelle pro Material
types: begin of t_sum_mat,
         matnr type matnr,
         labst type labst,
         meins type meins,
       end of t_sum_mat.

data: gs_sum_mat type t_sum_mat.
data: gt_sum_mat type standard table of t_sum_mat.

* Konstante
  constants: c_on type c value 'X'.
  constants: c_minus type c value '-'.

*----------------------------------------------------------------------*
*   Initialisierung                                                    *
*----------------------------------------------------------------------*
INITIALIZATION.

* Syntax von MASKE siehe Doku FB WS_FILENAME_GET
  DATA: BEGIN OF gs_maske,
          ',',
          f01(30),
          ',',
          '*.*',
          ',',
          f02(30),
          ',',
          '*.*',
          '.',
        END OF gs_maske.


*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filout.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            mask             = gs_maske
       IMPORTING
            filename         = p_filout
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.


*---------------------------------------------------------------------*
* Programmstart
*---------------------------------------------------------------------*
start-of-selection.


*---------------------------------------------------------------------*
* LAGP
*---------------------------------------------------------------------*

*  select * from lagp
*    where lgnum = p_lgnum and
*          lgtyp in s_lgtyp and
*          lgpla in s_lgpla.
*
* *........Nur Plätze mit bestimmten Kennzeichen Inventur...............
*.
*
*  CHECK LAGP-KZINV IN KZINV.
*
**........Nur Plätze mit Bestand?.......................................
*
*  IF NOT  PMITB  IS INITIAL.
*    CHECK PMITB NE LAGP-KZLER.         " Nur belegte Plätze
*  ELSE.
*    IF LAGP-KZLER = CON_X.             " Alle Plätze
**.......Leere Plätze in interne Tabelle ITAB schreiben.................
*      CLEAR ITAB.
*      if lv_tmcnv = con_false.
*        MOVE TEXT-002 TO ITAB-MATNR.
*      else.
*        move space to itab-matnr.
*      endif.
*      MOVE-CORRESPONDING LAGP TO ITAB.
*      APPEND ITAB.
*    ENDIF.
*  ENDIF.

clear gt_sum_mat.

*---------------------------------------------------------------------*
* LQUA-MARA (inner Join)
*---------------------------------------------------------------------*
  select lqua~werks lqua~lgtyp lqua~lgpla lqua~charg lqua~matnr
         lqua~gesme lqua~edatu
    into table it
    from lqua inner join mara
    on lqua~matnr = mara~matnr
     where lqua~lgnum in s_lgnum and
           lqua~matnr in s_matnr and
           lqua~werks in s_werks and
           lqua~lgtyp in s_lgtyp and
           lqua~lgpla in s_lgpla and
           mara~mtart in s_mtart and
           mara~prdha in s_prdha and
           mara~zzgb in s_zzgb.



  sort it by werks lgtyp lgpla matnr.

  loop at it.

*   Rollennummer ermitteln, welche als Selektion eingegeben werden kann
    perform get_data_mch1 changing gv_subrc.

    check gv_subrc eq 0.

    h-dauer =  sy-datlo - it-edatu.

    IF p_dauvo is initial.
      check h-dauer <= p_daubi.
    else.
      check h-dauer between p_dauvo and p_daubi.
    endif.

    if it-edatu is initial or
       it-gesme < 0.
      h-dauer = space.
    endif.

*---------------------------------------------------------------------*
* Ausgabetabelle aufbereiten
*---------------------------------------------------------------------*
    if p_rbwke eq c_on.  "Detailausgabe selektiert
      perform ausgabe_detail.
    else.
      perform ausgabe_zusammengefasst. "pro Material
    endif.

  endloop.  "it


*---------------------------------------------------------------------*
* Programmende
*---------------------------------------------------------------------*
end-of-selection.

* Bei Werke zusammengefasst muss die Summentabelle pro Material in die
* Ausgabetabelle für Excel übertragen werden. Da Excel nur Charakter
* wegen dem Titel zulässt, muss diese Summentabelle benötigt werden
  if p_rbwkz eq c_on.
    perform sumtab_material_to_exceltab.
  endif.

*---------------------------------------------------------------------*
* Ausgabe der aufbereiteten Daten in Excel / auf Bildschirm
*---------------------------------------------------------------------*
  perform excel_ausgabe.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA_MCH1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*----------------------------------------------------------------------*
FORM GET_DATA_MCH1  CHANGING P_SUBRC type sysubrc.

  p_subrc = 4.

  clear gs_mch1.

* Material ohne Charge
  if it-charg is initial.

    if s_rollnr[] is initial.  "Keine Selektion
      p_subrc = 0.
    endif.

    exit.  "Routine verlassen

  endif.

* Material mit Charge

* Rollennumer aus Tabelle mch1 besorgen
  select single zzrollnr from mch1 into gs_mch1
    where matnr = it-matnr and
          charg = it-charg and
          zzrollnr in s_rollnr.  "unschön

  p_subrc = sy-subrc.

ENDFORM.                    " GET_DATA_MCH1


*&---------------------------------------------------------------------*
*&      Form  ausgabe_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausgabe_detail.

  clear ot.

* Werk
  move it-werks to ot-werks.

* Lagertyp
  move it-lgtyp to ot-lgtyp.

* Lagerplatz
  move it-lgpla to ot-lgpla.

* Material
  move it-matnr to ot-matnr.

* Materialkurztext
  perform get_makt using it-matnr
                         sy-langu
                changing ot-maktx.


* Tabelle mara lesen
  perform get_mara using it-matnr.

* Produkthierarchie
  move mara-prdha to ot-prdha.

* Geschäftsbereich
  move mara-zzgb to ot-zzgb.

* Geschäftsbereich
  perform get_ztgbt using mara-zzgb
                 changing ot-gbtxt.

* Sortiment
  perform get_ztsbt using mara-zzsb
                 changing ot-sbtxt.

* Sortimentsteilbereich
  perform get_ztstbt using mara-zzstb
                  changing ot-stbtxt.

* Materialart
  move mara-mtart to ot-mtart.

* Charge
  move it-charg to ot-charg.

* Rollennummer
  move gs_mch1-zzrollnr to ot-zzrollnr.

* Dauer
  write h-dauer to ot-dauer.

* Aktueller Bestand aus Tabelle mard
*  perform aktueller_bestand_aus_mard using it-matnr
*                                           it-werks
*                                  changing ot-labst.

* aktueller Bestand
  write it-gesme to ot-labst decimals 3 no-grouping.

  if it-gesme lt 0.
    perform vorzeichen_voranstellen using c_minus
                                          ot-labst
                                 changing ot-labst.
  endif.

* Basismengeneinheit
  move mara-meins to ot-meins.

* Tabelle mbew lesen
  perform get_mbew using it-matnr
                         it-werks.

* Preissteuerungskennzeichen
  move mbew-vprsv to ot-vprsv.

* Preis, Einheit
  if mbew-vprsv = 'S'.
*   Standardpreis
    move mbew-stprs to h-stprs.
    write mbew-stprs to ot-stprs decimals 2 no-grouping.
    move mbew-peinh to h-peinh.
    write mbew-peinh to ot-peinh.  "Preiseinheit
  elseif mbew-vprsv = 'V'.
*   Durchschnitts-/Verrechnungspreis
    move mbew-verpr to h-stprs.
    write mbew-verpr to ot-stprs decimals 2 no-grouping.
    move mbew-peinh to h-peinh.  "Preiseinheit
    write mbew-peinh to ot-peinh.  "Preiseinheit
  endif.

* Währung via Werk->Bukrs bestimmen
  perform get_waers using it-werks
                 changing ot-waers.

*aktuellen Wert berechnen: aktueller Bestand * Preis / Einheit
  if h-peinh ne 0.
    h-salk3 = it-gesme * h-stprs / h-peinh.
    write h-salk3 to ot-salk3 decimals 3 no-grouping.

    if h-salk3 lt 0.
      perform vorzeichen_voranstellen using c_minus
                                            ot-salk3
                                   changing ot-salk3.
    endif.

  endif.  "h-peinh ne 0

*** move mbew-salk3 to ot-salk3.

* Tabelle zmkdisp1 lesen
  perform get_zmkdisp1 using it-matnr
                             it-charg
                             it-werks
                    changing wa_zmkdisp1.

* Bedarfsmenge
  write wa_zmkdisp1-bdmng to ot-bdmng no-zero no-grouping.

* Verkaufsbeleg
  write wa_zmkdisp1-vbeln to ot-vbeln no-zero.

* Erfassername vom Beleg
  perform get_erfassername using wa_zmkdisp1-vbeln
                        changing ot-ernam.
* Eröffnungsdatum
  write wa_zmkdisp1-erdat to ot-erdat DD/MM/YYYY no-zero.

* Bedarfsdatum
  write wa_zmkdisp1-bdter to ot-bdter DD/MM/YYYY no-zero.


*---------------------------------------------------------------------*
* Element in Ausgabetabelle hinzufügen
*---------------------------------------------------------------------*
  append ot.


ENDFORM.                    " ausgabe_detail


*&---------------------------------------------------------------------*
*&      Form  ausgabe_zusammengefasst
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausgabe_zusammengefasst.

  clear gs_sum_mat.

* Material
  move it-matnr to gs_sum_mat-matnr.

* aktueller Bestand
  move it-gesme to gs_sum_mat-labst.

*---------------------------------------------------------------------*
* Element in Ausgabetabelle hinzufügen
*---------------------------------------------------------------------*
  collect gs_sum_mat into gt_sum_mat.


ENDFORM.                    " ausgabe_zusammengefasst

*&---------------------------------------------------------------------*
*&      Form  get_mara
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_MATNR  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_mara USING p_matnr TYPE MARA-MATNR.     "smart: 2010-08-02 #105

  if p_matnr = mara-matnr.
    exit.
  endif.

  clear mara.
  select single * from mara where matnr = p_matnr.

ENDFORM.                    " get_mara


*&---------------------------------------------------------------------*
*&      Form  get_makt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_MATNR  text
*      -->P_0222   text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_makt USING p_matnr TYPE MARA-MATNR      "smart: 2010-08-02 #105
                    p_spras TYPE SYST-LANGU      "smart: 2010-08-02 #105
           changing p_maktx TYPE MAKT-MAKTX.     "smart: 2010-08-02 #105

  if makt-matnr = p_matnr and
     makt-spras = p_spras.
    p_maktx = makt-maktx.
    exit.
  endif.

  clear makt.
  select single * from makt where matnr = p_matnr and
                                  spras = p_spras.

  p_maktx = makt-maktx.

ENDFORM.                    " get_makt


*&---------------------------------------------------------------------*
*&      Form  get_ztgbt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MARA_ZZGB  text
*      <--P_OT_GBTXT  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_ztgbt USING p_zzgb TYPE MARA-ZZGB       "smart: 2010-08-02 #105
            CHANGING p_gbtxt TYPE ZTGBT-VTEXT.   "smart: 2010-08-02 #105


  clear p_gbtxt.

  select single vtext from ztgbt into p_gbtxt
    where spras = sy-langu and
          zzgb = p_zzgb.


ENDFORM.                    " get_ztgbt


*&---------------------------------------------------------------------*
*&      Form  get_ztsbt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MARA_ZZSB  text
*      <--P_OT_SBTXT  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_ztsbt USING p_zzsb TYPE MARA-ZZSB       "smart: 2010-08-02 #105
            CHANGING p_sbtxt TYPE ZTSBT-VTEXT.   "smart: 2010-08-02 #105


  clear p_sbtxt.

  select single vtext from ztsbt into p_sbtxt
    where spras = sy-langu and
          zzsb = p_zzsb.


ENDFORM.                    " get_ztsbt


*&---------------------------------------------------------------------*
*&      Form  get_ztstbt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MARA_ZZSTB  text
*      <--P_OT_STBTXT  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_ztstbt USING p_zzstb TYPE MARA-ZZSTB    "smart: 2010-08-02 #105
            CHANGING p_stbtxt TYPE ZTSTBT-VTEXT. "smart: 2010-08-02 #105


  clear p_stbtxt.

  select single vtext from ztstbt into p_stbtxt
    where spras = sy-langu and
          zzstb = p_zzstb.


ENDFORM.                    " get_ztstbt


*&---------------------------------------------------------------------*
*&      Form  aktueller_bestand_aus_mard
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_MATNR  text
*      -->P_LQUA_WERKS  text
*      <--P_OT_LABST  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM aktueller_bestand_aus_mard USING p_matnr
                                      p_werks
                             CHANGING p_labst.


  if mard-matnr = p_matnr and
     mard-werks = p_werks.
    p_labst = mard-labst.
    exit.
  endif.


  clear mard.
  select single * from mard where matnr = p_matnr and
                                  werks = p_werks.

  p_labst = mard-labst.


ENDFORM.                    " aktueller_bestand_aus_mard


*&---------------------------------------------------------------------*
*&      Form  get_mbew
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_MBEW  text
*      -->P_LQUA_WERKS  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_mbew USING p_matnr TYPE MARA-MATNR      "smart: 2010-08-02 #105
                    p_bwkey TYPE MARC-WERKS.     "smart: 2010-08-02 #105


  if mbew-matnr = p_matnr and
     mbew-bwkey = p_bwkey.
    exit.
  endif.


  clear mbew.
  select single * from mbew where matnr = p_matnr and
                                  bwkey = p_bwkey.


ENDFORM.                    " get_mbew


*&---------------------------------------------------------------------*
*&      Form  get_waers
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_WERKS  text
*      <--P_IT_WAERS  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_waers USING p_werks TYPE MARC-WERKS     "smart: 2010-08-02 #105
            CHANGING p_waers TYPE T001-WAERS.    "smart: 2010-08-02 #105

  data: l_bukrs like t001-bukrs.
  data: l_waers like t001-waers.

  clear p_waers.


* Buchungskreis via Werk bestimmen
  CALL FUNCTION 'DETERMIN_BWKEY_BUKRS_FOR_PLANT'
    EXPORTING
      WERK          = p_werks
   IMPORTING
*     BWKEY         =
     BUKRS         = l_bukrs.

  check sy-subrc eq 0.

* Währung via Buchungskreis bestimmen
  CALL FUNCTION 'REBW_WAERS_GET_FROM_BUKRS'
       EXPORTING
            IV_BUKRS = l_bukrs
       IMPORTING
            EV_WAERS = l_waers.


  p_waers = l_waers.

ENDFORM.                    " get_waers


*&---------------------------------------------------------------------*
*&      Form  get_zmkdisp1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_MATNR  text
*      -->P_LQUA_CHARG  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_zmkdisp1 USING  p_matnr TYPE MARA-MATNR "smart: 2010-08-02 #105
                         p_charg TYPE LQUA-CHARG "smart: 2010-08-02 #105
                         p_werks TYPE MARC-WERKS "smart: 2010-08-02 #105
                changing p_zmkdisp1 structure wa_zmkdisp1.


  refresh it_zmkdisp1.
  clear p_zmkdisp1.

  select matnr charg vbeln erdat bdmng bdter from zmkdisp1
    into table it_zmkdisp1
    where matnr = p_matnr and
          charg = p_charg and
          werks = p_werks.

  check sy-subrc eq 0.

* Nach Bedarfsmenge absteigend sortieren (Grösster Wert als 1. Element)
  sort it_zmkdisp1 by bdmng descending.

  read table it_zmkdisp1 index 1 into p_zmkdisp1.



ENDFORM.                    " get_zmkdisp1


*&---------------------------------------------------------------------*
*&      Form  get_erfassername
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZMKDISP1_VBELN  text
*      <--P_OT_ERNAM  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_erfassername USING    p_vbeln TYPE      "smart: 2010-08-02 #105
  ZMKDISP1-VBELN                                 "smart: 2010-08-02 #105
                      CHANGING p_ernam LIKE ot-ernam.
                                                 "smart: 2010-08-02 #105


  data: l_ernam like vbak-ernam.
  data: l_ernam_org like ot-ernam.
  data: l_wa_usr21 like usr21.
  data: l_department like adcp-department.

  clear: l_ernam, l_ernam_org, l_department.
  clear p_ernam.

  check not p_vbeln is initial.

* Eröffnungsname
  select single ernam from vbak into l_ernam
    where vbeln = p_vbeln.

  check sy-subrc = 0.

* Organisation via Benutzername holen
  select single adcp~department into l_department
    from usr21 inner join adcp
    on usr21~persnumber = adcp~persnumber
    where bname = l_ernam.


* Rückgabewert: Benutzername, Organisation.
  if l_department is initial.
    p_ernam = l_ernam.
  else.
    concatenate l_ernam l_department into p_ernam
    separated by space IN CHARACTER MODE .       "smart: 2010-08-02 #101
  endif.


****
  exit.
*****

  select single * from usr21 into l_wa_usr21
    where bname = l_ernam.

  check sy-subrc = 0.


*  select single adcp-department into l_department
*    where


ENDFORM.                    " get_erfassername


*&---------------------------------------------------------------------*
*&      Form  tabelle_ausgeben
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tabelle_ausgeben.

  loop at ot.

    write: / ot-werks,
             ot-lgpla,
             ot-matnr no-zero,
             ot-maktx,
             ot-prdha,
             ot-gbtxt,
             ot-sbtxt,
             ot-stbtxt,
             ot-mtart,
             ot-charg,
             ot-dauer,
             ot-labst decimals 2,
             ot-meins,
             ot-vprsv,
             ot-stprs,
             ot-peinh,
             ot-waers,
             ot-salk3,
             ot-bdmng decimals 2,
             ot-vbeln,
             ot-erdat DD/MM/YYYY,
             ot-bdter DD/MM/YYYY.

  endloop.

ENDFORM.                    " tabelle_ausgeben



*&---------------------------------------------------------------------*
*&      Form  excel_ausgabe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_ausgabe.

  data: begin of lt_fnames occurs 0,
          text(60) type c,
        end of lt_fnames.

  describe table ot lines h-anz_elem.

* Falls keine Daten vorhanden: nur Meldung auf Statuszeile ausgeben
  if h-anz_elem eq 0.
    message s252(aq).
    exit.
  endif.


* Titel aufbereiten

* Werk
  lt_fnames = text-t01.
  append lt_fnames.
* Lagertyp
  lt_fnames = text-t24.
  append lt_fnames.
* Lagerplatz
  lt_fnames = text-t02.
  append lt_fnames.
* Material
  lt_fnames = text-t03.
  append lt_fnames.
* Materialkurztext
  lt_fnames = text-t04.
  append lt_fnames.
* Produkthierarchie
  lt_fnames = text-t05.
  append lt_fnames.
* Gechäftsbereich
  lt_fnames = text-t23.
  append lt_fnames.
* Gechäftsbereichtext
  lt_fnames = text-t06.
  append lt_fnames.
* Sortimentsbereichtext
  lt_fnames = text-t07.
  append lt_fnames.
* Sortimentsteilbereichtext
  lt_fnames = text-t08.
  append lt_fnames.
* Materialart
  lt_fnames = text-t09.
  append lt_fnames.
* Charge
  lt_fnames = text-t10.
  append lt_fnames.
* Rollennummer
  lt_fnames = text-t26.
  append lt_fnames.
* Dauer
  lt_fnames = text-t11.
  append lt_fnames.
* aktueller Bestand
  lt_fnames = text-t12.
  append lt_fnames.
* Mengeneinheit
  lt_fnames = text-t13.
  append lt_fnames.
* Preissteurungscode
  lt_fnames = text-t14.
  append lt_fnames.
* Preis
  lt_fnames = text-t15.
  append lt_fnames.
* Preiseinheit
  lt_fnames = text-t16.
  append lt_fnames.
* Währungscode
  lt_fnames = text-t17.
  append lt_fnames.
* aktueller Wert
  lt_fnames = text-t18.
  append lt_fnames.
* Bedarfsmenge
  lt_fnames = text-t19.
  append lt_fnames.
* Verkaufsbeleg
  lt_fnames = text-t20.
  append lt_fnames.
* Erfassername
  lt_fnames = text-t25.
  append lt_fnames.
* Erfassungsdatum
  lt_fnames = text-t21.
  append lt_fnames.
* Bedarfstermin
  lt_fnames = text-t22.
  append lt_fnames.


  call function 'MS_EXCEL_OLE_STANDARD_DAT'
       exporting
            file_name                 = p_filout
            create_pivot              = 0
*           DATA_SHEET_NAME           = ' '
*           PIVOT_SHEET_NAME          = ' '
*           PASSWORD                  = ' '
*           PASSWORD_OPTION           = 0
       tables
*           PIVOT_FIELD_TAB           =
            data_tab                  = ot
            fieldnames                = lt_fnames.


ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  SUMTAB_MATERIAL_TO_EXCELTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMTAB_MATERIAL_TO_EXCELTAB .

  loop at gt_sum_mat into gs_sum_mat.

    clear ot.

*   Material
    move gs_sum_mat-matnr to ot-matnr.

*   Materialkurztext
    perform get_makt using gs_sum_mat-matnr
                           sy-langu
                  changing ot-maktx.

*   Tabelle mara lesen
    perform get_mara using gs_sum_mat-matnr.

*   Produkthierarchie
    move mara-prdha to ot-prdha.

*   Geschäftsbereich
    move mara-zzgb to ot-zzgb.

*   Geschäftsbereich
    perform get_ztgbt using mara-zzgb
                   changing ot-gbtxt.

*   Sortiment
    perform get_ztsbt using mara-zzsb
                   changing ot-sbtxt.

*   Sortimentsteilbereich
    perform get_ztstbt using mara-zzstb
                    changing ot-stbtxt.

*   Materialart
    move mara-mtart to ot-mtart.

*   aktueller Bestand
    write gs_sum_mat-labst to ot-labst decimals 3 no-grouping.

    write gs_sum_mat-labst to ot-labst decimals 3 no-grouping.

    if h-salk3 lt 0.
      perform vorzeichen_voranstellen using c_minus
                                            ot-labst
                                   changing ot-labst.
    endif.

*   Basismengeneinheit
    move mara-meins to ot-meins.

*   Vorzeichen
***    perform vorzeichen.

*---------------------------------------------------------------------*
*   Element in Ausgabetabelle hinzufügen
*---------------------------------------------------------------------*
    append ot.

  endloop.  "gt_sum_mat

ENDFORM.                    " SUMTAB_MATERIAL_TO_EXCELTAB

*&---------------------------------------------------------------------*
*&      Form  vorzeichen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vorzeichen_voranstellen using p_vorzeichen type c
                                   value(p_feldin) type clike
                          changing value(p_feldout) type clike.

* Wegen Excel muss das Vorzeichen von hinten nach vorne gebracht werden

  data: lv_offset type i.
  data: lv_feld_out type c length 30.  "lang genug definieren

* Falls unten was schief läuft, einfach den Eingabewert zurückgeben
  p_feldout = p_feldin.

  clear: lv_offset, lv_feld_out.

  lv_offset = strlen( p_feldin ) - 1.

  check lv_offset gt 0.

  write p_feldin to lv_feld_out no-grouping.

  clear lv_feld_out+lv_offset(1).  "Vorzeichen hinten löschen
  shift lv_feld_out right by 1 places in character mode.
  lv_feld_out+0(1) = p_vorzeichen.
  condense lv_feld_out no-gaps.

* Wertrückgabe
  p_feldout = lv_feld_out.



ENDFORM.                    " vorzeichen
