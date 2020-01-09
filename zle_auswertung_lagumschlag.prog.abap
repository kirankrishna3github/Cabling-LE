*& Report  ZLE_AUSWERTUNG_LAGUMSCHLAG
*&
*&
*&            Dätwyler AG
*&
*&
*&    Programm-Name....: ZLE_AUSWERTUNG_LAGUMSCHLAG
*&    Entwickler.......: Markus Raffeiner
*&    Erstell-Datum....: 20.05.2010
*&    Version..........: 1.0
*&    Zweck............: Ausgabe Lagerumschlag je:
*&                       - Werk, Lagerort, Material
*&
*&    Input............: Tabellen LQUA (Lagerquants), MARA (Material)
*&
*&
*&    Output...........: Excel-Ausgabe
*&
*&    Regeln...........:
*&
*&
*&    Bemerkung........: Wird aus der Transaktion ZLG_UMSCHLAG ange-
*&                       stossen
*&
*&
*&    Aenderungen:
*&
*&    22.11.2010: M.Raffeiner, Übersetzung auf Englisch




************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #102 #105
************************************************************************

REPORT ZLE_AUSWERTUNG_LAGUMSCHLAG line-size 500.


tables: lagp, lqua, mara, makt, marc, mard, mbew, s031, s032,
        zmkdisp1, ztgbt, ztsbt,ztstbt, zzacisi.


select-options: s_werks for lqua-werks obligatory.
select-options: s_lgnum for lqua-lgnum.
select-options: s_lgort for lqua-lgort.
select-options: s_matnr for lqua-matnr.
select-options: s_mtart for mara-mtart.
select-options: s_prdha for mara-prdha.
select-options: s_zzgb for mara-zzgb.
select-options: s_labst for mard-labst.

SELECTION-SCREEN BEGIN OF LINE.
***SELECTION-SCREEN COMMENT 1(31) TEXT-T01.
PARAMETERS:   p_DAUVO LIKE RLIST-DAUVO no-display.
***SELECTION-SCREEN POSITION 58.
PARAMETERS:   p_DAUBI LIKE RLIST-DAUBI  DEFAULT '999999' no-display.
SELECTION-SCREEN END OF LINE.

parameters: p_filout LIKE rlgrap-filename
            default 'C:\TEMP\LU_MATERIAL'.


data: begin of h,
        DAUER LIKE RL01S-DAUER,
        iYear type i,
        iMonth type i,
        anz_tage type i,
        labst like mard-labst,
        filout LIKE rlgrap-filename,
        anz_elem type i,
        sw_best_verb,
        reichw(6) type p decimals 0,
        stprs like mbew-stprs,
        peinh like mbew-peinh,
        salk3 like mbew-salk3,
      end of h.


data: begin of it occurs 0,
        werks like lqua-werks,
        lgort like lqua-lgort,
        matnr like lqua-matnr,
      end of it.


data: begin of it_s032 occurs 0,
        werks like s032-werks,
        matnr like s032-matnr,
        wbwbest like s032-wbwbest,
      end of it_s032.


data: begin of it_s031 occurs 0,
        werks like s031-werks,
        matnr like s031-matnr,
        mgvbr like s031-mgvbr,
      end of it_s031.


* Ausgabetabelle
data: begin of ot occurs 0,
        werks like marc-werks,
        lgort like lqua-lgort,
        matnr like mara-matnr,
        maktx like makt-maktx,
        prdha like mara-prdha,
        zzgb  like mara-zzgb,
        gbtxt like ztgbt-vtext,
        sbtxt like ztsbt-vtext,
        stbtxt like ztstbt-vtext,
        mtart like mara-mtart,
        zzargisi(2),
        argtxt like zzacisi-text,
* Mengen-/Wertfelder
        reichw(6),
        labst(13),
        meins like mara-meins,
        vprsv(2),
        stprs(11),
        peinh(5),
        waers like t001-waers,
        salk3(13),
        letztzug(10),
        letztver(10),
      end of ot.


RANGES: r_spmon FOR s031-spmon.
RANGES: r_datum FOR sy-datum.



constants: c_max_reichw(5) value '99999'.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
* Syntax von MASKE siehe Doku FB WS_FILENAME_GET
DATA: BEGIN OF maske,
        ',',
        f01(30),
        ',',
        '*.txt',
        ',',
        f02(30),
        ',',
        '*.*',
        '.',
      END OF maske.


*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_filout.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_path         = 'C:\TEMP'
            mask             = maske
       IMPORTING
            filename         = h-filout
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

  perform auswertungszeitraum_ermitteln.


*---------------------------------------------------------------------*
* LAGP
*---------------------------------------------------------------------*

*  select * from lagp
*    where lgnum = p_lgnum and
*          lgtyp in s_lgtyp and
*          lgort in s_lgort.
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



*---------------------------------------------------------------------*
* LQUA-MARA (inner Join)
*---------------------------------------------------------------------*
  select lqua~werks lqua~lgort lqua~matnr into table it
    from lqua inner join mara
    on lqua~matnr = mara~matnr
     where lqua~lgnum in s_lgnum and
           lqua~matnr in s_matnr and
           lqua~werks in s_werks and
           lqua~lgort in s_lgort and
           mara~mtart in s_mtart and
           mara~prdha in s_prdha and
           mara~zzgb in s_zzgb.


  sort it by werks lgort matnr.

  loop at it.

    at end of matnr.

*---------------------------------------------------------------------*
* Ausgabetabelle aufbereiten
*---------------------------------------------------------------------*
      perform ausgabetabelle_aufbereiten.

    endat.

  endloop.


*---------------------------------------------------------------------*
* Programmende
*---------------------------------------------------------------------*
end-of-selection.


*---------------------------------------------------------------------*
* Ausgabe der aufbereiteten Daten in Excel / auf Bildschirm
*---------------------------------------------------------------------*
  perform excel_ausgabe.

*&---------------------------------------------------------------------*
*&      Form  auswertungszeitraum_ermitteln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM auswertungszeitraum_ermitteln.

  data: l_spmon_low like s031-sptag.             "smart: 2010-08-02 #102
  data: l_apf_tage TYPE i.
  data: l_datum like sy-datum.
  data: l_last_day_of_month like sy-datum.


*----------------------------------------------------------------------
* Zeitraum von (JJJJMM)
*----------------------------------------------------------------------
* Vorjahr / aktueller Monat
  h-iYear = sy-datum(4) - 1.
  h-iMonth = sy-datum+4(2).

  if h-iMonth lt 1.
    h-iYear = h-iYear - 1.
    h-iMonth = 12.
  endif.

  r_spmon-low(4) = h-iYear.
  r_spmon-low+4(2) = h-iMonth.

*----------------------------------------------------------------------
* Zeitraum bis (JJJJMM)
*----------------------------------------------------------------------
* akt. Jahr / Vormonat
  h-iYear = sy-datum(4).
  h-iMonth = sy-datum+4(2) - 1.

  if h-iMonth lt 1.
    h-iYear = h-iYear - 1.
    h-iMonth = 12.
  endif.

  r_spmon-high(4) = h-iYear.
  r_spmon-high+4(2) = h-iMonth.


  r_spmon-sign   = 'I'.
  r_spmon-option = 'BT'.
  APPEND r_spmon.


*----------------------------------------------------------------------
* Zeitraum von (JJJJMMTT)
*----------------------------------------------------------------------
* Vorjahr / aktueller Monat / 1. Tag im Monat
  h-iYear = sy-datum(4) - 1.
  h-iMonth = sy-datum+4(2).

  if h-iMonth lt 1.
    h-iYear = h-iYear - 1.
    h-iMonth = 12.
  endif.

  r_datum-low(4) = h-iYear.
  r_datum-low+4(2) = h-iMonth.
  r_datum-low+6(2) = '01'.

*----------------------------------------------------------------------
* Zeitraum bis (JJJJMMTT)
*----------------------------------------------------------------------
* akt. Jahr / Vormonat / Letzter Tag des Monats
  h-iYear = sy-datum(4).
  h-iMonth = sy-datum+4(2) - 1.

  if h-iMonth lt 1.
    h-iYear = h-iYear - 1.
    h-iMonth = 12.
  endif.

  r_datum-high(4) = h-iYear.
  r_datum-high+4(2) = h-iMonth.


* Letzen Tag des Monats bestimmen
  l_datum = r_datum-high(6).
  l_datum+6(2) = '01'.


  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = l_datum
       IMPORTING
            LAST_DAY_OF_MONTH = l_last_day_of_month
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  r_datum-high = l_last_day_of_month.


  r_datum-sign   = 'I'.
  r_datum-option = 'BT'.
  APPEND r_datum.



*----------------------------------------------------------------------
* Anzahl Tage aus Jahr/Monat für den Periodenzeitraum ermitteln:
* Vorlage aus Include RMCB01F0
*----------------------------------------------------------------------
  l_spmon_low = r_spmon-low.                     "smart: 2010-08-02 #102

  while l_spmon_low le r_spmon-high.             "smart: 2010-08-02 #102

    if l_spmon_low+4(2) = '13'.                  "smart: 2010-08-02 #102
      l_spmon_low+4(2) = '01'.                   "smart: 2010-08-02 #102
      l_spmon_low(4) = l_spmon_low(4) + 1.       "smart: 2010-08-02 #102
    endif.

    PERFORM anzahl_tage USING l_spmon_low        "smart: 2010-08-02 #102
                     CHANGING l_apf_tage.

    ADD l_apf_tage TO h-anz_tage.      " genaue Anzahl Tage

    l_spmon_low+4(2) = l_spmon_low+4(2) + 1.     "smart: 2010-08-02 #102

  endwhile.


ENDFORM.                    " auswertungszeitraum_ermitteln


*---------------------------------------------------------------------*
*       FORM ANZAHL_TAGE                                              *
*---------------------------------------------------------------------*
*       ermittelt Anzahl der Tage für übergebenen Monat               *
*---------------------------------------------------------------------*
*  -->  MONAT   Format: Datum, aber: Tag ist nicht gefüllt!           *
*  -->  TAGE                                                          *
*---------------------------------------------------------------------*
FORM anzahl_tage USING    monat TYPE d
                 CHANGING tage  TYPE i.

  DATA: at_monat(2) TYPE n,
        at_dats     TYPE d.

  at_monat = monat+4(2).

  CASE at_monat.
    WHEN '01' OR '03' OR '05' OR '07' OR '08' OR '10' OR '12'.
      tage = 31.
    WHEN '02'.
      at_dats(4) = monat(4).
      at_dats+4  = '0301'.
      at_dats = at_dats - 1.
      tage = at_dats+6.
    WHEN OTHERS.
      tage = 30.
  ENDCASE.

ENDFORM.                               " ANZAHL_TAGE


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

FORM get_mara USING p_matnr TYPE LQUA-MATNR.     "smart: 2010-08-02 #105

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

FORM get_makt USING p_matnr TYPE LQUA-MATNR      "smart: 2010-08-02 #105
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
*&      Form  aktueller_bestand_aus_mard
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_MATNR  text
*      -->P_LQUA_WERKS  text
*      <--P_OT_LABST  text
*----------------------------------------------------------------------*
FORM get_mard USING p_matnr
                    p_werks.


  clear mard.
  select single * from mard where matnr = p_matnr and
                                  werks = p_werks.

ENDFORM.                    " aktueller_bestand_aus_mard


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
*&      Form  get_zzacisi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MARA_ZZARGISI  text
*      <--P_OT_ARGTXT  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_zzacisi USING    p_zzargisi TYPE        "smart: 2010-08-02 #105
  MARA-ZZARGISI                                  "smart: 2010-08-02 #105
                 CHANGING p_argtxt TYPE ZZACISI-TEXT.
                                                 "smart: 2010-08-02 #105


  clear p_argtxt.

  select single text from zzacisi into p_argtxt
    where zzargisi = p_zzargisi.

ENDFORM.                    " get_zzacisi



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

FORM aktueller_bestand_aus_mard USING p_matnr    "smart: 2010-08-02 #105
  TYPE LQUA-MATNR                                "smart: 2010-08-02 #105
                                      p_werks    "smart: 2010-08-02 #105
                                        TYPE     "smart: 2010-08-02 #105
                                        LQUA-WERKS
                             CHANGING p_labst    "smart: 2010-08-02 #105
                               TYPE MARD-LABST.  "smart: 2010-08-02 #105


  clear p_labst.

  select sum( labst ) from mard into p_labst
    where matnr = p_matnr and
          werks = p_werks.


***********
  exit.
***********


**  if mard-matnr = p_matnr and
**     mard-werks = p_werks.
**    p_labst = mard-labst.
**    exit.
**  endif.
**
**
**  clear mard.
**  select single * from mard where matnr = p_matnr and
**                                  werks = p_werks.

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

FORM get_mbew USING p_matnr TYPE LQUA-MATNR      "smart: 2010-08-02 #105
                    p_bwkey TYPE LQUA-WERKS.     "smart: 2010-08-02 #105


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

FORM get_waers USING p_werks TYPE LQUA-WERKS     "smart: 2010-08-02 #105
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
*&      Form  get_s032
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_WERKS  text
*      -->P_LQUA_LGORT  text
*      -->P_LQUA_MATNR  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM get_s032 USING p_werks TYPE LQUA-WERKS      "smart: 2010-08-02 #105
                    p_lgort TYPE LQUA-LGORT      "smart: 2010-08-02 #105
                    p_matnr TYPE LQUA-MATNR.     "smart: 2010-08-02 #105

  if s032-werks = p_werks and
     s032-lgort = p_lgort and
     s032-matnr = p_matnr.
    exit.
  endif.

  clear s032.
  select single * from s032 where vrsio = '000' and
                                  werks = p_werks and
                                  lgort = p_lgort and
                                  matnr = p_matnr.

ENDFORM.                                                    " get_s032


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
             ot-lgort,
             ot-matnr no-zero,
             ot-maktx,
             ot-prdha,
             ot-gbtxt,
             ot-sbtxt,
             ot-stbtxt,
             ot-mtart,
             ot-zzargisi,
             ot-reichw,
             ot-labst decimals 2,
             ot-meins,
             ot-vprsv,
             ot-stprs,
             ot-peinh,
             ot-waers,
             ot-salk3,
             ot-letztzug,
             ot-letztver.

  endloop.

ENDFORM.                    " tabelle_ausgeben


*&---------------------------------------------------------------------*
*&      Form  reichweite_ermitteln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_WERKS  text
*      -->P_LQUA_MATNR  text
*      <--P_OT_REICHW  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM reichweite_ermitteln USING  p_werks TYPE    "smart: 2010-08-02 #105
  LQUA-WERKS                                     "smart: 2010-08-02 #105
                                 p_matnr TYPE    "smart: 2010-08-02 #105
                                   LQUA-MATNR    "smart: 2010-08-02 #105
                        CHANGING p_sw_best_verb  "smart: 2010-08-02 #105
                          LIKE h-sw_best_verb    "smart: 2010-08-02 #105
                                 p_reichw LIKE h-reichw.
                                                 "smart: 2010-08-02 #105


  data: l_mgvbr like s031-mgvbr.
  data: l_gvbr_durchs type f.


  data: l_anz_tage.
  data: l_wbwbest like s032-wbwbest.

  data: l_divisor type i.
  data: l_reichw(6) type p decimals 2.



  clear: l_mgvbr, l_anz_tage, l_wbwbest.
  clear: p_reichw.



* Vorlage: Vergleich mit Auswertung MC.C

* Gesamtverbrauchswert ermitteln
  perform gesverbrauchswert using p_werks
                                  p_matnr
                         changing l_mgvbr.

* Gesamtverbrauchswert je Tag
  l_gvbr_durchs = l_mgvbr / h-anz_tage.


* Wert Bewerteter Bestand aus S032 holen.
***>  perform bewerteter_bestand using p_werks
***>                                   p_matnr
***>                          changing l_wbwbest.

*----------------------------------------------------------------------
* Reichweite ermitteln
*
* Regel für Reichweite:
* - Falls: kein Bestand und kein Verbrauch ==> keine Listenausgabe
* - Falls: Bestand und kein Verbrauch ==> Reichweite = 99999
* - Sonst: Reichweite = akt. Bestand / durchschn. Verbrauch 12 Monate
*----------------------------------------------------------------------

  clear p_sw_best_verb.

* Kein Bestand und kein Verbrauch
***>  if l_wbwbest eq 0 and
  if h-labst eq 0 and
     l_gvbr_durchs eq 0.
    exit.
  endif.

  p_sw_best_verb = 'X'.

* Kein Verbrauch
  if l_gvbr_durchs eq 0.
    p_reichw = c_max_reichw.
    exit.
  endif.

* Berechnung
  p_reichw = h-labst / l_gvbr_durchs.
*>>>>  p_reichw = l_wbwbest / l_gvbr_durchs.



* Reichweite > 100000
  if p_reichw gt 100000.
    p_reichw = c_max_reichw.
**  elseif p_reichw between 0 and 1.
**    p_reichw = 1.
  endif.


ENDFORM.                    " reichweite_ermitteln


*&---------------------------------------------------------------------*
*&      Form  gesverbrauchswert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*      -->P_P_MATNR  text
*      <--P_L_CWGVBR  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM gesverbrauchswert_old USING p_werks
                             p_matnr
                    CHANGING p_cwgvbr.

  data: l_cwgvbr like af61x-sum01.

  clear l_cwgvbr.

  if marc-matnr = p_matnr and
     marc-werks = p_werks.
  else.
    clear marc.
    select single * from marc where matnr = p_matnr and
                                    werks = p_werks.
  endif.

  CALL FUNCTION 'VERBRAUCH_SUMMIEREN'
    EXPORTING
     ABDATUM                        = r_datum-low
     BISDATUM                       = r_datum-high
*   FLAG_UNGEPLANT                 = ' '
      MATNR                          = p_matnr
      PERIV                          = marc-periv
      PERKZ                          = marc-perkz
      WERKS                          = p_werks
   IMPORTING
     GESAMTVERBRAUCH                = l_cwgvbr
*   T_ABDATUM                      =
*   T_BISDATUM                     =
 EXCEPTIONS
   ABDATUM_GREATER_BISDATUM       = 1
   BISDATUM_IN_FUTURE             = 2
   CALENDAR_NOT_COMPLETE          = 3
   CONSUMPTION_NOT_FOUND          = 4
   FV_NOT_FOUND                   = 5
   FV_PERIOD_ERROR                = 6
   NO_MATERIAL                    = 7
   NO_PLANT                       = 8
   OTHERS                         = 9
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  p_cwgvbr = l_cwgvbr.


ENDFORM.                    " gesverbrauchswert


*&---------------------------------------------------------------------*
*&      Form  gesverbrauchswert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*      -->P_P_MATNR  text
*      <--P_L_CWGVBR  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM gesverbrauchswert USING p_werks
                             p_matnr
                    CHANGING p_mgvbr TYPE S031-MGVBR.
                                                 "smart: 2010-08-02 #105


  data: l_mgvbr like s031-mgvbr.

  clear p_mgvbr.


* Zugriff auf interne Tabelle it_s031
  read table it_s031 with key werks = p_werks
                              matnr = p_matnr.

  if sy-subrc eq 0.
    p_mgvbr = it_s031-mgvbr.
    exit.
  endif.


* Zugriff auf DB-Tabelle s031
  select sum( mgvbr ) from s031 into l_mgvbr
    where vrsio = '000' and
          spmon in r_spmon and
          werks = p_werks and
          matnr = p_matnr.

* Element in interne Tabelle it_s031 hinzufügen
  clear it_s031.
  move p_werks  to it_s031-werks.
  move p_matnr  to it_s031-matnr.
  move l_mgvbr to it_s031-mgvbr.
  append it_s031.

* Wert zurückgeben
  p_mgvbr = l_mgvbr.


ENDFORM.                    " gesverbrauchswert


*&---------------------------------------------------------------------*
*&      Form  bewerteter_bestand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WERKS  text
*      -->P_P_MATNR  text
*      <--P_L_WBWBES  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM bewerteter_bestand USING    P_werks
                                 p_matnr
                        CHANGING p_wbwbest.


  data: l_wbwbest like s032-wbwbest.

  clear: l_wbwbest, p_wbwbest.


* Zugriff auf interne Tabelle it_s032
  read table it_s032 with key werks = p_werks
                              matnr = p_matnr.

  if sy-subrc eq 0.
    p_wbwbest = it_s032-wbwbest.
    exit.
  endif.


* Zugriff auf DB-Tabelle S032
  select sum( wbwbest ) from s032 into l_wbwbest
    where vrsio = '000' and
          werks = p_werks and
          matnr = p_matnr.


* Element in interne Tabelle it_s032 hinzufügen
  clear it_s032.
  move p_werks   to it_s032-werks.
  move p_matnr   to it_s032-matnr.
  move l_wbwbest to it_s032-wbwbest.
  append it_s032.

* Wert zurückgeben
  p_wbwbest = l_wbwbest.


ENDFORM.                    " bewerteter_bestand



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
* Lagerort
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
  lt_fnames = text-t22.
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
* Artikelartencode
  lt_fnames = text-t10.
  append lt_fnames.
* Artikelartencodestext
  lt_fnames = text-t11.
  append lt_fnames.
* Reichweite
  lt_fnames = text-t12.
  append lt_fnames.
* aktueller Bestand
  lt_fnames = text-t13.
  append lt_fnames.
* Mengeneinheit
  lt_fnames = text-t14.
  append lt_fnames.
* Preissteurungscode
  lt_fnames = text-t15.
  append lt_fnames.
* Preis
  lt_fnames = text-t16.
  append lt_fnames.
* Preiseinheit
  lt_fnames = text-t17.
  append lt_fnames.
* Währungscode
  lt_fnames = text-t18.
  append lt_fnames.
* aktueller Wert
  lt_fnames = text-t19.
  append lt_fnames.
* Datum letzter Eingang
  lt_fnames = text-t20.
  append lt_fnames.
* Datum letzter Verbrauch
  lt_fnames = text-t21.
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
*&      Form  ausgabetabelle_aufbereiten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ausgabetabelle_aufbereiten.

  clear ot.

* Aktueller Bestand aus Tabelle mard ermitteln
  perform aktueller_bestand_aus_mard using it-matnr
                                           it-werks
                                  changing h-labst.

* Selektion Aktueller Bestand
  check h-labst in s_labst.


* Reichweite
  perform reichweite_ermitteln using it-werks
                                     it-matnr
                            changing h-sw_best_verb
                                     h-reichw.

  write h-reichw to ot-reichw.

* Falls kein Bestand und kein Verbrauch: nicht auf Liste ausgeben
  check h-sw_best_verb eq 'X'.

* Werk
  move it-werks to ot-werks.

* Lagerort
  move it-lgort to ot-lgort.

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

* Artikelartencode
  write mara-zzargisi to ot-zzargisi no-zero.

* Artikelartencode Text
  perform get_zzacisi using mara-zzargisi
                   changing ot-argtxt.

*** Reichweite
**  perform reichweite_ermitteln using it-werks
**                                     it-matnr
**                            changing ot-reichw.

* Aktueller Bestand aus Tabelle mard
  write h-labst to ot-labst decimals 2 no-grouping.

* Basismengeneinheit
  move mara-meins to ot-meins.

* Tabelle mbew lesen
  perform get_mbew using it-matnr
                         it-werks.

* Preissteuerungskennzeichen
  move mbew-vprsv to ot-vprsv.

* Preis, Einheit
  if mbew-vprsv = 'S'.
    move mbew-stprs to h-stprs.
    write mbew-stprs to ot-stprs decimals 2 no-grouping.  "Standardpreis
    move mbew-peinh to h-peinh.  "Preiseinheit
    write mbew-peinh to ot-peinh.  "Preiseinheit
  elseif mbew-vprsv = 'V'.
    move mbew-verpr to h-stprs.
*   Durchschnitts-/Verrechnungspreis
    write mbew-verpr to ot-stprs decimals 2 no-grouping..
    move mbew-peinh to h-peinh.  "Preiseinheit
    write mbew-peinh to ot-peinh.  "Preiseinheit
  endif.

* Währung via Werk->Bukrs bestimmen
  perform get_waers using it-werks
                 changing ot-waers.

* Wert des gesamten bewerteten Bestandes
  write mbew-salk3 to ot-salk3 decimals 3 no-grouping.
* Tabelle S032 lesen
  perform get_s032 using it-werks
                         it-lgort
                         it-matnr.

* Letzter Eingang
  write s032-letztzug to ot-letztzug DD/MM/YYYY no-zero.

* Letzter Verbrauch
  write s032-letztver to ot-letztver DD/MM/YYYY no-zero.

*---------------------------------------------------------------------*
* Element in Ausgabetabelle hinzufügen
*---------------------------------------------------------------------*
  append ot.


ENDFORM.                    " ausgabetabelle_aufbereiten
