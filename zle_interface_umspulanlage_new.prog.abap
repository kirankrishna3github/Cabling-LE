*&---------------------------------------------------------------------*
*& Report  ZLE_INTERFACE_UMSPULANLAGE
*&
*&            Dätwyler AG
*&
*&
*&    Programm-Name....: ZLE_INTERFACE_UMSPULANLAGE
*&    Entwickler.......: Markus Raffeiner
*&    Erstell-Datum....: 15.12.2010
*&    Version..........: 1.0
*&    Zweck............: Schnittstelle Materialdaten SAP zu Umspulanlage
*&
*&    Input............: Tabellen: afko, afpo, mara
*&
*&    Output...........: - Textfile im Verzeichnis /mm/output
*&
*&    Regeln...........: - Bei der Status-Selektion "exklusiv" (s_staex) werden
*&                         die Stati überlesen, welche in diesem Selektionsfeld
*&                         gewählt werden.
*&                       - Das Ausgabefile wird mit einem Zeitstempel yyyymmddhhmm
*&                         aufbereitet, welcher zur Laufzeit aus dem aktuellen
*&                         Datum und der aktuellen Zeit ermittelt wird
*&
*&
*&    Bemerkung........: Wird aus der Transaktion SA38 oder SE38 ange-
*&                       stossen
*&
*&---------------------------------------------------------------------*
*&    Aenderungen:
*&
*&    05.06.2012: Zusatzselektion nach Produktehierarchie (mara)
*&    22.04.2016: Ergänzung der Struktur für CE/CPR-Label
*&---------------------------------------------------------------------*

REPORT  ZLE_INTERFACE_UMSPULANLAGE_NEW.

tables: afko, afpo, jest, tj02t, mara.

select-options: s_aufnr for afko-aufnr.
select-options: s_matnr for afpo-matnr.
select-options: s_zzsb  for mara-zzsb.
select-options: s_dauat for afpo-dauat default 'ZP01'.
select-options: s_staex for tj02t-txt04 obligatory no intervals default 'TABG'.
parameters: p_sysh1   LIKE jest-stat                    NO-DISPLAY.
parameters: p_bsprot as checkbox.

selection-screen skip.
parameters: p_fout(70) lower case default 'sap-umspul-yyyymmddhhmm.csv'.


* Felder für Verzeichnis und Filname
data: path(100).
data: path2(100) value '/mm/output/'.

* Filename
data: fname(100).

data: gs_labeling type ztmm_labeling.

*-----------------------------------------------------------------------
* Schnittstellen-Record von Umspulanlage
*-----------------------------------------------------------------------
data: begin of reco,
        aufnr like afko-aufnr,  "Auftragsnummer
        fil_aufnr(1),
        posnr like afpo-posnr,  "Nummer der Auftragsposition
        fil_posnr(1),
        matnr like afpo-matnr,  "Materialnummer
        fil_matnr(1),
        brgew(13),              "Bruttogewicht
        fil_brgew(1),
        gewei like mara-gewei,  "Gewichtseinheit
        fil_gewei(1),
        tyen(30),               "Merkmal Typ extern national (ZDAG_TYEN)
        fil_tyen(1),
        typi(30),               "Merkmal Typ intern (ZDAG_TYPI)
        fil_typi(1),
        mf(30),                 "Merkmal Mantelfarbe (ZDAG_MF)
        fil_mf(1),
        ral(30),                "Merkmal Farbton (ZDAG_RAL)
        fil_ral(1),

* ---------------------------------------------------------------------
* Ab hier: Struktur für CE/CPR-Label (Siehe Include ZINCL_ETIKETTE_001)
* ---------------------------------------------------------------------
*       Notifizierte Stelle
        notib           type zznotib,
        fil_notib(1),

*       Adressdaten Lieferwerk

*       Name
        name1           type name1,
        fil_name1(1),
*       Strasse und Hausnummer
        stras           type stras,
        fil_stras(1),
*       Länderschlüssel (3)
        land1           type land1,
        fil_land1(1),
*       Bezeichnung des Landes (15)
        landx           type landx,
        fil_landx(1),
*       Postleitzahl
        pstlz           type pstlz,
        fil_pstlz(1),
*       Ort
        ort01           type ort01,
        fil_ort01(1),
*       Deklaration Nummer
        decln           type zzdecln,
        fil_decln(1),
*       Harmonisierte Norm + ' : Jahr'
        hastd           type char23,
        fil_hastd(1),
*       Kodierung des Produktetyps
        encpt           type zzencpt,
        fil_encpt(1),
*       Langtext Zeile 1 Englisch
        text1_en        type tdline,
        fil_text1_en(1),
*       Langtext Zeile 2 Englisch
        text2_en        type tdline,
        fil_text2_en(1),
*       Langtext Zeile 1 Deutsch
        text1_de        type tdline,
        fil_text1_de(1),
*       Langtext Zeile 2 Deutsch
        text2_de        type tdline,
        fil_text2_de(1),
*       Brandverhalten
        reafi           type zzreafi,
        fil_reafi(1),
*       Gefährliche Stoffe
        dngsu           type zzdngsu,
        fil_dngsu(1),
*       Datum Klassifizierung
        kldat(10),
        fil_kldat(1),
*       Kennzeichen CE-Code drucken
        ceprint(01)     TYPE c,
        fil_ceprint(1)  VALUE '|',
*       CE-Etikettierung
        prat4(01)     TYPE c,
        fil_prat4(1)  VALUE '|',
      end of reco.

*-----------------------------------------------------------------------
* Inputtabelle für Tabellenjoin afko, afpo
*-----------------------------------------------------------------------
types: begin of t_it,
         aufnr like afko-aufnr,
         ftrms like afko-ftrms,
         posnr like afpo-posnr,
         matnr like afpo-matnr,
         pwerk like afpo-pwerk,
       end of t_it.

data: it type table of t_it.

data: wa_it type t_it.


*-----------------------------------------------------------------------
* Definitionen für Merkmale
*-----------------------------------------------------------------------
DATA  BEGIN OF g_t_sclass OCCURS 1.
        INCLUDE STRUCTURE sclass.
DATA  END   OF g_t_sclass.

DATA  BEGIN OF g_t_clobjdat OCCURS 1.
        INCLUDE STRUCTURE clobjdat.
DATA  END   OF g_t_clobjdat.
DATA: g_objec LIKE ausp-objek.


*-----------------------------------------------------------------------
* Selektionsrange für Status
*-----------------------------------------------------------------------
ranges: r_istat for tj02t-istat.

data: begin of it_tj02t occurs 0,
        istat like tj02t-istat,
        txt04 like tj02t-txt04,
      end of it_tj02t.

*-----------------------------------------------------------------------
* Tabelle für Labeltext
data: gs_lines type tline.
data: gt_labtext type standard table of tline.
*-----------------------------------------------------------------------


*-----------------------------------------------------------------------
* Tabelle für Protokoll
*-----------------------------------------------------------------------
data: begin of ot_prot occurs 0,
        aufnr like afko-aufnr,
        text(20),
      end of ot_prot.

*-----------------------------------------------------------------------
* Diverse Hilfsfelder
*-----------------------------------------------------------------------
data: z_anz_select type i.
data: z_anz_write type i.
data: h_sw_sel.
data: gv_subrc type sysubrc.
data: gs_adrc type adrc.
data: gs_t001w type t001w.

*-----------------------------------------------------------------------
* Konstanten
*-----------------------------------------------------------------------
constants: c_delim(1) value ';'.
constants: c_on(1) value 'X'.
constants: c_spras_de type spras value 'D'.
constants: c_spras_en type spras value 'E'.
constants: c_tdid_zlab type tdid value 'ZLAB'.
constants: c_object_material type tdobject value 'MATERIAL'.



*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_staex-low.
  PERFORM val_req_status USING 'S_STAEX-LOW' CHANGING s_staex-low p_sysh1.


*-----------------------------------------------------------------------
* Programmstart
*-----------------------------------------------------------------------
start-of-selection.

* Vorlaufroutinen
  perform vorlauf.


* Join: afko, afpo
  select afko~aufnr afko~ftrms afpo~posnr afpo~matnr afpo~pwerk
         into table it
         from ( ( afko
           inner join afpo
           on   afko~aufnr = afpo~aufnr )
           inner join mara
           on   afpo~matnr = mara~matnr )
         where afko~aufnr in s_aufnr and
               afpo~matnr in s_matnr and
               mara~zzsb in s_zzsb and
               dauat in s_dauat.


* Ermitteln Anzahl Materialien gelesen
  describe table it lines z_anz_select.


* Loop über Auftragstabelle
  loop at it into wa_it.

*   Exklusivselektion im Feld s_staex:
*   Alle Stati im Selektionsfeld "s_staex" werden nicht genommen
    perform selektion_status_exklusiv changing h_sw_sel.

    if h_sw_sel ne c_on and p_bsprot eq c_on.
      perform protokoll_aufbereiten using wa_it-aufnr
                                          text-p01.
    endif.

*   Record selektieren
    check h_sw_sel eq c_on.

*   Outputrecord mit Trennzeichen initialisieren
    perform init_reco.

*   Auftragsnummer
    move wa_it-aufnr to reco-aufnr.

*   Auftragsposition
    move wa_it-posnr to reco-posnr.

*   Materialnummer
    move wa_it-matnr to reco-matnr.

*   Bruttogewicht, Gewichtseinheit aus Tabelle mara holen
    perform get_bruttogewicht_aus_mara using wa_it-matnr
                                    changing reco-brgew
                                             reco-gewei.

*   Merkmalstabelle via Material laden
    perform load_merkmaltabelle using wa_it-matnr.

*   Merkmal "Typ extern national" holen
    perform get_merkmal using 'ZDAG_TYEN'
                     changing reco-tyen.
*   Merkmal "Typ intern" holen
    perform get_merkmal using 'ZDAG_TYPI'
                     changing reco-typi.
*   Merkmal "Mantelfarbe" holen
    perform get_merkmal using 'ZDAG_MF'
                     changing reco-mf.
*   Merkmal "Farbton" holen
    perform get_merkmal using 'ZDAG_RAL'
                     changing reco-ral.

*   Zusatzdaten für DoP und CE/CPR-Etiketten
    perform get_data_cecpr changing gs_labeling
                                    gv_subrc.

    if gv_subrc eq 0.
      move gs_labeling-notib    to reco-notib.
      move gs_labeling-name1    to reco-name1.
      move gs_labeling-stras    to reco-stras.
      move gs_labeling-land1    to reco-land1.

*     Landbezeichnung englisch
      select single landx from t005t into reco-landx
        where spras = c_spras_en and
              land1 = gs_adrc-country.

      move gs_labeling-pstlz    to reco-pstlz.
      move gs_labeling-ort01    to reco-ort01.
      move gs_labeling-decln    to reco-decln.
      move gs_labeling-hastd    to reco-hastd.
      move gs_labeling-encpt    to reco-encpt.

*     Texttabelle gt_labtext mit englischem Text laden
      perform load_tabelle_labeltexte using c_spras_en.

*     Langtext Zeile 1 Englisch
      perform get_labelzeile using 1
                          changing reco-text1_en.

*     Langtext Zeile 2 Englisch
      perform get_labelzeile using 2
                          changing reco-text2_en.

*     Texttabelle gt_labtext mit deutschem Text laden
      perform load_tabelle_labeltexte using c_spras_de.

*     Langtext Zeile 1 Deutsch
      perform get_labelzeile using 1
                          changing reco-text1_de.

*     Langtext Zeile 2 Deutsch
      perform get_labelzeile using 2
                          changing reco-text2_de.

      move gs_labeling-reafi    to reco-reafi.
      move gs_labeling-dngsu    to reco-dngsu.
      move gs_labeling-kldat    to reco-kldat.
      move gs_labeling-ceprint  to reco-ceprint.

      select single prat4 from mvke into reco-prat4
        where matnr = wa_it-matnr and
              vkorg = gs_t001w-vkorg and
              prat4 = 'X'.

    endif.  "gv_subrc

*   Outputrecord auf Filesystem schreiben
    perform outputrecord_schreiben.

  endloop.


*-----------------------------------------------------------------------
* Programmende
*-----------------------------------------------------------------------
end-of-selection.


* Nachlaufroutinen
  perform nachlauf.


*&---------------------------------------------------------------------*
*&      Form  VORLAUF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VORLAUF .

  data: l_istat like tj02-istat.

* Pfad 'DIR_MODULE' ermitteln
  perform get_path using 'DIR_MODULE'
                 changing path.

* Verzeichnis und Filname zusammensetzen
  concatenate path path2 p_fout into fname in character mode.

* Zeitstempel setzen
  replace 'yyyymmdd' with sy-datum into fname.
  replace 'hhmm' with sy-uzeit(4) into fname.

* File im Non-Unicode Modus öffnen
  open dataset fname in text mode FOR OUTPUT encoding non-unicode.

* Fehler beim Öffen der Datei
  if sy-subrc ne 0.
    message E500(26) with text-f02 fname.
  endif.


* Selektionsrange r_istat aus der Tabelle tj02t aus dem Selektionsfeld s_staex laden:
* Dieser Range wird für die Selektion 's_staex' benötigt, da in s_staex der Text
* (z.B. TABG) eingegeben wird, und in der Tabelle jest der Einzelstatus (z.B. I0045)
* benötigt wird.
  select istat from tj02t into l_istat
    where spras = sy-langu and
          txt04 in s_staex.

    clear r_istat.
    move 'I'     to r_istat-sign.
    move 'EQ'    to r_istat-option.
    move l_istat to r_istat-low.
    append r_istat.

  endselect.


ENDFORM.                    " VORLAUF


*&---------------------------------------------------------------------*
*&      Form  SELEKTION_STATUS_EXKLUSIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_H_SW_STAT_EXKL  text
*----------------------------------------------------------------------*
FORM SELEKTION_STATUS_EXKLUSIV  CHANGING p_rec_sel.

  data: l_objnr like jest-objnr.

  clear p_rec_sel.


* Zusammensetzen der Objektnummer: 'OR' (=Order) + Auftragsnummer
* (siehe Include COFORM00)
  concatenate 'OR' wa_it-aufnr into l_objnr.

* Falls irgendein Status in der Tabelle jest vorhanden ist, wird
* der Datensatz nicht selektiert, da die Selektion s_exkl "exklusiv"
* bedeutet
  select single * from jest
    where objnr = l_objnr and
***          stat in s_staex.
          stat in r_istat and
          inact ne c_on.  "Status aktiv

* Status nicht vorhanden: Record weiterverarbeiten
  if sy-subrc ne 0.
    p_rec_sel = c_on.
  endif.

ENDFORM.                    " SELEKTION_STATUS_EXKLUSIV


*&---------------------------------------------------------------------*
*&      Form  INIT_RECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_RECO .

  clear reco.

* Trennzeichen setzen
  move c_delim to reco-fil_aufnr.
  move c_delim to reco-fil_posnr.
  move c_delim to reco-fil_matnr.
  move c_delim to reco-fil_brgew.
  move c_delim to reco-fil_gewei.
  move c_delim to reco-fil_tyen.
  move c_delim to reco-fil_typi.
  move c_delim to reco-fil_mf.
  move c_delim to reco-fil_ral.

  move c_delim to reco-fil_notib.
  move c_delim to reco-fil_name1.
  move c_delim to reco-fil_stras.
  move c_delim to reco-fil_land1.
  move c_delim to reco-fil_landx.
  move c_delim to reco-fil_pstlz.
  move c_delim to reco-fil_ort01.
  move c_delim to reco-fil_decln.
  move c_delim to reco-fil_hastd.
  move c_delim to reco-fil_encpt.
  move c_delim to reco-fil_text1_en.
  move c_delim to reco-fil_text2_en.
  move c_delim to reco-fil_text1_de.
  move c_delim to reco-fil_text2_de.
  move c_delim to reco-fil_reafi.
  move c_delim to reco-fil_dngsu.
  move c_delim to reco-fil_kldat.
  move c_delim to reco-fil_ceprint.
  move c_delim to reco-fil_prat4.


ENDFORM.                    " INIT_RECO


*&---------------------------------------------------------------------*
*&      Form  GET_BRUTTOGEWICHT_AUS_MARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_IT_MATNR  text
*      <--P_RECO_BRGEW  text
*      <--P_RECO_GEWEI  text
*----------------------------------------------------------------------*
FORM GET_BRUTTOGEWICHT_AUS_MARA  USING    p_matnr
                                 CHANGING p_brgew
                                          p_gewei.

  data: begin of l_wa,
          brgew like mara-brgew,
          gewei like mara-gewei,
        end of l_wa.

  clear: p_brgew, p_gewei.

  select single brgew gewei from mara into l_wa
    where matnr = p_matnr.

  if sy-subrc = 0.
    write l_wa-brgew to p_brgew decimals 3.
    move l_wa-gewei to p_gewei.
  endif.


ENDFORM.                    " GET_BRUTTOGEWICHT_AUS_MARA


*&---------------------------------------------------------------------*
*&      Form  LOAD_MERKMALTABELLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_IT_MATNR  text
*----------------------------------------------------------------------*
FORM LOAD_MERKMALTABELLE  USING    p_matnr.

  g_objec = p_matnr.

  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
      classtext          = 'X'
      classtype          = '001'
      object             = g_objec
      no_value_descript  = 'X'
    TABLES
      t_class            = g_t_sclass
      t_objectdata       = g_t_clobjdat
    EXCEPTIONS
      no_classification  = 1
      no_classtypes      = 2
      invalid_class_type = 3
      OTHERS             = 4.


ENDFORM.                    " LOAD_MERKMALTABELLE


*&---------------------------------------------------------------------*
*&      Form  GET_MERKMAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0180   text
*      <--P_RECO_TYEN  text
*----------------------------------------------------------------------*
FORM GET_MERKMAL  USING    p_atnam
                  CHANGING p_ausp1.

  clear p_ausp1.

  read table g_t_clobjdat with key atnam = p_atnam.

  if sy-subrc = 0.
    move g_t_clobjdat-ausp1 to p_ausp1.
  endif.

ENDFORM.                    " GET_MERKMAL


*&---------------------------------------------------------------------*
*&      Form  OUTPUTRECORD_SCHREIBEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM OUTPUTRECORD_SCHREIBEN .

  transfer reco to fname.

  z_anz_write = z_anz_write + 1.

  if p_bsprot = c_on.
    perform protokoll_aufbereiten using reco-aufnr
                                        text-p02.
  endif.

ENDFORM.                    " OUTPUTRECORD_SCHREIBEN



*&---------------------------------------------------------------------*
*&      Form  PROTOKOLL_AUF_BILDSCHIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_IT_AUFNR  text
*      -->P_TEXT_P01  text
*      -->P_WRITE  text
*      -->P_/  text
*      -->P_WA_IT_AUFNR  text
*----------------------------------------------------------------------*
FORM PROTOKOLL_AUFBEREITEN USING    p_aufnr
                                    P_text.

  clear ot_prot.
  move p_aufnr to ot_prot-aufnr.
  move p_text  to ot_prot-text.
  append ot_prot.


ENDFORM.                    " PROTOKOLL_AUF_BILDSCHIRM




*---------------------------------------------------------------------
*       Pfad 'DIR_MODULE' ermitteln
*---------------------------------------------------------------------
include zincl_getpath.


*&---------------------------------------------------------------------*
*&      Form  NACHLAUF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NACHLAUF.

* File  schliesen
  close dataset fname.

* Protokoll ausgeben
  skip 2.

  write: / 'Ausgabestatistik', sy-datum, sy-uzeit, sy-mandt, sy-sysid.
  skip 1.

  write: / 'Anzahl Aufträge gelesen:       ',
            z_anz_select.
  write: / 'Anzahl Aufträge geschrieben:   ',
            z_anz_write.

  write: / 'Ausgabedatei:', fname.

* Protokollausgabe: Selektiert/nicht selektierte Aufträge
  if p_bsprot eq c_on.
    skip 2.
    loop at ot_prot.
      write: / ot_prot-aufnr, ot_prot-text.
    endloop.
  endif.

ENDFORM.                    " NACHLAUF

*&---------------------------------------------------------------------*
*&      Form  val_req_status
*&---------------------------------------------------------------------*
*       F4-Help to select system status by text, providing also code
*----------------------------------------------------------------------*
*      -->I_DYNPF    Name of dynpro field
*      -->C_TXT04    Status short text
*      -->C_STAT     Status code
*----------------------------------------------------------------------*
FORM val_req_status USING    i_dynpf
                    CHANGING c_txt04 LIKE tj02t-txt04
                             c_stat  LIKE jest-stat.

  TYPES: BEGIN OF ty_value,
           istat    TYPE j_istat,
           txt04    TYPE j_txt04,
           txt30    TYPE j_txt30,
         END OF ty_value.

  DATA: l_value    TYPE dynfieldvalue,
        l_repid    TYPE sy-repid,
        ls_dynp    TYPE dynpread,
        ls_tj02    TYPE tj02,
        ls_value   TYPE tj02t,
        ls_return  TYPE ddshretval,
        lt_dynp    TYPE STANDARD TABLE OF dynpread,
        lt_tj02    TYPE STANDARD TABLE OF tj02,
        lt_value   TYPE STANDARD TABLE OF tj02t,
        lt_return  TYPE STANDARD TABLE OF ddshretval.

* personal value key
  DATA: l_pvalkey TYPE ddshpvkey.

* Get current value from screen
  ls_dynp-fieldname = i_dynpf.
  APPEND ls_dynp TO lt_dynp.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynp
    EXCEPTIONS
      OTHERS     = 0.
  READ TABLE lt_dynp INTO ls_dynp INDEX 1.

* Get all possible values
  SELECT * FROM tj02 INTO TABLE lt_tj02
                     WHERE nodis = space.
  SELECT * FROM tj02t INTO TABLE lt_value
                      FOR ALL ENTRIES IN lt_tj02
                      WHERE istat = lt_tj02-istat
                        AND spras = sy-langu.
  IF LINES( lt_tj02 ) <> LINES( lt_value ).
*   Not all status available in current language, try in german
*   as in FORM value_state(saplcoss)
    LOOP AT lt_tj02 INTO ls_tj02.
      READ TABLE lt_value INTO ls_value WITH KEY istat = ls_tj02-istat.
      CHECK sy-subrc <> 0.
      SELECT SINGLE * FROM tj02t INTO ls_value
                          WHERE istat = ls_tj02-istat
                            AND spras = 'D'.
      IF sy-subrc = 0.
        APPEND ls_value TO lt_value.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT lt_value BY istat.

* Call own F4-help, not showing internal status number
  l_value = ls_dynp-fieldvalue.
  l_repid = sy-repid.
* fill key for personal value list
  l_pvalkey = sy-uname.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      ddic_structure   = 'TJ02T'
      retfield         = 'TXT04'
      pvalkey          = l_pvalkey
      value            = l_value
      value_org        = 'S'
      callback_program = l_repid
    TABLES
      value_tab        = lt_value
      return_tab       = lt_return.

* Get selectect value and internal status number
  READ TABLE lt_return INTO ls_return index 1.

  IF sy-subrc = 0.
    READ TABLE lt_value WITH KEY txt04 = ls_return-fieldval
                        INTO ls_value.
    CHECK sy-subrc = 0.
    c_txt04 = ls_value-txt04.
    c_stat  = ls_value-istat.
  ELSE.
    CLEAR: c_txt04, c_stat.
  ENDIF.

ENDFORM.                    "val_req_status

*&---------------------------------------------------------------------*
*&      Form  GET_DATA_CECPR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_LABELING  text
*----------------------------------------------------------------------*
FORM GET_DATA_CECPR  CHANGING PS_LABELING STRUCTURE ztmm_labeling
                              PV_SUBRC TYPE sysubrc.

  clear: ps_labeling, gs_adrc, gs_t001w.

  select single * from ztmm_labeling into ps_labeling
    where matnr = wa_it-matnr and
          werks = wa_it-pwerk and
          datab le wa_it-ftrms and
          datbi ge wa_it-ftrms and
          actsg = c_on. "Nur aktiven Satz selektieren

  pv_subrc = sy-subrc.

  check sy-subrc eq 0.

* Werksdaten
  select single * from t001w into gs_t001w
    where werks = wa_it-pwerk.

* Adressdaten
  if not ps_labeling-name1 is initial.  "OEM-Adresse
    move ps_labeling-name1 to gs_adrc-name1.
    move ps_labeling-stras to gs_adrc-street.
    move ps_labeling-land1 to gs_adrc-country.
    move ps_labeling-pstlz to gs_adrc-post_code1.
    move ps_labeling-ort01 to gs_adrc-city1.
  else. "Werksadresse
    perform get_absender.
  endif.

ENDFORM.                    " GET_DATA_CECPR

*&---------------------------------------------------------------------*
*&      Form  GET_ABSENDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ABSENDER .

  data: lv_adrnr type adrnr.

  clear lv_adrnr.
  clear gs_adrc.

* Adresse via Versandstelle ermitteln

* Adressnummer via Werk ermitteln
***  select single adrnr from t001w into lv_adrnr
***    where werks = wa_it-pwerk.
***
***  check sy-subrc eq 0.

* Absenderadresse
  select single * from adrc into gs_adrc
    where addrnumber = gs_t001w-adrnr.

ENDFORM.                    " GET_ABSENDER

*&---------------------------------------------------------------------*
*&      Form  LOAD_TABELLE_LABELTEXTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOAD_TABELLE_LABELTEXTE using p_spras type spras.

  data: ls_thead type thead.
  data: lv_spras type spras.

  data: lt_texttab type standard table of tline.
  data: ls_texttab type tline.

  clear gt_labtext.
  clear ls_thead.

  ls_thead-tdspras = p_spras.
  ls_thead-tdname = wa_it-matnr.
  ls_thead-tdobject = c_object_material.
  ls_thead-tdid = c_tdid_zlab.

* Labeltext laden
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                           = ls_thead-tdid
      language                     = ls_thead-tdspras
      name                         = ls_thead-tdname
      object                       = ls_thead-tdobject
    IMPORTING
     header                        = ls_thead
    TABLES
      lines                        = gt_labtext
    EXCEPTIONS
      id                           = 1
      language                     = 2
      name                         = 3
   not_found                       = 4
   object                          = 5
   reference_check                 = 6
    wrong_access_to_archive        = 7
  OTHERS                           = 8.

ENDFORM.                    " LOAD_TABELLE_LABELTEXTE

*&---------------------------------------------------------------------*
*&      Form  GET_LABELZEILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      <--P_ze001_TEXT1_EN  text
*----------------------------------------------------------------------*
FORM GET_LABELZEILE  USING    p_zeilnr type i
                     CHANGING ps_zeile type tdline.

  clear: ps_zeile, gs_lines.

*  read table gt_labtext into gs_lines index p_zeilnr.
  read table gt_labtext into gs_lines index p_zeilnr.

  if sy-subrc eq 0.
    move gs_lines-tdline to ps_zeile.
  endif.

ENDFORM.                    " GET_LABELZEILE
