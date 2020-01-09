*& Report  ZLE_INTERFACE_IQSOFT_MATERIAL
*&
*&
*&            Dätwyler AG
*&
*&
*&    Programm-Name....: ZLE_INTERFACE_IQSOFT_MATERIAL
*&    Entwickler.......: Markus Raffeiner
*&    Erstell-Datum....: 06.03.2009
*&    Version..........: 1.0
*&    Zweck............: Schnittstelle IQSoft: Materialstamm
*&
*&
*&    Input............: - mara
*&
*&    Output...........: - Textfile auf LINUX-Server im CSV-Format
*&                       - Protokollausgabe am Bildschirm
*&
*&    Regeln...........: -
*&
*&
*&    Bemerkung........: Wird aus der Transaktion SA38 oder SE38 ange-
*&                       stossen
*&
*&
*&    Aenderungen:
*&
*&    20.12.2009: Markus Raffeiner: Pfadanpassung auf Windows. Neuer
*&    Zugriff via Tabelle USER_DIR via Aliasname DIR_MODULE



************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105 #111
************************************************************************

REPORT ZL_INTERFACE_IQSOFT_MATERIAL .

tables: mara, makt, s001, ztsbt, user_dir.

select-options: s_mtart for mara-mtart,
                s_zzgb for mara-zzgb,
                s_zzpb for mara-zzpb.

selection-screen skip.
parameters: p_filout like rlgrap-filename obligatory
  default 'MATERIALSTAMM_IQS.CSV'.

constants: c_delim(1) value ';'.

data: path(100).
data: path2(20) value '/iqs/output/'.
*

* Ausgaberecord
data: begin of recout,
*       Materialnummer
        matnr like mara-matnr,
        fil_matnr,
*       Materialkurztext
        maktx like makt-maktx,
        fil_maktx,
*       Sortiment + Sortimentstext (mit Leerzeichen getrennt)
        sb_sbtxt(43),
        fil_sb_sbtxt,
*       Werkzeug-Nr.
        normt like mara-normt,
        fil_normt,
*       Dokumentart (ohne Dokumentenverwaltungssystem)
        zeinr like mara-zeinr,
        fil_zeinr,
*       Werkstoff
        wrkst like mara-wrkst,
        fil_wrkst,
      end of recout.




* Allgemeine Materialdaten
data: begin of itab_mara occurs 0,
        matnr like mara-matnr,
        zeinr like mara-zeinr,
        wrkst like mara-wrkst,
        normt like mara-normt,
        zzsb  like mara-zzsb,
      end of itab_mara.

* VIS: Kundenstatistik
data: begin of itab_s001 occurs 0,
        matnr like s001-matnr,
      end of itab_s001.

* Hilfsfelder
data: begin of h,
        dsn(170),
        subrc like sy-subrc,
      end of h.


data: begin of z,
        anz_sel_mara type i,
        anz_output type i,
        anz_elem type i,
        anz_write type i,
      end of z.



*---------------------------------------------------------------------
* Programmstart
*---------------------------------------------------------------------
start-of-selection.

* Vorlauf
  perform vorlauf.


  select matnr zeinr wrkst normt zzsb from mara into table itab_mara
    where lvorm = space and
          mtart in s_mtart and
          mstae = space and
          zzgb in s_zzgb and
          zzpb in s_zzpb.

* Records vorhanden?
  check sy-dbcnt gt 0.
  z-anz_sel_mara = sy-dbcnt.

* Load Statistiktabelle S001. Diese Tabelle wird benötigt, um
* festzustellen, ob ein Material irgendeine Bewegung (AE, UM usw.) hat
  perform load_itab_s001.

* Loop über interne Tabelle itab_mara
  loop at itab_mara.

* Test ob Record irgendeine Bewegung hatte (h-subrc = 0)
    perform test_record_verarbeiten using itab_mara-matnr
                                    changing h-subrc.


    check h-subrc eq 0.

* Tabelle MAKT (Materialkurztexte) lesen
    perform read_makt using itab_mara-matnr.

* Tabelle ZTSBT (Sortiment (Texte)) lesen
    perform read_ztsbt using itab_mara-zzsb.

* Output aufbereiten
    perform aufbereiten_outputrecord.

* Output auf Linux schreiben
    perform transfer_outputrecord.

    z-anz_write = z-anz_write + 1.

  endloop.


*---------------------------------------------------------------------
* Programmende
*---------------------------------------------------------------------
end-of-selection.

  perform nachlauf.


*&---------------------------------------------------------------------*
*&      Form  vorlauf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vorlauf.

* Pfad 'DIR_MODULE' ermitteln
  perform get_path using 'DIR_MODULE'
                 changing path.


* Verzeichnis und Filname zusammensetzen
  concatenate path path2 p_filout into h-dsn IN  "smart: 2010-08-02 #101
    CHARACTER MODE .                             "smart: 2010-08-02 #101

* Outputfile im Schreibmodus öffnen

*$smart (F) 2010-08-02 - #111 OPEN DATASET benötigt mode und encoding
*$smart (F) 2010-08-02 - #111 Spezifikation Adding encoding to OPEN
*$smart (F) 2010-08-02 - #111 DATASET statement (A)

  open dataset h-dsn for output in text mode     "smart: 2010-08-02 #111
    ENCODING UTF-8 .                             "smart: 2010-08-02 #111


ENDFORM.                    " vorlauf



*&---------------------------------------------------------------------*
*&      Form  load_itab_s001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_itab_s001.

  select distinct matnr from s001 into table itab_s001
    where vrsio = '000'.

ENDFORM.                    " load_itab_s001


*&---------------------------------------------------------------------*
*&      Form  test_record_verarbeiten
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_H_SUBRC  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM test_record_verarbeiten USING p_matnr TYPE  "smart: 2010-08-02 #105
  MARA-MATNR                                     "smart: 2010-08-02 #105
                             CHANGING p_subrc    "smart: 2010-08-02 #105
                               TYPE SYST-SUBRC.  "smart: 2010-08-02 #105

  read table itab_s001 with key matnr = p_matnr.

  p_subrc = sy-subrc.

ENDFORM.                    " test_record_verarbeiten



*&---------------------------------------------------------------------*
*&      Form  read_makt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_MARA_MATNR  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM read_makt USING    p_matnr TYPE MARA-MATNR. "smart: 2010-08-02 #105

  clear makt.

  select single * from makt where matnr = p_matnr and
                                  spras = sy-langu.

ENDFORM.                    " read_makt


*&---------------------------------------------------------------------*
*&      Form  read_ztsbt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_MARA_ZZSB  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM read_ztsbt USING    p_zzsb TYPE MARA-ZZSB.  "smart: 2010-08-02 #105

  clear ztsbt.

  select single * from ztsbt where spras = 'DE' and
                                   zzsb = p_zzsb.


ENDFORM.                    " read_ztsbt



*&---------------------------------------------------------------------*
*&      Form  aufbereiten_outputrecord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM aufbereiten_outputrecord.

  clear recout.

  perform trennzeichen.

  write itab_mara-matnr to recout-matnr no-zero.
  move makt-maktx      to recout-maktx.
  concatenate itab_mara-zzsb ztsbt-vtext into recout-sb_sbtxt
  separated by space IN CHARACTER MODE .         "smart: 2010-08-02 #101
  move itab_mara-normt to recout-normt.
  move itab_mara-zeinr to recout-zeinr.
  move itab_mara-wrkst to recout-wrkst.


ENDFORM.                    " aufbereiten_outputrecord


*&---------------------------------------------------------------------*
*&      Form  trennzeichen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trennzeichen.

  move c_delim to recout-fil_matnr.
  move c_delim to recout-fil_maktx.
  move c_delim to recout-fil_sb_sbtxt.
  move c_delim to recout-fil_normt.
  move c_delim to recout-fil_zeinr.
  move c_delim to recout-fil_wrkst.


ENDFORM.                    " trennzeichen


*&---------------------------------------------------------------------*
*&      Form  transfer_outputrecord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_outputrecord.

  transfer recout to h-dsn.
  write / recout.

ENDFORM.                    " transfer_outputrecord



*&---------------------------------------------------------------------*
*&      Form  nachlauf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nachlauf.

  skip 2.

  write: / 'Ausgabestatistik', sy-mandt, sy-sysid, sy-datum, sy-uzeit.
  skip.
  write: / 'Anzahl Records in Tabelle MARA gelesen:', 40 z-anz_sel_mara.
  write: / 'Anzahl Records geschrieben:', 40 z-anz_write.



  skip.
  write: / 'Outputfile: ' , 40 h-dsn.


ENDFORM.                    " nachlauf




*---------------------------------------------------------------------
*       Pfad 'DIR_MODULE' ermitteln
*---------------------------------------------------------------------
include zincl_getpath.
