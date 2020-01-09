
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #105 #111 #115 #142
************************************************************************

report: Z200_MM_BESTAND_IN_01.


************************************************************************
*MM Bestände, welche in einer EXCEL Datei bereitgestellt werden,
*übernehmen.
************************************************************************
*Vorgehen:

*Schritt: 1
*Bestände im Format itab bereitstellen
*
*Schritt: 2
*Vorbereitung:
*Logische Datei anlegen (ZBESTAND)
*Transaktion: FILE
*log. Datei      ZBESTAND_IN
*Bezeichnung     Schnittstelle für Materialbelegbearb
*phys. Datei     \\ERPR3T77\sapmnt\trans\ZBESTAND_IN
*Datenformat     ASC
*Arbeitsgebiet   MM

*Schritt: 3
*Report Z709_BESTAND_KONSI_UEB ausführen und Daten in ZBESTAND sichern

*Prüfen der Datei
*Transaktion:   FXDA
*Objekttyp:     BUS2017
*Programmtyp:   BINP
*Programm:      RM07MMBL
*Datei anzeigen und Felder beurteilen

*Schritt: 5
*Report RM07MMBL ausführen und Batch-Input Mappe anlegen
************************************************************************
*Aenderungen:
*06122006; Report für Bestandesübernahme DRC angepasst
************************************************************************

*Tabellen:
tables: mara, marc, mard, mbew, mlgn.  "zum Materialstamm
tables: lagp, ltap, apqi, ausp, inob.
tables: bmseg.

************************************************************************
*Felder
data: dataset(70).
data: count type p,
      xfilenew like rlgrap-filename,
      xmess(100),
      count_1 type p,
      xmatnr like mara-matnr,
      xmenge type p decimals 3,
      wmatnr(8),
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE,
      wgroup like APQI-GROUPID,
      g_objek LIKE ausp-objek.

*Bestände auf Plätzen
data: begin of inew occurs 0,
      LGPLA(10),
      MATNR(8),
      MENGE(13),
      CHARG(10),
      VFDAT(10),     "Verfalldatum
end of inew.


DATA  BEGIN OF g_t_sclass OCCURS 1.
        INCLUDE STRUCTURE sclass.
DATA  END   OF g_t_sclass.

DATA  BEGIN OF g_t_clobjdat OCCURS 1.
        INCLUDE STRUCTURE clobjdat.
DATA  END   OF g_t_clobjdat.

**Neue Tabelle für collect
*data: begin of inewc occurs 0,
*      matnr(8),
*      gesme like bmseg-erfmg,
*end of inewc.
*
*
**alte Tabelle für collect
*data: begin of ioldc occurs 0,
*      matnr(8),
*      gesme like bmseg-erfmg,
*end of ioldc.

*Materialbewertung
data:begin of imbew occurs 0.
        include structure mbew.
data: end of imbew.

**Lagerplatzkonvertierung
*data:begin of ilgpla occurs 0,
*     old(10),
*     new(10),
*end of ilgpla.

************************************************************************
*Benutzeroberfläche
parameters: dsninew like rlgrap-filename lower case
               default 'C:\Bestand_DRC.txt' obligatory.
parameter: p_bwkey like mbew-bwkey obligatory default '8000',
           p_werks like mard-werks obligatory default '8000',
           p_lgort like mard-lgort obligatory default '8000',
           p_lgnum like mlgn-lgnum obligatory default '800',
           p_nltyp like ltap-nltyp obligatory default '001',
           p_letyp like ltap-letyp obligatory default '001'.
*parameters: dsniold like rlgrap-filename lower case
*               default 'H:\Bestand_out.txt'
*               obligatory.

*parameters: dsnilp like rlgrap-filename lower case
*default 'H:\LGPLA_KONVERT.TXT'
*obligatory.

parameters: dsno(60) lower case
                default '\\ERPR3T77\sapmnt\trans\ZBESTAND_IN.DAT'.

parameters: p_datum like sy-datum default sy-datum.
selection-screen skip 1.
parameters: test as checkbox default 'X'.


INITIALIZATION.

************************************************************************
*Verarbeitung
start-of-selection.

*Bestandestabelle mit Lagerplätzen) hochladen

*$smart (F) 2010-08-02 - #142 Der nicht Unicode konforme GUI
*$smart (F) 2010-08-02 - #142 Upload/Download Funktionsbaustein wurde
*$smart (F) 2010-08-02 - #142 mit vorgeschlagenem kompatiblen
*$smart (F) 2010-08-02 - #142 Funktionsbaustein ersetzt (A)

DATA V_file1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_path1 TYPE string.                        "smart: 2010-08-02 #142
DATA V_user_action1 TYPE I.                      "smart: 2010-08-02 #142
DATA V_file_table1 TYPE filetable.               "smart: 2010-08-02 #142
DATA V_default_extension1 TYPE string.           "smart: 2010-08-02 #142
DATA V_rc1 TYPE i.                               "smart: 2010-08-02 #142
DATA V_window_title1 TYPE string.                "smart: 2010-08-02 #142
DATA V_file_filter1 TYPE string.                 "smart: 2010-08-02 #142
V_FILE1 = dsninew.                               "smart: 2010-08-02 #142
SHIFT V_FILE1 RIGHT DELETING TRAILING '\'.       "smart: 2010-08-02 #142
SHIFT V_FILE1 RIGHT DELETING TRAILING '/'.       "smart: 2010-08-02 #142
SHIFT V_FILE1 LEFT DELETING LEADING SPACE .      "smart: 2010-08-02 #142
V_DEFAULT_EXTENSION1 = 'DAT'.                    "smart: 2010-08-02 #142
CALL METHOD                                      "smart: 2010-08-02 #142
  cl_gui_frontend_services=>file_open_dialog     "smart: 2010-08-02 #142
  EXPORTING DEFAULT_EXTENSION  =                 "smart: 2010-08-02 #142
    V_DEFAULT_EXTENSION1                         "smart: 2010-08-02 #142
  DEFAULT_FILENAME  = V_FILE1                    "smart: 2010-08-02 #142
  INITIAL_DIRECTORY  = V_FILE1                   "smart: 2010-08-02 #142
  CHANGING FILE_TABLE = V_FILE_TABLE1            "smart: 2010-08-02 #142
    RC = V_RC1                                   "smart: 2010-08-02 #142
    USER_ACTION = V_USER_ACTION1.                "smart: 2010-08-02 #142
CHECK V_USER_ACTION1 EQ 0.                       "smart: 2010-08-02 #142
CHECK V_RC1 GT 0.                                "smart: 2010-08-02 #142
READ TABLE V_file_table1 INDEX 1 INTO V_file1 .  "smart: 2010-08-02 #142
XFILENEW = V_FILE1.                              "smart: 2010-08-02 #142
CALL METHOD cl_gui_frontend_services=>gui_upload "smart: 2010-08-02 #142
       EXPORTING
            FILENAME = V_FILE1                   "smart: 2010-08-02 #142
    FILETYPE = 'ASC'                             "smart: 2010-08-02 #142
  CHANGING                                       "smart: 2010-08-02 #142
            DATA_TAB     = inew[]                "smart: 2010-08-02 #142
  EXCEPTIONS                                     "smart: 2010-08-02 #142
    OTHERS = 1.                                  "smart: 2010-08-02 #142

  IF XFILENEW IS INITIAL.
    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
  ENDIF.
  IF NOT XFILENEW CS '.txt'.
    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
  ENDIF.

**Bestandestabelle alt Werk 7000, Lort 7000 hochladen
**Dieser Datenbestand entspricht den ausgebuchten Mengen
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            FILENAME     = dsniold
*            FILEtype     = 'DAT'
*       IMPORTING
*            ACT_FILENAME = XFILEOLD
*       TABLES
*            DATA_TAB     = iold.
*
*  IF XFILEOLD IS INITIAL.
*    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
*  ENDIF.
*  IF NOT XFILEOLD CS '.txt'.
*    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
*  ENDIF.

**Lagerplatzkonvertierungstabelle
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            FILENAME     = dsnilp
*            FILEtype     = 'DAT'
*       IMPORTING
*            ACT_FILENAME = XFILEOLD
*       TABLES
*            DATA_TAB     = ilgpla.
*
*  IF XFILEOLD IS INITIAL.
*    PERFORM ABBRUCH USING 'Upload vom Benutzer abgebrochen'.
*  ENDIF.
*  IF NOT XFILEOLD CS '.txt'.
*    PERFORM ABBRUCH USING 'Bitte File als ''.txt'' sichern'.
*  ENDIF.


*$smart (F) 2010-08-02 - #111 OPEN DATASET benötigt mode und encoding
*$smart (F) 2010-08-02 - #111 Spezifikation Adding encoding to OPEN
*$smart (F) 2010-08-02 - #111 DATASET statement (A)

  open dataset dsno for output in text mode message xmess
ENCODING UTF-8 .                                 "smart: 2010-08-02 #111
  write: / dsno, xmess.
  detail.

**Materialbewertung
*  select * into imbew from mbew where bwkey = p_bwkey.
*
*    while imbew-matnr+0(1) = '0'.
*      shift imbew-matnr left.
*    endwhile.
*
*    append imbew.
*  endselect.


*Bestandesvergleich new - old

*Collect Tabelle new
  loop at inew.
    if sy-tabix = 1.
      delete inew.      "Titelzeile weg
      exit.
    endif.
  endloop.

*  loop at inew.
*    inewc-matnr = inew-matnr.
*    inewc-gesme = inew-gesme.
*    collect inewc.
*  endloop.
*
**Collect Tabelle old
*  sort iold by wmatnr.
*  loop at iold.
*    at new wmatnr.
*      ioldc-matnr = iold-wmatnr.
*      ioldc-gesme = iold-labst + iold-insme + iold-speme.
*      collect ioldc.
*    endat.
*  endloop.

*Neue mit Differenz zu Alten

*  write:/ 'Matnr.  ', 'Bestand neu ', '          Bestand alt ',
*          '       Mengen-Diff ', '        Wert-Diff'.

*  loop at inewc.
*    clear: wmatnr.
*    wmatnr = inewc-matnr.
*    read table ioldc with key matnr = inewc-matnr.
*    if sy-subrc = 0.
*      if pdiff = ' '.
*        write: / inewc-matnr, inewc-gesme, ioldc-gesme.
*        perform wert.
*        write: wmdiff, wwdiff.
*      elseif pdiff = 'X'.
*        perform wert.
*        check: not wwdiff is initial.
*        write: / inewc-matnr, inewc-gesme, ioldc-gesme.
*        write: wmdiff, wwdiff.
*      endif.
*    else.
**      clear: ioldc-gesme.
*      clear: ioldc.
*      write:/  inewc-matnr, inewc-gesme, ioldc-gesme..
*      perform wert.
*      write: wmdiff, wwdiff,
*    ' kein Bestand OLD'.
*    endif.
*  endloop.
*
**Alt, welche in der Neuen fehlen
*  loop at ioldc.
*    clear: wmatnr.
*    wmatnr = ioldc-matnr.
*    read table inewc with key matnr = ioldc-matnr.
*    if sy-subrc ne 0.
*      clear: inewc.
*      write:/  ioldc-matnr, inewc-gesme, ioldc-gesme.
*      perform wert.
*      write: wmdiff, wwdiff,
*    ' kein Bestand NEW'.
*    endif.
*  endloop.

*MM Bewegungen buchen
  loop at inew.

    xmatnr = inew-matnr.
    if xmatnr co '0123456789 '.
      while xmatnr+17(1) = ' '.
        shift xmatnr right IN CHARACTER MODE .   "smart: 2010-08-02 #115
        xmatnr(1) = '0'.
      endwhile.
    endif.

*Grunddaten angelegt ?
    select single * from mara where matnr eq xmatnr.
    if sy-subrc ne 0.
      write: / xmatnr, ' - auf der MARA nicht angelegt !'.
    else.



*Klassifizierung ?
if mara-xchpf = 'X'.
*perform lesen_materialklassierung.
if mara-pstat na 'C'.
   write:/ xmatnr, ' - nicht klassifiziert !'.
endif.

select single * from inob where klart = '023'
                            and objek = xmatnr.
if sy-subrc ne 0.
   write:/ xmatnr, ' - hat keine Klasse 023 !'.
endif.
endif.

*Chargeninformationen vollständig ?
      if mara-xchpf = 'X' and inew-charg is initial.
        write:/ xmatnr, ' - Chargenpflichtig: Charge intern'.
      elseif mara-xchpf = ' ' and not inew-charg is initial.
   write:/ xmatnr, ' - nicht Chargenpflichtig: Charge initialisiert'.
*  clear: inew-charg. modify inew.
      endif.

      if mara-xchpf = space and not inew-vfdat is initial.
         write:/ xmatnr, ' - Verfalldatum wird initialisiert'.
   clear: inew-vfdat. modify inew.
      endif.

*Mindesthaltbarkeit prüfen
      if not mara-mhdrz is initial and inew-vfdat is initial
         and not mara-xchpf is initial.
        write:/ xmatnr, ' - Verfalldatum fehlt, wird gesetzt'.
        inew-vfdat = '2007-12-31'.
      elseif mara-mhdrz is initial and not inew-vfdat is initial.
   write:/ xmatnr, ' - Verfalldatum gepflegt, aber nicht MHD pflichtig'.
   write:/ xmatnr, ' - Verfalldatum wird initialisiert'.
   clear: inew-vfdat. modify inew.
      endif.


*Lagerort angelegt ?
      select single * from mard where matnr eq xmatnr
                                  and werks eq p_werks
                                  and lgort eq p_lgort.
      if sy-subrc ne 0.
       write:/ xmatnr, ' - LGORT nicht angelegt auf ', p_werks, p_lgort.
      endif.

*LVS Sicht angelegt ?
      select single * from mlgn where matnr eq xmatnr
                                  and lgnum eq p_lgnum.
      if sy-subrc ne 0.
   write:/ xmatnr, ' - LVS Sicht nicht angelegt auf ', p_werks, p_lgort.
      endif.

*MM Bestand buchen
      if test = ' '.
        perform verarbeiten_input.
        count_1 = count_1 + 1.
      endif.

    endif.
  endloop.

**Wertmässige Abweichung
*  skip 1.
*  write:/ 'Total der wertmässigen Abweichungen: ', wwsum.
*
*  skip 1.
*  write: / 'MM: Anzahl Datensätze: ', count_1.
*  skip 1.

*WM: Einlagerung der Bestände auf die def. Plätze

*  write:/

  if test = ' '.
    perform open_group.
  endif.

  clear: count.
  loop at inew.
    xmatnr = inew-matnr.
    if xmatnr co '0123456789 '.
      while xmatnr+17(1) = ' '.
        shift xmatnr right IN CHARACTER MODE .   "smart: 2010-08-02 #115
        xmatnr(1) = '0'.
      endwhile.
    endif.

      write:/ inew-matnr, inew-charg, inew-lgpla.

*Ist der neue Lagerplatz angelegt ?
      select single * from lagp where lgnum = p_lgnum
                                  and lgtyp = p_nltyp
                                  and lgpla = inew-lgpla.
      if not sy-subrc = 0.
        write:/ inew-lgpla, '  - Lagerplatz nicht angelegt !'.
      endif.
      if test = ' '.
        perform verarbeiten_wm.
      endif.
  endloop.

  if test = ' '.
    perform close_group.
  endif.

*---------------------------------------------------------------------*
*       FORM verarbeiten_input                                        *
*---------------------------------------------------------------------*
form verarbeiten_input.

  clear bmseg.
  bmseg-mappe = 'MATBESTIN'.
  bmseg-tcode = 'MB11'.
  bmseg-bldat =  P_DATUM.

  concatenate  'DRC 561er' p_datum into bmseg-bktxt
  separated by space IN CHARACTER MODE .         "smart: 2010-08-02 #101
  concatenate  'DRC 561er' p_datum inew-matnr inew-charg
  into bmseg-sgtxt separated by space IN         "smart: 2010-08-02 #101
    CHARACTER MODE .                             "smart: 2010-08-02 #101

*Bewegungsart setzen
  bmseg-bwart = '561'.                  "frei

  bmseg-matnr = inew-matnr.
  bmseg-werks = p_werks.
  bmseg-lgort = p_lgort.
  bmseg-erfmg = inew-menge.
  bmseg-charg = inew-charg.

*Datumformat 2007-12-25
  concatenate inew-vfdat+0(4) inew-vfdat+5(2) inew-vfdat+8(2)
    into bmseg-vfdat IN CHARACTER MODE .         "smart: 2010-08-02 #101

  if test is initial.
    transfer bmseg to dsno.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM ABBRUCH                                                  *
*---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM ABBRUCH USING TEXT TYPE CLIKE.              "smart: 2010-08-02 #105
  FORMAT COLOR 6 INTENSIFIED ON.
  SKIP 1.
  WRITE: / '**************  A B B R U C H  ***************************'.
  WRITE: /(58) TEXT.
  WRITE: / '**********************************************************'.
  STOP.
enDFORM.
*&---------------------------------------------------------------------*
*&      Form  wert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM wert.
*
*
*  read table imbew with key matnr = wmatnr
*                            bwkey = '7000'.
*
*  if imbew-vprsv = 'V'.
*    wwdiff = wmdiff * ( imbew-verpr / imbew-peinh ).
*  elseif imbew-vprsv = 'S'.
*    wwdiff = wmdiff * ( imbew-stprs / imbew-peinh ).
*  endif.
*
*  wwsum = wwsum + wwdiff.
*ENDFORM.                    " wert
*&---------------------------------------------------------------------*
*&      Form  verarbeiten_WM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verarbeiten_WM.


  perform bdc_dynpro      using 'SAPML03T' '0101'.
  perform bdc_field       using 'LTAK-LGNUM' p_lgnum.
  perform bdc_field       using 'LTAK-BWLVS' '999'.    "interne Bew.art

**Bestandesqualifikation setzen
*  if not inew-xinsme is initial.
*   perform bdc_field       using 'LTAP-BESTQ' 'Q'.
*  elseif not inew-xspeme is initial.
*   perform bdc_field       using 'LTAP-BESTQ' 'S'.
*  endif.

  perform bdc_field       using 'LTAP-MATNR' inew-matnr.
  perform bdc_field       using 'RL03T-ANFME' inew-menge.
  perform bdc_field       using 'LTAP-WERKS' p_werks.
  perform bdc_field       using 'LTAP-LGORT' p_lgort.
  if not inew-charg is initial and mara-xchpf = 'X'.
    perform bdc_field       using 'LTAP-CHARG' inew-charg.
  endif.
  perform bdc_dynpro      using 'SAPML03T' '0102'.
  perform bdc_field       using 'LTAP-VLTYP' '998'.
  perform bdc_field       using 'LTAP-LETYP' p_letyp.
  perform bdc_field       using 'LTAP-VLPLA' 'AUFNAHME'.
  perform bdc_field       using 'LTAP-NLTYP' p_nltyp.
  perform bdc_field       using 'LTAP-NLPLA' inew-lgpla.
  perform bdc_transaction using 'LT01'.

ENDFORM.                    " verarbeiten_WM

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*----------------------------------------------------------------------*
FORM OPEN_GROUP.

wgroup = 'WM_Bestand_IN'.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = wgroup
            USER   = sy-uname.
ENDFORM.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*----------------------------------------------------------------------*
FORM CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM BDC_TRANSACTION USING TCODE TYPE CLIKE.     "smart: 2010-08-02 #105
  if test = ' '.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              TCODE     = TCODE
         TABLES
              DYNPROTAB = BDCDATA.
  endif.
  REFRESH BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM BDC_DYNPRO USING PROGRAM TYPE CLIKE DYNPRO  "smart: 2010-08-02 #105
  TYPE CLIKE.                                    "smart: 2010-08-02 #105
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM BDC_FIELD USING FNAM TYPE CLIKE FVAL TYPE CLIKE.
                                                 "smart: 2010-08-02 #105

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  lesen_materialklassierung
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lesen_materialklassierung.

  g_objek = xmatnr.

  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
       EXPORTING
            classtext          = 'X'
            classtype          = '023'
            object             = g_objek
       TABLES
            t_class            = g_t_sclass
            t_objectdata       = g_t_clobjdat
       EXCEPTIONS
            no_classification  = 1
            no_classtypes      = 2
            invalid_class_type = 3
            OTHERS             = 4.

sy-uname = Sy-uname.


ENDFORM.                    " lesen_materialklassierung
