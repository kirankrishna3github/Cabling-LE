**----------------------------------------------------------------------*
**   INCLUDE ZXLTOU01                                                   *
**----------------------------------------------------------------------*
**
**"----------------------------------------------------------------------
**"*"Globale Schnittstelle:
**"       IMPORTING
**"              I_LTAK_VB LIKE  LTAK_VB STRUCTURE  LTAK_VB
**"       TABLES
**"              T_LTAP_VB STRUCTURE  LTAP_VB
**"----------------------------------------------------------------------
*************************************************************************
**Beschreibung:
*************************************************************************
** Etikettierung Filetransfer; 25.1.06, SAP Stäfa, Peter Huber
**
** Stellt für Codesoft mittels filetransfer die benötigten Daten bereit.
**
**Aenderungen
*************************************************************************
** 18.07.2007, Peter Huber
**    Anpassung Datumsaufbereitung und Lagereinheitennummer aus TA
**
** 04.06.2007, Peter Huber
**    bei 311er kein Filetransfer mehr für Etiketten
**
** 30.5.2007, SAP Stäfa Peter Huber
**    Mengeneinheit von G in KG geändert
**
** 11.05.06, Markus Raffeiner
**   Parameter ZM_CODESOFT_TEST für Etikettendruck aktivieren: Falls
**   ein "X" gesetzt, wird die Etikette im Codesoft nicht gedruckt
**   (Endung tmp), ansonsten gedruckt (Endung txt)
**
** 12.05.06, Markus Raffeiner
**   Falls die Materialbelegnummer (ltak-mblnr) leer ist, wird die
**   Bedarfsnummer (ltak-benum) auf der Etikette ausgegeben
**
** 01062006, Peter Huber
**   Bew.art abfragen, anderfalls bei anlegen TA (VL06O) ungewollt auch
**   ein filetransfer ausgeführt wird.
**
** 30102006; Peter Huber
**   für Lagernr. 491 erfolgt der Druck bei Druckkennzeichen 02
**   nicht mehr sofort, sondern im Job
**
** 30052007; Peter Huber
**   im Mdt. 200 Gewichtseinheit G in KG umsetzen
**
** 05032008; Markus Raffeiner
**   im Mdt. 200 Anpassen Druckersteuerung Etikettendruck mit neuer
**   Tabelle ZMETIK3 (Lagernummer, Bewegungsart)
**
** 15092010; Markus Raffeiner
**   FTP-Übergabe neu in Charaktermode wegen Unicode
**
** 09052011; Markus Raffeiner
** Konstante Mandantenabfragen elimineren
*************************************************************************
**
*************************************************************************
**Tabellen
*************************************************************************
*  TABLES: ltap, ltak, mseg, mkpf, mch1, caufv, afko, crhd, afru, afvc,
*          zmetik,           "Customizingtabelle
*          zledrucker,       "Drucker nach Lagernummer
*          zmetik3.          "Druckersteuerung Etikettendruck
*
*  DATA:    wa_usr05 TYPE usr05.
*
**Uebergabestrukur
*  INCLUDE: zincl_etikette_101.
*
*************************************************************************
**Konstanten
*************************************************************************
**Pfad, Name und Extension der Datei
*  CONSTANTS: dsn(200) VALUE
*     '<PATH>T.ETIK.<ETNR>.<TANUM>.<TAPOS>.<ERDAT>.<ERUHR>.<TEST>'.
*
***Benutzerparameter ZM_CODESOFT_TEST = 'X' ermöglicht download auf
***CODESOFT Server, ohne das UC4 die Datei abholt !
*  CONSTANTS: tmp(3) VALUE 'TMP',
*             txt(3) VALUE 'TXT'.
*
*************************************************************************
**Felder
*************************************************************************
*  DATA:  wmatnr(18),
*         waufnr(12),
*         wlenum(20),
*         drucker_name(30) TYPE c,
*         tfdsn(200),
*         h_mblnr          LIKE mseg-mblnr,                      "CSMIG2010
*         ev_jobstate      LIKE sugx_fpara-jobstate,
*         eventid          LIKE tbtcjob-eventid,
*         eventparm        LIKE tbtcjob-eventparm,
*         repid            LIKE sy-repid.
*
**FTP-Deklarationen
*  DATA: user(64)    TYPE c,
*        pwd(64)     TYPE c,
*        host(64)    TYPE c,
*        blob_length TYPE i,
*        dest        LIKE rfcdes-rfcdest VALUE 'SAPFTP',
*        desta       LIKE rfcdes-rfcdest VALUE 'SAPFTPA',
*        dstlen      TYPE i,
*        hdl         TYPE i,
*        slen        TYPE i,
*        key         TYPE i VALUE 26101957.
*
*  DESCRIBE FIELD pwd LENGTH dstlen IN CHARACTER MODE. "CSMIG2010
*
*************************************************************************
**Interne Tabellen
*************************************************************************
*
*  DATA: return_code LIKE sy-subrc.
*
*  DATA: itab_ze101 LIKE ze101 OCCURS 0.
*
**Hilfsfelder
*  DATA: BEGIN OF h,
*          drnr LIKE ze101-drnr,
*        END OF h.
*
*******************CHG0031412****************
*
*  DATA: ls_lips  TYPE lips,
*        ls_lqua  TYPE lqua,
*        lv_pikmg TYPE pikmg.
*
*  CHECK i_ltak_vb-bwlvs = '902'.
*
*  LOOP AT t_ltap_vb.
*
*    SELECT SINGLE *
*      FROM lips
*      INTO ls_lips
*      WHERE vbeln = i_ltak_vb-benum
*      AND matnr = t_ltap_vb-matnr
*      AND charg = t_ltap_vb-charg.
*
*    CALL FUNCTION 'WB2_GET_PICK_QUANTITY'
*      EXPORTING
*        i_vbeln             = ls_lips-vbeln
*        i_posnr             = ls_lips-posnr
**       I_MODE              = ' '
*      IMPORTING
*        e_pikmg             = lv_pikmg
*      EXCEPTIONS
*        document_read_error = 1
*        OTHERS              = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*
*    SELECT SINGLE *
*      FROM lqua
*      INTO ls_lqua
*      WHERE lgnum = t_ltap_vb-lgnum
*      AND matnr = t_ltap_vb-matnr
*      AND werks = t_ltap_vb-werks
*      AND charg = t_ltap_vb-charg
*      AND lgtyp = t_ltap_vb-nltyp
*      AND lgpla = t_ltap_vb-nlpla.
*
*    IF lv_pikmg < ( ls_lqua-verme + t_ltap_vb-vsolm ).
*      MESSAGE s000(l3) WITH 'Transfer order not created.' DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDLOOP.
