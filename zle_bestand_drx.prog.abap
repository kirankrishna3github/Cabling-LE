
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #105
************************************************************************

REPORT ZLE_BESTAND_DRX .
***********************************************************************
*Report für Bestandesauswertung
*Spezifikationen für defintive Lösung ist ausstehend.
*ZLE_GET_BESTAND ist ein generierter Report.
*Quick-Viewer: LAGP unter User I020676
*
***********************************************************************
*
*
*
*
*
***********************************************************************

tables: lagp.

DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

  DATA: BEGIN OF messtab OCCURS 10.
          INCLUDE STRUCTURE bdcmsgcoll.
  DATA: END OF messtab.

selection-screen begin of block b1 with frame title text-001.
selection-screen skip 1.
parameters: p600_001 radiobutton group g1.
*selection-screen skip 1.
parameters: p620_001 radiobutton group g1.
*selection-screen skip 1.
parameters: p620_100 radiobutton group g1.
selection-screen end of block b1.

initialization.

start-of-selection.
 PERFORM start_dynpro USING 'ZLE_GET_BESTAND' '1000' 'X'.
if p600_001 = 'X'.
PERFORM fill_dynpro USING '600'   'SP$00001-LOW'. "Lagernr.
PERFORM fill_dynpro USING '001'   'SP$00002-LOW'. "Lagertyp
*PERFORM fill_dynpro USING '001'   'SP$00003-LOW'. "Lagerbereich
elseif p620_001 = 'X'.
PERFORM fill_dynpro USING '620'   'SP$00001-LOW'.
PERFORM fill_dynpro USING '001'   'SP$00002-LOW'.
*PERFORM fill_dynpro USING '001'   'SP$00003-LOW'.
elseif p620_100 = 'X'.
PERFORM fill_dynpro USING '620'   'SP$00001-LOW'.
PERFORM fill_dynpro USING '100'   'SP$00002-LOW'.
*PERFORM fill_dynpro USING '100'   'SP$00003-LOW'.
endif.


  CALL TRANSACTION 'ZLE_GET_BESTAND'  "keine manuellen Aenderungen !
        USING bdcdata
        MODE 'E'
        MESSAGES INTO messtab.


*&---------------------------------------------------------------------*
*&      FORM fill_dynpro                                               *
*&---------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM fill_dynpro USING content TYPE CLIKE        "smart: 2010-08-02 #105
  fieldname TYPE CLIKE.                          "smart: 2010-08-02 #105
  CLEAR bdcdata.
  bdcdata-fnam  = fieldname.
  bdcdata-fval  = content.
  APPEND bdcdata.
ENDFORM.                    "fill_dynpro

*&---------------------------------------------------------------------*
*&      Form  start_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->program   text
*      -->dynpro    text
*      -->dynbegin  text
*----------------------------------------------------------------------*

*$smart (W) 2010-08-02 - #105 Automatische Auflösung untypisierter FORM
*$smart (W) 2010-08-02 - #105 Parameter, soweit dies möglich ist. Dies
*$smart (W) 2010-08-02 - #105 wird nur angewendet wenn alle Aufrufe
*$smart (W) 2010-08-02 - #105 (PERFORM Anweisung) die selbe Typisierung
*$smart (W) 2010-08-02 - #105 verwenden. (A)

FORM start_dynpro  USING    program TYPE CLIKE   "smart: 2010-08-02 #105
                            dynpro TYPE CLIKE    "smart: 2010-08-02 #105
                            dynbegin TYPE CLIKE. "smart: 2010-08-02 #105

  CLEAR   bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = dynbegin.
  APPEND bdcdata.
endform.
