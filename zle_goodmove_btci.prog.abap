************************************************************************
* 2010-08-02   smartShift project

************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_BTCI                                          *
*----------------------------------------------------------------------*
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


ENDFORM.                    " start_dynpro
