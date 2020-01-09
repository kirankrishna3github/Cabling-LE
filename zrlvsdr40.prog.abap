REPORT RLVSDR40 NO STANDARD PAGE HEADING MESSAGE-ID L3.
*---------------------------------------------------------------------*
*  Report RLVSDR40: Druck TA's über Formular  / Printing TO´s         *
*---------------------------------------------------------------------*
*     Anstoß zum Druck aus TA-Erstellung / via TO creation            *
*     Anstoß zum Druck aus Transaktion manuell / manually              *
*---------------------------------------------------------------------*


*........Reportspezifische Parameter und Select-Options.................
SELECTION-SCREEN  BEGIN OF BLOCK XXX WITH FRAME TITLE TEXT-010.
PARAMETERS:
        DRUCKKZ  LIKE RLDRU-DRUKZ,
        EDRUCKER LIKE RLDRU-LDEST,
        SPOOLPAR LIKE RLDRU-SPOOL,
        DRUCKEN  LIKE RLDRU-DRUCK  DEFAULT 'X',
        EXPLIZIT LIKE RLDRU-AUSDR.
SELECTION-SCREEN END OF BLOCK XXX.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN  BEGIN OF BLOCK YYY WITH FRAME TITLE TEXT-167.
PARAMETERS:
        TASCH AS CHECKBOX   DEFAULT 'X',    "Druck TA-Schein
        LESCH AS CHECKBOX   DEFAULT 'X',    "Druck LE-Schein
        LETASCH AS CHECKBOX DEFAULT 'X',    "Druck LE-TA-Schein
        LEINH AS CHECKBOX   DEFAULT 'X',    "Druck LE-Inhaltsverzeichnis
        HUMLA AS CHECKBOX   DEFAULT 'X',    "Pick-HU-Scheine     "HUM98
        ETIKETT AS CHECKBOX DEFAULT 'X'.    "Druck Etiketten

SELECTION-SCREEN END OF BLOCK YYY.

INCLUDE RLVSDTOP.
INCLUDE MLLVSKON.

*---------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                          *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION_SCREEN.

*---------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*---------------------------------------------------------------------*
START-OF-SELECTION.
*........compatibility because of new parameter etikett and RLVSEXTE
  MOVE ETIKETT TO FIX_ETIKE.
  PERFORM START_OF_SELECTION.

*---------------------------------------------------------------------*
*        GET LTAK                                                     *
*---------------------------------------------------------------------*
GET LTAK.
  PERFORM GET_LTAK.

*---------------------------------------------------------------------*
*        GET LTAP                                                     *
*---------------------------------------------------------------------*
GET LTAP.
  PERFORM GET_LTAP.

*---------------------------------------------------------------------*
*        END-OF-SELECTION                                             *
*---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM END_OF_SELECTION.
  PERFORM MESSAGE_OUTPUT.
*---------------------------------------------------------------------*
*        INCLUDES                                                     *
*---------------------------------------------------------------------*
*........Externer Aufruf des Druckprogramms.............................
*........Call of printing from external i.e. out of posting TOs
INCLUDE RLVSEXTE.
*........mutual form routeens with multiple processing.................
INCLUDE RLVSDFOR.
*---------------------------------------------------------------------*
*       Subrouteens except multiple processing                        *
*                                                                     *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
FORM START_OF_SELECTION.
  CLEAR:                 DRUCK_OK_TO_SINGLE,
                         DRUCK_OK_PO,
                         DRUCK_OK_MULTI,
                         DRUCK_OK_SU,
                         DRUCK_OK_HU,                           "HUM98
                         DRUCK_OK_LABELS.
*........Is used in mutual include RLVSEXTE...........................*
  FLG_TADRUCK_WIE = SAPSCRIPT.
*........Initialize internal table out ...............................*
  REFRESH: IRLDRH, IRLDRI, IRLDRC, IRLDRU.

   KZMEM = T4_KZMEM.        "for mutual routeens
*........Importieren von QPSERR und LSPERR aus Memory

  IF NOT T4_KZMEM IS INITIAL.
    IMPORT QSPERR
           LSPERR FROM MEMORY ID DRUCK_ID.
    SORT QSPERR.
    IMPORT RLDRU-DRUCK RLDRU-AUSDR FROM MEMORY ID DRUCK_ID.
    IF RLDRU-DRUCK IS INITIAL.
      CLEAR DRUCKEN.
    ENDIF.
    IF NOT RLDRU-AUSDR IS INITIAL.
      MOVE RLDRU-AUSDR TO EXPLIZIT.
    ENDIF.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       GET LTAK                                                      *
*---------------------------------------------------------------------*
FORM GET_LTAK.
*........check of  DRUKZ ..............................................
  IF LTAK-DRUKZ IS INITIAL.
    MOVE DRUCKKZ TO LTAK-DRUKZ.
  ENDIF.
  CHECK NOT LTAK-DRUKZ IS INITIAL.
*** OR 131098 HUM
*........Reset Flag for reading HU-Data once per TO...................
  CLEAR FLG_HUDRUCK.
***
*........rest together with multiple processing.......................
  PERFORM GET_LTAK_MUTUAL.
ENDFORM.

*---------------------------------------------------------------------*
*       GET LTAP                                                      *
*---------------------------------------------------------------------*
FORM GET_LTAP.
  CLEAR:  RLDRU, IRLDRU, IRLDRI.
  PERFORM GET_LTAP_MUTUAL.
ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       Dummy bbecause of RLVSEXTE and upwards compatibility
*----------------------------------------------------------------------*
FORM AT_SELECTION_SCREEN.

ENDFORM.                    " AT_SELECTION_SCREEN

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_OUTPUT
*&---------------------------------------------------------------------*
*       only when report is manually executed, not when called by ml03t
*----------------------------------------------------------------------*
FORM MESSAGE_OUTPUT.
  CHECK T4_KZMEM IS INITIAL.
*.........at least one print was successful............................
  IF DRUCK_OK_TO_SINGLE = 1 OR DRUCK_OK_MULTI = 1 OR
      DRUCK_OK_SU = 1 OR
      DRUCK_OK_PO = 1 OR DRUCK_OK_LABELS = 1 OR                 "HUM98
      DRUCK_OK_HU = 1.                                          "HUM98
     MESSAGE S133.  "Druck erfolgt
     EXIT.
  ENDIF.
ENDFORM.                    " MESSAGE_PUTPUT
*&---------------------------------------------------------------------*
*&      Form  END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM END_OF_SELECTION.
*........Sort of additional tables....................................
  PERFORM SORTING.
*........necessary to avoid too many lists............................
  SORT IRLDRI BY LDESI FORMI SPOOI TANUM TAPOS.
*........Call Customer function MWMD0001..............................
  PERFORM USER_EXIT.
*........call of MULTIPLE print ......................................
  PERFORM MULTI_PRINT.
*........Call of printing function modules ...........................
  PERFORM CALL_FUNCTION_MODULES_MUTUAL.

ENDFORM.                    " END_OF_SELECTION
*&---------------------------------------------------------------------*
*&      Form  USER_EXIT
*&---------------------------------------------------------------------*
*       you will get all relevant data:   Exit:  MWMD0001 -> /nCMOD
*----------------------------------------------------------------------*
FORM USER_EXIT.

*.......in standard, always printing is always allowed.................
    MOVE: CON_X TO PRINT_SINGLE,
          CON_X TO PRINT_PO,
          CON_X TO PRINT_MULTI,
          CON_X TO PRINT_SU,
          CON_X TO PRINT_HU,                                    "HUM98
          CON_X TO PRINT_LABEL,
          CON_X TO PRINT_MULTI_REF.

    CALL CUSTOMER-FUNCTION '001'
       TABLES    XRLDRC       =   IRLDRC
                 XVBLKK       =   IVBLKK
                 XVBLKP       =   IVBLKP
                 XSERNR       =   ISERNR
                 XRLDRH       =   IRLDRH
                 XRLDRI       =   IRLDRI
                 XRLDRP       =   IRLDRP
                 XRLDRU       =   IRLDRU
                 XT329P       =   IT329P
                 XRESB        =   IRESB
                 XRLVEK       =   IRLVEK
                 XREFTAB      =   REFTAB
       CHANGING
                 C_T312S      =   T312S
                 C_LESCH      =   LESCH
                 C_LETASCH    =   LETASCH
                 C_LEINH      =   LEINH
                 C_SINGLE     =   PRINT_SINGLE
                 C_PO         =   PRINT_PO
                 C_SU         =   PRINT_SU
                 C_LABEL      =   PRINT_LABEL
                 C_MULTI      =   PRINT_MULTI
                 C_MULTI_REF  =   PRINT_MULTI_REF              "HUM98
                 C_HUMLA      =   PRINT_HU.                    "HUM98



ENDFORM.                    " USER_EXIT
*&---------------------------------------------------------------------*
*&      Form  MULTI_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MULTI_PRINT.

*.......Printing of Multiple TOs:  with sorting .......................
 IF TASCH = CON_X.
   IF PRINT_MULTI = CON_X.
     CALL FUNCTION 'L_PRINT_TO_MULTIPLE'
        EXPORTING I_DRUCK =  DRUCKEN
        IMPORTING E_RETURN = DRUCK_OK_REF
        TABLES   XRLDRC = IRLDRC
                 XVBLKK = IVBLKK
                 XVBLKP = IVBLKP
                 XSERNR = ISERNR
                 XRLDRH = IRLDRH
                 XRLDRI = IRLDRI
                 XRLDRP = IRLDRP
                 XRLDRU = IRLDRU
                 XT329P = IT329P
                 XRESB  = IRESB
                 XRLVEK = IRLVEK
*** OR 141098
                 XLTHU  = ILTHU.
   ENDIF.
 ENDIF.
ENDFORM.                    " MULTI_PRINT
