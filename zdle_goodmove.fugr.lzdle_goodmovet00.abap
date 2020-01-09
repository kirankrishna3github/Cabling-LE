*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.05.2010 at 01:31:06 by user I020676
*---------------------------------------------------------------------*
*...processing: ZLE_GM_CHARGEN_P................................*
DATA:  BEGIN OF STATUS_ZLE_GM_CHARGEN_P              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_GM_CHARGEN_P              .
CONTROLS: TCTRL_ZLE_GM_CHARGEN_P
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_GM_CHARGEN_P              .
TABLES: ZLE_GM_CHARGEN_P               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
