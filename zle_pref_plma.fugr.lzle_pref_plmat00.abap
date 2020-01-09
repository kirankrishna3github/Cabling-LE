*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02.03.2018 at 09:32:19
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREF_PLMA...................................*
DATA:  BEGIN OF STATUS_ZLE_PREF_PLMA                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREF_PLMA                 .
CONTROLS: TCTRL_ZLE_PREF_PLMA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREF_PLMA                 .
TABLES: ZLE_PREF_PLMA                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
