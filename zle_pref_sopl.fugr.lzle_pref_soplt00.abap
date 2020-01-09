*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 09.09.2019 at 16:15:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREF_SOPL...................................*
DATA:  BEGIN OF STATUS_ZLE_PREF_SOPL                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREF_SOPL                 .
CONTROLS: TCTRL_ZLE_PREF_SOPL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREF_SOPL                 .
TABLES: ZLE_PREF_SOPL                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
