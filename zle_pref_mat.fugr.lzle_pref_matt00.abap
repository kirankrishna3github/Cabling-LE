*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.03.2018 at 08:49:19
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREF_MAT....................................*
DATA:  BEGIN OF STATUS_ZLE_PREF_MAT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREF_MAT                  .
CONTROLS: TCTRL_ZLE_PREF_MAT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREF_MAT                  .
TABLES: ZLE_PREF_MAT                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
