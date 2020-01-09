*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 09.08.2018 at 16:31:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREF_CM.....................................*
DATA:  BEGIN OF STATUS_ZLE_PREF_CM                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREF_CM                   .
CONTROLS: TCTRL_ZLE_PREF_CM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREF_CM                   .
TABLES: ZLE_PREF_CM                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
