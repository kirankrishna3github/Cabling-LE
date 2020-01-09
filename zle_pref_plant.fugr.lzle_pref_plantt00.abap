*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.11.2017 at 18:01:39
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREF_PLANT..................................*
DATA:  BEGIN OF STATUS_ZLE_PREF_PLANT                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREF_PLANT                .
CONTROLS: TCTRL_ZLE_PREF_PLANT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREF_PLANT                .
TABLES: ZLE_PREF_PLANT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
