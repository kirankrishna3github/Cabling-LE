*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.03.2018 at 15:41:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREF_NODEL..................................*
DATA:  BEGIN OF STATUS_ZLE_PREF_NODEL                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREF_NODEL                .
CONTROLS: TCTRL_ZLE_PREF_NODEL
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREF_NODEL                .
TABLES: ZLE_PREF_NODEL                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
