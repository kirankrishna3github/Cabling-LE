*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.12.2017 at 11:39:47
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_NOPREF......................................*
DATA:  BEGIN OF STATUS_ZLE_NOPREF                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_NOPREF                    .
CONTROLS: TCTRL_ZLE_NOPREF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_NOPREF                    .
TABLES: ZLE_NOPREF                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
