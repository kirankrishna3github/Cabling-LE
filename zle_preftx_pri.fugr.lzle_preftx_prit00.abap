*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 15.01.2018 at 15:15:58
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_PREFTX_PRI..................................*
DATA:  BEGIN OF STATUS_ZLE_PREFTX_PRI                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_PREFTX_PRI                .
CONTROLS: TCTRL_ZLE_PREFTX_PRI
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_PREFTX_PRI                .
TABLES: ZLE_PREFTX_PRI                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
