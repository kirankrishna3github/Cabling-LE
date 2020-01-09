*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.12.2017 at 08:29:09
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_NOPREFTEXT..................................*
DATA:  BEGIN OF STATUS_ZLE_NOPREFTEXT                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_NOPREFTEXT                .
CONTROLS: TCTRL_ZLE_NOPREFTEXT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_NOPREFTEXT                .
TABLES: ZLE_NOPREFTEXT                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
