*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10.04.2019 at 08:38:53
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPREF_USAUTH....................................*
DATA:  BEGIN OF STATUS_ZPREF_USAUTH                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPREF_USAUTH                  .
CONTROLS: TCTRL_ZPREF_USAUTH
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPREF_USAUTH                  .
TABLES: ZPREF_USAUTH                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
