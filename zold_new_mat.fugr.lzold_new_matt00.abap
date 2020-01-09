*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.01.2016 at 09:59:31
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZOLD_NEW_MAT....................................*
DATA:  BEGIN OF STATUS_ZOLD_NEW_MAT                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZOLD_NEW_MAT                  .
CONTROLS: TCTRL_ZOLD_NEW_MAT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZOLD_NEW_MAT                  .
TABLES: ZOLD_NEW_MAT                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
