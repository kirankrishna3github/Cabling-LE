*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 10.04.2019 at 08:28:57
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZPREF_PARAM.....................................*
DATA:  BEGIN OF STATUS_ZPREF_PARAM                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPREF_PARAM                   .
CONTROLS: TCTRL_ZPREF_PARAM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPREF_PARAM                   .
TABLES: ZPREF_PARAM                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
