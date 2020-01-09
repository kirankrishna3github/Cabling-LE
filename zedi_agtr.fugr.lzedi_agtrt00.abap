*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.12.2016 at 15:37:34
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZEDI_AGTR.......................................*
DATA:  BEGIN OF STATUS_ZEDI_AGTR                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDI_AGTR                     .
CONTROLS: TCTRL_ZEDI_AGTR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDI_AGTR                     .
TABLES: ZEDI_AGTR                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
