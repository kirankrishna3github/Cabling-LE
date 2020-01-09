*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 09.11.2016 at 15:59:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZEDI_EIKTO......................................*
DATA:  BEGIN OF STATUS_ZEDI_EIKTO                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDI_EIKTO                    .
CONTROLS: TCTRL_ZEDI_EIKTO
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDI_EIKTO                    .
TABLES: ZEDI_EIKTO                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
