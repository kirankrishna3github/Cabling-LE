*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.07.2016 at 08:59:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZEDI_GLN........................................*
DATA:  BEGIN OF STATUS_ZEDI_GLN                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDI_GLN                      .
CONTROLS: TCTRL_ZEDI_GLN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDI_GLN                      .
TABLES: ZEDI_GLN                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
