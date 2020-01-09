*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.10.2016 at 11:22:24
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZEDI_SPED_NKI...................................*
DATA:  BEGIN OF STATUS_ZEDI_SPED_NKI                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZEDI_SPED_NKI                 .
CONTROLS: TCTRL_ZEDI_SPED_NKI
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZEDI_SPED_NKI                 .
TABLES: ZEDI_SPED_NKI                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
