*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.11.2013 at 14:05:11 by user I020676
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_TRANS_MSEG..................................*
DATA:  BEGIN OF STATUS_ZMM_TRANS_MSEG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_TRANS_MSEG                .
CONTROLS: TCTRL_ZMM_TRANS_MSEG
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZMM_TRANS_MSEG                .
TABLES: ZMM_TRANS_MSEG                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
