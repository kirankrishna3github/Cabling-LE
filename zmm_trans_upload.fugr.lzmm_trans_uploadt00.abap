*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 25.11.2013 at 10:23:34 by user I020676
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_TRANS_UPLOAD................................*
DATA:  BEGIN OF STATUS_ZMM_TRANS_UPLOAD              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_TRANS_UPLOAD              .
CONTROLS: TCTRL_ZMM_TRANS_UPLOAD
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_TRANS_UPLOAD              .
TABLES: ZMM_TRANS_UPLOAD               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
