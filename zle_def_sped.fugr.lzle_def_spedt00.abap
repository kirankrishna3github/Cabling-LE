*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.05.2014 at 11:01:09 by user ARNOLDP1
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_DEF_SPED....................................*
DATA:  BEGIN OF STATUS_ZLE_DEF_SPED                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_DEF_SPED                  .
CONTROLS: TCTRL_ZLE_DEF_SPED
            TYPE TABLEVIEW USING SCREEN '0100'.
*.........table declarations:.................................*
TABLES: *ZLE_DEF_SPED                  .
TABLES: ZLE_DEF_SPED                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
