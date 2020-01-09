*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 14.03.2018 at 14:22:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZLE_SPVS_MAP....................................*
DATA:  BEGIN OF STATUS_ZLE_SPVS_MAP                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLE_SPVS_MAP                  .
CONTROLS: TCTRL_ZLE_SPVS_MAP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZLE_SPVS_MAP                  .
TABLES: ZLE_SPVS_MAP                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
