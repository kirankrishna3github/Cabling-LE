*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_PREF_PLANT
*   generation date: 08.11.2017 at 18:01:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_PREF_PLANT     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
