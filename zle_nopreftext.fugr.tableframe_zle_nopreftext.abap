*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_NOPREFTEXT
*   generation date: 04.12.2017 at 08:29:08
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_NOPREFTEXT     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
