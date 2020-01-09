*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_PREFTX_PRI
*   generation date: 15.01.2018 at 15:15:51
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_PREFTX_PRI     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
