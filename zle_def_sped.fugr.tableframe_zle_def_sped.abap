*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_DEF_SPED
*   generation date: 06.05.2014 at 11:13:44 by user ARNOLDP1
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_DEF_SPED       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
