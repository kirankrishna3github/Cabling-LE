*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_SPVS_MAP
*   generation date: 14.03.2018 at 14:22:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_SPVS_MAP       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
