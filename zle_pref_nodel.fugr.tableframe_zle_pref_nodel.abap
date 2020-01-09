*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_PREF_NODEL
*   generation date: 20.03.2018 at 15:41:09
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_PREF_NODEL     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
