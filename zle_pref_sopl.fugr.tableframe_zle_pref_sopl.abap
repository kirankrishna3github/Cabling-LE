*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZLE_PREF_SOPL
*   generation date: 09.09.2019 at 16:15:50
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZLE_PREF_SOPL      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
