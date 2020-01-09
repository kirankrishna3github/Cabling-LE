*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMM_TRANS_MSEG
*   generation date: 26.11.2013 at 14:05:09 by user I020676
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMM_TRANS_MSEG     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
