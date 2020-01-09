*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZOLD_NEW_MAT
*   generation date: 22.01.2016 at 09:59:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZOLD_NEW_MAT       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
