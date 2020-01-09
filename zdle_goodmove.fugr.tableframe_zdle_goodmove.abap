*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZDLE_GOODMOVE
*   generation date: 13.05.2010 at 01:31:04 by user I020676
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZDLE_GOODMOVE      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
