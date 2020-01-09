*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZEDI_SPED_NKI
*   generation date: 27.10.2016 at 11:22:24
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZEDI_SPED_NKI      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
