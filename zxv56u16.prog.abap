*----------------------------------------------------------------------*
*   INCLUDE ZXV56U16                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_STATUS) LIKE  VTTK-STTRG
*"     VALUE(I_NEW_STATUS_DATE) LIKE  VTTK-DTABF OPTIONAL
*"     VALUE(I_NEW_STATUS_TIME) LIKE  VTTK-UZABF OPTIONAL
*"     VALUE(I_TVTK) LIKE  TVTK STRUCTURE  TVTK
*"     VALUE(OPT_DIALOG) LIKE  RV56A-SELKZ DEFAULT 'X'
*"  TABLES
*"      C_XVTTK STRUCTURE  VTTKVB
*"      C_YVTTK STRUCTURE  VTTKVB
*"      C_XVTTP STRUCTURE  VTTPVB
*"      C_YVTTP STRUCTURE  VTTPVB
*"      C_XVTTS STRUCTURE  VTTSVB
*"      C_YVTTS STRUCTURE  VTTSVB
*"      C_XVTSP STRUCTURE  VTSPVB
*"      C_YVTSP STRUCTURE  VTSPVB
*"      C_XVBPA STRUCTURE  VBPAVB
*"      C_YVBPA STRUCTURE  VBPAVB
*"      C_XVBADR STRUCTURE  SADRVB
*"      C_YVBADR STRUCTURE  SADRVB
*"      I_XTRLK STRUCTURE  VTRLK
*"      I_XTRLP STRUCTURE  VTRLP
*"  CHANGING
*"     VALUE(C_XVTTK_WA) LIKE  VTTKVB STRUCTURE  VTTKVB
*"  EXCEPTIONS
*"      ERROR
*"----------------------------------------------------------------------
