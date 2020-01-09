*----------------------------------------------------------------------*
*   INCLUDE ZXF06U01                                                   *
*----------------------------------------------------------------------*
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(COMPANY_CODE) LIKE  BSEG-BUKRS
*"     VALUE(VENDOR) LIKE  BSEG-LIFNR
*"  EXPORTING
*"     VALUE(GL_ACCOUNT) LIKE  BSEG-SAKNR
*"     VALUE(GL_COMPANY_CODE) LIKE  BSEG-BUKRS
*"     VALUE(MSGID) LIKE  SY-MSGID
*"     VALUE(MSGNO) LIKE  SY-MSGNO
*"     VALUE(MSGV1) LIKE  SY-MSGV1
*"     VALUE(MSGV2) LIKE  SY-MSGV2
*"     VALUE(MSGV3) LIKE  SY-MSGV3
*"     VALUE(MSGV4) LIKE  SY-MSGV4
*"  TABLES
*"      EDI_FIELDS STRUCTURE  DFIELDS
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
