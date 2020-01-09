*----------------------------------------------------------------------*
*   INCLUDE ZXVBZU01                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(X_BNCOM) LIKE  BNCOM STRUCTURE  BNCOM OPTIONAL
*"       EXPORTING
*"             VALUE(CUST_NO_INTERNAL)
*"       CHANGING
*"             VALUE(NR_RANGE_NR) LIKE  INRI-NRRANGENR DEFAULT '01'
*"             VALUE(OBJECT) LIKE  INRI-OBJECT DEFAULT 'BATCH_CLT'
*"             VALUE(SUBOBJECT) LIKE  INRI-SUBOBJECT DEFAULT SPACE
*"             VALUE(TOYEAR) LIKE  INRI-TOYEAR DEFAULT '0000'
*"             VALUE(MESSAGE_WHEN_AUTO) LIKE  AM07M-XSELK
*"                             DEFAULT SPACE
*"             VALUE(MESSAGE_DONE) LIKE  AM07M-XSELK DEFAULT SPACE
*"       EXCEPTIONS
*"              CANCELLED
*"----------------------------------------------------------------------
************************************************************************
* Aenderungen:
* 09.05.2011; Markus Raffeiner: Konstante Mandantenabfragen elimineren

************************************************************************

************************************************************************
*Chargennummervergabe im Wareneingang zur Bestellung
*Sofern der Lieferant eine Konzerntochter ist oder in der Kunden-
*tabelle zmm_kred_handl als aktiv gekennzeichnet ist,
*muss die Charge vom Lieferwerk übernommen (manuell) werden.
*Andernfalls: fehlende Trommelnr. / Packstücknr.
*
*SAP Stäfa, 06.01.2005; Peter Huber

  tables: t076s,          "Lieferanten in der internen Verrechnung EDI
          zmm_Kred_handl. "Kreditoren im Handlagerablauf

  select single * from zmm_kred_handl where
                       werks  = x_bncom-werks and
                       zaktiv = 'X'.
  if sy-subrc = 0.

    check: x_bncom-bstyp = 'F'.
    check: x_bncom-kdauf is initial,   "keine SD Ablauf
           x_bncom-aufnr is initial,   "kein  CO Ablauf
           sy-tcode cs 'MIGO'.         "nur bei MIGO Transaktionen

    select single * from zmm_kred_handl where
                       werks  = x_bncom-werks and
                       zaktiv = 'X' and
                       lifnr  = x_bncom-lifnr.

    if sy-subrc = 0.
      CUST_NO_INTERNAL = 'X'.  "manuelle Chargeneingabe
      exit.

    else.
*prüfen, ob Lieferant in der internen Verrechnung verwendung findet . .
      select single * from t076s where parart = 'LI'
                                   and konto  = x_bncom-lifnr.

      if sy-subrc = 0.
        CUST_NO_INTERNAL = 'X'.  "manuelle Chargeneingabe
        exit.
      endif.
    endif.
  endif.
