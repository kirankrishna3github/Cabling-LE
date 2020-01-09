*----------------------------------------------------------------------*
*   INCLUDE ZXF06U06                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(IDOC_CONTRL_INDEX)
*"             VALUE(IDOC_DATA_INDEX)
*"             VALUE(DOCUMENT_LINE) TYPE  P
*"       EXPORTING
*"             VALUE(I_FIMSG) LIKE  FIMSG STRUCTURE  FIMSG
*"       TABLES
*"              IDOC_CONTRL STRUCTURE  EDIDC
*"              IDOC_DATA STRUCTURE  EDIDD
*"              DOCUMENT_DATA STRUCTURE  FTPOST1
*"              TAX_DATA STRUCTURE  FTTAX
*"              ADDITIONAL_DATA STRUCTURE  FTPOST1
*"----------------------------------------------------------------------
*SAP Stäfa, P. Huber, 5.4.2004
*Zahlungsbedingungsbedingungen für IV neu ermitteln aus empfangendem
*Kreditor Buchungskreisdaten.
data: begin of x_tax.                                       "Upgrade ECC
data:   fwste like fttax-fwste.                             "Upgrade ECC
data:   mwskz like fttax-mwskz.                             "Upgrade ECC
data:   bschl like fttax-bschl.                             "Upgrade ECC
data:   txjcd like fttax-txjcd.                             "Upgrade ECC
data:   hwste like fttax-hwste.                             "Upgrade ECC
data:   kschl like fttax-kschl.                             "Upgrade ECC
data:   h2ste like fttax-h2ste.                             "Upgrade ECC
data:   h3ste like fttax-h3ste.                             "Upgrade ECC
data:   sende like fttax-sende.                             "Upgrade ECC
data:   psosf like fttax-psosf.                             "Upgrade ECC
data: end of x_tax.                                         "Upgrade ECC
loop at tax_data.
 break-point tax_data-MWSKZ.
* message id 'ZZ' type 'i' number '999' with tax_data.      "Upgrade ECC
* Die Meldung oben lässt sich in UNICODE-Systemen so nicht  "Upgrade ECC
* mehr verwenden:                                           "Upgrade ECC
* Fehlermeldung:                                            "Upgrade ECC
* TAX_DATA lässt sich nicht in ein zeichenartiges Feld      "Upgrade ECC
* konvertieren                                              "Upgrade ECC
* Lösung:                                                   "Upgrade ECC
* Anstelle der vollständigen Struktur werden nur noch die   "Upgrade ECC
* zeichenartigen Felder der Struktur nach X_TAX übertragen  "Upgrade ECC
* und dann X_TAX übergeben.                                 "Upgrade ECC
 move-corresponding tax_data to x_tax.                      "Upgrade ECC
 message id 'ZZ' type 'i' number '999' with x_tax.          "Upgrade ECC
endloop.


if idoc_data_index = 1.

  loop at document_data where stype = 'P'
                        and   count = 1
                        and   fnam  = 'BSEG-ZTERM'.

    delete document_data.

    exit.
  endloop.
endif.
