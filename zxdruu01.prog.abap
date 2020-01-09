*----------------------------------------------------------------------*
*   INCLUDE ZXDRUU01                                                   *
*----------------------------------------------------------------------*

*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  TABLES
*"      XRLDRC STRUCTURE  RLDRC
*"      XVBLKK STRUCTURE  VBLKK
*"      XVBLKP STRUCTURE  VBLKP
*"      XSERNR STRUCTURE  RISERLS
*"      XRLDRH STRUCTURE  RLDRH
*"      XRLDRI STRUCTURE  RLDRI
*"      XRLDRP STRUCTURE  RLDRP
*"      XRLDRU STRUCTURE  RLDRU
*"      XT329P STRUCTURE  T329P
*"      XRESB STRUCTURE  RESB
*"      XRLVEK STRUCTURE  RLVEK
*"      XREFTAB STRUCTURE  LREFN
*"      XLTHU STRUCTURE  LTHU
*"  CHANGING
*"     VALUE(C_T312S) LIKE  T312S STRUCTURE  T312S
*"     VALUE(C_LESCH) LIKE  RLDRU-DRUCK
*"     VALUE(C_LETASCH) LIKE  RLDRU-DRUCK
*"     VALUE(C_LEINH) LIKE  RLDRU-DRUCK
*"     VALUE(C_SINGLE) LIKE  RLDRU-DRUCK
*"     VALUE(C_PO) LIKE  RLDRU-DRUCK
*"     VALUE(C_SU) LIKE  RLDRU-DRUCK
*"     VALUE(C_SU) LIKE  RLDRU-DRUCK
*"     VALUE(C_MULTI) LIKE  RLDRU-DRUCK
*"     VALUE(C_MULTI) LIKE  RLDRU-DRUCK
*"     REFERENCE(C_HUMLA) LIKE  RLDRU-DRUCK OPTIONAL
*"----------------------------------------------------------------------
*Aenderungen:
*02112006; Transportauftragsdruck bei 491 Lieferungen nicht mehr sofort,
*          sondern im Job, welcher mittels Event aufberufen wird.
*          weitere Infos siehe unten
*          SAP Stäfa, Peter Huber
* 09.05.2011; Markus Raffeiner
* Konstante Mandantenabfragen elimineren
*----------------------------------------------------------------------

tables: mard, tsp03d, zmmdrucker, ltak.
data:   wlgpla like mard-lgpbe.
