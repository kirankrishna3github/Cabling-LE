* ZINCL_ETIKETTE_001.
*
* Etikette zu ABAP ZM_ETIKDRUCK_04

  DATA: BEGIN OF ze001,
* Beginn
         begin_end(1)  VALUE '|',
* Etiketten-Typ
         etnr(8)      TYPE c,
         etnr_end(1)  VALUE '|',
* Drucker-Name
         drnr(50)     TYPE c,
         drnr_end(1)  VALUE '|',
* Anzahl Druckaufträge
         anz(3)       TYPE N VALUE '001',
         ANZ_end(1)  VALUE '|',
* Kunde Name1
         name1(35)     TYPE c,
         name1_end(1)  VALUE '|',
* Kunde Postleitzahl
         pstlz(10)     TYPE c,
         pstlz_end(1)  VALUE '|',
* Kunde Ort
         ort01(35)     TYPE c,
         ort01_end(1)  VALUE '|',
* Kunde Land
         land1(3)     TYPE c,
         land1_end(1)  VALUE '|',
* Auftragsnummer
         vbeln(10)    TYPE c,
         vbeln_end(1)  VALUE '|',
* Auftragsposition
         posnr(6)     TYPE c,
         posnr_end(1)  VALUE '|',
* Auftragspos.menge
         kwmeng(15)     TYPE c,
         kwmeng_end(1)  VALUE '|',
* Auftragspos.mengeneinheit
         vrkme(3)     TYPE c,
         vrkme_end(1)  VALUE '|',
* Bestellungnummer Kunde
         bstkd(23)    TYPE c,
         bdtkd_end(1)  VALUE '|',
* Bestellposition Kunde
         posex(6)    TYPE c,
         posex_end(1)  VALUE '|',
* Vertriebstext Zeile 1
         text1(25)   TYPE c,
         text1_end(1)  VALUE '|',
* Vertriebstext Zeile 2
         text2(25)   TYPE c,
         text2_end(1)  VALUE '|',
* Vertriebstext Zeile 3
         text3(25)   TYPE c,
         text3_end(1)  VALUE '|',
* Vertriebstext Zeile 4
         text4(25)   TYPE c,
         text4_end(1)  VALUE '|',
* End-of-Line
         eol TYPE C,  "end of line          "smart(M): 2010-08-05
        END OF ze001.
