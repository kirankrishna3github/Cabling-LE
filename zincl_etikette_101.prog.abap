*Uebergabestruktur Etikette Gummi bei Warenbewegungen LE Lagerverwaltung

  DATA: BEGIN OF ze101,
         begin(1)  VALUE '|',
* Etikettennr. (8)
         etnr(8)      TYPE c,
         etnr_end(1)  VALUE '|',
* Drucker-Name (50)
         drnr(50)     TYPE c,
         drnr_end(1)  VALUE '|',
* Anzahl Etiketten (3)
         anze(3)      TYPE n,
         anze_end(1)  VALUE '|',
* Wareneingangsdatum,
         wdatu(10),
         wdatu_end(1)  VALUE '|',
* Werk (4)
         werks         like mseg-werks,
         werks_end(1)  value '|',
* Lagernummer (3)
         lgnum         like ltap-lgnum,
         lgnum_end(1)  value '|',
* Lagertyp (3)
         lgtyp         like ltap-nltyp,
         lgtyp_end(1)  value '|',
* Lagerort (4)
         lgort         like ltap-lgort,
         lgort_end(1)  value '|',
* Nach-Lagerplatz
         nlpla(10)     TYPE c,
         nlpla_end(1)  VALUE '|',
* Materialbelegnummer
         mblnr(15)     TYPE c,
         mblnr_end(1)  VALUE '|',
* Materialbelegposition
         mbpos(15)     TYPE c,
         mbpos_end(1)  VALUE '|',
* Materialnummer (18)
         matnr         like mseg-matnr,
         matnr_end(1)  value '|',
* Materialkurztext (40)
         maktx         like makt-maktx,
         maktx_end(1)  VALUE '|',
* Charge (10)
         charg         like mseg-charg,
         charg_end(1)  VALUE '|',
* Menge
         menge(13)     type c,
         menge_end(1)  VALUE '|',
* Mengeneinheit (3)
         meins         like mseg-meins,
         meins_end(1)  VALUE '|',
* Bestellungsnr.
         ebeln        like mseg-ebeln,
         ebeln_end    VALUE '|',
* Bestellposnr.
         ebelp        like mseg-ebelp,
         ebelp_end    VALUE '|',
* Fertigungsauftragsnummer
         aufnr        like mseg-aufnr,
         aufnr_end    VALUE '|',
* Transportauftragsnummer (10)
         tanum        like ltap-tanum,
         tanum_end(1)  VALUE '|',
* Nach-Lagereinheitennummer
         nlenr        like ltap-nlenr,
         nlenr_end(1)  VALUE '|',
* Verfalldatum
         vfdat(10),
         vfdat_end     VALUE '|',
* Maschine / Presse
         masch(10)    type c,
         masch_end     VALUE '|',
* Vorgang (Etikettendruck DRX)
         vorga(5)     type c,
         vorga_end     VALUE '|',
* End-of-Line
         eol TYPE C,  "end of line     "smart(M): 2010-08-05
        END OF ze101.
