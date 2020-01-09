
*Uebergabestruktur Etikette Gummi bei Wareneingang

  DATA: BEGIN OF ze100,
         begin(1)  VALUE '|',
* Etikettennr.
         etnr(8)      TYPE c,
         etnr_end(1)  VALUE '|',
* Drucker-Name
         drnr(50)     TYPE c,
         drnr_end(1)  VALUE '|',
* Anzahl Etiketten
         anze(3)      TYPE n,
         anze_end(1)  VALUE '|',
* Materialnummer (18)
         matnr         like mseg-matnr,
         matnr_end(1)  value '|',
* Materialkurztext (40)
         mktx         like makt-maktx,
         mktx_end(1)  VALUE '|',
* Materialbelegnummer, -position
         mbnr(15)     TYPE c,
         mbnr_end(1)  VALUE '|',
* Charge (10)
         char         like mseg-charg,
         char_end(1)  VALUE '|',
* Charge Verfalldatum
         vfdat(10)    type c,
         vfdat_end(1) VALUE '|',
* Lieferantenchargenummer
         licha        like mcha-licha,
         licha_end(1)  VALUE '|',
* Wareneingangsdatum, -zeit
         buch(21)     TYPE c,
         buch_end(1)  VALUE '|',
* Menge
         meng(13)     type c,
         meng_end(1)  VALUE '|',
* Mengeneinheit (3)
         mebe         like mseg-meins,
         mebe_end(1)  VALUE '|',
* Lieferant
         lifnr        like mseg-lifnr,
         lifnr_end(1) VALUE '|',
* Lieferant Name 1
         name1         like lfa1-name1,
         name1_end(1)  VALUE '|',
* Lagerplatz
         lgpbe(10)     TYPE c,
         lgpbe_end(1)  VALUE '|',
* Bestellnr.
         ebeln        like ekpo-ebeln,
         ebeln_end    VALUE '|',
* Bestellposnr.
         ebelp        like ekpo-ebelp,
         ebelp_end    VALUE '|',
* Transportbedarfsnummer (10)
         tbnr         like mseg-tbnum,
         tbnr_end(1)  VALUE '|',
* Lagerplatz aus WM
         lagpl(14)     TYPE c,
         l_lagpl(1)    VALUE '|',
* End-of-Line
         eol TYPE C,  "end of line           "smart(M): 2010-08-05
        END OF ze100.
