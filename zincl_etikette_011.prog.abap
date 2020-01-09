*---------------------------------------------------------------------
*
* Struktur für Label Lageretiketten
*
*---------------------------------------------------------------------

  DATA: BEGIN OF ze011,
*        Etiketten-Typ
         etnr(25)          type c,
         etnr_end(1)       value '|',
*        Drucker-Name
         drnr(50)          type c,
         drnr_end(1)       value '|',
*        Anzahl Etiketten
         anze(3)           type n,
         anze_end(1)       value '|',
*        Name des Rechnungsempfängers (RE)
         name1             type name1_gp,
         name1_end(1)      value '|',
*        Langtext: Nummer der Charge / Anzahl Chargen / Versandtext intern
         tdline            type tdline,
         tdline_end(1)     value '|',
*        Bestellnummer des Kunden
         bstnk             type bstnk,
         bstnk_end(1)      value '|',
*        Projektnummer
         zzpronr           type zzpronr,
         zzpronr_end(1)    value '|',
*        Projektname
         prona             type zzprojname,
         prona_end(1)      value '|',
*        Kurztext der Kundenauftragsposition
         arktx             type arktx,
         arktx_end(1)      value '|',
*        Materialnummer
         matnr             type matnr,
         matnr_end(1)      value '|',
*        Kumulierte Auftragsmenge in VME: neu aus Versandtext I-VP (Position)
         kwmeng            type tdline,
***         kwmeng(13)        type c,
         kwmeng_end(1)     value '|',
*        Verkaufsmengeneinheit
         vrkme(3)          type c,
         vrkme_end(1)      value '|',
*        End-of-Line
         eol TYPE C,
        END OF ze011.
