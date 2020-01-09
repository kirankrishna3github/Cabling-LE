*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_DATA                                          *
*----------------------------------------------------------------------*

* Globale definitionen für alle Module

TABLES: mlgn,
        mara,
        mast,
        ekko,
        ekpo,
        jest,
        aufk.


DATA:   g_scode(1) TYPE c.
DATA: g_scanval(50) TYPE c.

DATA: scantitel(50) TYPE c.
DATA: f_input(50) TYPE c.

data: G_mestxt(70) type c.

*  Data zu Batchinput
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.


*Felder zum Wareneingang ab Bestellung

TABLES:   zle_gm_chargen_p.

DATA: t_imseg LIKE TABLE OF imseg.
DATA: w_imseg LIKE imseg.
DATA: g_menge LIKE imseg-menge.

* Felder zu Wareneingang ab Fert. Auftrag

  DATA: G_caufv LIKE caufv.


* Felder für Produktionsversorgung

Tables: LQUA.

DATA: t_lt01_batch TYPE TABLE OF zle_gm_s_lt01.
DATA: w_lt01_batch TYPE zle_gm_s_lt01.


* Felder für Rohmat Rücklagerung (Rest nach Ferigung)

Data: G_IWEF(14) type c.
Data: G_SWEF(14) type c.
Data: G_DWEF(14) type c.
Data: G_IWEC(14) type c.
Data: G_SWEC(14) type c.
Data: G_DWEC(14) type c.
Data: G_SPVBC(14) type c.
Data: G_IPVBC(14) type c.

data:  G_AUFNR like afko-aufnr.
data:  g_lgpla like lqua-lgpla.

data: g_lqua like lqua.
