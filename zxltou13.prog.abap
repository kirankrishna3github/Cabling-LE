*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU13                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Globale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_LTAK) LIKE  LTAK STRUCTURE  LTAK
*"             VALUE(I_LTAP) LIKE  LTAP STRUCTURE  LTAP
*"             VALUE(I_MLVS) LIKE  MLVS STRUCTURE  MLVS
*"             VALUE(I_MGEF) LIKE  MGEF STRUCTURE  MGEF
*"             VALUE(I_T333) LIKE  T333 STRUCTURE  T333
*"             VALUE(I_T340D) LIKE  T340D STRUCTURE  T340D
*"             VALUE(I_VORGA) LIKE  LTAP-VORGA
*"             VALUE(I_EINML) LIKE  RL03T-EINML
*"       TABLES
*"              T_LTAPE STRUCTURE  LTAPE
*"       CHANGING
*"             VALUE(C_LTKZE) LIKE  MLVS-LTKZE
*"             VALUE(C_ANZL1) LIKE  RL03T-ANZL1
*"             VALUE(C_LETY1) LIKE  RL03T-LETY1
*"             VALUE(C_LMEN1) LIKE  RL03T-LMEN1
*"             VALUE(C_LMEL1) LIKE  RL03T-LMEL1
*"             VALUE(C_LGTY1) LIKE  RL03T-LGTY1
*"             VALUE(C_LGBE1) LIKE  RL03T-LGBE1
*"             VALUE(C_ANZL2) LIKE  RL03T-ANZL2
*"             VALUE(C_LETY2) LIKE  RL03T-LETY2
*"             VALUE(C_LMEN2) LIKE  RL03T-LMEN2
*"             VALUE(C_LMEL2) LIKE  RL03T-LMEL2
*"             VALUE(C_LGTY2) LIKE  RL03T-LGTY2
*"             VALUE(C_LGBE2) LIKE  RL03T-LGBE2
*"----------------------------------------------------------------------
* CSJULI2010:
* Einlagerungsstrategie Leerplatz bei Eingängen ins Lagerort 6000
* Lagertyp 600 mit ermittlung der Lagerortes
* 09.05.2011; Markus Raffeiner
* Konstante Mandantenabfragen elimineren

DATA: lt_ltape TYPE zst_ltape,
      ls_ltape TYPE ltape,
      ls_rl03t TYPE rl03t.
