*----------------------------------------------------------------------*
*   INCLUDE ZXDRUU01                                                   *
*----------------------------------------------------------------------*

*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       TABLES
*"              XRLDRC STRUCTURE  RLDRC
*"              XVBLKK STRUCTURE  VBLKK
*"              XVBLKP STRUCTURE  VBLKP
*"              XSERNR STRUCTURE  RISERLS
*"              XRLDRH STRUCTURE  RLDRH
*"              XRLDRI STRUCTURE  RLDRI
*"              XRLDRP STRUCTURE  RLDRP
*"              XRLDRU STRUCTURE  RLDRU
*"              XT329P STRUCTURE  T329P
*"              XRESB STRUCTURE  RESB
*"              XRLVEK STRUCTURE  RLVEK
*"              XREFTAB STRUCTURE  LREFN
*"       CHANGING
*"             VALUE(C_T312S) LIKE  T312S STRUCTURE  T312S
*"             VALUE(C_LESCH) LIKE  RLDRU-DRUCK
*"             VALUE(C_LETASCH) LIKE  RLDRU-DRUCK
*"             VALUE(C_LEINH) LIKE  RLDRU-DRUCK
*"             VALUE(C_SINGLE) LIKE  RLDRU-DRUCK
*"             VALUE(C_PO) LIKE  RLDRU-DRUCK
*"             VALUE(C_SU) LIKE  RLDRU-DRUCK
*"             VALUE(C_LABEL) LIKE  RLDRU-DRUCK
*"             VALUE(C_MULTI) LIKE  RLDRU-DRUCK
*"             VALUE(C_MULTI_REF) LIKE  RLDRU-DRUCK
*"             REFERENCE(C_HUMLA) LIKE  RLDRU-DRUCK OPTIONAL
*"----------------------------------------------------------------------

**tables: mard, tsp03d.
**data:   wlgpla like mard-lgpbe.
***01022005; Beginn
***Drucker in Abhängigkeit des Lagerplatzes MARD-LGPBE eritteln
***aus Kundentabelle ZMDRUCKER
***SAP Stäfa, Peter Huber
**
*if sy-mandt = '200'.
*
*  read table xrldrh with key lgnum = '700'
*                             bwart = '101'.
**nur bei Lagernr. 700 und Bew.art 101
*  if sy-subrc = 0.
*    read table xrldri with key werks = '7000'
*                         lgort = '7000'.
**nur bei Lagerort 7000
*    if sy-subrc = 0.
*      select single * from mard where matnr eq xrldri-matnr
*                                  and werks eq xrldri-werks
*                                  and lgort eq xrldri-lgort.
*      if sy-subrc = 0 and not mard-lgpbe is initial.
*        clear: wlgpla.
*        write mard-lgpbe+0(2) to wlgpla.
*        select single * from zmdrucker where werks eq xrldri-werks
*                                         and lgort eq xrldri-lgort
*                                         and lgpbe eq wlgpla.
*        if sy-subrc = 0.
**Kurzname Drucker ermitteln
*          select single * from tsp03d where name = zmdrucker-rspolname
*.
*
*          loop at xrldrc.
*            xrldrc-ldest = tsp03d-padest.
*            modify xrldrc.
*          endloop.
*        endif.
*      endif.
*    endif.
*  endif.
*endif.
**01022005; Ende
