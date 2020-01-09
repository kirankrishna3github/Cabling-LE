
*---------------------------------------------------------------------*
*        INCLUDE RLVSDR91   Formroutine für Sammelliste-Druck         *
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*        FORM SAMMELLISTE_DRUCKEN_SAPSCRIPT                           *
*---------------------------------------------------------------------*
*        Anstoßen des Sammeldrucks über SAPSCRIPT                     *
*---------------------------------------------------------------------*
*20031215, Begin, SAP Stäfa, P. Huber
*Globale Datendeklaration
tables: vbak, vbrp, vbup, vbap, lips, mvke, marc.

data: begin of ivbap occurs 0.
        include structure vbap.
data: end of ivbap.

data: begin of imvke occurs 0.
        include structure mvke.
data: end of imvke.

data: begin of imarc occurs 0.
        include structure marc.
data: end of imarc.

data: wvsolm like ltap-vsolm,
      wmeinh like marm-meinh,
      wposnr(6),
      wpackung like marc-bstrf.

data: tdname like vbdka-tdname,
      tdnamep(16).
*20031215, Ende, SAP P Stäfa, P. Huber

FORM SAMMELLISTE_DRUCKEN_SAPSCRIPT.


*Transaktion LT31
if sy-tcode = 'LT31'.
clear: wposnr, tdnamep.
 write '000000' to wposnr.
 write ltap-tapos to wposnr+1(4).
 concatenate ltap-nlpla wposnr into tdnamep.

*Prüfzertikikat
  refresh: imvke.
  select single * from mvke where vkorg = vbak-vkorg
                              and vtweg = vbak-vtweg
                              and matnr = ltap-matnr.
   move-corresponding mvke to imvke.

*Verpackungseinheit
  clear: wpackung, marc.
  refresh: imarc.
  select single * from marc where matnr eq ltap-matnr
                                and werks eq ltap-werks.

  move-corresponding marc to imarc.
  if not marc-bstrf is initial.
    wpackung = ltap-vsolm / marc-bstrf.
  else.
    wpackung = ltap-vsolm.
  endif.


*Transaktion LT03
elseif sy-tcode = 'LT03'.
 clear: wposnr, tdnamep.
 write '000000' to wposnr.
 write ltap-tapos to wposnr+1(4).
 concatenate ltap-nlpla wposnr into tdnamep.

*Prüfzertikikat
  clear: mvke.
  refresh: imvke.
  select single * from mvke where vkorg = vbak-vkorg
                              and vtweg = vbak-vtweg
                              and matnr = ltap-matnr.
   move-corresponding mvke to imvke.

*Verpackungseinheit
  clear: marc, wpackung.
  refresh: imarc.
  select single * from marc where matnr eq ltap-matnr
                                and werks eq ltap-werks.

  move-corresponding marc to imarc.
  if not marc-bstrf is initial.
    wpackung = ltap-vsolm / marc-bstrf.
  else.
    wpackung = ltap-vsolm.
  endif.
  endif.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'MAIN'
            WINDOW  = 'MAIN'.

*               FUNCTION  = 'APPEND'.

ENDFORM.

*---------------------------------------------------------------------*
*        FORM SAMMELLISTE_KOPF_SAPSCRIPT                              *
*---------------------------------------------------------------------*
*        Aufbereitung der Formulardaten für Sammelliste, die am       *
*        Anfang des Formulars angedruckt werden ( Kopf )              *
*---------------------------------------------------------------------*
*20031215, Begin, SAP Stäfa, P. Huber
*Globale Datendeklaration
tables: kna1, likp,  T173T, tinct, usr03, adrc, vbpa.

data: begin of iWE occurs 0.
        include structure kna1.
data: end of iwe.

data: begin of iAG occurs 0.
        include structure kna1.
data: end of iAG.
*---------------------------------------------------------------------*
*       FORM SAMMELLISTE_KOPF_SAPSCRIPT                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SAMMELLISTE_KOPF_SAPSCRIPT.

*......................................................................
*20031215, SAP Stäfa, P. Huber
*Zusatzdaten
  select * from lips where vbeln eq ltak-vbeln.
    exit.
  endselect.

  clear: iwe, iag, imvke.


  "Warenempfänger
  select single * from likp where vbeln eq ltak-vbeln.
  select single * from kna1 where kunnr eq likp-kunnr.
  move-corresponding kna1 to iwe.


  "Versanddaten
  select single * from T173T where vsart eq likp-vsart
                               and spras eq kna1-spras.
  select single * from tinct where inco1 eq likp-inco1
                               and spras eq kna1-spras.

  "Auftraggeber
  select single * from vbak where vbeln eq lips-vgbel.
  select single * from kna1 where kunnr eq vbak-kunnr.
  move-corresponding kna1 to iag.
  tdname = likp-vbeln.

  "Warenempfängeradresse aus Kundenauftag
  select single * from vbpa where vbeln eq lips-vgbel
                              and parvw eq 'WE'
                              and posnr eq '000000'.

  select single * from adrc where addrnumber eq vbpa-adrnr.


*20031215, Ende, SAP Stäfa, P. Huber

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'KOPF'
            WINDOW  = 'KOPF'.



*20031215, Ende, SAP Stäfa, P. Huber

*........Fusszeile.....................................................

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            ELEMENT = 'FUSS'
            WINDOW  = 'FUSS'.

  if syst-sysid ne 'PC2'.
  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
           ELEMENT = 'SYSTEM'
           WINDOW  = 'SYSTEM'.
  endif.

ENDFORM.
