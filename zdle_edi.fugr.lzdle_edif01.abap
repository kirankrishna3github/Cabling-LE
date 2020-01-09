*----------------------------------------------------------------------*
***INCLUDE LZDLE_EDIF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_NVE_MIT_PRUEFZIFFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PV_NVE_IN  text
*      <--PV_NVE_OUT  text
*----------------------------------------------------------------------*
FORM GET_NVE_MIT_PRUEFZIFFER  USING    PV_NVE_IN TYPE CHAR20
                              CHANGING PV_NVE_OUT TYPE CHAR20.

  data: lv_counter type i.
  data: lv_offset type i.
  data: lv_ziffer type i.
  data: lv_rest type i.
  data: lv_faktor type i.
  data: lv_strlen type i.
  data: lv_summe type i.
  data: lv_summe_char type char5.
  data: lv_pruefziffer type c.

  clear pv_nve_out.

* ---------------------------------------------------------------------
* Die Prüfziffer am Ende hinzugefügt und errechnet sich nach folgendem
* Verfahren:
* Zunächst werden die Nutzziffern (die hinteren 17 Ziffern ohne die
* führenden beiden Ziffern) abwechselnd von rechts nach links (also von
* hinten nach vorne) mit dem Faktor 3 und dem Faktor 1 multipliziert
* und die Ergebnisse addiert.
*
* Ist die letzte Stelle dieser Summe eine 0, so ist auch die Prüfziffer 0,
* sonst die Differenz aus 10 und der letzten Ziffer (10−x).

* Als Beispiel die Berechnung der Prüfziffer der NVE 0034032685005480001
* Die Prüfziffer lautet also 7, die vollständige 20-stellige NVE lautet
* dementsprechend 00340326850054800017
* ---------------------------------------------------------------------

  clear: lv_counter, lv_faktor, lv_summe.

  lv_offset = 18.

  Do 17 times.

    lv_counter = lv_counter + 1.

    lv_rest = lv_counter mod 2.

    if lv_rest eq 0.
      lv_faktor = 1.
    else.
      lv_faktor = 3.
    endif.

*   Ziffer von hinten holen
    lv_ziffer = pv_nve_in+lv_offset(1).
    lv_summe = lv_summe + lv_ziffer * lv_faktor.

*   Shift nack links um eine Stelle
    lv_offset = lv_offset - 1.

  enddo.

* letzte Ziffer der Summe ermitteln
  lv_summe_char = lv_summe.
  condense lv_summe_char no-gaps.

  lv_strlen = strlen( lv_summe_char ).
  check lv_strlen gt 0.

  lv_offset = lv_strlen - 1.

  lv_ziffer = lv_summe_char+lv_offset(1).

  if lv_ziffer eq 0.
    lv_pruefziffer = '0'.
  else.
    lv_pruefziffer = 10 - lv_ziffer.
  endif.

* Prüfziffer ans Ende setzen
  concatenate pv_nve_in lv_pruefziffer into pv_nve_out in character mode.

ENDFORM.                    " GET_NVE_MIT_PRUEFZIFFER
