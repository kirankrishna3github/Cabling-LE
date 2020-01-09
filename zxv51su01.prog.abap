*&---------------------------------------------------------------------*
*&  Include           ZXV51SU01
*&---------------------------------------------------------------------*

*****data: lv_lifnr type lifnr.
*****data: lv_nve type char20.
*****
****** Ermitteln NVE-Nummer für gewisse Spediteure, z.B. Dachser
****** diese Nummer wird in das Feld EXIDV2 der HU übertragen
*****  clear lv_lifnr.
*****
*****  if cs_hu_header-exidv2 is initial.
*****    CALL FUNCTION 'Z_LE_GET_NVE_DACHSER'
*****      EXPORTING
*****        IS_HU_HEADER          = CS_HU_HEADER
*****        IS_GENERAL_DATA       = IS_GENERAL_DATA
*****     IMPORTING
*****        EV_NVE                = lv_nve.
*****
*****    cs_hu_header-exidv2 = lv_nve.
*****
*****  endif. "cs_hu_header-exidv2
