*&---------------------------------------------------------------------*
*& Report  ZLE_GOODMOVE_1                                              *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*


************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #101 #166
************************************************************************

REPORT  zle_goodmove_1.

INCLUDE zle_goodmove_1_data.
INCLUDE zle_goodmove_1_sel.
INCLUDE zle_goodmove_1_alv.
INCLUDE zle_goodmove_1_f00.




START-OF-SELECTION.


  PERFORM select_data.

  PERFORM output_selection.



END-OF-SELECTION.
