*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_1_DATA                                        *
*----------------------------------------------------------------------*

TABLES:  lqua.


DATA: t_lqua LIKE TABLE OF lqua,
      W_LQUA LIKE LQUA.




DATA: i_outtab TYPE TABLE OF ZLE_GOOD_ALV_OUT.
DATA: w_outtab TYPE ZLE_GOOD_ALV_OUT.

* Workfelder
DATA: w_begda LIKE prel-begda,    "Beginn Selektionszeitraum
      w_endda LIKE prel-endda,    "Ende Selektionszeitraum
      w_seite TYPE p.             "Seitenzähler

* Hilfsfelder
DATA: h_text1(132).            "Hilfsfeld für Text
