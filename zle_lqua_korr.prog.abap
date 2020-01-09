*&---------------------------------------------------------------------*
*& Report  ZLE_LQUA_KORR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZLE_LQUA_KORR.

tables: lqua.

select-options: slgnum for lqua-lgnum obligatory,
                slqnum for lqua-lqnum,
                sletyp for lqua-letyp obligatory default 'E1'.
parameters:     pupdate as checkbox.

if sy-uname ne 'I020676'.
  exit.
endif.

select * from lqua where lgnum in slgnum
                     and lqnum in slqnum
                     and letyp in sletyp.
  check: not lqua-letyp is initial.
  write:/ lqua-lgnum, lqua-lqnum, lqua-matnr, lqua-letyp.

  if pupdate = 'X'.
    clear: lqua-letyp.
    update lqua.
    write: ' - LETYP gelöscht'.
  endif.

endselect.
