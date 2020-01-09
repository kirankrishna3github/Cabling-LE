REPORT Z200_LE_LQUA_KORR .

tables: lqua.

select-options: slgnum for lqua-lgnum obligatory default '800',
                slgtyp for lqua-lgtyp obligatory default '998',
                slgpla for lqua-lgpla obligatory default 'AUFNAHME',
                smatnr for lqua-matnr.
parameter: ptest as checkbox default 'X'.

if sy-uname ne 'I020676'. exit. endif.

select * from lqua where  lgnum in slgnum
                     and lgtyp in slgtyp
                     and lgpla in slgpla
                     and matnr in smatnr.

  write:/ lqua-matnr, lqua-lgtyp, lqua-gesme.
  if ptest = ' '.
    delete lqua.
    write: '- delete'.
  endif.
endselect.
