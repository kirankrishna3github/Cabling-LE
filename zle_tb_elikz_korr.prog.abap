REPORT ZLE_TB_ELIKZ_KORR .

tables: ltbp.

parameters:       plgnum like ltbp-lgnum obligatory,
                  pmatnr like ltbp-matnr obligatory.


select-options:   stbnum for ltbp-tbnum,
                  stbpos for ltbp-tbpos.

parameters:       ptest as checkbox default 'X'.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



select * from ltbp where lgnum eq plgnum
                     and matnr eq pmatnr
                     and tbnum in stbnum
                     and tbpos in stbpos
                     and elikz eq ' '.

  write:/ ltbp-lgnum, ltbp-matnr, ltbp-tbnum, ltbp-tbpos.

  if ptest = ' '.
    ltbp-elikz = 'X'.
    modify ltbp.
  endif.

endselect.



* Test Programmberechtigung
  include zincl_progber.
