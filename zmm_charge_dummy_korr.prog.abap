REPORT ZMM_CHARGE_DUMMY_KORR .

tables: mch1.

parameters: test as checkbox.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



if sy-uname = 'I020676'.
  select * from mch1 where zzexidv = 'DUMMY-KORR'.
    write:/ mch1-zzexidv.
    if test = ' '.
      mch1-zzexidv = 'DUMMY-K'.
      modify mch1.
    endif.
  endselect.
endif.



* Test Programmberechtigung
  include zincl_progber.
