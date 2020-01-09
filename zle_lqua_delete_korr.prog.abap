REPORT ZLE_LQUA_DELETE_KORR .

tables: lagp.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



select-options: slgnum for lagp-lgnum.

delete from lagp client specified where lgnum in slgnum.



* Test Programmberechtigung
  include zincl_progber.
