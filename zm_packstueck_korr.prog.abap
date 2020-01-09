REPORT zm_packstueck_korr .

TABLES: mch1, lqua, mara.


SELECT-OPTIONS: smatnr FOR mch1-matnr.

PARAMETERS:     plgnum LIKE lqua-lgnum DEFAULT '392' NO-DISPLAY,
                test AS CHECKBOX DEFAULT 'X'.



INITIALIZATION.

* Test Programmberechtigung
  PERFORM test_berechtigung.



  SELECT * FROM mch1 WHERE matnr IN smatnr.
    CHECK: mch1-zzexidv IS INITIAL.
    SELECT SINGLE * FROM mara WHERE matnr EQ mch1-matnr.
    CHECK: mara-ihivi = 'X'.

    SELECT SINGLE * FROM lqua WHERE matnr EQ mch1-matnr
                                AND lgnum EQ plgnum
                                AND lgtyp EQ '001'.


    IF sy-subrc = 0.
      IF test = ' '.
        mch1-zzexidv = 'DUMMY-KORR'.
        UPDATE mch1.
      ENDIF.
      WRITE:/ mch1-matnr, mch1-charg, mch1-zzrollnr, mch1-zzexidv,
              lqua-lgnum, lqua-werks, lqua-lgort, lqua-gesme.
    ENDIF.

  ENDSELECT.



* Test Programmberechtigung
  INCLUDE zincl_progber.
