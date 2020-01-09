*&---------------------------------------------------------------------*
*& Report  ZMM_EC_PCA_TABLE_SAVE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_EC_PCA_WM_BELEGE_DELE.


tables: ltbk,
        ltbp.

data: counter(8) type n.

parameters: plgnum type lgnum obligatory default '391',
            pbdatu type bdatu obligatory.

select-options: stbnum for ltbk-tbnum.


parameters: ptest radiobutton group 0,
            pmodi radiobutton group 0.


initialization.
  pbdatu = sy-datum - 90.

start-of-selection.

*******************************************************************************

    select * from ltbk where lgnum = plgnum
                         and tbnum in stbnum
                         and bdatu le pbdatu
                         and statu ne 'E'.

      if pmodi = 'X'.
        ltbk-statu = 'E'.
        modify ltbk.
      endif.

      write:/ ltbk-lgnum,
              ltbk-tbnum,
              ltbk-statu,
              ltbk-bdatu.

      counter = counter + 1.

      select * from ltbp where lgnum = ltbk-lgnum
                           and tbnum = ltbk-tbnum
                           and elikz ne 'X'.

                           write:/ '   ',
                                   ltbp-tbpos,
                                   ltbp-elikz.

      if pmodi = 'X'.
        ltbp-elikz = 'X'.
        modify ltbp.
      endif.

      endselect.
    endselect.

write:/ 'Anzahl Datensätze LTBK ', counter.

*******************************************************************************
