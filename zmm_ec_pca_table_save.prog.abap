*&---------------------------------------------------------------------*
*& Report  ZMM_EC_PCA_TABLE_SAVE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_EC_PCA_TABLE_SAVE.


tables: zmkdisp1,
        zmkdisp1_save,
        lqua,
        zlqua_save,
        mch1,
        zmch1_save,
        mchb,
        zmchb_save,
        vbak,
        zvbak_save,
        vbap,
        zvbap_save,
        ltak,
        zltak_save,
        ltbk,
        zltbk_save,
        ltap,
        zltap_save,
        ltbp,
        zltbp_save.


data: counter(8) type n.


parameters: plauf(20) type c.

parameters: pins radiobutton group 0,
            pdel radiobutton group 0.

parameters: pall      radiobutton group 1,
            pdisp     radiobutton group 1,
            plqua     radiobutton group 1,
            pmch1     radiobutton group 1,
            pmchb     radiobutton group 1,
            pvbak     radiobutton group 1,
            pvbap     radiobutton group 1,
            pltak     radiobutton group 1,
            pltbk     radiobutton group 1.

initialization.
  concatenate sy-datum '-' sy-uzeit into plauf.

start-of-selection.

*******************************************************************************
  if pins = 'X'. "insert

*Save Kabeldisposition
    if pdisp = 'X' or pall = 'X'.
      select * from zmkdisp1.
        move-corresponding zmkdisp1 to zmkdisp1_save.
        zmkdisp1_save-lauf = plauf.
        insert zmkdisp1_save.
        counter = counter + 1.
      endselect.
      write: 'Total Anzahl Datensätze an zmkdisp1_save übergeben: ', counter.
    endif.

*Save Lagerquanten
    if plqua = 'X'  or pall = 'X'.
      select * from lqua.
        move-corresponding lqua to zlqua_save.
        zlqua_save-lauf = plauf.
        insert zlqua_save.
        counter = counter + 1.
      endselect.
      write: 'Total Anzahl Datensätze an zlqua_save übergeben: ', counter.
    endif.

*Save Chargen
    if pmch1 = 'X' or pall = 'X'.
      select * from mch1.
        move-corresponding mch1 to zmch1_save.
        zmch1_save-lauf = plauf.
        insert zmch1_save.
        counter = counter + 1.
      endselect.
      write: 'Total Anzahl Datensätze an zlqua_save übergeben: ', counter.
    endif.

*Save Chargen
    if pmchb = 'X' or pall = 'X'.
      select * from mchb.
        move-corresponding mchb to zmchb_save.
        zmchb_save-lauf = plauf.
        insert zmchb_save.
        counter = counter + 1.
      endselect.
      write: 'Total Anzahl Datensätze an zlqua_save übergeben: ', counter.
    endif.

*Ssve Kundenaufträge
    if pvbak = 'X' or pall = 'X'.
      select * from vbak.
        move-corresponding vbak to zvbak_save.
        zvbak_save-lauf = plauf.
        insert zvbak_save.
        counter = counter + 1.
      endselect.
      write: 'Total Anzahl Datensätze an zvbak_save übergeben: ', counter.
    endif.

*Save Kundenauftragspositionen
    if pvbap = 'X' or pall = 'X'.
      select * from vbap.
        move-corresponding vbap to zvbap_save.
        zvbap_save-lauf = plauf.
        insert zvbap_save.
        counter = counter + 1.
      endselect.
      write: 'Total Anzahl Datensätze an zvbap_save übergeben: ', counter.
    endif.

*Save Transportbedarfskopf, -position
    if pltbk = 'X' or pall = 'X'.
      select * from ltbk where lgnum = '391'.
        move-corresponding ltbk to zltbk_save.
        zltbk_save-lauf = plauf.
        insert zltbk_save.
        counter = counter + 1.
        select * from ltbp where lgnum = ltbk-lgnum
                             and tbnum = ltbk-tbnum.
          move-corresponding ltbp to zltbp_save.
          zltbp_save-lauf = plauf.
          insert zltbp_save.
        endselect.
      endselect.
      write: 'Total Anzahl Datensätze an zltbk_save übergeben: ', counter.
    endif.

*Save Transportauftragskopf, - position
    if pltak = 'X' or pall = 'X'.
      select * from ltak where lgnum = '391'.
        move-corresponding ltak to zltak_save.
        zltak_save-lauf = plauf.
        insert zltak_save.
        counter = counter + 1.
        select * from ltap where lgnum = ltak-lgnum
                     and tanum = ltak-tanum.
          move-corresponding ltap to zltap_save.
          zltap_save-lauf = plauf.
          insert zltap_save.
        endselect.
      endselect.
      write: 'Total Anzahl Datensätze an zltak_save übergeben: ', counter.
    endif.

*******************************************************************************

  elseif pdel = 'X'.

*Delete Kabeldisposition
    if pdisp = 'X' or pall = 'X'.
      delete from zmkdisp1_save where lauf = plauf.
      write: 'zmkdips1_save zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Lagerquanten
    if plqua = 'X'  or pall = 'X'.
      delete from zlqua_save where lauf = plauf.
      write: 'zlqua_save zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Chargen
    if pmch1 = 'X' or pall = 'X'.
      delete from zmch1_save where lauf = plauf.
      write: 'zmch1_save zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Chargen
    if pmchb = 'X' or pall = 'X'.
      delete from zmchb_save where lauf = plauf.
      write: 'zmchb_save zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Kundenaufträge
    if pvbak = 'X' or pall = 'X'.
      delete from zvbak_save where lauf = plauf.
      write: 'zvbak_save zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Kundenauftragspositionen
    if pvbap = 'X' or pall = 'X'.
      delete from zvbap_save where lauf = plauf.
      write: 'zvbap_save zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Transportbedarfkopf / -position
    if pltbk = 'X' or pall = 'X'.
      delete from zltbk_save where lauf = plauf.
      write: 'zltbk zur Laufnummer ', plauf, ' gelöscht'.
      delete from zltbp_save where lauf = plauf.
      write: 'zltbp zur Laufnummer ', plauf, ' gelöscht'.
    endif.

*Delete Transportauftragskopf / -position
    if pltak = 'X' or pall = 'X'.
      delete from zltak_save where lauf = plauf.
      write: 'zltak zur Laufnummer ', plauf, ' gelöscht'.
      delete from zltap_save where lauf = plauf.
      write: 'zltap zur Laufnummer ', plauf, ' gelöscht'.
    endif.


  endif.

*******************************************************************************
