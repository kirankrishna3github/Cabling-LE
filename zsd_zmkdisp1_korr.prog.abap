REPORT ZSD_ZMKDISP1_KORR.

tables: zmkdisp1, vbak, vbap, vbfa, lips, likp.

parameters:        pvbeln like vbak-vbeln obligatory,
                   pposnr like vbap-posnr.

select-options:    scposnr for vbap-posnr.  "Chargenpositionen löschen

select-options:    smatnr for zmkdisp1-matnr,
                   scharg for zmkdisp1-charg.


parameters: pcable   radiobutton group 1,
            pcabled  radiobutton group 1,
            pvbak    radiobutton group 1,
            pvbakd   radiobutton group 1,
            pvbap    radiobutton group 1,
            pvbapd   radiobutton group 1,
            plikp    radiobutton group 1,
            plikpd   radiobutton group 1,
            plips    radiobutton group 1,
            plipsd   radiobutton group 1,
            pcposnr  radiobutton group 1,
            pcposnrd radiobutton group 1,
            pvbfa    radiobutton group 1,
            pvbfad   radiobutton group 1.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.


start-of-selection.

*ZMKDISP1
if pcable = 'X' or pcabled = 'X'.

if pposnr is initial.
MESSAGE ID 'ZDMM' TYPE 'E' NUMBER '000' WITH 'Kundenauftragspos. fehlt'.
 exit.
endif.

  select * from zmkdisp1 where vbeln eq pvbeln
                           and posnr eq pposnr
                           and matnr in smatnr
                           and charg in scharg.

    write:/ zmkdisp1-vbeln,
            zmkdisp1-posnr,
            zmkdisp1-matnr,
            zmkdisp1-charg.

  endselect.

  if pcabled = 'X'.
    if sy-uname cs 'I0'.

      delete from zmkdisp1 where vbeln eq pvbeln
                             and posnr eq pposnr
                             and matnr in smatnr
                             and charg in scharg.

    endif.
  endif.
endif.

*VBAK & VBAP
if pvbak = 'X' or pvbakd = 'X'.

  select * from vbak where vbeln eq pvbeln.
    write:/ vbak-vbeln.
    select * from vbap where vbeln eq vbak-vbeln.
      write:/ vbap-posnr, vbap-matnr.
    endselect.
  endselect.

  if pvbakd = 'X'.
    if sy-uname cs 'I0'.

      select single * from vbfa where vbelv eq vbak-vbeln.
      if sy-subrc ne 0. "kein Nachfolger im Belegfluss
        delete from vbak    where vbeln eq pvbeln.
      else.
        write:/ vbak-vbeln, 'hat Nachfolger !'.
      endif.

      select single * from vbfa where vbelv eq vbap-vbeln
                                  and posnv eq vbap-posnr.

      if sy-subrc ne 0. "kein Nachfolger im Belegfluss
        delete from vbap    where vbeln eq pvbeln.
      else.
        write:/ vbap-vbeln, vbap-posnr, 'hat Nachfolger !'.
      endif.
    endif.
  endif.
endif.

*VBAP
if pvbap = 'X' or pvbapd = 'X'.

if pposnr is initial.
MESSAGE ID 'ZDMM' TYPE 'E' NUMBER '000' WITH 'Kundenauftragspos. fehlt'.
 exit.
endif.

  select * from vbap where vbeln eq pvbeln
                       and posnr eq pposnr.
    write:/ vbap-vbeln, vbap-posnr.
  endselect.

  if pvbapd = 'X'.
    if sy-uname cs 'I0'.

      select single * from vbfa where vbelv eq vbap-vbeln
                                and posnv eq vbap-posnr.

      if sy-subrc ne 0. "kein Nachfolger im Belegfluss
        delete from vbap    where vbeln eq pvbeln.
      else.
        write:/ vbap-vbeln, vbap-posnr, 'hat Nachfolger !'.
      endif.
    endif.
  endif.
endif.

*LIKP & LIPS
if plikp = 'X' or plikpd = 'X'.

  select * from likp where vbeln eq pvbeln.
    write:/ likp-vbeln.
    select * from lips where vbeln eq lips-vbeln.
      write:/ lips-posnr, lips-matnr.
    endselect.
  endselect.

  if pvbakd = 'X'.
    if sy-uname cs 'I0'.
      delete from likp    where vbeln eq pvbeln.
      delete from lips    where vbeln eq pvbeln.
    endif.
  endif.
endif.


*LIPS
if plips = 'X' or plipsd = 'X'.

if pposnr is initial.
MESSAGE ID 'ZDMM' TYPE 'E' NUMBER '000' WITH 'Lieferungspos. fehlt'.
 exit.
endif.


  select * from lips where vbeln eq pvbeln
                       and posnr eq pposnr.
    write:/ lips-vbeln, lips-posnr.
  endselect.

  if plipsd = 'X'.
    if sy-uname cs 'I0'.
      delete from lips    where vbeln eq pvbeln
                            and posnr eq pposnr.
    endif.
  endif.
endif.

*LIPS  (Löschen Chargensplittpositionen)
if pcposnr = 'X' or pcposnrd = 'X'.

if scposnr is initial.
MESSAGE ID 'ZDMM' TYPE 'E' NUMBER '000' WITH 'Lieferungspos. fehlt'.
 exit.
endif.


  select * from lips where vbeln eq pvbeln
                       and posnr in scposnr.
    write:/ lips-vbeln, lips-posnr.
  endselect.

  if pcposnrd = 'X'.
    if sy-uname cs 'I0'.
      delete from lips    where vbeln eq pvbeln
                            and posnr in scposnr.
    endif.
  endif.
endif.


*VBFA
if pvbfa = 'X' or pvbfad = 'X'.

if pposnr is initial.
MESSAGE ID 'ZDMM' TYPE 'E' NUMBER '000' WITH 'Kundenauftragspos. fehlt'.
 exit.
endif.

  select * from vbfa where vbelv eq pvbeln
                       and posnv eq pposnr.
    write:/ vbfa-vbelv, vbfa-posnv.
  endselect.

  if pvbfad = 'X'.
    if sy-uname cs 'I0'.
      delete from vbfa    where vbelv eq pvbeln
                            and posnv eq pposnr.
    endif.
  endif.
endif.



* Test Programmberechtigung
  include zincl_progber.
