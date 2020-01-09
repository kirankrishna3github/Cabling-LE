REPORT ZSD_DEBITOR_KORR .

tables: knvv, knvp, kna1.


select-options: skunnr for kna1-kunnr,
                svkorg for knvv-vkorg default '3000',
                svtweg for knvv-vtweg default '01',
                sktokd for kna1-ktokd default '0001',
                sspart for knvp-spart default '01',
                sparvw for knvp-parvw default 'SP'.

parameters:     pvsart like knvv-kvgr4 default '01',
                plifnr like knvp-lifnr default '0000093671'.

parameters:     test as checkbox default 'X'.

parameters: rvsart radiobutton group 1,
            rSP    radiobutton group 1.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



*Versandart ändern
if rvsart = 'X'.

  select * from kna1 where ktokd in sktokd
                       and kunnr in skunnr.

    select * from knvv where kunnr = kna1-kunnr
                         and vkorg in svkorg
                         and vtweg in svtweg.
      if not knvv-kvgr4 = pvsart .
        write:/ kna1-kunnr, kna1-ktokd, knvv-vkorg, knvv-vtweg,
                knvv-kvgr4.
        if test = ' '.
          knvv-kvgr4 = pvsart.
          modify knvv.
        endif.
      endif.
    endselect.
  endselect.
endif.

*Partner löschen in VORG 3000
if rsp = 'X'.
  select * from kna1 where ktokd in sktokd
                       and kunnr in skunnr.
    select * from knvp where kunnr = kna1-kunnr
                         and vkorg in svkorg
                         and vtweg in svtweg
                         and spart in sspart
                         and parvw in sparvw
                         and lifnr = plifnr.

      write:/ kna1-kunnr, kna1-ktokd, knvp-vkorg, knvp-vtweg,
              knvp-lifnr.
      if test = ' '.
        delete knvp.
      endif.
    endselect.
  endselect.

endif.



* Test Programmberechtigung
  include zincl_progber.
