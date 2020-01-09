REPORT ZSD_AUFTRAG_KORR .

tables: vbak, vbpa, vbuk.

select-options: svkorg for vbak-vkorg default '3000',
                svtweg for vbak-vtweg default '01',
                svbeln for vbak-vbeln,
                slifnr for vbpa-lifnr default '0000093671'.

parameters: test as checkbox default 'X'.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



select * from vbak where vkorg in svkorg
                     and vtweg in svtweg
                     and vbeln in svbeln.

  select * from vbpa where vbeln = vbak-vbeln
                       and parvw = 'SP'
                       and posnr = '000000'
                       and lifnr in slifnr.

   write:/ vbak-vbeln, vbak-vkorg, vbpa-lifnr.

   if test = ' '.
    delete vbpa.
   endif.

  endselect.
endselect.



* Test Programmberechtigung
  include zincl_progber.
