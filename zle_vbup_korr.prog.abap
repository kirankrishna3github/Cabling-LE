REPORT ZLE_VBUP_KORR .

tables: vbup, lips.

select-options: svbeln for lips-vbeln obligatory,
                sposnr for lips-posnr,
                suecha for lips-uecha obligatory,
                smatnr for lips-matnr.

parameters:     pfksta like vbup-fksta default 'C',
                pwbsta like vbup-wbsta default 'A',
                test as checkbox.



INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.



select * from lips where vbeln in svbeln
                     and matnr in smatnr
                     and posnr in sposnr
                     and uecha in suecha.


   select * from vbup where vbeln = lips-vbeln
                        and posnr = lips-posnr
                        and fksta = pfksta
                        and wbsta = pwbsta.

     write:/ lips-vbeln, lips-posnr, lips-uecha, vbup-wbsta.

     if test = ' '.
      vbup-wbsta = 'C'.
      modify vbup.
     endif.
   endselect.
 endselect.



* Test Programmberechtigung
  include zincl_progber.
