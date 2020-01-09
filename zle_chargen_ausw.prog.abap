REPORT ZLE_CHARGEN_AUSW .

tables: lips, lqua.

select-options: svbeln for lips-vbeln obligatory no intervals,
                sposnr for lips-posnr,
                smatnr for lips-matnr,
                scharg for lips-charg.


select * from lips where vbeln in svbeln
                     and posnr in sposnr
                     and matnr in smatnr
                     and charg in scharg.

   select * from lqua where matnr eq lips-matnr
                        and charg eq lips-charg.

     write:/ lips-vbeln, lips-posnr,
             lips-werks, lips-lgort,
             lips-matnr, lips-charg, lips-lfimg, lips-meins,
             lqua-lgnum, lqua-lgtyp, lqua-lgpla, lqua-gesme, lqua-meins.

   endselect.

endselect.
