FUNCTION-POOL ZDLE_PACK.                    "MESSAGE-ID ..

* Lieferpositionen
data: begin of gt_lips occurs 0,
        vbeln like lips-vbeln,
        posnr like lips-posnr,
        uecha like lips-uecha,
      end of gt_lips.

data: gt_vekp TYPE HUM_HU_HEADER_T.
data: gt_vepo TYPE HUM_HU_ITEM_T.
