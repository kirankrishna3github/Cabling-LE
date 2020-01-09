*&---------------------------------------------------------------------*
*&  Include           ZXTRKU02
*&---------------------------------------------------------------------*

data: ls_e1edl20 type e1edl20.
data: ls_e1adrm1 type e1adrm1.
data: ls_ze1adre101 type ze1adre101.
data: lv_ind type i.
data: ls_idoc_data type edidd.
data: lv_bukrs type vkbuk.
data: lv_append type c.

case segment_name.
  when 'E1ADRM1'.
    perform zusatzsegment_ze1adre101 tables idoc_data
                                     using segment_name.
endcase.
