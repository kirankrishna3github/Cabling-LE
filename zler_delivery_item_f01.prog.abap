*&---------------------------------------------------------------------*
*&  Include           ZLER_DELIVERY_ITEM_F01
*&---------------------------------------------------------------------*
* ---------------------------------------------------------------------*
*       FORM main
* ---------------------------------------------------------------------*
form entry using return_code us_screen.

  clear return_code.

  return_code = lcl_del_item=>entry( is_nast   = nast
                                     is_tnapr  = tnapr
                                     i_preview = us_screen ).

endform.                    "main
