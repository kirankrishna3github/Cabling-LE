*----------------------------------------------------------------------*
***INCLUDE ZXMBCI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

z-okcode = d100-okcode.                         "CSMIG2010
  clear d100-okcode.                            "CSMIG2010
  case z-okcode.
    when 'WEIT'.
      set screen 0.
      leave screen.
  endcase.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
