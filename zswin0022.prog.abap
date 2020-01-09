*&---------------------------------------------------------------------*
*&  Include           ZSWIN0022
*&---------------------------------------------------------------------*

CONSTANTS: lc_gname TYPE eqegraname VALUE 'MARC',
           lc_msgty TYPE sy-msgty VALUE 'W'.

DATA: lt_enq          TYPE STANDARD TABLE OF seqg3,
      lv_garg         TYPE eqegraarg,
      lv_matnr        TYPE matnr,
      lv_block_active TYPE vfpsp.

SELECT SINGLE vfpsp
  FROM tmvfp
  INTO lv_block_active
  WHERE mtvfp = lips-mtvfp .

IF sy-subrc EQ 0 AND
   lv_block_active EQ abap_true AND
   sy-tcode CP 'VL*' .


  CONCATENATE sy-mandt lips-matnr lips-werks INTO lv_garg .

  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gname                 = lc_gname
      garg                  = lv_garg
    TABLES
      enq                   = lt_enq
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  READ TABLE lt_enq ASSIGNING FIELD-SYMBOL(<fs_enq>) INDEX 1.
  IF sy-subrc = 0 .

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lips-matnr
      IMPORTING
        output = lv_matnr.

    PERFORM message_handling(sapmv50a) USING cvbap-posnr
                                            '023'
                                            lc_msgty
                                            'ZDLE'
                                            lv_matnr
                                            <fs_enq>-guname
                                            lips-werks
                                            space .
  ENDIF.
ENDIF.
