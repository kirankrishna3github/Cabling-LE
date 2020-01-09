*----------------------------------------------------------------------*
*   INCLUDE ZLE_GOODMOVE_PETI                                          *
*----------------------------------------------------------------------*
* Druckprogramm zum erstellen der Rüstetiketten im ZLEGM10
*----------------------------------------------------------------------*
* Author   R. Näf,  bits AG
*          2010-06-01
*----------------------------------------------------------------------*




FORM print_eti.


  itcpo-tdimmed  = 'X'.                " Sofortdruck
  itcpo-tddelete = ' '.                " Löschen nach Drucken "CSSPAU2010
  itcpo-tdnewid  = 'X'.                " Neue Spool-ID pro Job
  itcpo-tddest   = p_print1.           " Druckername
  IF sy-uname = 'KREBSERA' OR
     sy-uname = 'NAEFR'.
    x_dialog = 'X'.
  ELSE.
    x_dialog = ' '.
  ENDIF.



  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            dialog                      = x_dialog
            form                        = ' '
            language                    = sy-langu
            options                     = itcpo
       EXCEPTIONS
            canceled                    = 1
            device                      = 2
            form                        = 3
            options                     = 4
            unclosed                    = 5
            mail_options                = 6
            archive_error               = 7
            invalid_fax_number          = 8
            more_params_needed_in_batch = 9
            spool_error                 = 10
            OTHERS                      = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CALL FUNCTION 'START_FORM'
       EXPORTING
            form        = 'ZLE_UMB_RUEST'
            language    = sy-langu
            startpage   = 'FIRST'
       EXCEPTIONS
            form        = 1
            format      = 2
            unended     = 3
            unopened    = 4
            unused      = 5
            spool_error = 6
            OTHERS      = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            element = 'HEADING'
       EXCEPTIONS
            OTHERS  = 0.


  LOOP AT gt_printab INTO w_outtab.

    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element = 'POSDAT'
         EXCEPTIONS
              OTHERS  = 0.
  ENDLOOP.

  CALL FUNCTION 'CLOSE_FORM'
       EXCEPTIONS
            unopened                 = 1
            bad_pageformat_for_print = 2
            send_error               = 3
            spool_error              = 4
            OTHERS                   = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  print_ruest
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_ruest.

  IF NOT gt_printab[] IS INITIAL.

    READ TABLE gt_printab INTO w_outtab INDEX 1.

    PERFORM print_eti.

    CLEAR gt_printab.
    refresh gt_printab.

  ENDIF.

ENDFORM.                    " print_ruest
