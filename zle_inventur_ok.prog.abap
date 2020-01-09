REPORT ZLE_INVENTUR_OK message-id ZDLE.

*Tabellen
tables: zle_inventurhist, lqua, sscrfields.

*Workfelder
data: wok.
data: pstate like zle_inventurhist-state.

*Selekitonsoberfläche
parameters: pivnum like zle_inventurhist-ivnum.
selection-screen skip 1.
SELECTION-SCREEN PUSHBUTTON /1(21)  DO   USER-COMMAND DO.
SELECTION-SCREEN PUSHBUTTON 25(21)  OK   USER-COMMAND OK.
SELECTION-SCREEN PUSHBUTTON 49(21) DELE USER-COMMAND DELE.

*Inititialisierung
INITIALIZATION.

** Test Programmberechtigung
*  perform test_berechtigung.



  MOVE text-001 TO OK.
  MOVE text-002 TO DELE.
  MOVE text-003 TO DO.
*Parameter vorgelegen
  write 'E' to pstate.

*Verarbeitung
AT SELECTION-SCREEN.

*Inventur Beleg anlegen
  IF SSCRFIELDS-UCOMM = 'DO'.
    call transaction 'ZLEINVE'.
  endif.

*Inventur bestätigen (OK)
  IF SSCRFIELDS-UCOMM = 'OK'.
    if not  pivnum is initial.  "Inv.belegnummer muss eingegeben werden

      wok = '0'.
      select * from zle_inventurhist where ivnum eq pivnum.
        if zle_inventurhist-state = 'P'.
          wok = '1'.

          zle_inventurhist-state = pstate.
          modify zle_inventurhist.

          select * from lqua where lgnum = zle_inventurhist-lgnum
                               and lqnum = zle_inventurhist-lqnum.

            lqua-ivnum = zle_inventurhist-ivnum.
            lqua-ivpos = zle_inventurhist-ivpos.
            lqua-idatu = zle_inventurhist-idatu.
            modify lqua.
          endselect.
        endif.
      endselect.

      if wok = '1'.
       MESSAGE I000 WITH text-004 pivnum text-005.
      else.
   MESSAGE W000 WITH text-004 pivnum text-006.
        MESSAGE W000 WITH text-004 pivnum text-007.
      endif.

    else.
      MESSAGE E000 WITH text-008.
    endif.
  endif.


*Inventurbeleg löschen (nur im Status Pendent)

  IF SSCRFIELDS-UCOMM = 'DELE'.
   if not  pivnum is initial.    "Inv.belegnummer muss eingegeben werden
      select * from zle_inventurhist where ivnum eq pivnum.
        if zle_inventurhist-state = 'P'.
          wok = '1'.
          delete from zle_inventurhist where state eq 'P'
                                         and ivnum eq pivnum.
        endif.
      endselect.

      if wok = '1'.
        MESSAGE I000 WITH text-004 pivnum text-009.
      else.
   MESSAGE W000 WITH text-004 pivnum text-006.
        MESSAGE W000 WITH text-004 pivnum text-007.
      endif.
    else.
      MESSAGE E000 WITH text-010.
    endif.
  endif.


** Test Programmberechtigung
*  include zincl_progber.
