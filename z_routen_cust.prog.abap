REPORT Z_ROUTEN_CUST .

tables: troiz, trolz, tvro.


data: w_route like trolz-route.
data: counter type p.


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
parameters: pdtrolz as checkbox,
            pdtroiz as checkbox.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
parameters: paland like troiz-aland,
            pazone like troiz-azone.

Parameters: ptroiz as checkbox.

SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
Parameters: ptrolz as checkbox.

SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN COMMENT /1(40) TEXT-COM.


INITIALIZATION.

* Test Programmberechtigung
  perform test_berechtigung.

START-OF-SELECTION.

*Tabelleinhalt trolz löschen
  if pdtroiz = 'X' and sy-mandt = '300'.
    delete from troiz client specified where mandt = sy-mandt.
    write: / 'Tabelle TROIZ  gelöscht'.
  endif.

  if pdtrolz = 'X' and sy-mandt = '300'.
    delete from trolz client specified where mandt = sy-mandt.
    write: / 'Tabelle TROLZ gelöscht'.
  endif.

*Tabelle Routen TROIZ
  if ptroiz = 'X'.

    select * from tvro.
      troiz-aland = paland.
      troiz-azone = pazone.
      troiz-lland = tvro-route.
      troiz-lzone = tvro-route.
      insert troiz.
      counter = counter + 1.
    endselect.

    write:/ counter, ' Routen eingefügt'.

*Tabelle Routenfinung TROLZ
  elseif ptrolz = 'X'.
*
    select * from troiz.

      check: troiz-aland = 'MX'.
      check: troiz-azone co '0123456789 '.
      check: not troiz-lzone is initial.

**Versandbedingung initial (generisch)
* trolz-mandt = sy-mandt.
* trolz-aland = troiz-aland.
* trolz-azone = troiz-azone.
* trolz-vsbed = '  '.
* trolz-tragr = '    '.
* trolz-lland = troiz-lland.
* trolz-lzone = troiz-lzone.
* trolz-grulg = '    '.
* trolz-route = troiz-lland.
* insert trolz.
* counter = counter + 1.

*Versandbedingung 00
      trolz-mandt = sy-mandt.
      trolz-aland = troiz-aland.
      trolz-azone = troiz-azone.
      trolz-vsbed = '00'.
      trolz-tragr = '0001'.
      trolz-lland = troiz-lland.
      trolz-lzone = troiz-lzone.
      trolz-grulg = '    '.
      trolz-route = troiz-lland.
      modify trolz.
      counter = counter + 1.

**Versandbedingung 01
* trolz-mandt = sy-mandt.
* trolz-aland = troiz-aland.
* trolz-azone = troiz-azone.
* trolz-lland = troiz-lland.
* trolz-lzone = troiz-lzone.
* trolz-route = troiz-lland.
* trolz-vsbed = '01'.
* trolz-tragr = '0001'.
* trolz-grulg = '    '.
* insert trolz.
* counter = counter + 1.

*if troiz-aland = 'DE'.
**Versandbedingung 00, zusätzlich Transportgruppe 0002
* trolz-mandt = sy-mandt.
* trolz-aland = troiz-aland.
* trolz-azone = troiz-azone.
* trolz-vsbed = '00'.
* trolz-tragr = '0002'.
* trolz-lland = troiz-lland.
* trolz-lzone = troiz-lzone.
* trolz-grulg = '    '.
* trolz-route = troiz-lland.
* insert trolz.
* counter = counter + 1.
*
**Versandbedingung 01, , zusätzlich Transportgruppe 0002
* trolz-mandt = sy-mandt.
* trolz-aland = troiz-aland.
* trolz-azone = troiz-azone.
* trolz-lland = troiz-lland.
* trolz-lzone = troiz-lzone.
* trolz-route = troiz-lland.
* trolz-vsbed = '01'.
* trolz-tragr = '0002'.
* trolz-grulg = '    '.
* insert trolz.
* counter = counter + 1.
*endif.
    endselect.

    write:/ counter, ' Routenfindungen eingefügt'.

  endif.



* Test Programmberechtigung
  include zincl_progber.
