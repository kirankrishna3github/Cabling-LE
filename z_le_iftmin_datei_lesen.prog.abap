
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #111
************************************************************************

REPORT Z_LE_IFTMIN_DATEI_LESEN line-size 1000.

*Einlesen der IFTMIN Datei für Dachser
*Anforderung von DKS
*
*SAP Stäfa, P. Huber, 7.03
*

data: satz(1000).
parameters: dsni(60) lower case
                     default '/usr/sap/TR1/SYS/global/xxx'.


*$smart (F) 2010-08-02 - #111 OPEN DATASET benötigt mode und encoding
*$smart (F) 2010-08-02 - #111 Spezifikation Adding encoding to OPEN
*$smart (F) 2010-08-02 - #111 DATASET statement (A)

open dataset dsni for input in text mode ENCODING"smart: 2010-08-02 #111
   UTF-8 .                                       "smart: 2010-08-02 #111
do.
  read dataset dsni into satz.
  if sy-subrc ne 0.
    exit.
  endif.
  detail.
  write: / satz.
enddo.
