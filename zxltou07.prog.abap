*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU07                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Globale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_LTAK) LIKE  LTAK STRUCTURE  LTAK
*"             VALUE(I_LTAP) LIKE  LTAP STRUCTURE  LTAP
*"             VALUE(I_MLVS) LIKE  MLVS STRUCTURE  MLVS
*"             VALUE(I_MGEF) LIKE  MGEF STRUCTURE  MGEF
*"             VALUE(I_T331) LIKE  T331 STRUCTURE  T331
*"             VALUE(I_T333) LIKE  T333 STRUCTURE  T333
*"             VALUE(I_T340D) LIKE  T340D STRUCTURE  T340D
*"             VALUE(I_ANFML) LIKE  RL03A-ANFML
*"             VALUE(I_ANFME) LIKE  RL03A-ANFME
*"             VALUE(I_VORGA) LIKE  LTAP-VORGA
*"       TABLES
*"              T_QMAT STRUCTURE  LQUA_VB
*"              T_BDBATCH STRUCTURE  BDBATCH
*"----------------------------------------------------------------------
*Beim "Handlagerprozess" wird die Kommissionierung nicht über die
*Zusatzfunktionen der ZMOB ausgeführt.
*Schneiden und Austauschbarkeit ist NICHT vorgesehen.
*
*Achtung: der Exit wird nur angesprungen, wenn im cusomizing des
*Lagertyps das Kz. "User Exit aktiv" gesetzt ist.
*Ausserdem darf im Vonlagertyp KEIN fester Lagerplatz eingetragen sein.
*
*12.01.2005; SAP Stäfa, Peter Huber

 tables: zmkdisp1, zle_ohne_zmob.

 select single * from zle_ohne_zmob where lgnum eq i_ltak-lgnum
                                      and zhandl eq 'X'.
 if sy-subrc = 0.

*t_qmat löschen bei chargengeführten Matnr.
select single * from mara where matnr eq i_ltap-matnr
                            and xchpf eq 'X'.

   if sy-subrc = 0.
     refresh: t_qmat.
   endif.
*Kundenauftrag / -position aus Belegfluss
   select single * from vbfa where vbeln eq i_ltak-vbeln
                               and posnn eq i_ltap-posnr.

   if sy-subrc = 0.
*Chargen aus Kabeledisposition ermitteln
     select * from zmkdisp1
            where vbeln eq vbfa-vbelv
            and posnr eq vbfa-posnv
            and werks eq i_ltap-werks
            and lgort eq i_ltap-lgort
            and matnr eq i_ltap-matnr.

       if sy-subrc = 0.
*Bereits bereitgestellt Chargen ausschliessen
         read table t_qmat with key charg = zmkdisp1-charg.
         if sy-subrc ne 0.
*Lagerplatzdaten zum Chargenbestand
           select single * from lqua where lgnum eq i_ltak-lgnum
                                and werks eq i_ltap-werks
                                and lgort eq i_ltap-lgort
                                and matnr eq i_ltap-matnr
                                and charg eq zmkdisp1-charg.

           if sy-subrc = 0.
*         refresh: t_tqmat.
             move-corresponding lqua to t_qmat.
             append t_qmat.
           endif.
         endif.
       endif.
     endselect.
   endif.
 endif.
