*----------------------------------------------------------------------*
*   INCLUDE ZRLVSTOP                                                  *
*----------------------------------------------------------------------*
*Zusätzliche Datendeklaration:.
*
tables: kna1, vbak, vbap, lips, likp, t005t, vbpa, adrc, tinct,
        thead, tsp03a.

*Auftraggeber
data: begin of ikna1ag occurs 0.
      include structure kna1.
data: end of ikna1ag.

data: begin of it005tag occurs 0.
      include structure t005t.
data: end of it005tag.

*Warenempfänger
data: begin of ikna1we occurs 0.
      include structure kna1.
data: end of ikna1we.

data: begin of it005twe occurs 0.
      include structure t005t.
data: end of it005twe.


*Kundenauftragskopf
data: begin of ivbak occurs 0.
      include structure vbak.
data: end of ivbak.

*Lieferungskopf
data: begin of ilikp occurs 0.
      include structure likp.
data: end of ilikp.

*Incoterm
data: begin of itinct occurs 0.
      include structure tinct.
data: end of itinct.

*tdname
data: begin of ithead occurs 0.
      include structure thead.
data: end of ithead.

*Counter für 2ten Druck
data: dru2 type p.

*Druckerdestination für 2ten Druck, welcher vom Customizing abweicht
data: drucker2 like tsp03a-name.
