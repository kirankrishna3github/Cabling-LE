
************************************************************************
* 2010-08-02   smartShift project
* Regel IDs angewandt: #102
************************************************************************

REPORT ZZ_FI_AX_SOFTM_KNVP .
tables: ztsoftmd.
tables: knvp, kna1.
data:    begin of wempf_tab occurs 1024,         "smart: 2010-08-02 #102
                  bukrs like ztsoftmd-bukrs,
                  altkn like ztsoftmd-altkn,
                  kunnr like ztsoftmd-kunnr,
         end   of wempf_tab.                     "smart: 2010-08-02 #102

select-options: skunnr for kna1-kunnr.

start-of-selection.
select * from ztsoftmd where ktokd eq '0002'
                         and kunnr in skunnr.
         check ztsoftmd-altkn ne '0000000000'.
         move-corresponding: ztsoftmd to wempf_tab.
                                                 "smart: 2010-08-02 #102

         append                          wempf_tab.
                                                 "smart: 2010-08-02 #102

endselect.
break-point.
loop at wempf_tab.                               "smart: 2010-08-02 #102
         select * from ztsoftmd where bukrs eq   "smart: 2010-08-02 #102
           wempf_tab-bukrs and                   "smart: 2010-08-02 #102
                                      altkn eq   "smart: 2010-08-02 #102
                                        wempf_tab-altkn
 and                                             "smart: 2010-08-02 #102
                                      ktokd eq '0001'.
         if sy-subrc eq 0.
            clear knvp.
            move: sy-mandt to knvp-mandt.
            move: ztsoftmd-kunnr to knvp-kunnr.
            if wempf_tab-bukrs eq '0105'.        "smart: 2010-08-02 #102
               move: '2000' to knvp-vkorg.
            endif.
            if wempf_tab-bukrs eq '0108'.        "smart: 2010-08-02 #102
               move: '5000' to knvp-vkorg.
            endif.
            if wempf_tab-bukrs eq '0116'.        "smart: 2010-08-02 #102
               move: '6000' to knvp-vkorg.
            endif.
            move: '01'            to knvp-vtweg,
                  '01'            to knvp-spart,
                  'WE'            to knvp-parvw.
            move:  wempf_tab-kunnr to knvp-kunn2."smart: 2010-08-02 #102
            do.
            "insert knvp.
            if sy-subrc ne 0.
               knvp-parza = knvp-parza + 1.
            else.
            write: /001 knvp-mandt,
                        knvp-kunnr,
                        knvp-vkorg,
                        knvp-vtweg,
                        knvp-spart,
                        knvp-parvw,
                        knvp-parza,
                        knvp-kunn2.
               exit.
            endif.
            enddo.
         endif.
         endselect.
endloop.


end-of-selection.
