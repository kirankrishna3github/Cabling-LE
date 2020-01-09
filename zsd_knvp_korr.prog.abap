REPORT ZSD_KNVP_KORR .

tables: knvp.

parameters: pvkorg LIKE knvp-vkorg obligatory,
            pparvw like knvp-parvw.


parameters: test as checkbox default 'X'.


if sy-uname ne 'I020676'.
exit.
endif.

select * from knvp where vkorg eq pvkorg
                     and parvw eq pparvw.

check: ( knvp-lifnr = 0000093671 or knvp-lifnr = 93671 ).

write:/ knvp-KUNNR,
        knvp-VKORG,
        knvp-VTWEG,
        knvp-SPART,
        knvp-PARVW,
        knvp-PARZA,
        knvp-KUNN2,
        knvp-LIFNR,
        knvp-PERNR,
        knvp-PARNR,
        knvp-KNREF,
        knvp-DEFPA.

 if test = ' '.
  delete knvp.
endif.

endselect.
