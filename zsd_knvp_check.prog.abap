REPORT ZSD_KNVP_CHECK .

tables: knvp, knvv.

data: begin of input occurs 0,
      kunnr like knvp-kunnr,
      vkorg like knvp-vkorg,
      vtweg like knvp-vtweg,
      spart like knvp-spart,
      end of input.

data: begin of icollect occurs 0.
      include: structure input.
data: end of icollect.

data: counter(10) type n.

select-options: skunnr for knvp-kunnr obligatory,
                svkorg for knvp-vkorg.


select * from knvp where kunnr in skunnr
                     and vkorg in svkorg.

 select single * from knvv where kunnr eq knvp-kunnr
                             and vkorg eq knvp-vkorg
                             and vtweg eq knvp-vtweg
                             and spart eq knvp-spart.

 if sy-subrc ne 0.

 input-kunnr = knvp-kunnr.
 input-vkorg = knvp-vkorg.
 input-vtweg = knvp-vtweg.
 input-spart = knvp-spart.
 append input.

 endif.
endselect.

sort input by kunnr vkorg.
loop at input.
at end of vkorg.
 icollect-kunnr = input-kunnr.
 icollect-vkorg = input-vkorg.
 append icollect.
endat.
endloop.

loop at icollect.
 write:/ icollect-kunnr, icollect-vkorg.
   counter = counter + 1.
endloop.

skip 1.
write:/ counter.
