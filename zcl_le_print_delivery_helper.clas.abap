class ZCL_LE_PRINT_DELIVERY_HELPER definition
  public
  final
  create public .

public section.

  types: begin of ty_s_print_delivery_item,
           vbeln             type lips-vbeln,
           posnr             type lips-posnr,
           begin of s_header,
             vkorg_adr       type tvko-adrnr,
             bstkd           type vbkd-bstkd,
           end of s_header,
           begin of s_item,
             aufnr           type aufk-aufnr,
             prod_werks      type t001w-werks,
             prod_werks_adr  type t001w-adrnr,
             prod_werks_dsc  type string,
             prod_werks_city type string,
             prod_werks_ctr  type string,
             date_long       type string,
             matnr           type vbap-matnr,
             maktx           type makt-maktx,
             cable_matnr     type mara-matnr,
             cable_maktx     type makt-maktx,
             cable_charg     type mch1-charg,
             cable_menge     type resb-lmeng,
             cable_meins     type resb-meins,
             cable_mseht     type t006a-mseht,
             begin of s_basic_text,
               name          type stxh-tdname,
               spras         type stxh-tdspras,
               id            type stxh-tdid,
               object        type stxh-tdobject,
             end of s_basic_text,
             begin of s_inspection_text,
               name          type stxh-tdname,
               spras         type stxh-tdspras,
               id            type stxh-tdid,
               object        type stxh-tdobject,
             end of s_inspection_text,
             begin of s_chars,
               type_internal type ausp-atwrt,
             end of s_chars,
           end of s_item,
         end of ty_s_print_delivery_item.
protected section.
private section.
ENDCLASS.



CLASS ZCL_LE_PRINT_DELIVERY_HELPER IMPLEMENTATION.
ENDCLASS.
