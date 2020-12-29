*&---------------------------------------------------------------------*
*&  Include           ZXM08U22
*&---------------------------------------------------------------------*

DATA:
  ls_mrm_data TYPE ts_mrm_data,
  lt_lfb1     TYPE tt_lfb1.

gs_edidc  = i_idoc_contrl.
gv_dialog = boolc( syst-ucomm EQ 'FORE' ).

* IDOC data
PERFORM idoc_data USING    i_idoc_contrl
                           t_idoc_data[]
                  CHANGING ls_mrm_data.

* Invoice data: Linked document, header text lines
ls_mrm_data-link-bindata = zcl_saplmrmh=>instance( )->set(
  iv_file_name = ls_mrm_data-link-file-name
  iv_type      = ls_mrm_data-link-file-type
  iv_data      = ls_mrm_data-link-data
  it_tline     = ls_mrm_data-tline ).

CHECK
  i_lifnr           IS INITIAL OR "No vendor
  i_bukrs           IS INITIAL OR "No company code
  ls_mrm_data-bldat IS INITIAL.   "No invoice date

e_lifnr = i_lifnr.
e_bukrs = i_bukrs.

* Vendor company code on VAT numer
PERFORM lifnr_stceg USING    i_lifnr
                             i_bukrs
                             ls_mrm_data-stceg
                    CHANGING lt_lfb1.

* Vendor company code on IBAN account
PERFORM lifnr_iban USING    i_lifnr
                            i_bukrs
                            ls_mrm_data-iban
                   CHANGING lt_lfb1.

* Vendor company code on bank account
PERFORM lifnr_bankn USING    i_lifnr
                             i_bukrs
                             ls_mrm_data-bankl
                             ls_mrm_data-bankn
                    CHANGING lt_lfb1.

* Automatic changes
PERFORM change_auto USING    lt_lfb1
                    CHANGING e_lifnr
                             e_bukrs
                             e_change.

CHECK gv_dialog EQ abap_true. "Dialog required

* Dialog changes
PERFORM change_dialog USING    i_idoc_contrl
                               ls_mrm_data
                      CHANGING t_idoc_data[]
                               e_lifnr
                               e_bukrs
                               e_change.
