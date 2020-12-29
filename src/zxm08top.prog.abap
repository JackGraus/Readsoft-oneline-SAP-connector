*&---------------------------------------------------------------------*
*&  Include           ZXM08TOP
*&---------------------------------------------------------------------*

INCLUDE:
  mbdconwf.

TABLES:
  t001,
  lfa1,
  rbkp,
  tiban.

TYPES:
  BEGIN OF ts_link,
    file    TYPE        z1link,
    data    TYPE        string,
    bindata TYPE REF TO xstring,
  END OF ts_link,
  tt_iapi06 TYPE STANDARD TABLE OF edi_iapi06 WITH EMPTY KEY,
  BEGIN OF ts_mrm_data,
    iapi06 TYPE tt_iapi06,
    bldat  TYPE edidat8,
    stceg  TYPE stceg,
    iban   TYPE iban,
    bankl  TYPE bankl,
    bankn  TYPE bankn,
    link   TYPE ts_link,
    tline  TYPE tline_tab,
  END OF ts_mrm_data,
  mrm_t076s TYPE                 t076s,
  tt_lifnr  TYPE RANGE        OF lifnr,
  tt_bukrs  TYPE RANGE        OF bukrs,
  tt_lfb1   TYPE SORTED TABLE OF vmds_lfb1_key WITH NON-UNIQUE DEFAULT KEY.

DATA:
  c_segnum_initial TYPE idocdsgnum,
  gs_edidc         TYPE edidc,
  gv_dialog        TYPE dialog,
  gv_ucomm         TYPE syst_ucomm.
