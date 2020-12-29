*&---------------------------------------------------------------------*
*&  Include           LZMR1MD03
*&---------------------------------------------------------------------*

CLASS lcl_idoc DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor.

  PROTECTED SECTION.
    TYPES:
      tt_edidd TYPE STANDARD TABLE OF edidd WITH EMPTY             KEY
                                            WITH NON-UNIQUE SORTED KEY psgnum COMPONENTS docnum psgnum hlevel,
      tt_file  TYPE lif_channel=>tt_file,
      ts_file  TYPE lif_channel=>ts_file.

    DATA:
      go_log TYPE REF TO lcl_log.

    CLASS-DATA:
      gv_logsys     TYPE logsys,
      gv_cccategory TYPE cccategory.

    METHODS:
      structdescr
        IMPORTING
          is_edidc       TYPE edidc
        RETURNING
          VALUE(rr_idoc) TYPE REF TO data
        RAISING
          cx_t100_msg,
      get
        IMPORTING
          is_idoc  TYPE any
        CHANGING
          ct_edidd TYPE edidd_tt
        RAISING
          cx_t100_msg,
      set
        IMPORTING
          is_edidc TYPE edidc
          is_edidd TYPE edidd OPTIONAL
          it_edidd TYPE tt_edidd
        CHANGING
          cs_idoc  TYPE any
        RAISING
          cx_t100_msg.

  PRIVATE SECTION.
    TYPES:
      tt_iapi06 TYPE STANDARD TABLE OF edi_iapi06 WITH EMPTY         KEY
                                                  WITH UNIQUE SORTED KEY parpno COMPONENTS hlevel nr parpno.

    DATA:
      gt_iapi06 TYPE tt_iapi06.

    METHODS:
      component
        IMPORTING
          is_iapi06           TYPE edi_iapi06 OPTIONAL
        RETURNING
          VALUE(rt_component) TYPE abap_component_tab.
ENDCLASS.

CLASS lcl_idoc_inbound  DEFINITION INHERITING FROM lcl_idoc FINAL.
  PUBLIC SECTION.
    TYPES:
      tt_file_name TYPE lif_channel=>tt_file_name.

    METHODS:
      process
        IMPORTING
          io_channel   TYPE REF TO lif_channel
          iv_rfcdest   TYPE rfcdest
          iv_path      TYPE dms_path
          iv_file_name TYPE string
          iv_delete    TYPE abap_bool DEFAULT abap_true
          is_edidc     TYPE edidc
          iv_xsltdesc  TYPE cxsltdesc.

  PRIVATE SECTION.
    METHODS:
      open
        IMPORTING
          is_edidc    TYPE edidc
          iv_xml      TYPE xstring
          iv_xsltdesc TYPE cxsltdesc
        EXPORTING
          es_edidc    TYPE edidc
          et_edidd    TYPE edidd_tt
        RAISING
          cx_t100_msg,
      close
        CHANGING
          cs_edidc TYPE edidc
          ct_edidd TYPE edidd_tt
        RAISING
          cx_t100_msg.
ENDCLASS.

CLASS lcl_idoc_outbound DEFINITION INHERITING FROM lcl_idoc.
  PUBLIC SECTION.
    METHODS:
      process
        IMPORTING
          io_channel    TYPE REF TO lif_channel
          iv_pvrout     TYPE edi_pvrout
          iv_collective TYPE abap_bool OPTIONAL
          iv_overwrite  TYPE abap_bool OPTIONAL
          iv_xsltdesc   TYPE cxsltdesc
        CHANGING
          ct_edidc      TYPE edidc_tt.

  PRIVATE SECTION.
    TYPES:
      tr_edidc TYPE REF TO edidc,
      tt_edidc TYPE STANDARD TABLE OF tr_edidc WITH EMPTY KEY.

    METHODS:
      open
        IMPORTING
          it_edidc    TYPE tt_edidc
          iv_pvrout   TYPE edi_pvrout
          iv_xsltdesc TYPE cxsltdesc
        EXPORTING
          ev_path     TYPE dms_path
          es_file     TYPE ts_file
        RAISING
          cx_t100_msg,
      close
        IMPORTING
          it_edidc  TYPE tt_edidc
          it_return TYPE bapirettab
        RAISING
          cx_t100_msg.
ENDCLASS.

CLASS lcl_idoc_path DEFINITION INHERITING FROM lcl_idoc.
  PUBLIC SECTION.
    METHODS:
      name
        IMPORTING
          is_edidc           TYPE edidc
        RETURNING
          VALUE(rv_pathname) TYPE edi_pthnam.

ENDCLASS.

CLASS lcl_idoc_path_readsoft DEFINITION INHERITING FROM lcl_idoc_path.
  PUBLIC SECTION.
    METHODS:
      name REDEFINITION.
ENDCLASS.

CLASS lcl_idoc_path_supplier DEFINITION INHERITING FROM lcl_idoc_path_readsoft.
  PUBLIC SECTION.
    METHODS:
      name REDEFINITION.
ENDCLASS.

CLASS lcl_idoc_path_supplier_bank DEFINITION INHERITING FROM lcl_idoc_path_readsoft.
  PUBLIC SECTION.
    METHODS:
      name REDEFINITION.
ENDCLASS.
