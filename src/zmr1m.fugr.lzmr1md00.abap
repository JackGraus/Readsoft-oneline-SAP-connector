*&---------------------------------------------------------------------*
*&  Include           LZMR1MD00
*&---------------------------------------------------------------------*

CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ts_data_char,
        line(1022) TYPE c,
      END OF ts_data_char,
      tt_data_char TYPE STANDARD TABLE OF ts_data_char WITH EMPTY KEY.

    METHODS:
      open,
      close
        IMPORTING
          iv_dialog TYPE dialog DEFAULT abap_false
        EXPORTING
          et_return TYPE bapirettab,
      message
        IMPORTING
          iv_msgty TYPE syst_msgty
          iv_msgid TYPE syst_msgid
          iv_msgno TYPE syst_msgno
          iv_msgv1 TYPE clike OPTIONAL
          iv_msgv2 TYPE clike OPTIONAL
          iv_msgv3 TYPE clike OPTIONAL
          iv_msgv4 TYPE clike OPTIONAL
        RAISING
          cx_t100_msg,
      syst
        IMPORTING
          is_syst TYPE syst
          it_data TYPE tt_data_char OPTIONAL
        RAISING
          cx_t100_msg,
      exception
        IMPORTING
          io_exception TYPE REF TO cx_root
        RAISING
          cx_t100_msg.

  PRIVATE SECTION.
    DATA:
      gv_handle TYPE balloghndl.
ENDCLASS.
