*&---------------------------------------------------------------------*
*&  Include           LZMR1MD01
*&---------------------------------------------------------------------*

INTERFACE lif_channel.
  TYPES:
    BEGIN OF ts_file,
      file_name TYPE dfile_name,
      name      TYPE dfile_name,
      extension TYPE string,
      data      TYPE xstring,
    END OF ts_file,
    tt_file      TYPE STANDARD TABLE OF ts_file    WITH EMPTY KEY,
    tt_file_name TYPE STANDARD TABLE OF dfile_name WITH EMPTY KEY.

  DATA:
    go_log TYPE REF TO lcl_log.

  CLASS-METHODS:
    commit,
    rollback.

  METHODS:
    open
      IMPORTING
        io_log              TYPE REF TO lcl_log
        iv_rfcdest          TYPE rfcdest DEFAULT 'SAPFTPA'
        iv_logsys           TYPE logsys
        iv_path             TYPE dms_path OPTIONAL
        VALUE(iv_file_name) TYPE string OPTIONAL
      EXPORTING
        et_file             TYPE tt_file
      RAISING
        cx_t100_msg,
    close,
    read
      IMPORTING
        iv_delete TYPE abap_bool DEFAULT abap_true
      CHANGING
        cs_file   TYPE ts_file
      RAISING
        cx_t100_msg,
    send
      IMPORTING
        is_file      TYPE ts_file
        iv_overwrite TYPE abap_bool OPTIONAL
      RAISING
        cx_t100_msg.
ENDINTERFACE.

CLASS lcl_channel_ftp DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      lif_channel.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_data_char,
        line(1022) TYPE c,
      END OF ts_data_char,
      tt_data_char TYPE STANDARD TABLE OF ts_data_char WITH EMPTY KEY,
      BEGIN OF ts_data_bin,
        line TYPE sdok_sdatx,
      END OF ts_data_bin,
      tt_data_bin TYPE STANDARD TABLE OF ts_data_bin WITH EMPTY KEY,
      tt_command  TYPE STANDARD TABLE OF string      WITH EMPTY KEY.

    ALIASES:
      tt_file      FOR lif_channel~tt_file,
      ts_file      FOR lif_channel~ts_file,
      tt_file_name FOR lif_channel~tt_file_name,
      go_log       FOR lif_channel~go_log.

    CLASS-DATA:
      gv_handle  TYPE i,
      gt_command TYPE tt_command.

    DATA:
      gv_rfcdest TYPE rfcdest.

    CLASS-METHODS:
      command
        IMPORTING
          VALUE(it_command) TYPE tt_command
        EXPORTING
          et_data           TYPE tt_data_char
        RETURNING
          VALUE(rv_subrc)   TYPE syst_subrc.
ENDCLASS.
