*&---------------------------------------------------------------------*
*&  Include           LZMR1MP01
*&---------------------------------------------------------------------*

CLASS lcl_channel_ftp IMPLEMENTATION.
  METHOD lif_channel~commit.
*   Delete files
    command( gt_command ).

    CLEAR gt_command.
  ENDMETHOD.

  METHOD lif_channel~rollback.
    CLEAR gt_command.
  ENDMETHOD.

  METHOD lif_channel~open.
    TYPES:
      tt_lock TYPE STANDARD TABLE OF abap_bool WITH EMPTY KEY.

    CONSTANTS:
      lc_key TYPE i VALUE 26101957,
      BEGIN OF lc_file,
        file_name TYPE i VALUE 1,
        lock      TYPE i VALUE 2,
        name      TYPE i VALUE 3,
        extension TYPE i VALUE 4,
      END OF lc_file.

    FIELD-SYMBOLS:
      <lv_submatch> TYPE any.

    DATA:
      lo_matcher TYPE REF TO cl_abap_matcher,
      ls_tbdls   TYPE        tbdls,
      lv_passwd  TYPE        ftp_passwd,
      lt_command TYPE        tt_command,
      lt_data    TYPE        tt_data_char,
      lr_data    TYPE REF TO ts_data_char,
      lr_lock    TYPE REF TO abap_bool.

    go_log = io_log.

*   When not yet open
    syst-subrc = CONV syst_subrc( command( EXPORTING it_command = VALUE tt_command( ( |system| ) ) IMPORTING et_data = lt_data ) ).
    CASE syst-subrc.
      WHEN 3.
        cl_com_product_basic=>tbdls_read_single(
          EXPORTING
            iv_logsys  = iv_logsys
          IMPORTING
            es_tbdls   = ls_tbdls
          EXCEPTIONS
            wrong_call = 1
            not_found  = 2
            OTHERS     = 3 ).
        go_log->syst( syst ).

        CALL FUNCTION 'HTTP_SCRAMBLE'
          EXPORTING
            source      = ls_tbdls-passwd
            sourcelen   = strlen( ls_tbdls-passwd )
            key         = lc_key
          IMPORTING
            destination = lv_passwd.

        gv_rfcdest = iv_rfcdest.
        CALL FUNCTION 'FTP_CONNECT'
          EXPORTING
            user            = ls_tbdls-user
            password        = lv_passwd
            host            = ls_tbdls-host
            rfc_destination = gv_rfcdest
          IMPORTING
            handle          = gv_handle
          EXCEPTIONS
            not_connected   = 1
            OTHERS          = 2 ##FM_SUBRC_OK.
        go_log->syst( syst ).

        IF iv_path IS NOT INITIAL.
          syst-subrc = CONV syst_subrc( command( EXPORTING it_command = VALUE tt_command( ( |cd { iv_path }| ) ) IMPORTING et_data = lt_data ) ).
          go_log->syst( is_syst = syst it_data = lt_data ).
        ENDIF.

      WHEN OTHERS.
        go_log->syst( is_syst = syst it_data = lt_data ).
    ENDCASE.

    CHECK iv_file_name IS SUPPLIED.

*   FTP command list filename format
    REPLACE REGEX `^([[:print:]]*)\\\.([[:print:]]*)$` IN iv_file_name WITH `^-[-rw]{9}[[:blank:]][[:print:]]*[[:blank:]]((_*)($1)\\\.($2))\$`.

    LOOP AT VALUE tt_lock( ( abap_true ) ( abap_false ) ) REFERENCE INTO lr_lock.
      CLEAR:
        et_file,
        lt_command.

*     List files
      syst-subrc = CONV syst_subrc( command( EXPORTING it_command = VALUE tt_command( ( |ls *.*| ) ) IMPORTING et_data = lt_data ) ).
      go_log->syst( syst ).

*     FTP command list filename format
      lo_matcher = cl_abap_matcher=>create( pattern = iv_file_name table = lt_data ).
      WHILE lo_matcher->find_next( ).
        IF lo_matcher->get_submatch( lc_file-lock ) IS INITIAL.
*         File is not locked and to be locked: Add lock command
          CHECK lr_lock->* EQ abap_true.
          INSERT |rename { lo_matcher->get_submatch( lc_file-file_name ) } _{ lo_matcher->get_submatch( lc_file-file_name ) } | INTO TABLE lt_command.
        ELSE.
*         Locked file
          INSERT VALUE ts_file( file_name = lo_matcher->get_submatch( lc_file-file_name ) name = lo_matcher->get_submatch( lc_file-name ) extension = to_lower( lo_matcher->get_submatch( lc_file-extension ) ) ) INTO TABLE et_file.
        ENDIF.
      ENDWHILE.

*     Rename file and ignore error as error in renaming indicate a lock from the sending application which is not an error
      command( lt_command ).

*     No lock is required: exit
      CHECK lt_command IS INITIAL.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_channel~close.
    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = gv_handle.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE'
      EXPORTING
        destination          = gv_rfcdest
      EXCEPTIONS
        destination_not_open = 1
        OTHERS               = 2 ##FM_SUBRC_OK. "Ignore error as RFC is not open in case of connection error
  ENDMETHOD.

  METHOD lif_channel~read.
    DATA:
      lt_data      TYPE tt_data_bin,
      lv_length    TYPE i,
      lv_block     TYPE string,
      lv_file_name TYPE dfile_name,
      lv_extension TYPE string.

*   Flag commit handler
    PERFORM ftp_on_commit.

*   Get file data
    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = gv_handle
        fname         = cs_file-file_name
      IMPORTING
        blob_length   = lv_length
      TABLES
        blob          = lt_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4 ##FM_SUBRC_OK.
    go_log->syst( syst ).

*   File to be closed
    CASE iv_delete.
      WHEN abap_false.
        INSERT |rename { cs_file-file_name } Imported/{ cs_file-name }_{ syst-datum }_{ syst-uzeit }.{ cs_file-extension }| INTO TABLE gt_command.
      WHEN abap_true.
        INSERT |delete { cs_file-file_name }| INTO TABLE gt_command.
    ENDCASE.

*   Format
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_length
      IMPORTING
        buffer       = cs_file-data
      TABLES
        binary_tab   = lt_data
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2 ##FM_SUBRC_OK.
    go_log->syst( syst ).

*   Success message
    go_log->message( iv_msgty = 'S' iv_msgid = '26' iv_msgno = 318 iv_msgv1 = cs_file-file_name ).
  ENDMETHOD.

  METHOD lif_channel~send.
    DATA:
      lt_data_bin  TYPE tt_data_bin,
      lt_data_char TYPE tt_data_char,
      lv_length    TYPE i.

*   Format
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = is_file-data
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_data_bin.

*   Send FTP data
    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle        = gv_handle
        fname         = CONV dfile_name( |_{ is_file-name }| )
        blob_length   = lv_length
      TABLES
        blob          = lt_data_bin
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4 ##FM_SUBRC_OK.
    go_log->syst( syst ).

*   Delete
    IF iv_overwrite EQ abap_true.
      command( VALUE tt_command( ( |delete { is_file-name }| ) ) ).
    ENDIF.

*   Rename
    syst-subrc = CONV syst_subrc( command( EXPORTING it_command = VALUE tt_command( ( |rename _{ is_file-name } { is_file-name }| ) ) IMPORTING et_data = lt_data_char ) ).
    go_log->syst( is_syst = syst it_data = lt_data_char ).

*   Success message
    go_log->message( iv_msgty = 'S' iv_msgid = '26' iv_msgno = 171 iv_msgv1 = is_file-name ).
  ENDMETHOD.

  METHOD command.
    TYPES:
       tt_command TYPE STANDARD TABLE OF pfecommand WITH EMPTY KEY ##SHADOW.

    DATA:
      lt_command TYPE tt_command.

    CLEAR:
      et_data,
      rv_subrc.

    CHECK it_command IS NOT INITIAL.

    lt_command = CONV tt_command( it_command ).
    CALL FUNCTION 'FTP_COMMAND_LIST'
      EXPORTING
        handle        = gv_handle
      TABLES
        data          = et_data
        commands      = lt_command
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4 ##FM_SUBRC_OK.
    rv_subrc = syst-subrc.
  ENDMETHOD.
ENDCLASS.
