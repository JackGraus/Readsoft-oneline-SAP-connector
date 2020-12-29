*&---------------------------------------------------------------------*
*&  Include           LZMR1MP03
*&---------------------------------------------------------------------*

CLASS lcl_idoc IMPLEMENTATION.
  METHOD class_constructor.
*   Logical system
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = gv_logsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2 ##FM_SUBRC_OK.

*   Client setting
    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        system_client_role = gv_cccategory
      EXCEPTIONS
        no_systemname      = 1
        no_systemtype      = 2
        OTHERS             = 3 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD constructor.
    go_log = NEW lcl_log( ).
  ENDMETHOD.

  METHOD structdescr.
    DATA:
      lo_abap_structdescr TYPE REF TO cl_abap_structdescr.

*   IDOC syntax definition
    CALL FUNCTION 'EDI_IDOC_SYNTAX_GET'
      EXPORTING
        pi_idoctyp       = is_edidc-idoctp
        pi_cimtyp        = is_edidc-cimtyp
      TABLES
        pt_syntax_table  = gt_iapi06
      EXCEPTIONS
        syntax_not_found = 1
        OTHERS           = 2 ##FM_SUBRC_OK.
    go_log->syst( syst ).

*   IDOC data definition
    lo_abap_structdescr = cl_abap_structdescr=>create( component( ) ).
    CREATE DATA rr_idoc TYPE HANDLE lo_abap_structdescr.
  ENDMETHOD.

  METHOD get.
    FIELD-SYMBOLS:
      <lt_idoc> TYPE table,
      <ls_idoc> TYPE any.

    DATA:
      lo_abap_typedescr TYPE REF TO cl_abap_typedescr,
      lr_component      TYPE REF TO abap_componentdescr.

    lo_abap_typedescr = cl_abap_datadescr=>describe_by_data( is_idoc ).
    CASE lo_abap_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_table.
        ASSIGN is_idoc TO <lt_idoc>.
        LOOP AT <lt_idoc> ASSIGNING <ls_idoc>.
          get( EXPORTING is_idoc = <ls_idoc> CHANGING ct_edidd = ct_edidd ).
        ENDLOOP.
      WHEN cl_abap_typedescr=>typekind_struct1.
        INSERT VALUE edidd(
          segnam = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_idoc ) )->get_relative_name( )
          sdata  = is_idoc ) INTO TABLE ct_edidd.
      WHEN cl_abap_typedescr=>typekind_struct2.
        LOOP AT CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( is_idoc ) )->get_components( ) REFERENCE INTO lr_component.
          ASSIGN COMPONENT lr_component->name OF STRUCTURE is_idoc TO <ls_idoc>.
          get( EXPORTING is_idoc = <ls_idoc> CHANGING ct_edidd = ct_edidd  ).
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.

  METHOD set.
    TYPES:
      tt_segnum TYPE RANGE OF idocdsgnum.

    FIELD-SYMBOLS:
      <lt_idoc>  TYPE table,
      <ls_idoc>  TYPE any,
      <ls_sdata> TYPE clike.

    DATA:
      lr_edidd  TYPE REF TO edidd.

*   Child segment(s)
    LOOP AT it_edidd REFERENCE INTO lr_edidd USING KEY psgnum
      WHERE docnum EQ is_edidc-docnum
      AND   psgnum IN VALUE tt_segnum( ( sign = 'I' option = 'EQ' ) ( sign = 'I' option = 'EQ' low = is_edidd-segnum ) )
      AND   hlevel EQ is_edidd-hlevel + 1.

*     IDOC segment
      ASSIGN COMPONENT lr_edidd->segnam OF STRUCTURE cs_idoc TO <ls_idoc>.
      IF syst-subrc NE 0.
        RAISE EXCEPTION TYPE cx_t100_msg EXPORTING t100_msgid = 'E0' t100_msgno = 304 t100_msgv1 = CONV string( lr_edidd->segnam ).
      ENDIF.

*     Table segment
      CASE cl_abap_datadescr=>describe_by_data( <ls_idoc> )->type_kind.
        WHEN cl_abap_typedescr=>typekind_table.
          ASSIGN <ls_idoc> TO <lt_idoc>.
          INSERT INITIAL LINE INTO TABLE <lt_idoc> ASSIGNING <ls_idoc>.
      ENDCASE.

*     IDOC segment data
      ASSIGN COMPONENT 'SDATA' OF STRUCTURE <ls_idoc> TO <ls_sdata>.
      IF syst-subrc EQ 0.
        <ls_sdata> = lr_edidd->sdata.
      ELSE.
        <ls_idoc> = lr_edidd->sdata.
      ENDIF.

*     Child segment(s)
      set( EXPORTING is_edidc = is_edidc is_edidd = lr_edidd->* it_edidd = it_edidd CHANGING cs_idoc = <ls_idoc> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD component.
    TYPES:
      tt_posno TYPE RANGE OF posno.

    DATA:
      lo_abap_datadescr TYPE REF TO cl_abap_datadescr,
      lt_component      TYPE        abap_component_tab,
      lr_iapi06         TYPE REF TO edi_iapi06.

*   Child segment(s)
    LOOP AT gt_iapi06 REFERENCE INTO lr_iapi06 USING KEY parpno
      WHERE hlevel EQ CONV hlevel( is_iapi06-hlevel + 1 )
      AND   parpno IN VALUE tt_posno( ( sign = 'I' option = 'EQ' ) ( sign = 'I' option = 'EQ' low = is_iapi06-nr  ) ).
      INSERT LINES OF component( lr_iapi06->* ) INTO TABLE lt_component.
    ENDLOOP.

    IF is_iapi06 IS SUPPLIED.
*     IDOC segment
      lo_abap_datadescr = CAST cl_abap_datadescr( cl_abap_structdescr=>describe_by_name( is_iapi06-segtyp ) ).

      IF lt_component IS NOT INITIAL.
*       Parent segment
        INSERT VALUE abap_componentdescr( name = |SDATA| type = lo_abap_datadescr as_include = abap_true ) INTO lt_component INDEX 1.

*       Structure segment
        lo_abap_datadescr = CAST cl_abap_datadescr( cl_abap_structdescr=>create( lt_component ) ).
      ENDIF.

*     Table segment
      IF is_iapi06-parpno IS INITIAL OR "Root segment
         is_iapi06-occmax GT 1.         "Multiple occurence segment
        lo_abap_datadescr = CAST cl_abap_datadescr( cl_abap_tabledescr=>create( lo_abap_datadescr ) ).
      ENDIF.

      INSERT VALUE abap_componentdescr( name = is_iapi06-segtyp type = lo_abap_datadescr ) INTO TABLE rt_component.

    ELSE.
      rt_component = lt_component.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_idoc_inbound IMPLEMENTATION.
  METHOD process.
    DATA:
      lt_file  TYPE        tt_file,
      lr_file  TYPE REF TO ts_file,
      ls_edidc TYPE        edidc,
      lt_edidd TYPE        edidd_tt.

    TRY.
        go_log->open( ).
        io_channel->open( EXPORTING io_log = go_log iv_rfcdest = iv_rfcdest iv_logsys = is_edidc-sndprn iv_path = iv_path iv_file_name = iv_file_name IMPORTING et_file = lt_file ).
        LOOP AT lt_file REFERENCE INTO lr_file.
          TRY.
              io_channel->read( EXPORTING iv_delete = iv_delete CHANGING cs_file = lr_file->* ).
              open( EXPORTING is_edidc = is_edidc iv_xml = lr_file->data iv_xsltdesc = iv_xsltdesc IMPORTING es_edidc = ls_edidc et_edidd = lt_edidd ).
              close( CHANGING cs_edidc = ls_edidc ct_edidd = lt_edidd ).
            CATCH cx_t100_msg.
              ROLLBACK WORK.
          ENDTRY.
        ENDLOOP.

      CATCH cx_t100_msg ##NO_HANDLER.
    ENDTRY.

    io_channel->close( ).
    go_log->close( abap_true ).
  ENDMETHOD.

  METHOD open.
    FIELD-SYMBOLS:
      <ls_idoc> TYPE any.

    DATA:
      lo_transformation_error TYPE REF TO cx_transformation_error,
      lr_idoc                 TYPE REF TO data.

    CLEAR:
      es_edidc,
      et_edidd.

*   IDOC control record
    es_edidc        = is_edidc.
    es_edidc-direct = '2'.
    es_edidc-rcvprt = 'LS'.
    es_edidc-rcvprn = gv_logsys.
    es_edidc-sndprt = 'LS'.

*   IDOC data definition
    lr_idoc = structdescr( es_edidc ).
    ASSIGN lr_idoc->* TO <ls_idoc>.

*   XML data into IDOC data
    TRY.
        CALL TRANSFORMATION (iv_xsltdesc)
        SOURCE XML iv_xml
        RESULT idoc = <ls_idoc>.
      CATCH cx_transformation_error INTO lo_transformation_error.
        go_log->exception( lo_transformation_error ).
    ENDTRY.

*   IDOC data records
    get( EXPORTING is_idoc = <ls_idoc> CHANGING ct_edidd = et_edidd  ).
  ENDMETHOD.

  METHOD close.
*   Create IDOC
    CALL FUNCTION 'IDOC_WRITE_AND_START_INBOUND'
      EXPORTING
        i_edidc        = cs_edidc
        do_commit      = abap_false
      IMPORTING
        docnum         = cs_edidc-docnum
      TABLES
        i_edidd        = ct_edidd
      EXCEPTIONS
        idoc_not_saved = 1
        OTHERS         = 2 ##FM_SUBRC_OK.
    go_log->syst( syst ).

*   Success message
    go_log->message( iv_msgty = 'S' iv_msgid = 'E0' iv_msgno = 45 iv_msgv1 = cs_edidc-docnum ).

    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_idoc_outbound IMPLEMENTATION.
  METHOD process.
    TYPES:
      BEGIN OF ts_group,
        edidc TYPE tt_edidc,
      END OF ts_group,
      tt_group TYPE STANDARD TABLE OF ts_group WITH EMPTY KEY.

    DATA:
      lo_t100_msg TYPE REF TO cx_t100_msg,
      lt_group    TYPE        tt_group,
      lr_group    TYPE REF TO ts_group,
      lr_edidc    TYPE        tr_edidc,
      lv_path     TYPE        dms_path,
      ls_file     TYPE        ts_file,
      lt_return   TYPE        bapirettab.

*   Group IDOC's
    LOOP AT ct_edidc REFERENCE INTO lr_edidc.
*     Collective group
      IF iv_collective EQ abap_true.
        TRY.
            lr_group = REF ts_group( lt_group[ 1 ] ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.

*     Add group
      IF iv_collective EQ abap_false OR
         lr_group      IS NOT BOUND.
        INSERT INITIAL LINE INTO TABLE lt_group REFERENCE INTO lr_group.
      ENDIF.

      INSERT lr_edidc INTO TABLE lr_group->edidc.
    ENDLOOP.

    LOOP AT lt_group REFERENCE INTO lr_group.
*     Open IDOC's in group
      TRY.
          go_log->open( ).
          open( EXPORTING it_edidc = lr_group->edidc iv_pvrout = iv_pvrout iv_xsltdesc = iv_xsltdesc IMPORTING ev_path = lv_path es_file = ls_file ).
          io_channel->open( io_log = go_log iv_logsys = lr_edidc->rcvprn iv_path = lv_path ).
          io_channel->send( is_file = ls_file iv_overwrite = iv_overwrite ).
        CATCH cx_t100_msg INTO lo_t100_msg.
      ENDTRY.

*     Close IDOC's in group
      TRY.
          go_log->close( IMPORTING et_return = lt_return ).
          close( it_edidc = lr_group->edidc it_return = lt_return ).
        CATCH cx_t100_msg INTO lo_t100_msg.
      ENDTRY.

      CHECK lo_t100_msg IS BOUND.
      EXIT.
    ENDLOOP.

    io_channel->close( ).
  ENDMETHOD.

  METHOD open.
    FIELD-SYMBOLS:
      <ls_idoc> TYPE any.

    DATA:
      lo_sy_dyn_call_error    TYPE REF TO cx_sy_dyn_call_error,
      lo_transformation_error TYPE REF TO cx_transformation_error,
      lt_edidc                TYPE        tt_edidc,
      lr_edidc                TYPE        tr_edidc,
      lt_edidd                TYPE        tt_edidd,
      lr_idoc                 TYPE REF TO data,
      lv_pthnam               TYPE        edi_pthnam,
      lv_path                 TYPE        rstxtlg,
      lv_file                 TYPE        rsawbnobjnm.

    CLEAR:
      ev_path,
      es_file.

*   IDOC data definition
    lr_edidc = it_edidc[ 1 ].
    lr_idoc = structdescr( lr_edidc->* ).
    ASSIGN lr_idoc->* TO <ls_idoc>.

    LOOP AT it_edidc INTO lr_edidc.
*     IDOC control record
      CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_PROCESS'
        EXPORTING
          document_number          = lr_edidc->docnum
        IMPORTING
          idoc_control             = lr_edidc->*
        EXCEPTIONS
          document_foreign_lock    = 1
          document_not_exist       = 2
          document_number_invalid  = 3
          document_is_already_open = 4
          OTHERS                   = 5 ##FM_SUBRC_OK.
      go_log->syst( syst ).

*     IDOC data records
      CALL FUNCTION 'EDI_SEGMENTS_GET_ALL'
        EXPORTING
          document_number         = lr_edidc->docnum
        TABLES
          idoc_containers         = lt_edidd
        EXCEPTIONS
          document_number_invalid = 1
          end_of_document         = 2
          OTHERS                  = 3 ##FM_SUBRC_OK.
      go_log->syst( syst ).

*     Set IDOC data
      set( EXPORTING is_edidc = lr_edidc->* it_edidd = lt_edidd CHANGING cs_idoc = <ls_idoc> ).
    ENDLOOP.

*   Pathname
    TRY.
        CALL FUNCTION iv_pvrout
          EXPORTING
            datatype  = space
            directory = space
            filename  = space
            control   = lr_edidc->*
          IMPORTING
            pathname  = lv_pthnam.

        CALL FUNCTION 'RSDS_SPLIT_PATH_TO_FILENAME'
          EXPORTING
            i_filepath = CONV rsfilenm( lv_pthnam )
          IMPORTING
            e_pathname = lv_path
            e_filename = lv_file.
        ev_path      = lv_path.
        es_file-name = lv_file.

      CATCH cx_sy_dyn_call_error INTO lo_sy_dyn_call_error.
        go_log->exception( lo_sy_dyn_call_error ).
    ENDTRY.

*   IDOC data into XML data
    TRY.
        CALL TRANSFORMATION (iv_xsltdesc)
        SOURCE idoc = <ls_idoc>
        RESULT XML es_file-data.
      CATCH cx_transformation_error INTO lo_transformation_error.
        go_log->exception( lo_transformation_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD close.
    DATA:
      lt_edidc  TYPE        tt_edidc,
      lr_edidc  TYPE        tr_edidc,
      lr_return TYPE REF TO bapiret2,
      lv_status TYPE        edi_status.

    LOOP AT it_edidc INTO lr_edidc.
      lv_status = '18'.
      LOOP AT it_return REFERENCE INTO lr_return.
*       IDOC error status
        IF lr_return->type CA 'AE'.
          lv_status = '02'.
        ENDIF.

*       IDOC status
        CALL FUNCTION 'EDI_DOCUMENT_STATUS_SET'
          EXPORTING
            document_number         = lr_edidc->docnum
            idoc_status             = VALUE edi_ds(
                                        docnum = lr_edidc->docnum
                                        status = lv_status
                                        logdat = syst-datum
                                        logtim = syst-uzeit
                                        repid  = syst-repid
                                        stamqu = 'SAP'
                                        stamid = lr_return->id
                                        stamno = lr_return->number
                                        statxt = lr_return->message
                                        stapa1 = lr_return->message_v1
                                        stapa2 = lr_return->message_v2
                                        stapa3 = lr_return->message_v3
                                        stapa4 = lr_return->message_v4
                                        statyp = lr_return->type )
          EXCEPTIONS
            document_number_invalid = 1
            other_fields_invalid    = 2
            status_invalid          = 3
            OTHERS                  = 4 ##FM_SUBRC_OK.
      ENDLOOP.

      CALL FUNCTION 'EDI_DOCUMENT_CLOSE_PROCESS'
        EXPORTING
          document_number     = lr_edidc->docnum
        IMPORTING
          idoc_control        = lr_edidc->*
        EXCEPTIONS
          document_not_open   = 1
          failure_in_db_write = 2
          parameter_error     = 3
          status_set_missing  = 4
          OTHERS              = 5 ##FM_SUBRC_OK.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_idoc_path IMPLEMENTATION.
  METHOD name.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_idoc_path_readsoft IMPLEMENTATION.
  METHOD name.
    FIELD-SYMBOLS:
      <ls_e1lfb1m> TYPE e1lfb1m.

    DATA:
      ls_edidd TYPE edidd,
      ls_t001  TYPE t001.

    CALL FUNCTION 'EDI_SEGMENT_GET'
      EXPORTING
        document_number         = is_edidc-docnum
        segment_name            = 'E1LFB1M'
      IMPORTING
        idoc_container          = ls_edidd
      EXCEPTIONS
        document_number_invalid = 1
        segment_not_exist       = 2
        OTHERS                  = 3.

    IF syst-subrc EQ 0.
      ASSIGN ls_edidd-sdata TO <ls_e1lfb1m> CASTING.
      CALL FUNCTION 'FI_CC_CONVERT_GLOBAL_TO_LOCAL'
        EXPORTING
          bukrs_global             = <ls_e1lfb1m>-bukrs
        IMPORTING
          bukrs_local              = ls_t001-bukrs
        EXCEPTIONS
          global_cc_not_found      = 1
          no_local_cc_assigned     = 2
          multiple_ccodes_assigned = 3
          OTHERS                   = 4.
    ENDIF.

    IF syst-subrc EQ 0.
      CALL FUNCTION 'T001_SINGLE_READ'
        EXPORTING
          bukrs      = ls_t001-bukrs
        IMPORTING
          wt001      = ls_t001
        EXCEPTIONS
          not_found  = 1
          wrong_call = 2
          OTHERS     = 3.
    ENDIF.

    rv_pathname = |{ COND string( WHEN syst-subrc EQ 0 THEN |{ ls_t001-butxt } | ) }{ COND syst_sysid( WHEN gv_cccategory NE 'P' THEN syst-sysid ) } |.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_idoc_path_supplier IMPLEMENTATION.
  METHOD name.
    rv_pathname = 'suppliers.xml'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_idoc_path_supplier_bank IMPLEMENTATION.
  METHOD name.
    rv_pathname = 'supplierbankaccounts.xml'.
  ENDMETHOD.
ENDCLASS.
