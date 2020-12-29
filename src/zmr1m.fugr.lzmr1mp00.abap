*&---------------------------------------------------------------------*
*&  Include           LZMR1MP00
*&---------------------------------------------------------------------*

CLASS lcl_log IMPLEMENTATION.
  METHOD open.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = VALUE bal_s_log( )
      IMPORTING
        e_log_handle            = gv_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD close.
    DATA:
      lt_msghndl TYPE        bal_t_msgh,
      lr_msghndl TYPE REF TO balmsghndl,
      ls_bal_msg TYPE        bal_s_msg,
      lv_message TYPE        bapi_msg,
      lr_return  TYPE REF TO bapiret2,
      lv_msgty   TYPE        syst_msgty.

    CLEAR et_return.

*   Messages
    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = VALUE bal_t_logh( ( gv_handle ) )
      IMPORTING
        e_t_msg_handle = lt_msghndl
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.
    CHECK syst-subrc EQ 0.

    IF et_return  IS SUPPLIED    OR  "Return messages
     ( iv_dialog  EQ abap_true   AND "Display message(s) in batch mode
       lt_msghndl IS NOT INITIAL AND
       syst-batch EQ abap_true ).
      LOOP AT lt_msghndl REFERENCE INTO lr_msghndl.
        CALL FUNCTION 'BAL_LOG_MSG_READ'
          EXPORTING
            i_s_msg_handle = VALUE balmsghndl( log_handle = lr_msghndl->log_handle msgnumber = lr_msghndl->msgnumber )
          IMPORTING
            e_s_msg        = ls_bal_msg
          EXCEPTIONS
            log_not_found  = 1
            msg_not_found  = 2
            OTHERS         = 3.
        IF syst-subrc EQ 0.
          INSERT CORRESPONDING bapiret2( ls_bal_msg MAPPING
            type       = msgty
            id         = msgid
            number     = msgno
            message_v1 = msgv1
            message_v2 = msgv2
            message_v3 = msgv3
            message_v4 = msgv4 ) INTO TABLE et_return.
        ELSE.
          CALL FUNCTION 'BAL_LOG_EXCEPTION_READ'
            EXPORTING
              i_s_msg_handle = VALUE balmsghndl( log_handle = lr_msghndl->log_handle msgnumber = lr_msghndl->msgnumber )
            IMPORTING
              e_txt_msg      = lv_message
            EXCEPTIONS
              log_not_found  = 1
              msg_not_found  = 2
              OTHERS         = 3.
          CHECK syst-subrc EQ 0.
          INSERT VALUE bapiret2( type = 'E' message = lv_message ) INTO TABLE et_return.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   Display message(s)
    IF iv_dialog  EQ abap_true   AND
       lt_msghndl IS NOT INITIAL.

      CASE syst-batch.
        WHEN abap_false.
*         In dialog mode: Display message(s) by ALV
          CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
            EXPORTING
              i_t_log_handle       = VALUE bal_t_logh( ( gv_handle ) )
            EXCEPTIONS
              profile_inconsistent = 1
              internal_error       = 2
              no_data_available    = 3
              no_authority         = 4
              OTHERS               = 5 ##FM_SUBRC_OK.

        WHEN abap_true.
*         In batch mode: Display message by message
          LOOP AT et_return REFERENCE INTO lr_return.
            lv_msgty = COND msgty( WHEN syst-tabix EQ lines( et_return ) THEN lr_return->type ELSE COND msgty( WHEN lr_return->type CA 'AEW' THEN 'I' ELSE lr_return->type  ) ).
            IF lr_return->id IS INITIAL OR lr_return->number IS INITIAL.
              MESSAGE lr_return->message TYPE 'E'.
            ELSE.
              MESSAGE ID lr_return->id TYPE lv_msgty NUMBER lr_return->number WITH lr_return->message_v1 lr_return->message_v2 lr_return->message_v3 lr_return->message_v4.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = gv_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD message.
*   Add message
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = gv_handle
        i_s_msg          = VALUE bal_s_msg( msgty = iv_msgty msgid = iv_msgid msgno = iv_msgno msgv1 = iv_msgv1 msgv2 = iv_msgv2 msgv3 = iv_msgv3 msgv4 = iv_msgv4 )
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4 ##FM_SUBRC_OK.
  ENDMETHOD.

  METHOD syst.
    DATA:
      lr_data TYPE REF TO ts_data_char.

*   Only errors
    CHECK is_syst-subrc NE 0.

*   Add message
    message(
      iv_msgty = COND syst_msgty( WHEN is_syst-msgty CA 'AE' THEN is_syst-msgty ELSE 'E' )
      iv_msgid = is_syst-msgid
      iv_msgno = is_syst-msgno
      iv_msgv1 = is_syst-msgv1
      iv_msgv2 = is_syst-msgv2
      iv_msgv3 = is_syst-msgv3
      iv_msgv4 = is_syst-msgv4 ).

*   Add text
    LOOP AT it_data REFERENCE INTO lr_data.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle     = gv_handle
          i_msgty          = is_syst-msgty
          i_text           = lr_data->line
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4 ##FM_SUBRC_OK.
    ENDLOOP.

*   Raise error
    RAISE EXCEPTION TYPE cx_t100_msg
      EXPORTING
        t100_msgid = is_syst-msgid
        t100_msgno = is_syst-msgno
        t100_msgv1 = CONV string( is_syst-msgv1 )
        t100_msgv2 = CONV string( is_syst-msgv2 )
        t100_msgv3 = CONV string( is_syst-msgv3 )
        t100_msgv4 = CONV string( is_syst-msgv4 ).
  ENDMETHOD.

  METHOD exception.
    DATA:
      lo_exception TYPE REF TO cx_root.

*   Root exception
    lo_exception = io_exception.
    WHILE lo_exception->previous IS NOT INITIAL.
      lo_exception = lo_exception->previous.
    ENDWHILE.

*   Add exception
    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING
        i_log_handle     = gv_handle
        i_s_exc          = VALUE bal_s_exc( msgty = 'E' exception = lo_exception )
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4 ##FM_SUBRC_OK.

*   Raise error
    RAISE EXCEPTION TYPE cx_t100_msg.
  ENDMETHOD.
ENDCLASS.
