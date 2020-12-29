class ZCL_VENDOR_ADD_DATA_BI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_VENDOR_ADD_DATA_BI .
protected section.
private section.

  data GR_LFA1 type ref to LFA1 .

  methods SET
    importing
      !IV_MESTYP type EDI_MESTYP
    changing
      !CS_SDATA type ANY
      !CT_EDIDD type EDIDD_TT .
ENDCLASS.



CLASS ZCL_VENDOR_ADD_DATA_BI IMPLEMENTATION.


  method IF_EX_VENDOR_ADD_DATA_BI~CHECK_DATA_ROW.
  endmethod.


  METHOD if_ex_vendor_add_data_bi~fill_ale_segments_own_data.
    TYPES:
      tt_t042z TYPE STANDARD TABLE OF t042z WITH EMPTY KEY
                                            WITH UNIQUE HASHED KEY key COMPONENTS land1 zlsch.

    FIELD-SYMBOLS:
      <ls_lfa1>    TYPE lfa1,
      <ls_lfb1>    TYPE lfb1,
      <ls_lfbk>    TYPE lfbk,
      <ls_e1lfbkm> TYPE e1lfbkm.

    DATA:
      ls_z1lfa1m              TYPE        z1lfa1m,
      ls_z1lfb1m              TYPE        z1lfb1m,
      ls_z1lfb1m_payment_term TYPE        z1lfb1m_payment_term,
      lr_edidd                TYPE REF TO edidd,
      lv_idoctyp              TYPE        edi_idoctp,
      lv_active               TYPE        abap_bool,
      ls_t005                 TYPE        t005,
      lt_ttext                TYPE        ty_ttext,
      lt_t042z                TYPE        tt_t042z,
      lv_zlsch                TYPE        dzlsch.

    CASE i_segment_name.
      WHEN 'E1LFA1M'.
        ASSIGN i_data_ref->* TO <ls_lfa1>.
        gr_lfa1 = REF lfa1( <ls_lfa1> ).

*       IDOC extension
        CALL FUNCTION 'CHECK_IDOCTYPE_CUSTOMIZING'
          EXPORTING
            mestype         = i_message_type
          IMPORTING
            idoctyp         = lv_idoctyp
            cimtyp          = e_cimtype
          EXCEPTIONS
            entry_not_found = 1
            OTHERS          = 2.

        CALL FUNCTION 'IDOC_REDUCTION_SEGMENT_TEST'
          EXPORTING
            message_type = i_message_type
            segment_type = 'Z1LFA1M'
          IMPORTING
            active       = lv_active.
        CHECK lv_active EQ abap_true.

*       Short name
        CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
          EXPORTING
            address_1                      = CORRESPONDING adrs1( gr_lfa1->* )
            address_type                   = '1'
          IMPORTING
            address_short_form             = ls_z1lfa1m-addr_short
          EXCEPTIONS
            address_blocked                = 1
            person_blocked                 = 2
            contact_person_blocked         = 3
            addr_to_be_formated_is_blocked = 4
            OTHERS                         = 5.

*       Country currency
        CALL FUNCTION 'T005_SINGLE_READ'
          EXPORTING
            t005_land1 = gr_lfa1->land1
          IMPORTING
            wt005      = ls_t005
          EXCEPTIONS
            not_found  = 1
            OTHERS     = 2.
        IF syst-subrc EQ 0.
          ls_z1lfa1m-waers = ls_t005-waers.
          set( EXPORTING iv_mestyp = i_message_type CHANGING cs_sdata = ls_z1lfa1m ct_edidd = t_idoc_data ).
        ENDIF.

      WHEN 'E1LFB1M'.
        ASSIGN i_data_ref->* TO <ls_lfb1>.

        CALL FUNCTION 'IDOC_REDUCTION_SEGMENT_TEST'
          EXPORTING
            message_type = i_message_type
            segment_type = 'Z1LFB1M'
          IMPORTING
            active       = lv_active.

*       First payment term description
        IF lv_active EQ abap_true.
          CALL FUNCTION 'FI_PRINT_ZTERM'
            EXPORTING
              i_zterm         = <ls_lfb1>-zterm
              i_langu         = gr_lfa1->spras
            TABLES
              t_ztext         = lt_ttext
            EXCEPTIONS
              zterm_not_found = 1
              OTHERS          = 2.
          IF syst-subrc EQ 0.
            ls_z1lfb1m-zterm_text = VALUE #( lt_ttext[ 1 ]-text1 OPTIONAL ).
            set( EXPORTING iv_mestyp = i_message_type CHANGING cs_sdata = ls_z1lfb1m ct_edidd = t_idoc_data ).
          ENDIF.
        ENDIF.

        CALL FUNCTION 'IDOC_REDUCTION_SEGMENT_TEST'
          EXPORTING
            message_type = i_message_type
            segment_type = 'Z1LFB1M_PAYMENT_TERM'
          IMPORTING
            active       = lv_active.

*       Payment methods
        IF lv_active EQ abap_true.
          CALL FUNCTION 'FI_PAYMENT_METHOD_LIST_GET'
            EXPORTING
              i_zbukr   = <ls_lfb1>-bukrs
              i_land1   = gr_lfa1->land1
            TABLES
              t_t042z   = lt_t042z
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF syst-subrc EQ 0.
            DO 10 TIMES.
              TRY.
                  lv_zlsch = substring( val = <ls_lfb1>-zwels off = syst-index - 1 len = 1 ).
                  CHECK lv_zlsch IS NOT INITIAL.
                  ls_z1lfb1m_payment_term = VALUE z1lfb1m_payment_term(
                    zlsch = lv_zlsch
                    text = VALUE text1_042z( lt_t042z[ KEY key land1 = gr_lfa1->land1 zlsch = lv_zlsch ]-text1 OPTIONAL ) ).
                  set( EXPORTING iv_mestyp = i_message_type CHANGING cs_sdata = ls_z1lfb1m_payment_term ct_edidd = t_idoc_data ).
                CATCH cx_sy_range_out_of_bounds.
                  EXIT.
              ENDTRY.
            ENDDO.
          ENDIF.
        ENDIF.

      WHEN 'E1LFBKM'.
        ASSIGN i_data_ref->* TO <ls_lfbk>.
        ASSIGN t_idoc_data[ lines( t_idoc_data ) ]-sdata TO <ls_e1lfbkm> CASTING.

*       Iban
        CALL FUNCTION 'READ_IBAN'
          EXPORTING
            i_banks        = <ls_lfbk>-banks
            i_bankl        = <ls_lfbk>-bankl
            i_bankn        = <ls_lfbk>-bankn
            i_bkont        = <ls_lfbk>-bkont
            i_bkref        = <ls_lfbk>-bkref
          IMPORTING
            e_iban         = <ls_e1lfbkm>-iban
          EXCEPTIONS
            iban_not_found = 1
            OTHERS         = 2.
    ENDCASE.
  ENDMETHOD.


  method IF_EX_VENDOR_ADD_DATA_BI~FILL_BI_TABLE_WITH_OWN_SEGMENT.
  endmethod.


  method IF_EX_VENDOR_ADD_DATA_BI~FILL_FT_TABLE_USING_DATA_ROWS.
  endmethod.


  method IF_EX_VENDOR_ADD_DATA_BI~MODIFY_BI_STRUCT_FROM_STD_SEG.
  endmethod.


  method IF_EX_VENDOR_ADD_DATA_BI~PASS_NON_STANDARD_SEGMENT.
  endmethod.


  method IF_EX_VENDOR_ADD_DATA_BI~PROCESS_ALE_OWN_CHANGE_POINTER.
  endmethod.


  METHOD set.
    DATA:
      lr_edidd TYPE REF TO edidd.

    CHECK cs_sdata IS NOT INITIAL.

    INSERT INITIAL LINE INTO TABLE ct_edidd REFERENCE INTO lr_edidd.
    lr_edidd->segnam = cl_abap_structdescr=>describe_by_data( cs_sdata )->get_relative_name( ).
    lr_edidd->sdata  = cs_sdata.

    CALL FUNCTION 'IDOC_REDUCTION_FIELD_REDUCE'
      EXPORTING
        message_type = iv_mestyp
        segment_type = lr_edidd->segnam
        segment_data = lr_edidd->sdata
      IMPORTING
        segment_data = lr_edidd->sdata.
  ENDMETHOD.
ENDCLASS.
