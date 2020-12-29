class ZCL_SAPLMR1M definition
  public
  final
  create public .

public section.

  class-methods AUTHORITY_CHECK
    importing
      !IV_DISPLAY type DISPLAY
      !IS_RBKPV type MRM_RBKPV
    raising
      CX_T100_MSG .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SAPLMR1M IMPLEMENTATION.


  METHOD authority_check.
    DATA:
      ls_lfa1  TYPE lfa1,
      lv_activ TYPE activ_auth.

*   Vendor master data
    CALL FUNCTION 'WY_LFA1_SINGLE_READ'
      EXPORTING
        pi_lifnr                  = is_rbkpv-lifnr
        pi_read_cam               = abap_false
        pi_access_to_addr_version = abap_false
        pi_cvp_behavior           = abap_false
      IMPORTING
        po_lfa1                   = ls_lfa1
      EXCEPTIONS
        no_records_found          = 1
        internal_error            = 2
        lifnr_blocked             = 3
        OTHERS                    = 4.
    CHECK
      syst-subrc    EQ 0           AND
      ls_lfa1-begru IS NOT INITIAL.

*   Change / display activity
    lv_activ = SWITCH activ_auth( iv_display WHEN abap_false THEN '02' ELSE '03' ).

*   Authority check
    AUTHORITY-CHECK OBJECT 'F_LFA1_BEK'
      ID 'BRGRU' FIELD ls_lfa1-begru
      ID 'ACTVT' FIELD lv_activ.
    CHECK syst-subrc NE 0.

    RAISE EXCEPTION TYPE cx_t100_msg
      EXPORTING
        t100_msgid = 'M8'
        t100_msgno = 120.
  ENDMETHOD.
ENDCLASS.
