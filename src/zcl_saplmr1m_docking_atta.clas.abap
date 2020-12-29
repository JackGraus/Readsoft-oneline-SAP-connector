class ZCL_SAPLMR1M_DOCKING_ATTA definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods INSTANCE
    returning
      value(RO_ME) type ref to ZCL_SAPLMR1M_DOCKING_ATTA .
  methods PROCESS
    importing
      !IV_REPID type SYST-REPID
      !IV_DYNNR type SYST_DYNNR
      !IS_RBKPV type MRM_RBKPV .
protected section.
private section.

  class-data GO_ME type ref to ZCL_SAPLMR1M_DOCKING_ATTA .
  data GO_CONTAINER type ref to CL_GUI_DOCKING_CONTAINER .
  data GO_HTML_VIEWER type ref to CL_GUI_HTML_VIEWER .
  data GS_LINK type OBL_S_LINK .

  methods DOCKING
    importing
      !IV_REPID type SYST-REPID
      !IV_DYNNR type SYST_DYNNR
    returning
      value(RV_ACTIVE) type ABAP_BOOL .
  methods LINK
    importing
      !IS_RBKPV type MRM_RBKPV
    returning
      value(RV_UPDKZ) type UPDKZ_D .
  methods DOCUMENT .
ENDCLASS.



CLASS ZCL_SAPLMR1M_DOCKING_ATTA IMPLEMENTATION.


  METHOD class_constructor.
    CREATE OBJECT go_me.
  ENDMETHOD.


  METHOD docking.
    DATA:
      lv_parva TYPE xuvalue,
      lv_ratio TYPE i.

    rv_active = abap_false.

*   Docker container not yet instantiated
    IF go_container   IS NOT BOUND AND
       go_html_viewer IS NOT BOUND.

*     Get user parameter
      GET PARAMETER ID 'ZMR1M_RATIO' FIELD lv_parva.
      CHECK syst-subrc EQ 0.

*     Ratio
      TRY.
          lv_ratio = lv_parva.
        CATCH
          cx_sy_conversion_no_number
          cx_sy_conversion_overflow.
          EXIT.
      ENDTRY.
      CHECK lv_ratio IS NOT INITIAL.

*     Docking container
      IF go_container IS NOT BOUND.
        CREATE OBJECT go_container
          EXPORTING
            repid                       = iv_repid
            dynnr                       = iv_dynnr
            side                        = cl_gui_docking_container=>dock_at_left
            ratio                       = lv_ratio
          EXCEPTIONS
            cntl_error                  = 1
            cntl_system_error           = 2
            create_error                = 3
            lifetime_error              = 4
            lifetime_dynpro_dynpro_link = 5
            OTHERS                      = 6.
        CHECK syst-subrc EQ 0.
      ENDIF.

*     HTML viewer
      IF go_html_viewer IS NOT BOUND.
        CREATE OBJECT go_html_viewer
          EXPORTING
            parent             = go_container
          EXCEPTIONS
            cntl_error         = 1
            cntl_install_error = 2
            dp_install_error   = 3
            dp_error           = 4
            OTHERS             = 5.
      ENDIF.
    ENDIF.

*   Active when docker exists
    rv_active = xsdbool( go_container IS BOUND AND go_html_viewer IS BOUND ).
  ENDMETHOD.


  METHOD document.
    DATA:
      ls_document_data TYPE sofolenti1,
      lt_data          TYPE solix_tab,
      lv_url           TYPE url.

*   Read archivelink document
    CALL FUNCTION 'SO_DOCUMENT_READ_API1'
      EXPORTING
        document_id                = CONV so_entryid( gs_link-instid_b )
      IMPORTING
        document_data              = ls_document_data
      TABLES
        contents_hex               = lt_data
      EXCEPTIONS
        document_id_not_exist      = 1
        operation_no_authorization = 2
        x_error                    = 3
        OTHERS                     = 4.
    CHECK syst-subrc EQ 0.

*   Update viewer document
    go_html_viewer->load_data(
      EXPORTING
        url                    = ls_document_data-obj_descr
        type                   = 'application'
        subtype                = ls_document_data-obj_type
      IMPORTING
        assigned_url           = lv_url
      CHANGING
        data_table             = lt_data
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5 ).
    CHECK syst-subrc EQ 0.

*   Show viewer
    go_html_viewer->show_url(
      EXPORTING
        url                    = lv_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5 ).
    CHECK syst-subrc EQ 0.
  ENDMETHOD.


  METHOD instance.
    ro_me = go_me.
  ENDMETHOD.


  METHOD link.
    DATA:
      lt_link TYPE obl_t_link,
      lr_link TYPE REF TO obl_s_link.

    rv_updkz = abap_false.

    TRY.
*       Archivelink
        cl_binary_relation=>read_links_of_binrel(
          EXPORTING
            is_object   = VALUE sibflporb( instid = VALUE rbkp_awobj( belnr = is_rbkpv-belnr gjahr = is_rbkpv-gjahr ) typeid = 'BUS2081' catid = 'BO' )
            ip_relation = 'ATTA'
            ip_role     = 'GOSAPPLOBJ'
          IMPORTING
            et_links    = lt_link ).

*       Most recent link
        SORT lt_link BY utctime DESCENDING.
        lr_link = REF obl_s_link( lt_link[ 1 ] ).

*       Update flag
        rv_updkz = xsdbool( gs_link NE lr_link->* ).

*       Update link
        CHECK rv_updkz EQ abap_true.
        gs_link = lr_link->*.

*       Show dock
        go_container->set_visible( cl_gui_docking_container=>visible_true ).

      CATCH cx_obl_parameter_error .
      CATCH cx_obl_internal_error .
      CATCH cx_obl_model_error .

      CATCH cx_sy_itab_line_not_found .
*       Update flag when previous link was not initial
        rv_updkz = boolc( gs_link IS NOT INITIAL ).

*       Hide dock
        go_container->set_visible( cl_gui_docking_container=>visible_false ).
    ENDTRY.
  ENDMETHOD.


  METHOD process.
    CHECK
      docking( iv_repid = iv_repid iv_dynnr = iv_dynnr ) AND "When docking container is and should be instantiated
      link(    is_rbkpv ).                                   "When archivelink exists
    document( ). "Set archivelink document
  ENDMETHOD.
ENDCLASS.
