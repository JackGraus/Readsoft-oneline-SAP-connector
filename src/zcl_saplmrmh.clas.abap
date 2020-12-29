class ZCL_SAPLMRMH definition
  public
  final
  create public .

public section.

  class-methods CLASS_CONSTRUCTOR .
  class-methods INSTANCE
    returning
      value(RO_ME) type ref to ZCL_SAPLMRMH .
  methods SET
    importing
      !IV_FILE_NAME type DFILE_NAME
      !IV_TYPE type SO_OBJ_TP
      !IV_DATA type STRING
      !IT_TLINE type TLINE_TAB
    returning
      value(RR_DATA) type ref to XSTRING .
  methods CREATE
    importing
      !IV_DOCNUM type EDI_DOCNUM
      !IV_BELNR type RE_BELNR
      !IV_GJAHR type GJAHR
    changing
      !CV_SUBRC type SYST_SUBRC
      !CT_EDIDS type T_IDOC_STATUS .
protected section.
private section.

  class-data GO_ME type ref to ZCL_SAPLMRMH .
  data GV_OBJ_DES type SO_OBJ_DES .
  data GV_OBJ_TP type SO_OBJ_TP .
  data GV_DATA type XSTRING .
  data GT_TLINE type TLINE_TAB .

  methods EDIDS
    importing
      !IV_DOCNUM type EDI_DOCNUM
      !IV_MSGID type SYST_MSGID
      !IV_MSGNO type SYST_MSGNO
      !IV_MSGV1 type SYST_MSGV optional
      !IV_MSGV2 type SYST_MSGV optional
      !IV_MSGV3 type SYST_MSGV optional
      !IV_MSGV4 type SYST_MSGV optional
    changing
      !CT_EDIDS type T_IDOC_STATUS .
ENDCLASS.



CLASS ZCL_SAPLMRMH IMPLEMENTATION.


  METHOD class_constructor.
    CREATE OBJECT go_me.
  ENDMETHOD.


  METHOD create.
    DATA:
      lo_root       TYPE REF TO cx_root,
      lt_data       TYPE        solix_tab,
      lv_length     TYPE        i,
      ls_soodk      TYPE        soodk,
      ls_sofolenti1 TYPE        sofolenti1.

    CHECK cv_subrc EQ 0.

    IF gv_data  IS NOT INITIAL.
*     Table data
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = gv_data
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab    = lt_data.

*     Root folder
      CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
        EXPORTING
          region                = 'B'
        IMPORTING
          folder_id             = ls_soodk
        EXCEPTIONS
          communication_failure = 1
          owner_not_exist       = 2
          system_failure        = 3
          x_error               = 4
          OTHERS                = 5.
      cv_subrc = syst-subrc.
      IF cv_subrc NE 0.
        edids( EXPORTING iv_docnum = iv_docnum iv_msgid = 'SWUO' iv_msgno = 804 CHANGING ct_edids = ct_edids ).
        RETURN.
      ENDIF.

*     Office document
      CALL FUNCTION 'SO_DOCUMENT_INSERT_API1'
        EXPORTING
          folder_id                  = CONV so_obj_id( ls_soodk )
          document_data              = VALUE sodocchgi1( obj_descr = gv_obj_des doc_size = lv_length )
          document_type              = gv_obj_tp
        IMPORTING
          document_info              = ls_sofolenti1
        TABLES
          contents_hex               = lt_data
        EXCEPTIONS
          folder_not_exist           = 1
          document_type_not_exist    = 2
          operation_no_authorization = 3
          parameter_error            = 4
          x_error                    = 5
          enqueue_error              = 6
          OTHERS                     = 7.
      cv_subrc = syst-subrc.
      IF cv_subrc NE 0.
        edids( EXPORTING iv_docnum = iv_docnum iv_msgid = 'SWUO' iv_msgno = 806 iv_msgv1 = CONV syst_msgv( ls_soodk ) CHANGING ct_edids = ct_edids ).
        RETURN.
      ENDIF.

*     Link
      TRY.
          cl_binary_relation=>create_link(
            EXPORTING
              is_object_a = VALUE sibflporb( instid = VALUE rbkp_awobj( belnr = iv_belnr gjahr = iv_gjahr ) typeid = 'BUS2081' catid  = 'BO')
              is_object_b = VALUE sibflporb( instid = ls_sofolenti1-doc_id                                  typeid = 'MESSAGE' catid  = 'BO')
              ip_reltype  = 'ATTA' ).

        CATCH
          cx_obl_parameter_error
          cx_obl_model_error
          cx_obl_internal_error INTO lo_root.
          cv_subrc = 4.
          edids( EXPORTING iv_docnum = iv_docnum iv_msgid = 'SWUO' iv_msgno = 26 iv_msgv1 = CONV syst_msgv( lo_root->get_text( ) ) CHANGING ct_edids = ct_edids ).
          RETURN.
      ENDTRY.
    ENDIF.

    IF gt_tline IS NOT INITIAL.
*     Header note
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header   = VALUE thead( tdobject = 'RBKP' tdname = CONV tdobname( |{ iv_belnr }{ iv_gjahr }| ) tdid = '0001' tdspras = syst-langu tdtitle = 'Note' )
          insert   = abap_true
        TABLES
          lines    = gt_tline
        EXCEPTIONS
          id       = 1
          language = 2
          name     = 3
          object   = 4
          OTHERS   = 5.
      cv_subrc = syst-subrc.
      IF cv_subrc NE 0.
        edids( EXPORTING iv_docnum = iv_docnum iv_msgid = syst-msgid iv_msgno = syst-msgno iv_msgv1 = syst-msgv1 iv_msgv2 = syst-msgv2 iv_msgv3 = syst-msgv3 iv_msgv4 = syst-msgv4 CHANGING ct_edids = ct_edids ).
        RETURN.
      ENDIF.

      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          savemode_direct = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD edids.
    DATA:
      lv_segnum TYPE edi_segnum.

    CLEAR ct_edids.
    PERFORM mrm_idoc_status_fill IN PROGRAM saplmrmh
      TABLES ct_edids
      USING  iv_docnum '51' iv_msgid iv_msgno iv_msgv1 iv_msgv2 iv_msgv3 iv_msgv4 lv_segnum space space.
  ENDMETHOD.


  METHOD instance.
    ro_me = go_me.
  ENDMETHOD.


  METHOD set.
*   Linked document
    gv_obj_des = iv_file_name.
    gv_obj_tp  = iv_type.

*   Decode linked document
    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data                  = iv_data
      IMPORTING
        bindata                  = gv_data
      EXCEPTIONS
        ssf_krn_error            = 1
        ssf_krn_noop             = 2
        ssf_krn_nomemory         = 3
        ssf_krn_opinv            = 4
        ssf_krn_input_data_error = 5
        ssf_krn_invalid_par      = 6
        ssf_krn_invalid_parlen   = 7
        OTHERS                   = 8 ##FM_SUBRC_OK.

*   Header text lines
    gt_tline = it_tline.

*   Decoded linked document
    rr_data = REF xstring( gv_data ).
  ENDMETHOD.
ENDCLASS.
