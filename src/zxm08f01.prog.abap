*----------------------------------------------------------------------*
***INCLUDE ZXM08F01.
*----------------------------------------------------------------------*
FORM idoc_data USING    is_edidc TYPE edidc
                        it_edidd TYPE edidd_tt
               CHANGING es_data  TYPE ts_mrm_data.
  FIELD-SYMBOLS:
    <ls_e1edk01> TYPE e1edk01,
    <ls_e1edk03> TYPE e1edk03,
    <ls_e1edk28> TYPE e1edk28,
    <ls_e1edkt2> TYPE e1edkt2,
    <lv_bldat>   TYPE bldat.

  DATA:
    lr_edidd TYPE REF TO edidd,
    lv_data  TYPE        string.

  CLEAR es_data.

* IDOC definition
  CALL FUNCTION 'EDI_IDOC_SYNTAX_GET'
    EXPORTING
      pi_idoctyp       = is_edidc-idoctp
      pi_cimtyp        = is_edidc-cimtyp
    TABLES
      pt_syntax_table  = es_data-iapi06
    EXCEPTIONS
      syntax_not_found = 1
      OTHERS           = 2.

  LOOP AT it_edidd REFERENCE INTO lr_edidd
    WHERE docnum EQ is_edidc-docnum.
    CASE lr_edidd->segnam.
*     Linked document data
      WHEN 'Z1LINK'.
        es_data-link-file = lr_edidd->sdata.

*     Base 64 IDOC data
      WHEN 'Z1BASE64'.
        es_data-link-data = es_data-link-data && lr_edidd->sdata.

*     Header data
      WHEN 'E1EDK01'.
        ASSIGN lr_edidd->sdata TO <ls_e1edk01> CASTING.
        es_data-stceg = <ls_e1edk01>-kundeuinr.

*     Header date
      WHEN 'E1EDK03'.
        ASSIGN lr_edidd->sdata TO <ls_e1edk03> CASTING.
        CHECK <ls_e1edk03>-iddat EQ '012'.
        es_data-bldat = <ls_e1edk03>-datum.

*     Bank data
      WHEN 'E1EDK28'.
        ASSIGN lr_edidd->sdata TO <ls_e1edk28> CASTING.
        es_data-iban  = <ls_e1edk28>-biban.
        es_data-bankl = <ls_e1edk28>-brnum.
        es_data-bankn = <ls_e1edk28>-acnum.

*     Header text line
      WHEN 'E1EDKT2'.
        ASSIGN lr_edidd->sdata TO <ls_e1edkt2> CASTING.
        INSERT CORRESPONDING tline( <ls_e1edkt2> ) INTO TABLE es_data-tline.
    ENDCASE.
  ENDLOOP.
ENDFORM.

FORM lifnr_stceg  USING    iv_lifnr TYPE lifnr
                           iv_bukrs TYPE bukrs
                           iv_stceg TYPE stceg
                  CHANGING ct_lfb1  TYPE tt_lfb1.
  DATA:
    lt_lifnr TYPE tt_lifnr,
    lt_bukrs TYPE tt_bukrs.

  CHECK
    iv_stceg IS NOT INITIAL AND "VAT number is supplied
  ( iv_lifnr IS     INITIAL OR  "Vendor is not determined
    iv_bukrs IS     INITIAL ).  "Company code is not determined

  lt_lifnr = COND tt_lifnr( WHEN iv_lifnr IS NOT INITIAL THEN VALUE tt_lifnr( ( sign = 'I' option = 'EQ' low = iv_lifnr ) ) ).
  lt_bukrs = COND tt_bukrs( WHEN iv_bukrs IS NOT INITIAL THEN VALUE tt_bukrs( ( sign = 'I' option = 'EQ' low = iv_bukrs ) ) ).

* Select vendor on VAT number
  SELECT DISTINCT lfb1~lifnr lfb1~bukrs APPENDING TABLE ct_lfb1
    FROM lfa1
    INNER JOIN lfb1
    ON  lfb1~lifnr EQ lfa1~lifnr
    WHERE lfa1~stceg EQ iv_stceg
    AND   lfb1~lifnr IN lt_lifnr
    AND   lfb1~bukrs IN lt_bukrs.
ENDFORM.

FORM lifnr_iban USING    iv_lifnr TYPE lifnr
                         iv_bukrs TYPE bukrs
                         iv_iban  TYPE iban
                CHANGING ct_lfb1  TYPE tt_lfb1.
  DATA:
    lt_lifnr TYPE tt_lifnr,
    lt_bukrs TYPE tt_bukrs.

  CHECK
    iv_iban  IS NOT INITIAL AND "IBAN is supplied
  ( iv_lifnr IS     INITIAL OR  "Vendor is not determined
    iv_bukrs IS     INITIAL ).  "Company code is determined

  lt_lifnr = COND tt_lifnr( WHEN iv_lifnr IS NOT INITIAL THEN VALUE tt_lifnr( ( sign = 'I' option = 'EQ' low = iv_lifnr ) ) ).
  lt_bukrs = COND tt_bukrs( WHEN iv_bukrs IS NOT INITIAL THEN VALUE tt_bukrs( ( sign = 'I' option = 'EQ' low = iv_bukrs ) ) ).

* Select vendot on IBAN
  SELECT DISTINCT lfb1~lifnr lfb1~bukrs APPENDING TABLE ct_lfb1
    FROM tiban
    INNER JOIN lfb1
    ON lfb1~lifnr EQ tiban~tabkey
    WHERE tiban~iban    EQ iv_iban
    AND   tiban~tabname EQ 'LFBK'
    AND   lfb1~lifnr    IN lt_lifnr
    AND   lfb1~bukrs    IN lt_bukrs.
ENDFORM.

FORM lifnr_bankn USING    iv_lifnr TYPE lifnr
                          iv_bukrs TYPE bukrs
                          iv_bankl TYPE bankl
                          iv_bankn TYPE bankn
                 CHANGING ct_lfb1  TYPE tt_lfb1.
  DATA:
    lv_bankn TYPE bankn,
    lt_lifnr TYPE tt_lifnr,
    lt_bukrs TYPE tt_bukrs.

  CHECK
    iv_bankl IS NOT INITIAL AND "Bank account is supplied
    iv_bankn IS NOT INITIAL AND
  ( iv_lifnr IS     INITIAL OR  "Vendor is not determined
    iv_bukrs IS     INITIAL ).  "Company code is determined

* Bank account into internal format
  lv_bankn = |%{ iv_bankn  ALPHA = OUT }|.

  lt_lifnr = COND tt_lifnr( WHEN iv_lifnr IS NOT INITIAL THEN VALUE tt_lifnr( ( sign = 'I' option = 'EQ' low = iv_lifnr ) ) ).
  lt_bukrs = COND tt_bukrs( WHEN iv_bukrs IS NOT INITIAL THEN VALUE tt_bukrs( ( sign = 'I' option = 'EQ' low = iv_bukrs ) ) ).

* Select vendor on bank account
  SELECT DISTINCT lfb1~lifnr lfb1~bukrs APPENDING TABLE ct_lfb1
    FROM lfbk
    INNER JOIN lfb1
    ON lfb1~lifnr EQ lfbk~lifnr
    WHERE lfbk~bankl EQ   iv_bankl
    AND   lfbk~bankn LIKE lv_bankn
    AND   lfb1~lifnr IN   lt_lifnr
    AND   lfb1~bukrs IN   lt_bukrs.
ENDFORM.

FORM change_auto CHANGING it_lfb1   TYPE tt_lfb1
                          cv_lifnr  TYPE lifnr
                          cv_bukrs  TYPE bukrs
                          cv_change TYPE abap_bool.
  TYPES:
    tt_lifnr TYPE SORTED TABLE OF lifnr WITH UNIQUE DEFAULT KEY,
    tt_bukrs TYPE SORTED TABLE OF bukrs WITH UNIQUE DEFAULT KEY.

  DATA:
    lr_lfb1  TYPE REF TO vmds_lfb1_key,
    lt_lifnr TYPE        tt_lifnr,
    lt_bukrs TYPE        tt_bukrs.

  CHECK
    cv_lifnr IS INITIAL OR "Vendor is not determined
    cv_bukrs IS INITIAL.   "Company code is determined

  DELETE ADJACENT DUPLICATES FROM it_lfb1.
  LOOP AT it_lfb1 REFERENCE INTO lr_lfb1.
    INSERT:
      lr_lfb1->lifnr INTO TABLE lt_lifnr,
      lr_lfb1->bukrs INTO TABLE lt_bukrs.
  ENDLOOP.

  IF cv_lifnr          IS INITIAL AND "Vendor is not determined
     lines( lt_lifnr ) EQ 1.          "Vendor is unique
    cv_lifnr  = lt_lifnr[ 1 ].
    cv_change = abap_true.
  ENDIF.

  IF cv_bukrs          IS INITIAL AND "Company code is determined
     lines( lt_bukrs ) EQ 1.          "Company code is unique
    cv_bukrs  = lt_bukrs[ 1 ].
    cv_change = abap_true.
  ENDIF.
ENDFORM.

FORM change_dialog USING    is_edidc  TYPE edidc
                            is_data   TYPE ts_mrm_data
                   CHANGING ct_edidd  TYPE edidd_tt
                            cv_lifnr  TYPE lifnr
                            cv_bukrs  TYPE bukrs
                            cv_change TYPE abap_bool.
  FIELD-SYMBOLS:
    <ls_e1edk03> TYPE e1edk03.

  DATA:
    lo_container   TYPE REF TO cl_gui_custom_container,
    lo_html_viewer TYPE REF TO cl_gui_html_viewer,
    lt_data        TYPE        solix_tab,
    lv_url         TYPE        url,
    lr_iapi06      TYPE REF TO edi_iapi06,
    lr_edidd       TYPE REF TO edidd.

* When required
  CHECK
    is_data-link-bindata IS BOUND     AND "Linked document
  ( cv_lifnr             IS INITIAL   OR  "No vendor
    cv_bukrs             IS INITIAL   OR  "No company code
    is_data-bldat        IS INITIAL  ).   "No invoice date

  CLEAR rbkp.

* Linked document
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = is_data-link-bindata->*
    TABLES
      binary_tab = lt_data.

* Container
  CREATE OBJECT lo_container
    EXPORTING
      container_name              = 'HTML_CONTROL'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  CHECK syst-subrc EQ 0.

* HTML control
  CREATE OBJECT lo_html_viewer
    EXPORTING
      parent             = lo_container
    EXCEPTIONS
      cntl_error         = 1
      cntl_install_error = 2
      dp_install_error   = 3
      dp_error           = 4
      OTHERS             = 5.
  CHECK syst-subrc EQ 0.

* Set viewer document
  CALL METHOD lo_html_viewer->load_data
    EXPORTING
      url                    = is_data-link-file-name
      type                   = 'application'
      subtype                = is_data-link-file-type
    IMPORTING
      assigned_url           = lv_url
    CHANGING
      data_table             = lt_data
    EXCEPTIONS
      dp_invalid_parameter   = 1
      dp_error_general       = 2
      cntl_error             = 3
      html_syntax_notcorrect = 4
      OTHERS                 = 5.
  CHECK syst-subrc EQ 0.

* Show viewer
  lo_html_viewer->show_url(
    EXPORTING
      url                    = lv_url
    EXCEPTIONS
      cntl_error             = 1
      cnht_error_not_allowed = 2
      cnht_error_parameter   = 3
      dp_error_general       = 4
      OTHERS                 = 5 ).
  CHECK syst-subrc EQ 0.

* Value to screen
  rbkp-bukrs = cv_bukrs.      "Company code
  rbkp-lifnr = cv_lifnr.      "Vendor
  rbkp-bldat = is_data-bldat. "Invoice date

* Call screen
  CALL SCREEN 9000.
  cv_change = xsdbool( gv_ucomm EQ 'OKAY' ).

* Free container and child HTML control
  lo_container->free( ).

* Value from screen
  CHECK cv_change EQ abap_true.
  cv_bukrs = rbkp-bukrs. "Company code
  cv_lifnr = rbkp-lifnr. "Vendor

* Screen to IDOC
  TRY.
      lr_iapi06 = REF edi_iapi06( is_data-iapi06[ idoctyp = is_edidc-idoctp cimtyp = is_edidc-cimtyp segtyp = 'E1EDK03' ] ).
      LOOP AT ct_edidd REFERENCE INTO lr_edidd
        WHERE docnum EQ is_edidc-docnum.

*       IDOC segment equal to or after date segment
        CHECK is_data-iapi06[ idoctyp = is_edidc-idoctp cimtyp = is_edidc-cimtyp segtyp = lr_edidd->segnam ]-nr GE lr_iapi06->nr.

        IF lr_edidd->segnam EQ lr_iapi06->segtyp.
          ASSIGN lr_edidd->sdata TO <ls_e1edk03> CASTING.
          IF <ls_e1edk03>-iddat EQ '012'.
            <ls_e1edk03>-datum = rbkp-bldat.
            EXIT.
          ENDIF.
        ELSE.
          INSERT VALUE edidd( mandt = is_edidc-mandt docnum = is_edidc-docnum segnam = lr_iapi06->segtyp sdata = VALUE e1edk03( iddat = '012' datum = rbkp-bldat ) ) INTO ct_edidd INDEX syst-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.
ENDFORM.
