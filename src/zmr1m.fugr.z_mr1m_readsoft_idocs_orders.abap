FUNCTION z_mr1m_readsoft_idocs_orders .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(NAST_RECORD) LIKE  NAST STRUCTURE  NAST OPTIONAL
*"     VALUE(ERROR_FLAG) LIKE  EDI_HELP-ERROR_FLAG OPTIONAL
*"     VALUE(PORT_DESCRIPTION) LIKE  EDIPOX STRUCTURE  EDIPOX
*"  TABLES
*"      I_EDIDC STRUCTURE  EDIDC
*"      I_EDIDD STRUCTURE  EDIDD
*"----------------------------------------------------------------------

  NEW lcl_idoc_outbound( )->process(
    EXPORTING
      io_channel  = NEW lcl_channel_ftp( )
      iv_pvrout   = 'Z_MR1M_READSOFT_PATH_ORDER'
      iv_xsltdesc = 'ZMR1M_READSOFT_ORDER'
    CHANGING
      ct_edidc    = i_edidc[] ).

ENDFUNCTION.
