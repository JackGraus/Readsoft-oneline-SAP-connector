FUNCTION z_mr1m_readsoft_invoice .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_RFCDEST) TYPE  RFCDEST
*"     REFERENCE(IV_PATH) TYPE  DMS_PATH
*"     REFERENCE(IV_SNDPRN) TYPE  SNDPRN
*"     REFERENCE(IV_MESTYP) TYPE  EDI_MESTYP
*"----------------------------------------------------------------------

  NEW lcl_idoc_inbound( )->process(
    io_channel   = NEW lcl_channel_ftp( )
    iv_rfcdest   = iv_rfcdest
    iv_path      = iv_path
    iv_file_name = `[[:xdigit:]]{32}\.xml`
    iv_delete    = abap_false
    is_edidc     = VALUE edidc( mescod = 'MM' sndpor = 'READSOFT' sndprn = iv_sndprn mestyp = iv_mestyp idoctp = 'INVOIC02' cimtyp = 'ZINVOIC02' )
    iv_xsltdesc  = 'ZMR1M_READSOFT_INVOICE' ).

ENDFUNCTION.
