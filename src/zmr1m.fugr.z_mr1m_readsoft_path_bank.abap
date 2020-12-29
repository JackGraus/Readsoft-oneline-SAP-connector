FUNCTION z_mr1m_readsoft_path_bank .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DATATYPE) LIKE  EDIPO-ACTRIG
*"     VALUE(DIRECTORY) LIKE  EDIPO-OUTPUTDIR
*"     VALUE(FILENAME) LIKE  EDIPO-OUTPUTFILE
*"     VALUE(CONTROL) LIKE  EDIDC STRUCTURE  EDIDC
*"  EXPORTING
*"     VALUE(PATHNAME) LIKE  EDI_PATH-PTHNAM
*"----------------------------------------------------------------------

  pathname = NEW lcl_idoc_path_supplier_bank( )->name( control ).

ENDFUNCTION.
