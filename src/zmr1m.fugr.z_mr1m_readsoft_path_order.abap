FUNCTION Z_MR1M_READSOFT_PATH_ORDER .
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

  pathname = NEW lcl_idoc_path_supplier( )->name( control ).

ENDFUNCTION.
