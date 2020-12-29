REPORT zmr1m_readsoft_invoice.

SELECTION-SCREEN BEGIN OF BLOCK s01 WITH FRAME TITLE text-s01.
PARAMETERS:
  pa_rfcde TYPE rfcdest  OBLIGATORY MATCHCODE OBJECT zcprfcdes_ftp,
  pa_path  TYPE dms_path            LOWER CASE.
SELECTION-SCREEN END OF BLOCK s01.

SELECTION-SCREEN BEGIN OF BLOCK s02 WITH FRAME TITLE text-s02.
PARAMETERS:
  pa_sndls TYPE logsys     OBLIGATORY MATCHCODE OBJECT logsys1,
  pa_mesty TYPE edi_mestyp OBLIGATORY MATCHCODE OBJECT zedi_mestyp_invoic02.
SELECTION-SCREEN END OF BLOCK s02.

INITIALIZATION.
  PERFORM init CHANGING pa_rfcde
                        pa_mesty.

AT SELECTION-SCREEN OUTPUT.
  PERFORM hide USING 'PA_PASS'.

START-OF-SELECTION.
  CALL FUNCTION 'Z_MR1M_READSOFT_INVOICE'
    EXPORTING
      iv_rfcdest = pa_rfcde
      iv_path    = pa_path
      iv_sndprn  = pa_sndls
      iv_mestyp  = pa_mesty.

FORM init CHANGING ev_rfcdest TYPE rfcdest
                   ev_mestyp  TYPE edi_mestyp.

* Application server SAPFTP RFC destination
  SELECT rfcdest FROM rfcdes INTO ev_rfcdest UP TO 1 ROWS "#EC CI_GENBUFF
    WHERE rfctype        EQ   'T'
    AND   rfcoptions     LIKE '%N=sapftp%' "SAPFTP
    AND   rfcoptions NOT LIKE '%H=%'.      "Application server
  ENDSELECT.

* ZINVOIC02 message type
  SELECT mestyp FROM edimsg INTO ev_mestyp UP TO 1 ROWS
    WHERE idoctyp EQ 'INVOIC02'
    AND   cimtyp  EQ 'ZINVOIC02'.
  ENDSELECT.
ENDFORM.

FORM hide USING iv_name TYPE clike.
  LOOP AT SCREEN.
    CHECK screen-name EQ iv_name.
    screen-invisible = '1'.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
