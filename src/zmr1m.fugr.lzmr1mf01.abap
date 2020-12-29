*&---------------------------------------------------------------------*
*&  Include           LZMR1MF01
*&---------------------------------------------------------------------*

FORM ftp_on_commit.
  PERFORM ftp_commit   ON COMMIT.
  PERFORM ftp_rollback ON ROLLBACK.
ENDFORM.

FORM ftp_commit.
  lcl_channel_ftp=>lif_channel~commit( ).
ENDFORM.

FORM ftp_rollback.
  lcl_channel_ftp=>lif_channel~rollback( ).
ENDFORM.
