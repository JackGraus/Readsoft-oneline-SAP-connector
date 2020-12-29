*&---------------------------------------------------------------------*
*&  Include           ZXM08U25
*&---------------------------------------------------------------------*

FIELD-SYMBOLS:
  <ls_e1edk02> TYPE e1edk02.

DATA:
  lt_idoc_status TYPE t_idoc_status,
  ls_t076s       TYPE t076s.

CASE i_idoc_data-segnam.
  WHEN 'E1EDK01'.
    PERFORM mrm_t076s_read TABLES   lt_idoc_status
                           USING    gs_edidc
                                    e_rbkpv-bukrs
                           CHANGING ls_t076s
                                    syst-subrc.

    CHECK
      syst-subrc      EQ 0         AND
      ls_t076s-ediprp EQ abap_true.

    e_rbkpv-rbstat = 'A'.
    e_change       = abap_true.

  WHEN 'E1EDK02'.
    ASSIGN i_idoc_data-sdata TO <ls_e1edk02> CASTING.
    CHECK <ls_e1edk02>-qualf EQ '001'.
    e_rbkpv-sgtxt = <ls_e1edk02>-belnr.
    e_change      = abap_true.
ENDCASE.
