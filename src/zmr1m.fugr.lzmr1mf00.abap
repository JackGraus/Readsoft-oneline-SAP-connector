*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 13.11.2020 at 11:11:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVTBDLS.........................................*
FORM GET_DATA_ZVTBDLS.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM TBDLS WHERE
(VIM_WHERETAB) .
    CLEAR ZVTBDLS .
ZVTBDLS-LOGSYS =
TBDLS-LOGSYS .
ZVTBDLS-HOST =
TBDLS-HOST .
ZVTBDLS-USER =
TBDLS-USER .
ZVTBDLS-PASSWD =
TBDLS-PASSWD .
<VIM_TOTAL_STRUC> = ZVTBDLS.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZVTBDLS .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZVTBDLS.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZVTBDLS-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM TBDLS WHERE
  LOGSYS = ZVTBDLS-LOGSYS .
    IF SY-SUBRC = 0.
    DELETE TBDLS .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM TBDLS WHERE
  LOGSYS = ZVTBDLS-LOGSYS .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR TBDLS.
    ENDIF.
TBDLS-LOGSYS =
ZVTBDLS-LOGSYS .
TBDLS-HOST =
ZVTBDLS-HOST .
TBDLS-USER =
ZVTBDLS-USER .
TBDLS-PASSWD =
ZVTBDLS-PASSWD .
    IF SY-SUBRC = 0.
    UPDATE TBDLS .
    ELSE.
    INSERT TBDLS .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZVTBDLS-UPD_FLAG,
STATUS_ZVTBDLS-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZVTBDLS.
  SELECT SINGLE * FROM TBDLS WHERE
LOGSYS = ZVTBDLS-LOGSYS .
ZVTBDLS-LOGSYS =
TBDLS-LOGSYS .
ZVTBDLS-HOST =
TBDLS-HOST .
ZVTBDLS-USER =
TBDLS-USER .
ZVTBDLS-PASSWD =
TBDLS-PASSWD .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZVTBDLS USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZVTBDLS-LOGSYS TO
TBDLS-LOGSYS .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'TBDLS'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN TBDLS TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'TBDLS'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*