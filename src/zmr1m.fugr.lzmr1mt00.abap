*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 13.11.2020 at 11:11:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZVTBDLS.........................................*
TABLES: ZVTBDLS, *ZVTBDLS. "view work areas
CONTROLS: TCTRL_ZVTBDLS
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZVTBDLS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZVTBDLS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZVTBDLS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZVTBDLS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVTBDLS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZVTBDLS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZVTBDLS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZVTBDLS_TOTAL.

*.........table declarations:.................................*
TABLES: TBDLS                          .
