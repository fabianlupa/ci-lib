*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.12.2018 at 19:13:31
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCILIB_HOST_V...................................*
TABLES: ZCILIB_HOST_V, *ZCILIB_HOST_V. "view work areas
CONTROLS: TCTRL_ZCILIB_HOST_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZCILIB_HOST_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCILIB_HOST_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCILIB_HOST_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCILIB_HOST_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCILIB_HOST_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCILIB_HOST_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCILIB_HOST_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCILIB_HOST_V_TOTAL.

*.........table declarations:.................................*
TABLES: ZCILIB_HOST                    .
