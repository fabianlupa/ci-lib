"! Message tools
CLASS zcl_cilib_util_msg_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_msg_from_sy RETURNING VALUE(rs_msg) TYPE zcilib_msg,
      get_msg_from_exc IMPORTING ix_exception  TYPE REF TO cx_root
                       RETURNING VALUE(rs_msg) TYPE zcilib_msg,
      get_msg_from_string IMPORTING iv_message    TYPE csequence
                          RETURNING VALUE(rs_msg) TYPE zcilib_msg.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_msg_tools IMPLEMENTATION.
  METHOD get_msg_from_exc.
    DATA(ls_backup) = CORRESPONDING zcilib_msg( syst ).
    cl_message_helper=>set_msg_vars_for_any( ix_exception ).
    rs_msg = CORRESPONDING #( syst ).
    MOVE-CORRESPONDING ls_backup TO syst.
  ENDMETHOD.

  METHOD get_msg_from_string.
    DATA(ls_backup) = CORRESPONDING zcilib_msg( syst ).
    cl_message_helper=>set_msg_vars_for_clike( iv_message ).
    rs_msg = CORRESPONDING #( syst ).
    MOVE-CORRESPONDING ls_backup TO syst.
  ENDMETHOD.

  METHOD get_msg_from_sy.
    rs_msg = CORRESPONDING #( syst ).
  ENDMETHOD.
ENDCLASS.
