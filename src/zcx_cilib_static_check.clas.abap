"! Static check exception
CLASS zcx_cilib_static_check DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_t100_message,
      zif_cilib_exception.
    ALIASES:
      gc_default_message FOR zif_cilib_exception~gc_default_message.
    METHODS:
      constructor IMPORTING is_textid   TYPE scx_t100key OPTIONAL
                            is_msg      TYPE zcilib_msg OPTIONAL
                            ix_previous TYPE REF TO cx_root OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_cilib_static_check IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = ix_previous ).

    IF is_msg IS NOT INITIAL.
      if_t100_message~t100key = zif_cilib_exception=>gc_t100_message.
      if_t100_message~t100key-msgid = is_msg-msgid.
      if_t100_message~t100key-msgno = is_msg-msgno.
      zif_cilib_exception~mv_msgv1 = is_msg-msgv1.
      zif_cilib_exception~mv_msgv2 = is_msg-msgv2.
      zif_cilib_exception~mv_msgv3 = is_msg-msgv3.
      zif_cilib_exception~mv_msgv4 = is_msg-msgv4.
    ELSEIF is_textid IS NOT INITIAL.
      if_t100_message~t100key = is_textid.
    ELSE.
      if_t100_message~t100key = zif_cilib_exception~gc_default_message.
      zif_cilib_exception~mv_msgv1 = cl_abap_classdescr=>get_class_name( me ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
