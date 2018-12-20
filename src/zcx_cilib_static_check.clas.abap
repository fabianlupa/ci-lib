"! Static check exception
CLASS zcx_cilib_static_check DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_exception.
    ALIASES:
      gc_default_message FOR zif_cilib_exception~gc_default_message,
      ms_msg FOR zif_cilib_exception~ms_msg,
      ms_t100_key FOR if_t100_message~t100key.
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

    ms_msg = is_msg.

    CLEAR textid.

    IF is_msg IS NOT INITIAL.
      if_t100_message~t100key = VALUE #(
        msgid = is_msg-msgid
        msgno = is_msg-msgno
        attr1 = 'ZIF_CILIB_EXCEPTION~MS_MSG-MSGV1'
        attr2 = 'ZIF_CILIB_EXCEPTION~MS_MSG-MSGV2'
        attr3 = 'ZIF_CILIB_EXCEPTION~MS_MSG-MSGV3'
        attr4 = 'ZIF_CILIB_EXCEPTION~MS_MSG-MSGV4'
      ).
    ELSEIF is_textid IS INITIAL.
      if_t100_message~t100key = zif_cilib_exception=>gc_default_message.
      ms_msg-msgv1 = CAST cl_abap_objectdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->get_relative_name( ).
    ELSE.
      if_t100_message~t100key = is_textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
