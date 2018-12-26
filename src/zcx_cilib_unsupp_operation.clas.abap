"! Unsupported operation exception
CLASS zcx_cilib_unsupp_operation DEFINITION
  PUBLIC
  INHERITING FROM zcx_cilib_dynamic_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_name_and_key,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MV_OPERATION',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_name_and_key.
    METHODS:
      constructor IMPORTING is_textid    TYPE scx_t100key OPTIONAL
                            is_msg       TYPE zcilib_msg OPTIONAL
                            ix_previous  TYPE REF TO cx_root OPTIONAL
                            iv_operation TYPE csequence OPTIONAL.
    DATA:
      mv_operation TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_cilib_unsupp_operation IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( is_textid = is_textid is_msg = is_msg ix_previous = ix_previous ).

    IF ms_t100_key = gc_default_message.
      ms_t100_key = gc_no_arguments.
    ENDIF.

    mv_operation = iv_operation.
  ENDMETHOD.
ENDCLASS.
