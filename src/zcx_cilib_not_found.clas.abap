"! Not found exception
CLASS zcx_cilib_not_found DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcx_cilib_dynamic_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_with_name_and_key,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_TYPE_NAME',
        attr2 TYPE scx_attrname VALUE 'MV_KEY',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_name_and_key,
      BEGIN OF gc_with_name,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MV_TYPE_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_with_name.
    METHODS:
      constructor IMPORTING is_textid    TYPE scx_t100key OPTIONAL
                            is_msg       TYPE zcilib_msg OPTIONAL
                            ix_previous  TYPE REF TO cx_root OPTIONAL
                            iv_type_name TYPE csequence OPTIONAL
                            iv_key       TYPE csequence OPTIONAL
                            it_keys      TYPE stringtab OPTIONAL.
    DATA:
      mv_type_name TYPE string READ-ONLY,
      mv_key       TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_cilib_not_found IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( is_textid = is_textid is_msg = is_msg ix_previous = ix_previous ).

    IF ms_t100_key = gc_default_message.
      ms_t100_key = gc_no_arguments.
    ENDIF.

    mv_type_name = iv_type_name.
    mv_key = iv_key.

    IF it_keys IS NOT INITIAL.
      CONCATENATE LINES OF it_keys INTO mv_key SEPARATED BY '-'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
