"! Communication error
CLASS zcx_cilib_http_comm_error DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcx_cilib_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_no_arguments,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_no_arguments,
      BEGIN OF gc_code_and_message,
        msgid TYPE symsgid VALUE 'ZCILIB',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_RESPONSE_CODE',
        attr2 TYPE scx_attrname VALUE 'MV_ERROR_MESSAGE',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_code_and_message.
    METHODS:
      constructor IMPORTING is_textid        TYPE scx_t100key OPTIONAL
                            is_msg           TYPE zcilib_msg OPTIONAL
                            ix_previous      TYPE REF TO cx_root OPTIONAL
                            iv_response_code TYPE i OPTIONAL
                            iv_error_message TYPE csequence OPTIONAL.
    DATA:
      mv_response_code TYPE i READ-ONLY,
      mv_error_message TYPE string READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_cilib_http_comm_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( is_textid = is_textid is_msg = is_msg ix_previous = ix_previous ).

    IF ms_t100_key = gc_default_message.
      ms_t100_key = gc_no_arguments.
    ENDIF.

    mv_response_code = iv_response_code.
    mv_error_message = iv_error_message.
  ENDMETHOD.
ENDCLASS.
