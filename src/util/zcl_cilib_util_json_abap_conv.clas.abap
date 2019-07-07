"! JSON ABAP type converter
CLASS zcl_cilib_util_json_abap_conv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      to_boolean IMPORTING ir_data           TYPE REF TO data
                 RETURNING VALUE(rv_boolean) TYPE abap_bool
                 RAISING   zcx_cilib_unsupp_operation,
      to_string IMPORTING ir_data                     TYPE REF TO data
                          iv_replace_unicode_entities TYPE abap_bool DEFAULT abap_false
                RETURNING VALUE(rv_string)            TYPE string
                RAISING   zcx_cilib_unsupp_operation,
      to_int IMPORTING ir_data       TYPE REF TO data
             RETURNING VALUE(rv_int) TYPE i
             RAISING   zcx_cilib_unsupp_operation.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_json_abap_conv IMPLEMENTATION.
  METHOD to_boolean.
    FIELD-SYMBOLS: <lv_boolean> TYPE csequence.

    ASSIGN ir_data->* TO <lv_boolean>.
    ASSERT sy-subrc = 0.

    rv_boolean = SWITCH #( to_upper( <lv_boolean> )
      WHEN 'TRUE' OR abap_true   THEN abap_true
      WHEN 'FALSE' OR abap_false THEN abap_false
      ELSE abap_undefined
    ).
    IF rv_boolean = abap_undefined.
      RAISE EXCEPTION TYPE zcx_cilib_unsupp_operation.
    ENDIF.
  ENDMETHOD.

  METHOD to_string.
    DATA: lv_hex TYPE x LENGTH 2.
    FIELD-SYMBOLS: <lv_string> TYPE csequence.

    ASSIGN ir_data->* TO <lv_string>.
    ASSERT sy-subrc = 0.

    rv_string = <lv_string>.

    IF iv_replace_unicode_entities = abap_true.
      " https://stackoverflow.com/questions/52207088/cl-http-utility-not-normalizing-my-url-why
      FIND ALL OCCURRENCES OF REGEX '\\u....' IN rv_string RESULTS DATA(lt_matches).
      SORT lt_matches BY offset DESCENDING.
      LOOP AT lt_matches ASSIGNING FIELD-SYMBOL(<ls_match>).
        lv_hex = to_upper( substring( val = rv_string+<ls_match>-offset(<ls_match>-length) off = 2 ) ).
        DATA(lv_uchar) = cl_abap_conv_in_ce=>uccp( lv_hex ).
        REPLACE SECTION OFFSET <ls_match>-offset LENGTH <ls_match>-length OF rv_string WITH lv_uchar.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD to_int.
    FIELD-SYMBOLS: <lv_int> TYPE i.

    ASSIGN ir_data->* TO <lv_int>.
    ASSERT sy-subrc = 0.

    rv_int = <lv_int>.
  ENDMETHOD.
ENDCLASS.
