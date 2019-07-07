"! JSON elementary value
CLASS zcl_cilib_util_json_value DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_cilib_util_json_element
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_util_json_parser.

  PUBLIC SECTION.
    METHODS:
      as_string RETURNING VALUE(rv_string) TYPE string,
      as_int RETURNING VALUE(rv_int) TYPE i,
      as_boolean RETURNING VALUE(rv_boolean) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_json_value IMPLEMENTATION.
  METHOD as_boolean.
    rv_boolean = zcl_cilib_util_json_abap_conv=>to_boolean( mr_element ).
  ENDMETHOD.

  METHOD as_int.
    rv_int = zcl_cilib_util_json_abap_conv=>to_int( mr_element ).
  ENDMETHOD.

  METHOD as_string.
    rv_string = zcl_cilib_util_json_abap_conv=>to_string( mr_element ).
  ENDMETHOD.
ENDCLASS.
