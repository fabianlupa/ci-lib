"! JSON object
CLASS zcl_cilib_util_json_object DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_cilib_util_json_element
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_util_json_parser.

  PUBLIC SECTION.
    METHODS:
      get_string IMPORTING iv_name          TYPE csequence
                 RETURNING VALUE(rv_string) TYPE string
                 RAISING   zcx_cilib_not_found,
      get_int IMPORTING iv_name       TYPE csequence
              RETURNING VALUE(rv_int) TYPE i
              RAISING   zcx_cilib_not_found,
      get_boolean IMPORTING iv_name           TYPE csequence
                  RETURNING VALUE(rv_boolean) TYPE abap_bool
                  RAISING   zcx_cilib_not_found,
      get_object IMPORTING iv_name          TYPE csequence
                 RETURNING VALUE(ro_object) TYPE REF TO zcl_cilib_util_json_object
                 RAISING   zcx_cilib_not_found,
      get_array IMPORTING iv_name         TYPE csequence
                RETURNING VALUE(ro_array) TYPE REF TO zcl_cilib_util_json_array
                RAISING   zcx_cilib_not_found.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_json_object IMPLEMENTATION.
  METHOD get_array.
    FIELD-SYMBOLS: <lr_ref> TYPE REF TO data.
    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    ASSERT sy-subrc = 0.
    ro_array = mo_parser->get_array_by_ref( <lr_ref> ).
  ENDMETHOD.

  METHOD get_boolean.
    FIELD-SYMBOLS: <lv_boolean> TYPE csequence,
                   <lr_ref>     TYPE REF TO data.
    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    ASSERT sy-subrc = 0.
    ASSIGN <lr_ref>->* TO <lv_boolean>.
    ASSERT sy-subrc = 0.

    rv_boolean = SWITCH #( to_upper( <lv_boolean> )
      WHEN 'TRUE' OR abap_true   THEN abap_true
      WHEN 'FALSE' OR abap_false THEN abap_false
      ELSE abap_undefined
    ).
    IF rv_boolean = abap_undefined.
*      RAISE EXCEPTION TYPE...
      ASSERT 1 = 2 ##TODO.
    ENDIF.
  ENDMETHOD.

  METHOD get_object.
    FIELD-SYMBOLS: <lr_ref> TYPE REF TO data.
    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    ASSERT sy-subrc = 0.
    ro_object = mo_parser->get_object_by_ref( <lr_ref> ).
  ENDMETHOD.

  METHOD get_string.
    FIELD-SYMBOLS: <lv_string> TYPE csequence,
                   <lr_ref>    TYPE REF TO data.
    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    ASSERT sy-subrc = 0.
    ASSIGN <lr_ref>->* TO <lv_string>.
    ASSERT sy-subrc = 0.

    rv_string = <lv_string>.
  ENDMETHOD.

  METHOD get_int.
    FIELD-SYMBOLS: <lv_int> TYPE i,
                   <lr_ref> TYPE REF TO data.
    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    ASSERT sy-subrc = 0.
    ASSIGN <lr_ref>->* TO <lv_int>.
    ASSERT sy-subrc = 0.

    rv_int = <lv_int>.
  ENDMETHOD.
ENDCLASS.
