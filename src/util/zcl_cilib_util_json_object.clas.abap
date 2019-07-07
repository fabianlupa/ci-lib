"! JSON object
CLASS zcl_cilib_util_json_object DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_cilib_util_json_element
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_util_json_parser.

  PUBLIC SECTION.
    METHODS:
      get_string IMPORTING iv_name                     TYPE csequence
                           iv_replace_unicode_entities TYPE abap_bool DEFAULT abap_false
                 RETURNING VALUE(rv_string)            TYPE string
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
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    ro_array = mo_parser->get_array_by_ref( <lr_ref> ).
  ENDMETHOD.

  METHOD get_boolean.
    FIELD-SYMBOLS: <lr_ref> TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    rv_boolean = zcl_cilib_util_json_abap_conv=>to_boolean( <lr_ref> ).
  ENDMETHOD.

  METHOD get_object.
    FIELD-SYMBOLS: <lr_ref> TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    ro_object = mo_parser->get_object_by_ref( <lr_ref> ).
  ENDMETHOD.

  METHOD get_string.
    FIELD-SYMBOLS: <lr_ref> TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    rv_string = zcl_cilib_util_json_abap_conv=>to_string( <lr_ref> ).
  ENDMETHOD.

  METHOD get_int.
    FIELD-SYMBOLS: <lr_ref> TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    rv_int = zcl_cilib_util_json_abap_conv=>to_int( <lr_ref> ).
  ENDMETHOD.
ENDCLASS.
