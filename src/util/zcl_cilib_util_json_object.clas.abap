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
    FIELD-SYMBOLS: <lv_boolean> TYPE csequence,
                   <lr_ref>     TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

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
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    ro_object = mo_parser->get_object_by_ref( <lr_ref> ).
  ENDMETHOD.

  METHOD get_string.
    DATA: lv_hex TYPE x LENGTH 2.
    FIELD-SYMBOLS: <lv_string> TYPE csequence,
                   <lr_ref>    TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    ASSIGN <lr_ref>->* TO <lv_string>.
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

  METHOD get_int.
    FIELD-SYMBOLS: <lv_int> TYPE i,
                   <lr_ref> TYPE REF TO data.

    ASSIGN mr_element->* TO FIELD-SYMBOL(<lg_data>).
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT iv_name OF STRUCTURE <lg_data> TO <lr_ref>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    ASSIGN <lr_ref>->* TO <lv_int>.
    ASSERT sy-subrc = 0.

    rv_int = <lv_int>.
  ENDMETHOD.
ENDCLASS.
