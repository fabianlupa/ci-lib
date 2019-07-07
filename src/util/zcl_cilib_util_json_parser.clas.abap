"! JSON parser helper
CLASS zcl_cilib_util_json_parser DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_from_xstring IMPORTING iv_json          TYPE xsequence
                          RETURNING VALUE(ro_result) TYPE REF TO zcl_cilib_util_json_element,
      create_from_string IMPORTING iv_json          TYPE csequence
                         RETURNING VALUE(ro_result) TYPE REF TO zcl_cilib_util_json_element.
    METHODS:
      get_object_by_ref IMPORTING ir_ref           TYPE REF TO data
                        RETURNING VALUE(ro_object) TYPE REF TO zcl_cilib_util_json_object
                        RAISING   zcx_cilib_not_found,
      get_array_by_ref IMPORTING ir_ref           TYPE REF TO data
                       RETURNING VALUE(ro_object) TYPE REF TO zcl_cilib_util_json_array
                       RAISING   zcx_cilib_not_found.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_element,
        ref     TYPE REF TO data,
        element TYPE REF TO zcl_cilib_util_json_element,
      END OF gty_element.
    METHODS:
      constructor IMPORTING ir_result TYPE REF TO data,
      parse IMPORTING ir_from           TYPE REF TO data OPTIONAL
                      io_parent         TYPE REF TO zcl_cilib_util_json_element OPTIONAL
            RETURNING VALUE(ro_element) TYPE REF TO zcl_cilib_util_json_element,
      get_root_element RETURNING VALUE(ro_root) TYPE REF TO zcl_cilib_util_json_element.
    DATA:
      mr_result   TYPE REF TO data,
      mo_root     TYPE REF TO zcl_cilib_util_json_element,
      mt_elements TYPE HASHED TABLE OF gty_element WITH UNIQUE KEY ref.
ENDCLASS.



CLASS zcl_cilib_util_json_parser IMPLEMENTATION.
  METHOD create_from_string.
    DATA(lo_parser) = NEW zcl_cilib_util_json_parser(
      /ui2/cl_json=>generate( json = iv_json pretty_name = /ui2/cl_json=>pretty_mode-none )
    ).
    lo_parser->parse( ).
    ro_result = lo_parser->get_root_element( ).
  ENDMETHOD.

  METHOD create_from_xstring.
    ro_result = create_from_string( cl_abap_codepage=>convert_from( iv_json ) ).
  ENDMETHOD.

  METHOD constructor.
    mr_result = ir_result.
  ENDMETHOD.

  METHOD get_root_element.
    ro_root = mo_root.
  ENDMETHOD.

  METHOD parse.
    DATA: lr_new   TYPE REF TO gty_element,
          lo_child TYPE REF TO zcl_cilib_util_json_element.
    FIELD-SYMBOLS: <lg_struct> TYPE data,
                   <lg_comp>   TYPE data,
                   <lt_tab>    TYPE INDEX TABLE,
                   <lg_line>   TYPE data.

    DATA(lr_source) = COND #( WHEN io_parent IS NOT BOUND THEN mr_result ELSE ir_from ).

    IF lr_source IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lv_kind) = cl_abap_typedescr=>describe_by_data_ref( lr_source )->kind.

    CASE lv_kind.
      WHEN cl_abap_typedescr=>kind_struct.
        INSERT VALUE #(
          ref     = lr_source
          element =  NEW zcl_cilib_util_json_object( ir_element = lr_source io_parser = me io_parent = io_parent )
        ) INTO TABLE mt_elements REFERENCE INTO lr_new.
        ASSERT sy-subrc = 0.

        IF mo_root IS NOT BOUND.
          mo_root = lr_new->element.
        ENDIF.

        ASSIGN lr_source->* TO <lg_struct>.
        ASSERT sy-subrc = 0.

        ro_element = lr_new->element.

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <lg_struct> TO <lg_comp>.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          lo_child = parse( ir_from = CAST #( <lg_comp> ) io_parent = lr_new->element ).
          IF lo_child IS BOUND.
            APPEND lo_child TO ro_element->mt_children.
          ENDIF.
        ENDDO.

      WHEN cl_abap_typedescr=>kind_table.
        INSERT VALUE #(
          ref     = lr_source
          element =  NEW zcl_cilib_util_json_array( ir_element = lr_source io_parser = me io_parent = io_parent )
        ) INTO TABLE mt_elements REFERENCE INTO lr_new.
        ASSERT sy-subrc = 0.

        IF mo_root IS NOT BOUND.
          mo_root = lr_new->element.
        ENDIF.

        ASSIGN lr_source->* TO <lt_tab>.
        ASSERT sy-subrc = 0.

        ro_element = lr_new->element.

        LOOP AT <lt_tab> ASSIGNING <lg_line>.
          lo_child = parse( ir_from = CAST #( <lg_line> ) io_parent = lr_new->element ).
          IF lo_child IS BOUND.
            APPEND lo_child TO ro_element->mt_children.
          ENDIF.
        ENDLOOP.
      WHEN cl_abap_typedescr=>kind_elem.
        INSERT VALUE #(
          ref     = lr_source
          element = NEW zcl_cilib_util_json_value( ir_element = lr_source io_parser = me io_parent = io_parent )
        ) INTO TABLE mt_elements REFERENCE INTO lr_new.
        ASSERT sy-subrc = 0.

        IF mo_root IS NOT BOUND.
          mo_root = lr_new->element.
        ENDIF.

        ro_element = lr_new->element.
      WHEN OTHERS.
        ASSERT 1 = 2.
    ENDCASE.
  ENDMETHOD.

  METHOD get_array_by_ref.
    TRY.
        ro_object = CAST #( mt_elements[ KEY primary_key ref = ir_ref ]-element ).
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_cilib_not_found
          EXPORTING
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.

  METHOD get_object_by_ref.
    TRY.
        ro_object = CAST #( mt_elements[ KEY primary_key ref = ir_ref ]-element ).
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_cilib_not_found
          EXPORTING
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
