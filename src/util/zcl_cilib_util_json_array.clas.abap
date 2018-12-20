"! JSON array
CLASS zcl_cilib_util_json_array DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_cilib_util_json_element
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_util_json_parser.

  PUBLIC SECTION.
    METHODS:
      get_count RETURNING VALUE(rv_count) TYPE i,
      get_element_at IMPORTING iv_index          TYPE i
                     RETURNING VALUE(ro_element) TYPE REF TO zcl_cilib_util_json_element
                     RAISING   zcx_cilib_not_found.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_json_array IMPLEMENTATION.
  METHOD get_count.
    rv_count = lines( mt_children ).
  ENDMETHOD.

  METHOD get_element_at.
    TRY.
        ro_element = mt_children[ iv_index ].
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_cilib_not_found
          EXPORTING
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
