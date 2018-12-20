"! Path builder
CLASS zcl_cilib_http_path_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_parameter,
        name  TYPE string,
        value TYPE string,
      END OF gty_parameter,
      gty_parameter_tab TYPE STANDARD TABLE OF gty_parameter WITH EMPTY KEY.
    METHODS:
      constructor IMPORTING iv_path TYPE csequence OPTIONAL,
      set_path IMPORTING iv_path           TYPE csequence
               RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_http_path_builder,
      append_path_component IMPORTING iv_path           TYPE simple
                            RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_http_path_builder,
      append_escaped_path_component IMPORTING iv_path           TYPE simple
                                    RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_http_path_builder,
      add_parameter IMPORTING iv_name           TYPE csequence
                              iv_value          TYPE csequence
                    RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_http_path_builder,
      set_parameters IMPORTING it_parameters     TYPE gty_parameter_tab
                     RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_http_path_builder,
      build RETURNING VALUE(rv_path) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_path       TYPE string,
      mt_parameters TYPE gty_parameter_tab.
ENDCLASS.



CLASS zcl_cilib_http_path_builder IMPLEMENTATION.
  METHOD constructor.
    mv_path = iv_path.
  ENDMETHOD.

  METHOD add_parameter.
    APPEND VALUE #( name = iv_name value = cl_http_utility=>escape_url( iv_value ) ) TO mt_parameters.
    ro_builder = me.
  ENDMETHOD.

  METHOD append_path_component.
    mv_path = mv_path && '/' && condense( iv_path ).
    ro_builder = me.
  ENDMETHOD.

  METHOD set_parameters.
    CLEAR mt_parameters.
    LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      add_parameter( iv_name = <ls_parameter>-name iv_value = <ls_parameter>-value ).
    ENDLOOP.
    ro_builder = me.
  ENDMETHOD.

  METHOD set_path.
    mv_path = iv_path.
    ro_builder = me.
  ENDMETHOD.

  METHOD build.
    rv_path = mv_path.
    LOOP AT mt_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      AT FIRST.
        rv_path = |{ rv_path }?{ <ls_parameter>-name }={ <ls_parameter>-value }|.
        CONTINUE.
      ENDAT.

      rv_path = |{ rv_path }&{ <ls_parameter>-name }={ <ls_parameter>-value }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_escaped_path_component.
    append_path_component( cl_http_utility=>escape_url( iv_path ) ).
    ro_builder = me.
  ENDMETHOD.
ENDCLASS.
