"! Struct builder
CLASS zcl_cilib_util_struct_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      add_string IMPORTING iv_key            TYPE string
                           iv_value          TYPE csequence
                 RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_util_struct_builder
                 RAISING   cx_sy_itab_duplicate_key,
      add_int IMPORTING iv_key            TYPE string
                        iv_value          TYPE i
              RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_util_struct_builder
              RAISING   cx_sy_itab_duplicate_key,
      add_boolean IMPORTING iv_key            TYPE string
                            iv_value          TYPE abap_bool
                  RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_util_struct_builder
                  RAISING   cx_sy_itab_duplicate_key,
      add_any IMPORTING iv_key            TYPE string
                        ig_value          TYPE data
              RETURNING VALUE(ro_builder) TYPE REF TO zcl_cilib_util_struct_builder
              RAISING   cx_sy_itab_duplicate_key
                        zcx_cilib_unsupp_operation,
      get_struct RETURNING VALUE(rr_struct) TYPE REF TO data,
      get_json RETURNING VALUE(rv_json) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_line,
        key   TYPE string,
        value TYPE REF TO data,
        type  TYPE REF TO cl_abap_datadescr,
      END OF gty_line.
    DATA:
      mt_map TYPE HASHED TABLE OF gty_line WITH UNIQUE KEY key.
ENDCLASS.



CLASS zcl_cilib_util_struct_builder IMPLEMENTATION.
  METHOD add_boolean.
    ro_builder = add_any( iv_key = iv_key ig_value = iv_value ).
  ENDMETHOD.

  METHOD add_int.
    ro_builder = add_any( iv_key = iv_key ig_value = iv_value ).
  ENDMETHOD.

  METHOD add_string.
    ro_builder = add_any( iv_key = iv_key ig_value = iv_value ).
  ENDMETHOD.

  METHOD get_struct.
    DATA(lo_descr) = cl_abap_structdescr=>get( VALUE #( FOR l IN mt_map
      ( name = l-key type = l-type )
    ) ).
    CREATE DATA rr_struct TYPE HANDLE lo_descr.
    ASSIGN rr_struct->* TO FIELD-SYMBOL(<lg_struct>).

    LOOP AT mt_map ASSIGNING FIELD-SYMBOL(<ls_entry>).
      ASSIGN COMPONENT to_upper( <ls_entry>-key ) OF STRUCTURE <lg_struct> TO FIELD-SYMBOL(<lg_value_struct>).
      ASSIGN <ls_entry>-value->* TO FIELD-SYMBOL(<lg_value>).
      <lg_value_struct> = <lg_value>.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_any.
    DATA: lr_copy TYPE REF TO data.

    ro_builder = me.

    TRY.
        DATA(lo_descr) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( ig_value ) ).
        CREATE DATA lr_copy TYPE HANDLE lo_descr.
        ASSIGN lr_copy->* TO FIELD-SYMBOL(<lg_copy>).
        <lg_copy> = ig_value.

        INSERT VALUE #(
          key = iv_key
          value = lr_copy
          type = lo_descr
        ) INTO TABLE mt_map.

      CATCH cx_sy_move_cast_error INTO DATA(lx_ex).
        RAISE EXCEPTION TYPE zcx_cilib_unsupp_operation
          EXPORTING
            is_msg      = zcl_cilib_util_msg_tools=>get_msg_from_exc( lx_ex )
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.

  METHOD get_json.
    rv_json = /ui2/cl_json=>serialize(
      data             = get_struct( )
      pretty_name      = /ui2/cl_json=>pretty_mode-low_case
    ).
  ENDMETHOD.
ENDCLASS.
