"! ATC check
CLASS zcl_cilib_api_atc_check DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cl_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_check,
        id      TYPE i,
        package TYPE devclass,
      END OF gty_check.
    METHODS:
      if_rest_resource~get REDEFINITION,
      if_rest_resource~post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_api_atc_check IMPLEMENTATION.
  METHOD if_rest_resource~get.
    TRY.
        DATA(lv_check_id) = EXACT satc_d_id( mo_request->get_uri_attribute( 'ID' ) ).
        DATA(li_query) = cl_satc_adt_result_query=>create_for_result( lv_check_id ).
        DATA(li_reader) = li_query->get_reader( ).
        DATA(lt_findings) = li_reader->read_findings( lv_check_id ).

        IF lt_findings IS INITIAL.
          mo_response->set_status( cl_rest_status_code=>gc_client_error_not_found ).
          RETURN.
        ENDIF.

        DATA(lv_json) = /ui2/cl_json=>serialize(
          data        = lt_findings
          pretty_name = /ui2/cl_json=>pretty_mode-low_case
        ).

        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).
        DATA(lo_entity) = mo_response->create_entity( ).
        lo_entity->set_string_data( lv_json ).
      CATCH cx_sy_move_cast_error.
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    super->if_rest_resource~post( io_entity ).
*    DATA: ls_check TYPE gty_check.
*
*    DATA(li_entity) = mo_request->get_entity( ).
*    DATA(lv_request) = li_entity->get_binary_data( ).
*
*    /ui2/cl_json=>deserialize(
*      EXPORTING
*        jsonx       = lv_request
*        pretty_name = /ui2/cl_json=>pretty_mode-none
*      CHANGING
*        data        = ls_check
*    ).
**    CALL TRANSFORMATION id
**         SOURCE XML lv_request
**         RESULT check = ls_check.
*
*    IF ls_check IS INITIAL.
*      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
