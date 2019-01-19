"! ATC check run
CLASS zcl_cilib_api_atc_run DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_cilib_api_resource_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_finding,
        index             TYPE syst_tabix,
        object_type       TYPE trobj_name,
        object_name       TYPE sobj_name,
*        module_id         TYPE satc_d_ac_module_id,
*        module_msg_key    TYPE satc_d_ac_module_message_key,
*        checksum          TYPE satc_d_crc32,
        priority          TYPE satc_d_message_priority,
        package           TYPE satc_d_ac_devclass,
        timestamp         TYPE satc_d_created_on,
        error_object_type TYPE trobjtype,
        error_object_name TYPE satc_d_ac_r3tr_obj_name,
      END OF gty_finding,
      BEGIN OF gty_check_run,
        id             TYPE satc_d_ac_display_id,
        title          TYPE satc_d_ac_title,
        check_profile  TYPE satc_d_ac_chk_profile_name,
        count_prio1    TYPE satc_d_count_prio1,
        count_prio2    TYPE satc_d_count_prio2,
        count_prio3    TYPE satc_d_count_prio3,
        count_prio4    TYPE satc_d_count_prio4,
        count_failures TYPE satc_d_ac_count_plnerr,
        finished       TYPE satc_d_ac_rslt_is_complete,
        timestamp      TYPE satc_d_created_on,
        findings       TYPE STANDARD TABLE OF gty_finding WITH KEY index,
      END OF gty_check_run.
    METHODS:
      if_rest_resource~get REDEFINITION,
      if_rest_resource~post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      read_atc_check_run IMPORTING iv_id               TYPE satc_d_ac_display_id
                         RETURNING VALUE(rs_check_run) TYPE gty_check_run
                         RAISING   zcx_cilib_not_found
                                   zcx_cilib_illegal_argument.
ENDCLASS.



CLASS zcl_cilib_api_atc_run IMPLEMENTATION.
  METHOD if_rest_resource~get.
    TRY.
        DATA(ls_format) = determine_format( ).
        DATA(lv_check_id) = EXACT satc_d_id( mo_request->get_uri_attribute( 'ID' ) ).
        DATA(ls_result) = read_atc_check_run( lv_check_id ).

        DATA(lo_entity) = mo_response->create_entity( ).

        IF ls_format-response = gc_format_json.
          DATA(lv_json) = /ui2/cl_json=>serialize(
            data        = ls_result
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          ).
          lo_entity->set_string_data( lv_json ).
        ELSEIF ls_format-response = gc_format_xml.
          CALL TRANSFORMATION id
               SOURCE check = ls_result
               RESULT XML DATA(lv_xml).
          lo_entity->set_binary_data( lv_xml ).
        ENDIF.

        mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = ls_format-response ).
        mo_response->set_status( cl_rest_status_code=>gc_success_ok ).

      CATCH zcx_cilib_illegal_argument cx_sy_move_cast_error cx_rest_parser_error.
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      CATCH zcx_cilib_not_found.
        mo_response->set_status( cl_rest_status_code=>gc_client_error_not_found ).
    ENDTRY.
  ENDMETHOD.

  METHOD if_rest_resource~post.
    DATA: lv_exec_id TYPE satc_d_exec_id,
          lv_success TYPE abap_bool.

    TRY.
        DATA(ls_format) = determine_format( ).
        DATA(li_builder) = cl_satc_ac_project_builder=>create_builder( ).
        DATA(lo_parameters) = NEW cl_satc_ac_project_parameters( ).
*    lo_parameters->
        DATA(li_project) = li_builder->create_project( lo_parameters ).

        CALL FUNCTION 'SATC_EXECUTE_PROJECT'
          EXPORTING
            i_project = li_project
          IMPORTING
            e_exec_id = lv_exec_id
            e_success = lv_success.

        DATA(li_reader) = cl_satc_adt_result_reader=>create( ).
        DATA(li_access) = cl_satc_adt_result_read_access=>create( li_reader ).
        li_access->read_display_id_4_execution_id(
          EXPORTING i_execution_id = lv_exec_id
          IMPORTING e_display_id   = DATA(lv_display_id)
        ).

        DATA(ls_result) = read_atc_check_run( lv_display_id ).

        DATA(lo_entity) = mo_response->create_entity( ).

        IF ls_format-response = gc_format_json.
          DATA(lv_json) = /ui2/cl_json=>serialize(
            data        = ls_result
            pretty_name = /ui2/cl_json=>pretty_mode-low_case
          ).
          lo_entity->set_string_data( lv_json ).
        ELSEIF ls_format-response = gc_format_xml.
          CALL TRANSFORMATION id
               SOURCE check = ls_result
               RESULT XML DATA(lv_xml).
          lo_entity->set_binary_data( lv_xml ).
        ENDIF.

        mo_response->set_header_field( iv_name = if_http_header_fields=>content_type iv_value = ls_format-response ).
        mo_response->set_status( cl_rest_status_code=>gc_success_created ).

*      CATCH cx_satc_root INTO DATA(lx_ex).

      CATCH cx_rest_parser_error.
        mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
    ENDTRY.
  ENDMETHOD.

  METHOD read_atc_check_run.
    SELECT SINGLE * INTO @DATA(ls_header)
      FROM satc_ac_resulth
      WHERE display_id = @iv_id.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.

    rs_check_run = VALUE gty_check_run(
      id             = ls_header-display_id
      title          = ls_header-title
      check_profile  = ls_header-chk_profile_name
      count_prio1    = ls_header-count_prio1
      count_prio2    = ls_header-count_prio2
      count_prio3    = ls_header-count_prio3
      count_prio4    = ls_header-count_prio4
      count_failures = ls_header-count_plnerr
      finished       = ls_header-is_complete
      timestamp      = ls_header-scheduled_on_ts
    ).

    IF rs_check_run-finished = abap_true.
      DATA(li_query) = cl_satc_adt_result_query=>create_for_result( iv_id ).
      DATA(li_reader) = li_query->get_reader( ).
      DATA(lt_findings) = li_reader->read_findings( iv_id ).
      LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).
        INSERT VALUE gty_finding(
          index             = sy-tabix
          object_type       = <ls_finding>-obj_type
          object_name       = <ls_finding>-obj_name
*          module_id         = <ls_finding>-module_id
*          module_msg_key    = <ls_finding>-module_msg_key
*          checksum          = <ls_finding>-checksum
          priority          = <ls_finding>-priority
          package           = <ls_finding>-package_name
          timestamp         = <ls_finding>-timestamp
          error_object_type = <ls_finding>-error_obj_type
          error_object_name = <ls_finding>-error_obj_name
        ) INTO TABLE rs_check_run-findings.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
