"! REST resource base class
CLASS zcl_cilib_api_resource_base DEFINITION
  PUBLIC
  ABSTRACT
  INHERITING FROM cl_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
  PROTECTED SECTION.
    TYPES:
      BEGIN OF gty_format,
        request  TYPE string,
        response TYPE string,
      END OF gty_format.
    CONSTANTS:
      gc_uri_format_parameter_name TYPE string VALUE `format`,
      gc_uri_format_value_json     TYPE string VALUE `json`,
      gc_uri_format_value_xml      TYPE string VALUE `xml`,
      gc_format_json               TYPE string VALUE `application/json`,
      gc_format_xml                TYPE string VALUE `application/xml`,
      gc_header_fallback           TYPE string VALUE gc_format_json.
    METHODS:
      determine_format RETURNING VALUE(rs_format) TYPE gty_format
                       RAISING   cx_rest_parser_error,
      get_supported_content_types RETURNING VALUE(rt_content_types) TYPE string_table.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_api_resource_base IMPLEMENTATION.
  METHOD determine_format.
    DATA(lv_format) = mo_request->get_uri_query_parameter( gc_uri_format_parameter_name ).

    IF lv_format = gc_uri_format_value_json.
      rs_format-request = rs_format-response = gc_format_json.
    ELSEIF lv_format = gc_uri_format_value_xml.
      rs_format-request = rs_format-response = gc_format_xml.
    ELSEIF lv_format IS NOT INITIAL.
      RAISE EXCEPTION TYPE cx_rest_parser_error.
    ENDIF.

    CHECK rs_format IS INITIAL.

    DATA(lv_accept) = mo_request->get_header_field( if_http_header_fields=>accept ).
    IF lv_accept IS INITIAL.
      lv_accept = gc_header_fallback.
    ENDIF.

    DATA(lv_response_content_type) = cl_rest_http_utils=>negotiate_content_type(
      iv_header_accept          = lv_accept
      it_supported_content_type = get_supported_content_types( )
    ).
    rs_format-response = lv_response_content_type.

    DATA(lv_request_content_type) = mo_request->get_header_field( if_http_header_fields=>content_type ).
    IF lv_request_content_type IS INITIAL.
      lv_request_content_type = gc_header_fallback.
    ENDIF.

    rs_format-request = lv_request_content_type.
  ENDMETHOD.

  METHOD get_supported_content_types.
    rt_content_types = VALUE #(
      ( gc_format_json )
      ( gc_format_xml )
    ).
  ENDMETHOD.
ENDCLASS.
