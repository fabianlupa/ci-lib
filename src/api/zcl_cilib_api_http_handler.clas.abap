"! API handler
CLASS zcl_cilib_api_http_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_http_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_api_http_handler IMPLEMENTATION.
  METHOD if_http_extension~handle_request.

  ENDMETHOD.
ENDCLASS.
