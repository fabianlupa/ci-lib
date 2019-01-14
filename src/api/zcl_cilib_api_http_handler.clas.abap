"! API handler
CLASS zcl_cilib_api_http_handler DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cl_rest_http_handler
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      if_rest_application~get_root_handler REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_api_http_handler IMPLEMENTATION.
  METHOD if_rest_application~get_root_handler.
    DATA(lo_handler) = NEW cl_rest_router( ).
    lo_handler->attach( iv_template = `/atc/check/{ID:[0-9A-Za-z]*}` iv_handler_class = 'ZCL_CILIB_API_ATC_CHECK' ).
    ro_root_handler = lo_handler.
  ENDMETHOD.
ENDCLASS.
