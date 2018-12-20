"! Host configuration
CLASS zcl_cilib_host_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_data TYPE zcilib_host_cfg_data,
      get_host RETURNING VALUE(rv_host) TYPE string,
      get_rfc_destination RETURNING VALUE(rv_destination) TYPE rfcdest,
      get_api_token RETURNING VALUE(rv_token) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ms_data TYPE zcilib_host_cfg_data.
ENDCLASS.



CLASS zcl_cilib_host_config IMPLEMENTATION.
  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.

  METHOD get_host.

  ENDMETHOD.

  METHOD get_rfc_destination.
    rv_destination = ms_data-destination.
  ENDMETHOD.

  METHOD get_api_token.
    rv_token = ms_data-api_token.
  ENDMETHOD.
ENDCLASS.
