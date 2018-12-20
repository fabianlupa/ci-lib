"! Host configuration provider
INTERFACE zif_cilib_host_config_provider PUBLIC.
  METHODS:
    get_config_for_host IMPORTING iv_host          TYPE string
                        RETURNING VALUE(ro_config) TYPE REF TO zcl_cilib_host_config
                        RAISING   zcx_cilib_not_found.
ENDINTERFACE.
