"! Customizing factory
CLASS zcl_cilib_cust_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_settings RETURNING VALUE(ro_settings) TYPE REF TO zcl_cilib_settings,
      get_host_config IMPORTING iv_host          TYPE zcilib_host_hostpath
                      RETURNING VALUE(ro_config) TYPE REF TO zcl_cilib_host_config
                      RAISING   zcx_cilib_not_found.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_host_config_provider TYPE REF TO zif_cilib_host_config_provider,
      go_settings             TYPE REF TO zcl_cilib_settings.
ENDCLASS.



CLASS zcl_cilib_cust_factory IMPLEMENTATION.
  METHOD get_settings.
    IF go_settings IS NOT BOUND.
      ##TODO. " Abstract away database access
      SELECT SINGLE * INTO @DATA(ls_settings)
        FROM zcilib_settings
        WHERE version = '000'.
      IF sy-subrc <> 0.
        ls_settings = VALUE #( version = '000' ).
        INSERT zcilib_settings FROM @ls_settings.
        ASSERT sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
      go_settings = NEW #( ls_settings-data ).
    ENDIF.

    ro_settings = go_settings.
  ENDMETHOD.

  METHOD get_host_config.
    IF gi_host_config_provider IS NOT BOUND.
      gi_host_config_provider = NEW zcl_cilib_host_config_provider( ).
    ENDIF.

    ro_config = gi_host_config_provider->get_config_for_host( iv_host ).
  ENDMETHOD.
ENDCLASS.
