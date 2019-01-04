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
                      RAISING   zcx_cilib_not_found,
      get_bot_config IMPORTING iv_bot_name      TYPE zcilib_bot_name
                     RETURNING VALUE(ro_config) TYPE REF TO zcl_cilib_bot_config
                     RAISING   zcx_cilib_not_found,
      get_system_group IMPORTING iv_system_group        TYPE zcilib_cust_sysgrp
                       RETURNING VALUE(ro_system_group) TYPE REF TO zcl_cilib_cust_system_group.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_bot_cfg_cache_line,
        bot_name TYPE zcilib_bot_name,
        instance TYPE REF TO zcl_cilib_bot_config,
      END OF gty_bot_cfg_cache_line,
      BEGIN OF gty_system_group_cache_line,
        system_group TYPE zcilib_cust_sysgrp,
        instance     TYPE REF TO zcl_cilib_cust_system_group,
      END OF gty_system_group_cache_line.
    CLASS-DATA:
      gi_host_config_provider TYPE REF TO zif_cilib_host_config_provider,
      go_settings             TYPE REF TO zcl_cilib_settings,
      gt_bot_cfg_cache        TYPE HASHED TABLE OF gty_bot_cfg_cache_line WITH UNIQUE KEY bot_name,
      gt_system_group_cache   TYPE HASHED TABLE OF gty_system_group_cache_line WITH UNIQUE KEY system_group.
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

  METHOD get_bot_config.
    TRY.
        ro_config = gt_bot_cfg_cache[ KEY primary_key bot_name = iv_bot_name ]-instance.

      CATCH cx_sy_itab_line_not_found.
        SELECT SINGLE * INTO @DATA(ls_config)
          FROM zcilib_bot
          WHERE bot = @iv_bot_name.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cilib_not_found
            EXPORTING
              is_textid    = zcx_cilib_not_found=>gc_with_name_and_key
              iv_type_name = 'Bot config'
              iv_key       = iv_bot_name.
        ENDIF.

        INSERT VALUE #(
          bot_name = ls_config-bot
          instance = NEW #( iv_bot_name = ls_config-bot
                            is_data     = ls_config-data )
        ) INTO TABLE gt_bot_cfg_cache REFERENCE INTO DATA(lr_new).

        ro_config = lr_new->instance.
    ENDTRY.
  ENDMETHOD.

  METHOD get_system_group.
    TRY.
        ro_system_group = gt_system_group_cache[ KEY primary_key system_group = iv_system_group ]-instance.

      CATCH cx_sy_itab_line_not_found.
        SELECT SINGLE * INTO @DATA(ls_group)
          FROM zcilib_sysgrp
          WHERE system_group = @iv_system_group.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cilib_not_found
            EXPORTING
              is_textid    = zcx_cilib_not_found=>gc_with_name_and_key
              iv_type_name = 'System group'
              iv_key       = iv_system_group.
        ENDIF.

        SELECT * INTO TABLE @DATA(lt_systems)
          FROM zcilib_system
          WHERE system_group = @ls_group-system_group
          ORDER BY sys_order ASCENDING.

        INSERT VALUE #(
          system_group = ls_group-system_group
          instance = NEW #( iv_system_group = iv_system_group
                            it_systems      = CORRESPONDING #( lt_systems MAPPING order = sys_order ) )
        ) INTO TABLE gt_system_group_cache REFERENCE INTO DATA(lr_new).

        ro_system_group = lr_new->instance.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
