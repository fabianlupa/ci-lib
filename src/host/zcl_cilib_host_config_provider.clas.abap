"! Host configuration provider
CLASS zcl_cilib_host_config_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_cust_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_host_config_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_cache_line,
        host     TYPE string,
        instance TYPE REF TO zcl_cilib_host_config,
      END OF gty_cache_line.
    DATA:
      mt_cache TYPE HASHED TABLE OF gty_cache_line WITH UNIQUE KEY host.
ENDCLASS.



CLASS zcl_cilib_host_config_provider IMPLEMENTATION.
  METHOD zif_cilib_host_config_provider~get_config_for_host.
    DATA(lv_host) = to_upper( iv_host ).
    TRY.
        ro_config = mt_cache[ KEY primary_key host = lv_host ]-instance.
      CATCH cx_sy_itab_line_not_found.
        SELECT SINGLE * INTO @DATA(ls_config)
          FROM zcilib_host
          WHERE host = @lv_host.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cilib_not_found.
        ELSE.
          SELECT * INTO TABLE @DATA(lt_repo_configs)
            FROM zcilib_repo
            WHERE host = @lv_host.
          INSERT VALUE #(
            host     = ls_config-host
            instance = NEW #( iv_host  = ls_config-host
                              is_data  = ls_config-data
                              it_repos = VALUE #( FOR r IN lt_repo_configs (
                                                    repo = r-repo instance = NEW #( iv_repo = r-repo
                                                                                    is_data = r-data )
                                                  ) ) )
          ) INTO TABLE mt_cache REFERENCE INTO DATA(lr_new).
          ro_config = lr_new->instance.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
