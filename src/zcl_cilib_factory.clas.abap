"! Factory
CLASS zcl_cilib_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_bot RETURNING VALUE(ri_bot) TYPE REF TO zif_cilib_bot,
      get_abapgit_api RETURNING VALUE(ri_abapgit_api) TYPE REF TO zif_cilib_abapgit_api,
      get_host_for_repo_url IMPORTING iv_repo_url    TYPE string
                            RETURNING VALUE(ri_host) TYPE REF TO zif_cilib_host,
      get_cts_api RETURNING VALUE(ri_cts_api) TYPE REF TO zif_cilib_cts_api,
      get_logger RETURNING VALUE(ri_logger) TYPE REF TO zif_cilib_util_logger,
      get_settings RETURNING VALUE(ro_settings) TYPE REF TO zcl_cilib_settings.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_host_cache_line,
        repo_url TYPE string,
        instance TYPE REF TO zif_cilib_host,
      END OF gty_host_cache_line.
    CLASS-DATA:
      gi_abapgit_api          TYPE REF TO zif_cilib_abapgit_api,
      gi_cts_api              TYPE REF TO zif_cilib_cts_api,
      gi_logger               TYPE REF TO zif_cilib_util_logger,
      gi_host_config_provider TYPE REF TO zif_cilib_host_config_provider,
      gt_host_cache           TYPE HASHED TABLE OF gty_host_cache_line WITH UNIQUE KEY repo_url,
      go_settings             TYPE REF TO zcl_cilib_settings.
ENDCLASS.



CLASS zcl_cilib_factory IMPLEMENTATION.
  METHOD get_abapgit_api.
    IF gi_abapgit_api IS NOT BOUND.
      gi_abapgit_api = NEW zcl_cilib_abapgit_api( ).
    ENDIF.
    ri_abapgit_api = gi_abapgit_api.
  ENDMETHOD.

  METHOD get_bot.

  ENDMETHOD.

  METHOD get_host_for_repo_url.
    IF gi_host_config_provider IS NOT BOUND.
      gi_host_config_provider = NEW zcl_cilib_host_config_provider( ).
    ENDIF.

    TRY.
        ri_host = gt_host_cache[ KEY primary_key repo_url = iv_repo_url ]-instance.
      CATCH cx_sy_itab_line_not_found.
        DATA(lo_url) = NEW zcl_cilib_http_url( iv_repo_url ).
        DATA(lo_config) = gi_host_config_provider->get_config_for_host( lo_url->get_host( ) ).
        DATA(lv_classname) = lo_config->get_host_implementation( ).

        INSERT VALUE #( repo_url = iv_repo_url ) INTO TABLE gt_host_cache REFERENCE INTO DATA(lr_new).
        CREATE OBJECT lr_new->instance TYPE (lv_classname)
          EXPORTING
            io_host_config = lo_config.

        ri_host = lr_new->instance.
    ENDTRY.
  ENDMETHOD.

  METHOD get_cts_api.
    IF gi_cts_api IS NOT BOUND.
      gi_cts_api = NEW zcl_cilib_cts_api( ).
    ENDIF.
    ri_cts_api = gi_cts_api.
  ENDMETHOD.

  METHOD get_logger.
    IF gi_logger IS NOT BOUND.
      DATA(lo_settings) = get_settings( ).
      IF lo_settings->is_logging_enabled( ) = abap_true.
        DATA(lv_delete_date) = COND aldate_del( LET d = lo_settings->get_bal_keep_days( ) IN
                                                WHEN d IS INITIAL THEN space
                                                ELSE sy-datum + d ).
        gi_logger = NEW zcl_cilib_util_bal_logger( iv_object      = lo_settings->get_bal_object( )
                                                   iv_subobject   = lo_settings->get_bal_subobject( )
                                                   iv_date_delete = lv_delete_date ).
      ELSE.
        gi_logger = NEW zcl_cilib_util_dummy_logger( ).
      ENDIF.
    ENDIF.

    ri_logger = gi_logger.
  ENDMETHOD.

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
ENDCLASS.
