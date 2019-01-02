"! Factory
CLASS zcl_cilib_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_bot IMPORTING iv_bot_name   TYPE zcilib_bot_name
              RETURNING VALUE(ri_bot) TYPE REF TO zif_cilib_bot
              RAISING   zcx_cilib_illegal_argument,
      get_abapgit_api RETURNING VALUE(ri_abapgit_api) TYPE REF TO zif_cilib_abapgit_api,
      get_host IMPORTING iv_host        TYPE zcilib_host_hostpath
               RETURNING VALUE(ri_host) TYPE REF TO zif_cilib_host
               RAISING   zcx_cilib_not_found,
      get_host_for_repo_url IMPORTING iv_repo_url    TYPE string
                            RETURNING VALUE(ri_host) TYPE REF TO zif_cilib_host
                            RAISING   zcx_cilib_not_found,
      get_cts_api RETURNING VALUE(ri_cts_api) TYPE REF TO zif_cilib_cts_api,
      get_logger RETURNING VALUE(ri_logger) TYPE REF TO zif_cilib_util_logger.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF gty_host_cache_line,
        host_path TYPE zcilib_host_hostpath,
        instance  TYPE REF TO zif_cilib_host,
      END OF gty_host_cache_line,
      BEGIN OF gty_bot_cache_line,
        bot_name TYPE zcilib_bot_name,
        instance TYPE REF TO zif_cilib_bot,
      END OF gty_bot_cache_line.
    CLASS-DATA:
      gi_abapgit_api TYPE REF TO zif_cilib_abapgit_api,
      gi_cts_api     TYPE REF TO zif_cilib_cts_api,
      gi_logger      TYPE REF TO zif_cilib_util_logger,
      gt_host_cache  TYPE HASHED TABLE OF gty_host_cache_line WITH UNIQUE KEY host_path,
      gt_bot_cache   TYPE HASHED TABLE OF gty_bot_cache_line WITH UNIQUE KEY bot_name.
ENDCLASS.



CLASS zcl_cilib_factory IMPLEMENTATION.
  METHOD get_abapgit_api.
    IF gi_abapgit_api IS NOT BOUND.
      gi_abapgit_api = NEW zcl_cilib_abapgit_api( ).
    ENDIF.
    ri_abapgit_api = gi_abapgit_api.
  ENDMETHOD.

  METHOD get_bot.
    TRY.
        ri_bot = gt_bot_cache[ KEY primary_key bot_name = iv_bot_name ]-instance.
      CATCH cx_sy_itab_line_not_found.
        DATA(lo_config) = zcl_cilib_cust_factory=>get_bot_config( iv_bot_name ).
        INSERT VALUE #(
          bot_name = iv_bot_name
          instance = NEW zcl_cilib_bot( iv_bot_name = iv_bot_name
                                        io_config   = lo_config
                                        ii_logger   = get_logger( )
                                        ii_cts_api  = get_cts_api( ) )
        ) INTO TABLE gt_bot_cache REFERENCE INTO DATA(lr_new).
        ri_bot = lr_new->instance.
    ENDTRY.
  ENDMETHOD.

  METHOD get_host.
    TRY.
        ri_host = gt_host_cache[ KEY primary_key host_path = iv_host ]-instance.
      CATCH cx_sy_itab_line_not_found.
        DATA(lo_config) = zcl_cilib_cust_factory=>get_host_config( iv_host ).
        DATA(lv_classname) = lo_config->get_host_implementation( ).

        INSERT VALUE #( host_path = iv_host ) INTO TABLE gt_host_cache REFERENCE INTO DATA(lr_new).
        CREATE OBJECT lr_new->instance TYPE (lv_classname)
          EXPORTING
            iv_host_path   = iv_host
            io_host_config = lo_config.

        ri_host = lr_new->instance.
    ENDTRY.
  ENDMETHOD.

  METHOD get_host_for_repo_url.
    ri_host = get_host( EXACT #( NEW zcl_cilib_http_url( iv_repo_url )->get_host( ) ) ).
  ENDMETHOD.

  METHOD get_cts_api.
    IF gi_cts_api IS NOT BOUND.
      gi_cts_api = NEW zcl_cilib_cts_api( ).
    ENDIF.
    ri_cts_api = gi_cts_api.
  ENDMETHOD.

  METHOD get_logger.
    IF gi_logger IS NOT BOUND.
      DATA(lo_settings) = zcl_cilib_cust_factory=>get_settings( ).
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
ENDCLASS.
