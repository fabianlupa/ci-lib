"! Host configuration
CLASS zcl_cilib_host_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_repo,
        repo     TYPE zcilib_host_repo,
        instance TYPE REF TO zcl_cilib_host_repo_config,
      END OF gty_repo,
      gty_repo_tab TYPE SORTED TABLE OF gty_repo WITH UNIQUE KEY repo.
    METHODS:
      constructor IMPORTING iv_host  TYPE zcilib_host_hostpath
                            is_data  TYPE zcilib_host_cfg_data
                            it_repos TYPE gty_repo_tab,
      get_rfc_destination RETURNING VALUE(rv_destination) TYPE rfcdest,
      get_api_token RETURNING VALUE(rv_token) TYPE string,
      get_host_implementation RETURNING VALUE(rv_classname) TYPE abap_classname,
      get_repo_config IMPORTING iv_repo          TYPE zcilib_host_repo
                                iv_no_fallback   TYPE abap_bool DEFAULT abap_false
                      RETURNING VALUE(ro_config) TYPE REF TO zcl_cilib_host_repo_config
                      RAISING   zcx_cilib_not_found,
      get_default_bot_name RETURNING VALUE(rv_bot) TYPE zcilib_bot_name,
      get_username RETURNING VALUE(rv_username) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_fallback_repo_config IMPORTING iv_repo            TYPE zcilib_host_repo
                               RETURNING VALUE(ro_settings) TYPE REF TO zcl_cilib_host_repo_config.
    DATA:
      ms_data  TYPE zcilib_host_cfg_data,
      mt_repos TYPE gty_repo_tab.
ENDCLASS.



CLASS zcl_cilib_host_config IMPLEMENTATION.
  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.

  METHOD get_host_implementation.
    rv_classname = ms_data-host_impl.
  ENDMETHOD.

  METHOD get_rfc_destination.
    rv_destination = ms_data-destination.
  ENDMETHOD.

  METHOD get_api_token.
    rv_token = ms_data-api_token.
  ENDMETHOD.

  METHOD get_repo_config.
    TRY.
        ro_config = mt_repos[ KEY primary_key repo = iv_repo ]-instance.
      CATCH cx_sy_itab_line_not_found INTO DATA(lx_ex).
        IF iv_no_fallback = abap_true.
          RAISE EXCEPTION TYPE zcx_cilib_not_found
            EXPORTING
              is_textid    = zcx_cilib_not_found=>gc_with_name_and_key
              ix_previous  = lx_ex
              iv_type_name = 'Repo Config'
              iv_key       = iv_repo.
        ELSE.
          ro_config = get_fallback_repo_config( iv_repo ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD get_fallback_repo_config.
    ro_settings = NEW #(
      iv_repo = iv_repo
      is_data = VALUE #(
        bot = get_default_bot_name( )
      )
    ).
  ENDMETHOD.

  METHOD get_default_bot_name.
    rv_bot = ms_data-default_bot.
  ENDMETHOD.

  METHOD get_username.
    rv_username = 'cts-bot' ##TODO.
  ENDMETHOD.
ENDCLASS.
