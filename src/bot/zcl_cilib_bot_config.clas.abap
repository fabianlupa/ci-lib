"! Bot configuration
CLASS zcl_cilib_bot_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_bot_name TYPE zcilib_bot_name
                            is_data     TYPE zcilib_bot_cfg_data,
      is_cts_status_comment_enabled RETURNING VALUE(rv_enabled) TYPE abap_bool,
      are_cts_upd_comments_enabled RETURNING VALUE(rv_enabled) TYPE abap_bool,
      is_cts_wiki_status_enabled RETURNING VALUE(rv_enabled) TYPE abap_bool,
      get_cts_status_impl_classname RETURNING VALUE(rv_classname) TYPE abap_classname,
      get_cts_upd_impl_classname RETURNING VALUE(rv_classname) TYPE abap_classname,
      get_cts_wiki_st_impl_classname RETURNING VALUE(rv_classname) TYPE abap_classname,
      get_system_group RETURNING VALUE(rv_system_group) TYPE zcilib_cust_sysgrp.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_name TYPE zcilib_bot_name,
      ms_data TYPE zcilib_bot_cfg_data.
ENDCLASS.



CLASS zcl_cilib_bot_config IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_bot_name.
    ms_data = is_data.
  ENDMETHOD.

  METHOD are_cts_upd_comments_enabled.
    rv_enabled = ms_data-enable_cts_updates.
  ENDMETHOD.

  METHOD is_cts_status_comment_enabled.
    rv_enabled = ms_data-enable_cts_status.
  ENDMETHOD.

  METHOD get_cts_status_impl_classname.
    rv_classname = ms_data-cts_status_template_impl.
  ENDMETHOD.

  METHOD get_cts_upd_impl_classname.
    rv_classname = ms_data-cts_update_template_impl.
  ENDMETHOD.

  METHOD get_cts_wiki_st_impl_classname.
    rv_classname = ms_data-cts_status_wiki_template_impl.
  ENDMETHOD.

  METHOD get_system_group.
    rv_system_group = ms_data-system_group.
  ENDMETHOD.

  METHOD is_cts_wiki_status_enabled.
    rv_enabled = ms_data-enable_cts_status_wiki.
  ENDMETHOD.
ENDCLASS.
