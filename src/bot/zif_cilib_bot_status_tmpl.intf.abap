"! CTS status comment template
INTERFACE zif_cilib_bot_status_tmpl PUBLIC.
  TYPES:
    BEGIN OF gty_system,
      id          TYPE syst_sysid,
      description TYPE string,
    END OF gty_system,
    gty_system_tab    TYPE STANDARD TABLE OF gty_system WITH NON-UNIQUE KEY id
                           WITH UNIQUE HASHED KEY unique COMPONENTS id,
    gty_import_status TYPE c LENGTH 1,
    BEGIN OF gty_import_info,
      system        TYPE syst_sysid,
      import_status TYPE gty_import_status,
    END OF gty_import_info,
    gty_import_info_tab TYPE STANDARD TABLE OF gty_import_info WITH EMPTY KEY
                             WITH UNIQUE HASHED KEY unique COMPONENTS system,
    BEGIN OF gty_transport,
      transport   TYPE trkorr,
      text        TYPE string,
      released    TYPE abap_bool,
      import_info TYPE gty_import_info_tab,
      cts_url     TYPE string,
    END OF gty_transport,
    gty_transport_tab TYPE SORTED TABLE OF gty_transport WITH UNIQUE KEY transport.
  CONSTANTS:
    BEGIN OF gc_import_status,
      released        TYPE gty_import_status VALUE 'R',
      not_imported    TYPE gty_import_status VALUE 'N',
      imported        TYPE gty_import_status VALUE 'I',
      error_on_import TYPE gty_import_status VALUE 'E',
      import_planned  TYPE gty_import_status VALUE 'P',
      no_info         TYPE gty_import_status VALUE '?',
    END OF gc_import_status.
  CLASS-METHODS:
    parse_comment IMPORTING iv_comment         TYPE string
                  RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_status_tmpl
                  RAISING   zcx_cilib_illegal_argument,
    is_comment_parsable IMPORTING iv_comment         TYPE string
                        RETURNING VALUE(rv_parsable) TYPE abap_bool.
  METHODS:
    get_comment_as_string RETURNING VALUE(rv_comment) TYPE string,
    set_systems IMPORTING it_systems TYPE gty_system_tab,
    add_system IMPORTING iv_id          TYPE syst_sysid
                         iv_description TYPE string,
    remove_system IMPORTING iv_system TYPE syst_sysid
                  RAISING   zcx_cilib_not_found,
    get_systems RETURNING VALUE(rt_systems) TYPE gty_system_tab,
    add_transport IMPORTING iv_transport   TYPE trkorr
                            iv_text        TYPE csequence OPTIONAL
                            iv_released    TYPE abap_bool
                            iv_cts_url     TYPE string OPTIONAL
                            it_import_info TYPE gty_import_info_tab,
    get_transports RETURNING VALUE(rt_transports) TYPE gty_transport_tab,
    set_transports IMPORTING it_transports TYPE gty_transport_tab,
    add_history_entry IMPORTING iv_entry TYPE csequence.
ENDINTERFACE.
