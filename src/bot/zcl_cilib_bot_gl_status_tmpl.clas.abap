"! GitLab status comment template
CLASS zcl_cilib_bot_gl_status_tmpl DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_bot_status_tmpl.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      deserialize IMPORTING iv_xml        TYPE csequence
                  EXPORTING et_systems    TYPE zif_cilib_bot_status_tmpl=>gty_system_tab
                            et_transports TYPE zif_cilib_bot_status_tmpl=>gty_transport_tab
                            et_history    TYPE stringtab
                  RAISING   cx_transformation_error
                            zcx_cilib_unsupp_operation.
    METHODS:
      normalize_before_serialization.
    DATA:
      mt_systems    TYPE zif_cilib_bot_status_tmpl=>gty_system_tab,
      mt_transports TYPE zif_cilib_bot_status_tmpl=>gty_transport_tab,
      mt_history    TYPE stringtab.
ENDCLASS.



CLASS zcl_cilib_bot_gl_status_tmpl IMPLEMENTATION.
  METHOD zif_cilib_bot_status_tmpl~parse_comment.
    DATA(lo_instance) = NEW zcl_cilib_bot_gl_status_tmpl( ).

    deserialize(
      EXPORTING
        iv_xml        = iv_comment
      IMPORTING
        et_systems    = lo_instance->mt_systems
        et_transports = lo_instance->mt_transports
        et_history    = lo_instance->mt_history
    ).

    ri_instance = lo_instance.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~get_comment_as_string.
    normalize_before_serialization( ).
    CALL TRANSFORMATION zcilib_bot_gl_status_tmpl
         SOURCE systems    = mt_systems
                transports = mt_transports
                history    = mt_history
         RESULT XML rv_comment.
    rv_comment = rv_comment+40. " Remove byte order mark and XML version
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~add_system.
    APPEND VALUE #( id = iv_id description = iv_description ) TO mt_systems.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~add_transport.
    INSERT VALUE #(
      transport   = iv_transport
      text        = iv_text
      released    = iv_released
      cts_url     = iv_cts_url
      import_info = it_import_info
    ) INTO TABLE mt_transports.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~get_systems.
    rt_systems = mt_systems.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~remove_system.
    DELETE mt_systems USING KEY unique WHERE id = iv_system.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~set_systems.
    mt_systems = it_systems.
  ENDMETHOD.

  METHOD normalize_before_serialization.
    DATA: lt_systems_range TYPE RANGE OF syst_sysid.

    lt_systems_range = VALUE #( FOR s IN mt_systems ( sign = 'I' option = 'EQ' low = s-id ) ).

    LOOP AT mt_transports ASSIGNING FIELD-SYMBOL(<ls_transport>).
      " Delete irrelevant import info
      LOOP AT <ls_transport>-import_info ASSIGNING FIELD-SYMBOL(<ls_info>).
        IF <ls_info>-system NOT IN lt_systems_range.
          DELETE <ls_transport>-import_info.
        ENDIF.
      ENDLOOP.

      " Sort import_info by order in mt_systems
      LOOP AT mt_systems ASSIGNING FIELD-SYMBOL(<ls_system>).
        DATA(lv_index) = sy-tabix.
        TRY.
            DATA(ls_info) = <ls_transport>-import_info[ KEY unique system = <ls_system>-id ].
            DELETE <ls_transport>-import_info USING KEY unique WHERE system = <ls_system>-id.
            INSERT ls_info INTO <ls_transport>-import_info INDEX lv_index.
          CATCH cx_sy_itab_line_not_found.
            INSERT VALUE #(
              system        = <ls_system>-id
              import_status = zif_cilib_bot_status_tmpl=>gc_import_status-no_info
            ) INTO <ls_transport>-import_info INDEX lv_index.
        ENDTRY.
      ENDLOOP.

      " Set icons
      LOOP AT <ls_transport>-import_info ASSIGNING FIELD-SYMBOL(<ls_info2>).
        <ls_info2>-icon = SWITCH #( <ls_info2>-import_status
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-imported          THEN ':heavy_check_mark:'
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-released          THEN ':truck:'
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-error_on_import   THEN ':x:'
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-warning_on_import THEN ':warning:'
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-import_planned    THEN ':clock5:'
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-not_imported      THEN ''
          WHEN zif_cilib_bot_status_tmpl=>gc_import_status-no_info           THEN ''
        ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~add_history_entry.
    APPEND iv_entry TO mt_history.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~get_transports.
    rt_transports = mt_transports.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~is_comment_parsable.
    TRY.
        deserialize(
          EXPORTING
            iv_xml        = iv_comment
          IMPORTING
            et_systems    = DATA(lt_systems)
            et_transports = DATA(lt_transports)
            et_history    = DATA(lt_history)
        ).
        rv_parsable = abap_true.
      CATCH cx_transformation_error zcx_cilib_unsupp_operation.
        rv_parsable = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~set_transports.
    mt_transports = it_transports.
  ENDMETHOD.

  METHOD deserialize.
    CONSTANTS: lc_pattern TYPE string VALUE `<div\s+data-template="(\w+)"\s+data-version="(\d+)">`.

    FIND FIRST OCCURRENCE OF REGEX lc_pattern IN iv_xml SUBMATCHES DATA(lv_template) DATA(lv_version).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_unsupp_operation.
    ENDIF.

    CALL TRANSFORMATION zcilib_bot_gl_status_tmpl
         SOURCE XML iv_xml
         RESULT systems    = et_systems
                transports = et_transports
                history    = et_history.
  ENDMETHOD.
ENDCLASS.
