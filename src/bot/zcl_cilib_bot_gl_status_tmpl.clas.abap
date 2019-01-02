"! GitLab status comment template
CLASS zcl_cilib_bot_gl_status_tmpl DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_bot_status_tmpl.
  PROTECTED SECTION.
  PRIVATE SECTION.
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

    CALL TRANSFORMATION zcilib_bot_gl_status_tmpl
         SOURCE XML iv_comment
         RESULT systems    = lo_instance->mt_systems
                transports = lo_instance->mt_transports
                history    = lo_instance->mt_history.

    ri_instance = lo_instance.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~get_comment_as_string.
    normalize_before_serialization( ).
    CALL TRANSFORMATION zcilib_bot_gl_status_tmpl
         SOURCE systems    = mt_systems
                transports = mt_transports
                history    = mt_history
         RESULT XML rv_comment.
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
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~add_history_entry.
    APPEND iv_entry TO mt_history.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~get_transports.
    rt_transports = mt_transports.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~is_comment_parsable.
    rv_parsable = abap_true ##TODO.
  ENDMETHOD.

  METHOD zif_cilib_bot_status_tmpl~set_transports.
    mt_transports = it_transports.
  ENDMETHOD.
ENDCLASS.
