*"* use this source file for your ABAP unit test classes

CLASS ltc_test_xslt DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS:
      two_way_test_xslt FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ltc_test_xslt IMPLEMENTATION.
  METHOD two_way_test_xslt.
    DATA: lt_systems     TYPE zif_cilib_bot_status_tmpl=>gty_system_tab,
          lt_transports  TYPE zif_cilib_bot_status_tmpl=>gty_transport_tab,
          lt_history     TYPE stringtab,
          lt_systems2    TYPE zif_cilib_bot_status_tmpl=>gty_system_tab,
          lt_transports2 TYPE zif_cilib_bot_status_tmpl=>gty_transport_tab,
          lt_history2    TYPE stringtab,
          lv_xml         TYPE string.

    lt_systems = VALUE #(
      ( id = '001' description = 'Production System' )
      ( id = '002' description = 'Quality Assurance System' )
      ( id = '003' description = 'Development System' )
    ).

    lt_transports = VALUE #(
      ( transport = 'NPLK123456789123'
        text = 'Lorem ipsum dolor'
*        released = abap_true
        import_info = VALUE #(
          ( system        = '001'
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-imported
            icon          = ':heavy_check_mark:' )
          ( system        = '002'
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-warning_on_import
            icon          = ':heavy_check_mark:' )
          ( system        = '003'
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
            icon          = ':heavy_check_mark:' )
        )
        cts_url = 'http://localhost' )
      ( transport = 'NPLK123456789124'
        text = 'Lorem ipsum dolor'
*        released = abap_false
        import_info = VALUE #( )
        cts_url = 'http://localhost' )
      ( transport = 'NPLK123456789125'
        text = 'Lorem ipsum dolor'
*        released = abap_true
        import_info = VALUE #(
          ( system        = '002'
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-error_on_import
            icon          = ':heavy_check_mark:' )
          ( system        = '003'
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
            icon          = ':heavy_check_mark:' )
        )
        cts_url = 'http://localhost' )
      ( transport = 'NPLK123456789126'
        text = 'Lorem ipsum dolor'
*        released = abap_true
        import_info = VALUE #(
          ( system        = '003'
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
            icon          = ':heavy_check_mark:' )
        )
        cts_url = 'http://localhost' )
    ).

    lt_history = VALUE #(
      ( `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam ` )
      ( `nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam ` )
      ( `erat, sed diam voluptua. At vero eos et accusam et justo duo ` )
    ).

    CALL TRANSFORMATION zcilib_bot_gl_status_tmpl
         SOURCE systems    = lt_systems
                transports = lt_transports
                history    = lt_history
         RESULT XML lv_xml.

    CALL TRANSFORMATION zcilib_bot_gl_status_tmpl
         SOURCE XML lv_xml
         RESULT systems    = lt_systems2
                transports = lt_transports2
                history    = lt_history2.

    cl_abap_unit_assert=>assert_equals( exp = lt_systems act = lt_systems2 ).
    cl_abap_unit_assert=>assert_equals( exp = lt_transports act = lt_transports2 ).
    cl_abap_unit_assert=>assert_equals( exp = lt_history act = lt_history2 ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_test DEFINITION DEFERRED.
CLASS zcl_cilib_bot_gl_status_tmpl DEFINITION LOCAL FRIENDS ltc_test.

CLASS ltc_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PUBLIC SECTION.
    METHODS:
      two_way_test FOR TESTING.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      setup,
      teardown.
    DATA:
      mo_cut TYPE REF TO zcl_cilib_bot_gl_status_tmpl.
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.
  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    FREE mo_cut.
  ENDMETHOD.

  METHOD two_way_test.
    DATA(li_tmpl) = CAST zif_cilib_bot_status_tmpl( mo_cut ).

    li_tmpl->add_system( iv_id = '001' iv_description = 'Production System' ).
    li_tmpl->add_system( iv_id = '002' iv_description = 'Quality Assurance System' ).
    li_tmpl->add_system( iv_id = '003' iv_description = 'Development System' ).

    li_tmpl->add_transport(
      iv_transport   = 'NPLK123456789123'
      iv_text        = 'Lorem ipsum dolor'
      iv_released    = abap_false
      iv_cts_url     = 'http://localhost'
      it_import_info = VALUE #(
        ( system        = '001'
          import_status = zif_cilib_bot_status_tmpl=>gc_import_status-imported
          icon          = ':heavy_check_mark:' )
        ( system        = '002'
          import_status = zif_cilib_bot_status_tmpl=>gc_import_status-warning_on_import
          icon          = ':heavy_check_mark:' )
        ( system        = '003'
          import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
          icon          = ':heavy_check_mark:' )
       )
    ).

    li_tmpl->add_transport(
      iv_transport   = 'NPLK123456789124'
      iv_text        = 'Lorem ipsum dolor'
      iv_released    = abap_false
      iv_cts_url     = 'http://localhost'
      it_import_info = VALUE #( )
    ).

    li_tmpl->add_transport(
      iv_transport   = 'NPLK123456789125'
      iv_text        = 'Lorem ipsum dolor'
      iv_released    = abap_false
      iv_cts_url     = 'http://localhost'
      it_import_info = VALUE #(
        ( system        = '002'
          import_status = zif_cilib_bot_status_tmpl=>gc_import_status-error_on_import
          icon          = ':heavy_check_mark:' )
        ( system        = '003'
          import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
          icon          = ':heavy_check_mark:' )
      )
    ).

    li_tmpl->add_transport(
      iv_transport   = 'NPLK123456789126'
      iv_text        = 'Lorem ipsum dolor'
      iv_released    = abap_false
      iv_cts_url     = 'http://localhost'
      it_import_info = VALUE #(
        ( system        = '003'
          import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
          icon          = ':heavy_check_mark:' )
      )
    ).

    li_tmpl->add_history_entry( `Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam ` ).
    li_tmpl->add_history_entry( `nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam ` ).
    li_tmpl->add_history_entry( `erat, sed diam voluptua. At vero eos et accusam et justo duo ` ).

    DATA(lv_comment) = li_tmpl->get_comment_as_string( ).

    DATA(lo_cut2) = CAST zcl_cilib_bot_gl_status_tmpl(
      zcl_cilib_bot_gl_status_tmpl=>zif_cilib_bot_status_tmpl~parse_comment( lv_comment )
    ).
    cl_abap_unit_assert=>assert_equals( exp = mo_cut->mt_history act = lo_cut2->mt_history ).
    cl_abap_unit_assert=>assert_equals( exp = mo_cut->mt_systems act = lo_cut2->mt_systems ).
    cl_abap_unit_assert=>assert_equals( exp = mo_cut->mt_transports act = lo_cut2->mt_transports ).

    DATA(lv_comment2) = lo_cut2->zif_cilib_bot_status_tmpl~get_comment_as_string( ).

    cl_abap_unit_assert=>assert_equals( exp = lv_comment act = lv_comment2 ).
  ENDMETHOD.
ENDCLASS.
