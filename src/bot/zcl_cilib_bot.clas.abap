"! Bot
CLASS zcl_cilib_bot DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_bot.
    METHODS:
      constructor IMPORTING iv_bot_name TYPE zcilib_bot_name
                            io_config   TYPE REF TO zcl_cilib_bot_config
                            ii_logger   TYPE REF TO zif_cilib_util_logger
                            ii_cts_api  TYPE REF TO zif_cilib_cts_api.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_status_tmpl_intf_name TYPE abap_intfname VALUE 'ZIF_CILIB_BOT_STATUS_TMPL'.
    CLASS-METHODS:
      get_formatted_timestamp RETURNING VALUE(rv_timestamp) TYPE string,
      format_update_info_into_string IMPORTING is_info          TYPE zif_cilib_bot=>gty_transport_info
                                     RETURNING VALUE(rv_string) TYPE string.
    METHODS:
      call_is_parsable IMPORTING iv_comment         TYPE string
                       RETURNING VALUE(rv_parsable) TYPE abap_bool,
      call_parse_comment IMPORTING iv_comment         TYPE string
                         RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_status_tmpl,
      instantiate_status_template RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_status_tmpl,
      instantiate_wiki_status_templ RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_status_tmpl,
      instantiate_update_template RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_update_tmpl,
      handle_status_comment IMPORTING ii_host         TYPE REF TO zif_cilib_host
                                      iv_repo_name    TYPE string
                                      iv_pull_request TYPE i
                                      it_new_info     TYPE zif_cilib_bot=>gty_transport_info_tab
                            RAISING   zcx_cilib_http_comm_error
                                      zcx_cilib_not_found,
      handle_wiki_status IMPORTING ii_host           TYPE REF TO zif_cilib_host
                                   iv_repo_name      TYPE string
                                   iv_wiki_page_path TYPE zcilib_bot_ctsstatuswikipath
                                   it_new_info       TYPE zif_cilib_bot=>gty_transport_info_tab
                         RAISING   zcx_cilib_http_comm_error
                                   zcx_cilib_not_found,
      handle_update_comment IMPORTING ii_host         TYPE REF TO zif_cilib_host
                                      iv_repo_name    TYPE string
                                      iv_pull_request TYPE i
                                      it_new_info     TYPE zif_cilib_bot=>gty_transport_info_tab
                            RAISING   zcx_cilib_http_comm_error
                                      zcx_cilib_not_found,
      update_status_with_new_info IMPORTING ii_status_template TYPE REF TO zif_cilib_bot_status_tmpl
                                            it_new_info        TYPE zif_cilib_bot=>gty_transport_info_tab.
    DATA:
      mv_name    TYPE zcilib_bot_name,
      mi_logger  TYPE REF TO zif_cilib_util_logger,
      mo_config  TYPE REF TO zcl_cilib_bot_config,
      mi_cts_api TYPE REF TO zif_cilib_cts_api.
ENDCLASS.



CLASS zcl_cilib_bot IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_bot_name.
    mi_logger = ii_logger.
    mo_config = io_config.
    mi_cts_api = ii_cts_api.
  ENDMETHOD.

  METHOD zif_cilib_bot~add_info_to_cts_comment.
    rv_success = abap_false.

    mi_logger->info( |{ mv_name }-{ ii_host->get_host_path( ) }-{ iv_repo }: add_info_to_cts_comment| ).
    IF mo_config->is_cts_status_comment_enabled( ) = abap_false AND
       mo_config->are_cts_upd_comments_enabled( ) = abap_false.
      mi_logger->info( 'CTS comments on pull requests are not enabled' ).
      RETURN.
    ENDIF.

    IF lines( it_new_info ) = 0.
      mi_logger->warning( 'Transport info is initial' ).
      RETURN.
    ENDIF.

    DATA(lv_repo_name) = CONV string( iv_repo ).

    TRY.
        IF ii_host->does_repo_exist( lv_repo_name ) = abap_false.
          mi_logger->error( |Repository '{ iv_repo }' not found.| ).
          RETURN.
        ENDIF.

        DATA(lt_branches) = ii_host->get_repo_branches( lv_repo_name ).

        LOOP AT lt_branches INTO DATA(lv_branch) WHERE table_line = iv_branch
                                                    OR table_line = to_lower( iv_branch ).
          EXIT.
        ENDLOOP.

        IF sy-subrc <> 0 OR lv_branch IS INITIAL.
          mi_logger->error( |Could not find branch '{ iv_branch }'| ).
          RETURN.
        ENDIF.

        DATA(lv_pull_request) = ii_host->get_pull_request_for_branch(
          iv_repository = lv_repo_name
          iv_branch     = lv_branch
        ).
        mi_logger->debug( |Found pull request { lv_pull_request }| ).

        IF mo_config->is_cts_status_comment_enabled( ) = abap_true.
          handle_status_comment(
            ii_host         = ii_host
            iv_repo_name    = lv_repo_name
            iv_pull_request = lv_pull_request
            it_new_info     = it_new_info
          ).
        ENDIF.

        IF mo_config->are_cts_upd_comments_enabled( ) = abap_true.
          handle_update_comment(
            ii_host         = ii_host
            iv_repo_name    = lv_repo_name
            iv_pull_request = lv_pull_request
            it_new_info     = it_new_info
          ).
        ENDIF.

        rv_success = abap_true.

      CATCH zcx_cilib_http_comm_error INTO DATA(lx_comm_error).
        mi_logger->exception( lx_comm_error ).
      CATCH zcx_cilib_not_found INTO DATA(lx_not_found).
        " Repo / branch / pull request / comment / transport was not found
        mi_logger->exception( lx_not_found ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cilib_bot~add_info_to_wiki_page.
    rv_success = abap_false.

    mi_logger->info( |{ mv_name }-{ ii_host->get_host_path( ) }-{ iv_repo }: add_info_to_wiki_page| ).
    IF mo_config->is_cts_wiki_status_enabled( ) = abap_false.
      mi_logger->info( 'CTS wiki status pages are not enabled' ).
      RETURN.
    ENDIF.

    IF lines( it_new_info ) = 0.
      mi_logger->warning( 'Transport info is initial' ).
      RETURN.
    ENDIF.

    DATA(lv_repo_name) = CONV string( iv_repo ).

    TRY.
        IF ii_host->does_repo_exist( lv_repo_name ) = abap_false.
          mi_logger->error( |Repository '{ iv_repo }' not found.| ).
          RETURN.
        ENDIF.

        handle_wiki_status(
          ii_host           = ii_host
          iv_repo_name      = lv_repo_name
          iv_wiki_page_path = io_repo_config->get_cts_wiki_page_path( )
          it_new_info       = it_new_info
        ).

        rv_success = abap_true.

      CATCH zcx_cilib_http_comm_error INTO DATA(lx_comm_error).
        mi_logger->exception( lx_comm_error ).
      CATCH zcx_cilib_not_found INTO DATA(lx_not_found).
        " Repo / branch / pull request / comment / transport was not found
        mi_logger->exception( lx_not_found ).
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cilib_bot~reorg_cts_comments.
    RAISE EXCEPTION TYPE zcx_cilib_not_implemented.
  ENDMETHOD.

  METHOD call_is_parsable.
    CONSTANTS: lc_method TYPE abap_methname VALUE 'IS_COMMENT_PARSABLE'.

    DATA(lv_class) = mo_config->get_cts_status_impl_classname( ).
    DATA(lv_method) = |{ gc_status_tmpl_intf_name }~{ lc_method }|.

    CALL METHOD (lv_class)=>(lv_method)
      EXPORTING
        iv_comment  = iv_comment
      RECEIVING
        rv_parsable = rv_parsable.
  ENDMETHOD.

  METHOD call_parse_comment.
    CONSTANTS: lc_method TYPE abap_methname VALUE 'PARSE_COMMENT'.

    DATA(lv_class) = mo_config->get_cts_status_impl_classname( ).
    DATA(lv_method) = |{ gc_status_tmpl_intf_name }~{ lc_method }|.

    CALL METHOD (lv_class)=>(lv_method)
      EXPORTING
        iv_comment  = iv_comment
      RECEIVING
        ri_instance = ri_instance.
  ENDMETHOD.

  METHOD instantiate_status_template.
    DATA(lv_class) = mo_config->get_cts_status_impl_classname( ).
    CREATE OBJECT ri_instance TYPE (lv_class).
  ENDMETHOD.

  METHOD instantiate_wiki_status_templ.
    DATA(lv_class) = mo_config->get_cts_wiki_st_impl_classname( ).
    CREATE OBJECT ri_instance TYPE (lv_class).
  ENDMETHOD.

  METHOD get_formatted_timestamp.
    rv_timestamp = |{ sy-datum DATE = ISO } { sy-uzeit TIME = ISO }|.
  ENDMETHOD.

  METHOD instantiate_update_template.
    DATA(lv_class) = mo_config->get_cts_upd_impl_classname( ).
    CREATE OBJECT ri_instance TYPE (lv_class).
  ENDMETHOD.

  METHOD format_update_info_into_string.
    rv_string = SWITCH #( is_info-event
      WHEN zif_cilib_bot=>gc_events-imported THEN SWITCH #( is_info-return_code
        WHEN space
        THEN |Imported transport { is_info-transport } on { is_info-system }.|
        ELSE |Imported transport { is_info-transport } on { is_info-system }, RC { is_info-return_code }.|
      )
      WHEN zif_cilib_bot=>gc_events-released THEN
           |Released transport { is_info-transport } on { is_info-system }.|
    ).
    ASSERT rv_string IS NOT INITIAL.
  ENDMETHOD.

  METHOD handle_status_comment.
    DATA: lv_update_comment  TYPE i,
          li_status_template TYPE REF TO zif_cilib_bot_status_tmpl.

    DATA(lv_bot_user) = ii_host->get_config( )->get_username( ).
    DATA(lt_comments) = ii_host->get_comments_for_pull_request(
      iv_repository   = iv_repo_name
      iv_pull_request = iv_pull_request
      iv_by_author    = lv_bot_user
    ).
    mi_logger->debug(
      |Found { lines( lt_comments ) NUMBER = USER } comments by bot user '{ lv_bot_user }' on PR|
    ).

    LOOP AT lt_comments ASSIGNING FIELD-SYMBOL(<ls_comment>).
      DATA(lv_content) = ii_host->get_pr_comment_content(
        iv_repository   = iv_repo_name
        iv_pull_request = iv_pull_request
        iv_comment      = <ls_comment>-id
      ).
      IF call_is_parsable( lv_content ) = abap_true.
        lv_update_comment = <ls_comment>-id.
        li_status_template = call_parse_comment( lv_content ).
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_update_comment IS NOT INITIAL.
      ASSERT li_status_template IS BOUND.
      mi_logger->debug( |Found existing PR comment { lv_update_comment }, updating| ).
    ELSE.
      mi_logger->debug( |Creating new PR comment| ).
      li_status_template = instantiate_status_template( ).
    ENDIF.

    DATA(lo_system_group) = zcl_cilib_cust_factory=>get_system_group( mo_config->get_system_group( ) ).
    li_status_template->set_systems( VALUE #( FOR s IN lo_system_group->get_systems( )
      ( id = s-system_id description = s-description )
    ) ).

    update_status_with_new_info( ii_status_template = li_status_template it_new_info = it_new_info ).

    IF lv_update_comment IS NOT INITIAL.
      ii_host->set_pr_comment_content(
        iv_repository   = iv_repo_name
        iv_pull_request = iv_pull_request
        iv_comment      = lv_update_comment
        iv_content      = li_status_template->get_comment_as_string( )
      ).
    ELSE.
      ii_host->create_pr_comment(
        iv_repository   = iv_repo_name
        iv_pull_request = iv_pull_request
        iv_content      = li_status_template->get_comment_as_string( )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_wiki_status.
    DATA: li_status_template TYPE REF TO zif_cilib_bot_status_tmpl,
          lv_page_exists     TYPE abap_bool.

    DATA(lv_bot_user) = ii_host->get_config( )->get_username( ).

    TRY.
        DATA(ls_wiki_page) = ii_host->get_wiki_page( iv_repository = iv_repo_name
                                                     iv_page_name  = CONV #( iv_wiki_page_path ) ).
        mi_logger->debug( |Found existing wiki page '{ iv_wiki_page_path }' for '{ iv_repo_name }', updating.| ).
        lv_page_exists = abap_true.
      CATCH zcx_cilib_not_found.
        mi_logger->debug(
          |Could not find wiki page '{ iv_wiki_page_path }' for '{ iv_repo_name }',creating new page.|
        ).
        lv_page_exists = abap_false.
    ENDTRY.

    IF lv_page_exists = abap_true.
      IF call_is_parsable( ls_wiki_page-content ) = abap_true.
        li_status_template = call_parse_comment( ls_wiki_page-content ).
      ELSE.
        mi_logger->debug( |Could not parse existing wiki page.| ).
      ENDIF.
    ENDIF.

    IF li_status_template IS NOT BOUND.
      li_status_template = instantiate_wiki_status_templ( ).
    ENDIF.

    DATA(lo_system_group) = zcl_cilib_cust_factory=>get_system_group( mo_config->get_system_group( ) ).
    li_status_template->set_systems( VALUE #( FOR s IN lo_system_group->get_systems( )
      ( id = s-system_id description = s-description )
    ) ).

    update_status_with_new_info( ii_status_template = li_status_template it_new_info = it_new_info ).

    IF lv_page_exists = abap_true.
      ii_host->update_wiki_page(
        iv_repository = iv_repo_name
        iv_page_name  = ls_wiki_page-name
        iv_content    = li_status_template->get_comment_as_string( )
        iv_title      = ls_wiki_page-title
        iv_format     = ls_wiki_page-format
      ).
    ELSE.
      ii_host->create_wiki_page(
        iv_repository = iv_repo_name
        iv_page_name  = CONV #( iv_wiki_page_path )
        iv_content    = li_status_template->get_comment_as_string( )
        iv_title      = CONV #( iv_wiki_page_path )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_update_comment.
    DATA(li_update_template) = instantiate_update_template( ).
    LOOP AT it_new_info ASSIGNING FIELD-SYMBOL(<ls_info>).
      li_update_template->add_update_entry( format_update_info_into_string( <ls_info> ) ).
    ENDLOOP.
    IF sy-subrc = 0.
      ii_host->create_pr_comment(
        iv_repository   = iv_repo_name
        iv_pull_request = iv_pull_request
        iv_content      = li_update_template->get_comment_as_string( )
      ).
    ENDIF.
  ENDMETHOD.

  METHOD update_status_with_new_info.
    DATA(lt_transports) = ii_status_template->get_transports( ).

    LOOP AT it_new_info ASSIGNING FIELD-SYMBOL(<ls_info>).
      READ TABLE lt_transports WITH KEY transport = <ls_info>-transport ASSIGNING FIELD-SYMBOL(<ls_transport>).
      IF sy-subrc <> 0.
        INSERT VALUE #( transport = <ls_info>-transport ) INTO TABLE lt_transports ASSIGNING <ls_transport>.
      ENDIF.
      ASSERT <ls_transport> IS ASSIGNED.
      <ls_transport>-text = mi_cts_api->get_transport_text( <ls_transport>-transport ).
      <ls_transport>-cts_url = mi_cts_api->get_cts_organizer_web_ui_url( <ls_transport>-transport ).

      CASE <ls_info>-event.
        WHEN zif_cilib_bot=>gc_events-released.
          <ls_transport>-released = abap_true.
          DATA(ls_info_released) = VALUE zif_cilib_bot_status_tmpl=>gty_import_info(
            system        = <ls_info>-system
            import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
          ).

          TRY.
              INSERT ls_info_released INTO TABLE <ls_transport>-import_info.
            CATCH cx_sy_itab_duplicate_key.
              MODIFY TABLE <ls_transport>-import_info FROM ls_info_released USING KEY unique.
          ENDTRY.

        WHEN zif_cilib_bot=>gc_events-imported.
          DATA(ls_info_imported) = VALUE zif_cilib_bot_status_tmpl=>gty_import_info(
            system        = <ls_info>-system
            import_status = SWITCH #( <ls_info>-return_code
                              WHEN '0000' THEN zif_cilib_bot_status_tmpl=>gc_import_status-imported
                              WHEN '0004' THEN zif_cilib_bot_status_tmpl=>gc_import_status-warning_on_import
                                          ELSE zif_cilib_bot_status_tmpl=>gc_import_status-error_on_import
                            )
          ).

          TRY.
              INSERT ls_info_imported INTO TABLE <ls_transport>-import_info.
            CATCH cx_sy_itab_duplicate_key.
              MODIFY TABLE <ls_transport>-import_info FROM ls_info_imported USING KEY unique.
          ENDTRY.

      ENDCASE.

      ii_status_template->add_history_entry(
        |{ get_formatted_timestamp( ) }: { format_update_info_into_string( <ls_info> ) }|
      ) ##TODO.
    ENDLOOP.

    ii_status_template->set_transports( lt_transports ).
  ENDMETHOD.
ENDCLASS.
