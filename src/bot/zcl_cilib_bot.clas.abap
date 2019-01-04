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
    METHODS:
      call_is_parsable IMPORTING iv_comment         TYPE string
                       RETURNING VALUE(rv_parsable) TYPE abap_bool,
      call_parse_comment IMPORTING iv_comment         TYPE string
                         RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_status_tmpl,
      instantiate_status_template RETURNING VALUE(ri_instance) TYPE REF TO zif_cilib_bot_status_tmpl.
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
    DATA: lv_update_comment  TYPE i,
          li_status_template TYPE REF TO zif_cilib_bot_status_tmpl.

    rv_success = abap_false.

    mi_logger->info( |{ mv_name }-{ ii_host->get_host_path( ) }-{ iv_repo }: add_info_to_cts_comment| ).
    IF mo_config->is_cts_status_comment_enabled( ) = abap_false.
      mi_logger->info( 'CTS status comment is not enabled' ).
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

        DATA(lv_bot_user) = ii_host->get_config( )->get_username( ).
        DATA(lt_comments) = ii_host->get_comments_for_pull_request(
          iv_repository   = lv_repo_name
          iv_pull_request = lv_pull_request
          iv_by_author    = lv_bot_user
        ).
        mi_logger->debug( |Found { lines( lt_comments ) NUMBER = USER } comments by bot user '{ lv_bot_user }' on PR| ).

        LOOP AT lt_comments ASSIGNING FIELD-SYMBOL(<ls_comment>).
          DATA(lv_content) = ii_host->get_pr_comment_content(
            iv_repository   = lv_repo_name
            iv_pull_request = lv_pull_request
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

        DATA(lt_transports) = li_status_template->get_transports( ).

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
              IF <ls_transport>-released <> abap_true.
                li_status_template->add_history_entry(
                  |Released transport { <ls_transport>-transport } on { <ls_info>-system }|
                ) ##TODO.
              ENDIF.
              <ls_transport>-released = abap_true.
              TRY.
                  MODIFY TABLE <ls_transport>-import_info FROM VALUE #(
                    system        = <ls_info>-system
                    import_status = zif_cilib_bot_status_tmpl=>gc_import_status-released
                  ) USING KEY unique.
                CATCH cx_sy_itab_duplicate_key.
                  ##TODO.
              ENDTRY.

            WHEN zif_cilib_bot=>gc_events-imported.
              TRY.
                  INSERT VALUE #(
                    system        = <ls_info>-system
                    import_status = zif_cilib_bot_status_tmpl=>gc_import_status-imported
                  ) INTO TABLE <ls_transport>-import_info.
                  li_status_template->add_history_entry(
                    |Imported transport { <ls_transport>-transport } on { <ls_info>-system }|
                  ) ##TODO.
                CATCH cx_sy_itab_duplicate_key.
                  ##TODO.
              ENDTRY.
          ENDCASE.
        ENDLOOP.

        li_status_template->set_transports( lt_transports ).

        DATA(lo_system_group) = zcl_cilib_cust_factory=>get_system_group( mo_config->get_system_group( ) ).
        li_status_template->set_systems( VALUE #( FOR s IN lo_system_group->get_systems( )
          ( id = s-system_id description = s-description )
        ) ).

        IF lv_update_comment IS NOT INITIAL.
          ii_host->set_pr_comment_content(
            iv_repository   = lv_repo_name
            iv_pull_request = lv_pull_request
            iv_comment      = lv_update_comment
            iv_content      = li_status_template->get_comment_as_string( )
          ).
        ELSE.
          ii_host->create_pr_comment(
            iv_repository   = lv_repo_name
            iv_pull_request = lv_pull_request
            iv_content      = li_status_template->get_comment_as_string( )
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
ENDCLASS.
