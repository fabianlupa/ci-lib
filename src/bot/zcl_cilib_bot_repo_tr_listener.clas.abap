"! Bot repo transport listener
CLASS zcl_cilib_bot_repo_tr_listener DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_badi_interface,
      zif_cilib_exit_repo_tr.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      handle_event IMPORTING iv_system      TYPE syst_sysid
                             iv_transport   TYPE trkorr
                             iv_repo_id     TYPE zif_cilib_git_abapgit=>gty_repo_key
                             iv_repo_url    TYPE string
                             iv_event       TYPE zif_cilib_bot=>gty_event
                             iv_return_code TYPE trretcode OPTIONAL.
ENDCLASS.



CLASS zcl_cilib_bot_repo_tr_listener IMPLEMENTATION.
  METHOD zif_cilib_exit_repo_tr~on_transport_imported.
    handle_event( iv_system      = iv_system
                  iv_transport   = iv_transport
                  iv_repo_id     = iv_repo_id
                  iv_repo_url    = iv_repo_url
                  iv_event       = zif_cilib_bot=>gc_events-imported
                  iv_return_code = iv_return_code ).
  ENDMETHOD.

  METHOD zif_cilib_exit_repo_tr~on_transport_released.
    handle_event( iv_system    = iv_system
                  iv_transport = iv_transport
                  iv_repo_id   = iv_repo_id
                  iv_repo_url  = iv_repo_url
                  iv_event     = zif_cilib_bot=>gc_events-released ).
  ENDMETHOD.

  METHOD handle_event.
    DATA(li_host) = zcl_cilib_factory=>get_host_for_repo_url( iv_repo_url ).
    DATA(lv_repo_name) = li_host->get_repo_name_from_url( iv_repo_url ).
    DATA(lo_repo_config) = li_host->get_config( )->get_repo_config( EXACT #( lv_repo_name ) ).
    DATA(lv_bot_name) = lo_repo_config->get_bot_name( ).

    DATA(lv_branch) = zcl_cilib_factory=>get_branch_strategy_resolver( )->get_branch_for_transport(
      iv_transport = iv_transport
      iv_strategy  = lo_repo_config->get_branching_strategy( )
    ).

    DATA(li_bot) = zcl_cilib_factory=>get_bot( lv_bot_name ).

    DATA(lt_new_info) = VALUE zif_cilib_bot=>gty_transport_info_tab( (
      transport   = iv_transport
      system      = iv_system
      event       = iv_event
      return_code = iv_return_code
    ) ).

    li_bot->add_info_to_pull_request(
      ii_host     = li_host
      iv_repo     = EXACT #( lv_repo_name )
      iv_branch   = lv_branch
      it_new_info = lt_new_info
    ).

    li_bot->add_info_to_wiki_page(
      ii_host                    = li_host
      iv_repo                    = EXACT #( lv_repo_name )
      io_repo_config             = lo_repo_config
      it_new_info                = lt_new_info
    ).
  ENDMETHOD.
ENDCLASS.
