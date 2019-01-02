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
ENDCLASS.



CLASS zcl_cilib_bot_repo_tr_listener IMPLEMENTATION.
  METHOD zif_cilib_exit_repo_tr~on_transport_imported.
    ##TODO.
  ENDMETHOD.

  METHOD zif_cilib_exit_repo_tr~on_transport_released.
    " Determine which bot should be used
    DATA(li_abapgit) = zcl_cilib_factory=>get_abapgit_api( ).
    DATA(lv_repo_url) = li_abapgit->get_repo_url( iv_repo_id ).
    DATA(li_host) = zcl_cilib_factory=>get_host_for_repo_url( lv_repo_url ).
    DATA(lv_repo_name) = li_host->get_repo_name_from_url( lv_repo_url ).
    DATA(lv_bot_name) = li_host->get_config( )->get_repo_config( EXACT #( lv_repo_name ) )->get_bot_name( ).

    DATA(li_bot) = zcl_cilib_factory=>get_bot( lv_bot_name ).
    li_bot->add_info_to_cts_comment(
      ii_host     = li_host
      iv_repo     = EXACT #( lv_repo_name )
      iv_branch   = CONV #( iv_transport )
      it_new_info = VALUE #(
        ( transport = iv_transport
          system    = iv_system
          event     = zif_cilib_bot=>gc_events-released )
      )
    ).
  ENDMETHOD.
ENDCLASS.
