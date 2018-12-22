"! Event handler
CLASS zcl_cilib_exit_event_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      gty_event TYPE c LENGTH 1.
    CONSTANTS:
      BEGIN OF gc_events,
        after_export TYPE gty_event VALUE 'E',
        after_import TYPE gty_event VALUE 'I',
      END OF gc_events.
    CLASS-METHODS:
      handle_tr_feedback IMPORTING iv_event     TYPE gty_event
                                   iv_transport TYPE trkorr
                                   iv_system    TYPE syst_sysid
                         RAISING   zcx_cilib_illegal_argument.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_exit_event_handler IMPLEMENTATION.
  METHOD handle_tr_feedback.
    DATA: lt_repos        TYPE SORTED TABLE OF zif_cilib_abapgit_api=>gty_repo_key WITH UNIQUE KEY table_line,
          lv_repo_key     TYPE zif_cilib_abapgit_api=>gty_repo_key,
          lb_exit_repo_tr TYPE REF TO zcilib_exit_repo_tr.

    IF iv_event NA gc_events.
      RAISE EXCEPTION TYPE zcx_cilib_illegal_argument.
    ENDIF.

    DATA(li_cts_api) = zcl_cilib_factory=>get_cts_api( ).
    DATA(lt_objects) = li_cts_api->get_locked_r3tr_objects_in_tr( iv_transport ).
    DATA(lt_packages) = li_cts_api->get_packages_for_objects( lt_objects ).

    CHECK lt_objects IS NOT INITIAL.

    DATA(li_abapgit) = zcl_cilib_factory=>get_abapgit_api( ).

    LOOP AT lt_packages ASSIGNING FIELD-SYMBOL(<lv_package>).
      IF li_abapgit->is_object_part_of_online_repo(
           EXPORTING
             is_object   = VALUE #( type = 'DEVC' name = <lv_package> )
           IMPORTING
             ev_repo_key = lv_repo_key
         ) = abap_true.
        INSERT lv_repo_key INTO TABLE lt_repos.
      ENDIF.
    ENDLOOP.

    CASE lines( lt_repos ).
      WHEN 0.
        RETURN.
      WHEN 1.
        DATA(lv_repo_id) = lt_repos[ 1 ].
        GET BADI lb_exit_repo_tr.

        CASE iv_event.
          WHEN gc_events-after_import.
            CALL BADI lb_exit_repo_tr->on_transport_imported
              EXPORTING
                iv_system    = iv_system
                iv_transport = iv_transport
                iv_repo_id   = lv_repo_id.
          WHEN gc_events-after_export.
            CALL BADI lb_exit_repo_tr->on_transport_released
              EXPORTING
                iv_system    = iv_system
                iv_transport = iv_transport
                iv_repo_id   = lv_repo_id.
        ENDCASE.
*        DATA(lv_repo_url) = li_abapgit->get_repo_url( lt_repos[ 1 ] ).
*        DATA(li_host) = zcl_cilib_factory=>get_host_for_repo( lv_repo_url ).
        ##TODO.
      WHEN OTHERS.
        " Multiple git projects in one transport
        ##TODO.
    ENDCASE.

*    CASE iv_event.
*      WHEN gc_events-after_export.
*        DATA(li_abapgit) = zcl_cilib_factory=>get_abapgit_api( ).
*        IF li_abapgit->is_transport_relevant( trkorr ) = abap_true.
*          LOOP AT li_abapgit->get_repos_by_transport( trkorr ) INTO DATA(li_repo).
*            TRY.
*                DATA(li_bot) = zcl_cilib_factory=>get_bot( li_repo->get_provider( ) ).
**                li_bot->
*              catch zcx_cilib_unsupported_provider.
*
*            ENDTRY.
*          ENDLOOP.
*          IF sy-subrc <> 0.
*            " ???
*          ENDIF.
*        ENDIF.
*      WHEN gc_events-after_import.
*
*    ENDCASE.
  ENDMETHOD.
ENDCLASS.
