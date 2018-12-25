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

    DATA(li_logger) = zcl_cilib_factory=>get_logger( ).

    DATA(li_cts_api) = zcl_cilib_factory=>get_cts_api( ).
    DATA(lt_objects) = li_cts_api->get_locked_r3tr_objects_in_tr( iv_transport ).
    DATA(lt_packages) = li_cts_api->get_packages_for_objects( lt_objects ).

    li_logger->debug( |Found { lines( lt_objects ) NUMBER = USER } locked R3TR objects in { iv_transport } | &&
                      |belonging to { lines( lt_packages ) NUMBER = USER } different packages.| ).

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

    li_logger->debug( |The packages belong to { lines( lt_repos ) NUMBER = USER } abapGit repositories.| ).

    IF lines( lt_repos ) > 0.
      ##TODO. " Think about when lines( lt_repos ) is greater than 1
      GET BADI lb_exit_repo_tr.

      LOOP AT lt_repos ASSIGNING FIELD-SYMBOL(<lv_repo>).
        CASE iv_event.
          WHEN gc_events-after_import.
            CALL BADI lb_exit_repo_tr->on_transport_imported
              EXPORTING
                iv_system    = iv_system
                iv_transport = iv_transport
                iv_repo_id   = <lv_repo>.
          WHEN gc_events-after_export.
            CALL BADI lb_exit_repo_tr->on_transport_released
              EXPORTING
                iv_system    = iv_system
                iv_transport = iv_transport
                iv_repo_id   = <lv_repo>.
        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
