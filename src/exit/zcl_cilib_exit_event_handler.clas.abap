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
                                   iv_system    TYPE syst_sysid.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_exit_event_handler IMPLEMENTATION.
  METHOD handle_tr_feedback.
    IF iv_event NA gc_events.

    ENDIF.

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
