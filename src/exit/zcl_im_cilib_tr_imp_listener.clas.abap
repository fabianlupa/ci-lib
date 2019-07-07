CLASS zcl_im_cilib_tr_imp_listener DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_ex_cts_import_feedback.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_im_cilib_tr_imp_listener IMPLEMENTATION.
  METHOD if_ex_cts_import_feedback~feedback_after_import.
    DATA: lv_message TYPE c LENGTH 50.

    DATA(lv_destination) = zcl_cilib_cust_factory=>get_settings( )->get_exit_event_destination( ).
    CHECK lv_destination IS NOT INITIAL.

    LOOP AT requests ASSIGNING FIELD-SYMBOL(<ls_request>).
      IF <ls_request>-tarsystem = sy-sysid.
        CALL FUNCTION 'ZCILIB_EXIT_RAISE_EVENT'
          DESTINATION lv_destination
          EXPORTING
            iv_event              = zcl_cilib_exit_event_handler=>gc_events-after_import
            iv_transport          = <ls_request>-trkorr
            iv_system             = sy-sysid
            iv_return_code        = <ls_request>-retcode
          EXCEPTIONS
            communication_failure = 1 MESSAGE lv_message
            system_failure        = 2 MESSAGE lv_message
            OTHERS                = 3.
        IF sy-subrc <> 0.
          TRY.
              DATA(lv_error_message) = |Error raising event AFTER_IMPORT to destination '{ lv_destination }': | &&
                                       |subrc { sy-subrc }, '{ lv_message }|.
              cl_syslog_writer=>write_entry_with_words(
                iv_message_id = 'TR1'
                iv_word1      = |ci-lib: { lv_error_message }|
              ).
            CATCH cx_root INTO DATA(lx_ex) ##CATCH_ALL.
              " :( Better than shortdumping the CTS process
          ENDTRY.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
