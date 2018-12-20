CLASS zcl_im_cilib_tr_exp_listener DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      if_ex_cts_export_feedback.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_im_cilib_tr_exp_listener IMPLEMENTATION.
  METHOD if_ex_cts_export_feedback~feedback_after_export.
    DATA: lv_message TYPE string.
    CHECK type = 'K'.

*    DATA(lo_config) = zcl_cilib_factory=>get_exit_config( ).
*    DATA(lv_destination) = lo_config->get_exit_rfc_destination( ).
*
*    CALL FUNCTION 'ZCILIB_EXIT_RAISE_EVENT'
*      DESTINATION lv_destination
*      EXPORTING
*        iv_event              = zcl_cilib_exit_event_handler=>gc_events-after_export
*        iv_transport_request  = trkorr
*        iv_system             = sy-sysid
*      EXCEPTIONS
*        communication_failure = 1 MESSAGE lv_message
*        system_failure        = 2 MESSAGE lv_message
*        OTHERS                = 3.
*    IF sy-subrc <> 0.
*
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
