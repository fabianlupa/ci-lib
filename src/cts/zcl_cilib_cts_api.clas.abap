"! CTS API
CLASS zcl_cilib_cts_api DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_cilib_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_cts_api.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_cts_api IMPLEMENTATION.
  METHOD zif_cilib_cts_api~get_locked_r3tr_objects_in_tr.
    DATA: lt_objects TYPE tr_objects.

    CALL FUNCTION 'TR_GET_OBJECTS_OF_REQ_AN_TASKS'
      EXPORTING
        is_request_header      = VALUE trwbo_request_header( trkorr = iv_transport )
        iv_condense_objectlist = abap_true
      IMPORTING
        et_objects             = lt_objects
      EXCEPTIONS
        invalid_input          = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_illegal_argument
        EXPORTING
          is_msg = zcl_cilib_util_msg_tools=>get_msg_from_sy( ).
    ENDIF.

    LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      IF <ls_object>-pgmid = 'LIMU' AND <ls_object>-object = 'METH'.
        INSERT VALUE #( type = 'CLAS' name = <ls_object>-obj_name(30) ) INTO TABLE rt_objects.
        ##TODO.
      ELSEIF <ls_object>-pgmid = 'R3TR'.
        INSERT VALUE #( type = <ls_object>-object name = <ls_object>-obj_name ) INTO TABLE rt_objects.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cilib_cts_api~get_packages_for_objects.
    LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<ls_object>).
      INSERT zif_cilib_cts_api~get_package_for_object( <ls_object> ) INTO TABLE rt_packages.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cilib_cts_api~get_package_for_object.
    SELECT SINGLE devclass INTO @rv_package
      FROM tadir
      WHERE pgmid = 'R3TR'
        AND object = @is_object-type
        AND obj_name = @is_object-name.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cilib_cts_api~get_cts_organizer_web_ui_url.
    CALL FUNCTION 'CTS_CFG_GET_ORGANIZER_URL'
      EXPORTING
        sysname = iv_system
        request = iv_transport
        view    = COND char20( WHEN iv_transport IS NOT INITIAL THEN 'PROCESS_REQUEST' ELSE 'MANAGE_REQUESTS' )
      IMPORTING
        url     = rv_url.

    IF rv_url IS INITIAL.
      RAISE EXCEPTION TYPE zcx_cilib_illegal_argument.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
