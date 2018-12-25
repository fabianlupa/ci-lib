"! Application log logger
"! <p>
"! Implementation based on: https://github.com/flaiker/abap-log/blob/master/src/zcl_alog_bal_logger.clas.abap
"! </p>
CLASS zcl_cilib_util_bal_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_util_logger.
    METHODS:
      constructor IMPORTING iv_object      TYPE balobj_d OPTIONAL
                            iv_subobject   TYPE balsubobj OPTIONAL
                            iv_ext_id      TYPE balnrext OPTIONAL
                            iv_date_delete TYPE aldate_del OPTIONAL
                  RAISING   zcx_cilib_illegal_argument.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      save RAISING zcx_cilib_util_logging_failed.
    DATA:
      mv_log_handle TYPE balloghndl.
ENDCLASS.



CLASS zcl_cilib_util_bal_logger IMPLEMENTATION.
  METHOD constructor.
    DATA(ls_log) = VALUE bal_s_log(
      extnumber  = iv_ext_id
      object     = iv_object
      subobject  = iv_subobject
      aldate_del = iv_date_delete
    ).

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = mv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_illegal_argument
        EXPORTING
          is_msg = zcl_cilib_util_msg_tools=>get_msg_from_sy( ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~debug.
    zif_cilib_util_logger~entry( iv_text = iv_text iv_type = zif_cilib_util_logger~gc_entry_types-debug ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~debug_msg.
    zif_cilib_util_logger~entry_msg( iv_msgid = iv_msgid
                                     iv_msgno = iv_msgno
                                     iv_msgty = 'D'
                                     iv_msgv1 = iv_msgv1
                                     iv_msgv2 = iv_msgv2
                                     iv_msgv3 = iv_msgv3
                                     iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~entry.
    IF iv_type NA zif_cilib_util_logger~gc_entry_types.
      RAISE EXCEPTION TYPE zcx_cilib_illegal_argument.
    ENDIF.

    DATA(lv_msg_type) = SWITCH syst_msgty( iv_type
      WHEN zif_cilib_util_logger~gc_entry_types-debug   THEN 'I'
      WHEN zif_cilib_util_logger~gc_entry_types-info    THEN 'I'
      WHEN zif_cilib_util_logger~gc_entry_types-warning THEN 'W'
      WHEN zif_cilib_util_logger~gc_entry_types-error   THEN 'E'
    ).
    DATA(lv_probclass) = SWITCH balprobcl( iv_type
      WHEN zif_cilib_util_logger~gc_entry_types-debug   THEN '4'
      WHEN zif_cilib_util_logger~gc_entry_types-info    THEN '3'
      WHEN zif_cilib_util_logger~gc_entry_types-warning THEN '2'
      WHEN zif_cilib_util_logger~gc_entry_types-error   THEN '1'
    ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = lv_msg_type
        i_probclass      = lv_probclass
        i_text           = CONV char200( iv_text )
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_util_logging_failed
        EXPORTING
          is_msg = zcl_cilib_util_msg_tools=>get_msg_from_sy( ).
    ENDIF.

    save( ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~entry_msg.
    DATA(lv_probclass) = SWITCH balprobcl( iv_msgty
      WHEN 'D' THEN '4'
      WHEN 'I' THEN '3'
      WHEN 'W' THEN '2'
      WHEN 'E' OR 'X' OR 'A' THEN '1'
    ).

    DATA(lv_msgty) = COND #( WHEN iv_msgty <> 'D' THEN iv_msgty ELSE 'I' ).

    DATA(ls_msg) = VALUE bal_s_msg(
      msgty     = lv_msgty
      msgid     = iv_msgid
      msgno     = iv_msgno
      msgv1     = iv_msgv1
      msgv2     = iv_msgv2
      msgv3     = iv_msgv3
      msgv4     = iv_msgv4
      probclass = lv_probclass
    ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_s_msg          = ls_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_util_logging_failed
        EXPORTING
          is_msg = zcl_cilib_util_msg_tools=>get_msg_from_sy( ).
    ENDIF.

    save( ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~error.
    zif_cilib_util_logger~entry( iv_text = iv_text iv_type = zif_cilib_util_logger~gc_entry_types-error ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~error_msg.
    zif_cilib_util_logger~entry_msg( iv_msgid = iv_msgid
                                     iv_msgno = iv_msgno
                                     iv_msgty = 'E'
                                     iv_msgv1 = iv_msgv1
                                     iv_msgv2 = iv_msgv2
                                     iv_msgv3 = iv_msgv3
                                     iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~exception.
    CALL FUNCTION 'BAL_LOG_EXC_ADD'
      EXPORTING
        i_log_handle     = mv_log_handle
        i_msgty          = 'E'
        i_probclass      = '1'
        i_exception      = ix_ex
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_util_logging_failed
        EXPORTING
          is_msg = zcl_cilib_util_msg_tools=>get_msg_from_sy( ).
    ENDIF.

    save( ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~info.
    zif_cilib_util_logger~entry( iv_text = iv_text iv_type = zif_cilib_util_logger~gc_entry_types-info ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~info_msg.
    zif_cilib_util_logger~entry_msg( iv_msgid = iv_msgid
                                     iv_msgno = iv_msgno
                                     iv_msgty = 'I'
                                     iv_msgv1 = iv_msgv1
                                     iv_msgv2 = iv_msgv2
                                     iv_msgv3 = iv_msgv3
                                     iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~warning.
    zif_cilib_util_logger~entry( iv_text = iv_text iv_type = zif_cilib_util_logger~gc_entry_types-warning ).
  ENDMETHOD.

  METHOD zif_cilib_util_logger~warning_msg.
    zif_cilib_util_logger~entry_msg( iv_msgid = iv_msgid
                                     iv_msgno = iv_msgno
                                     iv_msgty = 'W'
                                     iv_msgv1 = iv_msgv1
                                     iv_msgv2 = iv_msgv2
                                     iv_msgv3 = iv_msgv3
                                     iv_msgv4 = iv_msgv4 ).
  ENDMETHOD.

  METHOD save.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = abap_false
        i_t_log_handle   = VALUE bal_t_logh( ( mv_log_handle ) )
        i_2th_connection = abap_true
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cilib_util_logging_failed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
