"! Logger
"! <p>
"! Methods copied from https://github.com/flaiker/abap-log
"! </p>
INTERFACE zif_cilib_util_logger PUBLIC.
  TYPES:
    gty_entry_type TYPE c LENGTH 1.
  CONSTANTS:
    BEGIN OF gc_entry_types,
      info    TYPE gty_entry_type VALUE 'I',
      warning TYPE gty_entry_type VALUE 'W',
      error   TYPE gty_entry_type VALUE 'E',
      debug   TYPE gty_entry_type VALUE 'D',
    END OF gc_entry_types.
  METHODS:
    info IMPORTING iv_text TYPE csequence
         RAISING   zcx_cilib_util_logging_failed,
    warning IMPORTING iv_text TYPE csequence
            RAISING   zcx_cilib_util_logging_failed,
    error IMPORTING iv_text TYPE csequence
          RAISING   zcx_cilib_util_logging_failed,
    debug IMPORTING iv_text TYPE csequence
          RAISING   zcx_cilib_util_logging_failed,
    entry IMPORTING iv_text TYPE csequence
                    iv_type TYPE gty_entry_type
          RAISING   zcx_cilib_util_logging_failed
                    zcx_cilib_illegal_argument,
    exception IMPORTING ix_ex TYPE REF TO cx_root
              RAISING   zcx_cilib_util_logging_failed
                        zcx_cilib_illegal_argument,
    info_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                       VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                       VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                       VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                       VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                       VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
             RAISING   zcx_cilib_util_logging_failed,
    warning_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                          VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                          VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                          VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                          VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                          VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
                RAISING   zcx_cilib_util_logging_failed,
    error_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                        VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                        VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                        VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                        VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                        VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
              RAISING   zcx_cilib_util_logging_failed,
    debug_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                        VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                        VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                        VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                        VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                        VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
              RAISING   zcx_cilib_util_logging_failed,
    entry_msg IMPORTING VALUE(iv_msgid) TYPE syst_msgid DEFAULT sy-msgid
                        VALUE(iv_msgno) TYPE syst_msgno DEFAULT sy-msgno
                        VALUE(iv_msgty) TYPE syst_msgty DEFAULT sy-msgty
                        VALUE(iv_msgv1) TYPE syst_msgv DEFAULT sy-msgv1
                        VALUE(iv_msgv2) TYPE syst_msgv DEFAULT sy-msgv2
                        VALUE(iv_msgv3) TYPE syst_msgv DEFAULT sy-msgv3
                        VALUE(iv_msgv4) TYPE syst_msgv DEFAULT sy-msgv4
              RAISING   zcx_cilib_util_logging_failed
                        zcx_cilib_illegal_argument.
ENDINTERFACE.
