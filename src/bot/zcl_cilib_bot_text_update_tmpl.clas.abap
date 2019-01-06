"! Simple text update template
CLASS zcl_cilib_bot_text_update_tmpl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_bot_update_tmpl.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mt_entries TYPE STANDARD TABLE OF string.
ENDCLASS.



CLASS zcl_cilib_bot_text_update_tmpl IMPLEMENTATION.
  METHOD zif_cilib_bot_update_tmpl~add_update_entry.
    APPEND iv_entry TO mt_entries.
  ENDMETHOD.

  METHOD zif_cilib_bot_update_tmpl~get_comment_as_string.
    DATA(lv_separator) = repeat( val = cl_abap_char_utilities=>cr_lf occ = 2 ).
    CONCATENATE LINES OF mt_entries INTO rv_comment SEPARATED BY lv_separator.
  ENDMETHOD.
ENDCLASS.
