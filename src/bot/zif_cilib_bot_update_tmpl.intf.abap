"! CTS update comment template
INTERFACE zif_cilib_bot_update_tmpl PUBLIC.
  METHODS:
    add_update_entry IMPORTING iv_entry TYPE csequence,
    get_comment_as_string RETURNING VALUE(rv_comment) TYPE string.
ENDINTERFACE.
