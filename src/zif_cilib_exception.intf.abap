"! Exception
INTERFACE zif_cilib_exception PUBLIC.
  INTERFACES:
    if_t100_message.
  CONSTANTS:
    BEGIN OF gc_default_message,
      msgid TYPE symsgid VALUE 'ZCILIB',
      msgno TYPE symsgno VALUE '006',
      attr1 TYPE scx_attrname VALUE 'ZIF_CILIB_EXCEPTION~MS_MSG-MSGV1',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF gc_default_message.
  DATA:
    ms_msg TYPE zcilib_msg READ-ONLY.
ENDINTERFACE.
