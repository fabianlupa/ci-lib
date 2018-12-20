"! Exception
INTERFACE zif_cilib_exception PUBLIC.
  CONSTANTS:
    BEGIN OF gc_t100_message,
      msgid TYPE symsgid VALUE 'ZCILIB',
      msgno TYPE symsgno VALUE '003',
      attr1 TYPE scx_attrname VALUE 'ZIF_CILIB_EXCEPTION~MV_MSGV1',
      attr2 TYPE scx_attrname VALUE 'ZIF_CILIB_EXCEPTION~MV_MSGV2',
      attr3 TYPE scx_attrname VALUE 'ZIF_CILIB_EXCEPTION~MV_MSGV3',
      attr4 TYPE scx_attrname VALUE 'ZIF_CILIB_EXCEPTION~MV_MSGV4',
    END OF gc_t100_message,
    BEGIN OF gc_default_message,
      msgid TYPE symsgid VALUE 'ZCILIB',
      msgno TYPE symsgno VALUE '006',
      attr1 TYPE scx_attrname VALUE 'ZIF_CILIB_EXCEPTION~MV_MSGV1',
      attr2 TYPE scx_attrname VALUE '',
      attr3 TYPE scx_attrname VALUE '',
      attr4 TYPE scx_attrname VALUE '',
    END OF gc_default_message.
  DATA:
    mv_msgv1 TYPE syst_msgv READ-ONLY,
    mv_msgv2 TYPE syst_msgv READ-ONLY,
    mv_msgv3 TYPE syst_msgv READ-ONLY,
    mv_msgv4 TYPE syst_msgv READ-ONLY.
ENDINTERFACE.
