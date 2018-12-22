"! General constants
INTERFACE zif_cilib_constants PUBLIC.
  TYPES:
    BEGIN OF gty_object,
      type TYPE trobjtype,
      name TYPE sobj_name,
    END OF gty_object,
    gty_object_tab TYPE SORTED TABLE OF gty_object WITH UNIQUE KEY type name.
ENDINTERFACE.
