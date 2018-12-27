PROGRAM zcilib_cust_events.

FORM after_initialization.
  zcl_cilib_cust_factory=>get_settings( ).
ENDFORM.
