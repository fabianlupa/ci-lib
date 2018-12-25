PROGRAM zcilib_cust_events.

FORM after_initialization.
  SELECT COUNT(*) FROM zcilib_settings.
  IF sy-dbcnt = 0.
    INSERT zcilib_settings FROM @( VALUE #( dummy = space ) ).
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
