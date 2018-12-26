"! General settings
CLASS zcl_cilib_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_data TYPE zcilib_settings_data,
      is_logging_enabled RETURNING VALUE(rv_enabled) TYPE abap_bool,
      get_bal_object RETURNING VALUE(rv_object) TYPE balobj_d,
      get_bal_subobject RETURNING VALUE(rv_subobject) TYPE balsubobj,
      get_bal_keep_days RETURNING VALUE(rv_days) TYPE i,
      get_exit_event_destination RETURNING VALUE(rv_destination) TYPE zcilib_exit_eventdest.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      ms_data TYPE zcilib_settings_data.
ENDCLASS.



CLASS zcl_cilib_settings IMPLEMENTATION.
  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.

  METHOD get_bal_keep_days.
    rv_days = ms_data-bal_keep_days.
  ENDMETHOD.

  METHOD get_bal_object.
    rv_object = ms_data-bal_object.
  ENDMETHOD.

  METHOD get_bal_subobject.
    rv_subobject = ms_data-bal_subobject.
  ENDMETHOD.

  METHOD is_logging_enabled.
    rv_enabled = ms_data-enable_logging.
  ENDMETHOD.

  METHOD get_exit_event_destination.
    rv_destination = ms_data-exit_event_destination.
  ENDMETHOD.
ENDCLASS.
