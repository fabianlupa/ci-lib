"! Dummy logger
CLASS zcl_cilib_util_dummy_logger DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_cilib_util_logger.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_dummy_logger IMPLEMENTATION.
  METHOD zif_cilib_util_logger~debug ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~debug_msg ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~entry ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~entry_msg ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~error ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~error_msg ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~exception ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~info ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~info_msg ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~warning ##NEEDED.
  ENDMETHOD.

  METHOD zif_cilib_util_logger~warning_msg ##NEEDED.
  ENDMETHOD.
ENDCLASS.
