"! Repository configuration
CLASS zcl_cilib_host_repo_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_repo TYPE zcilib_host_repo
                            is_data TYPE zcilib_host_repo_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_host_repo_config IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.
ENDCLASS.
