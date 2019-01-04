"! System group
CLASS zcl_cilib_cust_system_group DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF gty_system,
        order       TYPE i,
        system_id   TYPE syst_sysid,
        description TYPE ddtext,
      END OF gty_system,
      gty_system_tab TYPE STANDARD TABLE OF gty_system WITH EMPTY KEY.
    METHODS:
      constructor IMPORTING iv_system_group TYPE zcilib_cust_sysgrp
                            it_systems      TYPE gty_system_tab,
      get_systems RETURNING VALUE(rt_systems) TYPE gty_system_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_name    TYPE zcilib_sysgrp,
      mt_systems TYPE gty_system_tab.
ENDCLASS.



CLASS zcl_cilib_cust_system_group IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_system_group.
    mt_systems = it_systems.
  ENDMETHOD.

  METHOD get_systems.
    rt_systems = mt_systems.
  ENDMETHOD.
ENDCLASS.
