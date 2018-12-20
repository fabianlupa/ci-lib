"! JSON element
CLASS zcl_cilib_util_json_element DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED
  GLOBAL FRIENDS zcl_cilib_util_json_parser.

  PUBLIC SECTION.
    TYPES:
      gty_child_list TYPE STANDARD TABLE OF REF TO zcl_cilib_util_json_element WITH EMPTY KEY.
    METHODS:
      get_children RETURNING VALUE(rt_children) TYPE gty_child_list.
  PROTECTED SECTION.
    TYPES:
      gty_children_list TYPE STANDARD TABLE OF REF TO zcl_cilib_util_json_element WITH EMPTY KEY
                             WITH UNIQUE HASHED KEY unique COMPONENTS table_line.
    METHODS:
      constructor IMPORTING ir_element TYPE REF TO data
                            io_parser  TYPE REF TO zcl_cilib_util_json_parser
                            io_parent  TYPE REF TO zcl_cilib_util_json_element OPTIONAL.
    DATA:
      mr_element  TYPE REF TO data,
      mo_parser   TYPE REF TO zcl_cilib_util_json_parser,
      mo_parent   TYPE REF TO zcl_cilib_util_json_element,
      mt_children TYPE gty_children_list.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cilib_util_json_element IMPLEMENTATION.
  METHOD constructor.
    mr_element = ir_element.
    mo_parser = io_parser.
    mo_parent = io_parent.
  ENDMETHOD.

  METHOD get_children.
    rt_children = mt_children.
  ENDMETHOD.
ENDCLASS.
