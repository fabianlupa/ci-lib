"! URL
CLASS zcl_cilib_http_url DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_url TYPE csequence,
      get_full_url RETURNING VALUE(rv_full_url) TYPE string,
      get_host RETURNING VALUE(rv_host) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      mv_url       TYPE string,
      mv_scheme    TYPE string,
      mv_authority TYPE string,
      mv_user      TYPE string,
      mv_host      TYPE string,
      mv_port      TYPE string,
      mv_path      TYPE string,
      mv_query     TYPE string,
      mv_fragment  TYPE string,
      mt_segments  TYPE string_table,
      mv_type      TYPE i.
ENDCLASS.



CLASS zcl_cilib_http_url IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_dummy.

    mv_url = iv_url.

    cl_rest_uri_utils=>parse_uri(
      EXPORTING
        iv_uri       = CONV string( iv_url )
      IMPORTING
        ev_scheme    = mv_scheme
        ev_authority = mv_authority
        ev_path      = mv_path
        ev_query     = mv_query
        ev_fragment  = mv_fragment
        et_segment   = mt_segments
        ev_type      = mv_type
    ).
    IF mv_authority IS NOT INITIAL.
      FIND REGEX '^(//)?((\w*)@)?(\w*)(:(\w*))?$' IN mv_authority
           SUBMATCHES lv_dummy lv_dummy mv_user mv_host lv_dummy mv_port.
    ENDIF.

    IF mv_scheme IS INITIAL OR
       mv_host IS INITIAL.
      ASSERT 1 = 2 ##TODO.
    ENDIF.
  ENDMETHOD.

  METHOD get_full_url.
    rv_full_url =
       |{ mv_scheme }://| &&
       |{ COND #( WHEN mv_user IS NOT INITIAL THEN |{ mv_user }@| ) }| &&
       |{ mv_host }| &&
       |{ COND #( WHEN mv_port IS NOT INITIAL THEN |:{ mv_port }| ) }| &&
       |{ mv_path } | &&
       |{ COND #( WHEN mv_query IS NOT INITIAL THEN |?{ mv_query }| ) }|.
  ENDMETHOD.

  METHOD get_host.
    rv_host = mv_host.
  ENDMETHOD.
ENDCLASS.
