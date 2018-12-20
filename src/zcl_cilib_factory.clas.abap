"! Factory
CLASS zcl_cilib_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_bot RETURNING VALUE(ri_bot) TYPE REF TO zif_cilib_bot,
      get_abapgit_api RETURNING VALUE(ri_abapgit_api) TYPE REF TO zif_cilib_abapgit_api,
      get_host_for_repo IMPORTING iv_repo_url    TYPE string
                        RETURNING VALUE(ri_host) TYPE REF TO zif_cilib_host.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      gi_abapgit_api TYPE REF TO zif_cilib_abapgit_api.
ENDCLASS.



CLASS zcl_cilib_factory IMPLEMENTATION.
  METHOD get_abapgit_api.
    IF gi_abapgit_api IS NOT BOUND.
*      gi_abapgit_api = NEW zcl_cilib_abapgit_api( ).
    ENDIF.
    ri_abapgit_api = gi_abapgit_api.
  ENDMETHOD.

  METHOD get_bot.

  ENDMETHOD.

  METHOD get_host_for_repo.
    DATA(lo_url) = NEW zcl_cilib_http_url( iv_repo_url ).
  ENDMETHOD.
ENDCLASS.
