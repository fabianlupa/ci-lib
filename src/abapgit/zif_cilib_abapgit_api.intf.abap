"! abapGit API
INTERFACE zif_cilib_abapgit_api PUBLIC.
  TYPES:
    BEGIN OF gty_object,
      type TYPE trobjtype,
      name TYPE sobj_name,
    END OF gty_object,
    gty_repo_key TYPE c LENGTH 12.
  METHODS:
    get_repo_url_for_package IMPORTING iv_repo_key   TYPE gty_repo_key
                             RETURNING VALUE(rv_url) TYPE string
                             RAISING   zcx_cilib_not_found,
    is_object_part_of_online_repo IMPORTING is_object         TYPE gty_object
                                  EXPORTING ev_repo_key       TYPE gty_repo_key
                                  RETURNING VALUE(rv_is_part) TYPE abap_bool.
ENDINTERFACE.
