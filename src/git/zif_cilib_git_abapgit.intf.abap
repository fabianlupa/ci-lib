"! abapGit API
INTERFACE zif_cilib_git_abapgit PUBLIC.
  TYPES:
    gty_repo_key TYPE c LENGTH 12.
  METHODS:
    get_repo_url IMPORTING iv_repo_key   TYPE gty_repo_key
                 RETURNING VALUE(rv_url) TYPE string
                 RAISING   zcx_cilib_not_found,
    is_object_part_of_online_repo IMPORTING is_object         TYPE zif_cilib_constants=>gty_object
                                  EXPORTING ev_repo_key       TYPE gty_repo_key
                                  RETURNING VALUE(rv_is_part) TYPE abap_bool.
ENDINTERFACE.
