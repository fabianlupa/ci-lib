"! CTS API
INTERFACE zif_cilib_cts_api PUBLIC.
  TYPES:
    gty_package_tab TYPE SORTED TABLE OF devclass WITH UNIQUE KEY table_line.
  METHODS:
    get_locked_r3tr_objects_in_tr IMPORTING iv_transport      TYPE trkorr
                                  RETURNING VALUE(rt_objects) TYPE zif_cilib_constants=>gty_object_tab
                                  RAISING   zcx_cilib_illegal_argument,
    get_package_for_object IMPORTING is_object         TYPE zif_cilib_constants=>gty_object
                           RETURNING VALUE(rv_package) TYPE devclass
                           RAISING   zcx_cilib_not_found,
    get_packages_for_objects IMPORTING it_objects         TYPE zif_cilib_constants=>gty_object_tab
                             RETURNING VALUE(rt_packages) TYPE gty_package_tab
                             RAISING   zcx_cilib_not_found,
    get_cts_organizer_web_ui_url IMPORTING iv_system     TYPE syst_sysid DEFAULT sy-sysid
                                           iv_transport  TYPE trkorr OPTIONAL
                                 RETURNING VALUE(rv_url) TYPE string
                                 RAISING   zcx_cilib_illegal_argument.
ENDINTERFACE.
