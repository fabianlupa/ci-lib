"! Bot
INTERFACE zif_cilib_bot PUBLIC.
  TYPES:
    gty_event TYPE c LENGTH 1,
    BEGIN OF gty_transport_info,
      transport   TYPE trkorr,
      system      TYPE syst_sysid,
      event       TYPE gty_event,
      return_code TYPE trretcode,
    END OF gty_transport_info,
    gty_transport_info_tab TYPE STANDARD TABLE OF gty_transport_info WITH DEFAULT KEY.
  CONSTANTS:
    BEGIN OF gc_events,
      released TYPE gty_event VALUE 'R',
      imported TYPE gty_event VALUE 'I',
    END OF gc_events.
  METHODS:
    add_info_to_cts_comment IMPORTING ii_host           TYPE REF TO zif_cilib_host
                                      iv_repo           TYPE zcilib_host_repo
                                      iv_branch         TYPE string
                                      it_new_info       TYPE gty_transport_info_tab
                            RETURNING VALUE(rv_success) TYPE abap_bool
                            RAISING   zcx_cilib_illegal_argument,
    add_info_to_wiki_page IMPORTING ii_host           TYPE REF TO zif_cilib_host
                                    iv_repo           TYPE zcilib_host_repo
                                    io_repo_config    TYPE REF TO zcl_cilib_host_repo_config
                                    it_new_info       TYPE gty_transport_info_tab
                          RETURNING VALUE(rv_success) TYPE abap_bool
                          RAISING   zcx_cilib_illegal_argument,
    reorg_cts_comments IMPORTING ii_host   TYPE REF TO zif_cilib_host
                                 iv_repo   TYPE zcilib_host_repo
                                 iv_branch TYPE string
                       RAISING   zcx_cilib_http_comm_error
                                 zcx_cilib_illegal_argument
                                 zcx_cilib_not_found.
ENDINTERFACE.
